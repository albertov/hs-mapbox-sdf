{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mapbox.SDF (
  getGlyphsRange
, encodeGlyphs
, decodeGlyphs
, Range (..)
, SDFError (..)
, Glyphs
, Glyph
, Fontstack
, advance
, bitmap
, glyphs
, height
, id
, left
, maybe'bitmap
, name
, range
, stacks
, top
, width
) where

import Mapbox.SDF.Proto.Glyphs
import Mapbox.SDF.Context
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.String (fromString)
import Data.ByteString.Unsafe (unsafePackMallocCString, unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import qualified Data.ProtoLens.Encoding as PL
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import Foreign.C.String (CStringLen)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Protolude
import Prelude (String)

C.context (sdfCtx <> C.cppCtx <> C.bsCtx <> C.vecCtx <> C.fptrCtx)
C.include "<mapbox/glyph_foundry.hpp>"
C.include "<mapbox/glyph_foundry_impl.hpp>"
C.using "namespace sdf_glyph_foundry"

data Range = Range Word32 Word32
  deriving (Eq, Show)

getGlyphsRange :: ByteString -> Range -> Either SDFError Glyphs
getGlyphsRange fontData range_ = unsafePerformIO $ try $ withLibrary $ \ lib -> do
  fontStacks <- withFaces lib fontData (mapM (toFontStack range_))
  pure (def & stacks .~ fontStacks)

encodeGlyphs :: Glyphs -> ByteString
encodeGlyphs = PL.encodeMessage

decodeGlyphs :: ByteString -> Either String Glyphs
decodeGlyphs = PL.decodeMessage


toFontStack :: Range -> Face -> IO Fontstack
toFontStack (Range (fromIntegral->start) (fromIntegral->end)) face = do
  name_ <- faceName face
  glyphs_ <- catMaybes
         <$> mapM (\code -> maybe (pure Nothing) (fmap Just . toGlyph code) =<< renderGlyph face code)
                  [start..end]
  pure $ def & name   .~ name_
             & range  .~ fromString (show start <> "-" <> show end)
             & glyphs .~ glyphs_

toGlyph :: C.CULong -> GlyphInfo -> IO Glyph
toGlyph charCode p = do
  (w,h,l,t,a,bm,bml) <- C.withPtrs_ $ \(w,h,l,t,a,bm,bml) -> [CU.block|void {
    glyph_info const &g = *$fptr-ptr:(glyph_info *p);
    *$(uint32_t *w) = g.width;
    *$(uint32_t *h) = g.height;
    *$(uint32_t *l) = g.left;
    *$(uint32_t *t) = g.top - g.ascender;
    *$(uint32_t *a) = g.advance;
    if (g.width>0) {
      *$(uint32_t *bml) = g.bitmap.size();
      *$(char **bm) = static_cast<char *>(malloc(g.bitmap.size()));
      if (*$(char **bm)) {
        std::memcpy(*$(char **bm), g.bitmap.data(), g.bitmap.size());
      }
    } else {
      *$(char **bm) = nullptr;
    }
  }|]
  bitmap_ <- if bm /= nullPtr
             then Just <$> unsafePackMallocCStringLen (bm,fromIntegral bml)
             else pure Nothing
  pure $ def
       & id           .~ fromIntegral charCode
       & width        .~ fromIntegral w
       & height       .~ fromIntegral h
       & left         .~ fromIntegral l
       & top          .~ fromIntegral t
       & advance      .~ fromIntegral a
       & maybe'bitmap .~ bitmap_

faceName :: Face -> IO Text
faceName face = fmap toS . unsafePackMallocCString
            =<< throwIfNull identity NoFontFamilyName
            =<< C.withPtr_ (\p -> [CU.block|void {
  FT_Face face = $(FT_Face face);
  if (face->family_name) {
    if (face->style_name) {
      *$(char **p) = strdup((std::string(face->family_name) + " " + std::string(face->style_name)).c_str());
    } else {
      *$(char **p) = strdup(face->family_name);
    }
  } else {
    *$(char **p) = nullptr;
  }
  }|])


withLibrary :: (Library -> IO a) -> IO a
withLibrary = bracket createLibrary destroyLibrary

data SDFError = LibraryInitError
              | InvalidFontData
              | InvalidFaceIndex C.CInt
              | InvalidNumFaces C.CInt
              | NoFontFamilyName
  deriving (Eq, Show)
instance Exception SDFError


throwIfNull
  :: (Exception e, Eq b)
  => (Ptr a -> b) -> e -> b -> IO b
throwIfNull ctr err ptr | ptr == ctr nullPtr = throwIO err
throwIfNull _   _   ptr                      = pure ptr

createLibrary :: IO Library
createLibrary = throwIfNull Library LibraryInitError =<< C.withPtr_ (\p ->
  [CU.block|void {
    *$(FT_Library *p) = nullptr;
    FT_Error err = FT_Init_FreeType($(FT_Library *p));
    if (err) *$(FT_Library *p) = nullptr;
  }|])

destroyLibrary :: Library -> IO ()
destroyLibrary p =
  [CU.exp|void { FT_Done_FreeType($(FT_Library p)) }|]

destroyFace :: Face -> IO ()
destroyFace p =
  [CU.exp|void { FT_Done_Face($(FT_Face p)) }|]

withFaces :: Library -> ByteString -> ([Face] -> IO a) -> IO a
withFaces lib bs f = unsafeUseAsCStringLen bs (withFaces' lib f)

withFaces' :: Library -> ([Face] -> IO a) -> CStringLen -> IO a
withFaces' lib f (buf, fromIntegral -> len) = evalContT (getFaces >>= liftIO . f)
  where
    getFace ix = ContT $ bracket alloc destroyFace
      where
        alloc = throwIfNull Face exc =<< C.withPtr_ (\face -> [CU.block|void{
          FT_Error face_error =
            FT_New_Memory_Face($(FT_Library lib),
                               reinterpret_cast<FT_Byte const*>($(char *buf)),
                               static_cast<FT_Long>($(long len)),
                               static_cast<FT_Long>($(int ix)),
                               $(FT_Face *face));
          if (face_error) {
            *$(FT_Face *face) = nullptr;
          } else {
            // Set face size
            const double scale_factor = 1.0;
            double size = 24 * scale_factor;
            FT_Set_Char_Size(*$(FT_Face *face),0,(FT_F26Dot6)(size * (1<<6)),0,0);
            }
          }|])
        exc | ix==0     = InvalidFontData
            | otherwise = InvalidFaceIndex ix

    getFaces = do
      face0 <- getFace 0
      let numFaces = [CU.pure|int { $(FT_Face face0)->num_faces}|]
      unless (numFaces>=0) (liftIO (throwIO (InvalidNumFaces numFaces)))
      (face0:) <$> mapM getFace [1..numFaces-1]

type CharCode = C.CULong

renderGlyph :: Face -> CharCode -> IO (Maybe GlyphInfo)
renderGlyph face charCode =
  bracketOnError alloc freeGI
    (maybe (pure Nothing) (fmap (Just . GlyphInfo) . newForeignPtr destroyGlyphInfo) . maybePtr)
  where
    alloc = C.withPtr_ $ \p ->
      [CU.block|void {
        FT_UInt char_index = FT_Get_Char_Index($(FT_Face face), $(unsigned long charCode));
        if (char_index) {
          glyph_info *glyph = *$(glyph_info **p) = new glyph_info;
          glyph->glyph_index = char_index;
          RenderSDF(*glyph, 24, 3, 0.25, $(FT_Face face));
        } else {
          *$(glyph_info **p) = nullptr;
        }
      }|]
    freeGI p = [CU.exp|void{delete $(glyph_info *p)}|]
  
maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr p | p==nullPtr = Nothing
maybePtr p = Just p

foreign import ccall "&hs_mapbox_sdf_destroyGlyphInfo" destroyGlyphInfo :: FinalizerPtr GlyphInfo
