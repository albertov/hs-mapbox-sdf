{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mapbox.SDF.Context (Face(..), Library(..), GlyphInfo(..), sdfCtx) where

import Data.Monoid (mempty, (<>))
import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Storable (Storable)

newtype Face = Face (Ptr Face)
  deriving (Eq, Show, Storable)
newtype Library = Library (Ptr Library)
  deriving (Eq, Show, Storable)
newtype GlyphInfo = GlyphInfo (ForeignPtr GlyphInfo)
  deriving (Eq, Show)

sdfCtx :: C.Context
sdfCtx = C.baseCtx <> ctx
  where ctx = mempty {
    ctxTypesTable =
      [ (C.TypeName "FT_Library", [t| Library |])
      , (C.TypeName "FT_Face", [t| Face |])
      , (C.TypeName "glyph_info", [t| GlyphInfo |])
      ]
    }
