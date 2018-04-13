{- This file was auto-generated from glyphs.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Mapbox.SDF.Proto.Glyphs where
import qualified Data.ProtoLens.Reexport.Prelude as Prelude
import qualified Data.ProtoLens.Reexport.Data.Int as Data.Int
import qualified Data.ProtoLens.Reexport.Data.Word as Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Reexport.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Reexport.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Reexport.Data.Default.Class
       as Data.Default.Class
import qualified Data.ProtoLens.Reexport.Data.Text as Data.Text
import qualified Data.ProtoLens.Reexport.Data.Map as Data.Map
import qualified Data.ProtoLens.Reexport.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Reexport.Lens.Labels as Lens.Labels

data Fontstack = Fontstack{_Fontstack'name :: !Data.Text.Text,
                           _Fontstack'range :: !Data.Text.Text, _Fontstack'glyphs :: ![Glyph]}
               deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "name" f Fontstack Fontstack a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Fontstack'name
                 (\ x__ y__ -> x__{_Fontstack'name = y__}))
              Prelude.id

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "range" f Fontstack Fontstack a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Fontstack'range
                 (\ x__ y__ -> x__{_Fontstack'range = y__}))
              Prelude.id

instance (a ~ [Glyph], b ~ [Glyph], Prelude.Functor f) =>
         Lens.Labels.HasLens "glyphs" f Fontstack Fontstack a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Fontstack'glyphs
                 (\ x__ y__ -> x__{_Fontstack'glyphs = y__}))
              Prelude.id

instance Data.Default.Class.Default Fontstack where
        def
          = Fontstack{_Fontstack'name = Data.ProtoLens.fieldDefault,
                      _Fontstack'range = Data.ProtoLens.fieldDefault,
                      _Fontstack'glyphs = []}

instance Data.ProtoLens.Message Fontstack where
        descriptor
          = let name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required name)
                      :: Data.ProtoLens.FieldDescriptor Fontstack
                range__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "range"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required range)
                      :: Data.ProtoLens.FieldDescriptor Fontstack
                glyphs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "glyphs"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Glyph)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked glyphs)
                      :: Data.ProtoLens.FieldDescriptor Fontstack
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Text.pack "llmr.glyphs.fontstack")
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, name__field_descriptor),
                    (Data.ProtoLens.Tag 2, range__field_descriptor),
                    (Data.ProtoLens.Tag 3, glyphs__field_descriptor)])
                (Data.Map.fromList
                   [("name", name__field_descriptor),
                    ("range", range__field_descriptor),
                    ("glyphs", glyphs__field_descriptor)])

data Glyph = Glyph{_Glyph'id :: !Data.Word.Word32,
                   _Glyph'bitmap :: !(Prelude.Maybe Data.ByteString.ByteString),
                   _Glyph'width :: !Data.Word.Word32,
                   _Glyph'height :: !Data.Word.Word32, _Glyph'left :: !Data.Int.Int32,
                   _Glyph'top :: !Data.Int.Int32, _Glyph'advance :: !Data.Word.Word32}
           deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

instance (a ~ Data.Word.Word32, b ~ Data.Word.Word32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "id" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'id
                 (\ x__ y__ -> x__{_Glyph'id = y__}))
              Prelude.id

instance (a ~ Data.ByteString.ByteString,
          b ~ Data.ByteString.ByteString, Prelude.Functor f) =>
         Lens.Labels.HasLens "bitmap" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'bitmap
                 (\ x__ y__ -> x__{_Glyph'bitmap = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)

instance (a ~ Prelude.Maybe Data.ByteString.ByteString,
          b ~ Prelude.Maybe Data.ByteString.ByteString, Prelude.Functor f) =>
         Lens.Labels.HasLens "maybe'bitmap" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'bitmap
                 (\ x__ y__ -> x__{_Glyph'bitmap = y__}))
              Prelude.id

instance (a ~ Data.Word.Word32, b ~ Data.Word.Word32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "width" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'width
                 (\ x__ y__ -> x__{_Glyph'width = y__}))
              Prelude.id

instance (a ~ Data.Word.Word32, b ~ Data.Word.Word32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "height" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'height
                 (\ x__ y__ -> x__{_Glyph'height = y__}))
              Prelude.id

instance (a ~ Data.Int.Int32, b ~ Data.Int.Int32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "left" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'left
                 (\ x__ y__ -> x__{_Glyph'left = y__}))
              Prelude.id

instance (a ~ Data.Int.Int32, b ~ Data.Int.Int32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "top" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'top
                 (\ x__ y__ -> x__{_Glyph'top = y__}))
              Prelude.id

instance (a ~ Data.Word.Word32, b ~ Data.Word.Word32,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "advance" f Glyph Glyph a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyph'advance
                 (\ x__ y__ -> x__{_Glyph'advance = y__}))
              Prelude.id

instance Data.Default.Class.Default Glyph where
        def
          = Glyph{_Glyph'id = Data.ProtoLens.fieldDefault,
                  _Glyph'bitmap = Prelude.Nothing,
                  _Glyph'width = Data.ProtoLens.fieldDefault,
                  _Glyph'height = Data.ProtoLens.fieldDefault,
                  _Glyph'left = Data.ProtoLens.fieldDefault,
                  _Glyph'top = Data.ProtoLens.fieldDefault,
                  _Glyph'advance = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message Glyph where
        descriptor
          = let id__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "id"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required id)
                      :: Data.ProtoLens.FieldDescriptor Glyph
                bitmap__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bitmap"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField maybe'bitmap)
                      :: Data.ProtoLens.FieldDescriptor Glyph
                width__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "width"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required width)
                      :: Data.ProtoLens.FieldDescriptor Glyph
                height__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "height"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required height)
                      :: Data.ProtoLens.FieldDescriptor Glyph
                left__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "left"
                      (Data.ProtoLens.SInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required left)
                      :: Data.ProtoLens.FieldDescriptor Glyph
                top__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "top"
                      (Data.ProtoLens.SInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required top)
                      :: Data.ProtoLens.FieldDescriptor Glyph
                advance__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "advance"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required advance)
                      :: Data.ProtoLens.FieldDescriptor Glyph
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Text.pack "llmr.glyphs.glyph")
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, id__field_descriptor),
                    (Data.ProtoLens.Tag 2, bitmap__field_descriptor),
                    (Data.ProtoLens.Tag 3, width__field_descriptor),
                    (Data.ProtoLens.Tag 4, height__field_descriptor),
                    (Data.ProtoLens.Tag 5, left__field_descriptor),
                    (Data.ProtoLens.Tag 6, top__field_descriptor),
                    (Data.ProtoLens.Tag 7, advance__field_descriptor)])
                (Data.Map.fromList
                   [("id", id__field_descriptor),
                    ("bitmap", bitmap__field_descriptor),
                    ("width", width__field_descriptor),
                    ("height", height__field_descriptor),
                    ("left", left__field_descriptor), ("top", top__field_descriptor),
                    ("advance", advance__field_descriptor)])

data Glyphs = Glyphs{_Glyphs'stacks :: ![Fontstack]}
            deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

instance (a ~ [Fontstack], b ~ [Fontstack], Prelude.Functor f) =>
         Lens.Labels.HasLens "stacks" f Glyphs Glyphs a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Glyphs'stacks
                 (\ x__ y__ -> x__{_Glyphs'stacks = y__}))
              Prelude.id

instance Data.Default.Class.Default Glyphs where
        def = Glyphs{_Glyphs'stacks = []}

instance Data.ProtoLens.Message Glyphs where
        descriptor
          = let stacks__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "stacks"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Fontstack)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked stacks)
                      :: Data.ProtoLens.FieldDescriptor Glyphs
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Text.pack "llmr.glyphs.glyphs")
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, stacks__field_descriptor)])
                (Data.Map.fromList [("stacks", stacks__field_descriptor)])

advance ::
        forall f s t a b . (Lens.Labels.HasLens "advance" f s t a b) =>
          Lens.Family2.LensLike f s t a b
advance
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "advance")

bitmap ::
       forall f s t a b . (Lens.Labels.HasLens "bitmap" f s t a b) =>
         Lens.Family2.LensLike f s t a b
bitmap
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bitmap")

glyphs ::
       forall f s t a b . (Lens.Labels.HasLens "glyphs" f s t a b) =>
         Lens.Family2.LensLike f s t a b
glyphs
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "glyphs")

height ::
       forall f s t a b . (Lens.Labels.HasLens "height" f s t a b) =>
         Lens.Family2.LensLike f s t a b
height
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "height")

id ::
   forall f s t a b . (Lens.Labels.HasLens "id" f s t a b) =>
     Lens.Family2.LensLike f s t a b
id
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "id")

left ::
     forall f s t a b . (Lens.Labels.HasLens "left" f s t a b) =>
       Lens.Family2.LensLike f s t a b
left
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "left")

maybe'bitmap ::
             forall f s t a b .
               (Lens.Labels.HasLens "maybe'bitmap" f s t a b) =>
               Lens.Family2.LensLike f s t a b
maybe'bitmap
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'bitmap")

name ::
     forall f s t a b . (Lens.Labels.HasLens "name" f s t a b) =>
       Lens.Family2.LensLike f s t a b
name
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")

range ::
      forall f s t a b . (Lens.Labels.HasLens "range" f s t a b) =>
        Lens.Family2.LensLike f s t a b
range
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "range")

stacks ::
       forall f s t a b . (Lens.Labels.HasLens "stacks" f s t a b) =>
         Lens.Family2.LensLike f s t a b
stacks
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "stacks")

top ::
    forall f s t a b . (Lens.Labels.HasLens "top" f s t a b) =>
      Lens.Family2.LensLike f s t a b
top
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "top")

width ::
      forall f s t a b . (Lens.Labels.HasLens "width" f s t a b) =>
        Lens.Family2.LensLike f s t a b
width
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "width")
