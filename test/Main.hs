{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Control.Lens
import Data.Maybe (isJust)
import Mapbox.SDF
import Prelude hiding (id)

import qualified Data.ByteString as BS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "can render noto sans" $ do
    fontData <- BS.readFile "test/Noto/NotoSans-Regular.ttf"
    let gs  = getGlyphsRange fontData (Range 0 1024)
    gs^?_Right.stacks.to length `shouldBe` Just 1
    gs^?_Right.stacks.ix 0.name `shouldBe` Just "Noto Sans Regular"
    gs^?_Right.stacks.ix 0.glyphs.to length `shouldBe` Just 945
    gs^.._Right.stacks.ix 0.glyphs.traverse.maybe'bitmap `shouldSatisfy` any isJust
    
  it "can render font awesome" $ do
    fontData <- BS.readFile "test/Awesome/FontAwesome.otf"
    let gs  = getGlyphsRange fontData (Range 0 65550)
    gs^?_Right.stacks.to length `shouldBe` Just 1
    gs^?_Right.stacks.ix 0.name `shouldBe` Just "FontAwesome Regular"
    gs^?_Right.stacks.ix 0.glyphs.to length `shouldBe` Just 494
    let expected = [32,168,169,174,180,198,216]
    take (length expected) (gs^.._Right.stacks.ix 0.glyphs.traverse.id) `shouldBe` expected
    gs^.._Right.stacks.ix 0.glyphs.traverse.maybe'bitmap `shouldSatisfy` any isJust
