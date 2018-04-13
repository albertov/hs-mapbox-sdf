{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Control.Lens
import Mapbox.SDF
import Prelude hiding (id)

import qualified Data.ByteString as BS

main :: IO ()
main = hspec spec

spec :: Spec
spec = it "can render awesome fonts" $ do
  fontData <- BS.readFile "test/Awesome/FontAwesome.otf"
  let gs  = getRange fontData (Range minBound (maxBound `div` 4))
  gs^?_Right.stacks.to length `shouldBe` Just 1
  gs^?_Right.stacks.ix 0.name `shouldBe` Just "FontAwesome Regular"
  gs^?_Right.stacks.ix 0.glyphs.to length `shouldBe` Just 7
  gs^.._Right.stacks.ix 0.glyphs.traverse.id `shouldBe` [32,168,169,174,180,198,216]
  
