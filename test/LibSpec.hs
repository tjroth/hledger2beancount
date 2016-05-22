module LibSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ it "works" $ True `shouldBe` True
