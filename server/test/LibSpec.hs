module LibSpec (spec) where

import Lib (add)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Lib" $
    it "1+1" $ do
        add 1 1 `shouldBe` 2
