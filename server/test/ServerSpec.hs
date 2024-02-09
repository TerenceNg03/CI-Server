{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Server (sha)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "HMAC" $ do
    it "Github Example" $
        do
            sha "It's a Secret to Everybody" "Hello, World!" :: String
            `shouldBe` "sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17"
