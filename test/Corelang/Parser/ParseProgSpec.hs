module Corelang.Parser.ParseProgSpec where

import           SpecHelper

input = "main = double 21; double x = x + x;"

spec :: Spec
spec =
  describe "parseOr"
    $ context "in parsing boh"
    $ it "should be 1"
    $ fst (head (parse parseOr "true | false")) `shouldBe` Evar '|'
