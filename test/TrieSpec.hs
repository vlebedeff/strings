module TrieSpec where

import Test.Hspec
import Trie as T

spec :: Spec
spec = do
  describe "#fromPatterns" $ do
    it "builds trie from a list of patterns" $ do
      let check patterns edges =
            (lines . show . T.fromPatterns $ patterns) `shouldBe` edges
      check ["ATA"] ["0->1:A", "1->2:T", "2->3:A"]
      check ["AT", "AG", "AC"] ["0->1:A", "1->4:C", "1->3:G", "1->2:T"]
      check
        ["ATAGA", "ATC", "GAT"]
        [ "0->1:A"
        , "0->7:G"
        , "1->2:T"
        , "2->3:A"
        , "2->6:C"
        , "3->4:G"
        , "4->5:A"
        , "7->8:A"
        , "8->9:T"
        ]
      check
        ["A", "TO", "TEA", "TED", "TEN", "I", "IN", "INN"]
        [ "0->1:A"
        , "0->8:I"
        , "0->2:T"
        , "8->9:N"
        , "9->10:N"
        , "2->4:E"
        , "2->3:O"
        , "4->5:A"
        , "4->6:D"
        , "4->7:N"
        ]
  describe "#match" $ do
    let check text patterns indices =
          (match (fromPatterns patterns) text) `shouldBe` indices
    it "finds where patterns occur in a given string" $ do
      check "AAA" ["AA"] [0, 1]
      check "AA" ["T"] []
      check "AATCGGGTTCAATCGGGGT" ["ATCG", "GGGT"] [1, 4, 11, 15]
    context "when some patters are prefixes of other patters" $ do
      it "finds all occurrences" $ do check "ACATA" ["AT", "A", "AG"] [0, 2, 4]
