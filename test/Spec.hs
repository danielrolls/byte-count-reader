import Test.Hspec

import Data.ByteCountReader

main :: IO ()
main = hspec $ 
     do it "Unparsable strings should return Nothing" $
             sizeInBytes "unparseable" `shouldBe` Nothing
        testHappyPathScenarios
          [ ("1 kb", 1000)
          , ("0.5 kib", 512)
          , ("1 kib", 1024)
          , ("2 kb", 2000)
          , ("3 kb", 3000)
          , (".5 MiB", 524288)
          , ("4 GiB", 4294967296)
          , ("1      tIb", 1099511627776)
          ]
        where testHappyPathScenarios = foldl (>>) (return ()) . map parseTest
              parseTest (string, expectedValue) = it ("Parse " <> string) $
                                                     sizeInBytes string `shouldBe` Just expectedValue



