import Test.Hspec

import Data.ByteCountReader
import Data.Text (unpack)

main :: IO ()
main = hspec $ 
     do it "Unparsable strings should return Nothing" $
             sizeInBytes "unparseable" `shouldBe` Nothing
        testHappyPathScenarios sizeInBytes
          [ ("1 kb", 1000)
          , ("0.5 kib", 512)
          , ("1 kib", 1024)
          , ("2 kb", 2000)
          , ("3 kb", 3000)
          , (".5 MiB", 524288)
          , ("4 GiB", 4294967296)
          , ("1      tIb", 1099511627776)
          ]
        testHappyPathScenarios sizeInBytesAssumingBase2
          [ ("1 kb", 1024)
          , ("0.5 kib", 512)
          , ("1 kib", 1024)
          , ("2 kb", 2048)
          , ("3 kb", 3072)
          , (".5 MiB", 524288)
          , ("4 GiB", 4294967296)
          ]
        where testHappyPathScenarios functionUnderTest = foldl (>>) (return ()) . map (parseTest functionUnderTest)
              parseTest functionUnderTest (string, expectedValue) = it ("Parse " <> unpack string) $
                                                                       functionUnderTest string `shouldBe` Just expectedValue



