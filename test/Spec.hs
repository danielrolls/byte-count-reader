import Test.Hspec

import Data.ByteCountReader

parseTest string expectedValue = it ("Parse " <> string) $
                                    getSize string `shouldBe` Just expectedValue

convertTest = do 
         it "Unparsable strings should return Nothing" $
           getSize "unparseable" `shouldBe` Nothing
         parseTest "1 kb" 1000
         parseTest "0.5 kib" 512
         parseTest "1 kib" 1024
         parseTest "2 kb" 2000
         parseTest "3 kb" 3000
         parseTest ".5 MiB" 524288
         parseTest "4 GiB" 4294967296


main :: IO ()
main = hspec convertTest
