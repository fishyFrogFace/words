import Test.QuickCheck
import Test.Hspec 
import Lib

genPos :: Gen Int
genPos = choose (0, 1000)

main :: IO ()
main = hspec $ do
        describe "byLength" $ do
          it "counts length of successively longer lists" $ do
            property $ forAll genPos $ \n -> (take n $ byLength $ take n $ iterate (0:) []) == replicate n 1
          it "returns empty list for empty input" $ do
            byLength [] `shouldBe` []
          it "counts occurrences" $ do
            property $ forAll genPos $
              \n -> (take (n+3) $ byLength [[1..n], [], [1..n], [], []]) == 3:replicate (n-1) 0 ++ [2,0,0]
