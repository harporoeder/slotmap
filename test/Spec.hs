import Test.Hspec

import qualified Data.SlotMap as SM

main :: IO ()
main = hspec $ do
  describe "clone" $ do
    it "should be independent" $ do
      m <- SM.empty
      k <- SM.insert 9 m
      c <- SM.clone m
      SM.delete k m
      SM.lookup k m >>= (`shouldBe` Nothing)
      SM.lookup k c >>= (`shouldBe` Just 9)

  describe "insert" $ do
    it "should reuse space after deletion" $ do
      m <- SM.empty
      a <- SM.insert 3 m
      b <- SM.insert 6 m
      SM.delete a m
      SM.delete b m
      SM.insert 5 m
      SM.size m >>= (`shouldBe` 1)
      SM.capacity m >>= (`shouldBe` 2)

  describe "lookup" $ do
    it "should return value that exists" $ do
      m <- SM.empty
      k <- SM.insert 5 m
      SM.lookup k m >>= (`shouldBe` Just 5)

    it "should not return deleted value after replacement" $ do
      m <- SM.empty
      k <- SM.insert 5 m
      SM.delete k m
      SM.insert 12 m
      SM.lookup k m >>= (`shouldBe` Nothing)
      SM.size m >>= (`shouldBe` 1)
      SM.capacity m >>= (`shouldBe` 1)

  describe "elems" $ do
    it "should skip deleted" $ do
      m <- SM.empty
      SM.insert 5 m
      k <- SM.insert 9 m
      SM.insert 2 m
      SM.delete k m
      SM.elems m >>= (`shouldBe` [5, 2])
