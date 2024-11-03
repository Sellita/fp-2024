{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "get_list command test" $
      Lib2.parseQuery "get_list" @?= Right Lib2.GetCarList,
    testCase "get_list with not needed argumets fail test" $
      Lib2.parseQuery "get_list " @?= Right Lib2.GetCarList,
    testCase "Parsing Car should return correct Car" $
      Lib2.parseQuery "car BMW XM 2020 Black" @?= Right (Lib2.Car "BMW" "XM" 2020 "Black"),
    testCase "Color not found test" $
      Lib2.parseQuery "car BMW XM 2020" @?= Left "Color is not found",
    testCase "Parsing Car with different input" $
      Lib2.parseQuery "car Audi A4 2019 White" @?= Right (Lib2.Car "Audi" "A4" 2019 "White"),
    testCase "Parsing Car with incorrect date input" $
      Lib2.parseQuery "car Buic A90 1880 White" @?= Left "Year is incorrect",    
    testCase "Parsing invalid query should return error" $
      Lib2.parseQuery "invalid query" @?= Left "Command not found: invalid query",
    testCase "Parsing empty query should return error" $
      Lib2.parseQuery "" @?= Left "Nothing to parse"
  ]