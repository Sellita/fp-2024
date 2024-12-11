{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import qualified Lib3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

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
  --  testCase "Color not found test" $
--      Lib2.parseQuery "car BMW XM 2020" @?= Left "Color is not found",
    testCase "Parsing Car with different input" $
       Lib2.parseQuery "car Audi A4 2019 White" @?= Right (Lib2.Car "Audi" "A4" 2019 "White"),
 --   testCase "Parsing Car with incorrect date input" $
 --     Lib2.parseQuery "car Buic A90 1880 White" @?= Left "Year is incorrect",    
 --   testCase "Parsing invalid query should return error" $
 --     Lib2.parseQuery "invalid query" @?= Left "Command not found: invalid query",
    testCase "Parsing empty query should return error" $
      Lib2.parseQuery "" @?= Left "Nothing to parse"
  ]

propertyTests :: TestTree
propertyTests = testGroup "Test parse and rollBack"
  [
    QC.testProperty "render and parse statment should be the same" $
      \statement -> Lib3.parseCommand (Lib3.renderStatements (statement :: Lib3.Statements)) == Right (Lib3.StatementCommand statement,"")
  ]
--renderStatements :: Statements -> String
--parseStatements :: String -> Either String (Statements, String)

instance Arbitrary Lib3.Statements where
    arbitrary :: Gen Lib3.Statements
    arbitrary = oneof [
       Lib3.Batch <$> listOf1 arbitrary
      ]

instance Arbitrary Lib2.Query where
  arbitrary :: Gen Lib2.Query
  arbitrary =
    oneof
      [ 
        Lib2.Car <$> genSafeString <*> genSafeString <*> positiveInt <*> genSafeString
      ]

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf1 genSafeChar

positiveInt :: Gen Integer
positiveInt = arbitrary `suchThat` (> 1885) `suchThat` (< 2024) 