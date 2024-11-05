{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char as C -- todo remove me
import qualified Data.List as L -- todo remove me

type Parser a = String -> Either String (a, String)
type Car = (String, String, Integer, String)
type CarGarage = [Car]
type Model = String
type Year = Integer
type Color = String
type Brand = String

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query =
  GetCarList |
  RemoveCar Integer|
  Car String String Integer String |
  Debug

-- | The instances are needed basically for tests
instance Eq Query where
  (==) GetCarList GetCarList = True
  (==) (RemoveCar quantity) (RemoveCar quantity2) = quantity == quantity2
  (==) (Car brand model year color) (Car brand2 model2 year2 color2) = brand == brand2 && model == model2 && year == year2 && color == color2
  (==) _ _ = False

instance Show Query where
  show GetCarList = "GetCarList"
  show (RemoveCar i) = "RemoveCar " ++ show i
  show (Car brand model year color) = "Car " ++ brand ++ " " ++ model ++ " " ++ show year ++ " " ++ color
  show Debug = "Debug"

-- | Parses user's input.
-- The function must have tests.


parseQuery :: String -> Either String Query
parseQuery "" = Left "Nothing to parse"
-- parseQuery input = 
--     let 
--         command = parseVal parseWord' input
--     in 
--         case command of
--             Right ("car", _) -> concreteParser parseCar input
--             Right ("get_list", _) -> concreteParser parseGetList input
--             Right ("remove_car", _) -> concreteParser parseRemoveCar input
--             _ -> Left $ "Command not found: " ++ input

--or variant
parseQuery input = concreteParser (parseCar `or2` parseGetList `or2` parseRemoveCar `or2` parseDebug) input

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
   case a input of
       Right r1 -> Right r1
       Left e1 ->
           case b input of
               Right r2 -> Right r2
               Left e2 -> Left (e1 ++ ", " ++ e2)
--

concreteParser :: Parser a -> String -> Either String a
concreteParser p input =
    case p input of
        Right (v, "") -> Right v
        Right (_, rest) -> Left ("Unexpected input ending: " ++ rest)
        Left e -> Left e

-- <car> ::= "car" <brand> <model> <year> <color>
parseCar :: Parser Query
parseCar  = and5 (\_ brandValue modelValue yearValue colorValue -> Car brandValue modelValue yearValue colorValue )
                (parseVal (parseCommand "car")) 
                (parseVal parseBrand)
                (parseVal parseModel) 
                (parseVal (parseYear 1885 2024))
                (parseVal parseColor)
-- parseCar  = \input ->
--     let 
--         command = parseVal parseWord' input
--         brand = parseVal parseBrand (either id snd command)
--         model = parseVal parseModel (either id snd brand)
--         year = parseVal (parseYear 1885 2024) (either id snd model)
--         color = parseVal parseColor (either id snd year)
--     in    
--         case command of
--             Right ("car", _) ->
--                 case brand of 
--                     Right (brandValue, _) -> 
--                         case model of
--                             Right (modelValue, _) -> 
--                                 case year of
--                                     Right (yearValue, _) -> 
--                                         case color of
--                                             Right (colorValue, _) -> Right (Car brandValue modelValue yearValue colorValue, "")
--                                             Left _ -> Left "Color is not found"
--                                     Left _ -> Left "Year is incorrect"
--                             Left _ -> Left "Model is not found"
--                     Left _ -> Left "Brand is not found"
--             _ -> Left "Command is not car"

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f a b c d e = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> 
                            case d r3 of
                                Right(v4, r4) ->
                                    case e r4 of
                                        Right(v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

parseVal :: Parser a -> Parser a
parseVal a = \input ->
    case a input of
        Right (value, rest1) ->
            if rest1 == ""
                then Right (value, rest1)
                else 
                    case parseChar ' ' rest1 of
                        Right (_, rest2) -> Right (value, rest2)
                        Left e2 -> Left e2
        Left e1 -> Left e1

-- <get_list> ::= "get_list" 
parseGetList :: Parser Query
parseGetList input =
    case parseWord' input of
        Right("get_list", rest) -> 
            case parseEmptyString rest of 
                Right (_, "") -> Right (GetCarList, "")
                _ -> Left ("Unexpected input ending: " ++ rest)
        _ -> Left ("Unknown command: " ++ input)


parseDebug :: Parser Query
parseDebug input =
    case parseCommand input "debug" of
        Right(_, rest) -> 
            case parseEmptyString rest of 
                Right (_, "") -> Right (Debug, "")
                _ -> Left ("Unexpected input ending: " ++ rest)
        _ -> Left ("Unknown command: " ++ input)

-- <remove_car>::= "remove_car" <car_id>
parseRemoveCar :: Parser Query
parseRemoveCar = \input ->
    case parseRemoveCarCommand input of
        Right ("remove_car", rest) ->
            case parseNumber rest of
                Right (index, rest2) -> 
                    case rest2 of 
                        "" -> Right (RemoveCar index, rest2)
                        _ -> Left "remove_car should have only one argument"
                Left e -> Left $ "Wrong argument for remove_car: " ++ e ++ ". Should be an integer"
        _ -> Left "Command is not remove_car"
    
parseRemoveCarCommand :: Parser String
parseRemoveCarCommand input =
    case parseVal parseWord' input of
        Right ("remove_car", rest) -> Right ("remove_car", rest)
        Right (command, _) -> Left ("Unknown command " ++ command)
        Left e -> Left e

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State CarGarage deriving Show

   

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State garage) GetCarList = Right (Just (carGarageToString garage), State garage)
stateTransition (State garage) (Car brand model year color) = Right (Just "Car added", State (garage ++ [(brand, model, year, color)]))
stateTransition (State garage) (RemoveCar index) = 
  let 
    clearResult = removeCar garage index
  in
    case clearResult of
      Left e -> Left e
      Right r -> Right (Just "Car removed", State r)
stateTransition (State garage) (_) = Right(Just ("Debug" ++ show garage), State garage)

removeCar :: CarGarage -> Integer -> Either String CarGarage
removeCar [] _ = Left "No cars in the garage"
removeCar garage index = 
  if index < 0 || index >= fromIntegral (length garage)
    then Left "Index out of bounds"
    else Right (take (fromIntegral index) garage ++ drop (fromIntegral index + 1) garage)


carGarageToString :: CarGarage -> String
carGarageToString [] = "No cars in the garage"
carGarageToString garage = carToString' garage 0

carToString' :: [Car] -> Integer -> String
carToString' [] _ = ""
carToString' (car:rest) i = show i ++ ": " ++ show car ++ "\n" ++ carToString' rest (i + 1)

--helpers
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- <year> ::= <Integer>
parseYear :: Integer -> Integer -> Parser Year
parseYear minYear maxYear input =
    let
        result = parseNumber input
    in
        case result of
            Right (year, rest) ->
                if year >= minYear && year <= maxYear
                    then Right (year, rest)
                    else Left ("Year " ++ show year ++ " is out of bounds")
            Left e -> Left e

-- <brand> ::= <String>
parseBrand :: Parser Brand
parseBrand input =
    let
        result = parseUntilSpace input
    in
        case result of
            Right (brand, rest) -> Right (brand, rest)
            Left e -> Left e

-- <model> ::= <String>
parseModel :: Parser Model
parseModel input =
    let
        result = parseUntilSpace input
    in
        case result of
            Right (model, rest) -> Right (model, rest)
            Left e -> Left e

-- <color> ::= <String>
parseColor :: Parser Color
parseColor input =
    let
        result = parseUntilSpace input
    in
        case result of
            Right (color, rest) -> Right (color, rest)
            Left e -> Left e

parseCommand :: String -> Parser String
parseCommand concreteCommand input =
    let
        result = parseUntilSpace input
    in
        case result of
            Right (parsedWord, rest) -> 
                if parsedWord == concreteCommand then Right (concreteCommand, rest)
                else Left "Wrong command"
            Left e -> Left e

parseNumber :: Parser Integer
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseWord' :: Parser String
parseWord' input = parseWithRule isLetterOrDash input

parseUntilSpace :: Parser String
parseUntilSpace input = parseWithRule isNotWhiteSpace input

parseEmptyString :: Parser String
parseEmptyString [] = Right ("", "")
parseEmptyString input = parseWithRule isWhiteSpace input

parseWithRule :: (Char->Bool) -> Parser String
parseWithRule f input =
    let letters = L.takeWhile f input
        rest = L.drop (length letters) input
    in if not (null letters)
        then Right (letters, rest)
        else Left (input ++ " does not start with a letter")

isLetterOrDash :: Char -> Bool
isLetterOrDash c = C.isLetter c || c == '_'

isNotWhiteSpace :: Char -> Bool
isNotWhiteSpace c = c /= ' '

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' '
