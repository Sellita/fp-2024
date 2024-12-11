{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use <$>" #-}
module Lib2NewParser
    ( Query(..),
    parseQuery,
    State(..),
    CarGarage,
    Car,
    Parser,
    emptyState,
    parseWithRule,
    stateTransition
    ) where

import qualified Data.Char as C -- todo remove me
import qualified Data.List as L -- todo remove me

import qualified Control.Monad.Trans.State.Strict as St
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.State.Strict (StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Applicative (Alternative)
import GHC.Base (empty, Alternative ((<|>)))



type Parser a = ExceptT String (St.State String) a

--type Parser a = String -> Either String (a, String)
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
  show GetCarList = "get_list"
  show (RemoveCar i) = "remove_car " ++ show i
  show (Car brand model year color) = "car " ++ brand ++ " " ++ model ++ " " ++ show year ++ " " ++ color
  show Debug = "debug"

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
parseQuery input = concreteParser (parseCar <|> parseGetList <|> parseRemoveCar <|> parseDebug) input




-- instance Alternative Parser a where
--     empty :: Parser a
--     empty = Parser $ \input -> Left $ "Could not parse " ++ input
--     (<|>) :: Parser a -> Parser a -> Parser a
--     p1 <|> p2 = Parser $ \inp ->
--         case (runParser p1 inp) of
--             Right r1 -> Right r1
--             Left e1 -> case (runParser p2 inp) of
--                             Right r2 -> Right r2
--                             Left e2 -> Left $ "Failed twise: " ++ e1 ++ " AND " ++ e2
                            
-- or2 :: Parser a -> Parser a -> Parser a
-- or2 a b = \input ->
--    case a input of
--        Right r1 -> Right r1
--        Left e1 ->
--            case b input of
--                Right r2 -> Right r2
--                Left e2 -> Left (e1 ++ ", " ++ e2)
--

concreteParser :: Parser a -> String -> Either String a
concreteParser p input = 
    case runState (runExceptT p) input of
        (Right v, "") -> Right v
        (Right _, rest) -> Left ("Unexpected input ending: " ++ rest)
        (Left e, _) -> Left e



-- <car> ::= "car" <brand> <model> <year> <color>
parseCar :: Parser Query
parseCar = do
    _ <- parseCommand "car"
    brandValue <- parseBrand
    modelValue <- parseModel
    yearValue <- parseYear 1885 2024
    colorValue <- parseColor
    return $ Car brandValue modelValue yearValue colorValue
-- parseCar  = and5 (\_ brandValue modelValue yearValue colorValue -> Car brandValue modelValue yearValue colorValue )
--                 (parseVal (parseCommand "car")) 
--                 (parseVal parseBrand)
--                 (parseVal parseModel) 
--                 (parseVal (parseYear 1885 2024))
--                 (parseVal parseColor)
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

-- and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
-- and5 f a b c d e = \input ->
--     case a input of
--         Right (v1, r1) ->
--             case b r1 of
--                 Right (v2, r2) ->
--                     case c r2 of
--                         Right (v3, r3) -> 
--                             case d r3 of
--                                 Right(v4, r4) ->
--                                     case e r4 of
--                                         Right(v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
--                                         Left e5 -> Left e5
--                                 Left e4 -> Left e4
--                         Left e3 -> Left e3
--                 Left e2 -> Left e2
--         Left e1 -> Left e1

parseVal :: Parser String
parseVal  = do
    result <- parseChar ' '
    if result == ' ' 
        then return $ show result
        else throwE $ "Year is not valid: " ++ show result

-- <get_list> ::= "get_list" 
parseGetList :: Parser Query
parseGetList  = do 
    result <- parseWord'
    if(result == "get_list")
        then return $ GetCarList
        else throwE $ "command not found"



parseDebug :: Parser Query
parseDebug  = do
    result <- parseWord'
    if(result == "get_list")
        then return $ Debug
        else throwE $ "command not found"


-- <remove_car>::= "remove_car" <car_id>
parseRemoveCar :: Parser Query
parseRemoveCar = do
    input <- lift get
    _ <- parseRemoveCarCommand
    indexOfCar <- parseNumber
    if indexOfCar > 0 
        then return $ RemoveCar indexOfCar
        else throwE "RemoveComand not parsed"
     
    -- case parseRemoveCarCommand input of
    --     Right ("remove_car", rest) ->
    --         case parseNumber rest of
    --             Right (index, rest2) -> 
    --                 case rest2 of 
    --                     "" -> Right (RemoveCar index, rest2)
    --                     _ -> Left "remove_car should have only one argument"
    --             Left e -> Left $ "Wrong argument for remove_car: " ++ e ++ ". Should be an integer"
    --     _ -> Left "Command is not remove_car"
    
parseRemoveCarCommand :: Parser String
parseRemoveCarCommand = do
    _ <- parseVal
    result <- parseWord'
    if(result == "remove_car")
        then return result 
        else throwE "comand not recognized"


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
parseChar a = do
    input <- lift get
    case input of
        [] -> throwE "Empty input"
        (x:xs) -> if x == a
            then lift $ put xs >> return x
            else
                throwE $ a:" is not found"
--parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
--parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- <year> ::= <Integer>
parseYear :: Integer -> Integer -> Parser Year
parseYear minYear maxYear = do
    result <- parseNumber 
    if result >= minYear && result <= maxYear
                then return result
                else throwE $ "Year is not valid: " ++ show result

-- <brand> ::= <String>
parseBrand :: Parser Brand
parseBrand = do
    result <- parseUntilSpace
    case result of
        [] -> throwE "model not found"
        str -> return str

-- <model> ::= <String>
parseModel :: Parser Model
parseModel = do
    result <- parseUntilSpace
    case result of
        [] -> throwE "model not found"
        str -> return str

-- <color> ::= <String>
parseColor :: Parser Color
parseColor  = do 
    result <- parseUntilSpace
    case result of
        [] -> throwE "color not found"
        str -> return str

parseCommand :: String -> Parser String
parseCommand concreteCommand = do
    res <- parseUntilSpace
    if res == concreteCommand
        then return res
        else
            throwE $ concreteCommand ++ " is not found"

parseNumber :: Parser Integer
parseNumber = do 
    input <- lift get 
    case input of 
        [] -> throwE "Not a number"
        str -> let
                digits = L.takeWhile C.isDigit str
                rest = drop (length digits) str
                in
                lift $ put rest >> return (read digits)

-- parseNumber [] = Left "empty input, cannot parse a number"
-- parseNumber str =
--     let
--         digits = L.takeWhile C.isDigit str
--         rest = drop (length digits) str
--     in
--         case digits of
--             [] -> Left "not a number"
--             _ -> Right (read digits, rest)

parseWord' :: Parser String
parseWord' = parseWithRule isLetterOrDash

parseUntilSpace :: Parser String
parseUntilSpace = parseWithRule isNotWhiteSpace

parseEmptyString :: Parser String
parseEmptyString = parseWithRule isWhiteSpace

-- parseEmptyString [] = Right ("", "")
-- parseEmptyString input = parseWithRule isWhiteSpace input

parseWithRule :: (Char->Bool) -> Parser String
parseWithRule f = do
    input <- lift get
    case input of 
        [] -> throwE "Not a number"
        str -> let
                letters = L.takeWhile f str
                rest = drop (length letters) str
                in
                lift $ put rest >> return letters



isLetterOrDash :: Char -> Bool
isLetterOrDash c = C.isLetter c || c == '_'

isNotWhiteSpace :: Char -> Bool
isNotWhiteSpace c = c /= ' '

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' '
