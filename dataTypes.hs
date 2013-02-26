module Shapes
       (Point(..)
        , Shape(..)
        , surface
        , nudge
        , baseCircle
        , baseRect
          ) where

import qualified Data.Map as Map


data Point = Point Float Float  deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float  -> Shape
nudge (Circle (Point x y) r) a b = (Circle (Point (x + a) (y + b)) r)
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
  = (Rectangle (Point (x1+a) (x2+b)) (Point (x2+a) (y2+b)))

baseCircle :: Float -> Shape
baseCircle r = (Circle (Point 0 0) r)

baseRect :: Float -> Float -> Shape
baseRect a b = (Rectangle (Point 0 0) (Point a b))

data Person = Person { firstName :: String
                       , lastName :: String
                       , age :: Int
                       , height :: Float
                       , phoneNumber :: String
                       , flavor :: String
                       } deriving (Show, Eq, Read)

data Car = Car { company :: String, model :: String, year :: Int } deriving (Show)

data Vector a  = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a  -> Vector a  -> Vector a
(Vector a b c) `vplus` (Vector x y z) = Vector (a+x) (b+y) (c+z)

vectMult :: (Num a) => Vector a -> a -> Vector a
(Vector a b c) `vectMult` m = Vector (m*a) (m*b) (m*c)

scalarMult :: (Num a) => Vector a -> Vector a ->  a
(Vector a b c) `scalarMult` (Vector x y z) = (a*x) + (b*y) + (c * z)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Enum, Bounded, Eq, Ord, Show, Read)
                  -- [Monday .. Friday] :: [Day]

-- just aliases
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [("Betty", "234234-234"),
             ("Tina", "23423"),
             ("223", "23232")]


data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show, Read)




data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ "doesn't exist!"
    Just (state, code) -> if state /= Taken
                         then Right code
                         else Left $ "Locker number " ++ show lockerNumber ++ " already taken"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

main = putStrLn "Hello, World!"