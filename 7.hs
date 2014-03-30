-- Section 7
import Data.List
import qualified Data.Map as Map
import Data.Char

data Point = Point Float Float deriving (Show)
data Shape =
	Circle Point Float |
	Rectangle Point Point
	deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2))
	= (abs $ x2 - x1) * (abs $ y2 - y1)


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)


data Person = Person { firstName :: String
						, lastName :: String
						, age :: Int
					} deriving (Eq, Show, Read)

mikeD 	= Person {firstName = "Micheal", lastName = "Diamond", age = 43}
adRock	= Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca		= Person {firstName = "Adam", lastName = "Yauch", age = 44}


data Day = Monday | Tuesday | Wednesday | Thursday | Friday
	| Saturday | Sunday
	deriving (Eq, Ord, Show, Read, Bounded, Enum)


type PhoneNumber = String
type Name = String
type PhoneBook = [(String, String)]
phoneBook :: PhoneBook 
phoneBook =       
    [("betty", "555-2938")      
    ,("bonnie", "452-2928")      
    ,("patsy", "493-2928")      
    ,("lucille", "205-2928")      
    ,("wendy", "939-8282")      
    ,("penny", "853-2492")      
    ]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook


type AssocList k v = [(k, v)]

-- type IntMap = Map.Map Int

data Either a b = Left a | Right b
	deriving (Eq, Ord, Read, Show)


data LokerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LokerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup
lockerNumber map of
	Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist."
	Just (state, code) -> if state /= Taken
		then Right code
		else Left $ "Locker " ++ show lockerNumber ++ " is already taken."








