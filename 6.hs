-- Section 6
import Data.List
import qualified Data.Map as Map
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words


digitSum :: Int -> Int
digitSum = sum .map digitToInt . show

firstToInt :: Int -> Maybe Int
firstToInt n = find (\x -> digitSum x == n) [1..]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head .filter (\(k, v) -> key == k) $ xs

betterfindKey :: (Eq k) => k -> [(k, v)] -> Maybe v
betterfindKey key [] = Nothing
betterfindKey key ((k,v):xs)
	| key == k 	= Just v
	| otherwise	= betterfindKey key xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key xs = foldr
	(\(k,v) acc -> if key == k then Just v else acc)
	Nothing xs

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
	[("betty", "555-1938")
	,("bonnie", "42-2332")
	,("paty","44420-323")
	,("fasn","78-203")
	,("frontia","7-23-32")
	]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
	where add number1 number2 = number1 ++ ", " ++ number2


