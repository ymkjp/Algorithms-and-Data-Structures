-- Section 5
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []
quicksort (x:xs) =   
    let
    	smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs 
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
	| even n 	= n : chain (n `div` 2)
	| odd n		= n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- accumulator
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

max' :: (Ord a) => [a] -> a
max' = foldl1 max

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- 関数適用演算子: カッコを増やしたくないときに使える
--($) :: (a -> b) -> a -> b
--f $ x = f x

-- sum $ map sqrt [1..300]
-- sqrt $ 3 + 4 +5

-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (> 10) $ map (*2) [2..10]


-- 関数合成: ラムダっぽいことを簡潔にできる
--(.) :: (b -> c) -> (a -> b) -> a -> c
--f . g = \x -> f (g x)

-- map (\x -> negate (abs x)) [-4..4]
-- map (negate . abs) [-4..4]

-- だんだんメソッドチェーンのように見えてくる。逆順だけど。
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]

-- 合わせ技
-- sum . replicate 5 $ max 6.7 8.9


sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- sum' xs = foldl (+) 0 xs
-- 両辺の xs を消すことができる

