lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN !"
lucky x = "Sorry, you're out of luck, pal !"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dammy!"
head' (x:_) = x

bmiTell :: Double -> Double -> String
bmiTell weight height
	| bmi <= skinny	= "You're underweight, you emo, you!"
	| bmi <= normal = "You're supporsedly normal.\
		\ Pfft, I bet you're ugly!"
	| bmi <= fat = "You're fat! Lose some weight, fatty!"
	| otherwise 	= "You're a whale, congratulations!"
	where -- To new line
		bmi = weight / height ^ 2
		(skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where
		(f:_) = firstname
		(l:_) = lastname


calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi w h | (w, h) <- xs, let bmi weight height = weight / height ^ 2]

-- recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
	| n <= 0	= []
	| otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
	| n <= 0 	= []
take' _ [] 	= []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
	| a == x 		= True
	| otherwise = a `elem'` xs

-- Quick Sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let
			smallerOrEqual = [a | a <- xs, a <= x]
			larger = [a | a <- xs, a > x]
		in quicksort smallerOrEqual ++ [x] ++ quicksort larger

 
