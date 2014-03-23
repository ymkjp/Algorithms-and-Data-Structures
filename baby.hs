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

