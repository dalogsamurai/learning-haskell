doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doublePoint x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

list = [1, 2, 3]

onlyDecade lim = [x | x <- [1 .. lim], mod x 10 == 0]

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a * a + b * b == c * c, a + b + c == 24]

addVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
minVector (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vecLength :: (Double, Double, Double) -> Double
vecLength (a1, a2, a3) = sqrt l
  where
    l = a1 ** a1 + a2 ** a2 + a3 ** a3

mulVector :: (Double, Double, Double) -> Double -> (Double, Double, Double)
mulVector (x1, x2, x3) a = (x1 * a, x2 * a, x3 * a)

description :: Integer -> String
description a = "Num is" ++ if even a then " even" else " odd"

description1 :: Integer -> String
description1 a
  | even a = "Even"
  | otherwise = "Odd"

myTake :: (Num i, Ord i) => i -> [a] -> [a]
myTake n _
  | n <= 0 = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n -1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myElem :: (Eq a) => a -> [a] -> Bool
myElem a [] = False
myElem a (x : xs)
  | a == x = True
  | otherwise = myElem a xs

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 2
fib a = a

replica :: Int -> a -> [a]
replica n x
  | n <= 0 = []
  | otherwise = x : replica (n -1) x

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smSorted = quickSort [a | a <- xs, a <= x]
      bgSorted = quickSort [a | a <- xs, a > x]
   in smSorted ++ [x] ++ bgSorted

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

-- chain :: Integer -> [Integer]

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int -> Int
numLongChains l = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > l

sumFold :: (Num a) => [a] -> a
sumFold xs = foldl (\acc x -> acc + x) 0 xs

minFoldr :: (Num a) => [a] -> a
minFoldr xs = foldr (\x acc -> acc - x) 0 xs

minFold :: (Num a) => [a] -> a
minFold xs = foldl (\acc x -> acc - x) 0 xs

mulFold :: (Num a) => [a] -> a
mulFold xs = foldl (\acc x -> acc * x) 1 xs

