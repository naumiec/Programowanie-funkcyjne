module Lab2 where

{-# LANGUAGE BangPatterns #-}

import Data.List

myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: Num a => (a -> (a -> a)) -- prawostronnie łączne 
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> a -> a -> a
add3C x y z = x + y + z

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f x y = f (x,y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x,y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)
-- fiveToPower_ 3 = 125

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)
-- _ToPower5 2 = 32

subtr_From5 :: Num a => a -> a
subtr_From5 = (5 -)
-- subtrNFrom5 3 = 2

subtr5From_ :: Num a => a -> a 
subtr5From_ = flip (-) 5
-- subtr5From_ 6 = 1

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x
-- odpowiednik flip z biblioteki standardowej Prelude

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x
-- trójparametrowy odpowiednik funckji flip2

isPalindrome :: [Char] -> Bool
isPalindrome s = reverse s == s
-- isPalindrome "ABBA" = True

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx n xs = head (drop (n-1) xs ) -- getElemAtIdx 2 [1,2,3] = 2
-- z wykorzystaniem funkcji head i drop (<=> xs !! n)

capitalize :: [Char] -> [Char]
capitalize (x:xs) = toUpper x : xs
-- capitalize list = toUpper(head list) : tail list
-- capitalize "ala" = "Ala"

toUpper :: Char -> Char
toUpper c   | c >= 'a' && c <= 'z' = toEnum (fromEnum c + fromEnum 'A' - fromEnum 'a')
            | otherwise = c

pythagoreanTripleIn100 :: [(Int, Int, Int)]
pythagoreanTripleIn100 = [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]

numberOfPythagoreanTripleIn100 :: Int
numberOfPythagoreanTripleIn100 = length pythagoreanTripleIn100

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..floor (sqrt (fromIntegral n))], n `mod` i == 0] == [] -- szukanie dzielników do podłogi z pierwiastka kwadratowego z n - floor (sqrt (fromIntegral n))
-- złooność obliczeniowa O(sqrt(n))
{-
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == [] -- to nie jest efektywna implementacja
-- złożoność obliczeniowa O(n)
-}

primes :: Int -> [Int]
primes n = eratoSieve [2..n]
    where 
        eratoSieve :: [Int] -> [Int]
        eratoSieve [] = [] -- bez tego Non-exhaustive patterns in function eratoSieve -- Niewyczerpujące wzorce funkcji eratoSieve
        eratoSieve (p:xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]
{-
primes n = [i | i <- [2..n], isPrime i]
-}
{-
primes n = [x | x <- [2..n], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]
-}
{-
primes n = eratoSieve [2..n]
    where 
        eratoSieve :: [Int] -> [Int]
        eratoSieve (p:xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]
-}

numberOfPrimesInN :: Int -> Int
numberOfPrimesInN n = length (primes n)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = and [x == y | y <- xs]

fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 
        then n
    else 
        fib (n - 2) + fib (n - 1)
-- 5: 0,00 s
-- 10: 0,01 s
-- 15: 0,01 s
-- 20: 0,05 s
-- 25: 0,24 s
-- 30: 2,54 s
-- 31: 4,07 s
-- 32: 6,86 s
-- 33: 11,44 s
-- 34: 18,29 s
-- 35: 28,62 s
-- 40: 319,29 s
-- złożoność obliczeniowa O(2^n)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
-- fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)] -- to samo co wyżej

fib2 :: Int -> Int
fib2 n = fibs !! n

fib3 :: Int -> Int
fib3 n = fibs !! n
    where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs
-- sum' [1,2,3] = 6
-- sum' [] = 0

prod' :: Num a => [a] -> a
prod' [] = 0
prod' (x:xs) = x * prod' xs
-- prod' [1,2,3] = 6
-- prod' [] = 0

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs
-- lenght' [1,1,1,1] = 4

or' :: [Bool] -> Bool 
or' [] = False
or' (x:xs) = 
    if x == True
        then True
    else or' xs
-- or' [True, False, True] = True

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) = 
    if x == False
        then False
    else and'check xs
    where  
        and'check :: [Bool] -> Bool
        and'check [] = True
        and'check (x:xs) = 
            if x == False
                then False
            else and'check xs
-- and' [True, False, True] = False

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' i (x:xs) = 
    if x == i
        then True
    else elem' i xs
-- elem' 3 [1,2,3] = True

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = 2 * x : doubleAll xs
-- doubleAll [1,2] = [2,4]

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = x ^ 2 : squareAll xs
-- squareAll [2,3] = [4,9]

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = 
    if x `mod` 2 == 0
        then x : selectEven xs 
    else selectEven xs
-- selectEven [1,2,3] = [2]

-- za kazdym razem trzeba rozwazyc przypadek pustej tablicy, sprawdzamy pierwszy element obecnej listy i wykonujemy funckję dla reszty 

-- stos: maksymalnie 32767 = 2^15 - 1, powyzej przepełnienie 
{-
ghc: panic! (the 'impossible' happened)
  (GHC version 9.2.4:
        stack depth overflow

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
-}

arithmeticMean :: Fractional a => [a] -> a
arithmeticMean [] = 0
arithmeticMean xs = sum xs / fromIntegral (length xs)
-- arithmeticMean [1,2,3] = 2.0

geometricMean :: Floating a => [a] -> a
geometricMean [] = 0
geometricMean xs = product xs ** (1 / fromIntegral (length xs))
-- geometricMean [1,1,1,10000] = 10.0

aAndGMean :: Floating a => [a] -> (a, a)
aAndGMean [] = (0, 0)
aAndGMean xs = (sum xs / fromIntegral (length xs), product xs ** (1 / fromIntegral (length xs)))
{-
aAndGMean xs = (arithmeticMean xs, geometricMean xs)
-}
-- tak, jako krotkę 

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where   loop acc []     = acc
            loop acc (x:xs) = loop (x + acc) xs
-- sum'2 [1,2,3] = 6
-- dodano akuumulator, który przechowuje wynik

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where   loop acc []     = acc
            loop acc (x:xs) = loop (x + acc) xs
-- liczba argumentów jest mniejsza, można zastosować partial application
-- przepełnienie stosu tak samo jak przy sum' - maksymalny stos to: 32767

prod'2 :: Num a => [a] -> a
prof'2 [] = 0
prod'2 xs = 
    if null xs
        then 0
    else
        loop 1 xs
        where   loop acc []     = acc
                loop acc (x:xs) = loop (x * acc) xs
-- prod'2 [1,2,3] = 6
-- prod'2 [] = 0

length'2 :: [a] -> Int
length'2 [] = 0
length'2 xs = loop 0 xs
    where 
        loop acc [] = acc
        loop acc (x:xs) = loop (acc + 1) xs 
-- length'2 [1,2,3,4,5] = 5
-- length'2 [] = 0

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
   where loop !acc []     = acc
         loop !acc (x:xs) = loop (x + acc) xs

-- 100 [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
-- sum 100: 0.10 s
-- sum' 100: 0.07 s
-- sum'2 100: 0.01 s
-- sum'3 100: 0.01 s
-- sum'4 100: 0.02 s
-- różnica w czasie jest niewielka, ale sum'2 i sum'3 są najszybsze,
-- ponieważ nie tworzą kopii listy, a sum' tworzy kopię listy
-- ! - bang pattern, nie tworzy kopii listy(, ale nie jest to zalecane, ponieważ może prowadzić do błędów?)

isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)

-- rekurencja ogonowa - rekurencja, która nie tworzy kopii listy, ale zamiast tego zwraca wynik
-- rekurencja ogonowa jest szybsza(, ponieważ nie tworzy kopii listy, ale nie jest to zalecane, ponieważ może prowadzić do błędów?)
-- złożoność czasowa - O(n), gdzie n to długość listy
-- złożoność pamięciowa - O(1), nie tworzy kopii listy
-- złożoność obliczeniowa - O(n), gdzie n to długość listy

-- funkcja Ackermanna
ackerFun m n
 | m == 0    = n + 1
 | n == 0    = ackerFun (m - 1) 1
 | otherwise = ackerFun (m - 1) (ackerFun m (n - 1))

z1 y = ackerFun 3 y
z2 x = ackerFun x 3
-- złożoność obliczeniowa funkcji Ackermanna

-- z1 10 = 8189: 42.99 s
-- z2 3 = 0.01 s; z2 4 = bardzo duża liczba: bardzo dużo czasu

-- QuickSort - sortowanie szybkie - algorytm sortowania, który działa w czasie O(n log n)
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
    where
        smaller = filter (<= x) xs
        larger  = filter (> x) xs
{-
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart  xs = [ y | y <- xs, y <= x ]
        rightPart xs = [ y | y <- xs, y > x  ]
-}

-- MergeSort - sortowanie przez scalanie - algorytm sortowania, który działa w czasie O(n log n)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where
        (left, right) = splitAt (length xs `div` 2) xs
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys)
            | x <= y    = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys

-- InsertionSort - sortowanie przez wstawianie - algorytm sortowania, który działa w czasie O(n^2)
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where
        insert x [] = [x]
        insert x (y:ys)
            | x <= y    = x : y : ys
            | otherwise = y : insert x ys

-- SelectionSort - sortowanie przez wybieranie - algorytm sortowania, który działa w czasie O(n^2)
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = m : selectionSort (delete m xs)
    where
        m = minimum xs
        delete x [] = []
        delete x (y:ys)
            | x == y    = ys
            | otherwise = y : delete x ys

-- BubbleSort - sortowanie bąbelkowe - algorytm sortowania, który działa w czasie O(n^2)
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init sorted) ++ [last sorted]
    where
        sorted = onePass xs
        onePass [] = []
        onePass [x] = [x]
        onePass (x:y:xs)
            | x <= y    = x : onePass (y:xs)
            | otherwise = y : onePass (x:xs)

-- HeapSort - sortowanie przez kopcowanie - algorytm sortowania, który działa w czasie O(n log n)
heapSort :: Ord a => [a] -> [a]
heapSort xs = reverse (heapSort' (length xs) (xs ++ repeat undefined))
    where
        heapSort' 0 xs = []
        heapSort' n xs = m : heapSort' (n-1) (adjust 1 m (n-1) xs)
            where
                m = xs !! 1
                adjust i m n xs
                    | j > n        = xs
                    | k > n        = swap i j xs
                    | xs !! j <= xs !! k = swap i k xs
                    | otherwise    = swap i j xs
                    where
                        j = 2*i
                        k = 2*i+1
                swap i j xs = take i xs ++ [xs !! j] ++ take (j-i-1) (drop (i+1) xs) ++ [xs !! i] ++ drop (j+1) xs

-- BucketSort - sortowanie przez kosze - algorytm sortowania, który działa w czasie O(n)
bucketSort :: Ord a => [a] -> [a]
bucketSort xs = concat (map sort buckets)
    where
        buckets = map (\x -> filter (<= x) xs) (nub xs)
        sort [] = []
        sort (x:xs) = sort smaller ++ [x] ++ sort larger
            where
                smaller = filter (<= x) xs
                larger  = filter (> x) xs
                
-- CountingSort - sortowanie przez zliczanie - algorytm sortowania, który działa w czasie O(n)
countingSort :: Ord a => [a] -> [a]
countingSort xs = concat (map (\x -> replicate (count x) x) (nub xs))
    where
        count x = length (filter (== x) xs)

-- RadixSort - sortowanie przez cyfry - algorytm sortowania, który działa w czasie O(n)
radixSort :: Ord a => [a] -> [a]
radixSort xs = foldl (flip insert) [] xs
    where
        insert x [] = [x]
        insert x (y:ys)
            | x <= y    = x : y : ys
            | otherwise = y : insert x ys

-- ShellSort - sortowanie Shella - algorytm sortowania, który działa w czasie O(n^2)
shellSort :: Ord a => [a] -> [a]
shellSort xs = foldl (flip insert) xs gaps
    where
        gaps = takeWhile (<= length xs) (iterate (2*) 1)
        insert gap xs = foldl (flip insert') xs [gap..length xs-1]
            where
                insert' i xs
                    | xs !! i < xs !! (i-gap) = take (i-gap) xs ++ [xs !! i] ++ take (i-(i-gap)-1) (drop (i-gap+1) xs) ++ [xs !! (i-gap)] ++ drop (i+1) xs
                    | otherwise               = xs
        findPos x [] = 0
        findPos x (y:ys)
            | x <= y    = 0
            | otherwise = 1 + findPos x ys
        insertAt x 0 xs = x : xs
        insertAt x n (y:ys) = y : insertAt x (n-1) ys

-- Funkcja main
main = do
    let xs = [ 5, 3, 1, 4, 2 ]

    putStrLn "Quick Sort"
    print (quickSort xs)

    putStrLn "Merge Sort"
    print (mergeSort xs)

    putStrLn "Insertion Sort"
    print (insertionSort xs)

    putStrLn "Selection Sort"
    print (selectionSort xs)

    putStrLn "Bubble Sort"
    print (bubbleSort xs)

    putStrLn "Heap Sort"
    -- print (heapSort xs)

    putStrLn "Bucket Sort"
    -- print (bucketSort xs)

    putStrLn "Radix Sort"
    print (radixSort xs)

    putStrLn "Shell Sort"
    -- print (shellSort xs)

    putStrLn "Counting Sort"
    -- print (countingSort xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' = foldr (++) []

isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' [x] = True
isSorted' (x:y:xs) = x <= y && isSorted' (y:xs)
-- isSorted [1,2,2,3] = True

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- reverse [1,2,3] = [3,2,1]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
-- zip' [1,2] [3,4] = [(1,3), (2,4)]

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xs) = (x:xs', y:ys')
    where
        (xs', ys') = unzip' xs
-- unzip [(1,2), (3,4)] = ([1,3], [2,4])

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs
-- zip3 [1,2] [3,4] [5,6] = [(1,3,5), (2,4,6)]

subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList _ [] = False
subList (x:xs) (y:ys)
    | x == y    = subList xs ys
    | otherwise = subList (x:xs) ys
-- subList [1,2] [3,1,2,4] = True

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstDivSec :: Integral a => [a] -> Bool
fstDivSec (x : y : _) | y /= 0 && x `mod` y == 0 = True
fstDivSec _                                     = False

fstDivThird :: Integral a => [a] -> Bool
fstDivThird (x : _ : z : _) | z /= 0 && x `mod` z == 0 = True
fstDivThird _                                          = False

fstDivSecOrThird :: Integral a => [a] -> Bool
fstDivSecOrThird xs = fstDivSec xs || fstDivThird xs


