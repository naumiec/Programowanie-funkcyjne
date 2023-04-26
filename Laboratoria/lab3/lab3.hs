-- \ <- podobno wygląda jak lambda

import Data.List
import Data.Char
import Data.Text.Lazy (foldrChunks, break, pack)


-- 1

f1 :: Integer -> Integer
f1 = \x -> x-2

f2 :: Double -> Double -> Double
f2 = \x y -> sqrt (x^2 + y^2)

f3 :: Double -> Double -> Double -> Double
f3 = \x y z -> sqrt (x^2 + y^2 + z^2)

f4 :: Integer -> Integer
f4 = \x -> 2*x

f5 :: Integer -> Integer
f5 = \x -> x*2

f6 :: Integer -> Integer
f6 = \x -> 2^x

f7 :: Integer -> Integer
f7 = \x -> x^2

f8 :: Double -> Double
f8 = \x -> 2/x

f9 :: Double -> Double
f9 = \x -> x/3

f10 :: Integer -> Integer
f10 = \x -> 4-x

f11 :: Integer -> Bool
f11 = \x -> case x `mod` 2 of
            0 -> True
            _ -> False

f12 :: Double -> Double
f12 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f13 :: Integer -> Integer
f13 = \x -> case x of
            1 -> 3
            _ -> 0

lambda1 :: Integer -> Integer
lambda1 = \x -> x - 2
-- lambda1 5 = 3

lambda2 :: Double -> Double -> Double
lambda2 = \x y -> sqrt (x^2 + y^2)
-- lambda2 3 4 = 5.0

lambda3 :: Double -> Double -> Double -> Double
lambda3 = \x y z -> sqrt (x^2 + y^2 + z^2)
-- lambda3 1 2 2 = 3.0

lambda4 :: Integer -> Integer
lambda4 = \x -> 2*x
-- lambda4 5 = 10

lambda5 :: Integer -> Integer
lambda5 = \x -> x*2
-- lambda5 5 = 10

lambda6 :: Integer -> Integer
lambda6 = \x -> 2^x
-- lambda6 5 = 32

lambda7 :: Integer -> Integer
lambda7 = \x -> x^2
-- lambda7 5 = 25

lambda8 :: Double -> Double
lambda8 = \x -> 2/x
-- lambda8 5 = 0.4

lambda9 :: Double -> Double
lambda9 = \x -> x/3
-- lambda9 5 = 1.67

lambda10 :: Integer -> Integer
lambda10 = \x -> 4-x
-- lambda10 5 = -1

sqrt' :: Double -> Double
sqrt' = \x -> x**(1/2)
-- sqrt' 5 = 2.23606797749979

abs' :: Integer -> Integer
abs' = \x -> case x >= 0 of
        True -> x
        False -> -x
-- abs' 5 = 5

log' :: Double -> Double
log' = \x -> logBase 2 x
-- log' 5 = 2.321928094887362

id' :: a -> a
id' = \x -> x
-- id' 5 = 5

const' :: p -> Integer
const' = \_ -> 42
-- const' 0 = 42

lambda11 :: Integer -> Bool
lambda11 = \x -> case x `mod` 2 of
        0 -> True
        _ -> False
-- lambda11 5 = False

lambda12 :: Double -> Double
lambda12 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)
-- lambda12 5 = 15.0

lambda13 :: Integer -> Integer
lambda13 =      \x -> case x of
                1 -> 3
                _ -> 0
-- lambda13 5 = 0



-- 2

sum' :: Num a => [a] -> a
sum' []         = 0
sum' (x:xs)     = x + sum' xs
-- sum' [1,2,3] = 6

sumSqr' :: Num a => [a] -> a
sumSqr' []      = 0
sumSqr' (x:xs)  = x^2 + sumSqr' xs
-- sumSqr' [1,2,3] = 14

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f []            = 0
sumWith f (x:xs)        = f x + sumWith f xs
-- sumWith lambda7 [1,2,3] = 14

-- count: sum i = 1 to 15: i^5, using only sumWith function
-- ghci> sumWith (\x -> x^5) [1..15] = 2 299 200
-- sum5Pow [1..15] = 2 299 200

sum5Pow :: Num a => [a] -> a
sum5Pow x = sumWith (\x -> x^5) x

cube :: Integer -> Integer
cube = \x -> x^3
-- cube 5 = 125

abs2 :: Integer -> Integer
abs2 = \x -> case x >= 0 of
        True -> x
        False -> -x
-- abs2 5 = 5

sum2 :: Num a => [a] -> a
sum2 x = sumWith (\a -> a) x
-- sum2 [1,2,3] = 6

sumSqr :: [Integer] -> Integer
sumSqr x = sumWith lambda7 x
-- sumSqr [1,2,3] = 14

sumCube :: [Integer] -> Integer
sumCube x = sumWith cube x
-- sumCube [1,2,3] = 36

sumAbs :: [Integer] -> Integer
sumAbs x = sumWith abs2 x
-- sumAbs [1,2,-3] = 6

sumWidth :: Num a => [a] -> a
sumWidth x = sumWith (\x -> 1) x
-- sumWidth [1,2,3,1] = 4

listLength :: Num a => [a] -> a
listLength x = sumWidth x
-- listLength [1,2,3,1] = 4

prod' :: Num a => [a] -> a
prod' []        = 1
prod' (x:xs)    = x * prod' xs
-- prod' [1,2,3] = 6

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f []           = 1
prodWith f (x:xs)       = f x * prodWith f xs
-- prodWith lambda7 [1,2,3] = 36
-- lambda7 = \x -> x^2

prod :: Num a => [a] -> a
prod x = prodWith (\x -> x) x
-- prodSqr [4, 9, 16] = 24

prodSqr :: Floating a => [a] -> a
prodSqr x = prodWith (\x -> x ** 2) x
-- prodSqr [1,2,3] = 36

prodCube :: Floating a => [a] -> a
prodCube x = prodWith (\x -> x ** 3) x
-- prodCube [1,2,3] = 36

prodAbs :: Floating a => [a] -> a
prodAbs x = prodWith (\x -> abs x) x
-- prodAbs [1,2,-3] = 6

sumSqrt :: Floating a => [a] -> a
sumSqrt x = sumWith (\x -> sqrt x) x
-- SumSqrt [4, 9, 16] = 12

prodSqrt :: Floating a => [a] -> a
prodSqrt x = prodWith (\x -> sqrt x) x
-- prodSqrt [4, 9, 16] = 24



-- 3

sqr :: Num a => a -> a
sqr x = x^2
-- sqr 5 = 25

funcFactory :: Integer -> Integer -> Integer
funcFactory n = case n of 
        1 -> id
        2 -> sqr
        3 -> cube
        4 -> \x -> x^4
        5 -> intFunc
        _ -> const n
        where
                intFunc x = x^5
-- funcFactory 5 5 = 3125
-- :t funcFactory :: Integer -> Integer -> Integer
-- :t funcFactory 5 :: Integer -> Integer

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 -> \x -> 1 + x
    1 -> \x -> 1 + x + (x^2)/2
    2 -> \x -> 1 + x + (x^2)/2 + (x^3)/6
    3 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24
    4 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24 + (x^5)/120
    5 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24 + (x^5)/120 + (x^6)/720
-- expApproxUpTo 5 1 = 2.716666666666667
-- expApproxUpTo 5 2 = 7.38905609893065
-- expApproxUpTo 5 3 = 20.085536923187668
-- expApproxUpTo 5 4 = 54.598150033144236
-- expApproxUpTo 5 5 = 148.4131591025766

silnia :: Double -> Double
silnia 0 = 1
silnia n = n * silnia (n - 1)
-- silnia 5 = 120.0

expApproxUpTo2 :: Int -> Double -> Double
expApproxUpTo2 0 = \x -> 1 + x
expApproxUpTo2 n = \x -> (x^(n+1))/(silnia (nDouble + 1)) + (expApproxUpTo2 (n-1)) x where nDouble = fromIntegral n
-- expApproxUpTo2 5 1 = 2.716666666666667

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f x) / h
-- dfr (\x -> x^2) 0.0001 5 = 10.000099999984968

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> ((f (x + h) -  f (x - h)) / (2 * h))
-- dfc (\x -> x^2) 0.0001 5 = 9.999999999976694

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = \x -> ((f (x + h) - 2 * f x + f (x - h)) / (h^2))
-- d2f (\x -> x^2) 0.0001 5 = 2.000000165480742



-- 4

funcList :: [Double -> Double]
funcList = [\x -> (sin x) / x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x]
-- funcList = [(sin x)/x, log x + sqrt x + 1, (exp 1)^x]
-- log = ln
-- sin in radians (not degrees)

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x []     = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs
-- evalFuncListAt 1 funcList = [0.8414709848078965,2.0,2.718281828459045]
-- evalFuncListAt 5 funcList = [-0.1917848549326277,4.84550588993389,148.41315910257657]
-- evalFuncListAt 10 funcList = [-5.4402111088936986e-2,6.464862753162425,22026.465794806703]
-- evalFuncListAt 100 funcList = [-5.063656411097588e-3,15.605170185988092,2.6881171418161212e43]

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)
-- x_t 1 = 6.0
-- y_t 1 = 3.0

funcListExt :: [Double -> Double]
funcListExt = [\x -> (sin x) / x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x, \x -> (1 + x) ** (1/2)]
-- funcListExt = [(sin x)/x, log x + sqrt x + 1, (exp 1)^x, (1 + x)^(1/2)]
-- evalFuncListAt 1  funcListExt = [0.8414709848078965,2.0,2.718281828459045,1.4142135623730951]
-- evalFuncListAt 5  funcListExt = [-0.1917848549326277,4.84550588993389,148.41315910257657,2.449489742783178]
-- evalFuncListAt 10 funcListExt = [-5.4402111088936986e-2,6.464862753162425,22026.465794806703,3.3166247903554]
-- evalFuncListAt 100 funcListExt = [-5.063656411097588e-3,15.605170185988092,2.6881171418161212e43,10.04987562112089]

-- ???
velocEqs :: (Double -> Double) -> Double -> Double
velocEqs f t = (dfc f 0.0001) t

-- ???
accelEqs :: (Double -> Double) -> Double -> Double
accelEqs f t = (d2f f 0.0001) t


-- 5 

-- :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- :i (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c   -- Defined in ‘GHC.Base’
-- infixr 9 .
-- infixr 9 . -- (.) is right-associative
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (f . g) x = f (g x)
-- f . g = \x -> f (g x)
-- 9 to 0, 9 is the highest precedence

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs
-- sortDesc [1,2,3] = [3,2,1]

sortDesc2 :: Ord a => [a] -> [a]
sortDesc2 xs = reverse (sort xs)
-- sortDesc2 [1,2,3] = [3,2,1]

funF :: Integer -> Integer
funF = (+1)

funG :: Integer -> Integer
funG = (*2)

funH :: Integer -> Integer
funH = (^3)

--w3 :: Double -> Double -> Double -> Double
w3 :: Double -> Double -> Double -> Double
w3 = \x y z -> sqrt (x^2 + y^2 + z^2)
-- w3 1 2 2 = 3.0

-- ???
--func = (funF . w3 . funG 4 . funH) 3

--are2FuncsEqAt :: Eq a => (t -> a) -> [t] -> Bool
are2FunsEqAt :: Eq b => (a -> b) -> (a -> b) -> [a] -> Bool
are2FunsEqAt f g xs = map f xs == map g xs
-- are2FunsEqAt (\x -> x^2) (\x -> x^2) [1,2,3] = True
-- are2FunsEqAt (\x -> x^2) (\x -> x * x) [1,2,3,4] = True
-- are2FunsEqAt (+2) (\x -> x + 2) [1,2,3,4] = True

infixl 9 >.> -- (>.>) is left-associative
(>.>) :: (a -> b) -> (b -> c) -> a -> c
g >.> f = \x -> f (g x)
-- funF = (+1)
-- funG = (*2)
-- funH = (^3)
-- (funF >.> funG) 3 = 8
-- (funF >.> funG >.> funH) 3 = 512
-- (funF >.> funG >.> funH) 3 = (funF . funG . funH) 3
-- (funF >.> funG >.> funH) 3 = funH (funG (funF 3))

--composeFunList :: [a -> a] -> (a -> a)
composeFunList :: [b -> b] -> b -> b
composeFunList [] = id
composeFunList (f:fs) = f >.> composeFunList fs
-- composeFunList [funF, funG] 3 = 8
-- composeFunList [funF, funG= 3 = (funF . funG) 3
-- composeFunList [funF, funG, funH] 3 = 512
-- composeFunList [funF, funG, funH] 3 = (funF . funG . funH) 3

funF1 :: Num a => a -> a
funF1 x = x + 1

funF2 :: Num a => a -> a -> a
funF2 y z = y * z

funF3 :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
funF3 = (.).(.)

-- ???

-- (.).(.)
-- This is Scaramanga. 
-- He takes a one-parameter function on the left and 
-- a two-parameter function on the right, 
-- and yields a new two-parameter function that passes the result of the right-hand function to the left-hand one. 
-- In other words, he's like (\g f x y -> g (f x y)).
-- :t (.).(.)
-- (.).(.) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- Unfortunately you can't just write him infix between two terms; 
-- instead you have to use a definition like let o2 = (.).(.) and then write g `o2` f.


-- 6

-- :t ($)
-- ($) :: (a -> b) -> a -> b
-- :i ($)
-- infixr 0 $
-- :i (.)
-- infixr 9 .
-- :t ($!)
-- ($!) :: (a -> b) -> a -> b
-- :i ($!)
-- infixr 0 $!

-- (($) funF 3) == (funF $ 3)
-- True

-- funF (funG (funH (3)))
-- 55

--  funF 3 == _3 funF
-- True

-- ((,) $ 1) 2
-- (1,2)

-- (((,) $ 1) $ 2)
-- (1,2)

-- funF $ funG $ funH 10
-- 2001
-- (funF . funG . funH) 10
-- 2001
-- funF . funG . funH $ 10
-- 2001
-- równoważne
-- funF (funG (funH 10))
-- 2001

-- funF . $ funG . funH 3
-- add brackets
-- funF . ($) (funG . funH) 3 -- ???


-- 7

onlyEven :: Integral a => [a] -> [a]
onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x : onlyEven xs
    | otherwise      = onlyEven xs
-- onlyEven [1,2,3,4,5] = [2,4]
-- onlyEven [1..100] = [2,4..100] (0.10 secs, 246,896 bytes)

onlyOdd :: Integral a => [a] -> [a]
onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise      = onlyOdd xs
-- onlyOdd [1,2,3,4,5] = [1,3,5]
-- onlyOdd [1..100] = [1,3..99] (0.12 secs, 245,280 bytes)

onlyUpper :: [Char] -> [Char]
onlyUpper [] = []
onlyUpper (x:xs)
    | x `elem` ['A'..'Z'] = x : onlyUpper xs
    | otherwise           = onlyUpper xs
-- onlyUpper "Haskell" = "H"
-- onlyUpper "haskell" = ""
-- onlyUpper "HASKELL" = "HASKELL"
-- onlyUpper "My name is Bond, James Bond" = "MBJB"
-- onlyUpper "My name is Inigo Montoya. You killed my father. Prepare to die." = "MIMYP" (0.10 secs, 29,536 bytes)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
-- filter' (\x -> x `mod` 2 == 0) [1,2,3,4,5] = [2,4]

onlyEven' :: [Integer] -> [Integer]
onlyEven' = filter' (\x -> x `mod` 2 == 0)
-- onlyEven' [1..10] = [2,4,6,8,10]
-- onlyEven' [1..100] = [2,4..100] (0.12 secs, 214,528 bytes)

onlyOdd' :: [Integer] -> [Integer]
onlyOdd' = filter' (\x -> x `mod` 2 == 1)
-- onlyOdd' [1..10] = [1,3,5,7,9]
-- onlyOdd' [1..100] = [1,3..99] (0.01 secs, 212,920 bytes)
-- dlaczego taka różnica w czasie?

onlyUpper' :: [Char] -> [Char]
onlyUpper' = filter' (\x -> let ascii = fromEnum x in 
    case ascii >= 65 && ascii <= 90 of
        True -> True
        False -> False)
-- onlyUpper' "Haskell" = "H"
-- onlyUpper' "haskell" = ""
-- onlyUpper' "HASKELL" = "HASKELL"
-- onlyUpper' "My name is Bond, James Bond" = "MBJB"
-- onlyUpper' "My name is Inigo Montoya. You killed my father. Prepare to die." = "MIMYP" (0.00 secs, 98,240 bytes)
-- szybciej niż onlyUpper

-- length (onlyEven [1..10^6])
-- 500000
-- (1.87 secs, 616,069,368 bytes)

-- length (onlyOdd [1..10^6])
-- 500000
-- (0.17 secs, 156,069,232 bytes)

--biblioteczna funkcja duzo szybsza

-- length . onlyEven $ [1..10^6]
-- length . onlyOdd $ [1..10^6]
-- length . filter even $ [1..10^6]
-- length . filter odd $ [1..10^6]

-- [ i | i <- [1..10^6]]
-- [ i | i <- [1..10^6], i `mod` 2 == 0]
-- [ i | i <- [1..10^6], i `mod` 2 == 1]

-- filter (\s -> length s == 2) ["a", "aa", "aaa", "b", "bb"]
-- ["aa","bb"]
-- (0.12 secs, 74,120 bytes)

-- filter (\(x,y) -> x > y) [(1,2), (2,2), (2,1), (2,2), (3,2)]
-- [(2,1),(3,2)]
-- (0.01 secs, 75,440 bytes)

-- filter (\xs -> sum xs > 300) [[1..5], [56..60], [100..105]]
-- [[100,101,102,103,104,105]]
-- (0.01 secs, 88,256 bytes)

-- length . filter (\f -> f 2 > 10) $ [(+5), (*5), (^5), \x -> 3 * x + 7]
-- 2
-- (0.02 secs, 66,120 bytes)


-- 8

doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = x*2 : doubleElems xs
-- doubleElems [1,2,3] = [2,4,6]

sqrElems :: Num a => [a] -> [a]
sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs
-- sqrElems [1,2,3] = [1,4,9]

lowerCase :: [Char] -> [Char]
lowerCase [] = []
lowerCase (x:xs) = let ascii = fromEnum x in 
    case ascii >= 65 && ascii <= 90 of
        True -> toEnum (ascii + 32) : lowerCase xs
        False -> x : lowerCase xs
-- lowerCase "AlaMaKota" = "alamakota"

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map' (\x -> x^2) [1,2,3] = [1,4,9]

doubleElems' :: Num a => [a] -> [a]
doubleElems' = map' (\x -> x*2)
-- doubleElems [1,2,3] = [2,4,6]

sqrElems' :: Num a => [a] -> [a]
sqrElems'    = map' (\x -> x^2)
-- sqrElems' [1,2,3] = [1,4,9]

lowerCase' :: [Char] -> [Char]
lowerCase'   = map' (\x -> let ascii = fromEnum x in 
    case ascii >= 65 && ascii <= 90 of
        True -> toEnum (ascii + 32)
        False -> x
    )
-- lowerCase' "AlaMaKota" = "alamakota"

doubleElems'' :: Num a => [a] -> [a]
doubleElems'' xs = [i*2 | i <- xs]
-- doubleElems' [1,2,3] = [2,4,6]

sqrElems'' :: Num a => [a] -> [a]
sqrElems'' xs = [i^2 | i <- xs]
-- sqrElems'' [1,2,3] = [1,4,9]

lowerCase'' :: [Char] -> [Char]
lowerCase'' xs = [let ascii = fromEnum a in if ascii >= 65 && ascii <= 90 then toEnum (ascii + 32) else a | a <- xs]
-- lowerCase'' "AlaMaKota" = "alamakota"

-- length . filter even $ doubleElems [1..10^7]
-- 10000000
-- (7.60 secs, 4,320,072,072 bytes)

-- length . filter even . map (*2) $ [1..10^7]
-- z biblioteki standardowej
-- 10000000
-- (2.08 secs, 2,880,071,304 bytes)
-- biblioteczna funkcja map jest szybsza

-- length . filter even $ doubleElems $ [i | i <- [1..10^7]]
-- 10000000
-- (10.48 secs, 5,040,071,264 bytes)

-- length . filter even . map (*2) $ [i | i <- [1..10^7]]
-- 10000000
-- (6.17 secs, 3,600,071,384 bytes)

-- map (*2) [1..10]
-- [2,4,6,8,10,12,14,16,18,20]

-- map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

-- map toLower "ABCD" 
-- "abcd"

-- length show [1..10]
-- 10

-- map show [1..10]
-- ["1","2","3","4","5","6","7","8","9","10"]

-- map length [[1], [1,2], [1,2,3]]
-- [1,2,3]

-- map length [ [[1],[1,2],[1,2,3]], [[1],[1,2]] ]
-- [3,2]

-- map (map length) [ [[1],[1,2],[1,2,3]], [[1],[1,2]] ]
-- [[1,2,3],[1,2]]

-- map (\(x,y) -> (y,x)) [(1,'a'), (2,'b'), (3,'c')]
-- [('a',1),('b',2),('c',3)]

-- map (\(x,y) -> y) [(1,'a'), (2,'b'), (3,'c')]
-- "abc"

-- map (\s -> (s,length s)) ["manuscript", "do", "not", "burn"]
-- [("manuscript",10),("do",2),("not",3),("burn",4)]

evalFuncListAt' :: a -> [a -> b] -> [b]
evalFuncListAt' x = map (\f -> f x)
-- evalFuncListAt' 2 [(*2), (+1), (^2)] = [4,3,4]

-- map (\x -> x^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

-- map (\x -> x^2) [i | i <- [1..10]]
-- [1,4,9,16,25,36,49,64,81,100]

-- map f . filter p $ xs
-- filter p . map f $ xs


-- 9

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' g [] = 0
sumWith' g (x:xs) = g x + sumWith' g xs
-- (+) (g x) (sumWith' g xs)

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' g [] = 1
prodWith' g (x:xs) = g x * prodWith' g xs
-- (*) (g x) (prodWith' g xs)

sumWith'' :: Num a => (a -> a) -> [a] -> a
sumWith'' = go 0
    where
        go acc g [] = acc
        go acc g (x:xs) = go (g x + acc) g xs
-- sumWith'' (\x -> x^2) [1,2,3] = 14

prodWith'' :: Num a => (a -> a) -> [a] -> a
prodWith'' = go 1
    where
        go acc g [] = acc
        go acc g (x:xs) = go (g x * acc) g xs
-- prodWith'' (\x -> x^2) [1,2,3] = 36

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)
-- foldr' (\x acc -> x + acc) 0 [1,2,3] = 6

sumWith''' :: Num a => (t -> a) -> [t] -> a
sumWith''' g = foldr' (\x acc -> g x + acc) 0
-- sumWith''' (\x -> x^2) [1,2,3] = 14

prodWith''' :: Num a => (t -> a) -> [t] -> a
prodWith''' g = foldr' (\x acc -> g x * acc) 1
-- prodWith''' (\x -> x^2) [1,2,3] = 36

-- foldl' is defined in Data.List
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f z [] = z
foldl'' f z (x:xs) = foldl'' f (f z x) xs
-- foldl'' (\acc x -> x + acc) 0 [1,2,3] = 6

sumWith'''' :: Num a => (t -> a) -> [t] -> a
sumWith'''' g = foldl'' (\acc x -> g x + acc) 0
-- sumWith'''' (\x -> x^2) [1,2,3] = 14

prodWith'''' :: Num a => (t -> a) -> [t] -> a
prodWith'''' g = foldl'' (\acc x -> g x * acc) 1
-- prodWith'''' (\x -> x^2) [1,2,3] = 36

-- foldr' (+) 0 [1..10^6]
-- 500000500000
-- (0.29 secs, 161,591,240 bytes)

-- foldr (+) 0 [1..10^6]
-- 500000500000
-- (0.23 secs, 161,591,152 bytes)

-- foldr (\x acc -> x + 1 + acc) 0 [1..10^6]
-- 500001500000
-- (0.82 secs, 282,115,432 bytes)

-- foldl (\acc x -> acc + 1 + x) 0 [1..10^6]
-- 500001500000
-- (1.10 secs, 315,346,768 bytes)

-- foldl' (\acc x -> acc + 1 + x) 0 [1..10^6]
-- 500001500000
-- (0.39 secs, 208,074,064 bytes)

-- sum . map (+1) $ [1..10^6]
-- 500001500000
-- (0.24 secs, 192,074,384 bytes)

-- sum [x + 1 | x <- [1..10^6]]
-- 500001500000
-- (0.76 secs, 280,074,128 bytes)

-- let strList1 = ["My", "name", "is", "Inigo", "Montoya"]
-- (0.24 secs, 29,792 bytes)

-- foldr (++) [] strList1
-- "MynameisInigoMontoya"
-- (0.14 secs, 84,088 bytes)

-- foldr (\x acc -> x ++ " " ++ acc) [] strList1
-- "My name is Inigo Montoya "
-- (0.09 secs, 87,872 bytes)

-- foldr1 (\x acc -> x ++ " " ++ acc) strList1
-- "My name is Inigo Montoya"
-- (0.10 secs, 86,432 bytes)

-- let list1To5 = [1..5]
-- (0.09 secs, 29,512 bytes)

-- foldr (\_ acc -> 1 + acc) 0 list1To5
-- 5
-- (0.03 secs, 65,408 bytes)

-- foldr (:) [] list1To5
-- [1,2,3,4,5]
-- (0.01 secs, 73,528 bytes)

-- ???
-- foldl (:) [] list1To5
-- 

-- foldl (\acc x -> x : acc) [] list1To5
-- [5,4,3,2,1]
-- (0.54 secs, 73,544 bytes)

-- foldr (\x xs -> xs ++ [x]) [] list1To5
-- [5,4,3,2,1]
-- (0.03 secs, 74,168 bytes)

-- let listRand = [1,4,2,6,5,3]
-- foldr1 max listRand
-- foldl1 max listRand

-- let listBool = [True, False, True, False]
-- (0.11 secs, 29,672 bytes)

-- foldr (||) False listBool
-- True
-- (0.12 secs, 34,080 bytes)

-- foldr (&&) True listBool
-- False
-- (0.00 secs, 34,936 bytes)

-- foldr (+) 0 list1To5 == foldl (+) 0 list1To5
-- True
-- (0.12 secs, 68,184 bytes)

-- foldr (*) 1 list1To5 == foldl (*) 1 list1To5
-- True
-- (0.23 secs, 68,144 bytes)

-- foldr (-) 0 list1To5 == foldl (-) 0 list1To5
-- False
-- (0.09 secs, 68,952 bytes)

-- let list321 = [3,2,1]
-- (0.12 secs, 29,520 bytes)

-- foldr (-) 0 list321
-- 2
-- (0.00 secs, 64,840 bytes)

-- foldr1 (-) list321
-- 2
-- (0.15 secs, 64,744 bytes)

-- foldl (-) 0 list321
-- -6
-- (0.11 secs, 65,672 bytes)

-- foldl1 (-) list321
-- 0
-- (0.09 secs, 64,744 bytes)

-- foldl (\acc x -> "(" ++ acc ++ " f " ++ x ++ ")") "z " ["1", "2", "3"]
-- "(((z  f 1) f 2) f 3)"
-- (0.15 secs, 85,984 bytes)

-- foldl1 (\acc x -> "(" ++ acc ++ " f " ++ x ++ ")") ["1", "2", "3"]
-- "((1 f 2) f 3)"
-- (0.00 secs, 78,352 bytes)


-- 10

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl1 (&&) (zipWith (<=) xs (tail xs))
-- isSortedAsc [1,2,3] = True
-- (0.11 secs, 67,104 bytes)

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = foldl1 (&&) (zipWith (>=) xs (tail xs))
-- isSortedDesc [3,2,1] = True
-- (0.10 secs, 67,104 bytes)

isSorted :: Ord a => [a] -> Bool
isSorted xs = isSortedAsc xs || isSortedDesc xs
-- isSorted [1,2,3] = True
-- (0.12 secs, 68,232 bytes)

everySecond :: [t] -> [t]
everySecond [] = []
everySecond [x] = [x]
everySecond (x1:x2:xs) = x1 : everySecond xs
-- everySecond [1,2,3,4,5] = [1,3,5]
-- (0.02 secs, 71,064 bytes)

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs = zipWith3 (\x y z -> (x,y,z)) xs ys zs
-- zip3' [1,2,3] [4,5,6] [7,8,9] = [(1,4,7),(2,5,8),(3,6,9)]
-- (0.12 secs, 85,968 bytes)

unzip3' :: [(a,b,c)] -> ([a],[b],[c])
unzip3' xs = (map (\(x,_,_) -> x) xs, map (\(_,y,_) -> y) xs, map (\(_,_,z) -> z) xs)
-- unzip3' [(1,4,7),(2,5,8),(3,6,9)] = ([1,2,3],[4,5,6],[7,8,9])
-- (0.01 secs, 86,696 bytes)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
-- take 10 fibs = [0,1,1,2,3,5,8,13,21,34]
-- [0,1,1,2,3,5,8,13,21,34]
-- (0.13 secs, 86,256 bytes)

ones :: [Integer]
ones = 1 : ones
-- take 10 ones = [1,1,1,1,1,1,1,1,1,1]
-- (0.11 secs, 82,632 bytes)

nats :: [Integer]
nats = 0 : map (+1) nats
-- take 10 nats = [0,1,2,3,4,5,6,7,8,9]
-- (0.11 secs, 82,568 bytes)


-- 11

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- concat' ["abc", "def"]
-- "abcdef"
-- (0.12 secs, 72,272 bytes)

-- concat' [[1,2],[3,4]]
-- [1,2,3,4]
-- (0.11 secs, 71,872 bytes)

-- (concat' . concat') [ [[1,2], [3,4]], [[5,6], [7,8]] ]
-- [1,2,3,4,5,6,7,8]
-- (0.10 secs, 81,000 bytes)

-- concat' ["abc", "def"] == concat ["abc", "def"]
-- True
-- (0.55 secs, 68,632 bytes)

-- :t concatMap
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

-- :t map
-- map :: (a -> b) -> [a] -> [b]

-- :t concat
-- concat :: Foldable t => t [a] -> [a]

-- using list comprehension
concat'' :: [[a]] -> [a]
concat'' xs = [x | xss <- xs, x <- xss]
-- concat'' ["abc", "def"]
-- "abcdef"
-- (0.11 secs, 71,280 bytes)

concat''' :: [[a]] -> [a]
concat''' = foldr (++) []
-- concat''' ["abc", "def"]
-- "abcdef"
-- (0.11 secs, 71,056 bytes)

-- concat ___ map (___) ___ [1..5] -- [2,4,6,8,10]  
-- concatMap (___) [1..5] -- [2,4,6,8,10]
-- concatMap (___) ["Ready", "Steady", "Go"] -- "Ready!Steady!Go!"


-- 12

-- import Data.Char
-- import Data.List

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)
-- cpaitalize "haskell" = "Haskell"
-- capitalize "HASKELL" = "Haskell"

formatStr :: String -> [Char]
formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
    map capitalize .
    filter (\x -> length x > 1) $
    words s
-- formatStr "haskell     is a functional programming language" = "Haskell Is A Functional Programming Language"

prodPrices :: Num a => String -> a
prodPrices p = case p of
    "A" -> 100
    "B" -> 500
    "C" -> 1000
    _ -> error "Unknown product"
-- products = ["A", "B", "C"]

-- basic discount strategy
discStr1 :: (Ord a, Fractional a) => String -> a
discStr1 p
    | price > 999   = 0.3 * price
    | otherwise     = 0.1 * price
    where price     = prodPrices p

-- float discount strategy
discStr2 :: (Ord a, Fractional a) => String -> a
discStr2 p = 0.2 * prodPrices p

totalDiscount :: Num c => (String -> c) -> [String] -> c
totalDiscount discStr = foldl1 (+) .
    map discStr .
    filter (\p -> prodPrices p > 999)

-- totalDiscount discStr1 ["A", "B", "C"]
-- 300.0
-- (0.18 secs, 73,056 bytes)

-- totalDiscount discStr2 ["A", "B", "C"]
-- 200.0
-- (0.14 secs, 71,480 bytes)

-- replicate 2 . product . map (*3) $ zipWith max [4,2] [1,5]
-- [180,180]
-- (0.16 secs, 72,480 bytes)

-- sum . takeWhile (<1000) . filter odd . map (^2) $ [1..]
-- 5456
-- (0.18 secs, 84,600 bytes)

-- length . Data.Set.fromList . Prelude.map toLower $ "thirteen men must go" -- import Data.Set
-- 12
-- (0.11 secs, 71,528 bytes)


