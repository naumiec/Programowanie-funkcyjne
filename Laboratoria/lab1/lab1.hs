import Data.Bits (Bits(xor))
printHello = putStrLn "Hello, World!"
main = printHello

sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x / len, y / len)
    where len = sqrt(x * x + y ^ 2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x / len, y / len, z / len)
    where len = sqrt(x ^ 2 + y ^ 2 + z ^ 2)

swap :: (Int, Char) -> (Char, Int)
swap (x, y) = (y, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && y == z

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = 
    let {d = sqrt (b * b - 4 * a * c); e = 2 * a}
    in ( (-b - d) / e, (-b + d) / e )
{-
roots (a, b, c) = 
    let d = sqrt (b * b - 4 * a * c)
        e = 2 * a
    in ( (-b - d) / e, (-b + d) / e )
-}
{-
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
    where   {d = sqrt (b * b - 4 * a * c); e = 2 * a}
-}
{-
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
    where   d = sqrt (b * b - 4 * a * c)
            e = 2 * a 
-}

heron :: (Double, Double, Double) -> Double
heron (a, b, c) = 
    let p = (a + b + c) / 2
    in sqrt(p * (p - a) * (p - b) * (p - c))
{-
heron (a, b, c) = sqrt(p * (p - a) * (p - b) * (p - c))
    where p = (a + b + c) / 2
-}
{-
heron (a, b, c) = (sqrt((a+b+c)*(a+b-c)*(a-b+c)*(b+c-a)))/4
-}

sgn :: Int -> Int
sgn n   | n > 0 = 1
        | n < 0 = -1
        | otherwise = 0
{-
sgn n = if n > 0
    then 1
    else if n < 0
        then -1
        else 0
-}

absInt :: Int -> Int
absInt n = 
    case (n >= 0) of
        True -> n
        _ -> -n -- false
{-
absInt n | n >= 0 = n
         | otherwise = -n
-}
{-
absInt n | n >= 0 = n
         | n < 0 = -n
-}
{-
absInt n = if n >= 0
    then n
    else -n
-}

min2Int :: (Int, Int) -> Int
min2Int (x, y) | x <= y = x
               | otherwise = y
{-
min2Int (x, y) = if x < y
    then x
    else y
-}

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) | x <= y && x <= z = x
                  | y <= x && y <= z = y
                  | otherwise = z
{-
min3Int (x, y, z) = min2Int (x, min2Int (y, z))
-}
{-
min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = if x < y
    then if x < z 
        then x
        else z
    else if y < z
        then y
        else z
-}

toUpper :: Char -> Char
toUpper c   | c >= 'a' && c <= 'z' = toEnum (fromEnum c + fromEnum 'A' - fromEnum 'a')
            | otherwise = c
{-
toUpper c = if c >= 'a' && c <= 'z'
    then toEnum (fromEnum c + fromEnum 'A' - fromEnum 'a')
    else c
-}

toLower :: Char -> Char
toLower c   | c >= 'A' && c <= 'Z' = toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
            | otherwise = c
{-
toLower c = if c >= 'A' && c <= 'Z'
    then toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
    else c
-}

isDigit :: Char -> Bool
isDigit x | ascii >= 48 && ascii <= 57 = True
          | otherwise = False
    where ascii = fromEnum x
{-
isDigit c   | c >= '0' && c <= '9' = True
            | otherwise = False
-}
{-
isDigit c = if c >= '0' && c <= '9'
    then True
    else False
-}

charToNum :: Char -> Int
charToNum c | isDigit c = fromEnum c - fromEnum '0'
            | otherwise = -1
{-
charToNum c = if isDigit c
    then fromEnum c - fromEnum '0'
    else -1
-}

romanDigit :: Char -> String
romanDigit c    | isDigit c =
                    case charToNum c of 
                        0 -> ""
                        1 -> "I"
                        2 -> "II"
                        3 -> "III"
                        4 -> "IV"
                        5 -> "V"
                        6 -> "VI"
                        7 -> "VII"
                        8 -> "VIII"
                        9 -> "IX"
                | otherwise = ""
{-
romanDigit c = if isDigit c
    then case charToNum c of
        0 -> ""
        1 -> "I"
        2 -> "II"
        3 -> "III"
        4 -> "IV"
        5 -> "V"
        6 -> "VI"
        7 -> "VII"
        8 -> "VIII"
        9 -> "IX"
    else ""
-}

isItTheAnswer :: String -> Bool
isItTheAnswer s = case s of
    "Love" -> True
    _ -> False
{-
isItTheAnswer s = case (s == "Love") of
    True -> True
    _ -> False
-}
{-
isItTheAnswer "Love" = True
isItTheAnswer _ = False
-}

or' :: (Bool, Bool) -> Bool
or' (x, y) = case (x, y) of
    (True, _) -> True
    (_, True) -> True
    _ -> False
{-
or' (x, y)  | x == True = True
            | y == True = True
            | otherwise = False
-}

and' :: (Bool, Bool) -> Bool
and' (x, y) = case (x, y) of
    (True, True) -> True
    _ -> False
{-
and' (x, y) | x == True && y == True = True
            | otherwise = False
-}
{-
and' :: (Bool, Bool) -> Bool
and' (x, y) | x == True = y
            | otherwise = False
-}

nand' :: (Bool, Bool) -> Bool
nand' (x, y) = case (x, y) of
    (True, True) -> False
    _ -> True
{-
nand' (x, y) = case and'(x, y) of
    True -> False
    False -> True
-}
{-
nand' (x, y)    | x == True && y == True = False
                | otherwise = True
-}

xor' :: (Bool, Bool) -> Bool
xor' (x, y) = case (x /= y) of
    True -> True
    False -> False
{-
xor' (x, y) = case (x, y) of
    (True, False) -> True
    (False, True) -> True
    _ -> False
-}
{-
xor' (x, y) | x /= y = True
            | otherwise = False
-}

impl' :: (Bool, Bool) -> Bool
impl' (x, y) = case (x == True && y == False) of
    True -> False
    False -> True
{-
impl' (x, y)    | x == True && y == False = False
                | otherwise = True
-}

equ' :: (Bool, Bool) -> Bool
equ' (x, y) = case (x == y) of
    True -> True
    False -> False
{-
equ' (x, y) | x == y = True
            | otherwise = False
-}

not' :: (Bool) -> Bool
not' b = case b of 
    True -> False
    False -> True




fun1 = zip [1..] (reverse (take 3 (2 : [2..])))

sumSquares :: Num a => [a] -> a
sumSquares = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (x^2 + acc) xs

fun2 = reverse (take 5 (0 : [] ++ [2..])) !! 3

fun3 = [a-b | a <- [1..5], b <- [1..a-1], even(a+b)]

fun4 = [a*b | a <- [1..8], b <- [1..a], odd(a-b)]

fun5 = [(a,b) | a <- [1..8], b <- [1..a], odd(a-b)]

newBeer :: EmptBeer a => a -> Beer
newBeer e = Beer()
