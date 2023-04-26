-- kol6

-- import guard
import Control.Monad

-- 1

triples n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], (a^2 + b^2) `mod` c^2 == 1]

triples' n = do
    let vals = [1..n]
    a <- vals
    b <- vals
    c <- vals 
    --guard $ (a^2 + b^2) `mod` c^2 == 1
    guard $ a^2 + b^2 == c^2
    return (a, b, c)

-- 2

-- :t
-- (>>) Monad m => m a -> m b -> m b
-- return Monad m => a -> m a
-- (>=>) Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- return "Ala" "Ala" :: Monad m => m String

-- 3

tryFactorial :: Int -> Maybe Int
tryFactorial 0 = Just 1
tryFactorial n = 
    if n < 0 then Nothing
    else do
        prev <- tryFactorial $ n - 1
        return $ n * prev

-- 4
-- to samo co w 2

-- 5
-- to samo co w 1

-- 6

sumLog :: [Double] -> Maybe Double
sumLog [] = Just 0
sumLog (x:xs) = 
    if x <= 0 then Nothing
    else do
        s <- sumLog xs
        return $ s + log x
-- sumLog [1, 2, 3, 4, 5] == Just 4.787491742782046

