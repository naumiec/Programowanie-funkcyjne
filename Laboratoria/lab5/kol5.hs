-- kol5

-- load getLine, putStrLn
import System.IO () 

-- 1

{-
data Tree a = Node a (Tree a) (Tree a)
    | Leaf

pathSum :: Num a => Tree a -> [a]
pathSum Leaf = pure 0 
pathSum (Node a lt rt) = concat $ ([(a +)] <*>) <$> (fmap pathSum [lt, rt])
-- podwójna tablica
-- pathSum (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) == [3,4], ale jest [3,3,4,4]
-}

-- 2
--main = getLine >>= \s -> return 3 >>= \n -> putStrLn $ s ++ show n


{-}
main :: IO ()
main = do 
    s <- getLine
    n <- return 3
    putStrLn $ s ++ show n
-- test
-- >>
-}

-- 3

{-
data Tree a = Node a (Tree a) (Tree a)
    | Leaf

paths :: Tree a -> [[a]]
paths Leaf = pure []
paths (Node a lt rt) = concat $ ([(a:)] <*>) <$> (fmap paths [lt, rt])
-- test
-- paths (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) == [[1,2],[1,3]], ale jest [[1,2],[1,2],[1,3],[1,3]]
-}

-- 4

--main :: IO ()
{-
main = do 
    s <- getLine
    n <- return 3
    putStrLn $ show n ++ s
-}

main = getLine >>= \n -> return 3 >>= \s -> putStrLn $ show n ++ s
