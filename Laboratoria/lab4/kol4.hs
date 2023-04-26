

-- 1
{-
data Data = Data { first :: String, second :: String }
-}

-- 2 ???

{-
data Tree a = Node (Tree a) a (Tree a)
            | Leaf a

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node left x right) = toList left ++ [x] ++ toList right
-- toList (Node (Leaf 1) 2 (Leaf 3))
-- toList (Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5)))
-- toList (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7)))
-- toList (Node (Node (Leaf 7) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Node (Leaf 33) 8 (Leaf 9))))
-}

-- do wyświetlania
--instance Show a => Show (Tree a) where
  --  show (Node left x right) = show left ++ " " ++ show x ++ " " ++ show right



-- 3
{-
data Tree a = Node a (Tree a) (Tree a)
            | Leaf 

paths :: Tree a -> [[a]]
paths Leaf = pure []
paths (Node a left right) = concat $ ([(a:)] <*>) <$> (fmap paths [left, right])
-- test: paths (Node 1 (Node 2 (Leaf) (Leaf)) (Node 3 (Leaf) (Leaf)))

--paths :: Tree a -> [[a]]
--paths Leaf = pure []
--paths (Node x left right) = (x:) <$> (paths left ++ paths right)

-}
-- 4

data Foo a = MkFoo {value :: a, name :: String}
-- ghci> let o = MkFoo 11 "22"
-- ghci> oT
-- Foo "22" with 11 <-output

instance Show a => Show (Foo a) where
    show (MkFoo {value = v, name = n}) = "Foo " ++ show n ++ " with " ++ show v


-- 5

newtype Box a = MkBox {valueInside :: a}

instance Show a => Show (Box a) where
    show (MkBox {valueInside = v}) = "Box with " ++ show v


-- 6


-- data Tree a = Node (Tree a) a (Tree a)
--            | Leaf

--sumSq :: Num a => Tree a -> a
--sumSq Leaf = 0
--sumSq (Node left x right) = x^2 + sumSq left + sumSq right

-- z kol5 

data Tree a = Node a (Tree a) (Tree a)
            | Leaf

pathsSum :: Num a => Tree a -> [a]
pathsSum Leaf = pure 0
pathsSum (Node a lt rt) = concat $ ([(a + )] <*>) <$> (fmap pathsSum [lt, rt])

-- pathsSum (Node 1 (Node 2 (Leaf) (Leaf)) (Node 3 (Leaf) (Leaf)))
-- pathsSum (Node 1 (Leaf) (Leaf))
