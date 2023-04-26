-- lab5.hs 

{-
ghci> :i IO
type IO :: * -> *
newtype IO a
  = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
                  -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
        -- Defined in ‘GHC.Types’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance MonadFail IO -- Defined in ‘Control.Monad.Fail’
instance Monad IO -- Defined in ‘GHC.Base’
-}

{-
ghci> :t getChar
getChar :: IO Char
-}

{-
ghci> :t putChar
putChar :: Char -> IO ()
-}

{-
ghci> :t putChar 'a'
putChar 'a' :: IO ()
-}




