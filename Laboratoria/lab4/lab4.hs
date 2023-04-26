import Data.ByteString (StrictByteString)
import Prelude hiding (Show)

-- lab4.hs


-- 0

-- type t1 = Int
-- type T1 = Int
-- type T2 = (T1, Double)
-- type T3 = (a, a)
-- type T4 a = (a, a, a)
-- type T5 a = [a] 
-- type T6 a b = [([a], [b])]
-- type T7 a b = a -> b
-- type T8 a b = a -> (a -> b) -> Int
-- type T9 a = (a, T9 a)
-- type T10 = (Int, T10)


-- 1

polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)
-- polarToCartesian (1, pi/4) = (0.7071067811865475,0.7071067811865475)
-- polarToCartesian (1, pi/2) = (6.123233995736766e-17,1.0)

-- :t polarToCartesian
-- polarToCartesian :: Floating a => (a, a) -> (a, a)

-- let (x1, y1) = polarToCartesian (1, pi/4)
-- let (x2, y2) = polarToCartesian (x1, y1)

-- polarToCartesian . polarToCartesian $ (1, pi/4)
-- (0.5375741099526127,0.4593626849327842)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)
-- polarToCartesian' (1, pi/4) = (0.7071067811865475,0.7071067811865475)
-- polarToCartesian' (1, pi/2) = (6.123233995736766e-17,1.0)
-- polarToCartesian' (1, pi) = (-1.0,1.2246467991473532e-16)

-- :t polarToCartesian'
-- polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a

-- let (x1, y1) = polarToCartesian' (1, pi/4)
-- let (x2, y2) = polarToCartesian' (x1, y1)
-- polarToCartesian' . polarToCartesian' $ (1, pi/4)

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a)
newtype PolarCoord'' a = MkPolarCoord'' (a, a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

-- w ghci:
a1 = MkPolarCoord'' (1, 1)
a2 = polarToCartesian'' a1

b1 = MkPolarCoord'' (-1, -1)
b2 = polarToCartesian'' b1 
-- nie ma instancji show dla tego typu

-- cylindrical coordinates
cylindrical :: Floating a => (a, a) -> (a, a, a)
cylindrical (r, phi) = (r * cos phi, r * sin phi, r)
-- cylindrical (1, pi/4) = (0.7071067811865475,0.7071067811865475,1.0)

-- spherical coordinates
spherical :: Floating a => (a, a) -> (a, a, a)
spherical (r, phi) = (r * cos phi, r * sin phi, r)
-- spherical (1, pi/4) = (0.7071067811865475,0.7071067811865475,1.0)

personInfoToString :: (String, String, String) -> String
personInfoToString (name, surname, address) = name ++ " " ++ surname ++ " " ++ address

-- personInfoToString ("Michail", "Berlior", "ul. Sadowa 302a, m.50")
-- "Michail Berlior ul. Sadowa 302a, m.50"

-- personInfoToString ("ul. Sadowa 302a, m.50", "Stiopa", "Lichodiejew")
-- "ul. Sadowa 302a, m.50 Stiopa Lichodiejew"

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfo' -> String
personInfoToString' (name, surname, address) = name ++ " " ++ surname ++ " " ++ address
-- personInfoToString' ("Michail", "Berlior", "ul. Sadowa 302a, m.50")

newtype Name'' = MkName'' String
newtype Surname'' = MkSurname'' String
newtype Address'' = MkAddress'' String
newtype PersonInfo'' = MkPersonInfo'' (Name'', Surname'', Address'')
type PersonInfoToStringType'' = PersonInfo'' -> String

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkPersonInfo'' (name, surname, address)) = name ++ " " ++ surname ++ " " ++ address
-- personInfoToString'' (MkPersonInfo'' (MkName'' "Michail", MkSurname'' "Berlior", MkAddress'' "ul. Sadowa 302a, m.50"))