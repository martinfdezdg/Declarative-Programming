-- Sesión de laboratorio 4
-- Martín Fernández de Diego


-- Problema 1
-- a)
data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq,Ord,Show)

destino :: (Int,Int) -> [Direccion] -> (Int,Int)
destino (x,y) dir = foldl mover (x,y) dir
        where mover (x,y) ARRIBA = (x,y+1)
              mover (x,y) ABAJO = (x,y-1)
              mover (x,y) IZQUIERDA = (x-1,y)
              mover (x,y) DERECHA = (x+1,y)


-- Problema 2
data Nat = Cero | Suc Nat deriving (Eq,Ord)

infix 6 +++
(+++) :: Nat -> Nat -> Nat
Cero +++ x = x
Suc x +++ y = Suc(x +++ y)

infix 7 ***
(***) :: Nat -> Nat -> Nat
Suc(Cero) *** x = x
Suc x *** y = x +++ y *** y

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

instance Show Nat
        where show x = show (natToInt x)


-- Problema 3
data Complejo = C (Float,Float) deriving Eq

instance Num Complejo
        where C (a1,b1) + C (a2,b2) = C (a1+a2,b1+b2)
              C (a1,b1) - C (a2,b2) = C (a1-a2,b1-b2)
              C (a1,b1) * C (a2,b2) = C (a1*a2-b1*b2,a1*b2+a2*b1)

instance Show Complejo
        where show (C (a,b)) | b > 0 = show (a) ++ " + " ++ show (b) ++ "i"
                             | b < 0 = show (a) ++ " - " ++ show (-b) ++ "i"
                             | b == 0 = show (a)


-- Problema 4
class Medible a
        where medida :: a -> Int
              medida a = 1
instance Medible Bool
        where medida a = 0
instance Medible [a]
        where medida xs = length xs
instance Medible (a,b)
        where medida (a,b) = 2



