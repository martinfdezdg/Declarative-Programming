-- Sesión de laboratorio 4
-- Martín Fernández de Diego


-- Problema 1
-- a)
type Punto = (Int, Int)

data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq,Ord,Show)

destino :: Punto -> [Direccion] -> Punto
destino (x,y) dir = foldl mover (x,y) dir
        where mover (x,y) ARRIBA = (x,y+1)
              mover (x,y) ABAJO = (x,y-1)
              mover (x,y) IZQUIERDA = (x-1,y)
              mover (x,y) DERECHA = (x+1,y)

trayectoria :: Punto -> [Direccion] -> [Punto]
trayectoria (a,b) [] = [(a,b)]
trayectoria (a,b) (x:xs)
   | x == ARRIBA = (a,b+1):(trayectoria ((a,b+1)) xs)
   | x == ABAJO = (a,b-1):(trayectoria ((a,b-1)) xs)
   | x == IZQUIERDA = (a-1,b):(trayectoria ((a-1,b)) xs)
   | x == DERECHA = (a+1,b):(trayectoria ((a+1,b)) xs)

inf :: Int -> [Direccion] -> [Direccion] -> Int
inf n [] _ = 0
inf n _ [] = 0
inf n (x:xs) (y:ys) =
   if x == ARRIBA && y /= ARRIBA then (inf n xs ys)+1
   else if x /= ARRIBA && y == ARRIBA then (inf n xs ys)-1
   else (inf n xs ys)

inferior :: Int -> [Direccion] -> [Direccion] -> Bool
inferior n xs ys = if (inf n xs ys) <= 0 then True else False

-- Problema 2
