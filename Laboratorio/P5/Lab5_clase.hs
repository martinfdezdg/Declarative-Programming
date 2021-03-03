-- Sesión de laboratorio 5
-- Martín Fernández de Diego


-- Problema 1
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

adivina :: Int -> IO()
adivina n = do putStr "Introduce un número: "
               x <- getInt
               if x == n then
                  do putStrLn "Correcto"
               else if x > n then
                  do putStrLn "El número buscado es menor"
                     adivina n
               else
                  do putStrLn "El número buscado es mayor"
                     adivina n


-- Problema 2
-- formatea :: String -> String -> Int -> IO()
-- formatea fileIn fileOut n = do fIs <- readFile


-- Problema 3
type Vector = [Float]
type Matriz = [Vector]

-- a)
-- transp :: Matriz -> Matriz

-- b)
sumaVec :: Vector -> Vector -> Vector
sumaVec v1 v2 = zipWith (+) v1 v2

sumaMat :: Matriz -> Matriz -> Matriz
sumaMat m1 m2 = zipWith sumaVec m1 m2

-- c)
prodEscalar :: Vector -> Vector -> Float
prodEscalar v1 v2 = sum $ zipWith (*) v1 v2

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = [prodEscalar v1 v2 | v1 <- m1, v2 <- transp(m2)]


