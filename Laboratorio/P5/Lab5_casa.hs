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
formatea :: String -> String -> Int -> IO()
formatea fileIn fileOut n = do fI <- readFile fileIn
                               let ls = lines fI
                                   ls' = map concat [espaciar n l | l <- map words ls]
                                   fO = unlines ls'
                                   in writeFile fileOut fO

espaciar :: Int -> [String] -> [String]
espaciar n ws = if (length $ concat ws) < n || ws /= [] then
                   let huecos = length ws - 1
                       relleno = n - (length $ concat ws)
                       d = div relleno huecos
                       r = mod relleno huecos
                       espacios = [if i <= r then d+1 else d | i <- [1..length ws - 1]]
                       in [w ++ replicate e ' ' | (w,e) <- zip ws espacios] ++ [last ws]
                else ws


-- Problema 3
type Vector = [Float]
type Matriz = [Vector]

-- a)
transp :: Matriz -> Matriz
transp ([] : _) = []
transp m = (map head m) : transp (map tail m)

sumaVec :: Vector -> Vector -> Vector
sumaVec v1 v2 = zipWith (+) v1 v2

sumaMat :: Matriz -> Matriz -> Matriz
sumaMat m1 m2 = zipWith sumaVec m1 m2

prodEscalar :: Vector -> Vector -> Float
prodEscalar v1 v2 = sum $ zipWith (*) v1 v2

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = [[prodEscalar v1 v2 | v1 <- m1] | v2 <- transp(m2)]

-- b)
dibujaVector :: Vector -> IO()
dibujaVector [] = do putStr ""
dibujaVector v = do putStrLn (unwords [show (x) | x <- v])

dibujaMatriz :: Matriz -> IO()
dibujaMatriz [] = do putStr ""
dibujaMatriz (v:m) = do dibujaVector v
                        dibujaMatriz m

