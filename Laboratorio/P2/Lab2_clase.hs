-- Sesión de laboratorio 2
-- Martín Fernández de Diego

-- 1. a)
cuadrados :: (Num a, Eq a) => a -> [a]
cuadrados 0 = [0]
cuadrados n = cuadrados(n-1) ++ [n^2]

-- 1. b)
paresCuadrados :: (Num a, Eq a) => a -> [(a,a)]
paresCuadrados 0 = [(0,0)]
paresCuadrados n = [(n,n^2)] ++ paresCuadrados(n-1)

-- 1. c)
sumaCos :: (Floating a, Eq a) => a -> a
sumaCos 1 = abs(cos(1))
sumaCos n = n * abs(cos(n)) + sumaCos(n-1)

-- 1. d)
sumaMultiplos :: Integral a => a -> a
sumaMultiplos 0 = 0
sumaMultiplos n = if (mod n 3 == 0 || mod n 5 == 0) then n + sumaMultiplos(n-1)
                  else sumaMultiplos(n-1)

-- 1. e)


-- 1. f)


-- 2. a)
cuadradosFOS :: (Num a, Enum a) => a -> [a]
cuadradosFOS n = map (^2) [0..n]

-- 2. b)
paresCuadradosFOS :: (Num a, Enum a) => a -> [(a,a)]
paresCuadradosFOS n = zip [n,n-1..0] (map (^2) [n,n-1..0])

-- 2. c)
-- sumaCosFOS :: (Floating a, Enum a) => a -> a
-- sumaCosFOS n = map (x * abs(cos(x))) [0..n]

-- 3. a)
-- iguales :: Eq a => (Int -> a) -> (Int -> a) -> Int -> Int -> Bool
-- iguales f g n m = length(filter (map f [n..m] == map g [n..m]) [n..m])

-- 3. b)
menorA :: Int -> Int -> (Int -> Bool) -> Int
menorA n m p = head(filter p [n..m])

-- 3. c)
mayor :: Int -> (Int -> Bool) -> Int
mayor n p = last(filter p [0..n])

-- 3. d)
ex :: Int -> Int -> (Int -> Bool) -> Bool
ex n m p = any p [n..m]

-- 4. a)
-- filter2 :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> ([Int],[Int])