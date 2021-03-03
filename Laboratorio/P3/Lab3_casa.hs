-- Sesión de laboratorio 3
-- Martín Fernández de Diego


-- Problema 1
-- a) last
last' :: [a] -> a
last' (x:xs) = foldl (\_ x -> x) x xs

-- b) reverse
reverse' :: [a] -> [a]
reverse' xs = foldl (\x y -> y:x) [] xs

-- c) all
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldl (\x y -> x && p y) True xs

-- d) minimun
minimun' :: Ord a => [a] -> a
minimun' (x:xs) = foldl (\x y -> if x < y then x else y) x xs

-- e) map
map' :: (a -> b) -> [a] -> [b]
map' p xs = foldr (\x y -> p x:y) [] xs

-- f) filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x y -> if p x then x:y else y) [] xs

-- g) takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = foldr (\x y -> if p x then x:y else []) [] xs


-- Problema 2
foldr' :: (a -> a -> a) -> [a] -> a
foldr' p [x] = x
foldr' p (x:xs) = p x (foldr' p xs)

foldl' :: (a -> a -> a) -> [a] -> a
foldl' p [x] = x
foldl' p (x:xs) = foldl p x xs


-- Problema 3
-- a)
list1 :: Integral a => [a]
list1 = concat [[i,-i] | i <- [1..]]

-- b)
list2 :: Integral a => [(a,a)]
list2 = [(x,z-x) | z <- [0..], x <- [0..z]]


-- Problema 4
-- a) Lista de todos los sufijos de xs
sufijos :: [a] -> [[a]]
sufijos xs = [drop n xs | n <- [0..length xs]]

-- b) Lista de todas las sublistas de xs
sublistas :: [a] -> [[a]]
sublistas xs = []:[take n ys | n <- [1..length xs], ys <- sufijos xs, n <= length ys]

-- c) Lista de todas las permutaciones de xs
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x:ys | x <- xs, ys <- permutaciones (filter (/=x) xs)]

-- d) Lista de todas las descomposiciones en sumandos positivos de n
sumandos :: (Num a, Enum a, Eq a) => a -> [[a]]
sumandos 0 = [[]]
sumandos n = [x:ys | x <- [1..n], ys <- sumandos(n-x)]
