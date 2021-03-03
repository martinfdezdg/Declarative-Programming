-- Sesión de laboratorio 3
-- Martín Fernández de Diego

-- Problema 1
-- a)
last' :: [a] -> a
last' (x:xs) = foldl (\_ x -> x) x xs

-- b)
reverse' :: [a] -> [a]
reverse' xs = foldl (\x y -> y:x) [] xs

-- c)
-- all' :: (a -> Bool) -> [a] -> Bool
-- all' p xs = 

-- Problema 3
-- a)
list1 :: Integral a => [a]
list1 = concat [[i,-i] | i <- [1..]]

-- b)
list2 :: Integral a => [(a,a)]
list2 = [(i,j) | k <- [0..], i <- [0..k], j <- [k,k-1..0]]

-- Problema 4
-- a)
sufijos :: [a] -> [[a]]
sufijos xs = [drop n xs | n <- [0..length xs]]

-- b)
sublistas :: [a] -> [[a]]
sublistas xs = [take n xs' | xs' <- [drop n xs | n <- [0..length xs]], n <- [1..length xs]]

-- c)
-- permutaciones :: [a] -> [[a]]
-- permutaciones xs