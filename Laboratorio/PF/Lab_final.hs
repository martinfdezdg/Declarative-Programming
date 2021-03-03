-- Martín Fernández de Diego



-- DEFINICION: RELACION

data Rel a = R [(a,a)] deriving (Read,Show)
instance Eq a => Eq (Rel a) where
 (R r1) == (R r2) = (and [elem (x,y) r2 | (x,y) <- r1]) && (and [elem (x,y) r1 | (x,y) <- r2])
   
-- esRelacion xs = indica si es una relacion (lista sin repetidos).
esRelacion :: Eq a => [(a,a)] -> Bool
esRelacion [] = True
esRelacion (x:xs)
   | any (\y -> x==y) xs = False
   | otherwise = esRelacion xs



-- EJEMPLOS

ra = R[('D','8'), ('E','B'), ('C','B'), ('E','C'), ('8','D')]

r1 = R [('1','2'),('2','1'),('1','3')]

r2 = R [('3','1'),('1','2'),('2','1')]

r3 = R [('1','1'),('1','2'),('2','1'),('2','2'),('3','3')]

r4 = R [('2','2'),('3','3'),('4','4'),('7','7'),('3','4'),('4','7'),('3','7'),('4','3'),('7','4'),('7','3')]

corazon = R [('1','2'),('1','4'),('2','1'),('2','3'),('2','5'),('3','1'),('3','5'),('4','2'),('4','4'),('5','3')]



-- OPERACIONES: CONJUNTOS

-- Conversion a conjunto
cjto :: Eq a => [a] -> [a]
cjto [] = []
cjto (x:xs) = x : cjto (filter (/= x) xs)

cjtoOrd :: (Ord a,Eq a) => [a] -> [a]
cjtoOrd xs = quicksort $ cjto xs

-- Union conjuntista
union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not $ elem y xs]



-- OPERACIONES: RELACIONES

-- dominio r = conjunto dominio de la relacion r.
dominio :: Eq a => Rel a -> [a]
dominio (R r) = cjto [x | (x,_) <- r]

-- rango r = conjunto rango de la relacion r.
rango :: Eq a => Rel a -> [a]
rango (R r) = cjto [y | (_,y) <- r]

-- soporte r = conjunto sobre el que esta definida la relacion r.
soporte :: Eq a => Rel a -> [a]
soporte (R r) = (dominio (R r)) `union` (rango (R r))

soporteOrd :: (Ord a,Eq a) => Rel a -> [a]
soporteOrd (R r) = quicksort $ (dominio (R r)) `union` (rango (R r))


-- esReflexiva r = indica si r es una relacion reflexiva.
esReflexiva :: Eq a => Rel a -> Bool
esReflexiva (R r) = and [elem (x,x) r | x <- soporte (R r)]

-- esSimetrica r = indica si r es una relacion simetrica.
esSimetrica :: Eq a => Rel a -> Bool
esSimetrica (R r) = and [elem (y,x) r | (x,y) <- r]

-- esTransitiva r = indica si r es una relacion transitiva.
esTransitiva :: Eq a => Rel a -> Bool
esTransitiva (R r) = and [elem (x,z) r | (x,u) <- r, (v,z) <- r, u == v]

-- relEquivalencia r = indica si r es una relacion de equivalencia.
relEquivalencia :: Eq a => Rel a -> Bool
relEquivalencia (R r) = esReflexiva (R r) && esSimetrica (R r) && esTransitiva (R r)


-- conjCociente :: Eq a => Rel a -> [a]
relDirecta :: Eq a => Rel a -> [[a]]
relDirecta (R r) = [x : [y | y <- soporte (R r), (elem (x,y) r || elem (y,x) r)] | x <- soporte (R r)]

clases :: Eq a => [[a]] -> [[a]]
clases [] = []
clases (xs:xxs) = [xs !! 0] : clases xxs

clasesEquivalencia :: (Ord a,Eq a) => Rel a -> [[a]]
clasesEquivalencia (R r) = clases $ cjto $ map cjtoOrd [x : [y | cs <- relDirecta (R r), y <- cs, elem x cs] | x <- soporte (R r)]


-- generaDiv n m = r donde r es la relacion {(x,y) | n <= x, y <= m, x es divisor de y}.
generaDiv :: (Eq a, Integral a) => a -> a -> Rel a
generaDiv n m = R [(x,y) | x <- [n..m], y <- [n..m], mod y x == 0]

-- generaGE xs = r donde r es la relacion >= sobre el conjunto de elementos de la lista xs.
generaGE :: Ord a => [a] -> Rel a
generaGE xs = R [(x,y) | x <- xs, y <- xs, x >= y]


-- cierreR r = cierre reflexivo de la relacion r.
cierreR :: Eq a => Rel a -> Rel a
cierreR (R r) = R (r `union` [(x,x) | x <- soporte (R r)])

-- cierreS r = cierre simetrico de la relacion r.
cierreS :: Eq a => Rel a -> Rel a
cierreS (R r) = R (r `union` [(y,x) | (x,y) <- r])

-- cierreT r = cierre transitivo de la relacion r.
cierreT :: Eq a => Rel a -> Rel a
cierreT (R r) = R (r `union` [(x,z) | (x,u) <- r, (v,z) <- r, u == v])

-- cierreRST r = cierre reflexivo, simetrico y transitivo de la relacion r.
cierreRST :: Eq a => Rel a -> Rel a
cierreRST (R r) = R (r `union` [(x,x) | x <- soporte (R r)]
                       `union` [(y,x) | (x,y) <- r]
                       `union` [(x,z) | (x,u) <- r, (v,z) <- r, u == v])


-- composicion r1 r2 = r1 ◦ r2.
composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R r1) (R r2) = R [(x,z) | (x,u) <- r1, (v,z) <- r2, u == v]


matRel :: Ord a => Rel a -> [[Bool]]
matRel (R r) = [[elem (x,y) r | y <- soporteOrd (R r)] | x <- soporteOrd (R r)]



-- UI: AUXILIARES

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort $ filter (< p) xs) ++ [p] ++ (quicksort $ filter (>= p) xs)

separador :: (Ord t,Num t) => t -> IO ()
separador n
   | n > 0 = do
      putStr "-"
      separador (n-1)
   | otherwise = putStrLn ""


-- UI: ENTRADA

introPares :: IO [(Char,Char)]
introPares = do
   putStr "("
   x <- getChar
   putStr ","
   y <- getChar
   putStr ")"
   e <- getChar
   if (e == ('}')) then do
      putStrLn ""
      return [(x,y)]
   else do
      listaPar <- introPares 
      return $ (x,y):listaPar

introRel :: IO [(Char,Char)]
introRel = do
   putStrLn ""
   putStrLn "INFO: Introduce los pares separados por comas ',' y,"
   putStrLn "      cuando quieras terminar, cierra con una llave '}'."
   putStrLn ""
   putStr "Rel := {"
   rel <- introPares
   return rel


-- UI: SALIDA

imprimeLista :: [Char] -> IO ()
imprimeLista [] = putStrLn ""
imprimeLista (x:xs) = do
   putStr " "
   putChar x
   putStr " "
   imprimeLista xs

imprimeVecRel :: [Bool] -> IO ()
imprimeVecRel [] = putStr ""
imprimeVecRel (x:xs)
   | x == True = do
      putStr " x "
      imprimeVecRel xs
   | x == False = do
      putStr "   "
      imprimeVecRel xs


muestraL1 :: Rel Char -> IO ()
muestraL1 (R r) = do
   putStr "   "
   imprimeLista $ soporteOrd (R r)
   putStr "   "
   separador $ 3 * length (soporteOrd (R r))

muestraLi :: Int -> Rel Char -> IO ()
muestraLi i (R r) = do
   putChar $ (soporteOrd (R r)) !! i
   putStr " |"
   imprimeVecRel $ matRel (R r) !! i
   putStrLn "|"

muestraLn :: Int -> Rel Char -> IO ()
muestraLn n (R r)
   | n <= (length (soporteOrd (R r)) - 1) = do
      muestraLi n (R r)
      muestraLn (n+1) (R r)
   | otherwise = do
      putStr "   "
      separador $ 3 * length (soporteOrd (R r))


muestraRelR :: Rel Char -> IO ()
muestraRelR (R r)= do
   putStrLn ""
   muestraL1 (R r)
   muestraLn 0 (R r)
   putStrLn ""

muestraRel :: IO ()
muestraRel = do
   r <- introRel
   muestraRelR (R r)