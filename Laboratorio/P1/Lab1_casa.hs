-- Sesión de laboratorio 1
-- Martín Fernández de Diego


-- 1. a)
years :: Fractional a => a
years = (((10^6/60)/60)/24)/365


-- 1. b)
dataTime :: Integral a => (a, a, a, a, a)
dataTime = let seconds = 10^6 in
           let minutes = div seconds 60 in
           let hours = div minutes 60 in
           let days = div hours 24 in
           let years = div days 365 in
           (years, days-years*365, hours-days*24, minutes-hours*60, seconds-minutes*60)


-- 1. c)
years_function :: Fractional a => a -> a
years_function seconds = (((seconds/60)/60)/24)/365

dataTime_function :: Integral a => a -> (a, a, a, a, a)
dataTime_function seconds = let minutes = div seconds 60 in
                            let hours = div minutes 60 in
                            let days = div hours 24 in
                            let years = div days 365 in
                            (years, days-years*365, hours-days*24, minutes-hours*60, seconds-minutes*60)


-- 2
avg :: Fractional a => [a] -> a
avg list = sum list / fromIntegral(length list)


-- 3. a)
-- con patrones
num_digitos1 :: Integral a => a -> a
num_digitos1 0 = 0
num_digitos1 x = 1 + num_digitos1(div x 10)

-- con guardas
num_digitos2 :: Integral a => a -> a
num_digitos2 x
 | (abs(x) < 10) = 1
 | otherwise = 1 + num_digitos2(div x 10)


-- 3. b)
-- con patrones
sum_digits :: Integral a => a -> a
sum_digits 0 = 0
sum_digits x = abs(mod x 10) + sum_digits(div x 10)

reduccion1 :: Integral a => a -> a
reduccion1 x = if (abs x < 10) then abs(x)
               else reduccion1(sum_digits(abs(x)))

-- con guardas
reduccion2 :: Integral a => a -> a
reduccion2 x = let { sum_d y
                     | (abs(y) < 10) = abs(y)
                     | otherwise = abs(mod y 10) + sum_d(div y 10);
                     z = abs(x)
                   }
               in if z < 10 then z else reduccion2(sum_d(z))


-- 3. c)
factorial :: Integral a => a -> a
factorial k = if (k == 0) then 1
              else k * (factorial(k-1))

comb :: Integral a => a -> a -> a
comb n m = div (factorial n) ((factorial m) * (factorial(n - m)))


-- 4
-- Estricta exclusivamente en el primer argumento
and1 :: Bool -> Bool -> Bool
and1 True x = x
and1 False _ = False

-- Estricta exclusivamente en el segundo argumento
and2 :: Bool -> Bool -> Bool
and2 x True = x
and2 _ False = False

-- Estricta en ambos argumentos
and3 :: Bool -> Bool -> Bool
and3 True x = x
and3 False True = False
and3 False False = False
