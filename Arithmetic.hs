import Data.List

main = do
    putStrLn $ show $ gcd' 3 4

--Problem 31
isPrime :: (Integral a) => a -> Bool
isPrime k = k > 1 &&
    foldr (\p r -> rem k p /= 0 && r) True [2..s]
    where s = floor $ sqrt $ fromIntegral k

--Problem 32
gcd' :: (Integral a) => a -> a -> a
gcd' a b
    | a > b     = gcd' b a
    | a == 0    = b
    | otherwise = gcd' (mod b a) a

--Problem 33
coprime :: (Integral a) => a -> a -> Bool
coprime a b = gcd' a b == 1

--Problem 34
phi :: Int -> Int
phi 1 = 1
phi x = length $ filter (coprime x) [1..(x-1)]

--Problem 36
primeFactors :: Int -> [Int]
primeFactors n
    | n == 1 = []
    | otherwise = f : primeFactors (div n f)
    where f = firstFactor n

firstFactor :: Int -> Int
firstFactor n = firstFactor' n 2
    where firstFactor' n factor
            | n == 1              = 1
            | factor * factor > n = n
            | mod n factor == 0   = factor
            | otherwise           = firstFactor' n (factor + 1)

