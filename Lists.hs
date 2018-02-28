import Tree
import NestedList

main  = do
    let t = [1..10] :: [] Int
    putStrLn $ show $ reverse' t
    putStrLn $ show $ secondLast' t

    return ()

--Prolog Problem 1
last' :: [a] -> Maybe a
last' []     = Nothing
last' (x:[]) = Just x
last' (x:xs) = last' (xs)

--Prolog Problem 2
secondLast' :: [a] -> Maybe a
secondLast' []       = Nothing
secondLast' (x:[])   = Nothing
secondLast' (x:y:[]) = Just x
secondLast' (x:xs)   = secondLast' xs

--Prolog Problem 3
elementAt :: Int -> [a] -> Maybe a
elementAt n []     = Nothing
elementAt 0 (x:xs) = Just x
elementAt n (x:xs) = elementAt (n-1) xs

--Prolog Problem 4
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

--Prolog Problem 5
reverse' :: [a] -> [a]
reverse' xs = foldl  (flip (:)) [] xs

--Prolog Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (xs == reverse' xs)

--Prolog Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:y:xs)
    | x == y    = x : compress (y:xs)
    | otherwise = x : compress (y:xs)

--Prolog Problem 9
pack1 :: (Eq a) => [a] -> [[a]]
pack1 [] = []
pack1 (x:xs) = (x : (takeWhile (== x) xs)) :  (pack1 (dropWhile (== x) xs))

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x:first) : pack2 second where (first, second) = span (==x) xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f xs = span2 f xs ([], [])

span2 :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
span2 f [] (first, second) = (first, second)
span2 f (x:xs) (first, second)
    | f x == True = span2 f xs (first ++ [x], second)
    | otherwise   = (first, x:xs)

--Prolog Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = let y = pack1 xs in map (\x -> (length x, head x)) y

--Prolog Problem 11
data SandM a = Single a | Multiple (Int, a) deriving (Show)
encode2 :: (Eq a) => [a] -> [SandM a]
encode2 xs = let y = encode xs in map (\(first, second) -> if first == 1 then Single second else Multiple (first, second)) y

--Prolog Problem 12
decode :: (Eq a) => [SandM a] -> [a]
decode xs = foldr f [] xs
    where
        f (Single x) acc =  x : acc
        f (Multiple (x, y)) acc = replicate x y ++ acc

--Prolog Problem 14
dupli :: [a] -> [a]
dupli xs = foldr (\x acc -> x : x : acc ) [] xs

--Prolog Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = foldr (\(i, x) acc -> if (mod i n) == 0 then acc else (x:acc)) [] (zip [1..] xs)

--Prolog Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = foldr (\(i, x) (first, second) -> if (i > n) then (first, x:second) else (x:first, second)) ([], []) (zip [1..] xs)

--Prolog Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) $ take k xs

--Prolog Problem 20
removeAt :: [a] -> Int -> (Maybe a, [a])
removeAt xs n = foldr (\(i, x) (first, second) -> if i == n then (Just x, second) else (first, (x:second))) (Nothing, []) $ zip [1..] xs

--Prolog Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = foldr (\(i, y) acc -> if i == n then (x:y:acc) else (y:acc)) [] (zip [1..] xs)

--Prolog Problem 22
range :: (Enum a, Ord a) => a -> a -> [a]
range x y = if x > y then [] else x : range (succ x) y

