import Data.List
import Data.Map (toList, fromListWith)
import Data.Ord (comparing)
import System.IO

main = do
    putStrLn . show . makeHuffmanTree $ "ravi"


data HTree a = Leaf a | Branch (HTree a) (HTree a)

makeHuffmanTree :: String -> [(Char, String)]
makeHuffmanTree [] = []
makeHuffmanTree xs = sortBy (comparing fst) . serialize . makeHuffmanTree' . map (\(a, n) -> (Leaf a, n)) . sortBy (comparing snd) . calculateFrequency $ xs
        where
            makeHuffmanTree' :: [(HTree a, Int)] -> HTree a
            makeHuffmanTree' [(t, _)] = t
            makeHuffmanTree' ((t1, w1):(t2, w2):xs) = makeHuffmanTree' $ insertBy (comparing snd) (Branch t1 t2, w1 + w2) xs

            serialize :: HTree a -> [(a, String)]
            serialize (Leaf x) = (x, ""):[]
            serialize (Branch x y) = (map (\(a, s) -> (a, '0':s)) $ serialize x) ++ (map (\(a, s) -> (a, '1':s)) $ serialize y)

            calculateFrequency :: (Ord a) => [a] -> [(a, Int)]
            calculateFrequency xs = toList $ fromListWith (+) $ map (\x -> (x, 1)) xs


