module Tree 
(
    Tree(..),
    singleton,
    insertElement
) where

data Tree a = Empty | Node a (Tree a) (Tree a)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

insertElement :: (Ord a) => a -> Tree a -> Tree a
insertElement x Empty = singleton x
insertElement x (Node y l r)
    | x == y =  Node y l r
    | x  < y =  Node y (insertElement x l) r
    | x  > y =  Node y l (insertElement x r)

instance (Show a) => Show (Tree a) where
    show Empty = ""
    show (Node x l r) = show l ++ show x ++ " " ++ show r
