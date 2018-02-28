module NestedList
(
    NestedList(..),
    flattenList
) where

data NestedList a = Element a | List [NestedList a]

instance (Show a) => Show (NestedList a) where
    show (Element x) = show x
    show (List xs)   = show xs

flattenList :: (NestedList a) -> [a]
flattenList (Element x)   = [x]
flattenList (List [])     = []
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)
