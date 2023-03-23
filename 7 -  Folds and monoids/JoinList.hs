module JoinList where

import Data.Monoid
import Editor
import Sized
import StringBuffer

-- Exercise 1 -- Write an append function for JointLists that yields a new JoinList whose monoidal annotation is derived
-- from those of the two arguments.
data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) (Empty) a = a
(+++) a (Empty) = a
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2 ) jl1 jl2
-- ghci> Single (Sum 3) 'a' +++ Single (Sum 2) 'b'  
-- Append (Sum {getSum = 5}) (Single (Sum {getSum = 3}) 'a') (Single (Sum {getSum = 2}) 'b')

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m 
tag (Append m jl1 jl2) = tag jl1 `mappend` tag jl2
-- ghci> tag (Single (Product {getProduct = 4})'s')
-- Product {getProduct = 4}
-- ghci> tag $ Append (Sum 5) (Single (Sum 3) 'a') (Single (Sum 2) 'b')
-- Sum {getSum = 5}

-- Exercise 2 - 

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2
-- ghci> jlToList  $ Append (Sum {getSum = 5}) (Single (Sum {getSum = 3}) 'a') (Single (Sum {getSum = 2}) 'b')
-- "ab"
-- ghci> jlIndex1 = Append (Size 210) ( Append 30 (Single (Size 5) 'y') (Append 6 (Single 2 'e') (Single 3 'a')) ) (Single (Size 7) 'h')
-- ghci> jlToList jlIndex1 
-- "yeah"

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ el (Single m a) = if (getSize $ size m) == el then Just a else Nothing
indexJ el (Append _ jl1 jl2)
    | el > (getSize . size . tag) jl1 = indexJ el jl2
    | otherwise = indexJ el jl1
-- ghci> jlIndex1 = Append (Size 1) (Single (Size 0) "foo") (Single (Size 1) "bar")
-- ghci> indexJ 1 jlIndex1
-- Just "bar"

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n < 1 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append m jl1 jl2)
    | weight jl1 > weight jl2 = dropJ (n-1) jl1
    | otherwise = dropJ (n-1) jl2
        where weight = getSize . size . tag
-- ghci> jlIndex1 = Append (Size 210) ( Append 30 (Single (Size 5) 'y') (Append 6 (Single 2 'e') (Single 3 'a')) ) (Single (Size 7) 'h') 

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n < 1 = Empty
takeJ n (Append m jl1 jl2)
    | weight jl1 > weight jl2 = Append m (takeJ (n-1) jl1) jl2
    | otherwise = Append m jl1 (takeJ (n-1) jl2)
        where weight = getSize . size . tag

-- instance Buffer JoinList (Score, Size) String

-- main = runEditor editor $ JoinList ()
--          [ "This buffer is for notes you don't want to save, and for"
--          , "evaluation of steam valve coefficients."
--          , "To load a different file, type the character L followed"
--          , "by the name of the file."
--          ]
