{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1 - Compute the nth fibonacci number

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Now use fib to define the infinite list of all Fibonacci numbers... use ctrl+c to stop execution
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- fibs2 :: [Integer]

-- Exercise 3 - Create a datatype 'Stream' which is an infinite list, create a function that converts from Stream to List
-- and finally create your own instance of stream that shows the first 20 elements or so from the stream

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show st = show (take 20 $ streamToList st)

-- Exercise 4 - Create simple functions for working with streams

streamRepeat :: a -> Stream a -- generates a stream containing infinitely many copies of the given element
streamRepeat el =  Cons el (streamRepeat el)

streamMap :: (a -> b) -> Stream a -> Stream b -- applies a function to every element of a Stream
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a -- generates a Stream from a 'seed' (first element of the stream), and an 
-- “unfolding rule” of type a -> a which specifies how to transform the seed into a new seed
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))
-- ghci> streamFromSeed (+2) 1 ... [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39]


-- Exercise 5 - lets create a few streams

nats :: Stream Integer -- an infinite list of natural numbers: 0,1,2...
nats = streamFromSeed (+1) 0

ruler :: Stream Integer -- corresponds to the ruler function 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4 ... (up to 16)
ruler = streamMap powOf2 (streamFromSeed (+1) 1)

powOf2 :: Integer -> Integer
powOf2 x
    | odd x = 0
    | otherwise = 1 + powOf2 (x `div` 2)

fibStream :: Stream Integer -- Fibonacci numbers via generating functions
fibStream = streamMap (\(pre,cur) -> cur) $ streamFromSeed (\(pre,cur) -> (cur, pre+cur)) (0,1)

-- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0) -- fromInteger :: Num a => Integer -> a
    negate (Cons x xs) = Cons (-x) (negate xs) -- negate :: Num a => a -> a4
    (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs+ys) -- (+) :: Num a => a -> a -> a
    (*) as@(Cons a as') bs@(Cons b bs') = Cons (a*b) (fromInteger a*bs' + as'*bs) -- (*) :: Num a => a -> a -> a

-- ghci> nats + 1  ...  [1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
-- ghci> nats * 2  ...  [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38]

-- Implement an instance of the Fractional class for Stream Integer
-- instance Fractional (Stream Integer) where
--     (/) (Cons a as) (Cons b bs) = Cons (a `div` b) (fromInteger (1 `div` b) * (as / (negate bs) )) -- (/) :: Fractional a => a -> a -> a

-- fibs3 :: Stream Integer
-- fibs3 = streamMap (\x -> x / (1 - x - (x^2)))