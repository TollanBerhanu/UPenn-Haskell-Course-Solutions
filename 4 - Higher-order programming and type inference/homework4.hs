-- Exercise 1 - Wholemeal programming ... reimplement the functions (fun1, fun2) using idiomatic Haskell style
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (map (\x -> x-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = toInteger . sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then (x `div` 2) else (3 * x + 1))

-- Exercise 2 - Folding with trees
type Height = Integer
data Tree a = Leaf | Node Height (Tree a) a (Tree a) deriving (Show, Eq)

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
-- insertNode x (Node h Leaf n Leaf) = Node 1 (insertNode x Leaf) n Leaf
-- insertNode x (Node h Leaf n right) = Node h (insertNode x Leaf) n right
-- insertNode x (Node h left n Leaf) = Node h left n (insertNode x Leaf)
insertNode x (Node h left n right)
    | getHeight left > getHeight right = Node nextHeight left n (insertNode x right)
    | otherwise = Node nextHeight (insertNode x left) n right
    where getHeight Leaf = 0
          getHeight (Node h _ _ _) = h
          nextHeight = 1 + max (getHeight left) (getHeight right)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

-- Exercise 3 - More folds ... return true if there are an odd number of True values in the list
xor :: [Bool] -> Bool
xor = odd . length . foldl (\acc x -> if x then x:acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

-- Exercise 4 - Finding primes ... generate all odd prime numbers upto (2n+2) using Sieve Sundram algorithm
cartProd :: Integer -> [Integer]
cartProd n = [(i + j + (2*i*j)) | i <- [1..n], j <- [1..n] , i<=j]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x->2*x+1) $ filter (\x -> not$ x `elem` cartProd n) [1..n]
