-- Exercise 5

type Peg = String -- "A" "B" "C"
type Move = (Peg, Peg)

-- We are given no. of discs and the names of the three pegs... we are expected to return a list of Moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a,b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a) 

{- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = hanoi4 (n-2) a d c b ++ [(a, c), (a, b), (d, b)] ++ hanoi4 (n-2) c b a d -}