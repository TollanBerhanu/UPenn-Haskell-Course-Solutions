{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser (parseExp)
import qualified StackVM as SVM
import qualified Data.Map as M

-- Exercise 1 - Write an evaluator for ExprT
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add ex1 ex2) = (eval ex1) + (eval ex2)
eval (Mul ex1 ex2) = (eval ex1) * (eval ex2)
-- (2+3) * 4 ==  eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

-- Exercise 2 - You are provided with parseExp from Parse module. If you pass the constructors of ExprT to it as arguments,
-- it will convert Strings representing arithmetic expressions into values of type ExprT
parseExpEg1 =  parseExp Lit Add Mul "(2+3)*4" -- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
parseExpEg2 = parseExp Lit Add Mul "2+3*" -- Nothing

-- Use this function to implement a fuction that takes an arithmetic expression in the form of a String and evaluates the
-- expression (if it's a well-formed exp, otherwise return Nothing)
evalStr :: String -> Maybe Integer
evalStr exp = case parseExp Lit Add Mul exp of
                Just x -> Just $ eval x
                Nothing -> Nothing

-- Exersice 3 - Create a typeclass named Expr with three methods: lit, add, mul which correspond to the constructors of
-- ExprT. Also make an instance of Expr for the ExprT type.

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit a = Lit a 
    add e1 e2 = Add e1 e2 
    mul e1 e2 = Mul e1 e2 

-- ghci> :t mul (add (lit 2) (lit 3)) (lit 4) ... mul (add (lit 2) (lit 3)) (lit 4) :: Expr a => a
-- ghci> mul (add (lit 2) (lit 3)) (lit 4) ... Error: Ambiguous type variable
-- Wtiting this expression by itself is ambiguous because it can a have any type which is an instance of the Expr typeclass

-- To resolve this ambiguity, we can give it an explicit type signature like this:
-- ghci> mul (add (lit 2) (lit 3)) (lit 4) :: ExprT ... Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- Or we can use it as part of another expression with context that determines the type (in this case ... ExprT)
addContext :: ExprT -> ExprT
addContext = id  -- id :: a -> a  ...  (id 3 == 3, id True == True). It's the same as addContext c = c
-- ghci> :t reify $ mul (add (lit 2) (lit 3)) (lit 4) ... reify $ mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- ghci> reify $ mul (add (lit 2) (lit 3)) (lit 4) ... Mul (Add (Lit 2) (Lit 3)) (Lit 4)


-- Exercise 4 - Make instances of Expr for each of the following types ... (Integer, Bool, MinMax, Mod7)

instance Expr Integer where
    lit a = a
    add e1 e2 = e1 + e2
    mul e1 e2 = e1 * e2
-- ghci> mul (add (lit 2) (lit 3)) (lit 4) :: Integer  ... 20

instance Expr Bool where
    lit a = (a > 0)
    add e1 e2 = e1 || e2
    mul e1 e2 = e1 && e2
-- ghci> mul (add (lit 2) (lit 3)) (lit 4) :: Bool  ... True

newtype MinMax = MinMax Integer deriving (Show, Eq)
instance Expr MinMax where
    lit a = MinMax a
    add (MinMax e1) (MinMax e2) = MinMax $ max e1 e2
    mul (MinMax e1) (MinMax e2) = MinMax $ min e1 e2
-- ghci> mul (add (lit 2) (lit 3)) (lit 4) :: MinMax  ... MinMax 3

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit a = Mod7 $ a `mod` 7
    add (Mod7 e1) (Mod7 e2) = Mod7 (e1 + e2 `mod` 7)
    mul (Mod7 e1) (Mod7 e2) = Mod7 (e1 * e2 `mod` 7)
-- ghci> mul (add (lit 2) (lit 3)) (lit 4) :: Mod7  ... Mod7  6

-- You can test the defined instances using the following expressions
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5" -- Just (-7)

testInteger = testExp :: Maybe Integer -- Just (-7)
testBool = testExp :: Maybe Bool -- Just True
testMM = testExp :: Maybe MinMax -- Just (MinMax 5)
testSat = testExp :: Maybe Mod7 -- Just (Mod7 7)


-- Exercise 5 - Implement a compiler for arithmetic expressions by creating an instance of the Expr type class for Program,
-- so that arithmetic expressions can be interpreted as compiled programs

instance Expr SVM.Program where
    lit a = [SVM.PushI a]
    add e1 e2 = e1 ++ e2 ++ [SVM.Add]
    mul e1 e2 = e1 ++ e2 ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile exp = myProg
    where myProg = parseExp lit add mul exp :: Maybe SVM.Program

-- To test this function, use SVM.stackVM ... stackVM :: Program -> Either String StackVal
testSVM :: String -> Either String SVM.StackVal
testSVM = SVM.stackVM . (\(Just prog) -> prog) . compile 

testSVM1 = testSVM "(3+4)*5" -- Right (IVal 35)


-- Exercise 6 - 

class HasVars a where
    var :: String -> a

data VarExprT = Var String | VLit Integer | VAdd VarExprT VarExprT | VMul VarExprT VarExprT deriving (Show, Eq)

instance Expr VarExprT where
    lit a = VLit a
    add e1 e2 = VAdd e1 e2
    mul e1 e2 = VMul e1 e2

instance HasVars VarExprT where
    var str = Var str

-- ghci> add (lit 3) (var "x") :: VarExprT ... VAdd (VLit 3) (Var "x")

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var str = M.lookup str

-- instance Expr (M.Map String Integer -> Maybe Integer) where
    

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
