{-# LANGUAGE OverloadedStrings #-}

import          Prelude hiding (not, and, or)

import          System.Random

import          Data.Random
import          Data.Random.Extras

data Bit = Off | On
  deriving (Show, Eq, Enum)


data Logic = And Logic Logic
           | Nand Logic Logic
           | Or Logic Logic
           | Nor Logic Logic
           | Xor Logic Logic
           | Xnor Logic Logic
           | Not Logic
           | Id Logic
           | Var Int
    deriving (Show, Eq)

{- Potential issue TODO
It will be a tree and not a DAG, when it "reconnects" with other parts of the tree its basically a copy

verify that this is a problem, and if so:
solve this by using "pointers" instead
-}


{- Returns a random logic and all sublogics (used in the random process to make different parts of the tree connect into a DAG) (however see the note on the DAG issue)
-}
randomLogic :: RVar (Logic, [Logic])

{- Used to easily render evaluated logic (for debugging (?))
-}
data EvalLogic = EvalLogic Bit Logic
    deriving (Eq)

instance Show EvalLogic where
    show (EvalLogic val logic) = show val ++ "@" ++ show logic


allCombinations :: Int -> [[Bit]]
allCombinations 0 = [[]]
allCombinations n = ((Off:) <$> reminder) ++ ((On:) <$> reminder)
  where reminder = allCombinations (n - 1)

allOff _ = Off
allOn _ = On
evenOn i = if mod i 2 == 0 then On else Off

numSolved :: [Bit] -> Int
numSolved [] = 0
numSolved (Off:x) = numSolved x

a,b,c,d,e,f,g :: Logic
a = Var 0
b = Var 1
c = Var 2
d = Var 3
e = Var 4
f = Var 5
g = Var 6

eval :: Logic -> (Int -> Bit) -> Bit
eval (And logic logic') vars = and (eval logic vars) (eval logic' vars)
eval (Nand logic logic') vars = nand (eval logic vars) (eval logic' vars)
eval (Or logic logic') vars = or (eval logic vars) (eval logic' vars)
eval (Nor logic logic') vars = nor (eval logic vars) (eval logic' vars)
eval (Xor logic logic') vars = xor (eval logic vars) (eval logic' vars)
eval (Xnor logic logic') vars = xnor (eval logic vars) (eval logic' vars)
eval (Not logic) vars = not $ eval logic vars
eval (Id logic) vars = eval logic vars
eval (Var i) vars = vars i

test logic = (\varList-> eval logic (varList!!)) <$> allCombinations 6

not :: Bit -> Bit
not Off = On
not On = Off

and :: Bit -> Bit -> Bit
and On On = On
and _ _ = Off

nand :: Bit -> Bit -> Bit
nand On On = Off
nand _ _ = On

or :: Bit -> Bit -> Bit
or Off Off = Off
or _ _ = On

nor :: Bit -> Bit -> Bit
nor Off Off = On
nor _ _ = Off

xor :: Bit -> Bit -> Bit
xor On Off = On
xor Off On = On
xor _ _ = Off

xnor :: Bit -> Bit -> Bit
xnor Off Off = On
xnor On On = On
xnor _ _ = Off
