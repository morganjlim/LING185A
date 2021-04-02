module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), not, Bool(..), Int, Show)

-- Just to give us one more type to play around with.
data Shape = Rock | Paper | Scissors deriving Show

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

--A
mult :: Numb -> (Numb -> Numb)
mult = \n -> \m -> case n of
                   Z -> Z
                   S n' -> add m (mult n' m)

--B
sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of
	            Z -> Z
	            S n' -> (add n) (sumUpTo n')

--C
equal :: Numb -> (Numb -> Bool)
equal = \n -> \m -> case n of
                    Z -> case m of
                    	 Z -> True
                    	 S m' -> False
                    S n' -> case m of
                    	    Z -> False
                    	    S m' -> equal n' m'

--D
bigger :: Numb -> (Numb -> Numb)
bigger = \n -> \m -> case n of
                     Z -> m
                     S n' -> case m of
                             Z -> n
                             S m' -> S( bigger n' m')

--E
count :: (Int -> Bool) -> ([Int] -> Numb)
count = \f -> \l -> case l of
                    [] -> Z
                    x : rest -> case f x of
                    	        True -> S(count f rest)
                    	        False -> (count f rest)

--F
listOf :: Numb -> (Shape -> [Shape])
listOf = \n -> \s -> case n of
	                 Z -> []
	                 S n' -> s:(listOf n' s)

--G
addToEnd :: Shape -> ([Shape] -> [Shape])
addToEnd = \s -> \l -> case l of
	                   [] -> [s]
	                   x : rest -> x:(addToEnd s rest)

--H
remove :: (Int -> Bool) -> ([Int] -> [Int])
remove = \f -> \l -> case l of
                    [] -> []
                    x : rest -> case f x of
                    	        True -> (remove f rest)
                    	        False -> x:(remove f rest)

--I
prefix :: Numb -> ([Shape] -> [Shape])
prefix = \n -> \l -> case n of
                     Z -> []
                     S n' -> case l of
                     	     [] -> []
                     	     x : rest -> x:(prefix n' rest)

--J
countNegs :: Form -> Numb
countNegs = \f -> case f of
                  T -> Z
                  F -> Z
                  Neg phi -> add one (countNegs phi)
                  Cnj phi psi -> add (countNegs phi) (countNegs psi)
                  Dsj phi psi -> add (countNegs phi) (countNegs psi)
                  
--K
depth :: Form -> Numb
depth = \f -> case f of
	          T -> one
	          F -> one
	          Neg phi -> add one (depth phi)
	          Cnj phi psi -> add one (bigger (depth phi) (depth psi))
	          Dsj phi psi -> add one (bigger (depth phi) (depth psi))
	          

--L
leftmostLeaf :: Form -> Form
leftmostLeaf = \f -> case f of
	                 T -> T
	                 F -> F
	                 Neg phi -> leftmostLeaf phi
	                 Cnj phi psi -> leftmostLeaf phi
	                 Dsj phi psi -> leftmostLeaf phi
