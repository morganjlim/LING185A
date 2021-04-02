{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring v) => v -> v -> v -> v
distrib_lhs x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

--1
--A
distrib_rhs :: (Semiring v) => v -> v -> v -> v
distrib_rhs x y z = (x &&& y) ||| (x &&& z)

--B
dotprod :: (Semiring v) => [v] -> [v] -> v
dotprod x y = case x of
              [] -> case y of
                    [] -> gfalse
                    b:resty -> gfalse
              a:restx -> case y of
                        [] -> gfalse
                        b:resty -> (a &&& b) ||| (dotprod restx resty)

--C
expn :: (Semiring v) => v -> Numb -> v
expn x n = case n of
           Z -> gtrue
           S n' -> x &&& (expn x n')

--2
--D
backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> f q
    x:rest -> gen_or (map (\q1 -> delta (q,x,q1) &&& backward m rest q1) states)

--E
f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f m w =
    let (states, syms, i, f, delta) = m in
    gen_or (map (\q -> (i q) &&& backward m w q) states)

--3
--F
addCost :: Cost -> Cost -> Cost
addCost x y = case x of
              TheInt a -> case y of
                          TheInt b -> TheInt (a + b)
                          Inf -> Inf
              Inf -> Inf

--G
minCost :: Cost -> Cost -> Cost
minCost x y = case x of
              TheInt a -> case y of
                          TheInt b -> TheInt (min a b)
                          Inf -> TheInt a
              Inf -> case y of
                     TheInt b -> TheInt b
                     Inf -> Inf

--H
instance Semiring Cost where
    x &&& y = addCost x y
    x ||| y = minCost x y
    gtrue = TheInt 0
    gfalse = Inf

--4
--I
instance Semiring [[a]] where
     x &&& y = liftA2 (\a -> \b -> (a ++ b)) x y
     x ||| y = x ++ y
     gtrue = [[]]
     gfalse = []

--J
gfsa39 :: GenericAutomaton Int Char [[Char]]
gfsa39 = makeGFSA [] ([1,2,3], ['C','V'],
                         [(1, [[]])], [(1, [[]])], 
                         [((1,'V',1), [['V']]),
                          ((1,'C',2), [['C']]),
                          ((1,'V',3), [['V']]),
                          ((2,'V',1), [['V'],['V','V']]),
                          ((2,'V',3), [['V'],['V','V']]),
                          ((3,'C',1), [[]])])

--K
gfsa_flap :: GenericAutomaton Int Char [[Char]]
gfsa_flap = makeGFSA [] ([0,1,2], ['a','n','t','T'],
                         [(0, [[]])], [(0, [[]]), (1, [[]]), (2, [['t']])], 
                         [((0,'n',0), [['n']]),
                          ((0,'t',0), [['t']]),
                          ((0,'a',1), [['a']]),
                          ((1,'n',0), [['n']]),
                          ((1,'a',1), [['a']]),
                          ((1,'t',2), [[]]),
                          ((2,'a',1), [['t','a'],['T','a']]),
                          ((2,'t',0), [['t','t']]),
                          ((2,'n',0), [['t','n']])])

--5
--L
gfsa7_count :: GenericAutomaton Int Char Double
gfsa7_count = makeGFSA 0 ([1,2,3], ['C','V'],
                         [(1, 1)], [(1, 1)], 
                         [((1,'V',1), 1),
                          ((1,'C',2), 1),
                          ((1,'V',3), 1),
                          ((2,'V',1), 1),
                          ((2,'V',3), 1),
                          ((3,'C',1), 1)])

--M
gfsa7_paths :: GenericAutomaton Int Char [[Int]]
gfsa7_paths = makeGFSA [] ([1,2,3], ['C','V'],
                         [(1, [[1]])], [(1, [[]])], 
                         [((1,'V',1), [[1]]),
                          ((1,'C',2), [[2]]),
                          ((1,'V',3), [[3]]),
                          ((2,'V',1), [[1]]),
                          ((2,'V',3), [[3]]),
                          ((3,'C',1), [[1]])])