{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree

import qualified Memoization as M

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

treeToDeriv:: Tree nt t -> [RewriteRule nt t]
treeToDeriv t = case t of
                {
                Leaf x s -> [TRule x s];
                NonLeaf x (NonLeaf a (a1)(a2)) (NonLeaf b (b1)(b2)) -> [NTRule x (a, b)] ++ treeToDeriv (NonLeaf a (a1)(a2)) ++ treeToDeriv (NonLeaf b (b1)(b2));
                NonLeaf x (Leaf a (a1))(NonLeaf b (b1)(b2)) -> [NTRule x (a, b)] ++ treeToDeriv (Leaf a (a1)) ++ treeToDeriv (NonLeaf b (b1)(b2));
                NonLeaf x (NonLeaf a (a1)(a2)) (Leaf b (b1)) -> [NTRule x (a, b)] ++ treeToDeriv(NonLeaf a (a1)(a2))++ treeToDeriv(Leaf b (b1));
                NonLeaf x (Leaf a (a1)) (Leaf b (b1)) -> [NTRule x (a, b)] ++ treeToDeriv(Leaf a (a1))++ treeToDeriv(Leaf b (b1))
                }

fix f = (let x = f x in x)

fastInside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
fastInside cfg str' n' = 
    let (nts, ts, i, r) = cfg in
    let workerBooster otherWorker str n =
            case str of
            [] -> M.lift0 gfalse
            (x:[]) -> M.lift0 (r (TRule n x))
            (x:y:rest2) -> let conj i ld rd = M.lift2 (\a -> \b -> r (NTRule n (ld,rd)) &&& a &&& b)
                                                                     (otherWorker (take i str) ld)
                                                                     (otherWorker (drop i str) rd)
                           in
                           M.liftMany gen_or (liftA3 conj [1 .. (length str - 1)] nts nts)
    in
    (M.memoFix2 workerBooster) str' n'


f :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> [t] -> v
f cfg s = let (nts, ts, i, r) = cfg in
              gen_or (map( \w -> (i w) &&& fastInside cfg s w) nts)

outside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> ([t], [t]) -> nt -> v
outside cfg (ys, zs) n =  let (nts, ts, i, r) = cfg in
                     case (null ys && null zs) of
                     True -> i n
                     False -> let r_side i p rs = (outside cfg (ys, drop i zs) p) &&& (r(NTRule p (n,rs))) &&& (fastInside cfg (take i zs) rs)
                              in let l_side i p ls = (outside cfg (take i ys, zs) p) &&& (r(NTRule p (ls,n))) &&& (fastInside cfg (drop i ys) ls)
                              in gen_or (liftA3 r_side [1 .. length zs] nts nts) ||| gen_or (liftA3 l_side [0 .. length ys - 1] nts nts)

fastOutside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> ([t], [t]) -> nt -> v
fastOutside cfg (ys', zs') n' =
                    let (nts, ts, i, r) = cfg in
                    let workerBooster otherWorker (ys, zs) n = 
                             case (null ys && null zs) of
                             True -> M.lift0 (i n)
                             False -> M.lift2 (\a -> \b -> a ||| b)
                                    (let r_side i p rs = M.lift1 (\x -> x &&& (r(NTRule p (n,rs))) &&& (fastInside cfg (take i zs) rs)) (otherWorker (ys, drop i zs) p)
                                        in M.liftMany gen_or (liftA3 r_side [1 .. length zs] nts nts))
                                    (let l_side i p ls = M.lift1 (\x -> x  &&& (r(NTRule p (ls,n))) &&& (fastInside cfg (drop i ys) ls)) (otherWorker (take i ys, zs) p)
                                        in M.liftMany gen_or (liftA3 l_side [0 .. length ys - 1] nts nts))
                    in
                    (M.memoFix2 workerBooster) (ys', zs') n'

insideTrees :: GenericCFG nt t Double -> [t] -> nt -> [(Double, Tree nt t)]
insideTrees cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> gfalse
    (x:[]) -> (r (TRule n x), Leaf n x)
    (x:y:rest2) -> 
        let f i rd ld = 
               case (r (NTRule n (ld, rd))) == 0.0 of
               True ->
               False -> 
               (\(d1, t1) -> \(d2, t2) -> (d1*d2), concat t1 t2) (insideTrees cfg str1 n1) (inside trees cfg str2 n2)
               --in concat (lift A3 f [1..(length str - 1) nts nts])

completeTrees :: GenericCFG nt t Double -> [t] -> [(Double, Tree nt t)]
completeTrees = undefined