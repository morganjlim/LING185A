module Assignment04 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

--SECTION 1

--g = SLG s = given string q = transitions
backwardSLG :: (Eq sy) => SLG sy -> [sy] -> sy -> Bool
backwardSLG = \g -> \w -> \q-> let (syms, i, f, t) = g in
                               case w of
                               x:[] -> elem x f
                               y:rest -> elem (y, head rest) t && backwardSLG g rest (head rest)

-- g = SLG (gramar) s = given string
generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG = \g -> \s -> let(syms, i, f, t) = g in
                            case s of
                            [] -> False
                            [x] -> elem x i && elem x f
                            x:rest -> elem x i && backwardSLG g s x


slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (syms, i, f, t) =
	 let states = [ExtraState] ++ (map (\s -> StateForSymbol s) syms) in
     let final = map (\s -> StateForSymbol s) f in
     let firstT = map (\s -> (ExtraState, s, StateForSymbol s)) i in
     let restT = map (\(x,y) -> (StateForSymbol x, y, StateForSymbol y)) t in
     let delta = firstT ++ restT in
     (states, syms, [ExtraState], final, delta)



unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
unionFSAs (states, syms, i, f, d) (states', syms', i', f', d') =
    let newStates = (map (\s-> First s) states) ++ (map (\s -> Second s) states') in
    let newInitial = (map (\s -> First s) i) ++ (map(\s -> Second s) i') in
    let newFinal = (map (\s -> First s) f) ++ (map(\s -> Second s) f') in
    let newSyms = nub(syms ++ syms') in
    let newDelta = (map (\(x,y,z) -> (First x, y, First z)) d) ++ (map (\(x,y,z) -> (Second x, y, Second z)) d') in
    (newStates, newSyms, newInitial, newFinal, newDelta)

concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
concatFSAs (states, syms, i, f, d) (states', syms', i', f', d') =
    let newStates = (map (\s-> First s) states) ++ (map (\s -> Second s) states') in
    let newInitial = map(\s -> First s) i in
    let newFinal = map(\s -> Second s) f' in
    let newSyms = nub(syms ++ syms') in
    let newDelta = (map (\(x,y,z) -> (First x, y, First z)) d) ++ (map (\(x,y,z) -> (Second x, y, Second z)) d') ++ liftA2 (\x -> \y -> (First x, Nothing, Second y)) f i' in
    (newStates, newSyms, newInitial, newFinal, newDelta)

--can start and end at initial, go one or more passes through m
starFSA :: EpsAutomaton st sy -> EpsAutomaton (Either Int st) sy
starFSA (st, sym, i, f, d) =
	let newInitial = First 1 in
    let i' = map (\s -> Second s) i in
    let f' = map (\s -> Second s) f in
	let newFDelta = liftA2 (\x -> \y -> (y, Nothing,x)) i' f' in 
	let newIDelta = map (\s -> (newInitial, Nothing, s)) i' in 
	let newDelta = newFDelta ++ newIDelta ++ map (\(x, y, z) -> (Second x, y, Second z)) d in 
	let newFinal = [newInitial] ++ f' in
	let newStates = [newInitial] ++ map(\s -> Second s )st in
	(newStates, sym, [newInitial], newFinal, newDelta)

flatten :: Either Int Int -> Int
flatten x = case x of
            First a -> 2*a + 1
            Second b -> 2*b

mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates fx (states, syms, i, f, d) =
	let newStates = map fx states in
	let newI = map fx i in
	let newF = map fx f in
	let newD = map (\(x, y, z) -> (fx x, y, fx z)) d in
	(newStates, syms, newI, newF, newD)

reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA = \r -> case r of
                Lit x -> ([2,3], [x], [2], [3], [(2, Just x, 3)])
                Alt r1 r2 -> mapStates flatten (unionFSAs (reToFSA r1) (reToFSA r2))
                Concat r1 r2 -> mapStates flatten (concatFSAs (reToFSA r1) (reToFSA r2))
                Star r -> mapStates flatten (starFSA (reToFSA r))
                ZeroRE -> ([4], [], [4], [], [])
                OneRE -> ([4], [], [4], [4], [])

