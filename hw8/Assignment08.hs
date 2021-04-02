{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Assignment08 where

import Control.Applicative(liftA, liftA2, liftA3)

import TreeGrammars

plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]

------------------------------------------------------
-- Some tiny helpers for writing trees more compactly

lf :: a -> Tree a
lf x = Node x []

mrg :: Tree String -> Tree String -> Tree String
mrg t1 t2 = Node "*" [t1,t2]

------------------------------------------------------

-- (1a)/(2a) `C John ate an apple yesterday'
tree_1a :: Tree String
tree_1a = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (1b)/(2b) `Q John ate what yesterday'
tree_1b :: Tree String
tree_1b = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

-- (3a) `Q John ate an apple yesterday'
tree_3a :: Tree String
tree_3a = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (3b) `C John ate what yesterday'
tree_3b :: Tree String
tree_3b = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

tree_13 :: Tree String
tree_13 =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "laughed" [],
                Node "**" [
                    Node "because" [],
                    Node "*" [
                        Node "Mary" [],
                        Node "*" [
                            Node "*" [Node "bought" [], Node "books" []],
                            Node "why" []
                        ]
                    ]
                ]
            ]
        ]
    ]

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

count :: (Eq a) => a -> Tree a -> Int
count s t = let (Node x y) = t in
            case y of
            [] -> case (x == s) of
                  True -> 1
                  False -> 0
            a:rest -> case (x == s) of
                      True -> 1 + sum (map (\b -> count s b) y)
                      False -> 0 + sum (map (\b -> count s b) y)

leftEdge :: Tree a -> [a]
leftEdge t = let (Node x y) = t in
             case y of
             [] -> [x]
             a:rest -> x:leftEdge a

allLists :: Int -> [a] -> [[a]]
allLists n l = case n of
               0 -> [[]]
               x -> case l of
                    h:t -> liftA2(\y -> \z -> y:z) l (allLists (n-1) l)
                    [] -> [[]]

under :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> st -> Bool
under g t q = let (states, syms, f, d) = g in
              let (Node x y) = t in
              case y of
              [] ->  elem ([], x, q) d
              a:rest -> or (map (\qk -> elem (qk,x,q)d && (and (map (\(t',q') -> under g t' q') (zip y qk))))(allLists(length y) states)) 

--or (map (\qk -> elem (qk,x,q)d) && and (map (\t' -> or (map(\q' -> under g t' q')qk))rest)) (allLists(length rest) states)
--or (map (\(t', q') -> (elem (q',x,q)d && (under g t' q')))(allLists (length q) states))
generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates g t = let (states, syms, f, d) = g in
                let (Node x y) = t in
                or (map (\q -> under g t q && elem q f)states)

data Situ = Q | Wh | OK | Qn deriving (Eq, Show)
fsta_wh1 :: Automaton Situ String
fsta_wh1 = (
             [{-states-}OK, Q, Wh],
             plainWords ++ whWords ++ qWords ++ ["*"],
             [{-final-}OK],
             [{-deltas-}
             ([Wh, Wh], "*", Wh), 
             ([Q,  Wh], "*", OK), 
             ([OK, Wh], "*", Wh),
             ([Wh, OK], "*", Wh),
             ([OK, OK], "*", OK),
             ([Q, OK],  "*", Qn),
             ([OK, Q],  "*", Qn)]
             ++ map(\s -> ([], s, OK)) plainWords
             ++ map(\s -> ([], s, Q)) qWords
             ++ map(\s -> ([], s, Wh)) whWords)

-- always want extra states, make those states not the final state -> state where Q does not ccommand an wh word -> dont let that state trnasition
-- dont let it be final state

data Island = Qi | Whi | OKi | Qni | IslY | IslN deriving (Eq, Show)
fsta_wh2 :: Automaton Island String
fsta_wh2 = (
             [{-states-}OKi, Qi, Whi],
             plainWords ++ whWords ++ qWords ++ ["*"] ++ ["**"],
             [{-final-}OKi],
             [{-deltas-}
             ([Whi, Whi], "*",  Whi), 
             ([Qi,  Whi], "*",  OKi), 
             ([OKi, Whi], "*",  Whi),
             ([Whi, OKi], "*",  Whi),
             ([OKi, OKi], "*",  OKi),
             ([Qi, OKi],  "*",  Qni),
             ([OKi, Qi],  "*",  Qni),
             ([OKi, Whi], "**", IslN),
             ([OKi, OKi],  "**", IslY),
             ([Qi, Whi], "**", OKi)]
             ++ map(\s -> ([], s, OKi)) plainWords
             ++ map(\s -> ([], s, Qi)) qWords
             ++ map(\s -> ([], s, Whi)) whWords)


--island dont let wh go past ** locality requirement to encode locatlity