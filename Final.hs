module Final where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

nameF = ["Mary"]
nameM = ["John"]
anaphorF = ["herself"]
anaphorM = ["himself"]
specCPTrace = ["spec t"]
plainWords = ["about", "that", "I", "food","think", "ate", "terrify", "said", "saw", "likes", "thinks", "like", "see"]
c = ["What did", "Who did", "What does", "Why does", "Where does"]
occupiedSpec = ["why", "what", "who"]
trace = ["t"]
------------------------------------------------------
-- Some tiny helpers for writing trees more compactly
type Automaton st sy = ([st], [sy], [st], [([st],sy,st)])
data Tree sy = Node sy [Tree sy] deriving Show
lf :: a -> Tree a
lf x = Node x []

mrg :: Tree String -> Tree String -> Tree String
mrg t1 t2 = Node "*" [t1,t2]

------------------------------------------------------

--subjacency trees
--What did John think that Mary ate t
t1 :: Tree String
t1 =
    Node "**" [
        Node "What did" [],
        Node "**" [
            Node "John" [],
            Node "*" [
                Node "think" [],
                Node "**" [
                    Node "spec t" [],
                    Node "*" [
                        Node "that" [],
                        Node "**" [
                            Node "Mary" [],
                            Node "*" [
                                Node "ate" [],
                                Node "t" []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

-- *Who did stories about terrify John
t2 :: Tree String
t2 =
    Node "**" [
        Node "Who did" [],
        Node "**" [
            Node "**" [
                Node "stories" [],
                Node "*" [
                    Node "about"[],
                    Node "t" []
                    ]
                ],
            Node "*" [
                Node "terrify" [],
                Node "John" []
            ]
        ]
    ]

--What does John think that Mary ate t
t3 :: Tree String
t3 = Node "**" [
         Node "What does" [],
         Node "**" [
              Node "John" [],
              Node "*" [
                  Node "think" [],
                  Node "**" [
                      Node "spec t" [],
                      Node "*" [
                          Node "that" [],
                          Node "**" [
                              Node "Mary" [],
                              Node "*" [
                                  Node "said" [],
                                  Node "**" [
                                      Node "spec t" [],
                                      Node "*" [
                                          Node "that" [],
                                          Node "**" [
                                              Node "I" [],
                                              Node "*" [
                                                   Node "ate" [],
                                                   Node "t" []
                                              ]
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
         ]
    ]

--ungrammatical string attempting to ask where Mary is from
-- *Where did Mary from talked
t4 :: Tree String
t4 = 
    Node "**"[
        Node "Where did" [],
        Node "**" [
            Node "**"[
                Node "Mary" [
                    Node "*" [
                        Node "from" [],
                        Node "t" []
                        ]
                        ],
            Node "VP" [
                Node "talked" []
                ]
            ]
        ]
    ]

--principle a trees

t5 :: Tree String
t5 = 
    Node "**" [
        Node "Mary" [],
        Node "*" [
            Node "saw" [],
            Node "herself" []
        ]
    ]

t6 :: Tree String
t6 = 
    Node "**" [
        Node "herself" [],
        Node "*" [
            Node "ate" [],
            Node "food" []
        ]
    ]

t7 :: Tree String
t7 = Node "**" [
        Node "John" [],
        Node "*" [
            Node "thinks" [],
            Node "*" [
                Node "that" [],
                Node "**" [
                    Node "Mary" [],
                    Node "*" [
                        Node "likes" [],
                        Node "himself" []
                    ]
                ]
            ]
        ]
    ]

--What does Mary see t
t8 :: Tree String
t8 = Node "**" [
        Node "What does" [],
        Node "**" [
            Node "Mary" [],
            Node "*" [
                    Node "see" [],
                    Node "t" []
                ]
            ]
        ]

--Where does Mary see herself t
t9 :: Tree String
t9 = Node "**" [
         Node "Where does" [],
         Node "**" [
            Node "Mary" [],
            Node "*" [
                Node "see" [],
                Node "*" [
                    Node "herself" [],
                    Node "t" []
                ]
            ]
         ]
    ]

data Subjacency = NBN | BN1 | UG | Plain | OK | C | Spect deriving (Eq, Show)
fsta_sub :: Automaton Subjacency String
fsta_sub = (
             [{-states-}NBN, BN1, UG, Plain, OK, C, Spect],
            nameM ++ nameF ++ anaphorM ++ anaphorF ++ specCPTrace ++ plainWords ++ c ++ trace ++ ["*", "**"],
             [{-final-}OK, Plain],
             [{-deltas-}
             --traces that pass BNs or non BNs
             ([Plain, NBN],   "**", BN1),
             ([Plain, NBN],   "*",  NBN),             
             ([NBN, Plain],   "**", BN1),            
             ([NBN, Plain],   "*",  NBN),
             --traces that have already crossed 1 BN
             ([Plain, BN1],   "**", UG),
             ([Plain, BN1],   "*",  BN1),
             --if a trace reaches a Spec, CP the BNs are "reset"
             ([Spect, BN1],   "**", NBN),
             --trace reaches surface pos
             ([C,     BN1],   "**", OK),
             ([C,     NBN],   "**", OK),
             --other required deltas
             ([Plain, Plain], "*", Plain),
             ([Plain, Plain], "**", Plain)]
             ++ map(\s -> ([], s, Plain)) (plainWords++nameF++nameM++anaphorF++anaphorM)
             ++ map(\s -> ([], s, Spect)) specCPTrace
             ++ map(\s -> ([], s, C)) c
             ++ map(\s -> ([], s, NBN)) trace)


-- AP = anaphor; B = bound; UB = unbound
data PrincipleA = APm| APf | Plainp | NPf | NPm | B | UB deriving (Eq, Show)
fsta_pa :: Automaton PrincipleA String
fsta_pa = (
            --states
            [APm, APf, Plainp, NPf, NPm, B, UB],
            --strings
            nameM ++ nameF ++ anaphorM ++ anaphorF ++ specCPTrace ++ plainWords ++ c ++ trace ++ ["*", "**"],
            --final states
            [B, Plainp],
            --deltas
            [
                --anaphors are bound
               ([NPm, APm], "**", B),
               ([NPf, APf], "**", B),
               ([APm, NPm], "**", B),
               ([APf, NPf], "**", B),
               --other deltas               
               ([NPf, Plainp], "*", Plainp),
               ([NPm, Plainp], "*", Plainp),
               ([Plainp, NPf], "*", Plainp),
               ([Plainp, NPm], "*", Plainp),
               ([Plainp, APm], "*", APm),
               ([Plainp, APf], "*", APf),
               ([APm, Plainp], "*", APm),
               ([APf, Plainp], "*", APf),               
               ([Plainp, Plainp], "*", Plainp),
               ([Plainp, Plainp], "**", Plainp),
               ([NPf, Plainp], "**", Plainp),
               ([NPm, Plainp], "**", Plainp),
               ([Plainp, B], "**", Plainp),
               ([Plainp, B], "*", Plainp),
               ([B, Plainp], "**", Plainp),
               ([B, Plainp], "*", Plainp)]
             ++ map(\s -> ([], s, APf)) anaphorF
             ++ map(\s -> ([], s, APm)) anaphorM
             ++ map(\s -> ([], s, NPm)) nameM
             ++ map(\s -> ([], s, NPf)) nameF
             ++ map(\s -> ([], s, Plainp)) (specCPTrace ++ plainWords ++ c ++ trace) 
          )

data EX1 = A | F deriving (Eq, Show)
fsta_ex1 :: Automaton EX1 String
fsta_ex1 = ([A, F],
           ["*", "**", "a", "b"],
           [A],
           [
           ([A, F], "*", F),
           ([F, A], "*", A),
           ([], "a", A),
           ([], "b", F)
           ])

data EX2 = D | E deriving (Eq, Show)
fsta_ex2 :: Automaton EX2 String
fsta_ex2 = ([D, E],
           ["*", "**", "a", "b"],
           [D],
           [
           ([D, E], "*", E),
           ([E, D], "*", D),
           ([], "a", D),
           ([], "b", E)
           ])

intersect :: (Eq st, Eq st', Eq sy) => Automaton st sy -> Automaton st' sy -> Automaton (st,st') sy
intersect (st, sym, f, d) (st', sym', f', d') =
    let newStates = liftA2 (\x -> \y -> (x,y)) st st' in
    let newF = liftA2 (\x -> \y -> (x,y)) f f' in
    let newSyms = nub (sym ++ sym') in
    --possible binary branches from the new states 
    let tuples = liftA2 (\x -> \y -> [x,y]) newStates newStates in
    let candidateTransitions = liftA3 (\x -> \y -> \z -> (x,y,z)) tuples newSyms newStates in
    let isValidTransition ([(q1,q1'), (q2,q2')],x,(q3,q3')) = elem ([q1,q2], x, q3) d && elem ([q1',q2'], x, q3') d' in
    let newDelta = filter isValidTransition candidateTransitions in
    let possibleLeafNodes = liftA2 (\x -> \y -> ([], x, y)) newSyms newStates in
    let validLeafNodes ([], x, (q,q')) = elem ([], x, q) d && elem ([], x, q') d' in
    let newLeaf = filter validLeafNodes possibleLeafNodes in
    (newStates, newSyms, newF, newDelta++newLeaf)

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

generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates g t = let (states, syms, f, d) = g in
                let (Node x y) = t in
                or (map (\q -> under g t q && elem q f)states)