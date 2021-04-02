module Assignment03Test where

import FiniteState
import Assignment03
import Control.Exception

-- Allow comparing SnocLists with ==
instance Eq a => Eq (SnocList a) where
  ESL == ESL = True
  (r ::: x) == (s ::: y) = x == y && r == s
  _ == _ = False

test_fsa_countVs :: [Char]
test_fsa_countVs =
  assert (fsaSanityCheck fsa_countVs == True)
  assert (generates fsa_countVs [V, C, V] == False)
  assert (generates fsa_countVs [C, C] == False)
  assert (generates fsa_countVs [V, C, V, V, C] == True)
  assert (generates fsa_countVs [V, V, V, C, V, V] == False)
  assert (generates fsa_countVs [V, V, V, C, V, V, V] == True)
  "fsa_countVs success!"

test_addToFront :: [Char]
test_addToFront =
  assert (addToFront 2 (((ESL ::: 3) ::: 4) ::: 5) == ((((ESL ::: 2) ::: 3) ::: 4) ::: 5))
  assert (addToFront 'x' ((ESL ::: 'y') ::: 'z') == (((ESL ::: 'x') ::: 'y') ::: 'z'))
  assert (addToFront 'x' ESL == (ESL ::: 'x'))
  assert (addToFront False (ESL ::: True) == ((ESL ::: False) ::: True))
  "addToFront success!"

test_toSnoc :: [Char]
test_toSnoc =
  assert (toSnoc ['h','e','l','l','o'] == (((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'))
  assert (toSnoc [3,4,5] == (((ESL ::: 3) ::: 4) ::: 5))
  assert (toSnoc [True] == (ESL ::: True))
  "toSnoc success!"


test_forward :: [Char]
test_forward =
  assert (forward fsa_handout4 ((ESL ::: C) ::: V) 43 == False)
  assert (forward fsa_handout4 ((ESL ::: C) ::: V) 42 == True)
  assert (forward fsa_handout4 (toSnoc [C,V]) 42 == True)
  assert (map (forward fsa_handout4 (toSnoc [C,V,V])) [40,41,42,43] == [True,False,True,True])
  assert (map (forward fsa_handout5 (toSnoc [C,V])) [1,2,3] == [True,True,False])
  assert (map (forward fsa_handout5 (toSnoc [C,V,C])) [1,2,3] == [True,False,True])
  assert (map (forward fsa_handout5 (toSnoc [C,V,C,C])) [1,2,3] == [True,False,False])
  assert (map (forward fsa_handout5 (toSnoc [C,V,C,C,V])) [1,2,3] == [True,True,False])
  "forward success!"


test_generate2 :: [Char]
test_generate2 =
  assert (generates2 fsa_handout5 [C,V,C,C] == generates fsa_handout5 [C,V,C,C])
  "generate2 success!"


test_fsa_twoCs :: [Char]
test_fsa_twoCs =
  assert (fsaSanityCheck fsa_twoCs == True)
  assert (generates fsa_twoCs [C,C,C,C] == True)
  assert (generates fsa_twoCs [C,V,C,V] == True)
  assert (generates fsa_twoCs [V,V,C,V] == False)
  assert (generates fsa_twoCs [] == False)
  assert (generates fsa_twoCs [C] == False)
  assert (generates2 fsa_twoCs [C,C,C,C] == True)
  assert (generates2 fsa_twoCs [C,V,C,V] == True)
  assert (generates2 fsa_twoCs [V,V,C,V] == False)
  assert (generates2 fsa_twoCs [] == False)
  assert (generates2 fsa_twoCs [C] == False)
  "fsa_twoCs success!"

test_fsa_thirdC :: [Char]
test_fsa_thirdC =
  assert (fsaSanityCheck fsa_thirdC == True)
  assert (generates fsa_thirdC [V,C] == False)
  assert (generates fsa_thirdC [V,V,C] == True)
  assert (generates fsa_thirdC [V,V,C,C,V] == True)
  assert (generates fsa_thirdC [C,C,V,C,C] == False)
  assert (generates fsa_thirdC [C,C,C,C,C] == True)
  assert (generates2 fsa_thirdC [V,C] == False)
  assert (generates2 fsa_thirdC [V,V,C] == True)
  assert (generates2 fsa_thirdC [V,V,C,C,V] == True)
  assert (generates2 fsa_thirdC [C,C,V,C,C] == False)
  assert (generates2 fsa_thirdC [C,C,C,C,C] == True)
  "fsa_thirdC success!"

test_fsa_thirdlastC :: [Char]
test_fsa_thirdlastC =
  assert (fsaSanityCheck fsa_thirdlastC == True)
  assert (generates fsa_thirdlastC [C,C,C,C,C] == True)
  assert (generates fsa_thirdlastC [C,C,C,V,C] == True)
  assert (generates fsa_thirdlastC [C,C,V,V,C] == False)
  assert (generates fsa_thirdlastC [C,C,V] == True)
  assert (generates fsa_thirdlastC [C,V] == False)
  assert (generates2 fsa_thirdlastC [C,C,C,C,C] == True)
  assert (generates2 fsa_thirdlastC [C,C,C,V,C] == True)
  assert (generates2 fsa_thirdlastC [C,C,V,V,C] == False)
  assert (generates2 fsa_thirdlastC [C,C,V] == True)
  assert (generates2 fsa_thirdlastC [C,V] == False)
  "fsa_thirdlastC success!"

test_fsa_oddEven :: [Char]
test_fsa_oddEven =
  assert (fsaSanityCheck fsa_oddEven == True)
  assert (generates fsa_oddEven [C] == True)
  assert (generates fsa_oddEven [C,V,V] == True)
  assert (generates fsa_oddEven [C,C,V] == False)
  assert (generates fsa_oddEven [C,C,V,C,V] == True)
  assert (generates2 fsa_oddEven [C] == True)
  assert (generates2 fsa_oddEven [C,V,V] == True)
  assert (generates2 fsa_oddEven [C,C,V] == False)
  assert (generates2 fsa_oddEven [C,C,V,C,V] == True)
  "fsa_oddEven success!"

test_fsa_harmony :: [Char]
test_fsa_harmony =
  assert (fsaSanityCheck fsa_harmony == True)
  assert (generates fsa_harmony [P,K,I,K,MB,U,P,U] == True)
  assert (generates fsa_harmony [P,K,I,K,U,P,U] == False)
  assert (generates fsa_harmony [K,I,P,I] == True)
  assert (generates fsa_harmony [K,P,P,P] == True)
  assert (generates fsa_harmony [K,I,P,U] == False)
  assert (generates fsa_harmony [K,I,MB,P,U] == True)
  assert (generates fsa_harmony [MB,MB,K,MB,P] == True)
  assert (generates2 fsa_harmony [P,K,I,K,MB,U,P,U] == True)
  assert (generates2 fsa_harmony [P,K,I,K,U,P,U] == False)
  assert (generates2 fsa_harmony [K,I,P,I] == True)
  assert (generates2 fsa_harmony [K,P,P,P] == True)
  assert (generates2 fsa_harmony [K,I,P,U] == False)
  assert (generates2 fsa_harmony [K,I,MB,P,U] == True)
  assert (generates2 fsa_harmony [MB,MB,K,MB,P] == True)
  "fsa_harmony success!"

test_fsa_MBU :: [Char]
test_fsa_MBU =
  assert (fsaSanityCheck fsa_MBU == True)
  assert (generates fsa_MBU [MB,U] == True)
  assert (generates fsa_MBU [U,MB] == False)
  assert (generates fsa_MBU [MB] == True)
  assert (generates fsa_MBU [MB,K,K,K,K,K,K,U] == True)
  assert (generates fsa_MBU [K,K,K,K,K,K,K,U] == False)
  assert (generates fsa_MBU [K,K,K,K,K,K,K,K] == True)
  assert (generates fsa_MBU [MB,I,I,I] == True)
  assert (generates fsa_MBU [MB,U,U,U,U,U] == True)
  assert (generates2 fsa_MBU [MB,U] == True)
  assert (generates2 fsa_MBU [U,MB] == False)
  assert (generates2 fsa_MBU [MB] == True)
  assert (generates2 fsa_MBU [MB,K,K,K,K,K,K,U] == True)
  assert (generates2 fsa_MBU [K,K,K,K,K,K,K,U] == False)
  assert (generates2 fsa_MBU [K,K,K,K,K,K,K,K] == True)
  assert (generates2 fsa_MBU [MB,I,I,I] == True)
  assert (generates2 fsa_MBU [MB,U,U,U,U,U] == True)
  "fsa_MBU success!"

test_fsa_adjacentMBU :: [Char]
test_fsa_adjacentMBU =
  assert (fsaSanityCheck fsa_adjacentMBU == True)
  assert (generates fsa_adjacentMBU [MB,U] == True)
  assert (generates fsa_adjacentMBU [MB,K,K,K,K,K,K,U] == False)
  assert (generates fsa_adjacentMBU [MB,U,MB,U] == True)
  assert (generates fsa_adjacentMBU [MB,U,MB,U,U] == False)
  assert (generates fsa_adjacentMBU [MB,U,U,U,U] == False)
  assert (generates fsa_adjacentMBU [MB,I,MB] == True)
  assert (generates2 fsa_adjacentMBU [MB,U] == True)
  assert (generates2 fsa_adjacentMBU [MB,K,K,K,K,K,K,U] == False)
  assert (generates2 fsa_adjacentMBU [MB,U,MB,U] == True)
  assert (generates2 fsa_adjacentMBU [MB,U,MB,U,U] == False)
  assert (generates2 fsa_adjacentMBU [MB,U,U,U,U] == False)
  assert (generates2 fsa_adjacentMBU [MB,I,MB] == True)
  "fsa_adjacentMBU success!"


main = do
  putStrLn test_fsa_countVs
  putStrLn test_addToFront
  putStrLn test_toSnoc
  putStrLn test_forward
  putStrLn test_generate2
  putStrLn test_fsa_twoCs
  putStrLn test_fsa_thirdC
  putStrLn test_fsa_thirdlastC
  putStrLn test_fsa_oddEven
  putStrLn test_fsa_harmony
  putStrLn test_fsa_MBU
  putStrLn test_fsa_adjacentMBU
