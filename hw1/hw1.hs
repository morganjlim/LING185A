1. let x = 4 + 5 in (3 * x)
=> (3 * (4 + 5))
=> (3 * 9)
=> 27

2. (\x -> 3 * x) (4 + 5)
=> 3 * (4 + 5)
=> 3 * 9
=> 27

3. ((\x -> (\y -> x + (3 * y))) 4) 1
=> ((\y -> 4 + (3 * y)))1
=> 4 + (3 * 1)
=> 4 + 3
=> 7

4. let x = 4 in (let y = 1 in (x + (3 * y)))
=> let y = 1 in (4 + (3 * y))
=> 4 + (3 * 1)
=> 4 + 3
=> 7

5. let x = 4 in (let y = 1 + x in (x + (3 * y)))
=> let y = 1 + 4 in (4 + (3 * y))
=> let y = 5 in (4 + (3 * y))
=> 4 + (3 * 5)
=> 4 + 15
=> 19

6. ((\x -> (\y -> x + (3 * x))) 4) 1
=>(\y -> 1 + (3 * 1))4
=> 1 + (3 * 1)
=> 1 + 3
=> 4

7. ((\x -> (\y -> y + (3 * y))) 4) 1
=> (\y -> y + (3 * y))4
=> 4 + (3 * 4)
=> 4 + 12
=> 16

8. (\y -> y + ((\y -> 3*y) 4)) 5
=> 5 + (\y -> 3*y)4
=> 5 + (3*4)
=> 5 + 12
=> 17

9. (\y -> ((\y -> 3*y) 4) + y) 5
=> 5 + (\y -> 3*y)4
=> 5 + (3*4)
=> 5 + 12
=> 17

10. (\x -> x * (let x = 3*2 in (x + 7)) + x) 4
=> 4 * (let x = 3*2 in (x+7)) + 4
=> 4 * ((3*2) + 7) + 4
=> 4 * (6 + 7) + 4
=> 4 * (13) + 4
=> 52 + 4
=> 56

11. g ((let x = 4 in (\y -> x + y)) 2)
=> (\z -> z + 4) ((let x = 4 in (\y -> x + y))2)
=> (\z -> z + 4) ((\y -> 4 + y)2)
=> (\z -> z + 4) (4 + 2)
=> (\z -> z + 4) 6
=> 6 + 4
=> 10

12. let fn = \x -> (let y = 3 in x + y) in fn 4
=> (\x -> (let y = 3 in x + y))4
=> (let y = 3 in 4 + y)
=> 4 + 3
=> 7

13. let fn = (let y = 3 in \x -> x + y) in fn 4
=> (let y = 3 in \x -> x + y) 4
=> (\x -> x + 3) 4
=> 4 + 3
=> 7

14. f ((\fn -> fn Rock) (\x -> whatItBeats x))
=> f ((\x -> whatItBeats x) Rock)
=> f (whatItBeats Rock)
=> f (whatItBeats = \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock)
=> f Scissors
=> \s -> case s of {Rock -> 334; Paper -> 138; Scissors 99} Scissors
=> 99

15. ((\f -> ( \x -> f (f x))) whatItBeats) Paper
=> (\x -> whatItBeats (whatItBeats x))Paper
=> (whatItBeats (whatItBeats Paper))
=> whatItBeats (whatItBeats = \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Paper)
=> whatItBeats Rock
=> whatItBeats = \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock
=> Scissors


16. whatItBeats (case Paper of {Rock -> Paper; Paper -> Rock; Scissors -> Scissors})
=> whatItBeats Rock
=> (\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}) Rock
=> Scissors

17. (case (Win Rock) of {Draw -> whatItBeats; Win z -> (\s -> Scissors)}) Paper
=> (\s -> Scissors) Paper
=> Scissors

18. case (Win (whatItBeats Rock)) of {Draw -> n; Win x -> (n + f x)}
=> case (Win (\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock)) of {Draw -> n; Win x -> (n + f x)}
=> case Win Scissors of {Draw -> n; Win x -> (n + f x)}
=> n + f Scissors
=> n + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors 99}) Scissors
=> n + 99
=> 1 + 99
=> 100

19. let y = 2 in (case (Win (whatItBeats Rock)) of {Draw -> n; Win y -> (n + f y)} + y)
=> case (Win (WhatItBeats Rock)) of {Draw -> n; Win y -> (n + f y)} + 2
=> case (Win (\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock)) of {Draw -> n; Win y -> (n + f y)} + 2
=> case (Win Scissors) of {Draw -> n; Win y -> (n + f y)} +2
=> (n + f Scissors) + 2
=> (n + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors 99} Scissors)) + 2
=> (n + 99) + 2
=> (1 + 99) + 2
=> 100 + 2
=> 102