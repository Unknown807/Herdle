module PartB where

import Base

-- iteration 1, 2 and 3

slice :: Int -> Int -> String -> String
slice i j word = take (j-i) (drop i word)

checkGuess :: String -> String -> [(Char, Status)]
checkGuess guess word = checkGuess' guess word 0

checkGuess' :: String -> String -> Int -> [(Char, Status)]
checkGuess' [] _ _ = []
checkGuess' _ [] _ = []
checkGuess' (x:xs) word cnt
    | ( x == (word!!cnt) ) = nextGuess Here
    | ( x `elem` (slice 0 cnt word) ) = nextGuess (Elsewhere (Just Before))
    | ( x `elem` (slice cnt (length word) word) ) = nextGuess (Elsewhere (Just After))
    | otherwise = nextGuess Nowhere
    where nextGuess status = (x, status) : checkGuess' xs word (cnt+1)