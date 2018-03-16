module Pangram (isPangram) where

import Data.List
import Data.Char

alphabets :: [Char]
alphabets = ['a'..'z']
isPangram :: [Char] -> Bool
isPangram xs = (sort (checkPangram' xs [])) == alphabets

checkPangram' :: [Char] -> [Char] -> [Char]
checkPangram' [] acc = acc
checkPangram' (x:xs) acc = 
        if(elem x' alphabets) then (checkPangram' xs ys) else (checkPangram' xs acc)
        where
            ys = if(elem x' acc) then acc else (x':acc)
            x' = toLower x            