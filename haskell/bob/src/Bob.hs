module Bob (responseFor) where

import Data.Char    
import Data.Semigroup

responseFor :: String -> String
responseFor xs = 
    show $ case trim xs of
            "" -> Fine 
            trimmed -> endsWith trimmed


data Reaction = Sure | ChillOut | CalmDown | Fine | Whatever deriving (Eq)

instance Show Reaction where
    show reaction = 
        case reaction of 
            Sure -> "Sure."
            ChillOut -> "Whoa, chill out!"
            CalmDown -> "Calm down, I know what I'm doing!"
            Fine -> "Fine. Be that way!"
            _    -> "Whatever."

endsWith :: String -> Reaction    
endsWith xs 
    | (containsNumbers xs && (lastChar xs == '!')) = ChillOut
    | (containsNumbers xs && (lastChar xs == '?')) = Sure
    | (containsNumbers xs) = Whatever
    | (allCaps xs) || (lastChar xs == '!' && allCaps (init xs)) = ChillOut      
    | (lastChar xs == '?' && allCaps (init xs)) = CalmDown      
    | otherwise = case (lastChar xs) of
                    '?' -> Sure                    
                    _   -> Whatever

allCaps :: String -> Bool
allCaps = foldl (\b a -> if(a /= ' ') then b && (isUpper a) else b) True

lastChar :: String -> Char
lastChar xs = (xs !! ((length xs) - 1))

containsNumbers :: String -> Bool 
containsNumbers = foldl (\b a -> if(isNumber a) then True else b) False

trim :: String -> String
trim xs = dropSpace $ reverse $ dropSpace $ (reverse xs)

dropSpace :: String -> String
dropSpace = dropWhile isSpace
         
