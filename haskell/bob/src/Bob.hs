module Bob (responseFor) where

import Data.Char    
import Data.Semigroup

responseFor :: String -> String
responseFor xs = 
    case (endsWith xs) of 
        Sure -> "Sure."
        ChillOut -> "Whoa, chill out!"
        CalmDown -> "Calm down, I know what I'm doing!"
        _    -> "Whatever."

data Reaction = Sure | ChillOut | CalmDown | Fine | Whatever deriving (Eq, Show)

endsWith :: String -> Reaction    
endsWith xs 
    | (allCaps xs) || (lastChar xs == '!' && allCaps (init xs)) = ChillOut      
    | (lastChar xs == '?' && allCaps (init xs)) = CalmDown      
    | otherwise = case (lastChar xs) of
                    '?' -> Sure
                    '.' -> Whatever 
                    '!' -> Whatever 
                    _   -> Fine

allCaps :: String -> Bool
allCaps = foldl (\b a -> if(a /= ' ') then b && (isUpper a) else b) True

lastChar :: String -> Char
lastChar xs = (xs !! ((length xs) - 1))

-- anyCharIsCaps :: String -> Bool
-- anyCharIsCaps xs = getAny $ foldl (\b a -> Any (isUpper a) `mappend` b) (Any False) xs 
         
