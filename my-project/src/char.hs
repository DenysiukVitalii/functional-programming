module MyChar where

import Prelude hiding (isDigit, isUpper, isLower, isAlpha)
isDigit, isUpper, isLower, isAlpha, isSpace :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
isUpper c = c >= 'A' && c <= 'Z'
isLower c = c >= 'a' && c <= 'z'
isAlpha c = isUpper c || isLower c 
isSpace c = c == ' '