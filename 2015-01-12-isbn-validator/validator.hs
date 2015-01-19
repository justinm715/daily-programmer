-- http://www.reddit.com/r/dailyprogrammer/comments/2s7ezp/20150112_challenge_197_easy_isbn_validator/

import Data.Char (digitToInt)

isValidISBN :: String -> Bool
isValidISBN isbn = 
  let is_keepable_char x = x `elem` 'X':['0'..'9']
      mult (pos,dig)
        | dig == 'X' && pos == 1 = 10
        | otherwise  = pos * digitToInt dig
      sum' = sum $ map mult $ zip [10,9..] $ filter is_keepable_char isbn
  in sum' `mod` 11 == 0