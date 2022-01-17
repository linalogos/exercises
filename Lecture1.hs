{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lecture1
(--makeSnippet
 sumOfSquares
, lastDigit
, minmax
, subString
, strSum
, lowerAndGreater
, increase
) where




increase :: Integer -> Integer
increase x = x + 1

sumOfSquares :: Integer -> Integer -> Integer
sumOfSquares n1 n2 = ( n1 ^ 2 ) + (n2 ^ 2)

--finding last digit = finding the remainder when this number is divided by 10
lastDigit :: Integer -> Integer
lastDigit n = l
 where
     l = mod n 10

minmax :: Integer -> Integer -> Integer -> Integer
minmax n1 n2 n3 = l1 - l2
    where
       l = [n1, n2, n3]
       l1 = maximum l
       l2 = minimum l

subString :: Int -> Int -> [Char] -> [Char]
subString n1 n2 s1 = s
    where
        s2 = drop n1 s1
        s = take (n2-n1) s2

strSum :: String -> Int
strSum s = n
    where
        s1 = words s
        intList = map (read::String -> Int) s1
        sumList :: [Int] -> Int
        sumList [] = 0
        sumList (x:intList) = x + sumList intList
        n = sumList intList

lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater n1 n2 = s
   where
        count1 :: Int -> [Int] -> Int
        count1 result1 n2 =
            if head n2 == n1
            then result1 
             else if head n2 > n1
             then count1 (result1+1) (tail n2)
             else count1 result1 (tail n2)
    

        count2 :: Int -> [Int] -> Int
        count2 result2 n2 = 
            if head n2 == n1
            then result2
             else if head n2 < n1 
             then count2 (result2+1) (tail n2)
             else  count2 result2 (tail n2)
         

        s = show n1 ++ " is greater than " ++ show (count1  n1 n2 - 1 ) ++ " elements and lower than " ++ show (count2 n1 n2 + 1) ++ " elements"
























