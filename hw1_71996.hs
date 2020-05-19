import Data.List

main :: IO()
main = do

    print (findsum 0 2 10)
    print (isSquare 100)
    print (isSpecial 131 2)

    return ()

--task 1
findsum :: Int -> Int -> Int -> Int
findsum a b n 
    | n <= 3 = error "It doesn't satisfy the condition" -- validation
    | otherwise = (findpartial a b (n-1)) + (findpartial a b (n-2)) + (findpartial a b (n-3)) --finds the sum of the last three elements
    where --by using a new function we find the partial sum in each bracket with recursion
    findpartial :: Int -> Int -> Int -> Int
    findpartial a b num 
        | num < 0 = error "No negative integers" -- end of recursion
        | num == 0 = a + b   -- not sure if needed, but is a valid case
        | otherwise = (2^num)*b + (findpartial a b (num-1)) --this is the partial sum for each bracket

--task 2
isSquare :: Int -> Bool
isSquare x = helper x 1
--By using the helper function we check whether there is an existing number which multiplied by itself gives us x
    where
    helper :: Int -> Int -> Bool
    helper x n
        | x < n = False --if the number n becomes greater than x it means that there is no such number that satifies the condition and is also validation for x being negative
        | n * n == x = True -- if the number n multiplied by itself gives the number x it means x satifies the condition
        | otherwise = helper x (n + 1) -- here we make the number n bigger so we can check for every number smaller than x    

--task 3 idk why but it doesn't work
isSpecial :: Int -> Int -> Bool
isSpecial n k 
    | n <= 10 = error "Number must be bigger than 10"  --validation
    | n < 10^k = isPrime n -- end of recursion
    | otherwise = isPrime n `mod` (10^k) -- (131 2) - > 131 mod 100(10^k) = 31 first pair -> div 131 10 -> mod 13 100 -> second pair

isPrime :: Int -> Bool
isPrime n = helper n 2
 where
     helper :: Int -> Int -> Bool
     helper n currentDivisor
      | currentDivisor >= n = True && isSpecial (n `div` 10) k
      | mod n currentDivisor == 0 = False
      | otherwise = helper n (currentDivisor + 1)