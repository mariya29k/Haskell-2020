
main :: IO()
main = do
    
    --task 1
    print $ generate 1 3
    print $ generate 0.1 5

    --task 2
    print $ issquareList 1 25
    print $ issquareList 250 300

    --task 3
    print $ splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]
    print $ splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]
  
    --task 4

    return ()

--task 1
generate :: Float -> Int -> [Float] -- p is a real number, n is a natural number, and the result is a real number
generate p n = if n < 1 then error "Error" else [number (fromIntegral x) p | x <- [1..n]]

number :: Float -> Float -> Float
number 1 p = 1
number x p = 1.0 / (x ** p) + number (x-1) p

--task 2
--function that check whether a number is a square root // we used it in hw1
isSquare :: Int -> Bool
isSquare n = length [x | x <- [0 .. n], x * x == n] == 1

--the first element is a square root from the interval [a,b]
issquareList :: Int -> Int -> [(Int,Int)]
issquareList a b = if a >= b then error "Error" else [(x, (division x)) | x <- [a .. b], isSquare x] --izvejda vs tochni kvadrati
    where
        division :: Int -> Int
        division x = sum ( [y^2 | y <- [1 .. x], mod x y == 0] ) -- vtoriq element e sumata na delitelite na chisloto x, povdignati na kvadrat


--task 3
type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] ->  ([Point], [Point])
splitPoints p r ps = if ps == [] then error "Error" else (inCircle p r ps, notInCircle p r ps)

inCircle :: Point -> Double -> [Point] -> [Point]
inCircle (x,y) r ps = [(one, two) | (one, two) <- ps, (((x-one)^2 + (y-two)^2) <= r^2)]

notInCircle :: Point -> Double -> [Point] -> [Point]
notInCircle (x,y) r ps = [(one, two) | (one, two) <- ps, (((x-one)^2 + (y-two)^2) > r^2)] 

--task 4
--type Account = (Int, Int, Double) --id account, id person, balance
--type Person = (Int, String, String) --id person, name, address
--type Database = ([Account], [Person]) -- idk if necessary

--getAverageBalance :: Database -> (Person->Bool)-> Double

