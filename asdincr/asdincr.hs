import Control.Exception (assert)

mi :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
mi a [] = a
mi (p, i) ((ap, ai):xs) = mi (if p < ap then (ap, ai) else (p, i)) xs

shifted :: [Int] -> [Maybe Int]
shifted (_:xs) = (map Just xs) ++ [Nothing]

dif :: [(Int, Maybe Int)] -> [Int]
dif ((a, Just b):xs) = (abs $ (-) a b):(dif xs)
dif ((_, Nothing):_) = []

maxincr :: [Int] -> (Int, Int)
maxincr a = mi (-1, -1) $ zip (dif $ zip a $ shifted a) [0..]

intsSpaces :: [Char] -> [Int]
intsSpaces = (map read) . words

main = do
	cl <- getLine
	line <- getLine
	let c = read cl
	let l = intsSpaces line
	let (a, b) = (assert (length l == c) $ maxincr l :: (Int, Int))
	putStrLn $ (show a) ++ " " ++ (show b)
