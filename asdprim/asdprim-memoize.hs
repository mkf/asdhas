divisible :: Int -> Int -> Bool
divisible a b = a `mod` b == 0

whetherl :: [Bool]
whetherl = map isPrime [2..]

primesToExcl :: Int -> [Int]
primesToExcl n = map snd $ filter fst $ takeWhile (((floor $ sqrt $ fromIntegral n) >).snd) $ zip whetherl [2..]

whethere :: Int -> Bool
whethere n
	| n >= 2 = whetherl !! (n-2)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = and $ map (not.(divisible n)) $ primesToExcl n

main = do
    l <- getLine
    putStrLn $ if whethere $ read l then "1" else "0"
