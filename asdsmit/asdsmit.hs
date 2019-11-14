divisible :: Int -> Int -> Bool
divisible a b = a `mod` b == 0

sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits a = (a `rem` 10) + (sumOfDigits $ a `div` 10)

primeFactorsFrom :: Int -> Int -> [Int]
primeFactorsFrom _ 1 = []
primeFactorsFrom t n
	| t <= n = 
    	if divisible n t
        then t : (primeFactorsFrom t $ n `div` t)
    	else primeFactorsFrom (t+1) n

primeFactors :: Int -> [Int]
primeFactors = primeFactorsFrom 2

sumOfPFDigits :: Int -> Int
sumOfPFDigits a = sum $ map sumOfDigits $ primeFactors a

isSmith :: Int -> Bool
isSmith a = (sumOfDigits a) == (sumOfPFDigits a)

main = do
    n <- getLine
    putStrLn $ if isSmith $ read n then "1" else "0"
