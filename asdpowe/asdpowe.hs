import Data.Bits (shiftL, shiftR, (.&.))

sL = flip shiftL 1
sR = flip shiftR 1

mul :: Int -> Int -> Int
mul a b
	| b == 0 = 0
	| b == 1 = a
	| b > a = mul b a
	| otherwise = (sL $ mul a $ sR b) + (mul a $ b .&. 1) 

pow :: Int -> Int -> Int
pow a b
	| b == 0 = 1
	| b == 1 = a
	| b .&. 1 == 1 = mul (pow a $ b - 1) a
	| otherwise = let c = pow a $ sR b in mul c c

main = do
    line <- getLine
    let a = (read (takeWhile (/= ' ') line) :: Int)
    let b = (read (drop 1 $ dropWhile (/= ' ') line) :: Int)
    putStrLn $ show $ pow a b
