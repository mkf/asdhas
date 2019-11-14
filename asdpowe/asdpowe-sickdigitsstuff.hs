import Data.Char (isDigit, digitToInt)

type Digit = Int
type DNum = [Digit] -- big-endian — see the use of 'reverse' in 'prepare'

makeDigit :: Char -> Digit
makeDigit = digitToInt

isaDigit :: Digit -> Bool
isaDigit n = 0 <= n && n <= 9

verDigit :: Digit -> Digit
verDigit n
	| isaDigit n = n

prepare :: [Char] -> DNum
prepare = reverse . (map makeDigit) . (takeWhile isDigit)

theEvenDigit :: Digit -> Maybe Digit
theEvenDigit 1 = Just 0
theEvenDigit 3 = Just 2
theEvenDigit 5 = Just 4
theEvenDigit 7 = Just 6
theEvenDigit 9 = Just 8
theEvenDigit n
	| isaDigit n = Nothing

isEven :: DNum -> Bool
isEven (x:_) = theEvenDigit x == Nothing

data DigitByTwoResult = Half | Still Digit
digitByTwo :: Digit -> DigitByTwo
digitByTwo 0 = Half
digitByTwo 2 = Still 1
digitByTwo 4 = Still 2
digitByTwo 6 = Still 3
digitByTwo 8 = Still 4
digitByTwo n
	| isaDigit n = digitByTwo $ theEvenDigit n

addDigits :: Digit -> Digit -> (Digit, Maybe Digit)
addDigits a b
	| b == 0 = (a, Nothing)
	| b > a = addDigits b a
	| b == a = case b of
    	1 -> (2, Nothing)
    	2 -> (4, Nothing)
    	3 -> (6, Nothing)
    	4 -> (8, Nothing)
    	5 -> (0, Just 1)
    	6 -> (2, Just 1)
    	7 -> (4, Just 1)
    	8 -> (6, Just 1)
    	9 -> (8, Just 1)


main = do
    line <- getLine
    let a = prepare $ takeWhile (/= ' ') line
    let b = prepare $ drop 1 $ dropWhile (/= ' ') line
