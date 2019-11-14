import Data.Char (digitToInt, intToDigit)

sumd :: [Int] -> [Int] -> ([Int], Bool)
sumd (dah:dar) (dbh:dbr) =
    let hds = dah + dbh
    	((dch:dcr), dcb) = sumd dar dbr
    	hda = hds + if dcb then dch else 0
        bo = hda >= 10
        hdl = if bo then [hda `div` 10, hda `rem` 10] else [hda]
        fu = if dcb then dcr else dch:dcr
    in (hdl ++ fu, bo)
sumd [] [] = ([0], True)

treverse :: (a, b) -> (b, a)
treverse (a, b) = (b, a)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)

leftpad :: a -> ([a], [a]) -> ([a], [a])
leftpad e (a, b) = if (length a) > (length b) then
	(a, (take ((length a) - (length b)) $ repeat e) ++ b)
	else treverse $ leftpad e (b, a)

sumc :: [Char] -> [Char] -> [Char]
sumc ca cb =
    map intToDigit $ fst $ uncurry sumd $ tmap (map digitToInt) $ leftpad '0' (ca, cb)

main = do
	a <- getLine
	b <- getLine
	putStrLn $ sumc a b
