import Control.Monad (replicateM)
import Control.Exception (assert)
-- import Debug.Trace (trace)

traceVal :: Show a => String -> a -> a
-- traceVal s v = flip trace v $ s ++ ": " ++ (show v)
traceVal _ = id

prepRow :: [Char] -> [Bool]
prepRow [] = []
prepRow (x:xs) = (case x of
    '1' -> [True]
    '0' -> [False]
    _ -> []
    ) ++ (prepRow xs)

prepMat :: [[Char]] -> [[Bool]]
prepMat = (filter $ not . null) . (map prepRow)

surroundRow :: Int -> [Bool] -> [Bool]
surroundRow cC a = [True] ++ (assert (length a == cC) a) ++ [True]

surround :: Int -> Int -> [[Bool]] -> [[Bool]]
surround rC cC a = let t = take (cC+2) $ repeat True in
                       [t] ++ (assert (length a == rC) $ map (surroundRow cC) a) ++ [t]

suring :: [Bool] -> [Bool]
suring [_] = []
suring (_:[_]) = []
suring (True:a:True:xs) = True : (suring $ a : True : xs)
suring (_:a:b:xs) = False : (suring $ a : b : xs)

threes :: [Bool] -> [Bool]
threes [_] = []
threes (_:[_]) = []
threes (True:True:True:xs) = True : (threes $ True : True : xs)
threes (_:a:b:xs) = False : (threes $ a : b : xs)

threerow :: ([Bool], ([Bool], [Bool])) -> [Int]
threerow (aa, (b1, b2)) =
    let a = suring aa
        b = map (uncurry (&&)) $ zip (threes b1) (threes b2)
    in map fst $ filter snd $ zip [0..] $ map (uncurry (&&)) $ zip a b

tup :: a -> b -> (a, b)
tup a b = (a, b)

assertLen :: Int -> [a] -> [a]
assertLen n x = assert (length x == n) x
-- assertLen _ = id

whole :: (Int, Int) -> [[Bool]] -> [(Int, Int)]
whole (rC, cC) a =
    let s = map (assertLen $ cC+2) $ assertLen (rC+2) $ traceVal "surr" $ surround rC cC a
        mid = tail $ init s
        fTop = init $ init s
        fBot = tail $ tail s
        ftb = (zip fTop fBot :: [([Bool], [Bool])])
        tro = zip mid ftb
        troa = map threerow tro
        ztroa = zip [0..] troa
    in concat $ map (\(n, x) -> (map $ tup n) x) ztroa

presentIntInt :: (Int, Int) -> String
presentIntInt (x, y) = (show x) ++ " " ++ (show y)

presentListOfIntInt :: [(Int, Int)] -> IO ()
presentListOfIntInt = (.) (foldr (>>) (return ())) $ map $ putStrLn . presentIntInt

main :: IO ()
main = do
    dimL <- (map read) <$> words <$> getLine
    let rowsCount = head dimL
    let columnsCount = head $ tail dimL
    theMat <- prepMat <$> (replicateM rowsCount getLine)
    presentListOfIntInt $ whole (rowsCount, columnsCount) theMat
