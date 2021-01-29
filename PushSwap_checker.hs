{-
--EPITECH PROJECT, 2021
--FUN_SEMINAR
--File description:
--pushSwap_checker.hs
-}

import System.Environment
import System.Exit

sa :: [Int] -> [Int]
sa [] = []
sa [a] = [a]
sa (a:b:l) = b : a : l

ra :: [Int] -> [Int]
ra [] = []
ra (x:l) = l ++ [x]

rra :: [Int] -> [Int]
rra [] = []
rra l = last l : init l

pa :: [Int] -> [Int] -> [String] -> Maybe ([Int], [Int])
pa la [] sl = pushSwap la [] sl
pa la lb sl = pushSwap (head lb:la) (tail lb) sl

pb :: [Int] -> [Int] -> [String] -> Maybe ([Int], [Int])
pb [] lb sl = pushSwap [] lb sl
pb la lb sl = pushSwap (tail la) (head la:lb) sl

pushSwapBis :: [Int] -> [Int] -> [String] -> Maybe ([Int], [Int])
pushSwapBis [] [] _  = Just ([], [])
pushSwapBis la lb [] = Just (la, lb)
pushSwapBis la lb (s:sl) = case s of
                            "ra"  -> pushSwap (ra la) lb sl
                            "rb"  -> pushSwap la (ra lb) sl
                            "rr"  -> pushSwap (ra la) (ra lb) sl
                            "rra" -> pushSwap (rra la) lb sl
                            "rrb" -> pushSwap la (rra lb) sl
                            "rrr" -> pushSwap (rra la) (rra lb) sl
                            _     -> Nothing

pushSwap :: [Int] -> [Int] -> [String] -> Maybe ([Int], [Int])
pushSwap [] [] _  = Just ([], [])
pushSwap la lb [] = Just (la, lb)
pushSwap la lb (s:sl) = case s of
                            "sa" -> pushSwap (sa la) lb sl
                            "sb" -> pushSwap la (sa lb) sl
                            "sc" -> pushSwap (sa la) (sa lb) sl
                            "pa" -> pa la lb sl
                            "pb" -> pb la lb sl
                            _    -> pushSwapBis la lb (s:sl)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt a = case reads a :: [(Int, String)] of
                  [(nbr, "")] -> Just nbr
                  _           -> Nothing

atoiList :: [String] -> Maybe [Int]
atoiList = traverse readInt

checkSort :: [Int] -> Bool
checkSort [] = True
checkSort [_] = True
checkSort (x:y:l) | x <= y = checkSort (y : l)
                  | otherwise = False

printResult :: Maybe ([Int], [Int]) -> IO ()
printResult Nothing = exitWith (ExitFailure 84)
printResult (Just ([], [])) = putStrLn "OK"
printResult (Just ([], _)) = putStrLn "KO"
printResult (Just (la, [])) | checkSort la = putStrLn "OK"
printResult (Just l) = putStr "KO: " >> print l

test :: Maybe [Int] -> IO ()
test Nothing = exitWith (ExitFailure 84)
test _ = putStr ""

getMaybe :: Maybe [Int] -> [Int]
getMaybe Nothing = []
getMaybe (Just a) = a

main :: IO ()
main = do
        operands <- words <$> getLine
        numbers <- atoiList <$> getArgs
        test numbers
        printResult (pushSwap (getMaybe numbers) [] operands)