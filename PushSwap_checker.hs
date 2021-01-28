{-
--EPITECH PROJECT, 2021
--FUN_SEMINAR
--File description:
--pushSwap_checker.hs
-}

import System.Environment
--import System.Exit

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

pushSwap :: [Int] -> [Int] -> [String] -> ([Int], [Int])
pushSwap la lb [] = (la, lb)
pushSwap la lb (s:sl) =
                    case s of
                        "sa"  -> pushSwap (sa la) lb sl
                        "sb"  -> pushSwap la (sa lb) sl
                        "sc"  -> pushSwap (sa la) (sa lb) sl
                        "pa"  -> pushSwap (head lb:la) (tail lb) sl
                        "pb"  -> pushSwap (tail la) (head la:lb) sl
                        "ra"  -> pushSwap (ra la) lb sl
                        "rb"  -> pushSwap la (ra lb) sl
                        "rr"  -> pushSwap (ra la) (ra lb) sl
                        "rra" -> pushSwap (rra la) lb sl
                        "rrb" -> pushSwap la (rra lb) sl
                        "rrr" -> pushSwap (rra la) (rra lb) sl
                        _ -> (la, lb)

atoiList :: [String] -> [Int]
atoiList = map read

checkSort :: [Int] -> Bool
checkSort [] = True
checkSort [_] = True
checkSort (x:y:l) | x < y = checkSort (y : l)
                  | otherwise = False

printResult :: ([Int], [Int]) -> IO ()
printResult (la, []) | checkSort la = putStrLn "OK"
printResult l = putStr "KO: " >> print l

main :: IO ()
main = do
        numbers <- atoiList <$> getArgs
        printResult (pushSwap numbers [] ["sa", "pb", "pb", "pb", "sa", "pa", "pa", "pa"])