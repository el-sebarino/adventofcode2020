import System.IO
import Data.Set as S

readIntArray :: Handle -> IO [Int]
readIntArray h = do ineof <- hIsEOF h
                    if ineof then return ([])
                      else do inpStr <- hGetLine h
                              let x = read inpStr :: Int
                              xs <- readIntArray h
                              return (x:xs)

doProblem = doProblem' S.empty where
        doProblem' m [] = 0 :: Int
        doProblem' m (x:xs) | member (2020 - x) m = x * (2020 - x)
                            | otherwise = doProblem' (insert x m) xs

main :: IO ()
main = do
        inh <- openFile "one.txt" ReadMode
        a <- readIntArray inh
        print $ doProblem a
