import System.IO
import Data.Set as S

readIntArray :: Handle -> IO [Int]
readIntArray h = do ineof <- hIsEOF h
                    if ineof then return ([])
                      else do inpStr <- hGetLine h
                              let x = read inpStr :: Int
                              xs <- readIntArray h
                              return (x:xs)

-- Finding two elements of array that sum to a number t
doSubProblem = doSubProblem' S.empty where
        doSubProblem' m t [] = Nothing
        -- Since member is false for head of array, it'll always insert and look for two numbers
        doSubProblem' m t (x:xs) | member (t - x) m = Just $ x * (t - x)
                                 | otherwise = doSubProblem' (insert x m) t xs

doProblem [] = Nothing
doProblem (x:xs) = case doSubProblem (2020 - x) xs of Nothing -> doProblem xs
                                                      Just y -> Just $ x * y
main :: IO ()
main = do
        inh <- openFile "one.txt" ReadMode
        a <- readIntArray inh
        case doProblem a of Nothing -> print "No answer found"
                            Just x -> print x
