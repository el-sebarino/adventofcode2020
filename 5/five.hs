import Data.List

toSeat s = let exp = 2 ^ (length s - 1)
   in toSeat' exp s
     where toSeat' x (c:s) | c `elem` "FL" = toSeat' x' s
                           | c `elem` "BR" = x + toSeat' x' s
                where x' = x `div` 2
           toSeat' x _ = 0

getId s = let row = toSeat $ take 7 s
              col = toSeat $ drop 7 s
              in row * 8 + col

-- pt 2
-- Algorithmically, with a random access list it would be better to do a binary search.
-- The list is small, so just do this:
findMissingNum (a:b:xs) | b - a == 1 = findMissingNum (b:xs)
                        | b - a == 2 = a + 1
findMissingNum _ = -1

main :: IO ()
main = do
        seats <- lines <$> readFile "five.txt"
        let ids = map getId seats
        -- pt 1
        print $ foldl max 0 ids
        -- pt 2
        print $ findMissingNum $ sort ids
