

tree = '#'

ski :: [String] -> Int -> Int -> Int
ski [] _ _ = 0
ski (a:as) coord m = let n = length a
                         coord' = coord `mod` n
                         ground = a !! coord'
                         score = if ground == tree then 1 else 0
                         in score + ski as (coord + m) m


main = do
        slope <- lines <$> readFile "three.txt"
        print $ ski slope 0 3
