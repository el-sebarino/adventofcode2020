import Data.Array

tree = '#'

numTrees dx dy slope = let
        ((_,_),(mX, mY)) = bounds slope
        inds = [1..(mX `div` dx)]
        path = [ slope ! ( dx * i, (dy*i) `mod` (mY + 1)) | i <-  inds ]
   in length $ filter (== tree) path

main = do
        slope <- lines <$> readFile "three.txt"
        let maxX = length slope
        let maxY = length $ head slope
        let a = listArray ((0,0),(maxX-1,maxY-1)) $ concat slope
        let r1 = numTrees 1 1 a
        let r2 = numTrees 1 3 a
        let r3 = numTrees 1 5 a
        let r4 = numTrees 1 7 a
        let r5 = numTrees 2 1 a
        print $ r1 * r2 * r3 * r4 * r5
