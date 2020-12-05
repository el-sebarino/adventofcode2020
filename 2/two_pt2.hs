import System.IO
import Text.ParserCombinators.Parsec

xor a b = (a || b) && not (a && b)

f n1 n2 c p | length p < 3 = False
            | otherwise = let c1 = p !! (n1 - 1)
                              c2 = p !! (n2 - 1)
                          in xor (c1 == c) (c2 == c)

-- FIXME: part 1 & 2 in one file
checkpass =
        do
                low_range <- many digit
                char '-'
                high_range <- many digit
                space
                c <- anyChar
                char ':'
                space
                p <- many anyChar
                let l = read low_range :: Int
                let h = read high_range :: Int
                let ok = f l h c p
                if ok then return p else return ""



main :: IO ()
main = do
        password_entries <- lines <$> readFile "two.txt"
        let parsed_passwords = map (parse checkpass "") password_entries
        -- FIXME: more straighforward way to wrangle parser over password list
        let ok_passwords = filter (/= Right "") parsed_passwords
        print $ length  ok_passwords
