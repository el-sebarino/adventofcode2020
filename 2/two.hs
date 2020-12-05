import System.IO
import Text.ParserCombinators.Parsec

count_chars c = length . (filter (== c))

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
                let n = count_chars c p
                if n >= l && n <= h then return p else return ""



main :: IO ()
main = do
        password_entries <- lines <$> readFile "two.txt"
        let parsed_passwords = map (parse checkpass "") password_entries
        -- FIXME: more straighforward way to wrangle parser over password list
        let ok_passwords = filter (/= Right "") parsed_passwords
        print $ length  ok_passwords
