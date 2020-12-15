import System.IO
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

-- parser to split the test file by empty lines
eol = do
        char '\n'
        return ()

ppt_separator = do
        eol
        eol <|> eof
        return ()

ppt_entry = manyTill anyChar (try ppt_separator)

ppt_entries = many ppt_entry

get_entries s = let parsed_ppts = parse ppt_entries "" s
        in case parsed_ppts of Right stuff -> stuff
                               Left err -> [[]]

-- extra processing not using parser
sep_entry  = map (fst . break (== ':') ) . words

has_elems es l = let oks = map (\x -> (x `elem` l ) || (x == "cid")) es
        in all (== True) oks

-- main
main :: IO ()
main = do
        ppts <- readFile "four.txt"
        -- TODO: alternative methods to process file, should be way easier
        let parsed_ppts = get_entries ppts
        let finally_processed_ppts = map sep_entry parsed_ppts
        let elems = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        let oks =  map (has_elems elems) finally_processed_ppts
        print $ length $ filter (== True) oks
