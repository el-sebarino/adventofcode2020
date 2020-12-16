import System.IO
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

-- Discovered regex! for pt 2
--  ( this module need to be installed as text-pcre )
import Text.Regex.PCRE

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

-- update: for pt 2, discovered regexp module
-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.
--

yr :: String -> Maybe Int
yr s | s =~ "^\\d{4}$" = let n = read s :: Int
                            in Just n
     | otherwise = Nothing

checkyr min max s = case yr s of Just n -> n >= min && n <= max
                                 Nothing -> False

byr = checkyr 1920 2002

iyr = checkyr 2010 2020

eyr = checkyr 2020 2030

ht :: String -> Int
ht s = let h = s =~ "\\d+" :: String
        in read h

checknum :: String -> Int -> Int -> Bool
checknum s min max = let n = s =~ "\\d+" :: String
                         x = read n :: Int
                         in n >= min && n <= max

-- TODO
hgt s = let okheight = s =~ "^\\d+(cm|in)$"
            in if okheight then if s =~ "cm" then checknum 150 193 s else if s =~ "in" then checknum 59 76 s else False


hcl s = s =~ "^#[0-9a-f]{6}$"

ecl s = s =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"

pid s = s =~ "^\\d{9}$"


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
