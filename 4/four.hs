import System.IO
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

-- Discovered regex! for pt 2
--  ( this module need to be installed as text-pcre )
import Text.Regex.PCRE

--
-- pt 1
--
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

--
-- pt 2
--
-- Turn a field into a Data.Map
-- getMap "aaa:bbb" = fromList [("aaa","bbb")]
getMap :: String -> M.Map k a
getMap s = let k = s =~ "(\\s+):"
               v = s =~ ":(\\s+)"
               in M.fromList [(k,v)]

--- functions that return True if the field is valid
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

--- byr field
yr s | s =~ "^\\d{4}$" = let n = read s :: Int
                            in Just n
     | otherwise = Nothing

checkyr min max s = case yr s of Just n -> n >= min && n <= max
                                 Nothing -> False

byr = checkyr 1920 2002

--- iyr field
iyr = checkyr 2010 2020

--- eyr fields
eyr = checkyr 2020 2030

-- hgt field
ht s = let h = s =~ "\\d+" :: String
        in read h

checknum :: String -> Int -> Int -> Bool
checknum s min max = let n = s =~ "\\d+" :: String
                         x = read n :: Int
                         in n >= min && n <= max

hgt s | s =~ "cm" = checknum 150 193 s  && okheight
      | s =~ "in" = checknum 59 76 s && okheight
      | otherwise = false
      where okheight = s =~ "^\\d+(cm|in)"

-- hcl field
hcl s = s =~ "^#[0-9a-f]{6}$"

-- ecl field
ecl s = s =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"

-- pid field
pid s = s =~ "^\\d{9}$"

-- Processing
checkField :: (String, String -> Bool) -> Bool
checkField (s, f) = f s

fieldOk m (k,kf) = let v = m ! k
        in checkField (k,kf) v

allFieldsOk [] = True
allFieldsOk m (x:xs) = fieldOk m x && allFieldsOk m xs

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
        -- pt 2
        let maps = M.union $ map getMaps finally_processed_ppts
        let funcs = [("byr", byr), ("iyr", iyr), ("eyr",eyr), ("hgt",hgt), ("hcl",hcl), ("ecl",ecl), ("pid",pid)]
        let oks2 = map (\x -> allFieldsOk x funcs) maps
        print $ length $ filter (== True) oks
