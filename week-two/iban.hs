module Iban where

import Prelude hiding (lookup)
import Data.Char
import Data.Map (Map, fromList, lookup, member)
import Data.Maybe

countryRegistry :: Map [Char] Int
countryRegistry = fromList
  [ ("AL", 28), ("AT", 20), ("BE", 16), ("BA", 20), ("BG", 22), ("HR", 21)
  , ("CZ", 24), ("DO", 28), ("FO", 18), ("FR", 27), ("DE", 22), ("GR", 27)
  , ("GT", 28), ("IS", 26), ("IL", 23), ("KZ", 20), ("LV", 21), ("LI", 21)
  , ("LU", 20), ("MT", 31), ("MU", 30), ("MD", 24), ("NL", 18), ("PK", 24)
  , ("PL", 28), ("RO", 24), ("SA", 24), ("SK", 24), ("ES", 24), ("CH", 21)
  , ("TR", 26), ("GB", 22), ("AD", 24), ("AZ", 28), ("BH", 22), ("BR", 29)
  , ("CR", 21), ("CY", 28), ("DK", 18), ("EE", 20), ("FI", 18), ("GE", 22)
  , ("GI", 23), ("GL", 18), ("HU", 28), ("IE", 22), ("IT", 27), ("KW", 30)
  , ("LB", 28), ("LT", 20), ("MK", 19), ("MR", 27), ("MC", 27), ("ME", 22)
  , ("NO", 15), ("PS", 29), ("PT", 25), ("SM", 27), ("RS", 22), ("SI", 19)
  , ("SE", 24), ("TN", 24), ("AE", 23), ("VG", 24)
  ]

rearrangePrefix :: String -> String
rearrangePrefix str = (drop 4 str) ++ (take 4 str)

convertLetters :: String -> String
convertLetters "" = ""
convertLetters (first:rest)
  | isAlpha first = show ((ord first) - 55) ++ convertLetters rest
  | otherwise     = first:(convertLetters rest)

stringToInteger :: String -> Integer
stringToInteger str = read str :: Integer

iban :: String -> Bool
iban str = validCharacters && validCountry && validLength && checkModulus
  where
    validCharacters = all (\c -> isAlphaNum c) str
    ibanCountry = take 2 str
    validCountry = member ibanCountry countryRegistry
    validLength = length str == fromMaybe (-1) (lookup ibanCountry countryRegistry)
    checkModulus = stringToInteger (convertLetters (rearrangePrefix str)) `mod` 97 == 1

validIbans :: [String]
validIbans =
  [ "AD1200012030200359100100"
  , "AE070331234567890123456"
  , "AL47212110090000000235698741"
  , "AT611904300234573201"
  , "BA391290079401028494"
  , "BE68539007547034"
  , "BG80BNBG96611020345678"
  , "BH67BMAG00001299123456"
  , "CH9300762011623852957"
  , "CY17002001280000001200527600"
  , "CZ6508000000192000145399"
  , "DE89370400440532013000"
  , "DK5000400440116243"
  , "DO28BAGR00000001212453611324"
  , "EE382200221020145685"
  , "ES9121000418450200051332"
  , "FI2112345600000785"
  , "FO7630004440960235"
  , "FR1420041010050500013M02606"
  , "GB29NWBK60161331926819"
  , "GE29NB0000000101904917"
  , "GI75NWBK000000007099453"
  , "GL4330003330229543"
  , "GR1601101250000000012300695"
  , "HR1210010051863000160"
  , "HU42117730161111101800000000"
  , "IE29AIBK93115212345678"
  , "IL620108000000099999999"
  , "IS140159260076545510730339"
  , "IT60X0542811101000000123456"
  , "KW81CBKU0000000000001234560101"
  , "KZ86125KZT5004100100"
  , "LB62099900000001001901229114"
  , "LI21088100002324013AA"
  , "LT121000011101001000"
  , "LU280019400644750000"
  , "LV80BANK0000435195001"
  , "MC1112739000700011111000H79"
  , "ME25505000012345678951"
  , "MK07300000000042425"
  , "MR1300020001010000123456753"
  , "MT84MALT011000012345MTLCAST001S"
  , "MU17BOMM0101101030300200000MUR"
  , "NL91ABNA0417164300"
  , "NO9386011117947"
  , "PL27114020040000300201355387"
  , "PT50000201231234567890154"
  , "RO49AAAA1B31007593840000"
  , "RS35260005601001611379"
  , "SA0380000000608010167519"
  , "SE3550000000054910000003"
  , "SI56191000000123438"
  , "SK3112000000198742637541"
  , "SM86U0322509800000000270100"
  , "TN5914207207100707129648"
  , "TR330006100519786457841326"
  ]

-- We generate an invalid IBAN starting from a valid IBAN and changing its
-- second check digit. We change this specific digit because:
--  - it is always a number, hence easy to change;
--  - it becomes the last character after rearranging the original IBAN, hence
--    it easy to state the modulus will return a different value
generateInvalidIban :: String -> String
generateInvalidIban validIban = prefix ++ (show new_digit) ++ suffix
  where
    (prefix, (digit:suffix)) = splitAt 3 validIban
    new_digit = 10 - (digitToInt digit) + 1

invalidIbans :: [String]
invalidIbans = map (\i -> generateInvalidIban i) validIbans

testValidIbans :: Bool
testValidIbans = all (\i -> iban i) validIbans

-- *Iban> testValidIbans
-- True
-- (0.02 secs, 1,976,240 bytes)

testInvalidIbans :: Bool
testInvalidIbans = all (\i -> not (iban i)) invalidIbans

-- *Iban> testInvalidIbans
-- True
-- (0.03 secs, 1,692,688 bytes)
