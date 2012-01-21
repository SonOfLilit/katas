import Data.List
import Data.Maybe
import Control.Arrow
import Control.Monad

type Tuple9 a = (a, a, a, a, a, a, a, a, a)
type DigitRaw = ( (Bool, Bool, Bool)
                , (Bool, Bool, Bool)
                , (Bool, Bool, Bool))
type Digit = Maybe Int
type AccountRaw = Tuple9 DigitRaw
type Account = Tuple9 Digit

characters = lineTuplesToTuple3s . head . lineTuples . lines $ 
  " _     _  _     _  _  _  _  _ \n" ++
  "| |  | _| _||_||_ |_   ||_||_|\n" ++
  "|_|  ||_  _|  | _||_|  ||_| _|\n" ++                           
  "                              \n"
toCharacter :: DigitRaw -> Digit
toCharacter = maybe Nothing Just . flip findIndex characters . (==)

tuple3s [] = []
tuple3s (a : b : c : xs) = (a, b, c) : tuple3s xs
tuple3s _ = error "Wrong number of characters in line - not multiple of 3"

lineTuples [] = []
lineTuples (a : b : c : _ : xs) = (a', b', c') : lineTuples xs
  where a' = fromUnderscores a
        b' = fromPipes b
        c' = fromPipes c
lineTuples _ = error "Wrong number of ASCII lines - not multiple of 4"

fromUnderscores = map fromUnderscore

fromUnderscore '_' = True
fromUnderscore ' ' = False
fromUnderscore c = error $ "Illegal character: " ++ [c]

fromPipes = concat . map go . tuple3s
  where go (a, b, c) = [fromPipe a, fromUnderscore b, fromPipe c]

fromPipe '|' = True
fromPipe ' ' = False
fromPipe _ = error "Illegal character"

tuple9 :: [a] -> Tuple9 a
tuple9 [a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
tuple9 xs = error $ "Wrong number of digits in digit line - not 9: " ++ show (length xs)
untuple9 :: Tuple9 a -> [a]
untuple9 (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]

undigitraw :: DigitRaw -> [Bool]
undigitraw ( (a, b, c)
           , (d, e, f)
           , (g, h, i)) = [a, b, c, d, e, f, g, h, i]

digitraw :: [Bool] -> DigitRaw
digitraw [a, b, c, d, e, f, g, h, i] = ( (a, b, c)
                                       , (d, e, f)
                                       , (g, h, i))

lineTuplesToTuple3s (a, b, c) = tuple3s $ zip3 a b c
lineTuplesToCharacters = map toCharacter . lineTuplesToTuple3s

checksum :: Account -> Maybe Bool
checksum t9 = if length l9 == 9
              then Just $ calcChecksum l9
              else Nothing
  where l9 = catMaybes $ untuple9 t9
        calcChecksum = (==0) . (`mod` 11) . sum . zipWith (*) [1..] . reverse

fixes :: AccountRaw -> [Account]
fixes = filter ((== Just True) . checksum) . possibleFixes

possibleFixes :: AccountRaw -> [Account]
--possibleFixes = map (tuple9 . map toCharacter) . (:[]) . untuple9
possibleFixes = map (tuple9 . map toCharacter) . fix1 fix1digit . untuple9

fix1digit :: DigitRaw -> [DigitRaw]
fix1digit = map digitraw . fix1 ((:[]) . not) . undigitraw

fix1 :: (a -> [a]) -> [a] -> [[a]]
fix1 f = go
  where go xs = [before ++ [fx] ++ after | i <- [0..length xs - 1]
                                          , let (before, (x : after)) = splitAt i xs
                                          , fx <- f x]


showDigit (Just d) = show d
showDigit Nothing = "?"

printAccount account footer = do
  putStr $ showAccount account
  putStrLn footer

showAccount = concat . map showDigit . untuple9

main = do
  input <- getContents
  let accounts = map tuple9 . map lineTuplesToTuple3s . lineTuples $ lines input
  forM_ accounts $ \account -> do
    let account' = tuple9 . map toCharacter $ untuple9 account
    if checksum account' == Just True 
      then printAccount account' ""
      else case fixes account of
        [account''] -> printAccount account'' ""
        [] -> printAccount account' " ILL"
        options -> printAccount account' (" AMB " ++ show (map showAccount options))

