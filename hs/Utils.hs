{-# LANGUAGE
        FlexibleInstances, TypeSynonymInstances,
        OverlappingInstances, IncoherentInstances
  #-}

module Utils (
        showem,
        iterateIM,
        printPr,
        showListNice,
        meh,
        maybemeh,
        toFreqsWithDenom,
        selectPr,
        pickRandom,
        pickUser
    ) where

import Data.List (intersperse, sortBy)
import Data.Function (on)
import Data.Ratio (numerator, denominator)
import System.Random (randomRIO)
import Text.Printf (printf)
import Data.Char (isDigit)
import System.IO (isEOF)
import qualified Numeric.Probability.Distribution as Pr


class ShowemType t where
    showemVals :: [String] -> t

instance ShowemType String where
    showemVals strs = concat $ reverse strs

-- Put this one first so "Show a" takes precedence over String when ambiguous
instance (Show a, ShowemType r) => ShowemType (a -> r) where
    showemVals strs = (\x -> showemVals (("`" ++ show x ++ "`"):strs))

instance ShowemType r => ShowemType (String -> r) where
    showemVals strs = (\s -> showemVals (s:strs))

showem :: ShowemType t => t
showem = showemVals []


iterateIM :: (Show i, Integral i, Monad m) => i -> (a -> m a) -> (a -> m a)
iterateIM n f x | n == 0    = return x
                | n < 0     = error (showem "iterateIM given negative n " n)
                | otherwise = do
                                x2 <- iterateIM (n-1) f x
                                f x2



printPr :: (Num prob, Show prob, Ord a, Show a) => Pr.T prob a -> [String]
printPr (Pr.Cons pairs) = lines
    where
        width = maximum $ map (length . show . snd) pairs
        pad s = s ++ (take (width - length s) $ repeat ' ')
        oneline (a, pr) = (pad (show pr)) ++ "  " ++ show a
        lines = map oneline $ Pr.norm' $ sortBy (compare `on` fst) pairs


showListNice :: (Show a) => [a] -> String
showListNice [] = "[]"
showListNice xs = "[" ++ (concat $ intersperse ", " $ map show xs) ++ "]"


meh :: Monad m => m ()
meh = return ()

maybemeh :: Monad m => (a -> m b) -> Maybe a -> m ()
maybemeh act Nothing = meh
maybemeh act (Just v) = (act v >> return ())


type RandomM a = Pr.T Rational a

toFreqsWithDenom :: (Ord x) => RandomM x -> (Integer, [(x, Integer)])
toFreqsWithDenom dist = (denom, map fixPair pairs)
    where pairs = Pr.norm' (Pr.decons dist)
          denom = foldl lcm 1 (map (denominator . snd) pairs)
          fixPair (val, r) = let r' = r * fromIntegral denom in
                             case denominator r' of
                                1 -> (val, numerator r')

selectPr :: Integer -> [(x, Integer)] -> x
selectPr i []                           = error "selectPr"
selectPr i ((x, n) : rest)  | i < n     = x
                            | otherwise = selectPr (i - n)  rest

pickRandom :: (Ord v) => RandomM v -> IO v
pickRandom dist = do
                    let (denom, pairs) = toFreqsWithDenom dist
                    i <- randomRIO (0, denom - 1)
                    return $ selectPr i pairs

getIntStdin :: (Int, Int) -> IO Int
getIntStdin (min, max) = loop where
    loop = do
        putStr "> "
        eof <- isEOF
        if eof
        then fail "Unexpected EOF when reading choice"
        else do
                line <- getLine
                case line of
                    "q"                        -> fail "Quit requested"
                    (_:_) | all isDigit line   ->
                        let v = read line in
                        if min <= v && v <= max
                        then return v
                        else loop
                    _                          -> loop

pickUser :: (Ord v, Show v) => RandomM v -> IO v
pickUser dist = do
                    mapM_ (mapM_ putStrLn) choices
                    idx <- getIntStdin (1, length choices)
                    let (picked, pr) = pairs !! (idx - 1)
                    return picked
    where
        pairs = sortBy (compare `on` snd) $ Pr.norm' $ Pr.decons dist
        choices = map formatChoice $ zip pairs [1 :: Int ..]
        formatChoice ((value, pr), index) =
            [printf "%02f  choice %u" (fromRational pr :: Float) index]
            ++
            (map ("  " ++) $ lines $ show value)

