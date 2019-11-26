module Util where

reset :: String
reset = "\ESC[0m"

mkRichText :: String -> String -> Maybe String -> String
mkRichText s n Nothing = "\ESC[" ++ n ++ "m" ++ s ++ reset
mkRichText s n (Just env) =
    "\ESC[" ++ n ++ "m" ++ s ++ reset ++ "\ESC[" ++ env ++ "m"


mkBold :: String -> Maybe String -> String
mkBold s = mkRichText s "1"

mkItalic :: String -> Maybe String -> String
mkItalic s = mkRichText s "3"

colorUnique :: Int -> Maybe String -> String -> String
colorUnique n menv s = mkRichText s (show (n `mod` 6 + 31)) menv

nthColor :: Int -> String -> Maybe String -> String
nthColor n s = mkRichText s (show n)

violetBox :: String -> Maybe String -> String
violetBox s = mkRichText s "46;1"

cyanBox :: String -> Maybe String -> String
cyanBox s = mkRichText s "47;1"
