module Util where

reset           = "\ESC[0m"
mkBold s        = "\ESC[1m" ++ s ++ reset
mkItalic s      = "\ESC[3m" ++ s ++ reset
colorUnique n s = "\ESC[" ++ show (n `mod` 6 + 31) ++ "m" ++ s ++ reset
checkColor n s  = "\ESC[" ++ show n ++ "m" ++ s ++ reset