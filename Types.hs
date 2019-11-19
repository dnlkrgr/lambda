module Types where

import           Data.Unique
import Util

data Term = Num Integer      -- number
          | Var String       -- variable
          | Lam String Term  -- abstraction
          | App Term Term    -- function application
          deriving Show

-- | we need the Unique type to distinguish variables with the same 
-- identifier but different scopes
data Term' = Num' Integer
           | Var' Unique
           | Lam' Unique Term'
           | App' Term' Term'

instance Show Term' where
    show (Num' n) = mkItalic (show n)
    show (Var' i) = showUnique i
    show (Lam' i body) =
        mkBold "(" ++ "\\" ++ showUnique i ++ ". " ++ show body ++ mkBold ")"
    show (App' t1 t2) = show t1 ++ " " ++ show t2

showUnique :: Unique -> String
showUnique u = colorUnique n . (: []) . (['a' .. 'z'] !!) . (`mod` 26) $ n
        where n = hashUnique u
