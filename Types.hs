module Types where

import           Data.Unique (Unique, hashUnique)
import           Util
import           Data.Map                      as M (Map, lookup)
import Data.Maybe (fromMaybe)

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

showTerm :: M.Map Unique String -> Term' -> String
showTerm m (Num' n      ) = mkItalic (show n)
showTerm m (Var' i      ) = showUnique m i
showTerm m (Lam' i  body) = mkBold "(" ++ "\\" ++ showUnique m i ++ " . " ++ showTerm m body ++ mkBold ")"
showTerm m (App' t1 t2  ) = mkBold "$ " ++ showTerm m t1 ++ " " ++ showTerm m t2

showUnique :: M.Map Unique String -> Unique -> String
showUnique m u = colorUnique n . fromMaybe "arst" $ M.lookup u m
    where n = hashUnique u
