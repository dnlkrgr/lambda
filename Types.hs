module Types where

import           Data.Unique                    ( Unique
                                                , hashUnique
                                                )
import           Util
import           Data.Map                      as M
                                                ( Map
                                                , lookup
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.Reader

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
           | Redex Term' Term'

showTerm :: M.Map Unique String -> Term' -> Reader (Maybe String) String
showTerm _ (Num' n) = asks (mkItalic (show n))
showTerm m (Var' i) = asks (showUnique m i)
showTerm m (Lam' i body) = do
    menv <- ask
    bodyString <- showTerm m body
    pure $ mkBold "(" menv ++ "\\" ++ showUnique m i menv ++ " . " ++ bodyString ++ mkBold ")" menv
showTerm m (App' t1 t2)  = do
    menv <- ask
    t1String <- showTerm m t1 
    t2String <- showTerm m t2
    pure $ mkBold "(" menv ++ t1String ++ " " ++ t2String ++ mkBold ")" menv
showTerm m (Redex t1 t2) = do
    menv <- ask
    t1String <- local (const (Just "46;1")) $ showTerm m t1
    t2String <- local (const (Just "47;1")) $ showTerm m t2
    pure $ violetBox t1String menv ++ " " ++ cyanBox t2String menv

showUnique :: M.Map Unique String -> Unique -> Maybe String -> String
showUnique m u menv = colorUnique n menv . fromMaybe errMsg $ M.lookup u m
    where n = hashUnique u
          errMsg = "Error: could not regenerate string from unique identifier"
