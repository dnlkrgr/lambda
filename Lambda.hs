module Lambda (eval, runRename) where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , local
                                                , ask
                                                )
import           Data.Unique                    ( Unique
                                                , newUnique
                                                , hashUnique
                                                )
import Types
import qualified Data.Map                      as M
import Data.Either


-- | evaluates a lambda term, which is only applying beta reduction to redexes
eval :: Term' -> Term'
eval (App' f a) = case eval f of
    Lam' x body -> eval (subst body x a)
    g           -> App' g a
eval t = t

-- | performs substitution in lambda terms
--(\x. e) v = e[x / v]
subst :: Term' -> Unique -> Term' -> Term'
subst t@(Var' ident) target newValue | ident == target = newValue
                                     | otherwise       = t
subst (App' t1 t2) target newValue =
    App' (subst t1 target newValue) (subst t2 target newValue)
subst (Lam' ident term) target newValue =
    Lam' ident (subst term target newValue)

-- | rename turns a lambda term with strings for variable identifiers into 
-- our internal representation. It needs to run in IO because of Unique, 
-- which is used by the internal representation
runRename :: Term -> IO Term'
runRename t = runReaderT (rename t) M.empty

-- | rename' runs through a lambda term recursively with the current active 
-- environment of identifiers and their mapped unique values. Thanks to the
-- Reader monad keeping track of the current scope is very easy.
rename :: Term -> ReaderT (M.Map String Unique) IO Term'
rename (App t1 t2   ) = App' <$> rename t1 <*> rename t2
rename (Num n       ) = return $ Num' n
rename (Var oldIdent) = do
    env <- ask
    let mIdent = M.lookup oldIdent env
    case mIdent of
        Nothing -> do
            newIdent <- liftIO newUnique
            return $ Var' newIdent
        Just newIdent -> return $ Var' newIdent
rename (Lam oldIdent body) = do
    newIdent <- liftIO newUnique
    local (M.insert oldIdent newIdent) $ do
        newBody <- rename body
        return $ Lam' newIdent newBody