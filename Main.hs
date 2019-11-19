module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           System.Console.Haskeline
import           ParseInput (lambdaInteract)
import Util


main :: IO ()
main = do
    putStr "\ESC[2J"
    putStrLn $ unlines
        [ 
          mkBold "KIT programming paradigms Lambda Calculus Interpreter"
        , ""
        , mkItalic "Usage:"
        , "- enter lambda expressions and let them be evaluated"
        , "  by hitting enter"
        , "- quit by writing \"quit\"."
        , ""
        , mkItalic "Example lambda expressions:"
        , "number:      3"
        , "variable:    x"
        , "lambda:      /x. x"
        , "application: $ f g, $ ($ a b) c"
        ]
    runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine (mkBold "λ> ")
        case minput of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                output <- liftIO $ lambdaInteract input
                outputStrLn output
                loop