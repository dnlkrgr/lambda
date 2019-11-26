module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           System.Console.Haskeline
import           ParseInput                     ( lambdaInteract )
import           Util


main :: IO ()
main = do
    putStr "\ESC[2J"
    putStrLn $ unlines
        [ mkBold "KIT programming paradigms" Nothing
        , mkBold "Lambda Calculus Interpreter" Nothing
        , " © dnlkrgr.com"
        , ""
        , mkItalic "Usage:" Nothing
        , "- enter lambda expressions and let them be evaluated"
        , "  by hitting enter"
        , "- quit by writing \"quit\"."
        , ""
        , mkItalic "Example lambda expressions:" Nothing
        , "number:      3"
        , "variable:    x, y, z"
        , "lambda:      /x. x, /x. /y. x"
        , "application: $ f g, $ ($ a b) c, $ f (/x.x)"
        ]
    runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine (mkBold "λ> " Nothing)
        case minput of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                output <- liftIO $ lambdaInteract input
                mapM_ outputStrLn output
                loop
