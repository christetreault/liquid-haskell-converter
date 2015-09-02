module Main where

import System.Environment
import Dmp.Args
import Dmp.Parser
import Control.Monad

-- | Main. Parses arguments then calls execDmpHelper
main :: IO ()
main = do
   argRaw <- getArgs
   let argV = parseArgV argRaw
   case argV of
      Left _ -> printUsage
      Right b -> execDmpHelper b

-- | Executes the parser
execDmpHelper :: ArgV
                 -> IO ()
execDmpHelper argv = do
   inFile <- readFile $ inputFile argv
   printIfV argv inFile
   let result = parseDmpMarkup inFile
   case result of Left a -> printIfV argv $ show a
                  Right b -> do
                     printIfV argv b
                     writeFile (outputFile argv) b

-- | prints if --v
printIfV :: ArgV
            -> String
            -> IO ()
printIfV av s = when (isVerbose av) (putStrLn s)
