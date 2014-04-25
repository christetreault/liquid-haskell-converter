module Main where

import System.Environment
import Dmp.Args
import Dmp.Parser
import Data.List

-- | Main. Parses arguments then calls execDmpHelper
main :: IO ()
main = do argRaw <- getArgs
          argV <- return $ parseArgV argRaw
          case argV of Left a -> printUsage
                       Right b -> execDmpHelper b

-- | Executes the parser                    
execDmpHelper :: ArgV -> IO ()
execDmpHelper argv = do inFile <- readFile $ inputFile argv
                        printIfV argv inFile
                        result <- return $ parseDmpMarkup inFile
                        case result of Left a -> printIfV argv $ show a
                                       Right b -> do printIfV argv b
                                                     writeFile (outputFile argv) b

-- | prints if --v                     
printIfV :: ArgV -> String -> IO ()
printIfV av s = if (isVerbose av) then putStrLn s else return ()
