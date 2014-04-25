module Main where

import System.Environment
import Dmp.Args
import Dmp.Parser
import Data.List

main :: IO ()
main = do argRaw <- getArgs
          argV <- return $ parseArgV argRaw
          case argV of Left a -> printUsage
                       Right b -> execDmpHelper b
                    
execDmpHelper :: ArgV -> IO ()
execDmpHelper argv = do inFile <- readFile $ inputFile argv
                        printIfV argv inFile
                        result <- return $ parseDmpMarkup inFile
                        case result of Left a -> printIfV argv $ show a
                                       Right b -> do printIfV argv b
                                                     writeFile (outputFile argv) b
                                       
printIfV :: ArgV -> String -> IO ()
printIfV av s = if (isVerbose av) then putStrLn s else return ()
