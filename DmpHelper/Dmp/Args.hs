module Dmp.Args 
(ArgV,
 parseArgV,
 inputFile,
 outputFile,
 isVerbose,
 printUsage)where

import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Data.List
import Control.Monad

-- | Prints the usage statement
printUsage :: IO ()
printUsage = do putStrLn "Usage:"
                putStrLn "DmpHelper --i [input_file] --o [output_file] [--options]"
                putStrLn "Where options include:"
                putStrLn "--v\t\tVerbose Mode"

-- | Data structure representing a valid ArgV
data ArgV = ArgV {inputFile :: FilePath,
                  outputFile :: FilePath,
                  isVerbose :: Bool} deriving (Show)

-- | Parses an ArgV                  
parseArgV :: [String] -> Either ParseError ArgV
parseArgV i = parse pArgV "" $ foldl' (++) [] i

pArgV :: CharParser st ArgV
pArgV = do permute $ ArgV <$$> pInputFile
                          <||> pOutputFile
                          <|?> (False, pIsVerbose)

pInputFile :: CharParser st FilePath
pInputFile = do try $ string "--i"
                notFollowedBy $ pEndOfArg
                res <- manyTill anyChar pEndOfArg
                guard (not $ null res)
                return res

pOutputFile :: CharParser st FilePath
pOutputFile = do try $ string "--o"
                 res <- manyTill anyChar pEndOfArg
                 guard (not $ null res)
                 return res
              
pIsVerbose :: CharParser st Bool
pIsVerbose = do try $ string "--v"
                return True

pEndOfArg :: CharParser st ()
pEndOfArg = nextArg <|> eof 
            where nextArg = do lookAhead $ try $ string "--"
                               return ()
