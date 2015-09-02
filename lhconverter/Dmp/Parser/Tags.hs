{-# LANGUAGE OverloadedStrings #-}

-- | Convert old style comment delimiters to new style quasiquote delimiters
module Dmp.Parser.Tags
(tags) where

import Text.ParserCombinators.Parsec
import Data.List

oldOpen :: String
oldOpen = "{-@"

pOldOpen :: CharParser st String
pOldOpen = string oldOpen

openDelim :: String
openDelim = "[lq|"

pOpenDelim :: CharParser st String
pOpenDelim = string openDelim

oldClose :: String
oldClose = "@-}"

pOldClose :: CharParser st String
pOldClose = string oldClose

pIs :: String -> CharParser st String
pIs s = lookAhead $ try $ string s

closeDelim :: String
closeDelim = "|]"

pCloseDelim :: CharParser st String
pCloseDelim = string closeDelim

delimitOld :: String -> String
delimitOld s = oldOpen ++ s ++ oldClose

delimitNew :: String -> String
delimitNew s = openDelim ++ s ++ closeDelim

-- | A list of Tag parsers exported from this module
tags :: [CharParser st String]
tags = [convertOldConfig, convertOldTag]

-- | Converts "any" old-style tag. {-@ foo @-} becomes [lq| foo |]
convertOldTag :: CharParser st String
convertOldTag = try $ do
   tag <- between pOldOpen pOldClose content
   return $ delimitNew tag
   where
      content = anyChar `manyTill` pIs oldClose

-- | Converts an old-style LH config tag: {-@ LIQUID "--foo --bar" @-}
-- becomes [lq| config foo,bar |]
convertOldConfig :: CharParser st String
convertOldConfig = try $ do
   opts <- between pOldOpen pOldClose content
   assemble opts
   where
      assemble :: (Monad m) => [String] -> m String
      assemble o = do -- TODO: for now, not modifying this until spinda implements the config tags
         let optsStr = intercalate " --" o
         return $ delimitOld $ " LIQUID --" ++ optsStr ++ " "
         --let optsStr = intercalate "," o
         --return $ delimitNew $ " config " ++ optsStr ++ " "
      content = do
         spaces
         string "LIQUID"
         spaces
         res <- between (char '"') (char '"') options
         spaces
         return res
      options = option' `manyTill` lookAhead (try $ char '"')
      option' = do
         spaces
         string "--"
         res <- anyChar `manyTill` lookAhead (try space <|> try (char '"'))
         spaces
         return res
