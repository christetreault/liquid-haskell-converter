module Dmp.Parser
(parseDmpMarkup) where

import Text.ParserCombinators.Parsec
import Data.List
import Dmp.Parser.Tags
import Control.Monad

-- | Parses a string containig "DMP Markup"
parseDmpMarkup :: String -> Either ParseError String
parseDmpMarkup mu = parse pMarkupTop "" mu

pTokenDelimiter :: CharParser st ()
pTokenDelimiter = pDelim <|> eof 
            where pDelim = do lookAhead $ try $ pMarkup--string delimiter
                              return ()

pMarkupTop :: CharParser st String
pMarkupTop = do segments <- pMarkupSegments
                return $ foldl' (++) [] segments
                         
pMarkupSegments :: CharParser st [String]
pMarkupSegments = many pMarkupSegment

pMarkupSegment :: CharParser st String
pMarkupSegment = pPlainText <|> pMarkup

pPlainText :: CharParser st String
pPlainText = do res <- manyTill anyChar pTokenDelimiter
                guard (not $ null res)
                return res

pMarkup :: CharParser st String
pMarkup = choice tags
