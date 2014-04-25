{-# LANGUAGE OverloadedStrings #-}

-- | This module contains "tag" definitions. If you want to implement new tags, do it in here and add
-- your tags to the list in the tags function
module Dmp.Parser.Tags
(tags,
 delimiter) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R
import Text.Blaze ((!))
import Text.ParserCombinators.Parsec

-- | the delimiter that demarks the start of a Markup tag
delimiter :: String
delimiter = "`"

-- | Helper function that encloses a passed-in string within the delimiter
delimit :: String -> String
delimit s = delimiter ++ s ++ delimiter

-- | A list of Tag parsers
tags :: [CharParser st String]
tags = [pImg, pCode, pLink, pEscape, pSnippet]

pImg :: CharParser st String
pImg = do try $ string $ delimit "img"
          return $ R.renderHtml tImg
       where tImg = H.img ! A.src "PLACEHOLDER"
       
pCode :: CharParser st String
pCode = do try $ string $ delimit "code"
           content <- manyTill anyChar $ try $ string $ delimit "ecode"
           return $ R.renderHtml $ tCode content
        where tCode a = H.pre ! A.class_ "code"
                              $ do H.span $ do H.div ! A.class_ "code"
                                                     $ do H.code $ H.toMarkup a
                                                     
pLink :: CharParser st String
pLink = do try $ string $ delimit "ba"
           pHref <- manyTill anyChar $ try $ string $ delimit "bae"
           pLinkText <- manyTill anyChar $ try $ string $ delimit "ea"
           return $ R.renderHtml $ tLink pHref pLinkText
        where tLink h t = H.a ! A.href (H.toValue h)
                              $ H.toMarkup t
                              
pEscape :: CharParser st String
pEscape = do try $ string $ delimit "esc"
             toEscape <- manyTill anyChar $ try $ string $ delimit "eesc"
             return $ R.renderHtml $ H.toMarkup toEscape
             
pSnippet :: CharParser st String
pSnippet = do try $ string $ delimit "snip"
              toSnippet <- manyTill anyChar $ try $ string $ delimit "esnip"
              return $ R.renderHtml $ H.code $ H.toMarkup toSnippet
