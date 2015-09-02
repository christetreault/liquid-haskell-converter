module Dmp.SourceUpdate
(updateSource) where

import Text.ParserCombinators.Parsec

qqString :: String
qqString = "\n{-# LANGUAGE QuasiQuotes #-}\n"

lhString :: String
lhString = "\n\nimport LiquidHaskell\n"

updateSource :: String -> Either ParseError String
updateSource = parse pSourceTop ""

pSourceTop :: CharParser st String
pSourceTop = do
   header <- pHeader
   imports <- pImports
   return $ qqString ++ header ++ lhString ++ imports


pIsImport = lookAhead $ try $ do
   string "\n"
   spaces
   string "import "

pModuleDecl = try $ do
   string "module"
   anyChar `manyTill` try (string "where")

pHeader = anyChar `manyTill` (pModuleDecl <|> pIsImport)

pImports = many anyChar

pBody = undefined -- TODO: Find some way to tell when import list has ended
                  -- this is needed to eventually move options pragmas
