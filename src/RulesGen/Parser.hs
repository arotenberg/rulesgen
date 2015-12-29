module RulesGen.Parser(
    parseRules
) where

import Data.List(foldl')
import qualified Data.Map as Map

import RulesGen.Rules

parseRules :: String -> Rules
parseRules = Rules . foldl' addToRules Map.empty . lines

addToRules :: Map.Map NonterminalID Rule -> String -> Map.Map NonterminalID Rule
addToRules rs line
    | all (`elem` " \t") line = rs
    | otherwise = Map.alter addRule nonterminal rs
  where (nonterminal, count, body) = parseLine line
        addRule Nothing = Just $ Rule $ Map.singleton (ProductionID 0) (count, body)
        addRule (Just (Rule r)) = Just $ Rule $ Map.insert (succ maxID) (count, body) r
          where (maxID, _) = Map.findMax r

parseLine :: String -> (NonterminalID, Int, [Symbol])
parseLine line = (NonterminalID beforeTimes, count, parseBody rest)
  where (beforeEquals, '=':rest) = span (/= '=') line
        (beforeTimes, countChunk) = span (/= '*') beforeEquals
        count = case countChunk of
            "" -> 1
            (_:countString) -> read countString :: Int

parseBody :: String -> [Symbol]
parseBody "" = []
parseBody ('%':cs) = RandomNonterminal (NonterminalID n) : parseBody rest
  where (n, '%':rest) = span (/= '%') cs
parseBody ('@':cs) = BoundNonterminal (NonterminalID n) : parseBody rest
  where (n, '@':rest) = span (/= '@') cs
parseBody ('\\':c:cs) = Terminal (read ['\'', '\\', c, '\'']) : parseBody cs
parseBody (c:cs) = Terminal c : parseBody cs
