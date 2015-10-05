{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RulesGen.Rules where

import qualified Data.Map as Map

newtype NonterminalID = NonterminalID String
    deriving (Show, Eq, Ord)

newtype Rules = Rules (Map.Map NonterminalID Rule)
    deriving (Show)
newtype Rule = Rule (Map.Map ProductionID (Int, [Symbol]))
    deriving (Show)
newtype ProductionID = ProductionID Int
    deriving (Show, Eq, Ord, Enum)
data Symbol
    = Terminal !Char
    | RandomNonterminal !NonterminalID
    | BoundNonterminal !NonterminalID
  deriving (Show)

unionRules :: Rules -> Rules -> Rules
unionRules (Rules rulesMap1) (Rules rulesMap2) = Rules (rulesMap1 `Map.union` rulesMap2)
