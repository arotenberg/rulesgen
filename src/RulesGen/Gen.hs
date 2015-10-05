{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RulesGen.Gen(
    RulesGen,
    runRulesGen,
    genFromNonterminal
) where

import Control.Monad(liftM, when)
import Control.Monad.Random(MonadRandom, RandT, evalRandT, getRandomR)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(State, evalState, get, modify')
import qualified Data.Map as Map
import System.Random(StdGen, newStdGen)

import RulesGen.Rules

newtype RulesGen a = RulesGen (RandT StdGen (State GenState) a)
    deriving (Functor, Applicative, Monad, MonadRandom)

data GenState = GenState {
    gsRules :: Rules,
    gsRemainingProductions :: Rules,
    gsBoundNonterminals :: Map.Map NonterminalID String
  } deriving (Show)

runRulesGen :: Rules -> RulesGen a -> IO a
runRulesGen rules (RulesGen m) = do
    stdGen <- newStdGen
    return $ evalState (evalRandT m stdGen) initialState
  where initialState = GenState {
            gsRules = rules,
            gsRemainingProductions = rules,
            gsBoundNonterminals = Map.empty
          }

getAction :: (GenState -> a) -> RulesGen a
getAction f = RulesGen $ f `liftM` lift get

modifyAction :: (GenState -> GenState) -> RulesGen ()
modifyAction f = RulesGen $ lift (modify' f)

getRules :: RulesGen Rules
getRules = getAction gsRules

getRemainingProductions :: RulesGen Rules
getRemainingProductions = getAction gsRemainingProductions

putRemainingProductions :: Rules -> RulesGen ()
putRemainingProductions r = modifyAction $ \gs -> gs { gsRemainingProductions = r }

getBoundNonterminals :: RulesGen (Map.Map NonterminalID String)
getBoundNonterminals = getAction gsBoundNonterminals

putBoundNonterminals :: Map.Map NonterminalID String -> RulesGen ()
putBoundNonterminals m = modifyAction $ \gs -> gs { gsBoundNonterminals = m }

genFromNonterminal :: NonterminalID -> RulesGen String
genFromNonterminal n@(NonterminalID nString) = do
    Rules remainingRulesMap <- getRemainingProductions
    case Map.lookup n remainingRulesMap of
        Nothing -> error $ "Unknown nonterminal " ++ show nString ++ "."
        Just rule -> genFromRule n rule

genFromRule :: NonterminalID -> Rule -> RulesGen (String)
genFromRule n remainingRule@(Rule remainingProductions) = do
    let probMap = buildProbMap remainingRule
        (upperBound, _) = Map.findMax probMap
    randomValue <- getRandomR (0, upperBound - 1)
    let Just (_, pid) = Map.lookupGT randomValue probMap
        (_, symbols) = remainingProductions Map.! pid
    
    -- Avoid picking the same production twice in a row by removing the production we just picked
    -- from the basket of productions that can be selected for this nonterminal. Obviously this only
    -- makes sense if the nonterminal has more than one production.
    Rules originalRulesMap <- getRules
    let Rule originalProductions = originalRulesMap Map.! n
    when (Map.size originalProductions > 1) $ do
        -- If we've used up all our productions for this nonterminal, start over from the beginning
        -- with all of the productions available except the one we just selected.
        let productionsToUpdate
                | Map.size remainingProductions > 1 = remainingProductions
                | otherwise = originalProductions
            updatedRule = Rule (Map.delete pid productionsToUpdate)
        Rules remainingRulesMap <- getRemainingProductions
        let updatedRemainingRules = Rules (Map.insert n updatedRule remainingRulesMap)
        putRemainingProductions updatedRemainingRules
    
    genFromSymbols symbols

buildProbMap :: Rule -> Map.Map Int ProductionID
buildProbMap (Rule productions) = Map.fromList (zip productionIndices (Map.keys productions))
  where productionIndices = tail $ scanl nextIndex 0 (Map.toList productions)
        nextIndex i (_, (count, _)) = i + count

genFromSymbols :: [Symbol] -> RulesGen String
genFromSymbols symbols = concat `liftM` mapM genFromSymbol symbols

genFromSymbol :: Symbol -> RulesGen String
genFromSymbol (Terminal c) = return [c]
genFromSymbol (RandomNonterminal n) = genFromNonterminal n
genFromSymbol (BoundNonterminal n) = do
    bindingsMap <- getBoundNonterminals
    case Map.lookup n bindingsMap of
        Just result -> return result
        Nothing -> do
            result <- genFromNonterminal n
            putBoundNonterminals (Map.insert n result bindingsMap)
            return result
