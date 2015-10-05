module Main where

import Control.Applicative(some)
import Control.Monad(forM)
import Data.List(foldl1')
import qualified Options.Applicative as Opt
import System.Environment(getArgs)

import RulesGen.Rules
import RulesGen.Parser
import RulesGen.Gen

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
    let parsedOptions = Opt.execParserPure Opt.defaultPrefs mainInfo args
    rulesFiles <- Opt.handleParseResult parsedOptions
    parsedFiles <- forM rulesFiles $ \rulesFile -> do
        fileString <- readFile rulesFile
        return (parseRules fileString)
    let rules = foldl1' unionRules parsedFiles
    resultString <- runRulesGen rules (genFromNonterminal startNT)
    putStrLn resultString

mainInfo :: Opt.ParserInfo [FilePath]
mainInfo = Opt.info (Opt.helper <*> rulesFilesParser) (Opt.header mainHeader)

mainHeader :: String
mainHeader = "rulesgen - generate random text matching a grammar"

rulesFilesParser :: Opt.Parser [FilePath]
rulesFilesParser = some (Opt.argument Opt.str (Opt.metavar "RULES_FILES..."))

startNT :: NonterminalID
startNT = NonterminalID "Start"
