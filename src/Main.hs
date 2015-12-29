module Main(
    main,
    mainWithArgs
) where

import Control.Applicative(some)
import qualified Options.Applicative as Opt
import System.Environment(getArgs)

import RulesGen.Gen
import RulesGen.Loader
import RulesGen.Rules

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
    let parsedOptions = Opt.execParserPure Opt.defaultPrefs mainInfo args
    rulesFiles <- Opt.handleParseResult parsedOptions
    rules <- loadRulesFiles rulesFiles
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
