module Main(
    main,
    mainWithArgs
) where

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
    rulesFile <- Opt.handleParseResult parsedOptions
    rules <- loadRulesFile rulesFile
    resultString <- runRulesGen rules (genFromNonterminal startNT)
    putStrLn resultString

mainInfo :: Opt.ParserInfo FilePath
mainInfo = Opt.info (Opt.helper <*> rulesFileParser) (Opt.header mainHeader)

mainHeader :: String
mainHeader = "rulesgen - generate random text matching a grammar"

rulesFileParser :: Opt.Parser FilePath
rulesFileParser = Opt.argument Opt.str (Opt.metavar "RULES_FILE")

startNT :: NonterminalID
startNT = NonterminalID "Start"
