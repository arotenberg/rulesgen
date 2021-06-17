module Main(
    main
) where

import Control.Monad(replicateM_)
import qualified Options.Applicative as Opt

import RulesGen.Gen
import RulesGen.Loader
import RulesGen.Rules

main :: IO ()
main = do
    ra <- Opt.execParser mainInfo
    rules <- loadRulesFile (raRulesFilePath ra)
    replicateM_ (raCount ra) $ do
        resultString <- runRulesGen rules (genFromNonterminal startNT)
        putStrLn resultString

mainInfo :: Opt.ParserInfo RulesArgs
mainInfo = Opt.info (Opt.helper <*> rulesArgsParser) (Opt.header mainHeader)

mainHeader :: String
mainHeader = "rulesgen - generate random text matching a grammar"

data RulesArgs = RulesArgs {
    raRulesFilePath :: FilePath,
    raCount :: Int
  } deriving (Show)

rulesArgsParser :: Opt.Parser RulesArgs
rulesArgsParser = RulesArgs
    <$> Opt.argument Opt.str
            (Opt.metavar "RULES_FILE"
            <> Opt.help "File containing the input grammar")
    <*> Opt.option Opt.auto
            (Opt.long "count"
            <> Opt.short 'n'
            <> Opt.metavar "NUM"
            <> Opt.help "Number of outputs to generate"
            <> Opt.value 1
            <> Opt.showDefault)

startNT :: NonterminalID
startNT = NonterminalID "Start"
