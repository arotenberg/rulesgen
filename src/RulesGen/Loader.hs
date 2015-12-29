module RulesGen.Loader(
    loadRulesFile
) where

import qualified Language.Preprocessor.Cpphs as Cpphs

import RulesGen.Parser
import RulesGen.Rules

loadRulesFile :: FilePath -> IO Rules
loadRulesFile rulesFile = do
    rulesFileContents <- readFile rulesFile
    preprocessedContents <- Cpphs.runCpphs cpphsOptions rulesFile rulesFileContents
    return (parseRules preprocessedContents)

cpphsOptions :: Cpphs.CpphsOptions
cpphsOptions = Cpphs.defaultCpphsOptions { Cpphs.boolopts = cpphsBoolOptions }

cpphsBoolOptions :: Cpphs.BoolOptions
cpphsBoolOptions = Cpphs.BoolOptions {
    Cpphs.macros = False,
    Cpphs.locations = False,
    Cpphs.hashline = False,
    Cpphs.pragma = True,
    Cpphs.stripEol = True,
    Cpphs.stripC89 = True,
    Cpphs.lang = False,
    Cpphs.ansi = True,
    Cpphs.layout = False,
    Cpphs.literate = False,
    Cpphs.warnings = True
  }
