module Options where

import           Options.Applicative

data Command = Formula2CNF | DPLL | DPLLWatched | CDCL
  deriving (Show, Eq, Ord)

-- | All possible options supported
data Options = Options
    { optCommand :: !Command
    , -- ^ the chosen command
      optTseitinEquiv  :: !Bool
    , -- ^ Tseitin equivalences are used instead of left-to-right implications
      optVerbose :: !Bool
      -- ^ verbosity switch
    , optUnitRun :: !Int
      -- ^ used for computing the length of runs, given Luby sequence
    , optCache :: !Int
      -- ^ upper-bounds the size of learned clauses
    , optInput :: !String
      -- ^ input file
    }
    deriving (Show, Eq, Ord)

-- | Parses 'Options' from command-line inputs
parseOptions :: IO Options
parseOptions = execParser optionsParser

-- | An applicative parser for 'Options' made using the 'Options.Applicative' library
optionsParser :: ParserInfo Options
optionsParser = info
    ((Options <$> commandOption <*> tseitinEquivOption <*> verboseOption <*> unitRunOption <*> cacheOption <*> sourceOption) <**> helper)
    (  fullDesc
    <> progDesc "sat: a small SAT solver"
    <> header
           "sat - a proof-of-concept implementation of a SAT solver"
    )
  where
    commandOption = hsubparser (commandFormula2CNF <> commandDPLL <> commandDPLLWatched <> commandCDCL)
    commandFormula2CNF = command
        "formula2cnf"
        (info (pure Formula2CNF) (progDesc "Convert input to a CNF formula"))
    commandDPLL = command
        "dpll"
        (info (pure DPLL) (progDesc "Solve a formula/CNF using DPLL with adjacency list data structure"))
    commandDPLLWatched = command
        "watched"
        (info (pure DPLLWatched) (progDesc "Solve a formula/CNF using DPLL with watched literals data structure"))
    commandCDCL = command
        "cdcl"
        (info (pure CDCL) (progDesc "Solve a formula/CNF using CDCL with watched literals data structure"))

    tseitinEquivOption :: Parser Bool
    tseitinEquivOption = switch (short 'e' <> long "equiv" <> help "Use equivalences in Tseitin encoding")

    verboseOption :: Parser Bool
    verboseOption = switch (short 'v' <> long "verbose" <> help "Verbose (debug) output")

    unitRunOption :: Parser Int
    unitRunOption = option auto (short 'u' <> long "unitrun" <> value 100 <> help "Determines the length of runs")

    cacheOption :: Parser Int
    cacheOption = option auto (short 'c' <> long "cache" <> value 10000 <> help "Upper-bounds the size of learner clauses")

    sourceOption :: Parser FilePath
    sourceOption =
        strArgument (metavar "INPUT_FILE" <> help "Path to the input file")

