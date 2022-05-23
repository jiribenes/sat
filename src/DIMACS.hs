{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module DIMACS where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified FlatParse.Basic as FP
import Types
import Parser
import DIMACSParserUtils
import Control.Monad (unless)
import Data.Function ((&))
import Data.Functor ((<&>))

data ParseError
    = ExpectedHeader
    | ExpectedClause
    | ExpectedLiteral
    | ExpectedPosVar
    | ExpectedNegVar
    | IncorrectHeader
    | UnexpectedFail
    | Unconsumed !ByteString
    deriving (Eq, Show)

-- Note: comments necessarily consume their own '\n'.
-- Be warned!
comment :: Parser ()
comment = $(FP.char 'c') *> innerComment
  where
    innerComment :: Parser ()
    innerComment =
        FP.optioned
            FP.anyWord8
            ( \case
                10 -> pure ()
                _ -> innerComment
            )
            (pure ())

data DimacsHeader = DimacsHeader {headerVars :: Int, headerClauses :: Int}
    deriving (Eq, Show)

literal :: Parser Int
literal =
    FP.branch
        $(FP.char '-')
        ((negate <$> FP.readInt) `cut'` Msg "positive literal")
        (FP.readInt `cut'` Msg "negative literal")

literal' :: Parser Lit
literal' = Types.Lit <$> literal `cut'` Msg "literal"
{-# INLINE literal' #-}

header :: Parser DimacsHeader
header = do
    $(char' 'p') *> $(symbol' "cnf")
    vars <- token FP.readInt
    clauses <- token FP.readInt
    pure $ DimacsHeader vars clauses

header' :: Parser DimacsHeader
header' = header `cut'` Msg "header"
{-# INLINE header' #-}

clause :: Parser Types.Clause
clause = toClause <$> FP.some (token (FP.try literal'))
  where
    toClause = Types.Clause . V.fromList

clause' :: Parser Types.Clause
clause' = DIMACS.clause `cut'` Msg "clause"
{-# INLINE clause' #-}

topLevel :: Parser CNFFormula
topLevel = do
    FP.many_ comment

    DimacsHeader headerVars headerClauses <- header' <* newline <* whitespace

    clauses <- V.fromList <$> FP.many (FP.try $ clause' <* newline)

    pure $ CNFFormula clauses headerVars

data DIMACSParserError
    = ParserError !String
    | ParserFail 
    | ParserUnconsumed !ByteString
    deriving (Show)

parseFormula :: ByteString -> Either DIMACSParserError CNFFormula
parseFormula input = case runParser topLevel input of
    FP.Err e -> Left $ ParserError (prettyError input e)
    FP.Fail -> Left ParserFail
    FP.OK a _ -> Right a

parseFormulaFromFile :: FilePath -> IO CNFFormula
parseFormulaFromFile path = do
    contents <- BS.readFile path
    case parseFormula contents of
        Left err -> do
            print err
            error "Found error, quitting!"
        Right fla -> do
            let fla' = stripZeros fla
            pure fla'

stripZeros :: CNFFormula -> CNFFormula
stripZeros fla = fla { cnf = cnf fla <&> \(Clause cl) -> cl & V.reverse & V.drop 1 & V.reverse & Clause }
