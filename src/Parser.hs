-- | This module is stolen from the examples for the library @flatparse@
module Parser where

import FlatParse.Basic hiding (Parser, char, cut, runParser, string)

import Data.ByteString.Char8 (ByteString)
import Data.String (IsString (..))
import qualified FlatParse.Basic as FP

data Expected
    = Msg String
    | Lit String
    deriving (Eq, Show, Ord)

prettyExpected :: Expected -> String
prettyExpected (Msg s) = s
prettyExpected (Lit s) = show s -- i.e. quoted

instance IsString Expected where
    fromString = Lit

data Error
    = Precise Pos Expected
    | Imprecise Pos [Expected]
    deriving (Show)

errorPos :: Error -> Pos
errorPos (Precise p _) = p
errorPos (Imprecise p _) = p

instance Semigroup Error where
    e <> e' = case (errorPos e, errorPos e') of
        (p, p') | p < p' -> e'
        (p, p') | p > p' -> e
        (p, p') -> case (e, e') of
            (Precise{}, _) -> e
            (_, Precise{}) -> e'
            (Imprecise _ es, Imprecise _ es') -> Imprecise p (es <> es')

type Parser = FP.Parser Error
type Source = ByteString

prettyError :: Source -> Error -> String
prettyError src e =
    unlines
        [ show line <> ":" <> show col <> ":"
        , linePad <> "|"
        , show line <> "| " <> sourceLine
        , linePad <> "| " <> replicate col ' ' <> "^"
        , "parse error: expected " <> err e
        ]
  where
    pos = errorPos e
    sourceLines = FP.lines src
    [(line, col)] = posLineCols src [pos]
    sourceLine = if line < length sourceLines then sourceLines !! line else ""
    linePad = map (const ' ') (show line)

    err (Precise _ e) = prettyExpected e
    err (Imprecise _ es) = prettyImprecise es

    prettyImprecise :: [Expected] -> String
    prettyImprecise [] = error "impossible"
    prettyImprecise [e] = prettyExpected e
    prettyImprecise (e : es) = prettyExpected e <> go es
      where
        go [] = ""
        go [e] = " or " <> prettyExpected e
        go (e : es) = ", " <> prettyExpected e <> go es

cut :: Parser a -> [Expected] -> Parser a
cut p es = do
    pos <- getPos
    FP.cutting p (Imprecise pos es) (<>)

cut' :: Parser a -> Expected -> Parser a
cut' p e = do
    pos <- getPos
    FP.cutting p (Precise pos e) (<>)

runParser :: Parser a -> ByteString -> Result Error a
runParser = FP.runParser

testParser :: Show a => Parser a -> String -> IO ()
testParser p str = case packUTF8 str of
    b -> case runParser p b of
        Err e -> putStrLn $ prettyError b e
        OK a _ -> print a
        Fail -> putStrLn "parse error: fail"
