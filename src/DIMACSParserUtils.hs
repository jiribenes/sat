{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module DIMACSParserUtils where

import Parser
import Language.Haskell.TH ( Exp, Q)
import qualified FlatParse.Basic as FP

whitespace :: Parser ()
whitespace =
    $( FP.switch
        [|
            case _ of
                " " -> whitespace
                "\t" -> whitespace
                _ -> pure ()
            |]
     )

newline :: Parser ()
newline =
    $( FP.switch
        [|
            case _ of
                "\n" -> pure ()
                "\r\n" -> pure ()
                -- _ -> whitespace -- Greedily consume whitespace since e.g. 'uf20-01.cnf' has some unexplainable whitespace AT THE BEGINNING of a clause, what the hell?
            |]
     )

token :: Parser a -> Parser a
token x = x <* whitespace
{-# INLINE token #-}

symbol :: String -> Q Exp
symbol str = [| token $(FP.string str) |]

symbol' :: String -> Q Exp
symbol' str = [| $(symbol str) `cut'` Lit str |]

char :: Char -> Q Exp
char c = [| token $(FP.char c) |]

char' :: Char -> Q Exp
char' c = [| $(char c) `cut'` Lit [c] |]

