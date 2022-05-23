{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module FormulaParserUtils where

import Parser
import Language.Haskell.TH ( Exp, Q)
import qualified FlatParse.Basic as FP

nlWhitespace :: Parser ()
nlWhitespace =
    $( FP.switch
        [|
            case _ of
                " " -> nlWhitespace
                "\t" -> nlWhitespace
                "\n" -> nlWhitespace
                "\r" -> nlWhitespace
                _ -> pure ()
            |]
     )

token :: Parser a -> Parser a
token x = x <* nlWhitespace
{-# INLINE token #-}

symbol :: String -> Q Exp
symbol str = [| token $(FP.string str) |]

symbol' :: String -> Q Exp
symbol' str = [| $(symbol str) `cut'` Lit str |]

char :: Char -> Q Exp
char c = [| token $(FP.char c) |]

char' :: Char -> Q Exp
char' c = [| $(char c) `cut'` Lit [c] |]

