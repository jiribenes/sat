module Main where

import qualified Sat
import qualified Options

main :: IO ()
main = Options.parseOptions >>= Sat.sat
