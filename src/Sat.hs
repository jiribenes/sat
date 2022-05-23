module Sat (sat) where

import Options
import qualified Formula
import qualified DIMACS
import qualified Data.Time as Time
import qualified DPLL
import Types (assignmentToString)
import Control.Monad (when)
import qualified CDCL
import qualified Data.IntMap as IM
import qualified Watched

sat :: Options -> IO ()
sat opts = do
    let filepath = optInput opts
    fla <- if (take 3 (reverse filepath) == "sat") 
               then Formula.parseFormulaFromFile filepath (optTseitinEquiv opts)
               else DIMACS.parseFormulaFromFile filepath

    when (optCommand opts == DPLL) $ do
        putStrLn $ "c file = " <> filepath

        start <- Time.getCurrentTime
        let (state, result) = DPLL.runDPLL fla

        putStrLn $ "c #decisions = " <> show (DPLL.decisionCount state)
        putStrLn $ "c #steps of unit prop = " <> show (DPLL.unitPropCount state)
        putStrLn "c"

        case result of
          DPLL.Sat alpha -> do
              putStrLn "s SATISFIABLE"
              putStrLn ("v " <> assignmentToString alpha <> " 0")
          DPLL.Unsat -> do
              putStrLn "s UNSATISFIABLE"
        
        putStrLn "c"
        end <- Time.getCurrentTime
        putStrLn $ "c time = " <> show (Time.diffUTCTime end start)

    when (optCommand opts == DPLLWatched) $ do
        putStrLn $ "c file = " <> filepath

        start <- Time.getCurrentTime
        let (isSat, state) = Watched.runDPLL fla
        
        if isSat 
           then do
              putStrLn "s SATISFIABLE"
              putStrLn ("v " <> assignmentToString (Watched.alpha state) <> " 0")
           else do
              putStrLn "s UNSATISFIABLE"

        putStrLn "c"
        end <- Time.getCurrentTime
        putStrLn $ "c time = " <> show (Time.diffUTCTime end start)

    when (optCommand opts == CDCL) $ do
        putStrLn $ "c file = " <> filepath

        start <- Time.getCurrentTime
        let (isSat, state) = CDCL.runCDCL (optUnitRun opts) (optCache opts) fla
        
        if isSat 
           then do
              putStrLn "s SATISFIABLE"
              putStrLn ("v " <> assignmentToString (IM.map (\(CDCL.Value p _ _) -> p) (CDCL.alpha state)) <> " 0")
           else do
              putStrLn "s UNSATISFIABLE"

        putStrLn "c"
        end <- Time.getCurrentTime
        putStrLn $ "c time = " <> show (Time.diffUTCTime end start)
