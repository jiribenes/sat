module Watched where

import qualified Data.IntMap as IM
import Types
import qualified Data.Vector as V
import Control.Monad.State.Strict hiding (when, unless)
import Data.List (delete)
import Debug.Trace
import WatchedStructure

data WatchedState = WatchedState { fla :: CNFFormula, alpha :: Assignment, queue :: [Lit], wl :: Watchlist, wm :: WatchMap }
  deriving Show

getClause :: ClauseId -> State WatchedState Clause
getClause i = do
    st <- get
    return $ cnf (fla st) V.! i

assume :: Lit -> State WatchedState Bool
assume l = do
    st <- get
    case lookupIn l (alpha st) of
      Just True -> return True
      Just False -> return False
      Nothing -> do
          put $ st { alpha = assignIn l (alpha st)
                   , queue = neg l : (queue st) }
          return True

unitProp = do
    st <- get
    case queue st of
      []   -> do
          return True
      l:ls -> do
          put $ st { queue = ls }
          b <- unitPropStep l (watching l (wl st))
          if b then unitProp
               else do
                   put $ st { queue = [] }
                   return False

unitPropStep l [] = return True
unitPropStep l (i:is) = do
    b <- propagateAssignment l i
    when b $ do
             b' <- unitPropStep l is
             return b'

propagateAssignment l clauseId = do
    c@(Clause clause) <- getClause clauseId
    unless (V.null clause) $ do
        unless (V.length clause == 1) $ do
            st <- get
            let clauseSat = isClauseSat c clauseId (wm st) (alpha st) 
            if clauseSat
               then do
                   return True
               else do
                   case renewWatchedLit c clauseId (wm st) (alpha st) of
                     Nothing -> do -- unit prop
                         case watchedLiterals c clauseId (wm st) of
                             Left _ -> return False
                             Right (l1, l2) -> do
                                 let theOtherLit = if l1 == l then l2 else l1
                                 assume theOtherLit
                     Just l' -> do
                         let updatePair (l1, l2) = if l1 == l then (l', l2) else (l1, l')
                             wm' = IM.adjust updatePair clauseId (wm st)
                             foo = delete clauseId (watching l (wl st))
                             bar = clauseId : watching l' (wl st)
                             wl' = IM.insert (unLit l') bar (IM.insert (unLit l) foo (wl st))
                         put $ st { wm = wm', wl = wl' }
                         return True

dpll = do
    b <- unitProp
    when b $ do
        st <- get
        if IM.size (alpha st) == numberOfVariables (fla st) 
           then do
            return True
           else do
            let v = pickVariable (fla st) (IM.keysSet $ alpha st)
            let assignmentBackup = alpha st

            assume (varToLit v Pos) -- assume +v
            branch <- dpll
            if branch 
               then return True
               else do
                   st' <- get
                   put $ st' { alpha = assignmentBackup }
                   assume (varToLit v Neg) -- assume -v
                   dpll

runDPLL :: CNFFormula -> (Bool, WatchedState)
runDPLL fla = runState dpll initialState 
  where
    (wl, wm) = makeWatched fla
    initialState = WatchedState fla mempty mempty wl wm
