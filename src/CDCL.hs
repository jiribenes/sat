{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Contains a lot of code from 'Watched' which is unfortunately not really
-- reusable since it relies on the assignment being "just the assignment".
-- However, here we need a lot more information :(
module CDCL where

import Types hiding (when)
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Data.List (sortOn, delete)
import qualified Data.IntSet as IS
import qualified Control.Monad
import WatchedStructure

type DecisionLevel = Int

data Value = Value { valPolarity :: Polarity, valLevel :: DecisionLevel, valAnt :: ClauseId }
  deriving Show

type CDCLAssignment = IM.IntMap Value    -- Var -> Maybe Value

type DecidedAt = IM.IntMap Lit           -- DecisionLevel -> Maybe Lit

lookupInCDCL :: Lit -> CDCLAssignment -> Maybe Value
lookupInCDCL l alpha = 
    case IM.lookup var alpha of
      Nothing -> Nothing
      Just (Value p' d a) -> Just (Value (boolToPolarity $ p == p') d a)
  where
      p = polarity l
      (Var var) = litToVar l
{-# INLINE lookupInCDCL #-}

data CDCLState = CDCLState { level :: DecisionLevel, assigned :: V.Vector Lit, fla :: CNFFormula, alpha :: CDCLAssignment, queue :: [Lit], wl :: Watchlist, wm :: WatchMap }
  deriving Show

addClause :: Clause -> State CDCLState ()
addClause c = do
    modify $ \st -> st { fla = addClauseToFla (fla st) }
  where
    addClauseToFla :: CNFFormula -> CNFFormula
    addClauseToFla (CNFFormula cnf n) = CNFFormula (V.snoc cnf c) n

lookupClause :: ClauseId -> State CDCLState Clause
lookupClause clauseId = do
    st <- get 
    return $ cnf (fla st) V.! clauseId

findAssertiveClause :: Clause -> State CDCLState Clause
findAssertiveClause clause = do
    st <- get
    literals <- allOfLevel clause
    literals' <- orderLiteralsByAssignTime literals
    findAssertive literals' clause
  where
    findAssertive [] _ = error "impossible"
    findAssertive [_] c = return c
    findAssertive (l:_) clause = do
      st <- get
      case lookupInCDCL l (alpha st) of
        Nothing -> error "impossible: assigned in, but not in assignment!"
        Just (Value _ _ (-1)) -> error "this is suspicious"
        Just (Value _ _ antClauseId) -> do
          antClause <- lookupClause antClauseId
          let clause' = resolution clause antClause
          findAssertiveClause clause'

assertiveClauseLevel :: Clause -> State CDCLState DecisionLevel
assertiveClauseLevel (Clause cls) = do
    st <- get
    ordered <- orderLiteralsByAssignTime (V.toList cls)
    case ordered of
      [] -> error "impossible"
      [_] -> return 0
      (first:l:_) -> 
          case lookupInCDCL l (alpha st) of
            Nothing -> return 0
            Just (Value _ d _) -> return d

conflictAnalysis :: Clause -> State CDCLState DecisionLevel
conflictAnalysis conflict = do
    stDebug <- get

    assertive <- findAssertiveClause conflict
    addClause assertive
    
    st <- get
    let (wl', wm') = addWatched assertive (V.length (cnf (fla st)) - 1) (wl st, wm st)
    put $ st { wl = wl', wm = wm' }
    
    newLevel <- assertiveClauseLevel assertive
    return newLevel

backtrack :: DecisionLevel -> State CDCLState ()
backtrack newLevel = do -- watchlist, watchmap stay
    CDCLState {..} <- get
    let valueIsKept _ (Value _ d _) = d <= newLevel
    let (alpha', deleted) = IM.partitionWithKey valueIsKept alpha
    let noLongerAssigned = IM.keysSet deleted
    let assigned' = V.dropWhile (\(Lit l) -> l `IS.member` noLongerAssigned) assigned
    Control.Monad.unless (null queue) $ error "impossible"
    put $ CDCLState newLevel assigned' fla alpha' mempty wl wm

assignInCDCL :: Lit -> State CDCLState ()
assignInCDCL l = do
    st <- get
    let val = Value (polarity l) (level st) (-1)
    let alpha' = IM.insert (unVar (litToVar l)) val (alpha st)
    put $ st { alpha = alpha', queue = neg l : queue st, assigned = V.cons l (assigned st) }
{-# INLINE assignInCDCL #-}

derivedAssign :: ClauseId -> Lit -> State CDCLState ()
derivedAssign parent l = do
    st <- get
    let val = Value (polarity l) (level st) parent
    let alpha' = IM.insert (unVar (litToVar l)) val (alpha st)
    put $ st { alpha = alpha', queue = neg l : queue st, assigned = V.cons l (assigned st) }

assume :: Lit -> State CDCLState Bool
assume l = do
    st <- get
    case lookupInCDCL l (alpha st) of
      Just (Value p _ _) -> return (p == Pos)
      Nothing -> do
          assignInCDCL l
          return True

derivedAssume :: ClauseId -> Lit -> State CDCLState Bool
derivedAssume parent l = do
    st <- get
    case lookupInCDCL l (alpha st) of
      Just (Value p _ _) -> return (p == Pos)
      Nothing -> do
          derivedAssign parent l
          return True

data UnitPropResult
    = AllOK
    | Conflict Clause

unitProp = do
    st <- get
    case queue st of
      []   -> do
          return AllOK
      l:ls -> do
          put $ st { queue = ls }
          b <- unitPropStep l (watching l (wl st))
          case b of
            AllOK -> unitProp
            Conflict i -> do
               put $ st { queue = [] }
               return $ Conflict i

unitPropStep l [] = return AllOK
unitPropStep l (i:is) = do
    b <- propagateAssignment l i
    case b of
      AllOK -> do
         b' <- unitPropStep l is
         return b'
      Conflict i -> return $ Conflict i

propagateAssignment l clauseId = do
    c@(Clause clause) <- lookupClause clauseId
    if V.null clause
       then return $ Conflict c
       else if V.length clause == 1 
            then do
                    st <- get
                    let clauseSat = isClauseSat c clauseId (wm st) (IM.map (\(Value p _ _) -> p) (alpha st))
                    if clauseSat
                       then return AllOK
                       else do
                           case renewWatchedLit c clauseId (wm st) (IM.map (\(Value p _ _) -> p) (alpha st)) of
                             Nothing -> do -- unit prop
                                 case watchedLiterals c clauseId (wm st) of
                                     Left l' -> return $ Conflict c
                                     Right (l1, l2) -> do
                                         let theOtherLit = if l1 == l then l2 else l1
                                         result <- derivedAssume clauseId theOtherLit
                                         if result
                                            then return AllOK
                                            else return $ Conflict c 
                             Just l' -> do
                                 let updatePair (l1, l2) = if l1 == l then (l', l2) else (l1, l')
                                     wm' = IM.adjust updatePair clauseId (wm st)
                                     foo = delete clauseId (watching l (wl st))
                                     bar = clauseId : watching l' (wl st)
                                     wl' = IM.insert (unLit l') bar $ IM.insert (unLit l) foo (wl st)
                                 put $ st { wm = wm', wl = wl' }
                                 return AllOK
            else return $ Conflict c


cdcl :: State CDCLState Bool
cdcl = do
    b <- unitProp
    case b of
      AllOK -> do
         st <- get
         if IM.size (alpha st) == numberOfVariables (fla st)
           then return True
           else do
            put $ st { level = level st + 1 }
            let v = pickVariable (fla st) (IM.keysSet $ alpha st)

            assume (varToLit v Pos) -- assume +v
            cdcl
      Conflict clause -> do  -- conflict (!)
        st <- get
        if level st == 0
           then return False -- conflict at level 0
           else do
            newLevel <- conflictAnalysis clause
            backtrack newLevel
            cdcl

runCDCL :: Int -> Int -> CNFFormula -> (Bool, CDCLState)
runCDCL unitRun cacheSize fla = runState cdcl initialState 
  where
    (wl, wm) = makeWatched fla
    initialState = CDCLState 0 mempty fla mempty mempty wl wm

allOfLevel :: Clause -> State CDCLState [Lit]
allOfLevel (Clause cls) = do
    st <- get
    let d = level st
    return $ filter (\l -> hasLevel d $ lookupInCDCL l (alpha st)) $ V.toList cls
  where
      hasLevel d = \case 
        Just (Value _ d' _) -> d == d'
        Nothing -> False

orderLiteralsByAssignTime :: [Lit] -> State CDCLState [Lit]
orderLiteralsByAssignTime lits = do
    st <- get
    let vars = assigned st
    return $ sortOn (\x -> min (x `V.elemIndex` vars) (neg x `V.elemIndex` vars)) lits

lastAssignedIn :: Clause -> CDCLAssignment -> Maybe Lit
lastAssignedIn (Clause cls) alpha = fst <$> V.foldl' step Nothing cls
  where
      step acc lit =
          case lookupInCDCL lit alpha of
               Nothing -> acc
               Just (Value _ d _) -> 
                   case acc of
                     Nothing -> Just (lit, d)
                     Just (lit', d') 
                        | d > d' -> Just (lit, d)
                        | otherwise -> Just (lit', d')

resolution :: Clause -> Clause -> Clause
resolution (Clause cls) (Clause cls') = Clause $ V.filter (keep cls') cls <> V.filter (keep cls) cls'
  where
    keep klz x = 
        case V.find (== neg x) klz of
           Just _ -> False -- delete
           Nothing -> True -- keep
