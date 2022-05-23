{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

module DPLL where

import Control.Monad (when)
import Control.Monad.ST
import Data.Foldable (for_)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Stack
import Types
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

type ClauseID = Int

-- Each clause has an index
type LiteralAdjacency = V.Vector ClauseID

newtype AdjacencyList = AdjacencyList {unAdjacencyList :: V.Vector LiteralAdjacency}
    deriving (Eq, Ord, Show)

makeLists :: HasCallStack => CNFFormula -> (AdjacencyList, ClauseList)
makeLists fla = runST $ do
    adjList <- MV.replicate (2 * numberOfVariables fla) V.empty
    clsList <- MV.new (numberOfClauses fla)

    V.iforM_ (cnf fla) $ \clauseId clause@(Clause clauseContents) -> do
        V.forM_ clauseContents $ \lit -> do
            MV.modify adjList (clauseId `V.cons`) (getLiteralIndex lit)

        MV.write clsList clauseId (ClauseData 0 0 (clauseLength clause))

    adjList' <- V.freeze adjList
    clsList' <- V.freeze clsList
    pure (AdjacencyList adjList', clsList')

type LiteralIndex = Int

getLiteralIndex :: Lit -> LiteralIndex
getLiteralIndex lit@(Lit l) =
    case polarity lit of
        Pos -> 2 * l - 2
        Neg -> 2 * abs l - 1
{-# INLINE getLiteralIndex #-}

-- Counter-based, ala GRASP https://www.cs.cmu.edu/~emc/15-820A/reading/grasp_iccad96.pdf
data ClauseData = ClauseData {trueLiterals :: Int, falseLiterals :: Int, totalLiterals :: Int}
    deriving (Eq, Ord, Show)

type ClauseList = V.Vector ClauseData

clauseUnsat, clauseSat, clauseUnit :: ClauseData -> Bool
clauseUnsat clauseData = totalLiterals clauseData == falseLiterals clauseData
clauseSat clauseData = trueLiterals clauseData >= 1
clauseUnit clauseData = falseLiterals clauseData == (totalLiterals clauseData - 1) && trueLiterals clauseData == 0

addSat :: ClauseData -> ClauseData
addSat clauseData = clauseData{trueLiterals = trueLiterals clauseData + 1}
{-# INLINE addSat #-}

addUnsat :: ClauseData -> ClauseData
addUnsat clauseData = clauseData{falseLiterals = falseLiterals clauseData + 1}
{-# INLINE addUnsat #-}

-- | O(|clause| * log |assignment|)
findLitInUnitClause :: Assignment -> Clause -> Lit
findLitInUnitClause alpha (Clause c) =
    case V.find (\(v, l) -> v `isUnassignedIn` alpha) $ V.map (\l -> (litToVar l, l)) c of
        Just (_, l) -> l
        Nothing -> error "findVarInUnitClause: impossible, there are no unset variables!"

getClauseById :: CNFFormula -> ClauseID -> Clause
getClauseById fla i = cnf fla V.! i
{-# INLINE getClauseById #-}

propagateAssignment :: Lit -> AdjacencyList -> ClauseList -> ClauseList
propagateAssignment lit (AdjacencyList adjs) cls = runST go
  where
    literalIndex = getLiteralIndex lit
    {-# INLINE literalIndex #-}
    negLiteralIndex = getLiteralIndex $ Types.neg lit
    {-# INLINE negLiteralIndex #-}

    go :: forall s. ST s ClauseList -- this function is mutable
    go = do
        mut_cls :: MV.MVector s ClauseData <- V.thaw cls

        let literalAdj = adjs V.! literalIndex
        let negLiteralAdj = adjs V.! negLiteralIndex

        V.mapM_ (MV.modify mut_cls addSat) literalAdj
        V.mapM_ (MV.modify mut_cls addUnsat) negLiteralAdj

        V.freeze mut_cls

-- iterates until 'unitPropStep' stops changing everything
unitProp count clauseList adjList alpha fla = case unitPropStep clauseList adjList alpha fla of
    Nothing -> (count, alpha, clauseList)
    Just (alpha', clauseList') -> unitProp (count + 1) clauseList' adjList alpha' fla

unitPropStep clauseList adjList alpha fla =
    V.findIndex clauseUnit clauseList <&> \clauseId ->
        let clause = fla `getClauseById` clauseId
            lit = findLitInUnitClause alpha clause
            alpha' = lit `assignIn` alpha
            clauseList' = propagateAssignment lit adjList clauseList
         in (alpha', clauseList')

data DPLLResult
    = Sat Assignment
    | Unsat
    deriving (Show, Eq, Ord)

orTry :: (DPLLState, DPLLResult) -> (DPLLState, DPLLResult) -> (DPLLState, DPLLResult)
(st, Sat alpha) `orTry` _ = (st, Sat alpha)
_ `orTry` b = b

check alpha clauseList
    | V.all clauseSat clauseList = Just $ Sat alpha
    | V.any clauseUnsat clauseList = Just Unsat
    | otherwise = Nothing

data DPLLState = DPLLState
    { unitPropCount :: Int
    , decisionCount :: Int
    }
    deriving (Show, Eq, Ord)

addDecision :: Lit -> DPLLState -> DPLLState
addDecision l state = state { decisionCount = (decisionCount state + 1) }

initialDPLLState :: DPLLState
initialDPLLState = DPLLState 0 0

dpll state clauseList adjList alpha fla
    | V.all clauseSat clauseList = (state, Sat alpha)
    | V.any clauseUnsat clauseList = (state, Unsat)
    | IM.size alpha == numberOfVariables fla = error "This formula is satisfied, we should already know this!"
    | otherwise =
        let (upCount, alpha', clauseList') = unitProp (unitPropCount state) clauseList adjList alpha fla
            state' = state { unitPropCount = upCount }

            unitPropResult = check alpha' clauseList'

            literal = pickVariable fla (IM.keysSet alpha')

            -- branch where we set the literal to true
            truthyLiteral = varToLit literal Pos
            alphaTruthy = truthyLiteral `assignIn` alpha'
            clauseListTruthy = propagateAssignment truthyLiteral adjList clauseList'
            truthyResult@(state'', _) = dpll (addDecision truthyLiteral state') clauseListTruthy adjList alphaTruthy fla

            -- branch where we set the literal to false
            falseyLiteral = varToLit literal Neg
            alphaFalsey = falseyLiteral `assignIn` alpha'
            clauseListFalsey = propagateAssignment falseyLiteral adjList clauseList'
            falseyResult = dpll state'' clauseListFalsey adjList alphaFalsey fla
         in case unitPropResult of
              Just !result -> (state', result)
              Nothing -> truthyResult `orTry` falseyResult
  where

runDPLL :: CNFFormula -> (DPLLState, DPLLResult)
runDPLL fla = dpll initialDPLLState clsList adjList mempty fla
  where
    (adjList, clsList) = makeLists fla
