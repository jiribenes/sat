{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Vector as V
import GHC.Exts (IsList, fromList, toList)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

-- | Variable -- between 1 and #variables incl.
newtype Var = Var { unVar :: Int }
  deriving newtype (Eq, Show, Ord)

data Polarity = Pos | Neg
  deriving (Eq, Show, Ord)

type Assignment = IM.IntMap Polarity

-- | Checks if a variable is unassigned in an assignment
isUnassignedIn :: Var -> Assignment -> Bool
isUnassignedIn (Var v) alpha = IM.notMember v alpha
{-# INLINE isUnassignedIn #-}

isSatisfiedIn :: Lit -> Assignment -> Bool
isSatisfiedIn l alpha = 
    case IM.lookup var alpha of
      Nothing -> False
      Just p' -> p == p'
  where
      p = polarity l
      (Var var) = litToVar l
{-# INLINE isSatisfiedIn #-}

isUnsatisfiedIn :: Lit -> Assignment -> Bool
isUnsatisfiedIn l alpha = 
    case IM.lookup var alpha of
      Nothing -> False
      Just p' -> p /= p'
  where
      p = polarity l
      (Var var) = litToVar l
{-# INLINE isUnsatisfiedIn #-}

lookupIn :: Lit -> Assignment -> Maybe Bool
lookupIn l alpha = 
    case IM.lookup var alpha of
      Nothing -> Nothing
      Just p' -> Just $ p == p'
  where
      p = polarity l
      (Var var) = litToVar l
{-# INLINE lookupIn #-}

assignIn :: Lit -> Assignment -> Assignment
assignIn l = IM.insert (unVar (litToVar l)) (polarity l)
{-# INLINE assignIn #-}

-- | Takes an assignment and returns it as a numeric list of literals
-- which form the assignment.
assignmentToString :: Assignment -> String
assignmentToString alpha = unwords $ (\(v, p) -> show (varToLit (Var v) p)) <$> IM.toList alpha

-- | Literal -- between -#variables and #variables incl.
newtype Lit = Lit { unLit :: Int }
  deriving newtype (Eq, Show, Ord)

polarity :: Lit -> Polarity
polarity (Lit l)
    | l > 0 = Pos
    | l < 0 = Neg
    | otherwise = error "impossible"
{-# INLINE polarity #-}

flip :: Polarity -> Polarity
flip Pos = Neg
flip Neg = Pos
{-# INLINE flip #-}

litToVar :: Lit -> Var
litToVar (Lit l) = Var $ abs l
{-# INLINE litToVar #-}

varToLit :: Var -> Polarity -> Lit
varToLit (Var v) Pos = Lit v
varToLit (Var v) Neg = Lit $ negate v
{-# INLINE varToLit #-}

neg :: Lit -> Lit
neg (Lit l) = Lit $ negate l
{-# INLINE neg #-}

boolToPolarity :: Bool -> Polarity
boolToPolarity True = Pos
boolToPolarity False = Neg
{-# INLINE boolToPolarity #-}

data CNFFormula = CNFFormula {cnf :: CNF, numberOfVariables :: Int}
    deriving (Show, Eq)

type CNF = V.Vector Clause

newtype Clause = Clause {unClause :: V.Vector Lit}
    deriving (Show, Eq)
    deriving (IsList) via (V.Vector Lit)

clauseLength :: Clause -> Int
clauseLength (Clause vec) = V.length vec
{-# INLINE clauseLength #-}

clauseIsUnit :: Clause -> Bool
clauseIsUnit c = clauseLength c == 1
{-# INLINABLE clauseIsUnit #-}

numberOfClauses :: CNFFormula -> Int
numberOfClauses (CNFFormula cnf _) = V.length cnf
{-# INLINE numberOfClauses #-}

{- | Converts a 'Clause' to DIMACS-like text output

 This is probably horrifically inefficient!
 We could use a @showsPrec@-like trick to make it faster, but this has not been necessary (yet!)
-}
dimacsClause :: Clause -> ByteString
dimacsClause (Clause vec) =
    V.toList vec <&> (BS.pack . show)
        & BS.unwords
        & (<> " 0")

pickVariable :: CNFFormula -> IS.IntSet -> Var
pickVariable fla assigned = 
    Var $ IS.findMin $ IS.fromList [1 .. numberOfVariables fla] `IS.difference` assigned
{-# INLINE pickVariable #-}

unless b f = do
    if b then return False
         else f

when b f = do
    if b then f
         else return False
