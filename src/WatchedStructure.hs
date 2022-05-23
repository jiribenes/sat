{-# LANGUAGE LambdaCase #-}

module WatchedStructure where

import qualified Data.IntMap as IM
import Types
import qualified Data.Vector as V
import Control.Monad.State.Strict hiding (when, unless)

type ClauseId = Int

type Watchlist = IM.IntMap [ClauseId]

type WatchMap = IM.IntMap (Lit, Lit)

nullLiteral = Lit 0

watchedLiterals :: Clause -> ClauseId -> WatchMap -> Either Lit (Lit, Lit)
watchedLiterals (Clause cls) i wm 
  | V.null cls = Left nullLiteral
  | V.length cls == 1 = Left (V.head cls)
  | otherwise = case i `IM.lookup` wm of
                  Nothing -> Left nullLiteral
                  Just x -> Right x

watching :: Lit -> Watchlist -> [ClauseId]
watching (Lit i) wl = IM.findWithDefault [] i wl

isWatching :: Clause -> ClauseId -> Lit -> WatchMap -> Bool
isWatching c@(Clause cls) i l wm
    | V.null cls = False
    | otherwise = case watchedLiterals c i wm of
                    Left l' -> l == l' 
                    Right (l1, l2) -> l == l1 || l == l2

isClauseSat :: Clause -> ClauseId -> WatchMap -> Assignment -> Bool
isClauseSat c@(Clause cls) i wm alpha
    | V.null cls = False
    | otherwise = case watchedLiterals c i wm of
                    Left l -> l `isSatisfiedIn` alpha
                    Right (l1, l2) -> l1 `isSatisfiedIn` alpha || l2 `isSatisfiedIn` alpha

renewWatchedLit :: Clause -> ClauseId -> WatchMap -> Assignment -> Maybe Lit
renewWatchedLit c@(Clause cls) i wm alpha = V.find (\l -> not (isWatching c i l wm || l `isUnsatisfiedIn` alpha)) cls

makeWatched :: CNFFormula -> (Watchlist, WatchMap)
makeWatched (CNFFormula cnf _) = V.ifoldl' step (mempty, mempty) cnf
  where
    step :: (Watchlist, WatchMap) -> ClauseId -> Clause -> (Watchlist, WatchMap)
    step (wl, wm) i (Clause cls) = addWatched (Clause cls) i (wl, wm)

addWatched :: Clause -> ClauseId -> (Watchlist, WatchMap) -> (Watchlist, WatchMap)
addWatched (Clause cls) i (wl, wm)
        | V.null cls = (wl, wm)
        | V.length cls == 1 = (watch i (V.head cls) wl, wm)
        | otherwise = 
            let x = cls V.! 0
                y = cls V.! 1
             in (watch i x (watch i y wl), IM.insert i (x, y) wm)
  where
    watch :: ClauseId -> Lit -> Watchlist -> Watchlist
    watch i (Lit n) wl = IM.alter (\case
        Nothing -> Just [i]
        Just is -> Just (i:is)) n wl
