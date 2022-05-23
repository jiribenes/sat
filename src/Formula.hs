{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Formula where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor (void, ($>), (<&>))
import qualified FlatParse.Basic as FP
import FlatParse.Basic hiding (char, cut, cut', Parser, runParser)

import Control.Monad (when)
import Control.Monad.State.Strict (State, evalState, execState, gets, modify, runState)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Types
import Parser
import FormulaParserUtils
import DPLL (makeLists, dpll)

-- | NNF Formula representation
data Formula where
    Or :: Formula -> Formula -> Formula
    And :: Formula -> Formula -> Formula
    -- | This is a _variable_ negation, since the formula is in NNF!
    NegVar :: Variable -> Formula
    PosVar :: Variable -> Formula
    Empty :: Formula
    deriving (Show, Eq)

prettyFormula :: Formula -> ByteString
prettyFormula (And a b) = "(and " <> prettyFormula a <> " " <> prettyFormula b <> ")"
prettyFormula (Or a b) = "(or " <> prettyFormula a <> " " <> prettyFormula b <> ")"
prettyFormula (NegVar v) = "(not " <> unVariable v <> ")"
prettyFormula (PosVar v) = unVariable v
prettyFormula Empty = ""

newtype Variable = Variable {unVariable :: ByteString}
    deriving (Show, Eq, Ord)

-- Parser:

variable :: Parser Variable
variable = Variable <$> token (byteStringOf (variableStartChar *> many_ variableChar))
  where
    variableStartChar :: Parser Char
    variableStartChar = satisfyASCII isLatinLetter
    {-# INLINE variableStartChar #-}

    variableChar :: Parser Char
    variableChar = satisfyASCII (\c -> isLatinLetter c || isDigit c)
    {-# INLINE variableChar #-}

variable' :: Parser Variable
variable' = variable `cut'` Msg "a variable"
{-# INLINE variable' #-}

open = $(char '(')
close = $(char ')')
andToken = $(symbol "and")
orToken = $(symbol "or")
notToken = $(symbol "not")

formula :: Parser Formula
formula = branch open
                 (innerFormula <* close)
                 (PosVar <$> variable')
  where
    innerFormula =
        (andToken *> (And <$> formula <*> formula))
            <|> (orToken *> (Or <$> formula <*> formula))
            <|> (notToken *> (NegVar <$> variable'))

formula' :: Parser Formula
formula' = formula `cut` [Msg "a and-formula", Msg "a or-formula", Msg "a not-formula", Msg "a literal"]
{-# INLINE formula' #-}

topLevel :: Parser Formula
topLevel = (Empty <$ nlWhitespace <* eof) <|> (formula' <* eof) -- special case for empty formulas as they're a bit... special :)
 
data FormulaParserError
    = ParserError !String
    | ParserFail
    | ParserUnconsumed !ByteString
    deriving (Show)

parseFormula :: ByteString -> Either FormulaParserError Formula
parseFormula input = case runParser topLevel input of
    FP.Err e -> Left $ ParserError (prettyError input e)
    FP.Fail -> Left ParserFail
    FP.OK a _ -> Right a

parseFormulaFromFile :: FilePath -> Bool -> IO CNFFormula
parseFormulaFromFile path b = do
    contents <- BS.readFile path
    case parseFormula contents of
        Left err -> do
            print err
            error $ "Error: " <> show err
        Right Empty -> do
            error "Error: got empty file!"
        Right fla -> do
            let gatherEnv@(GatherEnv m c) = gatherVariables' fla

            let result = topLevelTseitin (if b then Equiv else LTRImpl) m c fla

            BS.putStrLn $ tseitinResultToDimacs result
            let fla = tseitinResultToCNFFormula result
            return fla
--
-- This part describes a pass through the data which gathers the variable names
-- and assigns a numerical value to each 'Variable' found in the 'Formula'
--
-- Note: This could be possibly quite slow.
--

type VariableMap = M.Map Variable Var

data GatherEnv = GatherEnv {variableMap :: VariableMap, counter :: Var}

prettyGatherEnv :: GatherEnv -> ByteString
prettyGatherEnv (GatherEnv varMap counter) = BS.unlines $ ["Gathered variables: "] <> prettyVarMap varMap <> ["", "Current counter: " <> pack' counter]
  where
    prettyVarMap variableMap = (\(var, c) -> "variable " <> unVariable var <> " is: " <> pack' c) <$> M.assocs variableMap
    pack' = BS.pack . show

tryAddVariable :: Variable -> GatherEnv -> GatherEnv
tryAddVariable var (GatherEnv varMap (Var counter)) =
    let (wasInMap, newVarMap) = emplace var (Var counter) varMap
     in GatherEnv newVarMap (if wasInMap then Var counter else Var $ counter + 1)
  where
    emplace :: Ord k => k -> v -> M.Map k v -> (Bool, M.Map k v)
    emplace key possibleNewValue oldMap =
        case M.insertLookupWithKey (\_ _ oldValue -> oldValue) key possibleNewValue oldMap of
            (Nothing, newMap) -> (False, newMap)
            (Just _, newMap) -> (True, newMap)
    {-# INLINE emplace #-}

tryAddVariable' :: Variable -> State GatherEnv ()
tryAddVariable' var = modify $ \env -> tryAddVariable var env
{-# INLINE tryAddVariable' #-}

gatherVariables :: Formula -> State GatherEnv ()
gatherVariables = \case
    PosVar v -> tryAddVariable' v
    NegVar v -> tryAddVariable' v
    Or a b -> gatherVariables a *> gatherVariables b
    And a b -> gatherVariables a *> gatherVariables b
    Empty -> error "unreachable"

gatherVariables' :: Formula -> GatherEnv
gatherVariables' fla = execState (gatherVariables fla) (GatherEnv mempty $ Var 1)

-- Tseitin

data TseitinSettings = LTRImpl | Equiv
    deriving (Eq, Show)

data TseitinEnv = TseitinEnv
    { tseitinVarMap :: VariableMap
    , tseitinCtr :: Var
    , tseitinCnf :: CNF
    , tseitinReasons :: [TseitinReason]
    , tseitinSettings :: TseitinSettings
    }

data TseitinReason
    = It'sOr Lit Lit Lit
    | It'sAnd Lit Lit Lit
    deriving (Eq, Show)

because :: TseitinReason -> State TseitinEnv ()
because r = modify $ \env ->
    let reasons = tseitinReasons env in env{tseitinReasons = r : reasons}

addClause :: Clause -> State TseitinEnv ()
addClause c = modify $ \env ->
    let cnf = tseitinCnf env in env{tseitinCnf = c `V.cons` cnf}

addClauseIfEquiv :: Clause -> State TseitinEnv ()
addClauseIfEquiv c = do
    settings <- gets tseitinSettings
    Control.Monad.when (settings == Equiv) $ do
        addClause c

fresh :: State TseitinEnv Lit
fresh = do
    v <- gets tseitinCtr
    modify $ \env ->
        let (Var ctr) = tseitinCtr env in env{tseitinCtr = Var $ ctr + 1}
    pure $ varToLit v Types.Pos

tseitinOr, tseitinAnd :: Formula -> Formula -> State TseitinEnv Lit
tseitinOr l r = do
    left <- tseitin l
    right <- tseitin r
    gate <- fresh

    because $ It'sOr gate left right

    addClauseIfEquiv [neg left, gate] -- left => gate
    addClauseIfEquiv [neg right, gate] -- right => gate
    addClause [neg gate, left, right] -- gate => (left | right)
    pure gate
tseitinAnd l r = do
    left <- tseitin l
    right <- tseitin r
    gate <- fresh

    because $ It'sAnd gate left right

    addClauseIfEquiv [neg left, neg right, gate] -- (left & right) => gate
    addClause [neg gate, left] -- gate => left
    addClause [neg gate, right] -- gate => right
    pure gate

tseitin :: Formula -> State TseitinEnv Lit
tseitin = \case
    PosVar var -> do
        varMap <- gets tseitinVarMap
        pure $ varToLit (varMap M.! var) Types.Pos-- Note: unsafe access, but we're quite sure that it _is_ there!
    NegVar var -> do
        varMap <- gets tseitinVarMap
        pure $ varToLit (varMap M.! var) Types.Neg -- Note: again, unsafe access, see above
    Or a b -> tseitinOr a b
    And a b -> tseitinAnd a b
    Empty -> error "unreachable"

data TseitinResult = TseitinResult {resultVarMap :: VariableMap, resultCounter :: Var, resultCnf :: CNF, resultReasons :: [TseitinReason], resultRoot :: Var}
    deriving (Eq, Show)

topLevelTseitin :: TseitinSettings -> VariableMap -> Var -> Formula -> TseitinResult
topLevelTseitin settings varMap ctr fla = envToResult $ runState (tseitin fla) initialEnv
  where
    initialEnv = TseitinEnv varMap ctr mempty mempty settings
    envToResult (rootLit, TseitinEnv resultVarMap resultCtr cnf reasons _) = TseitinResult resultVarMap resultCtr cnf reasons (litToVar rootLit)

tseitinResultToDimacs :: TseitinResult -> ByteString
tseitinResultToDimacs (TseitinResult varMap (Var ctr) cnf reasons rootVar) = BS.unlines $ header <> variables <> gates <> root <> cnfInfo <> clauses
  where
    pack' :: Show a => a -> ByteString
    pack' = BS.pack . show

    numOfVars = ctr - 1

    header, variables, gates, root, cnfInfo, clauses :: [ByteString]
    header = ["c generated by <sat>; enjoy"]
    variables = M.assocs varMap <&> (\(varName, varNumber) -> "c var " <> unVariable varName <> " is " <> pack' varNumber)

    gates =
        reasons <&> \case
            It'sOr gate left right -> "c gate " <> pack' gate <> " is " <> pack' left <> " or " <> pack' right
            It'sAnd gate left right -> "c gate " <> pack' gate <> " is " <> pack' left <> " and " <> pack' right

    root = ["c root is " <> pack' rootVar]

    cnfInfo = ["p cnf " <> pack' numOfVars <> " " <> pack' (V.length cnf)]

    clauses = V.toList cnf <&> dimacsClause

tseitinResultToCNFFormula :: TseitinResult -> CNFFormula
tseitinResultToCNFFormula (TseitinResult _ (Var ctr) cnf _ _) = CNFFormula cnf (ctr - 1)
