{-# LANGUAGE RecordWildCards #-}

module TenForty.Compile.JSON (
    -- * Compilation
    compileForm,
    compileFormToJSON,

    -- * Graph Types
    ComputationGraph (..),
    GraphMeta (..),
    Node (..),
    Op (..),
    BracketTable (..),
    StatusBrackets (..),
    StatusValues (..),
    StatusNodeIds (..),
    Bracket (..),
) where

import Control.Monad (forM, forM_)
import Control.Monad.State.Strict
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import TenForty.Expr hiding (Line)
import TenForty.Expr qualified as E
import TenForty.Form
import TenForty.Table qualified as T
import TenForty.Types

verboseLineId :: Line -> Text
verboseLineId ln =
    let lid = unLineId (lineId ln)
        name = lineName ln
     in if T.null name
            || T.isInfixOf "_" lid
            || not (T.isPrefixOf "L" lid)
            then lid
            else lid <> "_" <> name

data ComputationGraph = ComputationGraph
    { cgMeta :: GraphMeta
    , cgNodes :: Map Int Node
    , cgTables :: Map Text BracketTable
    , cgInputs :: [Int]
    , cgOutputs :: [Int]
    , cgImports :: [(Text, Text, Int)] -- [(form_id, line_id, year)]
    }
    deriving stock (Show)

data GraphMeta = GraphMeta
    { gmFormId :: Text
    , gmYear :: Int
    , gmGeneratedBy :: Text
    }
    deriving stock (Show)

data Node = Node
    { nodeId :: Int
    , nodeName :: Maybe Text
    , nodeOp :: Op
    }
    deriving stock (Show)

data Op
    = OpInput
    | OpImport Text Text Int -- (form_id, line_id, year)
    | OpLiteral Double
    | OpAdd Int Int
    | OpSub Int Int
    | OpMul Int Int
    | OpDiv Int Int
    | OpNeg Int
    | OpAbs Int
    | OpMax Int Int
    | OpMin Int Int
    | OpFloor Int
    | OpClamp Int Double Double
    | OpIfPositive Int Int Int
    | OpBracketTax Text Int
    | OpPhaseOut Double StatusValues Double Int
    | OpByStatus StatusNodeIds
    deriving stock (Show)

data StatusValues = StatusValues
    { svSingle :: Double
    , svMarriedJoint :: Double
    , svMarriedSeparate :: Double
    , svHeadOfHousehold :: Double
    , svQualifyingWidow :: Double
    }
    deriving stock (Show)

data StatusNodeIds = StatusNodeIds
    { snSingle :: Int
    , snMarriedJoint :: Int
    , snMarriedSeparate :: Int
    , snHeadOfHousehold :: Int
    , snQualifyingWidow :: Int
    }
    deriving stock (Show)

newtype BracketTable = BracketTable
    { btBrackets :: StatusBrackets
    }
    deriving stock (Show)

data StatusBrackets = StatusBrackets
    { sbSingle :: [Bracket]
    , sbMarriedJoint :: [Bracket]
    , sbMarriedSeparate :: [Bracket]
    , sbHeadOfHousehold :: [Bracket]
    , sbQualifyingWidow :: [Bracket]
    }
    deriving stock (Show)

data Bracket = Bracket
    { brThreshold :: Double
    , brRate :: Double
    }
    deriving stock (Show)

instance ToJSON ComputationGraph where
    toJSON ComputationGraph{..} =
        object
            [ "meta" .= cgMeta
            , "nodes" .= nodeMapToObject cgNodes
            , "tables" .= cgTables
            , "inputs" .= cgInputs
            , "outputs" .= cgOutputs
            , "imports" .= map importToJSON cgImports
            ]
      where
        nodeMapToObject :: Map Int Node -> Value
        nodeMapToObject m =
            Aeson.Object $
                KM.fromList
                    [ (Key.fromText (T.pack (show k)), toJSON v)
                    | (k, v) <- Map.toList m
                    ]

        importToJSON :: (Text, Text, Int) -> Value
        importToJSON (fid, lid, yr) = object ["form" .= fid, "line" .= lid, "year" .= yr]

instance ToJSON GraphMeta where
    toJSON GraphMeta{..} =
        object
            [ "form_id" .= gmFormId
            , "year" .= gmYear
            , "generated_by" .= gmGeneratedBy
            ]

instance ToJSON Node where
    toJSON Node{..} =
        let baseFields = ["id" .= nodeId, "op" .= nodeOp]
            nameField = maybe [] (\n -> ["name" .= n]) nodeName
         in object (baseFields ++ nameField)

instance ToJSON Op where
    toJSON = \case
        OpInput -> object ["type" .= ("input" :: Text)]
        OpImport fid lid yr -> object ["type" .= ("import" :: Text), "form" .= fid, "line" .= lid, "year" .= yr]
        OpLiteral v -> object ["type" .= ("literal" :: Text), "value" .= v]
        OpAdd a b -> object ["type" .= ("add" :: Text), "left" .= a, "right" .= b]
        OpSub a b -> object ["type" .= ("sub" :: Text), "left" .= a, "right" .= b]
        OpMul a b -> object ["type" .= ("mul" :: Text), "left" .= a, "right" .= b]
        OpDiv a b -> object ["type" .= ("div" :: Text), "left" .= a, "right" .= b]
        OpNeg a -> object ["type" .= ("neg" :: Text), "arg" .= a]
        OpAbs a -> object ["type" .= ("abs" :: Text), "arg" .= a]
        OpMax a b -> object ["type" .= ("max" :: Text), "left" .= a, "right" .= b]
        OpMin a b -> object ["type" .= ("min" :: Text), "left" .= a, "right" .= b]
        OpFloor a -> object ["type" .= ("floor" :: Text), "arg" .= a]
        OpClamp a lo hi -> object ["type" .= ("clamp" :: Text), "arg" .= a, "min" .= lo, "max" .= hi]
        OpIfPositive c t e -> object ["type" .= ("if_positive" :: Text), "cond" .= c, "then" .= t, "otherwise" .= e]
        OpBracketTax t a -> object ["type" .= ("bracket_tax" :: Text), "table" .= t, "income" .= a]
        OpPhaseOut base threshold r agi ->
            object
                [ "type" .= ("phase_out" :: Text)
                , "base" .= base
                , "threshold" .= threshold
                , "rate" .= r
                , "agi" .= agi
                ]
        OpByStatus sn ->
            object
                [ "type" .= ("by_status" :: Text)
                , "values" .= sn
                ]

instance ToJSON StatusValues where
    toJSON StatusValues{..} =
        object
            [ "single" .= svSingle
            , "married_joint" .= svMarriedJoint
            , "married_separate" .= svMarriedSeparate
            , "head_of_household" .= svHeadOfHousehold
            , "qualifying_widow" .= svQualifyingWidow
            ]

instance ToJSON StatusNodeIds where
    toJSON StatusNodeIds{..} =
        object
            [ "single" .= snSingle
            , "married_joint" .= snMarriedJoint
            , "married_separate" .= snMarriedSeparate
            , "head_of_household" .= snHeadOfHousehold
            , "qualifying_widow" .= snQualifyingWidow
            ]

instance ToJSON BracketTable where
    toJSON BracketTable{..} =
        object
            [ "brackets" .= btBrackets
            ]

instance ToJSON StatusBrackets where
    toJSON StatusBrackets{..} =
        object
            [ "single" .= sbSingle
            , "married_joint" .= sbMarriedJoint
            , "married_separate" .= sbMarriedSeparate
            , "head_of_household" .= sbHeadOfHousehold
            , "qualifying_widow" .= sbQualifyingWidow
            ]

instance ToJSON Bracket where
    toJSON Bracket{..} =
        object
            [ "threshold" .= brThreshold
            , "rate" .= brRate
            ]

data CompileState = CompileState
    { csNextId :: Int
    , csNodes :: Map Int Node
    , csLineToId :: Map LineId Int
    , csInputIds :: [Int]
    , csImports :: [(Text, Text, Int)] -- [(form_id, line_id, year)]
    , csYear :: Int
    , csFormId :: FormId
    }

newtype Compile a = Compile {unCompile :: State CompileState a}
    deriving newtype (Functor, Applicative, Monad, MonadState CompileState)

runCompile :: Form -> Compile a -> (a, Map Int Node, [Int], [(Text, Text, Int)])
runCompile frm c =
    let (a, st) = runState (unCompile c) (initState frm)
     in (a, csNodes st, reverse (csInputIds st), reverse (csImports st))
  where
    initState form =
        CompileState
            { csNextId = 0
            , csNodes = Map.empty
            , csLineToId = Map.empty
            , csInputIds = []
            , csImports = []
            , csYear = formYear form
            , csFormId = formId form
            }

freshId :: Compile Int
freshId = do
    n <- gets csNextId
    modify' $ \s -> s{csNextId = n + 1}
    pure n

emitNode :: Maybe Text -> Op -> Compile Int
emitNode mname op = do
    nid <- freshId
    let node = Node nid mname op
    modify' $ \s -> s{csNodes = Map.insert nid node (csNodes s)}
    pure nid

emitNamedNode :: Text -> Op -> Compile Int
emitNamedNode name = emitNode (Just name)

emitAnonymousNode :: Op -> Compile Int
emitAnonymousNode = emitNode Nothing

registerLine :: LineId -> Int -> Compile ()
registerLine lid nid = modify' $ \s ->
    s
        { csLineToId = Map.insert lid nid (csLineToId s)
        }

registerInput :: Int -> Compile ()
registerInput nid = modify' $ \s ->
    s
        { csInputIds = nid : csInputIds s
        }

registerImport :: Text -> Text -> Int -> Compile ()
registerImport fid lid yr = modify' $ \s ->
    s
        { csImports = (fid, lid, yr) : csImports s
        }

getYear :: Compile Int
getYear = gets csYear

getFormId :: Compile FormId
getFormId = gets csFormId

lookupLineId :: LineId -> Compile (Maybe Int)
lookupLineId lid = gets (Map.lookup lid . csLineToId)

compileForm :: Form -> ComputationGraph
compileForm frm =
    let (outputIds, nodes, inputIds, imports) = runCompile frm (compileLines frm)
        tables = Map.fromList [(getTableId tbl, compileTable tbl) | tbl <- formTables frm]
     in ComputationGraph
            { cgMeta =
                GraphMeta
                    { gmFormId = unFormId (formId frm)
                    , gmYear = formYear frm
                    , gmGeneratedBy = "tenforty-dsl"
                    }
            , cgNodes = nodes
            , cgTables = tables
            , cgInputs = inputIds
            , cgOutputs = outputIds
            , cgImports = imports
            }

getTableId :: T.Table -> Text
getTableId tbl = unTableId (T.tableId tbl)

compileFormToJSON :: Form -> ByteString
compileFormToJSON = Aeson.encode . compileForm

compileLines :: Form -> Compile [Int]
compileLines frm = do
    forM_ (formInputs frm) $ \ln -> do
        nid <- emitNamedNode (verboseLineId ln) OpInput
        registerLine (lineId ln) nid
        registerInput nid

    forM_ (formLines frm) $ \ln ->
        case lineType ln of
            LineInput -> pure ()
            LineComputed expr -> do
                nid <- compileExpr (Just $ verboseLineId ln) expr
                registerLine (lineId ln) nid
            LineWorksheet _ steps -> do
                forM_ steps $ \step -> do
                    nid <- compileExpr (Just $ unLineId (wsStepId step)) (wsStepExpr step)
                    registerLine (wsStepId step) nid
                case steps of
                    [] -> pure ()
                    _ -> do
                        let lastStep = last steps
                        mLastId <- lookupLineId (wsStepId lastStep)
                        for_ mLastId (registerLine (lineId ln))

    forM (formOutputs frm) $ \ln -> do
        mNid <- lookupLineId (lineId ln)
        nid <- case mNid of
            Just existing -> pure existing
            Nothing -> do
                fid <- getFormId
                error $
                    "Missing output line during compile: "
                        <> T.unpack (unFormId fid)
                        <> " -> "
                        <> T.unpack (unLineId (lineId ln))
        ensureOutputNamed ln nid

ensureOutputNamed :: Line -> Int -> Compile Int
ensureOutputNamed ln nid = do
    nodes <- gets csNodes
    let desired = verboseLineId ln
        lid = unLineId (lineId ln)
        okName nm = nm == lid || T.isPrefixOf (lid <> "_") nm
        currentName = Map.lookup nid nodes >>= nodeName
    if maybe False okName currentName
        then pure nid
        else do
            zero <- emitAnonymousNode (OpLiteral 0)
            emitNamedNode desired (OpAdd nid zero)

compileExpr :: Maybe Text -> Expr u -> Compile Int
compileExpr mname = \case
    Lit (Amount v) -> emitNode mname (OpLiteral v)
    E.Line lid -> do
        mNid <- lookupLineId lid
        case mNid of
            Just nid -> pure nid
            Nothing -> do
                fid <- getFormId
                error $
                    "Missing line reference during compile: "
                        <> T.unpack (unFormId fid)
                        <> " -> "
                        <> T.unpack (unLineId lid)
    Import (FormId fid) (LineId lid) -> do
        yr <- getYear
        registerImport fid lid yr
        emitNode mname (OpImport fid lid yr)
    Add a b -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        emitNode mname (OpAdd aid bid)
    Sub a b -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        emitNode mname (OpSub aid bid)
    Mul a b -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        emitNode mname (OpMul aid bid)
    Div a b -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        emitNode mname (OpDiv aid bid)
    Neg a -> do
        aid <- compileExpr Nothing a
        emitNode mname (OpNeg aid)
    Max a b -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        emitNode mname (OpMax aid bid)
    Min a b -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        emitNode mname (OpMin aid bid)
    IfPos c t e -> do
        cid <- compileExpr Nothing c
        tid <- compileExpr Nothing t
        eid <- compileExpr Nothing e
        emitNode mname (OpIfPositive cid tid eid)
    IfNeg c t e -> do
        cid <- compileExpr Nothing c
        negCid <- emitAnonymousNode (OpNeg cid)
        tid <- compileExpr Nothing t
        eid <- compileExpr Nothing e
        emitNode mname (OpIfPositive negCid tid eid)
    IfGte a b t e -> do
        aid <- compileExpr Nothing a
        bid <- compileExpr Nothing b
        diffId <- emitAnonymousNode (OpSub aid bid)
        tid <- compileExpr Nothing t
        eid <- compileExpr Nothing e
        emitNode mname (OpIfPositive diffId tid eid)
    Floor a -> do
        aid <- compileExpr Nothing a
        emitNode mname (OpFloor aid)
    Round a -> do
        aid <- compileExpr Nothing a
        halfId <- emitAnonymousNode (OpLiteral 0.5)
        plusHalfId <- emitAnonymousNode (OpAdd aid halfId)
        emitNode mname (OpFloor plusHalfId)
    BracketTax (TableId tid) income -> do
        iid <- compileExpr Nothing income
        emitNode mname (OpBracketTax tid iid)
    E.TableLookup (TableId tid) amount -> do
        aid <- compileExpr Nothing amount
        emitNode mname (OpBracketTax tid aid)
    PhaseOut base threshold rateE agi -> do
        baseVal <- evalConstExpr base
        thresholdVals <- evalStatusExpr threshold
        rateVal <- evalConstExpr rateE
        aid <- compileExpr Nothing agi
        emitNode mname (OpPhaseOut baseVal thresholdVals rateVal aid)
    ByStatusE bs -> do
        sId <- compileExpr Nothing (bsSingle bs)
        mjId <- compileExpr Nothing (bsMarriedJoint bs)
        msId <- compileExpr Nothing (bsMarriedSeparate bs)
        hhId <- compileExpr Nothing (bsHeadOfHousehold bs)
        qwId <- compileExpr Nothing (bsQualifyingWidow bs)
        emitNode mname (OpByStatus $ StatusNodeIds sId mjId msId hhId qwId)

evalConstExpr :: Expr u -> Compile Double
evalConstExpr = \case
    Lit (Amount v) -> pure v
    _ -> pure 0

evalStatusExpr :: Expr u -> Compile StatusValues
evalStatusExpr = \case
    ByStatusE bs -> do
        s <- evalConstExpr (bsSingle bs)
        mj <- evalConstExpr (bsMarriedJoint bs)
        ms <- evalConstExpr (bsMarriedSeparate bs)
        hh <- evalConstExpr (bsHeadOfHousehold bs)
        qw <- evalConstExpr (bsQualifyingWidow bs)
        pure $ StatusValues s mj ms hh qw
    Lit (Amount v) -> pure $ StatusValues v v v v v
    _ -> pure $ StatusValues 0 0 0 0 0

compileTable :: T.Table -> BracketTable
compileTable = \case
    T.TableBracket _ bt -> compileBracketTable bt
    T.TableLookup _ _ -> BracketTable (StatusBrackets [] [] [] [] [])

compileBracketTable :: T.BracketTable -> BracketTable
compileBracketTable bt =
    BracketTable
        { btBrackets =
            StatusBrackets
                { sbSingle = compileBracketsForStatus Single bt
                , sbMarriedJoint = compileBracketsForStatus MarriedJoint bt
                , sbMarriedSeparate = compileBracketsForStatus MarriedSeparate bt
                , sbHeadOfHousehold = compileBracketsForStatus HeadOfHousehold bt
                , sbQualifyingWidow = compileBracketsForStatus QualifyingWidow bt
                }
        }

compileBracketsForStatus :: FilingStatus -> T.BracketTable -> [Bracket]
compileBracketsForStatus status bt =
    [ Bracket
        { brThreshold = unAmount (forStatus (T.bracketThreshold b) status)
        , brRate = unAmount (T.bracketRate b)
        }
    | b <- T.bracketTableList bt
    ]
