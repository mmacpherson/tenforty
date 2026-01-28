{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TenForty.Form (
    -- * Form Definition
    Form (..),
    Line (..),
    LineType (..),
    LineImportance (..),
    WorksheetStep (..),

    -- * Form Builder Monad
    FormBuilder,
    BuilderState (..),
    runFormBuilder,
    buildForm,

    -- * Line Operations
    input,
    compute,
    worksheet,
    setOutput,

    -- * Form Utilities
    formLines,
    formInputs,
    formOutputs,
    formTables,
    lookupLine,

    -- * Validation
    validateForm,
    validateFormSet,
    FormError (..),
    FormSetError (..),
) where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import TenForty.Expr hiding (Line)
import TenForty.Expr qualified as E
import TenForty.Table
import TenForty.Types

data Form = Form
    { formId :: FormId
    , formYear :: Int
    , formLineMap :: Map LineId Line
    , formLineOrder :: [LineId]
    , formOutputIds :: Set LineId
    , formTableMap :: Map TableId Table
    }
    deriving stock (Show)

data Line = Line
    { lineId :: LineId
    , lineName :: Text
    , lineDescription :: Text
    , lineImportance :: LineImportance
    , lineType :: LineType
    }
    deriving stock (Show)

data LineImportance
    = -- | User-provided input, needs clear description
      KeyInput
    | -- | Important result (AGI, tax, refund), needs clear description
      KeyOutput
    | -- | Intermediate calculation, minimal description OK
      Interior
    deriving stock (Show, Eq)

data LineType
    = LineInput
    | LineComputed (Expr Dollars)
    | LineWorksheet Text [WorksheetStep]
    deriving stock (Show)

data WorksheetStep = WorksheetStep
    { wsStepId :: LineId
    , wsStepDesc :: Text
    , wsStepExpr :: Expr Dollars
    }
    deriving stock (Show)

data BuilderState = BuilderState
    { bsLines :: Map LineId Line
    , bsOutputs :: Set LineId
    , bsTables :: Map TableId Table
    , bsLineOrder :: [LineId]
    }

emptyBuilderState :: BuilderState
emptyBuilderState =
    BuilderState
        { bsLines = Map.empty
        , bsOutputs = Set.empty
        , bsTables = Map.empty
        , bsLineOrder = []
        }

newtype FormBuilder a = FormBuilder {unFormBuilder :: State BuilderState a}
    deriving newtype (Functor, Applicative, Monad, MonadState BuilderState)

runFormBuilder :: FormBuilder a -> (a, BuilderState)
runFormBuilder fb = runState (unFormBuilder fb) emptyBuilderState

buildForm :: FormId -> Int -> FormBuilder () -> Either FormError Form
buildForm fid year builder =
    let ((), st) = runFormBuilder builder
        form =
            Form
                { formId = fid
                , formYear = year
                , formLineMap = bsLines st
                , formLineOrder = bsLineOrder st
                , formOutputIds = bsOutputs st
                , formTableMap = bsTables st
                }
     in case validateForm form of
            [] -> Right form
            err : _ -> Left err

input :: LineId -> Text -> Text -> LineImportance -> FormBuilder (Expr Dollars)
input lid name desc importance = do
    let ln =
            Line
                { lineId = lid
                , lineName = name
                , lineDescription = desc
                , lineImportance = importance
                , lineType = LineInput
                }
    modify' $ \s ->
        s
            { bsLines = Map.insert lid ln (bsLines s)
            , bsLineOrder = bsLineOrder s ++ [lid]
            }
    pure (E.Line lid)

compute :: LineId -> Text -> Text -> LineImportance -> Expr Dollars -> FormBuilder (Expr Dollars)
compute lid name desc importance expr = do
    let ln =
            Line
                { lineId = lid
                , lineName = name
                , lineDescription = desc
                , lineImportance = importance
                , lineType = LineComputed expr
                }
    modify' $ \s ->
        s
            { bsLines = Map.insert lid ln (bsLines s)
            , bsLineOrder = bsLineOrder s ++ [lid]
            }
    pure (E.Line lid)

worksheet :: LineId -> Text -> Text -> LineImportance -> [(LineId, Text, Expr Dollars)] -> FormBuilder (Expr Dollars)
worksheet lid name desc importance steps = do
    let wsSteps =
            [ WorksheetStep stepId stepDesc stepExpr
            | (stepId, stepDesc, stepExpr) <- steps
            ]
        ln =
            Line
                { lineId = lid
                , lineName = name
                , lineDescription = desc
                , lineImportance = importance
                , lineType = LineWorksheet desc wsSteps
                }
    modify' $ \s ->
        s
            { bsLines = Map.insert lid ln (bsLines s)
            , bsLineOrder = bsLineOrder s ++ [lid]
            }
    pure (E.Line lid)

setOutput :: LineId -> FormBuilder ()
setOutput lid = modify' $ \s ->
    s
        { bsOutputs = Set.insert lid (bsOutputs s)
        }

formLines :: Form -> [Line]
formLines form =
    [ ln
    | lid <- formLineOrder form
    , Just ln <- [Map.lookup lid (formLineMap form)]
    ]

formInputs :: Form -> [Line]
formInputs = filter isInput . formLines
  where
    isInput ln = case lineType ln of
        LineInput -> True
        _ -> False

formOutputs :: Form -> [Line]
formOutputs form =
    [ ln
    | lid <- Set.toList (formOutputIds form)
    , Just ln <- [Map.lookup lid (formLineMap form)]
    ]

formTables :: Form -> [Table]
formTables = Map.elems . formTableMap

lookupLine :: Form -> LineId -> Maybe Line
lookupLine form lid = Map.lookup lid (formLineMap form)

data FormError
    = UndefinedLine LineId LineId
    | CyclicDependency [LineId]
    | DuplicateLine LineId
    | UndefinedOutput LineId
    | UndefinedTable TableId
    deriving stock (Show, Eq)

validateForm :: Form -> [FormError]
validateForm form =
    checkUndefinedLines form
        ++ checkCycles form
        ++ checkUndefinedOutputs form
        ++ checkUndefinedTables form

checkUndefinedLines :: Form -> [FormError]
checkUndefinedLines form =
    [ UndefinedLine lid ref
    | ln <- formLines form
    , let lid = lineId ln
    , ref <- Set.toList (lineRefs ln)
    , not (Map.member ref (formLineMap form))
    ]

lineRefs :: Line -> Set LineId
lineRefs ln = case lineType ln of
    LineInput -> Set.empty
    LineComputed expr -> extractLineRefs expr
    LineWorksheet _ ws -> foldMap (extractLineRefs . wsStepExpr) ws

checkCycles :: Form -> [FormError]
checkCycles form =
    case detectCycle (Map.keysSet $ formLineMap form) (lineDepMap form) of
        Just cycle' -> [CyclicDependency cycle']
        Nothing -> []

lineDepMap :: Form -> Map LineId (Set LineId)
lineDepMap form =
    Map.fromList
        [ (lineId ln, lineRefs ln)
        | ln <- formLines form
        ]

detectCycle :: Set LineId -> Map LineId (Set LineId) -> Maybe [LineId]
detectCycle nodes deps = go Set.empty Set.empty (Set.toList nodes)
  where
    go _ _ [] = Nothing
    go visited inStack (n : ns)
        | Set.member n inStack = Just [n]
        | Set.member n visited = go visited inStack ns
        | otherwise =
            let neighbors = fromMaybe Set.empty (Map.lookup n deps)
                inStack' = Set.insert n inStack
             in case go visited inStack' (Set.toList neighbors) of
                    Just cycle' -> Just (n : cycle')
                    Nothing -> go (Set.insert n visited) inStack ns

checkUndefinedOutputs :: Form -> [FormError]
checkUndefinedOutputs form =
    [ UndefinedOutput lid
    | lid <- Set.toList (formOutputIds form)
    , not (Map.member lid (formLineMap form))
    ]

checkUndefinedTables :: Form -> [FormError]
checkUndefinedTables form =
    let usedTables = foldMap lineTableRefs (formLines form)
        definedTables = Map.keysSet (formTableMap form)
     in [ UndefinedTable tid
        | tid <- Set.toList usedTables
        , not (Set.member tid definedTables)
        ]

lineTableRefs :: Line -> Set TableId
lineTableRefs ln = case lineType ln of
    LineInput -> Set.empty
    LineComputed expr -> extractTableRefs expr
    LineWorksheet _ ws -> foldMap (extractTableRefs . wsStepExpr) ws

data FormSetError
    = UnresolvedImport
        { fsErrorYear :: Int
        , fsErrorSourceForm :: FormId
        , fsErrorSourceLine :: LineId
        , fsErrorTargetForm :: FormId
        , fsErrorTargetLine :: LineId
        }
    | MissingTargetForm
        { fsErrorYear :: Int
        , fsErrorSourceForm :: FormId
        , fsErrorSourceLine :: LineId
        , fsErrorTargetForm :: FormId
        , fsErrorTargetLine :: LineId
        }
    deriving stock (Show, Eq)

validateFormSet :: [Form] -> [FormSetError]
validateFormSet forms =
    let formsByYear = Map.fromListWith (++) [(formYear f, [f]) | f <- forms]
     in concatMap (uncurry validateFormSetYear) (Map.toList formsByYear)

validateFormSetYear :: Int -> [Form] -> [FormSetError]
validateFormSetYear year forms =
    let formMap = Map.fromList [(formId f, f) | f <- forms]
        availableExports = Map.map (Map.keysSet . formLineMap) formMap
     in concatMap (checkFormImports year availableExports) forms

checkFormImports :: Int -> Map FormId (Set LineId) -> Form -> [FormSetError]
checkFormImports year availableExports sourceForm =
    concat
        [ checkImport sourceForm (lineId ln) targetFid targetLid
        | ln <- formLines sourceForm
        , (targetFid, targetLid) <- Set.toList (lineImports ln)
        ]
  where
    checkImport :: Form -> LineId -> FormId -> LineId -> [FormSetError]
    checkImport srcForm srcLid targetFid targetLid =
        case Map.lookup targetFid availableExports of
            Nothing ->
                [ MissingTargetForm
                    { fsErrorYear = year
                    , fsErrorSourceForm = formId srcForm
                    , fsErrorSourceLine = srcLid
                    , fsErrorTargetForm = targetFid
                    , fsErrorTargetLine = targetLid
                    }
                ]
            Just exports ->
                if Set.member targetLid exports
                    then []
                    else
                        [ UnresolvedImport
                            { fsErrorYear = year
                            , fsErrorSourceForm = formId srcForm
                            , fsErrorSourceLine = srcLid
                            , fsErrorTargetForm = targetFid
                            , fsErrorTargetLine = targetLid
                            }
                        ]

lineImports :: Line -> Set (FormId, LineId)
lineImports ln = case lineType ln of
    LineInput -> Set.empty
    LineComputed expr -> E.extractImports expr
    LineWorksheet _ ws -> foldMap (E.extractImports . wsStepExpr) ws
