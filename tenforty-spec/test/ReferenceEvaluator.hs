{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReferenceEvaluator (
    evalGraph,
    evalGraphDetailed,
    evalGraphNode,
    findNodeByName,
) where

import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import TenForty.Compile.JSON
import TenForty.Types (FilingStatus (..))

evalGraph :: ComputationGraph -> FilingStatus -> Double -> Double
evalGraph graph status wages =
    let (_, _, tax) = evalGraphDetailed graph status wages
     in tax

evalGraphNode :: ComputationGraph -> FilingStatus -> Double -> String -> Double
evalGraphNode graph status wages targetLine =
    let evaluated = evaluateGraph graph status wages
        nodeMap = cgNodes graph
     in fromMaybe 0 $ findNodeByName nodeMap targetLine >>= flip Map.lookup evaluated

evaluateGraph :: ComputationGraph -> FilingStatus -> Double -> Map.Map Int Double
evaluateGraph graph status wages =
    let nodeMap = cgNodes graph
        tables = cgTables graph

        -- Build evaluated map strictly by evaluating nodes in order (by ID)
        -- Since graph was compiled in topological order, node IDs are already sorted
        evaluated :: Map.Map Int Double
        evaluated = foldl' evalAndInsert Map.empty (Map.toAscList nodeMap)

        evalAndInsert :: Map.Map Int Double -> (Int, Node) -> Map.Map Int Double
        evalAndInsert acc (nid, n) =
            let !v = evalOp acc nid (nodeOp n)
             in Map.insert nid v acc

        evalOp :: Map.Map Int Double -> Int -> Op -> Double
        evalOp acc nid = \case
            OpInput ->
                case nodeName (nodeMap Map.! nid) of
                    Just nm
                        | nm == "L1a" || "L1a_" `T.isPrefixOf` nm -> wages
                    _ -> 0
            OpImport{} -> 0 -- Imports from other forms treated as 0 in tests
            OpLiteral v -> v
            OpAdd l r -> lkp l + lkp r
            OpSub l r -> lkp l - lkp r
            OpMul l r -> lkp l * lkp r
            OpDiv l r -> let rv = lkp r in if rv == 0 then 0 else lkp l / rv
            OpNeg a -> negate (lkp a)
            OpAbs a -> abs (lkp a)
            OpMax l r -> max (lkp l) (lkp r)
            OpMin l r -> min (lkp l) (lkp r)
            OpFloor a -> fromIntegral (floor (lkp a) :: Int)
            OpClamp a lo hi -> max lo (min hi (lkp a))
            OpIfPositive c t e -> if lkp c > 0 then lkp t else lkp e
            OpBracketTax tblId incomeNode ->
                evalBracketTaxFromTable tables tblId status (lkp incomeNode)
            OpPhaseOut base threshold phaseRate agiNode ->
                let agiVal = lkp agiNode
                    thresholdVal = getStatusValue status threshold
                    excess = max 0 (agiVal - thresholdVal)
                    reduction = excess * phaseRate
                 in max 0 (base - reduction)
            OpByStatus sn -> lkp (getStatusNodeId status sn)
          where
            lkp i = fromMaybe 0 (Map.lookup i acc)

        evalBracketTaxFromTable :: Map.Map T.Text BracketTable -> T.Text -> FilingStatus -> Double -> Double
        evalBracketTaxFromTable tbls tid fs inc =
            case Map.lookup tid tbls of
                Nothing -> 0
                Just bt ->
                    let brackets = getStatusBrackets fs (btBrackets bt)
                     in computeBracketTax brackets inc

        getStatusBrackets :: FilingStatus -> StatusBrackets -> [Bracket]
        getStatusBrackets fs sb = case fs of
            Single -> sbSingle sb
            MarriedJoint -> sbMarriedJoint sb
            MarriedSeparate -> sbMarriedSeparate sb
            HeadOfHousehold -> sbHeadOfHousehold sb
            QualifyingWidow -> sbQualifyingWidow sb

        computeBracketTax :: [Bracket] -> Double -> Double
        computeBracketTax brackets inc = go 0 0 brackets
          where
            go !acc _ [] = acc
            go !acc prevThresh (b : bs) =
                let thresh = brThreshold b
                    bracketRate = brRate b
                    taxableInBracket = min inc thresh - prevThresh
                 in if inc <= prevThresh
                        then acc
                        else go (acc + max 0 taxableInBracket * bracketRate) thresh bs

        getStatusValue :: FilingStatus -> StatusValues -> Double
        getStatusValue fs sv = case fs of
            Single -> svSingle sv
            MarriedJoint -> svMarriedJoint sv
            MarriedSeparate -> svMarriedSeparate sv
            HeadOfHousehold -> svHeadOfHousehold sv
            QualifyingWidow -> svQualifyingWidow sv

        getStatusNodeId :: FilingStatus -> StatusNodeIds -> Int
        getStatusNodeId fs sn = case fs of
            Single -> snSingle sn
            MarriedJoint -> snMarriedJoint sn
            MarriedSeparate -> snMarriedSeparate sn
            HeadOfHousehold -> snHeadOfHousehold sn
            QualifyingWidow -> snQualifyingWidow sn
     in evaluated

evalGraphDetailed :: ComputationGraph -> FilingStatus -> Double -> (Double, Double, Double)
evalGraphDetailed graph status wages =
    let evaluated = evaluateGraph graph status wages
        nodeMap = cgNodes graph
        agi = fromMaybe 0 $ findNodeByName nodeMap "L11" >>= flip Map.lookup evaluated
        taxableIncome = fromMaybe 0 $ findNodeByName nodeMap "L15" >>= flip Map.lookup evaluated
        tax = fromMaybe 0 $ findNodeByName nodeMap "L16" >>= flip Map.lookup evaluated
     in (agi, taxableIncome, tax)

findNodeByName :: Map.Map Int Node -> String -> Maybe Int
findNodeByName nodeMap name =
    let target = T.pack name
        matches nm = nm == target || (target <> "_") `T.isPrefixOf` nm
     in case [ nodeId n
             | n <- Map.elems nodeMap
             , Just nm <- [nodeName n]
             , matches nm
             ] of
            (nid : _) -> Just nid
            [] -> Nothing
