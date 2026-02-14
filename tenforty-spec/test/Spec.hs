{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (void)
import Data.Foldable (foldl', forM_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Environment (lookupEnv)
import Test.Hspec
import Test.QuickCheck
import Text.Read (readMaybe)

import CA540_2024
import CA540_2025
import CAFTB3506_2024
import CAFTB3506_2025
import CAFTB3514_2024
import CAFTB3514_2025
import CAScheduleCA_2024
import CAScheduleCA_2025
import ExprProperties qualified
import ReferenceEvaluator (evalGraph, evalGraphDetailed)
import Tables2024
import Tables2025
import TablesCA2024
import TablesCA2025
import TenForty
import TenForty.Compile.JSON qualified as JSON
import TenForty.Expr (extractLineRefs, extractTableRefs)
import US1040_2024
import US1040_2025
import USForm2441_2024
import USForm2441_2025
import USForm6251_2024
import USForm6251_2025
import USForm8812_2024
import USForm8812_2025
import USForm8863_2024
import USForm8863_2025
import USForm8959_2024
import USForm8959_2025
import USForm8960_2024
import USForm8960_2025
import USForm8995_2024
import USForm8995_2025
import USSchedule1_2024
import USSchedule1_2025
import USSchedule2_2024
import USSchedule2_2025
import USSchedule3_2024
import USSchedule3_2025
import USScheduleA_2024
import USScheduleA_2025
import USScheduleB_2024
import USScheduleB_2025
import USScheduleD_2024
import USScheduleD_2025
import USScheduleEIC_2024
import USScheduleEIC_2025
import USScheduleSE_2024
import USScheduleSE_2025

data TaxYear = TaxYear
    { tyYear :: Int
    , tyBrackets :: NonEmpty Bracket
    , tyStdDed :: ByStatus (Amount Dollars)
    , tyForm :: Either FormError Form
    }

taxYears :: [TaxYear]
taxYears =
    [ TaxYear 2024 federalBrackets2024 standardDeduction2024 us1040_2024
    , TaxYear 2025 federalBrackets2025 standardDeduction2025 us1040_2025
    ]

data CATaxYear = CATaxYear
    { caYear :: Int
    , caBrackets :: NonEmpty Bracket
    , _caStdDed :: ByStatus (Amount Dollars)
    , caForm :: Either FormError Form
    }

caTaxYears :: [CATaxYear]
caTaxYears =
    [ CATaxYear 2024 californiaBrackets2024 caStandardDeduction2024 ca540_2024
    , CATaxYear 2025 californiaBrackets2025 caStandardDeduction2025 ca540_2025
    ]

getExampleCount :: IO Int
getExampleCount = do
    env <- lookupEnv "QUICKCHECK_EXAMPLES"
    pure $ fromMaybe 1000 (env >>= readMaybe)

main :: IO ()
main = do
    n <- getExampleCount
    hspec $ parallel $ spec n

spec :: Int -> Spec
spec n = do
    ExprProperties.spec

    describe "FilingStatus" $ do
        it "has exactly 5 values" $
            length allFilingStatuses `shouldBe` 5

        it "byStatus covers all statuses" $
            let bs = byStatus 1 2 3 4 5 :: ByStatus Int
             in map (forStatus bs) allFilingStatuses `shouldBe` [1, 2, 3, 4, 5]

    describe "Amount" $ do
        it "addition is commutative" $ property $ \(a :: Double) (b :: Double) ->
            Amount a + Amount b == (Amount b + Amount a :: Amount Dollars)

        it "multiplication by rate scales correctly" $ property $ \(d :: Double) (r :: Double) ->
            unAmount (Amount d * Amount r :: Amount Dollars) == d * r

    describe "BracketTable" $ do
        forM_ taxYears $ \ty -> do
            let yr = show (tyYear ty)

            it ("validates federal brackets " ++ yr) $
                case mkBracketTable (tyBrackets ty) of
                    Right _ -> pure ()
                    Left err -> expectationFailure err

            it ("bracket tax is non-negative (" ++ yr ++ ")") $ property $ \(Positive inc) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (tyBrackets ty) of
                        Right bt -> evalBracketTax bt status (Amount inc) >= 0
                        Left _ -> False

            it ("bracket tax is monotonic in income (" ++ yr ++ ")") $ property $ \(Positive base) (Positive extra) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (tyBrackets ty) of
                        Right bt ->
                            let tax1 = evalBracketTax bt status (Amount base)
                                tax2 = evalBracketTax bt status (Amount (base + extra))
                             in tax2 >= tax1
                        Left _ -> False

            it ("bracket tax is less than income (" ++ yr ++ ")") $ property $ \(Positive inc) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (tyBrackets ty) of
                        Right bt ->
                            let tax = evalBracketTax bt status (Amount inc)
                             in tax <= Amount inc
                        Left _ -> False

            it ("marginal rate is bounded 0-37% (" ++ yr ++ ")") $ property $ \(Positive inc) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (tyBrackets ty) of
                        Right bt ->
                            let r = marginalRate bt status (Amount inc)
                             in r >= 0 && r <= 0.37
                        Left _ -> False

    describe "California BracketTable" $ do
        forM_ caTaxYears $ \cty -> do
            let yr = show (caYear cty)

            it ("validates California brackets " ++ yr) $
                case mkBracketTable (caBrackets cty) of
                    Right _ -> pure ()
                    Left err -> expectationFailure err

            it ("CA bracket tax is non-negative (" ++ yr ++ ")") $ property $ \(Positive inc) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (caBrackets cty) of
                        Right bt -> evalBracketTax bt status (Amount inc) >= 0
                        Left _ -> False

            it ("CA bracket tax is monotonic in income (" ++ yr ++ ")") $ property $ \(Positive base) (Positive extra) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (caBrackets cty) of
                        Right bt ->
                            let tax1 = evalBracketTax bt status (Amount base)
                                tax2 = evalBracketTax bt status (Amount (base + extra))
                             in tax2 >= tax1
                        Left _ -> False

            it ("CA bracket tax is less than income (" ++ yr ++ ")") $ property $ \(Positive inc) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (caBrackets cty) of
                        Right bt ->
                            let tax = evalBracketTax bt status (Amount inc)
                             in tax <= Amount inc
                        Left _ -> False

            it ("CA marginal rate is bounded 0-12.3% (" ++ yr ++ ")") $ property $ \(Positive inc) ->
                forAll (elements allFilingStatuses) $ \status ->
                    case mkBracketTable (caBrackets cty) of
                        Right bt ->
                            let r = marginalRate bt status (Amount inc)
                             in r >= 0 && r <= 0.123
                        Left _ -> False

    describe "PhaseOut" $ do
        let ctc = childTaxCredit2025

        it "phase-out decreases as AGI increases past threshold" $ property $ \(Positive excess) ->
            forAll (elements allFilingStatuses) $ \status ->
                let threshold = unAmount (forStatus (poThreshold ctc) status)
                    credit1 = evalPhaseOut ctc status (Amount threshold)
                    credit2 = evalPhaseOut ctc status (Amount (threshold + excess))
                 in credit2 <= credit1

        it "phase-out never exceeds base" $ property $ \(Positive agi) ->
            forAll (elements allFilingStatuses) $ \status ->
                evalPhaseOut ctc status (Amount agi) <= poBase ctc

        it "phase-out is non-negative" $ property $ \(Positive agi) ->
            forAll (elements allFilingStatuses) $ \status ->
                evalPhaseOut ctc status (Amount agi) >= 0

    describe "Form Validation" $ do
        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " is valid") $
                case tyForm ty of
                    Right _ -> pure ()
                    Left err -> expectationFailure $ show err

        it "detects unresolved cross-form imports in a form set" $ do
            let fid = FormId . T.pack
                lid = LineId . T.pack
            case form (fid "target") 2024 (void (input (lid "L1") "Target Line" "" Interior)) of
                Left err -> expectationFailure $ show err
                Right targetForm ->
                    case form
                        (fid "source")
                        2024
                        (void (compute (lid "L2") "Source Line" "" Interior (importForm (fid "target") (lid "L9")))) of
                        Left err -> expectationFailure $ show err
                        Right sourceForm -> do
                            let errors = validateFormSet [sourceForm, targetForm]
                            errors
                                `shouldBe` [ UnresolvedImport
                                                { fsErrorYear = 2024
                                                , fsErrorSourceForm = fid "source"
                                                , fsErrorSourceLine = lid "L2"
                                                , fsErrorTargetForm = fid "target"
                                                , fsErrorTargetLine = lid "L9"
                                                }
                                           ]

        it "US Schedule 1 2024 is valid" $
            case usSchedule1_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule 1 2025 is valid" $
            case usSchedule1_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule 2 2024 is valid" $
            case usSchedule2_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule 2 2025 is valid" $
            case usSchedule2_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule 3 2024 is valid" $
            case usSchedule3_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule 3 2025 is valid" $
            case usSchedule3_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule A 2024 is valid" $
            case usScheduleA_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule A 2025 is valid" $
            case usScheduleA_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule D 2024 is valid" $
            case usScheduleD_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule D 2025 is valid" $
            case usScheduleD_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule B 2024 is valid" $
            case usScheduleB_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule B 2025 is valid" $
            case usScheduleB_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule SE 2024 is valid" $
            case usScheduleSE_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule SE 2025 is valid" $
            case usScheduleSE_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule EIC 2024 is valid" $
            case usScheduleEIC_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Schedule EIC 2025 is valid" $
            case usScheduleEIC_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 2441 2024 is valid" $
            case usForm2441_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 2441 2025 is valid" $
            case usForm2441_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 6251 2024 is valid" $
            case usForm6251_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 6251 2025 is valid" $
            case usForm6251_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8812 2024 is valid" $
            case usForm8812_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8812 2025 is valid" $
            case usForm8812_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8863 2024 is valid" $
            case usForm8863_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8863 2025 is valid" $
            case usForm8863_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8959 2024 is valid" $
            case usForm8959_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8959 2025 is valid" $
            case usForm8959_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8960 2024 is valid" $
            case usForm8960_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "US Form 8960 2025 is valid" $
            case usForm8960_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        forM_ caTaxYears $ \cty ->
            it ("CA 540 " ++ show (caYear cty) ++ " is valid") $
                case caForm cty of
                    Right _ -> pure ()
                    Left err -> expectationFailure $ show err

        it "CA Schedule CA 2024 is valid" $
            case caScheduleCA_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "CA Schedule CA 2025 is valid" $
            case caScheduleCA_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "CA FTB 3506 2024 is valid" $
            case caFTB3506_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "CA FTB 3506 2025 is valid" $
            case caFTB3506_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "CA FTB 3514 2024 is valid" $
            case caFTB3514_2024 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

        it "CA FTB 3514 2025 is valid" $
            case caFTB3514_2025 of
                Right _ -> pure ()
                Left err -> expectationFailure $ show err

    describe "Form Compilation" $ do
        forM_ taxYears $ \ty ->
            it ("compiles US 1040 " ++ show (tyYear ty) ++ " to valid JSON") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        gmFormId (cgMeta graph) `shouldBe` "us_1040"
                        gmYear (cgMeta graph) `shouldBe` tyYear ty
                        Map.size (cgNodes graph) `shouldSatisfy` (> 0)
                        cgInputs graph `shouldSatisfy` (not . null)
                        cgOutputs graph `shouldSatisfy` (not . null)
                    Left err -> expectationFailure $ show err

        it "compiles US Schedule 1 2024 to valid JSON" $
            case usSchedule1_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_1"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule 1 2025 to valid JSON" $
            case usSchedule1_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_1"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Schedule 2 2024 to valid JSON" $
            case usSchedule2_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_2"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule 2 2025 to valid JSON" $
            case usSchedule2_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_2"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Schedule 3 2024 to valid JSON" $
            case usSchedule3_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_3"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule 3 2025 to valid JSON" $
            case usSchedule3_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_3"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Schedule A 2024 to valid JSON" $
            case usScheduleA_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_a"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule A 2025 to valid JSON" $
            case usScheduleA_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_a"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Schedule D 2024 to valid JSON" $
            case usScheduleD_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_d"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule D 2025 to valid JSON" $
            case usScheduleD_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_d"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Schedule B 2024 to valid JSON" $
            case usScheduleB_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_b"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule B 2025 to valid JSON" $
            case usScheduleB_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_b"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Schedule SE 2024 to valid JSON" $
            case usScheduleSE_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_se"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Schedule SE 2025 to valid JSON" $
            case usScheduleSE_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_schedule_se"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 2441 2024 to valid JSON" $
            case usForm2441_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_2441"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 2441 2025 to valid JSON" $
            case usForm2441_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_2441"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 6251 2024 to valid JSON" $
            case usForm6251_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_6251"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 6251 2025 to valid JSON" $
            case usForm6251_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_6251"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 8812 2024 to valid JSON" $
            case usForm8812_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8812"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 8812 2025 to valid JSON" $
            case usForm8812_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8812"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 8863 2024 to valid JSON" $
            case usForm8863_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8863"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 8863 2025 to valid JSON" $
            case usForm8863_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8863"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 8995 2024 to valid JSON" $
            case usForm8995_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8995"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 8995 2025 to valid JSON" $
            case usForm8995_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8995"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 8959 2024 to valid JSON" $
            case usForm8959_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8959"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 8959 2025 to valid JSON" $
            case usForm8959_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8959"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles US Form 8960 2024 to valid JSON" $
            case usForm8960_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8960"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles US Form 8960 2025 to valid JSON" $
            case usForm8960_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "us_form_8960"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        forM_ caTaxYears $ \cty ->
            it ("compiles CA 540 " ++ show (caYear cty) ++ " to valid JSON") $
                case caForm cty of
                    Right frm -> do
                        let graph = compileForm frm
                        gmFormId (cgMeta graph) `shouldBe` "ca_540"
                        gmYear (cgMeta graph) `shouldBe` caYear cty
                        Map.size (cgNodes graph) `shouldSatisfy` (> 0)
                        cgInputs graph `shouldSatisfy` (not . null)
                        cgOutputs graph `shouldSatisfy` (not . null)
                    Left err -> expectationFailure $ show err

        it "compiles CA Schedule CA 2024 to valid JSON" $
            case caScheduleCA_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "ca_schedule_ca"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles CA Schedule CA 2025 to valid JSON" $
            case caScheduleCA_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "ca_schedule_ca"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles CA FTB 3506 2024 to valid JSON" $
            case caFTB3506_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "ca_ftb_3506"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles CA FTB 3506 2025 to valid JSON" $
            case caFTB3506_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "ca_ftb_3506"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

        it "compiles CA FTB 3514 2024 to valid JSON" $
            case caFTB3514_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "ca_ftb_3514"
                    gmYear (cgMeta graph) `shouldBe` 2024
                Left err -> expectationFailure $ show err

        it "compiles CA FTB 3514 2025 to valid JSON" $
            case caFTB3514_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    gmFormId (cgMeta graph) `shouldBe` "ca_ftb_3514"
                    gmYear (cgMeta graph) `shouldBe` 2025
                Left err -> expectationFailure $ show err

    describe "Expression extraction" $ do
        it "extracts line references correctly" $ do
            let expr = line "L1" .+. line "L2" .-. line "L1"
                refs = extractLineRefs expr
            refs `shouldBe` Set.fromList ["L1", "L2"]

        it "extracts table references correctly" $ do
            let expr = bracketTax "federal_brackets_2025" (line "L15")
                refs = extractTableRefs expr
            refs `shouldBe` Set.fromList ["federal_brackets_2025"]

    describe "Cross-form imports" $ do
        forM_ caTaxYears $ \cty ->
            it ("CA 540 " ++ show (caYear cty) ++ " imports federal AGI with correct year") $
                case caForm cty of
                    Right frm -> do
                        let graph = compileForm frm
                        -- Check graph-level imports list
                        cgImports graph `shouldSatisfy` (not . null)
                        cgImports graph `shouldContain` [("us_1040", "L11", caYear cty)]
                    Left err -> expectationFailure $ show err

        forM_ caTaxYears $ \cty ->
            it ("CA 540 " ++ show (caYear cty) ++ " has OpImport nodes with correct year") $
                case caForm cty of
                    Right frm -> do
                        let graph = compileForm frm
                            importNodes = filter isImportOp (Map.elems $ cgNodes graph)
                        importNodes `shouldSatisfy` (not . null)
                        -- All imports should have the form's year
                        forM_ importNodes $ \node ->
                            case nodeOp node of
                                JSON.OpImport _fid _lid yr -> do
                                    yr `shouldBe` caYear cty
                                _ -> expectationFailure "Expected OpImport"
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports from Schedule D") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldSatisfy` (not . null)
                        cgImports graph `shouldContain` [("us_schedule_d", "L16", tyYear ty)]
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports from Schedule 1") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_schedule_1", "L10", tyYear ty)]
                        cgImports graph `shouldContain` [("us_schedule_1", "L26", tyYear ty)]
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports from Schedule 2") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_schedule_2", "L3", tyYear ty)]
                        cgImports graph `shouldContain` [("us_schedule_2", "L21", tyYear ty)]
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports from Schedule 3") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_schedule_3", "L8", tyYear ty)]
                        cgImports graph `shouldContain` [("us_schedule_3", "L15", tyYear ty)]
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports from Schedule A") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_schedule_a", "L17", tyYear ty)]
                    Left err -> expectationFailure $ show err

        it "US Schedule A 2024 imports from 1040 for AGI" $
            case usScheduleA_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_1040", "L11", 2024)]
                Left err -> expectationFailure $ show err

        it "US Schedule A 2025 imports from 1040 for AGI" $
            case usScheduleA_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_1040", "L11", 2025)]
                Left err -> expectationFailure $ show err

        forM_ caTaxYears $ \cty ->
            it ("CA 540 " ++ show (caYear cty) ++ " imports from Schedule CA") $
                case caForm cty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("ca_schedule_ca", "TOTAL_SUB", caYear cty)]
                        cgImports graph `shouldContain` [("ca_schedule_ca", "TOTAL_ADD", caYear cty)]
                    Left err -> expectationFailure $ show err

        forM_ caTaxYears $ \cty ->
            it ("CA 540 " ++ show (caYear cty) ++ " imports EITC from FTB 3514") $
                case caForm cty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("ca_ftb_3514", "L18", caYear cty)]
                    Left err -> expectationFailure $ show err

        it "CA Schedule CA 2024 imports QBI deduction from Form 8995" $
            case caScheduleCA_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_8995", "L16", 2024)]
                Left err -> expectationFailure $ show err

        it "CA Schedule CA 2025 imports QBI deduction from Form 8995" $
            case caScheduleCA_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_8995", "L16", 2025)]
                Left err -> expectationFailure $ show err

        it "CA FTB 3506 2024 imports CA AGI from CA 540" $
            case caFTB3506_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("ca_540", "L17", 2024)]
                Left err -> expectationFailure $ show err

        it "CA FTB 3506 2025 imports CA AGI from CA 540" $
            case caFTB3506_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("ca_540", "L17", 2025)]
                Left err -> expectationFailure $ show err

        it "CA FTB 3514 2024 imports CA AGI from CA 540" $
            case caFTB3514_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("ca_540", "L17", 2024)]
                Left err -> expectationFailure $ show err

        it "CA FTB 3514 2025 imports CA AGI from CA 540" $
            case caFTB3514_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("ca_540", "L17", 2025)]
                Left err -> expectationFailure $ show err

        it "US Schedule 2 2024 imports from Forms 8959 and 8960" $
            case usSchedule2_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_8959", "L18", 2024)]
                    cgImports graph `shouldContain` [("us_form_8960", "L17", 2024)]
                Left err -> expectationFailure $ show err

        it "US Schedule 2 2025 imports from Forms 8959 and 8960" $
            case usSchedule2_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_8959", "L18", 2025)]
                    cgImports graph `shouldContain` [("us_form_8960", "L17", 2025)]
                Left err -> expectationFailure $ show err

        it "US Schedule 1 2024 imports SE tax deduction from Schedule SE" $
            case usSchedule1_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_schedule_se", "L11", 2024)]
                Left err -> expectationFailure $ show err

        it "US Schedule 1 2025 imports SE tax deduction from Schedule SE" $
            case usSchedule1_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_schedule_se", "L11", 2025)]
                Left err -> expectationFailure $ show err

        it "US Schedule 2 2024 imports AMT from Form 6251 and SE tax from Schedule SE" $
            case usSchedule2_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_6251", "L11", 2024)]
                    cgImports graph `shouldContain` [("us_schedule_se", "L10", 2024)]
                Left err -> expectationFailure $ show err

        it "US Schedule 2 2025 imports AMT from Form 6251 and SE tax from Schedule SE" $
            case usSchedule2_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_6251", "L11", 2025)]
                    cgImports graph `shouldContain` [("us_schedule_se", "L10", 2025)]
                Left err -> expectationFailure $ show err

        it "US Schedule 3 2024 imports dependent care credit and education credits" $
            case usSchedule3_2024 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_2441", "L11", 2024)]
                    cgImports graph `shouldContain` [("us_form_8863", "L19", 2024)]
                Left err -> expectationFailure $ show err

        it "US Schedule 3 2025 imports dependent care credit and education credits" $
            case usSchedule3_2025 of
                Right frm -> do
                    let graph = compileForm frm
                    cgImports graph `shouldContain` [("us_form_2441", "L11", 2025)]
                    cgImports graph `shouldContain` [("us_form_8863", "L19", 2025)]
                Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports QBI deduction from Form 8995") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_form_8995", "L16", tyYear ty)]
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports CTC from Form 8812") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_form_8812", "L14", tyYear ty)]
                        cgImports graph `shouldContain` [("us_form_8812", "L27", tyYear ty)]
                    Left err -> expectationFailure $ show err

        forM_ taxYears $ \ty ->
            it ("US 1040 " ++ show (tyYear ty) ++ " imports AOTC from Form 8863") $
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                        cgImports graph `shouldContain` [("us_form_8863", "L9", tyYear ty)]
                    Left err -> expectationFailure $ show err

    describe "Standard deduction" $ do
        it "2024 Single is $14,600" $
            forStatus standardDeduction2024 Single `shouldBe` 14600

        it "2024 Married Filing Jointly is $29,200" $
            forStatus standardDeduction2024 MarriedJoint `shouldBe` 29200

        it "2024 Head of Household is $21,900" $
            forStatus standardDeduction2024 HeadOfHousehold `shouldBe` 21900

        it "2025 Single is $15,750" $
            forStatus standardDeduction2025 Single `shouldBe` 15750

        it "2025 Married Filing Jointly is $31,500" $
            forStatus standardDeduction2025 MarriedJoint `shouldBe` 31500

        it "2025 Head of Household is $23,625" $
            forStatus standardDeduction2025 HeadOfHousehold `shouldBe` 23625

    describe "California standard deduction" $ do
        it "2024 Single is $5,540" $
            forStatus caStandardDeduction2024 Single `shouldBe` 5540

        it "2024 Married Filing Jointly is $11,080" $
            forStatus caStandardDeduction2024 MarriedJoint `shouldBe` 11080

        it "2024 Head of Household is $11,080" $
            forStatus caStandardDeduction2024 HeadOfHousehold `shouldBe` 11080

        it "2025 Single is $5,706" $
            forStatus caStandardDeduction2025 Single `shouldBe` 5706

        it "2025 Married Filing Jointly is $11,412" $
            forStatus caStandardDeduction2025 MarriedJoint `shouldBe` 11412

        it "2025 Head of Household is $11,412" $
            forStatus caStandardDeduction2025 HeadOfHousehold `shouldBe` 11412

    describe "E2E Compiled Form Properties" $ do
        forM_ taxYears $ \ty -> do
            let yr = show (tyYear ty)

            it ("zero income = zero tax (" ++ yr ++ ")") $
                forAll (elements allFilingStatuses) $ \status ->
                    case tyForm ty of
                        Right frm ->
                            let graph = compileForm frm
                                tax = evalGraph graph status 0
                             in tax == 0
                        Left _ -> False

            it ("tax > 0 when wages exceed standard deduction (" ++ yr ++ ")") $
                property $
                    forAll (elements allFilingStatuses) $ \status ->
                        case tyForm ty of
                            Right frm ->
                                let graph = compileForm frm
                                    stdDed = forStatus (tyStdDed ty) status
                                    wages = unAmount stdDed + 10000
                                    tax = evalGraph graph status wages
                                 in tax > 0
                            Left _ -> False

            it ("tax is monotonically increasing with wages (" ++ yr ++ ")") $
                withMaxSuccess n $
                    property $
                        \(Positive base) (Positive extra) ->
                            forAll (elements allFilingStatuses) $ \status ->
                                case tyForm ty of
                                    Right frm ->
                                        let graph = compileForm frm
                                            tax1 = evalGraph graph status base
                                            tax2 = evalGraph graph status (base + extra)
                                         in tax2 >= tax1
                                    Left _ -> False

            it ("taxable income <= AGI (" ++ yr ++ ")") $
                withMaxSuccess n $
                    property $
                        \(Positive wages) ->
                            forAll (elements allFilingStatuses) $ \status ->
                                case tyForm ty of
                                    Right frm ->
                                        let graph = compileForm frm
                                            (agi, taxableInc, _) = evalGraphDetailed graph status wages
                                         in taxableInc <= agi
                                    Left _ -> False

            it ("tax <= taxable income (" ++ yr ++ ")") $
                withMaxSuccess n $
                    property $
                        \(Positive wages) ->
                            forAll (elements allFilingStatuses) $ \status ->
                                case tyForm ty of
                                    Right frm ->
                                        let graph = compileForm frm
                                            (_, taxableInc, tax) = evalGraphDetailed graph status wages
                                         in tax <= taxableInc
                                    Left _ -> False

            it ("effective rate <= 37% top marginal rate (" ++ yr ++ ")") $
                withMaxSuccess n $
                    property $
                        forAll (choose (1, 10_000_000)) $ \wages ->
                            forAll (elements allFilingStatuses) $ \status ->
                                case tyForm ty of
                                    Right frm ->
                                        let graph = compileForm frm
                                            tax = evalGraph graph status wages
                                            effectiveRate = tax / wages
                                         in effectiveRate <= 0.37
                                    Left _ -> False

            it ("all outputs non-negative (" ++ yr ++ ")") $
                withMaxSuccess n $
                    property $
                        \(Positive wages) ->
                            forAll (elements allFilingStatuses) $ \status ->
                                case tyForm ty of
                                    Right frm ->
                                        let graph = compileForm frm
                                            (agi, taxableInc, tax) = evalGraphDetailed graph status wages
                                         in agi >= 0 && taxableInc >= 0 && tax >= 0
                                    Left _ -> False

            it ("MFJ tax <= Single tax for same income (" ++ yr ++ ")") $
                withMaxSuccess (n `div` 2) $
                    property $
                        forAll (choose (50_000, 500_000)) $ \wages ->
                            case tyForm ty of
                                Right frm ->
                                    let graph = compileForm frm
                                        taxSingle = evalGraph graph Single wages
                                        taxMFJ = evalGraph graph MarriedJoint wages
                                     in taxMFJ <= taxSingle
                                Left _ -> False

            it ("tax at bracket boundaries is correct (" ++ yr ++ " Single)") $ do
                case tyForm ty of
                    Right frm -> do
                        let graph = compileForm frm
                            stdDed = unAmount (forStatus (tyStdDed ty) Single)
                            -- Test just above standard deduction (10% bracket)
                            tax1 = evalGraph graph Single (stdDed + 100)
                        -- Should be ~$10 (10% of $100)
                        tax1 `shouldSatisfy` (\t -> t >= 9 && t <= 11)
                    Left err -> expectationFailure $ show err

instance Arbitrary FilingStatus where
    arbitrary = elements allFilingStatuses

isImportOp :: Node -> Bool
isImportOp node = case nodeOp node of
    JSON.OpImport{} -> True
    _ -> False
