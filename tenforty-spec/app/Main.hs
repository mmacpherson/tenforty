module Main (main) where

import Control.Monad (forM_, unless)
import Data.Aeson.Encode.Pretty qualified as AP
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import ALForm40_2024
import ALForm40_2025
import ARAR1000F_2024
import ARAR1000F_2025
import AZForm140_2024
import AZForm140_2025
import CA540_2024
import CA540_2025
import CAFTB3506_2024
import CAFTB3506_2025
import CAFTB3514_2024
import CAFTB3514_2025
import CAScheduleCA_2024
import CAScheduleCA_2025
import COForm104_2024
import COForm104_2025
import CTForm1_2024
import CTForm1_2025
import DEFormPITRES_2024
import DEFormPITRES_2025
import GAForm500_2024
import GAForm500_2025
import HIHIN11_2024
import HIHIN11_2025
import IAIA1040_2024
import IAIA1040_2025
import IDFORM40_2024
import IDFORM40_2025
import IL1040_2024
import IL1040_2025
import INFormIT40_2024
import INFormIT40_2025
import KSFormK40_2024
import KSFormK40_2025
import KYForm740_2024
import KYForm740_2025
import LAIT540_2024
import LAIT540_2025
import MAForm1_2024
import MAForm1_2025
import MDForm502_2024
import MDForm502_2025
import MEME1040_2024
import MEME1040_2025
import MI1040_2024
import MI1040_2025
import MNFormM1_2024
import MNFormM1_2025
import MOForm1040_2024
import MOForm1040_2025
import MS80105_2024
import MS80105_2025
import NCFormD400_2024
import NCFormD400_2025
import NJ1040_2024
import NJ1040_2025
import NMPIT1_2024
import NMPIT1_2025
import NEForm1040N_2024
import NEForm1040N_2025
import NYIT201_2024
import NYIT201_2025
import OHIT1040_2024
import OHIT1040_2025
import OK511_2024
import OK511_2025
import ORForm40_2024
import ORForm40_2025
import PA40_2024
import PA40_2025
import SCForm1040_2024
import SCForm1040_2025
import TenForty
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
import UTTC40_2024
import UTTC40_2025
import VAForm760_2024
import VAForm760_2025
import WVIT140_2024
import WVIT140_2025
import WIForm1_2024
import WIForm1_2025

allForms :: [(FilePath, Either FormError Form)]
allForms =
    [ ("us_1040_2024.json", us1040_2024)
    , ("us_1040_2025.json", us1040_2025)
    , ("us_schedule_1_2024.json", usSchedule1_2024)
    , ("us_schedule_1_2025.json", usSchedule1_2025)
    , ("us_schedule_2_2024.json", usSchedule2_2024)
    , ("us_schedule_2_2025.json", usSchedule2_2025)
    , ("us_schedule_3_2024.json", usSchedule3_2024)
    , ("us_schedule_3_2025.json", usSchedule3_2025)
    , ("us_schedule_a_2024.json", usScheduleA_2024)
    , ("us_schedule_a_2025.json", usScheduleA_2025)
    , ("us_schedule_b_2024.json", usScheduleB_2024)
    , ("us_schedule_b_2025.json", usScheduleB_2025)
    , ("us_schedule_d_2024.json", usScheduleD_2024)
    , ("us_schedule_d_2025.json", usScheduleD_2025)
    , ("us_schedule_eic_2024.json", usScheduleEIC_2024)
    , ("us_schedule_eic_2025.json", usScheduleEIC_2025)
    , ("us_schedule_se_2024.json", usScheduleSE_2024)
    , ("us_schedule_se_2025.json", usScheduleSE_2025)
    , ("us_form_2441_2024.json", usForm2441_2024)
    , ("us_form_2441_2025.json", usForm2441_2025)
    , ("us_form_6251_2024.json", usForm6251_2024)
    , ("us_form_6251_2025.json", usForm6251_2025)
    , ("us_form_8812_2024.json", usForm8812_2024)
    , ("us_form_8812_2025.json", usForm8812_2025)
    , ("us_form_8863_2024.json", usForm8863_2024)
    , ("us_form_8863_2025.json", usForm8863_2025)
    , ("us_form_8959_2024.json", usForm8959_2024)
    , ("us_form_8959_2025.json", usForm8959_2025)
    , ("us_form_8960_2024.json", usForm8960_2024)
    , ("us_form_8960_2025.json", usForm8960_2025)
    , ("us_form_8995_2024.json", usForm8995_2024)
    , ("us_form_8995_2025.json", usForm8995_2025)
    , ("al_40_2024.json", alForm40_2024)
    , ("al_40_2025.json", alForm40_2025)
    , ("ar_ar1000f_2024.json", arAR1000F_2024)
    , ("ar_ar1000f_2025.json", arAR1000F_2025)
    , ("az_140_2024.json", azForm140_2024)
    , ("az_140_2025.json", azForm140_2025)
    , ("ca_540_2024.json", ca540_2024)
    , ("ca_540_2025.json", ca540_2025)
    , ("ca_schedule_ca_2024.json", caScheduleCA_2024)
    , ("ca_schedule_ca_2025.json", caScheduleCA_2025)
    , ("ca_ftb_3506_2024.json", caFTB3506_2024)
    , ("ca_ftb_3506_2025.json", caFTB3506_2025)
    , ("ca_ftb_3514_2024.json", caFTB3514_2024)
    , ("ca_ftb_3514_2025.json", caFTB3514_2025)
    , ("co_form104_2024.json", coForm104_2024)
    , ("co_form104_2025.json", coForm104_2025)
    , ("ct_1_2024.json", ctForm1_2024)
    , ("ct_1_2025.json", ctForm1_2025)
    , ("de_pit_res_2024.json", deFormPITRES_2024)
    , ("de_pit_res_2025.json", deFormPITRES_2025)
    , ("ga_500_2024.json", gaForm500_2024)
    , ("ga_500_2025.json", gaForm500_2025)
    , ("hi_n11_2024.json", hihiN11_2024)
    , ("hi_n11_2025.json", hihiN11_2025)
    , ("ia_ia1040_2024.json", iaIA1040_2024)
    , ("ia_ia1040_2025.json", iaIA1040_2025)
    , ("id_form40_2024.json", idForm40_2024)
    , ("id_form40_2025.json", idForm40_2025)
    , ("il_1040_2024.json", il1040_2024)
    , ("il_1040_2025.json", il1040_2025)
    , ("in_it40_2024.json", inFormIT40_2024)
    , ("in_it40_2025.json", inFormIT40_2025)
    , ("ks_k40_2024.json", ksFormK40_2024)
    , ("ks_k40_2025.json", ksFormK40_2025)
    , ("ky_740_2024.json", kyForm740_2024)
    , ("ky_740_2025.json", kyForm740_2025)
    , ("la_it540_2024.json", laIT540_2024)
    , ("la_it540_2025.json", laIT540_2025)
    , ("ma_1_2024.json", maForm1_2024)
    , ("ma_1_2025.json", maForm1_2025)
    , ("md_502_2024.json", mdForm502_2024)
    , ("md_502_2025.json", mdForm502_2025)
    , ("me_1040me_2024.json", meme1040_2024)
    , ("me_1040me_2025.json", meme1040_2025)
    , ("mi_1040_2024.json", mi1040_2024)
    , ("mi_1040_2025.json", mi1040_2025)
    , ("ms_80105_2024.json", ms80105_2024)
    , ("ms_80105_2025.json", ms80105_2025)
    , ("mn_m1_2024.json", mnFormM1_2024)
    , ("mn_m1_2025.json", mnFormM1_2025)
    , ("mo_1040_2024.json", moForm1040_2024)
    , ("mo_1040_2025.json", moForm1040_2025)
    , ("ny_it201_2024.json", nyIT201_2024)
    , ("ny_it201_2025.json", nyIT201_2025)
    , ("nc_d400_2024.json", ncFormD400_2024)
    , ("nc_d400_2025.json", ncFormD400_2025)
    , ("nj_1040_2024.json", nj1040_2024)
    , ("nj_1040_2025.json", nj1040_2025)
    , ("nm_pit1_2024.json", nmPIT1_2024)
    , ("nm_pit1_2025.json", nmPIT1_2025)
    , ("ne_1040n_2024.json", neForm1040N_2024)
    , ("ne_1040n_2025.json", neForm1040N_2025)
    , ("ok_511_2024.json", ok511_2024)
    , ("ok_511_2025.json", ok511_2025)
    , ("oh_it1040_2024.json", ohIT1040_2024)
    , ("oh_it1040_2025.json", ohIT1040_2025)
    , ("or_40_2024.json", orForm40_2024)
    , ("or_40_2025.json", orForm40_2025)
    , ("pa_40_2024.json", pa40_2024)
    , ("pa_40_2025.json", pa40_2025)
    , ("sc_1040_2024.json", scForm1040_2024)
    , ("sc_1040_2025.json", scForm1040_2025)
    , ("ut_tc40_2024.json", utTC40_2024)
    , ("ut_tc40_2025.json", utTC40_2025)
    , ("va_760_2024.json", vaForm760_2024)
    , ("va_760_2025.json", vaForm760_2025)
    , ("wv_it140_2024.json", wvIT140_2024)
    , ("wv_it140_2025.json", wvIT140_2025)
    , ("wi_form1_2024.json", wiForm1_2024)
    , ("wi_form1_2025.json", wiForm1_2025)
    ]

data Options = Options
    { optForm :: Text
    , optOutput :: Maybe FilePath
    , optPretty :: Bool
    }

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument
            ( metavar "FORM"
                <> help "Form to compile (us_1040_2024, us_1040_2025, us_schedule_1_2024, us_schedule_1_2025, us_schedule_2_2024, us_schedule_2_2025, us_schedule_3_2024, us_schedule_3_2025, us_schedule_a_2024, us_schedule_a_2025, us_schedule_d_2024, us_schedule_d_2025, us_schedule_eic_2024, us_schedule_eic_2025, al_40_2024, al_40_2025, ar_ar1000f_2024, ar_ar1000f_2025, az_140_2024, az_140_2025, ca_540_2024, ca_540_2025, co_form104_2024, co_form104_2025, ct_1_2024, ct_1_2025, de_pit_res_2024, de_pit_res_2025, ga_500_2024, ga_500_2025, hi_n11_2024, hi_n11_2025, ia_ia1040_2024, ia_ia1040_2025, id_form40_2024, id_form40_2025, il_1040_2024, il_1040_2025, in_it40_2024, in_it40_2025, ks_k40_2024, ks_k40_2025, ky_740_2024, ky_740_2025, la_it540_2024, la_it540_2025, ma_1_2024, ma_1_2025, md_502_2024, md_502_2025, me_1040me_2024, me_1040me_2025, mi_1040_2024, mi_1040_2025, mn_m1_2024, mn_m1_2025, mo_1040_2024, mo_1040_2025, ms_80105_2024, ms_80105_2025, nc_d400_2024, nc_d400_2025, nj_1040_2024, nj_1040_2025, nm_pit1_2024, nm_pit1_2025, ny_it201_2024, ny_it201_2025, oh_it1040_2024, oh_it1040_2025, ok_511_2024, ok_511_2025, or_40_2024, or_40_2025, pa_40_2024, pa_40_2025, sc_1040_2024, sc_1040_2025, ut_tc40_2024, ut_tc40_2025, va_760_2024, va_760_2025, wv_it140_2024, wv_it140_2025, wi_form1_2024, wi_form1_2025, all)"
            )
        <*> optional
            ( strOption
                ( long "output"
                    <> short 'o'
                    <> metavar "FILE"
                    <> help "Output file (stdout if not specified)"
                )
            )
        <*> switch
            ( long "pretty"
                <> short 'p'
                <> help "Pretty-print JSON output"
            )

optionsInfo :: ParserInfo Options
optionsInfo =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Compile tax forms to JSON computation graphs"
            <> header "tenforty-compile - Tax form DSL compiler"
        )

main :: IO ()
main = do
    opts <- execParser optionsInfo
    let formName = T.toLower (optForm opts)

    if formName == "all"
        then do
            -- 1. Check if any individual form failed its own validation
            let failures = [(fp, err) | (fp, Left err) <- allForms]
            unless (null failures) $ do
                hPutStrLn stderr "Individual form validation failed:"
                forM_ failures $ \(fp, err) ->
                    hPutStrLn stderr $ "  " ++ fp ++ ": " ++ show err
                exitFailure

            -- 2. Validate the set as a whole (cross-form imports)
            let forms = [f | (_, Right f) <- allForms]
                setErrors = validateFormSet forms

            unless (null setErrors) $ do
                hPutStrLn stderr "Cross-form import validation failed:"
                forM_ setErrors $ \err ->
                    hPutStrLn stderr $ "  " ++ show err
                exitFailure

            -- 3. Write output files
            forM_ allForms $ uncurry (compileToFile opts)
        else do
            let filename = T.unpack formName ++ ".json"
            case lookup filename allForms of
                Just res -> compileAndOutput opts res
                Nothing -> do
                    hPutStrLn stderr $ "Unknown form: " ++ T.unpack formName
                    hPutStrLn stderr "Available forms: us_1040, us_schedule_1, us_schedule_2, us_schedule_3, us_schedule_a, us_schedule_b, us_schedule_d, us_schedule_eic, us_schedule_se, us_form_2441, us_form_6251, us_form_8812, us_form_8863, us_form_8959, us_form_8960, us_form_8995, al_40, ar_ar1000f, az_140, ca_540, ca_schedule_ca, ca_ftb_3506, ca_ftb_3514, co_form104, ct_1, de_pit_res, ga_500, hi_n11, ia_ia1040, id_form40, il_1040, in_it40, ks_k40, ky_740, la_it540, ma_1, md_502, mi_1040, ms_80105, mn_m1, mo_1040, nc_d400, nj_1040, nm_pit1, ny_it201, oh_it1040, ok_511, or_40, pa_40, sc_1040, ut_tc40, va_760, wv_it140, wi_form1 (append _2024 or _2025), all"
                    exitFailure

compileAndOutput :: Options -> Either FormError Form -> IO ()
compileAndOutput opts formResult =
    case formResult of
        Left err -> do
            hPutStrLn stderr $ "Form validation error: " ++ show err
            exitFailure
        Right frm -> do
            let graph = compileForm frm
                json =
                    if optPretty opts
                        then AP.encodePretty graph
                        else compileFormToJSON frm
            case optOutput opts of
                Nothing -> BL.putStr json >> putStrLn ""
                Just fp -> BL.writeFile fp (json <> BL8.pack "\n")

compileToFile :: Options -> FilePath -> Either FormError Form -> IO ()
compileToFile opts fp formResult =
    case formResult of
        Left err -> do
            hPutStrLn stderr $ "Form validation error for " ++ fp ++ ": " ++ show err
            exitFailure
        Right frm -> do
            let outPath = "dist/" ++ fp
                graph = compileForm frm
                json =
                    if optPretty opts
                        then AP.encodePretty graph
                        else compileFormToJSON frm
            createDirectoryIfMissing True "dist"
            BL.writeFile outPath (json <> BL8.pack "\n")
            putStrLn $ "Compiled: " ++ outPath
