module Main (main) where

import AllForms (allForms)
import Control.Monad (forM_, unless)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as AP
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Either (rights)
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import TenForty

data Options = Options
  { optForm :: Text,
    optOutput :: Maybe FilePath,
    optPretty :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument
      ( metavar "FORM"
          <> help "Form to compile (us_1040_2024, us_1040_2025, us_schedule_1_2024, us_schedule_1_2025, us_schedule_2_2024, us_schedule_2_2025, us_schedule_3_2024, us_schedule_3_2025, us_schedule_a_2024, us_schedule_a_2025, us_schedule_d_2024, us_schedule_d_2025, us_schedule_eic_2024, us_schedule_eic_2025, ak_notax_2024, ak_notax_2025, al_40_2024, al_40_2025, ar_ar1000f_2024, ar_ar1000f_2025, az_140_2024, az_140_2025, ca_540_2024, ca_540_2025, co_form104_2024, co_form104_2025, ct_1_2024, ct_1_2025, dc_d40_2024, dc_d40_2025, de_pit_res_2024, de_pit_res_2025, fl_notax_2024, fl_notax_2025, ga_500_2024, ga_500_2025, hi_n11_2024, hi_n11_2025, ia_ia1040_2024, ia_ia1040_2025, id_form40_2024, id_form40_2025, il_1040_2024, il_1040_2025, in_it40_2024, in_it40_2025, ks_k40_2024, ks_k40_2025, ky_740_2024, ky_740_2025, la_it540_2024, la_it540_2025, ma_1_2024, ma_1_2025, md_502_2024, md_502_2025, me_1040me_2024, me_1040me_2025, mi_1040_2024, mi_1040_2025, mn_m1_2024, mn_m1_2025, mo_1040_2024, mo_1040_2025, mt_form2_2024, mt_form2_2025, ms_80105_2024, ms_80105_2025, nc_d400_2024, nc_d400_2025, nd_1_2024, nd_1_2025, ne_1040n_2024, ne_1040n_2025, nh_dp10_2024, nh_dp10_2025, nj_1040_2024, nj_1040_2025, nm_pit1_2024, nm_pit1_2025, nv_notax_2024, nv_notax_2025, ny_it201_2024, ny_it201_2025, oh_it1040_2024, oh_it1040_2025, ok_511_2024, ok_511_2025, or_40_2024, or_40_2025, pa_40_2024, pa_40_2025, ri_1040_2024, ri_1040_2025, sc_1040_2024, sc_1040_2025, sd_notax_2024, sd_notax_2025, tn_notax_2024, tn_notax_2025, tx_notax_2024, tx_notax_2025, ut_tc40_2024, ut_tc40_2025, va_760_2024, va_760_2025, vt_in111_2024, vt_in111_2025, wa_notax_2024, wa_notax_2025, wv_it140_2024, wv_it140_2025, wi_form1_2024, wi_form1_2025, wy_notax_2024, wy_notax_2025, all)"
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
    else
      if "resolve_" `T.isPrefixOf` formName
        then case T.splitOn "_" (T.drop 8 formName) of
          (yr : states) -> do
            -- resolve_<year>          -> ALL forms for the year (one graph per year)
            -- resolve_<year>_<state>  -> federal + those states (focused subset)
            let suffix = "_" <> yr <> ".json"
                forYear = [(T.pack fp, res) | (fp, res) <- allForms, suffix `T.isSuffixOf` T.pack fp]
                wanted =
                  if null states
                    then map snd forYear
                    else [res | (fpt, res) <- forYear, any (\p -> (p <> "_") `T.isPrefixOf` fpt) ("us" : states)]
                forms = rights wanted
                cgs = map compileForm forms
                badImports = unresolvedImports cgs
                name =
                  if null states
                    then "us_tax_graph_" ++ T.unpack yr ++ ".json"
                    else T.unpack formName ++ ".json"
                outPath = "dist/" ++ name
            unless (null badImports) $ do
              hPutStrLn stderr "resolve: unresolved cross-form imports (fix the importForm reference):"
              forM_ badImports $ \(src, tf, tl) ->
                hPutStrLn stderr $ "  " ++ T.unpack src ++ " imports " ++ T.unpack tf ++ ":" ++ T.unpack tl
              exitFailure
            createDirectoryIfMissing True "dist"
            -- compact: this is a machine-loaded artifact, not read by humans
            BL.writeFile outPath (Aeson.encode (resolveForms cgs) <> BL8.pack "\n")
            putStrLn $
              "Wrote " ++ outPath ++ " (" ++ show (length forms) ++ " forms -> one resolved graph)"
          [] -> hPutStrLn stderr "resolve: missing year" >> exitFailure
        else do
          let filename = T.unpack formName ++ ".json"
          case lookup filename allForms of
            Just res -> compileAndOutput opts res
            Nothing -> do
              hPutStrLn stderr $ "Unknown form: " ++ T.unpack formName
              hPutStrLn stderr "Available forms: us_1040, us_schedule_1, us_schedule_1a (2025 only), us_schedule_2, us_schedule_3, us_schedule_a, us_schedule_b, us_schedule_d, us_schedule_eic, us_schedule_se, us_form_2441, us_form_6251, us_form_8812, us_form_8863, us_form_8959, us_form_8960, us_form_8995, ak_notax, al_40, ar_ar1000f, az_140, ca_540, ca_schedule_ca, ca_ftb_3506, ca_ftb_3514, co_form104, ct_1, dc_d40, de_pit_res, fl_notax, ga_500, hi_n11, ia_ia1040, id_form40, il_1040, in_it40, ks_k40, ky_740, la_it540, ma_1, md_502, me_1040me, mi_1040, ms_80105, mn_m1, mo_1040, mt_form2, nc_d400, nd_1, ne_1040n, nh_dp10, nj_1040, nm_pit1, nv_notax, ny_it201, oh_it1040, ok_511, or_40, pa_40, ri_1040, sc_1040, sd_notax, tn_notax, tx_notax, ut_tc40, va_760, vt_in111, wa_notax, wv_it140, wi_form1, wy_notax (append _2024 or _2025), all"
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
