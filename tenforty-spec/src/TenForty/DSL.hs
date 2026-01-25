module TenForty.DSL
  ( -- * Form Construction
    form
  , defineTable
  , outputs

    -- * Line Definition (Full API)
  , input
  , compute
  , worksheet

    -- * Line Definition (Convenience)
    -- | Shorthand for common patterns
  , keyInput      -- ^ Important user input
  , keyOutput     -- ^ Important computed result
  , interior      -- ^ Intermediate calculation (carrier node)

    -- * Expression Operators
  , (.+.)
  , (.-.)
  , (.*.)
  , (./.)
  , neg
  , maxE
  , minE
  , max0
  , ifPos
  , ifNeg
  , ifGte
  , floorE
  , roundE

    -- * Tax-Expert-Friendly Helpers
  , sumOf
  , subtractNotBelowZero
  , excessOf
  , smallerOf
  , greaterOf
  , percent

    -- * Literals
  , dollars
  , rate
  , lit

    -- * References
  , line
  , importLine
  , importForm

    -- * Tax Operations
  , bracketTax
  , tableLookup
  , byStatusE

    -- * Re-exports
  , module TenForty.Types
  , module TenForty.Form
  , module TenForty.Table
  , module TenForty.PhaseOut
  , Expr
  ) where

import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import TenForty.Types
import TenForty.Expr
import TenForty.Form hiding (input, compute, worksheet)
import qualified TenForty.Form as F
import TenForty.Table hiding (TableLookup)
import TenForty.PhaseOut


form :: FormId -> Int -> FormBuilder () -> Either FormError Form
form = buildForm

defineTable :: Table -> FormBuilder ()
defineTable tbl = modify' $ \s -> s
  { bsTables = Map.insert (tableId tbl) tbl (bsTables s)
  }

outputs :: [LineId] -> FormBuilder ()
outputs lids = modify' $ \s -> s
  { bsOutputs = Set.union (Set.fromList lids) (bsOutputs s)
  }


-- | Full API: define an input with all metadata
input :: LineId -> Text -> Text -> LineImportance -> FormBuilder (Expr Dollars)
input = F.input

-- | Full API: define a computed line with all metadata
compute :: LineId -> Text -> Text -> LineImportance -> Expr Dollars -> FormBuilder (Expr Dollars)
compute = F.compute

-- | Full API: define a worksheet with all metadata
worksheet :: LineId -> Text -> Text -> LineImportance -> [(LineId, Text, Expr Dollars)] -> FormBuilder (Expr Dollars)
worksheet = F.worksheet


-- | Define an important user-provided input
--
-- Example:
-- > wages <- keyInput "L1a" "wages" "Total wages, salaries, tips from W-2 box 1"
keyInput :: LineId -> Text -> Text -> FormBuilder (Expr Dollars)
keyInput lid name desc = F.input lid name desc KeyInput

-- | Define an important computed output (AGI, tax liability, refund, etc.)
--
-- Example:
-- > agi <- keyOutput "L11" "agi" "Adjusted gross income" $
-- >          totalIncome .-. adjustments
keyOutput :: LineId -> Text -> Text -> Expr Dollars -> FormBuilder (Expr Dollars)
keyOutput lid name desc = F.compute lid name desc KeyOutput

-- | Define an intermediate calculation (carrier node)
-- These are internal steps that don't need detailed descriptions.
--
-- Example:
-- > subtotal <- interior "L9" "total_income" $
-- >               wages .+. interest .+. dividends
interior :: LineId -> Text -> Expr Dollars -> FormBuilder (Expr Dollars)
interior lid name = F.compute lid name "" Interior


max0 :: Expr Dollars -> Expr Dollars
max0 = maxE (dollars 0)

importForm :: FormId -> LineId -> Expr Dollars
importForm = Import

bracketTax :: TableId -> Expr Dollars -> Expr Dollars
bracketTax = BracketTax

tableLookup :: TableId -> Expr Dollars -> Expr Dollars
tableLookup = TableLookup

byStatusE :: ByStatus (Expr u) -> Expr u
byStatusE = ByStatusE


-- | "Subtract B from A. If zero or less, enter -0-"
subtractNotBelowZero :: Expr Dollars -> Expr Dollars -> Expr Dollars
subtractNotBelowZero a b = max0 (a .-. b)

-- | "If A is more than B, subtract B from A. Otherwise, enter -0-"
excessOf :: Expr Dollars -> Expr Dollars -> Expr Dollars
excessOf a b = ifPos (a .-. b) (a .-. b) (dollars 0)

-- | "Enter the smaller of A or B"
smallerOf :: Expr u -> Expr u -> Expr u
smallerOf = minE

-- | "Enter the greater of A or B"
greaterOf :: Expr u -> Expr u -> Expr u
greaterOf = maxE

-- | "Multiply by X%" - e.g., percent 22 for 22%
percent :: Double -> Expr Rate
percent p = rate (p / 100)
