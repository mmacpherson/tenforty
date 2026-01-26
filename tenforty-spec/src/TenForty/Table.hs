module TenForty.Table (
    -- * Tax Brackets
    Bracket (..),
    BracketTable (..),
    mkBracketTable,
    bracketTableList,

    -- * Lookup Tables
    LookupEntry (..),
    LookupTable (..),
    mkLookupTable,
    lookupTableList,

    -- * Combined Table Type
    Table (..),
    tableId,

    -- * Evaluation (for testing)
    evalBracketTax,
    evalLookup,
    marginalRate,
) where

import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)

import TenForty.Types

data Bracket = Bracket
    { bracketThreshold :: ByStatus (Amount Dollars)
    , bracketRate :: Amount Rate
    }
    deriving stock (Show, Eq)

newtype BracketTable = BracketTable {unBracketTable :: NonEmpty Bracket}
    deriving stock (Show, Eq)

mkBracketTable :: NonEmpty Bracket -> Either String BracketTable
mkBracketTable brackets =
    let sorted = NE.sortBy (comparing (bsSingle . bracketThreshold)) brackets
     in if validateMonotonic (NE.toList sorted)
            then Right (BracketTable sorted)
            else Left "Bracket thresholds must be monotonically increasing for all filing statuses"

validateMonotonic :: [Bracket] -> Bool
validateMonotonic bs = all checkStatus allFilingStatuses
  where
    checkStatus status =
        let thresholds = map (forStatus . bracketThreshold) bs
            values = map ($ status) thresholds
         in isAscending values
    isAscending xs = and $ zipWith (<=) xs (drop 1 xs)

bracketTableList :: BracketTable -> [Bracket]
bracketTableList = NE.toList . unBracketTable

data LookupEntry = LookupEntry
    { entryLowerBound :: Amount Dollars
    , entryUpperBound :: Amount Dollars
    , entryValue :: ByStatus (Amount Dollars)
    }
    deriving stock (Show, Eq)

newtype LookupTable = LookupTable {unLookupTable :: NonEmpty LookupEntry}
    deriving stock (Show, Eq)

mkLookupTable :: NonEmpty LookupEntry -> Either String LookupTable
mkLookupTable entries =
    let sorted = NE.sortBy (comparing entryLowerBound) entries
     in if validateLookupRanges (NE.toList sorted)
            then Right (LookupTable sorted)
            else Left "Lookup table ranges must be non-overlapping and cover continuous ranges"

validateLookupRanges :: [LookupEntry] -> Bool
validateLookupRanges [] = True
validateLookupRanges [_] = True
validateLookupRanges (e1 : e2 : rest) =
    entryUpperBound e1 <= entryLowerBound e2 && validateLookupRanges (e2 : rest)

lookupTableList :: LookupTable -> [LookupEntry]
lookupTableList = NE.toList . unLookupTable

data Table
    = TableBracket TableId BracketTable
    | TableLookup TableId LookupTable
    deriving stock (Show, Eq)

tableId :: Table -> TableId
tableId = \case
    TableBracket tid _ -> tid
    TableLookup tid _ -> tid

evalBracketTax :: BracketTable -> FilingStatus -> Amount Dollars -> Amount Dollars
evalBracketTax (BracketTable brackets) status income =
    go 0 0 (NE.toList brackets)
  where
    go :: Amount Dollars -> Amount Dollars -> [Bracket] -> Amount Dollars
    go acc _ [] = acc
    go acc prevThreshold (b : bs) =
        let threshold = forStatus (bracketThreshold b) status
            rate' = bracketRate b
            taxableInBracket = min income threshold - prevThreshold
         in if income <= prevThreshold
                then acc
                else
                    go
                        (acc + max 0 taxableInBracket * coerceAmount rate')
                        threshold
                        bs

    coerceAmount :: Amount Rate -> Amount Dollars
    coerceAmount (Amount x) = Amount x

evalLookup :: LookupTable -> FilingStatus -> Amount Dollars -> Amount Dollars
evalLookup (LookupTable entries) status amount =
    case filter inRange (NE.toList entries) of
        (e : _) -> forStatus (entryValue e) status
        [] -> 0
  where
    inRange e = entryLowerBound e <= amount && amount < entryUpperBound e

marginalRate :: BracketTable -> FilingStatus -> Amount Dollars -> Amount Rate
marginalRate (BracketTable brackets) status income =
    go 0 (NE.toList brackets)
  where
    go :: Amount Dollars -> [Bracket] -> Amount Rate
    go _ [] = 0
    go prevThreshold (b : bs) =
        let threshold = forStatus (bracketThreshold b) status
         in if income <= threshold
                then bracketRate b
                else go threshold bs
