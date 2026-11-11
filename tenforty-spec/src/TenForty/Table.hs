module TenForty.Table
  ( -- * Tax Brackets
    Bracket (..),
    bracketThreshold,
    bracketRate,
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
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import TenForty.Types

-- | One bracket of a rate schedule. 'Bracket' has a finite upper threshold
-- per filing status; 'TopBracket' is the explicitly unbounded last bracket
-- (tenforty-yxun.3), replacing a large sentinel threshold.
data Bracket
  = Bracket (ByStatus (Amount Dollars)) (Amount Rate)
  | TopBracket (Amount Rate)
  deriving stock (Show, Eq)

-- | The bracket's upper threshold per status; 'Nothing' for the top bracket.
bracketThreshold :: Bracket -> Maybe (ByStatus (Amount Dollars))
bracketThreshold = \case
  Bracket threshold _ -> Just threshold
  TopBracket _ -> Nothing

bracketRate :: Bracket -> Amount Rate
bracketRate = \case
  Bracket _ r -> r
  TopBracket r -> r

newtype BracketTable = BracketTable {unBracketTable :: NonEmpty Bracket}
  deriving stock (Show, Eq)

-- | Validate a schedule: finite brackets ascend for every status and exactly
-- the last bracket is the unbounded 'TopBracket'.
mkBracketTable :: NonEmpty Bracket -> Either String BracketTable
mkBracketTable brackets =
  let finite = [threshold | Bracket threshold _ <- NE.toList brackets]
      tops = [() | TopBracket _ <- NE.toList brackets]
      sorted = NE.sortBy (comparing upperKey) brackets
      upperKey b = maybe (1 :: Int, Nothing) (\t -> (0, Just (bsSingle t))) (bracketThreshold b)
   in case (tops, NE.last brackets) of
        ([()], TopBracket _)
          | validateMonotonic finite -> Right (BracketTable sorted)
          | otherwise -> Left "Bracket thresholds must be monotonically increasing for all filing statuses"
        _ -> Left "A bracket table needs exactly one TopBracket, and it must be last"

validateMonotonic :: [ByStatus (Amount Dollars)] -> Bool
validateMonotonic thresholds = all checkStatus allFilingStatuses
  where
    checkStatus status = isAscending (map (`forStatus` status) thresholds)
    isAscending xs = and $ zipWith (<=) xs (drop 1 xs)

bracketTableList :: BracketTable -> [Bracket]
bracketTableList = NE.toList . unBracketTable

data LookupEntry = LookupEntry
  { entryLowerBound :: Amount Dollars,
    entryUpperBound :: Amount Dollars,
    entryValue :: ByStatus (Amount Dollars)
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
      let upper = (`forStatus` status) <$> bracketThreshold b
          rate' = bracketRate b
          taxableInBracket = maybe income (min income) upper - prevThreshold
       in if income <= prevThreshold
            then acc
            else case upper of
              Nothing -> acc + max 0 taxableInBracket * coerceAmount rate'
              Just threshold ->
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
    go _prevThreshold (b : bs) =
      case (`forStatus` status) <$> bracketThreshold b of
        Nothing -> bracketRate b
        Just threshold
          | income <= threshold -> bracketRate b
          | otherwise -> go threshold bs
