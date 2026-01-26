{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TenForty.Types (
    -- * Filing Status
    FilingStatus (..),
    allFilingStatuses,

    -- * Type-Safe Units (Phantom Types)
    Amount (..),
    Dollars,
    Rate,
    Count,

    -- * Status-Indexed Values
    ByStatus (..),
    byStatus,
    forStatus,
    mapByStatus,

    -- * Identifiers
    LineId (..),
    FormId (..),
    TableId (..),
    NodeId (..),
) where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T

data FilingStatus
    = Single
    | MarriedJoint
    | MarriedSeparate
    | HeadOfHousehold
    | QualifyingWidow
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

allFilingStatuses :: [FilingStatus]
allFilingStatuses = [minBound .. maxBound]

data Dollars
data Rate
data Count

newtype Amount (u :: *) = Amount {unAmount :: Double}
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord)

instance Num (Amount u) where
    Amount a + Amount b = Amount (a + b)
    Amount a - Amount b = Amount (a - b)
    Amount a * Amount b = Amount (a * b)
    abs (Amount a) = Amount (abs a)
    signum (Amount a) = Amount (signum a)
    fromInteger = Amount . fromInteger
    negate (Amount a) = Amount (negate a)

instance Fractional (Amount u) where
    Amount a / Amount b = Amount (a / b)
    fromRational = Amount . fromRational

data ByStatus a = ByStatus
    { bsSingle :: !a
    , bsMarriedJoint :: !a
    , bsMarriedSeparate :: !a
    , bsHeadOfHousehold :: !a
    , bsQualifyingWidow :: !a
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

byStatus :: a -> a -> a -> a -> a -> ByStatus a
byStatus s mj ms hh qw =
    ByStatus
        { bsSingle = s
        , bsMarriedJoint = mj
        , bsMarriedSeparate = ms
        , bsHeadOfHousehold = hh
        , bsQualifyingWidow = qw
        }

forStatus :: ByStatus a -> FilingStatus -> a
forStatus bs = \case
    Single -> bsSingle bs
    MarriedJoint -> bsMarriedJoint bs
    MarriedSeparate -> bsMarriedSeparate bs
    HeadOfHousehold -> bsHeadOfHousehold bs
    QualifyingWidow -> bsQualifyingWidow bs

mapByStatus :: (a -> b) -> ByStatus a -> ByStatus b
mapByStatus = fmap

newtype LineId = LineId {unLineId :: Text}
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord)

instance IsString LineId where
    fromString = LineId . T.pack

newtype FormId = FormId {unFormId :: Text}
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord)

instance IsString FormId where
    fromString = FormId . T.pack

newtype TableId = TableId {unTableId :: Text}
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord)

instance IsString TableId where
    fromString = TableId . T.pack

newtype NodeId = NodeId {unNodeId :: Int}
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Num, Enum)
