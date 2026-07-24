{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TenForty.Types
  ( -- * Filing Status
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

    -- * Typed Cross-Form Output References
    LineRef (..),
  )
where

import Data.Kind (Type)
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

newtype Amount (u :: Type) = Amount {unAmount :: Double}
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
  { bsSingle :: !a,
    bsMarriedJoint :: !a,
    bsMarriedSeparate :: !a,
    bsHeadOfHousehold :: !a,
    bsQualifyingWidow :: !a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

byStatus :: a -> a -> a -> a -> a -> ByStatus a
byStatus s mj ms hh qw =
  ByStatus
    { bsSingle = s,
      bsMarriedJoint = mj,
      bsMarriedSeparate = ms,
      bsHeadOfHousehold = hh,
      bsQualifyingWidow = qw
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

-- | A typed, cross-form reference to a form's output line: it carries the
-- target form, the target line, and the line's unit as a phantom.
--
-- Handles are minted by 'TenForty.DSL.lineRef' from a @(form, line)@ pair, with
-- the unit fixed at the declaration site, and all live in one dependency-free
-- module (@FormRefs@). This consolidates every cross-form reference to a single
-- named handle: importers write @importForm us1040L11@ rather than the string
-- pair, so a mistyped or since-renamed reference is a compile-time \"not in
-- scope\" error at the reference site, and a unit mismatch is a type error.
--
-- The @(form, line)@ strings still live inside each handle, so a handle whose
-- strings name no real line — or name a line that is not a declared output — is
-- not a type error; it is caught at graph-resolution time by
-- 'TenForty.Compile.JSON.unresolvedImports', which fails the build.
data LineRef (u :: Type) = LineRef FormId LineId
  deriving stock (Show)
