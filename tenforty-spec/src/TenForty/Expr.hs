{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module TenForty.Expr
  ( -- * Expression GADT
    Expr(..)

    -- * Smart Constructors
  , lit
  , dollars
  , rate
  , line
  , importLine
  , (.+.)
  , (.-.)
  , (.*.)
  , (./.)
  , neg
  , maxE
  , minE
  , ifPos
  , ifNeg
  , ifGte
  , floorE
  , roundE

    -- * Combinators
  , sumOf

    -- * Analysis
  , extractLineRefs
  , extractImports
  , extractTableRefs
  , mapExprs
  ) where

import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set

import TenForty.Types


type Expr :: Type -> Type
data Expr u where
  Lit       :: Amount u -> Expr u
  Line      :: LineId -> Expr Dollars
  Import    :: FormId -> LineId -> Expr Dollars

  Add       :: Expr u -> Expr u -> Expr u
  Sub       :: Expr u -> Expr u -> Expr u
  Mul       :: Expr Dollars -> Expr Rate -> Expr Dollars
  Div       :: Expr Dollars -> Expr Dollars -> Expr Rate
  Neg       :: Expr u -> Expr u

  BracketTax :: TableId -> Expr Dollars -> Expr Dollars
  TableLookup :: TableId -> Expr Dollars -> Expr Dollars
  PhaseOut  :: Expr Dollars  -- base amount
            -> Expr Dollars  -- threshold
            -> Expr Rate     -- reduction rate
            -> Expr Dollars  -- AGI
            -> Expr Dollars

  ByStatusE :: ByStatus (Expr u) -> Expr u

  Max       :: Expr u -> Expr u -> Expr u
  Min       :: Expr u -> Expr u -> Expr u
  IfPos     :: Expr Dollars -> Expr u -> Expr u -> Expr u
  IfNeg     :: Expr Dollars -> Expr u -> Expr u -> Expr u
  IfGte     :: Expr Dollars -> Expr Dollars -> Expr u -> Expr u -> Expr u

  Floor     :: Expr Dollars -> Expr Dollars
  Round     :: Expr Dollars -> Expr Dollars

deriving instance Show (Expr u)


lit :: Amount u -> Expr u
lit = Lit

dollars :: Double -> Expr Dollars
dollars = Lit . Amount

rate :: Double -> Expr Rate
rate = Lit . Amount

line :: LineId -> Expr Dollars
line = Line

importLine :: FormId -> LineId -> Expr Dollars
importLine = Import

(.+.) :: Expr u -> Expr u -> Expr u
(.+.) = Add
infixl 6 .+.

(.-.) :: Expr u -> Expr u -> Expr u
(.-.) = Sub
infixl 6 .-.

(.*.) :: Expr Dollars -> Expr Rate -> Expr Dollars
(.*.) = Mul
infixl 7 .*.

(./.) :: Expr Dollars -> Expr Dollars -> Expr Rate
(./.) = Div
infixl 7 ./.

neg :: Expr u -> Expr u
neg = Neg

maxE :: Expr u -> Expr u -> Expr u
maxE = Max

minE :: Expr u -> Expr u -> Expr u
minE = Min

ifPos :: Expr Dollars -> Expr u -> Expr u -> Expr u
ifPos = IfPos

ifNeg :: Expr Dollars -> Expr u -> Expr u -> Expr u
ifNeg = IfNeg

ifGte :: Expr Dollars -> Expr Dollars -> Expr u -> Expr u -> Expr u
ifGte = IfGte

floorE :: Expr Dollars -> Expr Dollars
floorE = Floor

roundE :: Expr Dollars -> Expr Dollars
roundE = Round

sumOf :: [Expr u] -> Expr u
sumOf [] = error "sumOf: empty list"
sumOf xs = foldr1 Add xs


extractLineRefs :: Expr u -> Set LineId
extractLineRefs = \case
  Lit _           -> Set.empty
  Line lid        -> Set.singleton lid
  Import _ _      -> Set.empty
  Add a b         -> extractLineRefs a <> extractLineRefs b
  Sub a b         -> extractLineRefs a <> extractLineRefs b
  Mul a b         -> extractLineRefs a <> extractLineRefs b
  Div a b         -> extractLineRefs a <> extractLineRefs b
  Neg a           -> extractLineRefs a
  BracketTax _ e  -> extractLineRefs e
  TableLookup _ e -> extractLineRefs e
  PhaseOut b t r a -> extractLineRefs b <> extractLineRefs t
                   <> extractLineRefs r <> extractLineRefs a
  ByStatusE bs    -> foldMap extractLineRefs bs
  Max a b         -> extractLineRefs a <> extractLineRefs b
  Min a b         -> extractLineRefs a <> extractLineRefs b
  IfPos c t e     -> extractLineRefs c <> extractLineRefs t <> extractLineRefs e
  IfNeg c t e     -> extractLineRefs c <> extractLineRefs t <> extractLineRefs e
  IfGte a b t e   -> extractLineRefs a <> extractLineRefs b
                  <> extractLineRefs t <> extractLineRefs e
  Floor e         -> extractLineRefs e
  Round e         -> extractLineRefs e

extractImports :: Expr u -> Set (FormId, LineId)
extractImports = \case
  Lit _           -> Set.empty
  Line _          -> Set.empty
  Import fid lid  -> Set.singleton (fid, lid)
  Add a b         -> extractImports a <> extractImports b
  Sub a b         -> extractImports a <> extractImports b
  Mul a b         -> extractImports a <> extractImports b
  Div a b         -> extractImports a <> extractImports b
  Neg a           -> extractImports a
  BracketTax _ e  -> extractImports e
  TableLookup _ e -> extractImports e
  PhaseOut b t r a -> extractImports b <> extractImports t
                   <> extractImports r <> extractImports a
  ByStatusE bs    -> foldMap extractImports bs
  Max a b         -> extractImports a <> extractImports b
  Min a b         -> extractImports a <> extractImports b
  IfPos c t e     -> extractImports c <> extractImports t <> extractImports e
  IfNeg c t e     -> extractImports c <> extractImports t <> extractImports e
  IfGte a b t e   -> extractImports a <> extractImports b
                  <> extractImports t <> extractImports e
  Floor e         -> extractImports e
  Round e         -> extractImports e

extractTableRefs :: Expr u -> Set TableId
extractTableRefs = \case
  Lit _              -> Set.empty
  Line _             -> Set.empty
  Import _ _         -> Set.empty
  Add a b            -> extractTableRefs a <> extractTableRefs b
  Sub a b            -> extractTableRefs a <> extractTableRefs b
  Mul a b            -> extractTableRefs a <> extractTableRefs b
  Div a b            -> extractTableRefs a <> extractTableRefs b
  Neg a              -> extractTableRefs a
  BracketTax tid e   -> Set.singleton tid <> extractTableRefs e
  TableLookup tid e  -> Set.singleton tid <> extractTableRefs e
  PhaseOut b t r a   -> extractTableRefs b <> extractTableRefs t
                     <> extractTableRefs r <> extractTableRefs a
  ByStatusE bs       -> foldMap extractTableRefs bs
  Max a b            -> extractTableRefs a <> extractTableRefs b
  Min a b            -> extractTableRefs a <> extractTableRefs b
  IfPos c t e        -> extractTableRefs c <> extractTableRefs t <> extractTableRefs e
  IfNeg c t e        -> extractTableRefs c <> extractTableRefs t <> extractTableRefs e
  IfGte a b t e      -> extractTableRefs a <> extractTableRefs b
                     <> extractTableRefs t <> extractTableRefs e
  Floor e            -> extractTableRefs e
  Round e            -> extractTableRefs e

mapExprs :: (forall v. Expr v -> Expr v) -> Expr u -> Expr u
mapExprs f = go
  where
    go :: forall x. Expr x -> Expr x
    go expr = f $ case expr of
      Lit a           -> Lit a
      Line lid        -> Line lid
      Import fid lid  -> Import fid lid
      Add a b         -> Add (go a) (go b)
      Sub a b         -> Sub (go a) (go b)
      Mul a b         -> Mul (go a) (go b)
      Div a b         -> Div (go a) (go b)
      Neg a           -> Neg (go a)
      BracketTax t e  -> BracketTax t (go e)
      TableLookup t e -> TableLookup t (go e)
      PhaseOut b t r a -> PhaseOut (go b) (go t) (go r) (go a)
      ByStatusE bs    -> ByStatusE (fmap go bs)
      Max a b         -> Max (go a) (go b)
      Min a b         -> Min (go a) (go b)
      IfPos c t e     -> IfPos (go c) (go t) (go e)
      IfNeg c t e     -> IfNeg (go c) (go t) (go e)
      IfGte a b t e   -> IfGte (go a) (go b) (go t) (go e)
      Floor e         -> Floor (go e)
      Round e         -> Round (go e)
