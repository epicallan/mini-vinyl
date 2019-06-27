module Data.Vinyl.Core where

import Control.Applicative (Const (..))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Vinyl.TypeLevel (RecAll)

-- | record is parameterized by a list of fields.
data Rec :: (u -> Type) -> [u] -> Type where
  RNil :: Rec f '[]
  (:&) :: f r -> Rec f rs -> Rec f (r ': rs)

infixr 7 :&

-- | a type to wrap a value with a capability
data Dict (c :: Type -> Constraint) (a :: Type) :: Type where
  Dict :: c a => a -> Dict c a

type f :. g = Compose f g
infixr 9 :.

-- | a record with uniform fields may be turned into a list

recordToList :: Rec (Const a) rs -> [a]
recordToList  = \case
  RNil -> []
  (x :& xs) -> getConst x : recordToList xs

rmap :: (forall x. f x -> g x) -> Rec f rs -> Rec g rs
rmap f = \case
  RNil -> RNil
  (x :& xs) -> f x :& ( f `rmap` xs)

-- | some times we may know a constraint to be true for each field in a record
-- The constraint solver is not smart enough to realize this and thus we make
-- it obvious by reifying the constraint with a proof
reifyConstraint
  :: RecAll f rs c
  => Proxy c
  -> Rec f rs
  -> Rec (Dict c :. f) rs
reifyConstraint prx rec = case rec of
  RNil      -> RNil
  (x :& xs) -> Compose (Dict x) :& reifyConstraint prx xs

instance RecAll f rs Show => Show (Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
    . intercalate ","
    . recordToList
    -- RNil has no show instance and rmap  doesn't compute @f over @RNil which would be an error
    -- This is important since RecAll by definition doesn't constrain RNil to have a constraint capability
    . rmap (\(Compose (Dict x)) -> Const (show x))
    $ reifyConstraint (Proxy @Show) xs

-- TODO: applicative, monad and Monoid instances

