module Data.Vinyl.TypeLevel where

import Data.Kind

-- | A constraint-former which applies to every field in a record.
-- say you want to imply that the Show constraint applies to every row with in the
-- Rec list record
type family RecAll (f :: u -> Type) (rs :: [u]) (c :: Type -> Constraint) :: Constraint where
  RecAll f '[] c = ()
  RecAll f (r ': rs) c = (c (f r), RecAll f rs c)

data Nat = Z | S Nat

type family RIndex (r :: u) (rs :: [u]) :: Nat where
   RIndex r (r ': rs) = 'Z
   RIndex r (s ': rs) = 'S (RIndex r rs)
