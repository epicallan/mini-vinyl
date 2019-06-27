module Data.Vinyl.Lens where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Vinyl.Core (Rec (..))
import Data.Vinyl.TypeLevel (Nat (..), RIndex)


class RIndex r rs ~ i => RecElem (r :: k) (rs :: [k]) (i :: Nat) where
  rlens :: Functor g
        => Proxy r
        -> (f r -> g (f r))
        -> Rec f rs
        -> g (Rec f rs)

  rget :: sing r -> Rec f rs -> f r

  rput :: f r -> Rec f rs -> Rec f rs

instance RecElem r (r ': rs) 'Z where
  rlens _ f (r :& rs) = (:& rs) <$> f r

  rget _ = getConst . rlens (Proxy @r) Const

  rput x = runIdentity . rlens (Proxy @r) (\_ -> Identity x)

instance (RIndex r (s ': rs) ~ 'S i, RecElem r rs i) => RecElem r (s ': rs) ('S i) where
  rlens pxr f (r :& rs) = (r :&) <$> (rlens pxr f rs)

  rget _ = getConst . rlens (Proxy @r) Const

  rput x = runIdentity . rlens (Proxy @r) (\_ -> Identity x)


-- | A shorthand for 'RecElem' which supplies its index.
type r ∈ rs = RecElem r rs (RIndex r rs)

-- TODO: write view and set lens convenience utilities
