-- | Example showing usage
module Data.Vinyl.Tutorial.Example where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Vinyl

data Fields = Name | Age | Sleeping | Master deriving Show

$(genSingletons [''Fields])

type Person = ['Name, 'Age, 'Sleeping]

data Attr f = Attr { unAttr :: Elf f }

type family Elf (f :: Fields) :: Type where
  Elf 'Name = String
  Elf 'Age = Int
  Elf 'Sleeping = Bool
  Elf 'Master = Rec Attr Person

-- | I think show instances can be written by template haskell or
-- generic programming

instance Show (Attr 'Name) where
  show (Attr name) = name

instance Show (Attr 'Age) where
  show (Attr age) = show age

instance Show (Attr 'Sleeping) where
  show (Attr sleeping) = show sleeping

-- | Sing f is a type witness for f
-- Note we hav
(=::) :: Sing f -> Elf f -> Attr f
(=::) _ x = Attr x

person :: Rec Attr Person
person = SName =:: "Allan"
  :& SAge =:: 28
  :& SSleeping =:: True
  :& RNil

showPerson :: IO ()
showPerson = print person

-- we can have a type level function that pushes new fields to end of list
type Dog = 'Master ': Person

dog :: Rec Attr Dog
dog =
     SMaster =:: person
  :& SName =:: "scobby"
  :& SAge =:: 4
  :& SSleeping =:: True
  :& RNil

wakeup :: 'Sleeping ∈ fields => Rec Attr fields -> Rec Attr fields
wakeup = rput (SSleeping =:: False)

getAge :: 'Age ∈ fields => Rec Attr fields -> Attr 'Age
getAge = rget (Proxy @'Age)
