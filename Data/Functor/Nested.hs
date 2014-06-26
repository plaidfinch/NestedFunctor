{- |
Module      :  Control.Comonad.Sheet
Description :  Composition of functors with a type index tracking nesting.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module implements something akin to 'Data.Compose', but with a type index that tracks the order in which things
are nested. This makes it possible to write code using polymorphic recursion over the levels of the structure contained
in a 'Nested' value.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Functor.Nested where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Traversable
import Data.Distributive

-- | @Flat x@ is the type index used for the base case of a 'Nested' value. Thus, a @(Nested (Flat []) Int@ is
--   isomorphic to a @[Int]@.
data Flat (x :: * -> *)
-- | @Nest o i@ is the type index used for the recursive case of a 'Nested' value: the @o@ parameter is the type 
--   constructors corresponding to the /outside/ levels, and the @i@ parameter is the single type constructor
--   corresponding to the /inner-most/ level. Thus, a @(Nested (Nest (Flat Maybe) []) Int)@ is isomorphic to a
--   @(Maybe [Int])@.
data Nest (o :: *) (i :: * -> *)

-- | A @Nested fs a@ is the composition of all the layers mentioned in @fs@, applied to an @a@. Specifically, the @fs@
--   parameter is a sort of snoc-list holding type constructors of kind @(* -> *)@. The outermost layer appears as the
--   parameter to @Flat@; the innermost layer appears as the rightmost argument to the outermost @Nest@. For instance:
--
-- >                  [Just ['a']]   :: [Maybe [Char]]
-- >             Flat [Just ['a']]   :: Nested (Flat []) (Maybe [Char])
-- >       Nest (Flat [Just ['a']])  :: Nested (Nest (Flat []) Maybe) [Char]
-- > Nest (Nest (Flat [Just ['a']])) :: Nested (Nest (Nest (Flat []) Maybe) []) Char
data Nested fs a where
   Flat :: f a -> Nested (Flat f) a
   Nest :: Nested fs (f a) -> Nested (Nest fs f) a

-- | The @UnNest@ type family describes what happens when you peel off one @Nested@ constructor from a @Nested@ value.
type family UnNest x where
   UnNest (Nested (Flat f) a)    = f a
   UnNest (Nested (Nest fs f) a) = Nested fs (f a)

-- | Removes one @Nested@ constructor (either @Nest@ or @Flat@) from a @Nested@ value.
--
-- > unNest . Nest == id
-- > unNest . Flat == id
--
-- > unNest (Nest (Flat [['x']])) == Flat [['x']]
-- > unNest (Flat (Just 'x')) == Just 'x'
unNest :: Nested fs a -> UnNest (Nested fs a)
unNest (Flat x) = x
unNest (Nest x) = x

instance (Show (f a)) => Show (Nested (Flat f) a) where
   show (Flat x) = "(Flat " ++ show x ++ ")"

instance (Show (Nested fs (f a))) => Show (Nested (Nest fs f) a) where
   show (Nest x) = "(Nest " ++ show x ++ ")"

instance (Functor f) => Functor (Nested (Flat f)) where
   fmap f = Flat . fmap f . unNest

instance (Functor f, Functor (Nested fs)) => Functor (Nested (Nest fs f)) where
   fmap f = Nest . fmap (fmap f) . unNest

instance (Applicative f) => Applicative (Nested (Flat f)) where
   pure              = Flat . pure
   Flat f <*> Flat x = Flat (f <*> x)

instance (Applicative f, Applicative (Nested fs)) => Applicative (Nested (Nest fs f)) where
   pure              = Nest . pure . pure
   Nest f <*> Nest x = Nest ((<*>) <$> f <*> x)

instance (ComonadApply f) => ComonadApply (Nested (Flat f)) where
   Flat f <@> Flat x = Flat (f <@> x)

instance (ComonadApply f, Distributive f, ComonadApply (Nested fs)) => ComonadApply (Nested (Nest fs f)) where
   Nest f <@> Nest x = Nest ((<@>) <$> f <@> x)

instance (Comonad f) => Comonad (Nested (Flat f)) where
   extract   = extract . unNest
   duplicate = fmap Flat . Flat . duplicate . unNest

instance (Comonad f, Comonad (Nested fs), Distributive f, Functor (Nested (Nest fs f))) => Comonad (Nested (Nest fs f)) where
   extract   = extract . extract . unNest
   duplicate =
      fmap Nest . Nest   -- wrap it again: f (g (f (g a))) -> Nested (Nest f g) (Nested (Nest f g) a)
      . fmap distribute  -- swap middle two layers: f (f (g (g a))) -> f (g (f (g a)))
      . duplicate        -- duplicate outer functor f: f (g (g a)) -> f (f (g (g a)))
      . fmap duplicate   -- duplicate inner functor g: f (g a) -> f (g (g a))
      . unNest           -- NOTE: can't pattern-match on constructor or you break laziness!

instance (Foldable f) => Foldable (Nested (Flat f)) where
   foldMap f = foldMap f . unNest

instance (Foldable f, Foldable (Nested fs)) => Foldable (Nested (Nest fs f)) where
   foldMap f = foldMap (foldMap f) . unNest

instance (Traversable f) => Traversable (Nested (Flat f)) where
   traverse f = fmap Flat . traverse f . unNest

instance (Traversable f, Traversable (Nested fs)) => Traversable (Nested (Nest fs f)) where
   traverse f = fmap Nest . traverse (traverse f) . unNest

instance (Alternative f) => Alternative (Nested (Flat f)) where
   empty             = Flat empty
   Flat x <|> Flat y = Flat (x <|> y)

instance (Applicative f, Alternative (Nested fs)) => Alternative (Nested (Nest fs f)) where
   empty             = Nest empty
   Nest x <|> Nest y = Nest (x <|> y)

instance (Distributive f) => Distributive (Nested (Flat f)) where
   distribute = Flat . distribute . fmap unNest

instance (Distributive f, Distributive (Nested fs)) => Distributive (Nested (Nest fs f)) where
   distribute = Nest . fmap distribute . distribute . fmap unNest

class NestedAs x y where
   -- | Given some nested structure which is /not/ wrapped in @Nested@ constructors, and one which is, wrap the first
   --   in the same number of @Nested@ constructors so that they are equivalently nested.
   --
   -- > [['a']] `asNestedAs` Nest (Flat (Just (Just 0))) == Nest (Flat [['a']])
   asNestedAs :: x -> y -> x `AsNestedAs` y

instance ( AsNestedAs (f a) (Nested (Flat g) b) ~ Nested (Flat f) a )
         => NestedAs (f a) (Nested (Flat g) b) where
   x `asNestedAs` _ = Flat x

instance ( AsNestedAs (f a) (UnNest (Nested (Nest g h) b)) ~ Nested fs (f' a')
         , AddNest (Nested fs (f' a')) ~ Nested (Nest fs f') a'
         , NestedAs (f a) (UnNest (Nested (Nest g h) b)))
         => NestedAs (f a) (Nested (Nest g h) b) where
   x `asNestedAs` y = Nest (x `asNestedAs` (unNest y))

-- | This type family calculates the result type of applying the @Nested@ constructors to its first argument a number
--   of times equal to the depth of nesting in its second argument.
type family AsNestedAs x y where
   (f x) `AsNestedAs` (Nested (Flat g) b) = Nested (Flat f) x
   x     `AsNestedAs` y                   = AddNest (x `AsNestedAs` (UnNest y))

-- | This type family calculates the type of a @Nested@ value if one more @Nest@ constructor is applied to it.
type family AddNest x where
   AddNest (Nested fs (f x)) = Nested (Nest fs f) x
