{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      GenBashrc.PackageManager
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extension.
--
-- TODO: Module description.
module GenBashrc.PackageManager
--  (
--  )
  where

import Control.Applicative (Applicative)
import Data.Eq (Eq)
import Data.Foldable (for_)
import Data.Function (($), (.), const)
import Data.Maybe (Maybe)
import Data.Ord ((>))
import Data.Typeable (Typeable, cast)
import Text.Show (Show(showsPrec), showParen, showString)


data SomePackageManager where
    SomePackageManager :: IsPackageManager a => a -> SomePackageManager

instance Show SomePackageManager where
    showsPrec d (SomePackageManager x) =
        showParen (d > 10) $ showString "SomePackageManager " . showsPrec 11 x

class (Eq a, Show a, Typeable a) => IsPackageManager a where
    castPackageManager :: SomePackageManager -> Maybe a
    castPackageManager (SomePackageManager pkgMngr) = cast pkgMngr

data UnknownPackageManager = UnknownPackageManager
  deriving (Eq, Show)

instance IsPackageManager UnknownPackageManager

unknownPackageManager :: SomePackageManager
unknownPackageManager = SomePackageManager UnknownPackageManager

class HasPackageManager a where
    getPackageManager :: a -> SomePackageManager

whenPackageManager
    :: (HasPackageManager a, IsPackageManager b, Applicative f)
    => (SomePackageManager -> Maybe b)
    -> a
    -> (b -> f ())
    -> f ()
whenPackageManager patternMatch = for_ . patternMatch . getPackageManager

whenPackageManager_
    :: (HasPackageManager a, IsPackageManager b, Applicative f)
    => (SomePackageManager -> Maybe b)
    -> a
    -> f ()
    -> f ()
whenPackageManager_ patternMatch x = whenPackageManager patternMatch x . const
