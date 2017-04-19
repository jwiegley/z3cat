{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wall #-}

module Z3.Choice where

import Prelude hiding (id, (.), curry, uncurry, const)

import ConCat.AltCat (ccc)
import Text.Show.Functions ()
import Z3.Category

class Show p => Choiceable p where
    project :: p -> String
    project = show

instance Choiceable ()
instance Choiceable Bool
instance Choiceable Int
instance (Choiceable a, Choiceable b) => Choiceable (a, b)

data Z3Choice where
    Z3Choice :: (Choiceable p, EvalE p, GenE p) => (p -> Bool) -> Z3Choice

instance Monoid Z3Choice where
    mempty = Z3Choice (\() -> True)
    Z3Choice y `mappend` Z3Choice x =
        Z3Choice (\(p1, p2) -> y p1 && x p2)

satisfy :: (Choiceable a, EvalE a, GenE a) => (a -> Bool) -> Z3Choice
satisfy = Z3Choice

-- decide :: (Choiceable a, EvalE a, GenE a) => (a -> Z3Choice) -> Z3Choice
-- decide f = Z3Choice (\a -> case f a of Z3Choice g -> g undefined)

choose :: Z3Choice -> Z3Choice -> Z3Choice
choose (Z3Choice x) (Z3Choice y) =
    Z3Choice (\((p1, p2), b) -> if b then x p1 else y p2)
{-# INLINE choose #-}

collection :: [Z3Choice] -> Z3Choice
collection = foldr choose mempty

solution :: (EvalE p, GenE p) => (p -> Bool) -> IO (Maybe p)
solution f = runZ3 (ccc @Z3Cat f)
{-# INLINE satisfy #-}

runZ3Choice :: Z3Choice -> IO (Maybe String)
runZ3Choice (Z3Choice f) = fmap (fmap show) (solution f)
{-# INLINE runZ3Choice #-}
