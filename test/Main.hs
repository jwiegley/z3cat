{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

module Main where

import ConCat.AltCat (ccc)
import ConCat.Category hiding (it)
import Control.Arrow (Kleisli(runKleisli))
import Control.Monad.IO.Class
import Data.Maybe
import Debug.Trace
import Prelude hiding ((.), id, curry, uncurry)
import Test.Hspec
import Z3.Category
import Z3.Monad

assertShow :: AST -> Z3 ()
assertShow ast = do
    traceM =<< astToString ast
    assert ast

equation :: (Num a, Ord a) => a -> a -> Bool
equation x y =
    x < y &&
    y < 100 &&
    0 <= x - 3 + 7 * y &&
    (x == y || y + 20 == x + 30)
{-# INLINE equation #-}

main :: IO ()
main = hspec $
    describe "Basic tests" $
        it "Runs a Haskell function through Z3" $ do
            xs <- liftIO $ runZ3 (ccc (uncurry (equation @Int))) $ do
                x <- mkFreshIntVar "x"
                y <- mkFreshIntVar "y"
                return $ PairE (PrimE x) (PrimE y)
            xs `shouldBe` Just [-8, 2]
