{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Main where

import ConCat.AltCat (ccc)
import ConCat.Category hiding (it)
import ConCat.Syntactic (render)
import Data.Monoid
import Prelude hiding ((.), id, curry, uncurry)
import Test.Hspec
import Z3.Category
import Z3.Choice

foo :: (Num a, Ord a) => a -> a -> (a -> (a -> Bool) -> Bool) -> Bool
foo x y f = f 222 (< x + y)
{-# NOINLINE foo #-}

equation :: (Num a, Ord a) => a -> a -> Bool
equation x y =
    x < y &&
    y < 100 &&
    foo x 20 f &&
    0 <= x - 3 + 7 * y &&
    (x == y || y + 20 == x + 30)
  where
    f z k = z > 100 && k 20
{-# INLINE equation #-}

main :: IO ()
main = do
    putStrLn $ render (ccc (uncurry (equation @Int)))
    hspec $ do
        describe "Basic tests" $
            it "Runs a Haskell function through Z3" $
                runZ3Show (ccc (uncurry (equation @Int))) `shouldReturn` Just (1, 11)

        describe "Non-deterministic choice" $
            it "Let us satisfy something" $
                runZ3Choice (singleton ((> 3)   :: Int -> Bool) <>
                             (singleton ((< 10) :: Int -> Bool) `choose`
                              singleton ((> 5)  :: Int -> Bool)))
                    `shouldReturn` Nothing
