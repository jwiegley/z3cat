{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Main where

import ConCat.AltCat (ccc)
import ConCat.Category hiding (it)
import ConCat.Syntactic (render)
import Prelude hiding ((.), id, curry, uncurry)
import Test.Hspec
import Z3.Category

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

spec :: IO ()
spec = hspec $
    describe "Basic tests" $
        it "Runs a Haskell function through Z3" $
            runZ3Show (ccc (uncurry (equation @Int)))
                `shouldReturn` Just (1, 11)

main :: IO ()
main = do
    putStrLn $ render (ccc (uncurry (equation @Int)))
    spec
