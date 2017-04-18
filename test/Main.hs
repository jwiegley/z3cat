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

equation :: (Num a, Ord a) => a -> a -> Bool
equation x y =
    let f x y = x + y < 100 in
    x < y &&
    y < 100 &&
    uncurry f (10, 20) &&
    0 <= x - 3 + 7 * y &&
    (x == y || y + 20 == x + 30)
{-# INLINE equation #-}

spec :: IO ()
spec = hspec $
    describe "Basic tests" $
        it "Runs a Haskell function through Z3" $
            runZ3 (ccc (uncurry (equation @Int)))
                `shouldReturn` Just (-8, 2)

main :: IO ()
main = do
    putStrLn $ render (ccc (uncurry (equation @Int)))
    spec
