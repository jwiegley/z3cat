{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wall #-}

module Z3.Category where

import Prelude hiding (id, (.), curry, uncurry, const)

import ConCat.Category
import ConCat.Rep
import Control.Arrow (Kleisli(..))
import Data.Maybe (catMaybes)
import Prelude hiding ((.), id, curry, uncurry)
import Z3.Monad

data E :: * -> * where
  PrimE :: AST -> E a
  PairE :: E a -> E b -> E (a, b)
  SumE  :: Either (E a) (E b) -> E (Either a b)
  ArrE  :: (E a -> E b) -> E (a -> b)

newtype Z3Cat a b = Z3Cat { runZ3Cat :: Kleisli Z3 (E a) (E b) }

liftE2 :: (AST -> AST -> Z3 AST) -> Z3Cat (a,b) c
liftE2 f = Z3Cat $ Kleisli $ \ (PairE (PrimE a) (PrimE b)) -> PrimE <$> f a b

l2 :: ([a] -> b) -> a -> a -> b
l2 f a1 a2 = f [a1,a2]

instance Category Z3Cat where
    id  = Z3Cat id
    Z3Cat f . Z3Cat g = Z3Cat (f . g)

instance Eq a => EqCat Z3Cat a where
    equal    = liftE2 mkEq
    notEqual = liftE2 (\ a b -> mkNot =<< mkEq a b) -- notC . equal

instance Ord a => OrdCat Z3Cat a where
    lessThan           = liftE2 mkLt
    greaterThan        = liftE2 mkGt
    lessThanOrEqual    = liftE2 mkLe
    greaterThanOrEqual = liftE2 mkGe

instance Fractional a => FractionalCat Z3Cat a where
    recipC = undefined
    divideC = liftE2 mkDiv

instance (RealFrac a, Integral b) => RealFracCat Z3Cat a b where
    floorC = undefined
    ceilingC = undefined

instance Floating a => FloatingCat Z3Cat a where
    expC = undefined
    cosC = undefined
    sinC = undefined

instance (Integral a, Num b) => FromIntegralCat Z3Cat a b where
    fromIntegralC = undefined

instance DistribCat Z3Cat where
    distl = undefined
    distr = undefined

instance (r ~ Rep a) => RepCat Z3Cat a r where
    reprC = undefined
    abstC = undefined

instance (Enum a, Show a) => EnumCat Z3Cat a where
    succC = undefined
    predC = undefined

instance BoolCat Z3Cat where
    notC = Z3Cat $ Kleisli $ \(PrimE b) -> PrimE <$> mkNot b
    andC = liftE2 (l2 mkAnd)
    orC  = liftE2 (l2 mkOr)
    xorC = liftE2 mkXor

instance IfCat Z3Cat a where
  ifC = Z3Cat $ Kleisli $ \ (PairE (PrimE c) (PairE (PrimE t) (PrimE e))) ->
          PrimE <$> mkIte c t e

instance ProductCat Z3Cat where
    exl   = Z3Cat $ Kleisli $ \(PairE b _) -> return b
    exr   = Z3Cat $ Kleisli $ \(PairE _ b) -> return b
    Z3Cat f &&& Z3Cat g = Z3Cat $ Kleisli $ \b -> do
        x <- runKleisli f b
        y <- runKleisli g b
        return $ PairE x y

instance CoproductCat Z3Cat where
    inl   = Z3Cat $ Kleisli $ \b -> return $ SumE (Left b)
    inr   = Z3Cat $ Kleisli $ \b -> return $ SumE (Right b)
    Z3Cat f ||| Z3Cat g = Z3Cat $ Kleisli $ \(SumE b) -> case b of
        Left x -> runKleisli f x
        Right x -> runKleisli g x

constPrim :: (z -> Z3 AST) -> z -> Z3Cat a b
constPrim f x = Z3Cat $ Kleisli $ \_ -> PrimE <$> f x

instance ConstCat Z3Cat Int where
    const = constPrim mkIntNum

instance ConstCat Z3Cat Integer where
    const = constPrim mkIntNum

instance ConstCat Z3Cat Bool where
    const = constPrim mkBool

instance ClosedCat Z3Cat where
    curry = undefined
    uncurry = undefined

instance Num a => NumCat Z3Cat a where
    negateC = undefined
    addC    = liftE2 (l2 mkAdd)
    subC    = liftE2 (l2 mkSub)
    mulC    = liftE2 (l2 mkMul)
    powIC   = error "Z3 doesn't seem to have an exponentiation operator"

runZ3 :: Z3Cat a Bool -> Z3 (E a) -> IO (Maybe [Integer])
runZ3 eq mkVars = evalZ3With Nothing opts $ do
    vars <- mkVars
    PrimE ast <- runKleisli (runZ3Cat eq) vars
    assert ast

    -- check and get solution
    fmap snd $ withModel $ \m ->
        catMaybes <$> mapM (evalInt m) (tolist vars)
  where
    opts = opt "MODEL" True

    tolist :: forall b. E b -> [AST]
    tolist x = case x of
        PrimE p   -> [p]
        PairE p q -> tolist p ++ tolist q
        _         -> []

