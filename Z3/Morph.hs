{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=250 #-}
{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

module Z3.Morph where

import ConCat.Category
import ConCat.Misc ((:+),(:*))
import ConCat.Rep
import Control.Arrow (Kleisli(..), arr)
import Data.Coerce
import Prelude hiding (id, (.), curry, uncurry, const)
import Text.Show.Functions ()

data E f m :: * -> * where
    UnitE   :: E f m ()
    PrimE   :: f a -> E f m a
    PairE   :: E f m a :* E f m b -> E f m (a :* b)
    SumE    :: E f m a :+ E f m b -> E f m (a :+ b)
    ArrE    :: (E f m a -> m (E f m b)) -> E f m (a -> b)
    CoerceE :: Coercible a b => E f m a -> E f m b
    RepE    :: E f m (Rep a) -> E f m a

instance Show (E f m a) where
    show = \case
        UnitE     -> "unit"
        PrimE _x  -> "prim"
        PairE x   -> "(" ++ show x ++ ")"
        SumE x    -> "( " ++ show x ++ ")"
        ArrE _f   -> "arr"
        CoerceE x -> "(coerce " ++ show x ++ ")"
        RepE x    -> "(rep " ++ show x ++ ")"

newtype Morph f m a b = Morph { runMorph :: Kleisli m (E f m a) (E f m b) }

unpairE :: E f m (a :* b) -> E f m a :* E f m b
unpairE (PairE ab) = ab
unpairE _ = error "unpairE: non-pair"

unsumE :: E f m (a :+ b) -> E f m a :+ E f m b
unsumE (SumE ab) = ab
unsumE _ = error "unsumE: non-sum"

pattern M :: (E f m a -> m (E f m b)) -> Morph f m a b
pattern M f = Morph (Kleisli f)

eprim :: Monad m => (E f m a -> m (f b)) -> Morph f m a b
eprim f = M $ fmap PrimE . f

liftE1 :: Monad m => (f a -> m (f b)) -> Morph f m a b
liftE1 f = eprim $ \ (PrimE a) -> f a

liftE2 :: Monad m => (f a -> f b -> m (f c)) -> Morph f m (a, b) c
liftE2 f = eprim $ \ (PairE (PrimE a, PrimE b)) -> f a b

l2 :: ([a] -> b) -> a -> a -> b
l2 f a1 a2 = f [a1,a2]

instance Monad m => Category (Morph f m) where
    id  = Morph id
    Morph f . Morph g = Morph (f . g)

class EqSubcat m a b where
    equalS :: a -> a -> m b

instance (Monad m,
          BoolSubcat m (f (BoolOf (Morph f m))),
          EqSubcat m (f a) (f (BoolOf (Morph f m))))
         => EqCat (Morph f m) a where
    equal = liftE2 equalS

class OrdSubcat m a b where
    lessThanS           :: a -> a -> m b
    greaterThanS        :: a -> a -> m b
    lessThanOrEqualS    :: a -> a -> m b
    greaterThanOrEqualS :: a -> a -> m b

instance (Monad m,
          BoolSubcat m (f (BoolOf (Morph f m))),
          EqSubcat m (f a) (f (BoolOf (Morph f m))),
          OrdSubcat m (f a) (f (BoolOf (Morph f m))))
         => OrdCat (Morph f m) a where
    lessThan           = liftE2 lessThanS
    greaterThan        = liftE2 greaterThanS
    lessThanOrEqual    = liftE2 lessThanOrEqualS
    greaterThanOrEqual = liftE2 greaterThanOrEqualS

class FractionalSubcat m a where
    divideS :: a -> a -> m a

instance (Monad m,
          ConstCat (Morph f m) a, FractionalSubcat m (f a), Num a)
         => FractionalCat (Morph f m) a where
    divideC = liftE2 divideS

class RealFracSubcat m a b where
    floorS   :: a -> m b
    ceilingS :: a -> m b

instance (Monad m, RealFracSubcat m (f a) (f b))
         => RealFracCat (Morph f m) a b where
    floorC   = liftE1 floorS
    ceilingC = liftE1 ceilingS

class FloatingSubcat m a where
    expS :: a -> m a
    cosS :: a -> m a
    sinS :: a -> m a

instance (Monad m, FloatingSubcat m (f a))
         => FloatingCat (Morph f m) a where
    expC = liftE1 expS
    cosC = liftE1 cosS
    sinC = liftE1 sinS

class FromIntegralSubcat m a b where
    fromIntegralS :: a -> m b

instance (Monad m, FromIntegralSubcat m (f a) (f b))
         => FromIntegralCat (Morph f m) a b where
    fromIntegralC = liftE1 fromIntegralS

{-
instance (Monad m, Applicative f) => DistribCat (Morph f m) where
    distl = liftE2 (\x y -> return $ liftA2 go x y)
      where
        go a = \case
            Left z  -> Left (a, z)
            Right z -> Right (a, z)
    distr = liftE2 (\x y -> return $ liftA2 go x y)
      where
        go p a = case p of
            Left z  -> Left (z, a)
            Right z -> Right (z, a)
-}

instance (Monad m, Coercible a b) => CoerceCat (Morph f m) a b where
    coerceC = M $ pure . CoerceE

instance (Monad m, r ~ Rep a) => RepCat (Morph f m) a r where
    reprC = M $ \(RepE x) -> pure x
    abstC = M $ pure . RepE

class EnumSubcat m a where
    succS :: a -> m a
    predS :: a -> m a

instance (Monad m, EnumSubcat m (f a)) => EnumCat (Morph f m) a where
    succC = liftE1 succS
    predC = liftE1 predS

class BoolSubcat m a where
    notS :: a -> m a
    andS :: a -> a -> m a
    orS  :: a -> a -> m a
    xorS :: a -> a -> m a

instance (Monad m, BoolSubcat m (f (BoolOf (Morph f m))))
         => BoolCat (Morph f m) where
    notC = liftE1 notS
    andC = liftE2 andS
    orC  = liftE2 orS
    xorC = liftE2 xorS

class IfSubcat m b a where
    ifS :: b -> a -> a -> m a

instance (Monad m,
          BoolSubcat m (f (BoolOf (Morph f m))),
          IfSubcat m (f (BoolOf (Morph f m))) (f a))
         => IfCat (Morph f m) a where
    ifC = eprim $ \ (PairE (PrimE c, PairE (PrimE t, PrimE e))) -> ifS c t e

instance Monad m => ProductCat (Morph f m) where
    exl = Morph $ arr $ exl . unpairE
    exr = Morph $ arr $ exr . unpairE
    Morph f &&& Morph g = Morph $ arr PairE . (f &&& g)

instance Monad m => CoproductCat (Morph f m) where
    inl = Morph $ arr $ SumE . inl
    inr = Morph $ arr $ SumE . inr
    Morph f ||| Morph g = Morph $ (f ||| g) . arr unsumE

constPrim :: Monad m => (z -> m (f b)) -> z -> Morph f m a b
constPrim f x = eprim (const (f x))

{-
instance (Monad m, Applicative f) => ConstCat (Morph f m) Bool where
    const = constPrim (return . pure)
instance (Monad m, Applicative f) => ConstCat (Morph f m) Int where
    const = constPrim (return . pure)
instance (Monad m, Applicative f) => ConstCat (Morph f m) Integer where
    const = constPrim (return . pure)
instance (Monad m, Applicative f) => ConstCat (Morph f m) Float where
    const = constPrim (return . pure)
instance (Monad m, Applicative f) => ConstCat (Morph f m) Double where
    const = constPrim (return . pure)
-}

instance Monad m => ClosedCat (Morph f m) where
    curry   (Morph (Kleisli f)) = M $ \x -> return $ ArrE $ \y -> f (PairE (x,y))
    uncurry (Morph (Kleisli f)) = M $ \(PairE (x,y)) -> f x >>= \(ArrE f') -> f' y

class NumSubcat m a where
    negateS :: a -> m a
    addS    :: a -> a -> m a
    subS    :: a -> a -> m a
    mulS    :: a -> a -> m a
    powIS   :: a -> a -> m a

instance (Monad m, NumSubcat m (f a)) => NumCat (Morph f m) a where
    negateC = liftE1 negateS
    addC    = liftE2 addS
    subC    = liftE2 subS
    mulC    = liftE2 mulS
    powIC   = error "Z3 doesn't seem to have an exponentiation operator"
