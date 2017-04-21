{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Z3.Category where

import Prelude hiding (id, (.), curry, uncurry, const)

import ConCat.Misc ((:+),(:*))
import ConCat.Category
import ConCat.Rep
import Control.Applicative (liftA2)
import Control.Arrow (Kleisli(..), arr)
import Control.Monad (join)
import Debug.Trace
import Text.Show.Functions ()
import Z3.Monad

-- Typed AST structures ("expressions")
data E :: * -> * where
    UnitE :: E ()
    PrimE :: AST -> E a
    PairE :: E a :* E b -> E (a :* b)
    SumE  :: E a :+ E b -> E (a :+ b)
    ArrE  :: (E a -> Z3 (E b)) -> E (a -> b)
    RepE  :: E (Rep a) -> E a

deriving instance Show (E a)

unpairE :: E (a :* b) -> E a :* E b
unpairE (PairE ab) = ab
unpairE e = error ("unpairE: non-pair" ++ show e)

unsumE :: E (a :+ b) -> E a :+ E b
unsumE (SumE ab) = ab
unsumE e = error ("unsumE: non-sum" ++ show e)

newtype Z3Cat a b = Z3Cat { runZ3Cat :: Kleisli Z3 (E a) (E b) }

pattern Z :: (E t -> Z3 (E u)) -> Z3Cat t u
pattern Z f = Z3Cat (Kleisli f)

eprim :: (E a -> Z3 AST) -> Z3Cat a b
eprim f = Z $ fmap PrimE . f

liftE1 :: (AST -> Z3 AST) -> Z3Cat a c
liftE1 f = eprim $ \ (PrimE a) -> f a

liftE2 :: (AST -> AST -> Z3 AST) -> Z3Cat (a,b) c
liftE2 f = eprim $ \ (PairE (PrimE a,PrimE b)) -> f a b

l2 :: ([a] -> b) -> a -> a -> b
l2 f a1 a2 = f [a1,a2]

instance Category Z3Cat where
    id  = Z3Cat id
    Z3Cat f . Z3Cat g = Z3Cat (f . g)

instance Eq a => EqCat Z3Cat a where
    equal = liftE2 mkEq
    -- default notEqual = notC . equal

instance Ord a => OrdCat Z3Cat a where
    lessThan           = liftE2 mkLt
    greaterThan        = liftE2 mkGt
    lessThanOrEqual    = liftE2 mkLe
    greaterThanOrEqual = liftE2 mkGe

instance Fractional a => FractionalCat Z3Cat a where
    divideC = liftE2 mkDiv
    recipC = error "recipC not defined for Z3Cat"
    -- default recipC = divideC . lconst 1

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
    reprC = Z $ \(RepE x) -> pure x
    abstC = Z (pure . RepE)

instance (Enum a, Show a) => EnumCat Z3Cat a where
    succC = undefined
    predC = undefined

instance BoolCat Z3Cat where
    notC = liftE1 mkNot
    andC = liftE2 (l2 mkAnd)
    orC  = liftE2 (l2 mkOr)
    xorC = liftE2 mkXor

instance IfCat Z3Cat a where
    ifC = eprim $ \ (PairE (PrimE c, PairE (PrimE t, PrimE e))) -> mkIte c t e

instance ProductCat Z3Cat where
    exl   = Z3Cat $ arr $ exl . unpairE
    exr   = Z3Cat $ arr $ exr . unpairE
    Z3Cat f &&& Z3Cat g = Z3Cat $ arr PairE . (f &&& g)

-- f :: Kleisli Z3 (E a) (E c)
-- g :: Kleisli Z3 (E a) (E d)
-- f &&& g :: Kleisli Z3 (E a) (E c :* E d)
-- arr PairE :: Kleisli Z3 (E c :* E d) (E (c :* d))

instance CoproductCat Z3Cat where
    inl   = Z3Cat $ arr $ SumE . inl
    inr   = Z3Cat $ arr $ SumE . inr
    Z3Cat f ||| Z3Cat g = Z3Cat $ (f ||| g) . arr unsumE

-- f :: Kleisli Z3 (E a) (E c)
-- g :: Kleisli Z3 (E b) (E c)
-- f ||| g :: Kleisli Z3 (E a :+ E b) (E c)
-- (f ||| g) . arr unsumE :: Kleisli Z3 (E (a :+ b)) (E c)

constPrim :: (z -> Z3 AST) -> z -> Z3Cat a b
constPrim f x = eprim (const (f x))

instance ConstCat Z3Cat Int     where const = constPrim mkIntNum
instance ConstCat Z3Cat Integer where const = constPrim mkIntNum
instance ConstCat Z3Cat Bool    where const = constPrim mkBool

instance ClosedCat Z3Cat where
    curry   (Z3Cat (Kleisli f)) = Z $ \x -> return $ ArrE $ \y -> f (PairE (x,y))
    uncurry (Z3Cat (Kleisli f)) = Z $ \(PairE (x,y)) -> f x >>= \(ArrE f') -> f' y

-- Before GHC 8.2, the Z patterns here lead to erroneous warnings: "Pattern
-- match(es) are non-exhaustive". Switch back to Z when we're on 8.2. See
-- https://ghc.haskell.org/trac/ghc/ticket/8779.

instance Num a => NumCat Z3Cat a where
    negateC = liftE1 mkUnaryMinus
    addC    = liftE2 (l2 mkAdd)
    subC    = liftE2 (l2 mkSub)
    mulC    = liftE2 (l2 mkMul)
    powIC   = error "Z3 doesn't seem to have an exponentiation operator"

class GenE a where genE :: Z3 (E a)

genPrim :: (String -> Z3 AST) -> Z3 (E a)
genPrim mk = PrimE <$> mk "x"

instance GenE ()     where genE = return UnitE
instance GenE Bool   where genE = genPrim mkFreshBoolVar
instance GenE Int    where genE = genPrim mkFreshIntVar
instance GenE Float  where genE = genPrim mkFreshRealVar
instance GenE Double where genE = genPrim mkFreshRealVar

instance (GenE a, GenE b) => GenE (a,b) where
  genE = liftA2 (curry PairE) genE genE

class EvalE a where evalE :: Model -> E a -> Z3 (Maybe a)

-- TODO: maybe combine GenE and EvalE

evalPrim :: EvalAst Z3 a' -> (a' -> a) -> Model -> E a -> Z3 (Maybe a)
evalPrim ev f m (PrimE a) = (fmap.fmap) f (ev m a)
evalPrim _  _ _ e = error ("evalPrim: unexpected non-PrimE " ++ show e)

instance EvalE ()     where evalE = evalPrim evalBool (const ())
instance EvalE Bool   where evalE = evalPrim evalBool id
instance EvalE Int    where evalE = evalPrim evalInt  fromInteger
instance EvalE Float  where evalE = evalPrim evalReal fromRational
instance EvalE Double where evalE = evalPrim evalReal fromRational

instance (EvalE a, EvalE b) => EvalE (a :* b) where
    evalE m (PairE (a,b)) = (liftA2.liftA2) (,) (evalE m a) (evalE m b)
    evalE _ e = error ("evalE on pair: unexpected E " ++ show e)

instance (EvalE a, EvalE b) => EvalE (a :+ b) where
    evalE m (SumE (Left  a)) = (fmap.fmap) Left  (evalE m a)
    evalE m (SumE (Right b)) = (fmap.fmap) Right (evalE m b)
    evalE _ e = error ("evalE on sum: unexpected E " ++ show e)

assertShow :: AST -> Z3 ()
assertShow ast = do
    traceM =<< astToString ast
    assert ast

runZ3WithAST :: (EvalE a, GenE a)
             => (AST -> Z3 ()) -> Z3Cat a b -> IO (Maybe a)
runZ3WithAST f eq = evalZ3With Nothing opts $ do
    e <- genE
    res <- runKleisli (runZ3Cat eq) e
    reduce res
    -- check and get solution
    join . snd <$> withModel (`evalE` e)
  where
    opts = opt "MODEL" True

    reduce :: E a -> Z3 ()
    reduce UnitE = return ()
    reduce (PrimE x) = f x
    reduce (PairE (x, y)) = reduce x >> reduce y
    reduce (SumE e) = case e of
        Left x  -> reduce x
        Right x -> reduce x
    reduce (ArrE _) = error "ArrE from runZ3Cat"
    reduce (RepE e) = reduce e

runZ3 :: (EvalE a, GenE a) => Z3Cat a b -> IO (Maybe a)
runZ3 = runZ3WithAST assert

runZ3Show :: (EvalE a, GenE a) => Z3Cat a b -> IO (Maybe a)
runZ3Show = runZ3WithAST assertShow
