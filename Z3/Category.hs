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
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Z3.Category where

import Prelude hiding (id, (.), curry, uncurry, const)

import Control.Applicative (liftA2)
import Control.Monad (join)

import ConCat.Category
import ConCat.Rep
import Control.Arrow (Kleisli(..))
-- import Data.Maybe (catMaybes)
import Z3.Monad

data E :: * -> * where
  PrimE :: AST -> E a
  PairE :: E a -> E b -> E (a, b)
  SumE  :: Either (E a) (E b) -> E (Either a b)
  -- ArrE  :: (E a -> E b) -> E (a -> b)

deriving instance Show (E a)

flattenE :: E a -> [AST]
flattenE (PrimE ast)      = [ast]
flattenE (PairE a b)      = flattenE a ++ flattenE b
flattenE (SumE (Left a))  = flattenE a
flattenE (SumE (Right b)) = flattenE b
-- flattenE (ArrE _)         = error "flattenE: ArrE"


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

class GenE a where
  genE :: Z3 (E a)

genPrim :: (String -> Z3 AST) -> Z3 (E a)
genPrim mk = PrimE <$> mk "x"

instance GenE Bool   where genE = genPrim mkFreshBoolVar
instance GenE Int    where genE = genPrim mkFreshIntVar
instance GenE Float  where genE = genPrim mkFreshRealVar
instance GenE Double where genE = genPrim mkFreshRealVar

instance (GenE a, GenE b) => GenE (a,b) where
  genE = liftA2 PairE genE genE

class EvalE a where evalE :: Model -> E a -> Z3 (Maybe a)

-- TODO: maybe combine GenE and EvalE

unPrimE :: E a -> AST
unPrimE (PrimE a) = a
unPrimE e = error ("unPrimE: " ++ show e)

evalPrim :: EvalAst Z3 a -> Model -> E a -> Z3 (Maybe a)
evalPrim ev m (PrimE a) = ev m a
evalPrim _ _ e = error ("evalPrim: unexpected non-PrimE " ++ show e)

evalInt' :: Num a => EvalAst Z3 a
evalInt' = (fmap.fmap.fmap.fmap) fromInteger evalInt

evalReal' :: Fractional a => EvalAst Z3 a
evalReal' = (fmap.fmap.fmap.fmap) fromRational evalReal

instance EvalE Bool  where evalE = evalPrim evalBool
instance EvalE Int   where evalE = evalPrim evalInt'

instance EvalE Float   where evalE = evalPrim evalReal'
instance EvalE Double  where evalE = evalPrim evalReal'

instance (EvalE a, EvalE b) => EvalE (a,b) where
  evalE m (PairE a b) = (liftA2.liftA2) (,) (evalE m a) (evalE m b)
  evalE _ e = error ("evalE on pair: unexpected E " ++ show e)

instance (EvalE a, EvalE b) => EvalE (Either a b) where
  evalE m (SumE (Left  a)) = (fmap.fmap) Left  (evalE m a)
  evalE m (SumE (Right b)) = (fmap.fmap) Right (evalE m b)
  evalE _ e = error ("evalE on sum: unexpected E " ++ show e)

runZ3 :: (EvalE a, GenE a) => Z3Cat a Bool -> IO (Maybe a)
runZ3 eq = evalZ3With Nothing opts $ do
    vars <- genE
    PrimE ast <- runKleisli (runZ3Cat eq) vars
    assert ast
    -- check and get solution
    join <$> (fmap snd $ withModel $ flip evalE vars)
  where
    opts = opt "MODEL" True
