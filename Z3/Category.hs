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

instance Category Z3Cat where
    id  = Z3Cat id
    Z3Cat f . Z3Cat g = Z3Cat (f . g)

instance Eq a => EqCat Z3Cat a where
    equal    = Z3Cat $ Kleisli $ \(PairE (PrimE a) (PrimE b)) ->
        PrimE <$> mkEq a b
    notEqual = Z3Cat $ Kleisli $ \(PairE (PrimE a) (PrimE b)) ->
        PrimE <$> (mkNot =<< mkEq a b)

instance Ord a => OrdCat Z3Cat a where
    lessThan           = Z3Cat $ Kleisli $ \(PairE (PrimE a) (PrimE b)) ->
        PrimE <$> mkLt a b
    greaterThan        = Z3Cat $ Kleisli $ \(PairE (PrimE a) (PrimE b)) ->
        PrimE <$> mkGt a b
    lessThanOrEqual    = Z3Cat $ Kleisli $ \(PairE (PrimE a) (PrimE b)) ->
        PrimE <$> mkLe a b
    greaterThanOrEqual = Z3Cat $ Kleisli $ \(PairE (PrimE a) (PrimE b)) ->
        PrimE <$> mkGe a b

instance Fractional a => FractionalCat Z3Cat a where
    recipC = undefined
    divideC = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkDiv b c

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
    andC = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkAnd [b, c]
    orC  = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkOr [b, c]
    xorC = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkXor b c

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

instance ConstCat Z3Cat Int where
    const b = Z3Cat $ Kleisli $ \_ -> PrimE <$> mkIntNum b

instance ConstCat Z3Cat Integer where
    const b = Z3Cat $ Kleisli $ \_ -> PrimE <$> mkIntNum b

instance ConstCat Z3Cat Bool where
    const b = Z3Cat $ Kleisli $ \_ -> PrimE <$> mkBool b

instance ClosedCat Z3Cat where
    curry = undefined
    uncurry = undefined

instance Num a => NumCat Z3Cat a where
    negateC = undefined
    addC    = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkAdd [b, c]
    subC    = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkSub [b, c]
    mulC    = Z3Cat $ Kleisli $ \(PairE (PrimE b) (PrimE c)) -> PrimE <$> mkMul [b, c]
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
