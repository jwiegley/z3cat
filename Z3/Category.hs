{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Z3.Category where

import Prelude hiding (id, (.), curry, uncurry, const)

import ConCat.Category
import ConCat.Misc ((:+),(:*))
import Control.Applicative (liftA2)
import Control.Arrow (Kleisli(..))
import Control.Monad (join)
import Data.Coerce
import Debug.Trace
import Text.Show.Functions ()
import Z3.Monad
import qualified Z3.Morph as M

newtype Z3AST a = Z3AST { getZ3AST :: AST }

type Z3Cat = M.Morph Z3AST Z3
type E = M.E Z3AST Z3

instance M.EqSubcat Z3 (Z3AST a) (Z3AST (BoolOf Z3Cat)) where
    equalS (Z3AST x) (Z3AST y) = Z3AST <$> mkEq x y
    {-# INLINE equalS #-}

instance M.OrdSubcat Z3 (Z3AST a) (Z3AST (BoolOf Z3Cat)) where
    lessThanS (Z3AST x) (Z3AST y)           = Z3AST <$> mkLt x y
    {-# INLINE lessThanS #-}
    greaterThanS (Z3AST x) (Z3AST y)        = Z3AST <$> mkGt x y
    {-# INLINE greaterThanS #-}
    lessThanOrEqualS (Z3AST x) (Z3AST y)    = Z3AST <$> mkLe x y
    {-# INLINE lessThanOrEqualS #-}
    greaterThanOrEqualS (Z3AST x) (Z3AST y) = Z3AST <$> mkGe x y
    {-# INLINE greaterThanOrEqualS #-}

instance M.FractionalSubcat Z3 (Z3AST a) where
    divideS (Z3AST x) (Z3AST y) = Z3AST <$> mkDiv x y
    {-# INLINE divideS #-}

instance M.EnumSubcat Z3 (Z3AST a) where
    succS (Z3AST x) = Z3AST <$> (M.l2 mkAdd x =<< mkIntNum (1 :: Int))
    {-# INLINE succS #-}
    predS (Z3AST x) = Z3AST <$> (M.l2 mkSub x =<< mkIntNum (1 :: Int))
    {-# INLINE predS #-}

instance M.BoolSubcat Z3 (Z3AST (BoolOf Z3Cat)) where
    notS (Z3AST x) = Z3AST <$> mkNot x
    {-# INLINE notS #-}
    andS (Z3AST x) (Z3AST y) = Z3AST <$> mkAnd [x, y]
    {-# INLINE andS #-}
    orS (Z3AST x) (Z3AST y)  = Z3AST <$> mkOr [x, y]
    {-# INLINE orS #-}
    xorS (Z3AST x) (Z3AST y) = Z3AST <$> mkXor x y
    {-# INLINE xorS #-}

instance M.IfSubcat Z3 (Z3AST (BoolOf Z3Cat)) (Z3AST a) where
    ifS (Z3AST b) (Z3AST x) (Z3AST y) = Z3AST <$> mkIte b x y
    {-# INLINE ifS #-}

instance ConstCat Z3Cat Bool    where const = M.constPrim (fmap Z3AST . mkBool)
instance ConstCat Z3Cat Int     where const = M.constPrim (fmap Z3AST . mkIntNum)
instance ConstCat Z3Cat Integer where const = M.constPrim (fmap Z3AST . mkIntNum)
instance ConstCat Z3Cat Float   where const = M.constPrim (fmap Z3AST . mkRealNum)
instance ConstCat Z3Cat Double  where const = M.constPrim (fmap Z3AST . mkRealNum)

instance M.NumSubcat Z3 (Z3AST a) where
    negateS (Z3AST x) = Z3AST <$> mkUnaryMinus x
    {-# INLINE negateS #-}
    addS (Z3AST x) (Z3AST y)    = Z3AST <$> mkAdd [x, y]
    {-# INLINE addS #-}
    subS (Z3AST x) (Z3AST y)    = Z3AST <$> mkSub [x, y]
    {-# INLINE subS #-}
    mulS (Z3AST x) (Z3AST y)    = Z3AST <$> mkMul [x, y]
    {-# INLINE mulS #-}
    powIS (Z3AST x) (Z3AST y)   = error "Z3 doesn't seem to have an exponentiation operator"
    {-# INLINE powIS #-}

class GenE a where genE :: Z3 (E a)

genPrim :: (String -> Z3 AST) -> Z3 (E a)
genPrim mk = M.PrimE . Z3AST <$> mk "x"

instance GenE ()     where genE = return M.UnitE
instance GenE Bool   where genE = genPrim mkFreshBoolVar
instance GenE Int    where genE = genPrim mkFreshIntVar
instance GenE Float  where genE = genPrim mkFreshRealVar
instance GenE Double where genE = genPrim mkFreshRealVar

instance (GenE a, GenE b) => GenE (a,b) where
  genE = liftA2 (curry M.PairE) genE genE

class EvalE a where
    evalE :: Model -> E a -> Z3 (Maybe a)

evalPrim :: EvalAst Z3 a' -> (a' -> a) -> Model -> E a -> Z3 (Maybe a)
evalPrim ev f m (M.PrimE (Z3AST a)) = (fmap.fmap) f (ev m a)
evalPrim _  _ _ e = error ("evalPrim: unexpected non-PrimE " ++ show e)
{-# INLINE evalPrim #-}

instance EvalE ()     where evalE = evalPrim evalBool (const ()); {-# INLINE evalE #-}
instance EvalE Bool   where evalE = evalPrim evalBool id; {-# INLINE evalE #-}
instance EvalE Int    where evalE = evalPrim evalInt  fromInteger; {-# INLINE evalE #-}
instance EvalE Float  where evalE = evalPrim evalReal fromRational; {-# INLINE evalE #-}
instance EvalE Double where evalE = evalPrim evalReal fromRational; {-# INLINE evalE #-}

instance (EvalE a, EvalE b) => EvalE (a :* b) where
    evalE m (M.PairE (a,b)) = (liftA2.liftA2) (,) (evalE m a) (evalE m b)
    evalE _ e = error ("evalE on pair: unexpected E " ++ show e)
    {-# INLINE evalE #-}

instance (EvalE a, EvalE b) => EvalE (a :+ b) where
    evalE m (M.SumE (Left  a)) = (fmap.fmap) Left  (evalE m a)
    evalE m (M.SumE (Right b)) = (fmap.fmap) Right (evalE m b)
    evalE _ e = error ("evalE on sum: unexpected E " ++ show e)
    {-# INLINE evalE #-}

assertShow :: AST -> Z3 ()
assertShow ast = do
    traceM =<< astToString ast
    assert ast

runZ3WithAST :: (EvalE a, GenE a)
             => (AST -> Z3 ()) -> Z3Cat a b -> IO (Maybe a)
runZ3WithAST f eq = evalZ3With Nothing opts $ do
    e <- genE
    res <- runKleisli (M.runMorph eq) e
    reduce res
    -- check and get solution
    join . snd <$> withModel (`evalE` e)
  where
    opts = opt "MODEL" True

    reduce :: E a -> Z3 ()
    reduce M.UnitE = return ()
    reduce (M.PrimE (Z3AST x)) = f x
    reduce (M.PairE (x, y)) = reduce x >> reduce y
    reduce (M.SumE e) = case e of
        Left x  -> reduce x
        Right x -> reduce x
    reduce (M.ArrE _) = error "ArrE from runZ3Cat"
    reduce (M.RepE e) = reduce e
    reduce (M.CoerceE e) = reduce (coerce e)

runZ3 :: (EvalE a, GenE a) => Z3Cat a b -> IO (Maybe a)
runZ3 = runZ3WithAST assert

runZ3Show :: (EvalE a, GenE a) => Z3Cat a b -> IO (Maybe a)
runZ3Show = runZ3WithAST assertShow
