{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Arrow
import Control.Monad (void)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.Set as Set

import Control.Lens

newtype VarName = VarName { _unVar :: String }
  deriving (Eq, Ord, Show)
makeLenses ''VarName

newtype TVarName = TVarName { _unTVar :: String }
  deriving (Eq, Ord, Show)
makeLenses ''TVarName

newtype TConName = TConName { _unTCon :: String }
  deriving (Eq, Show)
makeLenses ''TConName

data Expr
    = Var VarName
    | Lam VarName Expr
    | App Expr Expr
    | Lit Lit
    | Op Embedded
  deriving (Show)

data Embedded = Add | Sub | Neg | Length
  deriving (Show)

data Lit
    = LInt Integer
    | LBool Bool
    | LList Lit
  deriving (Show)

data Type
    = TVar TVarName
    | TCon TConName [Type]
    | TArrow Type Type
  deriving (Eq, Show)

infixr 7 `TArrow`

data TypeError
    = NotInScope VarName
    | CannotMatch Type Type
    | InfiniteType TVarName Type
  deriving (Show)

data InferState = InferState
    { _count :: Int }
makeLenses ''InferState

type Infer = ExceptT TypeError (State InferState)

typeCheck :: Expr -> Either TypeError Scheme
typeCheck = right enclose . flip evalState emptyState . runExceptT . infer emptyEnv
  where
    emptyState = InferState 0

emptyEnv = M.empty

data Scheme = Scheme (Set.Set TVarName) Type
  deriving (Show)

type Subst = M.Map TVarName Type

emptySubst :: Subst
emptySubst = M.empty

type TypeEnv = M.Map VarName Scheme

class Substituable a where
    ftv :: a -> Set.Set TVarName
    substitute :: Subst -> a -> a

instance Substituable Type where
    ftv = \case
        TVar a -> Set.singleton a
        TCon _ targs -> Set.unions $ map ftv targs
        t1 `TArrow` t2 -> ftv t1 `Set.union` ftv t2

    substitute subst = \case
        t@(TVar a) -> M.findWithDefault t a subst
        TCon a targs -> TCon a $ map (substitute subst) targs
        t1 `TArrow` t2 -> substitute subst t1 `TArrow` substitute subst t2

instance Substituable Scheme where
    ftv (Scheme qVars t) = ftv t `Set.difference` qVars

instance Substituable TypeEnv where
    ftv env = Set.unions $ map ftv (M.elems env)
    substitute s1 env = M.map (substitute s1) env

class Fresh a where
    fresh :: Infer a

instance Fresh Type where
    fresh = do
        s <- lift get
        lift $ modify (count %~ (+1))
        return $ [TVar (TVarName $ 'a':show b) | b <- [0 ..]] !! (s ^. count)

instance Fresh Scheme where
    fresh = Scheme (Set.empty) <$> fresh

enclose :: (Subst, Type) -> Scheme
enclose (s, t) = generalize emptyEnv (substitute s t)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (ftv t `Set.difference` ftv env) t

instantiate :: Scheme -> Infer Type
instantiate (Scheme qVars t) = do
    fVars <- mapM (const fresh) (Set.toList qVars)
    let subst = M.fromList $ zip (Set.toList qVars) fVars
    return $ substitute subst t

lookupTypeEnv :: TypeEnv -> VarName -> Infer Scheme
lookupTypeEnv env name = case M.lookup name env of
    Nothing -> throwE $ NotInScope name
    Just x -> return x

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (substitute s2) s1

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env = \case
    Var a -> (emptySubst, ) <$> (lookupTypeEnv env a >>= instantiate)

    Lam var expr -> do
        fScheme@(Scheme _ fTVar) <- fresh
        let env' = M.insert var fScheme env
        (subst, t) <- infer env' expr
        return (subst, substitute subst  (fTVar `TArrow` t))

    App e1 e2 -> do
        tv <- fresh
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (substitute s1 env) e2
        s3 <- unify (substitute s2 t1) (t2 `TArrow` tv)
        return $ (s3 `compose` s2 `compose` s1, substitute s3 tv)

    Lit (LInt _) -> return (emptySubst, int)

    Lit (LBool _) -> return (emptySubst, boolean)

    Lit (LList _) -> do
        tv <- fresh
        return (emptySubst, genList tv)

    Op op -> inferEmbedded env op

inferEmbedded :: TypeEnv -> Embedded -> Infer (Subst, Type)
inferEmbedded env = \case
    Add -> return binaryNumeric
    Sub -> return binaryNumeric
    Neg -> return unaryLogic
    Length -> do
        tv <- fresh
        return (emptySubst, genList tv `TArrow` int)
  where
    binaryNumeric = (emptySubst, int `TArrow` int `TArrow` int)
    unaryLogic = (emptySubst, boolean `TArrow` boolean)

int :: Type
int = TCon (TConName "Integer") []

boolean :: Type
boolean = TCon (TConName "Boolean") []

genList :: Type -> Type
genList = TCon (TConName "List") . (:[])

unify :: Type -> Type -> Infer Subst
unify (t11 `TArrow` t12) (t21 `TArrow` t22) = do
    s1 <- unify t11 t21
    s2 <- unify (substitute s1 t12) (substitute s1 t22)
    return $ s2 `compose` s1

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a targs1) (TCon b targs2) | a == b = do
    substList <- mapM (uncurry unify) $ zip targs1 targs2
    return $ foldr compose emptySubst substList

unify t1 t2 = throwE $ CannotMatch t1 t2

occursCheck :: TVarName -> Type -> Bool
occursCheck name t = name `Set.member` ftv t

bind :: TVarName -> Type -> Infer Subst
bind name t
    | TVar name == t = return emptySubst
    | occursCheck name t = throwE $ InfiniteType name t
    | otherwise = return $ M.singleton name t
