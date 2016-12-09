{-# LANGUAGE LambdaCase #-}

import Data.Map as M
import Data.Set as S

import Control.Monad.Trans.State


type VarName = String
type SCName = String
type SCCounter = Int

data E
    = App E E
    | Lam VarName E
    | Var VarName
    | Plus E E
    | Lit Int
    | SC SCName
  deriving (Show)
v = Var

expr1 :: E
expr1 = Lam "x" ((Lam "y" (Plus (v "y") (v "x")) `App` (v "x"))) `App` Lit 4

expr2 :: E
expr2 = Lam "x" (Plus (v "z") (v "x"))

lambdaLifting :: E -> State SCCounter (E, M.Map SCName E)
lambdaLifting = \case
    lam@(Lam var expr) ->
            if topEndLambda expr
            then do
                let freeVars = fv expr `S.difference` S.singleton var
                (sc, subst) <- mkSC freeVars lam
                -- | Apply lifted variables
                return (S.foldl (\e var -> App e (Var var)) sc freeVars, subst)
            else do
                (lBody, substBody) <- lambdaLifting expr
                return (Lam var lBody, substBody)
    App e1 e2 -> do
        (l1, subst1) <- lambdaLifting e1
        (l2, subst2) <- lambdaLifting e2
        return (l1 `App` l2, M.union subst1 subst2)
    Plus e1 e2 -> do
        (l1, subst1) <- lambdaLifting e1
        (l2, subst2) <- lambdaLifting e2
        return (l1 `App` l2, M.union subst1 subst2)

    whatEver@_ -> return (whatEver, M.empty)

topEndLambda :: E -> Bool
topEndLambda (Lam _ _) = False
topEndLambda (App e1 e2) = topEndLambda e1 && topEndLambda e2
topEndLambda (Plus e1 e2) = topEndLambda e1 && topEndLambda e2
topEndLambda _ = True

fv :: E -> S.Set VarName
fv (App e1 e2) = fv e1 `S.union` fv e2
fv (Plus e1 e2) = fv e1 `S.union` fv e2
fv (Var name) = S.singleton name
fv (Lam name body) = error "this should not really happen"
fv _ = S.empty

-- | Create new lambdas for each of the free variables
mkSC :: S.Set VarName -> E -> State SCCounter (E, M.Map SCName E)
mkSC freeVars lam@(Lam _ _) = do
    sc <- get
    let s1 = M.singleton (show sc) (S.foldl (\e var -> Lam var e) lam freeVars)
    put $ sc + 1
    return (SC $ show sc, s1)
mkSC _ _ = error "this should also never happen"

lift :: E -> State SCCounter (E, M.Map SCName E)
lift expr =
    if (moreLambdas expr)
    then do
        (e, subst) <- lambdaLifting expr
        (e', subst') <- lift e
        return (e', subst `M.union` subst')
    else return (expr, M.empty)

moreLambdas :: E -> Bool
moreLambdas (Lam _ _) = True
moreLambdas (App e1 e2) = moreLambdas e1 || moreLambdas e2
moreLambdas (Plus e1 e2) = moreLambdas e1 || moreLambdas e2
moreLambdas _ = False
