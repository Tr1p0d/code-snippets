{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

type Name = String

data Type
    = TInt
    | TBool
    | Arr Type Type
    | Fun BuiltinFun
  deriving (Eq)

data BuiltinFun
    = Add Type Type
  deriving (Eq)

instance Show Type where
    show = \case
        TInt -> "Int"
        TBool -> "Bool"
        Arr t1 t2 -> show t1 ++ " -> " ++ show t2

data E
    = Var Name
    | App E E
    | Lam Name Type E
    | ENum Int
    | EBool Bool
    | EIf E E E

type Env = [(Name, Type)]

data TypeError
    = TypeMismatch Type Type
    | NotAFunctin Type
    | NotInScope Name
  deriving (Show)

type Check = ExceptT TypeError (Reader Env)

extend :: (Name, Type) -> Env -> Env
extend = (:)

inEnv :: (Name, Type) -> Check a -> Check a
inEnv x = mapExceptT (local $ extend x)

emptyEnv :: Env
emptyEnv = []

lookupVar :: Name -> Check Type
lookupVar v = do
    env <- lift ask
    case lookup v env of
        Just v -> return v
        Nothing -> throwE $ NotInScope v

runTypeChecker :: E -> Either TypeError Type
runTypeChecker = flip runReader emptyEnv . runExceptT . typeCheck

typeCheck :: E -> Check Type
typeCheck = \case
    Var n -> lookupVar n

    ENum _ -> return TInt

    EBool _ -> return TBool

    EIf ec e1 e2 -> do
        ecT <- typeCheck ec
        if ecT /= TBool
        then
            throwE $ TypeMismatch ecT TBool
        else do
            e1T <- typeCheck e1
            e2T <- typeCheck e2
            if e1T == e2T
            then return e1T
            else
                throwE $ TypeMismatch e1T e2T

    App f e2 -> do
        tf <- typeCheck f
        e2T <- typeCheck e2
        case tf of
            Arr tfa1 tfa2 ->
                if tfa1 == e2T
                then return tfa2
                else throwE $ TypeMismatch tfa1 e2T
            notAFun -> throwE $ NotAFunctin notAFun

    Lam x xt e -> do
        tb <- inEnv (x, xt) $ typeCheck e
        return $ Arr xt tb

    Fun builtin -> typeCheckBuiltIn builtin

--typeCheckBuiltIn :: BuiltinFun -> Check Type
typeCheckBuiltIn = \case
    Add _ _ -> _ --TInt `Arr` TInt `Arr` TInt
