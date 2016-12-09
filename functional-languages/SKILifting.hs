{-# LANGUAGE LambdaCase #-}

type VarName = String

data E
    = App E E
    | Lam VarName E
    | VarE VarName
    | Plus
    | Lit Int
  deriving (Show)
v = VarE

body (Lam _ b) = b
body _ = error "Not so easy ha?!"

expr1 = Lam "x" (Plus `App` Lit 1 `App` Lit 2)

data SKI
    = S SKI SKI
    | K SKI
    | I
    | AppSKI SKI SKI
    | VarSKI VarName
    | PlusSKI
    | LitSKI Int
  deriving (Show)

skiLifting :: E -> SKI
skiLifting = \case
    e@(Lam v b) ->
        if topEndLambda b
        then case b of
            App _ _ -> sTransform e
            VarE _ -> iTransform e
            _ -> kTransform e
        else skiLifting b

    App e1 e2 -> skiLifting e1 `AppSKI` skiLifting e2
    Plus -> PlusSKI
    VarE n -> VarSKI n
    Lit n -> LitSKI n
  where
    sTransform (Lam var (App e1 e2)) =
        S (skiLifting (Lam var e1)) (skiLifting (Lam var e2))
    iTransform (Lam var (VarE var'))
        | var == var' = I
        | otherwise = error "This should never happen."
    kTransform (Lam var const) = K (toSKI const)
      where
        toSKI (Lit n) = LitSKI n
        toSKI Plus = PlusSKI

topEndLambda :: E -> Bool
topEndLambda (Lam _ _) = False
topEndLambda (App e1 e2) = topEndLambda e1 && topEndLambda e2
topEndLambda _ = True
