{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    (main)
  where

import Data.Tuple (swap)
import System.Environment

import qualified Data.Map.Strict as M

import Control.Lens
import Control.Lens.Cons
import Text.Parsec
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import GMachine.Type.Common (Name)
import GMachine.Type.Core
import GMachine.Type.Heap
import GMachine.Type.GMOutput
import GMachine.Type.GMState
import GMachine.Type.InstructionSet
    ( GMCode
    , Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )
import GMachine.Core.Parser


instance Pretty Node where
    pPrint node = text $ show node

instance Pretty Globals where
    pPrint = vcat . map pPrintGlobal . M.toList
      where
        pPrintGlobal (symbol, address) =
            quotes (text symbol) <> text ":" <+> pPrint address

instance Pretty GMState where
    pPrint GMState{..} = vcat
        [ output
        , code
        , stack
        , dump
        , heap
        , globals
        ]
      where
        code = fsep [text "Code:", pPrint _gCode]
        stack = fsep [text "Stack:", pPrint _gStack]
        heap = fsep [text "Heap:", nest 4 $ prettyHeap _gHeap _gGlobals]
        globals = fsep [text "Globals:", nest 4 $ pPrint _gGlobals]
        dump = fsep [text "Dump:", pPrint _gDump]
        output = fsep [text "Output:", pPrint _gOutput]

prettyHeap :: Heap Node -> Globals -> Doc
prettyHeap (Heap _ _ m) globals = M.foldlWithKey prettyHeapCell empty m
  where
    prettyHeapCell doc addr node = doc $$ int addr <> text ":" <+> case node of
        NInd iAddr -> text "=>" <+> int iAddr
        NNode number -> printNum number
        NGlobal _ _ -> printGlobal addr
        NConstr tag args -> text "Data" <+> integer tag <+> pPrint args
        ap@(NApp a1 a2) -> prettyApplication addr ap

    prettyApplication addr = \case
        NInd addr' -> text "=>" <+> prettyApplication addr' (m M.! addr')
        NGlobal _ _ -> printGlobal addr
        NNode num -> printNum num
        NApp a1 a2 -> text "@" <+> nest 4 (vcat
            [ prettyApplication a2 (m M.! a2)
            , prettyApplication a1 (m M.! a1)
            ])

    printGlobal addr =
        let inverseG = M.fromList $ concatMap ((:[]) . swap) (M.toList globals)
        in text "SC" <+> quotes (text (inverseG M.! addr))

    printNum num = text "Num" <+> quotes (integer num)

instance Pretty Instruction where
    pPrint = text . show

evalG :: GMState -> [GMState]
evalG state
    | isFinal = [state]
    | otherwise = state : (evalG $ gStep state)
  where
    isFinal = null (state ^. gCode)

gStep :: GMState -> GMState
gStep state@(GMState _ (i:is) _ _ _ _) = dispatch i (state & gCode .~ is)

dispatch :: Instruction -> GMState -> GMState
dispatch (Pushglobal n) state@GMState{..} =
    let address = _gGlobals M.! n in state & gStack %~ (address:)

dispatch (Slide n) state = state & gStack %~ (drop n)

dispatch (Pushint n) state@GMState{..} =
    let (nAddress, nHeap) = hAlloc _gHeap (NNode n)
    in state & gStack %~ (nAddress:) & gHeap .~ nHeap

dispatch Mkap state@(GMState _ _ (a1:a2:as) _ heap _) =
    let (nAddress, nHeap) = hAlloc heap (NApp a1 a2)
    in state { _gStack = (nAddress:as), _gHeap = nHeap }

dispatch (Pop n) state = state & gStack %~ (drop n)

dispatch (Push n) state@GMState{..} = state & gStack %~ (_gStack !! n:)

dispatch (Update n) state@GMState{..} =
    let indNode = NInd $ head $ state ^. gStack
        newStack = tail _gStack
        (Just atAddr) = state ^? gStack._tail.ix n
    in state & gHeap %~ hSetAt atAddr indNode & gStack .~ newStack

dispatch Unwind state = unwind state

dispatch (Alloc n) state@GMState{..} =
    let (newHeap, addrs) = allocNodes n _gHeap
    in state & gHeap .~ newHeap & gStack %~ (addrs++)
  where
    allocNodes 0 heap = (heap, [])
    allocNodes n heap =
        let (addr, heap') = hAlloc heap (NInd hNull)
            (heap'', addrs) = allocNodes (n-1) heap'
        in (heap'', addr:addrs)

dispatch Eval state@GMState{..} = state
    & gCode .~ [Unwind]
    & gStack %~ ((:[]) . head)
    & gDump %~ ((_gCode, tail _gStack):)

dispatch (Cond i1 i2) state@GMState{..}
    | isNumberNode $ hLookup _gHeap (head _gStack) =
        if mapBoolean $ hLookup _gHeap (head _gStack)
        then state & gCode %~ (i1 ++)
        else state & gCode %~ (i2 ++)
    | otherwise = error "condition operation is not a number"
  where
    mapBoolean (NNode x)
        | x == 1 = True
        | x == 0 = False
        | otherwise = error "not a boolean"

dispatch (Pack t n) state@GMState{..} =
    let n' = fromInteger n
        (newAddr, newHeap) = hAlloc _gHeap (NConstr t (take n' _gStack))
    in state & gStack %~ ((newAddr:) . drop n') & gHeap .~ newHeap

dispatch (CaseJump tagAssoc) state@GMState{..} =
    case hLookup _gHeap (head _gStack) of
        NConstr t _ -> state
            & gCode %~ ((tagMap M.! (fromInteger t)) ++)
        _ -> error "cannot multiway jump on non-constructor"
  where
    tagMap = M.fromAscList tagAssoc

dispatch (Split n) state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NConstr _ addrs -> state & gStack %~ ((addrs ++) . tail)
    _ -> error "cannot split on non-constructor"

dispatch Print state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NNode n -> state & gOutput %~ (++ number n)
    NConstr tag addrs -> state
        & gCode %~ ((concat $ replicate (length addrs) [Eval, Print]) ++)
        & gStack %~ ((addrs ++) . tail)
        & gOutput %~ (++ "Constructor: " ++ show tag ++ " ")
  where
    number num = ("Number: " ++ show num ++ " ")

dispatch (Arith binop) state@GMState{..}
    | (isNumberNode $ hLookup _gHeap (head _gStack)) &&
      (isNumberNode $ hLookup _gHeap (head $ tail _gStack)) =
        let (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
            (nAddress, nHeap) = case binop of
                Add -> hAlloc _gHeap $ NNode (x+y)
                Sub -> hAlloc _gHeap $ NNode (x-y)
                Mul -> hAlloc _gHeap $ NNode (x*y)
                Div -> hAlloc _gHeap $ NNode (x `div` y)
        in state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
    | otherwise = error "dyadic operation not applied to numbers"

dispatch (Rel relop) state@GMState{..}
    | (isNumberNode $ hLookup _gHeap (head _gStack)) &&
      (isNumberNode $ hLookup _gHeap (head $ tail _gStack)) =
        let (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
            (nAddress, nHeap) = case relop of
                Eq -> hAlloc _gHeap $ NNode $ mapInteger (x==y)
                Neq -> hAlloc _gHeap $ NNode $ mapInteger (x/=y)
                Greater -> hAlloc _gHeap $ NNode $ mapInteger (x>y)
                Geq -> hAlloc _gHeap $ NNode $ mapInteger (x>=y)
                Less -> hAlloc _gHeap $ NNode $ mapInteger (x<y)
                Leq -> hAlloc _gHeap $ NNode $ mapInteger (x<=y)
        in state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
    | otherwise = error "dyadic operation not applied to numbers"
  where
    mapInteger x
        | x = 1
        | otherwise = 0

isNumberNode (NNode _) = True
isNumberNode _ = False

unwind :: GMState -> GMState
unwind state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NInd a -> state & gStack.ix 0 .~ a  & gCode .~ [Unwind]
    NConstr _ _ -> handleConstrNode
      where
        handleConstrNode
            | null $ state ^. gDump = error "dump should not be empty"
            | otherwise =
                let (instrs, stack) = head _gDump
                in state
                    & gCode .~ instrs
                    & gStack._tail .~ stack
                    & gDump %~ tail
    NNode n -> handleNumberNode
      where
        handleNumberNode
            | null $ state ^. gDump = state
            | otherwise =
                let (instrs, stack) = head _gDump
                in state
                    & gCode .~ instrs
                    & gStack._tail .~ stack
                    & gDump %~ tail
    NApp a1 a2 -> state & gStack %~ (a1:) & gCode .~ [Unwind]
    NGlobal nParams c ->
        if nParams > length (tail _gStack)
        then error $ "Not enough arguments to unwind"
        else state & gCode .~ c & gStack %~ rearrangeStack
      where
        rearrangeStack stack = take nParams newStack ++ drop nParams stack
          where
            getArg (NApp a1 a2) = a2
            getArg n = error $ "Not an application node!" ++ show n
            newStack = map (getArg . hLookup _gHeap) (tail stack)

compile :: CoreProgram -> GMState
compile program = GMState [] initialCode [] [] heap globals
  where
    (heap, globals) = buildInitialHeap program
    initialCode = [Pushglobal "main", Eval, Print]

buildInitialHeap :: CoreProgram -> (Heap Node, Globals)
buildInitialHeap program = foldl alloc (hInitial, M.empty) (compiled ++ primitives)
  where
    primitives =
        [ ("+", 2, [Push 1, Eval, Push 1, Eval, Arith Add, Update 2, Pop 2, Unwind])
        , ("-", 2, [Push 1, Eval, Push 1, Eval, Arith Sub, Update 2, Pop 2, Unwind])
        , ("*", 2, [Push 1, Eval, Push 1, Eval, Arith Mul, Update 2, Pop 2, Unwind])
        , ("/", 2, [Push 1, Eval, Push 1, Eval, Arith Div, Update 2, Pop 2, Unwind])
        , ("==", 2, [Push 1, Eval, Push 1, Eval, Rel Eq, Update 2, Pop 2, Unwind])
        , ("if", 3,
            [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
        ]
    compiled = map compileSc (program ++ preludes)
    alloc (heap, globals) (name, nArgs, code) = (newHeap, newGlobals)
      where
        (a, newHeap) = hAlloc heap (NGlobal nArgs code)
        newGlobals = M.insert name a globals

type GCompiledSC = (Name, Int, GMCode)

compileSc :: CoreScDefn -> GCompiledSC
compileSc (name, args, expr) =
    (name, length args, compileR expr (zip args [0..]))

compileR :: CoreExpr -> [(Name, Int)] -> GMCode
compileR e env =
    let d = length env
    in compileC env e ++ [Update d, Pop d, Unwind]

type GMCompiler = [(Name, Int)] -> CoreExpr -> GMCode

argOffset n env' = [(v, n+m) | (v,m) <- env']

compileC :: GMCompiler
compileC env (EVar name)
    | name `M.member` asc = [Push n]
    | otherwise = [Pushglobal name]
  where
    asc = M.fromAscList env
    n = asc M.! name
compileC env (ELet recursive defs e)
    | recursive = compileLetRec compileC defs env e
    | otherwise = compileLet compileC defs env e
compileC env (EConstr tag arity exprs) =
    compilePack env (reverse exprs) ++ [Pack tag arity]
  where
    compilePack env [] = []
    compilePack env (expr:exprs) =
        compileC env expr ++ compilePack (argOffset 1 env) exprs
compileC env (ECase expr alts) =
    compileC env expr ++ [CaseJump (compileAlts alts env)]
compileC env expr = case expr of
    ENum n -> [Pushint n]
    EAp e1 e2 -> compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]

compileArgs defs env =
    let n = length defs
    in  (zip (map fst defs) [n-1, n-2 .. 0]) ++ argOffset n env

compileAlts :: [CoreAlt] -> [(Name, Int)] -> [(Integer, GMCode)]
compileAlts alts env = map compileAlt alts
  where
    compileAlt (tag, names, body) =
        let n = length names
            newEnv = zip names [0..] ++ argOffset n env
        in (tag, [Split n] ++ compileR body newEnv ++ [Slide n])

compileLet :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLet compile' defs env expr =
    compileLet' defs env
    ++ compile' (compileArgs defs env) expr
    ++ [Slide (length defs)]
  where
    compileLet' [] env = []
    compileLet' ((name,expr):args) env =
        compileC env expr ++ compileLet' args (argOffset 1 env)

compileLetRec :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLetRec compile' defs env expr =
    let n = length defs
        env' = compileArgs defs env
    in [Alloc n] ++ compileLetRec' defs env' ++ compile' env' expr ++ [Slide n]
  where
    compileLetRec' [] env' = []
    compileLetRec' ((name, expr'):args) env' =
        compileC env' expr'
        ++ [Update (length args)]
        ++ compileLetRec' args (argOffset 1 env')

main :: IO ()
main = do
    (file:_) <- getArgs
    contents <- readFile file
    case parse parseCoreProgram file contents of
        Right program -> stepRun $ evalG $ compile program
        Left err -> error $ show err

stepRun :: [GMState] -> IO ()
stepRun [] = return ()
stepRun (s:ss) = do
    putStrLn $ render $ pPrint s
    getChar
    stepRun ss
