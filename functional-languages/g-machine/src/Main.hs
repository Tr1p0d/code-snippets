{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    (main)
  where

import Control.Monad (void)

import Text.Parsec
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import GMachine (evalG)
import GMachine.Compiler (compile)
import GMachine.Config
import GMachine.Core.Parser
import GMachine.Type.Config (Config(Config))
import GMachine.Type.GMState


main :: IO ()
main = withParsedOptions main'
  where
    main' (Config file) = do
        contents <- readFile file
        case parse parseCoreProgram file contents of
            Right program -> stepRun $ evalG $ compile program
            Left err -> error $ show err

stepRun :: [GMState] -> IO ()
stepRun [] = return ()
stepRun (s:ss) = do
    putStrLn $ render $ pPrint s
    void $ getChar
    stepRun ss
