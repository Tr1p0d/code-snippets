{-# LANGUAGE TemplateHaskell #-}
module GMachine.Type.Config
    ( Config(Config)
    , _inputProgram
    , inputProgram
    )
  where

import Control.Lens (makeLenses)


newtype Config = Config {_inputProgram :: FilePath}
makeLenses ''Config
