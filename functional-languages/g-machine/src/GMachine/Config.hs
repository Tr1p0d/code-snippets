module GMachine.Config
    (withParsedOptions)
  where

import Data.Monoid ((<>))

import Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    , strOption
    )

import GMachine.Type.Config (Config(Config))


parseConfig :: Parser Config
parseConfig = Config
    <$> strOption
        ( long "input"
        <> metavar "TARGET"
        <> help "Input program to be interpreted."
        )

withParsedOptions :: (Config -> IO ()) -> IO ()
withParsedOptions f = execParser opts >>= f
  where
    opts = info (helper <*> parseConfig)
        ( fullDesc
        <> progDesc "Interpret CORE functional language"
        <> header "g-machine - CORE interpret"
        )
