module CLI
    ( CLI(..)
    , getCliArgs
    ) where

import           RIO

import           Options.Applicative
import           Paths_cardano_knowledgebase_server (version)

data CLI
    = RunServer
    | NewKnowledge String
    | NewFAQ String
    deriving Show

cmdNewKnowledge :: Parser CLI
cmdNewKnowledge = NewKnowledge <$> argument str
                      ( metavar "KNOWLEDGE_NAME"
                      <> help "Name of new knowledge")

cmdNewFAQ :: Parser CLI
cmdNewFAQ = NewFAQ <$> argument str
                ( metavar "FAQ_NAME"
                <> help "Name of new FAQ")

-- | Parser for CLI commands
cli :: Parser CLI
cli = hsubparser $ mconcat
        [
          command "run-server" (info (pure RunServer)
            (progDesc "Run server"))
        , command "new-knowledge" (info cmdNewKnowledge
            (progDesc "Create new knowledge"))
        , command "new-faq" (info cmdNewFAQ
            (progDesc "Create new FAQ"))
        ]

getCliArgs :: IO CLI
getCliArgs = execParser opts
        where
        opts = info (cli <**> helper <**> versionHelper)
            ( fullDesc
            <> header "Log classifier"
            <> progDesc "Client for peforming analysis on Zendesk"
            )
        versionHelper = infoOption
            ("Log classifier version" <> show version)
            (long "version" <> help "Show version")
