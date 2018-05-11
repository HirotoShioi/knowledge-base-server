{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import qualified Data.List.NonEmpty       as N
import           Network.Wai.Handler.Warp (run)
import           RIO.Directory            (listDirectory)
import           RIO.Text                 (decodeUtf8With, lenientDecode)
import           RIO.Time                 (getCurrentTime)
import           Say                      (say)
import           Servant
import           System.ReadEnvVar        (readEnvDef)

import           API                      (Knowledgebase, api)
import           Exceptions
import           Parser.Parser            (Document (..), Parser, parseFAQ,
                                           parseKnowledge)
import           Types                    (FAQ (..), Knowledge, Output (..))

data Config = Config
    { cfgKnowledge :: ![Knowledge]
    , cfgFAQ       :: ![FAQ]
    }

-- | Path to knowledge directory
knowledgeDir :: FilePath
knowledgeDir = "./doc/Knowledges/"

-- | Path to FAQ directory
faqDir :: FilePath
faqDir = "./doc/FAQ/"

-- | Metadata path
metaDataPath :: FilePath -> FilePath
metaDataPath path = path <> "/MetaData.md"

-- | Description path
descPath :: FilePath -> [FilePath]
descPath path = map (\ f -> path <> "/" <> f <> ".md") ["en", "ja"]

-- | Parse each file
parseFiles :: (Document -> Parser a) -> FilePath -> IO a
parseFiles parser path = do
    descFiles    <- mapM readFileBinary $ descPath path
    categoryFile <- readFileBinary $ metaDataPath path
    let doc = Document (decodeUtf8With lenientDecode categoryFile)
                       (decodeUtf8With lenientDecode <$> descFiles)
        eitherParsedData = parser doc
    case eitherParsedData of
        Left e           -> throwM $ ParseError e path -- How do we throw error while running?
        Right parsedData -> return parsedData

-- | Parse directory
parseDirectory :: (Document -> Parser a) -> FilePath -> IO [a]
parseDirectory parser path = do
    pContent <- listDirectory path
-- Ignore file that starts with '.'
    let filteredContent = filter (\dir -> N.head dir /= '.') (N.fromList <$> pContent)
        contentPaths = (path ++) <$> (N.toList <$> filteredContent)
    mapM (parseFiles parser) contentPaths

-- | Given directory, parse them using the parser and return list of parsed datas.
generateData :: (Document -> Parser a) -> FilePath -> IO [a]
generateData parser path = do
    say $ "Parsing markdowns on: " <> tshow path
    parsedData <- parseDirectory parser path
    say "Parsing completed successfully!"
    return parsedData

-- | Server
server :: Config -> Server Knowledgebase
server Config{..} =
         getOutput cfgKnowledge
    :<|> getOutput cfgFAQ

-- | Create output data that server returns
getOutput :: [a] -> Servant.Handler (Output a)
getOutput xs = do
    currTime <- liftIO getCurrentTime
    let oNum = length xs
    return $ Output currTime xs oNum

main :: IO ()
main = do
    knowledge  <- generateData parseKnowledge knowledgeDir
    faqs  <- generateData parseFAQ faqDir
    port       <- readEnvDef "PORT" 8080
    let config = Config knowledge faqs
    say $ "Starting the server at: " <> tshow port
    run port $ serve api (server config)
