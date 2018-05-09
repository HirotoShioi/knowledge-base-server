{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Universum

import qualified Data.List.NonEmpty       as N
import           Data.Time                (getCurrentTime)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           System.Directory         (listDirectory)
-- import           System.ReadEnvVar        (readEnvDef)

import           API                      (Knowledgebase, api)
import           Exceptions
import           Parser.Parser            (Parser, Document(..), parseKnowledge,
                                           parseFAQ)
import           Types                    (Knowledge, Output (..),
                                           FAQ (..))

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
    descFiles    <- mapM readFile $ descPath path
    categoryFile <- readFile $ metaDataPath path
    let doc = Document (toLText categoryFile) (toLText <$> descFiles)
        eitherParsedData = parser doc
    case eitherParsedData of
        Left e    -> throwM $ ParseError e path -- How do we throw error while running?
        Right parsedData -> return parsedData

-- | Parse directory
parseDirectory :: (Document -> Parser a) -> FilePath -> IO [a]
parseDirectory parser path = do
    pContent <- listDirectory path
-- Ignore file that starts with '.'
    let filteredContent = filter (\dir -> head dir /= '.') (N.fromList <$> pContent)
        contentPaths = (path ++) <$> (N.toList <$> filteredContent)
    mapM (parseFiles parser) contentPaths

-- | Given directory, parse them using the parser and return list of parsed datas.
generateData :: (Document -> Parser a) -> FilePath -> IO [a]
generateData parser path = do
    putTextLn $ "Parsing markdowns on: " <> show path
    parsedData <- parseDirectory parser path
    putTextLn "Parsing completed successfully!"
    return parsedData

-- | Server
server :: Config -> Server Knowledgebase
server Config{..} = 
         getOutput cfgKnowledge
    :<|> getOutput cfgFAQ

-- | Create output data that server returns
getOutput :: [a] -> Handler (Output a)
getOutput xs = do
    currTime <- liftIO getCurrentTime
    let oNum = length xs
    return $ Output currTime xs oNum

main :: IO ()
main = do
    knowledge  <- generateData parseKnowledge knowledgeDir
    faqs  <- generateData parseFAQ faqDir
    -- port       <- readEnvDef "PORT" 8080
    let config = Config knowledge faqs
        port   = 8080
    putTextLn $ "Starting the server at: " <> show port
    run port $ serve api (server config)
