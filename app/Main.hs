{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Exception.Safe   (throw)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Semigroup           ((<>))
import qualified Data.Text.Lazy.IO        as LT
import           Data.Time                (getCurrentTime)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           System.Directory         (listDirectory)
import           System.ReadEnvVar        (readEnvDef)

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
    descFiles    <- mapM LT.readFile $ descPath path
    categoryFile <- LT.readFile $ metaDataPath path
    let doc = Document categoryFile descFiles
        eitherParsedData = parser doc
    case eitherParsedData of
        Left e    -> throw $ ParseError e path -- How do we throw error while running?
        Right parsedData -> return parsedData

-- | Parse directory
parseDirectory :: (Document -> Parser a) -> FilePath -> IO [a]
parseDirectory parser path = do
    pContent <- listDirectory path
        -- Ignore file that starts with '.'
    let filteredContent = filter (\dir -> head dir /= '.') pContent
        contentPaths = (path ++) <$> filteredContent
    mapM (parseFiles parser) contentPaths

-- | Given directory, parse them using the parser and return list of parsed datas.
generateData :: (Document -> Parser a) -> FilePath -> IO [a]
generateData parser path = do
    putStrLn $ "Parsing markdowns on: " <> path
    parsedData <- parseDirectory parser path
    putStrLn "Parsing completed successfully!"
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
    port       <- readEnvDef "PORT" 8080
    let config = Config knowledge faqs
    putStrLn $ "Starting the server at: " <> show port
    run port $ serve api (server config)
