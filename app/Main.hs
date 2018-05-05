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
import           Parser.Parser            (MdParser, mdParser, parseKnowledge,
                                           parseQuestion, runParser)
import           Types                    (Knowledge, Output (..),
                                           Question (..))

data Config = Config
    { cfgKnowledge :: ![Knowledge]
    , cfgQuestion  :: ![Question]
    }

-- | Path to knowledge directory
knowledgeDir :: FilePath
knowledgeDir = "./doc/Issues/"

-- | Path to question directory
questionDir :: FilePath
questionDir = "./doc/Questions/"

-- | Metadata path
metaDataPath :: FilePath -> FilePath
metaDataPath path = path <> "/MetaData.md"

-- | Description path
descPath :: FilePath -> [FilePath]
descPath path = map (\ f -> path <> "/" <> f <> ".md") ["en", "ja"]

-- | Parse each file
parseFiles :: MdParser a -> FilePath -> IO a
parseFiles parser path = do
    descFiles    <- mapM LT.readFile $ descPath path
    categoryFile <- LT.readFile $ metaDataPath path
    let eitherParsedData = runParser parser descFiles categoryFile
    case eitherParsedData of
        Left e    -> throw $ ParseError e path -- How do we throw error while running?
        Right parsedData -> return parsedData

-- | Parse directory
parseDirectory :: MdParser a -> FilePath -> IO [a]
parseDirectory parser path = do
    pContent <- listDirectory path
        -- Ignore file that starts with '.'
    let filteredContent = filter (\dir -> head dir /= '.') pContent
        contentPaths = (path ++) <$> filteredContent
    mapM (parseFiles parser) contentPaths

-- | Given directory, parse them using the parser and return list of parsed datas.
generateData :: MdParser a -> FilePath -> IO [a]
generateData parser path = do
    putStrLn $ "Parsing markdowns on: " <> path
    parsedData <- parseDirectory parser path
    putStrLn "Parsing completed successfully!"
    return parsedData

-- | Server
server :: Config -> Server Knowledgebase
server Config{..} = 
         getOutput cfgKnowledge
    :<|> getOutput cfgQuestion

-- | Create output data that server returns
getOutput :: [a] -> Handler (Output a)
getOutput xs = do
    currTime <- liftIO getCurrentTime
    let oNum = length xs
    return $ Output currTime xs oNum

main :: IO ()
main = do
    knowledge  <- generateData (mdParser parseKnowledge) knowledgeDir
    questions  <- generateData (mdParser parseQuestion) questionDir
    port       <- readEnvDef "PORT" 8080
    let config = Config knowledge questions
    putStrLn $ "Starting the server at: " <> show port
    run port $ serve api (server config)
