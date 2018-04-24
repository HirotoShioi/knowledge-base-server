{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.STM
import           Control.Exception.Safe   (throw)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Semigroup           ((<>))
import qualified Data.Text.Lazy.IO        as LT
import           Data.Time                (getCurrentTime)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           System.Directory         (listDirectory)

import           API                      (Knowledgebase, api)
import           Exceptions
import           Parser.Parser            (Parser (..), parseKnowledge,
                                           parseQuestion)
import           Types                    (Knowledge, Output (..),
                                           Question (..))

data Config = Config
    { cfgKnowledge :: TVar [Knowledge]
    , cfgQuestion  :: TVar [Question]
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
parseFiles :: Parser a -> FilePath -> IO a
parseFiles parser path = do
    descFiles    <- mapM LT.readFile $ descPath path
    categoryFile <- LT.readFile $ metaDataPath path
    let eitherParsedData = runParser parser descFiles categoryFile
    case eitherParsedData of
        Left e    -> throw $ ParseError e path -- How do we throw error while running?
        Right parsedData -> return parsedData

-- | Parse directory
parseDirectory :: Parser a -> FilePath -> IO [a]
parseDirectory parser path = do
    pContent <- listDirectory path
    let contentPaths = (path ++) <$> pContent
    mapM (parseFiles parser) contentPaths

-- | Given directory, parse them using the parser and return list of parsed datas.
generateData :: Parser a -> FilePath -> IO [a]
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
getOutput :: TVar [a] -> Handler (Output a)
getOutput xs = do
    datas    <- liftIO $ readTVarIO xs
    currTime <- liftIO getCurrentTime
    let oNum = length datas
    return $ Output currTime datas oNum

main :: IO ()
main = do
    knowledge  <- generateData (Parser parseKnowledge) knowledgeDir
    tKnowledge <- newTVarIO knowledge
    questions  <- generateData (Parser parseQuestion) questionDir
    tQuestions <- newTVarIO questions
    let config = Config tKnowledge tQuestions
        port   = 8080
    putStrLn $ "Starting the server at: " <> show port
    run port $ serve api (server config)
