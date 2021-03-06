{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import           Control.Lens.TH (makeLenses)
import           Data.Extensible
import qualified Data.List.NonEmpty as N
import           Network.Wai.Handler.Warp (run)
import           RIO.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist
                               , listDirectory)
import           RIO.Text (decodeUtf8With, lenientDecode)
import           RIO.Time (getCurrentTime)
import           Say (say, sayString)
import           Servant
import           System.ReadEnvVar (readEnvDef)

import           API (Knowledgebase, api)
import           CLI (CLI (..), getCliArgs)
import           Exceptions
import           Parser.Parser (Document, Parser, parseFAQ, parseKnowledge)
import           Types (FAQ, Knowledge, Output)

data Config = Config
    { _cfgKnowledge :: ![Knowledge]
    , _cfgFAQ       :: ![FAQ]
    }

makeLenses ''Config

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
parseFiles :: (MonadIO m, MonadThrow m)
           => (Document -> Parser a)
           -> FilePath -> m a
parseFiles parser path = do
    descFiles    <- mapM readFileBinary $ descPath path
    metadataFile <- readFileBinary $ metaDataPath path
    let doc :: Document
        doc = #metadata     @= decodeUtf8With lenientDecode metadataFile
           <: #descriptions @= decodeUtf8With lenientDecode <$> descFiles
           <: nil
        eitherParsedData = parser doc
    case eitherParsedData of
        Left e           -> do
            say $ "Parsing failed on: " <> tshow path
            throwM $ ParseError e path -- How do we throw error while running?
        Right parsedData -> do
            say $ "Parsed " <> tshow path <> " successfuly"
            return parsedData

-- | Parse directory
parseDirectory :: (MonadIO m, MonadThrow m) 
               => (Document -> Parser a)
               -> FilePath -> m [a]
parseDirectory parser path = do
    pContent <- listDirectory path
-- Ignore file that starts with '.'
    let filteredContent = filter (\dir -> N.head dir /= '.') (N.fromList <$> pContent)
        contentPaths = (path ++) <$> (N.toList <$> filteredContent)
    mapM (parseFiles parser) contentPaths

-- | Given directory, parse them using the parser and return list of parsed datas.
generateData :: (MonadIO m, MonadThrow m)
             => (Document -> Parser a)
             -> FilePath -> m [a]
generateData parser path = do
    say $ "\nParsing markdowns on: " <> tshow path
    parseDirectory parser path

-- | Server
server :: Config -> Server Knowledgebase
server cfg =
         getOutput (cfg ^. cfgKnowledge)
    :<|> getOutput (cfg ^. cfgFAQ)

-- | Create output data that server returns
getOutput :: [a] -> Servant.Handler (Output a)
getOutput xs = do
    currTime <- liftIO getCurrentTime
    let count = length xs
    return $ #timestamp       @= currTime
          <: #data            @= xs
          <: #numberOfOutputs @= count
          <: nil

-- | Create new knowledge/faq with filename
createNew :: (MonadIO m) => FilePath -> FilePath -> String -> m ()
createNew templatePath dirPath filename = do
    let filePath = dirPath  <> filename
    doeExist <- doesDirectoryExist filePath
    if doeExist
      then throwString "Existing directory"
      else do
        ds <- listDirectory templatePath
        createDirectoryIfMissing True filePath
        mapM_ (\name -> copyFile
                          (templatePath <> "/" <> name)
                          (dirPath <> filename <> "/" <> name))
                          ds
        sayString $ "Created new file at: " <> filePath

main :: IO ()
main = do
    cliArgs <- getCliArgs
    case cliArgs of
        (NewFAQ filename) -> createNew "./doc/Templates/FAQ" faqDir filename
        (NewKnowledge filename) -> createNew "./doc/Templates/Knowledge" knowledgeDir filename
        VerifyDocs -> do
            say "Verifying all the documents in the doc directory..."
            void $ generateData parseKnowledge knowledgeDir
            void $ generateData parseFAQ faqDir
            say "All documents are valid!"
        RunServer -> do
            knowledge  <- generateData parseKnowledge knowledgeDir
            faqs  <- generateData parseFAQ faqDir
            port       <- readEnvDef "PORT" 8080
            let config = Config knowledge faqs
            say $ "Starting the server at: " <> tshow port
            run port $ serve api (server config)
