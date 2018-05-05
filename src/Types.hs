{-# LANGUAGE DeriveGeneric #-}

module Types 
    ( Category(..)
    , KDescription(..)
    , Knowledge(..)
    , Locale(..)
    , Output(..)
    , QDescription(..)
    , Question(..)
    ) where

import           Data.Aeson
import qualified Data.Text.Lazy as LT
import           Data.Time
import           GHC.Generics

-- | Defining Locale
data Locale
    = En -- ^ English
    | Ja -- ^ Japanese
    deriving (Show, Generic)

-- | Defininng Category
data Category
    = Daedalus -- ^ Frontend
    | Cardano  -- ^ Cardano
    | Backend  -- ^ Backend/wallet
    | Core     -- ^ Core
    | Network  -- ^ Network
    deriving (Show, Generic)

-- | Description for Knowledge
data KDescription = KDescription
    { dLocale   :: !Locale  -- ^ Locale of the description
    , dProblem  :: !LT.Text -- ^ Text describing the problem
    , dSolution :: !LT.Text -- ^ Text describing solution
    } deriving (Show, Generic)

-- | Knowledge needed to perform analysis
data Knowledge = Knowledge
    { kErrorCode   :: !LT.Text       -- ^ Errorcode of the issue
    , kCategory    :: !Category      -- ^ Category of the issue
    , kErrorText   :: !LT.Text       -- ^ Text in which can be used to analyze log file
    , kDescription :: [KDescription] -- ^ Descrption of the issue
    } deriving (Show, Generic)

-- | Description for Question
data QDescription = QDescription
    { qLocale  :: !Locale  -- ^ Locale of the description
    , qQuetion :: !LT.Text -- ^ Question text
    , qAnswer  :: !LT.Text -- ^ Answer to the question
    } deriving (Show, Generic)

-- | Question and answers about Cardano, Daedalus
data Question = Question
    { qCategory    :: !Category       -- ^ Category of the question
    , qDescription :: ![QDescription] -- ^ Description for the question
    } deriving (Show, Generic)

-- | Data in which server returns when server API is called.
data Output a = Output
    { oTimestamp :: !UTCTime -- ^ Timestamp of when the api was called
    , oData      :: ![a]     -- ^ Either list of knowledge or question
    , oNum       :: !Int     -- ^ Number of data
    } deriving (Show, Generic)

-- Todo: Define better instance.. (current json is not formatted nicely)
instance ToJSON Locale where
instance ToJSON KDescription where
instance ToJSON Knowledge where
instance ToJSON Category where
instance ToJSON QDescription where
instance ToJSON Question where
instance ToJSON a => ToJSON (Output a) where
