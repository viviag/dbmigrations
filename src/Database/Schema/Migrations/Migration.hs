{-# LANGUAGE OverloadedStrings #-}
module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    , emptyMigration
    )
where

import Database.Schema.Migrations.Dependencies

import Data.ByteString (ByteString)
import Data.Time () -- for UTCTime Show instance
import qualified Data.Time.Clock as Clock

data Migration = Migration { mTimestamp :: Maybe Clock.UTCTime
                           , mId :: ByteString
                           , mDesc :: Maybe ByteString
                           , mApply :: ByteString
                           , mRevert :: Maybe ByteString
                           , mDeps :: [ByteString]
                           }
               deriving (Eq, Show, Ord)

instance Dependable Migration where
    depsOf = mDeps
    depId = mId

emptyMigration :: ByteString -> Migration
emptyMigration name =
  Migration { mTimestamp = Nothing
            , mId = name
            , mApply = ""
            , mRevert = Nothing
            , mDesc = Nothing
            , mDeps = []
            }

newMigration :: ByteString -> Migration
newMigration theId = 
  (emptyMigration theId) 
    { mApply = "(Apply SQL here.)"
    , mDesc = Just "(Describe migration here.)"
    }
