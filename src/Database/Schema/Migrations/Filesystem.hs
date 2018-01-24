{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Filesystem
    ( FilesystemStoreSettings(..)
    , migrationFromFile
    , migrationFromPath
    , filesystemStore
    )
where

import Prelude

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( (</>), takeExtension, dropExtension
                       , takeFileName, takeBaseName )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import Data.Typeable ( Typeable )
import Data.Time.Clock ( UTCTime )
import Data.Time () -- for UTCTime Show instance
import qualified Data.Map as Map

import Control.Monad ( filterM )
import Control.Exception ( IOException, Exception(..), throw, catch )

import Data.Yaml.YamlLight

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , emptyMigration
    )
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store

type FieldProcessor = ByteString -> Migration -> Maybe Migration

data FilesystemStoreSettings = FSStore { storePath :: FilePath }

data FilesystemStoreError = FilesystemStoreError String
                            deriving (Show, Typeable)

instance Exception FilesystemStoreError

throwFS :: String -> a
throwFS = throw . FilesystemStoreError

filenameExtension :: String
filenameExtension = ".txt"

filesystemStore :: FilesystemStoreSettings -> MigrationStore
filesystemStore s =
    MigrationStore { fullMigrationName = fsFullMigrationName s

                   , loadMigration = \theId -> migrationFromFile s theId

                   , getMigrations = do
                       contents <- getDirectoryContents $ storePath s
                       let migrationFilenames = [ f | f <- contents, isMigrationFilename (BSC.pack f) ]
                           fullPaths = [ (f, storePath s </> f) | f <- migrationFilenames ]
                       existing <- filterM (\(_, full) -> doesFileExist full) fullPaths
                       return [ BSC.pack $ dropExtension short | (short, _) <- existing ]

                   , saveMigration = \m -> do
                       filename <- fsFullMigrationName s $ mId m
                       writeFile (BSC.unpack filename) $ (BSC.unpack $ serializeMigration m)
                   }

fsFullMigrationName :: FilesystemStoreSettings -> ByteString -> IO ByteString
fsFullMigrationName s name = return $ BSC.pack $ storePath s </> BSC.unpack name ++ filenameExtension

isMigrationFilename :: ByteString -> Bool
isMigrationFilename path = takeExtension (BSC.unpack path) == filenameExtension

-- |Given a store and migration name, read and parse the associated
-- migration and return the migration if successful.  Otherwise return
-- a parsing error message.
migrationFromFile :: FilesystemStoreSettings -> ByteString -> IO (Either String Migration)
migrationFromFile store name =
    fsFullMigrationName store name >>= migrationFromPath

-- |Given a filesystem path, read and parse the file as a migration
-- return the 'Migration' if successful.  Otherwise return a parsing
-- error message.
migrationFromPath :: ByteString -> IO (Either String Migration)
migrationFromPath pathStr = do
  let name = takeBaseName $ takeFileName path
  (Right <$> process name) `catch` (\(FilesystemStoreError s) -> return $ Left $ "Could not parse migration " ++ path ++ ":" ++ s)

  where
    path = BSC.unpack pathStr
    process name = do
      yaml <- parseYamlFile path `catch` (\(e::IOException) -> throwFS $ show e)

      -- Convert yaml structure into basic key/value map
      let fields = getFields yaml
          missing = missingFields fields

      case length missing of
        0 -> do
          let newM = emptyMigration (BSC.pack name)
          case migrationFromFields newM fields of
            Nothing -> throwFS $ "Error in " ++ (show path) ++ ": unrecognized field found"
            Just m -> return m
        _ -> throwFS $ "Error in " ++ (show path) ++ ": missing required field(s): " ++ (show missing)

getFields :: YamlLight -> [(String, ByteString)]
getFields (YMap mp) = map toPair $ Map.assocs mp
    where
      toPair (YStr k, YStr v) = (BSC.unpack k, v)
      toPair (k, v) = throwFS $ "Error in YAML input; expected string key and string value, got " ++ (show (k, v))
getFields _ = throwFS "Error in YAML input; expected mapping"

missingFields :: [(String, ByteString)] -> [String]
missingFields fs =
    [ k | k <- requiredFields, not (k `elem` inputStrings) ]
    where
      inputStrings = map fst fs

-- |Given a migration and a list of parsed migration fields, update
-- the migration from the field values for recognized fields.
migrationFromFields :: Migration -> [(String, ByteString)] -> Maybe Migration
migrationFromFields m [] = Just m
migrationFromFields m ((name, value):rest) = do
  processor <- lookup name fieldProcessors
  newM <- processor value m
  migrationFromFields newM rest

requiredFields :: [String]
requiredFields = [ "Apply"
                 , "Depends"
                 ]

fieldProcessors :: [(String, FieldProcessor)]
fieldProcessors = [ ("Created", setTimestamp )
                  , ("Description", setDescription )
                  , ("Apply", setApply )
                  , ("Revert", setRevert )
                  , ("Depends", setDepends )
                  ]

setTimestamp :: FieldProcessor
setTimestamp value m = do
  ts <- case readTimestamp (BSC.unpack value) of
          [(t, _)] -> return t
          _ -> fail "expected one valid parse"
  return $ m { mTimestamp = Just ts }

readTimestamp :: String -> [(UTCTime, String)]
readTimestamp = reads

setDescription :: FieldProcessor
setDescription desc m = Just $ m { mDesc = Just desc }

setApply :: FieldProcessor
setApply apply m = Just $ m { mApply = apply }

setRevert :: FieldProcessor
setRevert revert m = Just $ m { mRevert = Just revert }

setDepends :: FieldProcessor
setDepends depString m = Just $ m { mDeps = BSC.words depString }
