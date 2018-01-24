{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Moo.CommandHandlers where

import Moo.Core
import Moo.CommandUtils
import Control.Monad ( when, forM_ )
import Data.ByteString.Char8 ( pack, unpack )
import Data.Maybe ( isJust )
import Data.Monoid ( (<>) )
import Control.Monad.Reader ( asks )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import qualified Data.Time.Clock as Clock
import Control.Monad.Trans ( liftIO )

import Database.Schema.Migrations.Store hiding (getMigrations)
import Database.Schema.Migrations
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend

newCommand :: CommandHandler
newCommand storeData = do
  required   <- asks _appRequiredArgs
  store      <- asks _appStore
  linear     <- asks _appLinearMigrations
  timestamp  <- asks _appTimestampFilenames
  timeString <- (++"_") <$> liftIO getCurrentTimestamp

  let [migrationId] = if timestamp
      then fmap (timeString++) required
      else required
  noAsk <- _noAsk <$> asks _appOptions

  liftIO $ do
    fullPath <- fullMigrationName store (pack migrationId)
    when (isJust $ storeLookup storeData (pack migrationId)) $
         do
           putStrLn $ "Migration " ++ (show fullPath) ++ " already exists"
           exitWith (ExitFailure 1)

    -- Default behavior: ask for dependencies if linear mode is disabled
    deps <- if linear then (return $ leafMigrations storeData) else
           if noAsk then (return []) else
           do
             putStrLn $ "Selecting dependencies for new \
                        \migration: " ++ migrationId
             interactiveAskDeps storeData

    result <- if noAsk then (return True) else
              (confirmCreation migrationId (map unpack deps))

    case result of
      True -> do
               now <- Clock.getCurrentTime
               status <- createNewMigration store $ (newMigration (pack migrationId)) { mDeps = deps
                                                    , mTimestamp = Just now
                                                    }
               case status of
                    Left e -> putStrLn e >> (exitWith (ExitFailure 1))
                    Right _ -> putStrLn $ "Migration created successfully: " ++
                               show fullPath
      False -> do
              putStrLn "Migration creation cancelled."

upgradeCommand :: CommandHandler
upgradeCommand storeData = do
  withBackend $ \backend -> do
    ensureBootstrappedBackend backend
    migrationNames <- missingMigrations backend storeData
    when (null migrationNames) $ do
                       putStrLn "Database is up to date."
                       exitSuccess
    forM_ migrationNames $ \migrationName -> do
        m <- lookupMigration storeData migrationName
        apply m storeData backend False
    putStrLn "Database successfully upgraded."

upgradeListCommand :: CommandHandler
upgradeListCommand storeData = do
  withBackend $ \backend -> do
        ensureBootstrappedBackend backend
        migrationNames <- missingMigrations backend storeData
        when (null migrationNames) $ do
                               putStrLn "Database is up to date."
                               exitSuccess
        putStrLn "Migrations to install:"
        forM_ migrationNames (putStrLn . unpack . ("  " <>))

reinstallCommand :: CommandHandler
reinstallCommand storeData = do
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
      ensureBootstrappedBackend backend
      m <- lookupMigration storeData (pack migrationId)

      revert m storeData backend
      apply m storeData backend True

      putStrLn "Migration successfully reinstalled."

listCommand :: CommandHandler
listCommand _ = do
  withBackend $ \backend -> do
      ensureBootstrappedBackend backend
      ms <- getMigrations backend
      forM_ ms $ \m ->
          when (not $ m == rootMigrationName) $ putStrLn (unpack m)

applyCommand :: CommandHandler
applyCommand storeData = do
  required  <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
        ensureBootstrappedBackend backend
        m <- lookupMigration storeData (pack migrationId)
        apply m storeData backend True
        putStrLn "Successfully applied migrations."

revertCommand :: CommandHandler
revertCommand storeData = do
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
      ensureBootstrappedBackend backend
      m <- lookupMigration storeData (pack migrationId)
      revert m storeData backend

      putStrLn "Successfully reverted migrations."

testCommand :: CommandHandler
testCommand storeData = do
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
        ensureBootstrappedBackend backend
        m <- lookupMigration storeData (pack migrationId)
        migrationNames <- missingMigrations backend storeData
        -- If the migration is already installed, remove it as part of
        -- the test
        when (not $ (pack migrationId) `elem` migrationNames) $
             do revert m storeData backend
                return ()
        applied <- apply m storeData backend True
        forM_ (reverse applied) $ \migration -> do
                             revert migration storeData backend
        revert m storeData backend
        putStrLn "Successfully tested migrations."
