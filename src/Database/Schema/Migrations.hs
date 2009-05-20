module Database.Schema.Migrations
    ( missingMigrations
    )
where

import qualified Data.Set as Set

import qualified Database.Schema.Migrations.Store as MS
import qualified Database.Schema.Migrations.Backend as B
import Database.Schema.Migrations.Dependencies

missingMigrations :: (B.Backend b, MS.MigrationStore s) => b -> s -> IO [String]
missingMigrations backend store = do
  sm <- MS.getMigrations store
  let storeMigrations = map depId sm
  backendMigrations <- B.getMigrations backend

  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrations)
         (Set.fromList backendMigrations)
