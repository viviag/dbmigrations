module Main
    ( main
    )
where

import Data.ByteString.Char8 (pack)
import Hasql.Connection
import Prelude  hiding (lookup)
import System.Environment (getArgs)
import System.Exit

import Database.Schema.Migrations.Backend.Hasql (hasqlBackend)
import Moo.Core
import Moo.Main

main :: IO ()
main = do
  args <- getArgs
  (_, opts, _) <- procArgs args
  loadedConf <- loadConfiguration $ _configFilePath opts
  case loadedConf of
    Left e -> putStrLn e >> exitFailure
    Right conf -> do
      let connectionString = pack $ _connectionString conf
      connection <- acquire connectionString
      case connection of
        Left (Just e) -> putStrLn (show e) >> exitFailure
        Left Nothing -> putStrLn "Cannot connect to db." >> exitFailure
        Right conn -> do
          let backend = hasqlBackend conn
              parameters = makeParameters conf backend
          mainWithParameters args parameters
