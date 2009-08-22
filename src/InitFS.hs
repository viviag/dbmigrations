{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where
import System.Environment ( getArgs )
import System.Exit
    ( exitWith
    , ExitCode(..)
    , exitSuccess
    )
import System.IO
    ( stdout
    , hFlush
    , hGetBuffering
    , hSetBuffering
    , stdin
    , BufferMode(..)
    )
import Control.Exception
    ( bracket
    )
import Data.Maybe
    ( listToMaybe
    , catMaybes
    , isJust
    , fromJust
    )
import Data.List
    ( intercalate
    , sortBy
    )
import qualified Data.Map as Map
import Control.Monad.Reader
    ( ReaderT
    , asks
    , runReaderT
    )
import Control.Monad
    ( when
    , forM_
    )
import Control.Monad.Trans
    ( liftIO
    )
import Database.HDBC.Sqlite3
    ( connectSqlite3
    , Connection
    )
import Database.HDBC
    ( IConnection(commit, rollback, disconnect)
    , catchSql
    , seErrorMsg
    , SqlError
    )
import Database.Schema.Migrations
    ( migrationsToApply
    , migrationsToRevert
    , missingMigrations
    , createNewMigration
    , ensureBootstrappedBackend
    )
import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationMap
    )
import Database.Schema.Migrations.Backend
    ( Backend
    , applyMigration
    , revertMigration
    )
import Database.Schema.Migrations.Store
    ( loadMigrations
    , fullMigrationName
    )
import Database.Schema.Migrations.Backend.HDBC ()

-- A command has a name, a number of required arguments' labels, a
-- number of optional arguments' labels, and an action to invoke.
data Command = Command { cName :: String
                       , cRequired :: [String]
                       , cOptional :: [String]
                       , cAllowedOptions :: [CommandOption]
                       , cDescription :: String
                       , cHandler :: CommandHandler
                       }

data AppState = AppState { appOptions :: [CommandOption]
                         , appCommand :: Command
                         , appRequiredArgs :: [String]
                         , appOptionalArgs :: [String]
                         }

type AppT a = ReaderT AppState IO a

-- The type of actions that are invoked to handle specific commands
type CommandHandler = AppT ()

-- Options which can be used to alter command behavior
data CommandOption = Test
                   | NoAsk
                   deriving (Eq)

reset :: String
reset = "\027[0m"

red :: String -> String
red s = "\27[31m" ++ s ++ reset

green :: String -> String
green s = "\27[32m" ++ s ++ reset

blue :: String -> String
blue s = "\27[34m" ++ s ++ reset

-- yellow :: String -> String
-- yellow s = "\27[33m" ++ s ++ reset

-- magenta :: String -> String
-- magenta s = "\27[35m" ++ s ++ reset

-- cyan :: String -> String
-- cyan s = "\27[36m" ++ s ++ reset

-- white :: String -> String
-- white s = "\27[37m" ++ s ++ reset

unbufferedGetChar :: IO Char
unbufferedGetChar = do
  bufferingMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin bufferingMode
  return c

prompt :: (Eq a) => String -> [(Char, a)] -> IO a
prompt _ [] = error "prompt requires a list of choices"
prompt message choiceMap = do
  putStr $ message ++ " (" ++ choiceStr ++ "): "
  hFlush stdout
  c <- unbufferedGetChar
  case lookup c choiceMap of
    Nothing -> if c == '\n'
               then retry
               else putStrLn "" >> retry
    Just val -> putStrLn "" >> return val
    where
      retry = prompt message choiceMap
      choiceStr = intercalate "" $ map (return . fst) choiceMap

optionMap :: [(String, CommandOption)]
optionMap = [ ("--test", Test)
            , ("--no-ask", NoAsk)]

hasOption :: CommandOption -> AppT Bool
hasOption o = asks ((o `elem`) . appOptions)

ifOption :: CommandOption -> AppT a -> AppT a -> AppT a
ifOption opt yes no = do
  result <- hasOption opt
  if result then yes else no

isSupportedCommandOption :: String -> Bool
isSupportedCommandOption s = isJust $ lookup s optionMap

isCommandOption :: String -> Bool
isCommandOption s = take 2 s == "--"

convertOptions :: [String] -> Either String ([CommandOption], [String])
convertOptions args = if null unsupportedOptions
                      then Right (supportedOptions, rest)
                      else Left $ "Unsupported option(s): " ++ intercalate ", " unsupportedOptions
    where
      allOptions = filter isCommandOption args
      supportedOptions = catMaybes $ map (\s -> lookup s optionMap) args
      unsupportedOptions = [ s | s <- allOptions, not $ isSupportedCommandOption s ]
      rest = [arg | arg <- args, not $ isCommandOption arg]

commands :: [Command]
commands = [ Command "new" ["store_path", "migration_name"] [] [NoAsk] "Create a new empty migration" newCommand
           , Command "apply" ["store_path", "db_path", "migration_name"] [] []
                         "Apply the specified migration and its dependencies" applyCommand
           , Command "revert" ["store_path", "db_path", "migration_name"] [] []
                         "Revert the specified migration and those that depend on it" revertCommand
           , Command "test" ["store_path", "db_path", "migration_name"] [] []
                         "Test the specified migration by applying and reverting it in a transaction, then roll back"
                         testCommand
           , Command "upgrade" ["store_path", "db_path"] [] [Test]
                         "Install all migrations that have not yet been installed" upgradeCommand
           , Command "upgrade-list" ["store_path", "db_path"] [] []
                         "Show the list of migrations to be installed during an upgrade" upgradeListCommand
           ]

withConnection :: FilePath -> (Connection -> IO a) -> AppT a
withConnection dbPath act = liftIO $ bracket (connectSqlite3 dbPath) disconnect act

interactiveAskDeps :: MigrationMap -> IO [String]
interactiveAskDeps mapping = do
  -- For each migration in the store, starting with the most recently
  -- added, ask the user if it should be added to a dependency list
  let migrations = Map.elems mapping
      sorted = sortBy compareTimestamps migrations
  interactiveAskDeps' mapping (map mId sorted)
      where
        compareTimestamps m1 m2 = compare (mTimestamp m2) (mTimestamp m1)

data AskDepsChoice = Yes | No | View | Done | Quit
                     deriving (Eq)

askDepsChoices :: [(Char, AskDepsChoice)]

askDepsChoices = [ ('y', Yes)
                 , ('n', No)
                 , ('v', View)
                 , ('d', Done)
                 , ('q', Quit)
                 ]

interactiveAskDeps' :: MigrationMap -> [String] -> IO [String]
interactiveAskDeps' _ [] = return []
interactiveAskDeps' mapping (name:rest) = do
  result <- prompt ("Depend on '" ++ (green name) ++ "'?") askDepsChoices
  if (result == Done) then return [] else
      do
        case result of
          Yes -> do
            next <- interactiveAskDeps' mapping rest
            return $ name:next
          No -> interactiveAskDeps' mapping rest
          View -> do
            -- load migration
            let Just m = Map.lookup name mapping
            -- print out description, timestamp, deps
            when (isJust $ mDesc m) (putStrLn $ (blue "  Description: ") ++ (fromJust $ mDesc m))
            putStrLn $ (blue "      Created: ") ++ (show $ mTimestamp m)
            when (not $ null $ mDeps m) $ putStrLn $ (blue "  Deps: ") ++ (intercalate "\n        " $ mDeps m)
            -- ask again
            interactiveAskDeps' mapping (name:rest)
          Quit -> do
            exitWith (ExitFailure 1)
          Done -> return []

confirmCreation :: String -> [String] -> IO Bool
confirmCreation migrationId deps = do
  putStrLn ""
  putStrLn $ "Confirm: create migration '" ++ (green migrationId) ++ "'"
  if (null deps) then putStrLn "  (No dependencies)"
     else putStrLn "with dependencies:"
  forM_ deps $ \d -> putStrLn $ "  " ++ (green d)
  result <- prompt "Are you sure?" [('y', 'y'), ('n', 'n')]
  return $ result == 'y'

newCommand :: CommandHandler
newCommand = do
  required <- asks appRequiredArgs
  let [fsPath, migrationId] = required
      store = FSStore { storePath = fsPath }
  mapping <- liftIO $ loadMigrations store
  fullPath <- liftIO $ fullMigrationName store migrationId

  when (isJust $ Map.lookup migrationId mapping) $
       liftIO $ do
         putStrLn $ red $ "Migration " ++ (show fullPath) ++ " already exists"
         exitWith (ExitFailure 1)

  -- Default behavior: ask for dependencies
  deps <- ifOption NoAsk (return [])
                           (liftIO $ do
                              putStrLn $ "Selecting dependencies for new migration: " ++ migrationId
                              interactiveAskDeps mapping)

  result <- liftIO $ confirmCreation migrationId deps
  liftIO $ case result of
             True -> do
               status <- createNewMigration store migrationId deps
               case status of
                 Left e -> putStrLn (red e) >> (exitWith (ExitFailure 1))
                 Right _ -> putStrLn $ "Migration created successfully: " ++ (green $ show fullPath)
             False -> do
               putStrLn $ red "Migration creation cancelled."

upgradeCommand :: CommandHandler
upgradeCommand = do
  required <- asks appRequiredArgs
  let [fsPath, dbPath] = required
      store = FSStore { storePath = fsPath }
  mapping <- liftIO $ loadMigrations store

  isTesting <- hasOption Test
  withConnection dbPath $ \conn -> do
        ensureBootstrappedBackend conn >> commit conn
        migrationNames <- missingMigrations conn mapping
        when (null migrationNames) (putStrLn "Database is up to date." >> exitSuccess)
        forM_ migrationNames $ \migrationName -> do
            m <- lookupMigration mapping migrationName
            apply m mapping conn
        case isTesting of
          True -> do
                 rollback conn
                 putStrLn "Upgrade test successful."
          False -> do
                 commit conn
                 putStrLn "Database successfully upgraded."

upgradeListCommand :: CommandHandler
upgradeListCommand = do
  required <- asks appRequiredArgs
  let [fsPath, dbPath] = required
      store = FSStore { storePath = fsPath }
  mapping <- liftIO $ loadMigrations store

  withConnection dbPath $ \conn -> do
        ensureBootstrappedBackend conn >> commit conn
        migrationNames <- missingMigrations conn mapping
        when (null migrationNames) (putStrLn "Database is up to date." >> exitSuccess)
        putStrLn "Migrations to install:"
        forM_ migrationNames (putStrLn . ("  " ++) . green)

reportSqlError :: SqlError -> IO a
reportSqlError e = do
  putStrLn $ "\n" ++ (red $ "A database error occurred: " ++ seErrorMsg e)
  exitWith (ExitFailure 1)

apply :: (IConnection b, Backend b IO) => Migration -> MigrationMap -> b -> IO [Migration]
apply m mapping backend = do
  -- Get the list of migrations to apply
  toApply' <- migrationsToApply mapping backend m
  toApply <- case toApply' of
               Left e -> do
                 putStrLn $ red $ "Error: " ++ e
                 exitWith (ExitFailure 1)
               Right ms -> return ms

  -- Apply them
  if (null toApply) then
      (nothingToDo >> return []) else
      mapM_ (applyIt backend) toApply >> return toApply

    where
      nothingToDo = do
        putStrLn $ "Nothing to do; " ++
                     (mId m) ++
                     " already installed."

      applyIt conn it = do
        putStr $ "Applying: " ++ (green $ mId it) ++ "... "
        applyMigration conn it
        putStrLn $ green "done."

revert :: (IConnection b, Backend b IO) => Migration -> MigrationMap -> b -> IO [Migration]
revert m mapping backend = do
  -- Get the list of migrations to revert
  toRevert' <- liftIO $ migrationsToRevert mapping backend m
  toRevert <- case toRevert' of
                Left e -> do
                  putStrLn $ red $ "Error: " ++ e
                  exitWith (ExitFailure 1)
                Right ms -> return ms

  -- Revert them
  if (null toRevert) then
      (nothingToDo >> return []) else
      mapM_ (revertIt backend) toRevert >> return toRevert

    where
      nothingToDo = do
        putStrLn $ "Nothing to do; " ++
                   (mId m) ++
                   " not installed."

      revertIt conn it = do
        putStr $ "Reverting: " ++ (green $ mId it) ++ "... "
        revertMigration conn it
        putStrLn $ green "done."

lookupMigration :: MigrationMap -> String -> IO Migration
lookupMigration mapping name = do
  let theMigration = Map.lookup name mapping
  case theMigration of
    Nothing -> do
      liftIO $ do
        putStrLn $ red $ "No such migration: " ++ name
        exitWith (ExitFailure 1)
    Just m' -> return m'

applyCommand :: CommandHandler
applyCommand = do
  required <- asks appRequiredArgs
  let [fsPath, dbPath, migrationId] = required
      store = FSStore { storePath = fsPath }
  mapping <- liftIO $ loadMigrations store

  withConnection dbPath $ \conn -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration mapping migrationId
        apply m mapping conn
        commit conn
        putStrLn "Successfully applied migrations."

revertCommand :: CommandHandler
revertCommand = do
  required <- asks appRequiredArgs
  let [fsPath, dbPath, migrationId] = required
      store = FSStore { storePath = fsPath }
  mapping <- liftIO $ loadMigrations store

  withConnection dbPath $ \conn ->
      liftIO $ do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration mapping migrationId
        revert m mapping conn
        commit conn
        putStrLn "Successfully reverted migrations."

testCommand :: CommandHandler
testCommand = do
  required <- asks appRequiredArgs
  let [fsPath, dbPath, migrationId] = required
      store = FSStore { storePath = fsPath }
  mapping <- liftIO $ loadMigrations store

  withConnection dbPath $ \conn -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration mapping migrationId
        migrationNames <- missingMigrations conn mapping
        -- If the migration is already installed, remove it as part of
        -- the test
        when (not $ migrationId `elem` migrationNames) $ (revert m mapping conn) >> return ()
        applied <- apply m mapping conn
        forM_ (reverse applied) $ \migration -> do
                             revert migration mapping conn
        rollback conn
        putStrLn "Successfully tested migrations."

usageString :: Command -> String
usageString command = intercalate " " ((blue $ cName command):requiredArgs ++ optionalArgs ++ options)
    where
      requiredArgs = map (\s -> "<" ++ s ++ ">") $ cRequired command
      optionalArgs = map (\s -> "[" ++ s ++ "]") $ cOptional command
      options = map (\s -> "[" ++ s ++ "]") $ optionStrings
      optionStrings = map (\o -> fromJust $ lookup o flippedOptions) $ cAllowedOptions command
      flippedOptions = map (\(a,b) -> (b,a)) optionMap

usage :: IO a
usage = do
  putStrLn $ "Usage: initstore-fs <" ++ (blue "command") ++ "> [args]"
  putStrLn "Commands:"
  forM_ commands $ \command -> do
          putStrLn $ "  " ++ usageString command
          putStrLn $ "    " ++ cDescription command
          putStrLn ""
  exitWith (ExitFailure 1)

usageSpecific :: Command -> IO a
usageSpecific command = do
  putStrLn $ "Usage: initstore-fs " ++ usageString command
  exitWith (ExitFailure 1)

findCommand :: String -> Maybe Command
findCommand name = listToMaybe [ c | c <- commands, cName c == name ]

main :: IO ()
main = do
  allArgs <- getArgs
  when (null allArgs) usage

  let (commandName:unprocessedArgs) = allArgs
  (opts, args) <- case convertOptions unprocessedArgs of
                    Left e -> putStrLn e >> usage
                    Right c -> return c

  command <- case findCommand commandName of
               Nothing -> usage
               Just c -> return c

  let (required,optional) = splitAt (length $ cRequired command) args
      st = AppState { appOptions = opts
                    , appCommand = command
                    , appRequiredArgs = required
                    , appOptionalArgs = optional
                    }

  if (length args) < (length $ cRequired command) then
      usageSpecific command else
      (runReaderT (cHandler command) st) `catchSql` reportSqlError
