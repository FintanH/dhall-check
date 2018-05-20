{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception hiding (TypeError)
import           Control.Monad
import qualified System.FilePath as File
import qualified System.FSNotify as FS

import           Dhall.Check.File
import           Dhall.Check.Options as O

main :: IO ()
main = do
  opts <- O.getOptions "dhall-check the dhall compiler"

  here <- maybe defaultDirectory pure $ directory opts
  let ext = O.fileExtension opts

  printExceptions $ do
    typedefs <- allTypeDefs ext here
    dhallfiles <- allDhallFiles ext here
    forM_ dhallfiles $ printExceptions . checkFile typedefs
    putStrLn "Watching for changes.."
    FS.withManager $ \mgr -> do
      _ <- FS.watchTree mgr here (isDhallFile ext) $ \event -> do
        case event of
          FS.Added f _ -> do
            putStrLn $ "\nAdded: " ++ f
            printExceptions $ do
              expr <- loadFile f
              checkFile typedefs (f, expr)
              putStrLn "No errors."
          FS.Modified f _ -> do
            putStrLn $ "\nModified: " ++ f
            printExceptions $ do
              expr <- loadFile f
              checkFile typedefs (f, expr)
              putStrLn "No errors."
          FS.Removed _ _ -> pure ()
      forever $ threadDelay 1000000
  where
    -- | Print 'CheckException's and recover.
    printExceptions comp = do
      result <- try comp
      case result of
        Left e -> putStrLn $ displayException (e :: CheckException)
        Right v -> pure v

    isDhallFile :: String -> FS.Event -> Bool
    isDhallFile ext f = ((==ext) . File.takeExtension . FS.eventPath) f
                 && (not . (==) '.' . head . File.takeFileName . FS.eventPath) f



