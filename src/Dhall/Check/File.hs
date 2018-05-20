{-# LANGUAGE OverloadedStrings #-}

module Dhall.Check.File
  ( allFiles
  , allTypeDefs
  , allDhallFiles
  , checkFile
  , dhallTypeOf
  , loadFile

  , CheckException (..)
  ) where

import           Control.Exception (Exception, displayException, throwIO, throw)
import           Control.Monad (forM, filterM, when)
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TLIO
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import qualified System.Directory as File
import qualified System.FilePath as File
import           System.FilePath ((</>))

-- | Turn a string into a 'DhallType'.
-- >>> dhallType "Entry"
-- DhallType "entry"
dhallType :: String -> DhallType
dhallType = DhallType . map toLower

-- | Get the 'DhallType' for a file.
-- >>> dhallTypeOf "file.entry.dh"
-- DhallType "entry"
dhallTypeOf :: FilePath -> DhallType
dhallTypeOf f =
  case File.takeExtension . File.dropExtension $ f of
    "" -> throw $ WithoutType f
    (_:xs) -> DhallType . map toLower $ xs


data CheckException
  = CouldntFindTypeDefs
  | ParseException FilePath Dhall.ParseError
  | UnknownType FilePath DhallType
  | WithoutType FilePath
  | TypeChecking FilePath (Dhall.TypeError Dhall.Src Dhall.X)
  deriving (Show)

instance Exception CheckException where
  displayException CouldntFindTypeDefs =
    "Error: Expected a .dhall file with type definitions, but couldn't find it."
  displayException (UnknownType f (DhallType t)) =
    "Error: The file " ++ f ++ "has an unknown type: " ++ t
  displayException (ParseException f pe) =
    "While parsing " ++ f ++ ":" ++ displayException pe
  displayException (TypeChecking f te) =
    "While type checking " ++ f ++ ":" ++ displayException te
  displayException (WithoutType f) =
    "The file " ++ f ++ " seems to have no type associated with it."

-- | A dhall type, e.g. "entry" for "file.entry.dh"
-- Should only contain lower-case strings.
newtype DhallType = DhallType String
  deriving (Eq, Ord, Show)

-- | A list of all files in a directory with the one of the given extensions,
-- using absolute paths.
--
-- >>> allFiles "/home/user" [".hs"]
-- ["/home/user/project/Main.hs", ...]
allFiles :: Foldable t => FilePath -> t String -> IO [FilePath]
allFiles dir exts = do
  absDir <- File.makeAbsolute dir
  files <- map (absDir </>) <$> File.listDirectory absDir
  inThisDir <- flip filterM files $ \f -> do
    exists <- File.doesFileExist (absDir </> f)
    let valid = File.takeExtension f `elem` exts
    pure $ exists && valid
  subdirs <- filterM File.doesDirectoryExist files
  inSubDirs <- forM subdirs $ \d -> do
    allFiles (dir </> d) exts
  pure $ inThisDir ++ concat inSubDirs


-- | Get all type definitions in the ".dhall" directory of a given directory
-- and return them in a map of filename without ".dht" -> expression in file.
allTypeDefs
  :: String
  -> FilePath
  -> IO (Map DhallType (Dhall.Expr Dhall.Src Dhall.X))
allTypeDefs ext dir = do
  let path = dir </> ext
  isThere <- File.doesDirectoryExist path
  when (not isThere) $ do
    throwIO CouldntFindTypeDefs
  files <- allFiles path [ext]
  exprs <- forM files $ \f -> do
    let name = dhallType $ File.takeFileName $ File.dropExtension f
    sequence (name, loadFile f)
  pure $ Map.fromList exprs


-- | Get all a list of all dhall files in the given directory using absolute paths.
allDhallFiles
  :: String
  -> FilePath
  -> IO [(FilePath, Dhall.Expr Dhall.Src Dhall.X)]
allDhallFiles ext dir = do
  files <- allFiles dir [ext]
  forM files $ \f -> do
    sequence (f, loadFile f)


-- | Compile a dhall file.
loadFile :: FilePath -> IO (Dhall.Expr Dhall.Src Dhall.X)
loadFile f = do
  let delta = "(input)"
  content <- TLIO.readFile f
  case Dhall.exprFromText delta content of
    Left e -> throwIO $ ParseException f e
    Right e -> do
      File.setCurrentDirectory (File.takeDirectory f)
      Dhall.load e


-- | Typecheck a single file against one of the given types.
-- Throws an exception on failure to do so.
--
-- >>> checkFile (Map.fromList [("conf", .. )]) ("file.conf.dh", ..)
-- /throws/ TypeChecking "file.conf.dh" (TypeError ..)
checkFile :: Map DhallType (Dhall.Expr Dhall.Src Dhall.X) -> (FilePath, Dhall.Expr Dhall.Src Dhall.X) -> IO ()
checkFile types (f, expr) = do
  let name = dhallTypeOf f
  case Map.lookup name types of
    Nothing -> do
      throwIO $ UnknownType f name
    Just t -> do
      let annot = case (expr, t) of
            (Dhall.Note (Dhall.Src begin1 end1 bytes1) _, Dhall.Note (Dhall.Src _ _ bytes2) _) ->
              Dhall.Note (Dhall.Src begin1 end1 bytes') (Dhall.Annot expr t)
              where
                bytes' = bytes1 <> "\n\n : \n\n" <> bytes2
            _ -> Dhall.Annot expr t
      case Dhall.typeOf annot of
        Left err -> throwIO $ TypeChecking f err
        Right _ -> pure ()
