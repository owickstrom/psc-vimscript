{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                          (Result (..), decode)
import           Language.PureScript.CoreFn.FromJSON
import qualified Data.ByteString.Lazy                as BS
import           Data.Aeson.Types                    (parse)
import           Control.Monad
import           Data.Foldable
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Version               (Version)
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Lib
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.PrettyPrint.Mainland  (pretty)
import qualified Vimscript.AST              as Vim
import           Vimscript.Render

loadModule :: FilePath -> IO (Version, Module Ann)
loadModule path = do
  j <- BS.readFile path
  case decode j of
    Just val ->
      case parse moduleFromJSON val of
        Success m -> return m
        Error e   -> fail e
    Nothing -> fail "Couldn't read CoreFn file."

moduleOutPath :: FilePath -> ModuleName -> FilePath
moduleOutPath outDir mn@(ModuleName properNames) =
  outDir </> "pack/purs/opt" </> T.unpack pn </> "plugin" </> pluginFilePath
  where
    (Vim.PackName pn) = modulePackName mn
    pluginFilePath = foldl' addProperName outDir properNames ++ ".vim"
    addProperName path n = path </> T.unpack (runProperName n)

moduleForeignsPath :: FilePath -> ModuleName -> FilePath
moduleForeignsPath srcDir (ModuleName properNames) =
  foldl' go srcDir properNames ++ ".vim"
  where
    go path n = path </> T.unpack (runProperName n)

readForeignFile :: FilePath -> IO (Maybe T.Text)
readForeignFile path = do
  exists <- doesFileExist path
  if exists
    then Just . T.pack <$> readFile path
    else pure Nothing

readForeignFileFromPaths :: [FilePath] -> IO (Maybe T.Text)
readForeignFileFromPaths [] = pure Nothing
readForeignFileFromPaths (p:ps) =
  readForeignFile p >>= \case
    Just t -> pure (Just t)
    Nothing -> readForeignFileFromPaths ps

data InputModule = InputModule
  { version       :: Version
  , pursModule    :: Module Ann
  , outPath       :: FilePath
  , foreignsPaths :: [FilePath]
  }

importedModuleNames :: InputModule -> Set ModuleName
importedModuleNames im = Set.fromList (map snd (moduleImports (pursModule im)))

loadModules :: IO [InputModule]
loadModules = do
  files <- getArgs
  forM files $ \f -> do
    (v, m) <- loadModule f
    pure
      InputModule
      { version = v
      , pursModule = m
      , outPath = moduleOutPath "vim-output" (moduleName m)
      , foreignsPaths =
          map
            (`moduleForeignsPath` moduleName m)
            [ "test/purs-src"
            , "lib/purescript-bifunctors/src"
            , "lib/purescript-eff/src"
            , "lib/purescript-foldable-traversable/src"
            , "lib/purescript-maybe/src"
            , "lib/purescript-newtype/src"
            , "lib/purescript-control/src"
            , "lib/purescript-either/src"
            , "lib/purescript-invariant/src"
            , "lib/purescript-monoid/src"
            , "lib/purescript-prelude/src"
            ]
      }

indexModules :: [InputModule] -> Map ModuleName (Module Ann)
indexModules ims =
  Map.fromList [(moduleName (pursModule im), pursModule im) | im <- ims]

main :: IO ()
main = do
  modules <- loadModules
  let modulesByName = indexModules modules
  forM_ modules $ \im -> do
    foreigns <- readForeignFileFromPaths (foreignsPaths im)
    let prg = genModule modulesByName foreigns (version im, pursModule im)
        t = pretty 200 (renderProgram prg)
    createDirectoryIfMissing True (takeDirectory (outPath im))
    writeFile (outPath im) t
