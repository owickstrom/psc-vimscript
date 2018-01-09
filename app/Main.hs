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

moduleForeignsPath :: FilePath -> FilePath
moduleForeignsPath mPath =
  replaceExtension mPath ".vim"

readForeignFile :: FilePath -> IO (Maybe T.Text)
readForeignFile path = do
  exists <- doesFileExist path
  if exists
    then Just . T.pack <$> readFile path
    else pure Nothing

data InputModule = InputModule
  { version       :: Version
  , pursModule    :: Module Ann
  , outPath       :: FilePath
  , foreignsPath  :: FilePath
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
      , foreignsPath = moduleForeignsPath (modulePath m)
      }

indexModules :: [InputModule] -> Map ModuleName (Module Ann)
indexModules ims =
  Map.fromList [(moduleName (pursModule im), pursModule im) | im <- ims]

main :: IO ()
main = do
  modules <- loadModules
  let modulesByName = indexModules modules
  forM_ modules $ \im -> do
    foreigns <- readForeignFile (foreignsPath im)
    let prg = genModule modulesByName foreigns (version im, pursModule im)
        t = pretty 200 (renderProgram prg)
    createDirectoryIfMissing True (takeDirectory (outPath im))
    writeFile (outPath im) t
