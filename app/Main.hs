module Main where

import           Control.Monad
import           Data.Foldable
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Version               (Version)
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Lib
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.PrettyPrint.Mainland  (pretty)
import           Vimscript.Render

moduleOutPath :: FilePath -> ModuleName -> FilePath
moduleOutPath outDir (ModuleName properNames) =
  foldl' go outDir properNames ++ ".vim"
  where
    go path n = path </> T.unpack (runProperName n)

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

data InputModule = InputModule
  { version      :: Version
  , pursModule   :: Module Ann
  , outPath      :: FilePath
  , foreignsPath :: FilePath
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
      , outPath = moduleOutPath ("vim-output" </> "autoload") (moduleName m)
      , foreignsPath = moduleForeignsPath "test/purs-src" (moduleName m)
      }

indexModules :: [InputModule] -> Map ModuleName (Module Ann)
indexModules ims =
  Map.fromList [(moduleName (pursModule im), pursModule im) | im <- ims]

main :: IO ()
main = do
  modules <- loadModules
  let modulesByName = indexModules modules
  forM_ modules $ \im -> do
    let importedModules = modulesByName -- Map.restrictKeys modulesByName (importedModuleNames im)
        prg = genModule importedModules (version im, pursModule im)
        t = T.pack (pretty 200 (renderProgram prg))
    foreigns <- fromMaybe mempty <$> readForeignFile (foreignsPath im)
    createDirectoryIfMissing True (takeDirectory (outPath im))
    T.writeFile (outPath im) (foreigns <> t)
