module Main where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
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

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \f -> do
    (version, m) <- loadModule f
    let out = moduleOutPath "vim-output" (moduleName m)
        foreignsPath = moduleForeignsPath "test/purs-src" (moduleName m)
        prg = genModule (version, m)
        t = T.pack (pretty 200 (renderProgram prg))
    foreigns <- fromMaybe mempty <$> readForeignFile foreignsPath
    createDirectoryIfMissing True (takeDirectory out)
    T.writeFile out (foreigns <> t)
