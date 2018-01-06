module Main where

import           Control.Monad
import           Data.Foldable
import qualified Data.Text                  as T
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Lib
import           System.Directory
import           System.Environment
import           System.FilePath
import Vimscript.Render
import           Text.PrettyPrint.Mainland  (pretty)

moduleOutPath :: FilePath -> ModuleName -> FilePath
moduleOutPath outDir (ModuleName properNames) =
  foldl' go outDir properNames ++ ".vim"
    where
      go path n = path </> T.unpack (runProperName n)

main :: IO ()
main = do
  files <- getArgs
  modules <- mapM loadModule files
  forM_ modules $ \(version, m) -> do
    let out = moduleOutPath "vim-output" (moduleName m)
        prg = genModule (version, m)
    createDirectoryIfMissing True (takeDirectory out)
    writeFile out (pretty 200 (renderProgram prg))
