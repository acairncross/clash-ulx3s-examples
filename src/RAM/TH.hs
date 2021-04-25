module RAM.TH where

import Control.Monad.IO.Class
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath ((</>))

import Paths_clash_ulx3s_examples (getDataDir)

tilePathTH :: ExpQ
tilePathTH = do
  dataDir <- liftIO getDataDir
  let tilePath = dataDir </> "res/bin/tile.mem"
  addDependentFile tilePath
  lift tilePath
