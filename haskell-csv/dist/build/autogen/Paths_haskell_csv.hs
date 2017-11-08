module Paths_haskell_csv (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "./"
libdir     = "/home/vitalii-dev/.cabal/lib/x86_64-linux-ghc-7.10.3/haskell-csv-0.1.0.0-HdKr2SWPSoQCXbC3GoBSQ1"
datadir    = "/home/vitalii-dev/.cabal/share/x86_64-linux-ghc-7.10.3/haskell-csv-0.1.0.0"
libexecdir = "/home/vitalii-dev/.cabal/libexec"
sysconfdir = "/home/vitalii-dev/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_csv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_csv_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_csv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_csv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_csv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
