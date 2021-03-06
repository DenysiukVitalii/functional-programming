module Paths_second_project (
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

bindir     = "/home/vitalii-dev/.cabal/bin"
libdir     = "/home/vitalii-dev/.cabal/lib/x86_64-linux-ghc-7.10.3/second-project-0.1.0.0-5zT79iM6xBWKuVCciQWyk6"
datadir    = "/home/vitalii-dev/.cabal/share/x86_64-linux-ghc-7.10.3/second-project-0.1.0.0"
libexecdir = "/home/vitalii-dev/.cabal/libexec"
sysconfdir = "/home/vitalii-dev/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "second_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "second_project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "second_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "second_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "second_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
