{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Juego (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lorena/.cabal/bin"
libdir     = "/home/lorena/.cabal/lib/x86_64-linux-ghc-8.0.1/Juego-0.1.0.0"
dynlibdir  = "/home/lorena/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/lorena/.cabal/share/x86_64-linux-ghc-8.0.1/Juego-0.1.0.0"
libexecdir = "/home/lorena/.cabal/libexec"
sysconfdir = "/home/lorena/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Juego_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Juego_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Juego_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Juego_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Juego_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Juego_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
