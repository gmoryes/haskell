{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_monopoly (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/malenkijkotik/Desktop/monopoly/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/bin"
libdir     = "/Users/malenkijkotik/Desktop/monopoly/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/lib/x86_64-osx-ghc-8.2.2/monopoly-0.1-GMoIG9xniVuAjlO5AsnEC4-demo-space-junk"
dynlibdir  = "/Users/malenkijkotik/Desktop/monopoly/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/malenkijkotik/Desktop/monopoly/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/share/x86_64-osx-ghc-8.2.2/monopoly-0.1"
libexecdir = "/Users/malenkijkotik/Desktop/monopoly/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/libexec/x86_64-osx-ghc-8.2.2/monopoly-0.1"
sysconfdir = "/Users/malenkijkotik/Desktop/monopoly/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monopoly_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monopoly_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monopoly_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monopoly_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monopoly_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monopoly_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
