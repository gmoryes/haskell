{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_demo_space_junk (
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

bindir     = "/Users/nadya73/Documents/university/haskell/monopoly_git/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/bin"
libdir     = "/Users/nadya73/Documents/university/haskell/monopoly_git/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/lib/x86_64-osx-ghc-8.2.2/demo-space-junk-0.1-HfePqCa4RzZA9RYPINVpCh"
dynlibdir  = "/Users/nadya73/Documents/university/haskell/monopoly_git/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/nadya73/Documents/university/haskell/monopoly_git/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/share/x86_64-osx-ghc-8.2.2/demo-space-junk-0.1"
libexecdir = "/Users/nadya73/Documents/university/haskell/monopoly_git/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/libexec/x86_64-osx-ghc-8.2.2/demo-space-junk-0.1"
sysconfdir = "/Users/nadya73/Documents/university/haskell/monopoly_git/haskell/.stack-work/install/x86_64-osx/lts-10.5/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "demo_space_junk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "demo_space_junk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "demo_space_junk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "demo_space_junk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "demo_space_junk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "demo_space_junk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
