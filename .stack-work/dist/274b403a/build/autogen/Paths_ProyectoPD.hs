{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ProyectoPD (
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

bindir     = "C:\\Users\\Usuario\\Desktop\\ProyectoPD2\\ProyectoPD\\.stack-work\\install\\9c1dd22d\\bin"
libdir     = "C:\\Users\\Usuario\\Desktop\\ProyectoPD2\\ProyectoPD\\.stack-work\\install\\9c1dd22d\\lib\\x86_64-windows-ghc-8.10.3\\ProyectoPD-0.1.0.0-IrRdcp3G0qJ7GmjgtrMZYh"
dynlibdir  = "C:\\Users\\Usuario\\Desktop\\ProyectoPD2\\ProyectoPD\\.stack-work\\install\\9c1dd22d\\lib\\x86_64-windows-ghc-8.10.3"
datadir    = "C:\\Users\\Usuario\\Desktop\\ProyectoPD2\\ProyectoPD\\.stack-work\\install\\9c1dd22d\\share\\x86_64-windows-ghc-8.10.3\\ProyectoPD-0.1.0.0"
libexecdir = "C:\\Users\\Usuario\\Desktop\\ProyectoPD2\\ProyectoPD\\.stack-work\\install\\9c1dd22d\\libexec\\x86_64-windows-ghc-8.10.3\\ProyectoPD-0.1.0.0"
sysconfdir = "C:\\Users\\Usuario\\Desktop\\ProyectoPD2\\ProyectoPD\\.stack-work\\install\\9c1dd22d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProyectoPD_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProyectoPD_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ProyectoPD_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ProyectoPD_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProyectoPD_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProyectoPD_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
