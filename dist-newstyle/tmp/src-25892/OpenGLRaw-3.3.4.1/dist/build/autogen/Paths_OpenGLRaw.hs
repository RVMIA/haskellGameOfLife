{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_OpenGLRaw (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [3,3,4,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ame/.cabal/store/ghc-9.4.7/OpenGLRaw-3.3.4.1-00d9e8cf7099aac9e3818f51c71733da89d824cab4d8c0b127513eb34c529eec/bin"
libdir     = "/home/ame/.cabal/store/ghc-9.4.7/OpenGLRaw-3.3.4.1-00d9e8cf7099aac9e3818f51c71733da89d824cab4d8c0b127513eb34c529eec/lib"
dynlibdir  = "/home/ame/.cabal/store/ghc-9.4.7/OpenGLRaw-3.3.4.1-00d9e8cf7099aac9e3818f51c71733da89d824cab4d8c0b127513eb34c529eec/lib"
datadir    = "/home/ame/.cabal/store/ghc-9.4.7/OpenGLRaw-3.3.4.1-00d9e8cf7099aac9e3818f51c71733da89d824cab4d8c0b127513eb34c529eec/share"
libexecdir = "/home/ame/.cabal/store/ghc-9.4.7/OpenGLRaw-3.3.4.1-00d9e8cf7099aac9e3818f51c71733da89d824cab4d8c0b127513eb34c529eec/libexec"
sysconfdir = "/home/ame/.cabal/store/ghc-9.4.7/OpenGLRaw-3.3.4.1-00d9e8cf7099aac9e3818f51c71733da89d824cab4d8c0b127513eb34c529eec/etc"

getBinDir     = catchIO (getEnv "OpenGLRaw_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "OpenGLRaw_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "OpenGLRaw_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "OpenGLRaw_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OpenGLRaw_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OpenGLRaw_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
