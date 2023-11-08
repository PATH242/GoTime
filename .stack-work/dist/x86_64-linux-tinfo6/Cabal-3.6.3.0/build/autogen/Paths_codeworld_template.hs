{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_codeworld_template (
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/menna242/GithubProjects/GoTime/.stack-work/install/x86_64-linux-tinfo6/905e241e03819f8dea0c2b73b42b9ebecf3464be509ff035252bae334fcc75c3/9.2.7/bin"
libdir     = "/home/menna242/GithubProjects/GoTime/.stack-work/install/x86_64-linux-tinfo6/905e241e03819f8dea0c2b73b42b9ebecf3464be509ff035252bae334fcc75c3/9.2.7/lib/x86_64-linux-ghc-9.2.7/codeworld-template-0.0.0-CsdV33OkgujI5IZ1SGBtG4"
dynlibdir  = "/home/menna242/GithubProjects/GoTime/.stack-work/install/x86_64-linux-tinfo6/905e241e03819f8dea0c2b73b42b9ebecf3464be509ff035252bae334fcc75c3/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/menna242/GithubProjects/GoTime/.stack-work/install/x86_64-linux-tinfo6/905e241e03819f8dea0c2b73b42b9ebecf3464be509ff035252bae334fcc75c3/9.2.7/share/x86_64-linux-ghc-9.2.7/codeworld-template-0.0.0"
libexecdir = "/home/menna242/GithubProjects/GoTime/.stack-work/install/x86_64-linux-tinfo6/905e241e03819f8dea0c2b73b42b9ebecf3464be509ff035252bae334fcc75c3/9.2.7/libexec/x86_64-linux-ghc-9.2.7/codeworld-template-0.0.0"
sysconfdir = "/home/menna242/GithubProjects/GoTime/.stack-work/install/x86_64-linux-tinfo6/905e241e03819f8dea0c2b73b42b9ebecf3464be509ff035252bae334fcc75c3/9.2.7/etc"

getBinDir     = catchIO (getEnv "codeworld_template_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "codeworld_template_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "codeworld_template_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "codeworld_template_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "codeworld_template_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "codeworld_template_sysconfdir") (\_ -> return sysconfdir)




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
