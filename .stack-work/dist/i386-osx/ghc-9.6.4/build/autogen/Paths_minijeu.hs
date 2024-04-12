{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_minijeu (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/zyu/Desktop/Projet/SimCity_Haskell_Jeu/.stack-work/install/i386-osx/e6e4ffe77561dc405d982c20db4b9270dde1ba68171e2cba413bbc5dc8e7c817/9.6.4/bin"
libdir     = "/Users/zyu/Desktop/Projet/SimCity_Haskell_Jeu/.stack-work/install/i386-osx/e6e4ffe77561dc405d982c20db4b9270dde1ba68171e2cba413bbc5dc8e7c817/9.6.4/lib/x86_64-osx-ghc-9.6.4/minijeu-0.1.0.0-2Zsll9NdNFTL3GpCAXqChJ"
dynlibdir  = "/Users/zyu/Desktop/Projet/SimCity_Haskell_Jeu/.stack-work/install/i386-osx/e6e4ffe77561dc405d982c20db4b9270dde1ba68171e2cba413bbc5dc8e7c817/9.6.4/lib/x86_64-osx-ghc-9.6.4"
datadir    = "/Users/zyu/Desktop/Projet/SimCity_Haskell_Jeu/.stack-work/install/i386-osx/e6e4ffe77561dc405d982c20db4b9270dde1ba68171e2cba413bbc5dc8e7c817/9.6.4/share/x86_64-osx-ghc-9.6.4/minijeu-0.1.0.0"
libexecdir = "/Users/zyu/Desktop/Projet/SimCity_Haskell_Jeu/.stack-work/install/i386-osx/e6e4ffe77561dc405d982c20db4b9270dde1ba68171e2cba413bbc5dc8e7c817/9.6.4/libexec/x86_64-osx-ghc-9.6.4/minijeu-0.1.0.0"
sysconfdir = "/Users/zyu/Desktop/Projet/SimCity_Haskell_Jeu/.stack-work/install/i386-osx/e6e4ffe77561dc405d982c20db4b9270dde1ba68171e2cba413bbc5dc8e7c817/9.6.4/etc"

getBinDir     = catchIO (getEnv "minijeu_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "minijeu_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "minijeu_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "minijeu_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minijeu_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minijeu_sysconfdir") (\_ -> return sysconfdir)



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
