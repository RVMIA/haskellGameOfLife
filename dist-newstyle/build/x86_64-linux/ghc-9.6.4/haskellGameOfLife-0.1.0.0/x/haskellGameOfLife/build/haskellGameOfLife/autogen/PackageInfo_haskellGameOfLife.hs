{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskellGameOfLife (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskellGameOfLife"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Conway's Game of Life, in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
