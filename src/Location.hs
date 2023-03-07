module Location
  ( HasCallStack
  , CallStack
  , SrcLoc
  , MaybeSrcLoc (..)
  , srcLocOf
  , callStack
  , showMaybeSrcLoc
  , prettyMaybeSrcLoc
  , prettySrcLoc
  , withFrozenCallStack
  ) where

import GHC.Stack
import Data.String (fromString)
import Prettyprinter (Doc)

data MaybeSrcLoc where
  UnknownLocation :: MaybeSrcLoc
  KnownLocation :: SrcLoc -> MaybeSrcLoc

instance Show MaybeSrcLoc where
  show = showMaybeSrcLoc

showMaybeSrcLoc :: MaybeSrcLoc -> String
showMaybeSrcLoc UnknownLocation = "Unknown location"
showMaybeSrcLoc (KnownLocation srcLoc) = prettySrcLoc srcLoc

prettyMaybeSrcLoc :: MaybeSrcLoc -> Doc ann
prettyMaybeSrcLoc = fromString . showMaybeSrcLoc

srcLocOf :: CallStack -> MaybeSrcLoc
srcLocOf cs = case getCallStack cs of
  [] -> UnknownLocation
  (_,srcLoc):_ -> KnownLocation srcLoc
