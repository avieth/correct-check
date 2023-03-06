module Location
  ( HasCallStack
  , CallStack
  , SrcLoc
  , MaybeSrcLoc (..)
  , srcLocOf
  , callStack
  , prettyMaybeSrcLoc
  , prettySrcLoc
  , withFrozenCallStack
  ) where

import GHC.Stack

data MaybeSrcLoc where
  UnknownLocation :: MaybeSrcLoc
  KnownLocation :: SrcLoc -> MaybeSrcLoc

prettyMaybeSrcLoc :: MaybeSrcLoc -> String
prettyMaybeSrcLoc UnknownLocation = "Unknown location"
prettyMaybeSrcLoc (KnownLocation srcLoc) = prettySrcLoc srcLoc

srcLocOf :: CallStack -> MaybeSrcLoc
srcLocOf cs = case getCallStack cs of
  [] -> UnknownLocation
  (_,srcLoc):_ -> KnownLocation srcLoc
