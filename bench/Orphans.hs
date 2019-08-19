{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import ClassyPrelude hiding (lookup)

import Template      (Template)
import World         (ParsedWorld, World)




instance NFData Template
instance NFData ParsedWorld
instance NFData World
