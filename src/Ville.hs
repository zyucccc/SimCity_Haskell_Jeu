module Ville where

import Data.Map (Map)
import qualified Data.Map as M

import Zones (Zone, ZoneId, BatId, CitId)

import Citoyens (Citoyen)

data Ville = Ville {viZones :: Map ZoneId Zone ,viCit :: Map CitId Citoyen}

