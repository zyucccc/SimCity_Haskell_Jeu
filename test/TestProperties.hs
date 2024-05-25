{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module TestProperties where

import Test.Hspec
import Test.QuickCheck
import Maps.Zones
import Maps.Monde
import Config.Config
import  Maps.Formes
import Model
import Foreign.C.Types (CInt)
import qualified Data.Map as Map
import SDL (V2(..), Point(..))
import Entitys.Entitys (Batiment(..), CitId(..), Citoyen(..), Occupation(..), BatId(..))
import Entitys.Ville
import GHC.Int (Int32)

instance Arbitrary GameState where
  arbitrary = do
    mouseState <- arbitrary
    displayText <- arbitrary
    monde <- arbitrary
    selectedZone <- arbitrary
    ville <- arbitrary
    nextZoneId <- arbitrary
    economic <- arbitrary
    economicUpdateTime <- arbitrary
    return $ GameState mouseState displayText monde selectedZone ville nextZoneId economic economicUpdateTime

--les coords generes doivent etre dans la carte
instance Arbitrary Coord where
  arbitrary = do
    x <- choose (0, window_largeur)
    y <- choose (0, window_hauteur)
    return $ C x y

instance Arbitrary (Point V2 Int32) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ P (V2 x y)

-- les coord,taille de forme generes doivent etre dans la carte
instance Arbitrary Forme where
  arbitrary = oneof [ Rectangle <$> arbitrary <*> (choose (0, window_hauteur)) <*> (choose (0, window_hauteur))
                    , VSegment <$> arbitrary <*> (choose (0, window_hauteur))
                    , HSegment <$> arbitrary <*> (choose (0, window_hauteur))
                    ]

instance Arbitrary Batiment where
  arbitrary = oneof [Cabane <$> arbitrary <*> arbitrary <*> arbitrary, Atelier <$> arbitrary <*> arbitrary <*> arbitrary, Epicerie <$> arbitrary <*> arbitrary <*> arbitrary, Commissariat <$> arbitrary]

instance Arbitrary Direction_Route where
  arbitrary = elements [Horizontal, Vertical]

instance Arbitrary Zone where
  arbitrary = oneof [ZR <$> arbitrary <*> arbitrary, ZI <$> arbitrary <*> arbitrary, ZC <$> arbitrary <*> arbitrary, Admin <$> arbitrary <*> arbitrary, Route <$> arbitrary <*> arbitrary, Eau <$> arbitrary, Grass <$> arbitrary, Terre <$> arbitrary, Centrale <$> arbitrary, Cable <$> arbitrary]

instance Arbitrary ZoneType where
  arbitrary = elements [ZRType, ZIType, ZCType, AdminType, RouteType_Horizontal, RouteType_Vertical, EauType, GrassType, TerreType, CentraleType, CableType]

instance Arbitrary Ville where
  arbitrary = Ville <$> arbitrary <*> arbitrary

instance Arbitrary ZoneId where
  arbitrary = ZoneId <$> arbitrary

instance Arbitrary Economic where
  arbitrary = (Economic <$> arbitrary) `suchThat` (\(Economic money) -> money >= 0)

instance Arbitrary CitId where
  arbitrary = CitId <$> arbitrary

instance Arbitrary BatId where
  arbitrary = BatId <$> arbitrary

instance Arbitrary Occupation where
  arbitrary = O <$> arbitrary

instance Arbitrary Citoyen where
  arbitrary = oneof [ Immigrant <$> arbitrary <*> arbitrary <*> arbitrary
                    , Habitant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Emigrant <$> arbitrary <*> arbitrary
                    ]

-- nous testons le invariant de GameState
prop_gameStateInvariant :: GameState -> Bool
prop_gameStateInvariant gs = inv_GameState gs

-- nous verifions si la fonction updateEconomic augmente bien la quantité d'argent dans l'etat économique
prop_updateEconomicIncreasesMoney :: GameState -> Bool
--on verifie si la quantité d'argent augmente bien
prop_updateEconomicIncreasesMoney gs =
  let initialMoney = money $ economic gs
      updatedGs = updateEconomic gs
      updatedMoney = money $ economic updatedGs
      zcCount = countZC (monde gs)
      nbZc = fromIntegral zcCount * 10
  in updatedMoney == initialMoney + nbZc

-- nous verifions si la fonction countZC compte bien le nombre de zones commerciales
prop_countZC_correct :: Monde -> Bool
prop_countZC_correct monde =
  let zcCount = countZC monde
      expectedCount = length (filter isZC (Map.elems monde)) `div` 4
  in zcCount == expectedCount
  where
    isZC (Just (ZC _ _)) = True
    isZC _ = False


-- nous testons la fonction placeZone
prop_placeZone :: Zone -> Bool
prop_placeZone zone =
  let newMonde = placeZone zone (initMondePure window_largeur window_hauteur)
      coord = zoneCoord zone
  in
    -- on verifie si la zone est bien placée
    case Map.lookup (coordToRowCol coord) newMonde of
      Just _ -> True
      _ -> False

-- test pour addZone_Ville
prop_addZone_Ville :: ZoneId -> Zone -> Ville -> Bool
prop_addZone_Ville zid z ville =
  let newVille = addZone_Ville zid z ville
  -- on verifie si la zone est bien ajoutée
  in case Map.lookup zid (viZones newVille) of
    Just _ -> True
    Nothing -> False

-- test buildBatiment
prop_buildBatiment :: Zone -> Batiment -> Bool
prop_buildBatiment zone bat
-- si c'est une zone de ZR, ZI, ZC, on verifie si le batiment est bien construit
  | isZR zone || isZC zone || isZI zone =
    let newZone = buildBatiment zone bat
    in case newZone of
      ZR _ (b:_) -> b == bat
      ZI _ (b:_) -> b == bat
      ZC _ (b:_) -> b == bat
      _ -> False
      -- si quickcheck genere d'autre type de zone, on renvoie true directement
  | otherwise = True
  where
    isZR (ZR _ _) = True
    isZR _ = False
    isZC (ZC _ _) = True
    isZC _ = False
    isZI (ZI _ _) = True
    isZI _ = False

testGameStateInvariant = do
  describe "GameState invariant" $ do
    it "is preserved by the GameState type" $
      property prop_gameStateInvariant

testUpdateEconomic = do
  describe "updateEconomic" $ do
    it "increases the money in the economic state" $
      property prop_updateEconomicIncreasesMoney

testCountZC = do
  describe "countZC" $ do
    it "counts the correct number of commercial zones" $
      property prop_countZC_correct

testPlaceZone = do
  describe "placeZone" $ do
    it "places a zone in the map" $
      property prop_placeZone

testAddZone_Ville = do
  describe "addZone_Ville" $ do
    it "add a zone to the ville" $
      property prop_addZone_Ville

testBuildBatiment = do
  describe "buildBatiment" $ do
    it "builds a batiment in a zone" $
      property prop_buildBatiment

return []

runTests :: IO Bool
runTests = $quickCheckAll
