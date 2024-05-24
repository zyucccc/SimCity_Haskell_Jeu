{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module TestProperties where

import Test.Hspec
import Test.QuickCheck
import Maps.Zones
import Maps.Monde
import Model
import Foreign.C.Types (CInt)
import qualified Data.Map as Map
import Config.Config (caseSize)
import SDL (V2(..), Point(..))
import Maps.Formes (Coord(..), Forme(..))
import Entitys.Entitys (Batiment(..), CitId(..), Citoyen(..), Occupation(..), BatId(..))
import Entitys.Ville (Ville(..))
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

instance Arbitrary Coord where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ C x y

instance Arbitrary (Point V2 Int32) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ P (V2 x y)

instance Arbitrary Forme where
  arbitrary = oneof [Rectangle <$> arbitrary <*> arbitrary <*> arbitrary, VSegment <$> arbitrary <*> arbitrary, HSegment <$> arbitrary <*> arbitrary]

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
  arbitrary = Economic <$> arbitrary

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



-- nous verifions si la fonction updateEconomic augmente bien la quantité d'argent dans l'etat économique
prop_updateEconomicIncreasesMoney :: GameState -> Bool
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


testUpdateEconomic = do
  describe "updateEconomic" $ do
    it "increases the money in the economic state" $
      property prop_updateEconomicIncreasesMoney

testCountZC = do
  describe "countZC" $ do
    it "counts the correct number of commercial zones" $
      property prop_countZC_correct

return []

runTests :: IO Bool
runTests = $quickCheckAll
