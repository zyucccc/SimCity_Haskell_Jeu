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

-- 为 GameState 提供 Arbitrary 实例
instance Arbitrary GameState where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    spd <- arbitrary
    mouseState <- arbitrary
    displayText <- arbitrary
    monde <- arbitrary
    selectedZone <- arbitrary
    ville <- arbitrary
    nextZoneId <- arbitrary
    economic <- arbitrary
    economicUpdateTime <- arbitrary
    return $ GameState x y spd mouseState displayText monde selectedZone ville nextZoneId economic economicUpdateTime

-- 为 Coord 提供 Arbitrary 实例
instance Arbitrary Coord where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ C x y

-- 启用 FlexibleInstances 扩展后，为 Point V2 Int32 提供 Arbitrary 实例
instance Arbitrary (Point V2 Int32) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ P (V2 x y)

-- 为 Forme 提供 Arbitrary 实例
instance Arbitrary Forme where
  arbitrary = oneof [Rectangle <$> arbitrary <*> arbitrary <*> arbitrary, VSegment <$> arbitrary <*> arbitrary, HSegment <$> arbitrary <*> arbitrary]

-- 为 Batiment 提供 Arbitrary 实例
instance Arbitrary Batiment where
  arbitrary = oneof [Cabane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary, Atelier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary, Epicerie <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary, Commissariat <$> arbitrary <*> arbitrary]

-- 为 Direction_Route 提供 Arbitrary 实例
instance Arbitrary Direction_Route where
  arbitrary = elements [Horizontal, Vertical]

-- 为 Zone 提供 Arbitrary 实例
instance Arbitrary Zone where
  arbitrary = oneof [ZR <$> arbitrary <*> arbitrary, ZI <$> arbitrary <*> arbitrary, ZC <$> arbitrary <*> arbitrary, Admin <$> arbitrary <*> arbitrary, Route <$> arbitrary <*> arbitrary, Eau <$> arbitrary, Grass <$> arbitrary, Terre <$> arbitrary]

-- 为 ZoneType 提供 Arbitrary 实例
instance Arbitrary ZoneType where
  arbitrary = elements [ZRType, ZIType, ZCType, AdminType, RouteType_Horizontal, RouteType_Vertical, EauType, GrassType, TerreType]

-- 为 Ville 提供 Arbitrary 实例
instance Arbitrary Ville where
  arbitrary = Ville <$> arbitrary <*> arbitrary

-- 为 ZoneId 提供 Arbitrary 实例
instance Arbitrary ZoneId where
  arbitrary = ZoneId <$> arbitrary

-- 为 Economic 提供 Arbitrary 实例
instance Arbitrary Economic where
  arbitrary = Economic <$> arbitrary

-- 为 CitId 提供 Arbitrary 实例
instance Arbitrary CitId where
  arbitrary = CitId <$> arbitrary

-- 为 CitId 提供 Eq 和 Ord 实例
instance Eq CitId where
  (CitId id1) == (CitId id2) = id1 == id2

instance Ord CitId where
  compare (CitId id1) (CitId id2) = compare id1 id2

-- 为 BatId 提供 Arbitrary 实例
instance Arbitrary BatId where
  arbitrary = BatId <$> arbitrary

-- 为 Occupation 提供 Arbitrary 实例
instance Arbitrary Occupation where
  arbitrary = O <$> arbitrary

-- 为 Citoyen 提供 Arbitrary 实例
instance Arbitrary Citoyen where
  arbitrary = oneof [ Immigrant <$> arbitrary <*> arbitrary <*> arbitrary
                    , Habitant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , Emigrant <$> arbitrary <*> arbitrary
                    ]

-- 确保世界初始化后有正确数量的地块
--prop_initMonde_correctSize :: CInt -> CInt -> Property
--prop_initMonde_correctSize width height =
--  width > 3 && height > 3 ==> ioProperty $ do
--    monde <- initMonde width height
--    let cols = width `div` caseSize
--    let rows = height `div` caseSize
--    return $ Map.size monde == fromIntegral (cols * rows)


-- 确保更新经济状态后，钱数增加一致
prop_updateEconomicIncreasesMoney :: GameState -> Bool
prop_updateEconomicIncreasesMoney gs =
  let initialMoney = money $ economic gs
      updatedGs = updateEconomic gs
      updatedMoney = money $ economic updatedGs
      zcCount = countZC (monde gs)
      nbZc = fromIntegral zcCount * 10
  in updatedMoney == initialMoney + nbZc

-- 确保创建的商业区占据正确数量的地块
prop_countZC_correct :: Monde -> Bool
prop_countZC_correct monde =
  let zcCount = countZC monde
      expectedCount = length (filter isZC (Map.elems monde)) `div` 4
  in zcCount == expectedCount
  where
    isZC (Just (ZC _ _)) = True
    isZC _ = False

-- 定义测试规范
--testInitMonde = do
--  describe "initMonde" $ do
--    it "creates a world with the correct number of cells" $
--      property prop_initMonde_correctSize

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
