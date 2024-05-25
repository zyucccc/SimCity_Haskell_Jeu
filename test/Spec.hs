
import Test.Hspec
import TestProperties as TP

main :: IO ()
main = hspec $ do
  --TP.testInitMonde
  TP.testUpdateEconomic
  TP.testCountZC
  TP.testGameStateInvariant
  TP.testPlaceZone
  TP.testAddZone_Ville
  TP.testBuildBatiment