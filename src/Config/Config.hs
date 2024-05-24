module Config.Config where

import Foreign.C.Types (CInt (..) )

-- constant global
--640
--960
window_largeur :: CInt
window_largeur = 1920

--480
window_hauteur :: CInt
window_hauteur = 1080

-- la taille d'une case : 30*30 pixels
caseSize :: CInt
caseSize = 30

cols :: CInt
cols = window_largeur `div` caseSize

rows :: CInt
rows = window_hauteur `div` caseSize

-- Toolbox
position_Toolbox_x :: CInt
position_Toolbox_x = 1600

position_Toolbox_y :: CInt
position_Toolbox_y = 100

largeur_Toolbox :: CInt
largeur_Toolbox = 300

hauteur_Toolbox :: CInt
hauteur_Toolbox = 350

position_ZRBouton_x :: CInt
position_ZRBouton_x = 1610

position_ZRBouton_y :: CInt
position_ZRBouton_y = 150

position_ZIBouton_x :: CInt
position_ZIBouton_x = 1610

position_ZIBouton_y :: CInt
position_ZIBouton_y = 230

position_ZCBouton_x :: CInt
position_ZCBouton_x = 1610

position_ZCBouton_y :: CInt
position_ZCBouton_y = 310

position_centrale_x :: CInt
position_centrale_x = 1610

position_centrale_y :: CInt
position_centrale_y = 380

position_cable_x :: CInt
position_cable_x = 1755

position_cable_y :: CInt
position_cable_y = 380

position_ADBouton_x :: CInt
position_ADBouton_x = 1755

position_ADBouton_y :: CInt
position_ADBouton_y = 150

position_routeBoutonVerticale_x :: CInt
position_routeBoutonVerticale_x = 1755

position_routeBoutonVerticale_y :: CInt
position_routeBoutonVerticale_y = 230

position_routeBoutonHoriz_x :: CInt
position_routeBoutonHoriz_x = 1755

position_routeBoutonHoriz_y :: CInt
position_routeBoutonHoriz_y = 310

largeur_Bouton :: CInt
largeur_Bouton = 140

hauteur_Bouton :: CInt
hauteur_Bouton = 65

--- Batiment
largeur_ZR :: CInt
largeur_ZR = caseSize

hauteur_ZR :: CInt
hauteur_ZR = caseSize

largeur_ZI :: CInt
largeur_ZI = caseSize

hauteur_ZI :: CInt
hauteur_ZI = caseSize

largeur_ZC :: CInt
largeur_ZC = caseSize

hauteur_ZC :: CInt
hauteur_ZC = caseSize

largeur_Admin :: CInt
largeur_Admin = caseSize

hauteur_Admin :: CInt
hauteur_Admin = caseSize

largeur_Centrale :: CInt
largeur_Centrale = caseSize

hauteur_Centrale :: CInt
hauteur_Centrale = caseSize

largeur_route :: CInt
largeur_route = caseSize

hauteur_route :: CInt
hauteur_route = caseSize

largeur_cable :: CInt
largeur_cable = caseSize

hauteur_cable :: CInt
hauteur_cable = caseSize
