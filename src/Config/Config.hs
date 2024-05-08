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

largeur_ZRBouton :: CInt
largeur_ZRBouton = 140

hauteur_ZRBouton :: CInt
hauteur_ZRBouton = 65

largeur_ZIBouton :: CInt
largeur_ZIBouton = 140

hauteur_ZIBouton :: CInt
hauteur_ZIBouton = 65

largeur_ZCBouton :: CInt
largeur_ZCBouton = 140

hauteur_ZCBouton :: CInt
hauteur_ZCBouton = 65

largeur_ADBouton :: CInt
largeur_ADBouton = 140

hauteur_ADBouton :: CInt
hauteur_ADBouton = 65

largeur_routeBouton :: CInt
largeur_routeBouton = 140

hauteur_routeBouton :: CInt
hauteur_routeBouton = 65

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

largeur_route :: CInt
largeur_route = caseSize

hauteur_route :: CInt
hauteur_route = caseSize
