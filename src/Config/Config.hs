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

largeur_ZRBouton :: CInt
largeur_ZRBouton = 140

hauteur_ZRBouton :: CInt
hauteur_ZRBouton = 65

--- Batiment
largeur_ZR :: CInt
largeur_ZR = 2*caseSize

hauteur_ZR :: CInt
hauteur_ZR = 2*caseSize