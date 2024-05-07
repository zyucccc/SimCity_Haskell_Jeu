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

-- la taille d'une case : 5*5 pixels
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

position_CabaneBouton_x :: CInt
position_CabaneBouton_x = 1650

position_CabaneBouton_y :: CInt
position_CabaneBouton_y = 150

largeur_CabaneBouton :: CInt
largeur_CabaneBouton = 200

hauteur_CabaneBouton :: CInt
hauteur_CabaneBouton = 50