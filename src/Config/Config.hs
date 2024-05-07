module Config.Config where

import Foreign.C.Types (CInt (..) )

-- constant global
--640
window_largeur :: CInt
window_largeur = 960

--480
window_hauteur :: CInt
window_hauteur = 720

-- la taille d'une case : 5*5 pixels
caseSize :: CInt
caseSize = 30

cols :: CInt
cols = window_largeur `div` caseSize

rows :: CInt
rows = window_hauteur `div` caseSize