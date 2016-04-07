module Assg3 where

-- Ex 1

data Reihe = A | B | C | D | E | F | G | H deriving (Eq, Ord, Enum, Show)

data Linie = Eins | Zwei | Drei | Vier | Fuenf | Sechs | Sieben | Acht deriving (Eq, Ord, Enum, Show)

type StartPosition = (Reihe, Linie)
type ZielPosition = (Reihe, Linie)
type AnzahlZuege = Int
type VonPosition = (Reihe, Linie)
type NachPosition = (Reihe, Linie)
type Zug = (VonPosition, NachPosition)
type Zugfolge = [Zug]

type VirtualVonPosition = (Integer, Integer)
type VirtualNachPosition = (Integer, Integer)
type VirtualZug = (VirtualVonPosition, VirtualNachPosition)
type VirtualZugfolge = [VirtualZug]

springer :: StartPosition -> AnzahlZuege -> ZielPosition -> [Zugfolge]
springer s a z = spgTransformer . spgSelektor . spgFilter . spgGenerator . transform $ (s, a, z)


transform :: (StartPosition, AnzahlZuege, ZielPosition) -> (VirtualVonPosition, AnzahlZuege, VirtualNachPosition)
transform ((sr, sl), a, (zr, zl)) = 
	let convert e = fromIntegral $ fromEnum e
	in ((convert sr, convert sl), a, (convert zr, convert zl))

spgGenerator :: (VirtualVonPosition, AnzahlZuege, VirtualNachPosition) -> [VirtualZugfolge]
spgGenerator _ = []

spgFilter :: [VirtualZugfolge] -> [VirtualZugfolge]
spgFilter _ = []

spgSelektor :: [VirtualZugfolge] -> [VirtualZugfolge]
spgSelektor _ = []

spgTransformer :: [VirtualZugfolge] -> [Zugfolge]
spgTransformer _ = []


-- Ex 2
