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
	let convert e = fromIntegral $ (fromEnum e) + 1
	in ((convert sr, convert sl), a, (convert zr, convert zl))

spgGenerator :: (VirtualVonPosition, AnzahlZuege, VirtualNachPosition) -> (VirtualNachPosition, [VirtualZugfolge])
spgGenerator (_, 0, np) = (np, [[]])
spgGenerator (vp@(vpr, vpl), a, np) = (np, moves1 ++ moves2 ++ moves3 ++ moves4 ++ moves5 ++ moves6 ++ moves7 ++ moves8)
	where
		moves1 = map ((vp, (vpr - 2, vpl + 1)):) $ snd $ spgGenerator ((vpr - 2, vpl + 1), a - 1, np)
		moves2 = map ((vp, (vpr - 1, vpl + 2)):) $ snd $ spgGenerator ((vpr - 1, vpl + 2), a - 1, np)
		moves3 = map ((vp, (vpr + 1, vpl + 2)):) $ snd $ spgGenerator ((vpr + 1, vpl + 2), a - 1, np)
		moves4 = map ((vp, (vpr + 2, vpl + 1)):) $ snd $ spgGenerator ((vpr + 2, vpl + 1), a - 1, np)
		moves5 = map ((vp, (vpr + 2, vpl - 1)):) $ snd $ spgGenerator ((vpr + 2, vpl - 1), a - 1, np)
		moves6 = map ((vp, (vpr + 1, vpl - 2)):) $ snd $ spgGenerator ((vpr + 1, vpl - 2), a - 1, np)
		moves7 = map ((vp, (vpr - 1, vpl - 2)):) $ snd $ spgGenerator ((vpr - 1, vpl - 2), a - 1, np)
		moves8 = map ((vp, (vpr - 2, vpl - 1)):) $ snd $ spgGenerator ((vpr - 2, vpl - 1), a - 1, np)

spgFilter :: (VirtualNachPosition, [VirtualZugfolge]) -> [VirtualZugfolge]
spgFilter _ = []

spgSelektor :: [VirtualZugfolge] -> [VirtualZugfolge]
spgSelektor _ = []

spgTransformer :: [VirtualZugfolge] -> [Zugfolge]
spgTransformer _ = []


-- Ex 2
