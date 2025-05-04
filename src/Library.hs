module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Carta = (Number, String)
-- data Palo = Corazones | Picas | Treboles | Diamantes -- no necesita validación!

data Jugador = Jugador {
    nombre :: String,
    mano :: [Carta],
    bebida :: String
} deriving Show



-- mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

ocurrenciasDe x = length . filter (== x)
concatenar = foldl (++) []


-- 1
-- 1.a
mayorSegun f v1 v2
  | f v1 > f v2 = v1
  | otherwise   = v2

-- 1.b
maximoSegun f lista = foldl1 (mayorSegun f) lista
-- maximoSegun que no se rompe, se cursa TADP

-- 1.c
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (x/=) xs)

-- [1,2,1,2] -> [1,2] y no [2,1]


--2 
--2.a
esoSeVale (n, p) = elem n [1..13] && elem p palos
esoNoSeVale = not.esoSeVale

--2.b
manoMalArmada jugador = ((/=5) . length . mano) jugador || any esoNoSeVale (mano jugador)

--3
numero = fst
palo = snd

repeticionesEnMano :: Number -> [Carta] -> Bool
repeticionesEnMano repeticiones cartas = any ((== repeticiones). flip ocurrenciasDe numerosEnMano) numerosEnMano
  where numerosEnMano = map numero cartas

--3.a 
par = repeticionesEnMano 2

--3.b
pierna = repeticionesEnMano 3

--3.c
-- color

--3.d
-- fullHouse

--3.e
poker = repeticionesEnMano 4

