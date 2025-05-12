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

pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

jamesBond   = Jugador "Bond... James Bond" pokerDeAses "Martini... shaken, not stirred"
leChiffre   = Jugador "Le Chiffre" fullDeJokers "Gin"
felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"

mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

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

repeticionesEnMano :: Number -> Juego
repeticionesEnMano repeticiones cartas = 
  any ((== repeticiones). flip ocurrenciasDe numerosEnMano) numerosEnMano
  where numerosEnMano = map numero cartas

--3.a 
par = repeticionesEnMano 2 

--3.b
pierna = repeticionesEnMano 3

--3.c
color cartas = all ((==(palo.head $ cartas)).palo) cartas
color' cartas = all (\ carta -> palo carta == (palo.head $ cartas)) cartas
color'' cartas = all (\ carta -> palo carta == palo (head cartas)) cartas
color''' cartas = all ((==paloPrimeraCarta).palo) cartas
  where paloPrimeraCarta = palo.head $ cartas

--3.d
fullHouse cartas = par cartas && pierna cartas
fullHouse' cartas = all ($ cartas) [par,pierna] -- This is the way (?)
               -- all (\ juego -> juego cartas) [par,pierna]
fullHouse'' cartas = flip all [par,pierna] ($ cartas)
fullHouse''' = flip all [par,pierna] . (\ cartas -> ($ cartas) )
fullHouse'''' = flip all [par,pierna] . flip ($) -- y la expresividad?
-- ($) f v = f v
-- f $ v = f v

--3.e
poker = repeticionesEnMano 4

--3.f
type Juego = [Carta] -> Bool
otro :: Juego
otro _ = True
otro' = const True


--4 
type Mesa = [Jugador]
-- alguienSeCarteo :: Mesa -> Bool
alguienSeCarteo mesa = todasLasCartas /= sinRepetidos todasLasCartas
  where todasLasCartas = concat . map mano $ mesa

--5
valores :: [(Juego, Number)]
valores = [(par,1), (pierna,2), (color,3), (fullHouse,4), (poker,5), (otro, 0)]
--5.a
valorJuego = snd
--valor :: [Carta] -> Number
valor cartas = 
  valorJuego . maximoSegun valorJuego $ (filter (($ cartas).juego) valores)
  where juego = fst
  -- maximum . map valorJuego $ (filter (($ cartas).juego) valores)
--5.b
bebidaWinner mesa = 
 bebida. maximoSegun (valor.mano) .filter (not.manoMalArmada) $ mesa

--6
--6.a
-- nombre . maximoSegun (length.bebida) $ mesa

--6.b
-- maximoSegun (length.filter esoNoSeVale. mano) $ mesa

--6.c
-- maximoSegun (negate.length.nombre) $ mesa

--6.d
-- nombre. maximoSegun (valor.mano) $ mesa

--7.a
-- ordenar (<) [5, 7, 2, 4, 6]
--ordenar :: Ord a => (a -> a -> Bool) -> [a] -> [a]
ordenar _ [] = []
ordenar criterio (x:xs) = 
    (ordenar criterio . filter (not. criterio x) $ xs) ++ [x] ++ 
    (ordenar criterio . filter (criterio x) $ xs)

menorAMayor lista = ordenar (<) lista
-- 7.b.i
escalera cartas = 
  all (\(p,s)-> s - p == 1 ) . zip numerosOrdenados $ (tail numerosOrdenados)
    where numerosOrdenados = menorAMayor $ map numero cartas

escalera' cartas = 
  and . zipWith (\ p s -> s - p == 1 ) numerosOrdenados $ (tail numerosOrdenados)
    where numerosOrdenados = menorAMayor . map numero $ cartas

-- 7.b.ii
escaleraDeColor cartas = all ($ cartas) [escalera, color]

suma = \x -> \y -> x + y


