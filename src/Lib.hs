module Lib where
import Text.Show.Functions

-- De los personajes necesitamos saber, en primer lugar, cuánta vida tienen y cómo está compuesta su armadura, para poder así calcular su poder de defensa: es su vida más la sumatoria de la defensa que aporta cada una las partes de su armadura que no estén rotas (De cada parte se conoce la defensa que aporta y su durabilidad; se asume que si la durabilidad es 0 significa que está  roto).

-- calcular el poder de defensa de un personaje
data Personaje = UnPersonaje {
  vida :: Int,
  arma :: Arma,
  armadura :: [Parte]
} deriving (Show)

restarVida :: Int -> Personaje -> Personaje
restarVida cantidad personaje = personaje { vida = vida personaje - cantidad }

metalizarArma :: Personaje -> Personaje
metalizarArma personaje = personaje { arma = metalizar (arma personaje) }

modificarArmadura :: (Parte -> Parte) -> Personaje -> Personaje 
modificarArmadura f personaje = personaje { armadura = map f (armadura personaje) }


type Parte = (Defensa, Durabildad)
type Defensa = Int
type Durabildad = Int

defensa :: Parte -> Int
defensa = fst
durabilidad :: Parte -> Int
durabilidad = snd


aumentarDefensaPorcentual :: Int -> Parte -> Parte
-- aumentarDefensaPorcentual porcentaje (defensa, durabilidad) = (defensa + porcentaje * defensa `div` 100, durabilidad)
aumentarDefensaPorcentual porcentaje = modificarDefensa (\defensa -> defensa + porcentaje * defensa `div` 100)

aumentarDefensa :: Int -> Parte -> Parte
-- aumentarDefensa cantidad (defensa, durabilidad) = (defensa + cantidad, durabilidad)
aumentarDefensa cantidad = modificarDefensa (+ cantidad)

-- Como también hay lógica repetida en las funciones anteriores, también
-- podemos abstraer:
modificarDefensa :: (Defensa -> Defensa) -> Parte -> Parte
modificarDefensa f (defensa, durabilidad) = (f defensa, durabilidad)  -- Con tuplas
-- modificarDefensa f parte = parte { defensa = f (defensa parte)}    -- Con data


restarDurabilidad :: Int -> Parte -> Parte
restarDurabilidad cantidad (defensa, durabilidad) = (defensa, (durabilidad - cantidad) `max` 0)


-- 1)

poderDefensa :: Personaje -> Int
poderDefensa personaje = vida personaje + defensaArmadura personaje

defensaArmadura :: Personaje -> Int
defensaArmadura = sum . map defensa . filter sirve . armadura

sirve :: Parte -> Bool
sirve = not . estaRota

estaRota :: Parte -> Bool
estaRota = (==0) . durabilidad


-- 2)
-- De cada personaje también nos interesa saber cuál es el arma que porta, que puede ser un báculo, un arco o una espada, para poder así obtener su poder de ataque:
-- Si el arma es un báculo, es la inteligencia del báculo más la longitud del nombre del arma.
-- En caso de ser un arco, se calcula como el rango máximo que alcanza el arco por la longitud del hilo del mismo, sumándole luego el daño base que tiene.
-- Si se trata de una espada, es la cantidad de almas cosechadas por la espada, multiplicado por un coeficiente según el material forjado. Si es de madera el coeficiente es 2, si es de metal es 3 y en otro caso el coeficiente es neutro.

data Arma =
  Baculo { inteligencia :: Int, nombre :: String } |
  Arco   { rangoMaximo :: Int, longitudHilo :: Int, danioBase :: Int } |
  Espada { cantAlmas :: Int, material :: Material }
  deriving (Show)

data Material = Madera | Metal | Otro deriving (Eq, Show)


poderAtaque :: Personaje -> Int
poderAtaque = nivelAtaque . arma

nivelAtaque :: Arma -> Int
nivelAtaque (Baculo inteligencia nombre) = inteligencia + length nombre
nivelAtaque (Arco rango hilo danio) = rango * hilo + danio
nivelAtaque (Espada almas material) = almas * coeficienteForjado material

coeficienteForjado :: Material -> Int
coeficienteForjado Madera = 2
coeficienteForjado Metal = 3
coeficienteForjado _ = 1


-- 3)
type Buff = Personaje -> Personaje

-- Frenesí: Aumenta la defensa de cada pieza de armadura en un 20%.
frenesi :: Buff
-- frenesi personaje = personaje { armadura = map (aumentarDefensaPorcentual 20) (armadura personaje) }
frenesi = modificarArmadura (aumentarDefensaPorcentual 20)

-- Manto etéreo: le suma 3 a la defensa de cada pieza de la armadura, a cambio le resta al personaje 100 de vida.
mantoEtereo :: Buff
-- mantoEtereo personaje = personaje { armadura = map (aumentarDefensa 3) (armadura personaje) }
-- otra versión de manto etereo (que no está buena ahora que tenemos la abstracción modifcarArmadura):
-- {vida = (vida personaje)- 100, armadura = mejorarArmadura (armadura personaje) sumar3 }
mantoEtereo = restarVida 100 . modificarArmadura (aumentarDefensa 3)

-- Berserker: Deja la defensa de cada pieza de la armadura en 2, a la vez que transforma en metal el arma preferida del personaje, en caso que fuera de madera (si no, queda igual). 
berserker :: Buff
berserker = metalizarArma . modificarArmadura (modificarDefensa (\_ -> 2))

-- Espejo de karma: Es un buff que hace que se realice dos veces la acción de otro buff indicado.
espejoKarma :: Buff -> Buff
espejoKarma buff = buff . buff

-- Sucesión de buffs inesperados: Es un buff que hace que se realice la acción de una serie de buffs  indicados. 
sucesionBuffs :: [Buff] -> Buff
sucesionBuffs buffs personaje = foldl potenciar personaje buffs

-- Otra forma de hacerlo (pensando en que construyo un Buff, y no un Personaje)
sucesionBuffsV2 buffs = foldl1 (.) buffs

-- > sucesionBuffs' [frenesi, mantoEtereo, berserker]
-- frenesi . mantoEtereo . berserker



potenciar :: Personaje -> Buff -> Personaje
potenciar personaje buff = buff personaje

esInofensivo :: [Personaje] -> Buff -> Bool
-- esInofensivo personajes buff = not . any (cambie)
esInofensivo personajes buff = all (not . cambioSkills buff) personajes

-- Detectar si un buff es inofensivo, lo cual significa que al utilizarlo con un grupo de personajes de prueba, no altera el poder de ataque ni el de defensa de ninguno de ellos.
cambioSkills :: Buff -> Personaje -> Bool
cambioSkills buff personaje = 
  mismoAtaque personaje (potenciar personaje buff) && mismaDefensa personaje (potenciar personaje buff)


mismoAtaque = mismoSkill poderAtaque
mismaDefensa = mismoSkill poderDefensa

mismoSkill f personaje1 personaje2 = f personaje1 == f personaje2


metalizar :: Arma -> Arma
metalizar (Espada almas Madera) = Espada almas Metal
metalizar arma = arma



-- 4)
desgastar :: Int -> Personaje -> Personaje
desgastar intensidad persona = persona { armadura = gastarArmadura intensidad (armadura persona) }

gastarArmadura :: Int -> [Parte] -> [Parte]
gastarArmadura _ [] = [] -- caso base
gastarArmadura intensidad (parte : partes) = restarDurabilidad intensidad parte : gastarArmadura (intensidad `div` 2) partes



-- 5)
data Clan = UnClan {
  miembros :: [Personaje],
  buffs :: [Buff]
}


leGana :: Clan -> Clan -> Bool
leGana clanAgresor clanDefensor = sumatoriaSegun poderAtaque clanAgresor > sumatoriaSegun poderDefensa clanDefensor

-- TODO: Abstraer lógica repetida
-- sumatoriaAtaque :: Clan -> Int
-- sumatoriaAtaque = sum . map poderAtaque . miembrosBuffadosSegun poderAtaque
-- sumatorioDefensa :: Clan -> Int
-- sumatorioDefensa = sum . map poderDefensa . miembrosBuffadosSegun poderDefensa

sumatoriaSegun f = sum . map f . miembrosBuffadosSegun f


-- miembrosBuffadosAtaque :: Clan -> [Personaje]
-- miembrosBuffadosAtaque = miembrosBuffadosSegun poderAtaque
-- miembrosBuffadosDefensa :: Clan -> [Personaje]
-- miembrosBuffadosDefensa = miembrosBuffadosSegun poderDefensa

miembrosBuffadosSegun :: (Personaje -> Int) -> Clan -> [Personaje]
miembrosBuffadosSegun criterio clan = map (buffearConMaximoCriterio criterio (buffs clan)) (miembros clan)


buffearConMaximoCriterio :: (Personaje -> Int) -> [Buff] -> Personaje -> Personaje
buffearConMaximoCriterio criterioDePersonaje buffs personaje = potenciar personaje buffMaximoAtaque
  where buffMaximoAtaque = foldl1 (maxDePotenciar criterioDePersonaje personaje) buffs

-- Estas funciones ya no tienen sentido, porque usar la función genérica me da la misma
-- declaratividad y expresividad.
-- maxAtaque :: Personaje -> Buff -> Buff -> Buff
-- maxAtaque = maxDePotenciar poderAtaque
-- maxDefensa :: Personaje -> Buff -> Buff -> Buff
-- maxDefensa = maxDePotenciar poderDefensa

maxDePotenciar criterioDePersonaje personaje = maxSegun (criterioDePersonaje . potenciar personaje)


maxSegun f buff1 buff2
  | f buff1 > f buff2 = buff1
  | otherwise = buff2


personajeGenerico = UnPersonaje {
  vida = 100,
  arma = Espada { cantAlmas = 20, material = Madera },
  armadura = [(10, 10), (20, 0)]
}


personajeDefensivo = UnPersonaje {
  vida = 100,
  arma = Espada { cantAlmas = 20, material = Madera },
  armadura = replicate 10 (10, 10)
}


personajeInfinitoDefensivo = UnPersonaje {
  vida = 100,
  arma = Espada { cantAlmas = 20, material = Madera },
  armadura = repeat (10, 10)
}