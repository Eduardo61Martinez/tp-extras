data Ingrediente = Salsa | Queso| Jamon | Aceitunas Int
     deriving Show

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
    
--eje 1.1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pizza) =  1 + cantidadDeCapas pizza 

--eje 1.2
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--eje 1.3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza  = Prepizza
sacarJamon (Capa ing p) = if  esJamon ing
    then Capa Salsa (sacarJamon p)
    else Capa ing (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

--eje 1.4
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = esQuesoOSalsa  (ing) && tieneSoloSalsaYQueso p 


esQuesoOSalsa:: Ingrediente -> Bool 
esQuesoOSalsa ing = esQueso ing || esSalsa ing 

esQueso :: Ingrediente -> Bool 
esQueso Queso = True
esQueso _ = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

--eje 1.5
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = if esAceitunas ing
    then Capa  (duplicar_ ing) (duplicarAceitunas p)
    else Capa  ing  (duplicarAceitunas p)

esAceitunas :: Ingrediente -> Bool
esAceitunas (Aceitunas _ ) = True
esAceitunas _ = False

duplicar_ :: Ingrediente -> Ingrediente 
duplicar_ (Aceitunas n) = Aceitunas (2*n) 


{--p1 = Capa Salsa ( Capa Queso (Capa Jamon Prepizza))

p2 = Capa Salsa ( Capa (Aceitunas 1) (Capa Salsa (Capa (Aceitunas 10) Prepizza)))

p3 = Capa Salsa ( Capa Jamon (Capa Queso Prepizza)) 
lista1 = [p1,p2, p3]
--}

--eje 1.6
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs 

-------------------------------------Mapa del tesoro ----------------------------------------------------

-- eje 2.1 
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c1) = hayTesoroEnCofre_ c1
hayTesoro (Bifurcacion c1 m1 m2) = hayTesoroEnCofre_ c1 || hayTesoro m1 || hayTesoro m2 

hayTesoroEnCofre_:: Cofre -> Bool
hayTesoroEnCofre_ (Cofre lObjetos)= hayTesoroEn_ lObjetos

hayTesoroEn_ :: [Objeto] -> Bool
hayTesoroEn_ [] = False
hayTesoroEn_ (x:xs) = esTesoro x || hayTesoroEn_ xs

esTesoro :: Objeto -> Bool
esTesoro  Tesoro = True
esTesoro _ = False

--eje2.2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []  (Fin cofreFinal) = hayTesoroEnCofre_ (cofreFinal)
hayTesoroEn (x:xs) (Bifurcacion _ mapa1 mapa2) = if esDerecha x
                    then  hayTesoroEn xs mapa2
                    else  hayTesoroEn xs mapa1

esDerecha:: Dir -> Bool 
esDerecha Der = True
esDerecha _ = False


--eje2.3s
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofreFinal) = []
caminoAlTesoro (Bifurcacion cofre mapa1 mapa2) = if hayTesoroEnCofre_ cofre
                                                then  []
                                                else if hayTesoro mapa1
                                                    then [Izq]  ++ caminoAlTesoro mapa1 
                                                    else [Der] ++ caminoAlTesoro mapa2

-- eje 2.4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- Observación : En la última dirección de la lista que regresa devuelve cualquier Dirección porque para que termine un mapa debe terminar sus dos caminos en fin fin.
caminoDeLaRamaMasLarga (Fin c1) = []
caminoDeLaRamaMasLarga (Bifurcacion c1 map1 map2 ) = if esMasLarga map1 map2 
    then Izq : elegirLaMasLarga (caminoDeLaRamaMasLarga map1) (caminoDeLaRamaMasLarga map2 )
    else Der : elegirLaMasLarga (caminoDeLaRamaMasLarga map1) (caminoDeLaRamaMasLarga map2 )

esMasLarga :: Mapa -> Mapa -> Bool
esMasLarga m1 m2  = cantidadDeCaminos m1 > cantidadDeCaminos m2 

cantidadDeCaminos :: Mapa -> Int 
cantidadDeCaminos (Fin _) = 0 
cantidadDeCaminos (Bifurcacion _ camino1 camino2) = 1 + cantidadDeCaminos camino1 + cantidadDeCaminos camino2  

elegirLaMasLarga :: [a] -> [a] -> [a]
elegirLaMasLarga os1 os2 = if length os1 > length os2
                            then os1
                            else os2
                                                
--eje 2.5 
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c1) = [tesorosDeCofre c1]
tesorosPorNivel (Bifurcacion c1 camino1 camino2)= [tesorosDeCofre c1 ]++ juntarListaPorLevel (tesorosPorNivel camino1)  (tesorosPorNivel camino2)

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre ls ) = tesorosDeLista ls 

tesorosDeLista :: [Objeto] -> [Objeto]
tesorosDeLista [] = []
tesorosDeLista (x:xs) = singular_Si_ x (esTesoro x)  ++ tesorosDeLista xs 

singular_Si_ :: a -> Bool -> [a]
singular_Si_ x True = [x]
singular_Si_ _ False = []

juntarListaPorLevel:: [[a]] -> [[a]] -> [[a]]
juntarListaPorLevel x []= x
juntarListaPorLevel [] y = y
juntarListaPorLevel (x:xs) (y:ys) = (x ++ y) : juntarListaPorLevel xs ys 

--eje 2.6 
data Dir = Izq | Der 
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre| Bifurcacion Cofre Mapa Mapa
    deriving Show

{-- 
camino1 = Bifurcacion c1 camino2 camino4 
---------------rama izquierda------------
camino2  = Bifurcacion c2 camino3 fin1 

camino3   = Bifurcacion c2 fin2 fin1 

---------------rama Derecha ------------
camino4 = Bifurcacion c2 fin1 camino5 

camino5 = Bifurcacion c2 fin1 fin1

--camino6 = Bifurcacion c2 fin1 fin1  

c1 = Cofre [Chatarra]
c2 = Cofre [Chatarra, Tesoro]
c3 = Cofre [Tesoro, Tesoro, Tesoro]

fin1 = Fin c1
fin2 = Fin c3
--}

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin cofre) = []
todosLosCaminos (Bifurcacion c1 map1 map2) =  [Izq] : consACada Izq (todosLosCaminos map1) ++  [Der] : consACada Der (todosLosCaminos map2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss
------------------------------ eje nave espacial -----------------------------------------

--eje3.1 
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show

sectores :: Nave -> [ SectorId ]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [ SectorId ]
sectoresT EmptyT          = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 
                                      ++ sectoresT t2

idSector :: Sector -> SectorId
idSector (S id _ _) = id

-- eje3.2 
{-- 
nave1 = N (arbol1) 

arbol1 :: Tree Sector 
arbol1 = NodeT s1 arbol2 arbol3 

arbol2 :: Tree Sector 
arbol2 =NodeT s2 arbol4 EmptyT 

arbol4 :: Tree Sector 
arbol4 = NodeT s3 EmptyT EmptyT 

arbol3 :: Tree Sector 
arbol3 = NodeT s4 EmptyT EmptyT 

s1 = S "Sector1" [c1, c2] ["t1" , "t2" , "t3"]
s2 = S "Sector2" [  Almacen [Combustible] ] ["t3"]
s3 = S "Sector3" l1 ["t2" , "t3" , "t4"]
s4 = S "Sector4" [] ["t1" , "t2" , "t3"]

c1 = LanzaTorpedos 
c2 =  Motor 200
c3 = Motor 100
c4 = Almacen [Comida, Oxigeno, Torpedo] 
c5 = Almacen [Comida, Oxigeno] 

l1 = [c3, c4, c5] --}

poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = propulsionT t

propulsionT :: Tree Sector -> Int
propulsionT EmptyT          = 0
propulsionT (NodeT s t1 t2) = propulsionS s + propulsionT t1 + propulsionT t2

propulsionS :: Sector -> Int
propulsionS (S _ cs _) = propulsionCs cs

propulsionCs :: [Componente] -> Int
propulsionCs []     = 0
propulsionCs (c:cs) = propulsionC c + propulsionCs cs

propulsionC :: Componente -> Int
propulsionC (Motor n) = n
propulsionC _         = 0

--eje3.3 
barriles :: Nave -> [Barril]
barriles (N t) = barrilesDeArbol t 

barrilesDeArbol :: Tree Sector -> [Barril]
barrilesDeArbol EmptyT = []
barrilesDeArbol (NodeT s t1 t2 ) = barrilesSector s ++ barrilesDeArbol t1 ++ barrilesDeArbol t2 


barrilesSector :: Sector -> [Barril]
barrilesSector (S _ lc _) = barrilesDeComponentes lc 

barrilesDeComponentes:: [Componente] -> [Barril] 
barrilesDeComponentes  [] = []
barrilesDeComponentes (x:xs) = barrilesDeComponente x ++ barrilesDeComponentes xs 

barrilesDeComponente :: Componente -> [Barril]
barrilesDeComponente (Almacen bs ) = bs 
barrilesDeComponente _ = []

--eje3.4 
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector lc nomS (N t) =  (N (agregarAArbolLista t lc nomS)) 


agregarAArbolLista:: Tree Sector -> [Componente] -> SectorId -> Tree Sector  
agregarAArbolLista EmptyT  _ _= EmptyT
agregarAArbolLista (NodeT s t1 t2) cs nomS = NodeT (agregarASectorConNombre  cs s nomS)  (agregarAArbolLista t1 cs nomS)   (agregarAArbolLista t2 cs nomS )


agregarASectorConNombre :: [Componente] -> Sector -> SectorId -> Sector
agregarASectorConNombre cs s nomS = if idSector s == nomS 
                                        then  agregarEnSector cs s 
                                        else  s 

agregarEnSector:: [Componente] -> Sector -> Sector 
agregarEnSector cs1 (S nom  cs2  ts) =  S nom (cs2++cs1)  ts 

-- eje 3.5 

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tr sectoresId (N arbolS) = N (asignarTripulanteASectores tr sectoresId arbolS)  

asignarTripulanteASectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector 
asignarTripulanteASectores _ _ EmptyT = EmptyT  
asignarTripulanteASectores t sectoresId (NodeT s t1 t2)  = NodeT (agregarTASectorSiPerteneceA t s sectoresId ) (asignarTripulanteASectores t sectoresId t1)  (asignarTripulanteASectores t sectoresId t2)  

agregarTASectorSiPerteneceA:: Tripulante -> Sector -> [SectorId] -> Sector 
agregarTASectorSiPerteneceA t s [] = s 
agregarTASectorSiPerteneceA t s (id:ids) = if idSector s == id 
                            then agregarTA t s 
                            else agregarTASectorSiPerteneceA t s ids 

agregarTA:: Tripulante -> Sector -> Sector 
agregarTA t (S nom  cs2  ts) = S nom cs2 (t:ts)

-- eje 3.5  

sectoresAsignados :: Tripulante -> Nave -> [SectorId] 
sectoresAsignados t (N arbolS) = idSectoresDondeAparece t arbolS 


idSectoresDondeAparece :: Tripulante -> Tree Sector -> [SectorId]
idSectoresDondeAparece tripulante  EmptyT  =  []
idSectoresDondeAparece trp   (NodeT s t1 t2)   = idDeSectorSiAparece s trp ++ idSectoresDondeAparece trp t1 ++  idSectoresDondeAparece trp t2 

idDeSectorSiAparece:: Sector -> Tripulante -> [SectorId]
idDeSectorSiAparece (S id  cs2  ts) t = singular_Si_ id (pertenece t ts)

-- eje 3.6 

tripulantes :: Nave -> [Tripulante]
tripulantes (N arbolSec) = listaTripulantesDe arbolSec 

listaTripulantesDe :: Tree Sector -> [Tripulante]
listaTripulantesDe EmptyT = []
listaTripulantesDe (NodeT s t1 t2) = agregarListaALista (tripulantesDe s)  (agregarListaALista (listaTripulantesDe t1)  (listaTripulantesDe t2)) 

tripulantesDe:: Sector -> [Tripulante] 
tripulantesDe (S id cs ts) = ts 

agregarListaALista :: [Tripulante] -> [Tripulante] -> [Tripulante]
agregarListaALista ts1 [] = ts1 
agregarListaALista []  ts2 = ts2 
agregarListaALista (t:ts) ts2 = if pertenece t ts2 
                            then agregarListaALista ts ts2 
                            else t: agregarListaALista ts ts2 

-------------------------------eje manada de lobos ----------------------------------------

type Presa = String -- nombre de presa
    
type Territorio = String -- nombre de territorio 
    
type Nombre = String -- nombre de lobo
    
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo| Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show


--eje1
manada0= M lobo0

lobo0 = Cazador "cazador1" [p1,p2, p3, p4] loboE1 loboE2 cria1

loboE1 = Explorador "explorador1" [t1,t2,t3]  cria2 cria3

loboE2 = Explorador "explorador2" [t4]  loboc2 cria3

loboc2 = Cazador "cazador2" [p1,p2, p3, p4, p5] cria1 cria1 cria1
---crias------
cria1 = Cria "cria1"
cria2 = Cria"cria2"
cria3 = Cria "cria3"
cria4 = Cria "cria4"
----- presas-----
p1 =  "presa1"
p2 =  "presa2"
p3 = "presa3"
p4 = "presa4"
p5 = "presa5"
------territorios---
t1 ="terreno1"
t2 ="terreno2"
t3 ="terreno3"
t4 ="terreno4"


-- eje 4.2 
buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimentos m > cantidadCriasDe m 

cantidadDeAlimentos :: Manada -> Int
cantidadDeAlimentos (M lobo) = cantidadDeAlimentosDeLobo lobo

cantidadDeAlimentosDeLobo :: Lobo -> Int
cantidadDeAlimentosDeLobo (Cria _) = 0 
cantidadDeAlimentosDeLobo (Explorador _ _ l1 l2) = cantidadDeAlimentosDeLobo l1 + cantidadDeAlimentosDeLobo l2 
cantidadDeAlimentosDeLobo (Cazador _ ps l1 l2 l3) = alimentosEn ps + cantidadDeAlimentosDeLobo l1 + cantidadDeAlimentosDeLobo l2 + cantidadDeAlimentosDeLobo l3


alimentosEn :: [Presa] -> Int
alimentosEn ps = length ps 

cantidadCriasDe :: Manada -> Int
cantidadCriasDe (M lobo) = cantidadCriasDeLobo lobo

cantidadCriasDeLobo :: Lobo -> Int
cantidadCriasDeLobo (Cria _) = 0
cantidadCriasDeLobo (Explorador _ _ l1 l2) = unoSiEsCria_ l1 +  unoSiEsCria_ l2 + cantidadCriasDeLobo l1 +cantidadCriasDeLobo l2
cantidadCriasDeLobo (Cazador _ _ l1 l2 l3) =  unoSiEsCria_ l1 +  unoSiEsCria_ l2 +  unoSiEsCria_ l3 + cantidadCriasDeLobo l1 +cantidadCriasDeLobo l2 + cantidadCriasDeLobo l3

unoSiEsCria_ :: Lobo -> Int
unoSiEsCria_ (Cria _) = 1
unoSiEsCria_ _ = 0

-- eje 4.3 

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaLobo lobo 

elAlfaLobo :: Lobo -> (Nombre, Int)
elAlfaLobo (Cria nom) = (nom, 0)
elAlfaLobo (Explorador nom _ l1 l2) = elegir [elAlfaLobo l1 ,elAlfaLobo l2, (nom, 0)]
elAlfaLobo (Cazador nom presas l1 l2 l3) = elegir [(nom, length presas), elAlfaLobo l1, elAlfaLobo l2, elAlfaLobo l3]

elegir:: [(Nombre, Int)] -> (Nombre, Int)
--Precondición: la lista dada no puede ser una lista vacía
elegir (nc : [])= nc
elegir (nc :ncs) = elegirEntre nc (elegir ncs)


elegirEntre::  (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1)  (nom2 , c2) = if (c1>= c2) 
                                    then (nom1, c1)
                                    else  (nom2, c2)

-- eje 4.4 

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron  t1 (M lobos)= lobosQuePasaronPor t1 lobos

lobosQuePasaronPor :: Territorio -> Lobo-> [Nombre]
lobosQuePasaronPor t1 (Cria nom) = []
lobosQuePasaronPor t1 (Explorador nom ts l1 l2) = agregarSiPaso nom t1 ts ++ lobosQuePasaronPor t1 l1 ++ lobosQuePasaronPor t1 l2 
lobosQuePasaronPor t1 (Cazador nom presas l1 l2 l3)= lobosQuePasaronPor t1 l1 ++ lobosQuePasaronPor t1 l2 ++  lobosQuePasaronPor t1 l3

agregarSiPaso :: Nombre -> Territorio -> [Territorio] -> [Nombre]
agregarSiPaso nom t1 ts= if (pertenece t1 ts)
                        then [nom] 
                        else []


pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []= False
pertenece a (x:xs)= x==a || pertenece a xs


-- eje 4.6
superioresDelCazador :: Nombre -> Manada -> [Nombre]
--Precondición: El nombre del cazador dado como parametro debe existir.
superioresDelCazador  nom (M lobo) = superioresDe nom lobo 


superioresDe :: Nombre -> Lobo -> [Nombre] 
--Precondición: El nombre del cazador dado como parametro debe existir.
superioresDe nomC1 (Cria _) = []
superioresDe nomC1 (Explorador _ _ l1 l2)= superioresDe nomC1 l1 ++ superioresDe nomC1 l2 
superioresDe nomC1 (Cazador nom _ l1 l2 l3) = if nomC1 == nom 
            then []
            else [nom] ++ superioresDe nomC1 l1 ++ superioresDe nomC1 l2 ++ superioresDe nomC1 l3 