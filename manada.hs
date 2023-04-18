
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
cantidadCriasDeLobo (Explorador _ _ l1 l2) = unoSiCeroSino (esCria_ l1) + unoSiCeroSino (esCria_ l2) + cantidadCriasDeLobo l1 +cantidadCriasDeLobo l2
cantidadCriasDeLobo (Cazador _ _ l1 l2 l3) =  unoSiCeroSino (esCria_ l1) +  unoSiCeroSino (esCria_ l2)+  unoSiCeroSino (esCria_ l3) + cantidadCriasDeLobo l1 +cantidadCriasDeLobo l2 + cantidadCriasDeLobo l3



esCria_ :: Lobo -> Bool
esCria_ (Cria _) = True
esCria_ _ = False

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0


----------------------------------------

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


------------------------------------------------------------
manada0= M lobo0

lobo0 = Cazador "cazador1" [p1, p2, p3, p4] loboE2 loboE1 cria1

loboE1 = Explorador "explorador1" [t1,t2,t3]  cria2 cria3

loboE2 = Explorador "explorador2" [t3, t2]  loboC2 cria3

loboC2 = Cazador "cazador2" [p1, p2, p3, p4 ] loboC3 cria3 cria4

loboC3 = Cazador "cazador3" [p1, p2, p3, p4 ] cria2 cria3 cria4
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


--eje 3 

type Presa = String -- nombre de presa
    
type Territorio = String -- nombre de territorio 
    
type Nombre = String -- nombre de lobo
    
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo| Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show
{-- 
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = lobosPorTerritorio lobo

lobosPorTerritorio :: Lobo ->  [(Territorio, [Nombre])]
lobosPorTerritorio (Cria nom)= []
lobosPorTerritorio (Cazador nom ps l1 l2 l3) = lobosPorTerritorio l1 ++ lobosPorTerritorio l2 ++ lobosPorTerritorio l3 
lobosPorTerritorio  (Explorador nom ts l1 l2) = consolidarLista ts  nom (lobosPorTerritorio l1)++ lobosPorTerritorio l2
                                                               

consolidarLista:: [Territorio] -> Nombre -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
consolidarLista [] nom ys = ys
consolidarLista (t:ts) nom ys = agregar t nom (consolidarLista ts nom ys) 

agregar :: Territorio -> Nombre -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregar t nom [] = (t, [nom]): []
agregar t nom ((y,n):yns) = if t == y 
                        then   (y,n ++ [nom] ): yns
                        else   (y,n):agregar t nom yns 


l1 = [("terreno2",["explorador2","explorador1"]),("terreno3",["explorador2"])]

l2 = [ "terreno2" , "terreno3", "Terreno5"]

--}

superioresDelCazador :: Nombre -> Manada -> [Nombre]
--Precondición: El nombre del cazador dado como parametro debe existir.
superioresDelCazador  nom (M lobo) = superioresDe nom lobo 


superioresDe :: Nombre -> Lobo -> [Nombre] 
--Precondición: El nombre del cazador dado como parametro debe existir.
superioresDe nomC1 (Cria _) = []
superioresDe nomC1 (Explorador _ _ l1 l2)= superioresDe nomC1 l1 ++ superioresDe nomC1 l2 
superioresDe nomC1 (Cazador nom _ l1 l2 l3) = if ( nomC1 == nom )
            then []
            else [nom] ++ superioresDe nomC1 l1 ++ superioresDe nomC1 l2 ++ superioresDe nomC1 l3 
        