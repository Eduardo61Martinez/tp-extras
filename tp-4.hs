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
armarPizza (x:xs) = (Capa x (armarPizza xs))

--eje 1.3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza  = Prepizza
sacarJamon (Capa ing p) = if ( esJamon ing)
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
duplicarAceitunas (Capa ing p) = if (esAceitunas ing)
    then Capa  (duplicar_ ing) (duplicarAceitunas p)
    else Capa  ing  (duplicarAceitunas p)

esAceitunas :: Ingrediente -> Bool
esAceitunas (Aceitunas _ ) = True
esAceitunas _ = False

duplicar_ :: Ingrediente -> Ingrediente 
duplicar_ (Aceitunas n) = (Aceitunas (2*n) )


{--p1 = Capa Salsa ( Capa Queso (Capa Jamon Prepizza))

p2 = Capa Salsa ( Capa (Aceitunas 1) (Capa Salsa (Capa (Aceitunas 10) Prepizza)))

p3 = Capa Salsa ( Capa Jamon (Capa Queso Prepizza)) 
lista1 = [p1,p2, p3]
--}

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs 

-------------------------------------Mapa del tesoro ----------------------------------------------------

data Dir = Izq | Der 
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre| Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa1 = Bifurcacion (Cofre l1) mapa2 fin

mapa2 = Bifurcacion (Cofre l2) fin fin 

l1 = [Chatarra, Chatarra, Chatarra, Chatarra]
l2 = [Tesoro]
fin = (Fin (Cofre l1))

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
hayTesoroEn []  = ...
hayTesoroEn _ (Fin cofre)= ...
hayTesoroEn (x:xs) (Bifurcacion cofre mapa1 mapa2) = if (esDerecha x)
                    then hayTesoro cofre || hayTesoroEn mapa2
                    else hayTesoro cofre || hayTesoroEn mapa1

esDerecha:: Dir -> Bool 
esDerecha Der = True
esDerecha _ = False


--eje2.3s

---------------------------------------------------------eje manada de lobos ----------------------------------------

type Presa = String -- nombre de presa
    
type Territorio = String -- nombre de territorio 
    
type Nombre = String -- nombre de lobo
    
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo| Cría Nombre
    deriving Show
data Manada = M Lobo
    deriving Show


--eje1

manada0= M lobo0

lobo0 = Cazador "cazador1" [p1,p2,p3] loboE1 loboE2 cria1

loboE1 = Explorador "explorador1" [t1,t2,t3]  cria2 cria3

loboE2 = Explorador "explorador2" [t4]  cria4 cria3
---crias------
cria1 = "cria1"
cria2 = "cria2"
cria3 = "cria3"
cria4 = "cria4"
----- presas-----
p1 = "presa1"
p2 = "presa2"
p3 = "presa3"
------territorios---
t1 ="terreno1"
t2 ="terreno2"
t3 ="terreno3"
t4 ="terreno4"


buenaCaza :: Manada -> Bool
buenaCaza (M lobos) =  alimentoMayorACrias lobos 

alimentoMayorACrias :: Lobo -> Bool
alimentoMayorACrias (Cría nmCria) = 
alimentoMayorACrias (Cazador nom ps l1 l2 l3) = alimentosEnMayorA ps (cantidadCriasDe l1) && alimentoMayorACrias l1 && alimentoMayorACrias l2
alimentoMayorACrias (Explorador nom ts l1 l2  ) = alimentosEnMayorA ps (cantidadCriasDe l1) && alimentoMayorACrias l2 

alimentosMayorA :: Bool

cantidadCriasDe :: Lobo -> Int
cantidadCriasDe (Crias _) = 0
cantidadCriasDe (Cazador _ _ l1 l2 l3) =  unoSiCeroSino (esCria_ l1) + cantidadCriasDe l2 + cantidadCriasDe l3
cantidadCriasDe (Explorador _ _ l1 l2) = unoSiCeroSino (esCria_ l1) + cantidadCriasDe l2 


esCria_ :: Lobo -> Bool
esCria_ (Cria _) = True
esCria_ _ = False

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0
