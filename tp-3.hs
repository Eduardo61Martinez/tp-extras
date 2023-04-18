-----------------------------------Recursivos simples ------------------------------------------------
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show


--eje1.1.1
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 c3)= unoSiCeroSiNo(esColor c1 c2 ) + nroBolitas c1  c3

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo True = 1
unoSiCeroSiNo False = 0

esColor :: Color -> Color -> Bool
esColor  Azul Azul = True
esColor Rojo Rojo = True
esColor _ _ = False


--eje1.1.2
poner :: Color -> Celda -> Celda
poner colorAPoner CeldaVacia = Bolita colorAPoner CeldaVacia
poner colorAPoner (Bolita colorBolita celda) =  Bolita colorBolita (Bolita colorAPoner CeldaVacia)

--eje1.1.3
sacar :: Color -> Celda -> Celda
sacar  colorASacar CeldaVacia = CeldaVacia
sacar  colorASacar (Bolita colorBolita celda) =if (esColor colorASacar colorBolita)
    then celda
    else Bolita colorBolita celda 
{--
celda1 = Bolita Azul CeldaVacia
celda2 = Bolita Azul celda1
celda3 = Bolita Rojo (Bolita Rojo celda1)
--}

--eje1.1.4
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ c1 = c1 
ponerN cant colorAP celda=  Bolita colorAP (ponerN (cant-1) colorAP celda)


---------------------------------Camino hacia el tesoro-----------------------------

--eje1.2.1
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre listObj camino) = hayTesoroEn_ listObj || hayTesoro camino 
hayTesoro (Nada camino) =  hayTesoro camino

hayTesoroEn_ :: [Objeto] -> Bool
hayTesoroEn_ [] = False
hayTesoroEn_ (x:xs) = esTesoro x || hayTesoroEn_ xs

esTesoro :: Objeto -> Bool
esTesoro  Tesoro = True
esTesoro _ = False

--eje1.2.2
pasosHastaTesoro :: Camino -> Int
--Precondición: Debe haber almenos un tesoro en el camino dado
pasosHastaTesoro (Cofre listObjetos camino) = if ( hayTesoroEn_ (listObjetos)  )
                                            then 0
                                            else 1 + pasosHastaTesoro camino
pasosHastaTesoro (Nada camino) =  1 + pasosHastaTesoro camino


negar :: Bool -> Bool
negar True = False
negar False = True

--eje1.2.3
hayTesoroEn :: Int -> Camino -> Bool
--precondicion: debe haber almenos un tesoro en el camino
hayTesoroEn _ Fin = False
hayTesoroEn  n camino= sonIGuales n (pasosHastaTesoro camino)
hayTesoroEn n (Nada camino) =sonIGuales n (pasosHastaTesoro camino)


sonIGuales:: Int -> Int -> Bool
sonIGuales a b =  a==b

--eje1.2.4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 Fin =True
alMenosNTesoros n camino = sonIGuales n (cantidadDeTesorosEn camino)
alMenosNTesoros _ Fin =False


cantidadDeTesorosEn:: Camino -> Int
cantidadDeTesorosEn Fin = 0
cantidadDeTesorosEn (Cofre listaObj camino) = unoSiCeroSiNo( hayTesoroEn_ (listaObj) ) + cantidadDeTesorosEn camino
cantidadDeTesorosEn (Nada camino) = cantidadDeTesorosEn camino
 
data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show
-------------------------------------------- eje desafio -----------------------------------------------------------
 --eje1.2.5
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n1 n2 camino =  contarTesorosDeListaListObjetos (listaDeListObetosDesdeCamino_Hasta_ n1 n2 camino)


listaDeListObetosDesdeCamino_Hasta_:: Int -> Int -> Camino -> [[Objeto]]
listaDeListObetosDesdeCamino_Hasta_ n1 n2 camino = if ( largoDeCamino_EsMayorA_ camino n1 && largoDeCamino_EsMayorA_ camino n2)
                                                                    then 
                                                                        lista_SinLosPrimeros_ (listaDeListaDeObjetosCamino_ camino) n1  ++ lista_SinLosUltimos_ (listaDeListaDeObjetosCamino_ camino) n2
                                                                    else  []


lista_SinLosUltimos_ :: [a] -> Int -> [a]
--Precondicion: La Lista debe tener almenos un largo mayor o igual al numero dado como paramétro
lista_SinLosUltimos_  [] _= []
lista_SinLosUltimos_ (x:xs) n = if (length (xs)>= n)
                                then x:(lista_SinLosUltimos_ xs n)
                                else lista_SinLosUltimos_ xs n

lista_SinLosPrimeros_ :: [a] -> Int -> [a]
--Precondicion: La Lista debe tener almenos un largo mayor o igual al numero dado como paramétro
lista_SinLosPrimeros_ x 0 = x
lista_SinLosPrimeros_ (x:xs) n = lista_SinLosPrimeros_ xs (n-1)
                                

largoDeCamino_EsMayorA_ :: Camino -> Int -> Bool
largoDeCamino_EsMayorA_ c n =  length (listaDeListaDeObjetosCamino_ c) > n  


listaDeListaDeObjetosCamino_:: Camino -> [[Objeto]]
listaDeListaDeObjetosCamino_ Fin  = []
listaDeListaDeObjetosCamino_ (Cofre listObj camino )= listObj : ( listaDeListaDeObjetosCamino_ camino)
listaDeListaDeObjetosCamino_ (Nada camino) = listaDeListaDeObjetosCamino_ camino 

contarTesorosDeListaListObjetos :: [[Objeto]] -> Int
contarTesorosDeListaListObjetos [] = 0
contarTesorosDeListaListObjetos (x:xss) = cantidadTesorosEnListaObj x + contarTesorosDeListaListObjetos xss 


cantidadTesorosEnListaObj:: [Objeto] -> Int
cantidadTesorosEnListaObj []= 0
cantidadTesorosEnListaObj (x:xs) = unoSiCeroSiNo (esTesoro x) + cantidadTesorosEnListaObj xs
--------------------------------Arboles binarios ----------------------------------------
--eje 2.1.1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT a t1 t2) =   a + sumarT t1 + sumarT t2

{-- 
arbol1:: Tree Int
arbol1 = NodeT 10 EmptyT (NodeT 10 EmptyT EmptyT)

arbol2:: Tree Int
arbol2 = NodeT 30 EmptyT arbol1
--}

--eje 2.1.2
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT a t1 t2) = 1 + sizeT t1 + sizeT t2

--eje 2.1.3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x t1 t2 ) = NodeT  (dobleDe_ x)  (mapDobleT t1)  (mapDobleT t2) 

dobleDe_ :: Int -> Int
dobleDe_ x = 2*x

--eje 2.1.4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x1 EmptyT = False
perteneceT x1 (NodeT x2 t1 t2)=  x1 == x2 ||perteneceT x1 t1 || perteneceT x1 t2


----dudoso consultar----
--eje 2.1.5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT x1 (NodeT x2 t1 t2)= unoSiCeroSiNo(x1 == x2 ) + aparicionesT x1 t1 + aparicionesT x1 t2



--eje 2.1.6
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x1 t1 t2) = [x1] ++ leaves t1 ++  leaves t2 


--eje 2.1.7
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x1 t1 t2) = 1 + max  (heightT t1)  (heightT t2)


--eje 2.1.8
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT ( NodeT x1 t1 t2)= NodeT  x1  (mirrorT t2) (mirrorT t1) 

--eje 2.1.9----- consultar------------
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x1 t1 t2) = [x1] ++ listaIzquierda (toList t1)  (toList t2) 


listaIzquierda :: [a]-> [a] -> [a]
listaIzquierda x1 x2 = x1 ++ x2 

{-- 
arbol3 :: Tree Char 
arbol3 = NodeT 'k' (NodeT 'M' EmptyT (NodeT 'n' EmptyT EmptyT)) (NodeT 'j' EmptyT EmptyT)

arbol4 :: Tree Int
arbol4 = NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 4 EmptyT EmptyT)
--}

--eje 2.1.10
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN n (NodeT x1 t1 t2) = consolidar x1 n ++ levelN (n-1) t1 ++ levelN (n-1) t2 

consolidar :: a -> Int -> [a]
consolidar x n = if ( n == 0)
    then [x]
    else []


--eje 2.1.11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x1 t1 t2) = [x1] :  juntarListaPorLevel (listPerLevel t1)  (listPerLevel t2)


juntarListaPorLevel:: [[a]] -> [[a]] -> [[a]]
juntarListaPorLevel x []= x
juntarListaPorLevel [] y = y
juntarListaPorLevel (x:xs) (y:ys) = (x ++ y) : juntarListaPorLevel xs ys 


-- eje 2.1.12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x1 t1 t2) = x1 : elegirLaMasLarga (ramaMasLarga t1) (ramaMasLarga t2) 

elegirLaMasLarga :: [a] -> [a] -> [a]
elegirLaMasLarga os1 os2 = if length os1 > length os2
                            then os1
                            else os2

-- eje 2.1.13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x1 t1 t2) = [x1] : (todosLosCaminos t1 ++ todosLosCaminos t2 ) 

---------------------------2.2. Expresiones Aritméticas --------------------------------------
--2.2.1
eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum n m) = eval n + eval m 
eval (Prod n m) = eval n * eval m  
eval (Neg n) =  - (eval n)

{-- 
exponente1:: ExpA 
exponente1 = Sum (Valor 12) (Sum (Valor 1) (Valor 2) )

exponente2:: ExpA 
exponente2 = Prod (Valor 2) (Sum (Valor 1) (Valor 2) )

exponente3:: ExpA 
exponente3 = Prod (exponente1) (Neg (exponente2))
--}

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

--2.2.2 

simplificar ::  ExpA  ->  ExpA 
simplificar (Sum expresion1 cero )  =  expresion1
simplificar (Sum cero expresion2)   =  expresion2
simplificar (Prod expresion1 cero)  = cero
simplificar (Prod cero expresion1)  = cero
simplificar (Prod uno expresion1)  = expresion1
simplificar (Prod expresion1 uno)  = expresion1
simplificar (Neg negat )= negat

