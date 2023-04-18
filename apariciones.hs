apariciones :: Eq a => [a] -> [(a,Int)]
apariciones  [] =   []
apariciones (y:ys) = agregar y (apariciones ys)


agregar :: Eq a => a -> [(a, Int)] -> [(a, Int)]
agregar x [] = (x, 1) : []
agregar x (y: ys) = if x == fst y 
                    then (fst y, snd y +1) : ys
                    else  y: agregar x ys
------------------------------------------

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


sector1 = S "Sector1" [c1, c2 ] ["t1" , "t2" , "t3"]

c1 = LanzaTorpedos 
c2 =  Motor 16

c3 = Motor 15
c4 = Motor 100
c5 = Almacen [Comida, Oxigeno] 

l1 = [c3, c4, c5]

agregarAArbolLista:: Tree Sector -> [Componente] -> SectorId -> Tree Sector  
agregarAArbolLista EmptyT  _ _= EmptyT
agregarAArbolLista (NodeT s t1 t2) cs nomS = NodeT (agregarASectorConNombre cs s nomS)  agregarAArbolLista t1  agregarAArbolLista t2 

agregarASectorConNombre :: [Componente] -> Sector -> SectorId -> Sector
agregarASectorConNombre cs s nomS = if idSector s == nomS 
                                        then  agregarEnSector cs s 
                                        else  s 
idSector :: Sector -> SectorId
idSector (S id _ _) = id

agregarEnSector:: [Componente] -> Sector -> Sector 
agregarEnSector cs1 (S nom  cs2  ts) =  S nom (cs2++cs1)  ts 