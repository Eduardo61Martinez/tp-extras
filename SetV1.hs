module SetV1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

    where 
    
data Set a = S [a] 
     deriving Show
    {--
        INV.REPRESENTACION :
            -> el conjunto dado no admite elementos repetidos.
    --}


emptyS :: Set a
--Crea un conjunto vacío.
emptyS = S []

addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS  x (S xs)  = if pertenece x xs 
                then    S  xs
                else    S  (x:xs)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []= False
pertenece a (x:xs)= x==a || pertenece a xs

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs)= pertenece x xs 

sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS  (S xs) = length xs

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS x (S xs) = S (sinElemento x xs)

sinElemento ::  Eq a => a  -> [a] -> [a]
sinElemento _ [] = []
sinElemento y  (x:xs) = if y == x 
                        then xs
                        else x:sinElemento y xs

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS  (S xs)  (S ys) = S (sinRepetidos xs ys)

sinRepetidos :: Eq a => [a] -> [a] -> [a]
sinRepetidos [] ys = ys
sinRepetidos xs [] = xs 
sinRepetidos (x:xs) ys = if  pertenece x ys 
                            then sinRepetidos xs ys 
                            else x:sinRepetidos xs ys


setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs) = xs
