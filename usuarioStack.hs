import StackV1 
--(Stack, emptyS, isEmptyS, push, top, pop,lenS)

p1 = apilar [1,2,3,4,5,6]

apilar :: [a] -> Stack a
-- Propósito: Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar xs = pilaCon xs 

pilaCon :: [a] -> Stack a 
pilaCon [] = emptyS
pilaCon (x:xs) = push x (pilaCon xs)


desapilar :: Stack a -> [a]
--Propósito: Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar p1  = if isEmptyS p1 
                then [] 
                else desapilar (pop p1)  ++ [top p1 ]


insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Propósito: Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).

insertarEnPos num x p1 = apilar (listaHastaNum num p1 ++ [x] ++ listaDespuesDeNum num p1)


listaHastaNum:: Int -> Stack a -> [a] 
listaHastaNum num p1 = elementosHastaNum num (desapilar p1) 

elementosHastaNum:: Int -> [a] -> [a] 
elementosHastaNum 0 _ = []
elementosHastaNum num (x:xs) = if num>0 
                                then x : elementosHastaNum (num -1) xs 
                                else elementosHastaNum 0 xs

listaDespuesDeNum:: Int -> Stack a -> [a] 
listaDespuesDeNum num p1 = elementosDespuesDeNum num (desapilar p1) 

elementosDespuesDeNum:: Int -> [a] -> [a] 
elementosDespuesDeNum 0 xs = xs 
elementosDespuesDeNum num (x:xs) = if num>0 
                                    then elementosDespuesDeNum (num -1) xs 
                                    else elementosDespuesDeNum 0 xs 

