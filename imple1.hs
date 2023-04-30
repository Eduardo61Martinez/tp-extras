import SetV2
--(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

ff :: Set String
ff  = emptyS

e1 :: Set String 
e1 = addS "R" ff

e2 :: Set String 
e2 = addS "S" e1 

e3:: Set String 
e3 = addS "S" e2 

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen ls c1 = losQuePertenecenA ls c1 

losQuePertenecenA:: Eq a => [a] -> Set a -> [a] 
losQuePertenecenA [] _ = []
losQuePertenecenA (x:xs) c1 = if  belongs x c1 
                                then x : losQuePertenecenA xs c1
                                else losQuePertenecenA xs c1


sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como
--estructura auxiliar.
sinRepetidos  xs = setToList ( agregarTodosA xs emptyS) 

agregarTodosA :: Eq a =>[a] -> Set a -> Set a 
agregarTodosA [] s = s
agregarTodosA (x:xs) c1 = unionS (addS x c1) (agregarTodosA xs  c1 )


unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos  EmptyT =  emptyS 
unirTodos  (NodeT c1 t1 t2 ) =  unionS c1  (unionS (unirTodos t1) (unirTodos t2))