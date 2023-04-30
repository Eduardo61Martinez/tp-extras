module SetV2 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

    where 
    
data Set a = S [a] Int 
    deriving Show
    {--
        INV.REPRESENTACION :
            -> ninguna
    --}

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]
---------------------------------------------------------------------------------
unoSiceroSiNo :: Bool -> Int 
unoSiceroSiNo True = 1
unoSiceroSiNo False = 0

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []= False
pertenece a (x:xs)= x==a || pertenece a xs

contarSiNoSonRepetidos :: Eq a => [a]-> [a] -> Int
contarSiNoSonRepetidos [] ys     = length ys 
contarSiNoSonRepetidos xs []     = length xs 
contarSiNoSonRepetidos (x:xs) ys =  unoSiceroSiNo ( not (pertenece x ys)  ) + contarSiNoSonRepetidos xs ys

remover_De :: Eq a => a-> [a] -> [a] 
remover_De x [] = [] 
remover_De x (y:ys) = if x == y 
                        then remover_De x ys 
                        else y:remover_De x ys 

sinRepetido:: Eq a => [a] -> [a] 
sinRepetido [] = []
sinRepetido (x:xs) = if pertenece x xs 
                        then sinRepetido xs 
                        else x:sinRepetido xs 
----------------------------------------------------------------------------

emptyS                    = S [] 0 
addS x (S xs n)             = S (x:xs) (n + unoSiceroSiNo ( not (pertenece x xs)) )
belongs x (S xs _)        = pertenece x xs 
sizeS  (S _ n )          = n 
unionS (S xs _) (S ys _)  = S (xs ++ ys) (contarSiNoSonRepetidos xs ys)
removeS x  (S xs m)       =  S (remover_De x xs) (m -1 ) 
setToList (S xs n)        = sinRepetido xs 


                

 
