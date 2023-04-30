module StackV1 (Stack, emptyS,isEmptyS, push, top, pop,lenS)
    where 

data Stack a = St [a] deriving Show
    {--
        INV.REPRESENTACION :
            -> el primer elemento que entra es el último en salir.
    --}


emptyS :: Stack a
 -- Propósito: Crea una pila vacía.
emptyS = St []
 
isEmptyS :: Stack a -> Bool
-- Propósito :Dada una pila indica si está vacía.
isEmptyS (St xs ) = null xs 

push :: a -> Stack a -> Stack a
-- Propósito: Dados un elemento y una pila, agrega el elemento a la pila.
push x (St xs) = St (x:xs) 

top :: Stack a -> a
-- Propósito: Dada un pila devuelve el elemento del tope de la pila.
top (St xs) = head xs 

pop :: Stack a -> Stack a
-- Propósito: Dada una pila devuelve la pila sin el primer elemento.
pop (St xs) = St (sinElPrimero xs) 

sinElPrimero:: [a] -> [a] 
sinElPrimero [x] = []
sinElPrimero (x:xs)  = x : sinElPrimero xs 

lenS :: Stack a -> Int
-- Propósito: Dada la cantidad de elementos en la pila.
--Costo: constante.
lenS (St xs) = length xs 

