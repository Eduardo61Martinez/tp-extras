import QueueV1
--(Queue, emptyQ ,isEmptyQ, enqueue, firstQ, dequeue)

c1 = enqueue "S" emptyQ 
c2 = enqueue "4" c1    


c3 = enqueue "d" emptyQ  
c4 = enqueue "m" c3

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.

lengthQ q1 = if (isEmptyQ q1)
    then 0
    else  1 +  lengthQ (dequeue q1)
    

queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList q1  = listaDeElementos q1 


listaDeElementos :: Queue a -> [a] 
listaDeElementos q1=  if isEmptyQ q1 
                    then [] 
                    else (firstQ q1): listaDeElementos (dequeue q1) 


unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = agregarTodosA q1 (queueToList q2)

agregarTodosA :: Queue a -> [a] -> Queue a 
agregarTodosA q1 [] = q1 
agregarTodosA q1 (x:xs) = enqueue x (agregarTodosA q1 xs) 