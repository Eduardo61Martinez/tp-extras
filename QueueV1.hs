module QueueV1 (Queue, emptyQ ,isEmptyQ, enqueue, firstQ, dequeue)

    where 
        
data Queue a = Q [a] 
    deriving Show


q2 = Q [1,3,4,5,6,7]


emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs 

enqueue:: a -> Queue a -> Queue a 
enqueue x (Q xs) = Q (agregarAlFinal x xs) 

agregarAlFinal :: a -> [a] -> [a] 
agregarAlFinal x [] = [x] 
agregarAlFinal x (y:ys) = y : agregarAlFinal x ys  

firstQ :: Queue a -> a 
firstQ (Q xs) = head xs 

dequeue :: Queue a -> Queue a 
dequeue (Q xs) = Q (tail xs )
