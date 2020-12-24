module Logica
       (
         hazTabla,
         esCiertoAlgun,
         esTautologia,
         casosCiertos,
         (==>),
         (<==>)
         ) where


import AuxiliarPrep
import BloquesPrep
import EjemplosPrep

--Esto hace la tabla de verdad de una expresion logica
hazTabla :: ArbolC -> TablaVerdad
hazTabla ac = hazTablaAux ac

--Se comprueba si una expresion logica es cierta en al menos un caso
esCiertoAlgun :: ArbolC -> Bool
esCiertoAlgun ac = esCiertaAlgunAux ac

-- Se comprueba si una expresion logica es una tautologia
esTautologia :: ArbolC -> Bool
esTautologia ac = let xs = map snd (snd (hazTabla ac))
                  in all (\x -> x == True) xs

--Se buscan los casos ciertos de una expresion logica
casosCiertos :: ArbolC -> [Caso]
casosCiertos ac = let xs = creaCasos (listaVarArbolC ac)
                  in filter (\x -> eval x ac) xs

--Implicacion simple
(==>) :: ArbolC -> ArbolC -> Bool
(==>) p q = implica p q

--Doble implicacion.
(<==>) :: ArbolC -> ArbolC -> Bool
(<==>) p q = dobleImplicacion p q
