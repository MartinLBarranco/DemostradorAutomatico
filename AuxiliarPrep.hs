module AuxiliarPrep
       ( 
         listaVarArbolC,
         creaCasos,
         unCaso, 
         toBinCifras,
         toBinCifrasAux,
         pasaABool,
         sustAux, 
         listaFinal,
         implica,
         implicaAux1,
         implicaAux,
         dobleImplicacion,
         esCiertaAlgunAux,
         hazTablaAux,
         eval
        ) where

import BloquesPrep
import Data.List (nub, sort)
import Data.Char

--Dado un ArbolC, dar una lista de sus variables sin repetir.
listaVarArbolCAux :: ArbolC -> [L]
listaVarArbolCAux (AndC a b) = listaVarArbolC a ++ listaVarArbolC b
listaVarArbolCAux (OrC a b) = listaVarArbolC a ++ listaVarArbolC b
listaVarArbolCAux (VarC l) = [l]

listaVarArbolC :: ArbolC -> [L]
listaVarArbolC a = (sort . nub . listaVarArbolCAux) a

--Dadas n variables, dar los posibles casos con esas variables
creaCasos :: [L] -> [Caso]
creaCasos xs = let q = length xs
               in nub [zip xs (unCaso q i)| i<-[0..(2^q)]]

unCaso :: Int -> Int -> [Bool]
unCaso n m = pasaABool (toBinCifras n m)

toBinCifras :: Int -> Int -> [Int]
toBinCifras n m = let xs = toBinCifrasAux m
                  in replicate (n-(length xs)) 0 ++ xs

toBinCifrasAux :: Int -> [Int]
toBinCifrasAux 0 = [0]
toBinCifrasAux 1 = [1]
toBinCifrasAux n = toBinCifrasAux (n `div` 2) ++ [n `mod` 2]

pasaABool :: [Int] -> [Bool]
pasaABool xs = map (\y -> if y == 0 then False else True) xs

sustAux :: Caso -> L -> Bool
sustAux [] _ = error("Lista de casos vacio")
sustAux (x:xs) a | fst x == a = snd x
                 | otherwise = sustAux xs a


listaFinal :: ArbolC -> [Caso] -> [(Caso, Bool)]
listaFinal _ [] = []
listaFinal ac (x:xs) = (x, eval x ac) : listaFinal ac xs

--Dado un Arbol, evaluarlo
cambia :: Arbol -> Bool
cambia (And a b) = (cambia a) && (cambia b)
cambia (Or a b) = (cambia a) || (cambia b)
cambia (Var (_,b)) = b

--Dado un caso y un arbolC, evaluarlo
eval :: Caso -> ArbolC -> Bool
eval xs ac = cambia (sustAux1 xs ac)

--Dado un caso y un arbolC, dar el Arbol correspondiente
sustAux1 :: Caso -> ArbolC -> Arbol
sustAux1 xs (AndC a b) = And (sustAux1 xs a) (sustAux1 xs b)
sustAux1 xs (OrC a b) = Or (sustAux1 xs a) (sustAux1 xs b)
sustAux1 xs (VarC a) = Var (a,sustAux xs a)


-- Definicion de una implicacion simple.

implica :: ArbolC -> ArbolC -> Bool
implica p q | listaVarArbolC p == listaVarArbolC q = implicaAux1 p q
            | otherwise = error("Hay un lio con las variables de las expresiones")

implicaAux1 :: ArbolC -> ArbolC -> Bool
implicaAux1 p q | esCiertaAlgunAux p == False = error ("La hipótesis nunca es cierta")
                | otherwise = implicaAux p q

implicaAux :: ArbolC -> ArbolC -> Bool
implicaAux p q = let xs = casosCiertosAux p
                 in all (\x -> eval x q) xs

-- Definicion de la doble implicacion.

dobleImplicacion :: ArbolC -> ArbolC -> Bool
dobleImplicacion p q = implica p q && implica q p


casosCiertosAux :: ArbolC -> [Caso]
casosCiertosAux ac = let xs = creaCasos (listaVarArbolC ac)
                     in filter (\x -> eval x ac) xs

esCiertaAlgunAux :: ArbolC -> Bool
esCiertaAlgunAux ac = let xs = map snd (snd (hazTablaAux ac))
                      in any (\x -> x == True) xs


hazTablaAux :: ArbolC -> TablaVerdad
hazTablaAux ac = let ys = (creaCasos . listaVarArbolC) ac
                 in  (ac, (listaFinal ac ys))
