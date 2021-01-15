module Auxiliares (
                   ej1,
                   ej2,
                   ej4,
                   ej5,
                   subformulas,
                   variables,
                   evalua,
                   aplicaCasos
                  ) where

import Definiciones
import Data.List (nub, genericLength)
-- -----------------------------------------
-- -----------------------------------------
--PARTE REFERIDA A LA LOGICA L_0
--Ejemplos de formulas logicas
ej1, ej2, ej3, ej4, ej5 :: FormulaPrep Int
ej1 = Bimplica (Or (Var 4) (Var 3)) (Var 7)
ej2 = Or (Implica (Var 4) (Var 3)) (And (Var 6) (Var 1))
ej3 = Bimplica (Var 5) (Neg (Neg (Var 5)))
ej4 = Implica (ej3) (ej1)
ej5 = Implica (Or (ej1) (ej4)) (Implica (ej3) (ej2))

-- -----------------------------------------
--DAr la lista de las subformulas de una formula
--Da Error non exhaustive patterns y no se por qué :(
subformulas :: FormulaPrep a -> [FormulaPrep a]
subformulas (Var x) = [Var x]
subformulas (And for1 for2) = [(And for1 for2)] ++ subformulas for1 ++ subformulas for2
subformulas (Or for1 for2) = [Or for1 for2] ++ subformulas for1 ++ subformulas for2
subformulas (Bimplica for1 for2) = [Bimplica for1 for2] ++ subformulas for1 ++ subformulas for2
subformulas (Implica for1 for2) = [Implica for1 for2] ++ subformulas for1 ++ subformulas for2
subformulas (Neg for1) = [Neg for1] ++ subformulas for1

-- -----------------------------------------
--Dar la lista de vsariables de una formula
variables :: Eq a => FormulaPrep a -> [a]
variables f = nub $ variablesAux f

variablesAux :: Eq a => FormulaPrep a -> [a]
variablesAux (Var x) = [x]
variablesAux (Neg for1) = variables for1
variablesAux (And for1 for2) = variables for1 ++ variables for2
variablesAux (Or for1 for2) = variables for1 ++ variables for2
variablesAux (Bimplica for1 for2) = variables for1 ++ variables for2
variablesAux (Implica for1 for2) = variables for1 ++ variables for2

-- -----------------------------------------
--Dado un caso, evaluarlo
evalua :: Eq a => FormulaPrep a -> [(a,Bool)] -> Bool
evalua _ [] = False
evalua (Var x) xs = coge x xs
evalua (And f1 f2) xs = evalua f1 xs && evalua f2 xs
evalua (Or f1 f2) xs = evalua f1 xs || evalua f2 xs
evalua (Implica f1 f2) xs = if evalua f1 xs && evalua f2 xs == False then False else True
evalua (Bimplica f1 f2) xs = if evalua f1 xs == evalua f2 xs then True else False
evalua (Neg f) xs = not $ evalua f xs

coge :: Eq a => a -> [(a,Bool)] -> Bool
coge _ [] = False
coge x (y:xs) | fst y == x = snd y
              | otherwise = coge x xs
-- ------------------------------------------
--COSAS PARA LA TABLA DE VERDAD
--dado un numero, dar una lista de los digitos en binario
aBinario :: Integer -> [Bool]
aBinario 0 = []
aBinario n | even n = aBinario (n `div` 2) ++ [False]
           | otherwise = aBinario (n `div` 2) ++ [True]

--lista de los numeros en binario desde 0 hasta n
numEnBinarios :: Integer -> [[Bool]]
numEnBinarios n = map aBinario [0..n]

--Añade la ristra de ceros para que todos tengan la misma longitud que el máximo
numBinariosCompleto :: [[Bool]] -> [[Bool]]
numBinariosCompleto xss =
  let n = genericLength (last xss)
  in map (\xs -> [False | k<-[1..(n-(genericLength xs))]] ++ xs) xss

--Lista de los numeros en binarios como lista de 0 hasta n
numBin :: Integer -> [[Bool]]
numBin n = numBinariosCompleto (numEnBinarios n)

aplicaCasos :: [a] -> [[Bool]] -> [[(a,Bool)]]
aplicaCasos xs yss = foldr (\y ys -> (zip xs y) : ys) [] yss
