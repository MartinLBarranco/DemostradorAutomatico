module Definiciones (
                     --Parte de la L. preposicional
                     FormulaPrep (..),
                     ConjFormulas,
                     Variable
                     --Parte de la L. primer orden
                    ) where


--import Auxiliares
-- ------------------------------------------------------
-- ------------------------------------------------------
--PARTE REFERIDA A LA LÓGICA PREPOSICIONAL (L_0)
{-
Ideas:
- for1 == for2 es ver si son logicamente equivalentes. Esto s ehace a la hora
de instanciar Formula como Eq
- FormulaPrep tiene un tipo dependiente a por si a la hora de definir
logicas de orden superior, es necesario que se cimenten sobre la L_0
-}
data FormulaPrep a = And (FormulaPrep a) (FormulaPrep a) |
                     Or (FormulaPrep a) (FormulaPrep a) |
                     Implica (FormulaPrep a) (FormulaPrep a) |
                     Bimplica (FormulaPrep a) (FormulaPrep a) |
                     Neg (FormulaPrep a) |
                     Var a
--Definicion de una variable
type Variable = String

--Definicion de conjuntos de formulas
type ConjFormulas a = [FormulaPrep a]

instance Show a => Show (FormulaPrep a) where
  show (Var xs) = show xs
  show (Neg for) = "¬(" ++ show for ++ ")"
  show (Bimplica for1 for2) = "(" ++ show for1 ++ "<==>" ++ show for2 ++ ")"
  show (Implica for1 for2) = "(" ++ show for1 ++ "==>" ++ show for2 ++ ")"
  show (Or for1 for2) = "(" ++ show for1 ++ "o" ++ show for2 ++ ")"
  show (And for1 for2) = "(" ++ show for1 ++ "y" ++ show for2 ++ ")"


-- ------------------------------------------------------
-- ------------------------------------------------------
--PARTE REFERIDA A LA LÓGICA DE PRIMER ORDEN (L_1)


--Definicion de un predicado.
--El string es "es mortal"
--El [a] podria ser de tipo Variable y ser "Socrates"
type Predicado a = (String, [a] -> Bool)

--Definicion provisional de los cuatificadores universales
{- Me gusta más la 4.
OPCIÓN 1:
data Cuanti a = E (a, Cuanti a) |
                Es (a, FormulaPrep a) |
                A (a, Cuanti a) |
                As (a, FormulaPrep a)

OPCIÓN 2:
data Cuanti a = E a (Cuanti a) |
                A a (Cuanti a) |
                FormulaPrep a

OPCIÓN 3:
data Cuanti a = E a [Cuanti a] |
                A a [Cuanti a] |
                FormulaPrep a

--OPCION 4: EL string es lo que representa: "... es mortal."
type Cuanti a = (String, ([a] -> Bool))

-}
