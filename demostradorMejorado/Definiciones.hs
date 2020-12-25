module Definiciones (
                     --Parte de la L. preposicional
                     FormulaPrep (..),
                     creaFor,
                     esSatisfacible,
                     esTautologia,
                     esContingente,
                     esContradiccion
                     --Parte de la L. primer orden
                    ) where
--A fin de implementar una consola de comandos o terminal
main :: IO ()
main = undefined

-- ------------------------------------------------------
-- ------------------------------------------------------
--PARTE REFERIDA A LA LÓGICA PREPOSICIONAL (L_0)
{-
NOTA: TODO VIENE MARCADO CON UN TOKEN QUE DICE SI VA A
UN ARCHIVO A PARTE DE FUNCIONES AUXILIARES U OTROS

Ideas:
- for1 == for2 es ver si son logicamente equivalentes. Esto s ehace a la hora
de instanciar Formula como Eq

-}
data FormulaPrep = And FormulaPrep FormulaPrep |
                   Or FormulaPrep FormulaPrep |
                   Implica FormulaPrep FormulaPrep |
                   Bimplica FormulaPrep FormulaPrep |
                   Neg FormulaPrep |
                   Var String

instance Show FormulaPrep where
  show (Var xs) = xs
  show (Neg for) = "not(" ++ show for ++ ")"
  show (Bimplica for1 for2) = "(" ++ show for1 ++ " <==> " ++ show for2 ++ ")"
  show (Implica for1 for2) = "(" ++ show for1 ++ " ==> " ++ show for2 ++ ")"
  show (Or for1 for2) = "(" ++ show for1 ++ " o " ++ show for2 ++ ")"
  show (And for1 for2) = "(" ++ show for1 ++ " y " ++ show for2 ++ ")"

--Funcion para crear una formula de logica preposicional
creaFor :: IO FormulaPrep
creaFor = undefined

--Core del programa.
esSatisfacible, esTautologia, esContingente, esContradiccion :: FormulaPrep -> Bool
esSatisfacible = undefined
esTautologia = undefined
esContingente = undefined
esContradiccion = undefined

-- ------------------------------------------------------
-- ------------------------------------------------------
--PARTE REFERIDA A LA LÓGICA DE PRIMER ORDEN (L_1)

