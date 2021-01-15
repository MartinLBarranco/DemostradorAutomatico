import Auxiliares
import Definiciones

--Core del programa.
esSatisfacible, esTautologia, esContingente, esContradiccion :: (FormulaPrep a) -> Bool
esSatisfacible for = undefined
esTautologia for = undefined
esContingente for = undefined
esContradiccion for = undefined

--Tabla de verdad de una ecuacion
tablaVerdad :: Eq a => FormulaPrep a -> [ ([(a,Bool)],Bool) ]
tablaVerdad for = undefined

--Se verifica si una interpretacion es modelo de un ConjFormulas
esModelo :: [(a,Bool)] -> [FormulaPrep a] -> Bool
esModelo xs ys = undefined

--Se verifica si un ConjFormulas es consistente/inconsistente
consistente, inconsistente :: [FormulaPrep a] -> Bool
consistente for = undefined
inconsistente for = not $ consistente for

--Se verifica si un ConjFormula2 es consecuencia de ConjFormula1
consecuenciaDe :: [FormulaPrep a] -> [FormulaPrep a] -> Bool
consecuenciaDe xs ys = undefined

--Se comprueba si dos formulas son equivalentes
equivalentes :: FormulaPrep a -> FormulaPrep a -> Bool
equivalentes for1 for2 = undefined -- for1 == for2 ???
