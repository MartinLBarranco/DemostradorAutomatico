module BloquesPrep
       (
         L (..),
         ArbolC (..),
         Arbol (..),
         Caso,
         TablaVerdad
       ) where

data L = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
         deriving (Show,Eq, Ord, Read)

--Esta es la expresion sintactica en crudo de una expresión lógica.
data ArbolC = AndC ArbolC ArbolC | OrC ArbolC ArbolC | VarC L
              deriving Read
instance Show ArbolC where
  show (AndC p1 p2) = show p1 ++ " y " ++ show p2
  show (OrC p1 p2) = "(" ++ show p1 ++ " o " ++ show p2 ++ ")"
  show (VarC c) = show c

-- Esto es una lista con los valores booleanos de cada variable
type Caso = [(L, Bool)]

--Esta es una version de la expresion con cada variable (L) con un booleano.
data Arbol = And Arbol Arbol | Or Arbol Arbol | Var (L,Bool)
             deriving Read
instance Show Arbol where
  show (And p1 p2) = show p1 ++ " y " ++ show p2
  show (Or p1 p2) = "(" ++ show p1 ++ " o " ++ show p2 ++ ")"
  show (Var (c,p)) = show p

-- Esto es una tabla de verdad. Podría ponerse más cosas como la lista
--de variables para facilitar el acceso de info más adelante, así como
--los casos ciertos, falsos, etc
type TablaVerdad = (ArbolC, [(Caso, Bool)])
