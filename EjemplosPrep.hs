module EjemplosPrep (
  ej1,
  ej2,
  ej3,
  ej4,
  ej5,
  ej6,
  ej7
  ) where



import BloquesPrep

--Aqui se pondran algunos ejemplos de expresiones logicas
--para dado el caso hacer testeos

ej1,ej2,ej3,ej4,ej5,ej6,ej7 :: ArbolC
ej1 = OrC (OrC (VarC P) (VarC Q)) (VarC R)   --(P o Q) o R
ej2 = AndC(OrC (VarC P) (VarC Q)) (VarC R)   --(P o Q) y R
ej3 = AndC (AndC (VarC P) (VarC Q)) (VarC R) --(P y Q) y R
ej4 = AndC (VarC P) (VarC Q)                 --P y Q
ej5 = OrC (VarC P) (VarC Q)                  --P o Q
ej6 = OrC ej1 (ej5)                          --((P o Q) o R) o (P o Q)
ej7 = AndC ej1 (ej5)                         --((P o Q) o R) y (P o Q)
