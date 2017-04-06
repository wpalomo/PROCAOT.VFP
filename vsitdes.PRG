* Programa de filtrado especial destino
*
Function vsitdes
Parameters _programa,_forma
*
do case
case _forma='LECTURA'
  select AYUDA
  delete all for at(left(F00CCodStk,1),'189')=0
endcase
*
return