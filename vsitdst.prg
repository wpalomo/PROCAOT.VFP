* Programa de filtrado especial destino
*
Function vsitdst
Parameters _programa,_forma
*
do case
case _forma='LECTURA'
  select AYUDA
  delete all for at(left(F00cCodStk,1),'189')=0
endcase
*
return