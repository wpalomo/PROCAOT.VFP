* Programa de filtrado especial origen 
*
Function vHGSitStk
Parameters _programa,_forma
*
do case
case _forma='LECTURA'
  select AYUDA
  *
  do case
  case _programa='ENTRRECM' 
    delete all for at(left(F00CCodStk,1),'12789')=0
  endcase
  
endcase
*
return
