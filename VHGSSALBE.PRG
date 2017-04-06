* Programa de filtrado especial origen 
*
Function vhgSSAlbE
Parameters _programa,_forma
*
do case
   case _programa='ENTRALBE'   
      do case
         case _forma='LECTURA' 
            select AYUDA
            delete all for at(left(F00CCodStk,1),'128')=0
      endcase
endcase   
*
return
