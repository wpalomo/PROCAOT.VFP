*>
*> Control de ayudas sobre cantidad física en grids.
Function vHGCodTar
Parameters _programa, _forma

Do Case
   Case _programa='ENTRVALO'
     Do Case
        Case _forma='LECTURA'
           Select AYUDA
           Delete For F34tCodPro <> CodPro
     EndCase
      
   Case _programa='SALIVALO'
     Do Case
        Case _forma='LECTURA'
           Select AYUDA
           Delete For F34tCodPro <> CodPro
     EndCase

EndCase

*>
Return

* Programa de filtrado especial NumEnt
*
*> _Programa: Nombre del programa que llama a la funci¢n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

