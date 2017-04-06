*> Programa de filtrado ayudas en zona de almacén.
*>
*> Parámetros:
*> _Programa: Nombre del programa que llama a la funci¢n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vczon
Parameters _Programa, _Forma

Do Case
   *> Filtros particulares para programas concretos.
   Case AllTrim(_Programa)=='????????'

   *> Filtro general.
   Otherwise
      Do Case
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete All For F03cCodAlm # _Alma
            Go Top
      EndCase
EndCase

Return
