*>
*> Filtrado en ayuda general de almacenes/zona.
*>
*> _Programa: Nombre del programa que llama a la funci¢n.
*> _Forma: 'INICIO', antes de cargar la ayuda.
*>         'LECTURA', al cargar la ayuda.
*>         'FINAL', al abandonar el programa.

Function VF03C
Parameters _Programa, _Forma

Do Case
   *> Filtrado aplicable a programas concretos.
   Case _Programa = '??????'
      Do Case
      EndCase

   *> Filtrado aplicable de forma general.
   Otherwise
      Do Case
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete All For F03cCodAlm <> _Alma
      EndCase
EndCase

Return
