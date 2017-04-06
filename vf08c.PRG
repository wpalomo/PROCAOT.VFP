*>
*> Filtrado en ayuda general de artículos.
*>
*> _Programa: Nombre del programa que llama a la función.
*> _Forma: 'INICIO', antes de cargar la ayuda.
*>         'LECTURA', al cargar la ayuda.
*>         'FINAL', al abandonar el programa.

Function VF08c
Parameters _Programa, _Forma

Do Case
   *> Filtrado aplicable a programas concretos.
   Case _Programa = '??????'
      Do Case
      EndCase

   *> Filtrado aplicable de forma general.
   Otherwise
      Do Case
         Case _Forma = 'INICIO'
            If Used('AYUDAF08C')
               Select 'AYUDAF08C'
               =CrtCursor('AYUDAF08C', 'AYUDA', 'C')
               Select AYUDA
               Append From Dbf('AYUDAF08C')
               Go Top
            EndIf

         Case _Forma = 'LECTURA'
            Select AYUDA
            If !Empty(_Procaot)
               Delete All For F08cCodPro <> _Procaot
            EndIf
            Go Top

         Case _Forma = 'FINAL'
            If !Used('AYUDAF08C')
               =CrtCursor('AYUDA', 'AYUDAF08C', 'C')
               Select AYUDAF08C
               Append From Dbf('AYUDA')
            EndIf
      EndCase
EndCase

Return
