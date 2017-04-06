
*> Programa de filtrado especial selecci�n de art�culos.

*> _Programa: Nombre del programa que llama a la funci�n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vIniArt
Parameters _Programa, _Forma

Do Case
   *> Relaci�n de documentos de entrada.
   Case _Programa = 'ENTRRELDE'
      Do Case
         *> Tomar solo los art�culos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F08cCodPro, m.Ini_Pro, m.Fin_Pro)
            Go Top
         OtherWise
      EndCase

   *> Relaci�n de documentos de salida.
   Case _Programa = 'SALIRELDS'
      Do Case
         *> Tomar solo los art�culos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F08cCodPro, m.Ini_Pro, m.Fin_Pro)
            Go Top
         OtherWise
      EndCase
EndCase

Return
