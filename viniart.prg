
*> Programa de filtrado especial selección de artículos.

*> _Programa: Nombre del programa que llama a la funci¢n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vIniArt
Parameters _Programa, _Forma

Do Case
   *> Relación de documentos de entrada.
   Case _Programa = 'ENTRRELDE'
      Do Case
         *> Tomar solo los artículos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F08cCodPro, m.Ini_Pro, m.Fin_Pro)
            Go Top
         OtherWise
      EndCase

   *> Relación de documentos de salida.
   Case _Programa = 'SALIRELDS'
      Do Case
         *> Tomar solo los artículos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F08cCodPro, m.Ini_Pro, m.Fin_Pro)
            Go Top
         OtherWise
      EndCase
EndCase

Return
