
*> Programa de filtrado especial selecci�n de art�culos.

*> _Programa: Nombre del programa que llama a la funci�n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vArtHas
Parameters _Programa, _Forma

Do Case
   *> Modificaci�n masiva de art�culos.
   Case _Programa = 'FTGENMMA'
      Do Case
         *> Tomar solo los art�culos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F08cCodPro, m.CodPro, m.CodPro)
            Go Top
         OtherWise
      EndCase
EndCase

Return
