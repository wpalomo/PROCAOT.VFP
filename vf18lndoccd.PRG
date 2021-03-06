
*> Programa de filtrado especial selecci�n de art�culos.

*> _Programa: Nombre del programa que llama a la funci�n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vF18lNDocCD
Parameters _Programa, _Forma

Do Case
   *> Edici�n de documentos de entrada
   Case _Programa = 'ENTRDOCE'
      Do Case
         *> Tomar solo los art�culos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F24lCodArt, m.F18lCodArt, m.F18lCodArt)
            Go Top
         OtherWise
      EndCase
EndCase

Return
