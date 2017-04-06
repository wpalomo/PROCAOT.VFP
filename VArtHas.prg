
*> Programa de filtrado especial selección de artículos.

*> _Programa: Nombre del programa que llama a la función.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vArtHas
Parameters _Programa, _Forma

Do Case
   *> Modificación masiva de artículos.
   Case _Programa = 'FTGENMMA'
      Do Case
         *> Tomar solo los artículos entre el rango de propietarios.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F08cCodPro, m.CodPro, m.CodPro)
            Go Top
         OtherWise
      EndCase
EndCase

Return
