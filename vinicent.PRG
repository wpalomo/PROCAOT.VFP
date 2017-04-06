
*> Programa de filtrado especial Entidad

*> _Programa: Nombre del programa que llama a la funci¢n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vIniCEnt
Parameters _Programa, _Forma

Do Case
   *> Relación de documentos de entrada.
   Case _Programa = 'ENTRRELDE'
      Do Case
         *> Tomar solo las entidades entre el rango de tipos de entidad.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For !Between(F01cTipEnt, m.Ini_TEnt, m.Fin_TEnt)
            Go Top
         OtherWise
      EndCase
EndCase

*
Return
