* Programa de filtrado especial NumEnt
*
*> _Programa: Nombre del programa que llama a la funci¢n.
*> _Forma: 'INICIO', Antes de generar la ayuda.
*>         'LECTURA', Al generar la ayuda.
*>         'FINAL', Antes de abandonar la ayuda.

Function vNumEnt
Parameters _Programa, _Forma
Do Case
   *> Recuento de Mercancía.
   Case _Programa = 'ENTRRECM'
      Do Case
         *> Tomar solo albaranes pendientes de paletizar.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For F18mEstado > '1'
         OtherWise
      EndCase

   *> Paletización de Entradas.
   Case _Programa = 'ENTRPALE'
      Do Case
         *> Tomar solo los albaranes no paletizados.
         Case _Forma = 'LECTURA'
            Select AYUDA
            * Delete For F18mEstado <> '0'
            Delete For F18mEstado > '1'
         OtherWise
      EndCase

   *> Confirmaci¢n de Entradas.
   Case _Programa = 'ENTRCNFE'
      Do Case
         *> Tomar solo los albaranes paletizados.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For F18mEstado # '2'
         OtherWise
      EndCase

   *> Cancelaci¢n de Entradas.
   Case _Programa = 'ENTRCANC'
      Do Case
         *> Tomar solo los albaranes paletizados.
         Case _Forma = 'LECTURA'
            Select AYUDA
            Delete For F18mEstado # '2'
         OtherWise
      EndCase
   Case _Programa = 'ENTRPES'
      Do Case
         *> Tomar solo albaranes pendientes de paletizar.
   *      Case _Forma = 'LECTURA'
   *         Select AYUDA
   *         Delete For F18nTipDoc <> '1400'
   *      OtherWise
     EndCase
      
EndCase

*
Return
