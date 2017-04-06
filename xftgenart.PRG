*>
*> Funciones anexas a variables en mantenimientos.
*> Controles a realizar antes y/o después de entrar en cada campo.
*>
*> Recibe:
*>   cAccion   -----> Evento que realiza la llamada.
*>                    'FOCUS', GotFocus/LostFocus.
*>   lModo     -----> .T., al inicio del evento.
*>                    .F., al final del evento.
*>   oObjeto   -----> Nombre del objeto (campo) que llama.
*>
*> Otros:
*>   nStado    -----> Variable definida en el campo. Return nStado
*> _itroldval  -----> Valor anterior del campo.

Parameters cAccion, lModo, oObjeto
Local lEstado, lIsUsed
Local lIsUsed, oValor, cValor

Store .T. To lEstado

*> Selección del momento en que se realiza la llamada.
Do Case
   *> Controles a realizar cuando los campos reciban el foco.
   Case cAccion = 'FOCUS'
      *> Recalcular volumen unitario.
      If Upper(oObjeto.Name) = 'F08CMATLAR' .Or. ;
         Upper(oObjeto.Name) = 'F08CMATALT' .Or. ;
         Upper(oObjeto.Name) = 'F08CMATANC'
         *>
      EndIf

   *> Resto de casos: Ignorar.
   Otherwise

EndCase

Return lEstado
