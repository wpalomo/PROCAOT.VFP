*>
*> Funciones anexas a mantenimiento de art�culos.
*> Controles a realizar al cargar � descargar el formulario.
*>
*> Recibe:
*>   cAccion   -----> M�dulo que realiza la llamada.
*>                    'INICIO', al inicializar el formulario.
*>                    'ABRIR', al cargar el formulario.
*>                    'CERRAR', al descargar el formulario.
*>   cModo     -----> Par�metro no utilizado.
*>   cPantalla -----> Nombre del programa que realiza la llamada.
*>                    No utilizado en mantenimiento de art�culos.

Parameters cAccion, cModo, cPantalla
Local lEstado, lIsUsed

Store .T. To lEstado

*> Selecci�n del momento en que se realiza la llamada.
Do Case
   *> Controles a realizar al inicializar el formulario.
   Case cAccion = 'INICIO'

   *> Controles a realizar al cargar el formulario.
   Case cAccion = 'ABRIR'

   *> Controles a realizar al descargar el formulario.
   Case cAccion = 'CERRAR'

   *> Resto de casos: Ignorar.
   Otherwise

EndCase

Return lEstado
