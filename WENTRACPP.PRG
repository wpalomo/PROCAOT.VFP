*>
*> Funciones anexas a aceptaci�n de paletizaci�n.
*> Controles a realizar al cargar � descargar el formulario.
*>
*> Recibe:
*>   cAccion   -----> M�dulo que realiza la llamada.
*>                    'INICIO', al inicializar el formulario.
*>                    'ABRIR', al cargar el formulario.
*>                    'CERRAR', al descargar el formulario.
*>                    'PROCESO', al pulsar el bot�n OK.

*>   cModo     -----> Par�metro no utilizado.

*>   cPantalla -----> Nombre del programa que realiza la llamada.
*>                    No utilizado en este programa.

Parameters cAccion, cModo, cPantalla
Private oPRC
Local lEstado, cAuditParm
Local cKey, cSeccion, cFile

Store .T. To lEstado

*> Selecci�n del momento en que se realiza la llamada.
Do Case
   *> Controles a realizar al inicializar el formulario.
   Case cAccion = 'INICIO'

   *> Controles a realizar al cargar el formulario.
   Case cAccion = 'ABRIR'
      *> Definir variables p�blicas.
      Release All Like _ENTRACPP*
      Public _ENTRACPPMaxPalets

      *> Cargar los valores por defecto del programa.
      oPRC = CreateObject('foxapi')

      cKey = 'MAXPALETS'
      cSeccion = '.DEFAULT'
      cFile = Sys(5) + CurDir() + AllTrim(cPantalla) + '.INI'

      _ENTRACPPMaxPalets = oPRC.GetKeyIni(cKey, cSeccion, cFile)
      Release oPRC

   *> Controles a realizar al descargar el formulario.
   Case cAccion = 'CERRAR'

   *> Controles a realizar al confirmar los par�metros del listado.
   Case cAccion = 'PROCESO'

   *> Resto de casos: Ignorar.
   Otherwise

EndCase

Return lEstado
