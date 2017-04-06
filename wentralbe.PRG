*>
*> Funciones anexas a generación de albaranes.
*> Controles a realizar al cargar ó descargar el formulario.
*>
*> Recibe:
*>   cAccion   -----> Módulo que realiza la llamada.
*>                    'INICIO', al inicializar el formulario.
*>                    'ABRIR', al cargar el formulario.
*>                    'CERRAR', al descargar el formulario.
*>                    'PROCESO', al pulsar el botón OK.
*>   cModo     -----> Parámetro no utilizado.
*>   cPantalla -----> Nombre del programa que realiza la llamada.
*>                    No utilizado en este programa.

Parameters cAccion, cModo, cPantalla
Private oPRC
Local lEstado, cAuditParm
Local cKey, cSeccion, cFile

Store .T. To lEstado

*> Selección del momento en que se realiza la llamada.
Do Case
   *> Controles a realizar al inicializar el formulario.
   Case cAccion = 'INICIO'
      *> Definir permisos de acceso:
      *> Usuarios 'P', acceso total.
      *> Usuarios 'A', Acceso total, excepto pantalla de detalle.
      lIsUsed = Used('SYSUSER')
      If !lIsUsed
         Use SYSUSER In 0 Alias SYSUSER Order 1 Shared
      EndIf

      Select SYSUSER
      Locate For usr_cod = _UsrCod
      lEstado = Found() .And. (usr_niv=='P' .Or. usr_niv=='A' .Or. usr_niv=='E')

      _ENTRALBERights = .T.   && usr_niv=='P'

      If !lIsUsed
         Use In SYSUSER
      EndIf

      If !lEstado
         _LxErr = "El usuario " + _UsrCod + " no tiene permiso para utilizar el programa" + cr
         Return lEstado
      EndIf

   *> Controles a realizar al cargar el formulario.
   Case cAccion = 'ABRIR'
      *> Definir variables públicas.
      Release All Like _ENTRALBE*
      Public _ENTRALBEPublicidad, _ENTRALBERights

      *> Cargar los valores por defecto del programa.
      oPRC = CreateObject('foxapi')

      cKey = '_PUBLICIDAD'
      cSeccion = '.DEFAULT'
      cFile = Sys(5) + CurDir() + AllTrim(cPantalla) + '.INI'

      _Publicidad = oPRC.GetKeyIni(cKey, cSeccion, cFile)
      Release oPRC

   *> Controles a realizar al descargar el formulario.
   Case cAccion = 'CERRAR'

   *> Controles a realizar al confirmar los parámetros del listado.
   Case cAccion = 'PROCESO'

   *> Resto de casos: Ignorar.
   Otherwise

EndCase

Return lEstado
