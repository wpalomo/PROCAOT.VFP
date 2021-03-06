*>
*> Funciones anexas a auditor�a de art�culos.
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
      *> Cargar los valores por defecto, si hay.
      cKey = 'AUDITPARM'
      cSeccion = _UsrCod
      cFile = Sys(5) + CurDir() + AllTrim(cPantalla) + '.INI'

      oPRC = CreateObject('foxapi')
      cAuditParm = oPRC.GetKeyIni(cKey, cSeccion, cFile)

      *> Relacionar par�metros con Source en pantalla.
      cAuditParm = PadR(cAuditParm, 8, Space(1))

      CodBar = SubStr(cAuditParm, 1, 1)
      TipPro = SubStr(cAuditParm, 2, 1)
      TipArt = SubStr(cAuditParm, 3, 1)
      FacPal = SubStr(cAuditParm, 4, 1)
      VolUni = SubStr(cAuditParm, 5, 1)
      PesUni = SubStr(cAuditParm, 6, 1)
      UbiPic = SubStr(cAuditParm, 7, 1)
      UniMed = SubStr(cAuditParm, 8, 1)

      Release oPRC

   *> Controles a realizar al descargar el formulario.
   Case cAccion = 'CERRAR'

   *> Controles a realizar al confirmar los par�metros del listado.
   Case cAccion = 'PROCESO'
      *> Guardar los valores por defecto, si cal.
      cKey = 'AUDITPARM'
      cSeccion = _UsrCod
      cFile = Sys(5) + CurDir() + AllTrim(cPantalla) + '.INI'

      oPRC = CreateObject('foxapi')
      cAuditParm = CodBar + TipPro + TipArt + FacPal + VolUni + PesUni + UbiPic + UniMed
      =oPRC.WriteKeyIni(cKey, cSeccion, cAuditParm, cFile)
      Release oPRC

   *> Resto de casos: Ignorar.
   Otherwise

EndCase

Return lEstado
