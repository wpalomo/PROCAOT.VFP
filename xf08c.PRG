*>
*> Funciones anexas a mantenimiento de ficheros.
*> 
*> Recibe:
*>   cAccion   -----> M�dulo que realiza la llamada.
*>                    'ALTA', al pulsar el bot�n "insertar registro".
*>                    'BAJA', al pulsar el bot�n "eliminar registro".
*>                    'BAJA_OK', al confirmar la eliminaci�n del registro.
*>                    'MODI', al pulsar el bot�n "modificar registro".
*>
*>   cModo     -----> .T.: Al entrar. .F.: Al salir.
*>
*>   cPantalla -----> Nombre del programa que realiza la llamada.

Parameters cAccion, cModo, cPantalla
Local lEstado, lIsUsed

Store .T. To lEstado

If Type('cPantalla') # 'C' 
	cPantalla='FTGENART'
EndIf


*> Selecci�n del momento en que se realiza la llamada.
Do Case
   *> Procesos a realizar desde el mantenimiento de art�culos.
   Case cPantalla='FTGENART'
      Do Case
         *> Controles a realizar al intentar dar de alta un art�culo.
         Case cAccion = 'ALTA' .And. cModo==.T.
            lIsUsed = Used('SYSUSER')
            If !lIsUsed
               Use SYSUSER In 0 Alias SYSUSER Order 1 Shared
            EndIf

            Select SYSUSER
            Locate For usr_cod = _UsrCod
            lEstado = Found() .And. usr_niv=='P'

            If !lIsUsed
              Use In SYSUSER
            EndIf

            If !lEstado
               _LxErr = "El usuario " + _UsrCod + " no tiene permiso para dar altas de art�culos" + cr
               Return lEstado
            EndIf

         *> Controles a realizar para dar de baja art�culos.
         Case cAccion = 'BAJA' .And. cModo==.T.
            lIsUsed = Used('SYSUSER')
            If !lIsUsed
               Use SYSUSER In 0 Alias SYSUSER Order 1 Shared
            EndIf

            Select SYSUSER
            Locate For usr_cod = _UsrCod
            lEstado = Found() .And. usr_niv=='P'

            If !lIsUsed
               Use In SYSUSER
            EndIf

            If !lEstado
               _LxErr = "El usuario " + _UsrCod + " no puede dar de baja un art�culo" + cr
               Return lEstado
            EndIf

         *> Controles a realizar para modificar art�culos.
         Case cAccion = 'MODI' .And. cModo==.T.
            lIsUsed = Used('SYSUSER')
            If !lIsUsed
               Use SYSUSER In 0 Alias SYSUSER Order 1 Shared
            EndIf

            Select SYSUSER
            Locate For usr_cod = _UsrCod
            lEstado = Found() .And. (usr_niv=='P' .Or. usr_niv=='A')

            If !lIsUsed
               Use In SYSUSER
            EndIf

            If !lEstado
               _LxErr = "El usuario " + _UsrCod + " no puede modificar un art�culo" + cr
               Return lEstado
            EndIf

         *> Resto de casos: Ignorar.
         Otherwise
      EndCase

   *> Resto de programas, no procesar.
   Otherwise
EndCase

Return lEstado
