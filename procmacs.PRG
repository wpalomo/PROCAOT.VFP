*> Cabecera

   *> Descripcio ................. Procesos varios para tratamiento de MACs.
   *> Modul ...................... ProcMacs.PRG
   *> Llenguatge ................. Visual FoxPro 6.0
   *> Sistema Operatiu ........... Windows

*> Notes
*>---------------------TRATAMIENTO MACs----------------------------------
*> Módulos ....................... DividirMvt (Divide un MP y/o MAC en dos).
*>         ....................... UpdateCMac (Actualiza la cabecera del MAC).
*>         ....................... UpdateDMac (Actualiza los MACs de un documento).
*>         ....................... AgruparMacsExp (Agrupar dos MACs en uno solo).
*>         ....................... ChgMMac    (Cambiar el MAC a UN movimiento).

*>
*> Dividir un movimiento en dos.
*> El programa que llama a este procedimiento debe realizar el Commit.
*>
*>   Recibe: NMov - Nº de movimiento a dividir.
*>           Cant - Cantidad del nuevo movimiento.
*>           GMac - Generar movimiento en MAC S/N.
*>
*> Devuelve: NumMov  - Proceso OK.
*>              .F.  - No se pudo dividir. (También devuelve _LxErr).
*>
*> LLamado desde:
*>   - DIVMAC ---> Dividir un MAC en dos.
*>   - DIVMOV ---> Dividir un movimiento en dos.
*>   - ORAFNCRESV.VOLDLG ---> Generar volumetría, desglose MACs.

Procedure DividirMvt
Parameters NMov, Cant
Private f_update, f_where, f_insert, f_select, _okk, CantStr
Private NumMov

   *> Comprobar la cantidad.
   If Cant <= 0
      _LxErr = _LxErr + 'La cantidad NO puede ser cero' + cr
      Return .F.
   EndIf

   NumMov = .F.

   *> Actualizar la nueva cantidad en el MP.
   m.F14cNumMov = NMov
   If f3_seek('F14c')
      *> Comprobar que la cantidad es correcta.
      Select F14c
      Go Top
      If F14cCanFis <= Cant
         _LxErr = _LxErr + 'La cantidad no es correcta (F14c)' + cr
         Return .F.
      EndIf

      CantStr = Str(F14cCanFis - Cant, 7, 0)

      f_where  = "F14cNumMov='" + NMov + _cm
      If !f3_UpdTun('F14c', , 'F14cCanFis', 'CantStr', , f_where, 'N')
         _LxErr = _LxErr + 'No se ha podido actualizar cantidad en MP origen' + cr
         Return .F. 
      EndIf

      *> Obtener nuevo número de movimiento.
      If Type('NumMov')# 'C'
         NumMov = Ora_NewMP()
      EndIf

      *> Insertar nuevo movimiento en F14c.
      Select F14c
      Go Top
      Replace F14cNumMov With NumMov, ;
              F14cCanFis With Cant

      If !f3_InsTun('F14c', , 'N')
         _LxErr = _LxErr + 'No se ha podido desdoblar movimiento (F14c)' + cr
         Return .F.
      EndIf 
   EndIf 

   *> Actualizar cantidad en la lista original.
   m.F26lNumMov = NMov
   If f3_seek('F26l')
      *> Comprobar que la cantidad es correcta.
      Select F26l
      Go Top
      If F26lCanFis <= Cant
         _LxErr = _LxErr + 'La cantidad no es correcta (F26l)' + cr
         Return .F.
      EndIf

      CantStr = Str(F26lCanFis - Cant, 7, 0)
      f_where  = "F26lNumMov='" + NMov + _cm
      If !f3_UpdTun('F26l', , 'F26lCanFis', 'CantStr', , f_where, 'N')
         _LxErr = _LxErr + 'No se ha podido actualizar cantidad en lista origen' + cr
         Return .F. 
      EndIf

      *> Obtener nuevo número de movimiento.
      If Type('NumMov')# 'C'
         NumMov = Ora_NewMP()
      EndIf

      *> Insertar nuevo movimiento en F26l.
      Select F26l
      Go Top
      Replace F26lNumMov With NumMov, ;
              F26lCanFis with Cant
      If !f3_InsTun('F26l', , 'N')
         _LxErr = 'No se puede insertar lista con cantidad ' + Str(Cant2) + cr
         Return .F.
      EndIf 

      *> Actualizar la cabecera de la lista.
      =Ora_AcCbLs(F26l.F26lNumLst)
   EndIf

   *> Dividir el movimiento en F26w.
   m.F26wNMovMp = NMov
   If f3_seek('F26w')
      *> Obtener nuevo número de movimiento.
      If Type('NumMov')# 'C'
         NumMov = Ora_NewMP()
      EndIf

      *> Insertar nuevo movimiento en F26w.
      Select F26w
      Go Top
      Replace F26wNMovMp With NumMov

      If !f3_InsTun('F26w', , 'N')
         _LxErr = _LxErr + 'No se ha podido desdoblar movimiento (F26w)' + cr
         Return .F.
      EndIf 
   EndIf 

Return NumMov

*> --------------------------------------------------------------------------

Function UpdateCMAc
Parameters cNMac
Private cCampos, cFrom, cWhere
Private cSql, cValores, cEstado

cCampos= "Min(F26lEstMov) As EstMov, Count(Distinct F26lEstMov) As Cont"
cFrom  = "F26l" + _em
cWhere = "F26lNumMac='" + cNMac + "'"
cSql   = "Select " + cCampos + " From " + cFrom + " Where " + cWhere

   Err = f3_SqlExec(_aSql, cSql, 'F26vCur')
   If Err <= 0
      _LxErr = _LxErr + 'Error actualizando cabecera de MAC' + cr + ;
               'MENSAJE: ' + Message() + cr
      Return .F.
   EndIf

   Select F26vCur
   Go Top
   If F26vCur.Cont <= 1 .And. F26vCur.EstMov = '3' 
      cEstado='3'
   Else
      cEstado='2'
   EndIf

   cCampos = "F26vEstBul"
   cValores= "cEstado"
   cWhere  = "F26vNumMac='" + cNMac + "'"
   =f3_UpdTun('F26v', , cCampos, cValores, , cWhere, 'N', 'N')

   If Used('F26vCur')
      Use In F26vCur
   EndIf

Return

************************************************************************************
*> Actualizar el estado de los MACs de expedición de un documento de salida.
*> El programa que llama a este procedimiento debe realizar el Commit.
*>
*>   Recibe: cCodPro - Propietario.
*>           cTipDoc - Tipo documento.
*>           cNumDoc - Nº documento.
*>
*> Devuelve:    .T.  - Proceso OK.
*>              .F.  - No se pudo actualizar. (También devuelve _LxErr).
************************************************************************************
Function UpdateDMac
Parameters cCodPro, cTipDoc, cNumDoc

Private cCampos, cFrom, cWhere
Private cSql, cValores, cEstado
Private oFnc, oPrm
Local lStado, lLib, cOldSele

*> Guardar el entorno de trabajo.
cOldSele = Select()

llIb = ("ALISFOUR" $ Upper(Set("ClassLib")))
If !lLib
   Set ClassLib To Alisfour Additive
EndIf

Store .T. To lStado

*> Clase para recálculo de bultos.
oFnc = CreateObject("OraFncResv")
oRsv = CreateObject("OraPrmResv")
oFnc.ObjParm = oRsv
oFnc.ObjParm.Inicializar

*> Asignar parámetros a la clase.
With oFnc.ObjParm
   .DscCPro = cCodPro
   .DscTDoc = cTipDoc
   .DscNDoc = cNumDoc
EndWith

*> Actualizar bultos totales en cabecera de documento de salida.
*> Actualizar situación bultos en F26v/F26w.
lStado = oFnc.VolDoc()				&& Bultos en documento.
lStado = oFnc.VolBulDoc()			&& Estado bultos del documento.

*> Restaurar el entorno de trabajo.
If !lLib
   Release ClassLib Alisfour
EndIf

If !Empty(cOldSele)
   Select (cOldSele)
EndIf

Return lStado

************************************************************************************
*> Agrupar TODOS los movimientos de un MAC en otro. Los dos MACs deben existir.
*> Puede agrupar MACs en estados distintos.
*> El programa que llama a este procedimiento debe realizar el Commit.
*>
*>   Recibe: cNMacOri - Nº de MAC origen.
*>           cNMacDes - Nº de MAC destino.
*>
*> Devuelve: .T. - Proceso OK.
*>           .F. - No se pudo agrupar. (También devuelve _LxErr).
************************************************************************************
Function AgruparMACsExp
Parameters cNMacOri, cNMacDes

Private cWhere, oFnc, oPrm
Local lStado, cOldSele

*> Inicializar entorno de trabajo.
cOldSele = Select()
Store .F. To lStado
_LxErr = ""

*> Validaciones.
If Empty(cNMacOri) .Or. Empty(cNMacDes) .Or. cNMacOri==cNMacDes
   _LxErr = "MAC origen y/o destino incorrectos" + cr
   =Anomalias()
   Return lStado
EndIf

*> Leer datos MAC origen.
Use In (Select('F26vORG'))
cWhere = "F26vNumMac='" + cNMacOri + "'"
lStado = f3_sql("*", "F26v", cWhere, , , "F26vORG")
If !lStado
   If _xier <= 0
      *> Error de BD.
      _LxErr = "Error en acceso a BD (origen)" + cr
   Else
      *> No existe el MAC origen.
      _LxErr = "El MAC origen no existe" + cr
   EndIf

   Use In (Select('F26vORG'))
   If !Empty(cOldSele)
      Select (cOldSele)
   EndIf

   =Anomalias()
   Return lStado
EndIf

*> Leer datos MAC destino.
Use In (Select('F26vDES'))
cWhere = "F26vNumMac='" + cNMacDes + "'"
lStado = f3_sql("*", "F26v", cWhere, , , "F26vDES")
If !lStado
   If _xier <= 0
      *> Error de BD.
      _LxErr = "Error en acceso a BD (destino)" + cr
   Else
      *> No existe el MAC destino.
      _LxErr = "El MAC destino no existe" + cr
   EndIf

   Use In (Select('F26vORG'))
   Use In (Select('F26vDES'))
   If !Empty(cOldSele)
      Select (cOldSele)
   EndIf

   =Anomalias()
   Return lStado
EndIf

Select F26vORG
Go Top
Select F26vDES
Go Top

*> Validar que los MAC sean del mismo documento.
If F26vORG.F26vCodPro + F26vORG.F26vTipDoc + F26vORG.F26vNumDoc # F26vDES.F26vCodPro + F26vDES.F26vTipDoc + F26vDES.F26vNumDoc
   _LxErr = "Los MAC son de distinto documento" + cr

   Use In (Select('F26vORG'))
   Use In (Select('F26vDES'))
   If !Empty(cOldSele)
      Select (cOldSele)
   EndIf

   =Anomalias()
   Return lStado
EndIf

*> Cargar el detalle del MAC origen.
cWhere = "F26wNumMac='" + cNMacOri + "'"
lStado = f3_sql("*", "F26w", cWhere, , , "F26wORG")
If _xier <= 0
   *> Error de BD.
   _LxErr = "Error en acceso a BD (detalle origen)" + cr

   Use In (Select('F26vORG'))
   Use In (Select('F26vDES'))
   If !Empty(cOldSele)
      Select (cOldSele)
   EndIf

   =Anomalias()
   Return lStado
EndIf

*> Objetos de la función de actualización.
oFnc = CreateObject("OraFncResv")        && Procedimientos.
oPrm = CreateObject("OraPrmResv")        && Parámetros.
oFnc.ObjParm = oPrm
oFnc.ObjParm.Inicializar

Store .T. To lStado

Select F26wORG
Go Top
Do While !Eof()      
   lStado = ChgMMac(F26wORG.F26wNMovMP, cNMacOri, cNMacDes, oFnc)
   If !lStado
      *> Error. El mensaje lo devuelve la función.
      Exit
   EndIf

   Select F26wORG
   Skip
EndDo

*> Actualizar bultos en el documento.
If lStado
   *> Borrar, si cal, el bulto origen.
   Use In (Select('F26wORG'))
   cWhere = "F26wNumMac='" + cNMacOri + "'"
   lStado = f3_sql("*", "F26w", cWhere, , , "F26wORG")
   If !lStado
      cWhere = "F26vNumMac='" + cNMacOri + "'"
      lStado = f3_DelTun("F26v", , cWhere, "N")
   EndIf

   With oFnc.ObjParm
      .DscCPro = F26vORG.F26vCodPro
      .DscTDoc = F26vORG.F26vTipDoc
      .DscNDoc = F26vORG.F26vNumDoc
   EndWith

   *> Actualizar datos de bultos.
   With oFnc
      .VolDoc							&& Cabecera documento de salida.
      .ObjParm.VolNbu = cNMacOri
      .VolBul							&& Bulto origen.
      .ObjParm.VolNbu = cNMacDes
      .VolBul							&& Bulto destino.
   EndWith
EndIf

*> Restaurar el entorno de trabajo anterior.
Use In (Select('F26vORG'))
Use In (Select('F26wORG'))
Use In (Select('F26vDES'))

If !Empty(cOldSele)
   Select (cOldSele)
EndIf

Return lStado

************************************************************************************
*> Cambiar el nº de MAC a UN movimiento.
*> El programa que llama a este procedimiento debe realizar el Commit.
*>
*>   Recibe: cNMov - Nº movimiento origen.
*>           cNMac - Nº de MAC destino.
*>           oFnc  - Objeto función actualización (Opcional)
*>
*> Devuelve: .T. - Proceso OK.
*>           .F. - No se pudo cambiar. (También devuelve _LxErr).
************************************************************************************
Function ChgMMAC
Parameters cNMov, cNMacOri, cNMacDes, oFnc
Private cWhere, cCampos, cValores
Local lStado

*> Crear objeto para actualizar bultos en documento de salida.
If Type('oFnc') # 'O'
   oFnc = CreateObject("OraFncResv")        && Procedimientos.
   oPrm = CreateObject("OraPrmResv")        && Parámetros.
   oFnc.ObjParm = oPrm
   oFnc.ObjParm.Inicializar
EndIf

*> Campos
*>Variables con los valores
cValores= "cNMacDes,cNMacOri"

*> Cambiar Nº de Mac de Movimiento Pendiente.
cCampos = "F14cNumMac,F14cNumMas"
cWhere = "F14cNumMov='" + cNMov + "'"
lStado = f3_UpdTun('F14c', , cCampos, cValores, , cWhere, 'N')

*> Cambiar Nº de Mac de detalle de lista de trabajo.
cCampos = "F26lNumMac,F26lNumMas"
cWhere = "F26lNumMov='" + cNMov + "'"
lStado = f3_UpdTun('F26l', , cCampos, cValores, , cWhere, 'N')

*> Cambiar línea de detalle del MAC.
cCampos = "F26wNumMac"
cValores= "cNMacDes"
cWhere = "F26wNMovMP='" + cNMov + "'"
lStado = f3_UpdTun('F26w', , cCampos, cValores, , cWhere, 'N')

Return lStado
