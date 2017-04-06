*> Cabecera

   *> Descripcio ................. Procesos varios para confirmaci�n de carga.
   *> Modul ...................... SaexCfca.PRG
   *> Llenguatge ................. Visual FoxPro 6.0
   *> Sistema Operatiu ........... Windows

*> Notes
*>---------------------TRATAMIENTO MACs----------------------------------
*> M�dulos........................ CnfC_Chiste        (Chiste rutas en Conf.Carga)
*>                                 CnfC_ProcDoc       (Procesar documento)
*>                                 CnfC_GrabRut       (Grabar cursor rutas en Conf.Carga)
*>                                 CnfC_GrabDoc       (Grabar cursor docs. en Conf.Carga)
*>                                 CnfC_ActStk        (Actualizar stock, ocupac. y ubic.)
*>                                 CnfC_ActTrn        (Actualizar stock en tr�nsito)
*>                                 CnfC_GenAlb        (Generar albar�n para rutas (no impl.)
*>                                 CnfC_MovParm       (Pasar par�metros a OraFncActz)

*>===============================================================
*> Realizar la confirmaci�n de carga de un documento.
*> El control de errores, as� como el Commit/RollBack se deben
*> realizar desde el programa principal.
*>
*> A�adir rec�lculo de stocks. AVC - 04.01.2001
*>
*> Recibe: CodPro - Propietario.
*>         TipDoc - Tipo documento.
*>         NumDoc - N�mero documento.
*>
*> Devuelve: .T./.F.
*>           _LxErr - Incidencias.
*>
*> Llamado desde: SAEXGRUT -----> Generaci�n de Hojas de Ruta.
*>===============================================================
Function CnfC_ProcDoc
Parameter CodPro, TipDoc, NumDoc
Private f_Where
Private  FncA, PrmA 

   =CrtCursor('F14c', 'CnfCRutF14c', 'C')
   =CrtCursor('F24l', 'CnfCRutF24l', 'C')

   b_ErrAct = .F.

   *> Crear cursor con los MPs de salida de este documento.
   f_Where = "F14cTipMov Like '2%' And " + ;
             "F14cCodPro='" + CodPro + "' And " + ;
             "F14cTipDoc='" + TipDoc + "' And " + ;
             "F14cNumDoc='" + NumDoc + "'"

   =f3_sql('*', 'F14c', f_Where, , , 'CnfCRutF14c')

   *> Comprobar que el documento se puede procesar:
   *> Listas confirmadas, MPs en lista, no bloqueos, ...
   _LxErr = ''
   If !CnfC_Chiste('CnfCRutF14c', CodPro, TipDoc, NumDoc)
      _LxErr = _LxErr + 'Documento ' + CodPro + '-' + TipDoc + '-' + NumDoc + ;
                        ' no procesable' + cr + cr + _LxErr
      Return .F.
   EndIf
   
   Select CnfCRutF14c
   Go Top
   If Eof()
      Return .T.                                && No hay datos.
   EndIf

   *> Crear objeto funci�n de actualizaci�n.
   FncA = CreateObject("OraFncActz")
   PrmA = CreateObject("OraPrmActz")
   PrmA.Inicializar
   FncA.ObjParm = PrmA

   Do While !Eof()
      Do CnfC_MovParm With 'CnfCRutF14c'        && Mover a par�metros funci�n actualizaci�n.
         
      =WaitWindow ('Generando movimiento de salida de muelle: ' + ;
                  F14cNumDoc + Space(1) + ;
                  F14cUbiOri + Space(1) + ;
                  F14cCodArt + Space(1) + ;
                  F14cNumDoc)

      *> Generar registro en hist�rico movimientos.----------------
      FncA.ObjParm.PWCRtn = Space(2)
      FncA.ObjParm.PMNMov = Space(10)
      FncA.ObjParm.PMFgHM='S'
      FncA.ActHM
      If FncA.ObjParm.PWCRtn >= '50'
         _LxErr = _LxErr + 'Error generando hist�rico de movimientos' + cr
         b_ErrAct = .T.
      EndIf

      =WaitWindow ('Actualizando situaciones de stock: ' + ;
                  F14cNumDoc + Space(1) + ;
                  F14cUbiOri + Space(1) + ;
                  F14cCodArt + Space(1) + ;
                  F14cSitStk)

      *> Actualizar situaci�n de stock reservado.
      FncA.ObjParm.PWCRtn = Space(2)
      Do CnfC_ActStk
      If FncA.ObjParm.PWCRtn >= '50'
         _LxErr = _LxErr + 'Error actualizando stock reservado' + cr
         b_ErrAct = .T.
      EndIf

*     *> Actualizar situaci�n de stock en tr�nsito.
*     FncA.ObjParm.PWCRtn = Space(2)
*     Do CnfC_ActTrn
*     If FncA.ObjParm.PWCRtn >= '50'
*        _LxErr = _LxErr + 'Error actualizando stock en tr�nsito' + cr
*        b_ErrAct = .T.
*     EndIf

      *> Actualizar l�nea de detalle del documento.
      f_Where = "F24lCodPro = '" + CnfCRutF14c.F14cCodPro + "' And " + ;
                "F24lTipDoc = '" + CnfCRutF14c.F14cTipDoc + "' And " + ;
                "F24lNumDoc = '" + CnfCRutF14c.F14cNumDoc + "' And " + ;
                "F24lLinDoc = '" + PadL(AllTrim(Str(CnfCRutF14c.F14cLinDoc, 4, 0)), 4, '0') + _cm

      =f3_sql('*', 'F24l', f_Where, , , 'CnfCRutF24l')

      CanRes = CnfCRutF24l.F24lCanRes - CnfCRutF14c.F14cCanFis
      CanEnv = CnfCRutF24l.F24lCanEnv + CnfCRutF14c.F14cCanFis
      Sw = F3_UpdTun('F24l', , 'F24lCanRes, F24lCanEnv', 'CanRes, CanEnv', , f_Where, 'N', 'N')
      If Sw = .F.
         _LxErr = _LxErr + 'Error actualizando cantidad enviada en detalle documento' + cr
         b_ErrAct = .T.
      EndIf
         
      =WaitWindow ('Borrando MP de expedici�n: ' + F14cNumDoc + Space(1) + F14cNumMov)

      *> Borrar movimiento pendiente '2999'.
      FncA.ObjParm.PWCRtn = Space(2)
      FncA.ObjParm.PMNMov = CnfCRutF14c.F14cNumMov
      FncA.DelMP
      If FncA.ObjParm.PWCRtn >= '50'
         _LxErr = _LxErr + 'Error borrando movimiento pendiente de expedici�n' + cr
         b_ErrAct = .T.
      EndIf

*      *> Actualizar stock reservado en muelle.
*      If !RStkCRes(CnfCRutF14c.F14cCodPro, CnfCRutF14c.F14cCodArt, .T.)

      *> Actualizar stock reservado en ocupaci�n.
      If !RStkCResOcu(CnfCRutF14c.F14cCodPro, ;
                      CnfCRutF14c.F14cCodArt, ;
                      CnfCRutF14c.F14cNumLot, ;
                      CnfCRutF14c.F14cFecCad, ;
                      CnfCRutF14c.F14cNumPal, ;
                      CnfCRutF14c.F14cSitStk, ;
                      CnfCRutF14c.F14cUbiOri, .T.)
         Do Form St3Inc With .T.
      EndIf

      *> Leer el siguiente MP del documento.
      Select CnfCRutF14c
      Skip
   EndDo

   =WaitWindow ('Actualizando estado cabecera de documento de salida: ' + NumDoc)

   *> Actualizar cabecera de documento.----------------------------
   Estado = '6'
   x_Where = "F24cCodPro='" + CodPro + "' And " + ;
             "F24cTipDoc='" + TipDoc + "' And " + ;
             "F24cNumDoc='" + NumDoc + "'"

   Sw = F3_UpdTun('F24c', , 'F24cFlgEst', 'Estado', , x_Where, 'N')
   If Sw = .F.
      b_ErrAct = .T.
      _LxErr = _LxErr + 'Error actualizando estado cabecera de documento' + cr
   EndIf

   *> Control de incidencias.
   If !Empty(_LxErr)
     _LxErr = 'Errores durante la confirmaci�n de carga' + cr + _LxErr
   EndIf

   *> Borrar objetos de actualizaci�n.
   Release FncA
   Release PrmA

   *> Borrar cursores de trabajo.
   If Used('CnfCRutF14c')
      Use In CnfCRutF14c
   EndIf
   If Used('CnfCRutF24l')
      Use In CnfCRutF24l
   EndIf

Return .T.

*>===============================================================
*> Comprobar que se puede confirmar la extracci�n de un documento.
*> Recibe: FichOri - Nombre del cursor con los MPs.
*>         CodPro  - Propietario.
*>         TipDoc  - Tipo documento.
*>         NumDoc  - N�mero documento.
*>===============================================================
Function CnfC_Chiste
Parameter FichOri, CodPro, TipDoc, NumDoc

   Store "0" To In51, In52, In53
   Store .F. To In99
   Store '' To _LxErr
   
   Select (FichOri)
   Go Top

   Locate For F14cCodPro==CodPro .And. F14cTipDoc==TipDoc .And. F14cNumDoc==NumDoc
   Do While Found() .And. (In51=='0' .Or. In52=='0' .Or. In53=='0')

      If F14cTipMov = '2999'         && MP de extracci�n: Ignorar.
         Continue
         Loop
      EndIf

      If Empty(F14cNumLst)
         In51 = "1"                  && Pendiente de confirmar lista.
         _LxErr = _LxErr + 'El documento: ' + ;
                  CodPro + '-' + ;
                  TipDoc + '-' + ;
                  NumDoc + ' tiene MPs pendientes de confirmar lista de trabajo' + cr
      Else
         In53 = "1"                  && Pendiente de asignar a lista.
         _LxErr = _LxErr + 'El documento: ' + ;
                  CodPro + '-' + ;
                  TipDoc + '-' + ;
                  NumDoc + ' tiene MPs pendientes de asignar a lista de trabajo' + cr
      EndIf

      If F14cEstMov = "B"            && MP bloqueado.
         In52 = "1"
         _LxErr = _LxErr + 'El documento: ' + ;
                  CodPro + '-' + ;
                  TipDoc + '-' + ;
                  NumDoc + ' tiene MPs bloqueados' + cr
      EndIf

      Select (FichOri)
      Continue
   EndDo

   *> Status de salida.
   In99 = In51=='0' .And. In52=='0' .And. In53=='0'

Return In99

*>======================================================================
*> CnfC_GrabRut ............. Grabar cursor rutas en Confirmaci�n Carga
*>======================================================================
Procedure CnfC_GrabRut
Private c_DesRut
   Select CnfRut
   Locate For RutHab = RutAct .And. FecEnt = FEnAct
   If !Found()
      *> Buscar descripci�n de la ruta.-------------------------------
      c_DesRut = ''
      m.F00lCodRut = RutAct
      =f3_seek('F00l', , , 'c_DesRut = m.F00lDescri')
      
      Append Blank
      Replace Sel With 0, ;
              RutHab With RutAct, ;
              DesRut With c_DesRut, ;
              FecEnt With FEnAct, ;
              UbiExp With c_UbiExp, ;
              DocRut With 0, ;
              RecNum With RecNo()
   EndIf
 
   Replace DocRut With DocRut + n_DocRut,  ;
                       NoAsig With IIf(In54 = '1', 'X', NoAsig), ;
                       MovBlq With IIf(In55 = '1', 'X', MovBlq), ;
                       PteLst With IIf(In56 = '1', 'X', PteLst)

   _RecNo = RecNo()

   *> Actualizar cursor documentos asociados a la ruta.
   Select CNFRUTDOC
   Replace Sel With 0, ;
           RutHab With RutAct, ;
           FecEnt With FEnAct, ;
           UbiExp With c_UbiExp, ;
           DocRut With 0, ;
           RecNum With _RecNo ;
      For RecNum = 0
Return

*>===========================================================================
*> CnfC_GrabDoc ............. Grabar cursor documentos en Confirmaci�n Carga
*>===========================================================================
Procedure CnfC_GrabDoc

   Select CNFDOC
   Locate For CodPro = ProAct .And. TipDoc = TDoAct .And. NumDoc = NDoAct

   If !Found()
      Append Blank
      Replace Sel With 0, ;
              CodPro With ProAct, ;
              TipDoc With TDoAct, ;
              NumDoc With NDoAct, ;
              CodCli With CliAct, ;
              OrdRut With n_OrdRut, ;
              RutHab With CNFRUT.RutHab, ;
              CodTra With TraAct, ;
              UbiExp With c_UbiExp, ;
              FecEnt With CNFRUT.FecEnt
   EndIf
 
   Replace NoAsig With IIf(In51 = '1', 'X', NoAsig), ;
           MovBlq With IIf(In52 = '1', 'X', MovBlq), ;
           PteLst With IIf(In53 = '1', 'X', PteLst)
Return

*>===========================================================================
*> CnfC_MovParm ............. Mover par�metros a funci�n de ubicaci�n
*>===========================================================================
Procedure CnfC_MovParm
Parameters FichOri
Private CnfCF16c, n_Peso, n_Volu

Select (FichOri)

*> Datos Ubicaciones
FncA.ObjParm.PUbOld=F14cUbiOri	        && Ubicaci�n nueva
FncA.ObjParm.PUFlag='N'	                && Flag actualizar ubicaci�n ('S' o 'N')

*> Datos Ocupaciones
FncA.ObjParm.POTDoc=F14cTipDoc			&& Tipo documento
FncA.ObjParm.PONDoc=F14cNumDoc			&& N�mero documento
FncA.ObjParm.POLDoc=F14cLinDoc			&& L�nea documento
* FncA.ObjParm.POTEnt=F14cTipEnt        && Tipo entidad
* FncA.ObjParm.POCEnt=F14cCodEnt        && C�digo entidad
FncA.ObjParm.PONPed=F14cNumPed			&& N�mero pedido
FncA.ObjParm.POLPed=F14cLinPed			&& L�nea pedido
FncA.ObjParm.POFMov=Date()    			&& Fecha movimiento

FncA.ObjParm.POCPro=F14cCodPro			&& C�digo propietario
FncA.ObjParm.POCArt=F14cCodArt			&& C�digo art�culo
FncA.ObjParm.PONLot=F14cNumLot			&& N�mero lote
FncA.ObjParm.POSStk=F14cSitStk			&& Situaci�n stock
FncA.ObjParm.POFCad=F14cFecCad			&& Fecha caducidad
FncA.ObjParm.POCFis=F14cCanFis			&& Cantidad f�sica
FncA.ObjParm.POCAlm=F14cCodAlm			&& C�digo almac�n
FncA.ObjParm.POCUbi=F14cUbiOri			&& C�digo ubicaci�n
FncA.ObjParm.PONPal=F14cNumPal			&& N�mero palet
FncA.ObjParm.POTPal=F14cTipPal			&& Tipo palet

FncA.ObjParm.POFUni=F14cUniVen			&& Factor unidad (unidades venta)
FncA.ObjParm.POFSer=F14cUniPac			&& Factor servicio (unidades / pack)
FncA.ObjParm.POFEnv=F14cPacCaj			&& Factor envase (packs / cajas)
FncA.ObjParm.POFPal=F14cCajPal			&& Factor palet (cajas / palet)
FncA.ObjParm.POFFab=F14cFecFab  		&& Fecha fabricaci�n
FncA.ObjParm.POFCal=F14cFecCal			&& Fecha calidad
FncA.ObjParm.PONAna=F14cNumAna			&& N�mero an�lisis
FncA.ObjParm.PONEnt=F14cNumEnt			&& N�mero entrada
FncA.ObjParm.POCRes=F14cCanFis			&& Cantidad reservada

*> Calcular volumen ocupado y si es pico.-----------------------------
Store 0 To n_Pes, n_Vol
Do PesVolAr In Ora_Ca00 With F14cCodPro, F14cCodArt, F14cCanFis, n_Pes, n_Vol
FncA.ObjParm.POVOcu = n_Vol		        && Volumen ocupado
	
Select (FichOri)
CanMov=FncA.ObjParm.POFUni * FncA.ObjParm.POFSer * FncA.ObjParm.POFEnv * FncA.ObjParm.POFPal
FncA.ObjParm.POPico='N'
If CanMov > F14cCanFis  
   FncA.ObjParm.POPico='S'              && Pico S/N
EndIf
FncA.ObjParm.POFlag='N'	&&Flag actualizar ocupaci�n ('S' o 'N')

*> Datos Situaciones Stock
FncA.ObjParm.PSCPro=F14cCodPro			&& C�digo propietario
FncA.ObjParm.PSCArt=F14cCodArt			&& C�digo art�culo
FncA.ObjParm.PSCAlm=F14cCodAlm			&& C�digo almac�n
FncA.ObjParm.PStOld=F14cSitStk			&& Situaci�n stock antigua
FncA.ObjParm.PStNew=Space(4)			&& Situaci�n stock nueva
FncA.ObjParm.PSCFis=F14cCanFis			&& Cantidad
FncA.ObjParm.PSFlag='N'					&& Flag actualizar situaci�n stock ('S' o 'N')

*> Datos Movimientos
FncA.ObjParm.PMTMov=F14cTipMov			&& Tipo movimiento
FncA.ObjParm.PMNMov=Space(10)			&& N�mero movimiento
FncA.ObjParm.PMFMov=F14cFecMov			&& Fecha movimiento
FncA.ObjParm.PMFDoc=F14cFecDoc			&& Fecha documento
FncA.ObjParm.PMRHab=F14cRutHab			&& Ruta habitual
FncA.ObjParm.PMCOpe=F14cCodOpe			&& C�digo operario
FncA.ObjParm.PMNLst=F14cNumLst			&& N�mero lista
FncA.ObjParm.PMNExp=F14cNumExp			&& N�mero expedici�n
FncA.ObjParm.PMORec=F14cOrdRec			&& Orden recorrido
FncA.ObjParm.PMStat=F14cEstMov			&& Estado movimiento
FncA.ObjParm.PMORes=F14cOriRes			&& Origen reserva
FncA.ObjParm.PMTUbi=F14cTipUbi			&& Tipo ubicaci�n
FncA.ObjParm.PMTMAs=F14cTipMAs			&& Tipo MAC asociado
FncA.ObjParm.PMNMAs=F14cNumMAs			&& N�mero MAC asociado
FncA.ObjParm.PMMUni=F14cMacUni			&& MAC unido
FncA.ObjParm.PMORut=F14cOrdRut			&& Orden ruta
FncA.ObjParm.PMTERe=F14cTEntRe			&& Tipo entidad reexpedici�n
FncA.ObjParm.PMCERe=F14cCEntRe			&& C�digo entidad reexpedici�n
FncA.ObjParm.PMVHab=F14cVenHab			&& Vendedor habitual
FncA.ObjParm.PMTMac=F14cTipMac			&& Tipo MAC
FncA.ObjParm.PMNMac=F14cNumMac			&& N�mero MAC
FncA.ObjParm.PMSecc=F14cSeccio			&& Secci�n
FncA.ObjParm.PMFgMP='N'					&& Flag actualizar movimiento pendiente ('S' o 'N')

*> M�s campos (para hist�rico de movimientos)

FncA.ObjParm.PMEnSa='S'       			&& Entrada / Salida
FncA.ObjParm.PMMvMP=F14cNumMov			&& N�mero Movimiento MP para HM
FncA.ObjParm.PMDAso=F14cDirAso			&& Direcci�n asociada
FncA.ObjParm.PMFEnv=_FecMin				&& Fecha env�o
FncA.ObjParm.PMFOrd=_FecMin             && Fecha orden
FncA.ObjParm.PMFgHM='N'                 && Flag actualizar hist�rico de movimientos ('S' o 'N')

Return

*>===========================================================================
*> CnfC_ActStk............... Actualizar stocks, ocupaciones y ubicaciones
*>===========================================================================
Procedure CnfC_ActStk
Private c_Error
   c_Error = Space(2)
   
   *> Restar de stock reservado.--------------------------------------
   Select CnfCRutF14c
   FncA.ObjParm.PWCRtn = Space(2)

   With FncA.ObjParm
      .PTAcc='08'
      .PUbOld=F14cUbiOri
      .PUFlag='N'
      .POSStk=F14cSitStk
      .POCFis=0
      .POCRes=F14cCanFis
      .POFlag='S'
      .PSTOld='2000'
      .PSCFis=F14cCanFis
      .PSFlag='S'
   EndWith

   FncA.Ejecutar

   c_Error = FncA.ObjParm.PWCRtn

   *> Restar de stock f�sico.-----------------------------------------
   Select CnfCRutF14c
   FncA.ObjParm.PWCRtn = Space(2)

   With FncA.ObjParm
      .PTAcc='08'
      .PUbOld=F14cUbiOri
      .PUFlag='S'
      .POSStk=F14cSitStk
      .POCFis=F14cCanFis
      .POCRes=0
      .POFlag='S'
*     .PSTOld='3000'
*     .PSCFis=F14cCanFis
      .PSFlag='N'             && ***** No actualiza stock????
   EndWith

   FncA.Ejecutar

   If FncA.ObjParm.PWCRtn < c_Error
      FncA.ObjParm.PWCRtn = c_Error
   EndIf

Return 

*>===========================================================================
*> CnfC_ActTrn............... Actualizar stocks tr�nsito
*>===========================================================================
Procedure CnfC_ActTrn
   Select CnfCRutF14c
   With FncA.ObjParm
      .PTAcc='07'
      .PUFlag='N'
      .POCFis=0
      .POCRes=0
      .POFlag='N'
      .PSTOld='6000'
      .PSCFis=F14cCanFis
      .PSFlag='S'
   EndWith
   FncA.Ejecutar
Return

*>===========================================================================
*> CnfC_GenAlb............... Generar albar�n para rutas
*>===========================================================================
Function CnfC_GenAlb
Parameters c_CodPro, c_TipDoc, c_NumDoc

*   FncA.ObjParm.PMNMov=Space(10)
*   FncA.ObjParm.PMFgHM='S'
*   FncA.ActHM
Return .T.
