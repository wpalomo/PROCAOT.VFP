Define Class oraprmactz as custom



PROCEDURE inicializar

*> Inicializar propiedades clase actualización.

With This
	.PTAcc  = Space(2)

	*> Datos Ubicaciones
	.PUbOld = Space(14)
	.PUbNew = Space(14)
	.PUFlag  = Space(1)

	*> Datos Ocupaciones
	.POTDoc = Space(4)
	.PONDoc = Space(13)
	.POLDoc = 0
	.POTEnt = Space(4)
	.POCEnt = Space(6)
	.PONPed = Space(13)
	.POLPed = 0
	.PODAso = Space(13)
	.POFMov = CToD(Space(8))
	.POFMovOra = CToD(Space(8))
	.POCPro = Space(6)
	.POCArt = Space(13)
	.PONLot = Space(15)
	.POSStk = Space(4)
	.POFCad = CToD(Space(8))
	.POFCadOra = CToD(Space(8))
	.POCFis = 0
	.POPAct = 0
	.POCAlm = Space(14)
	.POCUbi = Space(14)
	.PONPal = Space(10)
	.POTPal = Space(4)
	.POFUni = 0
	.POFSer = 0
	.POFEnv = 0
	.POFPal = 0
	.POFFab = CToD(Space(8))
	.POFDoc = CToD(Space(8))
	.POFFabOra = CToD(Space(8))
	.PONEnt = Space(10)
	.POFEnt = CToD(Space(8))
	.POFEntOra = CToD(Space(8))
	.POFCal = CToD(Space(8))
	.POFCalOra = CToD(Space(8))
	.PONAna = Space(6)
	.PONotE = Space(10)
	.POCRes = 0
	.POVOcu = 0
	.POKOcu = 0
	.POPico = Space(1)
	.POCrd1 = Space(1)
	.POCrd2 = Space(2)
	.POFlag = Space(1)
	.PORowId= Space(10)
	.PORowIdNew= Space(10)

	*> Datos Situaciones Stock
	.PSCPro = Space(6)
	.PSCArt = Space(13)
	.PSCAlm = Space(4)
	.PStOld = Space(4)
	.PStNew = Space(4)
	.PSCFis = 0
	.PSFlag = Space(1)

	*> Datos Movimientos
	.PMTMov = Space(4)
	.PMNMov = Space(10)
	.PMFMov = CToD(Space(8))
	.PMFMovOra = CToD(Space(8))
	.PMFDoc = CToD(Space(8))
	.PMFDocOra = CToD(Space(8))
	.PMRHab = Space(4)
	.PMCOpe = Space(4)
	.PMNLst = Space(6)
	.PMNExp = Space(6)
	.PMORec = Space(6)
	.PMStat = "0"
	.PMORes = Space(1)
	.PMTUbi = Space(1)
	.PMTMAs = Space(4)
	.PMNMAs = Space(9)
	.PMMUni = Space(1)
	.PMORut = 0
	.PMTERe = Space(4)
	.PMCERe = Space(6)
	.PMVHab = Space(6)
	.PMTMac = Space(4)
	.PMNMac = Space(9)
	.PMSecc = Space(2)
	.PMFlg1 = Space(1)
	.PMFlg2 = Space(1)
	.PMFgMP = Space(1)

	*> Más campos (para histórico de movimientos)
	.PMMvMP = Space(10)
	.PMEnSa = Space(1)
	.PMDAso = Space(13)
	.PMUDes = Space(14)
	.PMCTra = Space(4)
	.PMBloq = Space(1)
	.PMFlgE = Space(1)
	.PMFEnv = CToD(Space(8))
	.PMFEnvOra = CToD(Space(8))
	.PMNEnv = 0
	.PMFOrd = CToD(Space(8))
	.PMFOrdOra = CToD(Space(8))
	.PMFgHM = Space(1)

	*> Datos Albaranes
	.PAFAlb = CToD(Space(8))
	.PAFAlbOra = CToD(Space(8))
	.PAHEnt = Space(8)
	.PAHSal = Space(8)
	.PANAlb = Space(13)
	.PAAlbN = 'N'
	.PACtrG = 'N'
	.PACtrF = 'N'
	.PADrgA = Space(4)
	.PAFrio = Space(4)

	*> Otros Datos
	.PWUser = Space(10)
	.PWPrgm = Space(20)
	.PWFree = Space(1)
	.PWCRtn = Space(7)

	*> Filler
	.PWFill = Space(200)

	*> Varios
	.OF16cPr = .F.
	.OF16cPo = .F.
EndWith

ENDPROC
PROCEDURE cargarparametros

*> Cargar propiedades de la clase a partir de un origen de datos externo.
*> Recibe:
*>	- Origen (L)ista, (M)ovimiento pendiente, (O)cupación.
*>	- Modo carga datos:
*>		"": Nada. Toma la tabla activa.
*>		Objeto: Crea cursor con el objeto recibido.

Parameters cOrigen, cCargarDatos

Do Case
	*> Origen: Detalle de una lista de trabajo.
	Case cOrigen=='L'
		If Type('cCargarDatos')=='O'
			=CrtCursor("F26l", "F26lCargarLS", "C")
			Select F26lCargarLS
			Append Blank
			Gather Name cCargarDatos
		EndIf
		This._cargarparametrosls
		Use In (Select("F26lCargarLS"))

	*> Origen: Movimiento pendiente.
	Case cOrigen=='M'
		If Type('cCargarDatos')=='O'
			=CrtCursor("F14c", "F14cCargarMP", "C")
			Select F14cCargarMP
			Append Blank
			Gather Name cCargarDatos
		EndIf
		This._cargarparametrosmp
		Use In (Select("F14cCargarMP"))

	*> Origen: Ocupación.
	Case cOrigen=='O'
		If Type('cCargarDatos')=='O'
			=CrtCursor("F16c", "F16cCargarOC", "C")
			Select F16cCargarOC
			Append Blank
			Gather Name cCargarDatos
		EndIf
		This._cargarparametrosoc
		Use In (Select("F16cCargarOC"))
EndCase

Return

ENDPROC
PROCEDURE _cargarparametrosls

*> Cargar parámetros en propiedades a partir de los datos de una lista.
*> Recibe:
*>	- Registro activo del detalle de la lista de trabajo.

With This
   *> Datos Ubicación
   .PUbOld = F26lUbiOri			&& Ubicación antigua
   .PUbNew = F26lUbiDes			&& Ubicación nueva

   *> Datos Ocupaciones
   .POTDoc = F26lTipDoc  		&& Tipo documento
   .PONDoc = F26lNumDoc  		&& Número documento

   .POLDoc = F26lLinDoc   		&& Línea documento
   .POTEnt = Space(4)           && Tipo entidad
   .POCEnt = Space(6)           && Código entidad
   .PONPed = F26lNumPed   		&& Número pedido
   .POLPed = F26lLinPed   		&& Línea pedido
   .POFMov = Date()		 		&& Fecha movimiento

   .POCUbi = F26lUbiOri			&& Ubicación
   .POCPro = F26lCodPro   		&& Código propietario
   .POCArt = F26lCodArt   		&& Código artículo
   .PONLot = F26lNumLot   		&& Número lote
   .POSStk = F26lSitStk   		&& Situación stock
   .POFCad = F26lFecCad  		&& Fecha caducidad

   .POCFis = F26lCanFis   		&& Cantidad física
   .POCRes = F26lCanFis   		&& Cantidad reservada
   .POCAlm = Left(F26lUbiOri, 4)&& Código almacén
   .PONPal = F26lNumPal   		&& Número palet
   .POTPal = F26lTipPal   		&& Tipo palet

   .POFUni = F26lUniVen        	&& Factor unidad (unidades venta)
   .POFSer = F26lUniPac        	&& Factor servicio (unidades / pack)
   .POFEnv = F26lPacCaj        	&& Factor envase (packs / cajas)
   .POFPal = F26lCajPal        	&& Factor palet (cajas / palet)
   .POFFab = F26lFecFab         && Fecha fabricación
   .PONEnt = F26lNumEnt        	&& Número entrada
   .POFEnt = Date()             && Fecha entrada
   .POFCal = F26lFecCal      	&& Fecha calidad
   .PONAna = F26lNumAna        	&& Número análisis
   .POVOcu = 0					&& Volumen

   .POPico = 'N'
   If .POFUni * .POFSer * .POFEnv * .POFPal > F26lCanFis
      .POPico='S'				&& Pico S/N
   EndIf

   .POCrd1 = F26lFlag1			&& Flag 1 ocupación
   .POCrd2 = F26lFlag2			&& Flag 2 ocupación

   *> Datos Movimientos
   .PMFDoc = F26lFecDoc  		&& Fecha documento
   .PMRHab = F26lRutHab  		&& Ruta habitual
   .PMCOpe = F26lCodOpe  		&& Código operario
   .PMNLst = F26lNumLst  		&& Número lista
   .PMNExp = F26lNumExp  		&& Número expedición
   .PMORec = F26lOrdRec  		&& Orden recorrido
   .PMStat = F26lEstMov  		&& Estado movimiento
   .PMORes = F26lOriRes  		&& Origen reserva
   .PMTUbi = F26lTipUbi			&& Tipo ubicación
   .PMTMAs = F26lTipMAs			&& Tipo MAC asociado
   .PMNMAs = F26lNumMAs			&& Número MAC asociado
   .PMMUni = F26lMacUni  		&& MAC unido
   .PMORut = F26lOrdRut  		&& Orden ruta
   .PMTERe = F26lTEntRe  		&& Tipo entidad reexpedición
   .PMCERe = F26lCEntRe  		&& Código entidad reexpedición
   .PMVHab = F26lVenHab  		&& Vendedor habitual
   .PMTMac = F26lTipMac  		&& Tipo MAC
   .PMNMac = F26lNumMac  		&& Número MAC
   .PMSecc = F26lSeccio  		&& Sección

   *> Más campos (para histórico de movimientos)
   .PMTMov = F26lTipMov   		&& Tipo movimiento
   .PMDAso = F26lNumMov   		&& Dirección asociada
   .PMDAso = F26lDirAso   		&& Dirección asociada
   .PMFEnv = _FecMin            && Fecha envío
   .PMFOrd = _FecMin     		&& Fecha orden
   .PMNMov = ""         		&& Nº movimiento
   .PMMvMP = F26lNumMov			&& Movimiento del MP
EndWith

Return

ENDPROC
PROCEDURE _cargarparametrosmp

*> Cargar parámetros en propiedades a partir de los datos de un MP.
*> Recibe:
*>	- Registro activo del movimietno pendiente.

With This
   *> Datos Ubicación
   .PUbOld = F14cUbiOri			&& Ubicación antigua
   .PUbNew = F14cUbiDes			&& Ubicación nueva

   *> Datos Ocupaciones
   .POTDoc = F14cTipDoc  		&& Tipo documento
   .PONDoc = F14cNumDoc  		&& Número documento

   .POLDoc = F14cLinDoc   		&& Línea documento
   .POTEnt = Space(4)           && Tipo entidad
   .POCEnt = Space(6)           && Código entidad
   .PONPed = F14cNumPed   		&& Número pedido
   .POLPed = F14cLinPed   		&& Línea pedido
   .POFMov = Date()		 		&& Fecha movimiento

   .POCUbi = F14cUbiOri			&& Ubicación
   .POCPro = F14cCodPro   		&& Código propietario
   .POCArt = F14cCodArt   		&& Código artículo
   .PONLot = F14cNumLot   		&& Número lote
   .POSStk = F14cSitStk   		&& Situación stock
   .POFCad = F14cFecCad  		&& Fecha caducidad

   .POCFis = F14cCanFis   		&& Cantidad física
   .POCRes = F14cCanFis   		&& Cantidad reservada
   .POCAlm = Left(F14cUbiOri, 4)&& Código almacén
   .PONPal = F14cNumPal   		&& Número palet
   .POTPal = F14cTipPal   		&& Tipo palet

   .POFUni = F14cUniVen        	&& Factor unidad (unidades venta)
   .POFSer = F14cUniPac        	&& Factor servicio (unidades / pack)
   .POFEnv = F14cPacCaj        	&& Factor envase (packs / cajas)
   .POFPal = F14cCajPal        	&& Factor palet (cajas / palet)
   .POFFab = F14cFecFab         && Fecha fabricación
   .PONEnt = F14cNumEnt        	&& Número entrada
   .POFEnt = Date()             && Fecha entrada
   .POFCal = F14cFecCal      	&& Fecha calidad
   .PONAna = F14cNumAna        	&& Número análisis
   .POVOcu = 0					&& Volumen

   .POPico = 'N'
   If .POFUni * .POFSer * .POFEnv * .POFPal > F14cCanFis
      .POPico='S'				&& Pico S/N
   EndIf

   .POCrd1 = F14cFlag1			&& Flag 1 ocupación
   .POCrd2 = F14cFlag2			&& Flag 2 ocupación

   *> Datos Movimientos
   .PMFDoc = F14cFecDoc  		&& Fecha documento
   .PMRHab = F14cRutHab  		&& Ruta habitual
   .PMCOpe = F14cCodOpe  		&& Código operario
   .PMNLst = F14cNumLst  		&& Número lista
   .PMNExp = F14cNumExp  		&& Número expedición
   .PMORec = F14cOrdRec  		&& Orden recorrido
   .PMStat = F14cEstMov  		&& Estado movimiento
   .PMORes = F14cOriRes  		&& Origen reserva
   .PMTUbi = F14cTipUbi			&& Tipo ubicación
   .PMTMAs = F14cTipMAs			&& Tipo MAC asociado
   .PMNMAs = F14cNumMAs			&& Número MAC asociado
   .PMMUni = F14cMacUni  		&& MAC unido
   .PMORut = F14cOrdRut  		&& Orden ruta
   .PMTERe = F14cTEntRe  		&& Tipo entidad reexpedición
   .PMCERe = F14cCEntRe  		&& Código entidad reexpedición
   .PMVHab = F14cVenHab  		&& Vendedor habitual
   .PMTMac = F14cTipMac  		&& Tipo MAC
   .PMNMac = F14cNumMac  		&& Número MAC
   .PMSecc = F14cSeccio  		&& Sección

   *> Más campos (para histórico de movimientos)
   .PMTMov = F14cTipMov   		&& Tipo movimiento
   .PMDAso = F14cNumMov   		&& Dirección asociada
   .PMDAso = F14cDirAso   		&& Dirección asociada
   .PMFEnv = _FecMin            && Fecha envío
   .PMFOrd = _FecMin     		&& Fecha orden
   .PMNMov = ""         		&& Nº movimiento
   .PMMvMP = F14cNumMov			&& Movimiento del MP
EndWith

Return

ENDPROC
PROCEDURE _cargarparametrosoc

*> Cargar parámetros en propiedades a partir de los datos de una ocupación.
*> Recibe:
*>	- Registro activo de la ocupación.

With This
   *> Datos Ubicación
   .PUbOld = F16cCodUbi			&& Ubicación antigua
   .PUbNew = F16cCodUbi			&& Ubicación nueva

   *> Datos Ocupaciones
   .POTDoc = F16cTipDoc  		&& Tipo documento
   .PONDoc = F16cNumDoc  		&& Número documento

   .POLDoc = F16cLinDoc   		&& Línea documento
   .POTEnt = F16cTipEnt         && Tipo entidad
   .POCEnt = F16cCodEnt         && Código entidad
   .PONPed = ""			   		&& Número pedido
   .POLPed = 0			   		&& Línea pedido
   .POFMov = Date()		 		&& Fecha movimiento

   .POCUbi = F16cCodUbi			&& Ubicación
   .POCPro = F16cCodPro   		&& Código propietario
   .POCArt = F16cCodArt   		&& Código artículo
   .PONLot = F16cNumLot   		&& Número lote
   .POSStk = F16cSitStk   		&& Situación stock
   .POFCad = F16cFecCad  		&& Fecha caducidad

   .POCFis = F16cCanFis   		&& Cantidad física
   .POCRes = F16cCanFis   		&& Cantidad reservada
   .POCAlm = Left(F16cCodUbi, 4)&& Código almacén
   .PONPal = F16cNumPal   		&& Número palet
   .POTPal = F16cTamPal   		&& Tipo palet

   .POFUni = F16cUniVen        	&& Factor unidad (unidades venta)
   .POFSer = F16cUniPac        	&& Factor servicio (unidades / pack)
   .POFEnv = F16cPacCaj        	&& Factor envase (packs / cajas)
   .POFPal = F16cCajPal        	&& Factor palet (cajas / palet)
   .POFFab = F16cFecFab         && Fecha fabricación
   .PONEnt = F16cNotEnt        	&& Número entrada
   .POFEnt = F16cFecEnt         && Fecha entrada
   .POFCal = _FecMin	      	&& Fecha calidad
   .PONAna = F16cNumAna        	&& Número análisis
   .POVOcu = 0					&& Volumen

   .POPico = 'N'
   If .POFUni * .POFSer * .POFEnv * .POFPal > F16cCanFis
      .POPico='S'				&& Pico S/N
   EndIf

   .POCrd1 = F16cFlag1			&& Flag 1 ocupación
   .POCrd2 = F16cFlag2			&& Flag 2 ocupación

   *> Datos Movimientos
   .PMFDoc = ""           		&& Fecha documento
   .PMRHab = ""			  		&& Ruta habitual
   .PMCOpe = ""  				&& Código operario
   .PMNLst = ""			  		&& Número lista
   .PMNExp = ""			 		&& Número expedición
   .PMORec = ""			  		&& Orden recorrido
   .PMStat = "0"			  	&& Estado movimiento
   .PMORes = ""			  		&& Origen reserva
   .PMTUbi = ""					&& Tipo ubicación
   .PMTMAs = ""					&& Tipo MAC asociado
   .PMNMAs = ""					&& Número MAC asociado
   .PMMUni = ""			  		&& MAC unido
   .PMORut = 0			  		&& Orden ruta
   .PMTERe = ""		  			&& Tipo entidad reexpedición
   .PMCERe = ""			  		&& Código entidad reexpedición
   .PMVHab = ""			  		&& Vendedor habitual
   .PMTMac = ""			  		&& Tipo MAC
   .PMNMac = ""			  		&& Número MAC
   .PMSecc = ""			  		&& Sección

   *> Más campos (para histórico de movimientos)
   .PMTMov = ""			   		&& Tipo movimiento
   .PMDAso = ""			   		&& Dirección asociada
   .PMDAso = ""			   		&& Dirección asociada
   .PMFEnv = _FecMin            && Fecha envío
   .PMFOrd = _FecMin     		&& Fecha orden
   .PMNMov = ""         		&& Nº movimiento
   .PMMvMP = ""					&& Movimiento del MP
EndWith

Return

ENDPROC


EndDefine 
Define Class orafncactz as custom
Height = 17
Width = 101
in83 = .F.



PROCEDURE blkdat
		
*> Inicializar datos de trabajo.
*> Esto ya lo hace This.InitLocals()
*!*	With This
*!*		.In99 = "0"

*!*		.LgSOld = .ObjParm.PStOld
*!*		.LGArtB = 0
*!*		.LGArtA = 0
*!*		.LOFisB = 0
*!*		.LOFisA = 0
*!*		.LOResB = 0
*!*		.LOResA = 0
*!*		.LSFisB = 0
*!*		.LSFisA = 0

*!*		.PEnSa = Space(1)
*!*		.WKOcu = 0
*!*		.WVLib = 0
*!*		.Borro = "N"
*!*		.Sumar = "N"
*!*	EndWith

*> Error si CFis (ocupaciones) negativa
If This.ObjParm.POCFis < 0
   This.ObjParm.PwCRtn = "57"
   This.LgTAno = "POCFIS"
   This.LgCant = This.ObjParm.POCFis
   This.LgProc = "BLKDAT"
   This.Anom
EndIf

*> Error si CRes (ocupaciones) negativa
If This.ObjParm.POCRes < 0 .And. This.ObjParm.PwCRtn < "50"
   This.ObjParm.PwCRtn = "58"
   This.LgTAno = "POCRES"
   This.LgCant = This.ObjParm.POCRes
   This.LgProc = "BLKDAT"
   This.Anom
EndIf

*> Error si CFis (situacion stock) negativa
If This.ObjParm.PSCFis < 0 .And. This.ObjParm.PwCRtn < "50"
   This.ObjParm.PwCRtn = "59"
   This.LgTAno = "PSCFIS"
   This.LgCant = This.ObjParm.PSCFis
   This.LgProc = "BLKDAT"
   This.Anom
EndIf

If This.ObjParm.PwCRtn < "50"
   ***> Iniciar cursores de ficheros necesarios
   =CrtCursor("F16c", "ActzF16c")        && Ocupaciones
   =CrtCursor("F10c", "ActzF10c")        && Ubicaciones
   =CrtCursor("F13c", "ActzF13c")        && Stocks

   ***> Convertir datos de fecha a datos de fecha para búsqueda en la BBDD.
   This.ConvFec
EndIf

Return

ENDPROC
PROCEDURE actzoc

*> Actualización de ocupaciones.

Private Cant2
Private f_Asigna, f_Valores
Local Sw

*> Si Flag actualización no es "S", no se debe actualizar ocupaciones
If This.ObjParm.POFlag <> "S"
   Return
EndIf

This.In98 = "0"

This.LeeOc
If This.ObjParm.PwCRtn >= "50"
   Return
EndIf

*> Guardar valores antes de actualizar para F99c.
Select ActzF16c
Scatter Name This.ObjParm.OF16cPr

*> Proceso actualización según sea ENTRADA o SALIDA
If This.PEnSa = "E"
   *>  ==== ENTRADAS ====
   If This.In82 = "1"
      *> No existe la ocupación: Inicializar registro. Le asigna el RowId.
      This.BlkOc
   EndIf

   *> Calcula si es pico según cantidad y factores de ObjParm, actualizando PoPico.
   This.CalPic

   Select ActzF16c
   Replace F16cCanFis With F16cCanFis + This.ObjParm.POCFis, ;
           F16cCanRes With F16cCanRes + This.ObjParm.POCRes, ;
           F16cVolOcu With F16cVolOcu + This.WVLib, ;
           F16cEsPico With This.ObjParm.POPico
   *       F16cVolOcu With F16cVolOcu + This.ObjParm.POVOcu,

   *> Guardar valores después de actualizar para F99c.
   Select ActzF16c
   Scatter Name This.ObjParm.OF16cPo

   *> Controlar errores con cantidades física y reservada
   If ActzF16c.F16cCanRes < 0
      Replace ActzF16c.F16cCanRes With 0

      This.ObjParm.PwCRtn = "12"
      This.LgTAno = "OCCRE2"
      This.LgCant = ActzF16c.F16cCanRes
      This.LgProc = "ActzOc"
   ***This.Anom
   EndIf

   If ActzF16c.F16cCanRes > ActzF16c.F16cCanFis
      This.In99 = "1"
      This.ObjParm.PwCRtn = "60"
      This.LgTAno = "OCCRE1"
      This.LgCant = ActzF16c.F16cCanRes
      This.LgProc = "ActzOc"
      This.Anom
   EndIf
Else
   *>  ==== SALIDAS ====
   Select ActzF16c
   If This.In82="0"
      If This.ObjParm.POCRes > ActzF16c.F16cCanRes
         This.ObjParm.PwCRtn = "11"
         This.LgTAno = "OCCRE3"
         This.LgCant = This.ObjParm.POCRes
         This.LgProc = "ActzOc"
         This.Anom
      EndIf

      *> Si POCRes=0, salida sin cant.reservada ('08'), por lo que debe tenerse en cuenta
      *> la cantidad reservada anteriormente. Cant2 es la cantidad disponible.
      If This.ObjParm.POCRes = 0
         Cant2 = ActzF16c.F16cCanFis - ActzF16c.F16cCanRes
         If This.ObjParm.POCFis > Cant2
            This.ObjParm.PwCRtn = "56"
            This.LgTAno = "OCCRE4"
            This.LgCant = This.ObjParm.POCRes
            This.LgProc = "ActzOc"
            This.Anom
         EndIf
      EndIf

      If This.ObjParm.PwCRtn <= "50"
         Select ActzF16c
      *  Replace F16cVolOcu With F16cVolOcu - This.ObjParm.POVOcu,
         Replace F16cVolOcu With F16cVolOcu - This.WVLib, ;
                 F16cCanFis With F16cCanFis - This.ObjParm.POCFis, ;
                 F16cCanRes With F16cCanRes - This.ObjParm.POCRes
         If F16cVolOcu < 0
            Replace F16cVolOcu With 0
         EndIf

         *> Controlar que CanRes no sea negativa
         If F16cCanRes < 0
            Replace F16cCanRes With 0
            This.ObjParm.PwCRtn = "12"
            This.LgTAno = "OCCRE5"
            This.LgCant = This.ObjParm.POCRes
            This.LgProc = "ActzOc"
         ***This.Anom
         EndIf

         If F16cCanFis > 0 And This.ObjParm.POCFis > 0
            Replace F16cEsPico With "S"
         EndIf
      EndIf
   Else
      *> No existe la ocupación, aunque debería porque es una salida.
      This.ObjParm.PwCRtn = "61"
      This.LgTAno = "COCUNO"
      This.LgCant = This.ObjParm.POCFis
      This.LgProc = "ActzOc"
      This.Anom
   EndIf
EndIf

If This.ObjParm.PwCRtn <= "50"
   Select ActzF16c
   If F16cCanFis < 0
      This.ObjParm.PwCRtn = "13"
      This.LgTAno = "OCCFIS"
      This.LgCant = F16cCanFis
      This.LgProc = "ActzOc"
      This.Anom
      Replace F16cCanFis With 0
   EndIf

   *> ==== ACTUALIZACION ====
   Select ActzF16c
   This.LoFisA = F16cCanFis
   This.LoResA = F16cCanRes

   *> Recuperar el ID de la ocupación. Si existe, el que tenía. En caso contrario, el nuevo ID.
   This.WRowID = F16cIdOcup

   Do Case
      Case This.In82 = "0"           && Existe
         f_Where = "F16cIdOcup='" + This.WRowID + "'"

         If F16cCanFis <= 0
            *> Ocupación vacía: Eliminar registro.
            Sw = F3_DelTun('F16c', , f_Where, 'N')
            If Sw = .F.
               This.ObjParm.PwCRtn = "80"
               This.LgTAno = "OCUDEL"
               This.LgProc = "ActzOc"
               This.Anom
            EndIf
            This.Borro = "S"
         Else
            f_Asigna  = 'F16cCanFis, F16cCanRes, F16cVolOcu, F16cEsPico'
            f_Valores = f_Asigna
            Sw = F3_UpdTun('F16c', , f_Asigna, f_Valores, 'ActzF16c', f_Where, 'N')
            If Sw = .F.
               This.ObjParm.PwCRtn = "81"
               This.LgTAno = "OCUUPD"
               This.LgProc = "ActzOc"
               This.Anom
            EndIf
         EndIf

      Case This.In82 = "1"           && No Existe
         If This.PEnSa = "E"
            Sw = F3_InsTun('F16c', 'ActzF16c', 'N')
            If Sw = .F.
               This.ObjParm.PwCRtn = "82"
               This.LgTAno = "OCUINS"
               This.LgProc = "ActzOc"
               This.Anom
            EndIf
            This.Sumar = "S"
         EndIf
   EndCase
EndIf

Return

ENDPROC
PROCEDURE leeoc

*> Leer ocupación a partir de datos ocupación (Ubi,Pal,Pro,Art,Lot,Cad,Stk).
*> Leer ocupación a partir del RowID.
*> Modificar para que permita ignorar el Nº de palet y la caducidad si llegan en blanco.

Private f_Where
Local lStado

Select ActzF16c
Zap

Do Case
   *> Buscar ocupación a partir del RowId.
   Case !Empty(This.WRowId)
      f_Where = "F16cIdOcup='" + This.WRowId + "'"

   *> Buscar ocupación a partir de los datos de la misma.
   Case Empty(This.WRowId)
      f_Where =           "F16cCodUbi='" + This.WCUbi + "' And "
      f_Where = f_Where + "F16cCodPro='" + This.ObjParm.POCPro + "' And F16cCodArt='" + This.ObjParm.POCArt + "' And "
      f_Where = f_Where + "F16cNumLot='" + This.ObjParm.PONLot + "' And F16cSitStk='" + This.ObjParm.POSStk + "'"

      *> Agregar campos opcionales.
      f_Where = f_Where + Iif(!Empty(This.ObjParm.PONPal), " And F16cNumPal='" + This.ObjParm.PONPal + "'", "")
      f_Where = f_Where + Iif(!Empty(This.ObjParm.POFCadOra), " And F16cFecCad=" + _GCD(This.ObjParm.POFCadOra), "")
EndCase

lStado = f3_sql('*', 'F16c', f_Where, , , 'ActzF16c')

Select ActzF16c
Go Top
This.In82 = IIf(Eof(), "1", "0")
This.In91 = "0"    && Control Bloqueo

*> Control registro bloqueado
If This.In91 = "1"
   This.ObjParm.PwCRtn = "52"
   This.LgTAno = "OCUBLK"
   This.LgCant = 0
   This.LgProc = "LeeOc"
   This.Anom
   This.In99 = "1"
EndIf

If This.In82="0" .And. This.ObjParm.PwCRtn < "50"
   This.LoFisB = F16cCanFis
   This.LoFisA = F16cCanFis
   This.LoResB = F16cCanRes
   This.LoResA = F16cCanRes
EndIf

Return

ENDPROC
PROCEDURE blkoc

*> Inicializar registro ocupaciones.

Private f_Where

Select ActzF16c
Zap
Append Blank
Replace F16cCodPro With This.ObjParm.POCPro, F16cCodArt With This.ObjParm.POCArt, ;
        F16cCodUbi With This.ObjParm.POCUbi, F16cTamPal With This.ObjParm.POTPal, ;
        F16cNumLot With This.ObjParm.PONLot, F16cSitStk With This.ObjParm.POSStk, ;
        F16cFecCad With This.ObjParm.POFCad, F16cNumAna With This.ObjParm.PONAna, ;
        F16cCanFis With 0,                   F16cCanRes With 0, ;
        F16cUniVen With This.ObjParm.POFUni, F16cUniPac With This.ObjParm.POFSer, ;
        F16cPacCaj With This.ObjParm.POFEnv, F16cCajPal With This.ObjParm.POFPal, ;
        F16cNotEnt With This.ObjParm.PONEnt, F16cTipDoc With This.ObjParm.POTDoc, ;
        F16cNumDoc With This.ObjParm.PONDoc, F16cLinDoc With This.ObjParm.POLDoc, ;
        F16cFecFab With This.ObjParm.POFFab, F16cFecEnt With This.ObjParm.POFEnt, ;
        F16cVolOcu With 0,                   F16cNumPal With This.ObjParm.PONPal, ;
        F16cEsPico With This.ObjParm.POPico, F16cFlag1  With This.ObjParm.POCrd1, ;
        F16cFlag2  With This.ObjParm.POCrd2, F16cIdOcup With Ora_NewOCID()

*> Datos que no están en F16c
* Replace F16cTipEnt With This.ObjParm.POTEnt, F16cCodEnt With This.ObjParm.POCEnt, ;
*         F16cNumPed With This.ObjParm.PONPed, F16cLinPed With This.ObjParm.POLPed, ;
*         F16cFecMov With This.ObjParm.POFMov, F16cPreAct With This.ObjParm.POPAct

Return

ENDPROC
PROCEDURE calpic

Private Cant1

Cant1 = This.ObjParm.POFUni * This.ObjParm.POFSer * This.ObjParm.POFEnv * ;
    This.ObjParm.POFPal

If Cant1 <> 0
   If This.ObjParm.POCFis < Cant1
      This.ObjParm.POPico = "S"
   Else
      This.ObjParm.POPico = "N"
   EndIf
Else
   This.ObjParm.POPico = "S"
EndIf

Return

ENDPROC
PROCEDURE actzst

*> Actualizar stocks

Private f_Asigna, f_Valores
Local Sw

If This.ObjParm.PSFlag <> "S"
   Return
EndIf

This.In84 = "0"     && Hay stock (0=S, 1=N)
This.LeeSt
This.In98 = "0"
If This.ObjParm.PwCRtn >= "50"
   Return
EndIf

If This.PEnSa = "E"
   *> ==== ENTRADAS ====
   If This.In84 = "1"
      This.BlkSt
   EndIf

   Select ActzF13c
   Replace F13cCantid With F13cCantid + This.ObjParm.PSCFis
Else
   *> ==== SALIDAS ====
   Select ActzF13c
   If This.In84 = "0"
      Replace F13cCantid With F13cCantid - This.ObjParm.PSCFis
   Else
      This.ObjParm.PwCRtn = "01"
      This.LgTAno = "PSTOLD"
      This.LgCant = This.ObjParm.PSCFis
      This.LgProc = "ActzSt"
      This.Anom
   EndIf
EndIf

If This.ObjParm.PSCFis < 0
   This.ObjParm.PwCRtn = "05"
   This.LgTAno = "PSCFIS"
   This.LgCant = This.ObjParm.PSCFis
   This.LgProc = "ActzSt"
   This.Anom
EndIf

If ActzF13c.F13cCantid < 0
   This.ObjParm.PwCRtn = "06"
   This.LgTAno = "STCFIS"
   This.LgCant = This.ObjParm.PSCFis
   This.LsFisA = ActzF13c.F13cCantid
   This.LgProc = "ActzSt"
   This.Anom
EndIf

Select ActzF13c
This.LsFisA = F13cCantid
Do Case
   Case This.In84 = "0"
      f_Where = "F13cCodAlm='" + F13cCodAlm + "' And F13cCodPro='" + This.ObjParm.PSCPro + ;
          "' And F13cCodArt='" + This.ObjParm.PSCArt + "' And F13cSitStk='" + This.WSStk + "'"

      If F13cCantid <= 0
         Sw = F3_DelTun('F13c', , f_Where, 'N')
         If Sw = .F.
            This.ObjParm.PwCRtn = "83"
            This.LgTAno = "STKDEL"
            This.LgProc = "ActzSt"
            This.Anom
         EndIf
      Else
         f_Asigna  = 'F13cCantid'
         f_Valores = f_Asigna
         Sw = F3_UpdTun('F13c', , f_Asigna, f_Valores, 'ActzF13c', f_Where, 'N')
         If Sw = .F.
            This.ObjParm.PwCRtn = "84"
            This.LgTAno = "STKUPD"
            This.LgProc = "ActzSt"
            This.Anom
         EndIf
      EndIf

   Case This.In84 = "1"           && No Existe
      If This.PEnSa = "E"
         Sw = F3_InsTun('F13c', 'ActzF13c', 'N')
         If Sw = .F.
            This.ObjParm.PwCRtn = "85"
            This.LgTAno = "STKINS"
            This.LgProc = "ActzSt"
            This.Anom
         EndIf
      EndIf
EndCase

Return

ENDPROC
PROCEDURE blkst

Select ActzF13c
Zap
Append Blank
Replace F13cCodAlm With This.ObjParm.PSCAlm, ;
        F13cCodPro With This.ObjParm.PSCPro, ;
        F13cCodArt With This.ObjParm.PSCArt, ;
        F13cSitStk With This.WSStk, ;
        F13cCantid With 0

Return

ENDPROC
PROCEDURE leest

Private f_Where

*> Buscar stock (Alm,Pro,Art,Stk)
Select ActzF13c
Zap

f_Where = "F13cCodAlm='" + This.ObjParm.PSCAlm + "' And F13cCodPro='" + This.ObjParm.PSCPro + ;
    "' And F13cCodArt='" + This.ObjParm.PSCArt + "' And F13cSitStk='" + This.WSStk +"'"

=F3_Sql('*','F13c',f_Where,,,'ActzF13c')

Select ActzF13c
Go Top
This.In84 = IIf(EOF(), "1", "0")
This.In91 = "0"    && Control Bloqueo

*> Control registro bloqueado
If This.In91 = "1"
   This.ObjParm.PwCRtn = "55"
   This.LgTAno = "STKBLK"
   This.LgCant = 0
   This.LgProc = "LeeSt"
   This.Anom
   This.In99 = "1"
EndIf

If This.In84="0"
   This.LsFisB = F13cCantid
   This.LsFisA = F13cCantid
EndIf

Return

ENDPROC
PROCEDURE leeub

Private f_Where

*> Buscar ubicación (Ubi)
Select ActzF10c
Zap

f_Where = "F10cCodUbi='" + This.WCUbi + "'"

=F3_Sql('*','F10c',f_Where,,,'ActzF10c')

Select ActzF10c
Go Top
This.In83 = IIf(EOF(), "1", "0")
This.In91 = "0"    && Control Bloqueo

*> Control registro bloqueado
If This.In91 = "1"
   This.ObjParm.PwCRtn = "53"
   This.LgTAno = "UBIBLK"
   This.LgCant = 0
   This.LgProc = "LeeUb"
   This.Anom
   This.In99 = "1"
EndIf

If This.In83="1"        && Ubicación no encontrada
   This.ObjParm.PwCRtn = "54"
   This.LgTAno = "CUBINO"
   This.LgCant = 0
   This.LgProc = "LeeUb"
   This.Anom
   This.In99 = "1"
Else
   This.LgArtB = F10cNumOcu
   This.LgArtA = F10cNumOcu
EndIf

Return

ENDPROC
PROCEDURE actzub

***> Actualizar ubicaciones

*> Corregido el recálculo de peso / volumen de la ubicación. AVC - 08.06.2006

Private f_Valores, f_Asigna, f_Where
Local Sw

*> Si Flag actualización no es "S", no se debe actualizar ubicación
If This.ObjParm.PUFlag <> "S"
   Return
EndIf

This.In83 = "0"
This.LeeUb
If This.ObjParm.PwCRtn >= "50"
   Return
EndIf

*> Proceso actualización según sea ENTRADA o SALIDA
If This.PEnSa = "E"
   *>  ==== ENTRADAS ====
   Select ActzF10c

	With This.oRStkObj
		.Inicializar
		.RsCUbi = This.WCUbi
		Sw = .Ejecutar('11')
	EndWith

*   Replace F10cVolLib With F10cVolLib - This.ObjParm.POVOcu, F10cEstGen With "O", ;
*           F10cPesOcu With F10cPesOcu + This.WKOcu
*   Replace F10cVolLib With F10cVolLib - This.WVLib, ;
*           F10cEstGen With "O", ;
*           F10cPesOcu With F10cPesOcu + This.WKOcu

*>>>>>>>>>>>>>>>> PROCESO ANULADO ------------------------------------------------
*!*	   Store 0 To This.WNOcu, This.WKOcu, This.WVLib
*!*	   Do PesVolUb In Ora_Ca00 With This.WCUbi, This.WNOcu, This.WKOcu, This.WVLib

*!*	   Replace F10cVolLib With F10cVolTot - This.WVLib, ;
*!*	           F10cEstGen With "O", ;
*!*	           F10cPesOcu With This.WKOcu, ;
*!*	           F10cNumOcu With This.WNOcu

*!*	   If F10cVolLib < 0
*!*	      Replace F10cVolLib With 0
*!*	      This.ObjParm.PwCRtn = "07"
*!*	      This.LgTAno = "UBVLIB"
*!*	      This.LgCant = F10cVolLib
*!*	      This.LgProc = "ActzUb"
*!*	***** This.Anom
*!*	   EndIf
*>>>>>>>>>>>>>>>> PROCESO ANULADO ------------------------------------------------

*   If This.Sumar = "S"
*      Replace F10cNumOcu With F10cNumOcu + 1
*   EndIf
Else
   *>  ==== SALIDAS ====
*   Replace F10cVolLib With F10cVolLib + This.ObjParm.POVOcu, ;
*           F10cPesOcu With F10cPesOcu - This.WKOcu
*   Replace F10cVolLib With F10cVolLib + This.WVLib, ;
*           F10cPesOcu With F10cPesOcu - This.WKOcu

	With This.oRStkObj
		.Inicializar
		.RsCUbi = This.WCUbi
		Sw = .Ejecutar('11')
	EndWith

*>>>>>>>>>>>>>>>> PROCESO ANULADO ------------------------------------------------
*!*	   Store 0 To This.WNOcu, This.WKOcu, This.WVLib
*!*	   Do PesVolUb In Ora_Ca00 With This.WCUbi, This.WNOcu, This.WKOcu, This.WVLib

*!*	   Select ActzF10c
*!*	   Replace F10cVolLib With F10cVolTot - This.WVLib, ;
*!*	           F10cPesOcu With This.WKOcu, ;
*!*	           F10cNumOcu With This.WNOcu

*!*	   If F10cPesOcu < 0
*!*	      Select ActzF10c
*!*	      Replace F10cPesOcu With 0
*!*	      This.ObjParm.PwCRtn = "08"
*!*	      This.LgTAno = "UBKOCU"
*!*	      This.LgCant = F10cPesOcu
*!*	      This.LgProc = "ActzUb"
*!*	***** This.Anom
*!*	   EndIf
*!*	
*!*	   If F10cVolLib > F10cVolTot
*!*	      Select ActzF10c
*!*	      Replace F10cVolLib With F10cVolTot
*!*	      This.ObjParm.PwCRtn = "09"
*!*	      This.LgTAno = "UBVLIB"
*!*	      This.LgCant = F10cVolLib
*!*	      This.LgProc = "ActzUb"
*!*	***** This.Anom
*!*	   EndIf
*>>>>>>>>>>>>>>>> PROCESO ANULADO ------------------------------------------------

*   If This.Borro = "S"
*      Replace F10cNumOcu With F10cNumOcu - 1
*   EndIf

*!*	   If F10cNumOcu <= 0
*!*	      If F10cNumOcu < 0
*!*	         This.ObjParm.PwCRtn = "10"
*!*	         This.LgTAno = "UBNART"
*!*	         This.LgCant = F10cNumOcu
*!*	         This.LgProc = "ActzUb"
*!*	         This.Anom
*!*	         Select ActzF10c
*!*	      EndIf
*!*	
*!*	      Replace F10cNumOcu With 0, ;
*!*	              F10cEstGen With "L", ;
*!*	              F10cVolLib With F10cVolTot, ;
*!*	              F10cPesOcu With 0, ;
*!*	              F10cFecDes With Date()
*!*	   Else
*!*	      Replace F10cEstGen With "O"
*!*	   EndIf
EndIf

*> ==== ACTUALIZACION ====
Select ActzF10c
This.LgArtA = F10cNumOcu

*!*	If This.In83 = "0"           && Existe
*!*	   f_Where  = "F10cCodUbi='" + This.WCUbi + "'"
*!*	   f_Asigna  = 'F10cVolLib, F10cPesOcu, F10cNumOcu, F10cEstGen, F10cFecDes'
*!*	   f_Valores = f_Asigna
*!*	   Sw = F3_UpdTun('F10c', , f_Asigna, f_Valores, 'ActzF10c', f_Where, 'N')
*!*	   If Sw = .F.
*!*	      This.ObjParm.PwCRtn = "87"
*!*	      This.LgTAno = "UBIUPD"
*!*	      This.LgProc = "ActzUb"
*!*	      This.Anom
*!*	   EndIf
*!*	EndIf

Return

ENDPROC
PROCEDURE anom

Private c_CodInc, c_DesInc
Local _oldSele

If This.ObjParm.PwCRtn >= "50"
   *> Hay que salir de la función de actualización
   This.In99 = "1"
EndIf

c_CodInc = This.ObjParm.PwCRtn

Do Case
   Case c_CodInc = '01'
      c_DesInc = "Situación de stock no existe"
   Case c_CodInc = '05'
      c_DesInc = "Cantidad de Sit.Stock < 0 (Parámetro PSCFIS)"
   Case c_CodInc = '06'
      c_DesInc = "Cantidad de Sit.Stock < 0 (Campo F13cCantid)"
   Case c_CodInc = '07'
      c_DesInc = "Volumen libre de la ubicación < 0"
   Case c_CodInc = '08'
      c_DesInc = "Peso ocupado de la ubicación < 0"
   Case c_CodInc = '09'
      c_DesInc = "Volumen libre ubicación mayor que volumen total"
   Case c_CodInc = '10'
      c_DesInc = "Número ocupaciones de la ubicación < 0"
   Case c_CodInc = '11'
      c_DesInc = "POCRES > Cant.Reservada ocupación"
   Case c_CodInc = '12'
      c_DesInc = "Cant.Reservada de la ocupación < 0"
   Case c_CodInc = '13'
      c_DesInc = "Cant.Física de la ocupación < 0"
   Case c_CodInc = '51'
      c_DesInc = "Operación de actualización no válida"
   Case c_CodInc = '52'
      c_DesInc = "Ocupación bloqueada"
   Case c_CodInc = '53'
      c_DesInc = "Ubicación bloqueada"
   Case c_CodInc = '54'
      c_DesInc = "Ubicación no existe"
   Case c_CodInc = '55'
      c_DesInc = "Situación stock artículo bloqueada"
   Case c_CodInc = '56'
      c_DesInc = "Cantidad de salida > cantidad disponible ocupación"
   Case c_CodInc = '57'
      c_DesInc = "POCFIS < 0"
   Case c_CodInc = '58'
      c_DesInc = "POCRES < 0"
   Case c_CodInc = '59'
      c_DesInc = "PSCFIS < 0"
   Case c_CodInc = '60'
      c_DesInc = "Cantidad reservada ocupación mayor que cantidad física"
   Case c_CodInc = '61'
      c_DesInc = "Ocupación no existe"
   Case c_CodInc = '70'
      c_DesInc = "No existe cabecera de documento"
   Case c_CodInc = '71'
      c_DesInc = "Error al insertar albarán salida - cabecera"
   Case c_CodInc = '72'
      c_DesInc = "Error al insertar albarán salida - lineas"
   Case c_CodInc = '73'
      c_DesInc = "Error al actualizar albarán salida - cabecera"
   Case c_CodInc = '74'
      c_DesInc = "No existe cabecera de albarán de salida"
   Case c_CodInc = '75'
      c_DesInc = "Error al insertar albarán reparto - F27c"
   Case c_CodInc = '76'
      c_DesInc = "Error al insertar albarán reparto - F30c"
   Case c_CodInc = '77'
      c_DesInc = "No existe datos destinatario"
   Case c_CodInc = '78'
      c_DesInc = "Albarán ya relacionado"
   Case c_CodInc = '80'
      c_DesInc = "Error al borrar ocupación"
   Case c_CodInc = '81'
      c_DesInc = "Error al actualizar ocupación"
   Case c_CodInc = '82'
      c_DesInc = "Error al insertar ocupación"
   Case c_CodInc = '83'
      c_DesInc = "Error al borrar situación stock artículo"
   Case c_CodInc = '84'
      c_DesInc = "Error al actualizar situación stock artículo"
   Case c_CodInc = '85'
      c_DesInc = "Error al insertar situación stock artículo"
   Case c_CodInc = '86'
      c_DesInc = "Error al borrar ubicación"
   Case c_CodInc = '87'
      c_DesInc = "Error al actualizar ubicación"
   Case c_CodInc = '88'
      c_DesInc = "Error al insertar ubicación"
   Case c_CodInc = '89'
      c_DesInc = "Error al borrar movimiento pendiente"
   Case c_CodInc = '90'
      c_DesInc = "Error al actualizar movimiento pendiente"
   Case c_CodInc = '91'
      c_DesInc = "Error al insertar movimiento pendiente"
   Case c_CodInc = '92'
      c_DesInc = "Error al borrar movimiento histórico"
   Case c_CodInc = '93'
      c_DesInc = "Error al actualizar movimiento histórico"
   Case c_CodInc = '94'
      c_DesInc = "Error al insertar movimiento histórico"
   Otherwise
      c_DesInc = "Error sin codificar"
EndCase

_oldSele = Alias()

If !File('F99c.Dbf')
   Create Table F99c(F99cCodUsr C(6), ;
                     F99cCodAlm C(4), ;
                     F99cCodPro C(6), ;
                     F99cCodAno C(2), ;
                     F99cTipAno C(10), ;
                     F99cDesAno Memo, ;
                     F99cPrgAno Memo, ;
                     F99cErrAno Memo, ;
                     F99cProces C(10), ;
                     F99cPtAcc  C(2), ;
                     F99cPEnSa  C(1), ;
                     F99cFecAno D, ;
                     F99cHorAno C(8), ;
                     F99cTipDoc C(4), ;
                     F99cNumDoc C(13), ;
                     F99cLinDoc C(4), ;
                     F99cCodArt C(13), ;
                     F99cNumLst C(6), ;
                     F99cCanFis N(10, 0), ;
                     F99cCanRes N(10, 0), ;
                     F99cCodUbi C(14), ;
                     F99cNumLot C(15), ;
                     F99cFecCad DateTime, ;
                     F99cSitStk C(4), ;
                     F99cNumMov C(10), ;
                     F99cCUbiPr C(14), ;
                     F99cCFisPr N(10, 0), ;
                     F99cCResPr N(10, 0), ;
                     F99cSStkPr C(4), ;
                     F99cCUbiPo C(14), ;
                     F99cCFisPo N(10, 0), ;
                     F99cCResPo N(10, 0), ;
                     F99cSStkPo C(4))
EndIf

If Used('F99c')
   Use In F99c
EndIf

Select 0
Use F99c
Append Blank
Replace F99cCodUsr With _UsrCod, ;
        F99cCodAlm With _Alma, ;
        F99cCodPro With _Procaot, ;
        F99cCodAno With c_CodInc, ;
        F99cTipAno With This.LgTAno, ;
        F99cDesAno With c_DesInc, ;
        F99cPrgAno With Sys(16, Program(-1)), ;
        F99cProces With This.LgProc, ;
        F99cPtAcc  With This.ObjParm.PtAcc, ;
        F99cPEnSa  With Iif(Type('This.PEnSa')=='C', This.PEnSa, ""), ;
        F99cFecAno With Date(), ;
        F99cHorAno With Time(), ;
        F99cTipDoc With This.ObjParm.PoTDoc, ;
        F99cNumDoc With This.ObjParm.PoNDoc, ;
        F99cLinDoc With Str(This.ObjParm.PoLDoc, 4, 0), ;
        F99cCodArt With This.ObjParm.PoCArt, ;
        F99cNumLst With This.ObjParm.PmNLst, ;
        F99cCanFis With This.ObjParm.PoCFis, ;
        F99cCanRes With This.ObjParm.PoCRes, ;
        F99cCodUbi With This.ObjParm.PUbOld, ;
        F99cNumLot With This.ObjParm.PoNLot, ;
        F99cFecCad With This.ObjParm.PoFCad, ;
        F99cSitStk With This.ObjParm.PoSStk, ;
        F99cNumMov With This.ObjParm.PmNMov

*> Grabar valores antes de actualizar.
If Type('This.ObjParm.OF16cPr')=='O'
       Replace F99cCUbiPr With This.ObjParm.OF16cPr.F16cCodUbi, ;
               F99cCFisPr With This.ObjParm.OF16cPr.F16cCanFis, ;
               F99cCResPr With This.ObjParm.OF16cPr.F16cCanRes, ;
               F99cSStkPr With This.ObjParm.OF16cPr.F16cSitStk
EndIf

*> Grabar valores después de actualizar.
If Type('This.ObjParm.OF16cPo')=='O'
       Replace F99cCUbiPo With This.ObjParm.OF16cPo.F16cCodUbi, ;
               F99cCFisPo With This.ObjParm.OF16cPo.F16cCanFis, ;
               F99cCResPo With This.ObjParm.OF16cPo.F16cCanRes, ;
               F99cSStkPo With This.ObjParm.OF16cPo.F16cSitStk
EndIf

*> Restaurar el entorno anterior.
Use In (Select('F99c'))
If !Empty(_oldSele)
	Select (_oldSele)
EndIf

Return

ENDPROC
PROCEDURE reserva

*> Actualización por reserva

*> Actualizar ocupación
This.WCUbi = This.ObjParm.POCUbi
This.WNPal = This.ObjParm.PONPal
This.WRowID= This.ObjParm.PORowIdNew
This.PEnSa = "E"
This.ActzOc

If This.ObjParm.PwCRtn < "50"
   *> Decrementar situación stock antigua
   This.PEnSa = "S"
   This.WSStk = This.ObjParm.PStOld
   This.ActzSt
EndIf

If This.ObjParm.PwCRtn < "50"
   *> Incrementar situación stock nueva
   This.PEnSa = "E"
   This.WSStk = This.ObjParm.PStNew
   This.ActzSt
EndIf

Return

ENDPROC
PROCEDURE desreserva
*> Actualización por desreserva

*> Actualizar ocupación
This.WCUbi = This.ObjParm.POCUbi
This.WNPal = This.ObjParm.PONPal
This.WRowID= This.ObjParm.PORowId
This.PEnSa = "S"
This.ActzOc

*> Actualizar situación stock antigua
If This.ObjParm.PwCRtn < "50"
   This.WSStk = This.ObjParm.PStOld
   This.PEnSa = "S"
   This.ActzSt
   If This.In84 = "1" .And. This.ObjParm.PwCRtn < "50"
      This.ObjParm.PwCRtn = "01"
      This.LgTAno = "PSTOLD"
      This.LgCant = This.ObjParm.PSCFis
      This.LgProc = "Desreserva"
      This.Anom
   EndIf
EndIf

*> Actualizar situación stock nueva
If This.ObjParm.PwCRtn < "50"
   This.WSStk = This.ObjParm.PStNew
   This.PEnSa = "E"
   This.ActzSt
EndIf

Return

ENDPROC
PROCEDURE salidareservada

*> Actualización por salida reservada

With This.oUbicObj
	.CodPro = This.ObjParm.POCPro
	.CodArt = This.ObjParm.POCArt
	.CanUbi = This.ObjParm.POCFis
	=.PesVolAr()
	This.WKOcu = .PesOcu
	This.WVLib = .VolOcu
EndWith

*!*	Store 0 To This.WKOcu, This.WVLib
*!*	Do PesVolAr In Ora_Ca00 With This.ObjParm.POCPro, ;
*!*	                             This.ObjParm.POCArt, ;
*!*	                             This.ObjParm.POCFis, ;
*!*	                             This.WKOcu, ;
*!*	                             This.WVLib

*> Actualizar ocupación
This.WCUbi = This.ObjParm.POCUbi
This.WNPal = This.ObjParm.PONPal
This.WRowID= This.ObjParm.PORowId
This.PEnSa = "S"
This.ActzOc

If This.ObjParm.PwCRtn < "50"
   *> Actualizar ubicación
   This.WCUbi = This.ObjParm.PUbOld
   This.ActzUb
EndIf

If This.ObjParm.PwCRtn < "50"
   *> Actualizar situación stock
   This.WSStk = This.ObjParm.PStOld
   This.ActzSt
EndIf

Return

ENDPROC
PROCEDURE entrada
*> Actualización por entrada

With This.oUbicObj
	.CodPro = This.ObjParm.POCPro
	.CodArt = This.ObjParm.POCArt
	.CanUbi = This.ObjParm.POCFis
	=.PesVolAr()
	This.WKOcu = .PesOcu
	This.WVLib = .VolOcu
EndWith

*!*	Store 0 To This.WKOcu, This.WVLib
*!*	Do PesVolAr In Ora_Ca00 With This.ObjParm.POCPro, ;
*!*	                             This.ObjParm.POCArt, ;
*!*	                             This.ObjParm.POCFis, ;
*!*	                             This.WKOcu, ;
*!*	                             This.WVLib

*> Actualizar ocupación
This.WCUbi = This.ObjParm.POCUbi
This.WNPal = This.ObjParm.PONPal
This.WRowID= This.ObjParm.PORowIdNew
This.PEnSa = "E"
This.ActzOc

If This.ObjParm.PwCRtn < "50"
   *> Actualizar ubicación
   This.WCUbi = This.ObjParm.PUbOld
   This.ActzUb
EndIf

If This.ObjParm.PwCRtn < "50"
   *> Actualizar situación stock
   This.WSStk = This.ObjParm.PStOld
   This.ActzSt
EndIf

Return

ENDPROC
PROCEDURE salida
*> Actualización por salida (sin reserva previa, se saca del disponible)

With This.oUbicObj
	.CodPro = This.ObjParm.POCPro
	.CodArt = This.ObjParm.POCArt
	.CanUbi = This.ObjParm.POCFis
	=.PesVolAr()
	This.WKOcu = .PesOcu
	This.WVLib = .VolOcu
EndWith

*!*	Store 0 To This.WKOcu, This.WVLib
*!*	Do PesVolAr In Ora_Ca00 With This.ObjParm.POCPro, ;
*!*	                             This.ObjParm.POCArt, ;
*!*	                             This.ObjParm.POCFis, ;
*!*	                             This.WKOcu, ;
*!*	                             This.WVLib

*> Actualizar ocupación
This.WCUbi = This.ObjParm.POCUbi
This.WNPal = This.ObjParm.PONPal
This.WRowID= This.ObjParm.PORowId
This.PEnSa = "S"
This.ActzOc

If This.ObjParm.PwCRtn < "50"
   *> Actualizar ubicación
   This.WCUbi = This.ObjParm.PUbOld
   This.ActzUb
EndIf

If This.ObjParm.PwCRtn < "50"
   *> Actualizar situación stock
   This.WSStk = This.ObjParm.PStOld
   This.ActzSt
EndIf

Return

ENDPROC
PROCEDURE cambioubicacion

*> Actualización por cambio de ubicación.

Private WAlmNew, WAlmOld, f_Asigna, f_Valores, f_Where
Local Sw

*> Buscar si existe la ocupación nueva.
This.WCUbi = This.ObjParm.PUbNew
This.WRowID= This.ObjParm.PORowIdNew
This.LeeOc
This.In86 = This.In82         && En This.In82 devuelve "0" si la encuentra y "1" si no existe.

*> Buscar ocupación vieja.
This.WCUbi = This.ObjParm.PUbOld
This.WRowID= This.ObjParm.PORowId
This.LeeOc

*> Si la ocupación vieja no existe, es error grave.
If This.In82 = "1"
   This.ObjParm.PwCRtn = "54"
   This.LgTAno = "OCUINE"
   This.LgCant = 0
   This.LgProc = "ChgUbi"
   This.Anom
   Return
EndIf

*> Guardar el ID de la ocupación vieja.
This.WRowID = F16cIdOcup

With This.oUbicObj
	.CodPro = This.ObjParm.POCPro
	.CodArt = This.ObjParm.POCArt
	.CanUbi = This.ObjParm.POCFis
	=.PesVolAr()
	This.WKOcu = .PesOcu
	This.WVLib = .VolOcu
EndWith

*!*	Store 0 To This.WKOcu, This.WVLib
*!*	Do PesVolAr In Ora_Ca00 With This.ObjParm.POCPro, ;
*!*	                             This.ObjParm.POCArt, ;
*!*	                             ActzF16c.F16cCanFis, ;
*!*	                             This.WKOcu, ;
*!*	                             This.WVLib

If This.In86 = "1"            && Ocupación nueva
   *> Si ocupación nueva no existe, simplemente se cambia.
   Select ActzF16c
   Replace F16cCodUbi With This.ObjParm.PUbNew

   f_Where = "F16cIdOcup='" + This.WRowID + "'"
   f_Asigna  = "F16cCodUbi"
   f_Valores = f_Asigna

   Sw = F3_UpdTun('F16c', , f_Asigna, f_Valores, 'ActzF16c', f_Where, 'N')
   This.Sumar = "S"
   If Sw = .F.
      This.ObjParm.PwCRtn = "81"
      This.LgTAno = "OCUUPD"
      This.LgProc = "ChgUbi"
      This.Anom
   EndIf
Else
   *> Eliminar la ocupación vieja.
   f_Where = "F16cIdOcup='" + This.WRowID + "'"

   Sw = F3_DelTun('F16c', , f_Where, 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "80"
      This.LgTAno = "OCUDEL"
      This.LgProc = "ChgUbi"
      This.Anom
   EndIf

   *> Realizar entrada en la nueva ocupación.
   This.WCUbi = This.ObjParm.PUbNew
   This.WNPal = This.ObjParm.PONPal
   This.WRowID= This.ObjParm.PORowIdNew
   This.PEnSa = "E"
   This.ActzOc

   This.Sumar = "N"
EndIf

If This.ObjParm.PwCRtn < "50"
   *> Cambio ubicaciones
   This.WCUbi = This.ObjParm.PUbOld        && Sacar de antigua
   This.PEnSa = "S"
   This.Borro = "S"
   This.ActzUb
EndIf

If This.ObjParm.PwCRtn < "50"
   This.WCUbi = This.ObjParm.PUbNew        && Meter en nueva
   This.PEnSa = "E"
   This.ActzUb
EndIf

If This.ObjParm.PwCRtn < "50"
   *> Si es cambio entre almacenes, tener en cuenta fichero de stocks
   WAlmOld = Left(This.ObjParm.PUbOld, 4)
   WAlmNew = Left(This.ObjParm.PUbNew, 4)
   If WAlmNew <> WAlmOld
      *> Restar del stock antiguo
      This.ObjParm.PSCAlm = WAlmOld
      This.ObjParm.PSCPro = This.ObjParm.POCPro
      This.ObjParm.PSCArt = This.ObjParm.POCArt
      This.WSStk = This.ObjParm.POSStk
      This.PEnSa = "S"
      This.ActzSt

      If This.ObjParm.PwCRtn < "50"
         *> Sumar al stock nuevo
         This.ObjParm.PSCAlm = WAlmNew
         This.WSStk = This.ObjParm.POSStk
         This.PEnSa = "E"
         This.ActzSt
      EndIf
   EndIf
EndIf

Return

ENDPROC
PROCEDURE cambiostock

*> Actualización por cambio de stock

Private f_Where, cRowID
Local Sw

With This.oUbicObj
	.CodPro = This.ObjParm.POCPro
	.CodArt = This.ObjParm.POCArt
	.CanUbi = This.ObjParm.POCFis
	=.PesVolAr()
	This.WKOcu = .PesOcu
	This.WVLib = .VolOcu
EndWith

*> Comprobar si la ocupación (con la nueva sit.stock) existe.
This.WCUbi = This.ObjParm.PUbOld
This.WRowID= This.ObjParm.PORowIdNew
This.ObjParm.POSStk = This.ObjParm.PStNew

If This.ObjParm.POFlag = "S"
   This.LeeOc
   This.In86 = This.In82         && En This.In82 devuelve "0" si la encuentra y "1" si no existe.

   *> Guardar, si existe, el RowID de la ocupación nueva.
   Select ActzF16c
   cRowID = F16cIdOcup

   *> Buscar ocupación antigua
   This.WRowID = This.ObjParm.PORowId
   This.ObjParm.POSStk = This.ObjParm.PStOld
   This.LeeOc

   If This.In82 = "1"          && Si ocupación antigua no existe, error grave
      This.ObjParm.PwCRtn = "61"
      This.LgTAno = "OCNOEX"
      This.LgCant = 0
      This.LgProc = "ChgStk"
      This.Anom
      Return
   EndIf

   *> Recuperar el ID de la ocupación vieja.
   Select ActzF16c
   This.WRowID = F16cIdOcup

   *> Se descuenta la cantidad de la ocupación de origen, si corresponde se elimina.
   If F16cCanFis > This.ObjParm.POCFis

      Replace F16cCanFis With F16cCanFis - This.ObjParm.POCFis
      Replace F16cCanFis With IIf(F16cCanRes > F16cCanFis, F16cCanRes, F16cCanFis)

      f_Where = "F16cIdOcup='" + This.WRowID + "'"

      Sw = f3_UpdTun('F16c', , , , 'ActzF16c', f_Where, 'N')
      If Sw = .F.          && Si ocupación antigua no existe, error grave
         This.ObjParm.PwCRtn = "81"
         This.LgTAno = "OCUUPD"
         This.LgProc = "ChgStk"
         This.Anom
      EndIf
   Else
      *> Eliminar ocupación vieja.
      f_Where = "F16cIdOcup='" + This.WRowID + "'"

      Sw = f3_DelTun('F16c', , f_Where, 'N')
      If Sw = .F.          && Si ocupación antigua no existe, error grave
         This.ObjParm.PwCRtn = "80"
         This.LgTAno = "OCUDEL"
         This.LgProc = "ChgStk"
         This.Anom
      EndIf
   EndIf

   *> Actualizar ocupación nueva.
   This.ObjParm.POCFis = This.ObjParm.POCFis
   This.ObjParm.POCRes = 0
   This.ObjParm.POVOcu = This.WKOcu
   This.WVLib = This.WKOcu
   This.ObjParm.POSStk = This.ObjParm.PStNew
   This.WNPal = This.ObjParm.PONPal
   This.WRowID = cRowID
   This.PEnSa = "E"
   This.ActzOc

   If This.ObjParm.PwCRtn < "50"
      This.Borro = "S"
      This.PEnSa = "S"
      This.ObjParm.POVOcu = 0
      This.WKOcu = 0
      This.WVLib = 0
      This.WCUbi = This.ObjParm.PUbOld
      This.ActzUb
   EndIf


*!*	   If This.In86 = "1"
*!*	      *> Ocupación nueva no existe: Actualizar campos.
*!*	      Replace F16cSitStk With This.ObjParm.PStNew

*!*	      f_Where = "F16cIdOcup='" + This.WRowID + "'"

*!*	      Sw = f3_UpdTun('F16c', , , , 'ActzF16c', f_Where, 'N')
*!*	      If Sw = .F.          && Si ocupación antigua no existe, error grave
*!*	         This.ObjParm.PwCRtn = "81"
*!*	         This.LgTAno = "OCUUPD"
*!*	         This.LgProc = "ChgStk"
*!*	         This.Anom
*!*	      EndIf
*!*	   Else
*!*	      *> Eliminar ocupación vieja.
*!*	      f_Where = "F16cIdOcup='" + This.WRowID + "'"
*!*	
*!*	      Sw = f3_DelTun('F16c', , f_Where, 'N')
*!*	      If Sw = .F.          && Si ocupación antigua no existe, error grave
*!*	         This.ObjParm.PwCRtn = "80"
*!*	         This.LgTAno = "OCUDEL"
*!*	         This.LgProc = "ChgStk"
*!*	         This.Anom
*!*	      EndIf
*!*	
*!*	      If This.ObjParm.PwCRtn < "50"
*!*	         *> Realizar entrada en ocupación nueva.
*!*	         This.ObjParm.POCFis = F16cCanFis
*!*	         This.ObjParm.POCRes = F16cCanRes
*!*	         This.ObjParm.POVOcu = F16cVolOcu
*!*	         This.WVLib = F16cVolOcu
*!*	         This.ObjParm.POSStk = This.ObjParm.PStNew
*!*	         This.WNPal = This.ObjParm.PONPal
*!*	         This.WRowID = cRowID
*!*	         This.PEnSa = "E"
*!*	         This.ActzOc
*!*	      EndIf
*!*	
*!*	      If This.ObjParm.PwCRtn < "50"
*!*	         This.Borro = "S"
*!*	         This.PEnSa = "S"
*!*	         This.ObjParm.POVOcu = 0
*!*	         This.WKOcu = 0
*!*	         This.WVLib = 0
*!*	         This.WCUbi = This.ObjParm.PUbOld
*!*	         This.ActzUb
*!*	      EndIf
*!*	   EndIf

EndIf

If This.ObjParm.PwCRtn < "50"
   This.WSStk = This.ObjParm.PStOld
   This.PEnSa = "S"
   This.ActzSt
EndIf

If This.ObjParm.PwCRtn < "50"
   This.WSStk = This.ObjParm.PStNew
   This.PEnSa = "E"
   This.ActzSt
EndIf

Return

ENDPROC
PROCEDURE ejecutar

*> Proceso que centraliza las llamadas de actualización.

*> Inicializar propiedades globales para los procesos.
This.InitLocals

*> Inicializar campos
This.BlkDat

If This.ObjParm.PWCRtn < "50"
   *> Llamar a métodos según ObjParm.PTAcc.---------------------------
   Do Case
      Case This.ObjParm.PTAcc = "04"
         This.Reserva

      Case This.ObjParm.PTAcc = "05"
         This.Desreserva

      Case This.ObjParm.PTAcc = "06"
         This.SalidaReservada

      Case This.ObjParm.PTAcc = "07"
         This.Entrada

      Case This.ObjParm.PTAcc = "08"
         This.Salida

      Case This.ObjParm.PTAcc = "09"
         This.CambioUbicacion

      Case This.ObjParm.PTAcc = "10"
         This.CambioStock

      Otherwise
         This.ObjParm.PWCRtn = "51"
         This.LgTAno = "NO-OPER"
         This.LgCant = 0
         This.LgProc = "Ejecutar"
         This.Anom
   EndCase
EndIf

Return

ENDPROC
PROCEDURE actmp

*> Proceso de generación y actualización de movimientos pendientes.
*> Recibe el identificador de ocupación.

Private lxWhere
Local Sw

If This.ObjParm.PMFgMP <> "S"
   Return
EndIf

*> Buscar si el MP existe por número de movimiento
Private lxWhere

*> Crear cursor para MP
=CrtCursor("F14c", "ActzF14c")

*> Inicializar y convertir fechas
This.ConvFec

*> Si no pasan número de movimiento, buscar nuevo número.
If Empty(This.ObjParm.PMNMov)
   This.ObjParm.PMNMov = Ora_NewMP()
EndIf

*> Si no pasan el identificador de ocupación, buscar uno nuevo.
If Empty(This.ObjParm.PORowId)
   This.ObjParm.PORowId = Ora_NewOCID()
EndIf

*> Si pasan documento, obtener datos del documento, y si no, inicializar campos
If !Empty(This.ObjParm.POTDoc) .And. !Empty(This.ObjParm.PONDoc)
   *> Buscar documento
Else
*   This.ObjParm.POTDoc = Space(4)
*   This.ObjParm.PONDoc = Space(13)
*   This.ObjParm.POLDoc = 0
*   This.ObjParm.POFDoc = 0
EndIf

lxWhere = "F14cNumMov = '" + This.ObjParm.PMNMov + "'"
= F3_Sql('*', 'F14c', lxWhere, , , 'ActzF14c')

Select ActzF14c
Go Top
If EOF()
   If Empty(This.ObjParm.PMFMov)
      This.ObjParm.PMFMov = Date()
      This.ObjParm.PMFMovOra = Date()
   EndIf

   Append Blank
   Replace F14cNumMov With This.ObjParm.PMNMov, F14cNumEnt With This.ObjParm.PONEnt, ;
           F14cTipMov With This.ObjParm.PMTMov, F14cTipDoc With This.ObjParm.POTDoc, ;
           F14cNumDoc With This.ObjParm.PONDoc, F14cLinDoc With This.ObjParm.POLDoc, ;
           F14cFecDoc With This.ObjParm.PMFDoc, F14cDirAso With This.ObjParm.PMDAso, ;
           F14cNumPed With This.ObjParm.PONPed, F14cLinPed With This.ObjParm.POLPed, ;
           F14cFecMov With This.ObjParm.PMFMov, F14cCodArt With This.ObjParm.POCArt, ;
           F14cNumLot With This.ObjParm.PONLot, F14cSitStk With This.ObjParm.POSStk, ;
           F14cFecCad With This.ObjParm.POFCad, F14cCanFis With This.ObjParm.POCFis, ;
           F14cRutHab With This.ObjParm.PMRHab, F14cCodAlm With This.ObjParm.POCAlm, ;
           F14cUbiOri With This.ObjParm.PUbOld, ;
           F14cUbiDes With This.ObjParm.PMUDes, F14cNumPal With This.ObjParm.PONPal, ;
           F14cTipPal With This.ObjParm.POTPal, F14cUniVen With This.ObjParm.POFUni, ;
           F14cUniPac With This.ObjParm.POFSer, F14cPacCaj With This.ObjParm.POFEnv, ;
           F14cCajPal With This.ObjParm.POFPal, F14cFecFab With This.ObjParm.POFFab, ;
           F14cFecCal With This.ObjParm.POFCal, F14cNumAna With This.ObjParm.PONAna, ;
           F14cCodPro With This.ObjParm.POCPro, F14cCodOpe With This.ObjParm.PMCOpe, ;
           F14cNumLst With This.ObjParm.PMNLst, F14cNumExp With This.ObjParm.PMNExp, ;
           F14cOrdRec With This.ObjParm.PMORec, F14cEstMov With This.ObjParm.PMStat, ;
           F14cOriRes With This.ObjParm.PMORes, F14cTipUbi With This.ObjParm.PMTUbi, ;
           F14cTipMAs With This.ObjParm.PMTMAs, F14cNumMAs With This.ObjParm.PMNMAs, ;
           F14cMacUni With This.ObjParm.PMMUni, F14cOrdRut With This.ObjParm.PMORut, ;
           F14cTEntRe With This.ObjParm.PMTERe, F14cCEntRe With This.ObjParm.PMCERe, ;
           F14cVenHab With This.ObjParm.PMVHab, F14cTipMac With This.ObjParm.PMTMac, ;
           F14cNumMac With This.ObjParm.PMNMac, F14cSeccio With This.ObjParm.PMSecc, ;
           F14cFlag1  With This.ObjParm.PMFlg1, F14cFlag2  With This.ObjParm.PMFlg2, ;
           F14cIdOcup With This.ObjParm.PORowId

   Sw = F3_InsTun('F14c', 'ActzF14c', 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "91"
      This.LgTAno = "MPINS "
      This.LgProc = "ActMP"
      This.Anom
   EndIf
Else
   If !Empty(This.ObjParm.PUbOld)
      Replace F14cUbiOri With This.ObjParm.PUbOld
   EndIf
   If !Empty(This.ObjParm.PMTMov)
      Replace F14cTipMov With This.ObjParm.PMTMov
   EndIf
   Replace F14cCanFis With F14cCanFis + This.ObjParm.POCFis, ;
           F14cFlag1  With This.ObjParm.PMFlg1, F14cUbiDes With This.ObjParm.PUbNew

   Sw = F3_UpdTun('F14c', , , , 'ActzF14c', lxWhere, 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "90"
      This.LgTAno = "MPUPD "
      This.LgProc = "ActMP"
      This.Anom
   EndIf
EndIf

Use In (Select("ActzF14c"))

Return

ENDPROC
PROCEDURE convfec

***> Convertir datos de fecha a datos de fecha para búsqueda en la BBDD.
*> Fecha movimiento (ocupaciones)

If Empty(This.ObjParm.POFMov)
   This.ObjParm.POFMov = _FecMin
   This.ObjParm.POFMovOra = DToC(_FecMin)
Else
   This.ObjParm.POFMovOra = DToC(This.ObjParm.POFMov)
EndIf

*> Fecha caducidad (ocupaciones)
If Empty(This.ObjParm.POFCad)
   This.ObjParm.POFCad = _FecMin
   This.ObjParm.POFCadOra = DToC(_FecMin)
Else
   This.ObjParm.POFCadOra = DToC(This.ObjParm.POFCad)
EndIf

*> Fecha fabricación (ocupaciones)
If Empty(This.ObjParm.POFFab)
   This.ObjParm.POFFab = _FecMin
   This.ObjParm.POFFabOra = DToC(_FecMin)
Else
   This.ObjParm.POFFabOra = DToC(This.ObjParm.POFFab)
EndIf

*> Fecha calidad (ocupaciones)
If Empty(This.ObjParm.POFCal)
   This.ObjParm.POFCal = _FecMin
   This.ObjParm.POFCalOra = DToC(_FecMin)
Else
   This.ObjParm.POFCalOra = DToC(This.ObjParm.POFCal)
EndIf

*> Fecha entrada (ocupaciones)
If Empty(This.ObjParm.POFEnt)
   This.ObjParm.POFEnt = _FecMin
   This.ObjParm.POFEntOra = DToC(_FecMin)
Else
   This.ObjParm.POFEntOra = DToC(This.ObjParm.POFEnt)
EndIf

*> Fecha movimiento (movimientos)
If Empty(This.ObjParm.PMFMov)
   This.ObjParm.PMFMov = _FecMin
   This.ObjParm.PMFMovOra = DToC(_FecMin)
Else
   This.ObjParm.PMFMovOra = DToC(This.ObjParm.PMFMov)
EndIf

*> Fecha documento (movimientos)
If Empty(This.ObjParm.PMFDoc)
   This.ObjParm.PMFDoc = _FecMin
   This.ObjParm.PMFDocOra = DToC(_FecMin)
Else
   This.ObjParm.PMFDocOra = DToC(This.ObjParm.PMFDoc)
EndIf

*> Fecha envío (movimientos)
If Empty(This.ObjParm.PMFEnv)
   This.ObjParm.PMFEnv = _FecMin
   This.ObjParm.PMFEnvOra = DToC(_FecMin)
Else
   This.ObjParm.PMFEnvOra = DToC(This.ObjParm.PMFEnv)
EndIf

*> Fecha orden (movimientos)
If Empty(This.ObjParm.PMFOrd)
   This.ObjParm.PMFOrd = _FecMin
   This.ObjParm.PMFOrdOra = DToC(_FecMin)
Else
   This.ObjParm.PMFOrdOra = DToC(This.ObjParm.PMFOrd)
EndIf

*> Fecha albarán (albaranes)
If Empty(This.ObjParm.PAFAlb)
   This.ObjParm.PAFAlb = _FecMin
   This.ObjParm.PAFAlbOra = DToC(_FecMin)
Else
   This.ObjParm.PAFAlbOra = DToC(This.ObjParm.PAFAlb)
EndIf

Return

ENDPROC
PROCEDURE acthm

*> Proceso de generación y actualización de histórico de movimientos

Private lxWhere
Local Sw

If This.ObjParm.PMFgHM <> "S"
   Return
EndIf

*> Crear cursor para HM
=CrtCursor("F20c", "ActzF20c")

*> Inicializar y convertir fechas
This.ConvFec

*> Si no pasan número de movimiento, buscar nuevo número
If Empty(This.ObjParm.PMNMov)
   *> Si no pasan número HM, buscar por número MP.---------------------
   ***> DESACTIVADO - HM se generará al acabar MP.---------------------
*   If !Empty(This.ObjParm.PMMvMP)
*      c_PMMvMP = This.ObjParm.PMMvMP
*      =F3_SeekTun("F20c", "F20cNMovMP = '" + c_PMMvMP + "'", , ;
*              "This.ObjParm.PMNMov = m.F20cNumMov")
*      This.ObjParm.PMMvMP = c_PMMvMP
*   EndIf

   *> Si tampoco está por número MP, obtener nuevo número.-------------
   If Empty(This.ObjParm.PMNMov)
      This.ObjParm.PMNMov = Ora_NewHM()
   EndIf
EndIf

*> Si pasan documento, obtener datos del documento, y si no, inicializar campos
If !Empty(This.ObjParm.POTDoc) .And. !Empty(This.ObjParm.PONDoc)
   *> Buscar documento.-----------------------------------------------
Else
*   This.ObjParm.POTDoc = Space(4)
*   This.ObjParm.PONDoc = Space(13)
*   This.ObjParm.POLDoc = 0
*   This.ObjParm.POFDoc = 0
EndIf

lxWhere = "F20cNumMov = '" + This.ObjParm.PMNMov + "'"
=F3_Sql('*', 'F20c', lxWhere, , , 'ActzF20c')

Select ActzF20c
Go Top

A ='S'

if A = 'S'

*If EOF()

   If Empty(This.ObjParm.PMFMov)
      This.ObjParm.PMFMov = Date()
      This.ObjParm.PMFMovOra = Date()
   EndIf

   Append Blank
   Replace F20cNumMov With This.ObjParm.PMNMov, F20cNMovMP With This.ObjParm.PMMvMP, ;
           F20cEntSal With This.ObjParm.PMEnSa, F20cTipMov With This.ObjParm.PMTMov, ;
           F20cNumEnt With This.ObjParm.PONEnt, F20cTipDoc With This.ObjParm.POTDoc, ;
           F20cNumDoc With This.ObjParm.PONDoc, F20cLinDoc With This.ObjParm.POLDoc, ;
           F20cFecDoc With This.ObjParm.PMFDoc, F20cDirAso With This.ObjParm.PMDAso, ;
           F20cNumPed With This.ObjParm.PONPed, F20cLinPed With This.ObjParm.POLPed, ;
           F20cFecMov With This.ObjParm.POFMov, F20cRutHab With This.ObjParm.PMRHab, ;
           F20cCodAlm With This.ObjParm.POCAlm, ;
           F20cCodPro With This.ObjParm.POCPro, F20cCodArt With This.ObjParm.POCArt, ;
           F20cNumLot With This.ObjParm.PONLot, F20cSitStk With This.ObjParm.POSStk, ;
           F20cFecCad With This.ObjParm.POFCad, F20cCanFis With This.ObjParm.POCFis, ;
           F20cUbiOri With This.ObjParm.PUbOld, ;
           F20cUbiDes With This.ObjParm.PUbNew, F20cNumPal With This.ObjParm.PONPal, ;
           F20cTamPal With This.ObjParm.POTPal, F20cUniVen With This.ObjParm.POFUni, ;
           F20cUniPac With This.ObjParm.POFSer, F20cPacCaj With This.ObjParm.POFEnv, ;
           F20cCajPal With This.ObjParm.POFPal, F20cFecFab With This.ObjParm.POFFab, ;
           F20cFecCal With This.ObjParm.POFCal, F20cNumAna With This.ObjParm.PONAna, ;
           F20cCodOpe With This.ObjParm.PMCOpe, F20cNumLst With This.ObjParm.PMNLst, ;
           F20cPicoSN With This.ObjParm.POPico, ;
           F20cOriRes With This.ObjParm.PMORes, F20cTipUbi With This.ObjParm.PMTUbi, ;
           F20cTipMAs With This.ObjParm.PMTMAs, F20cNumMAs With This.ObjParm.PMNMAs, ;
           F20cMacUni With This.ObjParm.PMMUni, F20cOrdRut With This.ObjParm.PMORut, ;
           F20cTEntRe With This.ObjParm.PMTERe, F20cCEntRe With This.ObjParm.PMCERe, ;
           F20cVenHab With This.ObjParm.PMVHab, F20cTipMac With This.ObjParm.PMTMac, ;
           F20cNumMac With This.ObjParm.PMNMac, F20cNumExp With This.ObjParm.PMNExp, ;
           F20cFlgBlq With This.ObjParm.PMBloq, F20cFlgEnv With This.ObjParm.PMFlgE, ;
           F20cFecEnv With This.ObjParm.PMFEnv, F20cNumEnv With This.ObjParm.PMNEnv, ;
           F20cFecOrd With This.ObjParm.PMFOrd, ;
           F20cFlag1  With This.ObjParm.PMFlg1, F20cFlag2  With This.ObjParm.PMFlg2, ;
           F20cSeccio With This.ObjParm.PMSecc, F20cIdOcup With This.WRowID

   Sw = F3_InsTun('F20C', 'ActzF20c', 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "94"
      This.LgTAno = "HMINS "
      This.LgProc = "ActHM"
      This.Anom
   EndIf
Else
   Replace F20cCanFis With F20cCanFis + This.ObjParm.POCFis
   Sw = F3_UpdTun('F20C', , , , 'ActzF20c', lxWhere, 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "93"
      This.LgTAno = "HMUPD "
      This.LgProc = "ActHM"
      This.Anom
   EndIf
EndIf

If Used('ActzF20c')
   Use In ActzF20c
EndIf

Return

ENDPROC
PROCEDURE delmp

*> Eliminar MP a partir de número de movimiento (PMNMov)

Private f_Where
Local Sw

f_Where = "F14cNumMov='" + This.ObjParm.PMNMov + "'"

Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   This.ObjParm.PwCRtn = "89"
   This.LgTAno = "MPDEL "
   This.LgProc = "DelMP"
   This.Anom
EndIf

Return

ENDPROC
PROCEDURE actzas

*> Generación / Actualización de Albaranes de Salida.

*> Número de albaranes que se generan:
*>    This.ObjParm.PACtrG = 'N', No separar estupefacientes.
*>    This.ObjParm.PACtrG = 'S', Separar estupefacientes en otro albarán.
*>    This.ObjParm.PACtrF = 'S', Separar frío en otro albarán. (no implantado).
*>    This.ObjParm.PAAlbN = 'S', Generar nº de albarán nuevo.

*> Asignar transportista frío si hay productos a la vez de frío y estupefacientes. AVC - 31.08.2000
*> Asignar estupefacientes a transportista de frío. AVC - 03.10.2000
*> NO recalcular albaranes relacionados en hoja de ruta. AVC - 10.10.2000

Private lxWhere, lxOrder, lxCampos
Private l_HayEst, l_HayFri, l_HayNor
Private c_TipAlb, Cant
Private n_Bultos, n_Palets, n_Cajas, n_Fracciones
Private n_PesLin, _Peso, _PesoLin, _PesoTot, n_Volu, n_Peso
Private n_LinAlb, n_PesAlb, n_VolAlb, n_BulAlb, n_PalAlb, n_CajAlb, n_FrcAlb, n_EtiAlb
Private _FirstLinea, n_PesTotal
Private n_CanLin, n_VolLin, n_BulIni, n_BulFin, n_BulFrc, n_PesLinReal
Private f_Asigna, f_Valores, _NAlb
Local Sw, _Err, _Ok

Store 0 To n_Bultos, n_Palets, n_Cajas, n_Fracciones

*> Crear cursores de trabajo.
=CrtCursor("F24c", "ActzF24c")        && Documentos de salida, cabecera.
=CrtCursor("F24t", "ActzF24t")        && Documentos de salida, datos destinatario.
=CrtCursor("F27l", "ActzF27l")        && Albaranes de salida, detalle.

*> Cursor para almacenar los documento/albaranes/tipo ya existentes.
Create Cursor F27cAlbCur ;
(CPro Char(6), ;
 TDoc Char(4), ;
 NDoc Char(13), ;
 NAlb Char(13), ;
 FAlb Date, ;
 TAlb Char(1))

*> Convertir fechas e inicializar.
This.ConvFec

n_PesLin = 0
_Peso = 0
cant = 0
n_PesTotal = 0

*> Leer datos del documento de salida.
lxWhere = "F24cCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F24cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F24cNumDoc='" + This.ObjParm.PONDoc + "'"

If !f3_sql('*', 'F24c', lxWhere, , , 'ActzF24c')
   This.ObjParm.PwCRtn = "70"
   This.LgTAno = "DOCINE"
   This.LgProc = "ActzAS"
   This.Anom
   Return
EndIf

*> Dirección asociada (código del cliente).
If Empty(This.ObjParm.PMDAso)
   This.ObjParm.PMDAso = ActzF24c.F24cDirAso
EndIf

*> Leer los datos del destinatario (cliente).
lxWhere = "F24tCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F24tTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F24tNumDoc='" + This.ObjParm.PONDoc + "'"

If !f3_sql('*', 'F24t', lxWhere, , , 'ActzF24t')
   This.ObjParm.PwCRtn = "77"
   This.LgTAno = "DOCDAS"
   This.LgProc = "ActzAS"
   This.Anom
   Select ActzF24t
   Append Blank
   Go Top
   This.ObjParm.PwCRtn = "  "
EndIf

*> Leer los albaranes asociados a este documento de salida.
If Used('ActzF27c')
   Use In ActzF27c
EndIf

lxWhere = "F27cCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F27cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F27cNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
          "F30cCodEnt=F27cCodPro And " + ;
          "F30cTipDoc=F27cTipDoc And " + ;
          "F27cNumDoc=F27cNumDoc And " + ;
          "F30cAlbRep=F27cNumAlb"

lxOrder = "F27cCodPro, F27cTipDoc, F27cNumDoc, F27cNumAlb"

=f3_sql('*', 'F27c,F30c', lxWhere, lxOrder, , 'ActzF27c')

*> Comprobar que no se haya relacionado el albarán.
Select ActzF27c
Locate For F30cCodSit > '0'
If Found()
   This.ObjParm.PwCRtn = "78"
   This.LgTAno = "ALBREP"
   This.LgProc = "ActzAS"
   This.PEnSa = Space(1)
   This.Anom
   Return
EndIf

*> Borrar las líneas actuales, si hay del F27l.
*> Borrar ANTES de crear cabecera albarán, pues es posible generar más de uno.
lxWhere = "F27lCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F27lTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F27lNumDoc='" + This.ObjParm.PONDoc + "'"
=f3_DelTun('F27l', ,lxWhere, 'N')

*> Si ya existe, memorizar los Nºs de albarán asociados a a cada tipo de producto,
*> es decir, estupefacientes, ambiente y frío, este último no implantado.

*> Memorizar los albaranes ya existentes de este documento.
If This.ObjParm.PAAlbN # 'S'
   Select ActzF27c
   Go Top
   Do While !Eof()
      Select F27cAlbCur
      Append Blank
      Replace CPro With ActzF27c.F27cCodPro, ;
              TDoc With ActzF27c.F27cTipDoc, ;
              NDoc With ActzF27c.F27cNumDoc, ;
              NAlb With ActzF27c.F27cNumAlb, ;
              FAlb With ActzF27c.F27cFecAlb, ;
              TAlb With ActzF27c.F27cTipAlb

      Select ActzF27c
      Skip
   EndDo
EndIf

*> Borrar registros relacionados en F30c (FK entre F30c y F27c) .
lxWhere = "F30CCODENT='" + This.ObjParm.POCPro + "' And " + ;
          "F30CTIPDOC='" + This.ObjParm.POTDoc + "' And " + ;
          "F30CNUMDOC='" + This.ObjParm.PONDoc + "'"
=f3_DelTun('F30C', ,lxWhere, 'N')

*>
*> Borrar los albaranes existentes.
lxWhere = "F27cCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F27cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F27cNumDoc='" + This.ObjParm.PONDoc + "'"
=f3_DelTun('F27C', ,lxWhere, 'N')

Select ActzF27c
Delete All
This.ObjParm.PANAlb = Space(13)
This.ObjParm.PAFAlb = _FecMin

*> Crear cursores de trabajo para la generación del detalle de los albaranes.
*> NO crear cursor, ya lo hará f3_sql.         =CrtCursor("F24l", "ActzF24l")
=CrtCursor("F26l", "ActzF26l")
=CrtCursor("F26v", "ActzF26v")

*> Cargar los bultos correspondientes a este documento de salida.
*> De momento no sabemos para que sirve esto ...
lxWhere = "F26vCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F26vTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F26vNumDoc='" + This.ObjParm.PONDoc + "'"
=f3_sql('*', 'F26v', lxWhere, , , 'ActzF26v')

*> Cargar las líneas de detalle del documento de salida, así como el tipo de producto.
lxWhere = "F24lCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F24lTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F24lNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
          "F08cCodPro=F24lCodPro And F08cCodArt=F24lCodArt"

lxCampos= "F24l" + _em + ".*, F08cTipPro, 'N' As TipAlb"
lxOrder = "F24lCodPro, F24lTipDoc, F24lNumDoc, F08cTipPro"
=f3_sql(lxCampos, 'F24l,F08c', lxWhere, lxOrder, , 'ActzF24l')

*> Solo se generan albaranes de líneas de documento que estén en listas de trabajo.
Select ActzF24l
Go Top
Do While !Eof()
   lxWhere = "F26lCodPro='" + ActzF24l.F24lCodPro + "' And " + ;
             "F26lTipDoc='" + ActzF24l.F24lTipDoc + "' And " + ;
             "F26lNumDoc='" + ActzF24l.F24lNumDoc + "' And " + ;
             "F26lLinDoc=" + ActzF24l.F24lLinDoc + " And F26lEstMov>'0' And F26lEstMov<'4'"
*Jony             "F26lLinDoc=" + ActzF24l.F24lLinDoc + " And F26lEstMov>'0'"


   lxCampos = '*'
   lxOrder = "F26lCodPro, F26lTipDoc, F26lNumDoc"
   =f3_sql(lxCampos, 'F26l', lxWhere, lxOrder, , 'ActzF26l')

   Select ActzF26l
   Go Top
   If Eof()
      Select ActzF24l
      Delete Next 1
   EndIf

   Select ActzF24l
   Skip
EndDo

*> Actualizar el flag de tipo de albarán en el cursor de detalle.
*> Por defecto es 'N', albarán normal.
Select ActzF24l
Replace All TipAlb With 'E' For !Empty(F24lNumDrg) .And. This.ObjParm.PACtrG=='S'
Replace All TipAlb With 'F' For F08cTipPro==This.ObjParm.PAFrio && .And. This.ObjParm.PACtrF=='S'
Replace All TipAlb With 'F' For Empty(F24lNumDrg) .And. F08cTipPro==This.ObjParm.PAFriG

*> Controlar si hay estupefacientes.
Locate For TipAlb = 'E'
l_HayEst = Found()

*> Controlar si hay frío.
*> Si hay algún artículo de frío, TODO el albarán se considera como tal, excepto los
*> estupefacientes, asignando el transportista de frío a todo el albarán.
Locate For TipAlb = 'F'
l_HayFri = Found()
If l_HayFri
   Replace All TipAlb With 'F' For TipAlb <> 'E'
EndIf

*> Resto de líneas de detalle, albarán normal.
Locate For TipAlb = 'N'
l_HayNor = Found()

*> Creación de la cabecera de los albaranes.
*> Puede haber tres tipos:
*>    Albarán estupefacientes (F27cTipAlb = 'E').
*>    Albarán frío (F27cTipAlb = 'F').
*>    Albarán normal (F27cTipAlb = 'N').

c_TipAlb = 'E'

Do While .T.
   *> Proceso de generación de líneas de detalle del albarán.
   Store 0 To n_LinAlb, n_PesAlb, n_VolAlb, n_BulAlb, n_PalAlb, n_CajAlb, n_FrcAlb, n_EtiAlb
   Store .T. To _FirstLinea

   Select ActzF24l
   Locate For TipAlb = c_TipAlb
   Do While Found() .And. This.ObjParm.PwCRtn < "50"

      *> Crear, si cal, la cabecera del albarán.
      If _FirstLinea
         *> Ver si ya había un albarán para este tipo de albarán
         Select F27cAlbCur
         Locate For CPro = This.ObjParm.POCPro .And. TDoc = This.ObjParm.POTDoc .And. NDoc = This.ObjParm.PONDoc .And. TAlb = c_TipAlb		&& Buscar por documento.
         If Found()
            This.ObjParm.PANAlb = NAlb
            This.ObjParm.PAFAlb = FAlb
		 Else
	         Locate For TAlb = c_TipAlb													&& Buscar por tipo albarán.
	         If Found()
	            This.ObjParm.PANAlb = NAlb
	            This.ObjParm.PAFAlb = FAlb
	         Else
	            This.ObjParm.PAFAlb = Date()											&& Nuevo número de albarán.
	            This.ObjParm.PAFAlbOra = Date()
	            This.ObjParm.PANAlb = PadR(Ora_NewNum('NALB', 'N', 'N', 10), 13, Space(1))
	         EndIf
		 EndIf

         Select ActzF27c
         Append Blank
         Replace F27cCodPro With This.ObjParm.POCPro, ;
                 F27cTipDoc With This.ObjParm.POTDoc, ;
                 F27cNumDoc With This.ObjParm.PONDoc, ;
                 F27cDirAso With This.ObjParm.PMDAso, ;
                 F27cNumAlb With This.ObjParm.PANAlb, ;
                 F27cFecAlb With This.ObjParm.PAFAlb, ;
                 F27cFecEmb With _FecMin, ;
                 F27cHorEnt With This.ObjParm.PAHEnt, ;
                 F27cHorSal With This.ObjParm.PAHSal, ;
                 F27cFecLle With Date(), ;
                 F27cNumTrp With 0, ;
                 F27cFecTrp With _FecMin,  ;
                 F27cHorTrp With Space(8), ;
                 F27cConAlb With Space(1), ;
                 F27cNumTRu With 0, ;
                 F27cFecTRu With _FecMin, ;
                 F27cHorTRu With Space(8), ;
                 F27cTipUrg With Space(1), ;
                 F27cFlete  With Space(1), ;
                 F27cExtrar With Space(1), ;
                 F27cRutHab With Space(4), ;
                 F27cCodTra With Space(6), ;
                 F27cFlgEst With '0', ;
                 F27cTipAlb With c_TipAlb
         *------------------------------------------------------------
         *> Obtenemos peso a partir de F16T

         _Sentencia= " Select * from F26l" + _em + ;
         			 " Where F26lCodPro = '" + ActzF27c.F27cCodPro + "'" +;
         			 " And F26lNumDoc = '" + ActzF27c.F27cNumDoc + "'"

         *			 " And F26LLinDoc = " + ActzF24l.F24lLinDoc
         			
         _Err = f3_SqlExec(_Asql,_Sentencia,'F26lCur')
  		 If _Err <= 0
             _LxErr = "Error leyendo información de la lista: " +  ActzF27c.F27cNumDoc + cr + ;
                      "SENTENCIA: " + _Sentencia + cr + ;
                      "MENSAJE: " + Message() + cr
             =Anomalias()
		     Return
		 EndIf	
		
		 Select F26lCur
		 Go Top
		 Do while !Eof()
		  _Sentencia= "Select " + _GCN("F16tPesPal") + " As PesoPalet From F16t" + _em + ;
		  			  " Where F16tNumPal = '" + F26lCur.F26lNumPal + "'"
		  			
         _Err = f3_SqlExec(_Asql,_Sentencia,'F16tCur')
  		  If _Err <= 0
             _LxErr = "Error leyendo peso del palet: " + F26lCur.F26lNumPal + cr + ;
                      "SENTENCIA: " + _Sentencia + cr + ;
                      "MENSAJE: " + Message() + cr
             =Anomalias()
		     Return
		  EndIf	
		
		  _Sentencia= " Select F20cCanFis As Cantidad from F20c" + _em + ;
		  			  " Where F20cEntSal = 'E'" + ;
		  			  " And " + _GCSS("F20cTipMov", 1, 1) + " <> '3' " + ;
		  			  " And F20cNumPal = '" + F26lCur.F26lNumPal + "'"
		  			
		  _Err = f3_SqlExec(_Asql,_Sentencia,'F20cCur')
  		  If _Err < 0
             _LxErr = "Error leyendo datos de histórico de movimientos" + cr + ;
                      "SENTENCIA: " + _Sentencia + cr + ;
                      "MENSAJE: " + Message() + cr
             =Anomalias()
		     Return
		  EndIf			
		  If F16tCur.PesoPalet = 0 or F20cCur.Cantidad = 0
		  	_Peso = 0
		  Else
		  	_Peso = _Peso +(F16tCur.PesoPalet / F20cCur.Cantidad) * F26lCur.F26lCanFis
		  	_Peso=Round(_Peso,3)
		  	Cant=Cant+F26lCur.F26lCanFis

		  EndIf
		  *_Peso = ActzF24c.F24cTotKgs
		  Select F26lCur
  		 Skip
		 EndDo
		  *------------------------------------------------------------------------------

         Select ActzF24c
         Go Top
            If _Peso = 0
				 *> Calculamos peso.-------------------------------------------
				 Select F26lCur
				 Go top
				 Do while !Eof()
					_Sentencia = " Select F08cPesUni From F08c" + _em + ;
					             " Where F08cCodPro ='" + F26lCur.F26lCodPro + ;
								 "' And F08cCodArt ='" + F26lCur.F26lCodArt + "'"
					_Ok = f3_SqlExec(_Asql,_Sentencia,'pesoart')			
					=sqlMoreresults(_Asql)
					Select Pesoart
					If !Eof()
						_Peso = _Peso + (F26lCur.F26lCanfis * pesoart.F08cPesUni)
					EndIf
					Select F26lCur
					Skip
				 EndDo

            EndIf

         Select ActzF24c
         Go Top
         If !Eof()
            Select ActzF27c
            Replace F27cTipUrg With ActzF24c.F24cTipPed, ;
                    F27cRutHab With ActzF24c.F24cRutHab, ;
                    F27cDirAso With ActzF24c.F24cDirAso, ;
                    F27cCodTra With Iif(c_TipAlb=='F' .Or. c_TipAlb=='E' .Or. l_HayEst, ;
                                        ActzF24c.F24cCodTrF, ;
                                        ActzF24c.F24cCodTra), ;
                    F27cConAlb With ActzF24c.F24cConAlb, ;
                    F27cPalCmp With ActzF24c.F24cPalTot, ;
                    F27cCajLib With ActzF24c.F24cCajTot, ;
                    F27cCajPic With ActzF24c.F24cFraTot, ;
                    F27cPesoKg With _Peso, ;
                    F27cVolume With ActzF24c.F24cTotVol, ;
                    F27cBultos With ActzF24c.F24cNumBul, ;
                    F27cEtique With ActzF24c.F24cPalTot + ;
                                    ActzF24c.F24cCajTot + ;
                                    ActzF24c.F24cFraTot
         EndIf

         Select ActzF24t
         Go Top
         If !Eof()
            Select ActzF27c
            Replace F27cFlete  With ActzF24t.F24tFlete, ;
                    F27cExtrar With ActzF24t.F24tExtrad
         EndIf

         *> Generar el Albarán de Salida, cabecera.
         Select ActzF27c
         Sw = F3_InsTun('F27c', 'ActzF27c', 'N')
         If Sw = .F.
            This.ObjParm.PwCRtn = "71"
            This.LgTAno = "ASCINS"
            This.LgProc = "ActzAS"
            This.Anom
         EndIf

         Store .F. To _FirstLinea
      EndIf                        && If _FirstLinea

      *>
      *> Ver si existe el registro de F30c.
      lxWhere = "F30cTipEnt='PROP' And F30cCodEnt='" + This.ObjParm.POCPro + "' And " + ;
                "F30cTipAlb='D'    And F30cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
                "F30cNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
                "F30cAlbRep='" + This.ObjParm.PANAlb + "'"
      =f3_sql('*', 'F30c', lxWhere, , , 'ActzF30c')

      Select ActzF30c
      Go Top
      If Eof()
         c_Oper = "INS"
         c_CodSit = '0'
         c_HojRut = Space(10)
      Else
         c_Oper = "UPD"
         c_CodSit = F30cCodSit
         c_HojRut = F30cHojRut
         Zap
      EndIf

      Append Blank
      Replace F30cTipEnt With 'PROP',              F30cCodEnt With This.ObjParm.POCPro, ;
              F30cTipAlb With 'D',                 F30cTipDoc With This.ObjParm.POTDoc, ;
              F30cAlbRep With This.ObjParm.PANAlb, F30cNumDoc With This.ObjParm.PONDoc, ;
              F30cFecAlb With ActzF27c.F27cFecAlb, F30cFecEmb With ActzF27c.F27cFecEmb, ;
              F30cNomDes With ActzF24t.F24tNomAso, ;
              F30cDirDes With ActzF24t.F24t1erDir, F30cPobDes With ActzF24t.F24tDPobla, ;
              F30cPrvDes With ActzF24t.F24tCProvi, F30cCodPos With ActzF24t.F24tCodPos, ;
              F30cPunOpe With ActzF24t.F24tPunOpe, F30cPalCmp With ActzF27c.F27cPalCmp, ;
              F30cCajLib With ActzF27c.F27cCajLib, F30cCajPic With ActzF27c.F27cCajPic, ;
              F30cBultos With ActzF27c.F27cBultos, ;
              F30cPesoKg With ActzF27c.F27cPesoKg, F30cVolume With ActzF27c.F27cVolume, ;
              F30cFecLle With ActzF27c.F27cFecLle, F30cCodSit With c_CodSit, ;
              F30cFecEnt With _FecMin,             F30cPortes With 'P', ;
              F30cHojRut With c_HojRut,            ;
              F30cTipUrg With ActzF27c.F27cTipUrg, F30cFlete  With ActzF27c.F27cFlete,  ;
              F30cExtrar With ActzF27c.F27cExtrar, F30cFecEmi With _FecMin, ;
              F30cFecTrp With _FecMin,             F30cFecCFe With _FecMin, ;
              F30cRutHab With ActzF27c.F27cRutHab, F30cCodTra With ActzF27c.F27cCodTra

      Select ActzF30c

      *> Crear un nuevo registro en el F30c.
      If c_Oper = "INS"
         If !f3_InsTun('F30c', 'ActzF30c', 'N')
            This.ObjParm.PwCRtn = "75"
            This.LgTAno = "ARCINS"
            This.LgProc = "ActzAR"
            This.Anom
         EndIf
      Else
         *> Actualizar el registro del F30c.
         lxWhere = "F30cTipEnt='PROP' And F30cCodEnt='" + This.ObjParm.POCPro + "' And " + ;
                   "F30cTipAlb='D'    And F30cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
                   "F30cNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
                   "F30cAlbRep='" + This.ObjParm.PANAlb + "'"

         If !f3_UpdTun('F30c', , , , 'ActzF30c', lxWhere, 'N', 'N')
            This.ObjParm.PwCRtn = "76"
            This.LgTAno = "ARCUPD"
            This.LgProc = "ActzAR"
            This.Anom
         EndIf
      EndIf                      && If c_Oper

      *> Actualizar el Nº de albarán en la cabecera del documento de salida.
      lxWhere = "F24cCodPro='" + This.ObjParm.POCPro + "' And " + ;
                "F24cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
                "F24cNumDoc='" + This.ObjParm.PONDoc + "'"

      _NAlb = This.ObjParm.PANAlb
      Sw = f3_UpdTun("F24c", ,"F24cNumAlb", "_NAlb", , lxWhere, 'N', 'N')
      If Sw = .F.
         This.ObjParm.PwCRtn = "76"
         This.LgTAno = "UPDALB"
         This.LgProc = "ActzAR"
         This.Anom
      EndIf

      *>
      *> Cargar el detalle de las listas de trabajo de la línea documento actual.
      lxWhere = "F26lCodPro='" + ActzF24l.F24lCodPro + "' And " + ;
                "F26lTipDoc='" + ActzF24l.F24lTipDoc + "' And " + ;
                "F26lNumDoc='" + ActzF24l.F24lNumDoc + "' And " + ;
                "F26lLinDoc=" + ActzF24l.F24lLinDoc + " And F26lEstMov>'0' And F26lEstMov<'4'"

      lxOrder = "F26lCodArt, F26lNumLot, F26lFecCad"
      =F3_Sql('*', 'F26l', lxWhere, lxOrder, , 'ActzF26l')

      Select ActzF26l
      Go Top
      w_NumLot = ActzF26l.F26lNumLot
      Store 0 To n_CanLin, n_VolLin, n_BulIni, n_BulFin, n_BulFrc, n_PesLinReal,_PesoLin

      Do While !Eof() .And. This.ObjParm.PwCRtn < "50"

         *> Calcular peso y volumen de la cantidad del lote.
		 With This.oUbicObj
			 .CodPro = F26lCodPro
			 .CodArt = F26lCodArt
			 .CanUbi = F26lCanFis
			 =.PesVolAr()
			 n_Peso = .PesOcu
			 n_Volu = .VolOcu
		 EndWith

*         n_CanLin = n_CanLin + ActzF24l.F24lCanDoc
         n_CanLin = n_CanLin + ActzF26l.F26lCanFis
         *n_PesLin = n_PesLin + n_Peso

         n_VolLin = n_VolLin + n_Volu

         if ActzF27c.F27cPesoKg <> 0
         	n_PesTotal = ActzF27c.F27cPesoKg / Cant
         else
         	n_PesTotal = 0
         EndIf

         Select ActzF26l
         w_NumLot = ActzF26l.F26lNumLot
         w_SitStk = ActzF26l.F26lSitStk
         w_FecCad = ActzF26l.F26lFecCad

            _Sentencia= "Select " + _GCN("F16tPesPal") + " As PesoPalet From F16t" + _em + ;
                        " Where F16tNumPal = '" + ActzF26L.F26lNumPal + "'"
		  			
         	_Err = f3_SqlExec(_Asql,_Sentencia,'F16tCur')
  		  	If _Err < 0
    	         _LxErr = "Error leyendo peso del palet: " + ActzF26L.F26lNumPal + cr + ;
	                      "SENTENCIA: " + _Sentencia + cr + ;
                          "MENSAJE: " + Message() + cr
        	    =Anomalias()
		     	Return
		  	EndIf	
		  	_PesoLin =_PesoLin + F16tCur.PesoPalet
		  Select ActzF26L

         Skip
		*------------------------------------------------------------
*		n_PesLin = F16tCur.PesoPalet / n_CanLin
		*------------------------------------------------------------
         ***> Dar de alta línea albarán.------------------------------
         If ActzF26l.F26lNumLot <> w_NumLot .Or. Eof()
            n_LinAlb = n_LinAlb + 1
       	*------------------------------------------------------------
		
*		  	_PesoLin =F16tCur.PesoPalet
		  	_PesoLin=Round(_PesoLin,3)
*			_PesoTot=_PesoTot+_PesoLin
			
		*------------------------------------------------------------
            If _PesoLin = 0
				 *> Calculamos peso.-------------------------------------------
				 Select ActzF26l
				 Skip -1
					_Sentencia = " Select F08cPesUni From F08c" + _em + ;
					             " Where F08cCodPro ='" + ActzF26l.F26lCodPro + ;
								 "' And F08cCodArt ='" + ActzF26l.F26lCodArt + "'"
					_Ok = f3_SqlExec(_Asql,_Sentencia,'pesoart')			
					=sqlMoreresults(_Asql)
					Select Pesoart
					If !Eof()
						_PesoLin = _PesoLin + (n_CanLin * pesoart.F08cPesUni)
					EndIf
				 Select ActzF26l
				 Skip
					
            EndIf

            Select ActzF27l
            Append Blank
            Replace F27lCodPro With ActzF24l.F24lCodPro, ;
                    F27lTipDoc With ActzF24l.F24lTipDoc, ;
                    F27lNumDoc With ActzF24l.F24lNumDoc, ;
                    F27lDirAso With ActzF24c.F24cDirAso, ;
                    F27lNumAlb With This.ObjParm.PANAlb, ;
                    F27lLinAlb With Right(AllTrim(Str(10000 + n_LinAlb)), 4), ;
                    F27lLinDoc With ActzF24l.F24lLinDoc, ;
                    F27lCodArt With ActzF24l.F24lCodArt, ;
                    F27lNumLot With w_NumLot, ;
                    F27lSitStk With w_SitStk, ;
                    F27lFecCad With w_FecCad, ;
                    F27lCanPed With ActzF24l.F24lCanDoc, ;
                    F27lCanSer With n_CanLin, ;
                    F27lBulIni With 0, ;
                    F27lBulFin With 0, ;
                    F27lBulPic With 0, ;
                    F27lPalets With 0, ;
                    F27lPesBru With _PesoLin, ;
                    F27lVolume With n_VolLin, ;
                    F27lNumTrp With 0, ;
                    F27lFecTrp With Date(), ;
                    F27lHorTrp With Space(8), ;
                    F27lNumRel With 0, ;
                    F27lFecRel With Date(), ;
                    F27lHorRel With Space(8), ;
                    F27lFlgEst With '0'

            Sw = F3_InsTun('F27l', 'ActzF27l', 'N')
            If Sw = .F.
               This.ObjParm.PwCRtn = "72"
               This.LgTAno = "ASLINS"
               This.LgProc = "ActzAS"
               This.Anom
            EndIf

            n_PesAlb = n_PesAlb + n_PesLin
            n_VolAlb = n_VolAlb + n_VolLin
            Store 0 To n_CanLin, n_VolLin, n_BulIni, n_BulFin, n_BulFrc,_PesoLin
            Select ActzF27l
            Zap
            Select ActzF26l
         EndIf
      EndDo

      Select ActzF24l
      Continue
   EndDo                     && While Found()

   *> Una vez calculadas las líneas del albarán, actualizar la cabecera.
   If n_LinAlb > 0 .And. This.ObjParm.PwCRtn < "50"
      Select ActzF27c
      This.CalNumBul(F27cNumAlb, @n_Bultos, @n_Palets, @n_Cajas, @n_Fracciones)

      *> Actualizar el albarán (F27c).
      *F27cPesoKg With n_PesAlb, ;
      Select ActzF27c
      Replace F27cPesoKg With _Peso, ;
              F27cVolume With n_VolAlb, ;
              F27cBultos With n_Bultos, ;
              F27cPalCmp With n_Palets, ;
              F27cCajLib With n_Cajas, ;
              F27cCajPic With n_Fracciones

      lxWhere = "F27cCodPro='" + ActzF24c.F24cCodPro + "' And " + ;
                "F27cTipDoc='" + ActzF24c.F24cTipDoc + "' And " + ;
                "F27cNumDoc='" + ActzF24c.F24cNumDoc + "' And " + ;
                "F27cNumAlb='" + This.ObjParm.PANAlb + "'"

      f_Asigna = 'F27cPesoKg, F27cVolume, F27cBultos, F27cPalCmp, F27cCajLib, F27cCajPic'
      f_Valores = f_Asigna

      Sw = F3_UpdTun('F27c', , f_Asigna, f_Valores, 'ActzF27c', lxWhere, 'N')
      If Sw = .F.
         This.ObjParm.PwCRtn = "73"
         This.LgTAno = "ASCUPD"
         This.LgProc = "ActzAS"
         This.Anom
      EndIf

      *> Actualizar el albarán (F30c).
      Select ActzF30c
      Replace F30cPesoKg With n_PesAlb, ;
              F30cVolume With n_VolAlb, ;
              F30cBultos With n_Bultos, ;
              F30cPalCmp With n_Palets, ;
              F30cCajLib With n_Cajas, ;
              F30cCajPic With n_Fracciones

      lxWhere = "F30cTipEnt='PROP' And " + ;
                "F30cCodEnt='" + ActzF24c.F24cCodPro + "' And " + ;
                "F30cTipDoc='" + ActzF24c.F24cTipDoc + "' And " + ;
                "F30cNumDoc='" + ActzF24c.F24cNumDoc + "' And " + ;
                "F30cAlbRep='" + This.ObjParm.PANAlb + "'"

      f_Asigna = 'F30cPesoKg, F30cVolume, F30cBultos, F30cPalCmp, F30cCajLib, F30cCajPic'
      f_Valores = f_Asigna

      Sw = F3_UpdTun('F30c', , f_Asigna, f_Valores, 'ActzF30c', lxWhere, 'N')
      If Sw = .F.
         This.ObjParm.PwCRtn = "73"
         This.LgTAno = "ASTUPD"
         This.LgProc = "ActzAS"
         This.Anom
      EndIf
   EndIf

   *> Generación del siguiente tipo de albarán.
   Do Case
      *> Ha generado cabecera albarán estupefacientes: Generar albarán de frío.
      Case c_TipAlb == 'E'
         c_tipAlb = 'F'

      *> Ha generado cabecera albarán frío: Generar albarán normal.
      Case c_TipAlb == 'F'
         c_tipAlb = 'N'

      *> Ha generado cabecera albarán normal: Finalizar el proceso.
      Case c_TipAlb == 'N'
         Exit

      *> En cualquier otro caso, salir.
      Otherwise
         Exit
   EndCase
EndDo

If This.ObjParm.PwCRtn >= "50"
   Return
EndIf

*>
*> Aquí, crear el F30c.
*>

*> Cerrar los cursores de trabajo.
If Used('ActzF24c')
   Use In ActzF24c
EndIf
If Used('ActzF24l')
   Use In ActzF24l
EndIf
If Used('ActzF24t')
   Use In ActzF24t
EndIf
If Used('ActzF26l')
   Use In ActzF26l
EndIf
If Used('ActzF26v')
   Use In ActzF26v
EndIf
If Used('ActzF27c')
   Use In ActzF27c
EndIf
If Used('ActzF27l')
   Use In ActzF27l
EndIf
If Used('F27cAlbCur')
   Use In F27cAlbCur
EndIf
If Used('F26vCur')
   Use In F26vCur
EndIf

Return

ENDPROC
PROCEDURE actzar
*>
*> Generar actualizar albarán de reparto.
*> Anulado. Ahora se hace todo en This.ActzAs. AVC - 19.04.2000

Private lxWhere, _NAlb
Local Sw

*> Crear cursores.----------------------------------------------------
=CrtCursor("F24c", "ActzF24c")
=CrtCursor("F24t", "ActzF24t")
=CrtCursor("F27c", "ActzF27c")
=CrtCursor("F30c", "ActzF30c")

*> Inicializar y convertir fechas
This.ConvFec

*> Leer la cabecera del documento de salida.
lxWhere = "F24cCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F24cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F24cNumDoc='" + This.ObjParm.PONDoc + "'"

Sw = f3_sql('*', 'F24c', lxWhere, , , 'ActzF24c')
If Sw = .F.
   This.ObjParm.PwCRtn = "70"
   This.LgTAno = "DOCINE"
   This.LgProc = "ActzAR"
   This.Anom
   Return
EndIf

*> Ir a buscar datos del albarán.-------------------------------------
lxWhere = "F27cCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F27cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F27cNumDoc='" + This.ObjParm.PONDoc + "'"
Sw = f3_sql('*', 'F27c', lxWhere, , , 'ActzF27c')
If Sw = .F.
   This.ObjParm.PwCRtn = "74"
   This.LgTAno = "ASCINE"
   This.LgProc = "ActzAR"
   This.Anom
   Return
EndIf

Select ActzF27c
Go Top

*> Ir a buscar datos de la dirección asociada.------------------------
lxWhere = "F24tCodPro = '" + This.ObjParm.POCPro + "'"+;
          " And F24tTipDoc = '"+ This.ObjParm.POTDoc + "'"+;
          " And F24tNumDoc = '"+ This.ObjParm.PONDoc + "'"
=F3_Sql('*', 'F24t', lxWhere, , , 'ActzF24t')
Select ActzF24t
Go Top
If EOF()
   Append Blank
EndIf

*> Ver si existe el registro de F30c.
lxWhere = "F30cTipEnt='PROP' And F30cCodEnt='" + This.ObjParm.POCPro + "' And " + ;
          "F30cTipAlb='D'    And F30cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F30cNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
          "F30cAlbRep='" + This.ObjParm.PANAlb + "'"
=f3_sql('*', 'F30c', lxWhere, , , 'ActzF30c')

Select ActzF30c
Go Top
If Eof()
   c_Oper = "INS"
   c_CodSit = '0'
   c_HojRut = Space(10)
Else
   c_Oper = "UPD"
   c_CodSit = F30cCodSit
   c_HojRut = F30cHojRut
   Zap
EndIf

*> Tomar el Nº de albarán.
If c_Oper = 'INS' .Or. Empty(This.ObjParm.PANAlb)
   This.ObjParm.PANAlb = Ora_NewNum('NALB', 'N', 'N', 10)
EndIf

Append Blank
Replace F30cTipEnt With 'PROP',              F30cCodEnt With This.ObjParm.POCPro, ;
        F30cTipAlb With 'D',                 F30cTipDoc With This.ObjParm.POTDoc, ;
        F30cAlbRep With This.ObjParm.PANAlb, F30cNumDoc With This.ObjParm.PONDoc, ;
        F30cFecAlb With ActzF27c.F27cFecAlb, F30cFecEmb With ActzF27c.F27cFecEmb, ;
        F30cNomDes With ActzF24t.F24tNomAso, ;
        F30cDirDes With ActzF24t.F24t1erDir, F30cPobDes With ActzF24t.F24tDPobla, ;
        F30cPrvDes With ActzF24t.F24tCProvi, F30cCodPos With ActzF24t.F24tCodPos, ;
        F30cPunOpe With ActzF24t.F24tPunOpe, F30cPalCmp With ActzF27c.F27cPalCmp, ;
        F30cCajLib With ActzF27c.F27cCajLib, F30cCajPic With ActzF27c.F27cCajPic, ;
        F30cBultos With ActzF27c.F27cBultos, ;
        F30cPesoKg With ActzF27c.F27cPesoKg, F30cVolume With ActzF27c.F27cVolume, ;
        F30cFecLle With ActzF27c.F27cFecLle, F30cCodSit With c_CodSit, ;
        F30cFecEnt With _FecMin,             F30cPortes With 'P', ;
        F30cHojRut With c_HojRut,            ;
        F30cTipUrg With ActzF27c.F27cTipUrg, F30cFlete  With ActzF27c.F27cFlete,  ;
        F30cExtrar With ActzF27c.F27cExtrar, F30cFecEmi With _FecMin, ;
        F30cFecTrp With _FecMin,             F30cFecCFe With _FecMin, ;
        F30cRutHab With ActzF27c.F27cRutHab, F30cCodTra With ActzF27c.F27cCodTra

Select ActzF30c
If c_Oper = "INS"
   Sw = F3_InsTun('F30c', 'ActzF30c', 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "75"
      This.LgTAno = "ARCINS"
      This.LgProc = "ActzAR"
      This.Anom
   EndIf
Else
   lxWhere = "F30cTipEnt='PROP' And F30cCodEnt='" + This.ObjParm.POCPro + "' And " + ;
             "F30cTipAlb='D'    And F30cTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
             "F30cNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
             "F30cAlbRep='" + This.ObjParm.PANAlb + "'"

   Sw = F3_UpdTun('F30c', , , , 'ActzF30c', lxWhere, 'N', 'N')
   If Sw = .F.
      This.ObjParm.PwCRtn = "76"
      This.LgTAno = "ARCUPD"
      This.LgProc = "ActzAR"
      This.Anom
   EndIf
EndIf

*> Actualizar el Nº de albarán en la cabecera del documento de salida.
lxWhere = "F24cCodPro = '"     + This.ObjParm.POCPro + "'"+;
          " And F24cTipDoc = '"+ This.ObjParm.POTDoc + "'"+;
          " And F24cNumDoc = '"+ This.ObjParm.PONDoc + "'"

_NAlb = This.ObjParm.PANAlb
Sw = f3_UpdTun("F24c", ,"F24cNumAlb", "_NAlb", , lxWhere, 'N', 'N')
If Sw = .F.
   This.ObjParm.PwCRtn = "76"
   This.LgTAno = "UPDALB"
   This.LgProc = "ActzAR"
   This.Anom
EndIf

If Used('ActzF24c')
   Use In ActzF24c
EndIf
If Used('ActzF24t')
   Use In ActzF24t
EndIf
If Used('ActzF27c')
   Use In ActzF27c
EndIf
If Used('ActzF30c')
   Use In ActzF30c
EndIf

Return

ENDPROC
PROCEDURE calnumbul
*>
*> Calcular el nº de bultos de un albarán.
*> Calcular por tipo de bulto. AVC - 11.10.2000

Parameters NumAlbaran, NoDeBultos, NoDePalets, NoDeCajas, NoDeFracc
Private lxWhere, lxFrom, lxGroup, lxOrder, lxSelect

Store 0 To NoDeBultos, NoDeCajas, NoDePalets, NoDeFracc

Use In (Select("F27lCur"))

*> Crear el cursor de trabajo.
lxSelect= "F26lNumMac, F26lOriRes, F27lNumalb"
lxFrom  = "F26l,F27l"
lxWhere = "F26lCodPro='" + This.ObjParm.POCPro + "' And " + ;
          "F26lTipDoc='" + This.ObjParm.POTDoc + "' And " + ;
          "F26lNumDoc='" + This.ObjParm.PONDoc + "' And " + ;
          "F27lCodPro=F26lCodPro And " + ;
          "F27lTipDoc=F26lTipDoc And " + ;
          "F27lNumDoc=F26lNumDoc And " + ;
          "F27lCodArt=F26lCodArt"

lXGroup = "F26lNumMac, F26lOriRes, F27lNumAlb"

lxOrder = "F26lNumMac, F27lNumAlb"

If f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, lxGroup, 'F27lCur')
   Select F27lCur
   Count For F27lNumAlb==NumAlbaran To NoDeBultos                         && Bultos totales.
   Count For F27lNumAlb==NumAlbaran .And. F26lOriRes=='P' To NoDePalets   && Palets.
   Count For F27lNumAlb==NumAlbaran .And. F26lOriRes=='C' To NoDeCajas    && Cajas.
   Count For F27lNumAlb==NumAlbaran .And. F26lOriRes=='U' To NoDeFracc    && Fracciones.

   Use In F27lCur
EndIf

Select ActzF27c

Return

ENDPROC
PROCEDURE actzet

*> Actualizar tabla para reimpresión de etiquetas.
Local _oldSele

_oldSele = Alias()

If !File('F99t.Dbf')
   Create Table F99t(F99tCodPro C(6), ;
                     F99tCodUbi C(14), ;
                     F99tCodArt C(13), ;
                     F99tNumPal C(10), ;
                     F99tNumLot C(15), ;
                     F99tFecCad DateTime, ;
                     F99tSitStk C(4), ;
                     F99tUniPac N(10, 0), ;
                     F99tPacCaj N(10, 0), ;
                     F99tCajPal N(10, 0), ;
                     F99tCanFis N(10, 0), ;
                     F99tCanRes N(10, 0), ;
                     F99tNumLst C(6))
EndIf

Use In (Select('F99t'))
Select 0
Use F99t
Locate For F99tCodUbi==ActzF16c.F16cCodUbi .And. ;
           F99tCodPro==ActzF16c.F16cCodPro .And. ;
           F99tCodArt==ActzF16c.F16cCodArt .And. ;
           F99tNumLot==ActzF16c.F16cNumLot .And. ;
           F99tFecCad==ActzF16c.F16cFecCad .And. ;
           F99tSitStk==ActzF16c.F16cSitStk

If !Found()
   Append Blank
EndIf

Replace F99tCodPro With ActzF16c.F16cCodPro, ;
        F99tCodUbi With ActzF16c.F16cCodUbi, ;
        F99tCodArt With ActzF16c.F16cCodArt, ;
        F99tNumPal With ActzF16c.F16cNumPal, ;
        F99tNumLot With ActzF16c.F16cNumLot, ;
        F99tFecCad With ActzF16c.F16cFecCad, ;
        F99tSitStk With ActzF16c.F16cSitStk, ;
        F99tUniPac With ActzF16c.F16cUniPac, ;
        F99tPacCaj With ActzF16c.F16cPacCaj, ;
        F99tCajPal With ActzF16c.F16cCajPal, ;
        F99tCanFis With ActzF16c.F16cCanFis, ;
        F99tCanRes With ActzF16c.F16cCanRes

If F99tCanFis <= 0
   Delete Next 1
EndIf

*> Restaurar el entorno anterior.
Use In (Select('F99t'))
If !Empty(_oldSele)
	Select (_oldSele)
EndIf

Return

ENDPROC
PROCEDURE initlocals

*> Inicializar propiedades que se utilizan como variables locales y/o de trabajo.
With This
	.ObjParm.PWCRtn = Space(2)					&& Estado del proceso.

	*> Para el tratamiento de anomalías.
	.LgTAno = Space(10)
	.LgProc = Space(10)
	.LgCant = 0
	.LgArtB = 0
	.LgArtA = 0
	.LoFisB = 0
	.LoFisA = 0
	.LoResB = 0
	.LoResA = 0
	.LsFisB = 0
	.LsFisA = 0
	.LgSOld = Space(4)

	*> Cálculos internos.
	.PEnSa = Space(1)
	.WNPal = Space(10)
	.WCUbi = Space(14)
	.WSStk = Space(4)
	.WNOcu = 0
	.WKOcu = 0
	.WVLib = 0
	.WRowId = Space(10)

	.Sumar = "N"    && Si es "S", se suma  1 a NumOcu de ubicaciones en Entrada
	.Borro = "N"    && Si es "S", se resta 1 a NumOcu de ubicaciones en Salida

	*> Indicadores de búsqueda ("0" - Encontrado, "1" - No encontrado).
	.In82 = "0"
	.In83 = "0"
	.In84 = "0"
	.In85 = "0"
	.In86 = "0"

	*> Indicadores de error ("0" - No error, "1" - Error).
	.In91 = "0"
	.In98 = "0"
	.In99 = "0"
EndWith

Return

ENDPROC
PROCEDURE setlocals

*> Asigna valores a propiedades protegidas de la clase.
*> Ver This.InitLocals para su descripción.

Parameters cPropiedad, cValor

Local lStado

Store .T. To lStado

Do Case
	*> Ubicación para PK de ocupaciones.
	Case Upper(cPropiedad)=="WCUBI"
		This.WCUbi = cValor

	*> RowId de ocupación.
	Case Upper(cPropiedad)=="WROWID"
		This.WRowID = cValor

	*> Resto propiedades, no se permite.
	Otherwise
		Store .F. To lStado
EndCase

Return lStado

ENDPROC
PROCEDURE getlocals

*> Devolver el valor de una propiedad protegida de la clase.
*> Ver This.InitLocals para su descripción.

Parameters cPropiedad

Local cValor

Do Case
	*> Ubicación para PK de ocupaciones.
	Case Upper(cPropiedad)=="WCUBI"
		cValor = This.WCUbi

	*> ID ocupación activo.
	Case Upper(cPropiedad)=="WROWID"
		cValor = This.WRowId

	*> Resto propiedades, no se permite.
	Otherwise
		Store "" To cValor
EndCase

Return cValor

ENDPROC
PROCEDURE orstkobj_access

*> Inicializar clase stocks
If Type('This.oRStkObj') # 'O'
	This.oRStkObj = CreateObject('OraFncRStk')
EndIf

Return This.oRStkObj

ENDPROC
PROCEDURE oubicobj_access

*> Inicializar clase ubicación
If Type('This.oUbicObj') # 'O'
	This.oUbicObj = CreateObject('OraFncUbic')
EndIf

Return This.oUbicObj

ENDPROC


EndDefine 
Define Class orafncalbs as custom



PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

With This
	*> Propiedades de uso interno de la clase.
	._initlocals

	.CodPro = ""				&& Propietario.
	.TipDoc = ""				&& Tipo docuemnto.
	.NumDoc = ""				&& Nº documento.
	.NumLst = ""				&& Nº de lista.
	.NumAlb = ""				&& Nº de albarán.

	.NewAlb = 'N'				&& Albarán nuevo.

	.UsrError = ""				&& Texto errores.
EndWith

Return

ENDPROC
PROCEDURE _initlocals

*> Inicializar las propiedades locales de la clase.

With This
	.UbicObj = CreateObject('OraFncUbic')
	._of24c = ""
EndWith

ENDPROC
PROCEDURE albarandocumento

*> Calcular el / los albaranes de un documento de salida.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.TipDoc, tipo documento.
*>	- This.NumDoc, nº documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:


Local lStado

This.UsrError = ""

*> Cargar los datos  necesarios para los cálculos, a partir de las listas de trabajo del documento.
lStado = This._cargarlistasdocumento()
If !lStado
	*> El mensaje de error ya viene asignado.
	Return lStado
EndIf

*> Calcula los albaranes a partir de los datos cargados de las listas del documento.
lStado = This._albaraneslistas()
If !lStado
	*> El mensaje de error ya viene asignado.
	Return lStado
EndIf

*> Actualiza los albaranes en el detalles las listas del documento.
lStado = This._updatealbaraneslistas()
If !lStado
	*> El mensaje de error ya viene asignado.
	Return lStado
EndIf

*> Generar los albaranes calculados del documento.
lStado = This._albaranesdocumento()
If !lStado
	*> El mensaje de error ya viene asignado.
	Return lStado
EndIf

Return lStado

ENDPROC
PROCEDURE _albaraneslistas

*> Asigna / calcula nº albarán al detalle de listas del documento a calcular.

*> Recibe:
*>	- ___F26L, cursor con los datos a procesar. Se carga en This._cargarlistasdocumento()

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.
*>	- ___F26L, cursor actualizado con el albarán.

*> Llamado desde:
*>	- This.AlbaranDocumento, calcular albarán(es) de un documento.

Local lStado
Private cNumeroAlbaran

This.UsrError = ""
lStado = .T.

If !Used("___F26L")
	This._cargarlistasdocumento
EndIf

Select ___F26L
Locate For Empty(F26lNumAlb)
If Found()
	cNumeroAlbaran = PadR(Ora_NewNum('NALB', 'N', 'N', 10), 13, Space(1))
	Select ___F26L
	Replace All F26lNumAlb With cNumeroAlbaran For Empty(F26lNumAlb)
EndIf

Select ___F26L
Go Top

Return

ENDPROC
PROCEDURE _cargarlistasdocumento

*> Genera cursor de trabajo con las listas del documento a calcular.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.TipDoc, tipo documento.
*>	- This.NumDoc, nº documento.
*>	- This.NewAlb, renumerar albaranes.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.
*>	- ___F26L, cursor con los datos a procesar.

*> Llamado desde:
*>	- This.AlbaranDocumento, calcular albarán(es) documento.
*>	- This._albaraneslistas, calcular los albaranes con los datos de listas.

*> Historial de modificaciones:
*> 20.11.2007 (AVC) Guardar el nº de traspaso al ERP.
*> 08.07.2008 (AVC) Agregar columna 'Palet' al cursor de trabajo para poder calcular peso / volumen albarán por bultos.

Local lStado, oF26l, oF30c, oF27l, nRecNo
Private cWhere, cOrder, cField, cFromF, cLinDoc

This.UsrError = ""
lStado = .T.

Use In (Select ("___F26L"))

*> Cabecera del documento de salida.
m.F24cCodPro = This.CodPro
m.F24cTipDoc = This.TipDoc
m.F24cNumDoc = This.NumDoc

If !f3_seek("F24c")
	This.UsrError = "El documento no existe"
	Return .F.
EndIf

Select F24c
Scatter Name This._of24c

*> Cláusula de selección de listas del documento.
cWhere = 		  "F26lCodPro='" + This.CodPro + "' And "
cWhere = cWhere + "F26lTipDoc='" + This.TipDoc + "' And "
cWhere = cWhere + "F26lNumDoc='" + This.NumDoc + "' And "
cWhere = cWhere + "F26lEstMov > '0' And F26lEstMov < '4'"

cFromF = "F26l"
cOrder = "F26lCodPro,F26lTipDoc,F26lNumDoc"

cField = 		  "F26lCodPro,F26lTipDoc,F26lNumDoc,F26lLinDoc,F26lNumLst,F26lNumMac,F26lCodArt,F26lCanFis,"
cField = cField + "F26lLinDoc,F26lSitStk,F26lNumLot,F26lFecCad,F26lNumAlb,F26lIdOcup,F26lNumMov,F26lOriRes,F26lNumPal"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , "___F26L")
If !lStado
	If _xier<=0
		This.UsrError = "Error carga datos lista"
	Else
		This.UsrError = "No hay datos para procesar"
	EndIf

	Use In (Select ("___F26L"))
	Return lStado
EndIf

*> Agregar campos auxiliares (ruta, estado, ...)
=CrtFCursor("___F26L", [TBL=F30c,FLD=F30cHojRut])
=CrtFCursor("___F26L", [TBL=F30c,FLD=F30cCodSit])
=CrtFCursor("___F26L", [TBL=F27l,FLD=F27lNumTrp])
=AddFldToCursor("___F26L", [NAME=Estado,TYPE=C,LENGTH=1])

*> Cargar datos de los albaranes ya existentes, si los hay.
Select ___F26L
Scan For !Empty(F26lNumAlb)
	Scatter Name oF26l
	nRecNo = RecNo()

	*> Validar la política de servicio de la línea.
	cLinDoc = PadL(AllTrim(Str(oF26l.F26lLinDoc)), 4, '0')
	lStado = This._validarpoliticaservicio(cLinDoc)
	lStado = This._validarpoliticaservicio(, , , cLinDoc)

	If !lStado
		Select ___F26L
		Delete Next 1
		Loop
	EndIf

	cWhere = 		  "F27cCodPro='" + oF26l.F26lCodPro + "' And "
	cWhere = cWhere + "F27cTipDoc='" + oF26l.F26lTipDoc + "' And "
	cWhere = cWhere + "F27cNumDoc='" + oF26l.F26lNumDoc + "' And "
	cWhere = cWhere + "F27cNumAlb='" + oF26l.F26lNumAlb + "' And "
	cWhere = cWhere + "F30cTipEnt='PROP' And F30cCodEnt=F27cCodPro And F30cTipDoc=F27cTipDoc And F30cNumDoc=F27cNumDoc And F30cAlbRep=F27cNumAlb"

	lStado = f3_sql("*", "F27c,F30c", cWhere, , , "___F30C")
	If !lStado
		If _xier<=0
			This.UsrError = "Error carga datos albarán"
			Use In (Select ("___F26L"))
			Use In (Select ("___F30C"))
			Return lStado
		EndIf

		*> No hay albarán. Debería existir ó el proceso se ha lanzado y no ha terminado.
		*Select ___F26L
		*Replace All F26lNumAlb With Space(13) For F26lNumAlb==oF26l.F26lNumAlb
	Else
		Select ___F30C
		Go Top
		Scatter Name oF30c

		Select ___F26L
		Replace All F30cHojRut With oF30c.F30cHojRut For F26lNumAlb==oF26l.F26lNumAlb
		Replace All F30cCodSit With oF30c.F30cCodSit For F26lNumAlb==oF26l.F26lNumAlb
	EndIf

	*> Guardar el Nº de traspaso al ERP.
	If Empty(F27lNumTrp)
		cWhere = 		  "F27lCodPro='" + oF26l.F26lCodPro + "' And "
		cWhere = cWhere + "F27lTipDoc='" + oF26l.F26lTipDoc + "' And "
		cWhere = cWhere + "F27lNumDoc='" + oF26l.F26lNumDoc + "' And "
		cWhere = cWhere + "F27lLinDoc='" + cLinDoc + "' And "
		cWhere = cWhere + "F27lNumAlb='" + oF26l.F26lNumAlb + "'"

		lStado = f3_sql("*", "F27l", cWhere, , , "___F27LTRP")
		If !lStado
			If _xier<=0
				This.UsrError = "Error carga traspaso albarán"
				Use In (Select ("___F26L"))
				Use In (Select ("___F30C"))
				Use In (Select ("___F27LTRP"))
				Return lStado
			EndIf
		Else
			Select ___F27LTRP
			Go Top
			Scatter Name oF27l

			Select ___F26L
			Replace All F27lNumTrp With oF27l.F27lNumTrp For F26lCodPro==oF26l.F26lCodPro And ;
															 F26lTipDoc==oF26l.F26lTipDoc And ;
															 F26lNumDoc==oF26l.F26lNumDoc And ;
															 F26lLinDoc==oF26l.F26lLinDoc And ;
															 F26lNumAlb==oF26l.F26lNumAlb
		EndIf
	EndIf

	Select ___F26L
	Go nRecNo
EndScan

Use In (Select ("___F30C"))
Use In (Select ("___F27LTRP"))

Select ___F26L
Replace All Estado With '0'
Go Top

Return

ENDPROC
PROCEDURE _updatealbaraneslistas

*> Actualiza el albarán en el detalle listas del documento a procesar.

*> Recibe:
*>	- ___F26L, cursor con los datos a procesar. Se carga en This._cargarlistasdocumento()

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:
*>	- This.AlbaranDocumento, calcular albarán(es) documento.

Local lStado, oF26l
Private cWhere, cCampos, cValores, cNumAlb

This.UsrError = ""
lStado = .T.

Select ___F26L
Go Top
Do While !Eof()
	Scatter Name oF26l
	cNumAlb = oF26l.F26lNumAlb

	cWhere = "F26lNumMov='" + oF26l.F26lNumMov + "'"
	cCampos = "F26lNumAlb"
	cValores = "cNumAlb"
	=f3_updtun("F26l", , cCampos, cValores, , cWhere)

	Select ___F26L
	Skip
EndDo

Return

ENDPROC
PROCEDURE _albaranesdocumento

*> Actualizar los albaranes calculados a partir de las listas del documento.

*> Recibe:
*>	- ___F26L, cursor con los datos a procesar. Se carga en This._cargarlistasdocumento()

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:
*>	- This.AlbaranDocumento, calcular albarán(es) de un documento.

*> Historial de modificaciones:
*> 20.11.2007 (AVC) Guardar el nº de traspaso al ERP.
*> 08.07.2008 (AVC) Calcular peso / volumen a partir de palets (F16t), en lugar de por la composición (F26l).
*>					Esto puede provocar diferencias entre peso total albarán y la suma del peso de las líneas.

Local lStado, cLinDoc, cLinAlb, nLinAlb
Private oF26l, oF24t, oF27c, oF30c, oF24l
Private nBultos, nPalets, nCajas, nFracciones, nPeso, nVolumen, nPesoLin, nVoluLin

This.UsrError = ""
Store 0 To nBultos, nPalets, nCajas, nFracciones, nPeso, nVolumen, nPesoLin, nVoluLin

*> Se borran los albaranes que ya existan, si los hay.
lStado = This._eliminaralbaranesdocumento()
If !lStado
	*> Los mensajes se asignan dentro de la función.
	Return lStado
EndIf

*> Datos dirección.
m.F24tCodPro = This.CodPro
m.F24tTipDoc = This.TipDoc
m.F24tNumDoc = This.NumDoc

If f3_seek("F24t")
	Select F24t
	Scatter Name oF24t
Else
	Select F24t
	Scatter Name oF24t Blank
EndIf

*> Cursores de trabajo.
=CrtCursor("F27c", "___F27C")
=CrtCursor("F27l", "___F27L")
=CrtCursor("F30c", "___F30C")

Select ___F26L
Scan For !Empty(F26lNumAlb) .And. F30cCodSit<='0'
	Scatter Name oF26l

	Select ___F27C
	Locate For F27cNumAlb==oF26l.F26lNumAlb
	If !Found()
		*> Crear la cabecera del albarán.
		nLinAlb = 0
		Append Blank

		Replace F27cCodPro With oF26l.F26lCodPro
		Replace F27cTipDoc With oF26l.F26lTipDoc
		Replace F27cNumDoc With oF26l.F26lNumDoc
		Replace F27cDirAso With This._of24c.F24cDirAso
		Replace F27cNumAlb With oF26l.F26lNumAlb
		Replace F27cFecAlb With Date()
		Replace F27cFecEmb With _FecMin
		Replace F27cHorEnt With Space(8)
		Replace F27cHorSal With Time()
		Replace F27cEtique With 0
		Replace F27cFecLle With Date()
		Replace F27cNumTrp With 0
		Replace F27cFecTrp With _FecMin
		Replace F27cHorTrp With Space(8)
		Replace F27cConAlb With This._of24c.F24cConAlb
		Replace F27cNumTRu With 0
		Replace F27cFecTRu With _FecMin
		Replace F27cHorTRu With Space(8)
		Replace F27cTipUrg With Space(1)
		Replace F27cFlete  With oF24t.F24tFlete
		Replace F27cExtrar With oF24t.F24tExtrad
		Replace F27cRutHab With This._of24c.F24cRutHab
		Replace F27cCodTra With This._of24c.F24cCodTra
		Replace F27cFlgEst With '0'
		Replace F27cTipAlb With 'N'

		*> Calcular nº y tipo de bultos del albarán.
		=This._calnumbultos(oF26l.F26lNumAlb, @nBultos, @nPalets, @nCajas, @nFracciones)

		*> Calcular peso y volumen del albarán, a partir de los palets.
		=This._calpesovolumen(oF26l.F26lNumAlb, @nPeso, @nVolumen)

		Select ___F27C
		Replace F27cBultos With nBultos
		Replace F27cPalCmp With nPalets
		Replace F27cCajLib With nCajas
		Replace F27cCajPic With nFracciones

		Replace F27cPesoKg With nPeso
		Replace F27cVolume With nVolumen
	EndIf

     *> Calcular peso y volumen de la cantidad de la línea.
	 With This.UbicObj
		 .CodPro = oF26l.F26lCodPro
		 .CodArt = oF26l.F26lCodArt
		 .CanUbi = oF26l.F26lCanFis
		 =.PesVolAr()
		 nPesoLin = .PesOcu
		 nVoluLin = .VolOcu
	 EndWith

	Select ___F27C
	Scatter Name oF27c

	Select ___F30C
	Locate For F30cAlbRep==oF26l.F26lNumAlb
	If !Found()
		*> Crear los datos auxiliares del albarán.
		Append Blank
		Replace F30cTipEnt With 'PROP'
		Replace F30cCodEnt With oF26l.F26lCodPro
		Replace F30cTipAlb With 'D'
		Replace F30cTipDoc With oF26l.F26lTipDoc
		Replace F30cAlbRep With oF26l.F26lNumAlb
		Replace F30cNumDoc With oF26l.F26lNumDoc
		Replace F30cFecAlb With oF27c.F27cFecAlb
		Replace F30cFecEmb With oF27c.F27cFecEmb
		Replace F30cNomDes With oF24t.F24tNomAso
		Replace F30cDirDes With oF24t.F24t1erDir
		Replace F30cPobDes With oF24t.F24tDPobla
		Replace F30cPrvDes With oF24t.F24tCProvi
		Replace F30cCodPos With oF24t.F24tCodPos
		Replace F30cPunOpe With oF24t.F24tPunOpe
		Replace F30cPalCmp With oF27c.F27cPalCmp
		Replace F30cCajLib With oF27c.F27cCajLib
		Replace F30cCajPic With oF27c.F27cCajPic
		Replace F30cBultos With oF27c.F27cBultos
		Replace F30cPesoKg With oF27c.F27cPesoKg
		Replace F30cVolume With oF27c.F27cVolume
		Replace F30cFecLle With oF27c.F27cFecLle
		Replace F30cCodSit With '0'
		Replace F30cFecEnt With _FecMin
		Replace F30cPortes With This._of24c.F24cPortes
		Replace F30cHojRut With Space(10)
		Replace F30cTipUrg With oF27c.F27cTipUrg
		Replace F30cFlete  With oF27c.F27cFlete
		Replace F30cExtrar With oF27c.F27cExtrar
		Replace F30cFecEmi With _FecMin
		Replace F30cFecTrp With _FecMin
		Replace F30cFecCFe With _FecMin
		Replace F30cRutHab With oF27c.F27cRutHab
		Replace F30cCodTra With oF27c.F27cCodTra

		Scatter Name oF30c
	EndIf

	cLinDoc = PadL(AllTrim(Str(oF26l.F26lLinDoc)), 4, '0')

	Select ___F27L
	Locate For F27lNumAlb==oF26l.F26lNumAlb .And. ;
			   F27lCodPro==oF26l.F26lCodPro .And. ;
			   F27lTipDoc==oF26l.F26lTipDoc .And. ;
			   F27lNumDoc==oF26l.F26lNumDoc .And. ;
			   F27lLinDoc==cLinDoc .And. ;
			   F27lNumLot==oF26l.F26lNumLot .And. ;
			   F27lSitStk==oF26l.F26lSitStk
	If !Found()
		*> Crear la línea de detalle del albarán.
		nLinAlb = nLinAlb + 1
		cLinAlb = PadL(AllTrim(Str(nLinAlb)), 4, '0')

		*> Línea de detalle del documento de salida.
		m.F24lCodPro = This.CodPro
		m.F24lTipDoc = This.TipDoc
		m.F24lNumDoc = This.NumDoc
		m.F24lLinDoc = cLinDoc

		If f3_seek("F24l")
			Select F24l
			Scatter Name oF24l
		Else
			*> Tiene que existir la línea de detalle !!!!!
			Select F24l
			Scatter Name oF24l Blank
		EndIf

		Select ___F27L
		Append Blank
		Replace F27lCodPro With oF26l.F26lCodPro
		Replace F27lTipDoc With oF26l.F26lTipDoc
		Replace F27lNumDoc With oF26l.F26lNumDoc
		Replace F27lDirAso With This._of24c.F24cDirAso
		Replace F27lNumAlb With oF26l.F26lNumAlb
		Replace F27lLinAlb With cLinAlb
		Replace F27lLinDoc With cLinDoc
		Replace F27lCodArt With oF26l.F26lCodArt
		Replace F27lNumLot With oF26l.F26lNumLot
		Replace F27lSitStk With oF26l.F26lSitStk
		Replace F27lFecCad With oF26l.F26lFecCad
		Replace F27lCanPed With oF24l.F24lCanDoc
		Replace F27lBulIni With 0
		Replace F27lBulFin With 0
		Replace F27lBulPic With 0
		Replace F27lPalets With 0
		Replace F27lNumTrp With oF26l.F27lNumTrp
		Replace F27lFecTrp With Date()
		Replace F27lHorTrp With Space(8)
		Replace F27lNumRel With 0
		Replace F27lFecRel With Date()
		Replace F27lHorRel With Space(8)
		Replace F27lFlgEst With '0'
	EndIf

	Replace F27lCanSer With F27lCanSer + oF26l.F26lCanFis
	Replace F27lPesBru With F27lPesBru + nPesoLin
	Replace F27lVolume With F27lVolume + nVoluLin

	*>
EndScan

*> Grabar los datos generados.
Select ___F27C
Go Top
Do While !Eof()
	lStado = f3_instun("F27c", "___F27C")
	Select ___F27C
	Skip
EndDo

Select ___F30C
Go Top
Do While !Eof()
	lStado = f3_instun("F30c", "___F30C")
	Select ___F30C
	Skip
EndDo

Select ___F27L
Go Top
Do While !Eof()
	lStado = f3_instun("F27l", "___F27L")
	Select ___F27L
	Skip
EndDo

Use In (Select ("___F27C"))
Use In (Select ("___F27L"))
Use In (Select ("___F30C"))

Return

ENDPROC
PROCEDURE _eliminaralbaranesdocumento

*> Elimina los albaranes previos de un documento.

*> Recibe:
*>	- This.___F26L, cursor con los datos. Si no existe se crea.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:
*>	- This._albaranesdocumento(), generar los albaranes calculados del documento.

Local lStado, nRecNo
Private oF26l

This.UsrError = ""

If !Used("___F26L")
	This._cargarlistasdocumento
EndIf

Select ___F26L
Locate For !Empty(F26lNumAlb) .And. F30cCodSit<='0' .And. Estado=='0'
Do While Found()
	Scatter Name oF26l
	nRecNo = RecNo()

	lStado = This.__eliminaralbaran(oF26l.F26lCodPro, oF26l.F26lTipDoc, oF26l.F26lNumDoc, oF26l.F26lNumAlb)

	Select ___F26L
	Replace All Estado With '1' For F26lNumAlb==oF26l.F26lNumAlb
	Go Top
	Continue
EndDo

*> Se borran los posibles albaranes que no tengan correspondencia con las listas de trabajo.
lStado = This._eliminaralbaranesanteriores()

Return

ENDPROC
PROCEDURE __eliminaralbaran

*> Eliminar UN albarán de salida.

*> Recibe:
*>	- Propietario, Tipo, Documento, Albarán.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:
*>	- This._eliminaralbaranesdocumento(), eliminar los albaranes de un documento de salida.

Parameters cCodPro, cTipDoc, cNumDoc, cNumAlb
Local lStado
Private cWhere

This.UsrError = ""

*> Eliminar las líneas de detalle del albarán.
cWhere = 		  "F27lCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F27lTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F27lNumDoc='" + cNumDoc + "' And "
cWhere = cWhere + "F27lNumAlb='" + cNumAlb + "'"

lStado = f3_deltun('F27l', , cWhere)

*> Eliminar los datos auxiliares del albarán.
cWhere = 		  "F30cTipEnt='PROP' And F30cCodEnt='" + cCodPro + "' And "
cWhere = cWhere + "F30cTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F30cNumDoc='" + cNumDoc + "' And "
cWhere = cWhere + "F30cAlbRep='" + cNumAlb + "'"

lStado = f3_deltun('F30c', , cWhere)

*> Eliminar la cabecera del albarán.
cWhere = 		  "F27cCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F27cTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F27cNumDoc='" + cNumDoc + "' And "
cWhere = cWhere + "F27cNumAlb='" + cNumAlb + "'"

lStado = f3_deltun('F27c', , cWhere)

Return

ENDPROC
PROCEDURE _calnumbultos

*> Calcular el nº de bultos de un albarán.
*> Los parámetros de bultos que devuelve se deben de pasar por referencia.

*> Recibe:
*>	- Nº albarán.
*>	- ___F26L, cursor con detalle listas.

*> Devuelve:
*>	- Bultos totales.
*>	- Palets completos.
*>	- Cajas completas.
*>	- Bultos de fracciones.

*> Llamado desde:
*>	- This._albaranesdocumento(), generar albaranes de un documento de salida.

Parameters NumAlbaran, NoDeBultos, NoDePalets, NoDeCajas, NoDeFracc

Store 0 To NoDeBultos, NoDeCajas, NoDePalets, NoDeFracc

*> Crear el cursor de trabajo.
Select F26lNumMac, F26lOriRes ;
	From ___F26L ;
	Into Cursor ___F26LSUM ;
	Where F26lNumAlb==NumAlbaran ;
	Group By F26lNumMac, F26lOriRes

Select ___F26lSUM
Count To NoDeBultos                         	&& Bultos totales.
Count For F26lOriRes=='P' To NoDePalets   		&& Palets.
Count For F26lOriRes=='C' To NoDeCajas 	   		&& Cajas.
Count For F26lOriRes=='U' To NoDeFracc  	  	&& Fracciones.

Use In ___F26lSUM

Return

ENDPROC
PROCEDURE _eliminaralbaranesanteriores

*> Elimina los albaranes que no tienen correspondencia con las listas de trabajo.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.TipDoc, tipo documento.
*>	- This.NumDoc, nº documento.
*>	- This.___F26L, cursor con los datos.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:
*>	- This._eliminaralbaranesdocumento(), eliminar los albaranes previos del documento.

Local lStado, oF27c
Private cWhere

This.UsrError = ""

*> Cláusula de selección de datos.
cWhere = 		  "F27cCodPro='" + This.CodPro + "' And "
cWhere = cWhere + "F27cTipDoc='" + This.TipDoc + "' And "
cWhere = cWhere + "F27cNumDoc='" + This.NumDoc + "'"

lStado = f3_sql("*", "F27c", cWhere, , , "___F27CDEL")
If !lStado
	If _xier<=0
		This.UsrError = "Error carga albaranes anteriores"
	EndIf

	Use In (Select ("___F27CDEL"))
	Return lStado
EndIf

Select ___F27CDEL
Do While !Eof()
	Scatter Name oF27c

	Select ___F26L
	Locate For F26lNumAlb==oF27c.F27cNumAlb
	If !Found()
		*> Albarán sin correspondencia en listas: Borrar.
		lStado = This.__eliminaralbaran(oF27c.F27cCodPro, oF27c.F27cTipDoc, oF27c.F27cNumDoc, oF27c.F27cNumAlb)
	EndIf

	Select ___F27CDEL
	Skip
EndDo

Use In (Select ("___F27CDEL"))
Return

ENDPROC
PROCEDURE _validarpoliticaservicio

*> Validar la política de servicio de una línea de documento.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.TipDoc, tipo documento.
*>	- This.NumDoc, nº de documento.
*>	ó bien:
*>	- cPropietario, cTipo, cDocumento.
*>	- cLinea, nº de línea de documento (opcional).
*>	- This._of24c, cabecera del documento (leída en This._cargarlistasdocumento).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensaje de error.

*> Llamado desde:
*>	- This._cargarlistasdocumento, Cargar listas del documento a procesar.

Parameters cCodPro, cTipDoc, cNumDoc, cLinDoc

Local lStado, nPolSer
Private cWhere, cPropietario, cTipo, cDocumento

This.UsrError = ""

*> Validar estado documento según política de servicio.
If This._of24c.F24cPolSer<='0' .Or. This._of24c.F24cPolSer>'2'
	*> Sin política de servicio.
	Return .T.
EndIf

*> Asignar valores a propiedades.
cPropietario = Iif(Type('cCodPro')=='C', cCodPro, This.CodPro)
cTipo = Iif(Type('cTipDoc')=='C', cTipDoc, This.TipDoc)
cDocumento = Iif(Type('cNumDoc')=='C', cNumDoc, This.NumDoc)

*> Comparar estado líneas de documento con la política de servicio.
cWhere = 		  "F24lCodPro='" + cPropietario + "' And "
cWhere = cWhere + "F24lTipDoc='" + cTipo + "' And "
cWhere = cWhere + "F24lNumDoc='" + cDocumento + "' And "
cWhere = cWhere + "F24lFlgEst<'2'"

*> Completar cláusula de lectura según la política de servicio del documento.
If This._of24c.F24cPolSer=='1' .And. Type('cLinDoc')=='C'
	cWhere = cWhere + " And F24lLinDoc='" + cLinDoc + "'"
EndIf

lStado = f3_sql("*", "F24l", cWhere, , , "___F24LPOLSER")
If !lStado
	If _xier<=0
		This.UsrError = "Error carga detalle PolSer"
		Use In (Select ("___F24LPOLSER"))
		Return lStado
	EndIf
Else
	This.UsrError = "Documento no cumple política de servicio"
EndIf

Use In (Select ("___F24LPOLSER"))
Return !lStado

ENDPROC
PROCEDURE validarpolser

*> Validar la política de servicio de un alínea de pedido.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento.
*>	- cLinea, nº de línea de documento (opcional).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensajes de error.

Parameters cCodPro, cTipDoc, cNumDoc, cLinDoc
Local lStado

This.UsrError = ""

*> Cabecera del documento de salida.
m.F24cCodPro = cCodPro
m.F24cTipDoc = cTipDoc
m.F24cNumDoc = cNumDoc

If !f3_seek("F24c")
	This.UsrError = "El documento no existe"
	Return .F.
EndIf

Select F24c
Scatter Name This._of24c

lStado = This._validarpoliticaservicio(cCodPro, cTipDoc, cNumDoc, cLinDoc)

Return lStado

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:
*> 20.11.2007 (AVC) Memorizar el nº de traspaso de albarán al ERP para evitar duplicados en traspasos.
*>					Modificado método This._cargarlistasdocumento
*>					Modificado método This._albaranesdocumento
*> 08.07.2008 (AVC) Modificado el cálculo de peso y volumen a partir de palet (F16t), en lugar de artículos (F26l)
*>					Modificado método This._albaranesdocumento
*>					Modificado método This._cargarlistasdocumento
*>					Añadido método This._calpesovolumen

ENDPROC
PROCEDURE _calpesovolumen

*!*	*> Calcular peso y volumen de un albarán a partir de los bultos que los componen.
*!*	*> Los parámetros de peso / volumen que devuelve se deben de pasar por referencia.

*!*	*> Recibe:
*!*	*>	- Nº albarán.
*!*	*>	- ___F26L, cursor con detalle listas.

*!*	*> Devuelve:
*!*	*>	- Peso del albarán.
*!*	*>	- Volumen del albarán.

*!*	*> Llamado desde:
*!*	*>	- This._albaranesdocumento(), generar albaranes de un documento de salida.

*!*	*> Historial de modificaciones:
*!*	*> 08.07.2008 (AVC) Creación.

Parameters NumAlbaran, nPeso, nVolumen

*!*	Private oUbi, oAlb

*!*	Store 0 To nPeso, nVolumen
*!*	oUbi = CreateObject("OraFncUbic")

*!*	*> Crear el cursor de trabajo.
*!*	Select F26lNumPal ;
*!*		From ___F26L ;
*!*		Into Cursor ___F26LSUM ;
*!*		Where F26lNumAlb==NumAlbaran ;
*!*		Group By F26lNumPal

*!*	Select ___F26LSUM
*!*	Go Top
*!*	Do While !Eof()
*!*		Scatter Name oAlb

*!*		With oUbi
*!*			.NumPal = oAlb.F26lNumPal
*!*			.CalcPesVolPal

*!*			nPeso = nPeso + .PesOcu
*!*			nVolumen = nVolumen + .VolOcu
*!*		EndWith

*!*		Select ___F26LSUM
*!*		Skip
*!*	EndDo

*!*	Use In ___F26LSUM

Return

ENDPROC
PROCEDURE Init

*> Historial de modificaciones:

*>	Versión 1.00a.0001 - AVC 12.06.2007

=DoDefault()

ENDPROC


EndDefine 
