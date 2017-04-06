
*> Funciones anexas a importación de pedidos de clientes.

*> Recibe:
*>	- cModo, origen de la llamada:
*>                    'I', al empezar el proceso de exportación.
*>                    'F', al finalizar el proceso de exportación.
*>	- oRegistroFichero, registro actual fichero destino.
*>	- oRegistroOrigen, registro actual fichero origen.
*>	- oClass, referencia a la propia clase.
*>	- cPrm1, cPrm2, cPrm3, ..., parámetros adicionales:
*>		cPrm1: ValidarPedidosRecibidos --> Array de mensajes a devolver.

*> Devuelve:
*>	- lStado (.T. / .F.)
*>	- cEstado, texto de error.

*> Historial de modificaciones:
*> 11.07.2007 (AVC) Incorporar sistema automático de mensajes.
*>					Modificar tratamiento de baja de líneas de detalle.

Function WTien
Parameters cModo, cEstado, oRegistroFichero, oRegistroOrigen, oClass, cPrm1

local nInx
private vMensajes

lStado = .T.
cEstado = ""

*> Selección del momento en que se realiza la llamada.
Do Case
	*> Al empezar la importación de pedidos.
	Case cModo=='I'

	*> Al finalizar la importación de pedidos.
	Case cModo=='F'
		Dimension cPrm1[1]
		lStado = ValidarPedidosRecibidos(@cPrm1)
		=GrabarPedidosRecibidos(@cPrm1)
EndCase

return lStado

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> Funciones de uso general en pedidos de ventas.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*> Validar los datos de los pedidos recibidos. Paso previo a grabar los datos.
*> Recibe los cursores imagen de los ficheros de pedidos (F24?). Ver <Comm>.__crearcursoresventas.

*> Recibe:
*>	- Mensaje a devolver.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Array de mensajes de error.

Function ValidarPedidosRecibidos

Parameters cMensaje

local lStado, oDoc, nInx, lError
private cWhere

lStado = .T.
cMensaje = ""
nInx = 0

*> Se inicializa el cursor de control de pedidos.
select __DOCS
zap

*> Transformar en observaciones la referenciua de cliente, que viene como cabecera.
=GenerarReferenciaCliente()

*> Validar la cabecera de pedidos.
select __F24cDOCCAB
Go Top
do while !eof()
	scatter name oDoc

	select __DOCS
	locate for __codpro==oDoc.F24cCodPro and __tipdoc==oDoc.F24cTipDoc and __numdoc==oDoc.F24cNumDoc
	if !found()
		append blank
		replace __codpro with oDoc.F24cCodPro
		replace __tipdoc with oDoc.F24cTipDoc
		replace __numdoc with oDoc.F24cNumDoc
	endif
	replace __flgc with 'S'						&& Flag control cabeceras.

	select __F24cDOCCAB
	skip
enddo

*> Validar las líneas de detalle.
select __F24lDOCLIN
Go Top
do while !eof()
	scatter name oDoc	

	select __DOCS
	locate for __codpro==oDoc.F24lCodPro and __tipdoc==oDoc.F24lTipDoc and __numdoc==oDoc.F24lNumDoc
	if !found()
		append blank
		replace __codpro with oDoc.F24lCodPro
		replace __tipdoc with oDoc.F24lTipDoc
		replace __numdoc with oDoc.F24lNumDoc
	endif
	replace __flgl with 'S'						&& Flag control detalle.

	select __F24lDOCLIN
	skip
enddo

*> Validar las direcciones.
select __F24tDOCDIR
Go Top
do while !eof()
	scatter name oDoc	

	select __DOCS
	locate for __codpro==oDoc.F24tCodPro and __tipdoc==oDoc.F24tTipDoc and __numdoc==oDoc.F24tNumDoc
	if !found()
		append blank
		replace __codpro with oDoc.F24tCodPro
		replace __tipdoc with oDoc.F24tTipDoc
		replace __numdoc with oDoc.F24tNumDoc
	endif
	replace __flgd with 'S'						&& Flag control direcciones.

	select __F24tDOCDIR
	skip
enddo

*> Validar las observaciones.
select __F24oDOCOBS
Go Top
do while !eof()
	scatter name oDoc	

	select __DOCS
	locate for __codpro==oDoc.F24oCodPro and __tipdoc==oDoc.F24oTipDoc and __numdoc==oDoc.F24oNumDoc
	if !found()
		append blank
		replace __codpro with oDoc.F24oCodPro
		replace __tipdoc with oDoc.F24oTipDoc
		replace __numdoc with oDoc.F24oNumDoc
	endif
	replace __flgo with 'S'						&& Flag control observaciones.

	select __F24oDOCOBS
	skip
enddo

select __DOCS
Go Top
do while !eof()
	scatter name oDoc
	lError = .F.

	Do Case
		*> Pedido sin cabecera.
		case empty(__flgc)
			nInx = nInx + 1
			dimension cMensaje[nInx]
			cMensaje[nInx] = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000021]), oDoc.__numdoc)
			lError = .T.

		*> Pedido sin líneas
		case empty(__flgl)
			nInx = nInx + 1
			dimension cMensaje[nInx]
			cMensaje[nInx] = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000022]), oDoc.__numdoc)
			lError = .T.

		*> Pedido sin direcciones.
		case empty(__flgd)
			nInx = nInx + 1
			dimension cMensaje[nInx]
			cMensaje[nInx] = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000023]), oDoc.__numdoc)
			lError = .T.

		*> Las observaciones no se validan. Pueden no venir.
*!*			case empty(__flgo)
	EndCase

	if lError
		*> Eliminar pedido erróneo.
		select __F24cDOCCAB
		delete for F24cCodPro==oDoc.__codpro and F24cTipDoc==oDoc.__tipdoc and F24cNumDoc==oDoc.__numdoc

		select __F24lDOCLIN
		delete for F24lCodPro==oDoc.__codpro and F24lTipDoc==oDoc.__tipdoc and F24lNumDoc==oDoc.__numdoc

		select __F24tDOCDIR
		delete for F24tCodPro==oDoc.__codpro and F24tTipDoc==oDoc.__tipdoc and F24tNumDoc==oDoc.__numdoc

		select __F24oDOCOBS
		delete for F24oCodPro==oDoc.__codpro and F24oTipDoc==oDoc.__tipdoc and F24oNumDoc==oDoc.__numdoc
	endif

	select __DOCS
	skip
enddo

return lStado

*> Grabar los datos de los pedidos recibidos.
*> Recibe los cursores imagen de los ficheros de pedidos (F24?). Ver <Comm>.__crearcursoresventas.
*> Estos cursores deberían de haber sido validados antes en ValidarPedidosRecibidos(), en este mismo módulo.

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje a devolver.

*> Historial de modificaciones:
*> 11.07.2007 (AVC) Eliminar las líneas de pedido no recibidas, pues se consideran eliminadas en el ERP.

Function GrabarPedidosRecibidos

Parameters cMensaje

Local lStado, nInx
Private cWhere, oDir

lStado = .T.
nInx = 0

*> Grabar cabeceras de pedidos.
select __F24cDOCCAB
go Top
do while !eof()
	scatter fields F24cCodPro, F24cTipDoc, F24cNumDoc memvar

	*> Validar el registro.
	if f3_seek('F24c')
		=f3_updtun("F24c", , , , "__F24cDOCCAB")
	else
		=f3_instun("F24c", "__F24cDOCCAB")
	endif

	Select __F24cDOCCAB
	Skip
enddo

*> Grabar líneas de detalle de pedidos.
select __F24lDOCLIN
go Top
do while !eof()
	scatter fields F24lCodPro, F24lTipDoc, F24lNumDoc, F24lLinDoc memvar

	*> Validar el registro.
	if f3_seek('F24l')
		=f3_updtun("F24l", , , , "__F24lDOCLIN")
	else
		=f3_instun("F24l", "__F24lDOCLIN")
	endif

	Select __F24lDOCLIN
	Skip
enddo

*> Grabar direcciones de pedidos.
*> Tomar la precaución de controlar los apóstrofos de las narices.

select __F24tDOCDIR
go Top
do while !eof()
	Scatter Name oDir
	With oDir
		.F24tNomAso = Strtran(.F24tNomAso, "'", Space(1))
		.F24t1ErDir = Strtran(.F24t1ErDir, "'", Space(1))
		.F24t2NdDir = Strtran(.F24t2NdDir, "'", Space(1))
		.F24t3RdDir = Strtran(.F24t3RdDir, "'", Space(1))
		.F24tDPobla = Strtran(.F24tDPobla, "'", Space(1))
		.F24tDProvi = Strtran(.F24tDProvi, "'", Space(1))
	EndWith

	Gather Name oDir
	scatter fields F24tCodPro, F24tTipDoc, F24tNumDoc memvar

	*> Validar el registro.
	if f3_seek('F24t')
		=f3_updtun("F24t", , , , "__F24tDOCDIR")
	else
		=f3_instun("F24t", "__F24tDOCDIR")
	endif

	Select __F24tDOCDIR
	Skip
enddo

*> Grabar observaciones de pedidos.
Select __F24oDOCOBS
Go Top
Do while !Eof()
	Scatter Fields F24oCodPro, F24oTipDoc, F24oNumDoc, F24oLinObs MemVar

	*> Validar el registro.
	if f3_seek('F24o')
		=f3_updtun("F24o", , , , "__F24oDOCOBS")
	else
		=f3_instun("F24o", "__F24oDOCOBS")
	endif

	Select __F24oDOCOBS
	Skip
EndDo

*> Dar de baja las líneas no recibidas, pues se consideran eliminadas en el ERP.
=EliminarLineasNoRecibidas(@cMensaje)

Return lStado

*> Eliminar las líneas no recibidas de los pedidos procesados.

*> Recibe:
*>	__F24cDOCCAB, cursor de trabajo, generado durante el proceso.
*>	__F24lDOCLIN, cursor de trabajo, generado durante el proceso.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje a devolver.

Function EliminarLineasNoRecibidas

Parameters cMensaje

Local lStado, nInx
Private cWhere, oDoc, oF24l

lStado = .T.
nInx = 0

Select __F24cDOCCAB
Go Top
Do While !Eof()
	Scatter Name oDoc

	*> Cargar las líneas del pedido. Devuelve el cursor __F24LW.
	lStado = CargarPedidoSalida(oDoc.F24cCodPro, oDoc.F24cTipDoc, oDoc.F24cNumDoc, @cMensaje)
	If !lStado
		Select __F24cDOCCAB
		Skip
		Loop
	EndIf

*!*		Select __F24LW
*!*		Go Top
*!*		Do While !Eof()
*!*			Scatter Name oF24l

*!*			Select __F24lDOCLIN
*!*			Locate For F24lIdLDoc==oF24l.F24lIdLDoc
*!*			If !Found()
*!*				If oF24l.F24lFlgEst > '1'
*!*					nInx = nInx + 1
*!*					Dimension cMensaje[nInx]
*!*					cMensaje[nInx] = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000041]), oF24l.F24lNumDoc, oF24l.F24lLinDoc)
*!*					Select __F24LW
*!*					Skip
*!*					Loop
*!*				EndIf

*!*				*> Dar de baja la línea.
*!*				Select __F24LW
*!*				Scatter MemVar
*!*				=f3_baja("F24l")

*!*				Select __F24LW
*!*				Delete Next 1
*!*			EndIf

*!*			Select __F24LW
*!*			Skip
*!*		EndDo

	*> Si no quedan líneas de pedido, dar de baja también la cabecera.
	Select __F24LW
	Go Top
	If Eof()
		cWhere = "F24cCodPro='" + oDoc.F24cCodPro + "' And F24cTipDoc='" + oDoc.F24cTipDoc + "' And F24cNumDoc='" + oDoc.F24cNumDoc + "'"
		=f3_deltun("F24c", , cWhere)
		cWhere = "F24tCodPro='" + oDoc.F24cCodPro + "' And F24tTipDoc='" + oDoc.F24cTipDoc + "' And F24tNumDoc='" + oDoc.F24cNumDoc + "'"
		=f3_deltun("F24t", , cWhere)
	EndIf

	Select __F24cDOCCAB
	Skip
EndDo

Use In (Select("__F24LW"))
Return lStado

*> Dar de baja una línea de documento de salida.
*> Utilizado en importación de pedidos de salida.

*> Recibe:
*>	- Registro línea pedido.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje de error.

Function BajaLineaSalida

Parameters oReg, cMensaje

Local lStado, oF24l
Private cWhere

lStado = CargarPedidoSalida(oReg.F24lCodPro, oReg.F24lTipDoc, oReg.F24lNumDoc, @cMensaje)
If !lStado
	*> No existe la línea ó error en carga línea.
	Use In (Select("__F24LW"))
	Return lStado
EndIf

Select __F24LW
Locate For F24lLinDoc== oReg.F24lLinDoc
If Found()
	scatter name oF24l
	if oF24l.F24lFlgEst > '1'
		cMensaje = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000031]), oF24l.F24lNumDoc, oF24l.F24lLinDoc)
		use in (select("__F24LW"))
		return .F.
	endif

	*> Dar de baja la línea.
	Scatter MemVar
	=f3_baja("F24l")

	*> Si no quedan líneas de pedido, dar de baja también la cabecera.
	select __F24LW
	delete next 1
	go Top
	If Eof()
		cWhere = "F24cCodPro='" + oReg.F24lCodPro + "' And F24cTipDoc='" + oReg.F24lTipDoc + "' And F24cNumDoc='" + oReg.F24lNumDoc + "'"
		=f3_deltun("F24c", , cWhere)
		cWhere = "F24tCodPro='" + oReg.F24lCodPro + "' And F24tTipDoc='" + oReg.F24lTipDoc + "' And F24tNumDoc='" + oReg.F24lNumDoc + "'"
		=f3_deltun("F24t", , cWhere)
	EndIf
EndIf

Use In (Select("__F24LW"))
Return

*> Validar el estado de una línea de documento de salida.

*> Recibe:
*>	- Registro línea pedido.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje de error.
*>	- Registro línea pedido actualizado.

Function ValidarEstadoLineaSalida

Parameters oReg, cMensaje

local lStado, oF24l
private cWhere

cMensaje = ""

lStado = CargarPedidoSalida(oReg.F24lCodPro, oReg.F24lTipDoc, oReg.F24lNumDoc, @cMensaje)
if !lStado
	*> No existe la línea.
	use in (select("__F24LW"))
	return
EndIf

select __F24LW
locate for F24lLinDoc== oReg.F24lLinDoc
If found()
	*> Actualizar el registro con los datos actuales.
	scatter name oReg
	if oReg.F24lFlgEst > '1'
		cMensaje = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000032]), oReg.F24lNumDoc, oReg.F24lLinDoc)
		use in (select("__F24LW"))
		return .F.
	endif
EndIf

use in (select("__F24LW"))
return

*> Cargar las líneas de detalle de un pedido de salida.

*> Recibe:
*>	- Propietario, tipo, nº documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje de error.
*>	- Cursor __F24LW, con las líneas del pedido.

Function CargarPedidoSalida

Parameters cCodPro, cTipDoc, cNumDoc, cEstado

local lStado
private cWhere

cEstado = ""

cWhere = "F24lCodPro='" + cCodPro + "' And F24lTipDoc='" + cTipDoc + "' And F24lNumDoc='" + cNumDoc + "'"
lStado = f3_sql("*", "F24l", cWhere, , , "__F24LW")
if !lStado
	cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000033]), cNumDoc)
endif

return lStado
