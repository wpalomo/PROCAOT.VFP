
*> Procesos generales de comunicaciones.

*> Validaci�n de familia de producto.
*> Recibe:
*>	- C�digo de familia.
*>	- crear si no existe.

*> Devuelve:
*>	- Estado (.T. / .F.)

Function ValidarFamilia

Parameters cFamilia, lCrear, cDescri

local lStado

lStado = .T.

If Empty(cFamilia)
	Return
EndIf

m.F00gCodFam = cFamilia
If !f3_seek("F00g")
	if lCrear
		=CrtCursor("F00g", "F00gCURSOR", "F")
		select F00gCURSOR
		go Top
		if eof()
			append blank
		EndIf

		Replace F00gCodFam with cFamilia
		Replace F00gDescri with cDescri
		=f3_instun("F00g", "F00gCURSOR")
		use In(select("F00gCURSOR"))
	endif
EndIf

Return lStado

*> Validaci�n de tipo de producto.
*> Recibe:
*>	- C�digo de tipo.
*>	- crear si no existe.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarTipoProducto

Parameters cTipo, lCrear, cDescri

local lStado

lStado = .T.

m.F00eTipPro = cTipo
If !f3_seek("F00e")
	if lCrear
		=CrtCursor("F00e", "F00eCURSOR", "F")
		select F00eCURSOR
		go Top
		if eof()
			append blank
		EndIf

		Replace F00eTipPro with cTipo
		Replace F00eDescri with cDescri
		=f3_instun("F00e", "F00eCURSOR")
		use In(select("F00eCURSOR"))
	endif
EndIf

Return lStado

*> Validaci�n del tama�o habitual.
*> Recibe:
*>	- C�digo de tama�o.
*>	- crear si no existe.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarTamAbi

Parameters cTamAbi, lCrear, cDescri

local lStado

lStado = .T.

m.F00mCodTam = cTamAbi
If !f3_seek("F00m")
	if lCrear
		=CrtCursor("F00m", "F00mCURSOR", "F")
		select F00mCURSOR
		go Top
		if eof()
			append blank
		EndIf

		Replace F00mCodTam with cTamAbi
		Replace F00mDescri with cDescri
		=f3_instun("F00m", "F00mCURSOR")
		use In(select("F00mCURSOR"))
	endif
EndIf

Return lStado

*> Validaci�n del tama�o de palet.
*> Recibe:
*>	- C�digo de palet.
*>	- crear si no existe.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarTipPal

Parameters cTipo, lCrear, cDescri

local lStado

lStado = .T.

m.F00fTamPal = cTipo
If !f3_seek("F00f")
	if lCrear
		=CrtCursor("F00f", "F00fCURSOR", "F")
		select F00fCURSOR
		go Top
		if eof()
			append blank
		EndIf

		Replace F00fTamPal with cTipo
		Replace F00fDescri with cDescri
		=f3_instun("F00f", "F00fCURSOR")
		use In(select("F00fCURSOR"))
	endif
EndIf

Return lStado

*> Validaci�n de la unidad de producto.
*> Recibe:
*>	- Tipo unidad.
*>	- crear si no existe.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarUnidad

Parameters cTipo, lCrear, cDescri

local lStado

lStado = .T.

If Empty(cTipo)
	Return
EndIf

m.F00hCodUni = cTipo
If !f3_seek("F00h")
	if lCrear
		=CrtCursor("F00h", "F00hCURSOR", "F")
		select F00hCURSOR
		go Top
		if eof()
			append blank
		EndIf

		Replace F00hCodUni with cTipo
		Replace F00hDescri with cDescri
		=f3_instun("F00h", "F00hCURSOR")
		use In(select("F00hCURSOR"))
	endif
EndIf

Return lStado

*> Validaci�n de la situaci�n de stock.
*> Recibe:
*>	- Situaci�n de stock.

*> Devuelve:
*>	- Estado (.T. / .F.)

Function ValidarSituacionDeStock

Parameters cTipo

local lStado

lStado = .T.

m.F00cCodStk = cTipo
lStado = f3_seek("F00c")

Return lStado

*> Validaci�n del c�digo de proveedor.
*> Recibe:
*>	- C�digo de proveedor.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarProveedor

Parameters cCodigo

local lStado

lStado = .T.

m.F01cTipEnt = 'PROV'
m.F01cCodigo = cCodigo
lStado = f3_seek("F01c")

return lStado

*> Validaci�n del c�digo de entidad.
*> Recibe:
*>	- Tipo de entidad.
*>	- C�digo de entidad.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarEntidad

Parameters cTipo, cCodigo

local lStado

lStado = .T.

m.F01cTipEnt = cTipo
m.F01cCodigo = cCodigo
lStado = f3_seek("F01c")

return lStado

*> Validaci�n del c�digo de art�culo.
*> Recibe:
*>	- C�digo de propietario.
*>	- C�digo de art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarArticulo

Parameters cCodPro, cCodigo

local lStado

lStado = .T.

m.F08cCodPro = cCodPro
m.F08cCodArt = cCodigo
lStado = f3_seek("F08c")

return lStado

*> Validaci�n del c�digo de transportista.
*> Recibe:
*>	- C�digo de transportista.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarTransportista

Parameters cCodigo

local lStado

lStado = .T.

m.F01tCodigo = cCodigo
lStado = f3_seek("F01t")

If !lStado
	Use In (Select("cF01t"))
	=CrtCursor('F01t','cF01t')
	Select cF01t
	Append Blank
	Replace F01tCodigo With cCodigo, ;
	        F01tDescri With 'CREADO AUTOMATICAMENTE'

	lStado = f3_InsTun("F01T", "cF01T", "N")
EndIf

return lStado

*> Validaci�n del tipo de documento.
*> Recibe:
*>	- Tipo de documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
Function ValidarTipoDocumento

Parameters cCodigo

Local lStado

lStado = .T.

m.F00kCodDoc = cCodigo
lStado = f3_seek("F00k")

Return lStado

*> Validaci�n de los factores de paletizaci�n de un art�culo.
*> Recibe:
*>	- Registro art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Registro art�culo actuaizado.
Function ValidarFactores

Parameters oReg

Local lStado

lStado = .T.

With oReg
	.F08cUniPac = Iif(.F08cUniPac<=0, 1, .F08cUniPac)
	.F08cPacCaj = Iif(.F08cPacCaj<=0, 1, .F08cPacCaj)
	.F08cCajPal = Iif(.F08cCajPal<=0, 1, .F08cCajPal)
	.F08cUniVen = Iif(.F08cUniVen<=0, 1, .F08cUniVen)
EndWith

Return lStado

*> Dar de baja una l�nea de documento de entrada.
*> Utilizado en importaci�n de entradas / importaci�n de devoluciones de entrada.

*> Recibe:
*>	- Registro pedido.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje de error.

Function BajaLineaEntrada

Parameters oReg, cMensaje

Local lStado, oF18l
Private cWhere

lStado = CargarPedidoEntrada(oReg.F18lCodPro, oReg.F18lTipDoc, oReg.F18lNumDoc, @cMensaje)
If !lStado
	*> No existe la l�nea � error en carga l�nea.
	Use in (Select("__F18LW"))
	Return lStado
EndIf

Select __F18LW
Locate For F18lLinDoc== oReg.F18lLinDoc
If Found()
	Scatter Name oF18l
	if oF18l.F18lEstado<>'0'
		cMensaje = oClass.__setmessage_(oClass.__getmessage_([GRP=GENE,MJC=000001]), oReg.F18lNumDoc, oReg.F18lLinDoc)
		*> cMensaje = "L�nea pedido compra en estado no v�lido"
		Use In (Select("__F18LW"))
		Return .F.
	EndIf

	*> Dar de baja la l�nea.
	Scatter MemVar
	=f3_baja("F18l")

	*> Si no quedan l�neas de pedido, dar de baja tambi�n la cabecera.
	Select __F18LW
	Delete Next 1
	go Top
	If Eof()
		cWhere = "F18cCodPro='" + oReg.F18lCodPro + "' And F18cTipDoc='" + oReg.F18lTipDoc + "' And F18cNumDoc='" + oReg.F18lNumDoc + "'"
		=f3_deltun("F18c", , cWhere)
	EndIf
EndIf

Use In (Select("__F18LW"))
Return

*> Validar el estado de una l�nea de documento de entrada.

*> Recibe:
*>	- Registro pedido.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje de error.
*>	- Registro pedido actualizado.

Function ValidarEstadoLineaEntrada

Parameters oReg, cMensaje

Local lStado, oF18l
Private cWhere

cMensaje = ""

lStado = CargarPedidoEntrada(oReg.F18lCodPro, oReg.F18lTipDoc, oReg.F18lNumDoc, @cMensaje)
*> No se valida. Puede que el pedido no exista.
*!*	If !lStado
*!*		*> No existe la l�nea.
*!*		use in (select("__F18LW"))
*!*		Return
*!*	EndIf

Select __F18LW
Locate For F18lLinDoc== oReg.F18lLinDoc
If Found()
	*> Actualizar el registro con los datos actuales.
	scatter name oReg
	if oReg.F18lEstado<>'0'
		cMensaje = oClass.__setmessage_(oClass.__getmessage_([GRP=GENE,MJC=000002]), oReg.F18lNumDoc, oReg.F18lLinDoc)
		*> cMensaje = "L�nea pedido compra en estado no v�lido"
		Use In (Select("__F18LW"))
		Return .F.
	EndIf
EndIf

Use In (Select("__F18LW"))
Return

*> Cargar las l�neas de detalle de un pedido de entrada.

*> Recibe:
*>	- Propietario, tipo, n� documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- Mensaje de error.
*>	- Cursor __F18LW, con las l�neas del pedido.

Function CargarPedidoEntrada

Parameters cCodPro, cTipDoc, cNumDoc, cEstado

local lStado
private cWhere

cEstado = ""

cWhere = "F18lCodPro='" + cCodPro + "' And F18lTipDoc='" + cTipDoc + "' And F18lNumDoc='" + cNumDoc + "'"
lStado = f3_sql("*", "F18l", cWhere, , , "__F18LW")
if !lStado
	cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=GENE,MJC=000003]), cTipDoc, cNumDoc)
	*> cEstado = "No se ha podido cargar el detalle pedido"
endif

Return lStado

*> Filtrar las l�neas de pedido por tipo producto.
*> Solo acepta aquellos productos en los que su tipo tiene parametrizado que se deben aceptar.
*> Utilizado en importaci�n de pedidos de compra y pedidos de venta.

*> Recibe:
*>	- Propietario, art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)

Function FiltroArticulo

Parameters cCodPro, cCodArt

Local lStado, oF08c

m.F08cCodPro = cCodPro
m.F08cCodArt = cCodArt
If !f3_seek('F08c')
	*> Error !!
	Return .F.
EndIf

Select F08c
Go Top
Scatter Name oF08c

m.F00eTipPro = oF08c.F08cTipPro
lStado = f3_seek("F00e")
If lStado
	Select F00e
	lStado = Empty(F00eCodEst)
EndIf

Return lStado
