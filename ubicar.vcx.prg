Define Class orafncubic as custom



PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

With This
	*> Propiedades para los procesos de ubicaci�n de material.
	.Cantidad = 0				&& Cantidad a ubicar.
	.CanUbi = 0					&& Cantidad ubicada.
	.CodPro = Space(6)			&& Propietario.
	.CodArt = Space(13)			&& Art�culo.
	.CodTal = Space(5)			&& Talla.
	.CodCol = Space(5)			&& Color.
	.UbiFor = Space(14)			&& Ubicaci�n forzada.
	.CodUbi = Space(14)			&& Ubicaci�n devuelta.
	.NumLot = Space(15)			&& Lote.
	.NumPal = Space(10)			&& N� de palet.
	.SitStk = Space(4)			&& Situaci�n de stock.
	.PicoSN = "N"				&& Pico � palet completo.
	.UbicParcial = "N"			&& Ubicar parcialmente.

	.MulPro = "N"				&& Multiproducto.
	.MulLot = "N"				&& Multilote.
	.CtrCad = "N"				&& Control caducidad.
	.CtrCal = "N"				&& Control calidad.
	.NumDia = 0					&& D�as preaviso caducidad.
	.TipAlm = ""				&& Tipo almacenaje.
	.TipPro = ""				&& Tipo producto.
	.TamAbi = ""				&& Tama�o habitual de palet.
	.NumRem = 0					&& N� remontes (para moladas).
	.TipPal = ""				&& Tipo palet producto.

	.Pickin = "S"				&& Relleno de picking.
	.Criubi = "C"				&& Criterio ubicaci�n.
	.CoeRot = "N"				&& Coeficiente de rotaci�n.
	.ForTam = "N"				&& Forzar tama�o.
	.ForZon = "N"				&& Forzar zona.
	.ForUbi = "S"				&& Forzar ubicaci�n.

	.PesOcu = 0					&& Peso movimiento.
	.VolOcu = 0					&& Volumen movimiento.
	.IdOcup = ""				&& Identificador de ocupaci�n.

	*> Propiedades para el tratamiento de albaranes de entrada.
	.TMovDst = ""				&& Tipo movimiento entrada en Hist�rico.
	.NumMovMP = ""				&& N� movimiento MP.
	.UpdAlbC = "N"				&& Actualizar estado cabecera albar�n.
	.UpdAlbD = "N"				&& Actualizar estado detalle albar�n.
	.CieAlb = "N"				&& Cerrar albar�n.

	.CDocking = "N"				&& Cross Docking.
	.TDocCD = Space(4)			&& Tipo de documento de Cross Docking
	.NDocCD = Space(14)			&& N�mero de documento de Cross Docking


	.UsrError = ""				&& Texto errores.
EndWith

Return

ENDPROC
PROCEDURE initlocals

*> Inicializar las propiedades de uso interno de la clase.

With This
	.WFacCaj = 0			&& Factor caja.
	.WFacPal = 0			&& Factor palet.
	.WRngCurP = .F.			&& Control rango activo (previo).
	.WRngCurN = .F.			&& Control rango activo (siguiente).
	.WRotIni = 0			&& Rotaci�n inicial.
	.WRotFin = 999999		&& Rotaci�n final.
	.WHayStk = .F.			&& Control SSTKs asignadas a zonas.
	.WHayRng = .F.			&& Control rangos ubicaci�n.
	.WTipPro = ""			&& Tipo de producto.
	.WNMovOC = 0			&& Palets molada (OC).
	.WNMovMP = 0			&& Palets molada (MP).

	.CodUbiMol = ""			&& Ubicaci�n molada.
EndWith

Return

ENDPROC
PROCEDURE ubicar

*> Entrada principal de ubicaci�n de material.
*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.PicoSN, ubicar un pico � un palet.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado

This.UsrError = ""


lStado = This.prc_ubi()
If lStado
	This._pesvolar				&& Peso y volumen art�culo.
	This.CargaUbi(1)
EndIf

Return lStado

ENDPROC
PROCEDURE prc_for

*> Proceso de ubicaci�n forzada de material.
*> M�todo privado de la clase. Llamado desde This.UbicarFor.

*> Recibe:
*>	- This.UbiFor, ubicaci�n forzada.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""

*> Leer datos ubicaci�n.
If Empty(This.UbiFor) .Or. Type('This.UbiFor')<>'C'
	This.UsrError = "Ubicaci�n forzada no v�lida"
	Return .F.
EndIf

m.F10cCodUbi = This.UbiFor
If !f3_seek("F10c")
	This.UsrError = "Ubicaci�n forzada: " + This.UbiFor + " no existe"
	Return .F.
EndIf

*> Comprobar validez de la ubicaci�n.
Select F10c
If This.ForUbi<>"S"
	If F10cEstGen=='B'
		This.UsrError = "Ubicaci�n forzada: " + This.UbiFor + " bloqueada"
		Return .F.
	EndIf

	If F10cTipAlm<>This.TipAlm
		This.UsrError = "Ubicaci�n forzada: " + This.UbiFor + " tipo almacenaje distinto"
		Return .F.
	EndIf

	If F10cTamUbi<>This.TamAbi
		This.UsrError = "Ubicaci�n forzada: " + This.UbiFor + " tama�o distinto"
		Return .F.
	EndIf

	If F10cTipPro<>This.TiPro .And. !Empty(This.TipPro)
		This.UsrError = "Ubicaci�n forzada: " + This.UbiFor + " tipo producto distinto"
		Return .F.
	EndIf

	If F10cNumOcu >= F10cMaxPal
		This.UsrError = "Ubicaci�n forzada: " + This.UbiFor + " n� ocupaciones excedido"
		Return .F.
	EndIf

	*> Control de peso y volumen desactivado aqu�.
EndIf

This.CodUbi = F10cCodUbi
This.CanUbi = This.Cantidad

lStado = .T.
Return lStado

ENDPROC
PROCEDURE cargaprm

*> Cargar los par�metros de ubicaci�n para el art�culo.
*>
*> Recibe:
*>	-
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado, oF08c

This.UsrError = ""

*> Inicializar propiedades de uso interno de la clase.
This.InitLocals

*> Leer datos de la ficha del producto.
m.F08cCodPro = This.CodPro
m.F08cCodArt = This.CodArt

If !f3_seek("F08c")
	This.UsrError = "No existe el producto"
	Return .F.
EndIf

*> Pasar par�metros producto a propiedades.
Select F08c
Scatter Name oF08c

With This
	.MulLot = oF08c.F08cMulLot
	.MulPro = oF08c.F08cMulPro
	.CtrCad = oF08c.F08cCaduca
	.CtrCal = oF08c.F08cCalida
	.NumDia = oF08c.F08cNumDia
	.TipAlm = oF08c.F08cTipAlm
	.TipPro = oF08c.F08cTipPro
	.TamAbi = oF08c.F08cTamAbi
	.CoeFrt = oF08c.F08cCoeRot
	.NumRem = oF08c.F08cNumRem
	.TipPal = oF08c.F08cTipPal

	*> Guardar factores paletizaci�n para uso interno.
	.WFacCaj= oF08c.F08cUniPac * oF08c.F08cPacCaj
	.WFacPal= .WFacCaj * oF08c.F08cCajPal
EndWith

*> Cargar par�metros generales de ubicaci�n (art�culo).
cWhere = "F15cCodPro='" + This.CodPro + "' And F15cCodArt='" + This.CodArt + "'"
lStado = f3_sql("*", "F15c", cWhere, , , "F15c")
If _xier <= 0
	This.UsrError = "Error par�metros generales (art�culo)"
	Return .F.
EndIf

If !lStado
	*> Cargar par�metros generales de ubicaci�n (propietario).
	cWhere = "F15cCodPro='" + This.CodPro + "' And F15cCodArt='" + Space(13) + "'"
	lStado = f3_sql("*", "F15c", cWhere, , , "F15c")
	If _xier <= 0
		This.UsrError = "Error par�metros generales (propietario)"
		Return .F.
	EndIf
EndIf

If !lStado
	This.UsrError = "No hay par�metros generales"
	Return .F.
EndIf

*> Pasar par�metros generales a propiedades.
Select F15c
With This
	.Pickin = F15cPickin
	.CriUbi = F15cCriUbi
	.CoeRot = F15cCoeRot
	.ForTam = F15cForTam
	.ForZon = F15cForZon
	.ForUbi = F15cForUbi
EndWith

lStado = .T.
Return lStado

ENDPROC
PROCEDURE cargaubi

*> Cargar ocupaciones en la ubicaci�n.

*> Recibe:
*>	- nNumPal, N� de ocupaciones a agregar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.CodUbi, ubicaci�n asignada.
*>	- This.Cantidad, cantidad ubicada.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters nNumPal

Private cWhere
Local lStado

This.UsrError = ""

*> Calcular peso y volumen movimiento.
This._pesvolar

m.F10cCodUbi = This.CodUbi
If !f3_seek("F10c")
	This.UsrError = "Error en ubicaci�n: " + This.CodUbi
	Return .F.
EndIf

Select F10c

*!*	*> Estado ubicaci�n.
*!*	If ((F10cNumOcu + nNumPal) >= F10cMaxPal) .And. (F10cTipAlm = "M" .Or. F10cTipAlm = "P")
*!*		Replace F10cEstEnt With "S", F10cEstSal With "N"
*!*	EndIf

*> Volumen libre / ocupado.
Replace F10cVolLib With F10cVolLib - This.VolOcu
If F10cVolLib <  0
	Replace F10cVolLib With 0
EndIf

*> Peso libre / ocupado.
If F10cPesOcu + This.PesOcu  >  9999999
	Replace F10cPesOcu With 9999999
Else
	Replace F10cPesOcu With F10cPesOcu + This.PesOcu
EndIf
If F10cPesOcu > F10cPesTot
	Replace F10cPesOcu With F10cPesTot
EndIf

*> N� actual de ocupaciones.
*Replace F10cNumOcu With Iif(F10cNumOcu >= F10cMaxPal, F10cMaxPal, F10cNumOcu + nNumPal)
Replace F10cNumOcu With F10cNumOcu + nNumPal
Replace F10cEstGen With IIf(F10cNumOcu >= F10cMaxPal, "O", F10cEstGen)

cWhere = "F10cCodUbi='" + This.CodUbi + "'"
lStado = f3_updtun('F10c', , , , , cWhere)

Return lStado

ENDPROC
PROCEDURE desubi

*> Descargar ocupaciones en la ubicaci�n.

*> Recibe:
*>	- nNumPal, N� de ocupaciones a agregar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.CodUbi, ubicaci�n asignada.
*>	- This.Cantidad, cantidad ubicada.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters nNumPal

Private cWhere
Local lStado

This.UsrError = ""

*> Calcular peso y volumen movimiento.
This._pesvolar

m.F10cCodUbi = This.CodUbi
If !f3_seek("F10c")
	This.UsrError = "Error en ubicaci�n: " + This.CodUbi
	Return .F.
EndIf

Select F10c

*!*	*> Estado ubicaci�n.
*!*	If ((F10cNumOcu - nNumPal) <= 0) .And. (F10cTipAlm = "M" .Or. F10cTipAlm = "P")
*!*		Replace F10cEstEnt With "S", F10cEstSal With "N"
*!*	EndIf

*> Volumen libre / ocupado.
If (F10cVolLib + This.VolOcu) > F10cVolTot
	Replace F10cVolLib With F10cVolTot
Else
	Replace F10cVolLib With F10cVolLib + This.VolOcu
EndIf

*> Peso libre / ocupado.
Replace F10cPesOcu With F10cPesOcu - This.PesOcu
If F10cPesOcu < 0
	Replace F10cPesOcu With 0
EndIf

*> N� actual de ocupaciones.
Replace F10cNumOcu With F10cNumOcu - nNumPal
Replace F10cNumOcu With Iif(F10cNumOcu < 0, 0, F10cNumOcu)
Replace F10cEstGen With Iif(F10cNumOcu <= F10cMaxPal, "L", F10cEstGen)

cWhere = "F10cCodUbi='" + This.CodUbi + "'"
lStado = f3_updtun('F10c', , , , , cWhere)

Return lStado

ENDPROC
PROCEDURE pesvolar

*> Calcular el peso y volumen para un art�culo / cantidad.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.CanUbi, cantidad ubicada.

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado, nPeso, nVolumen, nTotPal

This.UsrError = ""
lStado = This._pesvolar()

Return lStado

ENDPROC
PROCEDURE _prcpic

*> Ubicaci�n de material en picking.
*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).
*>
*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""

*> Primer paso: Ubicar en ubicaciones asignadas al producto.
lStado = This._prcpicart()

*!*	If !lStado
*!*		*>Si el parametro Relleno de Picking Caotico es 'S'
*!*		If Parametro ='S'
*!*		
*!*		EndIf
*!*	EndIf

If !lStado
	*> Ubicar en picking general y/o por rangos de ubicaci�n (F08U).
	lStado = This._prcpicrng()
EndIf

Return lStado

ENDPROC
PROCEDURE ubipicasignada

*> Comprobar si la ubicaci�n est� asignada a otro producto.
*>
*> Recibe:
*>	- cUbicacion, ubicaci�n a comprobar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere
Local lStado

This.UsrError = ""

*!*	cWhere = "F12cCodAlm='" + _Alma + "' And F12cCodUbi='" + cUbicacion + "'"
*!*	=f3_sql("*", "F12c", cWhere, , , "F12cASG")

*!*	Select F12cASG
*!*	Locate For F12cCodPro<>This.CodPro .Or. F12cCodArt<>This.CodArt
*!*	lStado = Found()

*!*	Use In (Select("F12cASG"))


*!* cWhere = "F12eCodAlm='" + _Alma + "' And F12eCodUbi='" + cUbicacion + "'"
cWhere = "F12eCodUbi='" + cUbicacion + "'"
=f3_sql("*", "F12e", cWhere, , , "F12eASG")

Select F12eASG
Locate For F12eCodPro<>This.CodPro .Or. F12eCodArt<>This.CodArt
lStado = Found()

Use In (Select("F12eASG"))


Return lStado

ENDPROC
PROCEDURE ubiocupada

*> Comprobar si la ubicaci�n est� ocupada por otro producto.
*>
*> Recibe:
*>	- cUbicacion, ubicaci�n a comprobar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere
Local lStado


This.UsrError = ""
lStado = .F.

cWhere = "F16cCodUbi='" + cUbicacion + "'"
=f3_sql("*", "F16c", cWhere, , , 'F16cASG')

Select F16cASG
Scan For F16cCodPro<>This.CodPro .Or. F16cCodArt<>This.CodArt
	Do Case
		*> La ubicaci�n NO es multiproducto.
		Case This.MulPro=='N'
			lStado = .T.
			Exit

		*> La ubicaci�n SI es multiproducto.
		Case This.MulPro=='S'
			m.F08cCodPro = F16cCodPro
			m.F08cCodArt = F16cCodArt
			If  f3_seek('F08C')
				*> El producto que hay NO es multiproducto.
				If F08c.F08cMulPro=='N'
					lStado = .T.
					Exit
				EndIf
			EndIf
	EndCase
EndScan

Use In (Select("F16cASG"))
Return lStado

ENDPROC
PROCEDURE ubiocupadamp

*> Comprobar si la ubicaci�n est� asignada a otro MP.

*> Recibe:
*>	- cUbicacion, ubicaci�n a comprobar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere
Local lStado


This.UsrError = ""
lStado = .F.

cWhere = "(F14cUbiOri='" + cUbicacion + "' And " + _GCSS("F14cTipMov", 1, 1) + "='1') Or " + ;
		 "(F14cUbiDes='" + cUbicacion + "' And " +  + _GCSS("F14cTipMov", 1, 2) + "='35')"

lStado=f3_sql("*", "F14c", cWhere, , , 'F14cASG')

Select F14cASG
Scan For F14cCodPro<>This.CodPro .Or. F14cCodArt<>This.CodArt
	Do Case
		*> La ubicaci�n NO es multiproducto.
		Case This.MulPro=='N'
			lStado = .T.
			Exit

		*> La ubicaci�n SI es multiproducto.
		Case This.MulPro=='S'
			m.F08cCodPro = F14cCodPro
			m.F08cCodArt = F14cCodArt
			If  f3_seek('F08C')
				*> El producto que hay NO es multiproducto.
				If F08c.F08cMulPro=='N'
					lStado = .T.
					Exit
				EndIf
			EndIf
	EndCase
EndScan

Use In (Select("F14cASG"))
Return lStado

ENDPROC
PROCEDURE _prcpicart

*> Buscar ubicaci�n de picking asignada al producto.
*> M�todo privado de la clase. Llamado desde This._prcpic.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Local lStado, nCantidadPck, nCantidad, oF12c

This.UsrError = ""

nCantidadPck = 0				&& Cantidad ya acumulada en picking (MPs + OCs).

cWhere = "F12cCodPro='" + This.CodPro + "' And " + ;
         "F12cCodArt='" + This.CodArt + "' And " + ;
         "F12cCodAlm='" + _Alma + "' And " + ;
         "F10cCodUbi=F12cCodUbi"

cFromF = 'F12c,F10c'
cOrder = "F12cCodPro, F12cCodArt, F12cPriori"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F12cPPic')

Select F12cPPic
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF12c

	nCantidadPck = This.CanPickingMP(oF12c.F12cCodUbi) + This.CanPickingOC(oF12c.F12cCodUbi)
	If nCantidadPck < oF12c.F12cCanMax
		*> Cantidad m�xima a ubicar.
		nCantidad = This.RoundCantidad(This.Cantidad, (oF12c.F12cCanMax - nCantidadPck), oF12c.F10cPickSN)

		If nCantidad <= 0 .Or. (nCantidad < This.Cantidad .And. This.UbicParcial<>"S")
			*> Descartar ubicaci�n porque no se permite ubicar parcialmente.
			Select F12cPPic
			Skip
			Loop
		EndIf

		*> Ubicaci�n OK.
		With This
			.CodUbi = oF12c.F12cCodUbi
			.CanUbi = nCantidad
		EndWith
		lStado = .T.
		Exit
	EndIf

	Select F12cPPic
	Skip
EndDo

Use In (Select ("F12cPPic"))
Return lStado

ENDPROC
PROCEDURE canpickingmp

*> Calcular la cantidad en MPs de entrada para un producto / ubicaci�n.

*> Recibe:
*>	- cUbicacion, ubicaci�n a calcular.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.

*> Devuelve:
*>	- nCantidad, cantidad total.
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cField
Local lStado, nCantidad

This.UsrError = ""
nCantidad = 0

cField = _GCN("Sum(F14cCanFis)") + " As TotalMP"

cWhere = "F14cCodPro='" + This.CodPro + "' And " + ;
         "F14cCodArt='" + This.CodArt + "' And " + ;
         "((F14cUbiOri='" + cUbicacion + "' And " + _GCSS("F14cTipMov", 1, 1) + "='1') Or " + ;
		 "(F14cUbiDes='" + cUbicacion + "' And " +  + _GCSS("F14cTipMov", 1, 2) + "='35'))"

cWhere = cWhere + Iif(Type('This.CodCol')=='C', " And " + _GCSS("F14cNumLot", 1, 5) + "='" + This.CodCol + "'", "")
cWhere = cWhere + Iif(Type('This.CodTal')=='C', " And " + _GCSS("F14cNumLot", 6, 5) + "='" + This.CodTal + "'", "")

lStado = f3_sql(cField, "F14c", cWhere, , , "F14cPPic")
If lStado
	Select F14cPPic
	Go Top
	nCantidad = Iif(IsNull(TotalMP), 0, TotalMP)
EndIf

Use In (Select("F14cPPic"))
Return nCantidad

ENDPROC
PROCEDURE canpickingoc

*> Calcular la cantidad en ocupaciones para un producto / ubicaci�n.

*> Recibe:
*>	- cUbicacion, ubicaci�n a calcular.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.

*> Devuelve:
*>	- nCantidad, cantidad total.
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cField
Local lStado, nCantidad

This.UsrError = ""
nCantidad = 0

cField = _GCN("Sum(F16cCanFis)") + " As TotalMP"

cWhere = "F16cCodUbi='" + cUbicacion + _cm + ;
         " And   F16cCodPro='" + This.CodPro + _cm + ;
         " And   F16cCodArt='" + This.CodArt + _cm + ;
         " And " + _GCSS("F16cSitStk", 1, 1) + "='1'"

cWhere = cWhere + Iif(Type('This.CodCol')=='C', " And " + _GCSS("F16cNumLot", 1, 5) + "='" + This.CodCol + "'", "")
cWhere = cWhere + Iif(Type('This.CodTal')=='C', " And " + _GCSS("F16cNumLot", 6, 5) + "='" + This.CodTal + "'", "")

lStado = f3_sql(cField, "F16c", cWhere, , , "F16cPPic")
If lStado
	Select F16cPPic
	Go Top
	nCantidad = Iif(IsNull(TotalMP), 0, TotalMP)
EndIf

Use In (Select("F16cPPic"))
Return nCantidad

ENDPROC
PROCEDURE roundcantidad

*> Redondear cantidad a un factor dado (caja, grupo, palet).
*> Recibe:
*>	- nCantidad, cantidad a redondear.
*>	- nCantidadMaxima, cantidad m�xima despu�s del redondeo.
*>	- cEsPico, tipo de redondeo.
*>	- This.WFacCaj, Factor caja del producto.
*>	- This.WFacPal, Factor palet del producto.
*>
*> Devuelve:
*>	- Cantidad redondeada.
*>	- This.UsrError, texto errores.

Parameters nCantidad, nCantidadMaxima, cEsPico

Private cWhere
Local nCantidadRedondeada, nBultos, nResto

This.UsrError = ""

Do Case
	*> Cajas / grupos.
	Case cEsPico=="S" .Or. cEsPico=="G"
		nBultos = Int(nCantidad / This.WFacCaj)
		nCantidadRedondeada = nBultos * This.WFacCaj
		nResto = nCantidad - nCantidadRedondeada

		If nResto > 0 .And. (nCantidadRedondeada + This.WFacCaj) <= nCantidadMaxima
			nCantidadRedondeada = nCantidadRedondeada + This.WFacCaj
		EndIf

	*> Palet completo.
	Case cEsPico=="P"
		nBultos = Int(nCantidad / This.WFacPal)
		nCantidadRedondeada = nBultos * This.WFacPal
		nResto = nCantidad - nCantidadRedondeada

		If nResto > 0 .And. (nCantidadRedondeada + This.WFacPal) <= nCantidadMaxima
			nCantidadRedondeada = nCantidadRedondeada + This.WFacPal
		EndIf

	*> Resto de casos: no se var�a.
	Otherwise
	nCantidadRedondeada = nCantidad
EndCase

Return Iif(nCantidadRedondeada<=nCantidad, nCantidadRedondeada, nCantidad)

ENDPROC
PROCEDURE _prcpicgen

*> Buscar ubicaci�n de picking en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcpicrng.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Local lStado, nCantidadPck, nCantidad, oF10c

This.UsrError = ""

nCantidadPck = 0				&& Cantidad ya acumulada en picking (MPs + OCs).

m.F10cCodUbi = _Alma + Space(10)
=f3_pos("F10c")

Select F10c
Do While !Eof() .And. SubStr(F10cCodUbi, 1, 4)==_Alma
	lStado = .F.
	Scatter Name oF10c

	If (F10cPickSN=='S' .Or. F10cPickSN=='U' .Or.F10cPickSN=='G') .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
		If !This.UbiPicAsignada(oF10c.F10cCodUbi) .And. ;
		   !This.UbiOcupada(oF10c.F10cCodUbi) .And. ;
		   !This.UbiOcupadaMP(oF10c.F10cCodUbi)

			nCantidadPck = This.CanPickingMP(oF10c.F10cCodUbi) + This.CanPickingOC(oF10c.F10cCodUbi)
			If nCantidadPck < This.WFacPal
				*> Cantidad m�xima a ubicar.
				nCantidad = This.RoundCantidad(This.Cantidad, (This.WFacPal - nCantidadPck), oF10c.F10cPickSN)

				If nCantidad==This.Cantidad .Or. (nCantidad > 0 .And. This.UbicParcial=="S")
					*> Ubicaci�n OK.
					With This
						.CodUbi = oF10c.F10cCodUbi
						.CanUbi = nCantidad
					EndWith
					lStado = .T.
					Exit
				EndIf
			EndIf
		EndIf
	EndIf

	*> Carga la siguiente ubicaci�n del almac�n.
	Select F10c
	Scatter MemVar
	=f3_pos("F10c")
EndDo

Return lStado

ENDPROC
PROCEDURE _prcpicrng

*> Buscar ubicaci�n de picking en zona general asignada al producto.
*> Si no hay rangos (F08U), se ubica en picking general.
*> M�todo privado de la clase. Llamado desde This._prcpic.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Local lStado, nCantidadPck, nCantidad, oF08u, oF10c

This.UsrError = ""

nCantidadPck = 0				&& Cantidad ya acumulada en picking (MPs + OCs).

cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uPPic')
If !lStado
	*> No hay rangos: Ubicar en picking general (Anulado).
	Use In (Select ("F08uPPic"))
*	lStado = This._prcpicgen()
	Return lStado
EndIf

Select F08uPPic
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF08u

	*> Cargar ubicaciones de picking de este rango.
	m.F10cCodUbi = oF08u.F08uUbiDsd
	If !F3_seek("F10c")
		=f3_pos("F10c")
	EndIf

	Select F10c
	Do While !Eof() .And. F10cCodUbi <= oF08u.F08uUbiHst
		Scatter Name oF10c

		If (F10cPickSN=='S' .Or. F10cPickSN=='U' .Or.F10cPickSN=='G') .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
			If !This.UbiPicAsignada(oF10c.F10cCodUbi) .And. ;
			   !This.UbiOcupada(oF10c.F10cCodUbi) .And. ;
			   !This.UbiOcupadaMP(oF10c.F10cCodUbi)

				nCantidadPck = This.CanPickingMP(oF10c.F10cCodUbi) + This.CanPickingOC(oF10c.F10cCodUbi)
				If nCantidadPck < This.WFacPal
					*> Cantidad m�xima a ubicar.
					*> nCantidad = This.RoundCantidad(This.Cantidad, (This.WFacPal - nCantidadPck), oF10c.F10cPickSN)
					nCantidad = This.Cantidad
					If nCantidad==This.Cantidad .Or. (nCantidad > 0 .And. This.UbicParcial=="S")
						*> Ubicaci�n OK.
						With This
							.CodUbi = oF10c.F10cCodUbi
							.CanUbi = nCantidad
						EndWith
						lStado = .T.
						Exit
					EndIf
				EndIf
			EndIf
		EndIf

		*> Carga la siguiente ubicaci�n del rango activo.
		Select F10c
		Scatter MemVar
		=f3_pos("F10c")
	EndDo

	If lStado
		Exit
	EndIf

	*> Procesar el siguiente rango de ubicaciones.
	Select F08uPPic
	Skip
EndDo

Use In (Select ("F08uPPic"))
Return lStado

ENDPROC
PROCEDURE _prcpxpi

*> Buscar ubicaci�n por proximidad a picking.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""

*> Ubicar por proximidad a ocupaciones / MPs de picking que ya contengan el producto.
lStado = This._prcpxpiocu()

If !lStado
	*> Ubicar por proximidad a ubicaciones de picking asignadas al producto.
	lStado = This._prcpxpiart()
EndIf

If !lStado
	*> Ubicar por proximidad a ubicaciones de picking general y/o por rangos de ubicaci�n (F08U).
	lStado = This._prcpxpirng()
EndIf

*> Cursores para navegaci�n por rangos.
*> Creados y usados en This.BuscarUbicNext() y This.BuscarUbicPrev()
Use In (Select("F10cPRV"))
Use In (Select("F10cNXT"))

Return lStado

ENDPROC
PROCEDURE _prcpxpiart

*> Buscar ubicaci�n por proximidad a ubicaciones de picking asignadas al producto.
*> M�todo privado de la clase. Llamado desde This._prcpxpi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Private cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown
Local lStado, oF12c

This.UsrError = ""
Store "" To cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown

cWhere = "F12cCodPro='" + This.CodPro + "' And " + ;
         "F12cCodArt='" + This.CodArt + "' And " + ;
         "F12cCodAlm='" + _Alma + "'"

cFromF = 'F12c'
cOrder = "F12cCodPro, F12cCodArt, F12cPriori"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F12cPPic')

Select F12cPPic
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF12c

	*> Para control cursores de navegaci�n.
	With This
		.WRngCurP = .F.
		.WRngCurN = .F.
	EndWith

	*> Ubicaci�n de picking activa para realizar comparaciones.
	cCurUbiPic = oF12c.F12cCodUbi

	cCurUbiNext = cCurUbiPic
	cCurUbiPrev = cCurUbiPic
	cCurUbiUp = cCurUbiPic
	cCurUbiDown = cCurUbiPic

	Do While .T.
		If !Empty(cCurUbiNext)
			cCurUbiNext = This.BuscarUbicNext(cCurUbiNext)		&& Buscar la ubicaci�n siguiente a la actual de picking.
			lStado = This._prcpxpicnf(cCurUbiNext)				&& Validar la ubicaci�n obtenida.
			cCurUbiPic = cCurUbiNext
		EndIf

		If !Empty(cCurUbiPrev)
			If !lStado
				cCurUbiPrev = This.BuscarUbicPrev(cCurUbiPrev)	&& Buscar la ubicaci�n anterior a la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiPrev)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiPrev
			EndIf
		EndIf

		If !Empty(cCurUbiUp)
			If !lStado
				cCurUbiUp = This.BuscarUbicUp(cCurUbiUp)		&& Buscar la ubicaci�n encima de la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiUp)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiUp
			EndIf
		EndIf

		If !Empty(cCurUbiDown)
			If !lStado
				cCurUbiDown = This.BuscarUbicDown(cCurUbiDown)	&& Buscar la ubicaci�n debajo de la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiDown)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiDown
			EndIf
		EndIf

		If lStado .Or. (Empty(cCurUbiNext) .And. Empty(cCurUbiPrev) .And. Empty(cCurUbiUp) .And. Empty(cCurUbiDown))
			Exit
		EndIf
	EndDo

	If lStado
		*> Ubicaci�n OK.
		With This
			.CodUbi = cCurUbiPic
			.CanUbi = This.Cantidad
		EndWith
		lStado = .T.
		Exit
	EndIf

	Select F12cPPic
	Skip
EndDo

Use In (Select ("F12cPPic"))
Return lStado

ENDPROC
PROCEDURE _prcpxpirng

*> Buscar ubicaci�n por proximidad a picking en zona general asignada al producto.
*> Si no hay rangos (F08U), se ubica en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcpxpi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Private cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown
Local lStado, oF08u, oF10c

This.UsrError = ""
Store "" To cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown

cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uPPic')
If !lStado
	*> No hay rangos: Ubicar por proximidad a picking general.
	Use In (Select ("F08uPPic"))
	lStado = This._prcpxpigen()
	Return lStado
EndIf

Select F08uPPic
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF08u

	*> Para control cursores de navegaci�n.
	With This
		.WRngCurP = .F.
		.WRngCurN = .F.
	EndWith

	*> Cargar ubicaciones de picking de este rango.
	m.F10cCodUbi = oF08u.F08uUbiDsd
	If !F3_seek("F10c")
		=f3_pos("F10c")
	EndIf

	Select F10c
	Do While !Eof() .And. F10cCodUbi <= oF08u.F08uUbiHst
		Scatter Name oF10c

		If (F10cPickSN=='S' .Or. F10cPickSN=='U' .Or.F10cPickSN=='G') .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
			*> Ubicaci�n de picking activa para realizar comparaciones.
			cCurUbiPic = oF10c.F10cCodUbi

			cCurUbiNext = cCurUbiPic
			cCurUbiPrev = cCurUbiPic
			cCurUbiUp = cCurUbiPic
			cCurUbiDown = cCurUbiPic

			Do While .T.
				If !Empty(cCurUbiNext)
					*> Buscar la ubicaci�n siguiente a la actual de picking.
					cCurUbiNext = This.BuscarUbicNext(cCurUbiNext, oF08u.F08uUbiDsd, oF08u.F08uUbiHst)
					lStado = This._prcpxpicnf(cCurUbiNext)				&& Validar la ubicaci�n obtenida.
				EndIf

				If !Empty(cCurUbiNext)
					If !lStado
						*> Buscar la ubicaci�n anterior a la actual de picking.
						cCurUbiPrev = This.BuscarUbicPrev(cCurUbiPrev, oF08u.F08uUbiDsd, oF08u.F08uUbiHst)
						lStado = This._prcpxpicnf(cCurUbiPrev)			&& Validar la ubicaci�n obtenida.
					EndIf
				EndIf

				If !Empty(cCurUbiNext)
					If !lStado
						*> Buscar la ubicaci�n encima de la actual de picking.
						cCurUbiUp = This.BuscarUbicUp(cCurUbiUp, oF08u.F08uUbiHst)
						lStado = This._prcpxpicnf(cCurUbiUp)			&& Validar la ubicaci�n obtenida.
					EndIf
				EndIf

				If !Empty(cCurUbiNext)
					If !lStado
						*> Buscar la ubicaci�n debajo de la actual de picking.
						cCurUbiDown = This.BuscarUbicDown(cCurUbiDown, oF08u.F08uUbiDsd)
						lStado = This._prcpxpicnf(cCurUbiDown)			&& Validar la ubicaci�n obtenida.
					EndIf
				EndIf

				If lStado .Or. (Empty(cCurUbiNext) .And. Empty(cCurUbiPrev) .And. Empty(cCurUbiUp) .And. Empty(cCurUbiDown))
					Exit
				EndIf
			EndDo

			If lStado
				*> Ubicaci�n OK.
				With This
					.CodUbi = oF10c.F10cCodUbi
					.CanUbi = This.Cantidad
				EndWith
				lStado = .T.
				Exit
			EndIf
		EndIf

		*> Carga la siguiente ubicaci�n del rango activo.
		m.F10cCodUbi = cCurUbiPic
		=f3_pos("F10c")
	EndDo

	If lStado
		Exit
	EndIf

	*> Procesar el siguiente rango de ubicaciones.
	Select F08uPPic
	Skip
EndDo

Use In (Select ("F08uPPic"))
Return lStado

ENDPROC
PROCEDURE buscarubicnext

*> Obtener la ubicaci�n siguiente a una dada.
*> M�todo privado de la clase. Llamado desde This._prcpxpixxx.

*> Recibe:
*>	- Ubicaci�n actual.
*>	- L�mite inferior (opcional), para this._prxpxpirng().
*>	- L�mite superior (opcional), para this._prxpxpirng().

*> Devuelve:
*>	- Nueva ubicaci�n ("": No ha encontrado).

Parameters cUbicacionActual, cLimiteInferior, cLimiteSuperior

Private cWhere, cOrder
Local cUbicacionNueva, cAltura, cFila

cUbicacionNueva = ""
cAltura = SubStr(cUbicacionActual, 12, 2)
cFila = SubStr(cUbicacionActual, 9, 3)

*> Valores por defecto de los l�mites de b�squeda.
If Type('cLimiteInferior')<>"C"
	cLimiteInferior = _Alma + Space(10)
EndIf
If Type('cLimiteSuperior')<>"C"
	cLimiteSuperior = _Alma + Replicate("z", 10)
EndIf

*> Regenerar el cursor de trabajo.
If !This.WRngCurN
	Use In (Select("F10cNXT"))

	cWhere  = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
	cWhere = cWhere + "F10cCodUbi Between '" + cLimiteInferior + "' And '" + cLimiteSuperior + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 12, 2) + "='" + cAltura + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 9, 3) + ">'" + cFila + "'"

	cOrder = _GCSS("F10cCodUbi", 1, 8) + ", " + _GCSS("F10cCodUbi", 12, 3) + ", " + _GCSS("F10cCodUbi", 9, 3)

	=f3_sql("*", "F10c", cWhere, cOrder, , "F10cNXT")
	Select F10cNXT
	Go Top

	This.WRngCurN = .T.
EndIf

Select F10cNXT
If !Eof()
	cUbicacionNueva = F10cCodUbi
	Skip
EndIf

Return cUbicacionNueva

ENDPROC
PROCEDURE buscarubicprev

*> Obtener la ubicaci�n anterior a una dada.
*> M�todo privado de la clase. Llamado desde This._prcpxpixxx.

*> Recibe:
*>	- Ubicaci�n actual.
*>	- L�mite inferior (opcional), para this._prxpxpirng().
*>	- L�mite superior (opcional), para this._prxpxpirng().

*> Devuelve:
*>	- Nueva ubicaci�n ("": No ha encontrado).

Parameters cUbicacionActual, cLimiteInferior, cLimiteSuperior

Private cWhere, cOrder
Local cUbicacionNueva, cAltura, cFila

cUbicacionNueva = ""
cAltura = SubStr(cUbicacionActual, 12, 2)
cFila = SubStr(cUbicacionActual, 9, 3)

*> Valores por defecto de los l�mites de b�squeda.
If Type('cLimiteInferior')<>"C"
	cLimiteInferior = _Alma + Space(10)
EndIf
If Type('cLimiteSuperior')<>"C"
	cLimiteSuperior = _Alma + Replicate("z", 10)
EndIf

*> Regenerar el cursor de trabajo.
If !This.WRngCurP
	Use In (Select("F10cPRV"))

	cWhere  = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
	cWhere = cWhere + "F10cCodUbi Between '" + cLimiteInferior + "' And '" + cLimiteSuperior + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 12, 2) + "='" + cAltura + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 9, 3) + "<'" + cFila + "'"

	cOrder = _GCSS("F10cCodUbi", 1, 8) + ", " + _GCSS("F10cCodUbi", 12, 3) + ", " + _GCSS("F10cCodUbi", 9, 3)

	=f3_sql("*", "F10c", cWhere, cOrder, , "F10cPRV")
	Select F10cPRV
	Go Bottom

	This.WRngCurP = .T.
EndIf

Select F10cPRV
If !Bof()
	cUbicacionNueva = F10cCodUbi
	Skip - 1
EndIf

Return cUbicacionNueva

ENDPROC
PROCEDURE buscarubicup

*> Obtener la ubicaci�n superior a una dada.
*> M�todo privado de la clase. Llamado desde This._prcpxpixxx.

*> Recibe:
*>	- Ubicaci�n actual.
*>	- L�mite superior (opcional), para this._prxpxpirng().

*> Devuelve:
*>	- Nueva ubicaci�n ("": No ha encontrado).

Parameters cUbicacionActual, cLimiteSuperior

Local cUbicacionNueva

cUbicacionNueva = ""

*> Valor por defecto del l�mite de b�squeda.
If Type('cLimiteSuperior')<>"C"
	cLimiteSuperior = _Alma + Replicate("z", 10)
EndIf

m.F10cCodUbi = cUbicacionActual
=f3_pos("F10c")

Select F10c
If !Eof()
	If F10cCodUbi <= cLimiteSuperior .And. SubStr(F10cCodUbi, 1, 12)==SubStr(cUbicacionActual, 1, 12)
		cUbicacionNueva = F10cCodUbi
	EndIf
EndIf

Return cUbicacionNueva

ENDPROC
PROCEDURE buscarubicdown

*> Obtener la ubicaci�n inferior a una dada.
*> M�todo privado de la clase. Llamado desde This._prcpxpixxx.

*> Recibe:
*>	- Ubicaci�n actual.
*>	- L�mite inferior (opcional), para this._prxpxpirng().

*> Devuelve:
*>	- Nueva ubicaci�n ("": No ha encontrado).

Parameters cUbicacionActual, cLimiteInferior

Local cUbicacionNueva

cUbicacionNueva = ""

*> Valor por defecto del l�mite de b�squeda.
If Type('cLimiteInferior')<>"C"
	cLimiteInferior = _Alma + Space(10)
EndIf

m.F10cCodUbi = cUbicacionActual
=f3_ant("F10c")

Select F10c
If !Eof()
	If F10cCodUbi >= cLimiteInferior .And. SubStr(F10cCodUbi, 1, 12)==SubStr(cUbicacionActual, 1, 12)
		cUbicacionNueva = F10cCodUbi
	EndIf
EndIf

Return cUbicacionNueva

ENDPROC
PROCEDURE _prcpxpicnf

*> Validar la ubicaci�n encontrada en ubicaci�n por proximidad a picking.
*> M�todo privado de la clase. Llamado desde los m�todos que se encargan de buscar ubicaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbicacion, ubicaci�n candidata a validar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cCodUbi
Local lStado

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "La ubicaci�n no existe"
	Return .F.
EndIf

Select F10c
cCodUbi = F10cCodUbi

*> Validar tipo almacenaje.
If F10cTipAlm<>This.TipAlm
	This.UsrError = "Tipo almacenaje distinto"
	Return .F.
EndIf

*> Validar tipo ubicaci�n.
If F10cPickSN<>"N"
	This.UsrError = "La ubicaci�n no es de carga"
	Return .F.
EndIf

*> Validar estado bloqueo.
If F10cEstGen=="B" .Or. F10cEstEnt=="S"
	This.UsrError = "La ubicaci�n est� bloqueada"
	Return .F.
EndIf

*> Validar ubicaci�n ocupada.
If F10cNumOcu >= F10cMaxPal
	This.UsrError = "La ubicaci�n est� ocupada"
	Return .F.
EndIf

*> Validar tipo de producto.
If !Empty(F10cTipPro) .And. F10cTipPro<>This.TipPro
	*> Ver si pertenece a alguno de los tipos alternativos.
	lStado = This.VerTipPro(F10cTipPro)
	If !lStado
		This.UsrError = "Tipo de producto diferente"
		Return lStado
	EndIf
EndIf

If This.UbiOcupada(cCodUbi) .Or. This.UbiOcupadaMP(cCodUbi)
	This.UsrError = "Ubicaci�n ocupada por otro producto"
	Return .F.
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _prcpxpigen

*> Buscar ubicaci�n por proximidad a picking en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcpxpi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Private cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown
Local lStado, oF10c

This.UsrError = ""

m.F10cCodUbi = _Alma + Space(10)
=f3_pos("F10c")

Select F10c
Do While !Eof() .And. SubStr(F10cCodUbi, 1, 4)==_Alma
	lStado = .F.
	Scatter Name oF10c

	*> Para control cursores de navegaci�n.
	With This
		.WRngCurP = .F.
		.WRngCurN = .F.
	EndWith

	If (F10cPickSN=='S' .Or. F10cPickSN=='U' .Or.F10cPickSN=='G') .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
		*> Ubicaci�n de picking activa para realizar comparaciones.
		cCurUbiPic = oF10c.F10cCodUbi

		cCurUbiNext = cCurUbiPic
		cCurUbiPrev = cCurUbiPic
		cCurUbiUp = cCurUbiPic
		cCurUbiDown = cCurUbiPic

		Do While .T.
			If !Empty(cCurUbiNext)
				cCurUbiNext = This.BuscarUbicNext(cCurUbiNext)		&& Buscar la ubicaci�n siguiente a la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiNext)				&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiNext
			EndIf

			If !Empty(cCurUbiPrev)
				If !lStado
					cCurUbiPrev = This.BuscarUbicPrev(cCurUbiPrev)	&& Buscar la ubicaci�n anterior a la actual de picking.
					lStado = This._prcpxpicnf(cCurUbiPrev)			&& Validar la ubicaci�n obtenida.
					cCurUbiPic = cCurUbiPrev
				EndIf
			EndIf

			If !Empty(cCurUbiUp)
				If !lStado
					cCurUbiUp = This.BuscarUbicUp(cCurUbiUp)		&& Buscar la ubicaci�n encima de la actual de picking.
					lStado = This._prcpxpicnf(cCurUbiUp)			&& Validar la ubicaci�n obtenida.
					cCurUbiPic = cCurUbiUp
				EndIf
			EndIf

			If !Empty(cCurUbiDown)
				If !lStado
					cCurUbiDown = This.BuscarUbicDown(cCurUbiDown)	&& Buscar la ubicaci�n debajo de la actual de picking.
					lStado = This._prcpxpicnf(cCurUbiDown)			&& Validar la ubicaci�n obtenida.
					cCurUbiPic = cCurUbiDown
				EndIf
			EndIf

			If lStado .Or. (Empty(cCurUbiNext) .And. Empty(cCurUbiPrev) .And. Empty(cCurUbiUp) .And. Empty(cCurUbiDown))
				Exit
			EndIf
		EndDo

		If lStado
			*> Ubicaci�n OK.
			With This
				.CodUbi = cCurUbiPic
				.CanUbi = This.Cantidad
			EndWith
			lStado = .T.
			Exit
		EndIf
	EndIf

	*> Carga la siguiente ubicaci�n del almac�n.
	m.F10cCodUbi = oF10c.F10cCodUbi
	=f3_pos("F10c")
EndDo

Return lStado

ENDPROC
PROCEDURE _prcrot

*> Buscar ubicaci�n por coeficiente de rotaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cOrder
Local lStado

This.UsrError = ""

*> Inicializar el rango de rotaci�n del art�culo.
cWhere = "F11cCoeFrt>=" + Str(This.CoeFrt)
cOrder = "F11cCoeFrt"

lStado = f3_sql("*", "F11c", cWhere, cOrder, , "F11cCur")
If lStado
	Select F11cCur
	Go Top
	This.WRotIni = F11cPriIni
	This.WRotFin = F11cPriFin
EndIf

*> Ubicar por coeficiente de rotaci�n a ubicaciones de carga y/o por rangos de ubicaci�n (F08U).
lStado = This._prcrotrng()

Use In (Select("F11cCur"))
Return lStado

ENDPROC
PROCEDURE _prcpxpiocu

*> Buscar ubicaci�n por proximidad a ubicaciones de picking que ya tengan el producto.
*> M�todo privado de la clase. Llamado desde This._prcpxpi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Private cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown
Local lStado, oF16c

This.UsrError = ""
Store "" To cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown

cWhere = "F16cCodPro='" + This.CodPro + "' And " + ;
         "F16cCodArt='" + This.CodArt + "' And " + ;
         _GCSS("F16cSitStk", 1, 1) + "='1' And " + ;
         "F10cCodUbi=F16cCodUbi"

cFromF = 'F16c,F10c'
cOrder = "F16cCodPro, F16cCodArt, F16cCodUbi"

=f3_sql('*', cFromF, cWhere, cOrder, , 'F16cPPic')

lStado = .F.

Select F16cPPic
Scan For F10cPickSN=='S' .Or. F10cPickSN=='G' .Or. F10cPickSN=='U'
 	lStado = .F.
	Scatter Name oF16c

	*> Para control cursores de navegaci�n.
	With This
		.WRngCurP = .F.
		.WRngCurN = .F.
	EndWith

	*> Ubicaci�n de picking activa para realizar comparaciones.
	cCurUbiPic = oF16c.F16cCodUbi

	cCurUbiNext = cCurUbiPic
	cCurUbiPrev = cCurUbiPic
	cCurUbiUp = cCurUbiPic
	cCurUbiDown = cCurUbiPic

	Do While .T.
		If !Empty(cCurUbiNext)
			cCurUbiNext = This.BuscarUbicNext(cCurUbiNext)		&& Buscar la ubicaci�n siguiente a la actual de picking.
			lStado = This._prcpxpicnf(cCurUbiNext)				&& Validar la ubicaci�n obtenida.
			cCurUbiPic = cCurUbiNext
		EndIf

		If !Empty(cCurUbiPrev)
			If !lStado
				cCurUbiPrev = This.BuscarUbicPrev(cCurUbiPrev)	&& Buscar la ubicaci�n anterior a la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiPrev)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiPrev
			EndIf
		EndIf

		If !Empty(cCurUbiUp)
			If !lStado
				cCurUbiUp = This.BuscarUbicUp(cCurUbiUp)		&& Buscar la ubicaci�n encima de la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiUp)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiUp
			EndIf
		EndIf

		If !Empty(cCurUbiDown)
			If !lStado
				cCurUbiDown = This.BuscarUbicDown(cCurUbiDown)	&& Buscar la ubicaci�n debajo de la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiDown)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiDown
			EndIf
		EndIf

		If lStado .Or. (Empty(cCurUbiNext) .And. Empty(cCurUbiPrev) .And. Empty(cCurUbiUp) .And. Empty(cCurUbiDown))
			Exit
		EndIf
	EndDo

	If lStado
		*> Ubicaci�n OK.
		With This
			.CodUbi = cCurUbiPic
			.CanUbi = This.Cantidad
		EndWith
		lStado = .T.
		Exit
	EndIf

EndScan

Use In (Select ("F16cPPic"))

If !lStado
	*> Ubicar por proximidad a MPs de entrada a picking.
	lStado = This._prcpxpimp()
EndIf

Return lStado

ENDPROC
PROCEDURE _prcpxpimp

*> Buscar ubicaci�n por proximidad a MPs de entrada a picking que ya tengan el producto.
*> M�todo privado de la clase. Llamado desde This._prcpxpiocu.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Private cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown
Local lStado, oF14c, oF10c

This.UsrError = ""
Store "" To cCurUbiPic, cCurUbiPrev, cCurUbiNext, cCurUbiUp, cCurUbiDown

cWhere = "F14cCodPro='" + This.CodPro + "' And " + ;
         "F14cCodArt='" + This.CodArt + "' And " + ;
         "(" + _GCSS("F14cTipMov", 1, 1) + "='1' Or " + _GCSS("F14cTipMov", 1, 1) + "='3')"

cFromF = 'F14c'
cOrder = "F14cCodPro, F14cCodArt, F14cUbiOri"

=f3_sql('*', cFromF, cWhere, cOrder, , 'F14cPPic')

lStado = .F.

Select F14cPPic
Go Top
Do While !Eof()
 	lStado = .F.
	Scatter Name oF14c

	If SubStr(oF14c.F14cTipMov, 1, 1)=="1"
		m.F10cCodUbi = oF14c.F14cUbiOri
	Else
		m.F10cCodUbi = oF14c.F14cUbiDes
	EndIf

	If !f3_seek("F10c")
		Select F14cPPic
		Skip
		Loop
	EndIf

	Select F10c
	Scatter Name oF10c

	If oF10c.F10cPickSN<>'S' .And. oF10c.F10cPickSN<>'G' .And. oF10c.F10cPickSN<>'U'
		Select F14cPPic
		Skip
		Loop
	EndIf

	*> Para control cursores de navegaci�n.
	With This
		.WRngCurP = .F.
		.WRngCurN = .F.
	EndWith

	*> Ubicaci�n de picking activa para realizar comparaciones.
	cCurUbiPic = oF10c.F10cCodUbi

	cCurUbiNext = cCurUbiPic
	cCurUbiPrev = cCurUbiPic
	cCurUbiUp = cCurUbiPic
	cCurUbiDown = cCurUbiPic

	Do While .T.
		If !Empty(cCurUbiNext)
			cCurUbiNext = This.BuscarUbicNext(cCurUbiNext)		&& Buscar la ubicaci�n siguiente a la actual de picking.
			lStado = This._prcpxpicnf(cCurUbiNext)				&& Validar la ubicaci�n obtenida.
			cCurUbiPic = cCurUbiNext
		EndIf

		If !Empty(cCurUbiPrev)
			If !lStado
				cCurUbiPrev = This.BuscarUbicPrev(cCurUbiPrev)	&& Buscar la ubicaci�n anterior a la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiPrev)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiPrev
			EndIf
		EndIf

		If !Empty(cCurUbiUp)
			If !lStado
				cCurUbiUp = This.BuscarUbicUp(cCurUbiUp)		&& Buscar la ubicaci�n encima de la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiUp)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiUp
			EndIf
		EndIf

		If !Empty(cCurUbiDown)
			If !lStado
				cCurUbiDown = This.BuscarUbicDown(cCurUbiDown)	&& Buscar la ubicaci�n debajo de la actual de picking.
				lStado = This._prcpxpicnf(cCurUbiDown)			&& Validar la ubicaci�n obtenida.
				cCurUbiPic = cCurUbiDown
			EndIf
		EndIf

		If lStado .Or. (Empty(cCurUbiNext) .And. Empty(cCurUbiPrev) .And. Empty(cCurUbiUp) .And. Empty(cCurUbiDown))
			Exit
		EndIf
	EndDo

	If lStado
		*> Ubicaci�n OK.
		With This
			.CodUbi = cCurUbiPic
			.CanUbi = This.Cantidad
		EndWith
		lStado = .T.
		Exit
	EndIf

	Select F14cPPic
	Skip
EndDo

Use In (Select ("F14cPPic"))
Return lStado

ENDPROC
PROCEDURE _prcrotrng

*> Buscar ubicaci�n por coeficiente de rotaci�n en zona general asignada al producto.
*> Si no hay rangos (F08U), se ubica en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcrot.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devue|ve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, cCurUbi
Local lStado, oF08u, oF10c

This.UsrError = ""

cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uCRot')
If !lStado
	*> No hay rangos: Ubicar por coeficiente de rotaci�n en todo el almac�n.
	Use In (Select ("F08uCRot"))
	lStado = This._prcrotgen()
	Return lStado
EndIf

Select F08uCRot
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF08u

	lStado = This._prcrotgen(oF08u.F08uUbiDsd, oF08u.F08uUbiHst)

	If lStado
		Exit
	EndIf

	*> Procesar el siguiente rango de ubicaciones.
	Select F08uCRot
	Skip
EndDo

Use In (Select ("F08uCRot"))
Return lStado

ENDPROC
PROCEDURE _prcrotgen

*> Buscar ubicaci�n por coeficiente de rotaci�n en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcrotrng.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbiDesde, ubicaci�n inicial, si rangos.
*>	- cUbiHasta, ubicaci�n final, si rangos.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*>	- Tener en cuenta el tama�o habitual del producto. AVC 29.05.2006

Parameters cUbiDesde, cUbiHasta
Private cWhere, cFromF, cOrder, cCurUbi
Local lStado, oF10c

This.UsrError = ""

*> Valores por defecto de los l�mites.
If Type('cUbiDesde')<>'C'
	cUbiDesde = _Alma + Space(10)
EndIf
If Type('cUbiHasta')<>'C'
	cUbiHasta = _Alma + Replicate("z", 10)
EndIf

*> Cargar ubicaciones de este rango y de la prioridad seg�n coeficiente de rotaci�n del producto.
cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
cWhere = cWhere + "F10cCodUbi Between '" + cUbiDesde + "' And '" + cUbiHasta + "' And "
cWhere = cWhere + "F10cPriori Between " + Str(This.WRotIni) + " And " + Str(This.WRotFin) + " And "
cWhere = cWhere + "F10cPickSN='N' And F10cEstGen<>'B' And F10cEstEnt='N' And F10cTamUbi>='" + This.TamAbi + "'"

cOrder = "F10cPriori, F10cCodUbi"
cFromF = "F10c"

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F10cPPri")

Select F10cPPri
Go Top
Do While !Eof()
	Scatter Name oF10c

	cCurUbi = oF10c.F10cCodUbi
	lStado = This._prcrotcnf(cCurUbi)					&& Validar la ubicaci�n obtenida.

	If lStado
		*> Ubicaci�n OK.
		With This
			.CodUbi = oF10c.F10cCodUbi
			.CanUbi = This.Cantidad
		EndWith
		lStado = .T.
		Exit
	EndIf

	*> Carga la siguiente ubicaci�n del rango activo.
	Select F10cPPri
	Skip
EndDo

Use In (Select ("F10cPPri"))
Return lStado

ENDPROC
PROCEDURE _prcrotcnf

*> Validar la ubicaci�n encontrada en ubicaci�n por coeficiente de rotaci�n.
*> M�todo privado de la clase. Llamado desde los m�todos que se encargan de buscar ubicaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbicacion, ubicaci�n candidata a validar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cCodUbi
Local lStado

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "La ubicaci�n no existe"
	Return .F.
EndIf

Select F10c
cCodUbi = F10cCodUbi

*> Validar tipo almacenaje.
If F10cTipAlm<>This.TipAlm
	This.UsrError = "Tipo almacenaje distinto"
	Return .F.
EndIf

*> Validar tipo ubicaci�n.
If F10cPickSN<>"N"
	This.UsrError = "La ubicaci�n no es de carga"
	Return .F.
EndIf

*> Validar estado bloqueo.
If F10cEstGen=="B" .Or. F10cEstEnt=="S"
	This.UsrError = "La ubicaci�n est� bloqueada"
	Return .F.
EndIf

*> Validar ubicaci�n ocupada.
If F10cNumOcu >= F10cMaxPal
	This.UsrError = "La ubicaci�n est� ocupada"
	Return .F.
EndIf

*> Validar tipo de producto.
If !Empty(F10cTipPro) .And. F10cTipPro<>This.TipPro
	*> Ver si pertenece a alguno de los tipos alternativos.
	lStado = This.VerTipPro(F10cTipPro)
	If !lStado
		This.UsrError = "Tipo de producto diferente"
		Return lStado
	EndIf
EndIf

If This.UbiOcupada(cCodUbi) .Or. This.UbiOcupadaMP(cCodUbi)
	This.UsrError = "Ubicaci�n ocupada por otro producto"
	Return .F.
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE vertippro

*> Validar si el tipo producto est� el la tabla art�culo / tipos producto.
*> M�todo privado de la clase. Llamdao desde los m�todos que se encargan de buscar ubicaci�n.

*> Recibe:
*>	- cTipo, tipo de producto a validar.
*>	- This.TipPro, tipo producto del producto a ubicar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cTipoProducto

Private cWhere
Local lStado

cWhere = "F11tTipPro='" + This.TipPro + "' And F11tTipDes='" + cTipoProducto + "'"
lStado = f3_sql("*", "F11t", cWhere, , , "F11tCur")

Use In (Select("F11tCur"))
Return lStado

ENDPROC
PROCEDURE ubicarfor

*> Entrada principal de ubicaci�n FORZADA de material.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.PicoSN, ubicar un pico � un palet.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado

This.UsrError = ""

lStado = This.prc_for()
If lStado
	This._pesvolar				&& Peso y volumen movimiento.
	This.CargaUbi(1)
EndIf

Return lStado

ENDPROC
PROCEDURE _prcubi

*> Buscar ubicaci�n en carga de forma ca�tica.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""

*> Ubicar en zonas asignadas a situaciones de stock.
lStado = This._prcubistk()
If !lStado .And. (This.WHayStk==.F. .Or. This.ForZon=="S")
	*> Ubicar en rangos asignados al producto.
	lStado = This._prcubirng()

	If !lStado .And. (This.WHayRng==.F. .Or. This.ForZon=="S")
		*> Ubicar ca�ticamente en todo el almac�n
		lStado = This._prcubigen()
	EndIf
EndIf

Return lStado

ENDPROC
PROCEDURE _prcubistk

*> Buscar ubicaci�n en carga en zonas asignadas a situaciones de stock.
*> M�todo privado de la clase. Llamado desde This._prcubi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, oF10s
Local lStado

This.UsrError = ""
This.WHayStk = .F.

*> Ubicar en zonas asignadas a situaciones de stock.
cWhere = "F10sCodPro='" + This.CodPro + "' And F10sCodAlm='" + _Alma + "' And F10sSitStk='" + This.SitStk + "'"
cFromF = "F10s"
cOrder = "F10sCodPro, F10sCodAlm, F10sSitStk, F10sCodZon, F10sUbiDsd"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F10sUSTK')
If !lStado
	*> No hay SSTKs asignadas a zona.
	Use In (Select("F10sUSTK"))
	Return lStado
EndIf

Select F10sUSTK
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF10s

	lStado = This._prcubigen(oF10s.F10sUbiDsd, oF10s.F10sUbiHst)

	If lStado
		Exit
	EndIf

	Select F10sUSTK
	Skip
EndDo

This.WHayStk = .T.
Use In (Select("F10sUSTK"))
Return lStado

ENDPROC
PROCEDURE _prcubirng

*> Buscar ubicaci�n en carga en rangos de ubicaci�n asignados al producto.
*> M�todo privado de la clase. Llamado desde This._prcubi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, oF08u
Local lStado

This.UsrError = ""
This.WHayRng = .F.

*> Ubicar en rangos asignados al producto.
cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uURNG')
If !lStado
	*> No hay rangos.
	Use In (Select ("F08uURNG"))
	Return lStado
EndIf

Select F08uURNG
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF08u

	lStado = This._prcubigen(oF08u.F08uUbiDsd, oF08u.F08uUbiHst)

	If lStado
		Exit
	EndIf

	Select F08uURNG
	Skip
EndDo

This.WHayRng = .T.
Use In (Select ("F08uURNG"))
Return lStado

ENDPROC
PROCEDURE _prcubigen

*> Buscar ubicaci�n ca�tica en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcubistk y This._prcubirng.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbiDesde, ubicaci�n inicial, si rangos.
*>	- cUbiHasta, ubicaci�n final, si rangos.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbiDesde, cUbiHasta
Private cWhere, cWhereTam, cFromF, cOrder, cCurUbi
Local lStado, oF10c, oF11t

This.UsrError = ""

*> Valores por defecto de los l�mites.
If Type('cUbiDesde')<>'C'
	cUbiDesde = _Alma + Space(10)
EndIf
If Type('cUbiHasta')<>'C'
	cUbiHasta = _Alma + Replicate("z", 10)
EndIf

*> Generar cursor con los tipos de producto alternativos, si hay.
=CrtCursor("F11t", "F11tTPro")

*> Cargar resto de tipos de producto alternativos.
cWhere = "F11tTipPro='" + This.TipPro + "'"
cOrder = "F11tPriori"

lStado = f3_sql("*", "F11t", cWhere, cOrder, , "+F11tTPro")

*> Agregar el tipo de producto del art�culo.
Select F11tTPro
Append Blank
Replace F11tTipPro With This.TipPro, ;
		F11tPriori With 0

*> Condiciones de filtro general para las ubicaciones.
cOrder = "F10cCodUbi, F10cTamUbi"
cFromF = "F10c"

*> Filtro por tama�o de ubicaci�n.
cWhereUbi = ""
If This.ForTam<>"S"
	cWhereUbi = " And F10cTamUbi='" + This.TamAbi + "'"
Else
	cWhereUbi = " And F10cTamUbi>='" + This.TamAbi + "'"
EndIf

Select F11tTPro
Go Top
Do While !Eof()
	Scatter Name oF11t

	*> Cargar ubicaciones del rango, tama�o palet y tipo producto adecuados.
	cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
	cWhere = cWhere + "F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cCodUbi Between '" + cUbiDesde + "' And '" + cUbiHasta + "' And "
	cWhere = cWhere + "F10cTipPro='" + oF11t.F11tTipDes + "' And "
	cWhere = cWhere + "F10cPickSN='N' And F10cEstGen<>'B' And F10cEstEnt='N'"

	*> Agregar condiciones generales de filtro.
	cWhere = cWhere + cWhereUbi

	lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F10cUGen")

	Select F10cUGen
	Go Top
	Do While !Eof()
		Scatter Name oF10c

		cCurUbi = oF10c.F10cCodUbi
		lStado = This._prcubicnf(cCurUbi)					&& Validar la ubicaci�n obtenida.

		If lStado
			*> Ubicaci�n OK.
			With This
				.CodUbi = oF10c.F10cCodUbi
				.CanUbi = This.Cantidad
			EndWith
			lStado = .T.
			Exit
		EndIf

		*> Carga la siguiente ubicaci�n del rango activo.
		Select F10cUGen
		Skip
	EndDo

	If lStado
		Exit
	EndIf

	*> Probar con el siguiente tama�o alternativo.
	Select F11tTPro
	Skip
EndDo

Use In (Select ("F11tTPro"))
Use In (Select ("F10cUGen"))
Return lStado

ENDPROC
PROCEDURE _prcubicnf

*> Validar la ubicaci�n encontrada en ubicaci�n ca�tica.
*> M�todo privado de la clase. Llamado desde los m�todos que se encargan de buscar ubicaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbicacion, ubicaci�n candidata a validar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cCodUbi
Local lStado

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "La ubicaci�n no existe"
	Return .F.
EndIf

Select F10c
cCodUbi = F10cCodUbi

*!*	*> Validar tipo almacenaje.
*!*	If F10cTipAlm<>This.TipAlm
*!*		This.UsrError = "Tipo almacenaje distinto"
*!*		Return .F.
*!*	EndIf

*!*	*> Validar tipo ubicaci�n.
*!*	If F10cPickSN<>"N"
*!*		This.UsrError = "La ubicaci�n no es de carga"
*!*		Return .F.
*!*	EndIf

*> Validar estado bloqueo.
If F10cEstGen=="B" .Or. F10cEstEnt=="S"
	This.UsrError = "La ubicaci�n est� bloqueada"
	Return .F.
EndIf

*> Validar ubicaci�n ocupada.
If F10cNumOcu >= F10cMaxPal
	This.UsrError = "La ubicaci�n est� ocupada"
	Return .F.
EndIf

*!*	*> Validar tipo de producto.
*!*	If !Empty(F10cTipPro) .And. F10cTipPro<>This.TipPro
*!*		*> Ver si pertenece a alguno de los tipos alternativos.
*!*		lStado = This.VerTipPro(F10cTipPro)
*!*		If !lStado
*!*			This.UsrError = "Tipo de producto diferente"
*!*			Return lStado
*!*		EndIf
*!*	EndIf

If This.UbiOcupada(cCodUbi) .Or. This.UbiOcupadaMP(cCodUbi)
	This.UsrError = "Ubicaci�n ocupada por otro producto"
	Return .F.
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE updatealb

*> Actualizar el estado de un albar�n de entrada.

*> Recibe:
*>	- N� de albar�n de entrada a actualizar.
*>	- This.UpdAlbC, Actualizar estado cabecera albar�n (def: "N")

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cAlbaran

Private cWhere, cField, cFromF, cCampos, cValores, cStat
Local lStado, nReg

This.UsrError = ""

If This.UpdAlbC<>"S"
	Return .T.
EndIf

*> Cargamos la cabecera del albar�n.
m.F18mNumEnt = cAlbaran
If !f3_seek("F18m")
	This.UsrError = "No existe el albar�n"
	Return .F.
EndIf

*> Cargar el detalle del albar�n para calcular el estado de la cabecera.
cField = "*"
cFromF = "F18n"
cWhere = "F18nNumEnt='" + cAlbaran + "'"

lStado = f3_sql(cField, cFromF, cWhere, , , "F18nUPDS")
Do While .T.
	If !lStado
		*> Albar�n sin l�neas !!!
		cStat = "4"
		Exit
	EndIf

	Select F18nUPDS

	Locate For F18nEstado=="1"
	If Found()
		*> Albar�n con l�neas pendientes de paletizar.
		cStat = "1"
		Exit
	EndIf

	Locate For F18nEstado=="0"
	If Found()
		*> Albar�n con l�neas pendientes de paletizar.
		Count For F18nEstado<>"0" To nReg
		cStat = Iif(nReg==0, "0", "1")
		Exit
	EndIf

	Locate For F18nEstado=="2"
	If Found()
		*> Albar�n con MPs pendientes.
		cStat = "2"
		Exit
	EndIf

	*> Si llega aqu� salimos cerrando el albar�n.
	cStat = "3"
	Exit
EndDo

cCampos = "F18mEstado"
cValores = "cStat"
cWhere = "F18mNumEnt='" + cAlbaran + "'"
lStado = f3_updtun("F18m", , cCampos, cValores, , cWhere, "N")

Use In (Select("F18nUPDS"))
Return lStado

ENDPROC
PROCEDURE actzobj_access

*> Inicializar la clase para la funci�n de actualizaci�n.

If Type('This.ActzObj')<>'O'
	This.ActzObj = CreateObject("OraFncActz")
EndIf

Return This.ActzObj

ENDPROC
PROCEDURE actzprm_access

*> Inicializar la clase para los par�metros de la funci�n de actualizaci�n.

If Type('This.ActzPrm')<>'O'
	This.ActzPrm = CreateObject("OraPrmActz")
EndIf

Return This.ActzPrm

ENDPROC
PROCEDURE confirmarmp

*> Confirmar la ubicaci�n de UN MP de entrada.
*> Realiza la entrada en ocupaciones.
*> Crea un registro en el hist�rico de movimientos.
*> Elimina el MP.

*> Recibe:
*>	- This.NumMovMP � bien N� de MP a confirmar.
*>	- This.TMovDst, Tipo movimiento para hist�rico (generalmente "1000").
*>	- This.UpdAlbD, Actualizar estado l�nea albar�n (def: "N")
*>	- This.UpdAlbC, Actualizar estado cabecera albar�n (def: "N")

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cMovimiento

Private cNumMov, oF14c
Local lStado

This.UsrError = ""

cNumMov = Iif(PCount()==1, cMovimiento, This.NumMovMP)

*> Validar tipo de movimiento a generar en hist�rico.
m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Validar el MP recibido.
m.F14cNumMov = cNumMov
lStado = f3_seek("F14c")
If !lStado
	This.UsrError = "No existe el MP"
	Return lStado
EndIf

Select F14c
Scatter Name oF14c

If !Between(oF14c.F14cTipMov, "1000", "1999")
	This.UsrError = "El MP no es de entrada"
	Return .F.
EndIf

If Empty(oF14c.F14cUbiOri)
	This.UsrError = "El MP no tiene ubicaci�n"
	Return .F.
EndIf

*> Borrar el MP. Es necesario hacerlo ANTES de actualizar, pues en caso contrario recalcula mal la ubicaci�n.
=f3_baja("F14c")

*> Asignar valores a propiedades de la funci�n de actualizaci�n.
*> El objeto se crea de forma autom�tica.

This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de entrada al almac�n.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('M', oF14c)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF14c.F14cCanFis
	.PoCRes = 0
	.PoRowID= oF14c.F14cIdOcup
	
	.PmEnSa = "E"
	.PmTMov = This.TMovDst

	.PtAcc  = '07'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error confirmando MP entrada"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en hist�rico.


*> Actualizar el estado de la l�nea del albar�n.
With oF14c
	lStado = This.UpdateAlbLin(.F14cNumEnt, .F14cCodPro, .F14cTipDoc, .F14cNumDoc, .F14cCodArt, .F14cNumLot, .F14cSitStk, PadL(AllTrim(Str(.F14cLinDoc)),4,'0'))

	*> Actualizar el estado de la cabecera del albar�n.
	lStado = This.UpdateAlb(.F14cNumEnt)
EndWith

*>Si es no quedan m�s movimientos de esa Entrada/Documento, liberar el pedido para el ERP.
Use In (Select("cBloqF14c"))
_Sentencia="Select * From F14c001 Where " + ;
   	       "F14cCodPro='" + oF14c.F14cCodPro + "' And " + ;
	       "F14cTipDoc='" + oF14c.F14cTipDoc + "' And " + ;
	       "F14cNumDoc='" + oF14c.F14cNumDoc + "'"
	
=SqlExec(_Asql,_Sentencia,'cBloqF14c')

Select cBloqF14c
Go Top	
If Eof()
	*>Quitar Bloqueo
	oComm = CreateObject('commstd', , _Procaot, _Alma)
	oComm.Inicializar
	lStado = oComm.ProcesarBloqueoPedido('L', 'DOCS', oF14c.F14cTipDoc, oF14c.F14cNumDoc)
	If !Empty(lStado)
		*> Pedido no disponible.
		_LxErr = oComm.UsrError
	EndIf	
	Release oComm
EndIf

Use In (Select("cBloqF14c"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE updatealblin

*> Actualizar el estado de una l�nea de un albar�n de entrada.

*> Recibe:
*>	- N� albar�n.
*>	- Propietario.
*>	- Tipo de documento.
*>	- N� de documento.
*>	- Art�culo.
*>	- N� de lote.
*>	- Situaci�n de stock.
*>	- This.UpdAlbD, Actualizar estado l�nea albar�n (def: "N")
*>	- This.CieAlb, Cerrar albar�n (def: "N")

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 09.01.2008 (AVC) Calcular estado l�nea albar�n seg�n la cantidad recontada, en lugar de la cantidad albar�n.

Parameters cAlbaran, cPropietario, cTipoDocumento, cDocumento, cArticulo, cLote, cSitStk, cLinDoc

Private cWhere, cWhereN, cField, cFromF, cCampos, cValores, cStat
Local lStado, nCantidadRecontada

This.UsrError = ""

If This.UpdAlbD # "S"
	Return .T.
EndIf

*> Cargar la l�nea de detalle del albar�n.
cWhereN = 		    "F18nNumEnt='" + cAlbaran + "' And "
cWhereN = cWhereN + "F18nCodPro='" + cPropietario + "' And "
cWhereN = cWhereN + "F18nTipDoc='" + cTipoDocumento + "' And "
cWhereN = cWhereN + "F18nNumDoc='" + cDocumento + "' And "
cWhereN = cWhereN + "F18nLinDoc='" + cLinDoc + "' And "
cWhereN = cWhereN + "F18nCodArt='" + cArticulo + "' And "
cWhereN = cWhereN + "F18nNumLot='" + cLote + "' And "
cWhereN = cWhereN + "F18nSitStk='" + cSitStk + "'"

cFromF = "F18n"
cField = "*"
lStado = f3_sql(cField, cFromF, cWhereN, , , "F18nSTT")
If !lStado
	This.UsrError = "No existe la l�nea del albar�n"
	Use In (Select("F18nSTT"))
	Return lStado
EndIf

*>Paso la linea a num�rico.
_Linea=Val(cLinDoc)

*> Ver si hay MPs de entrada de esta l�nea del albar�n.
cWhere = 		  "F14cNumEnt='" + cAlbaran + "' And "
cWhere = cWhere + "F14cTipMov Between '1000' And '1999' And "
cWhere = cWhere + "F14cCodPro='" + cPropietario + "' And "
cWhere = cWhere + "F14cTipDoc='" + cTipoDocumento + "' And "
cWhere = cWhere + "F14cNumDoc='" + cDocumento + "' And "
cWhere = cWhere + "F14cLinDoc=" + Str(_Linea) + " And "
cWhere = cWhere + "F14cCodArt='" + cArticulo + "' And "
cWhere = cWhere + "F14cNumLot='" + cLote + "' And "
cWhere = cWhere + "F14cSitStk='" + cSitStk + "'"

cFromF = "F14c"
cField = "*"
If f3_sql(cField, cFromF, cWhere, , , "F14cSTT")
	*> Ver si ya hay movimientos en el hist�rico.
	cWhere = 		  "F20cNumEnt='" + cAlbaran + "' And "
	cWhere = cWhere + "F20cTipMov Between '1000' And '1999' And "
	cWhere = cWhere + "F20cCodPro='" + cPropietario + "' And "
	cWhere = cWhere + "F20cTipDoc='" + cTipoDocumento + "' And "
	cWhere = cWhere + "F20cNumDoc='" + cDocumento + "' And "
	cWhere = cWhere + "F20cLinDoc=" + Str(_Linea) + " And "		
	cWhere = cWhere + "F20cCodArt='" + cArticulo + "' And "
	cWhere = cWhere + "F20cNumLot='" + cLote + "' And "
	cWhere = cWhere + "F20cSitStk='" + cSitStk + "'"

	cFromF = "F20c"
	cField = "*"
	lStado = f3_sql(cField, cFromF, cWhere, , , "F20cSTT")
	
	nCantidadHistorico = 0
	
	Select F20cSTT
	Go Top
	If !Eof()
		Sum(F20cCanFis) To nCantidadHistorico
	EndIf

	*> Quedan MPs pendientes de confirmar.
	Select F14cSTT
	Sum(F14cCanFis) To nCantidadRecontada
	cStat = Iif((nCantidadRecontada + nCantidadHistorico) >= F18nSTT.F18nCanRec, "2", "1")
Else
	If This.CieAlb=="S"
		*> Se fuerza a cerrar la l�nea del albar�n.
		cStat = "3"
	Else
		*> Ver si ya hay movimientos en el hist�rico.
		cWhere = 		  "F20cNumEnt='" + cAlbaran + "' And "
		cWhere = cWhere + "F20cTipMov Between '1000' And '1999' And "
		cWhere = cWhere + "F20cCodPro='" + cPropietario + "' And "
		cWhere = cWhere + "F20cTipDoc='" + cTipoDocumento + "' And "
		cWhere = cWhere + "F20cNumDoc='" + cDocumento + "' And "
		cWhere = cWhere + "F20cLinDoc=" + Str(_Linea) + " And "		
		cWhere = cWhere + "F20cCodArt='" + cArticulo + "' And "
		cWhere = cWhere + "F20cNumLot='" + cLote + "' And "
		cWhere = cWhere + "F20cSitStk='" + cSitStk + "'"

		cFromF = "F20c"
		cField = "*"
		lStado = f3_sql(cField, cFromF, cWhere, , , "F20cSTT")
		
		Select F20cSTT
		Go Top
		If Eof()
			*>Si no existen ni mps ni hist�rico, no se ha hecho nada.
			cStat = "0"
		Else
			Select F20cSTT
			Sum(F20cCanFis) To nCantidadRecontada
			cStat = Iif(nCantidadRecontada >= F18nSTT.F18nCanRec, "3", "1")
		EndIf
		
		
		*> Estado de la l�nea del albar�n.
		*cStat = Iif(lStado, "3", "0")
	EndIf
EndIf

*> Actualizar el estado de la l�nea de detalle del albar�n.
cCampos = "F18nEstado"
cValores = "cStat"

If !f3_UpdTun("F18n", , cCampos, cValores, , cWhereN)
	_LxErr = "Error al actualizar la l�nea del albar�n"
	Do Form St3Inc With .T.
EndIf

Use In (Select("F18nSTT"))
Use In (Select("F14cSTT"))
Use In (Select("F20cSTT"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE ubicarmref

*> Entrada principal de ubicaci�n de palets multi-referencia.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.NumPal, n� de palet a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado

This.UsrError = ""

lStado = This.prc_mref()
If lStado
	This._pesvolpal				&& Peso y volumen palet.
	This.CargaUbi(1)
EndIf

Return lStado

ENDPROC
PROCEDURE pesvolpal

*> Calcular el peso y volumen para un palet multireferencia.

*> Recibe:
*>	- This.NumPal, n� de palet a calcular.

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""
lStado = This._pesvolpal()

Return lStado

ENDPROC
PROCEDURE prc_mref

*> Proceso de ubicaci�n de palets multireferencia.
*> M�todo privado de la clase. Llamado desde This.UbicarMRef.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.NumPal, n� de palet a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cCampos, cValores, cCodUbi
Local lStado

lStado = This._ubicar()
If lStado
	*> Actualizar la ubicaci�n en todos los MPs del palet.
	cWhere = "F14cNumPal='" + This.NumPal + "' And " + _GCSS("F14cTipMov", 1, 1) + "='1'"
	cCampos = "F14cUbiOri"
	cCodUbi = This.CodUbi
	cValores = "cCodUbi"
	=f3_updtun("F14c", , cCampos, cValores, , cWhere)
EndIf

Return lStado

ENDPROC
PROCEDURE prc_ubi

*> Proceso de ubicaci�n general de material.
*> M�todo privado de la clase. Llamado desde This.Ubicar.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.PicoSN, ubicar un pico � un palet.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	This.Ubicar, entrada a la funci�n de ubicaci�n.

Local lStado

lStado = This._ubicar()
Return lStado

ENDPROC
PROCEDURE _ubicar

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.PicoSN, ubicar un pico � un palet.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	This.prc_ubi(), ubicaci�n general.
*>	This.prc_mref(), ubicaci�n multireferencia.

Local lStado

*> Si el MP es de Cross Docking ubicar en muelle del pedido o muelle de cross docking gen�rico.
If This.CDocking=='S'
	lStado = This.prc_cd()
	Return lStado
EndIf


*> Ubicaci�n en Moladas, si cal.
If This.TipAlm=='M'
	lStado = This.prc_mol()
	Return lStado
EndIf


*> Primer paso: Ubicar primero en rangos.
lStado = This._prcpicrng()
If lStado
	Return lStado
EndIf

*> Primer paso: Ubicar primero en picking.
lStado = This.Calcular_Ubicacion()
If lStado
	Return lStado
EndIf



*> Primer paso: Ubicar primero en picking.
If This.Pickin=="S" .Or. This.PicoSn=="S"
	lStado = This._prcpic()
	If lStado
		Return lStado
	EndIf
EndIf


*> Segundo paso: Ubicar en carga por proximidad a picking.
If This.CriUbi=="P"
	lStado = This._prcpxpi()
	If lStado
		Return lStado
	EndIf
EndIf

*> Tercer paso: Ubicar en carga por coeficiente de rotaci�n.
If This.CoeRot=="S"
	lStado = This._prcrot()
	If lStado
		Return lStado
	EndIf
EndIf

*>Primero buscar por situaci�n de stock.
lStado = This._prcubistk()
If lStado
	Return lStado
EndIf


*> Finalmente: Ubicar en carga ca�ticamente.
lStado = This._prcubi()

Return lStado

ENDPROC
PROCEDURE _pesvolar

*> Calcular el peso y volumen para un art�culo / cantidad.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.CanUbi, cantidad ubicada.

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.PesVolAr.

*> Historial de modificaciones:
*>	- Separar la versi�n privada de la clase de la versi�n p�blica para programaci�n. AVC 04.01.2006
*>	- Tener en cuenta los productos sin datos de peso / volumen unitario. AVC - 27.03.2008

Local lStado, nPeso, nVolumen, nTotPal, oF08c

This.UsrError = ""

m.F08cCodPro = This.CodPro
m.F08cCodArt = This.CodArt

If !f3_seek("F08c")
	This.PesOcu = 0
	This.VolOcu = 0
	Return
EndIf

Select F08c
Scatter Name oF08c

*nPeso = oF08c.F08cPesUni * This.CanUbi / 1000.0
nPeso = oF08c.F08cPesUni * This.CanUbi
nVolumen = oF08c.F08cVolUni * This.CanUbi / 1000.0
nTotPal = oF08c.F08cUniPac * oF08c.F08cPacCaj * oF08c.F08cCajPal
If nTotPal <= 0
	nTotPal = 1
EndIf

*> Calcular volumen a partir del tama�o, si el art�culo es cero.
If nVolumen==0
	m.F00mCodTam = oF08c.F08cTamAbi
	If f3_seek("F00m")
		Select F00m
		nVolumen = (F00mDimVol / nTotPal) * This.CanUbi / 1000.0
	EndIf
EndIf

This.PesOcu = nPeso
This.VolOcu = nVolumen

Return

ENDPROC
PROCEDURE _pesvolpal

*> Calcular el peso y volumen para un palet multireferencia.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.NumPal, n� de palet a calcular.

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.PesVolPal.

*> Historial de modificaciones:
*>	- Separar la versi�n privada de la clase de la versi�n p�blica para programaci�n. AVC 04.01.2006

Private cWhere
Local lStado, nPeso, nVolumen

This.UsrError = ""
Store 0 To nPeso, nVolumen

*> Cargar cursor con los MPs de entrada del palet.
cWhere = "F14cNumPal='" + This.NumPal + "' And " + _GCSS("F14cTipMov", 1, 1) + "='1'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cMREF")

Select F14cMREF
Go Top
Do While !Eof()
	*> Calcular volumen / peso del art�culo actual.
	With This
		.CodPro = F14cCodPro
		.CodArt = F14cCodArt
		.CanUbi = F14cCanFis
		._pesvolar

		nPeso = nPeso + .PesOcu
		nVolumen = nVolumen + .VolOcu
	EndWith

	Select F14cMREF
	Skip
EndDo

*> Asignar valores a propiedades.
This.PesOcu = nPeso
This.VolOcu = nVolumen

Use In (Select("F14cMREF"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE pesvolid

*> Calcular el peso y volumen para un ID ocupaci�n.

*> Recibe:
*>	- This.IdOcup, ID de la ocupaci�n a calcular.

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado, nPeso, nVolumen

This.UsrError = ""
lStado = This._pesvolid()

Return lStado

ENDPROC
PROCEDURE _pesvolid

*> Calcular el peso y volumen para un ID ocupaci�n.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.IdOcup, ID de la ocupaci�n a calcular.

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.PesVolPal.

Private cWhere
Local lStado, nPeso, nVolumen

This.UsrError = ""
Store 0 To nPeso, nVolumen

*> Cargar cursor con los MPs de entrada del palet.
cWhere = "F14cIdOcup='" + This.IdOcup + "' And " + _GCSS("F14cTipMov", 1, 1) + "='1'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cIDOC")

Select F14cIDOC
Go Top
Do While !Eof()
	*> Calcular volumen / peso del art�culo actual.
	With This
		.CodPro = F14cCodPro
		.CodArt = F14cCodArt
		.CanUbi = F14cCanFis
		._pesvolar

		nPeso = nPeso + .PesOcu
		nVolumen = nVolumen + .VolOcu
	EndWith

	Select F14cIDOC
	Skip
EndDo

*> Asignar valores a propiedades.
This.PesOcu = nPeso
This.VolOcu = nVolumen

Use In (Select("F14cIDOC"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE calcpesvolpal

*> Calcular el peso y volumen de un palet. Genera archivo de palets, F16t.

*> Recibe:
*>	- This.NumPal

*> Devuelve:
*>	- This.PesOcu, peso movimiento.
*>	- This.VolOcu, volumen movimiento.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado, nPeso, nVolumen, nTotPal

This.UsrError = ""
lStado = This._calcpesvolpal()

Return lStado

ENDPROC
PROCEDURE _calcpesvolpal
*!*	*> Calcular el peso y volumen de entrada de un palet.
*!*	*> M�todo privado de la clase.

*!*	*> Recibe:
*!*	*>	- This.NumPal, n� de palet a calcular.

*!*	*> Devuelve:
*!*	*>	- This.PesOcu, peso movimiento.
*!*	*>	- This.VolOcu, volumen movimiento.
*!*	*>	- Estado (.T. / .F.)
*!*	*>	- This.UsrError, texto errores.

*!*	*> Llamado desde:
*!*	*>	- This.CalcPesVolPal.

*!*	*> Historial de modificaciones:
*!*	*> 27.03.2008 (AVC) Tener en cuenta los productos sin datos de peso / volumen unitario.
*!*	*> 08.07.2008 (AVC) Modificado para calcular F16t s�lo si no existe.

*!*	Local lStado, lStt, nPeso, nVolumen, cCurMov
*!*	Private cWhere, cOrder, oF20c

*!*	Store "" To This.UsrError, cCurMov
*!*	Store 0 To nPeso, nVolumen

*!*	*> Ver si ya existe la ficha de palet.
*!*	m.F16tNumPal = This.NumPal
*!*	lStt = f3_seek("F16t")
*!*	If lStt
*!*		*> Existe: Tomar los datos de peso y volumen y volver sin calcular.
*!*		Select F16t
*!*		This.PesOcu = F16tPesPal
*!*		This.VolOcu = F16tVolPal
*!*		Return
*!*	EndIf

*!*	*> Cargar los movimientos de entrada del palet.
*!*	cWhere = "F20cNumPal='" + This.NumPal + "' And F20cEntSal='E'"
*!*	cOrder = "F20cFecMov, F20cTipMov"

*!*	lStado = f3_sql("*", "F20c", cWhere, , , "F20cCalc")
*!*	Select F20cCalc
*!*	Go Top
*!*	Do While !Eof()
*!*		Scatter Name oF20c

*!*		If !Empty(cCurMov) .And. cCurMov<>oF20c.F20cTipMov
*!*			*> Considerar solo los primeros movimientos de entrada (si es un palet multireferencia.
*!*			*> Esto permite calcular palets entrados inicialmente por inventario.
*!*			Exit
*!*		EndIf

*!*		With This
*!*			.CodPro = oF20c.F20cCodpro
*!*			.CodArt = oF20c.F20cCodArt
*!*			.CanUbi = oF20c.F20cCanFis
*!*			=._pesvolar()

*!*			nPeso = nPeso + .PesOcu
*!*			nVolumen = nVolumen + .VolOcu
*!*		EndWith

*!*		cCurMov = oF20c.F20cTipMov

*!*		Select F20cCalc
*!*		Skip
*!*	EndDo

*!*	*> Actualizar peso y volumen en tabla de control de palet.
*!*	m.F16tNumPal = This.NumPal
*!*	lStt = f3_seek("F16t")
*!*	Select F16t

*!*	If !lStt
*!*		Select F16t
*!*		Append Blank
*!*		Replace F16tNumPal With This.NumPal
*!*	EndIf

*!*	Select F16t
*!*	Replace F16tPesPal With nPeso
*!*	Replace F16tVolPal With nVolumen

*!*	If !lStt
*!*		*> Siempre se crea la ficha, aunque el producto no tenga par�metros de peso y volumen.
*!*		=f3_instun("F16t")
*!*	Else
*!*		*> Si el art�culo no tiene par�metros de peso / volumen (valores cero), no se actualiza.
*!*		If !Empty(nPeso) .Or. !Empty(nVolumen)
*!*			=f3_updtun("F16t")
*!*		EndIf
*!*	EndIf

*!*	*> Valores que se devuelven al programa que llama.
*!*	This.PesOcu = nPeso
*!*	This.VolOcu = nVolumen

Return

ENDPROC
PROCEDURE updatealbent

*> Recalcular una albar�n de entrada.

*> Recibe:
*>	- N� albar�n de entrada.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cAlbaran

Private cWhere, oF18n, oF14c, oF20c
Local lStado, nCantidadRecontada

This.UsrError = ""

cWhere = "F18nNumEnt='" + cAlbaran + "'"

lStado = f3_sql("*", "F18n", cWhere, , , "F18nRECALC")
If !lStado
	This.UsrError = "No hay datos"
	Use In (Select ("F18nRECALC"))
	Return lStado
EndIf

*> Por cada l�nea cargar la cantidad recontada: HMs del albar�n + MPs del albar�n pendientes de ubicar.
Select F18nRECALC
Go Top
Do While !Eof()
	Scatter Name oF18n
	nCantidadRecontada = 0

	*> Cargar Los MPs pendientes de ubicar.
	cWhere = 		  "F14cNumEnt='" + cAlbaran + "' And "
	cWhere = cWhere + "F14cCodPro='" + oF18n.F18nCodPro + "' And "
	cWhere = cWhere + "F14cTipDoc='" + oF18n.F18nTipDoc + "' And "
	cWhere = cWhere + "F14cNumDoc='" + oF18n.F18nNumDoc + "' And "
	cWhere = cWhere + "F14cLinDoc=" + oF18n.F18nLinDoc

	lStado = f3_sql("*", "F14c", cWhere, , , "F14cRECALC")
	Select F14cRECALC
	Go Top
	Do While !Eof()
		Scatter Name oF14c
		nCantidadRecontada = nCantidadRecontada + oF14c.F14cCanFis

		Select F14cRECALC
		Skip
	EndDo

	*> Agregar las cantidades ya ubicadas, seg�n el hist�rico
	cWhere = 		  "F20cNumEnt='" + cAlbaran + "' And "
	cWhere = cWhere + "F20cCodPro='" + oF18n.F18nCodPro + "' And "
	cWhere = cWhere + "F20cTipDoc='" + oF18n.F18nTipDoc + "' And "
	cWhere = cWhere + "F20cNumDoc='" + oF18n.F18nNumDoc + "' And "
	cWhere = cWhere + "F20cLinDoc=" + oF18n.F18nLinDoc

	lStado = f3_sql("*", "F20c", cWhere, , , "F20cRECALC")
	Select F20cRECALC
	Go Top
	Do While !Eof()
		Scatter Name oF20c

		If oF20c.F20cEntSal=='E'
			nCantidadRecontada = nCantidadRecontada + oF20c.F20cCanFis
		Else
			nCantidadRecontada = nCantidadRecontada - oF20c.F20cCanFis
		EndIf

		Select F20cRECALC
		Skip
	EndDo

	*> Actualizar la cantidad recontada de la l�nea.
	Select F18nRECALC
	Replace F18nCanRec With nCantidadRecontada
	=f3_updtun("F18n", , , , "F18nRECALC")
	
	*> Actualizar el estado de la l�nea del albar�n.
	lStado = This.UpdateAlbLin(cAlbaran, oF18n.F18nCodPro, oF18n.F18nTipDoc, oF18n.F18nNumDoc, oF18n.F18nCodArt, oF18n.F18nNumLot, oF18n.F18nSitStk)

	*> Recalcular la siguiente l�nea.
	Select F18nRECALC
	Skip
EndDo

*> Recalcular el estado del albar�n.
lStado = This.UpdateAlb(cAlbaran)

Use In (Select("F18nRECALC"))
Use In (Select("F14cRECALC"))
Use In (Select("F20cRECALC"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE ubicarmol

*> Entrada principal de ubicaci�n de productos en masivo (moladas).

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.NumPal, n� de palet a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado

This.UsrError = ""

lStado = This.prc_mol()
If lStado
	This._pesvolpal				&& Peso y volumen palet.
	This.CargaUbi(1)
EndIf

Return lStado

ENDPROC
PROCEDURE prc_mol

*> Proceso de ubicaci�n de material en moladas.
*> M�todo privado de la clase. Llamado desde This.Ubicar.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.PicoSN, ubicar un pico � un palet.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	This.Ubicar, entrada a la funci�n de ubicaci�n.

Local lStado

If This.TipAlm<>'M'
	This.UsrError = 'El art�culo no es de moladas'
	Return .F.
EndIf

lStado = This._prcmol()
Return lStado

ENDPROC
PROCEDURE _prcmol

*> Buscar ubicaci�n en moladas.
*> M�todo privado de la clase.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.NumRem, n� de remontes.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamdo desde:
*>	- This.prc_mol()

Local lStado

This.UsrError = ""

*> Intentar ubicar donde ya est� ubicado el producto.
lStado = This._prcmolcur()
If !lStado
	*> Crear una nueva molada.
	lStado = This._prcmolnew()
EndIf

Return lStado

ENDPROC
PROCEDURE _prcmolcur

*> Buscar ubicaci�n en moladas que ya contengan el producto.
*> M�todo privado de la clase.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.NumRem, n� de remontes.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamdo desde:
*>	This._prcmol(), ubicaci�n en moladas.

Local lStado
Private cWhere, cOrder, cField, cFromF
Private oMol, oMP

This.UsrError = ""

cFromF = "F16c,F10c"
cOrder = "Cuantos DESC"

*> Cargar las ubicaciones donde ya est� el producto.
If This.MulLot=='N'
	*> Sin control de lote.
	cField = "F16cCodPro, F16cCodArt, F16cNumLot, F16cCodUbi, " + _GCN("Count(*)", 0) + " As Cuantos"
	cGroup = "F16cCodPro, F16cCodArt, F16cNumLot, F16cCodUbi"

	cWhere = 		  "F16cCodPro='" + This.CodPro + "' And F16cCodArt='" + This.CodArt + "' And F16cNumLot='" + This.NumLot + "' And "
	cWhere = cWhere + "F10cCodUbi=F16cCodUbi And F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cEstEnt='N' And F10cPickSn<>'E' And F10cPickSn<>'L'"
Else
	*> Con control de lote.
	cField = "F16cCodPro, F16cCodArt, F16cCodUbi, " + _GCN("Count(*)", 0) + " As Cuantos"
	cGroup = "F16cCodPro, F16cCodArt, F16cCodUbi"

	cWhere = 		  "F16cCodPro='" + This.CodPro + "' And F16cCodArt='" + This.CodArt + "' And "
	cWhere = cWhere + "F10cCodUbi=F16cCodUbi And F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cEstEnt='N' And F10cPickSn<>'E' And F10cPickSn<>'L'"
EndIf

lStado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "_F16cCURMOL")
If !lStado
	If _xier<=0
		This.UsrError = "Error en carga ocupaciones moladas"
		Use In (Select ("_F16cCURMOL"))
		Return lStado
	EndIf
EndIf

cFromF = "F14c,F10c"
cOrder = "Cuantos DESC"

*> Cargar los MPs donde ya va el producto.
If This.MulLot=='N'
	*> Sin control de lote.
	cField = "F14cCodPro, F14cCodArt, F14cNumLot, F14cUbiOri, " + _GCN("Count(*)", 0) + " As Cuantos"
	cGroup = "F14cCodPro, F14cCodArt, F14cNumLot, F14cUbiOri"

	cWhere = 		  "F14cCodPro='" + This.CodPro + "' And F14cCodArt='" + This.CodArt + "' And F14cNumLot='" + This.NumLot + "' And "
	cWhere = cWhere + "F10cCodUbi=F14cUbiOri And F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cEstEnt='N' And F10cEstGen<>'B' And F10cPickSn<>'E' And F10cPickSn<>'L'"
Else
	*> Con control de lote.
	cField = "F14cCodPro, F14cCodArt, F14cUbiOri, " + _GCN("Count(*)", 0) + " As Cuantos"
	cGroup = "F14cCodPro, F14cCodArt, F14cUbiOri"

	cWhere = 		  "F14cCodPro='" + This.CodPro + "' And F14cCodArt='" + This.CodArt + "' And "
	cWhere = cWhere + "F10cCodUbi=F14cUbiOri And F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cEstEnt='N' And F10cEstGen<>'B' And F10cPickSn<>'E' And F10cPickSn<>'L'"
EndIf

lStado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "_F14cCURMOL")
If !lStado
	If _xier<=0
		This.UsrError = "Error en carga MPs moladas"
		Use In (Select ("_F16cCURMOL"))
		Use In (Select ("_F14cCURMOL"))
		Return lStado
	EndIf
EndIf

Select _F14cCURMOL
Delete All For IsNull(Cuantos)

*> Ubicaci�n a devolver.
This.CodUbi = ""

*> Validar las ubicaciones donde ya est� el producto.
Select _F16cCURMOL
Delete All For IsNull(Cuantos)
Go Top

Do While !Eof() And Empty(This.CodUbi)
	Scatter Name oMol

	*> Buscar, si hay, los MPs asignados a la ubicaci�n.
	Select _F16cCurMOL
	Locate For F14cUbiOri==oMol.F16cCodUbi
	If Found()
		Scatter Name oMP
		Delete For F14cUbiOri==oMol.F16cCodUbi
	Else
		Scatter Name oMP Blank
	EndIf

	With This
		.WNMovOC = oMol.Cuantos
		.WNMovMP = oMP.Cuantos
		lStado = ._calcularbrecha(oMol.F16cCodUbi)
	EndWith

	If lStado
		*> Ha encontrado ubicaci�n (se asigna a This.CodUbi).
		Exit
	EndIf

	Select _F16cCURMOL
	Skip
EndDo

*> Validar los MPs asignados, que no se hayan validado antes.
Select _F14cCURMOL
Delete All For IsNull(Cuantos)
Go Top

Do While !Eof() And Empty(This.CodUbi)
	Scatter Name oMP

	With This
		.WNMovOC = 0
		.WNMovMP = oMP.Cuantos
		lStado = ._calcularbrecha(oMP.F14cUbiOri)
	EndWith

	If lStado
		*> Ha encontrado ubicaci�n (se asigna a This.CodUbi).
		Exit
	EndIf

	Select _F14cCURMOL
	Skip
EndDo

Use In (Select ("_F16cCURMOL"))
Use In (Select ("_F14cCURMOL"))
Return lStado

ENDPROC
PROCEDURE _prcmolnew

*> Buscar ubicaci�n nueva para ubicar en molada.
*> M�todo privado de la clase.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.NumRem, n� de remontes.
*>	- This.CoeRot, buscar por coeficiente de rotaci�n.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamdo desde:
*>	This._prcmol(), ubicaci�n en moladas.

Local lStado
Private cWhere

This.UsrError = ""
lStado = .F.

*> Ubicar en moladas por coeficiente de rotaci�n.
If This.CoeRot=="S"
	lStado = This._prcmolrot()
EndIf

If !lStado
	*> Ubicar en moladas de forma ca�tica.
	lStado = This._prcmolubi()
EndIf

Return lStado

ENDPROC
PROCEDURE _calcularbrecha

*> Buscar ubicaci�n en moladas que ya contengan el producto.
*> M�todo privado de la clase.

*> Recibe:
*>	cUbicacion, ubicaci�n a validar.
*>	- This.WNMovOC, n� de palets ya existentes.
*>	- This.WNMovMP, n� de MPs asignados a la ubicaci�n.
	
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CodUbi, ubicaci�n OK.
*>	- This.UsrError, texto errores.

*> Llamdo desde:
*>	This._prcmol(), ubicaci�n en moladas.

Parameters cUbicacion

Local lStado, nPaletsQueCaben
Private cWhere, oF10c, oF11r

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "Ubicaci�n inexistente"
	Return .F.
EndIf

Select F10c
Scatter Name oF10c

*> Obtener la parametrizaci�n de palets por tama�o / tipo.
cWhere = "F11rTamUbi='" + oF10c.F10cTamUbi + "' And F11rTamPal='" + This.TipPal + "'"
lStado = f3_sql("*", "F11r", cWhere, , , "F11RCURMOL")
If !lStado
	If _xier<=0
		This.UsrError = "Error carga par�metros tama�o / tipo"
		Use In (Select ("F11RCURMOL"))
		Return lStado
	EndIf

	Select F11RCURMOL
	Scatter Name oF11r Blank
Else
	Select F11RCURMOL
	Go Top
	Scatter Name oF11r
EndIf

nPaletsQueCaben = oF11r.F11rNumPal * Iif(This.NumRem > 0, This.NumRem, 1) - (This.WNMovOC + This.WNMovMP)
lStado = (nPaletsQueCaben > 0)

If lStado
	With This
		.CodUbi = cUbicacion
		.CanUbi = This.Cantidad
	EndWith
EndIf

Use In (Select ("F11RCURMOL"))
Return lStado

ENDPROC
PROCEDURE _prcmolrot

*> Buscar ubicaci�n de moladas por coeficiente de rotaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cOrder
Local lStado

This.UsrError = ""

*> Inicializar el rango de rotaci�n del art�culo.
cWhere = "F11cCoeFrt>=" + Str(This.CoeFrt)
cOrder = "F11cCoeFrt"

lStado = f3_sql("*", "F11c", cWhere, cOrder, , "F11cCur")
If lStado
	Select F11cCur
	Go Top
	This.WRotIni = F11cPriIni
	This.WRotFin = F11cPriFin
EndIf

*> Ubicar por coeficiente de rotaci�n a ubicaciones de moladas y/o por rangos de ubicaci�n (F08U).
lStado = This._prcmolrotrng()

Use In (Select("F11cCur"))
Return lStado

ENDPROC
PROCEDURE _prcmolrotrng

*> Buscar ubicaci�n de molada por coeficiente de rotaci�n en zona general asignada al producto.
*> Si no hay rangos (F08U), se ubica en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcrot.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.

*> Devue|ve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, cCurUbi
Local lStado, oF08u, oF10c

This.UsrError = ""

cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uCRot')
If !lStado
	*> No hay rangos: Ubicar en moladas por coeficiente de rotaci�n en todo el almac�n.
	Use In (Select ("F08uCRot"))
	lStado = This._prcmolrotgen()
	Return lStado
EndIf

Select F08uCRot
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF08u

	lStado = This._prcmolrotgen(oF08u.F08uUbiDsd, oF08u.F08uUbiHst)

	If lStado
		Exit
	EndIf

	*> Procesar el siguiente rango de ubicaciones.
	Select F08uCRot
	Skip
EndDo

Use In (Select ("F08uCRot"))
Return lStado

ENDPROC
PROCEDURE _prcmolrotgen

*> Buscar ubicaci�n de moladas por coeficiente de rotaci�n en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcmolrotrng.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbiDesde, ubicaci�n inicial, si rangos.
*>	- cUbiHasta, ubicaci�n final, si rangos.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbiDesde, cUbiHasta
Private cWhere, cFromF, cOrder, cCurUbi
Local lStado, oF10c

This.UsrError = ""

*> Valores por defecto de los l�mites.
If Type('cUbiDesde')<>'C'
	cUbiDesde = _Alma + Space(10)
EndIf

If Type('cUbiHasta')<>'C'
	cUbiHasta = _Alma + Replicate("z", 10)
EndIf

*> Cargar ubicaciones de este rango y de la prioridad seg�n coeficiente de rotaci�n del producto.
cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
cWhere = cWhere + "F10cCodUbi Between '" + cUbiDesde + "' And '" + cUbiHasta + "' And "
cWhere = cWhere + "F10cPriori Between " + Str(This.WRotIni) + " And " + Str(This.WRotFin) + " And "
cWhere = cWhere + "F10cTipAlm='" + This.TipAlm + "' And "
cWhere = cWhere + "F10cPickSN<>'E' And F10cPickSN<>'L' And F10cEstGen<>'B' And F10cEstEnt='N' And F10cTamUbi>='" + This.TamAbi + "'"

cOrder = "F10cPriori, F10cCodUbi"
cFromF = "F10c"

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F10cPPri")

Select F10cPPri
Go Top
Do While !Eof()
	Scatter Name oF10c

	cCurUbi = oF10c.F10cCodUbi
	lStado = This._prcmolrotcnf(cCurUbi)					&& Validar la ubicaci�n obtenida.

	If lStado
		*> Ha encontrado ubicaci�n (se asigna a This.CodUbi).
		Exit
	EndIf

	*> Carga la siguiente ubicaci�n del rango activo.
	Select F10cPPri
	Skip
EndDo

Use In (Select ("F10cPPri"))
Return lStado

ENDPROC
PROCEDURE _prcmolrotcnf

*> Validar la ubicaci�n de molada encontrada en ubicaci�n moladas por coeficiente de rotaci�n.
*> M�todo privado de la clase. Llamado desde los m�todos que se encargan de buscar ubicaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbicacion, ubicaci�n candidata a validar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cCodUbi
Local lStado

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "La ubicaci�n no existe"
	Return .F.
EndIf

Select F10c
cCodUbi = F10cCodUbi

*> Validar tipo almacenaje.
If F10cTipAlm<>This.TipAlm
	This.UsrError = "Tipo almacenaje distinto"
	Return .F.
EndIf

*> Validar tipo ubicaci�n.
If F10cPickSn=='E' Or F10cPickSn=='L'
	This.UsrError = "La ubicaci�n no es v�lida"
	Return .F.
EndIf

*> Validar estado bloqueo.
If F10cEstGen=="B" .Or. F10cEstEnt=="S"
	This.UsrError = "La ubicaci�n est� bloqueada"
	Return .F.
EndIf

*> Validar tipo de producto.
If !Empty(F10cTipPro) .And. F10cTipPro<>This.TipPro
	*> Ver si pertenece a alguno de los tipos alternativos.
	lStado = This.VerTipPro(F10cTipPro)
	If !lStado
		This.UsrError = "Tipo de producto diferente"
		Return lStado
	EndIf
EndIf

With This
	.WNMovOC = 0
	.WNMovMP = 0
	If !._calcularbrecha(cCodUbi)
		.UsrError = "Ubicaci�n ocupada por otro producto"
		Return .F.
	EndIf
EndWith

Return

ENDPROC
PROCEDURE _prcmolubi

*> Buscar ubicaci�n de moladas de forma ca�tica.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""

*> Ubicar en zonas asignadas a situaciones de stock.
lStado = This._prcmolubistk()
If !lStado .And. (This.WHayStk==.F. .Or. This.ForZon=="S")
	*> Ubicar en rangos asignados al producto.
	lStado = This._prcmolubirng()

	If !lStado .And. (This.WHayRng==.F. .Or. This.ForZon=="S")
		*> Ubicar ca�ticamente en todo el almac�n.
		lStado = This._prcmolubigen()
	EndIf
EndIf

Return lStado

ENDPROC
PROCEDURE _prcmolubistk

*> Buscar ubicaci�n de moladas en zonas asignadas a situaciones de stock.
*> M�todo privado de la clase. Llamado desde This._prcubi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, oF10s
Local lStado

This.UsrError = ""
This.WHayStk = .F.

*> Ubicar en zonas asignadas a situaciones de stock.
cWhere = "F10sCodPro='" + This.CodPro + "' And F10sCodAlm='" + _Alma + "' And F10sSitStk='" + This.SitStk + "'"
cFromF = "F10s"
cOrder = "F10sCodPro, F10sCodAlm, F10sSitStk, F10sCodZon, F10sUbiDsd"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F10sUSTK')
If !lStado
	*> No hay SSTKs asignadas a zona.
	Use In (Select("F10sUSTK"))
	Return lStado
EndIf

Select F10sUSTK
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF10s

	lStado = This._prcmolubigen(oF10s.F10sUbiDsd, oF10s.F10sUbiHst)

	If lStado
		Exit
	EndIf

	Select F10sUSTK
	Skip
EndDo

This.WHayStk = .T.
Use In (Select("F10sUSTK"))
Return lStado

ENDPROC
PROCEDURE _prcmolubirng

*> Buscar ubicaci�n de moladas en rangos de ubicaci�n asignados al producto.
*> M�todo privado de la clase.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, oF08u
Local lStado

This.UsrError = ""
This.WHayRng = .F.

*> Ubicar en rangos asignados al producto.
cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uURNG')
If !lStado
	*> No hay rangos.
	Use In (Select ("F08uURNG"))
	Return lStado
EndIf

Select F08uURNG
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF08u

	lStado = This._prcmolubigen(oF08u.F08uUbiDsd, oF08u.F08uUbiHst)

	If lStado
		Exit
	EndIf

	Select F08uURNG
	Skip
EndDo

This.WHayRng = .T.
Use In (Select ("F08uURNG"))
Return lStado

ENDPROC
PROCEDURE _prcmolubigen

*> Buscar ubicaci�n moladas ca�tica en todo el almac�n.
*> M�todo privado de la clase.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbiDesde, ubicaci�n inicial, si rangos.
*>	- cUbiHasta, ubicaci�n final, si rangos.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbiDesde, cUbiHasta
Private cWhere, cWhereTam, cFromF, cOrder, cCurUbi
Local lStado, oF10c, oF11t

This.UsrError = ""

*> Valores por defecto de los l�mites.
If Type('cUbiDesde')<>'C'
	cUbiDesde = _Alma + Space(10)
EndIf
If Type('cUbiHasta')<>'C'
	cUbiHasta = _Alma + Replicate("z", 10)
EndIf

*> Generar cursor con los tipos de producto alternativos, si hay.
=CrtCursor("F11t", "F11tTPro")

*> Cargar resto de tipos de producto alternativos.
cWhere = "F11tTipPro='" + This.TipPro + "'"
cOrder = "F11tPriori"

lStado = f3_sql("*", "F11t", cWhere, cOrder, , "+F11tTPro")

*> Agregar el tipo de producto del art�culo.
Select F11tTPro
Append Blank
Replace F11tTipPro With This.TipPro, ;
		F11tPriori With 0

*> Condiciones de filtro general para las ubicaciones.
cOrder = "F10cCodUbi, F10cTamUbi"
cFromF = "F10c"

*> Filtro por tama�o de ubicaci�n.
cWhereUbi = ""
If This.ForTam<>"S"
	cWhereUbi = " And F10cTamUbi='" + This.TamAbi + "'"
Else
	cWhereUbi = " And F10cTamUbi>='" + This.TamAbi + "'"
EndIf

Select F11tTPro
Go Top
Do While !Eof()
	Scatter Name oF11t

	*> Cargar ubicaciones del rango, tama�o palet y tipo producto adecuados.
	cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
	cWhere = cWhere + "F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cCodUbi Between '" + cUbiDesde + "' And '" + cUbiHasta + "' And "
	cWhere = cWhere + "F10cTipPro='" + oF11t.F11tTipDes + "' And "
	cWhere = cWhere + "F10cPickSN<>'E' And F10cPickSN<>'L' And F10cEstGen<>'B' And F10cEstEnt='N'"

	*> Agregar condiciones generales de filtro.
	cWhere = cWhere + cWhereUbi

	lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F10cUGen")

	Select F10cUGen
	Go Top
	Do While !Eof()
		Scatter Name oF10c

		cCurUbi = oF10c.F10cCodUbi
		lStado = This._prcmolubicnf(cCurUbi)					&& Validar la ubicaci�n obtenida.

		If lStado
			*> Ubicaci�n OK.
			Exit
		EndIf

		*> Carga la siguiente ubicaci�n del rango activo.
		Select F10cUGen
		Skip
	EndDo

	If lStado
		Exit
	EndIf

	*> Probar con el siguiente tama�o alternativo.
	Select F11tTPro
	Skip
EndDo

Use In (Select ("F11tTPro"))
Use In (Select ("F10cUGen"))
Return lStado

ENDPROC
PROCEDURE _prcmolubicnf

*> Validar la ubicaci�n encontrada en ubicaci�n ca�tica.
*> M�todo privado de la clase. Llamado desde los m�todos que se encargan de buscar ubicaci�n.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbicacion, ubicaci�n candidata a validar.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion

Private cWhere, cCodUbi
Local lStado

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "La ubicaci�n no existe"
	Return .F.
EndIf

Select F10c
cCodUbi = F10cCodUbi

*> Validar tipo ubicaci�n.
If F10cPickSn=='E' Or F10cPickSn=='L'
	This.UsrError = "La ubicaci�n no es v�lida"
	Return .F.
EndIf

*> Validar estado bloqueo.
If F10cEstGen=="B" .Or. F10cEstEnt=="S"
	This.UsrError = "La ubicaci�n est� bloqueada"
	Return .F.
EndIf

*> Validar tipo de producto.
If !Empty(F10cTipPro) .And. F10cTipPro<>This.TipPro
	*> Ver si pertenece a alguno de los tipos alternativos.
	lStado = This.VerTipPro(F10cTipPro)
	If !lStado
		This.UsrError = "Tipo de producto diferente"
		Return lStado
	EndIf
EndIf

With This
	.WNMovOC = 0
	.WNMovMP = 0
	If !._calcularbrecha(cCodUbi)
		.UsrError = "Ubicaci�n ocupada por otro producto"
		Return .F.
	EndIf
EndWith

Return

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:
*> 09.01.2008 (AVC) Calcular estado l�nea albar�n seg�n la cantidad recontada, en lugar de la cantidad albar�n.
*>					Modificado m�todo This.UpdateAlbLin
*> 27.03.2008 (AVC) Tener en cuenta productos sin peso / volumen para calcular peso y volumen palets.
*>					Modificado m�todo This.CalcPesVolPal
*>					Modificado m�todo This._calcpesvolpal
*>					Modificado m�todo This._pesvolar
*> 08.07.2008 (AVC) Recalcular ficha peso palet (F16t) solo si no existe.
*>					Modificado m�todo This._calcpesvolpal

ENDPROC
PROCEDURE crossdocking
Parameters cNumEnt,cCodPro,cTipDoc,cNumDoc,cCodArt,cPOLDoc
*>Buscar si tiene documento de CrossDocking Asociado.

lStado = .T.

*>Buscar Palet.
Use In (Select("verPal"))

_Sentencia="Select * From F18l001 Where F14cTipMov='1000' And F14cNumPal='" + This.NumPal + "'"

_Ok =SqlExec(_Asql,_Sentencia,'verPal')

If _ok < 0
	lStado=.F.
	Return lStado
EndIf


Select verPal
Go Top





Return lStado

ENDPROC
PROCEDURE calcular_cajas
Parameters _CodPro,_CodArt,_SitStk,_TipPro,_TipBul,_Cajas,_Cantidad,_CodUbiI,_CodUbiF

*> Condiciones de filtro general para las ubicaciones.
cOrder = "F11xCanBul,F10cCodUbi, F10cTamUbi"
cFromF = "F10c,F11x"

*>Calculamos sobre ubicaciones de cajas que esten vacias, intentaremos rellenar aprovechando tama�os de ubicacion y bulto.
*> Cargar ubicaciones del rango, tama�o palet y tipo producto adecuados.
cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
cWhere = cWhere + "F10cTipAlm='" + This.TipAlm + "' And "

If Type('_CodUbiF')<>'L'
	cWhere = cWhere + "F10cCodUbi Between '" + cCodUbiI + "' And '" + cCodUbiF + "' And "
Else
	cWhere = cWhere + "F10cCodUbi = '" + cCodUbiI + "' And "
EndIf		

cWhere = cWhere + "F10cTamUbi= F11xTamUbi And F11xTamBul='"+ _TipBul + "' And "
cWhere = cWhere + "F10cTipPro='" + _TipPro + "' And "
cWhere = cWhere + "F10cPickSN='S' And F10cEstGen<>'B' And F10cEstEnt='N' And F10cCodUbi Not In (Select F16cCodUbi From F16c001)"

*> Agregar condiciones generales de filtro.
cWhere = cWhere + cWhereUbi

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F10cUGen")


*>Intentamos ubicar en el menor numero de movimientos.
Select F10cUGen
Go Top
Locate For F10cUGen.F11xCanBul==_Cajas
If Found()
	This.CodUbi = F10cUGen.F10cCodUbi
	Return .T.
EndIf
	
Select F10cUGen
Go Top
Locate For F10cUGen.F11xCanBul > _Cajas
If Found()
	This.CodUbi = F10cUGen.F10cCodUbi
	Return .T.
EndIf


Return .F.
	


ENDPROC
PROCEDURE calcular_ubicacion
Parameters _CodPro,_CodArt,_CodUbi,_SitStk,_TipPro,_TipBul,_Cajas,_Cantidad


*>Primero por Situaci�n de Stock
If This._Prc_Calcular_SitStk()
	Return .T.
EndIf

*>Relleno de Picking
If This._Prc_Calcular_Picking()
	Return .T.
EndIf

*>Caotico.
If This._Prc_Calcular_Caotico()
	Return .T.
EndIf




Return .F.


















ENDPROC
PROCEDURE _prc_calcular_sitstk

*> Buscar ubicaci�n en carga en zonas asignadas a situaciones de stock.
*> M�todo privado de la clase. Llamado desde This._prcubi.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
	
*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder, oF10s
Local lStado

This.UsrError = ""
This.WHayStk = .F.

*> Ubicar en zonas asignadas a situaciones de stock.
cWhere = "F10sCodPro='" + This.CodPro + "' And F10sCodAlm='" + _Alma + "' And F10sSitStk='" + This.SitStk + "'"
cFromF = "F10s"
cOrder = "F10sCodPro, F10sCodAlm, F10sSitStk, F10sCodZon, F10sUbiDsd"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F10sUSTK')
If !lStado
	*> No hay SSTKs asignadas a zona.
	Use In (Select("F10sUSTK"))
	Return lStado
EndIf

Select F10sUSTK
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF10s

	lStado = This._prc_calcular_ubigen_(oF10s.F10sUbiDsd, oF10s.F10sUbiHst)

	If lStado
		Exit
	EndIf

	Select F10sUSTK
	Skip
EndDo

This.WHayStk = .T.
Use In (Select("F10sUSTK"))
Return lStado

ENDPROC
PROCEDURE _prc_calcular_ubigen

*> Buscar ubicaci�n ca�tica en todo el almac�n.
*> M�todo privado de la clase. Llamado desde This._prcubistk y This._prcubirng.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- cUbiDesde, ubicaci�n inicial, si rangos.
*>	- cUbiHasta, ubicaci�n final, si rangos.
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbiDesde, cUbiHasta
Private cWhere, cWhereTam, cFromF, cOrder, cCurUbi
Local lStado, oF10c, oF11t

This.UsrError = ""

*> Valores por defecto de los l�mites.
If Type('cUbiDesde')<>'C'
	cUbiDesde = _Alma + Space(10)
EndIf
If Type('cUbiHasta')<>'C'
	cUbiHasta = _Alma + Replicate("z", 10)
EndIf

*> Generar cursor con los tipos de producto alternativos, si hay.
=CrtCursor("F11t", "F11tTPro")

*> Cargar resto de tipos de producto alternativos.
cWhere = "F11tTipPro='" + This.TipPro + "'"
cOrder = "F11tPriori"

lStado = f3_sql("*", "F11t", cWhere, cOrder, , "+F11tTPro")

*> Agregar el tipo de producto del art�culo.
Select F11tTPro
Append Blank
Replace F11tTipPro With This.TipPro, ;
		F11tPriori With 0

*> Condiciones de filtro general para las ubicaciones.
cOrder = "F10cCodUbi, F10cTamUbi"
cFromF = "F10c"

*> Filtro por tama�o de ubicaci�n.
cWhereUbi = ""
If This.ForTam<>"S"
	cWhereUbi = " And F10cTamUbi='" + This.TamAbi + "'"
Else
	cWhereUbi = " And F10cTamUbi>='" + This.TamAbi + "'"
EndIf

Select F11tTPro
Go Top
Do While !Eof()
	Scatter Name oF11t

	*> Cargar ubicaciones del rango, tama�o palet y tipo producto adecuados.
	cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
	cWhere = cWhere + "F10cTipAlm='" + This.TipAlm + "' And "
	cWhere = cWhere + "F10cCodUbi Between '" + cUbiDesde + "' And '" + cUbiHasta + "' And "
	cWhere = cWhere + "F10cTipPro='" + oF11t.F11tTipDes + "' And "
	cWhere = cWhere + "F10cPickSN='N' And F10cEstGen<>'B' And F10cEstEnt='N'"

	*> Agregar condiciones generales de filtro.
	cWhere = cWhere + cWhereUbi

	lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F10cUGen")

	Select F10cUGen
	Go Top
	Do While !Eof()
		Scatter Name oF10c

		cCurUbi = oF10c.F10cCodUbi
		lStado = This._prcubicnf(cCurUbi)					&& Validar la ubicaci�n obtenida.

		If lStado
			*> Ubicaci�n OK.
			lStado =This._Prc_Calculo_Cajas(cCurUbi,.T.)
			If lStado
				With This
					.CodUbi = oF10c.F10cCodUbi
					.CanUbi = This.Cantidad
				EndWith
				lStado = .T.
				Exit
			EndIf
		EndIf

		*> Carga la siguiente ubicaci�n del rango activo.
		Select F10cUGen
		Skip
	EndDo

	If lStado
		Exit
	EndIf

	*> Probar con el siguiente tama�o alternativo.
	Select F11tTPro
	Skip
EndDo

Use In (Select ("F11tTPro"))
Use In (Select ("F10cUGen"))
Return lStado

ENDPROC
PROCEDURE _prc_calculo_cajas
LParameters _CodUbi, _Relleno
Private _Existe

lStado = .T.
_Ubicados = 0
_Existe = .F.

m.F10cCodUbi = _CodUbi
If !F3_Seek('F10c')
	lStado = .F.
	Return lStado
EndIf

Select F10c
Go Top
If F10cPickSn<>'S'
	lStado = .F.
	Return lStado
EndIf

m.F08cCodPro = _Procaot
m.F08cCodArt = This.CodArt
If !F3_Seek('F08c')
	lStado = .F.
	Return lStado
EndIf

Select F08c
Go Top

Use In (Select("cTamBul"))

_Sentencia="Select * From F11x001 Where F11xTamUbi='" + F10c.F10cTamUbi + "' And F11xTamBul='" + F08c.F08cUltLot + "'"
=SqlExec(_Asql,_Sentencia,'cTamBul')

Select cTamBul
Go Top
If Eof()
	*>Si no hay relaci�n activa, que ubique normalmente.
	lStado = .F.
	Return lStado
EndIf



*>Buscar en ubicaciones de picking libres, ordenar de mayor a menor.
*>Buscar si la ubicaci�n y calcular.
_Sentencia="Select F16cCodPro,F16cCodArt,F16cSitStk,F16cCodUbi,F16cUniPac,F16cPacCaj,F16cNumAna,F00eTipPro, " + ;
		   "F10cTamUbi,F10cPickSn,Count(Distinct(F16cCodArt)) As F16cCodArt,Sum(F16cCanFis-F16cCanRes) As F16cCanFis " + ;
		   "From F16c001,F10c001,F00e001,F08c001 Where " + ;
		   "F16cCodUbi='" + _CodUbi + "' And F10cCodUbi=F16cCodUbi And " + ;
		   "F08cCodPro=F16cCodPro And F08cCodArt=F16cCodArt And " + ;
		   "F00eTipPro = F08cTipPro " + ;
		   "Group By F16cCodPro,F16cCodArt,F16cSitStk,F16cCodUbi,F16cUniPac,F16cPacCaj,F16cNumAna,F00eTipPro,F10cTamUbi,F10cPickSn "


		

		
_Ok=SqlExec(_Asql,_Sentencia,'cCurCajas')

If _Ok < 0
	*>Error en SQL
	Return .F.
EndIf

Select cCurCajas
Go Top
If Eof()
	*>Esta vacia,calcular si cabe todo.
	*>Mirar si es del mismo tipo de producto.
	Select cCurCajas
	Go Top
	If !Eof()
		Locate For AllTrim(F00eTipPro)<>AllTrim(F08c.F08cTipPro)
		If Found()
			lStado = .F.
			Return lStado
		EndIf
	EndIf
	
	*>Ver si caben los bultos.
	If cTamBul.F11xCanBul < This.Cantidad / (F08c.F08cUniPac / F08c.F08cPacCaj)
		lStado = .F.
		Return lStado
	EndIf
Else
	*>Mirar si el tama�o de bulto es distinto.
	Select cCurCajas
	Go Top
	If !Eof()
		Locate For AllTrim(F16cNumAna)<>AllTrim(F08c.F08cUltLot) .And. !Empty(F16cNumAna)
		If Found()
			lStado = .F.
			Return lStado
		EndIf
	EndIf
	
	*>Mirar si es del mismo tipo de producto.
	Select cCurCajas
	Go Top
	If !Eof()
		Locate For AllTrim(F00eTipPro)<>AllTrim(F08c.F08cTipPro)
		If Found()
			lStado = .F.
			Return lStado
		EndIf
	EndIf

	*>Veamos si existe el articulo
	Select cCurCajas
	Go Top
	Locate For F16cCodArt = This.CodArt
	If Found()
		_Existe = .T.
		_Ubicados = F16cCanFis / (F16cUniPac * F16cPacCaj)
	EndIf

	_Cajas = cTamBul.F11xCanBul / cTamBul.F11xCanLin
	
	Select cCurCajas
	Go Top
	_Articulos = RecCount()
	
	If _Articulos >= cTamBul.F11xCanLin
		*>Todos los frontales estan cubiertos.
		*>Veamos si podemos rellenar con el mismo articulo
		If _Existe = .T.
			*>Controlamos que de nuestro articulo quepa todo.
			*>Cajas que caben por lineal
			_Cajas = cTamBul.F11xCanBul / cTamBul.F11xCanLin
			
			If (_Cajas - _Ubicados) < This.Cantidad / (F08c.F08cUniPac / F08c.F08cPacCaj)
				*>Si el total de cajas por frontal es mayor que las cajas ubicadas, podemos rellenar la ubicaci�n.
				lStado = .F.
				Return lStado
			Else
				*>Si nos cabe todo.
				lStado = .T.
				Return lStado
			EndIf
		
		Else
			*>Devolvemos que no es una ubicaci�n valida.
			*>No quedan frontales libres y tampoco hay nada del mismo articulo.
			lStado = .F.
			Return lStado
		EndIf
	EndIf
	
	*>Se puede abrir un frontal.
	*>Calculamos el total que cabe, teniendo en cuenta nuestro articulo y los frontales vacios.
	_Frontales = (cTamBul.F11xCanLin - _Articulos) * (_Cajas + (_Cajas - _Ubicados))
	
	If _Frontales < This.Cantidad / (F08c.F08cUniPac / F08c.F08cPacCaj)
		lStado = .F.
		Return lStado
	EndIf

EndIf

Return lStado






ENDPROC
PROCEDURE _prc_calcular_picking

*> Buscar ubicaci�n de picking asignada al producto.
*> M�todo privado de la clase. Llamado desde This._prcpic.

*> Adem�s de los par�metros aqu� mencionados, trabaja con:
*>	- Par�metros generales (This.CargaPrm).
*>	- Par�metros de art�culo (This.CargaPrm).

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.Cantidad, cantidad a ubicar.
*>	- This.UbicParcial, ubicar una parte.

*> Devuelve:
*>	- This.CodUbi, ubicaci�n encontrada.
*>	- This.CanUbi, cantidad ubicada.
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Private cWhere, cFromF, cOrder
Local lStado, nCantidadPck, nCantidad, oF12c

This.UsrError = ""

nCantidadPck = 0				&& Cantidad ya acumulada en picking (MPs + OCs).

cWhere = "F12eCodPro='" + This.CodPro + "' And " + ;
         "F12eCodArt='" + This.CodArt + "' And " + ;
         "F10cCodUbi=F12eCodUbi"

cFromF = 'F12e,F10c'
cOrder = "F12eCodPro, F12eCodArt"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F12cPPic')

Select F12cPPic
Go Top
Do While !Eof()
	lStado = .F.
	Scatter Name oF12c
	
		lStado =This._prc_calculo_cajas(oF12c.F12eCodUbi,.T.)

*!*		nCantidadPck = This.CanPickingMP(oF12c.F12cCodUbi) + This.CanPickingOC(oF12c.F12cCodUbi)
*!*		If nCantidadPck < oF12c.F12cCanMax
*!*			*> Cantidad m�xima a ubicar.
*!*			nCantidad = This.RoundCantidad(This.Cantidad, (oF12c.F12cCanMax - nCantidadPck), oF12c.F10cPickSN)

*!*			If nCantidad <= 0 .Or. (nCantidad < This.Cantidad .And. This.UbicParcial<>"S")
*!*				*> Descartar ubicaci�n porque no se permite ubicar parcialmente.
*!*				Select F12cPPic
*!*				Skip
*!*				Loop
*!*			EndIf
		If lStado
			*> Ubicaci�n OK.
			With This
				.CodUbi = oF12c.F12eCodUbi
				*.CanUbi = nCantidad
			EndWith
			Exit
		Endif
*!*		EndIf

	Select F12cPPic
	Skip
EndDo

If Empty(This.CodUbi)
	*>No hace falta que sea del mismo articulo.
	*>Si el parametro relleno de picking es si.
	*>Ordenamos todo por articulo.
	_Sentencia="Select Distinct F16cCodUbi From F16c001,F10c001 Where F16cCodArt='" + This.CodArt + "' And F10cPickSn='S' And F10cCodUbi = F16cCodUbi And F10cEstEnt<>'S' And F10cTipPro='" + This.TipPro + "'"
	=Sqlexec(_Asql,_Sentencia,'cPalla')
	Select cPalla
	Go Top
	Do While !Eof()
		lStado =This._prc_calculo_cajas(cPalla.F16cCodUbi,.T.)
		If lStado
			This.CodUbi = cPalla.F16cCodUbi
			Exit
		EndIf
		Select cPalla
		Skip	
	EndDo
	
	If Empty(This.CodUbi)
		*>Ordenamos todo por articulo.
		_Sentencia="Select Distinct F16cCodUbi From F16c001,F10c001 Where F16cCodArt<>'" + This.CodArt + "' And F10cPickSn='S' And F10cCodUbi = F16cCodUbi And F10cEstEnt<>'S' And F10cTipPro='" + This.TipPro + "'"
		=Sqlexec(_Asql,_Sentencia,'cPalla')
		Select cPalla
		Go Top
		Do While !Eof()
			lStado =This._prc_calculo_cajas(cPalla.F16cCodUbi,.T.)
			If lStado
				This.CodUbi = cPalla.F16cCodUbi
				Exit
			EndIf
			Select cPalla
			Skip	
		EndDo
	EndIf
EndIf

Use In (Select ("F12cPPic"))
Return lStado

ENDPROC
PROCEDURE _prc_calcular_caotico
lStado = .F.

m.F08cCodPro= _Procaot
m.F08cCodArt= This.CodArt

If !f3_Seek('F08c')
	Return .F.
EndIf

Select F08c
Go Top
cEntrar = This.Cantidad /(F08c.F08cUniPac * F08c.F08cPacCaj)

*>Buscar la ubicaci�n de forma calculada.
_Sentencia="Select * From F10c001,F11x001 Where F11xTamUbi= F10cTamUbi And F11xTamBul ='" + F08c.F08cUltLot + "' And F10cPickSn='S' And F10cEstEnt<>'S' " + ;
		   " And F10cTipPro='" + F08c.F08cTipPro + "' And F10cCodUbi Not In (Select F16cCodUbi From F16c001) Order By F11xCanBul"

=SqlExec(_Asql,_Sentencia,'cVamosTodos')

Select cVamosTodos
Go Top
Locate For F11xCanBul>=cEntrar
Do While Found()
	*>Todo el frontal
	lStado=This._Prc_Calculo_Cajas(cVamosTodos.F10cCodUbi,.T.)
	If lStado
		This.CodUbi = cVamosTodos.F10cCodUbi
		lStado = .T.
		Exit
	EndIf
	
	Select cVamosTodos
	Continue
EndDo

*>Mp




Return lStado
ENDPROC
PROCEDURE prc_cd

*> Proceso de ubicaci�n de material de Cross Docking.
*> M�todo privado de la clase. Llamado desde This._Ubicar.

*> Recibe:
*>	- This.CodPro, Propietario.
*>	- This.TDocCD, Tipo documento Cross Docking.
*>	- This.NDocCD, N�mero documento Cross Docking.
*>	- _RADIO, carpeta de trabajo de RF. Se toma de MSI2.INI

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificacones:
*> 15.09.2008 (AVC) Tomar carpeta de RF del MSI2, no 'a pelo'.

Local lStado, cMuelleCD, oApi, cRadioIni
Private cWhere

This.UsrError = ""

*> Recupero el muelle de Cross Docking por defecto.
oApi = CreateObject('FoxApi')

*> -------------------------------------------------------------------------------------------------------
cRadioIni = Sys(5) + CurDir() + "RADIO.INI"
If !File(cRadioIni)
	cRadioIni = AddBs(_RADIO) + "RADIO.INI"
	If !File(cRadioIni)
		This.UsrError = "no se encuentra el archivo de configuraci�n RF (RADIO.INI)"
		Return .F.
	EndIf
EndIf

cMuelleCD = oApi.GetKeyIni("_MUELLE_CD", "COMMON", cRadioIni)			&& Muelle Cross Docking.

*> -------------------------------------------------------------------------------------------------------

If !Empty(cMuelleCD)													&& Validar la existencia del muelle de CD gen�rico
	m.F10cCodUbi = cMuelleCD
	If !f3_seek("F10c")
		cMuelleCD = ''
		*This.UsrError = "Muelle de Cross Docking " + cMuelleCD + " no definido"
		*Return .F.
	EndIf
EndIf

If Empty(cMuelleCD)														&& Validar la existencia del muelle de CD gen�rico
	cWhere = "F10cPickSN='C'"
	If f3_sql("*", "F10c", cWhere, , , "F10cCur")
		Select F10cCur
		Go Top
		cMuelleCD = IIf(!Eof(), F10cCodUbi, Space(14))
		Use In (Select("F10cCur"))
	EndIf
	
	If Empty(cMuelleCD)
		Use In (Select("F10cCur"))
		This.UsrError = "No hay ning�n muelle de Cross Docking"
		Return .F.
	EndIf
EndIf

*> Verifico si hay material preparado del pedido en alg�n muelle.
Use In (Select("F14cCur"))

cWhere =                "F14cCodPro='" + This.CodPro + "'"
cWhere =  cWhere + " And F14cTipDoc='" + This.TDocCD + "'"
cWhere =  cWhere + " And F14cNumDoc='" + This.NDocCD + "'"
=f3_sql("*", "F14c", cWhere, , , "F14cCur")

Select F14cCur
Locate For F14cTipMov='2999'
This.CodUbi = IIf(Found(), F14cUbiOri, cMuelleCD)

Use In (Select("F14cCur"))

Return

ENDPROC


EndDefine 
