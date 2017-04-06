Define Class inventario as custom



PROCEDURE generarinventario

*> Generar la lista de inventario si no existe.
*> Función que hasta ahora solo hacía LINVLIST de PROCAOT.

*> Recibe:
*>	- cNumInv, ó bien This.LisInv, nº de lista de inventario.

*> Devuelve:
*>	- Estado, resultado de la operación (.T. / .F.).
*>	- This.UsrError, texto error.

*> Historial de modificaciones:
*> 03.01.2008 (AVC) Permitir inventariar cualquier situación de stock.
*>					Grabar cantidad recuento a cero, para forzar a leer todos los artículos.
*> 16.09.2009 (AVC) Generar listas de inventario con rangos de artículos.
*> 21.09.2009 (AVC) Sustituir rango artículos por selección individual de artículos en inventario, tabla F50R.
*> 22.09.2009 (AVC) Cancelar inventario si, al generar lista, no hay ubicaciones.

Parameters cNumInv

Local oF50c, oF10c, oF50l, oF50r, oF16c, lStado, cCurUbi
Private cNumeroInventario, cWhere, cWhere1, cWhereArt, cField, cFromF

This.UsrError = ""

*> Asignar parámetros.
cNumeroInventario = Iif(Type('cNumInv')=='C', cNumInv, This.LisInv)

If Empty(cNumeroInventario)
	This.UsrError = "Nº inventario en blanco"
	Return .F.
EndIf

If Type('cNumeroInventario')<>'C'
	This.UsrError = "Tipo lista no válido"
	Return .F.
EndIf

*> Guardar como propiedad.
This.LisInv = cNumeroInventario

*> Para selección de artículos.
cWhereArt = ""

*!*	Anulado. Lo toma de la propia lista.
*!*	If Empty(This.CodOpe) .Or. Type('This.CodOpe')<>'C'
*!*		This.UsrError = "Operario no definido"
*!*		Return .F.
*!*	EndIf

Use In (Select ('F10cCUR'))
Use In (Select ('F50cCUR'))
Use In (Select ('F50clCUR'))
Use In (Select ('F50lCUR'))
Use In (Select ('LISTAS'))
Use In (Select ('F16cCUR'))
Use In (Select ('F50RGENL'))

*> Comprobar si ya existe la lista.
cWhere = "F50cNuminv='" + cNumeroInventario + "'"
lStado = f3_sql('*', 'F50c', cWhere,  , , 'F50cCUR')
If !lStado
	This.UsrError = "Inventario no existe"
	Use In (Select ('F50cCUR'))
	Return lStado
EndIf

*> Compruebo si ya existen líneas para esa lista.
cWhere = "F50cNuminv='" + cNumeroInventario + "' And F50lNumInv=F50cNumInv"
lStado = f3_sql('*', 'F50c,F50l', cWhere,  , , 'F50clCUR')

If lStado
	*> Ya existe la lista.
	Use In (Select ('F50cCUR'))
	Use In (Select ('F50clCUR'))
	Return lStado
EndIf

Select F50cCUR
Go Top
If F50cEstado<>'0'
	*> Inventario no pendiente
	This.UsrError = "Estado no válido"
	Use In (Select ('F50cCUR'))
	Return .F.
EndIf

Scatter Name oF50c

*> Generar la lista de inventario.
*> Hago una consulta de los registros del F10/F16 que cumplen las condiciones del F50C.
*> Lo guardo en el cursor 'UBICS', aprovechando que tiene la estructura del F50l.
cField = "*"
cFromF = "F10c"
cWhere = _GCSS("F10cCodUbi", 1, 4) + " ='" + _Alma + _cm + ;
	" And (" + _GCSS("F10cCodUbi", 5, 2) + " Between '" + SubStr(oF50c.F50cUbiIni, 5, 2) + "' And '" + SubStr(oF50c.F50cUbiDsd, 5, 2) + "')" + ;
	" And (" + _GCSS("F10cCodUbi", 7, 2) + " Between '" + SubStr(oF50c.F50cUbiIni, 7, 2) + "' And '" + SubStr(oF50c.F50cUbiDsd, 7, 2) + "')" + ;
	" And (" + _GCSS("F10cCodUbi", 9, 3) + " Between '" + SubStr(oF50c.F50cUbiIni, 9, 3) + "' And '" + SubStr(oF50c.F50cUbiDsd, 9, 3) + "')" + ;
	" And (" + _GCSS("F10cCodUbi",12, 2) + " Between '" + SubStr(oF50c.F50cUbiIni,12, 2) + "' And '" + SubStr(oF50c.F50cUbiDsd,12, 2) + "')"

*> Agregar filtro de ubicaciones con artículos, si cal.
If !Empty(oF50c.F50cArtDsd) .Or. !Empty(oF50c.F50cArtHst)
	*> Cargar los artículos seleccionados para realizar el inventario.
	cWhere1 = "F50rNumInv='" + cNumeroInventario + "'"
	lStado = f3_sql("*", "F50r", cWhere1, , , "F50RGENL")
	If _xier<=0
		This.UsrError = "Error carga rangos"
		Use In (Select ('F50RGENL'))
		Use In (Select ('F50cCUR'))
		Use In (Select ('F50clCUR'))
	EndIf

	If lStado
		cFromF = "F10c,F16c"
		cWhere = cWhere + Iif(!Empty(_Procaot), " And F16cCodPro='" + _Procaot + "'", "")
		cWhere = cWhere + " And F16cCanRes=0 And F16cCodUbi=F10cCodUbi"

		Select F50RGENL
		Go Top
		Do While !Eof()
			Scatter Name oF50r
			cWhereArt = cWhereArt + Iif(Empty(cWhereArt), "F16cCodPro" + _GCC() + "F16cCodArt In ('", ", '")
			cWhereArt = cWhereArt + oF50r.F50rCodPro + oF50r.F50rCodArt + "'"
			Select F50RGENL
			Skip
		EndDo
	EndIf

	If !Empty(cWhereArt)
		cWhereArt = cWhereArt + ")"
	EndIf

	*> Una vez generadas las líneas, marcar selección de artículos.
	cWhere1 = "F50rNumInv='" + cNumeroInventario + "'"

	lStado = f3_updtun("F50r", ,"F50rEstado", "'3'", , cWhere1)
	If !lStado
		This.UsrError = "Error generación inventario (rangos)"
		Return lStado
	EndIf

	Use In (Select ('F50RGENL'))
EndIf

cWhere = cWhere + Iif(!Empty(cWhereArt), " And " + cWhereArt, "")

lStado = f3_sql(cField, cFromF, cWhere, , , 'F10cCUR')
If !lStado
	*> No hay ubicaciones para generar la lista de inventario: Se cancela la lista.
	This.CancelarInventario
	This.UsrError = "No hay ubicaciones"

	Use In (Select ('F10cCUR'))
	Use In (Select ('F50cCUR'))
	Use In (Select ('F50clCUR'))

	Return lStado
EndIf

*> Según la paridad, eliminar las ubicaciones sobrantes.
Select F10cCUR
Go Top
Do Case
	*> Paridad impar.
	Case oF50c.F50cParida=='I'
		Delete For Mod(Val(SubStr(F10cCUR.F10cCodUbi, 9, 3)), 2)==0

	*> Paridad par.
	Case oF50c.F50cParida=='D'
		Delete For Mod(Val(SubStr(F10cCUR.F10cCodUbi, 9, 3)), 2)==1
EndCase

*> Crear un cursor imagen del fichero de detalle de listas. (Generación).
=CrtCursor("F50l", "LISTAS")

*> Generar detalle de la lista de inventario.
cCurUbi = Space(14)

Select F10cCUR
Go Top
Do While !Eof()
	Scatter Name oF10c
	If oF10c.F10cCodUbi==cCurUbi
		Skip
		Loop
	EndIf

	cCurUbi = oF10c.F10cCodUbi

    *> Generar el detalle de inventario.
    Select LISTAS
    Append Blank

    Replace F50lNumInv With cNumeroInventario, ;
            F50lCodUbi With oF10c.F10cCodUbi, ;
            F50lCodOpe With oF50c.F50cCodOpe, ;
            F50lCodPrr With Space(6), ;
            F50lCodArr With Space(13), ;
            F50lNumLor With Space(15), ;
            F50lNumPar With Space(10), ;
            F50lFecCar With _FecMin, ;
            F50lSitStr With Space(4), ;
            F50lCanFir With 0, ;
            F50lRowId  With Ora_NewNum('NROW'), ;
            F50lEstado With '0', ;
            F50lIdOcup With Space(10)

    If !f3_InsTun("F50l", "LISTAS")
		This.UsrError = "Error insertando detalle"
		Use In (Select ('F10cCUR'))
		Use In (Select ('F50cCUR'))
		Use In (Select ('F50clCUR'))
		Use In (Select ('LISTAS'))
		Return .F.
	EndIf

	Select F10cCUR
	Skip
EndDo

*> Crear un cursor imagen del fichero de detalle de listas. (Carga ocupaciones).
=CrtCursor("F50l", "F50lCUR")

*> Cargar las ocupaciones de la lista de inventario generada.
Select F50lCUR
Append Blank

Select LISTAS
Go Top
Do While !Eof()
	Scatter Name oF50l

	cWhere = 		  "F16cCodUbi='" + oF50l.F50lCodUbi + "'"
	cWhere = cWhere + " And F16cCanRes=0"
	cWhere = cWhere + Iif(!Empty(_Procaot), " And F16cCodPro='" + _Procaot + "'", "")
	cWhere = cWhere + Iif(!Empty(cWhereArt), " And " + cWhereArt, "")

	lStado = f3_sql("*", "F16c", cWhere, , , 'F16cCUR')
	If _xier <= 0
		This.UsrError = "Error en carga ocupaciones"
		Use In (Select ('F10cCUR'))
		Use In (Select ('F50cCUR'))
		Use In (Select ('F50clCUR'))
		Use In (Select ('F50lCUR'))
		Use In (Select ('LISTAS'))
		Return .F.
	EndIf

	Select F16cCUR
	Go Top
	Do While !Eof()
		Scatter Name oF16c

		Select F50lCUR
		Gather Name oF50l

		*> Cargar datos teóricos, según PROCAOT.
		Replace F50lCodPrt With oF16c.F16cCodPro
		Replace F50lCodArt With oF16c.F16cCodArt
		Replace F50lNumLot With oF16c.F16cNumLot
		Replace F50lNumPat With oF16c.F16cNumPal
		Replace F50lFecCat With oF16c.F16cFecCad
		Replace F50lSitStt With oF16c.F16cSitStk
		Replace F50lCanFit With oF16c.F16cCanFis

		*> Cargar datos recuento.
		Replace F50lCodPrr With oF16c.F16cCodPro
		Replace F50lCodArr With oF16c.F16cCodArt
		Replace F50lNumLor With oF16c.F16cNumLot
		Replace F50lNumPar With oF16c.F16cNumPal
		Replace F50lFecCar With oF16c.F16cFecCad
		Replace F50lSitStr With oF16c.F16cSitStk
		Replace F50lCanFir With 0

		*> Cargar ID ocupación.
		Replace F50lIdOcup With oF16c.F16cIdOcup

		Scatter MemVar

		If Empty(F50lRowId)
			*> Dar de alta una nueva línea de inventario.
			Replace F50lRowId With Ora_NewNum("NROW")

			lStado = f3_instun("F50l", "F50lCUR")
			If !lStado
				This.UsrError = "Error al insertar detalle"
				Use In (Select ('F10cCUR'))
				Use In (Select ('F50cCUR'))
				Use In (Select ('F50clCUR'))
				Use In (Select ('F50lCUR'))
				Use In (Select ('LISTAS'))
				Use In (Select ('F16cCUR'))
				Return lStado
			EndIf
		Else
			*> Actualizar la nueva línea de inventario.
			lStado = f3_updtun("F50l", , , , "F50lCUR")
			If !lStado
				This.UsrError = "Error al actualizar detalle"
				Use In (Select ('F10cCUR'))
				Use In (Select ('F50cCUR'))
				Use In (Select ('F50clCUR'))
				Use In (Select ('F50lCUR'))
				Use In (Select ('LISTAS'))
				Use In (Select ('F16cCUR'))
				Return lStado
			EndIf
		EndIf

		*> Si hay más ocupaciones, crear nuevo registro.
		oF50l.F50lRowId = Space(10)

		Select F16cCUR
		Skip
	EndDo

	Select LISTAS
	Skip
EndDo

Use In (Select ('F10cCUR'))
Use In (Select ('F50cCUR'))
Use In (Select ('F50clCUR'))
Use In (Select ('F50lCUR'))
Use In (Select ('LISTAS'))
Use In (Select ('F16cCUR'))

Return

ENDPROC
PROCEDURE oprc_access

If Type('This.oPRC')<>'O'
	This.oPRC = CreateObject('procaot')
EndIf

Return This.oPRC

ENDPROC
PROCEDURE actualizarlistacab

*> Actualizar el estado de la cabecera de inventario.

*> Recibe:
*>	- cNumInv, ó bien This.LisInv, nº de lista de inventario.
*>	- CieInv, cerrar el inventario.
*>	- FecCie, fecha de cierre de inventario.
*>	- Estado, estado a grabar, si cierre.

*> Devuelve:
*>	- Estado, resultado de la operación (.T. / .F.)
*>	- This.UsrError, texto error.

*> Historial de modificaciones:
*> 03.01.2008 (AVC) Modificado para tratamiento de inventarios OFF-LINE.

Parameters cNumInv

Local lStado
Private cNumeroInventario, cWhere, cEstado, dFecCie, cCampos, cValores

This.UsrError = ""

*> Asignar parámetros.
cNumeroInventario = Iif(Type('cNumInv')=='C', cNumInv, This.LisInv)

Use In (Select("F50cUPD"))

*> Cargar líneas de detalle.
cWhere = "F50lNumInv='" + cNumeroInventario + "' And (F50lEstado<'2' Or F50lEstado='5')"
lStado = f3_sql("*", "F50l", cWhere, , ,"F50cUPD")

If _xier <= 0
	This.UsrError = "No se pueden leer lineas"
	Use In (Select("F50cUPD"))
	Return .F.
EndIf

Select F50cUPD
Go Top

*> Estado inventario por defecto.
cEstado = Iif(Eof(), "2", "1")

Do Case
	*> No se puede cerrar: Hay líneas pendientes.
	Case This.CieInv=="S" .And. cEstado=="1"
		This.UsrError = "No se puede cerrar. Hay líneas pendientes"
		Use In (Select("F50cUPD"))
		Return .F.

	*> Cerrar el inventario.
	Case This.CieInv=="S"
		cEstado = This.Estado
		dFecCie = This.FecCie
		cCampos= "F50cEstado,F50cFecFin"
		cValores = "cEstado,dFecCie"

	*> Actualizar el estado de la cabecera.
	Otherwise
		cCampos= "F50cEstado"
		cValores = "cEstado"
EndCase

cWhere = "F50cNumInv='" + cNumeroInventario + "'"

lStado = f3_updtun("F50c", , cCampos, cValores, , cWhere)
If !lStado
	This.UsrError = "No se puede actualizar cabecera"
	Use In (Select("F50cUPD"))
	Return lStado
EndIf

Use In (Select("F50cUPD"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

With This
	.LisInv = ""				&& Nº de inventario.
	.RowID = ""					&& ID línea inventario.
	.CieInv = "N"				&& Cierre inventario.
	.FecCie = .F.				&& Fecha de cierre inventario.
	.NewOpe = ""				&& Nuevo operario.
	.Estado = .F.				&& Estado inventario.

	.UsrError = ""				&& Texto errores.
EndWith

Return

ENDPROC
PROCEDURE cancelarinventario

*> Cancelar la lista de inventario.
*> Marca las líneas que estén pendientes de procesar y actualiza el estado de la cabecera.

*> Recibe:
*>	- cNumInv, ó bien This.LisInv, nº de lista de inventario.

*> Devuelve:
*>	- Estado, resultado de la operación (.T. / .F.).
*>	- This.UsrError, texto error.

*> Historial de modificaciones:
*> 21.09.2009 (AVC) Agregar selección individual de artículos en inventario, actualizando estado de tabla rangos, F50R.
*> 22.09.2009 (AVC) Cancelar inventario si, al generar lista, no hay ubicaciones. Llamada realizada desde This.GenerarInventario

Parameters cNumInv

Private cNumeroInventario, cWhere

This.UsrError = ""

*> Asignar parámetros.
cNumeroInventario = Iif(Type('cNumInv')=='C', cNumInv, This.LisInv)

*> Validar estado de la cabecera.
m.F50cNumInv = cNumeroInventario
lStado = f3_seek("F50c")
If !lStado
	This.UsrError = "No existe la lista de inventario"
	Return lStado
EndIf

Select F50c
If F50cEstado >= "2"
	This.UsrError = "Inventario en estado no válido"
	Return .F.
EndIf

*> Actualizar estado de las líneas de detalle pendientes.
cWhere = "F50lNumInv='" + cNumeroInventario + "' And (F50lEstado<'2' Or F50lEstado='5')"

lStado = f3_updtun("F50l", ,"F50lEstado", "'3'", , cWhere, 'N')
If !lStado
	This.UsrError = "Error cancelación inventario (líneas)"
	Return lStado
EndIf

*> Actualizar los rangos por artículo, si hay.
cWhere = "F50rNumInv='" + cNumeroInventario + "'"

lStado = f3_updtun("F50r", ,"F50rEstado", "'3'", , cWhere)
If !lStado
	This.UsrError = "Error cancelación inventario (rangos)"
	Return lStado
EndIf

*> Cerrar la lista inventario.
With This
	.CieInv = "S"
	.FecCie = Date()
	.Estado = "3"
	lStado = .ActualizarListaCab(cNumeroInventario)
EndWith

Return lStado

ENDPROC
PROCEDURE asignarinventario

*> Asignar la lista de inventario a otro operario.
*> Cambia el operario a la cabecera y a TODAS las líneas de detalle.

*> Recibe:
*>	- cNumInv, ó bien This.LisInv, nº de lista de inventario.
*>	- cNewOpe, ó bien This.NewOpe, nuevo operario.

*> Devuelve:
*>	- Estado, resultado de la operación (.T. / .F.).
*>	- This.UsrError, texto error.

Parameters cNumInv, cNewOpe

Private cNumeroInventario, cOperario, cWhere, cCampos

This.UsrError = ""

*> Asignar parámetros.
cNumeroInventario = Iif(Type('cNumInv')=='C', cNumInv, This.LisInv)
cOperario = Iif(Type('cNewOpe')=='C', cNewOpe, This.NewOpe)

*> Validar estado de la cabecera.
m.F50cNumInv = cNumeroInventario
lStado = f3_seek("F50c")
If !lStado
	This.UsrError = "No existe la lista de inventario"
	Return lStado
EndIf

Select F50c
If F50cEstado >= "2"
	This.UsrError = "Inventario en estado no válido"
	Return .F.
EndIf

*> Validar el operario.
If Empty(cOperario)
	This.UsrError = "Operario en blanco"
	Return .F.
EndIf

If cOperario==F50c.F50cCodOpe
	This.UsrError = "Operario origen y destino iguales"
	Return .F.
EndIf

m.F05cCodOpe = cOperario
lStado = f3_seek("F05c")
If !lStado
	This.UsrError = "No existe el operario destino"
	Return lStado
EndIf

*> Actualizar operario de las líneas de detalle pendientes.
cWhere = "F50lNumInv='" + cNumeroInventario + "'"
cCampos = "F50lCodOpe"

lStado = f3_updtun("F50l", , cCampos, 'cOperario', , cWhere, 'N')
If !lStado
	This.UsrError = "Error asignando operario (detalle)"
	Return lStado
EndIf

*> Actualizar operario en la cabecera de la lista.
cWhere = "F50cNumInv='" + cNumeroInventario + "'"
cCampos = "F50cCodOpe"

lStado = f3_updtun("F50c", , cCampos, 'cOperario', , cWhere, 'N')
If !lStado
	This.UsrError = "Error asignando operario (cabecera)"
	Return lStado
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:
*> 03.01.2008 (AVC) Permitir inventariar cualquier situación de stock.
*>					Grabar cantidad recuento a cero por defecto, para obligar a leer todos los artículos.
*>					Modificado para tratamiento de inventarios OFF-LINE.
*>					Modificado método This.GenerarInventario
*>					Modificado método This.ActualizarListaCab
*>					Modificado método This.CancelarInventario
*> 16.09.2009 (AVC) Generar listas de inventario con rangos de artículos.
*>					Modificada cabecera de inventarios (F50C), agregando artículo desde-hasta. Si NO está en blanco, genera
*>					listas de inventario de ubicaciones que contengan los artículos seleccionados. Si el rango está en blanco,
*>					el proceso funciona como hasta ahora.
*>					Modificado método This.GenerarInventario
*> 21.09.2009 (AVC) Sustituir rango artículos por selección individual de artículos en inventario, tabla F50R.
*>					Modificado método This.GenerarInventario
*>					Modificado método This.CancelarInventario
*> 22.09.2009 (AVC) Cancelar inventario si, al generar lista, no hay ubicaciones.
*>					Modificado método This.GenerarInventario
*>					Modificado método This.CancelarInventario

ENDPROC
PROCEDURE Init

*> Abre los ficheros necesarios para trabajar, si no lo están.

=DoDefault()

With This
	.l_F50cIsOpen = Used("F50c")
	.l_F50lIsOpen = Used("F50l")

	If !.l_F50cIsOpen
		=.oPRC.OpenTabla("F50c")
	EndIf
	If !.l_F50lIsOpen
		=.oPRC.OpenTabla("F50l")
	EndIf
EndWith

ENDPROC
PROCEDURE Destroy

*> Cerrar los ficheros utilizados y restaurar entorno anterior.

With This
	If !.l_F50cIsOpen
		Use In (Select("F50c"))
	EndIf
	If !.l_F50lIsOpen
		Use In (Select("F50c"))
	EndIf
EndWith

=DoDefault()

ENDPROC


EndDefine 
