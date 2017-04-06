Define Class orafncrepos as custom
Height = 17
Width = 99



PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

With This
	.CodPro = ""				&& Propietario.
	.CodArt = ""				&& Art�culo.
	.TMovOrg = ""				&& Tipo movimiento origen.
	.TMovDst = ""				&& Tipo movimiento destino.
	.TipMac = ""				&& Tipo de MAC por defecto.
	.NumMovMP = ""				&& N� de movimiento de reposici�n.

	.UsrError = ""				&& Texto errores.
EndWith

Return

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
PROCEDURE ubicobj_access

*> Inicializar la clase para la funci�n de ubicaci�n.

If Type('This.UbicObj')<>'O'
	This.UbicObj = CreateObject("OraFncUbic")
EndIf

Return This.UbicObj

ENDPROC
PROCEDURE initlocals

*> Inicializar las propiedades de uso interno de la clase.

With This
	.WFacCaj = 0				&& Factor caja.
	.WFacPal = 0				&& Factor palet.
	.WRowId = .F.				&& RowId ocupaci�n.
	.WCodUbi = ""				&& Ubicaci�n candidata.
	.WCanUbi = 0				&& Cantidad ya ubicada en destino.
	.oF12e = .F.				&& Registro activo de art�culos / picking.
	.oF12c = .F.				&& Registro activo de art�culos / picking.
	.oF14c = .F.				&& Registro activo de MP.
	.SerPic = "P"				&& Criterio para hacer la reposici�n.
	.DejPic = "C"				&& Criterio para la cantidad a tomar.
EndWith

Return

ENDPROC
PROCEDURE reppalcaj

*> Generar reposiciones de palets a cajas (por m�nimos).

*> Recibe:
*>	- This.CodPro � bien cPropietario, propietario.
*>	- This.CodArt � bien cArticulo, art�culo.
*>	- This.TMovOrg, tipo movimiento origen a generar, generalmente "3570".
*>	- This.TMovDst, tipo movimiento destino a generar, generalmente "3070".
*>	- This.TipMac, tipo de MAC por defecto, generalmente "MSTD".

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cPropietario, cArticulo, cTalla, cColor

Private cCodPro, cCodArt, cCodTal, cCodCol
Local lStado

This.UsrError = ""

*> Asignar las propiedades a variables de trabajo.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cCodArt = Iif(Type('cArticulo')=='C', cArticulo, This.CodArt)
cCodTal = Iif(Type('cTalla')=='C', cTalla, This.CodTal)
cCodCol = Iif(Type('cColor')=='C', cColor, This.CodCol)

This.CodPro = cCodPro
This.CodArt = cCodArt
This.CodTal = cCodTal
This.CodCol = cCodCol

This.UbicObj.CodPro = This.CodPro
This.UbicObj.CodArt = This.CodArt
This.UbicObj.CodCol = This.CodCol
This.UbicObj.CodTal = This.CodCol

*> Validar los tipo de movimiento a generar.
lStado = This._validartmovs()
If !lStado
	*> El texto de mensaje se devuelve por la funci�n.
	Return lStado
EndIf

*> Inicializar propiedades de uso interno de la clase.
This.InitLocals

*> Cargar los par�metros del art�culo e inicializar propiedades privadas.
lStado = This._cargarprmart()
If !lStado
	*> El texto de mensaje se devuelve por la funci�n.
	Return lStado
EndIf

*!*	*> Proceso de generaci�n de reposiciones.
*!*	Select F12cRepMin
*!*	Go Top
*!*	Do While !Eof()
*!*		Scatter Name This.oF12c

*!*		*> Validar que la ubicaci�n sea de cajas y est� disponible.
*!*		lStado = This._validarubicacion(This.oF12c.F12cCodUbi, "S")
*!*		If lStado
*!*			*> Validar si hay reposiciones ya generadas.
*!*			This.WCodUbi = This.oF12c.F12cCodUbi
*!*			lStado = This._erepant()
*!*			If !lStado
*!*				*> Validar la cantidad que ya hay en la ubicaci�n candidata (OC + MP).
*!*				This.WCanUbi = This._canocubic()
*!*				If This.WCanUbi <= This.oF12c.F12cCanMin
*!*					*> Generar reposiciones a cajas.
*!*					lStado = This._reppalcaj()
*!*				EndIf
*!*			EndIf
*!*		EndIf

*!*		*> Siguiente ubicaci�n destino para reponer.
*!*		Select F12cRepMin
*!*		Skip
*!*	EndDo


*> Proceso de generaci�n de reposiciones.
Select F12eRepMin
Go Top
Do While !Eof()
	Scatter Name This.oF12e

	*> Validar que la ubicaci�n sea de cajas y est� disponible.
	lStado = This._validarubicacion(This.oF12e.F12eCodUbi, "S")
	If lStado
		*> Validar si hay reposiciones ya generadas.
		This.WCodUbi = This.oF12e.F12eCodUbi
		lStado = This._erepant()
		If !lStado
			*> Validar la cantidad que ya hay en la ubicaci�n candidata (OC + MP).
			This.WCanUbi = This._canocubic()
			If This.WCanUbi <= This.oF12e.F12eStkMin
				*> Generar reposiciones a cajas.
				lStado = This._reppalcaj()
			EndIf
		EndIf
	EndIf

	*> Siguiente ubicaci�n destino para reponer.
	Select F12eRepMin
	Skip
EndDo

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _validartmovs

*> Validar los tipos de movimiento a generar.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- This.TMovOrg, tipo movimiento origen a generar, generalmente "35xx".
*>	- This.TMovDst, tipo movimiento destino a generar, generalmente "30xx".
*>	- This.TipMac, tipo de MAC por defecto, generalmente "MSTD".

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Local lStado

This.UsrError = ""

If Empty(This.TMovOrg)
	This.UsrError = "Tipo de movimiento origen no recibido"
	Return .F.
EndIf
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

*> Validar el tipo de movimiento a generar (destino).
If Empty(This.TMovDst)
	This.UsrError = "Tipo de movimiento destino no recibido"
	Return .F.
EndIf
m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Validar el tipo de MAC.
If Empty(This.TipMac)
	This.UsrError = "Tipo de MAC no recibido"
	Return .F.
EndIf

m.F46cCodigo = This.TipMac
If !f3_seek('F46c')
	This.UsrError = "Tipo de MAC: " + This.TipMac + " no existe"
	Return .F.
EndIf

Return

ENDPROC
PROCEDURE _cargarprmart

*> Cargar los par�metros del art�culo.
*> Funci�n privada de uso interno de la clase.

*> Recibe:
*>	- This.CodPro, Propietario.
*>	- This.CodArt, Art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.ReposAutoMPK, reposiciones autom�ticas de cajas.
*>	- This.ReposAutoMPU, reposiciones autom�ticas de unidades.

Private cWhere, cOrder
Local lStado

This.UsrError = ""

*> Leer datos de la ficha del producto.
m.F08cCodPro = This.CodPro
m.F08cCodArt = This.CodArt

If !f3_seek("F08c")
	This.UsrError = "No existe el producto"
	Return .F.
EndIf

*> Pasar par�metros producto a propiedades.
Select F08c
With This
	.WFacCaj = F08cUniPac * F08cPacCaj
	.WFacPal = .WFacCaj * F08cCajPal
EndWith

*!*	*> Cargar cursor con las ubicaciones asignadas al producto.
*!*	cWhere = "F12cCodPro='" + This.CodPro + "' And F12cCodArt='" + This.CodArt + "' And F12cCodAlm='" + _Alma + "'"
*!*	cOrder = "F12cCodPro, F12cCodArt, F12cCodAlm, F12cPriori"

*!*	lStado = f3_sql("*", "F12c", cWhere, cOrder, , "F12cRepMin")
*!*	If !lStado
*!*		This.UsrError = "Art�culo sin ubicaciones asignadas"
*!*		Return .F.
*!*	EndIf

*> Cargar cursor con las ubicaciones asignadas al producto.
cWhere = "F12eCodPro='" + This.CodPro + "' And F12eCodArt='" + This.CodArt + "'"

cWhere = cWhere + Iif(Type('This.CodCol')=='C', " And F12eCodCol='" + This.CodCol + "'", "")
cWhere = cWhere + Iif(Type('This.CodTal')=='C', " And F12eCodTal='" + This.CodTal + "'", "")

cOrder = "F12eCodPro, F12eCodArt, F12ePriori"

lStado = f3_sql("*", "F12e", cWhere, cOrder, , "F12eRepMin")
If !lStado
	This.UsrError = "Art�culo sin ubicaciones asignadas"
	Return .F.
EndIf

Return

ENDPROC
PROCEDURE _validarubicacion

*> Valida ubicaci�n destino en reposiciones por m�nimos.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- cUbicacion, ubicaci�n candidata.
*>	- cTipo, tipo de ubicaci�n requerido:
*>		"S": Caja.
*>		"U": Fracciones.
*>		"G": Agrupaci�n.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cUbicacion, cTipo

Local lStado

This.UsrError = ""

m.F10cCodUbi = cUbicacion
If !f3_seek("F10c")
	This.UsrError = "Ubicaci�n no existe"
	Return lStado
EndIf

Select F10c

If F10cPickSn<>cTipo
	This.UsrError = "Tipo ubicaci�n distinto"
	Return .F.
EndIf

If F10cEstGen=="B"
	This.UsrError = "La ubicaci�n est� bloqueada"
	Return .F.
EndIf

If F10cEstEnt<>"N"
	This.UsrError = "La ubicaci�n est� bloqueada de entrada"
	Return .F.
EndIf

Return

ENDPROC
PROCEDURE _erepant

*> Valida reposiciones anteriores para una ubicaci�n destino en reposiciones por m�nimos.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.WCodUbi, ubicaci�n a validar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de odificaciones:
*> 11.06.2008 (AVC) Adaptar longitud de los segmentos del lote.

Private cWhere
Local lStado

This.UsrError = ""

cWhere = 		  "F14cCodPro='" + This.CodPro + "' And F14cCodArt='" + This.CodArt + "' And "
cWhere = cWhere + "F14cUbiDes='" + This.WCodUbi + "' And " + _GCSS("F14cTipMov", 1, 1) + "='3'"

cWhere = cWhere + Iif(Type('This.CodCol')=='C', " And " + _GCSS("F14cNumLot", 1, 4) + "='" + This.CodCol + "'", cWhere)
cWhere = cWhere + Iif(Type('this.codtal')=='C', " And " + _GCSS("F14cNumLot", 5, 4) + "='" + this.codtal + "'", cWhere)

lStado = f3_sql("*", "F14c", cWhere, , , "F14cRepAnt")

Use In (Select("F14cRepAnt"))
Return lStado

ENDPROC
PROCEDURE _canocubic

*> Valida cantidad ya existente en una ubicaci�n candidata en reposiciones por m�nimos.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- This.WCodUbi, ubicaci�n a validar.

*> Devuelve:
*>	- Cantidad que hay en la ubicaci�n
*>	- This.UsrError, texto errores.

Private cWhere
Local lStado, nCantidad

This.UsrError = ""

nCantidad = 0

With This.UbicObj
	.CodPro = This.CodPro
	.CodArt = This.CodArt
	.CodTal = This.CodTal
	.CodCol = This.CodCol
	nCantidad = .CanPickingMP(This.WCodUbi) + .CanPickingOC(This.WCodUbi)
EndWith

Return nCantidad

ENDPROC
PROCEDURE _getocurep

*> Obtener ocupaciones origen para generar reposiciones.
*> Calcula la cantidad disponible, restando la reservada y la ya asignada a otras reposiciones.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- This.CodPro, propietario.
*>	- This.CodArt, art�culo.
*>	- cTipo, Tipo de ubicaci�n origen.
*>	- cRowID, ID ocupaci�n fija, opcional.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de odificaciones:
*> 11.06.2008 (AVC) Adaptar longitud de los segmentos del lote.

Parameters cTipo, cRowID

Private cWhere, cFromF, cOrder
Local lStado, oF16c, nCantidadMP

This.UsrError = ""

cWhere = 		  "F16cCodPro='" + This.CodPro + "' And F16cCodArt='" + This.CodArt + "' And "
cWhere = cWhere + "F10cCodUbi=F16cCodUbi And F10cPickSn='" + cTipo + "'"
If Type('cRowID')=='C'
	cWhere = cWhere + Space(1) + "And F16cIdOcup='" + cRowId + "'"
EndIf

cWhere = cWhere + Iif(Type('This.CodCol')=='C', " And " + _GCSS("F16cNumLot", 1, 4) + "='" + This.CodCol + "'", cWhere)
cWhere = cWhere + Iif(Type('this.codtal')=='C', " And " + _GCSS("F16cNumLot", 5, 4) + "='" + this.codtal + "'", cWhere)

cOrder = ""
cFromF = "F16c,F10c"

lStado = f3_sql("*", cFromF, cWhere, , , "F16cOcuRep")

*> Eliminar las ocupaciones origen candidatas que tengan MPs de preparaci�n.
*> Descontar la cantidad ya asignada a otras reposiciones.
Select F16cOcuRep
Go Top
Do While !Eof()
	Scatter Name oF16c

	cWhere = "F14cIdOcup='" + oF16c.F16cIdOcup + "'"
	lStado = f3_sql("*", "F14c", cWhere, , , "F16cMPs")

	Select F16cMPs
	Locate For Between(F14cTipMov, "2000", "2999")
	If Found()
		*> Si hay MPs de preparaci�n, ignorar la ocupaci�n, pues la reposici�n se generar�
		*> en el proceso de reposiciones autom�ticas.
		Select F16cOcuRep
		Delete Next 1
		Skip
		Loop
	EndIf

*!*		Locate For Between(F14cTipMov, "3500", "3599")
*!*		If Found()
*!*			*> Si ya hay alguna reposici�n, ignorar la ocupaci�n.
*!*			Select F16cOcuRep
*!*			Delete Next 1
*!*			Skip
*!*			Loop
*!*		EndIf

	*> Cantidad no disponible.
	Sum(F14cCanFis) To nCantidadMP For Between(F14cTipMov, "3500", "3599")

	Select F16cOcuRep
	Replace F16cCanFis With F16cCanFis - nCantidadMP
	If F16cCanFis <= 0
		*> No hay cantidad disponible.
		Delete Next 1
	EndIf

	Skip
EndDo

Go Top
lStado = !Eof()

Use In (Select("F16cMPs"))
Return lStado

ENDPROC
PROCEDURE _reppalcaj

*> Generar reposiciones a cajas a la ubicaci�n destino actual.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- This.WCanUbi, Cantidad actual en la ubicaci�n destino.
*> 	- This.oF12c, registro actual de art�culos / picking.
*>	- This.DejPic, Criterio para tomar cantidad (para palet):
*>		C: Parte de la cantidad pedida.
*>		P: Ajusta a palet.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Revisiones:
*>	AVC - 20.06.2005 Generar MP con documento en blanco, en lugar de tomar el de la ocupaci�n.

Private cWhere, nCantidadNecesaria, nCantidad
Local lStado, oF16c

This.UsrError = ""

*> Generar el cursor con las ocupaciones candidatas. Devuelve F16cOcuRep.
lStado = This._getocurep("N")
If !lStado
	This.UsrError = "No hay ocupaciones candidatas"
	Return lStado
EndIf

*> Calcular la cantidad a reponer, redondeada a cajas.
*nCantidadNecesaria = This.oF12c.F12cCanMax - This.WCanUbi
nCantidadNecesaria = This.oF12e.F12eStkMax - This.WCanUbi
nCantidadNecesaria = This.RoundCantidad(nCantidadNecesaria, nCantidadNecesaria + This.WFacCaj, "S")

Select F16cOcuRep
Go Top
Do While !Eof()
	Scatter Name oF16c

	nCantidad = Iif(oF16c.F16cCanFis >= nCantidadNecesaria, nCantidadNecesaria, oF16c.F16cCanFis)
	If This.DejPic=="P"
		*> Ajustar a cantidad palet, seg�n criterios de reserva.
		nCantidad = oF16c.F16cCanFis
	EndIf

	*> Asignar propiedades de la funci�n de actualizaci�n.
	This.ActzObj.ObjParm = This.ActzPrm

	*> Movimiento de salida origen.
	With This.ActzObj.ObjParm
		.Inicializar
		.CargarParametros('O', oF16c)

		.PuFlag = 'N'
		.PoFlag = 'N'
		.PsFlag = 'N'
		.PmFgHM = 'N'
		.PmFgMP = "S"

		.PuBOld = oF16c.F16cCodUbi
		.PoCFis = nCantidad
		.PoCRes = 0
		.PoTDoc = Space(4)
		.PoNDoc = Space(13)
		.PoLDoc = 0
		.PoNEnt = Space(10)
		.PoRowId = oF16c.F16cIdOcup

*		.PmUDes = This.oF12c.F12cCodUbi
		.PmUDes = This.oF12e.F12eCodUbi
		.PmTMov = This.TMovOrg
		.PmFMov = Date()
		.PmTMac = This.TipMac
		.PmORes = "N"
		.PmEnSa = "S"
	EndWith

	*> Generar el MP.
	This.ActzObj.ActMP
	If This.ActzObj.ObjParm.PwCrtn >= '50'
		This.UsrError = "Error al generar MP de reposici�n"
		Return .F.
	EndIf

	*> Establecer el estado de bloqueo.
	This.SetBloqMP(oF16c.F16cIdOcup, "B")		&& MPs y listas.
	This.SetBloqOC(oF16c.F16cIdOcup)			&& Ocupaci�n.

	nCantidadNecesaria = nCantidadNecesaria - nCantidad
	If nCantidadNecesaria <= 0
		*> Ya no hace falta reponer mas.
		Exit
	EndIf

	*> Probar con la siguiente ocupaci�n candidata.
	Select F16cOcuRep
	Skip
EndDo

lStado = (nCantidadNecesaria <= 0)
Return lStado

ENDPROC
PROCEDURE cncrepmp

*> Cancelaci�n de UN MP de reposici�n.

*> Recibe:
*>	- cMovimiento, � bien This.NumMovMP, n� de MP a cancelar.

*> Devuelve:
*>	- Estado (.T./.F.).
*>	- This.UsrError, texto errores.

Parameters cMovimiento

Private cNumMov, cWhere, oF14c
Local lStado

This.UsrError = ""

*> Asignar par�metros a variables.
cNumMov = Iif(PCount()==1, cMovimiento, This.NumMovMP)

*> Validar el movimiento de reposici�n.
If !f3_seek("F14c", "[m.F14cNumMov=cNumMov]")
	This.UsrError = "No existe el MP de reposici�n"
	Return .F.
EndIf

Select F14c
Scatter Name oF14c

If SubStr(oF14c.F14cTipMov, 1, 1)<>"3"
	This.UsrError = "El MP no es una reposici�n"
	Return .F.
EndIf

*> Validar si el MP est� en listas.
If f3_seek("F26l", "[m.F26lNumMov=cNumMov]")
	This.UsrError = "El MP est� en listas"
	Return .F.
EndIf

*> Eliminar el MP de reposici�n.
=f3_baja("F14c")

*> Actualizar el estado de bloqueo de los MPs asociados.
This.SetBloqMP(oF14c.F14cIdOcup, Space(1))			&& MPs y listas.
This.SetBloqOC(oF14c.F14cIdOcup)					&& Ocupaci�n.

lStado = .T.
Return lStado

ENDPROC
PROCEDURE setbloqmp

*> Establecer el estado de bloqueo los MPs de preparaci�n (MPs + listas) asociados a una reposici�n.

*> Recibe:
*>	- RowId de la ocupaci�n.
*> 	- Valor estado bloqueo:
*>		"B", bloquea los MPs.
*>		" ", libera los MPs.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cRowID, cValorBloqueo

Private cWhere, cCampos, cValores
Local lStado

This.UsrError = ""

cWhere = "F14cIdOcup='" + cRowID + "' And " + _GCSS("F14cTipMov", 1, 1) + "='2'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cBLOQ")
If !lStado
	*> No hay MPs de preparaci�n.
	Use In (Select("F14cBLOQ"))
	Return .T.
EndIf

*> Actualizar el estado bloqueo de las listas.
Select "F14cBLOQ"
Go Top
Do While !Eof()
	m.F26lNumMov = F14cNumMov
	If f3_seek("F26l")
		Select F26l
		Replace F26lFlag1 With cValorBloqueo
		=f3_updtun("F26l")
	EndIf

	Select "F14cBLOQ"
	Skip
EndDo

*> Actualizar el estado de bloqueo de los MPs de preparaci�n.
cCampos = "F14cFlag1"
cValores = "cValorBloqueo"

=f3_updtun("F14c", , cCampos, cValores, , cWhere, "N")

Use In (Select("F14cBLOQ"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE roundcantidad

*> Redondear cantidad a un factor dado (caja, grupo, palet).
*> Aplica los par�metros de reserva del art�culo.

*> Recibe:
*>	- nCantidad, cantidad a redondear.
*>	- nCantidadMaxima, cantidad m�xima despu�s del redondeo.
*>	- cEsPico, tipo de redondeo.
*>	- This.WFacCaj, Factor caja del producto.
*>	- This.WFacPal, Factor palet del producto.

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
PROCEDURE setbloqoc

*> Establecer el estado reposici�n de una ocupaci�n.

*> Recibe:
*>	- RowId de la ocupaci�n.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cRowID

Private cWhere, cCampos, cValores, cStat
Local lStado

This.UsrError = ""

cWhere = "F14cIdOcup='" + cRowID + "' And " + _GCSS("F14cTipMov", 1, 1) + "='3'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cBLOQ")

cStat = Iif(lStado, "R", Space(1))

*> Actualizar el estado de bloqueo de la ocupaci�n.
cWhere = "F16cIdOcup='" + cRowID + "'"
cCampos = "F16cFlag1"
cValores = "cStat"

=f3_updtun("F16c", , cCampos, cValores, , cWhere, "N")

Use In (Select("F14cBLOQ"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE reposauto

*> Generar reposiciones autom�ticas.

*> Recibe:
*>	- This.CodPro, Propietario (opcional).
*>	- This.CodArt, Art�culo (opcional).
*>	- This.TMovOrg, tipo movimiento origen a generar, generalmente "3570".
*>	- This.TMovDst, tipo movimiento destino a generar, generalmente "3070".
*>	- This.TipMac, tipo de MAC por defecto, generalmente "MSTD".
*>	- This.SerPic, Criterio para generar reposici�n.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de odificaciones:
*> 11.06.2008 (AVC) Adaptar longitud de los segmentos del lote.

Parameters cPropietario, cArticulo, cTalla, cColor

Private cWhere, cFromF, cCodPro, cCodArt, cCodTal, cCodCol
Local lStado

This.UsrError = ""

*> Inicializar propiedades de uso interno de la clase.
This.InitLocals

If PCount()==4
	cCodPro = cPropietario
	cCodArt = cArticulo
	cCodTal = cTalla
	cCodCol = cColor
Else
	cCodPro = Iif(Type('This.CodPro')=='C', This.CodPro, "")
	cCodArt = Iif(Type('This.CodArt')=='C', This.CodArt, "")
	cCodTal = Iif(Type('This.CodTal')=='C', This.CodTal, "")
	cCodCol = Iif(Type('This.CodCol')=='C', This.CodCol, "")
EndIf

*> Cargar los MPs de reserva pendientes de reposici�n.
cWhere = 		  "F14cTipMov Between '2000' And '2998' And F14cFlag1<>'B' And "
cWhere = cWhere + "F10cCodUbi=F14cUbiOri"
cWhere = cWhere + Iif(!Empty(cCodPro), " And F14cCodPro='" + cCodPro + "'", "")
cWhere = cWhere + Iif(!Empty(cCodArt), " And F14cCodArt='" + cCodArt + "'", "")

cWhere = cWhere + Iif(!Empty(cCodCol), " And " + _GCSS("F14cNumLot", 1, 4) + "='" + cCodCol + "'", "")
cWhere = cWhere + Iif(!Empty(cCodTal), " And " + _GCSS("F14cNumLot", 5, 4) + "='" + cCodTal + "'", "")

cFromF = "F14c,F10c"

lStado = f3_sql("*", cFromF, cWhere, , , "F14cReposAuto")
If !lStado
	This.UsrError = "No hay datos"
	Use In (Select("F14cReposAuto"))
	Return lStado
EndIf

Select F14cReposAuto
Go Top
Do While !Eof()
	Scatter Name This.oF14c

	*> Asignar valores a propiedades.
	This.CodPro = This.oF14c.F14cCodPro
	This.CodArt = This.oF14c.F14cCodArt

	*> Cargar los par�metros de reserva del art�culo.
	=This._cargarprmrsv(This.oF14c.F14cCodPro, This.oF14c.F14cCodArt)

	If This.SerPic=="N"
		*> Los par�metros de reserva no permiten reposici�n.
		Select F14cReposAuto
		Delete For F14cIdOcup==This.oF14c.F14cIdOcup
		Go Top
		Loop
	EndIf

	Do Case
		*> Reposici�n forzada seg�n par�metros de reserva.
		Case This.oF14c.F10cPickSn=="N" .And. This.SerPic=="S"
			lStado = This.ReposAutoMPK(This.oF14c.F14cNumMov)

		*> Reposici�n de palet a CAJAS.
		Case At(This.oF14c.F14cOriRes, "CU") > 0 .And. This.oF14c.F10cPickSn=="N"
			lStado = This.ReposAutoMPK(This.oF14c.F14cNumMov)

		*> Reposici�n de cajas a UNIDADES.
		Case At(This.oF14c.F14cOriRes, "U") > 0 .And. This.oF14c.F10cPickSn=="S"
			lStado = This.ReposAutoMPU(This.oF14c.F14cNumMov)
	EndCase

	Select F14cReposAuto
	Delete For F14cIdOcup==This.oF14c.F14cIdOcup
	Go Top
EndDo

Use In (Select("F14cReposAuto"))
Return lStado

ENDPROC
PROCEDURE reposautompk

*> Generar reposici�n autom�tica a cajas de UN MP.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- cNumMov � bien This.NumMovMP, N� de MP a procesar.

*>	- This.TMovOrg, tipo movimiento origen a generar, generalmente "3570".
*>	- This.TMovDst, tipo movimiento destino a generar, generalmente "3070".
*>	- This.TipMac, tipo de MAC por defecto, generalmente "MSTD".

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 29.05.2006 (AVC) Cargar ubicaciones de picking general si no hay de picking fijo.
*> 11.06.2008 (AVC) Adaptar longitud de los segmentos del lote.

Parameters cMovimiento

Private cWhere, cField, nCantidad, cNumMov
Local lStado

This.UsrError = ""

*> Asignar las propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)

*> Cargar el MP, en caso de que no venga ya de otro proceso (This.ReposAuto, p.ej).
If Type('This.oF14c')<>'O'
	lStado = f3_seek("F14c", '[m.F14cNumMov=cNumMov]')
	If !lStado
		This.UsrError = "No existe el MP: " + cNumMov
		Return lStado
	EndIf

	Select F14c
	Scatter Name This.oF14c
EndIf

*> Pasar valores a propiedades.
With This
	.CodPro = .oF14c.F14cCodPro
	.CodArt = .oF14c.F14cCodArt
	.CodCol = SubStr(.oF14c.F14cNumLot, 1, 4)
	.CodTal = SubStr(.oF14c.F14cNumLot, 5, 4)

	.UbicObj.CodPro = .oF14c.F14cCodPro
	.UbicObj.CodArt = .oF14c.F14cCodArt
	.UbicObj.CodCol = SubStr(.oF14c.F14cNumLot, 1, 4)
	.UbicObj.CodTal = SubStr(.oF14c.F14cNumLot, 5, 4)
EndWith

*> Validar los tipo de movimiento a generar.
lStado = This._validartmovs()
If !lStado
	*> El texto de mensaje se devuelve por la funci�n.
	Return lStado
EndIf

*> Cargar los par�metros (ubicaciones) de picking del art�culo e inicializar propiedades privadas.
lStado = This._cargarprmart()

If !lStado .And. (This.SerPic=='R' .Or. This.SerPic=='G')
	*> No hay ubicaciones de picking fijo: Cargar ubicaciones de picking (rangos).
	lStado = This._cargarprmrng("S")

	If !lStado .And. (This.SerPic=='G')
		*> No hay ubicaciones de picking rangos: Cargar ubicaciones de picking general.
		lStado = This._cargarprmgen("S")

		If !lStado
			*> El texto de mensaje se devuelve por la funci�n.
			Return lStado
		EndIf
	EndIf
EndIf

*> Calcular la cantidad total reservada en MPs.
cWhere = "F14cIdOcup='" + This.oF14c.F14cIdOcup + "' And F14cTipMov Between '2000' And '2998'"
cField = _GCN("Sum(F14cCanFis)", 0) + " As F14cCFis"

lStado = f3_sql(cField, "F14c", cWhere, , , "F14cRepSum")
If !lStado
	This.UsrError = "No se puede sumar cantidad reservada"
	Use In (Select("F14cRepSum"))
	Return lStado
EndIf

Select F14cRepSum
Go Top
If IsNull(F14cCFis)
	Replace F14cCFis With 0
EndIf

If F14cCFis <= 0
	This.UsrError = "No se puede sumar cantidad reservada"
	Use In (Select("F14cRepSum"))
	Return .F.
EndIf

nCantidad = F14cCFis

*!*	*> Recorrer las ubicaciones de picking candidatas.
*!*	Select F12cRepMin
*!*	Go Top
*!*	Do While !Eof()
*!*		Scatter Name This.oF12c

*!*		*> Validar que la ubicaci�n sea de cajas y est� disponible.
*!*		lStado = This._validarubicacion(This.oF12c.F12cCodUbi, "S")
*!*		If lStado
*!*			*> Validar si hay reposiciones ya generadas.
*!*			This.WCodUbi = This.oF12c.F12cCodUbi
*!*			lStado = This._erepant()
*!*			If !lStado
*!*				*> Generar reposici�n autom�tica a cajas.
*!*				lStado = This._reposautompk(nCantidad, This.oF14c.F14cIdOcup)
*!*				Exit
*!*			EndIf
*!*		EndIf

*!*		*> Siguiente ubicaci�n destino para reponer.
*!*		Select F12cRepMin
*!*		Skip
*!*	EndDo

*> Recorrer las ubicaciones de picking candidatas.
Select F12eRepMin
Go Top
Do While !Eof()
	Scatter Name This.oF12e

	*> Validar que la ubicaci�n sea de cajas y est� disponible.
	lStado = This._validarubicacion(This.oF12e.F12eCodUbi, "S")
	If lStado
		*> Validar si hay reposiciones ya generadas.
		This.WCodUbi = This.oF12e.F12eCodUbi
		lStado = This._erepant()
		If !lStado
			*> Generar reposici�n autom�tica a cajas.
			lStado = This._reposautompk(nCantidad, This.oF14c.F14cIdOcup)
			Exit
		EndIf
	EndIf

	*> Siguiente ubicaci�n destino para reponer.
	Select F12eRepMin
	Skip
EndDo

Use In (Select("F14cRepSum"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE _reposautompk

*> Generar reposiciones autom�ticas a cajas.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- Cantidad reservada total en MPs (m�nimo para reponer).
*>	- RowID de la ocupaci�n origen.
*> 	- This.WCodUbi, ubicaci�n destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters nCantidadReservada, cRowID

Private cWhere, nCantidad
Local lStado, oF16c

This.UsrError = ""

*> Leer la ocupaci�n origen sobre la que se realizar� la reposici�n.
cWhere = "F16cIdOcup='" + cRowID + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "F16cIdOrg")
If !lStado
	This.UsrError = "No existe ocupaci�n origen"
	Use In (Select("F16cIdOrg"))
	Return .F.
EndIf

*> Calcular la cantidad a reponer, redondeada a cajas.
nCantidadReservada = This.RoundCantidad(nCantidadReservada, nCantidadReservada + This.WFacCaj, "S")

Select F16cIdOrg
Scatter Name oF16c

nCantidad = Iif(oF16c.F16cCanFis >= nCantidadReservada, nCantidadReservada, oF16c.F16cCanFis)
If This.DejPic=="P"
	*> Ajustar a cantidad palet, seg�n criterios de reserva.
	nCantidad = oF16c.F16cCanFis
EndIf

*> Asignar propiedades de la funci�n de actualizaci�n.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros("O", oF16c)

	.PuFlag = "N"
	.PoFlag = "N"
	.PsFlag = "N"
	.PmFgHM = "N"
	.PmFgMP = "S"

	.PuBOld = oF16c.F16cCodUbi
	.PoCFis = nCantidad
	.PoCRes = 0
	.PoRowId = oF16c.F16cIdOcup

	.PmUDes = This.WCodUbi
	.PmTMov = This.TMovOrg
	.PmFMov = Date()
	.PmTMac = This.TipMac
	.PmORes = "N"
	.PmEnSa = "S"
EndWith

*> Generar el MP.
This.ActzObj.ActMP
If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error al generar MP de reposici�n"
	Return .F.
EndIf

*> Establecer el estado de bloqueo.
This.SetBloqMP(oF16c.F16cIdOcup, "B")		&& MPs y listas.
This.SetBloqOC(oF16c.F16cIdOcup)			&& Ocupaci�n.

Return

ENDPROC
PROCEDURE reposautompu

*> Generar reposici�n autom�tica a unidades de UN MP.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- cNumMov � bien This.NumMovMP, N� de MP a procesar.
*>	- This.TMovOrg, tipo movimiento origen a generar, generalmente "3580".
*>	- This.TMovDst, tipo movimiento destino a generar, generalmente "3080".
*>	- This.TipMac, tipo de MAC por defecto, generalmente "MSTD".

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 29.05.2006 (AVC) Cargar ubicaciones de picking general si no hay de picking fijo.
*> 11.06.2008 (AVC) Adaptar longitud de los segmentos del lote.

Parameters cMovimiento

Private cWhere, cField, nCantidad, cNumMov
Local lStado

This.UsrError = ""

*> Asignar las propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)

*> Cargar el MP, en caso de que no venga ya de otro proceso (This.ReposAuto, p.ej).
If Type('This.oF14c')<>'O'
	lStado = f3_seek("F14c", '[m.F14cNumMov=cNumMov]')
	If !lStado
		This.UsrError = "No existe el MP: " + cNumMov
		Return lStado
	EndIf

	Select F14c
	Scatter Name This.oF14c
EndIf

*> Pasar valores a propiedades.
With This
	.CodPro = .oF14c.F14cCodPro
	.CodArt = .oF14c.F14cCodArt
	.CodCol = SubStr(.oF14c.F14cNumLot, 1, 4)
	.CodTal = SubStr(.oF14c.F14cNumLot, 5, 4)

	.UbicObj.CodPro = .oF14c.F14cCodPro
	.UbicObj.CodArt = .oF14c.F14cCodArt
	.UbicObj.CodCol = SubStr(.oF14c.F14cNumLot, 1, 4)
	.UbicObj.CodTal = SubStr(.oF14c.F14cNumLot, 5, 4)
EndWith

*> Validar los tipo de movimiento a generar.
lStado = This._validartmovs()
If !lStado
	*> El texto de mensaje se devuelve por la funci�n.
	Return lStado
EndIf

*> Cargar los par�metros (ubicaciones) de picking del art�culo e inicializar propiedades privadas.
lStado = This._cargarprmart()

If !lStado .And. (This.SerPic=='R' .Or. This.SerPic=='G')
	*> No hay ubicaciones de picking fijo: Cargar ubicaciones de picking (rangos).
	lStado = This._cargarprmrng("U")

	If !lStado .And. (This.SerPic=='G')
		*> No hay ubicaciones de picking rangos: Cargar ubicaciones de picking general.
		lStado = This._cargarprmgen("U")

		If !lStado
			*> El texto de mensaje se devuelve por la funci�n.
			Return lStado
		EndIf
	EndIf
EndIf

*> Calcular la cantidad total reservada en MPs.
cWhere = "F14cIdOcup='" + This.oF14c.F14cIdOcup + "' And F14cTipMov Between '2000' And '2998'"
cField = _GCN("Sum(F14cCanFis)", 0) + " As F14cCFis"

lStado = f3_sql(cField, "F14c", cWhere, , , "F14cRepSum")
If !lStado
	This.UsrError = "No se puede sumar cantidad reservada"
	Use In (Select("F14cRepSum"))
	Return lStado
EndIf

Select F14cRepSum
Go Top
If IsNull(F14cCFis)
	Replace F14cCFis With 0
EndIf

If F14cCFis <= 0
	This.UsrError = "No se puede sumar cantidad reservada"
	Use In (Select("F14cRepSum"))
	Return .F.
EndIf

nCantidad = F14cCFis

*!*	*> Recorrer las ubicaciones de picking candidatas.
*!*	Select F12cRepMin
*!*	Go Top
*!*	Do While !Eof()
*!*		Scatter Name This.oF12c

*!*		*> Validar que la ubicaci�n sea de unidades y est� disponible.
*!*		lStado = This._validarubicacion(This.oF12c.F12cCodUbi, "U")
*!*		If lStado
*!*			*> Validar si hay reposiciones ya generadas.
*!*			This.WCodUbi = This.oF12c.F12cCodUbi
*!*			lStado = This._erepant()
*!*			If !lStado
*!*				*> Generar reposici�n autom�tica a unidades.
*!*				lStado = This._reposautompk(nCantidad, This.oF14c.F14cIdOcup)
*!*				Exit
*!*			EndIf
*!*		EndIf

*!*		*> Siguiente ubicaci�n destino para reponer.
*!*		Select F12cRepMin
*!*		Skip
*!*	EndDo

*> Recorrer las ubicaciones de picking candidatas.
Select F12eRepMin
Go Top
Do While !Eof()
	Scatter Name This.oF12e

	*> Validar que la ubicaci�n sea de unidades y est� disponible.
	lStado = This._validarubicacion(This.oF12e.F12eCodUbi, "U")
	If lStado
		*> Validar si hay reposiciones ya generadas.
		This.WCodUbi = This.oF12e.F12eCodUbi
		lStado = This._erepant()
		If !lStado
			*> Generar reposici�n autom�tica a unidades.
			lStado = This._reposautompk(nCantidad, This.oF14c.F14cIdOcup)
			Exit
		EndIf
	EndIf

	*> Siguiente ubicaci�n destino para reponer.
	Select F12eRepMin
	Skip
EndDo

Use In (Select("F14cRepSum"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE _reposautompu

*> Generar reposiciones autom�ticas a unidades.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- Cantidad reservada total en MPs (m�nimo para reponer).
*>	- RowID de la ocupaci�n origen.
*> 	- This.WCodUbi, ubicaci�n destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters nCantidadReservada, cRowID

Private cWhere, nCantidad
Local lStado, oF16c

This.UsrError = ""

*> Leer la ocupaci�n origen sobre la que se realizar� la reposici�n.
cWhere = "F16cIdOcup='" + cRowID + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "F16cIdOrg")
If !lStado
	This.UsrError = "No existe ocupaci�n origen"
	Use In (Select("F16cIdOrg"))
	Return .F.
EndIf

*> Calcular la cantidad a reponer, redondeada a cajas.
nCantidadReservada = This.RoundCantidad(nCantidadReservada, nCantidadReservada + This.WFacCaj, "S")

Select F16cIdOrg
Scatter Name oF16c

nCantidad = Iif(oF16c.F16cCanFis >= nCantidadReservada, nCantidadReservada, oF16c.F16cCanFis)

*> Asignar propiedades de la funci�n de actualizaci�n.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros("O", oF16c)

	.PuFlag = "N"
	.PoFlag = "N"
	.PsFlag = "N"
	.PmFgHM = "N"
	.PmFgMP = "S"

	.PuBOld = oF16c.F16cCodUbi
	.PoCFis = nCantidad
	.PoCRes = 0
	.PoRowId = oF16c.F16cIdOcup
	
	.PmUDes = This.WCodUbi
	.PmTMov = This.TMovOrg
	.PmFMov = Date()
	.PmTMac = This.TipMac
	.PmORes = "S"
	.PmEnSa = "S"
EndWith

*> Generar el MP.
This.ActzObj.ActMP
If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error al generar MP de reposici�n"
	Return .F.
EndIf

*> Establecer el estado de bloqueo.
This.SetBloqMP(oF16c.F16cIdOcup, "B")		&& MPs y listas.
This.SetBloqOC(oF16c.F16cIdOcup)			&& Ocupaci�n.

Return

ENDPROC
PROCEDURE repcajuni

*> Generar reposiciones de cajas a unidades (por m�nimos).

*> Recibe:
*>	- This.CodPro � bien cPropietario, propietario.
*>	- This.CodArt � bien cArticulo, art�culo.
*>	- This.TMovOrg, tipo movimiento origen a generar, generalmente "3560".
*>	- This.TMovDst, tipo movimiento destino a generar, generalmente "3060".
*>	- This.TipMac, tipo de MAC por defecto, generalmente "MSTD".

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cPropietario, cArticulo, cTalla, cColor

Private cCodPro, cCodArt, cTalla, cColor
Local lStado

This.UsrError = ""

*> Asignar las propiedades a variables de trabajo.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cCodArt = Iif(Type('cArticulo')=='C', cArticulo, This.CodArt)
cCodTal = Iif(Type('cTalla')=='C', cTalla, This.CodTal)
cCodCol = Iif(Type('cColor')=='C', cColor, This.CodCol)

This.CodPro = cCodPro
This.CodArt = cCodArt
This.CodTal = cCodTal
This.CodCol = cCodCol

This.UbicObj.CodPro = This.CodPro
This.UbicObj.CodArt = This.CodArt
This.UbicObj.CodCol = This.CodCol
This.UbicObj.CodTal = This.CodCol

*> Validar los tipo de movimiento a generar.
lStado = This._validartmovs()
If !lStado
	*> El texto de mensaje se devuelve por la funci�n.
	Return lStado
EndIf

*> Inicializar propiedades de uso interno de la clase.
This.InitLocals

*> Cargar los par�metros del art�culo e inicializar propiedades privadas.
lStado = This._cargarprmart()
If !lStado
	*> El texto de mensaje se devuelve por la funci�n.
	Return lStado
EndIf

*!*	*> Proceso de generaci�n de reposiciones.
*!*	Select F12cRepMin
*!*	Go Top
*!*	Do While !Eof()
*!*		Scatter Name This.oF12c

*!*		*> Validar que la ubicaci�n sea de unidades y est� disponible.
*!*		lStado = This._validarubicacion(This.oF12c.F12cCodUbi, "U")
*!*		If lStado
*!*			*> Validar si hay reposiciones ya generadas.
*!*			This.WCodUbi = This.oF12c.F12cCodUbi
*!*			lStado = This._erepant()
*!*			If !lStado
*!*				*> Validar la cantidad que ya hay en la ubicaci�n candidata (OC + MP).
*!*				This.WCanUbi = This._canocubic()
*!*				If This.WCanUbi <= This.oF12c.F12cCanMin
*!*					*> Generar reposiciones a unidades.
*!*					lStado = This._repcajuni()
*!*				EndIf
*!*			EndIf
*!*		EndIf

*!*		*> Siguiente ubicaci�n destino para reponer.
*!*		Select F12cRepMin
*!*		Skip
*!*	EndDo

*> Proceso de generaci�n de reposiciones.
Select F12eRepMin
Go Top
Do While !Eof()
	Scatter Name This.oF12e

	*> Validar que la ubicaci�n sea de unidades y est� disponible.
	lStado = This._validarubicacion(This.oF12e.F12eCodUbi, "U")
	If lStado
		*> Validar si hay reposiciones ya generadas.
		This.WCodUbi = This.oF12e.F12eCodUbi
		lStado = This._erepant()
		If !lStado
			*> Validar la cantidad que ya hay en la ubicaci�n candidata (OC + MP).
			This.WCanUbi = This._canocubic()
			If This.WCanUbi <= This.oF12e.F12eStkMin
				*> Generar reposiciones a unidades.
				lStado = This._repcajuni()
			EndIf
		EndIf
	EndIf

	*> Siguiente ubicaci�n destino para reponer.
	Select F12eRepMin
	Skip
EndDo


lStado = .T.
Return lStado

ENDPROC
PROCEDURE _repcajuni

*> Generar reposiciones a unidades a la ubicaci�n destino actual.
*> M�todo privado de uso interno de la clase.

*> Recibe:
*>	- This.WCanUbi, Cantidad actual en la ubicaci�n destino.
*> 	- This.oF12c, registro actual de art�culos / picking.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Revisiones:
*>	AVC - 20.06.2005 Generar MP con documento en blanco, en lugar de tomar el de la ocupaci�n.

Private cWhere, nCantidadNecesaria, nCantidad
Local lStado, oF16c

This.UsrError = ""

*> Generar el cursor con las ocupaciones candidatas. Devuelve F16cOcuRep.
lStado = This._getocurep("S")
If !lStado
	This.UsrError = "No hay ocupaciones candidatas"
	Return lStado
EndIf

*> Calcular la cantidad a reponer, redondeada a cajas.
*nCantidadNecesaria = This.oF12c.F12cCanMax - This.WCanUbi
nCantidadNecesaria = This.oF12e.F12eStkMax - This.WCanUbi
nCantidadNecesaria = This.RoundCantidad(nCantidadNecesaria, nCantidadNecesaria + This.WFacCaj, "S")

Select F16cOcuRep
Go Top
Do While !Eof()
	Scatter Name oF16c
	
	If nCantidadNecesaria < 1
		Select F16cOcuRep
		Skip
		Loop
	EndIf

	nCantidad = Iif(oF16c.F16cCanFis >= nCantidadNecesaria, nCantidadNecesaria, oF16c.F16cCanFis)

	*> Asignar propiedades de la funci�n de actualizaci�n.
	This.ActzObj.ObjParm = This.ActzPrm

	*> Movimiento de salida origen.
	With This.ActzObj.ObjParm
		.Inicializar
		.CargarParametros('O', oF16c)

		.PuFlag = 'N'
		.PoFlag = 'N'
		.PsFlag = 'N'
		.PmFgHM = 'N'
		.PmFgMP = "S"

		.PuBOld = oF16c.F16cCodUbi
		.PoCFis = nCantidad
		.PoCRes = 0
		.PoTDoc = Space(4)
		.PoNDoc = Space(13)
		.PoLDoc = 0
		.PoNEnt = Space(10)
		.PoRowId = oF16c.F16cIdOcup
		
*		.PmUDes = This.oF12c.F12cCodUbi
		.PmUDes = This.oF12e.F12eCodUbi
		.PmTMov = This.TMovOrg
		.PmFMov = Date()
		.PmTMac = This.TipMac
		.PmORes = "S"
		.PmEnSa = "S"
	EndWith

	*> Generar el MP.
	This.ActzObj.ActMP
	If This.ActzObj.ObjParm.PwCrtn >= '50'
		This.UsrError = "Error al generar MP de reposici�n"
		Return .F.
	EndIf

	*> Establecer el estado de bloqueo.
	This.SetBloqMP(oF16c.F16cIdOcup, "B")		&& MPs y listas.
	This.SetBloqOC(oF16c.F16cIdOcup)			&& Ocupaci�n.

	nCantidadNecesaria = nCantidadNecesaria - nCantidad
	If nCantidadNecesaria <= 0
		*> Ya no hace falta reponer mas.
		Exit
	EndIf

	*> Probar con la siguiente ocupaci�n candidata.
	Select F16cOcuRep
	Skip
EndDo

lStado = (nCantidadNecesaria <= 0)
Return lStado

ENDPROC
PROCEDURE _cargarprmrsv

*> Obtener los par�metros generales de reserva.
*> Define el criterio para generar la reposici�n y la cantidad a tomar,
*> seg�n los par�metros de reserva del art�culo que se le aplican.

*> Funci�n privada de uso interno de la clase.

*> Recibe:
*>	- This.CodPro, Propietario.
*>	- This.CodArt, Art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.SerPic, Criterio para generar reposici�n.
*>	- This.DejPic, Criterio para tomar cantidad.
*>	- This.UsrError, texto errores.

Parameters cPropietario, cArticulo

Private cWhere, cCodPro, cCodArt
Local lStado

*> Asignar propiedades a variables locales.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cCodArt = Iif(Type('cArticulo')=='C', cArticulo, This.CodArt)

   *> Obtener par�metros del art�culo, si hay.
   cWhere = "F25cCodPro='" +  cCodPro + "' And F25cCodArt='" + cCodArt + "'"
   lStado = f3_sql('*', 'F25c', cWhere, , , "F25cPrm")

   If !lStado
	   *> Obtener los par�metros del propietario, si hay.
	   cWhere = "F25cCodPro='" +  cCodPro + "' And F25cCodArt='" + Space(13) + "'"
	   lStado = f3_sql('*', 'F25c', cWhere, , , "F25cPrm")
   EndIf

   If !lStado
	   *> Obtener los par�metros generales.
	   cWhere = "F25cCodPro='" + Space(6) + "' And F25cCodArt='" + Space(13) + "'"
	   lStado = f3_sql('*', 'F25c', cWhere, , , "F25cPrm")
   EndIf

   Select F25cPrm
   Go Top
   With This
	  .SerPic = F25cSerPic
	  .DejPic = F25cDejPic
   EndWith

Use In (Select("F25cPrm"))
Return lStado

ENDPROC
PROCEDURE _cargarprmrng

*> Cargar los par�metros del art�culo, ubicaciones de picking rangos.
*> Funci�n privada de uso interno de la clase.

*> Recibe:
*>	- Tipo de ubicaci�n a cargar.
*>	- This.CodPro, Propietario.
*>	- This.CodArt, Art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.ReposAutoMPK, reposiciones autom�ticas de cajas.

Parameters cTipoUbicacion

Private cWhere, cOrder, oF08u, oF10c
Local lStado

This.UsrError = ""

cWhere = "F08uCodPro='" + This.CodPro + "' And " + ;
         "F08uArtDsd<='" + This.CodArt + "' And F08uArtHst>='" + This.CodArt + "'"

cFromF = "F08u"
cOrder = "F08uCodPro, F08uUbiDsd, F08uUbiHst"

lStado = f3_sql('*', cFromF, cWhere, cOrder, , 'F08uPRng')
If !lStado
	*> No hay rangos.
	Use In (Select ("F08uPRng"))

	Return lStado
EndIf

*> Cargar datos en el cursor general de trabajo.
Select F08uPRng
Go Top
Do While !Eof()
	Scatter Name oF08u

	*> Cargar las ubicaciones de picking del rango actual.
	cWhere =          _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And "
	cWhere = cWhere + "F10cCodUbi Between '" + oF08u.F08uUbiDsd + "' And '" + oF08u.F08uUbiHst + "' And F10cPickSn='" + cTipoUbicacion + "'"
	lStado = f3_sql("*", "F10c", cWhere, , , "F10cPRng")

	Select F10cPRng
	Go Top
	Do While !Eof()
		Scatter Name oF10c

		*> Validar la ubicaci�n.
		If !This.UbicObj.UbiPicAsignada(oF10c.F10cCodUbi) .And. ;
		   !This.UbicObj.UbiOcupada(oF10c.F10cCodUbi) .And. ;
		   !This.UbicObj.UbiOcupadaMP(oF10c.F10cCodUbi)

			*> Ubicaci�n OK.
			Select F12eRepMin
			Append Blank
			Replace F12eCodPro With This.CodPro
			Replace F12eCodArt With This.CodArt
			Replace F12eCodTal With This.CodTal
			Replace F12eCodCol With This.CodCol
			Replace F12ePriori With 1
			Replace F12eCodUbi With oF10c.F10cCodUbi
			Replace F12eStkMin With 0
			Replace F12eStkMax With This.WFacPal			&& Calculado en This._cargarprmart
		EndIf

		Select F10cPRng
		Skip
	EndDo

	Select F08uPRng
	Skip
EndDo

Use In (Select ("F10cPRng"))
Use In (Select ("F08uPRng"))

Return lStado

ENDPROC
PROCEDURE _cargarprmgen

*> Cargar los par�metros del art�culo, ubicaciones de picking general.
*> Funci�n privada de uso interno de la clase.

*> Recibe:
*>	- Tipo de ubicaci�n a cargar.
*>	- This.CodPro, Propietario.
*>	- This.CodArt, Art�culo.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.ReposAutoMPK, reposiciones autom�ticas de cajas.

Parameters cTipoUbicacion

Private cWhere, cOrder, oF10c
Local lStado

This.UsrError = ""

*> Cargar las ubicaciones de picking general
cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And F10cPickSn='" + cTipoUbicacion + "'"
lStado = f3_sql("*", "F10c", cWhere, , , "F10cPGen")

Select F10cPGen
Go Top
Do While !Eof()
	Scatter Name oF10c

	*> Validar la ubicaci�n.
	If !This.UbicObj.UbiPicAsignada(oF10c.F10cCodUbi) .And. ;
	   !This.UbicObj.UbiOcupada(oF10c.F10cCodUbi) .And. ;
	   !This.UbicObj.UbiOcupadaMP(oF10c.F10cCodUbi)

		*> Ubicaci�n OK.
		Select F12eRepMin
		Append Blank
		Replace F12eCodPro With This.CodPro
		Replace F12eCodArt With This.CodArt
		Replace F12eCodTal With This.CodTal
		Replace F12eCodCol With This.CodCol
		Replace F12ePriori With 1
		Replace F12eCodUbi With oF10c.F10cCodUbi
		Replace F12eStkMin With 0
		Replace F12eStkMax With This.WFacPal			&& Calculado en This._cargarprmart
	EndIf

	Select F10cPGen
	Skip
EndDo

Use In (Select ("F10cPGen"))

Return lStado

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de odificaciones:

*> 11.06.2008 (AVC) Adaptar longitud de los segmentos del lote.
*>					Modificado m�todo This._erepant
*>					Modificado m�todo This._getocurep
*>					Modificado m�todo This.ReposAuto
*>					Modificado m�todo This.ReposAutoMPK
*>					Modificado m�todo This.ReposAutoMPU

ENDPROC
PROCEDURE Destroy

*> Eliminar cursores de trabajo.

Use In (Select("F12cRepMin"))
Use In (Select("F12eRepMin"))
Use In (Select("F14cRepAnt"))
Use In (Select("F16cOcuRep"))
Use In (Select("F16cMPs"))
Use In (Select("F14cReposAuto"))
Use In (Select("F14cRepSum"))

=DoDefault()

ENDPROC


EndDefine 
