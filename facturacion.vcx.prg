Define Class orafncfact as custom
Height = 14
Width = 99



PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

*> Historial de modificaciones:
*> 21.05.2008 (AVC) Agregar propiedad This.AgrFac


With This
	*> Propiedades de uso interno de la clase.
	.InitLocals

	.CodPro = Space(6)			&& Propietario.
	.CodTar = Space(4)			&& N� de tarifa.
	.BorrarTar = "N"			&& Borrar tarifa anterior en creaci�n.

	.CliDsd = ""				&& Cliente inicial.
	.CliHst = ""				&& Cliente final.
	.ArtDsd = ""				&& Art�culo inicial.
	.ArtHst = ""				&& Art�culo final.
	.FchDsd = ""				&& Fecha inicial.
	.FchHst = ""				&& Fecha final.
	.TarDsd = ""				&& Tarifa origen.
	.TarHst = ""				&& Tarifa destino.

	.Descri = ""				&& Descripci�n (de uso general).
	.Abrevi = ""				&& Descripci�n abreviada (de uso general).

	.FecCie = .F.				&& Fecha de cierre.
	.FecCal = .F.				&& Fecha de c�lculo.
	.CrtFra = "S"				&& Operaci�n si factura ya existe.
	.NumFra = ""				&& N� de factura.
	.FecFra = .F.				&& Fecha factura.

	.AgrFra = "N"				&& Agrupar factura.

	.UsrError = ""				&& Texto errores.
EndWith

Return

ENDPROC
PROCEDURE initlocals

*> Inicializar las propiedades protegidas de la clase.

With This
	.DefaultCon = .F.			&& Tipo concepto facturaci�n por defecto.
	.DefaultIva = .F.			&& Tipo impuesto por defecto.

	.CurCli = .F.				&& Cliente activo.
	.CurTar = .F.				&& Tarifa activa.
	.CurFra = "N"				&& Existe factura actual.
	._numfraget = ""			&& N� de factura obtenida.

	*> Constantes generales de la aplicaci�n. Se definen en MSI2.INI
	._valorminimoentero = Iif(Type('_VALORMINIMOENTERO')=='N', _VALORMINIMOENTERO, 0)
	._valormaximoentero = Iif(Type('_VALORMAXIMOENTERO')=='N', _VALORMAXIMOENTERO, 999999)
	._valorminimofloat  = Iif(Type('_VALORMINIMOFLOAT')=='N', _VALORMINIMOFLOAT, 0)
	._valormaximofloat  = Iif(Type('_VALORMAXIMOFLOAT')=='N', _VALORMAXIMOFLOAT, 99999999)
EndWith

Return

ENDPROC
PROCEDURE generartarifa

*> Generar la plantilla de una tarifa, a partir de la plantilla de conceptos.

*> Recibe:
*>	- cPropietario, c�digo de cliente.
*>	- cTarifa, c�digo de tarifa.
*>	- cBorrar, flag de borrado de la tarifa antigua, si ya existe.
*> o bien
*>	- This.CodPro, This.CodTar, This.BorrarTar par�metros recibidos como propiedades.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cPropietario, cTarifa, cBorrar

Local lStado

Store "" To This.UsrError
lStado = .T.

*> Asignar par�metros recibidos a propiedades.
With This
	.CodPro = Iif(Type('cPropietario')=='C', cPropietario, .CodPro)
	.CodTar = Iif(Type('cTarifa')=='C', cTarifa, .CodTar)
	.BorrarTar = Iif(Type('cBorrar')=='C', cBorrar, .BorrarTar)
EndWith

*> Validar tarifa / conceptos.
lStado = This._validarplantilla()
If !lStado
	*> El mensaje de error ya viene asignado.
	Return lStado
EndIf

*> Generar la tarifa y el escalado de precios por defecto.
*> Los posibles mensajes de error ya vienen asignados.
lStado = This._generartarifa()

Return lStado

ENDPROC
PROCEDURE _validarplantilla

*> Validar pantilla de conceptos y estado tarifas.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CodPro, c�digo de cliente.
*>	- This.CodTar, c�digo de tarifa.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.
*>	- This.DefaultCon, Concepto almacenaje por defecto.
*>	- This.DefaultIva, Tipo de IVA pro defecto.

*> Llamado desde:
*>	- This.GenerarTarifa, generar plantilla de tarifas.
*>	- This.EliminarTarifa, borrar plantilla de tarifas.
*>	- This.CopiarTarifa, copiar conceptos de tarifas.

Private cWhere, cField
Local lStado

This.UsrError = ""
lStado = .T.

*> Validar el c�digo de cliente.
m.F32cCodPro = This.CodPro
If !f3_seek("F32c")
	This.UsrError = "No hay datos de facturaci�n"
	Return lStado
EndIf

*> Validar la tarifa.
m.F38cCodPro = This.CodPro
m.F38cCodTar = This.CodTar
If !f3_seek("F38c")
	This.UsrError = "La tarifa no existe"
	Return lStado
EndIf

*> Validar el tipo de concepto por defecto.
lStado = f3_sql("*", "F34n", , , , "F34nCur")
If !lStado
	This.UsrError = "No se pueden cargar los tipos de concepto"
	Use In (Select ("F34nCur"))
	Return lStado
EndIf

Select F34nCur
Locate For F34nNumCon > 0
If !Found()
	This.UsrError = "No hay tipo concepto por defecto"
	Use In (Select ("F34nCur"))
	lStado = .F.
	Return lStado
EndIf

This.DefaultCon = F34nCodCon
Use In (Select ("F34nCur"))

*> Validar el tipo de impuesto por defecto.
lStado = f3_sql("*", "F34v", , , , "F34vCur")
If !lStado
	This.UsrError = "No se pueden cargar los tipos de impuesto"
	Use In (Select ("F34vCur"))
	Return lStado
EndIf

Select F34vCur
Go Top
This.DefaultIva = F34vCodCon
Use In (Select ("F34vCur"))

*!*	*> Validar si ya existe una plantilla de tarifa.
*!*	cField = "Min(F38lRowId) As F38lRowId"
*!*	cWhere = "F38lCodPro='" + This.CodPro + "' And F38lCodTar='" + This.CodTar + "'"
*!*	lStado = f3_sql(cField, "F38l", cWhere, , , "F38lCur")

*!*	Use In (Select ("F38lCur"))

*!*	If lStado .And. This.BorrarTar<>"S"
*!*		*> Ya existe la plantilla y no se permite el borrado.
*!*		This.UsrError = "Ya existe la tarifa"
*!*		lStado = .F.
*!*		Return lStado
*!*	EndIf

*> Validar si existe la correspondiente plantilla de conceptos.
cField = "Min(C37lRowId) As C37lRowId"
cWhere = "C37lCodPro='" + This.CodPro + "' And C37lCodTar='" + This.CodTar+ "'"
lStado = f3_sql(cField, "C37l", cWhere, , , "C37lCur")

Use In (Select ("C37lCur"))

If !lStado
	*> No existe la plantilla.
	This.UsrError = "No existe la plantilla de conceptos"
EndIf

Return lStado

ENDPROC
PROCEDURE _generartarifa

*> Generar la plantilla de tarifas de precios.
*> Las validaciones ya se han hecho en la funci�n que llame a este m�todo.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CodPro, c�digo de cliente.
*>	- This.CodTar, c�digo de tarifa.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.GenerarTarifa, generar plantilla de tarifas.

Private cWhere, cField, oC37l, oF38l
Local lStado, cDescri

This.UsrError = ""
lStado = .T.

*> Validar si ya existe una plantilla de tarifa.
cField = "Min(F38lRowId) As F38lRowId"
cWhere = "F38lCodPro='" + This.CodPro + "' And F38lCodTar='" + This.CodTar + "'"
lStado = f3_sql(cField, "F38l", cWhere, , , "F38lCur")

Use In (Select ("F38lCur"))

If lStado .And. This.BorrarTar<>"S"
	*> Ya existe la plantilla y no se permite el borrado.
	This.UsrError = "Ya existe la tarifa"
	lStado = .F.
	Return lStado
EndIf

*> Cursor de trabajo.
=CrtCursor("F38l", "F38LTARI")
Select F38LTARI
Append Blank

*> Cargar la plantilla de conceptos.
cWhere = "C37lCodPro='" + This.CodPro + "' And C37lCodTar='" + This.CodTar+ "'"
lStado = f3_sql("*", "C37l", cWhere, , , "C37lCUR")

Select C37lCUR
Go Top
Do While !Eof()
	*> Con el RowID del registro, eliminar las posibles claves anteriores.
	Scatter Name oC37l
	=This._eliminarconcepto(oC37l.C37lRowID)

	*> Obtener la descripci�n por defecto para la factura.
	cDescri = ""
	m.C36cCodCon = oC37l.C37lCodCon
	If f3_seek("C36c")
		cDescri = C36c.C36cDescri
	EndIf

	*> Generar datos. Default??? se obtienen en This._validartarifa.
	Select F38LTARI
	Go Top
	Replace F38lCodPro With oC37l.C37lCodPro, ;
			F38lCodTar With oC37l.C37lCodTar, ;
			F38lCodCon With oC37l.C37lCodCon, ;
			F38lCodTrm With oC37l.C37lCodTrm, ;
			F38lTipCon With This.DefaultCon, ;
			F38lCodIva With This.DefaultIva, ;
			F38lDescri With cDescri, ;
	        F38lPrtCnt With "S", ;
	        F38lPrtPrc With "S", ;
	        F38lPrtImp With "S", ;
	        F38lRowID  With Ora_NewNum("NROW"), ;
	        F38lRowCab With oC37l.C37lRowID

	Scatter Name oF38l
	=f3_instun("F38l", "F38LTARI")

	*> Con el registro generado, crear el escalado b�sico de precios.
	=This._generarescalado(oF38l.F38lRowID)

	Select C37lCUR
	Skip
EndDo

Use In (Select ("C37LCUR"))
Use In (Select ("F38LTARI"))

Return

ENDPROC
PROCEDURE _generarescalado

*> Generar el escalado de precios por defecto para una tarifa.
*> M�todo privado de la clase.

*> Recibe:
*>	- cID, Id del registro de la plantilla de tarifas (F38l).

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generartarifa(), generar plantilla de tarifas.

Parameters cRowID

Private cWhere, oF38l
Local lStado, nValorminimo, nValorMaximo

This.UsrError = ""
lStado = .T.

*> Cursor de trabajo.
=CrtCursor("F38e", "F38ETARI")

*> Cargar el registro de la plantilla de tarifas.
m.F38lRowID = cRowID
lStado = f3_seek("F38l")
If !lStado
	This.UsrError = "No existe la plantilla de la tarifa"
	Use In (Select ("F38ETARI"))
	Return lStado
EndIf

Select F38l
Scatter Name oF38l

*> Obtener rangos por defecto del escalado.
nValorMinimo = This._valorminimoentero
nValorMaximo = This._valormaximoentero

m.C35cCodPro = oF38l.F38lCodPro
m.C35cCodTrm = oF38l.F38lCodTrm
If f3_seek("C35c")
	nValorMinimo = C35c.C35cValIni
	nValorMaximo = C35c.C35cValFin
EndIf

*> Generar datos. Las constantes _valorminimo, etc, se obtienen en This.InitLocals y se definen en MSI2.INI
Select F38ETARI
Append Blank

Replace F38eCodPro With oF38l.F38lCodPro, ;
		F38eCodTar With oF38l.F38lCodTar, ;
		F38eCodCon With oF38l.F38lCodCon, ;
		F38eCodTrm With oF38l.F38lCodTrm, ;
        F38eTipCal With "P", ;
        F38ePrecio With This._valorminimofloat, ;
        F38eCanDsd With nValorMinimo, ;
        F38eCanHst With nValorMaximo, ;
        F38eCanMin With This._valorminimoentero, ;
        F38eCanMax With This._valormaximoentero, ;
        F38eImpMin With This._valorminimofloat, ;
        F38eImpMax With This._valormaximofloat, ;
        F38eRowID With Ora_NewNum("NROW"), ;
        F38eRowDet With cRowID

=f3_instun("F38e", "F38ETARI")
Use In (Select ("F38ETARI"))

Return

ENDPROC
PROCEDURE _eliminarconcepto

*> Eliminar la plantilla de una tarifa.
*> M�todo privado de la clase.

*> Recibe:
*>	- cID, Id del concepto actual de la plantilla de conceptos (C37l).

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generartarifa(), generar plantilla de tarifas.
*>	- This._eliminartarifa(), generar plantilla de tarifas.

Parameters cRowID

Private cWhere, oF38l
Local lStado

This.UsrError = ""
lStado = .T.

cWhere = "F38lRowCab='" + cRowID + "'"
lStado = f3_sql("*", "F38l", cWhere, , , "_F38lCur")
If !lStado
	*> No hay datos anteriores a eliminar.
	Use In (Select("_F38lCur"))
	lStado = .T.
	Return lStado
EndIf

Select _F38lCur
Go Top
Do While !Eof()
	*> Eliminar el escalado de precios, ya que es clave dependiente.
	Scatter Name oF38l
	=This._eliminarescalado(oF38l.F38lRowID)

	Select _F38lCur
	Skip
EndDo

*> Eliminar la plantilla de tarifa.
cWhere = "F38lRowCab='" + cRowID + "'"
=f3_deltun("F38l", , cWhere)

Use In (Select("_F38lCur"))

Return

ENDPROC
PROCEDURE _eliminarescalado

*> Eliminar el escalado de precios de una tarifa.
*> M�todo privado de la clase.

*> Recibe:
*>	- cID, Id del escalado de precios (F38e).

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._eliminarconcepto()

Parameters cID

Private cWhere

This.UsrError = ""

cWhere = "F38eRowDet='" + cID + "'"
=f3_deltun("F38e", , cWhere)

Return

ENDPROC
PROCEDURE cierrediario

*> Proceso de cierre diario de facturaci�n, para un art�culo / d�a.

*> Recibe:
*>	- FechaCierre, fecha de c�lculo.
*>	- cPropietario, c�digo de cliente (opcional).
*>	- cArticulo, art�culo (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.ArtDsd, art�culo inicial.
*>	- This.ArtHst, art�culo final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters dFechaCierre, cPropietario, cArticulo

Private cWhere, cField, cGroup, cOrder, cFromF, oF30d
Local lStado, dFechaInicial, nSigno, oF20c

Store .T. To lStado
This.UsrError = ""

*> Cursores de trabajo temporales.
Use In (Select ("F20cInfo"))
Use In (Select ("F30dInfo"))

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.ArtDsd = Iif(Type('cArticulo')=='C', cArticulo, .ArtDsd)
	.ArtHst = Iif(Type('cArticulo')=='C', cArticulo, .ArtHst)
EndWith

*> Calcular la fecha de cierre anterior m�s cercana al d�a a calcular para obtener los valores iniciales.
cWhere = "F30dFecha < " + _GCD(dFechaCierre)
cFromF = "F30d"
cField = "Max(F30dFecha) As F30dFecha"

lStado = f3_sql(cField, cFromF, cWhere, , , 'F30dPrev')

Select F30dPrev
Go Top

If Day(F30dFecha) < 1 .Or. IsNull(F30dFecha)
	lStado = .F.
EndIf

If lStado
	*> Existe fecha de cierre: Obtener los datos de cierre de ese d�a.
	Select F30dPrev
	Go Top
	dFechaInicial = F30dFecha

	cFromF = "F30d"
	cWhere = "F30dCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
			 "F30dCodArt Between '" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
			 "F30dFecha=" + _GCD(dFechaInicial)

	lStado = f3_sql("*", cFromF, cWhere, , , 'F30dInfo')
Else
	*> No hay cierre anterior a la fecha a calcular: Partir de un fichero de cierre vac�o.
	=CrtCursor("F30d", "F30dInfo", "C")
	dFechaInicial = _FecMin
EndIf

Use In (Select ("F30dPrev"))

*> Cargar los movimientos producidos desde la fecha del �ltimo cierre hasta el d�a de c�lculo.
cFromF = "F20c"
cOrder = "F20cCodPro, F20cCodArt, F20cFecMov"
cWhere = "F20cCodPro Between'" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F20cCodArt Between'" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
         "F20cFecMov > " + _GCD(dFechaInicial) + " And F20cFecMov <= " + _GCD(dFechaCierre)

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F20cInfo")

Select F20cInfo
Go Top

Do While !Eof()
	Scatter Name oF20c

	nSigno = Iif(oF20c.F20cEntSal=='E', 1, -1)

	*> A�adir registro calculado de cierre.
	Select F30dInfo
	Locate For F30dCodPro==oF20c.F20cCodPro .And. ;
			   F30dCodArt==oF20c.F20cCodArt .And. ;
			   F30dSitStk==oF20c.F20cSitStk

	If !Found()
		Append Blank
		Replace F30dCodPro With oF20c.F20cCodPro, ;
	    	    F30dCodArt With oF20c.F20cCodArt, ;
	            F30dSitStk With oF20c.F20cSitStk, ;
	            F30dFecha  With dFechaCierre, ;
	            F30dTotVol With 0, ;
	            F30dTotPes With 0, ;
	            F30dStock  With 0
	EndIf

	*> Actualizar la cantidad.
	Replace F30dStock With F30dStock  + (oF20c.F20cCanFis * nSigno)

   *> Tomar el siguiente movimiento del hist�rico.
   Select F20cInfo
   Skip
EndDo

*> Calcular el peso y el volumen de los datos generados.
Select F30dInfo
Delete For F30dStock <= 0
Replace All F30dFecha With dFechaCierre
Go Top

Do While !Eof()
	Scatter Name oF30d

	*> Obtener datos de la ficha de producto.
	m.F08cCodPro = oF30d.F30dCodPro
	m.F08cCodArt = oF30d.F30dCodArt
	=f3_seek("F08c")

	*> Obtener el peso y el volumen de los datos generados.
	=This._pesovolumenmov(oF30d.F30dCodPro, oF30d.F30dCodArt, oF30d.F30dStock)

	Select F30dInfo
	Replace F30dTotVol With This.oUbicObj.PesOcu, ;
			F30dTotPes With This.oUbicObj.VolOcu, ;
			F30dTipPro With F08c.F08cTipPro
	Skip
EndDo

*> Borrar los datos del d�a a calcular, si los hay.
cFromF = "F30d"
cWhere = "F30dCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F30dCodArt Between '" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
		 "F30dFecha=" + _GCD(dFechaCierre)

=f3_DelTun(cFromF, , cWhere, 'N')

*> Insertar los datos del cursor en la tabla de cierre.
Select F30dInfo
Go Top

Do While !Eof()
   =f3_InsTun('F30d', 'F30dInfo', 'N')

   Select F30dInfo
   Skip
EndDo

*> Eliminar datos temporales.
Use In (Select ("F20cInfo"))
Use In (Select ("F30dInfo"))

Return

ENDPROC
PROCEDURE _pesovolumenmov

*> Calcular el peso y el volumen para un art�culo / cantidad dados.
*> M�todo privado de la clase.

*> Recibe:
*>	- Propietario / art�culo / cantidad

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.oUbicObj.PesOcu, peso calculado.
*>	- This.oUbicObj.VolOcu, volumen calculado.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CierreDiario, generar cierre diario a fecha.
*>	- This.FotoAlmacen, generar foto almac�n a fecha.
*>	- This._orderbymac, ordernar movimientos por MAC.
*>	- This._orderbytippro, ordernar movimientos por tipo producto.
*>	- This._calculardoculs, generar acumulados de l�neas de salida.
*>	- This._calcularmovi, generar acumulados de movimientos.

Parameters cPropietario, cArticulo, nCantidad
Local lStado

*> Calcular peso y volumen.
With This.oUbicObj
	.CodPro = cPropietario
	.CodArt = cArticulo
	.CanUbi = nCantidad

	.PesVolAr
EndWith

Return

ENDPROC
PROCEDURE oubicobj_access

*> Inicializar clase ubicaci�n.
If Type('This.oUbicObj')<>'O'
	This.oUbicObj = CreateObject('OraFncUbic')
	This.oUbicObj.Inicializar
	This.oUbicObj.CargaPrm
EndIf

Return This.oUbicObj

ENDPROC
PROCEDURE fotoalmacen

*> Proceso de generaci�n de la foto de almac�n para una fecha dada.

*> Recibe:
*>	- FechaCierre, fecha de c�lculo.
*>	- cPropietario, c�digo de cliente (opcional).
*>	- cArticulo, art�culo (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.ArtDsd, art�culo inicial.
*>	- This.ArtHst, art�culo final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 30.04.2008 (AVC) Permitir registros con stock cero, en caso contrario podr�an quedar fuera de los c�lculos de d�as estancia.

Parameters dFechaCierre, cPropietario, cArticulo

Private cWhere, cField, cGroup, cOrder, cFromF, oF30l
Local lStado, dFechaInicial, nSigno, oF20c

lStado = .T.
This.UsrError = ""

*> Cursores de trabajo temporales.
Use In (Select ("F20cInfo"))
Use In (Select ("F30lInfo"))
Use In (Select ("F30lPrev"))

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.ArtDsd = Iif(Type('cArticulo')=='C', cArticulo, .ArtDsd)
	.ArtHst = Iif(Type('cArticulo')=='C', cArticulo, .ArtHst)
EndWith

*> Calcular la fecha de cierre anterior m�s cercana al d�a a calcular para obtener los valores iniciales.
cWhere = "F30lFecha < " + _GCD(dFechaCierre)
cFromF = "F30l"
cField = "Max(F30lFecha) As F30lFecha"

lStado = f3_sql(cField, cFromF, cWhere, , , 'F30lPrev')

Select F30lPrev
Go Top

If Day(F30lFecha) < 1 .Or. IsNull(F30lFecha)
	lStado = .F.
EndIf

If lStado
	*> Existe fecha de cierre: Obtener los datos de cierre de ese d�a.
	Select F30lPrev
	Go Top
	dFechaInicial = F30lFecha

	cFromF = "F30l"
	cWhere = "F30lCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
			 "F30lCodArt Between '" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
			 "F30lFecha=" + _GCD(dFechaInicial)

	lStado = f3_sql("*", cFromF, cWhere, , , 'F30lInfo')
Else
	*> No hay cierre anterior a la fecha a calcular: Partir de un fichero de cierre vac�o.
	=CrtCursor("F30l", "F30lInfo", "C")
	dFechaInicial = _FecMin
EndIf

Use In (Select ("F30lPrev"))

*> Cargar los movimientos producidos desde la fecha del �ltimo cierre hasta el d�a de c�lculo.
cFromF = "F20c"
cOrder = "F20cCodPro, F20cCodArt, F20cFecMov"
cWhere = "F20cCodPro Between'" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F20cCodArt Between'" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
         "F20cFecMov > " + _GCD(dFechaInicial) + " And F20cFecMov <= " + _GCD(dFechaCierre)

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F20cInfo")

Select F20cInfo
Go Top

Do While !Eof()
	Scatter Name oF20c

	nSigno = Iif(oF20c.F20cEntSal=='E', 1, -1)

	*> Datos del art�culo.
	m.F08cCodPro = oF20c.F20cCodPro
	m.F08cCodArt = oF20c.F20cCodArt
	=f3_seek("F08c")

	*> A�adir registro calculado de cierre.
	Select F30lInfo
	Locate For F30lCodPro==oF20c.F20cCodPro .And. ;
			   F30lCodArt==oF20c.F20cCodArt .And. ;
			   F30lSitStk==oF20c.F20cSitStk .And. ;
			   F30lNumPal==oF20c.F20cNumPal .And. ;
			   F30lCodUbi==oF20c.F20cUbiOri

	If !Found()
		Append Blank
		Replace F30lCodPro With oF20c.F20cCodPro, ;
	    	    F30lCodArt With oF20c.F20cCodArt, ;
	            F30lSitStk With oF20c.F20cSitStk, ;
	            F30lFecha  With dFechaCierre, ;
	            F30lNumPal With oF20c.F20cNumPal, ;
	            F30lNumLot With oF20c.F20cNumLot, ;
	            F30lCodUbi With oF20c.F20cUbiOri, ;
	            F30lTamPal With oF20c.F20cTamPal, ;
	            F30lTipPro With F08c.F08cTipPro, ;
	            F30lTotVol With 0, ;
	            F30lTotPes With 0, ;
	            F30lStock  With 0
	EndIf

	*> Actualizar la cantidad.
	Replace F30lStock With F30lStock  + (oF20c.F20cCanFis * nSigno)

   *> Tomar el siguiente movimiento del hist�rico.
   Select F20cInfo
   Skip
EndDo

*> Borrar los datos del d�a a calcular, si los hay.
cFromF = "F30l"
cWhere = "F30lCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F30lCodArt Between '" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
		 "F30lFecha=" + _GCD(dFechaCierre)

=f3_DelTun(cFromF, , cWhere, 'N')

*> Generar registros para aquellos palets que entran y salen el mismo d�a.
Select F30lInfo
Scan For F30lStock<=0
	Scatter Name oF30l

	cWhere = 		  "F30lCodPro='" + oF30l.F30lCodPro + "' And "
	cWhere = cWhere + "F30lCodArt='" + oF30l.F30lCodArt + "' And "
	cWhere = cWhere + "F30lSitStk='" + oF30l.F30lSitStk + "' And "
	cWhere = cWhere + "F30lFecha=" + _GCD(oF30l.F30lFecha) + " And "
	cWhere = cWhere + "F30lNumPal='" + oF30l.F30lNumPal + "' And "
	cWhere = cWhere + "F30lCodUbi='" + oF30l.F30lCodUbi + "'"

	lStado = f3_sql("*", "F30l", cWhere, , , "__F30LCERO")
	If !lStado
		*> Crear registro con stock cero.
		Select __F30LCERO
		Append Blank
		Gather Name oF30l
	   =f3_InsTun('F30l', '__F30LCERO', 'N')
	EndIf

	Use In (Select("__F30LCERO"))
EndScan

*> Calcular el peso y el volumen de los datos generados.
Select F30lInfo
Delete For F30lStock <= 0
Replace All F30lFecha With dFechaCierre
Go Top

Do While !Eof()
	Scatter Name oF30l

	*> Obtener el peso y el volumen de los datos generados.
	=This._pesovolumenmov(oF30l.F30lCodPro, oF30l.F30lCodArt, oF30l.F30lStock)

	Select F30lInfo
	Replace F30lTotVol With This.oUbicObj.PesOcu, ;
			F30lTotPes With This.oUbicObj.VolOcu
	Skip
EndDo

*> Insertar los datos del cursor en la tabla de cierre.
Select F30lInfo
Go Top

Do While !Eof()
   =f3_InsTun('F30l', 'F30lInfo', 'N')

   Select F30lInfo
   Skip
EndDo

*> Eliminar datos temporales.
Use In (Select ("F20cInfo"))
Use In (Select ("F30lInfo"))

Return

ENDPROC
PROCEDURE diariopalets

*> Actualizar el diario de palets.
*> Parte del proceso de foto almac�n (que debe haberse realizado previamente).

*> Recibe:
*>	- Fecha c�lculo.
*>	- Propietario (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.

*> Devuelve:
*>	- lStado, resultado de la operaci�n (.T. / .F.)
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 01.04.2008 (AVC) Tener en cuenta productos sin peso / volumen para calcular peso y volumen palets.
*>					Permitir recalcular palets.
*> 08.07.2008 (AVC) Corregir rec�lculo de palet.

Parameters dFechaCalculo, cPropietario

Private cWhere, cField, cGroup, cOrder, cFromF, oF30l, oF36p
Local lStado, dFechaCalculoLocal, nPesOcu, nVolOcu, lSttF36p

This.UsrError = ""

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
EndWith

*> Actualizar el estado de los palets abiertos.
lStado = This._cierrepalets()

*> Cursores de trabajo temporales.
Use In (Select ("F30lInfo"))

*> Calcular la fecha de cierre m�s reciente.
cWhere = 		  "F30lCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And "
cWhere = cWhere + "F30lFecha<=" + _GCD(dFechaCalculo)
cFromF = "F30l"

cOrder = ""
cField = "Max(F30lFecha) As F30lFecha"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , "F30lFecMin")
If !lStado
	This.UsrError = "No hay datos para procesar"
	Use In (Select ("F30lFecMin"))
	Return lStado
EndIf

Select F30lFecMin
Go Top
dFechaCalculoLocal = F30lFecha

Use In (Select ("F30lFecMin"))

If IsNull(dFechaCalculoLocal)
	This.UsrError = "No hay datos para procesar"
	Use In (Select ("F30lFecMin"))
	Return .F.
EndIf

*> Preparar los datos del periodo a calcular.
cWhere = 		  "F30lCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And "
cWhere = cWhere + "F30lFecha=" + _GCD(dFechaCalculo)
cOrder = "F30lCodPro, F30lFecha"
cFromF = "F30l"
cField = "*"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , "F30lInfo")
If !lStado
	This.UsrError = "No hay datos para procesar"
	Use In (Select ("F30lInfo"))
	Return lStado
EndIf

*> Crear el cursor de trabajo.
=CrtCursor("F36p", "F36pInfo", "C")

Select F30lInfo
Go Top
Do While !Eof()
	Scatter Name oF30l

	*> Leer el registro correspondiente al palet le�do.
	m.F36pCodPro = oF30l.F30lCodPro
	m.F36pNumPal = oF30l.F30lNumPal

	lF36pStt = f3_seek("F36p")
	Select F36p

	If lF36pStt
		If F36p.F36pEstado > '0'
			*> Ya existe el palet en el registro y est� procesado.
			Select F30lInfo
			Skip
			Loop
		EndIf
		Scatter Name oF36p
	Else
		Scatter Name oF36p Blank
	EndIf

	*> Obtener peso y volumen del palet original.
	m.F16tNumPal = oF30l.F30lNumPal
	If f3_seek("F16t")
		nPesOcu = F16t.F16tPesPal
		nVolOcu = F16t.F16tVolPal
	Else
		*> Recalcular peso / volumen.
		=This._pesovolumenpal(oF30l.F30lNumPal)
		nPesOcu = This.oUbicObj.PesOcu
		nVolOcu = This.oUbicObj.VolOcu
	EndIf

	*> Crear el registro de palets.
	Select F36pInfo
	Append Blank
	Gather Name oF36p

	If lF36pStt
		Replace F36pTotVol With nVolOcu, ;
				F36pTotPes With nPesOcu

		=f3_updtun("F36p", , , , "F36pInfo")
	Else
		Replace F36pCodPro With oF30l.F30lCodPro, ;
				F36pNumPal With oF30l.F30lNumPal, ;
				F36pFecEnt With oF30l.F30lFecha, ;
				F36pFecSal With _FecMax, ;
				F36pFecVal With dFechaCalculo, ;
				F36pTamPal With oF30l.F30lTamPal, ;
				F36pTotVol With nVolOcu, ;
				F36pTotPes With nPesOcu, ;
				F36pEstado With "0"

		=f3_instun("F36p", "F36pInfo")
	EndIf

	Select F30lInfo
	Skip
EndDo

*> Cursores de trabajo temporales.
Use In (Select ("F30lInfo"))
Use In (Select ("F36pInfo"))

Return

ENDPROC
PROCEDURE _cierrepalets

*> Validar el estado de palets del diario palets.
*> Parte del proceso de diario palets (que debe haberse realizado previamente).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CodPro, c�digo de propietario.

*> Devuelve:
*>	- lStado, resultado de la operaci�n (.T. / .F.)
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.DiarioPalets, generaci�n del dario de palets.

*> Historial de modificaciones:
*> 08.07.2008 (AVC) Modificado para que calcule el palet aunque todav�a est� en ocupaciones.

Private cWhere, cField, cGroup, cOrder, cFromF, oF36p
Local lStado, dFechaEntrada, dFechaSalida

Store .T. To lStado
This.UsrError = ""

*> Cursores de trabajo temporales.
Use In (Select ("F36pDiar"))

*> Preparar los datos del periodo a calcular.
cWhere = "F36pCodPro Between'" + This.CliDsd + "' And '" + This.CliHst + "' And F36pEstado='0'"
cOrder = ""
cFromF = "F36p"
cField = "*"

*> Carga los palets del diario en estado de abiertos.
lStado = f3_sql(cField, cFromF, cWhere, cOrder, , "F36pDiar")

Select F36pDiar
Go Top
Do While !Eof()
	Scatter Name oF36p
	dFechaSalida = ""

	*> Buscar el palet en ocupaciones. Si existe,  tomar _FecMax como fecha salida.
	cWhere = "F16cCodPro='" + oF36p.F36pCodPro + "' And F16cNumPal='" + oF36p.F36pNumPal + "'"
	lStado = f3_sql("F16cNumPal", "F16c", cWhere, , , "F16cDiar")
	If lStado
		*> El palet todav�a existe en el almac�n.
		dFechaSalida = _FecMax
	EndIf

	*> Averiguar fecha de entrada / y salida del palet.
	cWhere = "F30lCodPro='" + oF36p.F36pCodPro + "' And F30lNumPal='" + oF36p.F36pNumPal + "'"
	cField = "Min(F30lFecha) As F30lFecMin, Max(F30lFecha) As F30lFecMax"
	lStado = f3_sql(cField, "F30l", cWhere, , , "F30lDiar")

	If lStado
		Select F30lDiar
		Go Top
		If Eof()
			*If Day(F30lFecha) < 1 .Or. IsNull(F30lFecha)
			dFechaEntrada = Date()
			dFechaSalida = Iif(Empty(dFechaSalida), Date(), dFechaSalida)
		Else
			dFechaEntrada = F30lFecMin
			dFechaSalida = Iif(Empty(dFechaSalida), F30lFecMax, dFechaSalida)

			If Type('dFechaEntrada')=='T'
				dFechaEntrada = TToD(dFechaEntrada)
			EndIf
			If Type('dFechaSalida')=='T'
				dFechaSalida = TToD(dFechaSalida)
			EndIf
			*dFechaSalida = dFechaSalida + 1
		EndIf
	EndIf

	*> Cerrar el palet.
	Select F36pDiar
	Replace F36pFecEnt With dFechaEntrada
	Replace F36pFecSal With dFechaSalida
	=f3_updtun("F36p", , , , "F36pDiar")

	Select F36pDiar
	Skip
EndDo

*> Cursores de trabajo temporales.
Use In (Select ("F16cDiar"))
Use In (Select ("F36pDiar"))
Use In (Select ("F30lDiar"))

Return

ENDPROC
PROCEDURE _calcularpale

*> Calcular acumulados de conceptos de facturaci�n - palets de entrada.
*> Trabaja a partir del fichero de diario de palets (F36p).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF36p, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F36pPale"))

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='PALE'"
cFromF = "F38l,C36c"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo PALE: Palets de entrada.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35aCodPro='" + This.CurCli + "' And F35aFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35a", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35a", "F35aPale")

*> Cl�usula de lectura del diario de palets.
cWhere = 		  "F36pCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F36pFecEnt Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst)
cOrder = ""

*> Generar el cursor con los palets a procesar.
lStado = f3_sql("*", "F36p", cWhere, cOrder, , "F36pPale")

Select F36pPale
Go Top

Do While !Eof()
	Scatter Name oF36p

	*> Procesar los diferentes conceptos a generar para el palet actual.
	Select F38lc
	Go Top
	Do While !Eof()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF36p.F36pTamPal<>oF38lc.F38lCodigo
			*> La tarifa discrimina por concepto - tama�o de palet -, y �ste es distinto al del palet actual.
			Skip
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF36p.F36pCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades. En este caso siempre ser� 1.
			Case cTipoCalculo=='U'

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If oF36p.F36pTotPes < nValorIni .Or. oF36p.F36pTotPes > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If oF36p.F36pTotVol < nValorIni .Or. oF36p.F36pTotVol > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Resto de casos: Error
			Otherwise
				Select F38lc
				Skip
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - tama�o palet - (puede estar en blanco).

		*> Clave base del registro.
		cWhere = "F35aCodPro='" + oF36p.F36pCodPro + "' And F35aFecCal=" + _GCD(dFechaCalculo)
		cWhere = cWhere + " And F35aCodTrm='" + oF38lc.F38lCodTrm + "'"
		cWhere = cWhere + " And F35aTamPal='" + oF38lc.F38lCodigo + "'"

*!*			cWhere = "F35aRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35a", cWhere, , , "F35aPale")
		Select F35aPale

		If !lStado
			Append Blank
			Replace F35aCodPro With oF38lc.F38lCodPro
			Replace F35aFecCal With dFechaCalculo
			Replace F35aTamPal With oF38lc.F38lCodigo
			Replace F35aCodTrm With oF38lc.F38lCodTrm
			Replace F35aRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35aBultos With F35aBultos + 1
		Replace F35aPeso   With F35aPeso   + oF36p.F36pTotPes
		Replace F35aVolum  With F35aVolum  + oF36p.F36pTotVol

		If !lStado
			=f3_instun("F35a", "F35aPale")
		Else
			=f3_updtun("F35a", , , , "F35aPale", cWhere)
		EndIf

		Select F38lc
		Skip
	EndDo

	Select F36pPale
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35aPale"))
Use In (Select ("F36pPale"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _calculartpro

*> Calcular acumulados de conceptos de facturaci�n - tipo de producto.
*> Trabaja a partir del fichero de diario de palets (F36p).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

Private cWhere, cOrder, cFromF, oF36p, dFechaCalculo
Local lStado, oF38lc, oF30l, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F36pTPro"))

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='TPRO'"
cFromF = "F38l,C36c"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo TPRO: Tipo de producto.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35bCodPro='" + This.CurCli + "' And F35bFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35b", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35b", "F35bTPro")

*> Cl�usula de lectura del diario de palets.
cWhere = 		  "F36pCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F36pFecEnt Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst)
cOrder = ""

*> Generar el cursor con los palets a procesar.
lStado = f3_sql("*", "F36p", cWhere, cOrder, , "F36pTPro")

Select F36pTPro
Go Top

Do While !Eof()
	Scatter Name oF36p

	*> Procesar los diferentes conceptos a generar para el palet actual.
	Select F38lc
	Go Top
	Do While !Eof()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo)
			*> Obtener el tipo de producto del palet.
			cWhere = "F30lCodPro='" + oF36p.F36pCodPro + "' And F30lNumPal='" + oF36p.F36pNumPal + "'"
			lStado = f3_sql("*", "F30l", cWhere, , , "F30lTPro")

			Select F30lTPro
			Locate For F30lTipPro==oF38lc.F38lCodigo
			If !Found()
				*> La tarifa discrimina por concepto - tipo de producto -, y �ste es distinto al del palet actual.
				Use In (Select ("F30lTPro"))
				Select F38lc
				Skip
				Loop
			EndIf
		EndIf

		Use In (Select ("F30lTPro"))

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF36p.F36pCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades. En este caso siempre ser� 1.
			Case cTipoCalculo=='U'

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If oF36p.F36pTotPes < nValorIni .Or. oF36p.F36pTotPes > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If oF36p.F36pTotVol < nValorIni .Or. oF36p.F36pTotVol > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Resto de casos: Error
			Otherwise
				Select F38lc
				Skip
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - tipo producto - (puede estar en blanco).

		*> Clave base del registro.
*!*			cWhere = "F35bCodPro='" + oF36p.F36pCodPro + "' And F35bFecCal=" + _GCD(dFechaCalculo)
*!*			cWhere = cWhere + " And F35bCodTrm='" + oF38lc.F38lCodTrm + "'"
*!*			cWhere = cWhere + " And F35bTipPro='" + oF38lc.F38lCodigo + "'"

		cWhere = "F35bRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35b", cWhere, , , "F35bTPro")
		Select F35bTPro

		If !lStado
			Append Blank
			Replace F35bCodPro With oF38lc.F38lCodPro
			Replace F35bFecCal With dFechaCalculo
			Replace F35bTipPro With oF38lc.F38lCodigo
			Replace F35bCodTrm With oF38lc.F38lCodTrm
			Replace F35bRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35bBultos With F35bBultos + 1
		Replace F35bPeso   With F35bPeso   + oF36p.F36pTotPes
		Replace F35bVolum  With F35bVolum  + oF36p.F36pTotVol

		If !lStado
			=f3_instun("F35b", "F35bTPro")
		Else
			=f3_updtun("F35b", , , , "F35bTPro", cWhere)
		EndIf

		Select F38lc
		Skip
	EndDo

	Select F36pTPro
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35bTPro"))
Use In (Select ("F36pTPro"))
Use In (Select ("F30lTPro"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE calcularacumulados

*> Calcular acumulados de conceptos.

*> Recibe:
*>	- cPropietario, c�digo de cliente (opcional).
*>	- dFecha, fecha de c�lculo (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 14.06.2006 (AVC)	Agregar conceptos directos.

Parameters cPropietario, dFecha

Local lStado
Private cWhere, oF32c

Store .T. To lStado
This.UsrError = ""

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.FchDsd = Iif(Type('dFecha')=='D', dFecha, .FchDsd)
	.FchHst = Iif(Type('dFecha')=='D', dFecha, .FchHst)
EndWith

*> Cargar un cursor con los clientes a procesar.
cWhere = "F32cCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "'"
lStado = f3_sql("*", "F32c", cWhere, , , "F32cACUM")
If !lStado
	*> No hay clientes para cargar.
	Use In (Select ("F32cACUM"))
	This.UsrError = "No se pueden cargar los clientes"
	Return lStado
EndIf

Select F32cACUM
Go Top
Do While !Eof()
	Scatter Name oF32c

	*> Validar la tarifa del cliente en curso.
	lStado = This._validartarifa(oF32c.F32cCodPro)
	If !lStado
		*> Cliente sin tarifa v�lida.
		Select F32cACUM
		Skip
		Loop
	EndIf

	*> Llamadas a c�lculos de ficheros de acumulados.
	lStado = This._calcularpale()				&& Palets de entrada.
	lStado = This._calcularpals()				&& Palets de salida.
	lStado = This._calculartpro()				&& Tipo producto entradas.
	lStado = This._calculardiap()				&& Estancia d�as.
	lStado = This._calcularocup()				&& Ocupaciones / ubicaciones.
	lStado = This._calculartprs()				&& Tipo producto salidas.
	lStado = This._calculardocu()				&& Documentos.
	lStado = This._calcularmovi()				&& Movimientos.
	lStado = This._calculardire()				&& Conceptos directos.

	Select F32cACUM
	Skip
EndDo

Use In (Select ("F32cACUM"))
Return lStado

ENDPROC
PROCEDURE _validartarifa

*> Validar la tarifa del cliente actual para calcular acumulados.
*> M�todo privado de la clase.

*> Recibe:
*>	- cCodPro, cliente a validar.
*>	- This.FchDsd, fecha inicial periodo.
*>	- This.FchHst, fecha final periodo.

*> Devuelve:
*>	- Estado, resultado de la operaci�n (.T. / .F.)
*>	- This.CurCli, cliente en curso.
*>	- This.CurTar, tarifa activa.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.
*>	- This.CalcularFacturas(), calcular facturas de cliente.

Parameters cCodigo

Private cWhere
Local oTar, lStado

lStado = .T.
This.UsrError = ""

cWhere = "F38cCodPro='" + cCodigo + "'"
lStado = f3_sql("*", "F38c", cWhere, , , "F38cTARI")

Do Case
	Case !lStado
		This.UsrError = "Propietario sin tarifas definidas"

	Otherwise
		Select F38cTARI
		Locate For F38cActiva=='S' .And. F38cFecDes	<= This.FchDsd .And. F38cFecHas >= This.FchHst
		lStado = Found()
		If !lStado
			This.UsrError = "Propietario sin tarifas vigentes"
		Else
			Scatter Name oTar
			This.CurCli = cCodigo
			This.CurTar = oTar.F38cCodTar
		EndIf
EndCase

Use In (Select ("F38cTARI"))
Return lStado

ENDPROC
PROCEDURE _calcularpals

*> Calcular acumulados de conceptos de facturaci�n - palets de salida.
*> Trabaja a partir del hist�rico de movimientos (F20c).
*> Realiza los c�lculos a partir de los movimientos de expedici�n (2999).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cFromF, dFechaCalculo, oF20c
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo
Local cBultoActual

Store .T. To lStado
This.UsrError = ""
cBultoActual = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F20cPalS"))

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='PALS'"
cFromF = "F38l,C36c"

lStado = f3_sql("*", cFromF, cWhere, , , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo PALS: Bultos de salida.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35cCodPro='" + This.CurCli + "' And F35cFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35c", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35c", "F35cPalS")

*> Cl�usula de lectura del hist�rico de movimientos.
cWhere = 		  "F20cCodPro='" + This.CurCli + "' And F20cTipMov='2999' And "
cWhere = cWhere + "F20cFecMov Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst)

*> Generar el cursor con los datos a procesar.
lStado = f3_sql("*", "F20c", cWhere, , , "F20cPalSCur")

*> Ordenar los registros por bulto, priorizando si hay MAC sobre el n� de palet.
*> Devuelve el cursor F20cPalS, agrupado por bulto, peso, etc.
Select F20cPalSCur

=This._orderbymac()

Select F20cPalS
Go Top

Do While !Eof()
	Scatter Name oF20c

	*> Procesar los diferentes conceptos a generar para el palet actual.
	Select F38lc
	Go Top
	Do While !Eof()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF20c.F20cOriRes<>Trim(oF38lc.F38lCodigo)
			*> La tarifa discrimina por concepto - P / C / G / U -, y �ste es distinto al del palet actual.
			Skip
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin = This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF20c.F20cCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If oF20c.F20cBultos < nValorIni .Or. oF20c.F20cBultos > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If oF20c.F20cPesPal < nValorIni .Or. oF20c.F20cPesPal > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If oF20c.F20cVolPal < nValorIni .Or. oF20c.F20cVolPal > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Skip
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - tama�o palet - (puede estar en blanco).

		*> Clave base del registro.
		cWhere = "F35cCodPro='" + oF20c.F20cCodPro + "' And F35cFecCal=" + _GCD(dFechaCalculo)
		cWhere = cWhere + " And F35cCodTrm='" + oF38lc.F38lCodTrm + "'"
		cWhere = cWhere + " And F35cTipBul='" + oF38lc.F38lCodigo + "'"

*!*			cWhere = "F35cRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35c", cWhere, , , "F35cPalS")
		Select F35cPalS

		If !lStado
			Append Blank
			Replace F35cCodPro With oF38lc.F38lCodPro
			Replace F35cFecCal With dFechaCalculo
			Replace F35cTipBul With oF38lc.F38lCodigo
			Replace F35cCodTrm With oF38lc.F38lCodTrm
			Replace F35cRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35cBultos With F35cBultos + oF20c.F20cBultos
		Replace F35cPeso   With F35cPeso   + oF20c.F20cPesPal
		Replace F35cVolum  With F35cVolum  + oF20c.F20cVolPal

		If !lStado
			=f3_instun("F35c", "F35cPalS")
		Else
			=f3_updtun("F35c", , , , "F35cPalS", cWhere)
		EndIf

		Select F38lc
		Skip
	EndDo

	Select F20cPalS
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35cPalS"))
Use In (Select ("F20cPalS"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _pesovolumenpal

*> Calcular el peso y el volumen inicial de un palet.
*> M�todo privado de la clase.

*> Recibe:
*>	- N� de palet.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.oUbicObj.PesOcu, peso calculado.
*>	- This.oUbicObj.VolOcu, volumen calculado.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.DiarioPalets, Generar diario de palets de entrada.

Parameters cPalet
Local lStado

*> Calcular peso y volumen.
With This.oUbicObj
	.NumPal = cPalet
	.CalcPesVolPal
EndWith

Return

ENDPROC
PROCEDURE _orderbymac

*> Ordena los datos de c�lculo de palets de salida.
*> DEBE estar activo el cursor a clasificar, F20cPalSCur, imagen del F20c.
*> M�todo privado de la clase.

*> Recibe:
*>	- Cursor activo, a partir de los datos de hist�rico de movimiento.

*> Devuelve:
*>	- El mismo cursor, ordenado.
*>	- Estado, resultado de la operaci�n (.T. / .F.)
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- _calcularpals(), c�lculo acumulados de salidas.

Local lStado, cOldSele
Private oPal, oPlr, nFCaj, nFPal, nCaj, nPal, nUni

This.UsrError = ""
lStado = .T.

cOldSele = Select()
If Empty(cOldSele)
	This.UsrError = "No hay ning�n fichero activo"
	lStado = .F.
	Return lStado
EndIf

*> Si hay MAC, �ste sustituye al n� de palet.
Replace All F20cNumPal With F20cNumMac For !Empty(F20cNumMac)

*> Generar el cursor de trabajo real, F20cPalS.
=CrtFCursor("F20cPalS", [TBL=F20c,FLD=F20cCodPro,FLD=F20cNumPal,FLD=F20cOriRes])
=AddFldToCursor("F20cPalS", [NAME=F20cBultos,TYPE=N,LENGTH=3,DECIMALS=0])
=AddFldToCursor("F20cPalS", [NAME=F20cPesPal,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor("F20cPalS", [NAME=F20cVolPal,TYPE=N,LENGTH=12,DECIMALS=3])

*> Acumular datos sobre el cursor real de trabajo.
Select F20cPalSCur
Go Top
Do While !Eof()
	Scatter Name oPal

	*> Peso y volumen del movimiento.
	=This._pesovolumenmov(oPal.F20cCodPro, oPal.F20cCodArt, oPal.F20cCanFis)

	*> Calcular bultos seg�n tipo (P/C/U).
	Select F20cPalS
	Locate For F20cCodPro==oPal.F20cCodPro .And. F20cNumPal==oPal.F20cNumPal
	If !Found()
		Append Blank
		Replace F20cCodPro With oPal.F20cCodPro
		Replace F20cNumPal With oPal.F20cNumPal
		Replace F20cOriRes With oPal.F20cOriRes
		Replace F20cBultos With 1
	EndIf

	Replace F20cPesPal With F20cPesPal + This.oUbicObj.PesOcu
	Replace F20cVolPal With F20cVolPal + This.oUbicObj.VolOcu

	Scatter Name oPlr

	*> Calcular bultos seg�n factores (L/J/N).
	nFCaj = oPal.F20cUniPac * oPal.F20cPacCaj
	nFPal = nFCaj * oPal.F20cCajPal

	nUni = oPal.F20cCanFis
	nPal = Floor(nUni / nFPal)
	nUni = nUni - (nPal * nFPal)
	nCaj = Floor(nUni / nFCaj)
	nUni = nUni - (nCaj * nFCaj)

	If nPal > 0
		Append Blank
		Gather Name oPlr
		Replace F20cOriRes With 'L'
		Replace F20cBultos With nPal
	EndIf

	If nCaj > 0
		Append Blank
		Gather Name oPlr
		Replace F20cOriRes With 'J'
		Replace F20cBultos With nCaj
	EndIf

	If nUni > 0
		Append Blank
		Gather Name oPlr
		Replace F20cOriRes With 'N'
		Replace F20cBultos With 1			&& With nUni
	EndIf

	Select F20cPalSCur
	Skip
EndDo

Use In (Select ("F20cPalSCur"))

Return

ENDPROC
PROCEDURE _calculardiap

*> Calcular acumulados de conceptos de facturaci�n - d�as de estancia de palets.
*> Trabaja a partir del fichero de diario de palets (F36p).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF36p, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo
Local dFechaEntrada, dFechaSalida, nDiasPeriodo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F36pDiap"))

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='DIAP'"
cFromF = "F38l,C36c"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo DIAP: Acumulado estancia d�as por palet.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35dCodPro='" + This.CurCli + "' And F35dFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35d", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35d", "F35dDiap")

*> Cl�usula de lectura del diario de palets.
cWhere = 		  "F36pCodPro='" + This.CurCli + "' And F36pEstado='0'"
*> cWhere = cWhere + "F36pFecEnt Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst)
cOrder = ""

*> Generar el cursor con los palets a procesar.
lStado = f3_sql("*", "F36p", cWhere, cOrder, , "F36pDiap")

Select F36pDiap
Go Top

Do While !Eof()
	Scatter Name oF36p

	*> Calcular los d�as de este palet para el periodo de c�lculo.
	dFechaEntrada = Iif(oF36p.F36pFecEnt < This.FchDsd, This.FchDsd, TToD(oF36p.F36pFecEnt))
	dFechaSalida = Iif(oF36p.F36pFecSal > This.FchHst, This.FchHst, TToD(oF36p.F36pFecSal))
	nDiasPeriodo = (dFechaSalida - dFechaEntrada) + 1

	*> Procesar los diferentes conceptos a generar para el palet actual.
	Select F38lc
	Go Top
	Do While !Eof()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF36p.F36pTamPal<>oF38lc.F38lCodigo
			*> La tarifa discrimina por concepto - tama�o de palet -, y �ste es distinto al del palet actual.
			Skip
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF36p.F36pCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades (d�as).
			Case cTipoCalculo=='U'
				If nDiasPeriodo < nValorIni .Or. nDiasPeriodo > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If oF36p.F36pTotPes < nValorIni .Or. oF36p.F36pTotPes > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If oF36p.F36pTotVol < nValorIni .Or. oF36p.F36pTotVol > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Resto de casos: Error
			Otherwise
				Select F38lc
				Skip
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - tama�o palet - (puede estar en blanco).
		*>	- Trabajar con palets individuales.

		*> Clave base del registro.
		cWhere = "F35dCodPro='" + oF36p.F36pCodPro + "' And F35dFecCal=" + _GCD(dFechaCalculo)
		cWhere = cWhere + " And F35dNumPal='" + oF36p.F36pNumPal + "'"

		*> Teoricamente no debe existir el registro.
		lStado = f3_sql("*", "F35d", cWhere, , , "F35dDiap")
		Select F35dDiap

		If !lStado
			Append Blank
			Replace F35dCodPro With oF38lc.F38lCodPro
			Replace F35dFecCal With dFechaCalculo
			Replace F35dNumPal With oF36p.F36pNumPal
			Replace F35dRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35dNumDia With nDiasPeriodo
		Replace F35dTamPal With oF38lc.F38lCodigo
		Replace F35dCodTrm With oF38lc.F38lCodTrm
		Replace F35dPeso   With F35dPeso   + oF36p.F36pTotPes
		Replace F35dVolum  With F35dVolum  + oF36p.F36pTotVol

		If !lStado
			=f3_instun("F35d", "F35dDiap")
		Else
			=f3_updtun("F35d", , , , "F35dDiap", cWhere)
		EndIf

		Exit
	EndDo

	Select F36pDiap
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35dDiap"))
Use In (Select ("F36pDiap"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _calcularocup

*> Calcular acumulados de conceptos de facturaci�n - ocupaciones / ubicaciones.
*> Trabaja a partir de la foto de almac�n (F30l) y diario palets (F36p).

*> Primera parte: Calcular conceptos directos.
*> A diferencia de otros tipos de acumulados, aqu� se calculan conceptos totalizadores, de forma
*> predeterminada y, a continuaci�n se calculan los conceptos derivados (m�ximos, m�nimos, media, ...)
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

Private cWhere, cOrder, cFromF, cGroup, cField, oF30l, dFechaCalculo
Local lStado, oC38lc, cCurUbi

Store .T. To lStado
This.UsrError = ""
cCurUbi = Space(1)

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Crear el cursor de trabajo.
Use In (Select ("F30lOcup"))

=CrtFCursor("F30lOcup", [TBL=F30l,FLD=F30lCodPro,FLD=F30lCodUbi,FLD=F30lNumPal,FLD=F30lFecha])
=CrtFCursor("F30lOcup", [TBL=F36p,FLD=F36pTotVol,FLD=F36pTotPes])
=CrtFCursor("F30lOcup", [TBL=F10c,FLD=F10cPickSn])

*> Tomar la lista de conceptos de ocupaci�n.
*> (N. de P.) A diferencia de otros c�lculos, aqu� NO toma los conceptos de la plantilla de tarifas.
cWhere = ""
cFromF = "C38c"

lStado = f3_sql("*", cFromF, cWhere, , , "C38lc")
If !lStado
	*> No hay conceptos OCUP definidos.
	This.UsrError = "No hay conceptos OCUP definidos"
	Use In (Select ("C38lc"))
	Return lStado
EndIf

*> Crear cursor para los datos a generar.
=CrtCursor("F35e", "F35eOcup")					&& Acumulado.
=CrtCursor("F35e", "F35eOcupD")					&& Acumulados d�a para calcular promedios.

*> Cl�usula de lectura de los ficheros de trabajo.
cWhere = 		  "F30lCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F30lFecha Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F10cCodUbi=F30lCodUbi And "
cWhere = cWhere + "F36pCodPro=F30lCodPro And F36pNumPal=F30lNumPal"

cField = "Distinct F30lCodPro, F30lCodUbi,F30lNumPal,  F30lFecha, F10cPickSn, F36pTotVol, F36pTotPes"
cFromF = "F30l,F10c,F36p"
cOrder = "F30lCodPro, F30lCodUbi, F30lNumPal"
cGroup = ""

*> Generar el cursor con los palets a procesar.
lStado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F30lOcup")

Select F30lOcup
Go Top

*> Primer paso: Generar los acumulados totales, a partir de los cuales se generar�n
*> los c�lculos de los promedios, m�nimo, m�ximo, etc.
Do While !Eof()
	Scatter Name oF30l

	*> Procesar los diferentes conceptos directos a generar para la ocupaci�n actual.
	Select C38lc
	Locate For C38cDirect=='S'
	Do While Found()
		Scatter Name oC38lc

		*> Comparar el tipo de ubicaci�n con el tipo de concepto.
		Do Case
			*> Palets completos.
			Case SubStr(oC38lc.C38cCodCon, 4, 1)=='P'
				If oF30l.F10cPickSn<>'N'
					Select C38lc
					Continue
					Loop
				EndIf

			*> Cajas completas.
			Case SubStr(oC38lc.C38cCodCon, 4, 1)=='C'
				If oF30l.F10cPickSn<>'S'
					Select C38lc
					Continue
					Loop
				EndIf

			*> Agrupaciones.
			Case SubStr(oC38lc.C38cCodCon, 4, 1)=='G'
				If oF30l.F10cPickSn<>'G'
					Select C38lc
					Continue
					Loop
				EndIf

			*> Picking.
			Case SubStr(oC38lc.C38cCodCon, 4, 1)=='U'
				If oF30l.F10cPickSn<>'U'
					Select C38lc
					Continue
					Loop
				EndIf

			*> Totalizar.
			Case SubStr(oC38lc.C38cCodCon, 4, 1)=='T'
				If oF30l.F10cPickSn=='E'
					Select C38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Ignorar.
			Otherwise
				Select C38lc
				Continue
				Loop
		EndCase

		*> Acumular en totales por ubicaci�n � en totales por ocupaci�n.
		If SubStr(oC38lc.C38cCodCon, 1, 1)=='U'
			If (oF30l.F30lCodPro + oF30l.F30lCodUbi)==cCurUbi
				*> Es la misma ubicaci�n.
				Select C38lc
				Continue
				Loop
			EndIf
		EndIf

		*> Obtener el registro activo de acumulados periodo.
		Select F35eOcup
		Locate For F35eCodPro==oF30l.F30lCodPro .And. ;
				   F35eFecCal==dFechaCalculo .And. ;
				   F35eCodigo==oC38lc.C38cCodCon

		If !Found()
			Append Blank
			Replace F35eCodPro With oF30l.F30lCodPro
			Replace F35eFecCal With dFechaCalculo
			Replace F35eCodigo With oC38lc.C38cCodCon
		EndIf

		Replace F35eBultos With F35eBultos + 1
		Replace F35ePeso   With F35ePeso   + oF30l.F36pTotPes
		Replace F35eVolum  With F35eVolum  + oF30l.F36pTotVol

		*> Obtener el registro activo de acumulados d�a.
		Select F35eOcupD
		Locate For F35eCodPro==oF30l.F30lCodPro .And. ;
				   F35eFecCal==oF30l.F30lFecha .And. ;
				   F35eCodigo==oC38lc.C38cCodCon

		If !Found()
			Append Blank
			Replace F35eCodPro With oF30l.F30lCodPro
			Replace F35eFecCal With oF30l.F30lFecha
			Replace F35eCodigo With oC38lc.C38cCodCon
			Replace F35eRowCon With ""
		EndIf

		Replace F35eBultos With F35eBultos + 1
		Replace F35ePeso   With F35ePeso   + oF30l.F36pTotPes
		Replace F35eVolum  With F35eVolum  + oF30l.F36pTotVol

		Select C38lc
		Continue
	EndDo

	*> Memorizar la ubicaci�n actual.
	cCurUbi = oF30l.F30lCodPro + oF30l.F30lCodUbi

	Select F30lOcup
	Skip
EndDo

*> Realizar el c�lculo de los conceptos secundarios (promedio, m�nimos, m�ximos, etc), que dependen
*> de los conceptos calculados en este proceso. Este segundo proceso parte de los mismos datos.
lStado = This._calcularocups()

*> Cursores de trabajo.
Use In (Select ("F30lOcup"))
Use In (Select ("F35eOcup"))
Use In (Select ("F35eOcupD"))
Use In (Select ("C38lc"))

Return

ENDPROC
PROCEDURE _calcularocups

*> Calcular acumulados de conceptos de facturaci�n - ocupaciones / ubicaciones.

*> Segunda parte: Calcular conceptos no directos (m�nimo, m�ximo, media, ...)
*> Trabaja a partir de la foto de almac�n (F30l) y diario palets (F36p).
*> A diferencia de otros tipos de acumulados, aqu� se calculan conceptos totalizadores, de forma
*> predeterminada y, a continuaci�n se calculan los conceptos derivados (m�ximos, m�nimos, media, ...)
*> M�todo privado de la clase.

*> Recibe:
*>	- F35eOcup, cursor con los totales periodo generados en This._calcularocup.
*>	- F35eOcupD, cursor con los totales d�a generados en This._calcularocup.
*>	- C38lc, cursor con los conceptos de ocupaci�n, generado en This._calcularocup.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calcularocup(), calcular conceptos directos de ocupaci�n.

Private cWhere, dFechaCalculo
Local lStado, oF38lc, oC38c, cConcepto, nFind, nBultos, nPeso, nVolumen

Store .T. To lStado
This.UsrError = ""
cCurUbi = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='OCUP'"
cFromF = "F38l,C36c"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo PALE: Palets de entrada.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35eCodPro='" + This.CurCli + "' And F35eFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35e", , cWhere)

*> A partir de los conceptos de ocupaci�n NO directos, calcular promedios de los conceptos de ocupaci�n directos.
Select F38lc
Go Top
Do While !Eof()
	Scatter Name oF38lc

	*> Procesar los diferentes conceptos de c�lculo a generar.
	Select C38lc
	Locate For C38cCodCon==oF38lc.F38lCodigo
	If !Found()
		Select F38lc
		Skip
		Loop
	EndIf

	Scatter Name oC38c

	*> Obtener el concepto directo de referencia.
	Do Case
		*> Calcular a partir de conceptos DIRECTOS.
		Case oC38c.C38cDirect=='S'
			cConcepto = oC38c.C38cCodCon

		*> Calcular a partir de UBICACIONES.
		Case SubStr(oC38c.C38cCodCon, 3, 1)=='U'
			cConcepto = "UBI" + SubStr(oC38c.C38cCodCon, 4, 1)

		*> Calcular a partir de OCUPACIONES.
		Case SubStr(oC38c.C38cCodCon, 3, 1)=='O'
			cConcepto = "OCU" + SubStr(oC38c.C38cCodCon, 4, 1)

		*> Resto de casos: Error.
		Otherwise
			Select F38lc
			Skip
			Loop
	EndCase

	*> Obtener el tipo de c�lculo a realizar.
	Do Case
		*> Valor TOTAL.
		Case oC38c.C38cDirect=='S'
			Select F35eOcupD
			Calculate Sum(F35eBultos) For F35eCodigo==cConcepto To nBultos
			Calculate Sum(F35ePeso)   For F35eCodigo==cConcepto To nPeso
			Calculate Sum(F35eVolum)  For F35eCodigo==cConcepto To nVolumen

		*> Valor M�NIMO.
		Case SubStr(oC38c.C38cCodCon, 1, 2)=='MI'
			Select F35eOcupD
			Calculate Min(F35eBultos) For F35eCodigo==cConcepto To nBultos
			Calculate Min(F35ePeso)   For F35eCodigo==cConcepto To nPeso
			Calculate Min(F35eVolum)  For F35eCodigo==cConcepto To nVolumen

		*> Valor M�XIMO.
		Case SubStr(oC38c.C38cCodCon, 1, 2)=='MX'
			Select F35eOcupD
			Calculate Max(F35ePeso)   For F35eCodigo==cConcepto To nPeso
			Calculate Max(F35eVolum)  For F35eCodigo==cConcepto To nVolumen
			Calculate Max(F35eBultos) For F35eCodigo==cConcepto To nBultos

		*> Valor MEDIO.
		Case SubStr(oC38c.C38cCodCon, 1, 2)=='ME'
			Select F35eOcupD
			Calculate Avg(F35ePeso)   For F35eCodigo==cConcepto To nPeso
			Calculate Avg(F35eVolum)  For F35eCodigo==cConcepto To nVolumen
			Calculate Avg(F35eBultos) For F35eCodigo==cConcepto To nBultos

		*> Resto de casos: Error.
		Otherwise
			Select F38lc
			Skip
			Loop
	EndCase

	*> Obtener el registro activo de acumulados.
	Select F35eOcup
	Locate For F35eCodPro==oF38lc.F38lCodPro .And. ;
			   F35eFecCal==dFechaCalculo .And. ;
			   F35eCodigo==oF38lc.F38lCodigo

	If !Found()
		Append Blank
		Replace F35eCodPro With oF38lc.F38lCodPro
		Replace F35eFecCal With dFechaCalculo
		Replace F35eCodigo With oF38lc.F38lCodigo
		Replace F35eRowCon With oF38lc.F38lRowID
	EndIf

	Replace F35eBultos With F35eBultos + nBultos
	Replace F35ePeso   With F35ePeso   + nPeso
	Replace F35eVolum  With F35eVolum  + nVolumen

	Select F38lc
	Skip
EndDo

*> Grabar datos generados en fichero de acumulados.
Select F35eOcup
Go Top
Do While !Eof()
	=f3_instun("F35e", "F35eOcup")

	Select F35eOcup
	Skip
EndDo

Return

ENDPROC
PROCEDURE _calculartprs

*> Calcular acumulados de conceptos de facturaci�n - tipo producto salida.
*> Trabaja a partir del hist�rico de movimientos (F20c).
*> Realiza los c�lculos a partir de los movimientos de expedici�n (2999).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cFromF, dFechaCalculo, oF20c
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo
Local cBultoActual

Store .T. To lStado
This.UsrError = ""
cBultoActual = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F20cTprS"))

=CrtCursor("F20c", "F20cTprSCur")
=AddFldToCursor("F20cTprSCur", [NAME=F08cTipPro,TYPE=C,LENGTH=4])

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='TPRS'"
cFromF = "F38l,C36c"

lStado = f3_sql("*", cFromF, cWhere, , , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo TPRS: Salida tipo producto.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35fCodPro='" + This.CurCli + "' And F35fFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35f", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35f", "F35fTprS")

*> Cl�usula de lectura del hist�rico de movimientos.
cWhere = 		  "F20cCodPro='" + This.CurCli + "' And F20cTipMov='2999' And "
cWhere = cWhere + "F20cFecMov Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

*> Generar el cursor con los datos a procesar.
lStado = f3_sql("*", "F20c,F08c", cWhere, , , "F20cTprSCur")

*> Ordenar los registros por Tipo producto / bulto, priorizando si hay MAC sobre el n� de palet.
*> Devuelve el cursor F20cTprS, agrupado por TPRO, bulto, peso, etc.
Select F20cTprSCur
=This._orderbytippro()

Select F20cTprS
Go Top

Do While !Eof()
	Scatter Name oF20c

	*> Procesar los diferentes conceptos a generar para el palet actual.
	Select F38lc
	Go Top
	Do While !Eof()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF20c.F20cTipPro<>Trim(oF38lc.F38lCodigo)
			*> La tarifa discrimina por concepto - tipo producto -, y �ste es distinto al del palet actual.
			Skip
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin = This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF20c.F20cCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades. En este caso siempre ser� 1.
			Case cTipoCalculo=='U'

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If oF20c.F20cPesPal < nValorIni .Or. oF20c.F20cPesPal > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If oF20c.F20cVolPal < nValorIni .Or. oF20c.F20cVolPal > nValorFin
					Select F38lc
					Skip
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Skip
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - tama�o palet - (puede estar en blanco).

		*> Clave base del registro.
*!*			cWhere = "F35fCodPro='" + oF20c.F20cCodPro + "' And F35fFecCal=" + _GCD(dFechaCalculo)
*!*			cWhere = cWhere + " And F35fCodTrm='" + oF38lc.F38lCodTrm + "'"
*!*			cWhere = cWhere + " And F35fTipPro='" + oF38lc.F38lCodigo + "'"

		cWhere = "F35fRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35f", cWhere, , , "F35fTprS")
		Select F35fTprS

		If !lStado
			Append Blank
			Replace F35fCodPro With oF38lc.F38lCodPro
			Replace F35fFecCal With dFechaCalculo
			Replace F35fTipPro With oF38lc.F38lCodigo
			Replace F35fCodTrm With oF38lc.F38lCodTrm
			Replace F35fRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35fBultos With F35fBultos + 1
		Replace F35fPeso   With F35fPeso   + oF20c.F20cPesPal
		Replace F35fVolum  With F35fVolum  + oF20c.F20cVolPal

		If !lStado
			=f3_instun("F35f", "F35fTprS")
		Else
			=f3_updtun("F35f", , , , "F35fTprS", cWhere)
		EndIf

		Select F38lc
		Skip
	EndDo

	Select F20cTprS
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35fTprS"))
Use In (Select ("F20cTprS"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _orderbytippro

*> Ordena los datos de c�lculo de salida por tipo de producto.
*> DEBE estar activo el cursor a clasificar, F20cTprSCur, imagen del F20c.
*> M�todo privado de la clase.

*> Recibe:
*>	- Cursor activo, a partir de los datos de hist�rico de movimiento.

*> Devuelve:
*>	- El mismo cursor, ordenado.
*>	- Estado, resultado de la operaci�n (.T. / .F.)
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- _calculartprs(), c�lculo acumulados de salida por tipo de producto.

Local lStado, cOldSele
Private oPal

This.UsrError = ""
lStado = .T.

cOldSele = Select()
If Empty(cOldSele)
	This.UsrError = "No hay ning�n fichero activo"
	lStado = .F.
	Return lStado
EndIf

*> Si hay MAC, �ste sustituye al n� de palet.
Replace All F20cNumPal With F20cNumMac For !Empty(F20cNumMac)

*> Generar el cursor de trabajo real, F20cTprS.
=CrtFCursor("F20cTprS", [TBL=F20c,FLD=F20cCodPro,FLD=F20cNumPal,FLD=F20cOriRes])
=AddFldToCursor("F20cTprS", [NAME=F20cTipPro,TYPE=C,LENGTH=4])
=AddFldToCursor("F20cTprS", [NAME=F20cPesPal,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor("F20cTprS", [NAME=F20cVolPal,TYPE=N,LENGTH=12,DECIMALS=3])

*> Acumular datos sobre el cursor real de trabajo.
Select F20cTprSCur
Go Top
Do While !Eof()
	Scatter Name oPal

	*> Peso y volumen del movimiento.
	=This._pesovolumenmov(oPal.F20cCodPro, oPal.F20cCodArt, oPal.F20cCanFis)

	Select F20cTprS
	Locate For F20cCodPro==oPal.F20cCodPro .And. F20cTipPro==oPal.F08cTipPro .And. F20cNumPal==oPal.F20cNumPal
	If !Found()
		Append Blank
		Replace F20cCodPro With oPal.F20cCodPro
		Replace F20cTipPro With oPal.F08cTipPro
		Replace F20cNumPal With oPal.F20cNumPal
		Replace F20cOriRes With oPal.F20cOriRes
	EndIf

	Replace F20cPesPal With F20cPesPal + This.oUbicObj.PesOcu
	Replace F20cVolPal With F20cVolPal + This.oUbicObj.VolOcu

	Select F20cTprSCur
	Skip
EndDo

Use In (Select ("F20cTprSCur"))

Return

ENDPROC
PROCEDURE _lastdayofmonth

*> Calcular el �ltimo d�a de un mes.
*> M�todo privado de la clase.

*> Recibe:
*>	- A�o, en formato AAAA, Mes

*> Devuelve:
*>	- nLastDayOfMonth, �ltimo d�a del mes.

Parameters nYear, nMonth

Local nYearL, nMonthL
Private nLastDayOfMonth

*> Asignar los par�metros a proiedades locales.
nYearL = Iif(Type('nYear')=='N', nYear, Year(Date()))
nMonthL = Iif(Type('nMonth')=='N', nMonth, Month(Date())) + 1

If nMonthL > 12
	nTearL = nYearL + 1
	nMonthL = 1
EndIf

Return Day(Date(nYearL, nMonthL, 1) - 1)

ENDPROC
PROCEDURE _calculardocu

*> Calcular acumulados de conceptos de facturaci�n - documentos.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Documentos de entrada (F18c/F18l).
*>	- Documentos de salida (F24c/F24l).
*>	- Albaranes de entrada (F18m/F18n).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F36pDocu"))

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos) con C38d (conceptos documentos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='DOCU' And C38dCodCon=F38lCodigo"

cFromF = "F38l,C36c,C38d"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo DOCU: Documentos.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35gCodPro='" + This.CurCli + "' And F35gFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35g", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35g", "F35gDocu")

*> Calcular acumulados de documentos de entrada.
=This._calculardocue()

*> Calcular acumulados de documentos de salida.
=This._calculardocus()

*> Calcular acumulados de albaranes de entrada.
=This._calculardocuae()

*> Calcular acumulados de albaranes de salida
=This._calculardocuas()

*> Calcular acumulados de l�neas de salida.
=This._calculardoculs()

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _calculardocue

*> Calcular acumulados de conceptos de facturaci�n - documentos de entrada.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Documentos de entrada (F18c/F18l).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- F38lc, cursor con la tarifa de conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calculardocu(), calcular ficheros de acumulados de documentos.

*> Pendiente:
*>	fecha de c�lculo

*> Historial de modificaciones:
*> 02.04.2008 (AVC) Calcular peso / volumen por documento.

Private cField, cWhere, cFromF, cGroup, oF18c, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo, nTotVol, nTotPes

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

If !Used("F38lc")
	This.UsrError = "No existen los conceptos a facturar"
	lStado = .F.
	Return lStado
EndIf

*> Validar si hay conceptos de entrada definidos.
Select F38lc
Locate For C38dTipCon=='D' .And. C38dEntSal=='E'
lStado = Found()
If !lStado
	*> No est� definido en las tarifas del cliente.
	This.UsrError = "Conceptos de entradas no definidos en tarifas"
	Return lStado
EndIf

*> Cursores de trabajo.
Use In (Select ("F18cDocu"))
Use In (Select ("F35gDocu"))

*> Crear cursores para los datos a generar.
=CrtCursor("F35g", "F35gDocu")

=CrtFCursor("F18cDocu", [TBL=F18c,FLD=F18cCodPro,FLD=F18cTipDoc,FLD=F18cNumDoc])
=AddFldToCursor("F18cDocu", [NAME=TotLin,TYPE=N,LENGTH=3,DECIMALS=0])
=AddFldToCursor("F18cDocu", [NAME=PesDoc,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor("F18cDocu", [NAME=VolDoc,TYPE=N,LENGTH=12,DECIMALS=3])

*> Cl�usula de lectura de documentos de entrada.
cWhere = 		  "F18cCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F18cFecPed Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F18lCodPro=F18cCodPro And F18lTipDoc=F18cTipDoc And F18lNumDoc=F18cNumDoc"

cField = "F18cCodPro,F18cTipDoc,F18cNumDoc," + _GCN("Count(*)", 0) + " As TotLin"
cFromF = "F18c,F18l"
cGroup = "F18cCodPro,F18cTipDoc,F18cNumDoc"

*> Generar el cursor con los documentos a procesar.
lStado = f3_sql(cField, cFromF, cWhere, , cGroup, "F18cDocu")

Select F18cDocu
Go Top

Do While !Eof()
	Scatter Name oF18c

	*> Calcular peso / volumen del documento de entrada.
	cWhere = 		  "F20cCodPro='" + This.CurCli + "' And F20cTipMov='1000' And "
	cWhere = cWhere + "F20cCodPro='" + oF18c.F18cCodPro + "' And "
	cWhere = cWhere + "F20cTipDoc='" + oF18c.F18cTipDoc + "' And "
	cWhere = cWhere + "F20cNumDoc='" + oF18c.F18cNumDoc + "' And "
	cWhere = cWhere + "F16tNumPal=F20cNumPal"

	lStado = f3_sql("*", "F20c,F16t", cWhere, , , "F18cPale")
	If lStado
		Select F18cPale
		Sum(F16tVolPal) To nTotVol
		Sum(F16tPesPal) To nTotPes

		Select F18cDocu
		Replace PesDoc With nTotPes
		Replace VolDoc With nTotVol
	EndIf

	*> Procesar los diferentes conceptos a generar para el documento actual.
	*> S�lo los conceptos que son tipo de documento y de entrada.
	Select F38lc
	Locate For C38dTipCon=='D' .And. C38dEntSal=='E'
	Do While Found()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF18c.F18cTipDoc<>oF38lc.F38lCodigo
			*> La tarifa discrimina por concepto - documento -, y �ste es distinto al del actual.
			Continue
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF18c.F18cCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del documento actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If !Between(oF18c.TotLin, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If !Between(oF18c.PesDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If !Between(oF18c.VolDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Continue
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - documento - (puede estar en blanco).

		*> Clave base del registro.
*!*			cWhere = "F35gCodPro='" + oF18c.F18cCodPro + "' And F35gFecCal=" + _GCD(dFechaCalculo)
*!*			cWhere = cWhere + " And F35gCodTrm='" + oF38lc.F38lCodTrm + "'"
*!*			cWhere = cWhere + " And F35gCodigo='" + oF38lc.F38lCodigo + "'"

		cWhere = "F35gRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35g", cWhere, , , "F35gDocu")
		Select F35gDocu

		If !lStado
			Append Blank
			Replace F35gCodPro With oF38lc.F38lCodPro
			Replace F35gFecCal With dFechaCalculo
			Replace F35gCodigo With oF38lc.F38lCodigo
			Replace F35gCodTrm With oF38lc.F38lCodTrm
			Replace F35gRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35gCantid With F35gCantid + 1
		Replace F35gPeso   With F35gPeso   + oF18c.PesDoc
		Replace F35gVolum  With F35gVolum  + oF18c.VolDoc

		If !lStado
			=f3_instun("F35g", "F35gDocu")
		Else
			=f3_updtun("F35g", , , , "F35gDocu", cWhere)
		EndIf

		Select F38lc
		Continue
	EndDo

	Select F18cDocu
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F18cDocu"))
Use In (Select ("F18cPale"))

Return

ENDPROC
PROCEDURE _calculardocus

*> Calcular acumulados de conceptos de facturaci�n - documentos de salida.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Documentos de salida (F24c/F24l).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- F38lc, cursor con la tarifa de conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calculardocu(), calcular ficheros de acumulados de documentos.

*> Pendiente:
*>	fecha de c�lculo

Private cField, cWhere, cFromF, cGroup, oF24c, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

If !Used("F38lc")
	This.UsrError = "No existen los conceptos a facturar"
	lStado = .F.
	Return lStado
EndIf

*> Validar si hay conceptos de salida definidos.
Select F38lc
Locate For C38dTipCon=='D' .And. C38dEntSal=='S'
lStado = Found()
If !lStado
	*> No est� definido en las tarifas del cliente.
	This.UsrError = "Conceptos de salidas no definidos en tarifas"
	Return lStado
EndIf

*> Cursores de trabajo.
Use In (Select ("F24cDocu"))
Use In (Select ("F35gDocu"))

*> Crear cursores para los datos a generar.
=CrtCursor("F35g", "F35gDocu")

=CrtFCursor("F24cDocu", [TBL=F24c,FLD=F24cCodPro,FLD=F24cTipDoc,FLD=F24cNumDoc])
=AddFldToCursor("F24cDocu", [NAME=TotLin,TYPE=N,LENGTH=3,DECIMALS=0])
=AddFldToCursor("F24cDocu", [NAME=PesDoc,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor("F24cDocu", [NAME=VolDoc,TYPE=N,LENGTH=12,DECIMALS=3])

*> Cl�usula de lectura de documentos de salida. Toma s�lo los que est�n preparados.
cWhere = 		  "F24cCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F24cFecDoc Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F24lCodPro=F24cCodPro And F24lTipDoc=F24cTipDoc And F24lNumDoc=F24cNumDoc And "
cWhere = cWhere + "F24cFlgEst='6'"

cField = "F24cCodPro,F24cTipDoc,F24cNumDoc," + _GCN("Count(*)", 0) + " As TotLin"
cFromF = "F24c,F24l"
cGroup = "F24cCodPro,F24cTipDoc,F24cNumDoc"

*> Generar el cursor con los documentos a procesar.
lStado = f3_sql(cField, cFromF, cWhere, , cGroup, "F24cDocu")

Select F24cDocu
Go Top

Do While !Eof()
	Scatter Name oF24c

	*> Procesar los diferentes conceptos a generar para el documento actual.
	*> S�lo los conceptos que son tipo de documento y de salida.
	Select F38lc
	Locate For C38dTipCon=='D' .And. C38dEntSal=='S'
	Do While Found()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF24c.F24cTipDoc<>oF38lc.F38lCodigo
			*> La tarifa discrimina por concepto - documento -, y �ste es distinto al del actual.
			Continue
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF24c.F24cCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del documento actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If !Between(oF24c.TotLin, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If !Between(oF24c.PesDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If !Between(oF24c.VolDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Continue
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto - documento - (puede estar en blanco).

		*> Clave base del registro.
*!*			cWhere = "F35gCodPro='" + oF24c.F24cCodPro + "' And F35gFecCal=" + _GCD(dFechaCalculo)
*!*			cWhere = cWhere + " And F35gCodTrm='" + oF38lc.F38lCodTrm + "'"
*!*			cWhere = cWhere + " And F35gCodigo='" + oF38lc.F38lCodigo + "'"

		cWhere = "F35gRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35g", cWhere, , , "F35gDocu")
		Select F35gDocu

		If !lStado
			Append Blank
			Replace F35gCodPro With oF38lc.F38lCodPro
			Replace F35gFecCal With dFechaCalculo
			Replace F35gCodigo With oF38lc.F38lCodigo
			Replace F35gCodTrm With oF38lc.F38lCodTrm
			Replace F35gRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35gCantid With F35gCantid + 1
		Replace F35gPeso   With F35gPeso   + oF24c.PesDoc
		Replace F35gVolum  With F35gVolum  + oF24c.VolDoc

		If !lStado
			=f3_instun("F35g", "F35gDocu")
		Else
			=f3_updtun("F35g", , , , "F35gDocu", cWhere)
		EndIf

		Select F38lc
		Continue
	EndDo

	Select F24cDocu
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F24cDocu"))

Return

ENDPROC
PROCEDURE _calculardocuae

*> Calcular acumulados de conceptos de facturaci�n - albaranes de entrada.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Albaranes de entrada (F18m/F18n).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- F38lc, cursor con la tarifa de conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calculardocu(), calcular ficheros de acumulados de documentos.

*> Pendiente:
*>	fecha de c�lculo

Private cField, cWhere, cFromF, cGroup, oF18m, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

If !Used("F38lc")
	This.UsrError = "No existen los conceptos a facturar"
	lStado = .F.
	Return lStado
EndIf

*> Validar si el concepto a calcular est� definido.
Select F38lc
Locate For C38dCodCon=='ALBE'
lStado = Found()
If !lStado
	*> No est� definido en las tarifas del cliente.
	This.UsrError = "Concepto 'Albaranes de Entrada' no definido en tarifas"
	Return lStado
EndIf

*> Cursores de trabajo.
Use In (Select ("F18mDocu"))
Use In (Select ("F35gDocu"))

*> Crear cursores para los datos a generar.
=CrtCursor("F35g", "F35gDocu")

=CrtFCursor("F18mDocu", [TBL=F18m,FLD=F18mCodPro,FLD=F18mNumEnt])
=AddFldToCursor("F18mDocu", [NAME=TotLin,TYPE=N,LENGTH=3,DECIMALS=0])
=AddFldToCursor("F18mDocu", [NAME=PesDoc,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor("F18mDocu", [NAME=VolDoc,TYPE=N,LENGTH=12,DECIMALS=3])

*> Cl�usula de lectura de albaranes de entrada.
cWhere = 		  "F18mCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F18mFecEnt Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F18nNumEnt=F18mNumEnt"

cField = "F18mCodPro,F18mNumEnt," + _GCN("Count(*)", 0) + " As TotLin"
cFromF = "F18m,F18n"
cGroup = "F18mCodPro,F18mNumEnt"

*> Generar el cursor con los documentos a procesar.
lStado = f3_sql(cField, cFromF, cWhere, , cGroup, "F18mDocu")

Select F18mDocu
Go Top

Do While !Eof()
	Scatter Name oF18m

	*> Procesar los diferentes conceptos a generar para el documento actual.
	*> S�lo los conceptos que son de albaranes de entrada.
	Select F38lc
	Locate For C38dCodCon=='ALBE'
	Do While Found()
		Scatter Name oF38lc

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF18m.F18mCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del albar�n actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If !Between(oF18m.TotLin, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If !Between(oF18m.PesDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If !Between(oF18m.VolDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Continue
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*> Clave base del registro.
*!*			cWhere = "F35gCodPro='" + oF18m.F18mCodPro + "' And F35gFecCal=" + _GCD(dFechaCalculo)
*!*			cWhere = cWhere + " And F35gCodTrm='" + oF38lc.F38lCodTrm + "'"
*!*			cWhere = cWhere + " And F35gCodigo='" + oF38lc.F38lCodigo + "'"

		cWhere = "F35gRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35g", cWhere, , , "F35gDocu")
		Select F35gDocu

		If !lStado
			Append Blank
			Replace F35gCodPro With oF38lc.F38lCodPro
			Replace F35gFecCal With dFechaCalculo
			Replace F35gCodigo With oF38lc.F38lCodigo
			Replace F35gCodTrm With oF38lc.F38lCodTrm
			Replace F35gRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35gCantid With F35gCantid + 1
		Replace F35gPeso   With F35gPeso   + oF18m.PesDoc
		Replace F35gVolum  With F35gVolum  + oF18m.VolDoc

		If !lStado
			=f3_instun("F35g", "F35gDocu")
		Else
			=f3_updtun("F35g", , , , "F35gDocu", cWhere)
		EndIf

		Select F38lc
		Continue
	EndDo

	Select F18mDocu
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F18mDocu"))

Return

ENDPROC
PROCEDURE _calculardoculs

*> Calcular acumulados de conceptos de facturaci�n - l�neas de salida.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Documentos de salida, detalle (F24l).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- F38lc, cursor con la tarifa de conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calculardocu(), calcular ficheros de acumulados de documentos.

*> Pendiente:
*>	fecha de c�lculo

Private cField, cWhere, cFromF, oF24l, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo
Local nPesoLinea, nVolumenLinea

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

If !Used("F38lc")
	This.UsrError = "No existen los conceptos a facturar"
	lStado = .F.
	Return lStado
EndIf

*> Validar si el concepto a calcular est� definido.
Select F38lc
Locate For C38dCodCon=='LINS'
lStado = Found()
If !lStado
	*> No est� definido en las tarifas del cliente.
	This.UsrError = "Concepto 'L�neas de Salida' no definido en tarifas"
	Return lStado
EndIf

*> Cursores de trabajo.
Use In (Select ("F24lDocu"))
Use In (Select ("F35gDocu"))

*> Crear cursores para los datos a generar.
=CrtCursor("F35g", "F35gDocu")
=CrtCursor("F24l", "F24lDocu")

*> Cl�usula de lectura de l�neas de salida.
cWhere = 		  "F24lCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F24lFecDoc Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F24cCodPro=F24lCodPro And F24cTipDoc=F24lTipDoc And F24cNumDoc=F24lNumDoc And "
cWhere = cWhere + "F24cFlgEst='6'"

cField = "*"
cFromF = "F24l,F24c"

*> Generar el cursor con las l�neas de documento a procesar.
lStado = f3_sql(cField, cFromF, cWhere, , , "F24lDocu")

Select F24lDocu
Go Top

Do While !Eof()
	Scatter Name oF24l

	*> Procesar los diferentes conceptos a generar para el documento actual.
	*> S�lo los conceptos que son de l�neas de salida.
	Select F38lc
	Locate For C38dCodCon=='LINS'
	Do While Found()
		Scatter Name oF38lc

		*> Obtener el peso y el volumen de los datos generados.
		=This._pesovolumenmov(oF24l.F24lCodPro, oF24l.F24lCodArt, oF24l.F24lCanDoc)

		nPesoLinea = This.oUbicObj.PesOcu
		nVolumenLinea = This.oUbicObj.VolOcu

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF24l.F24lCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del documento actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If !Between(oF24l.F24lCanDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If !Between(nPesoLinea, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If !Between(nVolumenLinea, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Continue
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*> Clave base del registro.
*!*			cWhere = "F35gCodPro='" + oF24l.F24lCodPro + "' And F35gFecCal=" + _GCD(dFechaCalculo)
*!*			cWhere = cWhere + " And F35gCodTrm='" + oF38lc.F38lCodTrm + "'"
*!*			cWhere = cWhere + " And F35gCodigo='" + oF38lc.F38lCodigo + "'"

		cWhere = "F35gRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35g", cWhere, , , "F35gDocu")
		Select F35gDocu

		If !lStado
			Append Blank
			Replace F35gCodPro With oF38lc.F38lCodPro
			Replace F35gFecCal With dFechaCalculo
			Replace F35gCodigo With oF38lc.F38lCodigo
			Replace F35gCodTrm With oF38lc.F38lCodTrm
			Replace F35gRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35gCantid With F35gCantid + 1
		Replace F35gPeso   With F35gPeso   + nPesoLinea
		Replace F35gVolum  With F35gVolum  + nVolumenLinea

		If !lStado
			=f3_instun("F35g", "F35gDocu")
		Else
			=f3_updtun("F35g", , , , "F35gDocu", cWhere)
		EndIf

		Select F38lc
		Continue
	EndDo

	Select F24lDocu
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F24lDocu"))

Return

ENDPROC
PROCEDURE _calcularmovi

*> Calcular acumulados de conceptos de facturaci�n - movimientos de almac�n.
*> Trabaja a partir del hist�rico de movimientos (F20c).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cFromF, dFechaCalculo, oF20c
Local lStado, oF38lc, cClaseMovimiento, nPesoLinea, nVolumenLinea

Store .T. To lStado
This.UsrError = ""
cBultoActual = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F20cMovi"))

*!*	*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos) con C38e (conceptos movimiento).
*!*	cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
*!*	cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='MOVI' And C38eCodCon=F38lCodigo"
*!*	cFromF = "F38l,C36c,C38e"

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='MOVI'"				&& And C38eCodCon=F38lCodigo"
cFromF = "F38l,C36c"

lStado = f3_sql("*", cFromF, cWhere, , , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo MOVI: Movimientos de almac�n.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35hCodPro='" + This.CurCli + "' And F35hFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35h", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35h", "F35hMovi")

*> Cl�usula de lectura del hist�rico de movimientos.
cWhere = 		  "F20cCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F20cFecMov Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst)

*> Generar el cursor con los datos a procesar.
lStado = f3_sql("*", "F20c", cWhere, , , "F20cMovi")

Select F20cMovi
Go Top

Do While !Eof()
	Scatter Name oF20c

	*> Validar la clase de movimiento.
	cClaseMovimiento = This._clasemovimiento(oF20c)

	*> Validar si la clase de movimiento est� en la tarifa a calcular.
	Select F38lc
	Locate For Trim(F38lCodigo)==cClaseMovimiento
	If !Found()
		Locate For Empty(F38lCodigo)
		If !Found()
			Select F20cMovi
			Skip
			Loop
		EndIf
	EndIf

	Scatter Name oF38lc

	*> Obtener el peso y el volumen de los datos generados.
	=This._pesovolumenmov(oF20c.F20cCodPro, oF20c.F20cCodArt, oF20c.F20cCanFis)

	nPesoLinea = This.oUbicObj.PesOcu
	nVolumenLinea = This.oUbicObj.VolOcu

	*> Obtener el registro activo de acumulados.
	*> Clave base del registro.
	cWhere = "F35hCodPro='" + oF20c.F20cCodPro + "' And F35hFecCal=" + _GCD(dFechaCalculo)
	cWhere = cWhere + " And F35hCodigo='" + oF38lc.F38lCodigo + "'"

	cWhere = "F35hRowCon='" + oF38lc.F38lRowID + "'"

	lStado = f3_sql("*", "F35h", cWhere, , , "F35hMovi")
	Select F35hMovi

	If !lStado
		Append Blank
		Replace F35hCodPro With oF38lc.F38lCodPro
		Replace F35hFecCal With dFechaCalculo
		Replace F35hCodigo With oF38lc.F38lCodigo
		Replace F35hCodTrm With oF38lc.F38lCodTrm
		Replace F35hRowCon With oF38lc.F38lRowID
	EndIf

	Replace F35hCantid With F35hCantid + 1
	Replace F35hPeso   With F35hPeso   + nPesoLinea
	Replace F35hVolum  With F35hVolum  + nVolumenLinea

	If !lStado
		=f3_instun("F35h", "F35hMovi")
	Else
		=f3_updtun("F35h", , , , "F35hMovi", cWhere)
	EndIf

	Select F20cMovi
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35hMovi"))
Use In (Select ("F20cMovi"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _clasemovimiento

*> Calcular la clase de movimiento que corresponde a un registro del hist�rico.
*> M�todo privado de la clase.

*> Recibe:
*>	- oF20c, copia del registro le�do del hist�rico, � bien
*>	- cMovimiento, n� de movimiento.

*> Devuelve:
*>	- cTipo, tipo de movimiento:
*>		'E', entrada manual.
*>		'S', salida manual.
*>		'C', control de calidad.
*>		'I', movimiento interno entre ubicaciones.
*>		'L', regularizaci�n por inventario.
*>		'R', Reposici�n de material.
*>		'P', movimiento de preparaci�n.
*>		'X', movimiento de expedici�n.
*>		'?', tipo de movimiento no procesable.
*>		'*', error en el programa.
*>	- This.UsrError, textos de error.

*> Llamado desde:
*>	- This._calcularmovi(), calcular acumulados por movimentos.

Parameters xMovimiento

Local cTipo, oF20c

This.UsrError = ""
cTipo = '?'					&& Tipo desconocido � no procesable.

*> En funci�n de la clase de par�metro recibido, leer o no el movimiento en el hist�rico.
If Type('xMovimiento')=='C'
	m.F20cNumMov = xMovimiento
	If !f3_seek("F20c")
		This.UsrError = "No existe movimiento en hist�rico"
		cTipo = "*"
		Return cTipo
	EndIf

	Select F20c
	Scatter Name oF20c
Else
	*> Pasar par�metro a variable de uso local.
	oF20c = xMovimiento
EndIf

Do Case
	*> Entrada manual.
	Case SubStr(oF20c.F20cTipMov, 1, 1)=='1' .And. Empty(oF20c.F20cNumEnt)
		cTipo = 'E'
		Return cTipo

	*> Salida manual.
	Case SubStr(oF20c.F20cTipMov, 1, 1)=='2' .And. Empty(oF20c.F20cNumLst)
		cTipo = 'S'
		Return cTipo

	*> Movimiento de expedici�n.
	Case oF20c.F20cTipMov=='2999'
		cTipo = 'X'
		Return cTipo

	*> Movimiento de regularizaci�n.
	Case Between(oF20c.F20cTipMov, "4500", "4999")
		cTipo = 'L'
		Return cTipo

	*> Movimientos entre ubicaciones.
	Case Between(oF20c.F20cTipMov, "3500", "3999")
		If !Empty(oF20c.F20cNumLst)
			*> Es un movimiento de preparaci�n � de reposici�n.
			m.F26lNumMov = oF20c.F20cNMovMP
			If f3_seek("F26l")
				cTipo = F26l.F26lTipLst
			EndIf

			Return cTipo
		EndIf

		*> Leer el movimiento destino.
		m.F20cNumMov = PadL(AllTrim(Str(Val(oF20c.F20cNumMov) + 1)), 10, '0')
		If f3_seek("F20c")
			If F20c.F20cSitStk<>oF20c.F20cSitStk
				*> Es un cambio de situaci�n de stock.
				cTipo = 'C'
			Else
				*> Es un movimiento interno.
				cTipo = 'I'
			EndIf

			Return cTipo
		EndIf

	*> Tipo de movimiento no procesable.
	Otherwise
		cTipo = '?'
EndCase

Return cTipo

ENDPROC
PROCEDURE calcularfacturas

*> Calcular las facturas de un periodo dado.

*> Recibe:
*>	- cPropietario, c�digo de cliente (opcional).
*>	- dFecha, fecha de c�lculo (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- This.CrtFra, Tratamiento si la factura existe:
*>			A: A�adir nuevos datos a la factura.
*>			R: Reemplazar la factura.
*>			N: Crear una nueva factura.
*>			C: Cancelar el proceso.
*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cPropietario, dFecha

Local lStado
Private cWhere, oF32c

Store .T. To lStado
This.UsrError = ""

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.FchDsd = Iif(Type('dFecha')=='D', dFecha, .FchDsd)
	.FchHst = Iif(Type('dFecha')=='D', dFecha, .FchHst)
EndWith

*> Cargar un cursor con los clientes a procesar.
cWhere = "F32cCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "'"
lStado = f3_sql("*", "F32c", cWhere, , , "F32cGenF")
If !lStado
	*> No hay clientes para cargar.
	Use In (Select ("F32cGenF"))
	This.UsrError = "No se pueden cargar los clientes"
	Return lStado
EndIf

Select F32cGenF
Go Top
Do While !Eof()
	Scatter Name oF32c

	*> Validar la tarifa del cliente en curso.
	lStado = This._validartarifa(oF32c.F32cCodPro)
	If !lStado
		*> Cliente sin tarifa v�lida.
		Select F32cGenF
		Skip
		Loop
	EndIf

	*> Validar si ya existe la factura y, si existe, la operaci�n a realizar.
	lStado = This._validarfactura(oF32c.F32cCodPro)
	If !lStado
		*> No puede generar la factura.
		Select F32cGenF
		Skip
		Loop
	EndIf

	*> Llamadas a c�lculos de facturas para el cliente en curso.
	lStado = This._generarfactura()
	lStado = This._generaracumuladosfactura()
	lStado = This._generardatospagofactura()

	Select F32cGenF
	Skip
EndDo

Use In (Select ("F32cGenF"))

Return

ENDPROC
PROCEDURE _validarfactura

*> Validar si existe la factura actual en generaci�n de facturas.
*> M�todo privado de la clase.

*> Recibe:
*>	- cCodPro, cliente a validar.
*>	- This.FchDsd, fecha inicial periodo.
*>	- This.FchHst, fecha final periodo.

*> Devuelve:
*>	- Estado, resultado de la operaci�n (.T. / .F.)
*>	- This.CurFra, indica si existe la factura a generar.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularFacturas(), calcular facturas.

Parameters cCodigo

Private cWhere
Local lStado, oF70c

lStado = .T.

With This
	.UsrError = ""
	.CurFra = "N"
	._numfraget = ""
EndWith

cWhere = "F70cCodPro='" + cCodigo + "' And "
cWhere = cWhere + "F70cFecFac=" + _GCD(This.FecFra)

lStado = f3_sql("*", "F70c", cWhere, , , "F70cFACT")

If !lStado
	If _xier <= 0
		*> Error.
		This.UsrError = "Error validar factura"
		Use In (Select ("F70cFACT"))
		Return lStado
	EndIf

	*> No existe: Tratamiento general de generaci�n de facturas.
	Use In (Select ("F70cFACT"))
	Return !lStado
EndIf

*> Operaciones a realizar si la factura existe.
Select F70cFACT
Go Top
Scatter Name oF70c

With This
	Do Case
		*> La factura no se puede modificar.
		Case oF70c.F70cEstado > '0'
			This.UsrError = "La factura existe y est� cerrada"
			lStado = .F.

		*> Si la factura existe, a�adir l�neas nuevas. Dejar las que ya existan.
		Case .CrtFra=='A'
			._numfraget = F70cNumFac
			.CurFra = "S"
			lStado = .T.

		*> Si la factura existe, a�adir l�neas nuevas. Modificar las que ya existan.
		Case .CrtFra=='S'
			._numfraget = F70cNumFac
			.CurFra = "S"
			lStado = .T.

		*> Si la factura ya existe, no hacer nada.
		Case .CrtFra=='C'
			This.UsrError = "La factura ya existe"
			lStado = .F.

		*> Si la factura existe, crear otra con las l�neas nuevas.
		Case .CrtFra=='N'
			lStado = .T.
	EndCase
EndWith

Use In (Select ("F70cFACT"))
Return lStado

ENDPROC
PROCEDURE eliminartarifa

*> Eliminar la plantilla de una tarifa.

*> Recibe:
*>	- cPropietario, c�digo de cliente.
*>	- cTarifa, c�digo de tarifa.
*> o bien
*>	- This.CodPro, This.CodTar par�metros recibidos como propiedades.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cPropietario, cTarifa

Private cWhere
Local lStado

Store "" To This.UsrError
lStado = .T.

*> Asignar par�metros recibidos a propiedades.
With This
	.CodPro = Iif(Type('cPropietario')=='C', cPropietario, .CodPro)
	.CodTar = Iif(Type('cTarifa')=='C', cTarifa, .CodTar)
EndWith

*> Validar tarifa / conceptos.
lStado = This._validarplantilla()
If !lStado
	*> El mensaje de error ya viene asignado.
	Return lStado
EndIf

*> Crear cursor con la plantilla de la tarifa.
lStado = This._eliminartarifa()

Return lStado

ENDPROC
PROCEDURE _eliminartarifa

*> Eliminar la plantilla de tarifas de precios.
*> Las validaciones ya se han hecho en la funci�n que llame a este m�todo.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CodPro, c�digo de cliente.
*>	- This.CodTar, c�digo de tarifa.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.EliminarTarifa, Eliminar plantilla de tarifas.

Private cWhere, cField, oC37l
Local lStado

This.UsrError = ""
lStado = .T.

*> Validar si ya existe una plantilla de tarifa.
cField = "Min(F38lRowId) As F38lRowId"
cWhere = "F38lCodPro='" + This.CodPro + "' And F38lCodTar='" + This.CodTar + "'"
lStado = f3_sql(cField, "F38l", cWhere, , , "F38lCur")

Use In (Select ("F38lCur"))

If lStado .And. This.BorrarTar<>"S"
	*> Ya existe la plantilla y no se permite el borrado.
	This.UsrError = "Ya existe la tarifa"
	lStado = .F.
	Return lStado
EndIf

*> Cargar la plantilla de conceptos.
cWhere = "C37lCodPro='" + This.CodPro + "' And C37lCodTar='" + This.CodTar+ "'"
lStado = f3_sql("*", "C37l", cWhere, , , "C37lCUR")

Select C37lCUR
Go Top
Do While !Eof()
	*> Con el RowID del registro, eliminar el detalle de la tarifa.
	Scatter Name oC37l
	=This._eliminarconcepto(oC37l.C37lRowID)

	Select C37lCUR
	Skip
EndDo

Use In (Select ("C37LCUR"))

Return

ENDPROC
PROCEDURE eliminarconcepto

*> Eliminar un concepto de la plantilla de una tarifa.

*> Recibe:
*>	- cID, Id del concepto actual de la plantilla de conceptos (C37l).

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cRowID

Private cWhere, oF38l
Local lStado

This.UsrError = ""

lStado = This._eliminarconcepto(cRowID)
Return lStado

ENDPROC
PROCEDURE _generarfactura

*> Calcular la factura del cliente actual.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Ficheros de c�lculos (F35x).
*>	- Ficheros de conceptos (F38x).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- This.CrtFra, tratamiento con facturas existentes.
*>	- This.CurFra, factura ya existe S/N.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularFacturas(), generaci�n de facturas.

*> Historial de modificaciones:
*> 14.06.2006 (AVC)	Agregar conceptos directos.

Private cField, cWhere, cFromF, cGroup
Local lStado

Store .T. To lStado
This.UsrError = ""

*> Cargar conceptos y escalado de c�lculo.
cWhere = "F38eCodPro='" + This.CurCli + "' And F38eCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "F38lRowID=F38eRowDet And C37lRowID=F38lRowCab"

lStado = f3_sql("*", "F38e,F38l,C37l", cWhere, , , "F38eFACT")
If !lStado
	This.UsrError = "Cliente sin conceptos"
	Use In (Select ("F38eFACT"))
	Return lStado
EndIf

*> Llamadas a generar facturas por ficheros de acumulados.
lStado = This._generarfacturapale()				&& Palets de entrada.
lStado = This._generarfacturapals()				&& Palets de salida.
lStado = This._generarfacturatpro()				&& Tipo producto entradas.
lStado = This._generarfacturadiap()				&& Estancia d�as.
lStado = This._generarfacturaocup()				&& Ocupaciones / ubicaciones.
lStado = This._generarfacturatprs()				&& Tipo producto salidas.
lStado = This._generarfacturadocu()				&& Documentos.
lStado = This._generarfacturamovi()				&& Movimientos.
lStado = This._generarfacturadire()				&& Directos.

Use In (Select ("F38eFACT"))
Return lStado

ENDPROC
PROCEDURE _generarfacturapale

*> Generar factura acumulados de conceptos de facturaci�n - palets de entrada.
*> Trabaja a partir de los ficheros de acumulados de palets (F35a) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35a, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35aCodPro='" + This.CurCli + "' And F35aFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35a"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35aFACT")
If !lStado
	*> Cliente sin acumulados de tipo PALE: Palets de entrada.
	This.UsrError = "No hay acumulados concepto de tipo PALE"
	Use In (Select ("F35aFACT"))
	Return lStado
EndIf

Select F35aFACT
Go Top
Do While !Eof()
	Scatter Name oF35a

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35a.F35aRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35a.F35aCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35a.F35aCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35a.F35aBultos

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35a.F35aPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35a.F35aVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35aFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35aFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturapals

*> Generar factura acumulados de conceptos de facturaci�n - palets de salida.
*> Trabaja a partir de los ficheros de acumulados de palets (F35c) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35c, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35cCodPro='" + This.CurCli + "' And F35cFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35c"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35cFACT")
If !lStado
	*> Cliente sin acumulados de tipo PALS: Palets de salida.
	This.UsrError = "No hay acumulados concepto de tipo PALS"
	Use In (Select ("F35cFACT"))
	Return lStado
EndIf

Select F35cFACT
Go Top
Do While !Eof()
	Scatter Name oF35c

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35c.F35cRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35c.F35cCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35c.F35cCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del palet actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35c.F35cBultos

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35c.F35cPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35c.F35cVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35cFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35cFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturatpro

*> Generar factura acumulados de conceptos de facturaci�n - tipo de producto - entradas.
*> Trabaja a partir de los ficheros de acumulados de tipo producto (F35b) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35b, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35bCodPro='" + This.CurCli + "' And F35bFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35b"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35bFACT")
If !lStado
	*> Cliente sin acumulados de tipo TPRO: Tipo de producto - entrada.
	This.UsrError = "No hay acumulados concepto de tipo TPRO"
	Use In (Select ("F35bFACT"))
	Return lStado
EndIf

Select F35bFACT
Go Top
Do While !Eof()
	Scatter Name oF35b

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35b.F35bRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35b.F35bCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35b.F35bCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35b.F35bBultos

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35b.F35bPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35b.F35bVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35bFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35bFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturadiap

*> Generar factura acumulados de conceptos de facturaci�n - d�as estancia de palets.
*> Trabaja a partir de los ficheros de acumulados de d�as estancia (F35d) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

*> Historial de modificaciones:
*> 07.04.2008 (AVC) Tener en cuenta m�nimos y m�ximos de cantidad / importe.

Private cWhere, cOrder, cFromF, oF35d, oF38e, dFechaCalculo, nCantidad, nCantidadCal
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35dCodPro='" + This.CurCli + "' And F35dFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35d"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35dFACT")
If !lStado
	*> Cliente sin acumulados de tipo DIAP: D�as estancia palets.
	This.UsrError = "No hay acumulados concepto de tipo DIAP"
	Use In (Select ("F35dFACT"))
	Return lStado
EndIf

*> Eliminar los conceptos previos.
cWhere = "F70lNumFac='" + This._numfraget + "' And F70lCodigo='DIAP'"
=f3_deltun("F70l", , cWhere)

Select F35dFACT
Go Top
Do While !Eof()
	Scatter Name oF35d

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35d.F35dRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35d.F35dCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35d.F35dCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35d.F35dNumDia

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35d.F35dPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35d.F35dVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Aplicar valores m�ximos y m�nimos de cantidad.
		nCantidadCal = Iif(nCantidad < oF38e.F38eCanMin, oF38e.F38eCanMin, nCantidad)
		nCantidadCal = Iif(nCantidad > oF38e.F38eCanMax, oF38e.F38eCanMax, nCantidad)

*!*		*> Aplicar valores m�ximos y m�nimos de importe.
*!*		nBasImp = Iif(nBasImp < oF38e.F38eImpMin, oF38e.F38eImpMin, nBasImp)
*!*		nBasImp = Iif(nBasImp > oF38e.F38eImpMax, oF38e.F38eImpMax, nBasImp)

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With F70lUniCal + nCantidad, ;
		        F70lUniCor With F70lUniCor + nCantidadCal, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35dFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35dFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturaocup

*> Generar factura acumulados de conceptos de facturaci�n - ocupaciones / ubicaciones.
*> Trabaja a partir de los ficheros de acumulados de ocupaciones (F35e) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35e, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35eCodPro='" + This.CurCli + "' And F35eFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35e"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35eFACT")
If !lStado
	*> Cliente sin acumulados de tipo OCUP: Ocupaciones / Ubicaciones.
	This.UsrError = "No hay acumulados concepto de tipo OCUP"
	Use In (Select ("F35eFACT"))
	Return lStado
EndIf

Select F35eFACT
Go Top
Do While !Eof()
	If Empty(F35eRowCon)
		*> Es un concepto directo.
		Skip
		Loop
	EndIf

	Scatter Name oF35e

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35e.F35eRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35e.F35eCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35e.F35eCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35e.F35eBultos

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35e.F35ePeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35e.F35eVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35eFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35eFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturatprs

*> Generar factura acumulados de conceptos de facturaci�n - tipo de producto - salidas.
*> Trabaja a partir de los ficheros de acumulados de tipo producto (F35f) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35f, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35fCodPro='" + This.CurCli + "' And F35fFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35f"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35fFACT")
If !lStado
	*> Cliente sin acumulados de tipo TPRS: Tipo de producto - Salida.
	This.UsrError = "No hay acumulados concepto de tipo TPRS"
	Use In (Select ("F35fFACT"))
	Return lStado
EndIf

Select F35fFACT
Go Top
Do While !Eof()
	Scatter Name oF35f

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35f.F35fRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35f.F35fCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35f.F35fCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35f.F35fBultos

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35f.F35fPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35f.F35fVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35fFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35fFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturadocu

*> Generar factura acumulados de conceptos de facturaci�n - documentos.
*> Trabaja a partir de los ficheros de acumulados de documentos (F35g) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35g, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35gCodPro='" + This.CurCli + "' And F35gFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35g"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35gFACT")
If !lStado
	*> Cliente sin acumulados de tipo DOCU: Documentos.
	This.UsrError = "No hay acumulados concepto de tipo DOCU"
	Use In (Select ("F35gFACT"))
	Return lStado
EndIf

Select F35gFACT
Go Top
Do While !Eof()
	Scatter Name oF35g

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35g.F35gRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35g.F35gCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35g.F35gCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35g.F35gCantid

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35g.F35gPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35g.F35gVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35gFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarfacturamovi

*> Generar factura acumulados de conceptos de facturaci�n - movimientos.
*> Trabaja a partir de los ficheros de acumulados de documentos (F35h) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35h, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35hCodPro='" + This.CurCli + "' And F35hFecCal=" + _GCD(dFechaCalculo)
cFromF = "F35h"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35hFACT")
If !lStado
	*> Cliente sin acumulados de tipo MOVI: Movimientos.
	This.UsrError = "No hay acumulados concepto de tipo MOVI"
	Use In (Select ("F35hFACT"))
	Return lStado
EndIf

Select F35hFACT
Go Top
Do While !Eof()
	Scatter Name oF35h

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde.
	Select F38eFACT
	Locate For F38eRowDet=oF35h.F35hRowCon
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35h.F35hCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35h.F35hCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35h.F35hCantid

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35h.F35hPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35h.F35hVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowId  With Ora_NewNum("NROW"), ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF38e.F38lDescri, ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With oF38e.F38ePrecio, ;
		        F70lPreCor With oF38e.F38ePrecio, ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		If !lStado
			=f3_instun("F70l", "F70lFACT")
		Else
			*> Se actualiza la l�nea en funci�n de los par�metros de generaci�n.
			If This.CrtFra=='S'
				=f3_updtun("F70l", , , , "F70lFACT", cWhere)
			EndIf
		EndIf

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35hFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35hFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generarcabecerafactura

*> Generar la cabecera de la factura.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This._numfraget, n� de factura para trabajar.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfacturapale(), generar detalle factura a partir de acumulados de palets de entrada.
*>	- This._generarfacturapals(), generar detalle factura a partir de acumulados de palets de salida.
*>	- This._generarfacturatpro(), generar detalle factura a partir de acumulados tipo producto - entrada.
*>	- This._generarfacturatprs(), generar detalle factura a partir de acumulados tipo producto - salida.
*>	- This._generarfacturadiap(), generar detalle factura a partir de acumulados de d�as estancia palets.
*>	- This._generarfacturaocup(), generar detalle factura a partir de acumulados de ocupaciones.
*>	- This._generarfacturadocu(), generar detalle factura a partir de acumulados de documentos.
*>	- This._generarfacturamovi(), generar detalle factura a partir de acumulados de movimientos.
*>	- This._generarfacturadirecta(), generar detalle factura a partir de conceptos manuales.

Private cWhere
Local lStado

Store .T. To lStado
This.UsrError = ""

If !Used("F70cFACT")
	=CrtCursor("F70c", "F70cFACT")
EndIf

This._numfraget = Ora_NewNum('NFAC', 'N', 'N', 10, 1)

Select F70cFACT
Append Blank
Replace F70cCodPro With This.CurCli
Replace F70cFecCal With Date()
Replace F70cNumFac With This._numfraget
Replace F70cFecFac With This.FecFra
Replace F70cFchIni With This.FchDsd
Replace F70cFchFin With This.FchHst
Replace F70cFecCie With _FecMin
Replace F70cEstado With '0'

=f3_instun("F70c", "F70cFACT")

Use In (Select ("F70cFACT"))

Return

ENDPROC
PROCEDURE _calculardire

*> Calcular acumulados de conceptos de facturaci�n - conceptos directos.
*> Trabaja a partir del fichero de diario de conceptos directos (F36m).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularAcumulados(), calcular ficheros de acumulados de facturaci�n.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF36m, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F36mDire"))

*> Tomar el detalle de tarifas. Relaciona F38l (plantilla) con C36c (origen de conceptos).
cWhere = "F38lCodPro='" + This.CurCli + "' And F38lCodTar='" + This.CurTar + "' And "
cWhere = cWhere + "C36cCodCon=F38lCodCon And C36cOrigen='DIRE' And "
cWhere = cWhere + "C37lRowId=F38lRowCab"

cFromF = "F38l,C36c,C37l"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F38lc")
If !lStado
	*> Cliente sin conceptos a calcular de tipo DIRE: Conceptos directos.
	This.UsrError = "No hay plantilla de tarifas del cliente"
	Use In (Select ("F38lc"))
	Return lStado
EndIf

*> Eliminar los datos ya existentes del periodo.
cWhere = "F35iCodPro='" + This.CurCli + "' And F35iFecCal=" + _GCD(dFechaCalculo)
=f3_deltun("F35i", , cWhere)

*> Crear cursor para los datos a generar.
=CrtCursor("F35i", "F35iDire")

*> Cl�usula de lectura del diario de conceptos directos.
cWhere = 		  "F36mCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F36mFecCal Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst)
cOrder = ""

*> Generar el cursor con los registros a procesar.
lStado = f3_sql("*", "F36m", cWhere, cOrder, , "F36mDire")

Select F36mDire
Go Top

Do While !Eof()
	Scatter Name oF36m

	*> Procesar los diferentes conceptos a generar para el registro actual.
	Select F38lc
	Go Top
	Do While !Eof()
		Scatter Name oF38lc

		If !Empty(oF38lc.F38lCodigo) .And. oF36m.F36mCodCon<>oF38lc.F38lCodigo
			*> La tarifa discrimina por concepto - c�digo de concepto -, y �ste es distinto al del actual.
			Skip
			Loop
		EndIf

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat										&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat										&& M�ximo para generar registro de acumulados.
		cTipoCalculo = Iif (!Empty(oF38lc.C37lTipTrm), oF38lc.C37lTipTrm, "U")	&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF36m.F36mCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad del concepto actual est� dentro del tramo actual.
		If oF36m.F36mUnidad < nValorIni .Or. oF36m.F36mUnidad > nValorFin
			Select F38lc
			Skip
			Loop
		EndIf

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		*>	- Trabajar con tramo de c�lculo (puede estar en blanco).
		*>	- Trabajar con c�digo de concepto (puede estar en blanco).

		*> Clave base del registro.
 		cWhere = "F35iRowCon='" + oF38lc.F38lRowID + "' And F35iRowDet='" + oF36m.F36mRowID + "'"

		lStado = f3_sql("*", "F35i", cWhere, , , "F35iDire")
		Select F35iDire

		If !lStado
			Append Blank
			Replace F35iCodPro With oF38lc.F38lCodPro
			Replace F35iFecCal With dFechaCalculo
			Replace F35iCodigo With oF38lc.F38lCodigo
			Replace F35iCodTrm With oF38lc.F38lCodTrm
			Replace F35iRowCon With oF38lc.F38lRowID
			Replace F35iRowDet With oF36m.F36mRowID
		EndIf

		Do Case
			*> Acumular unidades.
			Case cTipoCalculo=='U'
				Replace F35iCantid With F35iCantid + oF36m.F36mUnidad

			*> Acumular peso.
			Case cTipoCalculo=='P'
				Replace F35iPeso   With F35iPeso   + oF36m.F36mUnidad

			*> Acumular volumen.
			Case cTipoCalculo=='V'
				Replace F35iVolum  With F35iVolum  + oF36m.F36mUnidad

			*> Por defecto: Acumular unidades.
			Otherwise
				Replace F35iCantid With F35iCantid + oF36m.F36mUnidad
		EndCase

		If !lStado
			=f3_instun("F35i", "F35iDire")
		Else
			=f3_updtun("F35i", , , , "F35iDire", cWhere)
		EndIf

		Select F38lc
		Skip
	EndDo

	Select F36mDire
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35iDire"))
Use In (Select ("F36mDire"))
Use In (Select ("F38lc"))

Return

ENDPROC
PROCEDURE _generarfacturadire

*> Generar factura acumulados de conceptos de facturaci�n - conceptos directos.
*> Trabaja a partir de los ficheros de acumulados directos (F35i) y escalado (F38e).
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- Cursor F38eFACT, escalado de precios.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._generarfactura(), generar la factura del cliente y periodo actual.

*> Pendiente:
*>	fecha de c�lculo

Private cWhere, cOrder, cFromF, oF35i, oF38e, dFechaCalculo, nCantidad
Local lStado, cTipoCalculo, cTipoUnidad

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Si no se han cargado los escalados de precios, error.
If !Used("F38eFACT")
	This.UsrError = "No se han cargado los escalados de precios"
	lStado = .F.
	Return lStado
EndIf

*> Cargar los acumulados por concepto.
cWhere = "F35iCodPro='" + This.CurCli + "' And F35iFecCal=" + _GCD(dFechaCalculo) + " And F36mRowId=F35iRowDet"
cFromF = "F35i,F36m"
cOrder = ""

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F35iFACT")
If !lStado
	*> Cliente sin acumulados de tipo DIRE: Conceptos directos.
	This.UsrError = "No hay acumulados concepto de tipo DIRE"
	Use In (Select ("F35iFACT"))
	Return lStado
EndIf

*> En el caso de los conceptos directos es necesario borrar las l�neas de factura previas pues, a diferencia
*> de otros tipos de concepto, en este caso siempre se insertan las l�neas nuevas.
cWhere = "F70lNumFac='" + This._numfraget + "' And F70lCodigo='DIRE'"
=f3_deltun("F70l", , cWhere)

Select F35iFACT
Go Top
Do While !Eof()
	Scatter Name oF35i

	*> Para cada registro de acumulado, buscar a qu� escalado de precios corresponde:
	*>	- Si hay escalado, tomar los valores del escalado.
	*>	- Si no hay escalado, tomar los valores de los acumulados.

	Select F38eFACT
	Locate For F38eRowDet=oF35i.F35iRowCon
	If !Found()
		*> No tiene escalados de precios: Tomar de detalle de conceptos directos.
		*>	 ......
		Select F35iFACT
		Skip
		Loop
	EndIf

	*> Hay escalado de precios: Proceso general.
	Do While Found()
		Scatter Name oF38e

		*> Tipo de c�lculo por defecto. Se asume c�lculo de unidades.
		*> Tipo de c�lculo para tramos gen�ricos (sin tramo).
		*> OJO!!: La referencia a C37l NO es un error. oF38e es el registro de un cursor multi-tabla.
		cTipoCalculo = Iif (!Empty(oF38e.C37lTipTrm), oF38e.C37lTipTrm, "U")
		cTipoUnidad = Space(4)

		*> Si hay tramo, leer ficha para obtener el tipo de unidad (U / P / V).
		If !Empty(oF35i.F35iCodTrm)
			m.C35cCodPro = This.CurCli
			m.C35cCodTrm = oF35i.F35iCodTrm
			If f3_seek("C35c")
				cTipoCalculo = C35c.C35cTipTrm
				cTipoUnidad = C35c.C35cUnidad
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del registro actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				nCantidad = oF35i.F35iCantid

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				nCantidad = oF35i.F35iPeso

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				nCantidad = oF35i.F35iVolum

			*> Resto de casos: Error
			Otherwise
				Select F38eFACT
				Continue
				Loop
		EndCase

		If nCantidad < oF38e.F38eCanDsd .Or. nCantidad > oF38e.F38eCanHst
			Select F38eFACT
			Skip
			Loop
		EndIf

		*> Generar la l�nea de detalle en la factura.
		cWhere = "F70lRowDet='" + oF38e.F38eRowID + "' And F70lNumFac='" + This._numfraget + "'"
		lStado = f3_sql("*", "F70l", cWhere, , , "F70lFACT")

		Select F70lFACT

		If !lStado
			*> Generar, si cal, la cabecera de la factura.
			If Empty(This._numfraget)
				=This._generarcabecerafactura()
			EndIf

			*> Crear una nueva l�nea. OJO!!: oF38e hace referencia a un cursor multi-tabla.
			Select F70lFACT
			Append Blank
			Replace F70lCodPro With This.CurCli, ;
			        F70lCodTar With This.CurTar, ;
			        F70lNumFac With This._numfraget, ;
			        F70lRowDet With oF38e.F38eRowID, ;
			        F70lBasImp With 0, ;
			        F70lImpIva With 0, ;
			        F70lImpEqv With 0
		EndIf

		Replace F70lCodCon With oF38e.F38lTipCon, ;
		        F70lCodigo With oF38e.F38lCodCon, ;
		        F70lCodTrm With oF38e.F38lCodTrm, ;
		        F70lTipUni With cTipoUnidad, ;
		        F70lDescri With oF35i.F36mDescri, ;
		        F70lRowId  With Ora_NewNum("NROW"), ;
		        F70lUniCal With nCantidad, ;
		        F70lUniCor With nCantidad, ;
		        F70lPreCal With Iif(!Empty(oF35i.F36mPrecio), oF35i.F36mPrecio, oF38e.F38ePrecio), ;
		        F70lPreCor With Iif(!Empty(oF35i.F36mPrecio), oF35i.F36mPrecio, oF38e.F38ePrecio), ;
		        F70lCodIva With oF38e.F38lCodIva, ;
		        F70lPrtCnt With oF38e.F38lPrtCnt, ;
		        F70lPrtPrc With oF38e.F38lPrtPrc, ;
		        F70lPrtImp With oF38e.F38lPrtImp

		=f3_instun("F70l", "F70lFACT")

		*> Trabajar con el siguiente escalado. Generalmente solo hay uno.
		Select F38eFACT
		Continue
	EndDo

	*> Trabajar con el siguiente acumulado de conceptos.
	Select F35iFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35iFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC
PROCEDURE _generaracumuladosfactura

*> Calcular los importes acumulados de la factura del cliente actual.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Detalle de factura (F70l).

*> Genera :
*>	- F70i, acumulados de factura.

*> Recibe:
*>	- cFactura, factura a calcular (tiene que estar generada). � bien
*>	- This._numfraget, factura calculada.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularFacturas(), generaci�n de facturas.
*>	- This.CalcularAcumuladosFactura(), generaci�n externa de acumulados de factura.
*>	- This.CalcularFacturasDirectas(), generaci�n de facturas manuales.

Parameters cNumFac

Private cWhere, cFactura
Local lStado, nInx, nTipos, lHayTipo, cIndex, oF70c, oF70l, oF34v
Local nBasImp, nTotIva, nTotEqv
Local Array vTotales(5, 6)

This.UsrError = ""

cFactura = Iif(Empty(cNumFac), This._numfraget, cNumFac)

m.F70cNumFac = cFactura
If !f3_seek("F70c")
	This.UsrError = "No existe la factura"
	Return .F.
EndIf

Select F70c
Scatter Name oF70c

=CrtCursor("F70i", "F70iCALC")

cWhere = "F70lNumFac='" + cFactura + "'"
lStado = f3_sql("*", "F70l", cWhere, , , "F70lCALC")

If !lStado
	If _xier <= 0
		This.UsrError = "Error a cargar detalle"
	Else
		This.UsrError = "No hay detalle"
	EndIf

	Use In (Select ("F70lCALC"))
	Use In (Select ("F70iCALC"))
	Return lStado
EndIf

Store 0 To nTipos
Store .T. To lEstado

*> Inicializar el array.
For nInx = 1 To 5
	vTotales(nInx, 1) = 0                           && Base imponible.
	vTotales(nInx, 2) = Space(4)                    && C�digo de impuesto.
	vTotales(nInx, 3) = 0                           && % de IVA.
	vTotales(nInx, 4) = 0                           && Importe IVA.
	vTotales(nInx, 5) = 0                           && % de equivalencia.
	vTotales(nInx, 6) = 0                           && Importe equivalencia.
EndFor

Select F70lCALC
Go Top
Do While !Eof()
	Store .F. To lHayTipo
	Scatter Name oF70l

	*> Buscar el tipo de impuesto en el array.
	For nInx = 1 To nTipos
		If vTotales(nInx, 2)==oF70l.F70lCodIva
			Store .T. To lHayTipo
			Exit
		EndIf
	EndFor

	*> A�adir un un nuevo tipo al array.
	If !lHayTipo
		nTipos = Iif(nTipos < 5, nTipos + 1, nTipos)
		nInx = nTipos	

		*> Leer los % de impuesto.
		m.F34vCodCon = oF70l.F70lCodIva
		If f3_seek("F34v")
			Select F34v
			Scatter Name oF34v
		Else
			Select F34v
			Scatter Name oF34v Blank
		EndIf

		vTotales(nInx, 2) = oF70l.F70lCodIva
		vTotales(nInx, 3) = oF34v.F34vValIva
		vTotales(nInx, 5) = oF34v.F34vValEqv
	EndIf

	*> Calcular los importes de la l�nea.
	nBasImp = oF70l.F70lUniCor * oF70l.F70lPreCor
	nTotIva = nBasImp * oF34v.F34vValIva / 100
	nTotEqv = nBasImp * oF34v.F34vValEqv / 100

	*> Redondear los importes a x decimales.
	nBasImp = Ceiling(nBasImp * 10000) / 10000
	nTotIva = Ceiling(nTotIva * 10000) / 10000
	nTotEqv = Ceiling(nTotEqv * 10000) / 10000

	*> Acumular importes.
	vTotales(nInx, 1) = vTotales(nInx, 1) + nBasImp
	vTotales(nInx, 4) = vTotales(nInx, 4) + nTotIva
	vTotales(nInx, 6) = vTotales(nInx, 6) + nTotEqv

	*> Actualizar los valores calculados de la l�nea.
	Select F70lCALC
	Replace F70lBasImp With nBasImp, ;
	        F70lImpIva With nTotIva, ;
	        F70lImpEqv With nTotEqv

	cWhere = "F70lRowId='" + oF70l.F70lRowId + "'"
	=f3_updtun("F70l", , , , "F70lCALC")

	Select F70lCALC
	Skip
EndDo

*> Borrar los acumulados previos.
cWhere = "F70iNumFac='" + cFactura + "'"
=f3_deltun("F70i", , cWhere)

*> Pasar los acumulados del array al cursor F70I.
Select F70iCALC
Append Blank

For nInx = 1 To 4
	cIndex = Str(nInx, 1, 0)

	Replace F70iBasIm&cIndex With vTotales(nInx, 1), ;
	        F70iCodIm&cIndex With vTotales(nInx, 2), ;
	        F70iValIv&cIndex With vTotales(nInx, 3), ;
	        F70iTotIv&cIndex With vTotales(nInx, 4), ;
	        F70iValEq&cIndex With vTotales(nInx, 5), ;
	        F70iTotEq&cIndex With vTotales(nInx, 6)
EndFor

Replace F70iNumFac With cFactura, ;
        F70iCodPro With oF70c.F70cCodPro, ;
        F70iBasImR With vTotales(nInx, 1), ;
        F70iTotIvR With vTotales(nInx, 4), ;
        F70iTotEqR With vTotales(nInx, 6)

=f3_instun("F70i", "F70iCALC")

Use In (Select ("F70lCALC"))
Use In (Select ("F70iCALC"))

Return

ENDPROC
PROCEDURE _generardatospagofactura

*> Guardar las condiciones de pago de la factura del cliente actual.
*> M�todo privado de la clase.

*> Genera :
*>	- F70p, condiciones de pago de factura.

*> Recibe:
*>	- cFactura, factura a calcular (tiene que estar generada). � bien
*>	- This._numfraget, factura calculada.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularFacturas(), generaci�n de facturas.
*>	- This.CalcularFacturasDirectas(), generaci�n de facturas manuales.

Parameters cNumFac

Private cWhere, cFactura
Local lStado, oF32c, oF70c

This.UsrError = ""

cFactura = Iif(Empty(cNumFac), This._numfraget, cNumFac)

m.F70cNumFac = cFactura
If !f3_seek("F70c")
	This.UsrError = "No existe la factura"
	Return .F.
EndIf

Select F70c
Scatter Name oF70c

*> Tomar las condiciones de pago del cliente.
m.F32cCodPro = oF70c.F70cCodPro
If !f3_seek("F32c")
	This.UsrError = "Cliente sin condiciones de pago"
	Return .F.
EndIf

Select F32c
Scatter Name oF32c

*> Borrar los datos previos.
cWhere = "F70pNumFac='" + cFactura + "'"
=f3_deltun("F70p", , cWhere)

=CrtCursor("F70p", "F70pPAGO")

Select F70pPAGO
Append Blank
Replace F70pCodPro With oF70c.F70cCodPro, ;
        F70pNumFac With cFactura, ;
        F70pDiapg1 With oF32c.F32cDiaPg1, ;
        F70pDiapg2 With oF32c.F32cDiaPg2, ;
        F70pDiapg3 With oF32c.F32cDiaPg3, ;
        F70pDia1er With oF32c.F32cDia1er, ;
        F70pDiaiev With oF32c.F32cDiaIEv, ;
        F70pFecFra With oF32c.F32cFecFra, ;
        F70pNumRec With oF32c.F32cNumRec, ;
        F70pDtoDc1 With oF32c.F32cDtoDc1, ;
        F70pDtoDc2 With oF32c.F32cDtoDc2, ;
        F70pDtoDpp With oF32c.F32cDtoDpp, ;
        F70pCodFpg With oF32c.F32cCodFpg, ;
        F70pDesFpg With oF32c.F32cDesFpg, ;
        F70pDomFpg With oF32c.F32cDomFpg, ;
        F70pPobFpg With oF32c.F32cPobFpg, ;
        F70pCtaBan With oF32c.F32cCtaBan, ;
        F70pNbaNco With oF32c.F32cNbanco

=f3_instun("F70p", "F70pPAGO")

Use In (Select("F70pPAGO"))
Return

ENDPROC
PROCEDURE calcularacumuladosfactura

*> Proceso de rec�lculo de acumulado de importes de una factura.
*> La factura deber�a de estar previamente generada.
*> Uso preferente para edici�n externa de la factura, pues la clase ya genera los acumulados al generar la factura.

*> Recibe:
*>	- cNumFac, factura a calcular.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cNumFac

Local lStado

This.UsrError = ""
lStado = This._generaracumuladosfactura(cNumFac)

Return lStado

ENDPROC
PROCEDURE calculardatospagofactura

*> Proceso de rec�lculo de las condiciones de pago de una factura.
*> La factura deber�a de estar previamente generada.
*> Uso preferente para edici�n externa de la factura, pues la clase ya genera las condiciones de pago al generar la factura.

*> Recibe:
*>	- cNumFac, factura a calcular.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cNumFac

Local lStado

This.UsrError = ""
lStado = This._generardatospagofactura(cNumFac)

Return lStado

ENDPROC
PROCEDURE eliminarfactura

*> Eliminar una factura.

*> Recibe:
*>	- cNumFac, factura a eliminar.
*>	- cModo, modo de borrado de la factura:
*>		T: Eliminar toda la factura.
*>		D: Eliminar las l�neas de detalle.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cNumFac, cModo

Private cWhere
Local lStado, cLocalModo

Store "" To This.UsrError
lStado = .T.
cLocalModo = Iif(Type('cModo')=='C', cModo, "T")

Do Case
	*> Eliminar TODA la factura.
	Case cLocalModo=='T'
		lStado = This._eliminarfactura(cNumFac)

	*> Eliminar las l�neas de detalle.
	Case cLocalModo=='D'
		lStado = This._eliminardetallefactura(cNumFac)
EndCase

Return lStado

ENDPROC
PROCEDURE _eliminarfactura

*> Eliminar una factura completa.
*> M�tdo privado de la clase.

*> Recibe:
*>	- cNumFac, factura a eliminar.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.EliminarFactura, borrar una factura, total � parcialmente.

Parameters cNumFac

Local lStado

Store "" To This.UsrError
lStado = .T.

*> Validar que la factura existe.
m.F70cNumFac = cNumFac
If !f3_Seek("F70c")
	This.UsrError = "La factura no existe"
	Return lStado
EndIf

*> Eliminar las l�neas de detalle.
lStado = This._eliminardetallefactura(cNumFac)
If lStado
	*> Elimina los datos de cabecera (pago, acumulados, ...)
	lStado = This._eliminardatosfactura(cNumFac)
EndIf

Return lStado

ENDPROC
PROCEDURE _eliminardetallefactura

*> Eliminar las l�neas de detalle de una factura.
*> M�tdo privado de la clase.

*> Recibe:
*>	- cNumFac, factura a eliminar.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._eliminarfactura, eliminar una factura completa.

Parameters cNumFac

Store "" To This.UsrError

*> Eliminar las l�neas.
cWhere = "F70lNumFac='" + cNumFac + "'"
=f3_deltun("F70l", , cWhere)

Return

ENDPROC
PROCEDURE _eliminardatosfactura

*> Eliminar los datos de cabecera de una factura.
*> M�tdo privado de la clase.

*> Recibe:
*>	- cNumFac, factura a eliminar.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._eliminarfactura, eliminar una factura completa.

Parameters cNumFac

Store "" To This.UsrError

*> Eliminar los acumulados.
cWhere = "F70iNumFac='" + cNumFac + "'"
=f3_deltun("F70i", , cWhere)

*> Eliminar las condiciones de pago.
cWhere = "F70pNumFac='" + cNumFac + "'"
=f3_deltun("F70p", , cWhere)

*> Eliminar la cabecera de la factura.
cWhere = "F70cNumFac='" + cNumFac + "'"
=f3_deltun("F70c", , cWhere)

Return

ENDPROC
PROCEDURE _calculardocuas

*> Calcular acumulados de conceptos de facturaci�n - albaranes de salida.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Albaranes de salida (F27c/F27l).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- F38lc, cursor con la tarifa de conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calculardocu(), calcular ficheros de acumulados de documentos.

*> Pendiente:
*>	fecha de c�lculo

Private cField, cWhere, cFromF, cGroup, oF18m, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

If !Used("F38lc")
	This.UsrError = "No existen los conceptos a facturar"
	lStado = .F.
	Return lStado
EndIf

*> Validar si el concepto a calcular est� definido.
Select F38lc
Locate For C38dCodCon=='ALBS'
lStado = Found()
If !lStado
	*> No est� definido en las tarifas del cliente.
	This.UsrError = "Concepto 'Albaranes de Salida' no definido en tarifas"
	Return lStado
EndIf

*> Cursores de trabajo.
Use In (Select ("F27cDocu"))
Use In (Select ("F35gDocu"))

*> Crear cursores para los datos a generar.
=CrtCursor("F35g", "F35gDocu")

=CrtFCursor("F27cDocu", [TBL=F27c,FLD=F27cCodPro,FLD=F27cNumAlb])
=AddFldToCursor("F27cDocu", [NAME=TotLin,TYPE=N,LENGTH=3,DECIMALS=0])
=AddFldToCursor("F27cDocu", [NAME=PesDoc,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor("F27cDocu", [NAME=VolDoc,TYPE=N,LENGTH=12,DECIMALS=3])

*> Cl�usula de lectura de albaranes de salida.
cWhere = 		  "F27cCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F27cFecAlb Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F27lNumAlb=F27cNumAlb"

cField = "F27cCodPro,F27cNumAlb," + _GCN("Count(*)", 0) + " As TotLin"
cFromF = "F27c,F27l"
cGroup = "F27cCodPro,F27cNumAlb"

*> Generar el cursor con los documentos a procesar.
lStado = f3_sql(cField, cFromF, cWhere, , cGroup, "F27cDocu")

Select F27cDocu
Go Top

Do While !Eof()
	Scatter Name oF27c

	*> Procesar los diferentes conceptos a generar para el documento actual.
	*> S�lo los conceptos que son de albaranes de salida.
	Select F38lc
	Locate For C38dCodCon=='ALBS'
	Do While Found()
		Scatter Name oF38lc

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF27c.F27cCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del albar�n actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If !Between(oF27c.TotLin, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If !Between(oF27c.PesDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If !Between(oF27c.VolDoc, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Continue
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		cWhere = "F35gRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35g", cWhere, , , "F35gDocu")
		Select F35gDocu

		If !lStado
			Append Blank
			Replace F35gCodPro With oF38lc.F38lCodPro
			Replace F35gFecCal With dFechaCalculo
			Replace F35gCodigo With oF38lc.F38lCodigo
			Replace F35gCodTrm With oF38lc.F38lCodTrm
			Replace F35gRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35gCantid With F35gCantid + 1
		Replace F35gPeso   With F35gPeso   + oF27c.PesDoc
		Replace F35gVolum  With F35gVolum  + oF27c.VolDoc

		If !lStado
			=f3_instun("F35g", "F35gDocu")
		Else
			=f3_updtun("F35g", , , , "F35gDocu", cWhere)
		EndIf

		Select F38lc
		Continue
	EndDo

	Select F27cDocu
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F27cDocu"))

Return

ENDPROC
PROCEDURE _calculardocule

*> Calcular acumulados de conceptos de facturaci�n - l�neas de entrada.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Documentos de entrada, detalle (F18l).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.CurTar, c�digo de tarifa.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- F38lc, cursor con la tarifa de conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._calculardocu(), calcular ficheros de acumulados de documentos.

*> Pendiente:
*>	fecha de c�lculo

*> Historial de modificaciones:
*> 16.05.2008 (AVC) Corregir codificaci�n de conceptos LINE.

Private cField, cWhere, cFromF, oF18l, dFechaCalculo
Local lStado, oF38lc, nValorIni, nValorFin, cTipoCalculo
Local nPesoLinea, nVolumenLinea

Store .T. To lStado
This.UsrError = ""

*> PROVISIONAL !!!!!!
dFechaCalculo = This.FchHst

If !Used("F38lc")
	This.UsrError = "No existen los conceptos a facturar"
	lStado = .F.
	Return lStado
EndIf

*> Validar si el concepto a calcular est� definido.
Select F38lc
Locate For C38dCodCon=='LINE'
lStado = Found()
If !lStado
	*> No est� definido en las tarifas del cliente.
	This.UsrError = "Concepto 'L�neas de Entrada' no definido en tarifas"
	Return lStado
EndIf

*> Cursores de trabajo.
Use In (Select ("F18lDocu"))
Use In (Select ("F35gDocu"))

*> Crear cursores para los datos a generar.
=CrtCursor("F35g", "F35gDocu")
=CrtCursor("F18l", "F18lDocu")

*> Cl�usula de lectura de l�neas de salida.
cWhere = 		  "F18lCodPro='" + This.CurCli + "' And "
cWhere = cWhere + "F18lFecPed Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And "
cWhere = cWhere + "F18cCodPro=F18lCodPro And F18cTipDoc=F18lTipDoc And F18cNumDoc=F18lNumDoc And "
cWhere = cWhere + "F18cEstado>='3'"

cField = "*"
cFromF = "F18l,F18c"

*> Generar el cursor con las l�neas de documento a procesar.
lStado = f3_sql(cField, cFromF, cWhere, , , "F18lDocu")

Select F18lDocu
Go Top

Do While !Eof()
	Scatter Name oF18l

	*> Procesar los diferentes conceptos a generar para el documento actual.
	*> S�lo los conceptos que son de l�neas de entrada.
	Select F38lc
	Locate For C38dCodCon=='LINE'
	Do While Found()
		Scatter Name oF38lc

		*> Obtener el peso y el volumen de los datos generados.
		=This._pesovolumenmov(oF18l.F18lCodPro, oF18l.F18lCodArt, oF18l.F18lCanSer)

		nPesoLinea = This.oUbicObj.PesOcu
		nVolumenLinea = This.oUbicObj.VolOcu

		*> Leer los par�metros del tramo de c�lculo, si lo hay.
		nValorIni = This._valorminimofloat					&& M�nimo para generar registro de acumulados.
		nValorFin =  This._valormaximofloat					&& M�ximo para generar registro de acumulados.
		cTipoCalculo = "U"									&& Tipo: Unidades / Volumen / Peso.

		If !Empty(oF38lc.F38lCodTrm)
			m.C35cCodPro = oF18l.F18lCodPro
			m.C35cCodTrm = oF38lc.F38lCodTrm
			If f3_seek("C35c")
				nValorIni = C35c.C35cValIni
				nValorFin = C35c.C35cValFin
				cTipoCalculo = C35c.C35cTipTrm
			EndIf
		EndIf

		*> Validar si la cantidad / peso / volumen del documento actual est� dentro del tramo actual.
		Do Case
			*> Calcular por unidades.
			Case cTipoCalculo=='U'
				If !Between(oF18l.F18lCanSer, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por peso.
			Case cTipoCalculo=='P'
				If !Between(nPesoLinea, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Calcular por volumen.
			Case cTipoCalculo=='V'
				If !Between(nVolumenLinea, nValorIni, nValorFin)
					Select F38lc
					Continue
					Loop
				EndIf

			*> Resto de casos: Error.
			Otherwise
				Select F38lc
				Continue
				Loop
		EndCase

		*> Obtener el registro activo de acumulados. Tener en cuenta:
		cWhere = "F35gRowCon='" + oF38lc.F38lRowID + "'"

		lStado = f3_sql("*", "F35g", cWhere, , , "F35gDocu")
		Select F35gDocu

		If !lStado
			Append Blank
			Replace F35gCodPro With oF38lc.F38lCodPro
			Replace F35gFecCal With dFechaCalculo
			Replace F35gCodigo With oF38lc.F38lCodigo
			Replace F35gCodTrm With oF38lc.F38lCodTrm
			Replace F35gRowCon With oF38lc.F38lRowID
		EndIf

		Replace F35gCantid With F35gCantid + 1
		Replace F35gPeso   With F35gPeso   + nPesoLinea
		Replace F35gVolum  With F35gVolum  + nVolumenLinea

		If !lStado
			=f3_instun("F35g", "F35gDocu")
		Else
			=f3_updtun("F35g", , , , "F35gDocu", cWhere)
		EndIf

		Select F38lc
		Continue
	EndDo

	Select F18lDocu
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F35gDocu"))
Use In (Select ("F18lDocu"))

Return

ENDPROC
PROCEDURE copiartarifa

*> Copiar una tarifa sobre otra. Si �sta ya existe actualiza los datos (validez, por ejemplo).

*> Recibe:
*>	- cPropietario, c�digo de cliente.
*>	- cTarifa, c�digo de tarifa.
*>	- cDescri, descripci�n nueva tarifa.
*>	- cAbrevi, descripci�n nueva tarifa.
*>	- cBorrar, flag de borrado de la tarifa antigua, si ya existe.

*> o bien
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino (puede ser el mismo).
*>	- This.TarDsd, tarifa origen (debe existir).
*>	- This.TarHst, tarifa destino.
*>	- This.FchDsd, fecha vaidez inicial.
*>	- This.FchHst, fecha validez final.
*>	- This.Descri, Descripci�n nueva tarifa.
*>	- This.Abrevi, Descripci�n abreviada nueva tarifa.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cPropietario, cTarifa, cDescri, cAbrevi, cBorrar

Local lStado

Store "" To This.UsrError
lStado = .T.

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.TarDsd = Iif(Type('cTarifa')=='C', cTarifa, .TarDsd)
	.TarHst = Iif(Type('cTarifa')=='C', cTarifa, .TarHst)
	.Descri = Iif(Type('cDescri')=='C', cDescri, .Descri)
	.Abrevi = Iif(Type('cAbrevi')=='C', cAbrevi, .Abrevi)
	.BorrarTar = Iif(Type('cBorrar')=='C', cBorrar, .BorrarTar)
EndWith

*> Validar tarifa origen.
With This
	.CodPro = .CliDsd
	.CodTar = .TarDsd
	lStado = ._validarplantilla()
	If !lStado
		*> El mensaje de error ya viene asignado.
		Return lStado
	EndIf
EndWith

*> Generar la tarifa, tramos, conceptos, escalado de precios, etc.
*> Los posibles mensajes de error ya vienen asignados.
lStado = This._copiartarifa()

Return lStado

ENDPROC
PROCEDURE _copiartarifa

*> Procesos de copia de tarifa de conceptos.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino (puede ser el mismo).
*>	- This.TarDsd, tarifa origen (debe existir).
*>	- This.TarHst, tarifa destino.
*>	- This.FchDsd, fecha vaidez inicial.
*>	- This.FchHst, fecha validez final.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CopiarTarifa, copiar tarifa de conceptos.

*> Genera:
*>	- C35C, archivo general de tramos de cliente.

Private cWhere
Local lStado

This.UsrError = ""

*> Borrar los datos previos.
If This.BorrarTar=='S'
	This.CodPro = This.CliHst
	This.CodTar = This.TarHst
	=This._eliminartarifa()
EndIf

*> Crear la lista de tramos del cliente (C35C).
lStado = This._copiartramoscliente()
If !lStado
	*> Los mensajes de error ya vienen asignados.
	Return lStado
EndIf

*> Crear las tarifas del cliente (F38C).
lStado = This._copiartarifascliente()
If !lStado
	*> Los mensajes de error ya vienen asignados.
	Return lStado
EndIf

*> Crear los conceptos del cliente (C37L, F38L, F38E).
lStado = This._copiartarifaconceptos()
If !lStado
	*> Los mensajes de error ya vienen asignados.
	Return lStado
EndIf

*!*	*> Crear la plantilla de conceptos del cliente (C37L).
*!*	lStado = This._copiarplantillacliente()
*!*	If !lStado
*!*		*> Los mensajes de error ya vienen asignados.
*!*		Return lStado
*!*	EndIf

*!*	*> Crear los conceptos del cliente (F38L).
*!*	lStado = This._copiarconceptoscliente()
*!*	If !lStado
*!*		*> Los mensajes de error ya vienen asignados.
*!*		Return lStado
*!*	EndIf

*!*	*> Crear el escalado de precios del cliente (F38E).
*!*	lStado = This._copiarescaladocliente()
*!*	If !lStado
*!*		*> Los mensajes de error ya vienen asignados.
*!*		Return lStado
*!*	EndIf

Return lStado

ENDPROC
PROCEDURE _copiartramoscliente

*> Copia los tramos de un cliente sobre otro, Tabla C35C.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._copiartarifa, copiar tarifa de conceptos.

Private cWhere, oTrm
Local lStado

This.UsrError = ""
lStado = .T.

*> Si cliente origen y destino son iguales, no se copia. Solo se actualizan datos del origen.
If This.CliDsd==This.CliHst
	Return lStado
EndIf

*> Borrar los datos previos.
If This.BorrarTar=='S'
	cWhere = "C35cCodPro='" + This.CliHst + "'"
	=f3_deltun("C35c", , cWhere)
EndIf

*> Cargar los datos origen.
cWhere = "C35cCodPro='" + This.CliDsd + "'"
lStado = f3_sql("*", "C35c", cWhere, , , "C35CTRM")
If !lStado
	This.UsrError = "No hay tramos del cliente origen"
	Use In (Select ("C35CTRM"))
	Return lStado
EndIf

*> Se cambia el cliente en el cursor.
Select C35CTRM
Replace All C35cCodPro With This.CliHst
Go Top

Do While !Eof()
	Scatter Name oTrm

	cWhere = "C35cCodPro='" + oTrm.C35cCodPro + "' And C35cCodTrm='" + oTrm.C35cCodTrm + "'"
	If f3_sql("*", "C35c", cWhere, , , "_C35cCur")
		*> Actualizar datos
		=f3_updtun("C35c", , , , "C35CTRM")
	Else
		*> Insertar datos
		=f3_instun("C35c", "C35CTRM")
	EndIf

	Select C35CTRM
	Skip
EndDo

Use In (Select ("_C35cCur"))
Use In (Select ("C35CTRM"))

Return

ENDPROC
PROCEDURE _copiartarifascliente

*> Copia las tarifas de un cliente sobre otro, tabla F38C.
*> Crea tanto tarifas nuevas para elmismo cliente como para otros, as� como actualizar una tarifa ya existente.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino.
*>	- This.TarDsd, tarifa origen.
*>	- This.TarHst, tarifa destino.
*>	- This.FchDsd, fecha vaidez inicial.
*>	- This.FchHst, fecha validez final.
*>	- This.Descri, Descripci�n nueva tarifa.
*>	- This.Abrevi, Descripci�n abreviada nueva tarifa.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._copiartarifa, copiar tarifa de conceptos.

Private cWhere, oTar
Local lStado

This.UsrError = ""
lStado = .T.

*> Cargar los datos origen.
cWhere = "F38cCodPro='" + This.CliDsd + "' And F38cCodTar='" + This.TarDsd + "'"
lStado = f3_sql("*", "F38c", cWhere, , , "F38CTARI")
If !lStado
	This.UsrError = "No existe la tarifa del cliente origen"
	Use In (Select ("F38CTARI"))
	Return lStado
EndIf

*> Se cambia el cliente y/o tarifa en el cursor. Si origen y destino son iguales se actualiza el origen.
Select F38CTARI
Go Top
Replace F38cCodPro With This.CliHst
Replace F38cCodTar With This.TarHst
Replace F38cFecDes With This.FchDsd
Replace F38cFecHas With This.FchHst

If !Empty(This.Descri)
	Replace F38cDescri With This.Descri
EndIf
If !Empty(This.Abrevi)
	Replace F38cAbrevi With This.Abrevi
EndIf

Scatter Name oTar

cWhere = "F38cCodPro='" + oTar.F38cCodPro + "' And F38cCodTar='" + oTar.F38cCodTar + "'"
If f3_sql("*", "F38c", cWhere, , , "_F38cCur")
	*> Actualizar datos
	=f3_updtun("F38c", , , , "F38CTARI")
Else
	*> Insertar datos
	=f3_instun("F38c", "F38CTARI")
EndIf

Use In (Select ("_F38cCur"))
Use In (Select ("F38CTARI"))

Return

ENDPROC
PROCEDURE _copiarplantillacliente

*> Copia la plantilla de conceptos de un cliente sobre otro, Tabla C37L.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino.
*>	- This.TarDsd, tarifa origen.
*>	- This.TarHst, tarifa destino.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._copiartarifa, copiar tarifa de conceptos.

Private cWhere, oPla
Local lStado

This.UsrError = ""
lStado = .T.

*> Borrar los datos previos.
If This.BorrarTar=='S'
	cWhere = "C37lCodPro='" + This.CliHst + "' And C37lCodTar='" + This.TarHst + "'"
	=f3_deltun("C37l", , cWhere)
EndIf

*> Cargar los datos origen.
cWhere = "C37lCodPro='" + This.CliDsd + "' And C37lCodTar='" + This.TarDsd + "'"
lStado = f3_sql("*", "C37l", cWhere, , , "C37LPLA")
If !lStado
	This.UsrError = "No hay plantilla de conceptos del cliente origen"
	Use In (Select ("C37LPLA"))
	Return lStado
EndIf

*> Se cambia el cliente / tarifa en el cursor.
Select C37LPLA
Replace All C37lCodPro With This.CliHst
Replace All C37lCodTar With This.TarHst
Go Top

Do While !Eof()
	Scatter Name oPla

	cWhere = 		  "C37lCodPro='" + oPla.C37lCodPro + "' And "
	cWhere = cWhere + "C37lCodTar='" + oPla.C37lCodTar + "' And "
	cWhere = cWhere + "C37lCodCon='" + oPla.C37lCodCon + "' And "
	cWhere = cWhere + "C37lCodOrg='" + oPla.C37lCodOrg + "' And "
	cWhere = cWhere + "C37lCodTrm='" + oPla.C37lCodTrm + "' And "
	cWhere = cWhere + "C37lRowId='" + oPla.C37lRowId + "'"

	If f3_sql("*", "C37l", cWhere, , , "_C37lCur")
		*> Actualizar datos
		=f3_updtun("C37l", , , , "C37LPLA")
	Else
		*> Insertar datos
		Select C37LPLA
		Replace C37lRowID With Ora_NewNum("NROW")
		=f3_instun("C37l", "C37LPLA")
	EndIf

	Select C37LPLA
	Skip
EndDo

Use In (Select ("_C37lCur"))
Use In (Select ("C37LPLA"))

Return

ENDPROC
PROCEDURE _copiarconceptoscliente

*> Copia la definici�n de conceptos de un cliente sobre otro, Tabla F38L.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino.
*>	- This.TarDsd, tarifa origen.
*>	- This.TarHst, tarifa destino.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._copiartarifa, copiar tarifa de conceptos.

Private cWhere, oCon, oPla
Local lStado

This.UsrError = ""
lStado = .T.

*> Borrar los datos previos.
If This.BorrarTar=='S'
	cWhere = "F38lCodPro='" + This.CliHst + "' And F38lCodTar='" + This.TarHst + "'"
	=f3_deltun("F38l", , cWhere)
EndIf

*> Cargar los datos origen.
cWhere = "F38lCodPro='" + This.CliDsd + "' And F38lCodTar='" + This.TarDsd + "'"
lStado = f3_sql("*", "F38l", cWhere, , , "F38LCONC")
If !lStado
	This.UsrError = "No hay definici�n de conceptos del cliente origen"
	Use In (Select ("F38lCONC"))
	Return lStado
EndIf

*> Se cambia el cliente / tarifa en el cursor.
Select F38LCONC
Replace All F38lCodPro With This.CliHst
Replace All F38lCodTar With This.TarHst
Go Top

Do While !Eof()
	Scatter Name oCon

	*> Obtener el registro de la plantilla de conceptos.
	cWhere = 		  "C37lCodPro='" + oCon.F38lCodPro + "' And "
	cWhere = cWhere + "C37lCodTar='" + oCon.F38lCodTar + "' And "
	cWhere = cWhere + "C37lCodCon='" + oCon.F38lCodCon + "' And "
	cWhere = cWhere + "C37lCodTrm='" + oCon.F38lCodTrm + "' And "
	cWhere = cWhere + "C37lRowId='" + oCon.C37lRowId + "'"

	If !f3_sql("*", "C37l", cWhere, , , "_C37lCur")
		This.UsrError = "No existe plantilla generada cliente destino"
		Use In (Select ("_C37lCur"))
		Return lStado
	EndIf

	Select _C37lCur
	Go Top
	Scatter Name oPla

	cWhere = 		  "F38lCodPro='" + oCon.F38lCodPro + "' And "
	cWhere = cWhere + "F38lCodTar='" + oCon.F38lCodTar + "' And "
	cWhere = cWhere + "F38lCodCon='" + oCon.F38lCodCon + "' And "
	cWhere = cWhere + "F38lCodTrm='" + oCon.F38lCodTrm + "' And "
	cWhere = cWhere + "F38lCodigo='" + oCon.F38lCodigo + "'"

	If f3_sql("*", "F38l", cWhere, , , "_F38lCur")
		*> Actualizar datos
		Select F38LCONC
		Replace F38lRowCab With oPla.C37lRowId
		=f3_updtun("F38l", , , , "F38LCONC")
	Else
		*> Insertar datos, con nuevo RowID.
		Select F38LCONC
		Replace F38lRowId With Ora_NewNum("NROW")
		Replace F38lRowCab With oPla.C37lRowId
		=f3_instun("F38l", "F38LCONC")
	EndIf

	Select F38LCONC
	Skip
EndDo

Use In (Select ("_C37lCur"))
Use In (Select ("_F38lCur"))
Use In (Select ("F38LCONC"))

Return

ENDPROC
PROCEDURE _copiarescaladocliente

*> Copia el escalado de precios de un cliente sobre otro, Tabla F38E.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino.
*>	- This.TarDsd, tarifa origen.
*>	- This.TarHst, tarifa destino.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._copiartarifa, copiar tarifa de conceptos.

Private cWhere, oPrc, oPla
Local lStado

This.UsrError = ""
lStado = .T.

*> Borrar los datos previos.
If This.BorrarTar=='S'
	cWhere = "F38eCodPro='" + This.CliHst + "' And F38eCodTar='" + This.TarHst + "'"
	=f3_deltun("F38e", , cWhere)
EndIf

*> Cargar los datos origen.
cWhere = "F38eCodPro='" + This.CliDsd + "' And F38eCodTar='" + This.TarDsd + "'"
lStado = f3_sql("*", "F38e", cWhere, , , "F38EPRC")
If !lStado
	This.UsrError = "No hay escalado de precios del cliente origen"
	Use In (Select ("F38EPRC"))
	Return lStado
EndIf

*> Se cambia el cliente / tarifa en el cursor.
Select F38EPRC
Replace All F38eCodPro With This.CliHst
Replace All F38eCodTar With This.TarHst
Go Top

Do While !Eof()
	Scatter Name oPrc

	*> Buscar la plantilla de conceptos, para tomar el RowID.
	cWhere = "F38lRowID='" + oPrc.F38eRowDet + "'"
	If !f3_sql("*", "F38l", cWhere, , , "_F38lCur")
		This.UsrError = "No existe ID concepto generado cliente destino"
		Use In (Select ("_F38lCur"))
		Return lStado
	EndIf

	Select _F38lCur
	Go Top
	Scatter Name oPla

	cWhere = 		  "F38lCodPro='" + oPla.F38lCodPro + "' And "
	cWhere = cWhere + "F38lCodTar='" + oPla.F38lCodTar + "' And "
	cWhere = cWhere + "F38lCodCon='" + oPla.F38lCodCon + "' And "
	cWhere = cWhere + "F38lCodTrm='" + oPla.F38lCodTrm + "' And "
	cWhere = cWhere + "F38lCodigo='" + oPla.F38lCodigo + "'"

	If !f3_sql("*", "F38l", cWhere, , , "_F38lCur")
		This.UsrError = "No existe concepto generado cliente destino"
		Use In (Select ("_F38lCur"))
		Return lStado
	EndIf

	Select _F38lCur
	Go Top
	Scatter Name oPla

	cWhere = 		  "F38eCodPro='" + oPrc.F38eCodPro + "' And "
	cWhere = cWhere + "F38eCodTar='" + oPrc.F38eCodTar + "' And "
	cWhere = cWhere + "F38eCodCon='" + oPrc.F38eCodCon + "' And "
	cWhere = cWhere + "F38eCodTrm='" + oPrc.F38eCodTrm + "'"

	If f3_sql("*", "F38e", cWhere, , , "_F38eCur")
		*> Actualizar datos
		Select F38EPRC
		Replace F38eRowDet With oPla.F38lRowID
		=f3_updtun("F38e", , , , "F38EPRC")
	Else
		*> Insertar datos
		Select F38EPRC
		Replace F38eRowDet With oPla.F38lRowID
		Replace F38eRowID With Ora_NewNum("NROW")
		=f3_instun("F38e", "F38EPRC")
	EndIf

	Select F38EPRC
	Skip
EndDo

Use In (Select ("_F38lCur"))
Use In (Select ("_F38eCur"))
Use In (Select ("F38EPRC"))

Return

ENDPROC
PROCEDURE _copiartarifaconceptos

*> Copia la definici�n de conceptos de un cliente sobre otro, Tablas C37L, F38L, F38E.
*> M�todo privado de la clase.

*> Recibe:
*>	- This.CliDsd, cliente origen.
*>	- This.CliHst, cliente destino.
*>	- This.TarDsd, tarifa origen.
*>	- This.TarHst, tarifa destino.
*>	- This.BorrarTar, sustituir datos existentes.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._copiartarifa, copiar tarifa de conceptos.

Private cWhere, oCon, oPla
Local lStado, cNewId

This.UsrError = ""
lStado = .T.

*> Cargar los datos origen.
cWhere = 		  "C37lCodPro='" + This.CliDsd + "' And C37lCodTar='" + This.TarDsd + "' And "
cWhere = cWhere + "F38lRowCab=C37lRowId And "
cWhere = cWhere + "F38eRowDet=F38lRowId"

lStado = f3_sql("*", "F38c,C37l,F38l,F38e", cWhere, , , "F38LCONC")
If !lStado
	This.UsrError = "No hay definici�n de conceptos del cliente origen"
	Use In (Select ("F38lCONC"))
	Return lStado
EndIf

=AddFldToCursor("F38LCONC", [NAME=MARCA,TYPE=C,LENGTH=1])

*> Actualizar registros de plantilla de conceptos (C37L).
Select F38LCONC
Replace All MARCA With Space(1)

Scan For Empty(MARCA)
	Scatter Name oCon

	cNewId = Ora_NewNum("NROW")
	Replace All C37lRowId  With cNewId, ;
				F38lRowCab With cNewId, ;
				C37lCodPro With This.CliHst, ;
				C37lCodTar With This.TarHst, ;
				MARCA      With "1" For C37lRowId==oCon.C37lRowId
	Go Top
EndScan

*> Se actualizar registros de conceptos (F38L).
Select F38LCONC
Replace All MARCA With Space(1)

Scan For Empty(MARCA)
	Scatter Name oCon

	cNewId = Ora_NewNum("NROW")
	Replace All F38lRowId  With cNewId, ;
				F38eRowDet With cNewId, ;
				F38lCodPro With This.CliHst, ;
				F38lCodTar With This.TarHst, ;
				MARCA      With "1" For F38lRowId==oCon.F38lRowId
	Go Top
EndScan

*> Se actualizar registros de escalado de precios (F38E).
Select F38LCONC
Replace All MARCA With Space(1)

Scan For Empty(MARCA)
	Scatter Name oCon

	cNewId = Ora_NewNum("NROW")
	Replace All F38eRowId  With cNewId, ;
				F38eCodPro With This.CliHst, ;
				F38eCodTar With This.TarHst, ;
				MARCA      With "1" For F38eRowId==oCon.F38eRowId
	Go Top
EndScan

*> Actualizar datos en la BBDD.
Select F38LCONC
Go Top

Do While !Eof()
	Scatter Name oCon

	*> Crear / actualizar registro de plantilla de conceptos (C37L).
	cWhere = 		  "C37lCodPro='" + oCon.C37lCodPro + "' And "
	cWhere = cWhere + "C37lCodTar='" + oCon.C37lCodTar + "' And "
	cWhere = cWhere + "C37lRowId='"  + oCon.C37lRowId + "'"

	If !f3_sql("*", "C37l", cWhere, , , "_C37lCur")
		Select _C37lCur
		Append Blank
		Gather Name oCon
		=f3_instun("C37l", "_C37lCur")
	EndIf

	*> Crear / actualizar registro de conceptos (F38L).
	cWhere = 		  "F38lCodPro='" + oCon.F38lCodPro + "' And "
	cWhere = cWhere + "F38lCodTar='" + oCon.F38lCodTar + "' And "
	cWhere = cWhere + "F38lRowId='"  + oCon.F38lRowId + "'"

	If !f3_sql("*", "F38l", cWhere, , , "_F38lCur")
		Select _F38lCur
		Append Blank
		Gather Name oCon
		=f3_instun("F38l", "_F38lCur")
	EndIf

	*> Crear / actualizar registro de escalado de precios (F38E).
	cWhere = 		  "F38eCodPro='" + oCon.F38eCodPro + "' And "
	cWhere = cWhere + "F38eCodTar='" + oCon.F38eCodTar + "' And "
	cWhere = cWhere + "F38eRowId='"  + oCon.F38eRowId + "'"

	If !f3_sql("*", "F38e", cWhere, , , "_F38eCur")
		Select _F38eCur
		Append Blank
		Gather Name oCon
		=f3_instun("F38e", "_F38eCur")
	EndIf

	Select F38LCONC
	Skip
EndDo

Use In (Select ("_C37lCur"))
Use In (Select ("_F38lCur"))
Use In (Select ("_F38eCur"))
Use In (Select ("F38LCONC"))

Return

ENDPROC
PROCEDURE fotoalmacendia

*> Proceso de generaci�n de la foto de almac�n del d�a en curso.
*> Trabajar a partir de las ocupaciones.

*> Recibe:
*>	- FechaCierre, fecha de c�lculo.
*>	- cPropietario, c�digo de cliente (opcional).
*>	- cArticulo, art�culo (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.ArtDsd, art�culo inicial.
*>	- This.ArtHst, art�culo final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 30.04.2008 (AVC) Permitir registros con stock cero, en caso contrario podr�an quedar fuera de los c�lculos de d�as estancia.
*> 22.05.2008 (AVC) Anular cambio.

Parameters dFechaCierre, cPropietario, cArticulo

Private cWhere, cField, cGroup, cOrder, cFromF, oF30l
Local lStado, dFechaInicial, oF16c

lStado = .T.
This.UsrError = ""

*> Cursores de trabajo temporales.
Use In (Select ("F16cInfo"))
Use In (Select ("F30lInfo"))

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.ArtDsd = Iif(Type('cArticulo')=='C', cArticulo, .ArtDsd)
	.ArtHst = Iif(Type('cArticulo')=='C', cArticulo, .ArtHst)
EndWith

*> Existe fecha de cierre: Obtener los datos de cierre de ese d�a.
dFechaInicial = F30lFecha
=CrtCursor("F30l", "F30lInfo", "C")

*> Cargar las ocupaciones.
cFromF = "F16c"
cOrder = "F16cCodPro, F16cCodArt"
cWhere = "F16cCodPro Between'" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F16cCodArt Between'" + This.ArtDsd + "' And '" + This.ArtHst + "'"

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F16cInfo")

Select F16cInfo
Go Top

Do While !Eof()
	Scatter Name oF16c

	*> Datos del art�culo.
	m.F08cCodPro = oF16c.F16cCodPro
	m.F08cCodArt = oF16c.F16cCodArt
	=f3_seek("F08c")

	*> A�adir registro calculado de cierre.
	Select F30lInfo
	Locate For F30lCodPro==oF16c.F16cCodPro .And. ;
			   F30lCodArt==oF16c.F16cCodArt .And. ;
			   F30lSitStk==oF16c.F16cSitStk .And. ;
			   F30lNumPal==oF16c.F16cNumPal .And. ;
			   F30lNumLot==oF16c.F16cNumLot .And. ;
			   F30lCodUbi==oF16c.F16cCodUbi

	If !Found()
		Append Blank
		Replace F30lCodPro With oF16c.F16cCodPro, ;
	    	    F30lCodArt With oF16c.F16cCodArt, ;
	            F30lSitStk With oF16c.F16cSitStk, ;
	            F30lFecha  With dFechaCierre, ;
	            F30lNumPal With oF16c.F16cNumPal, ;
	            F30lNumLot With oF16c.F16cNumLot, ;
	            F30lCodUbi With oF16c.F16cCodUbi, ;
	            F30lTamPal With oF16c.F16cTamPal, ;
	            F30lTipPro With F08c.F08cTipPro, ;
	            F30lTotVol With 0, ;
	            F30lTotPes With 0, ;
	            F30lStock  With 0
	EndIf

	*> Actualizar la cantidad.
	Replace F30lStock With oF16c.F16cCanFis

   *> Tomar el siguiente movimiento del hist�rico.
   Select F16cInfo
   Skip
EndDo

*> Calcular el peso y el volumen de los datos generados.
Select F30lInfo
Delete For F30lStock <= 0
Replace All F30lFecha With dFechaCierre
Go Top

Do While !Eof()
	Scatter Name oF30l

	*> Obtener el peso y el volumen de los datos generados.
	=This._pesovolumenmov(oF30l.F30lCodPro, oF30l.F30lCodArt, oF30l.F30lStock)

	Select F30lInfo
	Replace F30lTotVol With This.oUbicObj.PesOcu, ;
			F30lTotPes With This.oUbicObj.VolOcu
	Skip
EndDo

*> Borrar los datos del d�a a calcular, si los hay.
cFromF = "F30l"
cWhere = "F30lCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F30lCodArt Between '" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
		 "F30lFecha=" + _GCD(dFechaCierre)

=f3_DelTun(cFromF, , cWhere, 'N')

*> Insertar los datos del cursor en la tabla de cierre.
Select F30lInfo
Go Top

Do While !Eof()
   =f3_InsTun('F30l', 'F30lInfo', 'N')

   Select F30lInfo
   Skip
EndDo

*> Eliminar datos temporales.
Use In (Select ("F16cInfo"))
Use In (Select ("F30lInfo"))

Return

ENDPROC
PROCEDURE cierrediariodia

*> Proceso de cierre diario de facturaci�n, para un art�culo / d�a en curso.
*> Parte de la foto de almac�n del d�a a calcular.

*> Recibe:
*>	- FechaCierre, fecha de c�lculo.
*>	- cPropietario, c�digo de cliente (opcional).
*>	- cArticulo, art�culo (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.ArtDsd, art�culo inicial.
*>	- This.ArtHst, art�culo final.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters dFechaCierre, cPropietario, cArticulo

Private cWhere, cField, cGroup, cOrder, cFromF, oF30d
Local lStado, dFechaInicial, oF16c

Store .T. To lStado
This.UsrError = ""

*> Cursores de trabajo temporales.
Use In (Select ("F30lInfo"))
Use In (Select ("F30dInfo"))

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.ArtDsd = Iif(Type('cArticulo')=='C', cArticulo, .ArtDsd)
	.ArtHst = Iif(Type('cArticulo')=='C', cArticulo, .ArtHst)
EndWith

dFechaInicial = F30dFecha
=CrtCursor("F30d", "F30dInfo", "C")

*> Cargar la foto de almac�n del d�a anterior al del c�lculo.
cFromF = "F30l"
cOrder = "F30lCodPro, F30lCodArt, F30lFecha"
cWhere = "F30lCodPro Between'" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F30lCodArt Between'" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
         "F30lFecha<" + _GCD(dFechaInicial)

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F30lInfo")

Select F30lInfo
Go Top

Do While !Eof()
	Scatter Name oF30l

	*> A�adir registro calculado de cierre.
	Select F30dInfo
	Locate For F30dCodPro==oF30d.F30dCodPro .And. ;
			   F30dCodArt==oF30d.F30dCodArt .And. ;
			   F30dSitStk==oF30d.F30dSitStk

	If !Found()
		Append Blank
		Replace F30dCodPro With oF30d.F30dCodPro, ;
	    	    F30dCodArt With oF30d.F30dCodArt, ;
	            F30dSitStk With oF30d.F30dSitStk, ;
	            F30dFecha  With dFechaCierre, ;
	            F30dTotVol With 0, ;
	            F30dTotPes With 0, ;
	            F30dStock  With 0
	EndIf

	*> Actualizar la cantidad.
	Replace F30dStock With F30dStock + oF30l.F30lStock

   *> Tomar el siguiente movimiento del hist�rico.
   Select F30dInfo
   Skip
EndDo

*> Calcular el peso y el volumen de los datos generados.
Select F30dInfo
Delete For F30dStock <= 0
Replace All F30dFecha With dFechaCierre
Go Top

Do While !Eof()
	Scatter Name oF30d

	*> Obtener datos de la ficha de producto.
	m.F08cCodPro = oF30d.F30dCodPro
	m.F08cCodArt = oF30d.F30dCodArt
	=f3_seek("F08c")

	*> Obtener el peso y el volumen de los datos generados.
	=This._pesovolumenmov(oF30d.F30dCodPro, oF30d.F30dCodArt, oF30d.F30dStock)

	Select F30dInfo
	Replace F30dTotVol With This.oUbicObj.PesOcu, ;
			F30dTotPes With This.oUbicObj.VolOcu, ;
			F30dTipPro With F08c.F08cTipPro
	Skip
EndDo

*> Borrar los datos del d�a a calcular, si los hay.
cFromF = "F30d"
cWhere = "F30dCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "' And " + ;
		 "F30dCodArt Between '" + This.ArtDsd + "' And '" + This.ArtHst + "' And " + ;
		 "F30dFecha=" + _GCD(dFechaCierre)

=f3_DelTun(cFromF, , cWhere, 'N')

*> Insertar los datos del cursor en la tabla de cierre.
Select F30dInfo
Go Top

Do While !Eof()
   =f3_InsTun('F30d', 'F30dInfo', 'N')

   Select F30dInfo
   Skip
EndDo

*> Eliminar datos temporales.
Use In (Select ("F30lInfo"))
Use In (Select ("F30dInfo"))

Return

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:

*> 01.04.2008 (AVC) Tener en cuenta productos sin peso / volumen para calcular peso y volumen palets.
*>					Modificado m�todo This.DiariosPalets
*> 02.04.2008 (AVC) Calcular peso / volumen por documento.
*>					Modificado m�todo This._calculardocue
*> 07.04.2008 (AVC)	Crear conceptos de detalle de palets de entrada.
*>					Modificado m�todo This.Calcularacumulados.
*>					Modificado m�todo This._generarfacturadiap.
*> 30.04.2008 (AVC) Permitir registros con stock cero, en caso contrario podr�an quedar fuera de los c�lculos de d�as estancia.
*>					Modificado m�todo This.FotoAlmacen
*>					Modificado m�todo This.FotoAlmacenDia
*> 16.05.2008 (AVC) Corregir codificaci�n conceptos de l�neas de entrada (LINE).
*>					Modificado m�todo This._calculardocule
*> 21.05.2008 (AVC) Agregar generaci�n de facturas directas.
*>					Agregada propiedad This.AgrFra
*>					Hacer p�blicas las propiedades This.DefaultCon y This.DefaultIva
*>					Modificado m�todo This.Inicializar
*>					Agregado m�todo This.CalcularFacturasDirectas
*>					Agregado m�todo This._generarfacturadirecta
*> 22.05.2008 (AVC) Corregir conversi�n de tipo hora a fecha.
*>					MOdificado m�todo This._cierrepalets
*> 22.05.2008 (AVC) Eliminar registros con stock cero en c�lculos de d�as estancia.
*>					Modificado m�todo This.FotoAlmacen
*>					Modificado m�todo This.FotoAlmacenDia
*> 08.07.2008 (AVC) Corregir rec�lculo de palets.
*>					Modificar proceso de cierre de palet.
*>					Modificado m�todo This.DiarioPalets
*>					Modificado procedimiento This._cierrepalets

ENDPROC
PROCEDURE calcularfacturasdirectas

*> Calcular las facturas directas de un periodo dado.

*> Recibe:
*>	- cPropietario, c�digo de cliente (opcional).
*>	- dFecha, fecha de c�lculo (opcional).
*>	- cAgrFra, factura agrupada (opcional).

*> o bien, si se recibe como propiedades
*>	- This.CliDsd, cliente inicial.
*>	- This.CliHst, cliente final.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.

*>	- This.CrtFra, Tratamiento si la factura existe:
*>			A: A�adir nuevos datos a la factura.
*>			R: Reemplazar la factura.
*>			N: Crear una nueva factura.
*>			C: Cancelar el proceso.

*>	- This.AgrFra, Tratamiento si la factura existe:
*>			A: A�adir nuevos datos a la factura.

*>	- This.DefaultCon, concepto facturaci�n por defecto.
*>	- This.DefaultIva, IVA por defecto.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

Parameters cPropietario, dFecha, cAgrFra

Local lStado
Private cWhere, oF32c

Store .T. To lStado
This.UsrError = ""

*> Asignar par�metros recibidos a propiedades.
With This
	.CliDsd = Iif(Type('cPropietario')=='C', cPropietario, .CliDsd)
	.CliHst = Iif(Type('cPropietario')=='C', cPropietario, .CliHst)
	.FchDsd = Iif(Type('dFecha')=='D', dFecha, .FchDsd)
	.FchHst = Iif(Type('dFecha')=='D', dFecha, .FchHst)
	.AgrFra = Iif(Type('cAgrFra')=='C', cAgrFra, .AgrFra)
EndWith

*> Cargar un cursor con los clientes a procesar.
cWhere = "F32cCodPro Between '" + This.CliDsd + "' And '" + This.CliHst + "'"
lStado = f3_sql("*", "F32c", cWhere, , , "F32cGenFD")
If !lStado
	*> No hay clientes para cargar.
	Use In (Select ("F32cGenFD"))
	This.UsrError = "No se pueden cargar los clientes"
	Return lStado
EndIf

Select F32cGenFD
Go Top
Do While !Eof()
	Scatter Name oF32c

	*> Validar la tarifa del cliente en curso.
	lStado = This._validartarifa(oF32c.F32cCodPro)
	If !lStado
		*> Cliente sin tarifa v�lida.
		Select F32cGenFD
		Skip
		Loop
	EndIf

	*> Llamadas a c�lculos de facturas para el cliente en curso.
	lStado = This._generarfacturadirecta()
	lStado = This._generaracumuladosfactura()
	lStado = This._generardatospagofactura()

	Select F32cGenFD
	Skip
EndDo

Use In (Select ("F32cGenFD"))

Return lStado

ENDPROC
PROCEDURE _generarfacturadirecta

*> Calcular la factura directa del cliente actual.
*> M�todo privado de la clase.

*> Trabaja a partir de:
*>	- Fichero de conceptos manuales (F36d).

*> Recibe:
*>	- This.CurCli, c�digo de cliente.
*>	- This.FchDsd, fecha inicial.
*>	- This.FchHst, fecha final.
*>	- This.CrtFra, tratamiento con facturas existentes.
*>	- This.AgrFra, agrupar en una factura.
*>	- This.CurFra, factura ya existe S/N.
*>	- This.DefaultCon, concepto facturaci�n por defecto.
*>	- This.DefaultIva, IVA por defecto.

*> Devuelve:
*>	- Estado (.T. / .F.), resultado de la operaci�n.
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This.CalcularFacturasDirectas(), generaci�n de facturas.

*> Historial de modificaciones:
*> 21.05.2008 (AVC)	Creaci�n.

Private cWhere, cOrder, cFromF, oF36d, oF36dCur
Local lStado

Store .T. To lStado
This.UsrError = ""

*> Cursores de trabajo.
Use In (Select ("F70lFACT"))

*> Cargar el detalle de conceptos manuales.
cWhere = "F36dCodPro='" + This.CurCli + "' And F36dFecCal Between " + _GCD(This.FchDsd) + " And " + _GCD(This.FchHst) + " And F36dFactur='N'"
cFromF = "F36d"
cOrder = "F36dCodPro,F36dFecCal,F36dNumDoc,F36dRowID"

lStado = f3_sql("*", cFromF, cWhere, cOrder, , "F36dFACT")
If !lStado
	*> Cliente sin conceptos manuales.
	This.UsrError = "No hay conceptos manuales"
	Use In (Select ("F36dFACT"))
	Return lStado
EndIf

*!*	*> En el caso de las facturas manuales es necesario borrar las l�neas de factura previas pues, a diferencia
*!*	*> de otros tipos de concepto, en este caso siempre se insertan las l�neas nuevas.
*!*	cWhere = "F70lNumFac='" + This._numfraget + "' And F70lCodigo='MANU'"
*!*	=f3_deltun("F70l", , cWhere)

*> Imagen detalle factura.
=CrtCursor("F70l", "F70lFACT")

Select F36dFACT
Go Top

*> Se guarda el registro actual para comparaciones.
If !Eof()
	Scatter Name oF36dCur Blank
EndIf

Do While !Eof()
	Scatter Name oF36d

	*> Si NO hay agrupaci�n de factura, nueva factura.
	If This.AgrFra<>"S"
		If oF36dCur.F36dCodPro<>oF36d.F36dCodPro Or oF36dCur.F36dFecCal<>oF36d.F36dFecCal Or oF36dCur.F36dNumDoc<>oF36d.F36dNumDoc
			This._numfraget = ""
			oF36dCur = oF36d
		EndIf
	EndIf

	*> Generar, si cal, la cabecera de la factura.
	If Empty(This._numfraget)
		=This._generarcabecerafactura()
	EndIf

	Select F70lFACT
	Append Blank
	Replace F70lCodPro With This.CurCli, ;
	        F70lCodTar With This.CurTar, ;
	        F70lNumFac With This._numfraget, ;
	        F70lRowDet With oF36d.F36dRowID, ;
        	F70lCodTrm With Space(4), ;
	        F70lTipUni With Space(4), ;
	        F70lBasImp With 0, ;
	        F70lImpIva With 0, ;
	        F70lImpEqv With 0

	Replace F70lCodCon With This.DefaultCon, ;
	        F70lCodigo With oF36d.F36dCodCon, ;
	        F70lCodigo With Space(4), ;
	        F70lDescri With oF36d.F36dDescri, ;
	        F70lRowId  With Ora_NewNum("NROW"), ;
	        F70lUniCal With oF36d.F36dCantid, ;
	        F70lUniCor With oF36d.F36dCantid, ;
	        F70lPreCal With oF36d.F36dPrecio, ;
	        F70lPreCor With oF36d.F36dPrecio, ;
	        F70lCodIva With This.DefaultIva, ;
	        F70lPrtCnt With Iif(oF36d.F36dCantid > 0, "S", "N"), ;
	        F70lPrtPrc With Iif(oF36d.F36dPrecio > 0, "S", "N"), ;
	        F70lPrtImp With Iif(oF36d.F36dCantid > 0, "S", "N")

	=f3_instun("F70l", "F70lFACT")

	*> Actualizar estado registro y trabajar con el siguiente registro directo.
	Select F36dFACT
	Replace F36dFactur With "S"
	=f3_updtun("F36d", , , , "F36dFACT")

	Select F36dFACT
	Skip
EndDo

*> Cursores de trabajo.
Use In (Select ("F36dFACT"))
Use In (Select ("F70cFACT"))
Use In (Select ("F70lFACT"))

Return

ENDPROC


EndDefine 
