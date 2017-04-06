Define Class orafnclist as custom
Height = 17
Width = 100



PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

*> Historial de modificaciones:
*> 07.02.2008 (AVC) Inicializar propiedad This.NumMovNew.
*> 06.05.2008 (AVC) Agregar propiedades This.UpdLsC, This.NumLsC, This.AgrTrp, This.IdentD

With This
	.CodOpe = Space(4)			&& Operario.
	.NumLst = Space(6)			&& Nº de lista de trabajo.
	.NumLsC = Space(6)			&& Nº de lista de carga.
	.NumLstNew = Space(6)		&& Nº de lista nueva.
	.CodPro = Space(6)			&& Propietario.
	.TipDoc = Space(4)			&& Tipo documento.
	.NumDoc = Space(13)			&& Nº documento.

	.NumMovMP = ""				&& Nº de movimiento pendiente.
	.NumMovLS = ""				&& Nº de MP lista.
	.NumMovNew = ""				&& Nº movimiento para inserción.
	.RowID = ""					&& Identificador ocupación.
	.RowIDNew = ""				&& Identificador ocupación nueva.

	.AgrDoc = "N"				&& Agrupar documentos en lista.
	.PrmOpe = "N"				&& Trabajar con los parámetros del operario.
	.TipLst = ""				&& Tipo de lista a generar (P: Preparación, R: Reposición, I:Inventario, U: No definido).
	.FrzCnf = "N"				&& Forzar confirmación.
	.UpdLst = "N"				&& Actualizar estado lista trabajo.
	.UpdLsC = "N"				&& Actualizar estado lista carga.
	.LstParcial = "N"			&& Lista parcial (en borrado, p.ej).
	.GenLista = "N"				&& Generar lista (al asignar MP a otra lista, p. ej).

	.UpdDoc = "N"				&& Actualizar estado documento.
	.UpdTransito = "N"			&& Actualizar stock en tránsito.
	.StkTrn = Space(4)			&& Situación stock en tránsito.
	.Muelle = ""				&& Muelle por defecto.
	.StkBlq = Space(4)			&& Situación stock de bloqueado.

	.AgrTrp = "N"				&& Agrupar listas carga por transportista.
	.IdentD = "N"				&& Identificación destino.

	.TMovLst = ""				&& Tipo movimiento preparación.
	.TMovExp = ""				&& Tipo movimiento expedición.
	.TMovOrg = ""				&& Tipo movimiento origen.
	.TMovDst = ""				&& Tipo movimiento destino.

	.DocumentoCerrado = 'N'		&& Documento finalizado.
	.ListaCerrada = 'N'			&& Lista finalizada.

	.UsrError = ""				&& Texto errores.
EndWith

Return

ENDPROC
PROCEDURE initlocals

*> Inicializar las propiedades protegidas de la clase.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Agregar propiedad This.CirLsC

With This
	.WRowId = ""
	.CurLst = ""
	.CurLsC = ""
	.oF14c = ""
	.RStkObj = .F.
	.ActzObj = .F.
	.ActzPrm = .F.
EndWith

Return

ENDPROC
PROCEDURE actualizarlista

*> Actualizar el estado de una lista de trabajo.
*> Versión en clase de ORA_PROC.ORA_ACCBLS().
*>
*> Recibe:
*>	- cNumeroLista ó bien This.NumLst, lista a actualizar.
*>	- This.UpdLst, actualizar estado lista de trabajo (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cNumeroLista

Private nPeso, nVolumen, cNumLst, cWhere
Local lStado, dFPrMov, dFUlMov, nTotPes, nTotPesP, nTotVol, nTotVolP, nTotMov, nTotMovP

If This.UpdLst<>'S'
	Return
EndIf

*> Aviso a Main de lista cerrada.
This.ListaCerrada = 'N'

lStado = .T.
This.UsrError = ""
Store _FecMin To dFPrMov, dFUlMov
Store 0 To nTotPes, nTotPesP, nTotVol, nTotVolP, nTotMov, nTotMovP

*> Tomar la lista recibida, bien como parámetro o bien como propiedad.
cNumLst = Iif(PCount()==1, cNumeroLista, This.NumLst)

*> Validar los datos recibidos.
If Empty(cNumLst)
	*> Lista vacía.
	This.UsrError = "La lista de trabajo está vacía"
	lStado = .F.
	Return lStado
EndIf

m.F26cNumLst = cNumLst
If !f3_seek("F26c")
	*> No existe
	This.UsrError = "La lista de trabajo: " + cNumLst + " no existe"
	lStado = .F.
	Return lStado
EndIf

*=CrtCursor("F26l", "F26lEst", "C")

*> Cargar datos de detalle de la lista.
cWhere = "F26lNumLst='" + cNumLst + "'"
If !f3_sql("*", "F26l", cWhere, , , "F26lEst")
	*> No existe
	This.UsrError = "La lista de trabajo: " + cNumLst + " no existe"
	lStado = .F.
	Return lStado
EndIf

Select F26lEst
Go Top
Do While !Eof()
	*> Fecha primer movimiento.
	If F26lFecMov < dFPrMov
		dFPrMov = F26lFecMov
	EndIf

	*> Fecha último movimiento.
	If F26lFecMov > dFUlMov
		dFUlMov = F26lFecMov
	EndIf

	*> Peso y volumen movimiento.
	Store 0 To nPeso, nVolumen
	Do PesVolAr In Ora_Ca00 With F26lEst.F26lCodPro, F26lEst.F26lCodArt, F26lEst.F26lCanFis, nPeso, nVolumen

	nTotPes = nTotPes + nPeso
	nTotPesP = nTotPesP + Iif(F26lEst.F26lEstMov=='0', nPeso, 0)
	nTotVol = nTotVol + nVolumen
	nTotVolP = nTotVolP + Iif(F26lEst.F26lEstMov=='0', nVolumen, 0)

	Select F26lEst
	Skip
EndDo

*> Nº de movimientos de la lista.
Count To nTotMov							&& Totales.
Count To nTotMovP For F26lEstMov=='0'		&& Pendientes.

*> Actualizar datos en la cabecera de la lista.
Select F26c

Replace F26cFPrMov With dFPrMov
Replace F26cFUlMov With dFUlMov
Replace F26cMovAsg With nTotMov
Replace F26cMovPte With nTotMovP
Replace F26cKgsAsg With nTotPes
Replace F26cKgsPte With nTotPesP
Replace F26cVolAsg With nTotVol
Replace F26cVolPte With nTotVolP
Replace F26cEstLst With Iif(nTotMovP==0, '3', Iif(nTotMovP < nTotMov, '1', '0'))

Scatter MemVar

m.F26cNumLst = cNumLst
If nTotMov <= 0
	=f3_baja("F26c")
Else
	=f3_upd("F26c")

	If nTotMovP==0
		*> Aviso a Main de lista cerrada.
		This.ListaCerrada = 'S'
	EndIf
EndIf


Use In (Select("F26lEst"))
lStado = .T.
Return lStado

ENDPROC
PROCEDURE genlstdoc

*> Crear una lista de trabajo a partir de los MPs de un documento de salida.
*> Recibe:
*>	- This.CodPro, Propietario.
*>	- This.TipDoc, Tipo documento.
*>	- This.NumDoc, Nº documento.
*>	- This.CodOpe, Operario al que se asigna la lista.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto error.
*>	- This.NumLst, Nº de lista generada.

Private cWhere
Local lStado

Private cWhere, cNumLst
Local lStado, cMensajes, oF14c

Store "" To This.UsrError, cMensajes

=This.InitLocals()

*!*	*> Validar los datos recibidos. CONTROL ANULADO.
*!*	If Empty(This.CodPro) .Or. Empty(This.TipDoc) .Or. Empty(This.NumDoc)
*!*	   *> Datos documento vacíos.
*!*	   This.UsrError = "Datos documento vacíos"
*!*	   lStado = .F.
*!*	   Return lStado
*!*	EndIf

If Empty(This.CodOpe)
   *> Operario en blanco.
   This.UsrError = "Código de operario en blanco"
   lStado = .F.
   Return lStado
EndIf

m.F05cCodOpe = This.CodOpe
If !f3_seek("F05c")
	*> Operario no existe.
   This.UsrError = "El operario: " + This.CodOpe + " no existe"
   lStado = .F.
   Return lStado
EndIf

*> Obtener los MPs del documento en curso que estén pendientes.
cWhere = "F14cCodPro='" + This.CodPro + "' And " + ;
         "F14cTipDoc='" + This.TipDoc + "' And " + ;
         "F14cNumDoc='" + This.NumDoc + "' And " + ;
         "(F14cTipMov Between '2000' And '2998' Or F14cTipMov Between '3500' And '3599') And " + ;
		 "F14cNumMov Not In (Select F26lNumMov From F26l" + _em + " Where F26lNumMov=F14cNumMov)"

lStado = f3_sql("*", "F14c", cWhere, "F14cNumMov", , "F14cPdte")
If !lStado
	*> No hay MPs de este documento.
	This.UsrError = "Documento: " + This.NumDoc + " sin datos para procesar"
	Use In (Select("F14cPdte"))
	Return lStado
EndIf

*> Cursores de trabajo para actualización de datos.
=CrtCursor("F14c", "F14cUpd", "C")
=CrtCursor("F26c", "F26cUpd", "C")
=CrtCursor("F26l", "F26lUpd", "C")

Select F14cPdte
Go Top
Do While !Eof()
	*> Preparar datos para actualizar el MP.
	Scatter Name This.oF14c

	*> Insertar registro de cabecera de lista.
	If Empty(This.NumLst)
		This.CurLst = ""
		lStado = This._genlstcab()
		This.NumLst = This.CurLst

		If !lStado
			Use In (Select("F14cPdte"))
			Use In (Select("F14cUpd"))
			Use In (Select("F26cUpd"))
			Use In (Select("F26lUpd"))
			Return lStado
		EndIf
	Else
		This.CurLst = This.NumLst
	EndIf

	*> Generar la línea de detalle.
	lStado = This._genlstdet()
	If !lStado
		=This.BorrarLista()

		Use In (Select("F14cPdte"))
		Use In (Select("F14cUpd"))
		Use In (Select("F26cUpd"))
		Use In (Select("F26lUpd"))
		Return lStado
	EndIf

	Select F14cPdte
	Skip
EndDo

*> Borrar cursores de trabajo.
Use In (Select("F14cPdte"))
Use In (Select("F14cUpd"))
Use In (Select("F26cUpd"))
Use In (Select("F26lUpd"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE genlstmp

*> Agregar un MP a una lista de trabajo.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovMP, nº de MP.
*>	- This.UpdLst, Actualizar estado lista (S/N).
*>	- This.NumLst, Lista a la que se añade el MP. Se crea si blancos.
*>	- This.CodOpe, Operario al que se asigna la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLst, Lista generada, si no existía.
*>	- This.UsrError, Texto errores.

Parameters cNumeroMovimiento
Private cWhere, cNumMov, cLista
Local lStado

This.UsrError = ""

=This.InitLocals()

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovMP)

*> Validar el MP.
If ! f3_seek("F14c", '[m.F14cNumMov=cNumMov]')
	This.UsrError = "No existe el MP: " + cNumMov
	lStado = .F.
	Return lStado
EndIf

*> Validar si el MP ya está en una lista.
If f3_seek("F26l", '[m.F26lNumMov=cNumMov]')
	This.UsrError = "El MP: " + cNumMov + " ya está en lista"
	lStado = .F.
	Return lStado
EndIf

*> Validar el operario.
If Empty(This.CodOpe)
   This.UsrError = "Código de operario en blanco"
   lStado = .F.
   Return lStado
EndIf

m.F05cCodOpe = This.CodOpe
If !f3_seek("F05c")
	*> Operario no existe.
   This.UsrError = "El operario: " + This.CodOpe + " no existe"
   lStado = .F.
   Return lStado
EndIf

*> Guardar el registro en una propiedad.
Select F14c
Scatter Name oF14c
This.CurLst = This.NumLst

*> Crear la cabecera de la lista, si cal.
If Empty(This.CurLst)
	lStado = This._genlstcab()
EndIf

*> Crear la línea de detalle de la lista.
lStado = This._genlstdet()

*> Actualizar el estado de la lista.
lStado = This.ActualizarLista()

Return lStado

ENDPROC
PROCEDURE borrarlista

*> Borrar una lista de trabajo.
*> Recibe:
*>	- cNumeroLista ó bien This.NumLst, lista a borrar.
*>	- This.LstParcial, borrar parcialmente (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cNumeroLista

Private cWhere, cNumLst
Local lStado

This.UsrError = ""

*> Tomar la lista recibida, bien como parámetro o bien como propiedad.
cNumLst = Iif(PCount()==1, cNumeroLista, This.NumLst)

*> Validar los datos recibidos.
If Empty(cNumLst)
	*> Lista vacía.
	This.UsrError = "La lista está vacía"
	lStado = .F.
	Return lStado
EndIf

*> Cursores de trabajo para borrado de listas.
=CrtCursor("F26c", "F26cDel", "C")
=CrtCursor("F26l", "F26lDel", "C")

*> Cargar datos de cabecera de la lista.
cWhere = "F26cNumLst='" + cNumLst + "'"
lStado = f3_sql("*", "F26c", cWhere, , , "F26cDel")
If !lStado
	*> No existe la cabecera de la lista.
	This.UsrError = "No existe la cabecera de la lista: " + cNumLst
	Use In (Select("F26cDel"))
	Return lStado
EndIf

Select F26cDel
If F26cEstLst > "0" .And. This.LstParcial<>"S"
	*> Estado incorrecto.
	This.UsrError = "La lista: " + cNumLst + " no se puede borrar"
	Use In (Select("F26cDel"))
	lStado = .F.
	Return lStado
EndIf

*> Cargar datos de detalle de la lista.
cWhere = "F26lNumLst='" + cNumLst + "' And F26lEstMov='0'"
lStado = f3_sql("*", "F26l", cWhere, , , "F26lDel")
If !lStado
	*> No hay líneas de detalle de la lista.
	This.UsrError = "No hay líneas de la lista: " + cNumLst
	Use In (Select("F26cDel"))
	Use In (Select("F26lDel"))
	Return lStado
EndIf

Select F26lDel
Go Top
Do While !Eof()
	This.NumMovLS = F26lNumMov		&& Nº MP a borrar de la lista.
	This.UpdLst = "N"				&& NO se actualiza la cabecera.

	lStado = This.BorrarListaMP()

	Select F26lDel
	Skip
EndDo

*> Actualizar estado de la lista, si se ha borrado de forma parcial.
=This.ActualizarLista()

*> Borrar cursores de trabajo.
Use In (Select("F26cDel"))
Use In (Select("F26lDel"))

lStado = .T.
Return lStado


ENDPROC
PROCEDURE borrarlistamp

*> Borrar un MP de una lista de trabajo.
*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovLS, nº de MP a borrar.
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.NumLst, lista a actualizar.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cNumeroMovimiento
Private cWhere, cNumMov, cCampos, cValores, cLista
Local lStado

This.UsrError = ""

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovLS)

*> Borrar línea de la lista.
m.F26lNumMov = cNumMov
=f3_baja("F26l")

*> Borrar lista y operario de los MPs.
cWhere = "F14cNumMov='" + cNumMov + "'"
cCampos = "F14cCodOpe,F14cNumLst"
cValores = "'" + Space(4) + "','" + Space(6) + "'"
=f3_updtun("F14c", , cCampos, cValores, , cWhere)

*> Actualizar, si cal, la cabecera de la lista de trabajo.
This.ActualizarLista()

lStado = .T.
Return lStado

ENDPROC
PROCEDURE dividirmvt

*> Dividir en dos un movimiento pendiente.

*> Recibe:
*>	- cMovimiento ó bien This.NumMovMP, nº de MP.
*>	- nCantidad ó bien This.CanFis, cantidad nuevo movimiento.
*>	- This.NumMovNew, nuevo nº de movimiento (opcional).
*>	- This.UpdLst, actualizar lista (S/N).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumMovNew, nuevo nº de movimiento.
*>	- This.UsrError, Texto errores.

Parameters cMovimiento, nCantidad
Private cWhere, cNumMov, nCanFis, nCanOld, cLista
Local lStado

This.UsrError = ""

*> ------------------------------------------------------------------
*> Asignar parámetros / propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)
nCanFis = Iif(Type('nCantidad')=='N', nCantidad, This.CanFis)

*> Validar parámetros.
If Empty(cNumMov) .Or. Empty(nCanFis)
	This.UsrError = "Parámetros en blanco"
	Return .F.
EndIf
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Verificar la existencia del movimiento y la cantidad del mismo.
Use In (Select("F14cMvt"))

cWhere = "F14cNumMov='" + cNumMov + "'"
If !F3_sql("*", "F14c", cWhere, , , "F14cMvt")
	This.UsrError = "MP no encontrado"
	Return .F.
EndIf

Select F14cMvt
Go Top
If Eof()
	This.UsrError = "MP no encontrado"
	Return .F.
EndIf

If F14cCanFis <= nCanFis
	This.UsrError = "Cantidad insuficiente (MP)"
	Return .F.
EndIf
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Cantidad a actualizar en el MP actual.
nCanOld = F14cMvt.F14cCanFis - nCanFis
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Actualizo MP actual con la cantidad pendiente de preparar.
cWhere = "F14cNumMov='" + cNumMov + _cm
If !f3_UpdTun('F14c', , 'F14cCanFis', 'nCanOld', , cWhere, 'N')
	This.UsrError = "Error actualizando cantidad (MP)"
	Return .F.
EndIf
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Actualizo LT actual con la cantidad pendiente de preparar.
cWhere = "F26lNumMov='" + cNumMov + _cm
If !f3_UpdTun('F26l', , 'F26lCanFis', 'nCanOld', , cWhere, 'N')
	This.UsrError = "Error actualizando cantidad (LS)"
	Return .F.
EndIf
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Valido la coherencia de los datos del MP y de LT.
*> En principio, si el MP esta en lista, debería coincidir F14c y F26l

Use In (Select("F26lMvt"))

Do Case
	Case Empty(F14cMvt.F14cNumLst)

		*> Verificar que no existe el registro en LT
		cWhere = "F26lNumMov='" + cNumMov + "'"
		If F3_sql("*", "F26l", cWhere, , , "F26lMvt")		&& Insertar en LT.

			*> Renumerar el MP.
			This.NumMovMP = Ora_NewMP()
			
			lxWhere = "F14cNumMov = '" + This.NumMovMP + "'"
			Do While F3_Sql('*', 'F14c', lxWhere, , , 'ActzF14c')
				This.NumMovMP = Ora_NewMP()
			EndDo

			cWhere = "F14cNumMov='" + This.NumMovMP + _cm
			If !f3_UpdTun('F14c', , 'F14cNumMov', 'cNumMov', , cWhere, 'N')
				This.UsrError = "Error actualizando núm. movimiento (MP)"
				Return .F.
			EndIf
			
			cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)

			Select F14cMvt
			Replace F14cNumMov With cNumMov
		
		EndIf
		
	Case !Empty(F14cMvt.F14cNumLst)

		*> Verificar que existe el registro en LT, y que coinciden los datos.
		cWhere = "F26lNumMov='" + cNumMov + "'"
		If !F3_sql("*", "F26l", cWhere, , , "F26lMvt")		&& Insertar en LT.

			*> Insertar en LT el MP, ya que tiene operario y LT, o bien eliminar el numero de LT del MP
			cNewLst=Space(6)
			cNewOpe=Space(4)

			cWhere = "F26lNumMov='" + cNumMov + "'"
			If !f3_UpdTun('F26l', , 'F26lNumLst,F26lCodOpe', 'cNewLst,cNewOpe', , cWhere, 'N')
				This.UsrError = "Error actualizando lista (LS)"
				Return .F.
			EndIf

			Select F14cMvt
			Replace F14cNumLst With cNewLst
			Replace F14cCodOpe With cNewOpe
		Else
			*> Verificar que los datos de MP y LT coinciden.
		EndIf

EndCase
*> ------------------------------------------------------------------


*> ------------------------------------------------------------------
*> Obtener nuevo número de movimiento.
If Empty(This.NumMovNew)

	This.NumMovNew = Ora_NewMP()
	
	lxWhere = "F14cNumMov = '" + This.NumMovNew + "'"
	Do While F3_Sql('*', 'F14c', lxWhere, , , 'ActzF14c')
		This.NumMovNew = Ora_NewMP()
	EndDo
	
EndIf
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Genero nuevo MP actual con la cantidad preparada.
Select F14cMvt
Go Top
Replace F14cNumMov With This.NumMovNew
Replace F14cCanFis With nCanFis

If !F3_InsTun('F14c', 'F14cMvt', 'N')
	This.UsrError = "Error insertando movimiento (MP)"
	Return .F.
EndIf
*> ------------------------------------------------------------------

*> ------------------------------------------------------------------
*> Incorporo el nuevo registro a LT.
cLista = ''
Use In (Select("F26lMvt"))

cWhere = "F26lNumMov='" + cNumMov + "'"
If F3_sql("*", "F26l", cWhere, , , "F26lMvt")		&& Insertar en LT.

	Select F26lMvt
	Go Top
	If !Eof()
		cLista = F26lNumLst								&& Para actualizar cabecera.

		Replace F26lNumMov With This.NumMovNew
		Replace F26lCanFis With nCanFis

		If !F3_InsTun('F26l', 'F26lMvt', 'N')
			This.UsrError = "Error insertando movimiento (LS)"
			Return .F.
		EndIf
	EndIf

EndIf
*> ------------------------------------------------------------------

*> Actualizar la cabecera de la lista.
If !Empty(cLista)
	=This.ActualizarLista(cLista)
EndIf

lStado = .T.
Return lStado



*!*	*> Dividir en dos un movimiento pendiente.

*!*	*> Recibe:
*!*	*>	- cMovimiento ó bien This.NumMovMP, nº de MP.
*!*	*>	- nCantidad ó bien This.CanFis, cantidad nuevo movimiento.
*!*	*>	- This.NumMovNew, nuevo nº de movimiento (opcional).
*!*	*>	- This.UpdLst, actualizar lista (S/N).

*!*	*> Devuelve:
*!*	*>	- Estado (.T. / .F.)
*!*	*>	- This.NumMovNew, nuevo nº de movimiento.
*!*	*>	- This.UsrError, Texto errores.

*!*	Parameters cMovimiento, nCantidad
*!*	Private cWhere, cNumMov, nCanFis, nCanOld, cLista
*!*	Local lStado

*!*	This.UsrError = ""

*!*	*> Asignar parámetros / propiedades a variables de trabajo.
*!*	cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)
*!*	nCanFis = Iif(Type('nCantidad')=='N', nCantidad, This.CanFis)

*!*	*> Validar parámetros.
*!*	If Empty(cNumMov) .Or. Empty(nCanFis)
*!*		This.UsrError = "Parámetros en blanco"
*!*		Return .F.
*!*	EndIf

*!*	*> Actualizar la nueva cantidad en el MP.
*!*	m.F14cNumMov = cNumMov
*!*	If f3_seek('F14c')
*!*		*> Comprobar que la cantidad es correcta.
*!*		Select F14c
*!*		Go Top
*!*		If F14cCanFis <= nCanFis
*!*			This.UsrError = "Cantidad insuficiente (MP)"
*!*			Return .F.
*!*		EndIf

*!*		*> Cantidad a actualizar en el MP actual.
*!*		nCanOld = F14c.F14cCanFis - nCanFis

*!*		cWhere  = "F14cNumMov='" + cNumMov + _cm
*!*		If !f3_UpdTun('F14c', , 'F14cCanFis', 'nCanOld', , cWhere, 'N')
*!*			This.UsrError = "Error actualizando cantidad (MP)"
*!*			Return .F.
*!*		EndIf

*!*		*> Obtener nuevo número de movimiento.
*!*		If Empty(This.NumMovNew)
*!*			This.NumMovNew = Ora_NewMP()
*!*		EndIf

*!*		*> Insertar nuevo movimiento en MPs.
*!*		Select F14c
*!*		Go Top
*!*		Replace F14cNumMov With This.NumMovNew, ;
*!*				F14cCanFis With nCanFis

*!*		If !f3_InsTun('F14c', , 'N')
*!*			This.UsrError = "Error insertando movimiento (MP)"
*!*			Return .F.
*!*		EndIf
*!*	EndIf

*!*	*> Actualizar cantidad en la lista original.
*!*	m.F26lNumMov = cNumMov
*!*	If f3_seek('F26l')
*!*		*> Comprobar que la cantidad es correcta.
*!*		Select F26l
*!*		Go Top
*!*		If F26lCanFis <= nCanFis
*!*			This.UsrError = "Cantidad insuficiente (LS)"
*!*			Return .F.
*!*		EndIf

*!*		cWhere  = "F26lNumMov='" + cNumMov + _cm
*!*		If !f3_UpdTun('F26l', , 'F26lCanFis', 'nCanOld', , cWhere, 'N')
*!*			This.UsrError = "Error actualizando cantidad (LS)"
*!*			Return .F.
*!*		EndIf

*!*		*> Obtener nuevo número de movimiento.
*!*		If Empty(This.NumMovNew)
*!*			This.NumMovNew = Ora_NewMP()
*!*		EndIf

*!*		*> Insertar nuevo movimiento en listas.
*!*		Select F26l
*!*		Go Top
*!*		cLista = F26lNumLst							&& Para actualizar cabecera.

*!*		Replace F26lNumMov With This.NumMovNew, ;
*!*				F26lCanFis with nCanFis

*!*		If !f3_InsTun('F26l', , 'N')
*!*			This.UsrError = "Error insertando movimiento (LS)"
*!*			Return .F.
*!*		EndIf

*!*		*> Actualizar la cabecera de la lista.
*!*		=This.ActualizarLista(cLista)
*!*	EndIf

*!*	*> Dividir el movimiento en detalle MACs.
*!*	m.F26wNMovMp = cNumMov
*!*	If f3_seek('F26w')
*!*		*> Obtener nuevo número de movimiento.
*!*		If Empty(This.NumMovNew)
*!*			This.NumMovNew = Ora_NewMP()
*!*		EndIf

*!*		*> Insertar nuevo movimiento en detalle MACs.
*!*		Select F26w
*!*		Go Top
*!*		Replace F26wNMovMp With This.NumMovNew

*!*		If !f3_InsTun('F26w', , 'N')
*!*			This.UsrError = "Error insertando movimiento (MAC)"
*!*			Return .F.
*!*		EndIf
*!*	EndIf


*!*	*> ------------------------------------------------------------------------------------------
*!*	*> LRC. 30/1/9. Validar que MP y LT coincidan ya que a veces hay diferencias que dan problemas.
*!*	cWhere = "F14cNumMov in ('" + cNumMov + "', '" + This.NumMovNew + "') And F14cNumMov=F26lNumMov"
*!*	If F3_sql("*", "F14c,F26l", cWhere, , , "F14c26l")
*!*		Select F14c26l
*!*		Go Top
*!*		Do While !Eof()
*!*		
*!*			*> Si hay diferencias, damos por buena la cantidad inferior.
*!*			Do Case
*!*				Case F14c26l.F26lCanFis < F14c26l.F14cCanFis			&& Actualizamos MP con la cantidad del LS
*!*					TxtSQL =          "Update F14c001"
*!*					TxtSQL = TxtSQL + "	  Set F14cCanFis=" + Str(F14c26l.F26lCanFis)
*!*					TxtSQL = TxtSQL + " Where F14cNumMov='" + F14c26l.F26lNumMov + "'"		
*!*					_Ok = f3_SqlExec(_Asql,TxtSQL)
*!*					
*!*				Case F14c26l.F14cCanFis < F14c26l.F26lCanFis			&& Actualizamos LS con la cantidad del MP
*!*					TxtSQL =          "Update F26l001"
*!*					TxtSQL = TxtSQL + "	  Set F26lCanFis=" + Str(F14c26l.F14cCanFis)
*!*					TxtSQL = TxtSQL + " Where F26lNumMov='" + F14c26l.F26lNumMov + "'"		
*!*					_Ok = f3_SqlExec(_Asql,TxtSQL)
*!*			EndCase
*!*					
*!*			Select F14c26l
*!*			Skip
*!*		EndDo
*!*	EndIf
*!*	*> ------------------------------------------------------------------------------------------

*!*	lStado = .T.
*!*	Return lStado

ENDPROC
PROCEDURE _genlstcab

*> Creación de la cabecera de la lista de trabajo.
*> Método protegido, de uso interno.

*> Recibe:
*>	- This.CurLst, lista actual.
*>	- This.CodOpe, operario al que se le asigna la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLst, lista actual.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 05.11.2007 (AVC) Incorporar flag de agrupación de documentos en lista.
*>					Se graba F26cFlag2 con la propiedad This.AgrDoc

Local lStado, lIsOpenF26c

lIsOpenF26c = Used("F26cUpd")
If !lIsOpenF26c
	=CrtCursor("F26c", "F26cUpd", "C")
EndIf

If Empty(This.CurLst)
	This.CurLst = Ora_NewLst()
EndIf
This.NumLst = This.CurLst

Select F26cUpd
Delete All
Append Blank

Replace F26cNumLst With This.CurLst, ;
		F26cCodOpe With This.CodOpe, ;
		F26cFecCre With Date(), ;
		F26cFPrMov With Date(), ;
		F26cFUlMov With Date(), ;
		F26cKgsAsg With 0, ;
		F26cBulAsg With 0, ;
		F26cMovAsg With 0, ;
		F26cMovAsg With 0, ;
		F26cMacAsg With 0, ;
		F26cVolAsg With 0, ;
		F26cKgsPte With 0, ;
		F26cBulPte With 0, ;
		F26cMovPte With 0, ;
		F26cMovPte With 0, ;
		F26cMacPte With 0, ;
		F26cVolPte With 0, ;
		F26cUbiExp With Space(14), ;
		F26cEstLst With "0", ;
		F26cFlag1  With Space(1), ;
		F26cFlag2  With This.AgrDoc

lStado = f3_InsTun('F26c', 'F26cUpd', 'N')
If !lStado
	This.UsrError = "No se puede insertar cabecera lista de trabajo " + This.CurLst
EndIf

If !lIsOpenF26c
	Use In F26cUpd
EndIf

Return lStado

ENDPROC
PROCEDURE _genlstdet

*> Creación de una línea de detalle de la lista de trabajo.
*> Recibe:
*>	- cNumero movimiento ó bien This.NumMovMP, MP del que toma los datos (opcional).
*>	- This.CurLst, lista de trabajo actual.
*>	- This.TipLst, tipo de lista.
*>	- This.CodOpe, operario al que se le asigna la lista.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>
*> Método protegido, de uso interno.

Parameters cNumeroMovimiento

Private cWhere, cNumMov
Local lStado, lIsOpenF26l

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovMP)

*> Leer, si cal, el MP.
If Type('This.oF14c')<>'O'
	If !f3_seek("F14c", "[m.F14cNumMov=cNumMov]")
		This.UsrError = "No se puede leer el MP: " + cNumMov
		lStado = .F.
		Return lStado
	EndIf

	Select F14c
	Scatter Name This.oF14c
EndIf

lIsOpenF26l = Used("F26lUpd")
If !lIsOpenF26l
	=CrtCursor("F26l", "F26lUpd", "C")
EndIf

*> Calcular el tipo de lista.
If Empty(This.TipLst)
	With This.oF14c
		Do Case
			Case SubStr(.F14cTipMov, 1, 1)=="2"			&& Lista de PREPARACION.
				This.TipLst = "P"
			Case SubStr(.F14cTipMov, 1, 1)=="3"			&& Lista de REPOSICION.
				This.TipLst = "R"
			Case SubStr(.F14cTipMov, 1, 1)=="4"			&& Lista de INVENTARIO.
				This.TipLst = "I"
			OtherWise									&& Tipo no definido.
				This.TipLst = "U"
		EndCase
	EndWith
EndIf

*> Preparar datos para insertar registro en detalle lista.
Select F26lUpd
Delete All
Append Blank

Replace F26lNumMov With This.oF14c.F14cNumMov, ;
		F26lNumEnt With This.oF14c.F14cNumEnt, ;
		F26lTipMov With This.oF14c.F14cTipMov, ;
		F26lTipDoc With This.oF14c.F14cTipDoc, ;
		F26lNumDoc With This.oF14c.F14cNumDoc, ;
		F26lLinDoc With This.oF14c.F14cLinDoc, ;
		F26lFecDoc With This.oF14c.F14cFecDoc, ;
		F26lDirAso With This.oF14c.F14cDirAso, ;
		F26lNumPed With This.oF14c.F14cNumPed, ;
		F26lLinPed With This.oF14c.F14cLinPed, ;
		F26lFecMov With This.oF14c.F14cFecMov, ;
		F26lFecLst With Date(), ;
		F26lCodArt With This.oF14c.F14cCodArt, ;
		F26lNumLot With This.oF14c.F14cNumLot, ;
		F26lSitStk With This.oF14c.F14cSitStk, ;
		F26lFecCad With This.oF14c.F14cFecCad, ;
		F26lCanFis With This.oF14c.F14cCanFis, ;
		F26lRutHab With This.oF14c.F14cRutHab, ;
		F26lUbiOri With This.oF14c.F14cUbiOri, ;
		F26lUbiDes With This.oF14c.F14cUbiDes, ;
		F26lNumPal With This.oF14c.F14cNumPal, ;
		F26lTipPal With This.oF14c.F14cTipPal, ;
		F26lUniVen With This.oF14c.F14cUniVen, ;
		F26lUniPac With This.oF14c.F14cUniPac, ;
		F26lPacCaj With This.oF14c.F14cPacCaj, ;
		F26lCajPal With This.oF14c.F14cCajPal, ;
		F26lFecFab With This.oF14c.F14cFecFab, ;
		F26lFecCal With This.oF14c.F14cFecCal, ;
		F26lNumAna With This.oF14c.F14cNumAna, ;
		F26lCodPro With This.oF14c.F14cCodPro, ;
		F26lCodOpe With This.CodOpe, ;
		F26lTipLst With This.TipLst, ;
		F26lNumLst With This.CurLst, ;
		F26lNumExp With This.oF14c.F14cNumExp, ;
		F26lOrdRec With This.oF14c.F14cOrdRec, ;
		F26lEstMov With "0", ;
		F26lOriRes With This.oF14c.F14cOriRes, ;
		F26lTipUbi With This.oF14c.F14cTipUbi, ;
		F26lTipMas With This.oF14c.F14cTipMas, ;
		F26lNumMas With This.oF14c.F14cNumMas, ;
		F26lMacUni With This.oF14c.F14cMacUni, ;
		F26lOrdRut With This.oF14c.F14cOrdRut, ;
		F26lTEntRe With This.oF14c.F14cTEntRe, ;
		F26lCEntRe With This.oF14c.F14cCEntRe, ;
		F26lVenHab With This.oF14c.F14cVenHab, ;
		F26lTipMac With This.oF14c.F14cTipMac, ;
		F26lNumMac With Iif(This.oF14c.F14cOriRes=='P', SubStr(This.oF14c.F14cNumPal, 2), This.oF14c.F14cNumMac), ;
		F26lSeccio With This.oF14c.F14cSeccio, ;
		F26lFlag1  With This.oF14c.F14cFlag1, ;
		F26lFlag2  With This.oF14c.F14cFlag2, ;
		F26lIdOcup With This.oF14c.F14cIdOcup

lStado = f3_InsTun('F26l', 'F26lUpd', 'N')
If !lStado
	*> Error.
	This.UsrError = "No se puede insertar línea lista de trabajo " + This.CurLst
	If !lIsOpenF26l
		Use In F26lUpd
	EndIf
	Return lStado
Endif

*> Actualizar operario y nº de lista en el MP.
Select F14cUpd
Delete All
Append Blank
Gather Name This.oF14c

Replace F14cCodOpe With This.CodOpe, ;
		F14cNumLst With This.CurLst

cWhere = "F14cNumMov='" + This.oF14c.F14cNumMov + "'"
lEstado = f3_UpdTun('F14c', , , , "F14cUpd", cWhere, 'N')

If !lStado
	This.UsrError = "No se puede actualizar lista de trabajo en MP " + This.CurLst
EndIf

If !lIsOpenF26l
	Use In F26lUpd
EndIf

Return lStado

ENDPROC
PROCEDURE chgope

*> Asignar una lista a otro operario.
*> Recibe:
*>	- cLista ó bien This.NumLst, nº de lista.
*>	- cOperario ó bien This.CodOpe, nuevo operario.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cLista, cOperario
Private cWhere, cNumLst, cNewOpe
Local lStado

This.UsrError = ""

*> Asignar parámetros / propiedades a variables de trabajo.
cNumLst = Iif(Type('cLista')=='C', cLista, This.NumLst)
cNewOpe = Iif(Type('cOperario')=='C', cOperario, This.CodOpe)

*> Validar parámetros.
If Empty(cNumLst) .Or. Empty(cNewOpe)
	This.UsrError = "Parámetros en blanco"
	Return .F.
EndIf

*> Reemplazar el operario en el detalle de la lista.
cWhere = "F26lNumLst='" + cNumLst + "'"
If !f3_UpdTun('F26l', , 'F26lCodOpe', 'cNewOpe', , cWhere, 'N')
	This.UsrError = "Error actualizando operario (LS)"
	Return .F.
EndIf

*> Reemplazar el operario en movimientos pendientes.
cWhere = "F14cNumLst='" + cNumLst + "'"
If !f3_UpdTun('F14c', , 'F14cCodOpe', 'cNewOpe', , cWhere, 'N')
	This.UsrError = "Error actualizando operario (MP)"
	Return .F.
EndIf

*> Reemplazar el operario en la cabecera de la lista.
cWhere = "F26cNumLst='" + cNumLst + "'"
If !f3_UpdTun('F26c', , 'F26cCodOpe', 'cNewOpe', , cWhere, 'N')
	This.UsrError = "Error actualizando operario (Lista)"
	Return .F.
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE chglstmp

*> Asignar un MP a otra lista.
*> Recibe:
*>	- cMovimiento ó bien This.NumMovLS, movimiento a cambiar.
*>	- cLista ó bien This.NumLst, nº de lista.
*>	- This.UpdLst, actualizar listas (S/N).
*>	- This.GenLista, lista nueva (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumLstNew, nº de lista, si se crea nueva.
*>	- This.UsrError, Texto errores.

Parameters cMovimiento, cLista
Private cWhere, cNumMov, cNewLst, cOldLst, cCodOpe, cNewOpe
Local lStado

This.UsrError = ""
cOldLst = ""

*> Asignar parámetros / propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovLS)
cNewLst = Iif(Type('cLista')=='C', cLista, This.NumLst)

*> Validar parámetros.
If Empty(cNumMov) .Or. (Empty(cNewLst) .And. This.GenLista<>"S")
	This.UsrError = "Parámetros en blanco"
	Return .F.
EndIf

*> Reemplazar la lista en el detalle de la lista.
m.F26lNumMov = cNumMov
If f3_seek("F26l")
	Select F26l
	If F26lEstMov<>"0"
		This.UsrError = "Movimiento en estado no válido"
		Return .F.
	EndIf

	cOldLst = F26lNumLst				&& Guardar lista origen para actualizar estado.
	cCodOpe = F26lCodOpe				&& Guardar operario para crear nueva lista.

	Do Case
		*> Asignar a una lista ya existente.
		Case This.GenLista<>"S" .Or. !Empty(cNewLst)
			m.F26cNumLst = cNewLst
			If !f3_seek("F26c")
				This.UsrError = "Lista nueva no existe"
				Return .F.
			EndIf

			*> Guardar operario nueva lista para actualizar.
			cNewOpe = F26cCodOpe

		*> Crear una nueva lista.
		Otherwise
			This.CurLst = ""
			This.CodOpe = cCodOpe
			lStado = This._genlstcab()
			If !lStado
				This.UsrError = "Error creando nueva lista"
				Return .F.
			EndIf

			cNewLst = This.CurLst
			cNewOpe = This.CodOpe
	EndCase

	*> Actualizar el nº de lista en el movimiento.
	cWhere = "F26lNumMov='" + cNumMov + "'"
	If !f3_UpdTun('F26l', , 'F26lNumLst,F26lCodOpe', 'cNewLst,cNewOpe', , cWhere, 'N')
		This.UsrError = "Error actualizando lista (LS)"
		Return .F.
	EndIf
EndIf

*> Reemplazar la lista en movimientos pendientes.
m.F14cNumMov = cNumMov
If f3_seek("F14c")
	*> Guardar la lista origen para actualizar estado.
	If Empty(cOldLst)
		cOldLst = F14c.F14cNumLst
		cCodOpe = f14c.F26lCodOpe
	EndIf

	cWhere = "F14cNumMov='" + cNumMov + "'"
	If !f3_UpdTun('F14c', , 'F14cNumLst,F14cCodOpe', 'cNewLst,cNewOpe', , cWhere, 'N')
		This.UsrError = "Error actualizando lista (MP)"
		Return .F.
	EndIf
EndIf

*> Actualizar el estado de la lista origen.
If !Empty(cOldLst)
	=This.ActualizarLista(cOldLst)
EndIf

*> Actualizar el estado de la lista nueva.
=This.ActualizarLista(cNewLst)

This.NumLstNew = cNewLst
lStado = .T.
Return lStado

ENDPROC
PROCEDURE chglst

*> Asignar una lista completa a otra lista.
*> Recibe:
*>	- cListaOrigen ó bien This.NumLst, lista origen.
*>	- cListaDestino ó bien This.NumLstNew, nº de lista nueva.
*>	- This.GenLista, Lista nueva (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumLstNew, nº de lista, si se crea nueva.
*>	- This.UsrError, Texto errores.

Parameters cListaOrigen, cListaDestino
Private cWhere, cNumMov, cNewLst, cOldLst
Local lStado

This.UsrError = ""
cOldLst = ""

*> Asignar parámetros / propiedades a variables de trabajo.
cOldLst = Iif(Type('cListaOrigen')=='C', cListaOrigen, This.NumLst)
cNewLst = Iif(Type('cListaDestino')=='C', cListaDestino, This.NumLstNew)

*> Validar parámetros.
If Empty(cOldLst) .Or. (Empty(cNewLst) .And. This.GenLista<>"S")
	This.UsrError = "Parámetros en blanco"
	Return .F.
EndIf

If cOldLst==cNewLst
	This.UsrError = "Parámetros incorrectos"
	Return .F.
EndIf

*> Cargar cursor con las líneas de la lista.
cWhere = "F26lNumLst='" + cOldLst + "' And F26lEstMov='0'"
lStado = f3_sql("*", "F26l", cWhere, , , "F26lChgLst")

If !lStado
	This.UsrError = "No hay datos a procesar"
	Use In (Select("F26lChgLst"))
	Return .F.
EndIf

Select F26lChgLst
Go Top
Do While !Eof()
	cNumMov = F26lNumMov
	lStado = This.ChgLstMP(cNumMov, cNewLst)
	If !lStado
		This.UsrError = "Error al cambiar movimiento: " + cNumMov
		Use In (Select("F26lChgLst"))
		Return lStado
	EndIf

	*> Recuperar la lista destino, si es nueva.
	If Empty(cNewLst)
		cNewLst = This.CurLst
	EndIf

	Select F26lChgLst
	Skip
EndDo

*> Actualizar el estado de las listas origen y destino.
With This
	.UpdLst = "S"
	=.ActualizarLista(cOldLst)
	=.ActualizarLista(cNewLst)
EndWith

Use In (Select("F26lChgLst"))
This.NumLstNew = cNewLst
lStado = .T.
Return lStado

ENDPROC
PROCEDURE chgubimp

*> Cambiar la ubicación origen a un MP.

*> Recibe:
*>	- cMovimiento ó bien This.NumMovMP, movimiento a cambiar.
*>	- cIdNew ó bien This.RowIDNew, identificador ocupación destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

*> Llamado desde:
*>		- This.CncCrgLstMP, Cancelar carga listas.
*>		- This._cnflstmpr, Confirmar lista de reposición.
*>		- This.ChgUbiID, Cancelar ubicación origen por ID ocupación.

*> Historial de modificaciones:
*> 07.01.2008 (LRC) Ajustar cantidad en reposiciones.

Parameters cMovimiento, cIdDestino

Private cWhere, cNumMov, cRowId, cRowIdNew, oF14c, oF16cDst

Local lStado

This.UsrError = ""

*> Asignar parámetros / propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)
cRowIdNew = Iif(Type('cIdDestino')=='C', cIdDestino, This.RowIDNew)

*> Validar parámetros.
If Empty(cNumMov) .Or. Empty(cRowIdNew)
	This.UsrError = "Parámetros en blanco"
	Return .F.
EndIf

*> Leer el movimiento pendiente.
If !f3_seek("F14c", '[m.F14cNumMov=cNumMov]')
	This.UsrError = "No existe el movimiento (MP)"
	Return .F.
EndIf

*> Guardar ocupación origen.
Select F14c
Scatter Name oF14c
cRowId = oF14c.F14cIdOcup

If cRowId==cRowIdNew
	This.UsrError = "Origen y destino son iguales"
	Return .F.
EndIf

*> Cargar ocupación destino.
cWhere = "F16cIdOcup='" + cRowIdNew + _cm
If !f3_sql("*", "F16c", cWhere, , , "F16cIdDst")
	This.UsrError = "No existe ocupación destino"
	Use In (Select("F16cIdDst"))
	Return .F.
EndIf

*> Guardar ocupación destino.
Select F16cIdDst
Go Top
Scatter Name oF16cDst

*> Validar datos de ambas ocupaciones.
If oF14c.F14cCodPro<>oF16cDst.F16cCodPro .Or. ;
   oF14c.F14cCodArt<>oF16cDst.F16cCodArt
	This.UsrError = "El artículo es distinto"

	Use In (Select("F16cIdDst"))
	Return .F.
EndIf

*> Validar cantidad en ocupación destino, sólo si se trata de reservas.
If Left(oF14c.F14cTipMov, 1) == '2'
	If (oF16cDst.F16cCanFis < oF14c.F14cCanFis)
		This.UsrError = "Cantidad destino insuficiente"

		Use In (Select("F16cIdDst"))
		Return .F.
	EndIf
EndIf

*> LRC. 7/1/2008
*> Si el MP es de reposoción se debe ajustar la cantidad a la menor entre la del MP y la del palet leido.
If Left(oF14c.F14cTipMov, 1) == '3'
	cCanRep = IIf(oF14c.F14cCanFis < oF16cDst.F16cCanFis, oF14c.F14cCanFis, oF16cDst.F16cCanFis)
EndIf

*> Cambiar datos ocupación en el movimiento pendiente.
Select F14c
Replace F14cUbiOri With oF16cDst.F16cCodUbi
Replace F14cNumLot With oF16cDst.F16cNumLot
Replace F14cNumPal With oF16cDst.F16cNumPal
Replace F14cSitStk With oF16cDst.F16cSitStk
Replace F14cFecCad With oF16cDst.F16cFecCad
Replace F14cIdOcup With oF16cDst.F16cIdOcup
Replace F14cCanFis With IIf(Left(oF14c.F14cTipMov, 1) == '3', cCanRep, F14cCanFis)	&& LRC. 7/1/2008

cWhere = "F14cNumMov='" + cNumMov + _cm
If !f3_updtun("F14c", , , , , cWhere, "N")
	This.UsrError = "No se ha podido actualizar origen (MP)"

	Use In (Select("F16cIdDst"))
	Return .F.
EndIf

*> Cambiar datos ocupación en la lista.
If f3_seek("F26l", '[m.F26lNumMov=cNumMov]')
	Select F26l
	Replace F26lUbiori With oF16cDst.F16cCodUbi
	Replace F26lNumLot With oF16cDst.F16cNumLot
	Replace F26lNumPal With oF16cDst.F16cNumPal
	Replace F26lSitStk With oF16cDst.F16cSitStk
	Replace F26lFecCad With oF16cDst.F16cFecCad
	Replace F26lIdOcup With oF16cDst.F16cIdOcup
	Replace F26lCanFis With IIf(Left(oF14c.F14cTipMov, 1) == '3', cCanRep, F26lCanFis)	&& LRC. 7/1/2008

	cWhere = "F26lNumMov='" + cNumMov + _cm
	If !f3_updtun("F26l", , , , , cWhere, "N")
		This.UsrError = "No se ha podido actualizar origen (LS)"

		Use In (Select("F16cIdDst"))
		Return .F.
	EndIf
EndIf

*> Recalcular ocupación origen.
With This.RStkObj
	.Inicializar
	.RSDelE = "S"
	.RowID = cRowId
	.Ejecutar('01')
EndWith

*> Recalcular ocupación destino.
With This.RStkObj
	.Inicializar
	.RSDelE = "S"
	.RowID = cRowIdNew
	.Ejecutar('01')
EndWith

Use In (Select("F16cIdDst"))

Return

ENDPROC
PROCEDURE rstkobj_access

*> Inicializar clase de recálculo de stocks.
If Type('This.RStkObj')<>'O'
	This.RStkObj = CreateObject('OraFncRStk')
EndIf

Return This.RStkObj

ENDPROC
PROCEDURE cnflstmp

*> Confirmar un MP de una lista de trabajo. En función del tipo (preparación, reposición),
*> realiza la llamada al método específico para el tratamiento del movimiento.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovLS, nº de MP a confirmar.
*>	- This.FrzCnf, Forzar si el movimiento está bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedición (listas de preparación). (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (listas de preparación). (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (listas de preparación). (usualmente 3000).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cNumeroMovimiento

Private cWhere, cNumMov
Local lStado

This.UsrError = ""

=This.InitLocals()

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovLS)

*> Cargar MP de la lista.
m.F26lNumMov = cNumMov
If !f3_seek("F26l")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F26l
Scatter Name This.oF26l

*> Forzar confirmación, aunque el MP esté bloqueado.
If This.oF26l.F26lFlag1<>Space(1) .And. This.FrzCnf<>'S'
	This.UsrError = "Movimiento bloqueado"
	Return .F.
EndIf

*> Seleccionar el proceso a realizar, en función del tipo de lista.
Do Case
	*> Lista de PREPARACION.
	Case Between(This.oF26l.F26lTipMov, '2000', '2998')
		lStado = This._cnflstmpp()

	*> Lista de REPOSICION.
	Case Between(This.oF26l.F26lTipMov, '3000', '3999')
		lStado = This._cnflstmpr()

	*> Tipo de lista no definido.
	Otherwise
		This.UsrError = "Tipo de lista: " + This.oF26l.F26lTipLst + " no definido"
		lStado = .F.
EndCase

Return lStado

ENDPROC
PROCEDURE actzobj_access

*> Crear objeto de la clase para la función de actualización.
If Type('This.ActzObj')<>'O'
	This.ActzObj = CreateObject('OraFncActz')
EndIf

Return This.ActzObj

ENDPROC
PROCEDURE actzprm_access

*> Crear objeto de la clase para la asignar parámetros a la función de actualización.
If Type('This.ActzPrm')<>'O'
	This.ActzPrm = CreateObject('OraPrmActz')
EndIf

Return This.ActzPrm

ENDPROC
PROCEDURE cnfcrgmp

*> Confirmar salida de muelle de un movimiento pendiente.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovMP, nº de MP a confirmar.
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.UpdTransito, actualizar stock en tránsito (S/N). (NO UTILIZADO).
*>	- This.StkTrn, Situación stock en tránsito (S/N).       (NO UTILIZADO).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.CnfCrgDoc, confirmar carga de un pedido.
*>	- SAEXCFCA, confirmar carga MP (PANTALLA)
*>	- <prorad>.CnfActualizar, confirmar carga MP (RF).

*> Historial de modificaciones:
*>	- Cambiar el cálculo del estado del documento. AVC - 13.06.2007
*>	- Adaptar a expediciones parciales. AVC - 14.06.2007
*>	- Adaptar a partes de montaje. AVC - 03.12.2007
*> 13.05.2008 (AVC) Adaptar a listas de carga, dando de baja el documento confirmado si está en una lista de carga.

Parameters cNumeroMovimiento

Private cWhere, oF14c, cFlag, cNumMov, cCampos
Local lStado, lParteMontaje, oF24c

This.UsrError = ""

=This.InitLocals()

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovMP)

*> Cargar el movimiento pendiente.
m.F14cNumMov = cNumMov
If !f3_seek("F14c")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F14c
Scatter Name oF14c

*> Validar tipo de movimiento a generar en histórico.
m.F00bCodMov = oF14c.F14cTipMov
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + oF14c.F14cTipMov + " no existe"
	Return .F.
EndIf

*> Validar la situación de stock en tránsito.
If This.UpdTransito=="S"
	m.F00cCodStk = This.StkTrn
	If !f3_seek('F00c')
		This.UsrError = "Situación stock tránsito: " + This.StkTrn + " no existe"
		Return .F.
	EndIf
EndIf

*> Según el tipo de documento se conoce si se trata de un parte de salida.
lParteMontaje = Iif(oF14c.F14cTipDoc==TDocSalida, .T., .F.)

*>
If lParteMontaje

	*> Recuperar la ubicación de la entidad.
	m.F24cCodPro = oF14c.F14cCodPro
	m.F24cTipDoc = TDocSalida
	m.F24cNumDoc = oF14c.F14cNumDoc
	_UbiExp = IIf(F3_Seek("F24c"), F24c.F24cUbiExp, oF14c.F14cCodUbi)

	_NewHMO = Ora_NewHM()                   && Número movimiento origen
	_NewHMD = Ora_NewHM()                   && Número movimiento destino
	Do While Val(_NewHMD) # (Val(_NewHMO) + 1)
      *> Los números NO son correlativos.
      _NewHMO = Ora_NewHM()
      _NewHMD = Ora_NewHM()
	EndDo

	_TMovHMO = '3600'
	_TMovHMD = '3100'
	
	*> Validar tipo de movimiento a generar en histórico.
	m.F00bCodMov = _TMovHMO
	If !f3_seek('F00b')
		This.UsrError = "Tipo movimiento: " + _TMovHMO + " no existe"
		Return .F.
	EndIf

	*> Validar tipo de movimiento a generar en histórico.
	m.F00bCodMov = _TMovHMD
	If !f3_seek('F00b')
		This.UsrError = "Tipo movimiento: " + _TMovHMD + " no existe"
		Return .F.
	EndIf

EndIf


*!*	*> Leer el flag de documento normal / parte de montaje.
*!*	If f3_seek("F24c")
*!*		Select F24c
*!*		Go Top
*!*		Scatter Name oF24c
*!*		lParteMontaje = Iif(oF24c.F24cFlag1=='1', .T., .F.)
*!*	Else
*!*		lParteMontaje = .F.
*!*	EndIf

*> Asignar propiedades a la función de actualización.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('M', oF14c)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'

	.PmFgHM = 'S'			&& Iif(lParteMontaje, 'N', 'S')
	.PoCFis = oF14c.F14cCanFis

	.PoCRes = oF14c.F14cCanFis
	.PoRowID= oF14c.F14cIdOcup

	.PmEnSa = "S"
	.PmTMov = oF14c.F14cTipMov

	.PtAcc  = '06'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
*	This.UsrError = "Error confirmando salida muelle"
*	Return .F.
EndIf


*> Si es un pedido normal se realiza una anotación en HM del MP de expedición, sino el MP de salida por reubicación.
If lParteMontaje
	With This.ActzObj.ObjParm
		*> Datos actualización MPs/HMs
		.PMTMov = _TMovHMO                   && Tipo movimiento
		.PMNMov = _NewHMO                    && Número movimiento
		.PMFMov = Date()                     && Fecha movimiento
		.PMEnSa = 'S'                        && Entrada / Salida
	EndWith
EndIf

This.ActzObj.ActHM									&& Registro en histórico con tipo de Movimiento

*> -----------------------------------------------------------------------------------------
*> Si se trata de un parte de taller hacer una entrada sin reservar.
*> Actualizar el campo lote con el numero de parte y en el campo Nº analisis guardar el lote.
If lParteMontaje

	*> Movimiento de entrada.
	With This.ActzObj.ObjParm
		.PONLot = oF14c.F14cNumDoc
		.PONAna = oF14c.F14cNumLot
		.PoCRes = 0

		.POCUbi = _UbiExp

		.PtAcc  = '07'
	EndWith
	
	This.ActzObj.Ejecutar
	
	If This.ActzObj.ObjParm.PwCrtn >= '50'
		This.UsrError = "Error entrando stock de taller"
		Return .F.
	EndIf

	With This.ActzObj.ObjParm
		*> Datos actualización MPs/HMs
		.PMTMov = _TMovHMD                   && Tipo movimiento
		.PMNMov = _NewHMD                    && Número movimiento
		.PMFMov = Date()                     && Fecha movimiento
		.PMEnSa = 'E'                        && Entrada / Salida
	EndWith

	This.ActzObj.ActHM									&& Registro en histórico con tipo de Movimiento

EndIf
*> -----------------------------------------------------------------------------------------

*> Borrar el MP.
m.F14cNumMov = cNumMov
=f3_baja('F14c')

*> Actualizar la cantidad enviada en la línea de detalle del documento.
With oF14c
	m.F24lCodPro = .F14cCodPro
	m.F24lTipDoc = .F14cTipDoc
	m.F24lNumDoc = .F14cNumDoc
	m.F24lLinDoc = PadL(AllTrim(Str(.F14cLinDoc)), 4, "0")
EndWith

If f3_seek("F24l")
	Select F24l
	Replace F24lCanEnv With F24lCanEnv + oF14c.F14cCanFis

	Scatter MemVar
	=f3_upd("F24l")
EndIf

*> Actualizar, si cal, el estado de la cabecera del documento.
If This.UpdDoc=='S'
	*> Estado general del documento.
	lStado = This.ResvObj.UpdDocRsv(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)

	*> Actualizar el estado de la línea de la lista de carga.
	lStado = This.ActualizarListaCD(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)
EndIf

Return

ENDPROC
PROCEDURE resvobj_access

*> Inicializar clase de reserva.
If Type('This.ResvObj')<>'O'
	This.ResvObj = CreateObject('OraFncResv')
EndIf

Return This.ResvObj

ENDPROC
PROCEDURE resvprm_access

*> Crear objeto de la clase para la asignar parámetros a la función de reserva.
If Type('This.ResvPrm')<>'O'
	This.ResvPrm = CreateObject('OraPrmResv')
EndIf

Return This.ResvPrm

ENDPROC
PROCEDURE cancellstmp
	
*> Cancelar un MP de una lista de trabajo.
*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovLS, nº de MP a confirmar.
*>	- This.FrzCnf, Forzar si el movimiento está bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.TMovOrg, Tipo movimiento para HM (usualmente 4500).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cNumeroMovimiento

Private cWhere, cNumMov, oF26l
Local lStado

This.UsrError = ""

=This.InitLocals()

*> Asignar priopiedades a variables de trabajo.
cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovLS)

*> Validar tipo de movimiento a generar en histórico.
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

*> Cargar MP de la lista.
m.F26lNumMov = cNumMov
If !f3_seek("F26l")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F26l
Scatter Name oF26l

If oF26l.F26lEstMov<>'0'
	This.UsrError = "El movimiento ya está confirmado"
	Return .F.
EndIf

*> Forzar regularización aunque esté bloqueado.
If oF26l.F26lFlag1<>Space(1) .And. This.FrzCnf<>'S'
	This.UsrError = "Movimiento bloqueado"
	Return .F.
EndIf

*> Primer paso: Eliminar el movimiento de la lista.
*> El resto de propiedades ya se han recibido al llamar a este método.
With This
	.NumLst = oF26l.F26lNumLst
	lStado = .BorrarListaMP()
EndWith

If !lStado
	Return lStado
EndIf

*> Segundo paso: Desreservar el MP.
*> El resto de propiedades ya se han recibido al llamar a este método.
With This.ResvObj
	.ObjParm = This.ResvPrm

	With .ObjParm
		.Inicializar
	EndWith

	lStado = .DesReserva(oF26l.F26lNumMov)
	If lStado
		lStado = .UpdDocRsv(oF26l.F26lCodPro, oF26l.F26lTipDoc, oF26l.F26lNumDoc)
	EndIf
EndWith

If !lStado
	This.UsrError = This.ResvObj.UsrError
	Return lStado
EndIf

*> Finalmente, Regularizar stock físico y crear movimiento en el histórico.
This.ActzObj.ObjParm = This.ActzPrm

With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('L', oF26l)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF26l.F26lCanFis
	.PoCRes = 0
	.PoRowID= oF26l.F26lIdOcup
	
	.PmEnSa = "S"
	.PmTMov = This.TMovOrg

	.PtAcc  = '08'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error al regularizar movimiento"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico

lStado = .T.
Return lStado

ENDPROC
PROCEDURE cnccrgmp

*> Cancelar salida de muelle de un movimiento pendiente.
*> El material queda en el muelle y pendiente de reubicar.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovMP, nº de MP a confirmar.
*>	- This.UpdDoc, actualizar estado documento (S/N).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*>	- Cambiar el cálculo del estado del documento. AVC - 13.06.2007
*>	- Adaptar a expediciones parciales. AVC - 14.06.2007
*> 13.05.2008 (AVC) Adaptar a listas de carga, dando de baja el documento confirmado si está en una lista de carga.

Parameters cNumeroMovimiento

Private cWhere, oF14c, cFlag, cNumMov
Local lStado

This.UsrError = ""

=This.InitLocals()

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovMP)

*> Cargar el movimiento pendiente.
m.F14cNumMov = cNumMov
If !f3_seek("F14c")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F14c
Scatter Name oF14c

*> Asignar propiedades a la función de actualización.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('M', oF14c)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'N'

	.PoCFis = 0
	.PoCRes = oF14c.F14cCanFis
	.PoRowID= oF14c.F14cIdOcup

	.PmEnSa = "S"
	.PmTMov = oF14c.F14cTipMov

	.PtAcc  = '06'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error confirmando salida muelle"
	Return .F.
EndIf

*> Borrar el MP.
m.F14cNumMov = cNumMov
=f3_baja('F14c')

*> Actualizar, si cal, el estado de la cabecera del documento.
If This.UpdDoc=='S'

	*> -----------------------------------------------------------------------------
	*> LRC. 17/2/9.
	*> Se actualiza el esado cabecera a 3 para permitir que el método UpdDocRsv lo pueda recalcular.
	cCampos  = "F24cFlgEst"
	cValores = "3"
	cWhere   =               "F24cCodPro='" + oF14c.F14cCodPro + "'"
	cWhere   = cWhere + " And F24cTipDoc='" + oF14c.F14cTipDoc + "'"
	cWhere   = cWhere + " And F24cNumDoc='" + oF14c.F14cNumDoc + "'"
	=f3_updtun('F24c', , cCampos, cValores, , cWhere, 'N', 'N')
	*> -----------------------------------------------------------------------------

	*> Estado general del documento.
	lStado = This.ResvObj.UpdDocRsv(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)
	
	*> Actualizar el estado de la línea de la lista de carga.
	lStado = This.ActualizarListaCD(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)
EndIf

Return

ENDPROC
PROCEDURE cnccrglstmp

*> Cancelar expedición de un MP y reasignar a una lista de trabajo.
*> El material queda en su ubicación origen y pendiente de asignar a lista.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovMP, nº de MP a cancelar.
*>	- This.TMovLst, Tipo movimiento preparación (usualmente 2000).
*>	- This.TMovOrg, Tipo movimiento origen para HM (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (usualmente 3000).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Revisiones:
*>	AVC - 16.06.2005 Borrar el nº de MAC.
*>	AVC - 13.06.2006 Asignar la ubicación destino (muelle) al MP.
*>	AVC - 14.06.2007 Adaptar a expediciones parciales.
*> 13.05.2008 (AVC) Adaptar a listas de carga, dando de baja el documento confirmado si está en una lista de carga.

Parameters cNumeroMovimiento

Private cWhere, cNumMov, oF14c, oF26l, oF16c, cNewMov, cRowID, cCampos, cValores
Local lStado

This.UsrError = ""

=This.InitLocals()

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovMP)

*> Leer el MP.
m.F14cNumMov = cNumMov
If !f3_seek("F14c")
	This.UsrError = "No se encuentra el movimiento (MP): " + cNumMov
	Return .F.
EndIf

Select F14c
Scatter Name oF14c

*> Validación de los datos del movimiento
If F14cTipMov<>'2999'
	This.UsrError = "Tipo de movimiento incorrecto"
	Return .F.
EndIf

*> Leer la lista para reubicación.
m.F26lNumMov = cNumMov
If !f3_seek("F26l")
	This.UsrError = "No se encuentra el movimiento (LS): " + cNumMov
	Return .F.
EndIf

Select F26l
Scatter Name oF26l

*> Validar tipo de movimiento de preparación.
m.F00bCodMov = This.TMovLst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovLst + " no existe"
	Return .F.
EndIf

*> Validar tipos de movimiento a generar en histórico.
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Asignar propiedades a la función de actualización.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida reservada desde muelle.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('M', oF14c)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF14c.F14cCanFis
	.PoCRes = oF14c.F14cCanFis
	.PoRowID= oF14c.F14cIdOcup
	.PoRowIDNew = ""
	
	.PmEnSa = "S"
	.PmTMov = This.TMovOrg

	.PtAcc  = '06'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error en salida reservada muelle"
	*> Continuar el proceso, aunque haya errores.	>>>>> Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico

*> Movimiento de entrada destino.
With This.ActzObj.ObjParm
	.PuBOld = oF26l.F26lUbiOri

	.PoCUbi = oF26l.F26lUbiOri
	.PoRowID = ""
	.PoRowIDNew = ""		&& oF26l.F26lIdOcup

	.PmEnSa = "E"
	.PmTMov = This.TMovDst
	.PmNMov = ""

	.PtAcc  = '07'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error en entrada reservada destino"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico

*> Leer los datos de la nueva ocupación.
cRowID = This.ActzObj.GetLocals("WRowID")

If !f3_seek("F16c", '[m.F16cIdOcup=cRowID]')
	This.UsrError = "Error leyendo nueva ocupación destino"
	Return .F.
EndIf

Select F16c
Scatter Name oF16c

*> Cambiar el Nº de MP para evitar conflictos con el movimiento original.
cNewMov = Ora_NewMP()

*> Actualizar los datos en el MP (para la lista).
Select F14c
Replace F14cTipMov With This.TMovLst				&& Tipo movimiento (usualmente 2000)
Replace F14cUbiDes With oF26l.F26lUbiDes			&& Destino (muelle)
Replace F14cNumMov With cNewMov						&& Nuevo nº de movimiento
Replace F14cNumLst With Space(6)
Replace F14cCodOpe With Space(4)
Replace F14cNumMac With Space(9)

cWhere = "F14cNumMov='" + cNumMov + "'"
=f3_updtun("F14c", , , , , cWhere)

*> Actualizar estado en detalle lista.
cCampos  = "F26lEstMov"
cValores = "4"
cWhere   = "F26lNumMov='" + cNumMov + "'"
=f3_updtun('F26l', , cCampos, cValores, , cWhere, 'N', 'N')

*> Actualizar los datos de la nueva ocupación en el MP.
lStado = This.ChgUbiMP(cNewMov, cRowID)

*> -----------------------------------------------------------------------------
*> LRC. 17/2/9.
*> Se actualiza el esado cabecera a 3 para permitir que el método UpdDocRsv lo pueda recalcular.
cCampos  = "F24cFlgEst"
cValores = "3"
cWhere   =               "F24cCodPro='" + oF14c.F14cCodPro + "'"
cWhere   = cWhere + " And F24cTipDoc='" + oF14c.F14cTipDoc + "'"
cWhere   = cWhere + " And F24cNumDoc='" + oF14c.F14cNumDoc + "'"
=f3_updtun('F24c', , cCampos, cValores, , cWhere, 'N', 'N')
*> -----------------------------------------------------------------------------

*> Actualizar el estado general del documento.
lStado = This.ResvObj.UpdDocRsv(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)

*> Actualizar el estado de la línea de la lista de carga.
lStado = This.ActualizarListaCD(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)

Return lStado

ENDPROC
PROCEDURE _cnflstmpp

*> Confirmar un MP de una lista de PREPARACION.

*> Recibe (de This.CnfLstMP):
*>	- This.FrzCnf, Forzar si el movimiento está bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedición (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (usualmente 3000).
*>	- This.oF26l, Movimiento que contiene los datos de la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Método privado, de uso interno de la clase. LLamado desde This.CnfLstMP.

*> Modificaciones:
*>		AVC - 02.06.2005 : Unificar funcionalidad de cálculo de ubicación destino, si está en blanco, con
*>						   la clase de reserva, pues este proceso se utilizará aquí y en reserva de material.
*>		AVC - 17.06.2005 : Agregar validación de MP con reposiciones pendientes.
*>		??? - 10.08.2006 : Agregar actualización estado documento 'pedido preparado'.
*>		AVC - 17.08.2006 : Grabar Nº palet como MAC si bulto completo.

Private cWhere, cCampos, cValores, cNumMov, oF26l
Private dFecMov, cEstMov, cFlag, cTipMov, cRowID
Private cUbiOri, cUbiDes, cCodPro, cTipDoc, cNumDoc, cNumMac
Local lStado

This.UsrError = ""

oF26l = This.oF26l
cNumMov = oF26l.F26lNumMov
cCodPro = oF26l.F26lCodPro
cTipDoc = oF26l.F26lTipDoc
cNumDoc = oF26l.F26lNumDoc

*> Validar tipos de movimiento a generar en histórico.
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Validar tipo de movimiento expedición.
m.F00bCodMov = This.TMovExp
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovExp + " no existe"
	Return .F.
EndIf

*> Ubicación destino por defecto, si cal.
If Empty(oF26l.F26lUbiDes)
	lStado = This.AsignarMuelle(oF26l.F26lCodPro, oF26l.F26lTipDoc, oF26l.F26lNumDoc)
	If !lStado
		*> El mensaje ya viene asignado.
		Return lStado
	EndIf

	oF26l.F26lUbiDes = This.Muelle
EndIf

*> Validar si tiene reposiciones pendientes.
If This.ValidarReposicion(oF26l.F26lIdOcup)
	This.UsrError = "Movimiento con reposiciones pendientes"
	lStado = .F.
	Return lStado
EndIf

*> Asignar propiedades a la función de actualización.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('L', oF26l)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF26l.F26lCanFis
	.PoCRes = oF26l.F26lCanFis
	.PoRowID= oF26l.F26lIdOcup
	
	.PmEnSa = "S"
	.PmTMov = This.TMovOrg

	.PtAcc  = '06'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error confirmando salida origen"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico.

*> Movimiento de entrada destino.
With This.ActzObj.ObjParm
	.PuBOld = oF26l.F26lUbiDes

	.PoCUbi = oF26l.F26lUbiDes
	.PoRowIDNew = ""

	.PmEnSa = "E"
	.PmTMov = This.TMovDst
	.PmNMov = ""

	.PtAcc  = '07'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error confirmando entrada destino"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico.

*> Actualizar el estado de la línea de detalle de la lista.
dFecMov = Date()
cEstMov = "3"
cFlag = Space(1)

If (oF26l.F26lOriRes=='P' .Or. oF26l.F26lOriRes=='C') .And. Empty(oF26l.F26lNumMac)
	cNumMac = SubStr(oF26l.F26lNumPal, 2, 9)
Else
	cNumMac = oF26l.F26lNumMac
EndIf

cCampos  = "F26lFecMov,F26lEstMov,F26lFlag1,F26lNumMac"
cValores = "dFecMov,cEstMov,cFlag,cNumMac"
cWhere   = "F26lNumMov='" + cNumMov + "'"

lStado = f3_updtun('F26l', , cCampos, cValores, , cWhere, 'N', 'N')

*> Actualizar, si cal, la cabecera de la lista de trabajo.
=This.ActualizarLista(oF26l.F26lNumLst)

*> Recuperar la ID ocupación destino.
cRowID = This.ActzObj.GetLocals("WRowID")

*> Actualizar TMOV, Ubicación e ID ocupación en el MP.
cWhere = "F14cNumMov='" + cNumMov + "'"
cCampos = "F14cTipMov,F14cIdOcup,F14cUbiOri,F14cUbiDes,F14cNumMac"
cValores = "cTipMov,cRowID,cUbiOri,cUbiDes,cNumMac"

cTipMov = This.TMovExp
cUbiOri = oF26l.F26lUbiDes
cUbiDes = Space(14)

=f3_updtun('F14c', , cCampos, cValores, , cWhere, 'N', 'N')

*> Estado de preparación del documento.
=This._actualizarestadodocumento(cCodPro, cTipDoc, cNumDoc)

*> -------------------------------------------------------------------------------------------
*> LRC. 6.2.9. Si no quedan MP de tipo 2000 se debe liberar el pedido.
_Where =               "F14cTipMov='2000'"
_Where = _Where + " And F14cCodPro='" + oF26l.F26lCodPro + "'"
_Where = _Where + " And F14cTipDoc='" + oF26l.F26lTipDoc + "'"
_Where = _Where + " And F14cNumDoc='" + oF26l.F26lNumDoc + "'"

lStado=f3_sql('*', 'F14c', _Where, , , 'F14c2000')

If !lStado and _xier > 0
    oComm = CreateObject('commstd', , _Procaot, _Alma)

	oComm.Inicializar
	lStado = oComm.ProcesarBloqueoPedido('L', 'DOCS', oF26l.F26lCodPro, oF26l.F26lNumDoc)
	If !Empty(lStado)
		*> Pedido no disponible.
		_LxErr = oComm.UsrError
	EndIf
EndIf

Use In (Select("F14c2000"))
*> -------------------------------------------------------------------------------------------



lStado = .T.
Return lStado

ENDPROC
PROCEDURE _cnflstmpr

*> Confirmar un MP de una lista de REPOSICION. Actualiza los MPs de preparación asociados a la reposición. Se valida que la cantidad
*> de los MPs 2000 con la cantidad de la reposición, dividiendo el MP de preparación si la cantidad es mayor que la de la reposición.

*> Método privado, de uso interno de la clase.

*> Recibe (de This.CnfLstMP):
*>	- This.FrzCnf, Forzar si el movimiento está bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.oF26l, Movimiento que contiene los datos de la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*>	LLamado desde This.CnfLstMP.

*> Revisiones:
*>	AVC - 13.06.2006 Validar la cantidad de los MPs 2000 que se procesan.
*>	Jony - 02.11.2006 Cambiar estado en MP origen

Private cWhere, cWhereMPs, cCampos, cValores, cNumMov, oF14c, oF26l, dFecMov, cEstMov, cFlag, cRowID, nCantidadResto
Local lStado, lHayMPs, nCantidadRepos

This.UsrError = ""

oF26l = This.oF26l
cNumMov = oF26l.F26lNumMov

*> Calcular y validar tipos de movimiento a generar en histórico.
This.TMovOrg = oF26l.F26lTipMov
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

This.TMovDst = PadL(AllTrim(Str(Val(oF26l.F26lTipMov) - 500)), 4, '0')
m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Asignar propiedades a la función de actualización.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('L', oF26l)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF26l.F26lCanFis
	.PoCRes = oF26l.F26lCanFis
	.PoRowID= oF26l.F26lIdOcup
	
	.PmEnSa = "S"
	.PmTMov = This.TMovOrg

	.PtAcc  = '06'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error confirmando salida origen"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico.

*> Movimiento de entrada destino.
With This.ActzObj.ObjParm
	.PuBOld = oF26l.F26lUbiDes

	.PoCUbi = oF26l.F26lUbiDes
	.PoRowIDNew = ""

	.PmEnSa = "E"
	.PmTMov = This.TMovDst
	.PmNMov = ""

	.PtAcc  = '07'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error confirmando entrada destino"
	Return .F.
EndIf

This.ActzObj.ActHM				&& Registro en histórico.

*> Actualizar el estado de la línea de detalle de la lista.
dFecMov = Date()
cEstMov = "3"
cFlag = Space(1)

cCampos  = "F26lFecMov,F26lEstMov,F26lFlag1"
cValores = "dFecMov,cEstMov,cFlag"
cWhere   = "F26lNumMov='" + cNumMov + "'"

lStado = f3_updtun('F26l', , cCampos, cValores, , cWhere, 'N', 'N')

*> Actualizar, si cal, la cabecera de la lista de trabajo.
=This.ActualizarLista(oF26l.F26lNumLst)

*> Recuperar la ID ocupación destino.
cRowID = This.ActzObj.GetLocals("WRowID")

*> Cantidad máxima a repones en los MPs.
nCantidadRepos = oF26l.F26lCanFis

*> Realizar el cambio de ocupación en los MPs de preparación asociados a la reposición.
cWhereMPs = "F14cIdOcup='" + oF26l.F26lIdOcup + "' And F14cTipMov Between '2000' And '2999' And F14cFlag1='B'"
=f3_sql("*", "F14c", cWhereMPs, , , "F14cUpdRep")

Select F14cUpdRep
Go Top
lHayMPs = !Eof()

Do While !Eof() .And. nCantidadRepos > 0
	Scatter Name oF14c
	If oF14c.F14cCanFis > nCantidadRepos
		*> Dividir el MP y reload del cursor.
		nCantidadResto = oF14c.F14cCanFis - nCantidadRepos
		lStado = This.DividirMvt(oF14c.F14cNumMov, nCantidadResto)
		If !lStado
			*> Error al dividir el MP.
			Use In (Select("F14cUpdRep"))
			Return lStado
		EndIf

		=f3_sql("*", "F14c", cWhereMPs, , , "F14cUpdRep")
		Select F14cUpdRep
		Go Top
		Loop
	EndIf

	=This.ChgUbiMP(oF14c.F14cNumMov, cRowID)				&& Realizar el cambio de ubicación.
	nCantidadRepos = nCantidadRepos - oF14c.F14cCanFis		&& Cantidad restante para procesar.

	Select F14cUpdRep
	Skip
EndDo

*> Eliminar el MP de reposición.
m.F14cNumMov = oF26l.F26lNumMov
=f3_baja("F14c")

*> Actualizar el estado de bloqueo de los MPs asociados.
This.ReposObj.SetBloqMP(cRowID, Space(1))			&& MPs y listas destino.
This.ReposObj.SetBloqMP(oF26l.F26lIdOcup, Space(1))	&& MPs y listas origen.
This.ReposObj.SetBloqOC(oF26l.F26lIdOcup)			&& Ocupación origen.

*> Si no hay MPs es necesario recalcular ocupación destino.
If !lHayMPs
	With This.RStkObj
		*> Ocupación origen.
		.Inicializar
		.RSDelE = "S"
		.RowID = oF26l.F26lIdOcup
		.Ejecutar('01')
	EndWith

	With This.RStkObj
		*> Ocupación destino.
		.Inicializar
		.RSDelE = "S"
		.RowID = cRowID
		.Ejecutar('01')
	EndWith
EndIf

Use In (Select("F14cUpdRep"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE reposobj_access

*> Crear objeto de la clase para el tratamiento de reposiciones.
If Type('This.ReposObj')<>'O'
	This.ReposObj = CreateObject('OraFncRepos')
EndIf

Return This.ReposObj

ENDPROC
PROCEDURE asignarmuelle

*> Asignar muelle por defecto.
*> Calcula la ubicación destino cuando genera los MPs de reserva, si ésta viene en blanco.
*> Método público, pero de uso privado de las clases.

*> Criterios:
*>	- Ubicación de expedición del documento de salida.
*>	- Ubicación asignada al transportista en entidades / ubicación (F01u).
*>	- Ubicación genérica de muelle.

*> Llamado desde:
*>	- OraFncResv.AsignarMuelle()
*>	- OraFncList._cnflstmpp()

*> Recibe:
*>	- Documento (Propietario, tipo y número).

*> Devuelve:
*>	- This.Muelle, ubicación de muelle.

Parameters cPropietario, cTipo, cDocumento

Private cWhere, cCodPro, cTipDoc, cNumDoc, cMuelle
Local lStado, oF24c

This.UsrError = ""
This.Muelle = ""

*> Asignar parámetros a variables.
If PCount()==3
	cCodPro = cPropietario
	cTipDoc = cTipo
	cNumDoc = cDocumento
Else
	cCodPro = Space(6)
	cTipDoc = Space(4)
	cNumDoc = Space(13)
EndIf

*> Primero, tomar la ubicación de expedición original del documento de salida.
m.F24cCodPro = cCodPro
m.F24cTipDoc = cTipDoc
m.F24cNumDoc = cNumDoc

If f3_seek("F24c")
	cMuelle = F24c.F24cUbiExp
	This.Muelle = ""

	*> Validar la ubicación por defecto, la del documento.
	If f3_seek("F10c", "[m.F10cCodUbi=cMuelle]")
		Select F10c
		Go Top
		If (F10cPickSN=="M" .Or. F10cPickSN=="E") .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
			This.Muelle = cMuelle
		EndIf
	EndIf

	*> Buscar la ubicación asignada al transportista.
	If Empty(This.Muelle)
		cWhere = 		  "F01uTipEnt='TRAN' And "
		cWhere = cWhere + "F01uCodEnt='" + F24c.F24cCodTra + "' And "
		cWhere = cWhere + "(F01uTipOri='M' Or F01uTipOri='E') And F01uActivo='S'"

		lStado = f3_sql("*", "F01u", cWhere, , , "F01uMUELLE")

		Select F01uMUELLE
		Go Top
		Do While !Eof()
			*> Validar la ubicación.
			m.F10cCodUbi = F01uUbiExp
			If f3_seek("F10c")
				Select F10c
				If (F10cPickSN=="M" .Or. F10cPickSN=="E") .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
					This.Muelle = F10cCodUbi
					Exit
				EndIf
			EndIf

			Select F01uMUELLE
			Skip
		EndDo
	EndIf

	*> Ubicación destino por defecto.
	If Empty(This.Muelle)
		*> Buscar una ubicación genérica de expedición.
		cWhere = "F10cPickSn='E' And F10cEstGen<>'B' And F10cEstEnt='N'"
		lStado = f3_sql("*", "F10c", cWhere, , , "F10cUbiDes")
		If lStado
			Select F10cUbiDes
			Go Top
			This.Muelle = F10cCodUbi
		EndIf
	EndIf

	*> Asignar el muelle, si cal, al documento de salida.
	If !Empty(This.Muelle) .And. cMuelle<>This.Muelle
		Select F24c
		Replace F24cUbiExp With This.Muelle
		Scatter MemVar
		=f3_upd("F24c")
	EndIf
EndIf

lStado = !Empty(This.Muelle)
If !lStado
	This.UsrError = "No hay ubicación destino"
	lStado = .F.
EndIf

Use In (Select("F10cUbiDes"))
Use In (Select("F01uMUELLE"))
Return lStado

ENDPROC
PROCEDURE validarreposicion

*> Validar si un MP está pendiente de reposiciones.

*> Recibe:
*>	- cRowID, ID ocupación del MP a validar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cRowID

Private cWhere
Local lStado

This.UsrError = ""

cWhere = "F14cIdOcup='" + cRowID + "' And " + _GCSS("F14cTipMov", 1, 1) + "='3'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cRepos")

Use In (Select ("F14cREPOS"))
Return lStado

ENDPROC
PROCEDURE bloquearlstmp
	
*> Bloquear un MP de una lista de trabajo.
*> Cancela el MP de la lista (si hay), desreserva y pasa el stock de la ocupación a situación de bloqueado.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovMP, nº de MP a confirmar.
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.TMovOrg, Tipo movimiento salida disponible para HM (usualmente 4500).
*>	- This.TMovDst, Tipo movimiento entrada bloqueado para HM (usualmente 4000).
*>	- This.StkBlq, Situación de stock de bloqueado (usualmente 9000).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cNumeroMovimiento

Private cWhere, cNumMov, oF14c
Local lStado

This.UsrError = ""

=This.InitLocals()

*> Asignar propiedades a variables de trabajo.
cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovMP)

*> Obtener el MP que generó la incidencia.
m.F14cNumMov = cNumMov
If !f3_seek("F14c")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F14c
Scatter Name oF14c

*> A partir del MP, obtener la ID de la ocupación a bloquear.
lStado = This.BloquearLstOC(oF14c.F14cIdOcup)

Return lStado

ENDPROC
PROCEDURE bloquearlstoc
	
*> Bloquear una ocupación del almacén.
*> Cancela el MP de la lista (si hay), desreserva y pasa el stock de la ocupación a situación de bloqueado.
*> Trata tanto MPs de preparación como de reposición.

*> Recibe:
*>	- cIdOcup ó bien This.RowID, ID de la ocupación a bloquear.
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.TMovOrg, tipo movimiento salida disponible para HM (usualmente 3500).
*>	- This.TMovDst, tipo movimiento entrada bloqueado para HM (usualmente 3000).
*>	- This.StkBlq, situación de stock de bloqueado (usualmente 9000).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.nCurDoc, nº de documentos cancelados.
*>	- This.cDocCnc, Array con los documentos cancelados.
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.BloquearLstMP, bloquear ocupación a partir de un MP.

*> Revisiones:
*>	AVC - 21.07.2006 Corregir nº movimiento al generar histórico de entrada en destino.

Parameters cIdOcup

Private cWhere, cOcup, oF14c, oF26l, oF16c
Private cCodPro, cTipDoc, cNumDoc, cLinDoc
Local lStado, lStato, cCurDoc, nInx

This.UsrError = ""
This.nCurDoc = 0
cCurDoc = ""

=This.InitLocals()

*> Asignar priopiedades a variables de trabajo.
cOcup = Iif(PCount()==1, cIdOcup, This.RowID)

*> Validar tipo de movimiento a generar en histórico.
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Validar la situación de stock bloqueado.
m.F00cCodStk = This.StkBlq
If !f3_seek('F00c')
	This.UsrError = "Situación stock bloqueo: " + This.StkBlq + " no existe"
	Return .F.
EndIf


*> Obtener todos los de la misma ID ocupación para su cancelación.
*cWhere = "F14cIdOcup='" + cOcup + "'"

*> LRC. 6.2.2009. Si tiene MAC no se debe anular la reserva.
cWhere = "F14cIdOcup='" + cOcup + "' And F14cNumMAC=' '"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cCncMP")

If !lStado
	If _xier <= 0
		This.UsrError = "Error carga bloqueo MP"
		Return lStado
	EndIf
EndIf

*> Cancelación de los MPs obtenidos.
Select F14cCncMP
Go Top
Do While !Eof()
	*> Con el MP, obtener la lista.
	Scatter Name oF14c
	m.F26lNumMov = oF14c.F14cNumMov
	If f3_seek("F26l")
		*> Eliminar el movimiento de la lista.
		Select F26l
		Scatter Name oF26l
		If oF26l.F26lEstMov=='0'
			With This
				.NumLst = oF26l.F26lNumLst
				lStado = .BorrarListaMP(oF26l.F26lNumMov)
			EndWith
			If !lStado
				Use In (Select ("F14cCncMP"))
				Return lStado
			EndIf
		EndIf
	EndIf

	*> Si el MP no es de preparación, ignorar la desreserva / nueva reserva y pasar al siguiente.
	If !Between(oF14c.F14cTipMov, '2000', '2999')
		Select F14cCncMP
		Skip
		Loop
	EndIf

	*> Desreservar el MP.
	With This.ResvObj
		.ObjParm = This.ResvPrm

		With .ObjParm
			.Inicializar
		EndWith

		lStado = .DesReserva(oF14c.F14cNumMov)
		If lStado
			lStado = .UpdDocRsv(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)
		EndIf
	EndWith

	If !lStado
		This.UsrError = This.ResvObj.UsrError
		Use In (Select ("F14cCncMP"))
		Return lStado
	EndIf

	*> Guardar en la lista de documentos para volver a reservar.
	cCurDoc = oF14c.F14cCodPro + oF14c.F14cTipDoc + oF14c.F14cNumDoc + PadL(AllTrim(Str(oF14c.F14cLinDoc)), 4, '0')
	lStato = .F.
	For nInx = 1 To This.nCurDoc
		If This.cDocCnc[nInx]==cCurDoc
			lStato = .T.
			Exit
		EndIf
	EndFor

	If !lStato
		This.nCurDoc = This.nCurDoc + 1
		Dimension This.cDocCnc[This.nCurDoc]
		This.cDocCnc[This.nCurDoc] = cCurDoc
	EndIf

	*> Procesar siguiente MP de la ocupación a bloquear.
	Select F14cCncMP
	Skip
EndDo

*> Obtener la ocupación a bloquear.
m.F16cIdOcup = cOcup
If !f3_seek("F16c")
	This.UsrError = "No existe ocupación"
	Use In (Select ("F14cCncMP"))
	Return .F.
EndIf

*> Cambiar la ocupación a stock bloqueado.
Select F16c
Scatter Name oF16c
This.ActzObj.ObjParm = This.ActzPrm

With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('O', oF16c)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF16c.F16cCanFis - oF16c.F16cCanRes
	.PoCRes = 0
	.PoRowID= oF16c.F16cIdOcup
	
	.PsTOld = oF16c.F16cSitStk
	.PsTNew = This.StkBlq

	.PtAcc  = '10'
EndWith

This.ActzObj.Ejecutar

If This.ActzObj.ObjParm.PwCrtn >= '50'
	This.UsrError = "Error al bloquear movimiento"
	Use In (Select ("F14cCncMP"))
	Return .F.
EndIf

With This.ActzObj
	.ObjParm.PoSStk = oF16c.F16cSitStk
	.ObjParm.PmEnSa = "S"
	.ObjParm.PmTMov = This.TMovOrg
	.ActHM				&& Salida origen disponible.

	.ObjParm.PmNMov = ""
	.ObjParm.PoSStk = This.StkBlq
	.ObjParm.PmEnSa = "E"
	.ObjParm.PmTMov = This.TMovDst
	.ActHM				&& Entrada destino bloqueado.
EndWith

Use In (Select ("F14cCncMP"))

Return lStado

ENDPROC
PROCEDURE _actualizarestadodocumento
*> Actualizar el estado de preparación de un documento.
*> Actualiza a estado '4' la cabecera, en el caso de que esté completamente preparado.
*> Método privado de la clase.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento, documento a validar estado.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This._cnflstmpp, actualizar MP de preparación.

*> Revisiones:
*>	AVC - 12.11.2006 Enlace con tareas BATCH.

Parameters cCodPro, cTipDoc, cNumDoc

Local lStado, lLib
Private cWhere, cCampos, cValores, Private cEstado
Private cParams

Store "" To This.UsrError
Store .T. To lStado

This.DocumentoCerrado = 'N'

cWhere =          "F14cCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F14cTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F14cNumDoc='" + cNumDoc + "' And "
cWhere = cWhere + "F14cTipMov Between '2000' And '2998'"

lStado = f3_sql("*", "F14c", cWhere, , , "F14cPdtLst")
If _xier <= 0
	This.UsrError = "Error al cargar los MPs estado preparación"
	lStado = .F.
	Return lStado
EndIf

If !lStado
	*> No hay nada pendiente de preparar.
	cWhere =          "F24cCodPro='" + cCodPro + "' And "
	cWhere = cWhere + "F24cTipDoc='" + cTipDoc + "' And "
	cWhere = cWhere + "F24cNumDoc='" + cNumDoc + "'"

	cCampos = "F24cFlgEst"
	cEstado = "4"
	cValores = "cEstado"
	=f3_updtun("F24c", , cCampos, cValores, , cWhere)

	*> Generar el albarán del pedido finalizado.
	lStado = This._generaralbaran(cCodPro, cTipDoc, cNumDoc)

	*> Aviso a Main de pedido cerrado.
	This.DocumentoCerrado = 'S'

	*> Lanzar proceso de tareas en BATCH.
	This._proceso = "CPS2000"
EndIf

Use In (Select("F14cPdtLst"))
Return lStado

ENDPROC
PROCEDURE _generaralbaran

*> Generar el albarán de un pedido, una vez confirmado.
*> Método privado de la clase.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento, documento a generar albarán.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This._actualizarestadodocumento() , actualizar estado cabecera del pedido.

*> Historial de modificaciones:
*>	- Incorporar nueva generación de albaranes. AVC - 13.06.2007

Parameters cCodPro, cTipDoc, cNumDoc

Local lStado

*> Realizar la generación del albarán de salida. <<<<<<< OLD METHOD >>>>>>>>
*!*	With This.ActzObj
*!*		With .ObjParm
*!*			.Inicializar

*!*			.PACtrG = Space(1)		&& Separar estupefacientes de ambiente-frío.
*!*			.PAFrio = Space(4)		&& Productos de frío.
*!*			.PAFriG = Space(4)		&& Productos de frío/estupefacientes.
*!*			.PAAlbN = "N"			&& Renumerar albaranes.
*!*			.PAFAlb = Date()
*!*			.PAHEnt = Space(8)
*!*			.PAHSal = Space(8)
*!*			.PANAlb = Space(13)
*!*			.POCPro = cCodPro
*!*			.POTDoc = cTipDoc
*!*			.PONDoc = cNumDoc
*!*		EndWith

*!*		.ActzAS
*!*	EndWith

*> Generar el albarán de salida. <<<<<<< NEW METHOD >>>>>>>>
With This.AlbSObj
	.Inicializar

	.CodPro = cCodPro
	.TipDoc = cTipDoc
	.NumDoc = cNumDoc

	lStado = .AlbaranDocumento()
EndWith

Return

ENDPROC
PROCEDURE taskobj_access

*> Crear objeto de la clase para tareas BATCH.
If Type('This.TaskObj')<>'O'
	This.TaskObj = CreateObject('batch')
	This.TaskObj.Inicializar
EndIf

Return This.TaskObj

ENDPROC
PROCEDURE _proceso_assign

*> Control dinámico de procesos BATCH.

*> Recibe:
*>	- Nuevo valor de la propiedad.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensajes de error.

*> Llamado desde:
*>	- This._actualizarestadodocumento()

LParameters vNewVal

Private cParams
Local lStado

If !This.lProBatch
	Return
EndIf

Do Case
	*> Cierre documentos (This._actualizarestadodocumento)
	*> cCodPro/cTipDoc/cNumDoc, Documento activo; _UsrCod, usuario de PROCAOT.
	Case vNewVal=='CPS2000'
		*cParams = "PROPIETARIO='" + cCodPro + "', TIPODOCUMENTO='" + cTipDoc + "', DOCUMENTO='" + cNumDoc  + "', ALBARAN='" + This.AlbSObj.NumAlb + "'"
		cParams = "PROPIETARIO='" + cCodPro + "', TIPODOCUMENTO='" + cTipDoc + "', DOCUMENTO='" + cNumDoc + "'"
		lStado = This.TaskObj.Execute('CPS2000', _UsrCod, cParams)
		If !lStado
			This.UsrError = This.TaskObj.UsrError
		EndIf

	*> Proceso no implementado.
	Otherwise
EndCase

This._proceso = vNewVal
Return lStado

ENDPROC
PROCEDURE cnfcrgdoc

*> Confirmar la carga de un documento de salida.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento, documento a confirmar.
*>	- This.TMovExp, tipo movimiento expedición (generalmente 2999).
*>	- This.UpdDoc, actualizar estado documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:

*> Historial de modificaciones:
*> 17.12.2007 (LRC) Adaptar a tratamiento de partes de montaje.

Parameters cCodPro, cTipDoc, cNumDoc

Local lStado
Private cWhere, oF14c, x_Where, Estado, cValores

Store "" To This.UsrError
Store .T. To lStado

cWhere =          "F14cCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F14cTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F14cNumDoc='" + cNumDoc + "' And "
cWhere = cWhere + "F14cTipMov Between '2000' And '2999'"

lStado = f3_sql("*", "F14c", cWhere, , , "F14cCnfDoc")
If !lStado
	If _xier <= 0
		This.UsrError = "Error al cargar los MPs a confirmar"
	Else
		This.UsrError = "No hay datos para procesar"
	EndIf

	Use In (Select ("F14cCnfDoc"))
	lStado = .F.
	Return lStado
EndIf

Select F14cCnfDoc
Locate For F14cTipMov<>This.TMovExp
If Found()
	This.UsrError = "Hay MPs pendientes de preparar"

	Use In (Select ("F14cCnfDoc"))
	lStado = .F.
	Return lStado
EndIf

Go Top
Do While !Eof()
	Scatter Name oF14c
	lStado = This.CnfCrgMP(oF14c.F14cNumMov)
	If !lStado
		*> Error al confirmar un MP. El mensaje ya viene asignado.
		Exit
	EndIf

	Select F14cCnfDoc
	Skip
EndDo

*!*	*> -------------------------------------------------------------
*!*	*> LRC. 17/12/2007. Actualizar la ubicación de taller de todas las ocupaciones del parte de montaje procesado. Se identifica por el lote que
*!*	*> contiene el numero de parte.
*!*	If cTipDoc = TDocSalida
*!*		*> Recuperar la ubicación de la entidad.
*!*		m.F24cCodPro = cCodPro
*!*		m.F24cTipDoc = cTipDoc
*!*		m.F24cNumDoc = cNumDoc
*!*		If f3_seek("F24c")
*!*			cValores = "F24c.F24cUbiExp"
*!*			x_Where =                "F16cCodPro = '" + cCodPro + "'"
*!*			x_Where = x_Where + " And F16cNumLot = '" + cNumDoc + "'"
*!*			lStado = F3_UpdTun('F16c', , 'F16cCodUbi', cValores, , x_Where, 'N')
*!*		EndIf
*!*	EndIf
*!*	*> -------------------------------------------------------------

*> -------------------------------------------------------------
*> LRC. 17/12/2007. Actualizar cabecera de documento.----------------------------
If cTipDoc = TDocSalida
	Estado = '0'

	x_Where =                "F18cCodPro = '" + cCodPro     + "'"
	x_Where = x_Where + " And F18cTipDoc = '" + TDocEntrada + "'"
	x_Where = x_Where + " And F18cNumDoc = '" + cNumDoc     + "'"

	lStado = F3_UpdTun('F18c', , 'F18cEstado', 'Estado', , x_Where, 'N')
EndIf
*> -------------------------------------------------------------

Use In (Select("F14cCnfDoc"))
Return lStado

ENDPROC
PROCEDURE albsobj_access

*> Crear objeto de la clase para la generación de albaranes.
If Type('This.AlbSObj')<>'O'
	This.AlbSObj = CreateObject('OraFncAlbS')
EndIf

Return This.AlbSObj

ENDPROC
PROCEDURE ___historial__de__modificaciones___

*> Historial de modificaciones:

*> 12.11.2006 (AVC) Enlace con tareas BATCH.
*> 13.06.2007 (AVC) (CnfCrgMP) Cambiar el cálculo del estado del documento.
*> 13.06.2007 (AVC) (_generaralbaran) Permitir expediciones parciales.
*> 14.06.2007 (AVC) (CnfCrgMP) Permitir expediciones parciales.
*> 14.06.2007 (AVC) (CncCrgMP) Permitir expediciones parciales.
*> 14.06.2007 (AVC) (CncCrgLstMP) Permitir expediciones parciales.
*> 02.08.2007 (AVC) (ChgUbiId) Cambiar ubicación origen por ID. Llamará a ChgUbiMp. Método nuevo.
*> 05.11.2007 (AVC) (_GenLstCab) Incorporar flag de agrupación de documentos en lista.
*> 03.12.2007 (AVC) Adecuar a tratamiento de partes de montaje.
*>					Modificado método This.CnfCrgMP, confirmar carga de UN movimiento Pendiente.
*> 17.12.2007 (LRC) Agregar funcionalidades en tratamiento de partes de montaje.
*>					Modificado método This.CnfCrgMP
*>					Modificado método This.CnfCrgDoc
*> 07.01.2008 (LRC) Ajustar cantidades al confirmar reposiciones.
*>					Modificado método This.ChgUbiMP
*> 30.01.2008 (AVC) Corregir error descarga biblioteca BATCH.
*>					Anulado método This.Destroy
*> 07.02.2008 (AVC) Corregir tratamiento de división de MPs de preparación.
*>					Modificado método This.Inicializar
*> 06.05.2008 (AVC) Agregar tratamiento de listas de carga.
*>					Agregado método This.GenLsCDoc, agregar un documento a lista de carga.
*>					Agregado método This.DelLsCDoc, quitar un documento a lista de carga.
*>					Agregado método This.UpdLsCCab, Actualizar datos de la cabecera de la lista de carga.
*>					Agregado método This.ActualizarListaC, actualizar estado de la lista de carga.
*>					Agregado método This._genlsccab, generar cabecera de lista de carga.
*>					Agregado método This._genlscdet, agregar documento a línea de lista de carga.
*>					Agregada Propiedad This.CurLsC, lista carga activa.
*>					Agregada Propiedad This.NumLsC, lista carga generada.
*>					Agregada Propiedad This.UpdLsC, actualizar estgado lista carga.
*>					Agregada Propiedad This.AgrTrp, generar listas por transportista.
*>					Agregada Propiedad This.IdentD, identificación destino.
*>					Modificado método This.Inicializar
*>					Modificado método This.InitLocals
*> 13.05.2008 (AVC) En Confirmación de carga, validar si un documento está en lista de carga y darlo de baja.
*>					En Cancelación de carga, validar si un documento está en lista de carga y darlo de baja.
*>					Modificado método This.CnfCrgMP, confirmar carga
*>					Modificado método This.CncCrgMP, cancelar carga
*>					Modificado método This.CncCrgLstMP, desasignar carga
*> 13.05.2008 (AVC) Agregar confirmación de carga de un MAC.
*>					Agregado método This.CnfCrgMAC
*>					Agregado método This.ActualizarListaCD, actualizar estado línea de la lista de carga.

ENDPROC
PROCEDURE chgubiid

*> Cambiar la ubicación origen a los MPs de una ID ocupación.

*> Recibe:
*>	- cID ó bien This.RowID, ID de la ocupación actual.
*>	- cIdNew ó bien This.RowIDNew, identificador ocupación destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cIdOrigen, cIdDestino

Private cId, cIdNew, cWhere, oF14c
Local lStado

This.UsrError = ""

*> Asignar parámetros / propiedades a variables de trabajo.
cId = Iif(Type('cIdOrigen')=='C', cIdOrigen, This.RowID)
cIdNew = Iif(Type('cIdDestino')=='C', cIdDestino, This.RowIDNew)

cWhere = "F14cIdOcup='" + cId + "'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cIDOCUP")

Select F14cIDOCUP
Go Top
Do While !Eof()
	Scatter Name oF14c

	lStado = This.ChgUbiMP(oF14c.F14cNumMov, cIdDestino)

	Select F14cIDOCUP
	Skip
EndDo

Use In (Select ("F14cIDOCUP"))
Return lStado

ENDPROC
PROCEDURE ctrlrepblq

*> Valida si el palet leido es un candidato valido para hacer el swapping en las reposiciones de bloque

*> Recibe:
*>	- cID ó bien This.RowID, ID de la ocupación actual.
*>	- cIdNew ó bien This.RowIDNew, identificador ocupación destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cNumMov, cNumPal

Private cId, cIdNew, cWhere, oF14c, oF16cOrg, oF16cDst, oF25c
Local lStado

This.UsrError = ""

*!*	*> Asignar parámetros / propiedades a variables de trabajo.
*!*	cId = Iif(Type('cIdOrigen')=='C', cIdOrigen, This.RowID)
*!*	cIdNew = Iif(Type('cIdDestino')=='C', cIdDestino, This.RowIDNew)

*> --------------------------------------------------------------------------------------------------
*> Recupero el movimiento pendiente de reposición.
cWhere =          "     F14cNumMov='" + cNumMov + "'"
cWhere = cWhere + " And F10cCodUbi=F14cUbiOri"
cWhere = cWhere + " And F08cCodPro=F14cCodPro And F08cCodArt=F14cCodArt"
If !f3_sql("*", "F14c,F10c,F08c", cWhere, , , "F14cMP")
	This.UsrError = "No existe el movimiento (MP)"
	Return .F.
EndIf

Select F14cMP
Scatter Name oF14c
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Si el almacenaje de la ubicación es convencional no se permite intercambiar palets.
*> F10cTipAlm = 'C' -> Convencional
*>              'M' -> Compacto
*>              'M' -> Masivo

If cNumPal == oF14c.F14cNumPal
	Return .T.
Else
	If oF14c.F10cTipAlm=='C'	&& Convencional
		This.UsrError = "El palet debe ser igual"
		Return .F.
	EndIf
EndIf
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Recupero información del palet origen.
cWhere = "F16cIdOcup='" + oF14c.F14cIdOcup + "'"
If !f3_sql("*", "F16c", cWhere, , , "F16cOrg")
	This.UsrError = "No existe el palet origen (OC)"
	Return .F.
EndIf

Select F16cOrg
Scatter Name oF16cOrg
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Recupero los parametros de reposición.
cWhere = "F25cCodPro='" + oF14c.F14cCodPro + "' And F25cCodArt='" + oF14c.F14cCodArt + "'"
If !f3_sql("*", "F25c", cWhere, , , "F25cCur")
	cWhere = "F25cCodPro='" + oF14c.F14cCodPro + "' And F25cTipPro='" + oF14c.F08cTipPro + "'"
	If !f3_sql("*", "F25c", cWhere, , , "F25cCur")
		cWhere = "F25cCodPro='" + oF14c.F14cCodPro + "'"
		If !f3_sql("*", "F25c", cWhere, , , "F25cCur")
			This.UsrError = "Parametros no encontrados"
			Return .F.
		EndIf
	EndIf
EndIf

Select F25cCur
Scatter Name oF25c
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Recupero información del palet leido.
cWhere =               "F16cNumPal='" + cNumPal             + "'"
cWhere = cWhere + " And F16cCodUbi='" + oF16cOrg.F16cCodUbi + "'"
cWhere = cWhere + " And F16cCodPro='" + oF16cOrg.F16cCodPro + "'"
cWhere = cWhere + " And F16cCodArt='" + oF16cOrg.F16cCodArt + "'"
cWhere = cWhere + " And F16cSitStk='" + oF16cOrg.F16cSitStk + "'"

*> Aplico la parametrización.
If oF25c.F25cRepLot=='S'
	cWhere = cWhere + " And F16cNumLot='" + oF16cOrg.F16cNumLot + "'"
EndIf

If oF25c.F25cRepCad=='S'
	cWhere = cWhere + " And F16cFecCad= " + _GCD(oF16cOrg.F16cFecCad)
EndIf

If oF25c.F25cRepQty=='S'
	cWhere = cWhere + " And F16cCanFis=" + Str(oF16cOrg.F16cCanFis)
EndIf

If !f3_sql("*", "F16c", cWhere, , , "F16cDst")
	This.UsrError = "No existe el palet destino (OC)"	&& Palet con las características necesarias no encontrado.
	Return .F.
EndIf

Select F16cDst
Scatter Name oF16cDst
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Si se permite cantidades diferentes, analizar la cantidad reservada.
If oF25c.F25cRepQty<>'S'
	If oF16cDst.F16cCanRes > oF16cOrg.F16cCanFis or oF16cOrg.F16cCanRes > oF16cDst.F16cCanFis
		This.UsrError = "Reservas no intercambiables"
		Return .F.
	EndIf
EndIf
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Obtengo los cursores de trabajo con los MP a actualizar. Se recuperan ambos antes de procesarlos por que sino
*> los MP modificados se selecionarían en el segundo cursor.
cWhere = "F14cIdOcup='" + oF16cOrg.F16cIdOcup + "'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cOrigen")

cWhere = "F14cIdOcup='" + oF16cDst.F16cIdOcup + "'"
lStado = f3_sql("*", "F14c", cWhere, , , "F14cDestino")

*> --------------------------------------------------------------------------------------------------
*> Traspaso las reservas originales a destino
Select F14cOrigen
Go Top
Do While !Eof()
	Scatter Name oF14c

	lStado = This.ChgUbiMP(oF14c.F14cNumMov, oF16cDst.F16cIdOcup)

	Select F14cOrigen
	Skip
EndDo
Use In (Select ("F14cOrigen"))
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Traspaso las reservas del palet leido al origen.

Select F14cDestino
Go Top
Do While !Eof()
	Scatter Name oF14c

	lStado = This.ChgUbiMP(oF14c.F14cNumMov, oF16cOrg.F16cIdOcup)

	Select F14cDestino
	Skip
EndDo
Use In (Select ("F14cDestino"))
*> --------------------------------------------------------------------------------------------------

Use In (Select ("F14cMP"))
Use In (Select ("F25cCur"))
Use In (Select ("F16cOrg"))
Use In (Select ("F16cDst"))

Return .T.

ENDPROC
PROCEDURE genlscdoc

*> Agregar un documento de salida a una lista de carga.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento ó bien This.CodPro, This.TipDoc, This.NumDoc
*>	- This.UpdLsC, Actualizar estado lista (S/N).
*>	- This.NumLsC, Lista a la que se añade el MP. Se crea si blancos.
*>	- This.AgrTrp, generar lista por transportista (Def=N)
*>	- This.IdentD, etiqueta identificación destino (Def=N)

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLsC, Lista generada, si no existía.
*>	- This.UsrError, Texto errores.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creación.

Parameters cPropietario, cTipo, cDocumento
Private cCodPro, cTipDoc, cNumDoc
Private cWhere, cLista
Local lStado

This.UsrError = ""

=This.InitLocals()

*> Asignar parámetros a propiedades.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cTipDoc = Iif(Type('cTipo')=='C', cTipo, This.TipDoc)
cNumDoc = Iif(Type('cDocumento')=='C', cDocumento, This.NumDoc)

*> Validar el documento.
If ! f3_seek("F24c", '[m.F24cCodPro=cCodPro,m.F24cTipDoc=cTipDoc,m.F24cNumDoc=cNumDoc]')
	This.UsrError = "No existe el documento: " + cNumDoc
	Return .F.
EndIf

*> Validar si el documento ya está en una lista.
cWhere = "F80lCodPro='" + cCodPro + "' And F80lTipDoc='" + cTipDoc + "' And F80lNumDoc='" + cNumDoc + "'"
lStado = f3_sql("*", "F80l", cWhere, , , "__F80LLCRC")

Use In (Select ("__F80LLCRC"))

If lStado
	This.UsrError = "El documento: " + cNumDoc + " ya está en lista"
	Return .F.
EndIf

*> Guardar lista en una propiedad.
This.CurLsC = This.NumLsC

*> Crear la cabecera de la lista, si cal.
If Empty(This.CurLsC)
	lStado = This._genlsccab()
EndIf

*> Crear la línea de detalle de la lista.
lStado = This._genlscdet()

*> Actualizar el estado de la lista.
lStado = This.ActualizarListaC()

Return lStado

ENDPROC
PROCEDURE _genlsccab

*> Creación de la cabecera de la lista de carga.
*> Método protegido, de uso interno.

*> Recibe:
*>	- This.CurLsC, lista actual.
*>	- This.AgrTrp, lista por transportista (S/N)
*>	- This.IdentD, identificación destino (S/N)

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLsC, lista actual.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creación.

Local lStado, lIsOpenF80c

lIsOpenF80c = Used("F80cUpd")
If !lIsOpenF80c
	=CrtCursor("F80c", "F80cUpd")
EndIf

If Empty(This.CurLsC)
	This.CurLsC = Ora_NewLCC()
EndIf

This.NumLsC = This.CurLsC

Select F80cUpd
Delete All
Append Blank

Replace F80cNumLst With This.CurLsC
Replace F80cFecCre With Date()
Replace F80cFecCnf With _FecMin
Replace F80cAgrTrp With This.AgrTrp
Replace F80cIdentD With This.IdentD
Replace F80cEstLst With "0"

lStado = f3_instun('F80c', 'F80cUpd', 'N')
If !lStado
	This.UsrError = "No se puede insertar cabecera lista de carga " + This.CurLsC
EndIf

If !lIsOpenF80c
	Use In F80cUpd
EndIf

Return lStado

ENDPROC
PROCEDURE _genlscdet

*> Creación de una línea de detalle de la lista de carga..
*> Método protegido, de uso interno de la clase.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento ó bien This.CodPro, This.TipDoc, This.NumDoc
*>	- This.CurLsC, lista de carga actual.

*> Devuelve:
*>	- Estado (.T. / .F.)

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creación.

Parameters cPropietario, cTipo, cDocumento

Private cWhere
Private cCodPro, cTipDoc, cNumDoc
Local lStado, lIsOpenF80l

*> Asignar parámetros a propiedades.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cTipDoc = Iif(Type('cTipo')=='C', cTipo, This.TipDoc)
cNumDoc = Iif(Type('cDocumento')=='C', cDocumento, This.NumDoc)

lIsOpenF80l = Used("F80lUpd")
If !lIsOpenF80l
	=CrtCursor("F80l", "F80lUpd")
EndIf

*> Preparar datos para insertar registro.
Select F80lUpd
Delete All
Append Blank

Replace F80lNumLst With This.CurLsC
Replace F80lCodPro With cCodPro
Replace F80lTipDoc With cTipDoc
Replace F80lNumDoc With cNumDoc
Replace F80lEstMov With "0"
Replace F80lOrdRec With 0

lStado = f3_instun('F80l', 'F80lUpd', 'N')
If !lStado
	*> Error.
	This.UsrError = "No se puede insertar línea lista de carga " + This.CurLsC
	If !lIsOpenF80l
		Use In F80lUpd
	EndIf
	Return lStado
Endif

If !lIsOpenF80l
	Use In F80lUpd
EndIf

Return lStado

ENDPROC
PROCEDURE actualizarlistac

*> Actualizar el estado de una lista de trabajo.

*> Recibe:
*>	- cNumeroListaC ó bien This.NumLsC, lista de carga a actualizar.
*>	- This.UpdLsC, actualizar estado lista carga (S/N).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creación.

Parameters cNumeroListaC

Private cNumLsC, cWhere
Local lStado, nTotMov, nTotMovP

If This.UpdLsC<>'S'
	Return
EndIf

lStado = .T.
This.UsrError = ""

*> Tomar la lista recibida, bien como parámetro o bien como propiedad.
cNumLsC = Iif(PCount()==1, cNumeroListaC, This.NumLsC)

*> Validar los datos recibidos.
If Empty(cNumLsC)
	*> Lista vacía.
	This.UsrError = "La lista de carga está vacía"
	Return .F.
EndIf

m.F80cNumLst = cNumLsC
If !f3_seek("F80c")
	*> No existe.
	This.UsrError = "La lista de carga: " + cNumLsC + " no existe"
	Return .F.
EndIf

*> Cursores de trabajo para borrado de listas.
=CrtCursor("F80l", "F80lEst")

*> Cargar datos de detalle de la lista de carga.
cWhere = "F80lNumLst='" + cNumLsC + "'"
=f3_sql("*", "F80l", cWhere, , , "F80lEst")

Select F80lEst

*> Nº de movimientos de la lista.
Count To nTotMov							&& Totales.
Count To nTotMovP For F80lEstMov=='0'		&& Pendientes.

*> Actualizar datos en la cabecera de la lista.
Select F80c

Replace F80cEstLst With Iif(nTotMovP==0, '3', Iif(nTotMovP < nTotMov, '1', '0'))

Scatter MemVar

m.F80cNumLst = cNumLsC
If nTotMov <= 0
	=f3_baja("F80c")
Else
	=f3_upd("F80c")
EndIf

Use In (Select("F80lEst"))

Return

ENDPROC
PROCEDURE dellscdoc

*> Borrar un documento de salida de una lista de carga.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento ó bien This.CodPro, This.TipDoc, This.NumDoc
*>	- This.UpdLsC, actualizar estado lista de carga (S/N).
*>	- This.NumLsC, lista a actualizar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cPropietario, cTipo, cDocumento

Private cCodPro, cTipDoc, cNumDoc, cWhere
Local lStado, oF80l

This.UsrError = ""

*> Asignar parámetros a propiedades.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cTipDoc = Iif(Type('cTipo')=='C', cTipo, This.TipDoc)
cNumDoc = Iif(Type('cDocumento')=='C', cDocumento, This.NumDoc)

*> Obtener la lista de carga a la que pertenece el documento.
cWhere = "F80lCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F80lTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F80lNumDoc='" + cNumDoc + "'"

lStado = f3_sql("*", "F80l", cWhere, , , "__F80LDELLSC")
If !lStado
	Use In (Select ("__F80LDELLSC"))
	Return
EndIf

*> Quitar el documento de la lista de carga.
Select __F80LDELLSC
Go Top
Scatter Name oF80l
Use In (Select ("__F80LDELLSC"))

m.F80lCodPro = cCodPro
m.F80lTipDoc = cTipDoc
m.F80lNumDoc = cNumDoc

=f3_baja("F80l")

*> Actualizar, si cal, la cabecera de la lista de carga.
This.ActualizarListaC(oF80l.F80lNumLst)

Return

ENDPROC
PROCEDURE updlsccab

*> Actualización de la cabecera de la lista de carga.

*> Recibe:
*>	- This.CurLsC, lista actual.
*>	- This.AgrTrp, lista por transportista (S/N)
*>	- This.IdentD, identificación destino (S/N)

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumLsC, lista actual.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creación.

Local lStado
Private cWhere

This.UsrError = ""
lStado = .T.

If Empty(This.NumLsC)
	This.UsrError = "La lista de carga está en blanco"
	Return .F.
EndIf

m.F80cNumLst = This.NumLsC
If !f3_seek("F80c")
	This.UsrError = "No existe la lista de carga: " + This.NumLsC
	Return .F.
EndIf

Select F80c
Replace F80cAgrTrp With This.AgrTrp
Replace F80cIdentD With This.IdentD

cWhere = "F80cNumLst='" + This.NumLsC + "'"
lStado = f3_updtun("F80c", , , , , cWhere)
If !lStado
	This.UsrError = "No se puede actualizar cabecera lista de carga " + This.NumLsC
EndIf

Return lStado

ENDPROC
PROCEDURE cnfcrgmac

*> Confirmar la carga de un MAC.

*> Recibe:
*>	- cMAC, MAC a conformar.
*>	- This.TMovExp, tipo movimiento expedición (generalmente 2999).
*>	- This.UpdDoc, actualizar estado documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 13.05.2008 (AVC) Creación.

Parameters cMAC

Local lStado
Private cWhere, oF14c

Store "" To This.UsrError
Store .T. To lStado

If Empty(cMAC)
	This.UsrError = "MAC en blanco"
	Return .F.
EndIf

cWhere =          "F14cNumMac='" + cMAC + "' And "
cWhere = cWhere + "F14cTipMov Between '2000' And '2999'"

lStado = f3_sql("*", "F14c", cWhere, , , "F14cCnfMac")
If !lStado
	If _xier <= 0
		This.UsrError = "Error al cargar los MPs a confirmar"
	Else
		This.UsrError = "No hay datos para procesar"
	EndIf

	Use In (Select ("F14cCnfMac"))
	Return lStado
EndIf

Select F14cCnfMac
Locate For F14cTipMov<>This.TMovExp
If Found()
	This.UsrError = "Hay MPs pendientes de preparar"

	Use In (Select ("F14cCnfMac"))
	lStado = .F.
	Return lStado
EndIf

Go Top
Do While !Eof()
	Scatter Name oF14c
	lStado = This.CnfCrgMP(oF14c.F14cNumMov)
	If !lStado
		*> Error al confirmar un MP. El mensaje ya viene asignado.
		Exit
	EndIf

	Select F14cCnfMac
	Skip
EndDo

Use In (Select("F14cCnfMac"))
Return lStado

ENDPROC
PROCEDURE actualizarlistacd

*> Actualizar el estado de una línea de una lista de trabajo.
*> Si el documento está totalmente confirmado, se elimina de la lista.

*> Recibe:
*>	- cCodPro, cTipDoc, cNumDoc ó bien
*>	  This.NumLsC, This.CodPro, This.TipDoc, This.numDoc, documento / lista de carga a actualizar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 13.05.2008 (AVC) Creación.

Parameters cPropietario, cTipo, cDocumento

Private cCodPro, cTipDoc, cNumDoc
Private cWhere, cNumLsC
Local lStado

lStado = .T.
This.UsrError = ""

*> Pasar valores a propiedades.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cTipDoc = Iif(Type('cTipo')=='C', cTipo, This.TipDoc)
cNumDoc = Iif(Type('cDocumento')=='C', cDocumento, This.NumDoc)

cWhere = 		  "F80lCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F80lTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F80lNumDoc='" + cNumDoc + "'"

If !f3_sql("*", "F80l", cWhere, , , "F80l")
	*> No existe.
	*This.UsrError = "La línea no existe"
	
	*>PARA QUE DE MOMENTO NO SALGA EL ODIOSO MENSAJE
	Return .T.
EndIf

*> Cargar los MPs pendientes de confirmación de carga.
cWhere = 		  "F14cCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F14cTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F14cNumDoc='" + cNumDoc + "' And F14cTipMov='" + This.TMovExp + "'"

lStado = f3_sql("*", "F14c", cWhere, , , "__F14CLSC")

If lStado
	*> Actualiza estado de la línea.
	Select F80l
	cNumLsC = F80lNumLst
	Replace F80lEstMov With "1"
	=f3_updtun("F80l")

	*> Actualiza el estado de la cabecera de la lista de carga.
	=This.ActualizarListaC(cNumLsC)
Else
	*> Dar de baja el documento de la lista de carga.
	lStado = This.DelLsCDoc(cCodPro, cTipDoc, cNumDoc)
EndIf

Use In (Select ("__F14CLSC"))
Return

ENDPROC
PROCEDURE cnflstmp_cross

*> Confirmar un MP de una lista de trabajo. En función del tipo (preparación, reposición),
*> realiza la llamada al método específico para el tratamiento del movimiento.

*> Recibe:
*>	- cNumeroMovimiento ó bien This.NumMovLS, nº de MP a confirmar.
*>	- This.FrzCnf, Forzar si el movimiento está bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedición (listas de preparación). (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (listas de preparación). (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (listas de preparación). (usualmente 3000).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

Parameters cNumeroMovimiento

Private cWhere, cNumMov
Local lStado

This.UsrError = ""

=This.InitLocals()

cNumMov = Iif(PCount()==1, cNumeroMovimiento, This.NumMovLS)

*> Cargar MP de la lista.
m.F26lNumMov = cNumMov
If !f3_seek("F26l")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F26l
Scatter Name This.oF26l

*> Forzar confirmación, aunque el MP esté bloqueado.
If This.oF26l.F26lFlag1 # Space(1) .And. This.FrzCnf # 'S'
	This.UsrError = "Movimiento bloqueado"
	Return .F.
EndIf

*> Seleccionar el proceso a realizar, en función del tipo de lista.
Do Case
	*> Lista de PREPARACION.
	Case Between(This.oF26l.F26lTipMov, '2000', '2998')
		lStado = This._cnflstmpp_cross()


	*> Tipo de lista no definido.
	Otherwise
		This.UsrError = "Tipo de lista: " + This.oF26l.F26lTipLst + " no definido"
		lStado = .F.
EndCase

Return lStado

ENDPROC
PROCEDURE _cnflstmpp_cross

*> Confirmar un MP de una lista de PREPARACION.

*> Recibe (de This.CnfLstMP):
*>	- This.FrzCnf, Forzar si el movimiento está bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedición (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (usualmente 3000).
*>	- This.oF26l, Movimiento que contiene los datos de la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Método privado, de uso interno de la clase. LLamado desde This.CnfLstMP.

*> Modificaciones:
*>		AVC - 02.06.2005 : Unificar funcionalidad de cálculo de ubicación destino, si está en blanco, con
*>						   la clase de reserva, pues este proceso se utilizará aquí y en reserva de material.
*>		AVC - 17.06.2005 : Agregar validación de MP con reposiciones pendientes.

Private cWhere, cCampos, cValores, cNumMov, oF26l, dFecMov, cEstMov, cFlag, cTipMov, cRowID, cUbiOri, cUbiDes
Local lStado

This.UsrError = ""

oF26l = This.oF26l
cNumMov = oF26l.F26lNumMov
cCodPro = oF26l.F26lCodPro
cTipDoc = oF26l.F26lTipDoc
cNumDoc = oF26l.F26lNumDoc

*> Validar tipos de movimiento a generar en histórico.
m.F00bCodMov = This.TMovOrg
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovOrg + " no existe"
	Return .F.
EndIf

m.F00bCodMov = This.TMovDst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovDst + " no existe"
	Return .F.
EndIf

*> Validar tipo de movimiento expedición.
m.F00bCodMov = This.TMovExp
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovExp + " no existe"
	Return .F.
EndIf

*> Ubicación destino por defecto, si cal.
If Empty(oF26l.F26lUbiDes)
	lStado = This.AsignarMuelle(oF26l.F26lCodPro, oF26l.F26lTipDoc, oF26l.F26lNumDoc)
	If !lStado
		*> El mensaje ya viene asignado.
		Return lStado
	EndIf

	oF26l.F26lUbiDes = This.Muelle
EndIf


*> Asignar propiedades a la función de actualización.
This.ActzObj.ObjParm = This.ActzPrm

*> Movimiento de salida origen.
With This.ActzObj.ObjParm
	.Inicializar
	.CargarParametros('L', oF26l)

	.PuFlag = 'S'
	.PoFlag = 'S'
	.PsFlag = 'N'
	.PmFgHM = 'S'
	
	.PoCFis = oF26l.F26lCanFis
	.PoCRes = oF26l.F26lCanFis
	.PoRowID= oF26l.F26lIdOcup
	
	.PmEnSa = "S"
	.PmTMov = This.TMovOrg

	.PtAcc  = '06'

EndWith

This.ActzObj.Grabar_F20c(oF26l.F26lIdOcup)

*!*	This.ActzObj.Ejecutar

*!*	If This.ActzObj.ObjParm.PwCrtn >= '50'
*!*		This.UsrError = "Error confirmando salida origen"
*!*		Return .F.
*!*	EndIf

This.ActzObj.ActHM				&& Registro en histórico.

*> Movimiento de entrada destino.
With This.ActzObj.ObjParm
	.PuBOld = oF26l.F26lUbiDes

	.PoCUbi = oF26l.F26lUbiDes
	.PoRowIDNew = ""

	.PmEnSa = "E"
	.PmTMov = This.TMovDst
	.PmNMov = ""

	.PtAcc  = '07'
EndWith

*!*	This.ActzObj.Ejecutar

*!*	If This.ActzObj.ObjParm.PwCrtn >= '50'
*!*		This.UsrError = "Error confirmando entrada destino"
*!*		Return .F.
*!*	EndIf

This.ActzObj.ActHM				&& Registro en histórico.

*> Actualizar el estado de la línea de detalle de la lista.
dFecMov = Date()
cEstMov = "3"
cFlag = Space(1)

cNumMac = SubStr(oF26l.F26lNumPal, 2, 9)

cCampos  = "F26lFecMov,F26lEstMov,F26lNumMac,F26lFlag1"
cValores = "dFecMov,cEstMov,cNumMac,cFlag"
cWhere   = "F26lNumMov='" + cNumMov + "'"

lStado = f3_updtun('F26l', , cCampos, cValores, , cWhere, 'N', 'N')

*> Actualizar, si cal, la cabecera de la lista de trabajo.
=This.ActualizarLista(oF26l.F26lNumLst)

*> Recuperar la ID ocupación destino.
cRowID = This.ActzObj.GetLocals("WRowID")

*> Actualizar TMOV, Ubicación e ID ocupación en el MP.
cWhere = "F14cNumMov='" + cNumMov + "'"
cCampos = "F14cTipMov,F14cIdOcup,F14cUbiOri,F14cUbiDes"
cValores = "cTipMov,cRowID,cUbiOri,cUbiDes"

cTipMov = This.TMovExp
cUbiOri = oF26l.F26lUbiOri
cUbiDes = Space(14)

*=f3_updtun('F14c', , cCampos, cValores, , cWhere, 'N', 'N')

_Sentencia =              " Update F14c001"
_Sentencia = _Sentencia + "    Set F14cTipMov ='" + cTipMov + "'"
_Sentencia = _Sentencia + "      , F14cUbiOri ='" + cUbiOri + "'"
_Sentencia = _Sentencia + "      , F14cUbiDes ='" + cUbiDes + "'"
_Sentencia = _Sentencia + "      , F14cNumMac ='" + cNumMac + "'"
_Sentencia = _Sentencia + "  Where F14cNumMov ='" + cNumMov + "'"

=SqlExec(_Asql,_Sentencia)

*> Estado de preparación del documento.
=This._actualizarestadodocumento(cCodPro, cTipDoc, cNumDoc)

lStado = .T.
Return lStado

ENDPROC
PROCEDURE Init

=DoDefault()

*> Agregar propiedades temporales.
With This
	.AddProperty("cDocCnc[1]", "")										&& Documentos cancelados.
	.AddProperty("nCurDoc", 0)											&& Cantidad de documentos cancelados.
	.AddProperty("lProBatch", ("PROBATCH" $ Upper(Set("ClassLib"))))	&& Enlaces BATCH.
EndWith

If !This.lProBatch
	*> Para tareas en BATCH.
	Set ClassLib To ProBATCH Additive
EndIf

This.lProBatch = .T.

ENDPROC


EndDefine 
