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
	.NumLst = Space(6)			&& N� de lista de trabajo.
	.NumLsC = Space(6)			&& N� de lista de carga.
	.NumLstNew = Space(6)		&& N� de lista nueva.
	.CodPro = Space(6)			&& Propietario.
	.TipDoc = Space(4)			&& Tipo documento.
	.NumDoc = Space(13)			&& N� documento.

	.NumMovMP = ""				&& N� de movimiento pendiente.
	.NumMovLS = ""				&& N� de MP lista.
	.NumMovNew = ""				&& N� movimiento para inserci�n.
	.RowID = ""					&& Identificador ocupaci�n.
	.RowIDNew = ""				&& Identificador ocupaci�n nueva.

	.AgrDoc = "N"				&& Agrupar documentos en lista.
	.PrmOpe = "N"				&& Trabajar con los par�metros del operario.
	.TipLst = ""				&& Tipo de lista a generar (P: Preparaci�n, R: Reposici�n, I:Inventario, U: No definido).
	.FrzCnf = "N"				&& Forzar confirmaci�n.
	.UpdLst = "N"				&& Actualizar estado lista trabajo.
	.UpdLsC = "N"				&& Actualizar estado lista carga.
	.LstParcial = "N"			&& Lista parcial (en borrado, p.ej).
	.GenLista = "N"				&& Generar lista (al asignar MP a otra lista, p. ej).

	.UpdDoc = "N"				&& Actualizar estado documento.
	.UpdTransito = "N"			&& Actualizar stock en tr�nsito.
	.StkTrn = Space(4)			&& Situaci�n stock en tr�nsito.
	.Muelle = ""				&& Muelle por defecto.
	.StkBlq = Space(4)			&& Situaci�n stock de bloqueado.

	.AgrTrp = "N"				&& Agrupar listas carga por transportista.
	.IdentD = "N"				&& Identificaci�n destino.

	.TMovLst = ""				&& Tipo movimiento preparaci�n.
	.TMovExp = ""				&& Tipo movimiento expedici�n.
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
*> Versi�n en clase de ORA_PROC.ORA_ACCBLS().
*>
*> Recibe:
*>	- cNumeroLista � bien This.NumLst, lista a actualizar.
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

*> Tomar la lista recibida, bien como par�metro o bien como propiedad.
cNumLst = Iif(PCount()==1, cNumeroLista, This.NumLst)

*> Validar los datos recibidos.
If Empty(cNumLst)
	*> Lista vac�a.
	This.UsrError = "La lista de trabajo est� vac�a"
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

	*> Fecha �ltimo movimiento.
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

*> N� de movimientos de la lista.
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
*>	- This.NumDoc, N� documento.
*>	- This.CodOpe, Operario al que se asigna la lista.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto error.
*>	- This.NumLst, N� de lista generada.

Private cWhere
Local lStado

Private cWhere, cNumLst
Local lStado, cMensajes, oF14c

Store "" To This.UsrError, cMensajes

=This.InitLocals()

*!*	*> Validar los datos recibidos. CONTROL ANULADO.
*!*	If Empty(This.CodPro) .Or. Empty(This.TipDoc) .Or. Empty(This.NumDoc)
*!*	   *> Datos documento vac�os.
*!*	   This.UsrError = "Datos documento vac�os"
*!*	   lStado = .F.
*!*	   Return lStado
*!*	EndIf

If Empty(This.CodOpe)
   *> Operario en blanco.
   This.UsrError = "C�digo de operario en blanco"
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

*> Obtener los MPs del documento en curso que est�n pendientes.
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

*> Cursores de trabajo para actualizaci�n de datos.
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

	*> Generar la l�nea de detalle.
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
*>	- cNumeroMovimiento � bien This.NumMovMP, n� de MP.
*>	- This.UpdLst, Actualizar estado lista (S/N).
*>	- This.NumLst, Lista a la que se a�ade el MP. Se crea si blancos.
*>	- This.CodOpe, Operario al que se asigna la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLst, Lista generada, si no exist�a.
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

*> Validar si el MP ya est� en una lista.
If f3_seek("F26l", '[m.F26lNumMov=cNumMov]')
	This.UsrError = "El MP: " + cNumMov + " ya est� en lista"
	lStado = .F.
	Return lStado
EndIf

*> Validar el operario.
If Empty(This.CodOpe)
   This.UsrError = "C�digo de operario en blanco"
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

*> Crear la l�nea de detalle de la lista.
lStado = This._genlstdet()

*> Actualizar el estado de la lista.
lStado = This.ActualizarLista()

Return lStado

ENDPROC
PROCEDURE borrarlista

*> Borrar una lista de trabajo.
*> Recibe:
*>	- cNumeroLista � bien This.NumLst, lista a borrar.
*>	- This.LstParcial, borrar parcialmente (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cNumeroLista

Private cWhere, cNumLst
Local lStado

This.UsrError = ""

*> Tomar la lista recibida, bien como par�metro o bien como propiedad.
cNumLst = Iif(PCount()==1, cNumeroLista, This.NumLst)

*> Validar los datos recibidos.
If Empty(cNumLst)
	*> Lista vac�a.
	This.UsrError = "La lista est� vac�a"
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
	*> No hay l�neas de detalle de la lista.
	This.UsrError = "No hay l�neas de la lista: " + cNumLst
	Use In (Select("F26cDel"))
	Use In (Select("F26lDel"))
	Return lStado
EndIf

Select F26lDel
Go Top
Do While !Eof()
	This.NumMovLS = F26lNumMov		&& N� MP a borrar de la lista.
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
*>	- cNumeroMovimiento � bien This.NumMovLS, n� de MP a borrar.
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

*> Borrar l�nea de la lista.
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
*>	- cMovimiento � bien This.NumMovMP, n� de MP.
*>	- nCantidad � bien This.CanFis, cantidad nuevo movimiento.
*>	- This.NumMovNew, nuevo n� de movimiento (opcional).
*>	- This.UpdLst, actualizar lista (S/N).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumMovNew, nuevo n� de movimiento.
*>	- This.UsrError, Texto errores.

Parameters cMovimiento, nCantidad
Private cWhere, cNumMov, nCanFis, nCanOld, cLista
Local lStado

This.UsrError = ""

*> ------------------------------------------------------------------
*> Asignar par�metros / propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)
nCanFis = Iif(Type('nCantidad')=='N', nCantidad, This.CanFis)

*> Validar par�metros.
If Empty(cNumMov) .Or. Empty(nCanFis)
	This.UsrError = "Par�metros en blanco"
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
*> En principio, si el MP esta en lista, deber�a coincidir F14c y F26l

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
				This.UsrError = "Error actualizando n�m. movimiento (MP)"
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
*> Obtener nuevo n�mero de movimiento.
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
*!*	*>	- cMovimiento � bien This.NumMovMP, n� de MP.
*!*	*>	- nCantidad � bien This.CanFis, cantidad nuevo movimiento.
*!*	*>	- This.NumMovNew, nuevo n� de movimiento (opcional).
*!*	*>	- This.UpdLst, actualizar lista (S/N).

*!*	*> Devuelve:
*!*	*>	- Estado (.T. / .F.)
*!*	*>	- This.NumMovNew, nuevo n� de movimiento.
*!*	*>	- This.UsrError, Texto errores.

*!*	Parameters cMovimiento, nCantidad
*!*	Private cWhere, cNumMov, nCanFis, nCanOld, cLista
*!*	Local lStado

*!*	This.UsrError = ""

*!*	*> Asignar par�metros / propiedades a variables de trabajo.
*!*	cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)
*!*	nCanFis = Iif(Type('nCantidad')=='N', nCantidad, This.CanFis)

*!*	*> Validar par�metros.
*!*	If Empty(cNumMov) .Or. Empty(nCanFis)
*!*		This.UsrError = "Par�metros en blanco"
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

*!*		*> Obtener nuevo n�mero de movimiento.
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

*!*		*> Obtener nuevo n�mero de movimiento.
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
*!*		*> Obtener nuevo n�mero de movimiento.
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

*> Creaci�n de la cabecera de la lista de trabajo.
*> M�todo protegido, de uso interno.

*> Recibe:
*>	- This.CurLst, lista actual.
*>	- This.CodOpe, operario al que se le asigna la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLst, lista actual.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 05.11.2007 (AVC) Incorporar flag de agrupaci�n de documentos en lista.
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

*> Creaci�n de una l�nea de detalle de la lista de trabajo.
*> Recibe:
*>	- cNumero movimiento � bien This.NumMovMP, MP del que toma los datos (opcional).
*>	- This.CurLst, lista de trabajo actual.
*>	- This.TipLst, tipo de lista.
*>	- This.CodOpe, operario al que se le asigna la lista.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>
*> M�todo protegido, de uso interno.

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
	This.UsrError = "No se puede insertar l�nea lista de trabajo " + This.CurLst
	If !lIsOpenF26l
		Use In F26lUpd
	EndIf
	Return lStado
Endif

*> Actualizar operario y n� de lista en el MP.
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
*>	- cLista � bien This.NumLst, n� de lista.
*>	- cOperario � bien This.CodOpe, nuevo operario.
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cLista, cOperario
Private cWhere, cNumLst, cNewOpe
Local lStado

This.UsrError = ""

*> Asignar par�metros / propiedades a variables de trabajo.
cNumLst = Iif(Type('cLista')=='C', cLista, This.NumLst)
cNewOpe = Iif(Type('cOperario')=='C', cOperario, This.CodOpe)

*> Validar par�metros.
If Empty(cNumLst) .Or. Empty(cNewOpe)
	This.UsrError = "Par�metros en blanco"
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
*>	- cMovimiento � bien This.NumMovLS, movimiento a cambiar.
*>	- cLista � bien This.NumLst, n� de lista.
*>	- This.UpdLst, actualizar listas (S/N).
*>	- This.GenLista, lista nueva (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumLstNew, n� de lista, si se crea nueva.
*>	- This.UsrError, Texto errores.

Parameters cMovimiento, cLista
Private cWhere, cNumMov, cNewLst, cOldLst, cCodOpe, cNewOpe
Local lStado

This.UsrError = ""
cOldLst = ""

*> Asignar par�metros / propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovLS)
cNewLst = Iif(Type('cLista')=='C', cLista, This.NumLst)

*> Validar par�metros.
If Empty(cNumMov) .Or. (Empty(cNewLst) .And. This.GenLista<>"S")
	This.UsrError = "Par�metros en blanco"
	Return .F.
EndIf

*> Reemplazar la lista en el detalle de la lista.
m.F26lNumMov = cNumMov
If f3_seek("F26l")
	Select F26l
	If F26lEstMov<>"0"
		This.UsrError = "Movimiento en estado no v�lido"
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

	*> Actualizar el n� de lista en el movimiento.
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
*>	- cListaOrigen � bien This.NumLst, lista origen.
*>	- cListaDestino � bien This.NumLstNew, n� de lista nueva.
*>	- This.GenLista, Lista nueva (S/N).
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumLstNew, n� de lista, si se crea nueva.
*>	- This.UsrError, Texto errores.

Parameters cListaOrigen, cListaDestino
Private cWhere, cNumMov, cNewLst, cOldLst
Local lStado

This.UsrError = ""
cOldLst = ""

*> Asignar par�metros / propiedades a variables de trabajo.
cOldLst = Iif(Type('cListaOrigen')=='C', cListaOrigen, This.NumLst)
cNewLst = Iif(Type('cListaDestino')=='C', cListaDestino, This.NumLstNew)

*> Validar par�metros.
If Empty(cOldLst) .Or. (Empty(cNewLst) .And. This.GenLista<>"S")
	This.UsrError = "Par�metros en blanco"
	Return .F.
EndIf

If cOldLst==cNewLst
	This.UsrError = "Par�metros incorrectos"
	Return .F.
EndIf

*> Cargar cursor con las l�neas de la lista.
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

*> Cambiar la ubicaci�n origen a un MP.

*> Recibe:
*>	- cMovimiento � bien This.NumMovMP, movimiento a cambiar.
*>	- cIdNew � bien This.RowIDNew, identificador ocupaci�n destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

*> Llamado desde:
*>		- This.CncCrgLstMP, Cancelar carga listas.
*>		- This._cnflstmpr, Confirmar lista de reposici�n.
*>		- This.ChgUbiID, Cancelar ubicaci�n origen por ID ocupaci�n.

*> Historial de modificaciones:
*> 07.01.2008 (LRC) Ajustar cantidad en reposiciones.

Parameters cMovimiento, cIdDestino

Private cWhere, cNumMov, cRowId, cRowIdNew, oF14c, oF16cDst

Local lStado

This.UsrError = ""

*> Asignar par�metros / propiedades a variables de trabajo.
cNumMov = Iif(Type('cMovimiento')=='C', cMovimiento, This.NumMovMP)
cRowIdNew = Iif(Type('cIdDestino')=='C', cIdDestino, This.RowIDNew)

*> Validar par�metros.
If Empty(cNumMov) .Or. Empty(cRowIdNew)
	This.UsrError = "Par�metros en blanco"
	Return .F.
EndIf

*> Leer el movimiento pendiente.
If !f3_seek("F14c", '[m.F14cNumMov=cNumMov]')
	This.UsrError = "No existe el movimiento (MP)"
	Return .F.
EndIf

*> Guardar ocupaci�n origen.
Select F14c
Scatter Name oF14c
cRowId = oF14c.F14cIdOcup

If cRowId==cRowIdNew
	This.UsrError = "Origen y destino son iguales"
	Return .F.
EndIf

*> Cargar ocupaci�n destino.
cWhere = "F16cIdOcup='" + cRowIdNew + _cm
If !f3_sql("*", "F16c", cWhere, , , "F16cIdDst")
	This.UsrError = "No existe ocupaci�n destino"
	Use In (Select("F16cIdDst"))
	Return .F.
EndIf

*> Guardar ocupaci�n destino.
Select F16cIdDst
Go Top
Scatter Name oF16cDst

*> Validar datos de ambas ocupaciones.
If oF14c.F14cCodPro<>oF16cDst.F16cCodPro .Or. ;
   oF14c.F14cCodArt<>oF16cDst.F16cCodArt
	This.UsrError = "El art�culo es distinto"

	Use In (Select("F16cIdDst"))
	Return .F.
EndIf

*> Validar cantidad en ocupaci�n destino, s�lo si se trata de reservas.
If Left(oF14c.F14cTipMov, 1) == '2'
	If (oF16cDst.F16cCanFis < oF14c.F14cCanFis)
		This.UsrError = "Cantidad destino insuficiente"

		Use In (Select("F16cIdDst"))
		Return .F.
	EndIf
EndIf

*> LRC. 7/1/2008
*> Si el MP es de reposoci�n se debe ajustar la cantidad a la menor entre la del MP y la del palet leido.
If Left(oF14c.F14cTipMov, 1) == '3'
	cCanRep = IIf(oF14c.F14cCanFis < oF16cDst.F16cCanFis, oF14c.F14cCanFis, oF16cDst.F16cCanFis)
EndIf

*> Cambiar datos ocupaci�n en el movimiento pendiente.
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

*> Cambiar datos ocupaci�n en la lista.
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

*> Recalcular ocupaci�n origen.
With This.RStkObj
	.Inicializar
	.RSDelE = "S"
	.RowID = cRowId
	.Ejecutar('01')
EndWith

*> Recalcular ocupaci�n destino.
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

*> Inicializar clase de rec�lculo de stocks.
If Type('This.RStkObj')<>'O'
	This.RStkObj = CreateObject('OraFncRStk')
EndIf

Return This.RStkObj

ENDPROC
PROCEDURE cnflstmp

*> Confirmar un MP de una lista de trabajo. En funci�n del tipo (preparaci�n, reposici�n),
*> realiza la llamada al m�todo espec�fico para el tratamiento del movimiento.

*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovLS, n� de MP a confirmar.
*>	- This.FrzCnf, Forzar si el movimiento est� bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedici�n (listas de preparaci�n). (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (listas de preparaci�n). (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (listas de preparaci�n). (usualmente 3000).
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

*> Forzar confirmaci�n, aunque el MP est� bloqueado.
If This.oF26l.F26lFlag1<>Space(1) .And. This.FrzCnf<>'S'
	This.UsrError = "Movimiento bloqueado"
	Return .F.
EndIf

*> Seleccionar el proceso a realizar, en funci�n del tipo de lista.
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

*> Crear objeto de la clase para la funci�n de actualizaci�n.
If Type('This.ActzObj')<>'O'
	This.ActzObj = CreateObject('OraFncActz')
EndIf

Return This.ActzObj

ENDPROC
PROCEDURE actzprm_access

*> Crear objeto de la clase para la asignar par�metros a la funci�n de actualizaci�n.
If Type('This.ActzPrm')<>'O'
	This.ActzPrm = CreateObject('OraPrmActz')
EndIf

Return This.ActzPrm

ENDPROC
PROCEDURE cnfcrgmp

*> Confirmar salida de muelle de un movimiento pendiente.

*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovMP, n� de MP a confirmar.
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.UpdTransito, actualizar stock en tr�nsito (S/N). (NO UTILIZADO).
*>	- This.StkTrn, Situaci�n stock en tr�nsito (S/N).       (NO UTILIZADO).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.CnfCrgDoc, confirmar carga de un pedido.
*>	- SAEXCFCA, confirmar carga MP (PANTALLA)
*>	- <prorad>.CnfActualizar, confirmar carga MP (RF).

*> Historial de modificaciones:
*>	- Cambiar el c�lculo del estado del documento. AVC - 13.06.2007
*>	- Adaptar a expediciones parciales. AVC - 14.06.2007
*>	- Adaptar a partes de montaje. AVC - 03.12.2007
*> 13.05.2008 (AVC) Adaptar a listas de carga, dando de baja el documento confirmado si est� en una lista de carga.

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

*> Validar tipo de movimiento a generar en hist�rico.
m.F00bCodMov = oF14c.F14cTipMov
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + oF14c.F14cTipMov + " no existe"
	Return .F.
EndIf

*> Validar la situaci�n de stock en tr�nsito.
If This.UpdTransito=="S"
	m.F00cCodStk = This.StkTrn
	If !f3_seek('F00c')
		This.UsrError = "Situaci�n stock tr�nsito: " + This.StkTrn + " no existe"
		Return .F.
	EndIf
EndIf

*> Seg�n el tipo de documento se conoce si se trata de un parte de salida.
lParteMontaje = Iif(oF14c.F14cTipDoc==TDocSalida, .T., .F.)

*>
If lParteMontaje

	*> Recuperar la ubicaci�n de la entidad.
	m.F24cCodPro = oF14c.F14cCodPro
	m.F24cTipDoc = TDocSalida
	m.F24cNumDoc = oF14c.F14cNumDoc
	_UbiExp = IIf(F3_Seek("F24c"), F24c.F24cUbiExp, oF14c.F14cCodUbi)

	_NewHMO = Ora_NewHM()                   && N�mero movimiento origen
	_NewHMD = Ora_NewHM()                   && N�mero movimiento destino
	Do While Val(_NewHMD) # (Val(_NewHMO) + 1)
      *> Los n�meros NO son correlativos.
      _NewHMO = Ora_NewHM()
      _NewHMD = Ora_NewHM()
	EndDo

	_TMovHMO = '3600'
	_TMovHMD = '3100'
	
	*> Validar tipo de movimiento a generar en hist�rico.
	m.F00bCodMov = _TMovHMO
	If !f3_seek('F00b')
		This.UsrError = "Tipo movimiento: " + _TMovHMO + " no existe"
		Return .F.
	EndIf

	*> Validar tipo de movimiento a generar en hist�rico.
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

*> Asignar propiedades a la funci�n de actualizaci�n.
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


*> Si es un pedido normal se realiza una anotaci�n en HM del MP de expedici�n, sino el MP de salida por reubicaci�n.
If lParteMontaje
	With This.ActzObj.ObjParm
		*> Datos actualizaci�n MPs/HMs
		.PMTMov = _TMovHMO                   && Tipo movimiento
		.PMNMov = _NewHMO                    && N�mero movimiento
		.PMFMov = Date()                     && Fecha movimiento
		.PMEnSa = 'S'                        && Entrada / Salida
	EndWith
EndIf

This.ActzObj.ActHM									&& Registro en hist�rico con tipo de Movimiento

*> -----------------------------------------------------------------------------------------
*> Si se trata de un parte de taller hacer una entrada sin reservar.
*> Actualizar el campo lote con el numero de parte y en el campo N� analisis guardar el lote.
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
		*> Datos actualizaci�n MPs/HMs
		.PMTMov = _TMovHMD                   && Tipo movimiento
		.PMNMov = _NewHMD                    && N�mero movimiento
		.PMFMov = Date()                     && Fecha movimiento
		.PMEnSa = 'E'                        && Entrada / Salida
	EndWith

	This.ActzObj.ActHM									&& Registro en hist�rico con tipo de Movimiento

EndIf
*> -----------------------------------------------------------------------------------------

*> Borrar el MP.
m.F14cNumMov = cNumMov
=f3_baja('F14c')

*> Actualizar la cantidad enviada en la l�nea de detalle del documento.
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

	*> Actualizar el estado de la l�nea de la lista de carga.
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

*> Crear objeto de la clase para la asignar par�metros a la funci�n de reserva.
If Type('This.ResvPrm')<>'O'
	This.ResvPrm = CreateObject('OraPrmResv')
EndIf

Return This.ResvPrm

ENDPROC
PROCEDURE cancellstmp
	
*> Cancelar un MP de una lista de trabajo.
*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovLS, n� de MP a confirmar.
*>	- This.FrzCnf, Forzar si el movimiento est� bloqueado (S/N).
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

*> Validar tipo de movimiento a generar en hist�rico.
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
	This.UsrError = "El movimiento ya est� confirmado"
	Return .F.
EndIf

*> Forzar regularizaci�n aunque est� bloqueado.
If oF26l.F26lFlag1<>Space(1) .And. This.FrzCnf<>'S'
	This.UsrError = "Movimiento bloqueado"
	Return .F.
EndIf

*> Primer paso: Eliminar el movimiento de la lista.
*> El resto de propiedades ya se han recibido al llamar a este m�todo.
With This
	.NumLst = oF26l.F26lNumLst
	lStado = .BorrarListaMP()
EndWith

If !lStado
	Return lStado
EndIf

*> Segundo paso: Desreservar el MP.
*> El resto de propiedades ya se han recibido al llamar a este m�todo.
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

*> Finalmente, Regularizar stock f�sico y crear movimiento en el hist�rico.
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

This.ActzObj.ActHM				&& Registro en hist�rico

lStado = .T.
Return lStado

ENDPROC
PROCEDURE cnccrgmp

*> Cancelar salida de muelle de un movimiento pendiente.
*> El material queda en el muelle y pendiente de reubicar.

*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovMP, n� de MP a confirmar.
*>	- This.UpdDoc, actualizar estado documento (S/N).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*>	- Cambiar el c�lculo del estado del documento. AVC - 13.06.2007
*>	- Adaptar a expediciones parciales. AVC - 14.06.2007
*> 13.05.2008 (AVC) Adaptar a listas de carga, dando de baja el documento confirmado si est� en una lista de carga.

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

*> Asignar propiedades a la funci�n de actualizaci�n.
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
	*> Se actualiza el esado cabecera a 3 para permitir que el m�todo UpdDocRsv lo pueda recalcular.
	cCampos  = "F24cFlgEst"
	cValores = "3"
	cWhere   =               "F24cCodPro='" + oF14c.F14cCodPro + "'"
	cWhere   = cWhere + " And F24cTipDoc='" + oF14c.F14cTipDoc + "'"
	cWhere   = cWhere + " And F24cNumDoc='" + oF14c.F14cNumDoc + "'"
	=f3_updtun('F24c', , cCampos, cValores, , cWhere, 'N', 'N')
	*> -----------------------------------------------------------------------------

	*> Estado general del documento.
	lStado = This.ResvObj.UpdDocRsv(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)
	
	*> Actualizar el estado de la l�nea de la lista de carga.
	lStado = This.ActualizarListaCD(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)
EndIf

Return

ENDPROC
PROCEDURE cnccrglstmp

*> Cancelar expedici�n de un MP y reasignar a una lista de trabajo.
*> El material queda en su ubicaci�n origen y pendiente de asignar a lista.

*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovMP, n� de MP a cancelar.
*>	- This.TMovLst, Tipo movimiento preparaci�n (usualmente 2000).
*>	- This.TMovOrg, Tipo movimiento origen para HM (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (usualmente 3000).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Revisiones:
*>	AVC - 16.06.2005 Borrar el n� de MAC.
*>	AVC - 13.06.2006 Asignar la ubicaci�n destino (muelle) al MP.
*>	AVC - 14.06.2007 Adaptar a expediciones parciales.
*> 13.05.2008 (AVC) Adaptar a listas de carga, dando de baja el documento confirmado si est� en una lista de carga.

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

*> Validaci�n de los datos del movimiento
If F14cTipMov<>'2999'
	This.UsrError = "Tipo de movimiento incorrecto"
	Return .F.
EndIf

*> Leer la lista para reubicaci�n.
m.F26lNumMov = cNumMov
If !f3_seek("F26l")
	This.UsrError = "No se encuentra el movimiento (LS): " + cNumMov
	Return .F.
EndIf

Select F26l
Scatter Name oF26l

*> Validar tipo de movimiento de preparaci�n.
m.F00bCodMov = This.TMovLst
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovLst + " no existe"
	Return .F.
EndIf

*> Validar tipos de movimiento a generar en hist�rico.
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

*> Asignar propiedades a la funci�n de actualizaci�n.
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

This.ActzObj.ActHM				&& Registro en hist�rico

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

This.ActzObj.ActHM				&& Registro en hist�rico

*> Leer los datos de la nueva ocupaci�n.
cRowID = This.ActzObj.GetLocals("WRowID")

If !f3_seek("F16c", '[m.F16cIdOcup=cRowID]')
	This.UsrError = "Error leyendo nueva ocupaci�n destino"
	Return .F.
EndIf

Select F16c
Scatter Name oF16c

*> Cambiar el N� de MP para evitar conflictos con el movimiento original.
cNewMov = Ora_NewMP()

*> Actualizar los datos en el MP (para la lista).
Select F14c
Replace F14cTipMov With This.TMovLst				&& Tipo movimiento (usualmente 2000)
Replace F14cUbiDes With oF26l.F26lUbiDes			&& Destino (muelle)
Replace F14cNumMov With cNewMov						&& Nuevo n� de movimiento
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

*> Actualizar los datos de la nueva ocupaci�n en el MP.
lStado = This.ChgUbiMP(cNewMov, cRowID)

*> -----------------------------------------------------------------------------
*> LRC. 17/2/9.
*> Se actualiza el esado cabecera a 3 para permitir que el m�todo UpdDocRsv lo pueda recalcular.
cCampos  = "F24cFlgEst"
cValores = "3"
cWhere   =               "F24cCodPro='" + oF14c.F14cCodPro + "'"
cWhere   = cWhere + " And F24cTipDoc='" + oF14c.F14cTipDoc + "'"
cWhere   = cWhere + " And F24cNumDoc='" + oF14c.F14cNumDoc + "'"
=f3_updtun('F24c', , cCampos, cValores, , cWhere, 'N', 'N')
*> -----------------------------------------------------------------------------

*> Actualizar el estado general del documento.
lStado = This.ResvObj.UpdDocRsv(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)

*> Actualizar el estado de la l�nea de la lista de carga.
lStado = This.ActualizarListaCD(oF14c.F14cCodPro, oF14c.F14cTipDoc, oF14c.F14cNumDoc)

Return lStado

ENDPROC
PROCEDURE _cnflstmpp

*> Confirmar un MP de una lista de PREPARACION.

*> Recibe (de This.CnfLstMP):
*>	- This.FrzCnf, Forzar si el movimiento est� bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedici�n (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (usualmente 3000).
*>	- This.oF26l, Movimiento que contiene los datos de la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> M�todo privado, de uso interno de la clase. LLamado desde This.CnfLstMP.

*> Modificaciones:
*>		AVC - 02.06.2005 : Unificar funcionalidad de c�lculo de ubicaci�n destino, si est� en blanco, con
*>						   la clase de reserva, pues este proceso se utilizar� aqu� y en reserva de material.
*>		AVC - 17.06.2005 : Agregar validaci�n de MP con reposiciones pendientes.
*>		??? - 10.08.2006 : Agregar actualizaci�n estado documento 'pedido preparado'.
*>		AVC - 17.08.2006 : Grabar N� palet como MAC si bulto completo.

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

*> Validar tipos de movimiento a generar en hist�rico.
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

*> Validar tipo de movimiento expedici�n.
m.F00bCodMov = This.TMovExp
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovExp + " no existe"
	Return .F.
EndIf

*> Ubicaci�n destino por defecto, si cal.
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

*> Asignar propiedades a la funci�n de actualizaci�n.
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

This.ActzObj.ActHM				&& Registro en hist�rico.

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

This.ActzObj.ActHM				&& Registro en hist�rico.

*> Actualizar el estado de la l�nea de detalle de la lista.
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

*> Recuperar la ID ocupaci�n destino.
cRowID = This.ActzObj.GetLocals("WRowID")

*> Actualizar TMOV, Ubicaci�n e ID ocupaci�n en el MP.
cWhere = "F14cNumMov='" + cNumMov + "'"
cCampos = "F14cTipMov,F14cIdOcup,F14cUbiOri,F14cUbiDes,F14cNumMac"
cValores = "cTipMov,cRowID,cUbiOri,cUbiDes,cNumMac"

cTipMov = This.TMovExp
cUbiOri = oF26l.F26lUbiDes
cUbiDes = Space(14)

=f3_updtun('F14c', , cCampos, cValores, , cWhere, 'N', 'N')

*> Estado de preparaci�n del documento.
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

*> Confirmar un MP de una lista de REPOSICION. Actualiza los MPs de preparaci�n asociados a la reposici�n. Se valida que la cantidad
*> de los MPs 2000 con la cantidad de la reposici�n, dividiendo el MP de preparaci�n si la cantidad es mayor que la de la reposici�n.

*> M�todo privado, de uso interno de la clase.

*> Recibe (de This.CnfLstMP):
*>	- This.FrzCnf, Forzar si el movimiento est� bloqueado (S/N).
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

*> Calcular y validar tipos de movimiento a generar en hist�rico.
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

*> Asignar propiedades a la funci�n de actualizaci�n.
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

This.ActzObj.ActHM				&& Registro en hist�rico.

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

This.ActzObj.ActHM				&& Registro en hist�rico.

*> Actualizar el estado de la l�nea de detalle de la lista.
dFecMov = Date()
cEstMov = "3"
cFlag = Space(1)

cCampos  = "F26lFecMov,F26lEstMov,F26lFlag1"
cValores = "dFecMov,cEstMov,cFlag"
cWhere   = "F26lNumMov='" + cNumMov + "'"

lStado = f3_updtun('F26l', , cCampos, cValores, , cWhere, 'N', 'N')

*> Actualizar, si cal, la cabecera de la lista de trabajo.
=This.ActualizarLista(oF26l.F26lNumLst)

*> Recuperar la ID ocupaci�n destino.
cRowID = This.ActzObj.GetLocals("WRowID")

*> Cantidad m�xima a repones en los MPs.
nCantidadRepos = oF26l.F26lCanFis

*> Realizar el cambio de ocupaci�n en los MPs de preparaci�n asociados a la reposici�n.
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

	=This.ChgUbiMP(oF14c.F14cNumMov, cRowID)				&& Realizar el cambio de ubicaci�n.
	nCantidadRepos = nCantidadRepos - oF14c.F14cCanFis		&& Cantidad restante para procesar.

	Select F14cUpdRep
	Skip
EndDo

*> Eliminar el MP de reposici�n.
m.F14cNumMov = oF26l.F26lNumMov
=f3_baja("F14c")

*> Actualizar el estado de bloqueo de los MPs asociados.
This.ReposObj.SetBloqMP(cRowID, Space(1))			&& MPs y listas destino.
This.ReposObj.SetBloqMP(oF26l.F26lIdOcup, Space(1))	&& MPs y listas origen.
This.ReposObj.SetBloqOC(oF26l.F26lIdOcup)			&& Ocupaci�n origen.

*> Si no hay MPs es necesario recalcular ocupaci�n destino.
If !lHayMPs
	With This.RStkObj
		*> Ocupaci�n origen.
		.Inicializar
		.RSDelE = "S"
		.RowID = oF26l.F26lIdOcup
		.Ejecutar('01')
	EndWith

	With This.RStkObj
		*> Ocupaci�n destino.
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
*> Calcula la ubicaci�n destino cuando genera los MPs de reserva, si �sta viene en blanco.
*> M�todo p�blico, pero de uso privado de las clases.

*> Criterios:
*>	- Ubicaci�n de expedici�n del documento de salida.
*>	- Ubicaci�n asignada al transportista en entidades / ubicaci�n (F01u).
*>	- Ubicaci�n gen�rica de muelle.

*> Llamado desde:
*>	- OraFncResv.AsignarMuelle()
*>	- OraFncList._cnflstmpp()

*> Recibe:
*>	- Documento (Propietario, tipo y n�mero).

*> Devuelve:
*>	- This.Muelle, ubicaci�n de muelle.

Parameters cPropietario, cTipo, cDocumento

Private cWhere, cCodPro, cTipDoc, cNumDoc, cMuelle
Local lStado, oF24c

This.UsrError = ""
This.Muelle = ""

*> Asignar par�metros a variables.
If PCount()==3
	cCodPro = cPropietario
	cTipDoc = cTipo
	cNumDoc = cDocumento
Else
	cCodPro = Space(6)
	cTipDoc = Space(4)
	cNumDoc = Space(13)
EndIf

*> Primero, tomar la ubicaci�n de expedici�n original del documento de salida.
m.F24cCodPro = cCodPro
m.F24cTipDoc = cTipDoc
m.F24cNumDoc = cNumDoc

If f3_seek("F24c")
	cMuelle = F24c.F24cUbiExp
	This.Muelle = ""

	*> Validar la ubicaci�n por defecto, la del documento.
	If f3_seek("F10c", "[m.F10cCodUbi=cMuelle]")
		Select F10c
		Go Top
		If (F10cPickSN=="M" .Or. F10cPickSN=="E") .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
			This.Muelle = cMuelle
		EndIf
	EndIf

	*> Buscar la ubicaci�n asignada al transportista.
	If Empty(This.Muelle)
		cWhere = 		  "F01uTipEnt='TRAN' And "
		cWhere = cWhere + "F01uCodEnt='" + F24c.F24cCodTra + "' And "
		cWhere = cWhere + "(F01uTipOri='M' Or F01uTipOri='E') And F01uActivo='S'"

		lStado = f3_sql("*", "F01u", cWhere, , , "F01uMUELLE")

		Select F01uMUELLE
		Go Top
		Do While !Eof()
			*> Validar la ubicaci�n.
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

	*> Ubicaci�n destino por defecto.
	If Empty(This.Muelle)
		*> Buscar una ubicaci�n gen�rica de expedici�n.
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
	This.UsrError = "No hay ubicaci�n destino"
	lStado = .F.
EndIf

Use In (Select("F10cUbiDes"))
Use In (Select("F01uMUELLE"))
Return lStado

ENDPROC
PROCEDURE validarreposicion

*> Validar si un MP est� pendiente de reposiciones.

*> Recibe:
*>	- cRowID, ID ocupaci�n del MP a validar.

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
*> Cancela el MP de la lista (si hay), desreserva y pasa el stock de la ocupaci�n a situaci�n de bloqueado.

*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovMP, n� de MP a confirmar.
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.TMovOrg, Tipo movimiento salida disponible para HM (usualmente 4500).
*>	- This.TMovDst, Tipo movimiento entrada bloqueado para HM (usualmente 4000).
*>	- This.StkBlq, Situaci�n de stock de bloqueado (usualmente 9000).

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

*> Obtener el MP que gener� la incidencia.
m.F14cNumMov = cNumMov
If !f3_seek("F14c")
	This.UsrError = "No se encuentra el movimiento: " + cNumMov
	Return .F.
EndIf

Select F14c
Scatter Name oF14c

*> A partir del MP, obtener la ID de la ocupaci�n a bloquear.
lStado = This.BloquearLstOC(oF14c.F14cIdOcup)

Return lStado

ENDPROC
PROCEDURE bloquearlstoc
	
*> Bloquear una ocupaci�n del almac�n.
*> Cancela el MP de la lista (si hay), desreserva y pasa el stock de la ocupaci�n a situaci�n de bloqueado.
*> Trata tanto MPs de preparaci�n como de reposici�n.

*> Recibe:
*>	- cIdOcup � bien This.RowID, ID de la ocupaci�n a bloquear.
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.UpdDoc, actualizar estado documento (S/N).
*>	- This.TMovOrg, tipo movimiento salida disponible para HM (usualmente 3500).
*>	- This.TMovDst, tipo movimiento entrada bloqueado para HM (usualmente 3000).
*>	- This.StkBlq, situaci�n de stock de bloqueado (usualmente 9000).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.nCurDoc, n� de documentos cancelados.
*>	- This.cDocCnc, Array con los documentos cancelados.
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This.BloquearLstMP, bloquear ocupaci�n a partir de un MP.

*> Revisiones:
*>	AVC - 21.07.2006 Corregir n� movimiento al generar hist�rico de entrada en destino.

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

*> Validar tipo de movimiento a generar en hist�rico.
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

*> Validar la situaci�n de stock bloqueado.
m.F00cCodStk = This.StkBlq
If !f3_seek('F00c')
	This.UsrError = "Situaci�n stock bloqueo: " + This.StkBlq + " no existe"
	Return .F.
EndIf


*> Obtener todos los de la misma ID ocupaci�n para su cancelaci�n.
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

*> Cancelaci�n de los MPs obtenidos.
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

	*> Si el MP no es de preparaci�n, ignorar la desreserva / nueva reserva y pasar al siguiente.
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

	*> Procesar siguiente MP de la ocupaci�n a bloquear.
	Select F14cCncMP
	Skip
EndDo

*> Obtener la ocupaci�n a bloquear.
m.F16cIdOcup = cOcup
If !f3_seek("F16c")
	This.UsrError = "No existe ocupaci�n"
	Use In (Select ("F14cCncMP"))
	Return .F.
EndIf

*> Cambiar la ocupaci�n a stock bloqueado.
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
*> Actualizar el estado de preparaci�n de un documento.
*> Actualiza a estado '4' la cabecera, en el caso de que est� completamente preparado.
*> M�todo privado de la clase.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento, documento a validar estado.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This._cnflstmpp, actualizar MP de preparaci�n.

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
	This.UsrError = "Error al cargar los MPs estado preparaci�n"
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

	*> Generar el albar�n del pedido finalizado.
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

*> Generar el albar�n de un pedido, una vez confirmado.
*> M�todo privado de la clase.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento, documento a generar albar�n.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Llamado desde:
*>	- This._actualizarestadodocumento() , actualizar estado cabecera del pedido.

*> Historial de modificaciones:
*>	- Incorporar nueva generaci�n de albaranes. AVC - 13.06.2007

Parameters cCodPro, cTipDoc, cNumDoc

Local lStado

*> Realizar la generaci�n del albar�n de salida. <<<<<<< OLD METHOD >>>>>>>>
*!*	With This.ActzObj
*!*		With .ObjParm
*!*			.Inicializar

*!*			.PACtrG = Space(1)		&& Separar estupefacientes de ambiente-fr�o.
*!*			.PAFrio = Space(4)		&& Productos de fr�o.
*!*			.PAFriG = Space(4)		&& Productos de fr�o/estupefacientes.
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

*> Generar el albar�n de salida. <<<<<<< NEW METHOD >>>>>>>>
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

*> Control din�mico de procesos BATCH.

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
*>	- This.TMovExp, tipo movimiento expedici�n (generalmente 2999).
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
*!*	*> LRC. 17/12/2007. Actualizar la ubicaci�n de taller de todas las ocupaciones del parte de montaje procesado. Se identifica por el lote que
*!*	*> contiene el numero de parte.
*!*	If cTipDoc = TDocSalida
*!*		*> Recuperar la ubicaci�n de la entidad.
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

*> Crear objeto de la clase para la generaci�n de albaranes.
If Type('This.AlbSObj')<>'O'
	This.AlbSObj = CreateObject('OraFncAlbS')
EndIf

Return This.AlbSObj

ENDPROC
PROCEDURE ___historial__de__modificaciones___

*> Historial de modificaciones:

*> 12.11.2006 (AVC) Enlace con tareas BATCH.
*> 13.06.2007 (AVC) (CnfCrgMP) Cambiar el c�lculo del estado del documento.
*> 13.06.2007 (AVC) (_generaralbaran) Permitir expediciones parciales.
*> 14.06.2007 (AVC) (CnfCrgMP) Permitir expediciones parciales.
*> 14.06.2007 (AVC) (CncCrgMP) Permitir expediciones parciales.
*> 14.06.2007 (AVC) (CncCrgLstMP) Permitir expediciones parciales.
*> 02.08.2007 (AVC) (ChgUbiId) Cambiar ubicaci�n origen por ID. Llamar� a ChgUbiMp. M�todo nuevo.
*> 05.11.2007 (AVC) (_GenLstCab) Incorporar flag de agrupaci�n de documentos en lista.
*> 03.12.2007 (AVC) Adecuar a tratamiento de partes de montaje.
*>					Modificado m�todo This.CnfCrgMP, confirmar carga de UN movimiento Pendiente.
*> 17.12.2007 (LRC) Agregar funcionalidades en tratamiento de partes de montaje.
*>					Modificado m�todo This.CnfCrgMP
*>					Modificado m�todo This.CnfCrgDoc
*> 07.01.2008 (LRC) Ajustar cantidades al confirmar reposiciones.
*>					Modificado m�todo This.ChgUbiMP
*> 30.01.2008 (AVC) Corregir error descarga biblioteca BATCH.
*>					Anulado m�todo This.Destroy
*> 07.02.2008 (AVC) Corregir tratamiento de divisi�n de MPs de preparaci�n.
*>					Modificado m�todo This.Inicializar
*> 06.05.2008 (AVC) Agregar tratamiento de listas de carga.
*>					Agregado m�todo This.GenLsCDoc, agregar un documento a lista de carga.
*>					Agregado m�todo This.DelLsCDoc, quitar un documento a lista de carga.
*>					Agregado m�todo This.UpdLsCCab, Actualizar datos de la cabecera de la lista de carga.
*>					Agregado m�todo This.ActualizarListaC, actualizar estado de la lista de carga.
*>					Agregado m�todo This._genlsccab, generar cabecera de lista de carga.
*>					Agregado m�todo This._genlscdet, agregar documento a l�nea de lista de carga.
*>					Agregada Propiedad This.CurLsC, lista carga activa.
*>					Agregada Propiedad This.NumLsC, lista carga generada.
*>					Agregada Propiedad This.UpdLsC, actualizar estgado lista carga.
*>					Agregada Propiedad This.AgrTrp, generar listas por transportista.
*>					Agregada Propiedad This.IdentD, identificaci�n destino.
*>					Modificado m�todo This.Inicializar
*>					Modificado m�todo This.InitLocals
*> 13.05.2008 (AVC) En Confirmaci�n de carga, validar si un documento est� en lista de carga y darlo de baja.
*>					En Cancelaci�n de carga, validar si un documento est� en lista de carga y darlo de baja.
*>					Modificado m�todo This.CnfCrgMP, confirmar carga
*>					Modificado m�todo This.CncCrgMP, cancelar carga
*>					Modificado m�todo This.CncCrgLstMP, desasignar carga
*> 13.05.2008 (AVC) Agregar confirmaci�n de carga de un MAC.
*>					Agregado m�todo This.CnfCrgMAC
*>					Agregado m�todo This.ActualizarListaCD, actualizar estado l�nea de la lista de carga.

ENDPROC
PROCEDURE chgubiid

*> Cambiar la ubicaci�n origen a los MPs de una ID ocupaci�n.

*> Recibe:
*>	- cID � bien This.RowID, ID de la ocupaci�n actual.
*>	- cIdNew � bien This.RowIDNew, identificador ocupaci�n destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cIdOrigen, cIdDestino

Private cId, cIdNew, cWhere, oF14c
Local lStado

This.UsrError = ""

*> Asignar par�metros / propiedades a variables de trabajo.
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
*>	- cID � bien This.RowID, ID de la ocupaci�n actual.
*>	- cIdNew � bien This.RowIDNew, identificador ocupaci�n destino.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cNumMov, cNumPal

Private cId, cIdNew, cWhere, oF14c, oF16cOrg, oF16cDst, oF25c
Local lStado

This.UsrError = ""

*!*	*> Asignar par�metros / propiedades a variables de trabajo.
*!*	cId = Iif(Type('cIdOrigen')=='C', cIdOrigen, This.RowID)
*!*	cIdNew = Iif(Type('cIdDestino')=='C', cIdDestino, This.RowIDNew)

*> --------------------------------------------------------------------------------------------------
*> Recupero el movimiento pendiente de reposici�n.
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
*> Si el almacenaje de la ubicaci�n es convencional no se permite intercambiar palets.
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
*> Recupero informaci�n del palet origen.
cWhere = "F16cIdOcup='" + oF14c.F14cIdOcup + "'"
If !f3_sql("*", "F16c", cWhere, , , "F16cOrg")
	This.UsrError = "No existe el palet origen (OC)"
	Return .F.
EndIf

Select F16cOrg
Scatter Name oF16cOrg
*> --------------------------------------------------------------------------------------------------

*> --------------------------------------------------------------------------------------------------
*> Recupero los parametros de reposici�n.
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
*> Recupero informaci�n del palet leido.
cWhere =               "F16cNumPal='" + cNumPal             + "'"
cWhere = cWhere + " And F16cCodUbi='" + oF16cOrg.F16cCodUbi + "'"
cWhere = cWhere + " And F16cCodPro='" + oF16cOrg.F16cCodPro + "'"
cWhere = cWhere + " And F16cCodArt='" + oF16cOrg.F16cCodArt + "'"
cWhere = cWhere + " And F16cSitStk='" + oF16cOrg.F16cSitStk + "'"

*> Aplico la parametrizaci�n.
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
	This.UsrError = "No existe el palet destino (OC)"	&& Palet con las caracter�sticas necesarias no encontrado.
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
*> los MP modificados se selecionar�an en el segundo cursor.
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
*>	- cPropietario, cTipo, cDocumento � bien This.CodPro, This.TipDoc, This.NumDoc
*>	- This.UpdLsC, Actualizar estado lista (S/N).
*>	- This.NumLsC, Lista a la que se a�ade el MP. Se crea si blancos.
*>	- This.AgrTrp, generar lista por transportista (Def=N)
*>	- This.IdentD, etiqueta identificaci�n destino (Def=N)

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLsC, Lista generada, si no exist�a.
*>	- This.UsrError, Texto errores.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creaci�n.

Parameters cPropietario, cTipo, cDocumento
Private cCodPro, cTipDoc, cNumDoc
Private cWhere, cLista
Local lStado

This.UsrError = ""

=This.InitLocals()

*> Asignar par�metros a propiedades.
cCodPro = Iif(Type('cPropietario')=='C', cPropietario, This.CodPro)
cTipDoc = Iif(Type('cTipo')=='C', cTipo, This.TipDoc)
cNumDoc = Iif(Type('cDocumento')=='C', cDocumento, This.NumDoc)

*> Validar el documento.
If ! f3_seek("F24c", '[m.F24cCodPro=cCodPro,m.F24cTipDoc=cTipDoc,m.F24cNumDoc=cNumDoc]')
	This.UsrError = "No existe el documento: " + cNumDoc
	Return .F.
EndIf

*> Validar si el documento ya est� en una lista.
cWhere = "F80lCodPro='" + cCodPro + "' And F80lTipDoc='" + cTipDoc + "' And F80lNumDoc='" + cNumDoc + "'"
lStado = f3_sql("*", "F80l", cWhere, , , "__F80LLCRC")

Use In (Select ("__F80LLCRC"))

If lStado
	This.UsrError = "El documento: " + cNumDoc + " ya est� en lista"
	Return .F.
EndIf

*> Guardar lista en una propiedad.
This.CurLsC = This.NumLsC

*> Crear la cabecera de la lista, si cal.
If Empty(This.CurLsC)
	lStado = This._genlsccab()
EndIf

*> Crear la l�nea de detalle de la lista.
lStado = This._genlscdet()

*> Actualizar el estado de la lista.
lStado = This.ActualizarListaC()

Return lStado

ENDPROC
PROCEDURE _genlsccab

*> Creaci�n de la cabecera de la lista de carga.
*> M�todo protegido, de uso interno.

*> Recibe:
*>	- This.CurLsC, lista actual.
*>	- This.AgrTrp, lista por transportista (S/N)
*>	- This.IdentD, identificaci�n destino (S/N)

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.CurLsC, lista actual.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creaci�n.

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

*> Creaci�n de una l�nea de detalle de la lista de carga..
*> M�todo protegido, de uso interno de la clase.

*> Recibe:
*>	- cPropietario, cTipo, cDocumento � bien This.CodPro, This.TipDoc, This.NumDoc
*>	- This.CurLsC, lista de carga actual.

*> Devuelve:
*>	- Estado (.T. / .F.)

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creaci�n.

Parameters cPropietario, cTipo, cDocumento

Private cWhere
Private cCodPro, cTipDoc, cNumDoc
Local lStado, lIsOpenF80l

*> Asignar par�metros a propiedades.
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
	This.UsrError = "No se puede insertar l�nea lista de carga " + This.CurLsC
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
*>	- cNumeroListaC � bien This.NumLsC, lista de carga a actualizar.
*>	- This.UpdLsC, actualizar estado lista carga (S/N).

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creaci�n.

Parameters cNumeroListaC

Private cNumLsC, cWhere
Local lStado, nTotMov, nTotMovP

If This.UpdLsC<>'S'
	Return
EndIf

lStado = .T.
This.UsrError = ""

*> Tomar la lista recibida, bien como par�metro o bien como propiedad.
cNumLsC = Iif(PCount()==1, cNumeroListaC, This.NumLsC)

*> Validar los datos recibidos.
If Empty(cNumLsC)
	*> Lista vac�a.
	This.UsrError = "La lista de carga est� vac�a"
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

*> N� de movimientos de la lista.
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
*>	- cPropietario, cTipo, cDocumento � bien This.CodPro, This.TipDoc, This.NumDoc
*>	- This.UpdLsC, actualizar estado lista de carga (S/N).
*>	- This.NumLsC, lista a actualizar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto errores.

Parameters cPropietario, cTipo, cDocumento

Private cCodPro, cTipDoc, cNumDoc, cWhere
Local lStado, oF80l

This.UsrError = ""

*> Asignar par�metros a propiedades.
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

*> Actualizaci�n de la cabecera de la lista de carga.

*> Recibe:
*>	- This.CurLsC, lista actual.
*>	- This.AgrTrp, lista por transportista (S/N)
*>	- This.IdentD, identificaci�n destino (S/N)

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.NumLsC, lista actual.
*>	- This.UsrError, mensajes de error.

*> Historial de modificaciones:
*> 06.05.2008 (AVC) Creaci�n.

Local lStado
Private cWhere

This.UsrError = ""
lStado = .T.

If Empty(This.NumLsC)
	This.UsrError = "La lista de carga est� en blanco"
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
*>	- This.TMovExp, tipo movimiento expedici�n (generalmente 2999).
*>	- This.UpdDoc, actualizar estado documento.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 13.05.2008 (AVC) Creaci�n.

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

*> Actualizar el estado de una l�nea de una lista de trabajo.
*> Si el documento est� totalmente confirmado, se elimina de la lista.

*> Recibe:
*>	- cCodPro, cTipDoc, cNumDoc � bien
*>	  This.NumLsC, This.CodPro, This.TipDoc, This.numDoc, documento / lista de carga a actualizar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> Historial de modificaciones:
*> 13.05.2008 (AVC) Creaci�n.

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
	*This.UsrError = "La l�nea no existe"
	
	*>PARA QUE DE MOMENTO NO SALGA EL ODIOSO MENSAJE
	Return .T.
EndIf

*> Cargar los MPs pendientes de confirmaci�n de carga.
cWhere = 		  "F14cCodPro='" + cCodPro + "' And "
cWhere = cWhere + "F14cTipDoc='" + cTipDoc + "' And "
cWhere = cWhere + "F14cNumDoc='" + cNumDoc + "' And F14cTipMov='" + This.TMovExp + "'"

lStado = f3_sql("*", "F14c", cWhere, , , "__F14CLSC")

If lStado
	*> Actualiza estado de la l�nea.
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

*> Confirmar un MP de una lista de trabajo. En funci�n del tipo (preparaci�n, reposici�n),
*> realiza la llamada al m�todo espec�fico para el tratamiento del movimiento.

*> Recibe:
*>	- cNumeroMovimiento � bien This.NumMovLS, n� de MP a confirmar.
*>	- This.FrzCnf, Forzar si el movimiento est� bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedici�n (listas de preparaci�n). (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (listas de preparaci�n). (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (listas de preparaci�n). (usualmente 3000).
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

*> Forzar confirmaci�n, aunque el MP est� bloqueado.
If This.oF26l.F26lFlag1 # Space(1) .And. This.FrzCnf # 'S'
	This.UsrError = "Movimiento bloqueado"
	Return .F.
EndIf

*> Seleccionar el proceso a realizar, en funci�n del tipo de lista.
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
*>	- This.FrzCnf, Forzar si el movimiento est� bloqueado (S/N).
*>	- This.UpdLst, actualizar estado lista (S/N).
*>	- This.TMovExp, Tipo movimiento expedici�n (usualmente 2999).
*>	- This.TMovOrg, Tipo movimiento origen para HM (usualmente 3500).
*>	- This.TMovDst, Tipo movimiento destino para HM (usualmente 3000).
*>	- This.oF26l, Movimiento que contiene los datos de la lista.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, texto errores.

*> M�todo privado, de uso interno de la clase. LLamado desde This.CnfLstMP.

*> Modificaciones:
*>		AVC - 02.06.2005 : Unificar funcionalidad de c�lculo de ubicaci�n destino, si est� en blanco, con
*>						   la clase de reserva, pues este proceso se utilizar� aqu� y en reserva de material.
*>		AVC - 17.06.2005 : Agregar validaci�n de MP con reposiciones pendientes.

Private cWhere, cCampos, cValores, cNumMov, oF26l, dFecMov, cEstMov, cFlag, cTipMov, cRowID, cUbiOri, cUbiDes
Local lStado

This.UsrError = ""

oF26l = This.oF26l
cNumMov = oF26l.F26lNumMov
cCodPro = oF26l.F26lCodPro
cTipDoc = oF26l.F26lTipDoc
cNumDoc = oF26l.F26lNumDoc

*> Validar tipos de movimiento a generar en hist�rico.
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

*> Validar tipo de movimiento expedici�n.
m.F00bCodMov = This.TMovExp
If !f3_seek('F00b')
	This.UsrError = "Tipo movimiento: " + This.TMovExp + " no existe"
	Return .F.
EndIf

*> Ubicaci�n destino por defecto, si cal.
If Empty(oF26l.F26lUbiDes)
	lStado = This.AsignarMuelle(oF26l.F26lCodPro, oF26l.F26lTipDoc, oF26l.F26lNumDoc)
	If !lStado
		*> El mensaje ya viene asignado.
		Return lStado
	EndIf

	oF26l.F26lUbiDes = This.Muelle
EndIf


*> Asignar propiedades a la funci�n de actualizaci�n.
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

This.ActzObj.ActHM				&& Registro en hist�rico.

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

This.ActzObj.ActHM				&& Registro en hist�rico.

*> Actualizar el estado de la l�nea de detalle de la lista.
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

*> Recuperar la ID ocupaci�n destino.
cRowID = This.ActzObj.GetLocals("WRowID")

*> Actualizar TMOV, Ubicaci�n e ID ocupaci�n en el MP.
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

*> Estado de preparaci�n del documento.
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
