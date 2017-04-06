Define Class orafncrstk as custom



PROCEDURE ejecutar

*> Punto de entrada a la clase.
*> Recibe:
*>	- cOperacion ó bien This.PtAcc, acción a realizar.

Parameters cOperacion

Private cPtAcc
Local lStado

lStado = .T.

*> Asignar parámetros a variables locales.
cPtAcc = Iif(PCount()==1, cOperacion, This.PtAcc)

Do Case
	*> Recalcular reserva ocupación por ID ocupación.
	Case cPtAcc=='01'
		lStado = This.RStkCResID()

	*> Recalcular stock reservado de un producto.
	Case cPtAcc=='02'
		lStado = This.RStkCResArt()

	*> Recalcular stock reservado de una ubicación.
	Case cPtAcc=='03'
		lStado = This.RStkCResUbi()

	*> Recalcular stock físico de un producto.
	Case cPtAcc=='04'
		lStado = This.RStkCFisArt()

	*> Recalcular volumen de una ubicación.
	Case cPtAcc=='11'
		lStado = This.RStkVolUbi()

	*> Operación no permitida.
	Otherwise
		This.UsrError = "Operación no permitida"
		lStado = .F.
EndCase

Return lStado


ENDPROC
PROCEDURE rstkcresid

*> Calcular el stock reservado de una ocupación a partir de su ID.
*> Recibe:
*>	- ID ocupación, ó bien This.RowID, identificado de la ocupación.
*>	- This.RSDelE, Borrar si CRES=0 y ubicación es de expedición (Def="N")
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.Cantidad, canntidad reservada, de uso interno.
*>	- This.UsrError, textos error.

Parameters cIdOcupacion

Private cRowID, cWhere, cWhereMP, nCanRes
Local cTipoUbicacion, lStado

*> Pasar parámetros a variables.
cRowID = Iif(PCount()==1, cIdOcupacion, This.RowID)

This.UsrError = ""

cWhere = "F16cIdOcup='" + cRowID + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "CF16C")
If !lStado
	If _xier <= 0
		This.UsrError = "Error leyendo ocupación"
	EndIf

	Use In (Select("CF16C"))
	Return lStado
EndIf

Select CF16C
Go Top

*> Leer la ubicación. Si es de expedición borraremos la ocupación en caso de CRES = 0.
=f3_seek("F10c", '[m.F10cCodUbi=CF16C.F16cCodUbi]')
Select F10c
cTipoUbicacion = F10cPickSN

*> Acumular cantidad reservada, según los MPs.
cWhereMP = "F14cIdOcup='" + cRowID + "' And F14cTipMov Between '2000' and '2999'"
lStado = f3_sql("*", "F14c", cWhereMP, , , "CF14C")
If !lStado
	If _xier <= 0
		This.UsrError = "Error leyendo MPs"
		Use In (Select("CF14C"))
		Use In (Select("CF16C"))
		Return lStado
	EndIf
EndIf

Select CF14C
Sum (F14cCanFis) To nCanRes
If Type('nCanRes') # 'N'
	nCanRes = 0
EndIf

If This.RSDelE=="S" .And. nCanRes <= 0 .And. cTipoUbicacion=='E'
	*> Ubicación de expedición: Borrar la ocupación.
	lStado = f3_deltun('F16c', , cWhere, 'N')
Else
	*> Actualizar la cantidad reservada de la ocupación.
	lStado = f3_updtun("F16c", , "F16cCanRes", "nCanRes", , cWhere, , "N")
EndIf

*> Guardar la cantidad, para uso interno de la clase (This.RStkCResArt).
This.Cantidad = nCanRes

*> Recuperar el entorno de trabajo anterior.
Use In (Select('CF14C'))
Use In (Select('CF16C'))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE inicializar

*> Inicializar las propiedades generales de la clase.

With This
	.RowID = ""					&& Identificador ocupación.
	.RSDelE = "N"				&& Borrar ocupación expedición si cantidad es cero.
	.RSDelS = "S"				&& Borrar stock (F13C) si cantidad es cero.
	.RSStkR = "2000"			&& SSTK reservado por defecto.

	.RsCPro = ""				&& Propietario.
	.RsCArt = ""				&& Artículo.
	.RsCUbi = ""				&& Ubicación.

	.UsrError = ""				&& Texto errores.
EndWith

Return

ENDPROC
PROCEDURE rstkcresart

*> Calcular el stock reservado de un artículo.
*> Calcula también el stock reservado de cada ocupación del artículo.
*>
*> Recibe:
*>	- cPropietario, ó bien This.RsCPro.
*>	- cArticulo, ó bien This.RsCArt.
*>	- This.RSDelS, Borrar stock si cantidad=0 (Def="S")
*>	- This.RSDelE, Borrar si CRES=0 y ubicación es de expedición (Def="N")
*>	- This.RSStkR, Situación de stock reservado (Def="2000")
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, textos error.

Parameters cPropietario, cArticulo

Private cPropietario, cArticulo, cWhere, cWhereF13c, oF16c
Local lStado, lStadoS

*> Pasar parámetros a variables.
cPropietario = Iif(Type('cPropietario')=='C', cPropietario, This.RsCPro)
cArticulo = Iif(Type('cArticulo')=='C', cArticulo, This.RsCArt)

This.UsrError = ""

*> Validar la SSTK de reservado.
m.F00cCodStk=This.RsStkR
If !f3_seek("F00c")
	This.UsrError = "Situación de stock: " + This.RsStkR + " no definida"
	Return .F.
EndIf

*> Cursor de trabajo para stocks.
=CrtCursor("F13c", "F13cStkR", "C")

*> Cargar el stock del producto.
cWhereF13c = "F13cCodAlm='" + _Alma + "' And " + ;
			 "F13cCodPro='" + cPropietario + "' And F13cCodArt='" + cArticulo + "' And " + ;
			 "F13cSitStk='" + This.RsStkR + "'"

lStadoS = f3_sql("*", "F13c", cWhereF13c, , , "F13cStkR")
If _xier <= 0
	This.UsrError = "Error en carga stock reservado"
	Use In (Select("F13cStkR"))
	Return .F.
EndIf

*> Crear registro para acumulados, si cal.
Select F13cStkR

If !lStadoS
	Append Blank
	Replace F13cCodAlm With _Alma, ;
	        F13cCodPro With cPropietario, ;
	        F13cCodArt With cArticulo, ;
	        F13cSitStk With This.RsStkR
EndIf

Replace F13cCantid With 0

*> Cargar las ocupaciones del producto.
cWhere = "F16cCodPro='" + cPropietario + "' And F16cCodArt='" + cArticulo + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "F16cStkR")
If _xier <= 0
	This.UsrError = "Error en carga ocupaciones"
	Use In (Select("F13cStkR"))
	Return .F.
EndIf

Select F16cStkR
Go Top
Do While !Eof()
	Scatter Name oF16c

	*> Calcular el stock reservado de la ocupación.
	lStado = This.RStkCResID(oF16c.F16cIdOcup)
	If !lStado
		*> Ya vuelve con el texto del error.
		Use In (Select("F13cStkR"))
		Use In (Select("F16cStkR"))
		Return lStado
	EndIf

	*> Actualizar cantidad reservada actual.
	Select F13cStkR
	Replace F13cCantid With F13cCantid + This.Cantidad

	Select F16cStkR
	Skip
EndDo

*> Actualizar acumulados de stock reservado del artículo.
Select F13cStkR
Go Top

Do Case
	Case F13cCantid <= 0 .And. This.RsDelS=="S"
		=f3_deltun("F13c", , cWhereF13c)

	Case lStadoS
		=f3_updtun("F13c", , , , "F13cStkR", cWhereF13c)

	Case !lStadoS .And. F13cCanFis > 0
		=f3_instun("F13c", "F13cStkR")
EndCase

Use In (Select("F13cStkR"))
Use In (Select("F16cStkR"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE rstkcresubi

*> Calcular el stock reservado de una ubicación.
*> Calcula también el stock reservado de cada ocupación de la ubicación.
*>
*> Recibe:
*>	- cUbicacion, ó bien This.RsCUbi.
*>	- This.RSDelE, Borrar si CRES=0 y ubicación es de expedición (Def="N")
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, textos error.

Parameters cUbicacion

Private cUbicacion, cWhere, oF16c
Local lStado, lStadoS

*> Pasar parámetros a variables.
cUbicacion = Iif(Type('cUbicacion')=='C', cUbicacion, This.RsCUbi)

This.UsrError = ""

*> Validar la SSTK de reservado.
m.F00cCodStk=This.RsStkR
If !f3_seek("F00c")
	This.UsrError = "Situación de stock: " + This.RsStkR + " no definida"
	Return .F.
EndIf

*> Cargar las ocupaciones de la ubicación.
cWhere = "F16cCodUbi='" + cUbicacion + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "F16cStkR")
If _xier <= 0
	This.UsrError = "Error en carga ocupaciones"
	Use In (Select("F13cStkR"))
	Return .F.
EndIf

Select F16cStkR
Go Top
Do While !Eof()
	Scatter Name oF16c

	*> Calcular el stock reservado de la ocupación.
	lStado = This.RStkCResID(oF16c.F16cIdOcup)
	If !lStado
		*> Ya vuelve con el texto del error.
		Use In (Select("F16cStkR"))
		Return lStado
	EndIf

	Select F16cStkR
	Skip
EndDo

Use In (Select("F16cStkR"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE initlocals

*> Inicializar las propiedades protegidas de la clase.

With This
	.Cantidad = 0
EndWith

Return

ENDPROC
PROCEDURE rstkcfisart

*> Calcular el stock físico de un artículo.
*>
*> Recibe:
*>	- cPropietario, ó bien This.RsCPro.
*>	- cArticulo, ó bien This.RsCArt.
*>	- This.RSDelS, Borrar stock si cantidad=0 (Def="S")
*>	- This.RSStkR, Situación de stock reservado (Def="2000")
*>
*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, textos error.

Parameters cPropietario, cArticulo

Private cPropietario, cArticulo, cWhere, oF16c
Local lStado

*> Pasar parámetros a variables.
cPropietario = Iif(Type('cPropietario')=='C', cPropietario, This.RsCPro)
cArticulo = Iif(Type('cArticulo')=='C', cArticulo, This.RsCArt)

This.UsrError = ""

*> Cursor de trabajo para stocks.
=CrtCursor("F13c", "F13cStkF", "C")

*> Cargar el stock del producto, excepto reservado.
cWhere = "F13cCodAlm='" + _Alma + "' And " + ;
		 "F13cCodPro='" + cPropietario + "' And F13cCodArt='" + cArticulo + "' And " + ;
		 "F13cSitStk<>'" + This.RsStkR + "'"

lStadoS = f3_sql("*", "F13c", cWhere, , , "F13cStkF")
If _xier <= 0
	This.UsrError = "Error en carga stocks"
	Use In (Select("F13cStkF"))
	Return .F.
EndIf

*> Generar el resto de situaciones de stock. Se excluye la de stock reservado.
=f3_top("F00c")

Do While !Eof()
	If F00c.F00cCodStk # This.RsStkR
		Select F13cStkF
		Locate For F13cSitStk==F00c.F00cCodStk

		If !Found()
			Append Blank
			Replace F13cCodAlm With _Alma, ;
			        F13cCodPro With cPropietario, ;
			        F13cCodArt With cArticulo, ;
			        F13cSitStk With F00c.F00cCodStk
		EndIf
	EndIf

	*> Carga la siguiente situación de stock.
	Select F00c
	Scatter MemVar
	=f3_pos("F00c")
EndDo

*> Inicializar cantidades en cursor de stock de trabajo.
Select F13cStkF
Replace All F13cCantid With 0

*> Cargar las ocupaciones del producto.
cWhere = "F16cCodPro='" + cPropietario + "' And F16cCodArt='" + cArticulo + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "F16cStkF")
If _xier <= 0
	This.UsrError = "Error en carga ocupaciones"
	Use In (Select("F13cStkF"))
	Return .F.
EndIf

Select F16cStkF
Go Top
Do While !Eof()
	Scatter Name oF16c

	*> Actualizar cantidad en acumulado stock por situación.
	Select F13cStkF
	Locate For F13cSitStk==oF16c.F16cSitStk
	If !Found()
		*> Esto es imposible !!!
		Use In (Select("F13cStkF"))
		Use In (Select("F16cStkF"))
		This.UsrError = "Error imposible actualizando stock"
		Return .F.
	EndIf

	Replace F13cCantid With F13cCantid + oF16c.F16cCanFis

	Select F16cStkF
	Skip
EndDo

*> Actualizar acumulados de stock reservado del artículo.
Select F13cStkF
Go Top
Do While !Eof()
	*> Buscar registro de stocks.
	Scatter MemVar
	lStado = f3_seek("F13c")

	cWhere = "F13cCodAlm='" + F13cStkF.F13cCodAlm + "' And " + ;
			 "F13cCodPro='" + cPropietario + "' And F13cCodArt='" + cArticulo + "' And " + ;
			 "F13cSitStk='" + F13cStkF.F13cSitStk + "'"

	Do Case
		Case lStado .And. F13cStkF.F13cCantid <= 0 .And. This.RsDelS=="S"
			=f3_deltun("F13c", , cWhere)

		Case lStado
			=f3_updtun("F13c", , , , "F13cStkF", cWhere)

		Case !lStado .And. F13cStkF.F13cCantid > 0
			=f3_instun("F13c", "F13cStkF")
	EndCase

	Select F13cStkF
	Skip
EndDo

Use In (Select("F13cStkF"))
Use In (Select("F16cStkF"))

lStado = .T.
Return lStado

ENDPROC
PROCEDURE rstkvolubi

*> Calcular el volumen de una ubicación.

*> Recibe:
*>	- cUbicacion, ó bien This.RsCUbi.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, textos error.

*> Historial de modificaciones:
*>	- Calcular peso / volumen con la clase OraFncUbic. AVC - 04.01.2006

Parameters cUbicacion

Private cUbicacion, cWhere, cOrder, oF16c, cCampos
Local lStado, nOcupaciones, vOcupacion

*> Pasar parámetros a variables.
cUbicacion = Iif(Type('cUbicacion')=='C', cUbicacion, This.RsCUbi)

This.UsrError = ""

*> Cargar la ubicación a calcular.
If !f3_seek("F10c", '[m.F10cCodUbi=cUbicacion]')
	This.UsrError = "La ubicación: " + cUbicacion + " no existe"
	Return .F.
EndIf

*> Inicializar los valores a actualizar de la ubicación.
Select F10c
Go Top
Replace F10cNumOcu With 0, ;
        F10cPesOcu With 0, ;
        F10cVolLib With F10cVolTot

*> Cargar ls ocupaciones de la ubicación.
cWhere = "F16cCodUbi='" + cUbicacion + "'"
cCampos = "F16cCodPro, F16cCodArt, F16cCanFis"
lStado = f3_sql(cCampos, "F16c", cWhere, , , "F16cRVol")
If _xier <= 0
	This.UsrError = "Error al cargar las ocupaciones"
	Return .F.
EndIf

*> Agregar los MPs de entrada. OJO !! Se utiliza el mismo cursor.
cWhere = "F14cUbiOri='" + cUbicacion + "' And " + _GCSS("F14cTipMov", 1, 1) + "='1'"
cCampos = "F14cCodPro As F16cCodPro, F14cCodArt As F16cCodArt, F14cCanFis As F16cCanFis"

lStado = f3_sql(cCampos, "F14c", cWhere, , , "+F16cRVol")
If _xier <= 0
	This.UsrError = "Error al cargar los MPs de entrada"
	Return .F.
EndIf

Select F16cRVol
Go Top
_TotReg = RecCount()

If _TotReg > 3
	Return .T.
EndIf

Select F16cRVol
Go Top
Do While !Eof()
	Scatter Name oF16c

	*> Cargar datos artículo.
	If !f3_seek("F08c", '[m.F08cCodPro=oF16c.F16cCodPro,m.F08cCodArt=oF16c.F16cCodArt]')
		Select F08c
		Append Blank
		Replace F08cPesuni With 0, ;
				F08cVolUni With 0
	EndIf

	*> Calcular peso y volumen.
	With This.oUbicObj
		.Inicializar
		.CargaPrm

		.CodPro = oF16c.F16cCodPro
		.CodArt = oF16c.F16cCodArt
		.CanUbi = oF16c.F16cCanFis
		.PesVolAr

		Select F10c
		Replace F10cNumOcu With F10cNumOcu + 1, ;
				F10cPesOcu With F10cPesOcu + .PesOcu, ;
				F10cVolLib With F10cVolLib - .VolOcu
	EndWith

	Select F16cRVol
	Skip
EndDo

*> Corregir posibles errores de desbordamiento.
Select F10c
Replace F10cPesOcu With Iif(F10cPesOcu > F10cPesTot, F10cPesTot, F10cPesOcu)
Replace F10cPesOcu With Iif(F10cPesOcu < 0, 0, F10cPesOcu)
Replace F10cVolLib With Iif(F10cVolLib < 0, 0, F10cVolLib)
*Replace F10cNumOcu With Iif(F10cNumOcu > F10cMaxPal, F10cMaxPal, F10cNumOcu)

*> Actualiza el estado de la ubicación, solo aquellas que NO estén bloqueadas.
If F10cestGen # 'B'
    Replace F10cEstGen With Iif(F10cNumOcu > 0, 'O', 'L')
EndIf

*> Actualizar la ubicación.
lStado = f3_updtun("F10c")

If !lStado
	This.UsrError = "Error al actualizar peso / volumen"
EndIf

*> Recuperar el entorno de trabajo anterior.
Use In (Select('F16cRVol'))

Return lStado

ENDPROC
PROCEDURE oubicobj_access

*> Inicializar clase ubicación
If Type('This.oUbicObj') # 'O'
	This.oUbicObj = CreateObject('OraFncUbic')
EndIf

Return This.oUbicObj

ENDPROC


EndDefine 
