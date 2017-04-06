**********************************************************************************************
****** Este módulo es el encargado de realizar listados de facturación. ************************
******						(a incorporar a LISTADOS.PRG)				************************
************************************************************************************************

*>                                 GenPalEnt          (Relación de palets entrados - Generar)
*>                                 PrtPalEnt          (Relación de palets entrados - Imprimir)
*>                                 GenPalEntDia       (Relación de palets entrados/día - Generar)
*>                                 PrtPalEntDia       (Relación de palets entrados/dá - Imprimir)
*>                                 GenPalEntDet       (Relación de contenido de palets entrados - Generar)
*>                                 PrtPalEntDet       (Relación de contenido de palets entrados - Imprimir)
*>                                 GenPalSal          (Relación de palets salidos - Generar)
*>                                 PrtPalSal          (Relación de palets salidos - Imprimir)
*>                                 GenPalSalDet       (Relación de contenido de palets salidos - Generar)
*>                                 PrtPalSalDet       (Relación de contenido de palets salidos - Imprimir)
*>                                 GenPalFac          (Diario de palets facturación - Generar)
*>                                 PrtPalFac          (Diario de palets facturación - Imprimir)
*>                                 GenPalEntSal       (Relación de entradas/salidas palets - Generar)
*>                                 PrtPalEntSal       (Relación de entradas/salidas palets - Imprimir)
*>
*>								   ValGenSeguroMes    (Relación Seguro)
*>                                 GenSeguroMes       (Relación aseguradora/mes - Generar)
*>                                 PrtSeguroMes       (Relación aseguradora/mes - Imprimir)
*>                                 ObtenerSaldoDia    (Calcular saldo de un día concreto)
*>                                 ObtenerEntradasDia (Calcular entradas de un día concreto)
*>                                 ObtenerSalidasDia  (Calcular salidas de un día concreto)

*>                                 GenTarF            (Aplicar Tarifa Almacenaje - Generar)
*>                                 PrtTarF            (Aplicar Tarifa Almacenaje - Imprimir)

*>                                 GenFraD            (Listado conceptos directos facturación - Generar)
*>                                 PrtFraD            (Listado conceptos directos facturación - Imprimir)
*>                                 GenFraM            (Listado conceptos manuales facturación - Generar)
*>                                 PrtFraM            (Listado conceptos manuales facturación - Imprimir)

*>                                 SelDiarF           (Diario de Facturación - Selección)
*>                                 SelDiarI           (Diario de I. V. A. - Selección)
*>                                 GenDiarF           (Diario de Facturación - Generar)
*>                                 GenDiarI           (Diario de I. V. A. - Generar)
*>                                 PrtDiarF           (Diario de Facturación - Imprimir)
*>                                 PrtDiarI           (Diario de I. V. A. - Imprimir)

*>---------------------------------------------------------------------------------------
*> Proceso para imprimir UNA factura.
*>---------------------------------------------------------------------------------------
Procedure FacList

   Parameters NumFac

   Select FRACAB
   Locate For Marca==1
   Do While Found()

      *> Generar datos de ESTA factura.
      Do GenCursor With F70cNumFac

      *> Leer la siguiente factura.
      Select FRACAB
      Continue
   EndDo      

   *> Imprimir las facturas
   Do PrtFactura
   Use In (Select('FACCURSOR'))

Return
	
*>---------------------------------------------------------------------------------------
*> Generar datos al cursor que se enviará al report de impresión de facturas.
*>---------------------------------------------------------------------------------------
PROCEDURE GenCursor

    Parameters NumFac
    Private cSelec, cWhere, cFromF, cOrder, cGroup, oFAC, oF34v, oF00j, oF70o, oEMP
	Local lStado

	*> Cursor de trabajo:
	*>	- Detalle factura.
	*>	- Condiciones de pago.
	*>	- Acumulados factura.
	*>	- Ficha propietario.
	=CrtMCursor("F70l,F70p,F70o,F70i,F01p,F01c", "FACCURSOR")

	*> Agregar campos de cálculo del detalle de factura.
	=AddFldToCursor("FACCURSOR", [NAME=F70lImpLin,TYPE=N,LENGTH=13,DECIMALS=3])

	*> Agregar campos de:
	*>	- Cabecera de factura.
	*>	- Datos IVA.
	*>	- Provincia.
	*>	- Unidad.
	=CrtFCursor("FACCURSOR", [TBL=F70c,FLD=F70cCodPro,FLD=F70cNumFac,FLD=F70cFecFac])
	=CrtFCursor("FACCURSOR", [TBL=F34v,FLD=F34vValIva,FLD=F34vValEqv,TBL=F00j,FLD=F00jDescri,TBL=F34n,FLD=F34nDescri])

	*> Cargar los datos generales de la factura.
	cFromF = "F70c,F70i,F70p,F70l,F34n,F01p"

	cWhere = 		  "F70cNumFac='" + NumFac + "' And "
	cWhere = cWhere + "F70lNumFac=F70cNumFac And F70pNumFac=F70cNumFac And F70iNumFac=F70cNumFac And "
	cWhere = cWhere + "F34nCodCon=F70lCodCon And "
	cWhere = cWhere + "F01pCodigo=F70cCodPro"

    cOrder = "F70cNumFac,F70lCodCon,F70lCodigo,F70lCodTrm"
    cGroup = ""

	lStado = f3_sql("*", cFromF, cWhere, cOrder, cGroup, "FACCURSOR")
	If !lStado
		*> No hay datos.
		Use In (Select ("FACCURSOR"))
		Return lStado
	EndIf

	Select FACCURSOR
	Go Top
	Do While !Eof()
		Scatter Name oFAC

		*> Observaciones de la factura.
		m.F70oNumFac = NumFac
		=f3_seek("F70o")
		Select F70o
		Scatter Name oF70o

		*> Agregar los datos de IVA.
		m.F34vCodCon = oFAC.F70lCodIva
		=f3_seek("F34v")
		Select F34v
		Scatter Name oF34v

		*> Descripción de la provincia.
		m.F00jCodPrv = oFac.F01pProvin
		If f3_seek("F00j")
			Select F00j
			Scatter Name oF00j
		Else
			Select F00j
			Scatter Name oF00j Blank
		EndIf

		*> Datos de la empresa.
		cWhere = "F01cTipEnt='EMPR'"
		If f3_sql("*", "F01c", cWhere, , , "F01cFACT")
			Select F01cFACT
			Go Top
			Scatter Name oEMP
		Else
			Select F01cFACT
			Scatter Name oEMP Blank
		EndIf

		Select FACCURSOR
		Gather Name oF34v
		Gather Name oF00j
		Gather Name oF70o
		Gather Name oEMP

		Replace F70lImpLin With oFAC.F70lBasImp + oFAC.F70lImpIva + oFAC.F70lImpEqv

		Skip
	EndDo

    *> Crear, si no existe, el cursor que se enviará al report.
    If !Used('_XLFacCur')
    	=CrtCursor('FACCURSOR', '_XLFacCur', 'C')
    EndIf

    *> Añadir la factura actual al cursor que se envía al report.
    Select _XLFacCur
    Append From Dbf('FACCURSOR')

    Use In (Select ("FACCURSOR"))
    Use In (Select ("F01cFACT"))

Return

*>---------------------------------------------------------------------------------------
*> Proceso de impresión de facturas propiamente dicho.
*> Se le llama desde: FACEDIF ----> Edición de facturas. (Antes FCMEDIFAC).
*>                    FACPRTF ----> Impresión de facturas con rangos.
*>---------------------------------------------------------------------------------------
Procedure PrtFactura

	Private _NullDisplay, _Label1, loImp

    _NullDisplay = Set('NULLDISPLAY')
    Set NullDisplay To Space(1)
    _Label1 = "IMPRESION DE FACTURAS"

    If Used('_XLFacCur')
        Select _XLFacCur
        Go Top

		*> Objeto impresión.
		loImp2 = CreateObject('st3imp2', , [CR=_XLFacCur, RPT=FcmPrtFL1])
		=loImp2.Exec('?', [LBL=&_Label1])
		loImp2.Release

        Use In _XLFacCur
    Else
       _LxErr = 'No hay facturas para imprimir'
       Do Form St3Inc With .T.
    EndIf

    Set NullDisplay To _NullDisplay

Return

************************************************************************
*> Impresión del Diario de Facturación - Selección.                    *
*> Primer paso: Selección facturas Propietario/Periodo.                *
*>                                                                     *
*> Recibe: cCodPro -----> Propietario.                                 *
*>         dFechaD -----> Fecha inicial.                               *
*>         dFechaF -----> Fecha final.                                 *
*>                                                                     *
*> Genera: XLGENDIAF --> Cursor con los datos a imprimir.              *
*>                                                                     *
*>  Llama: GenDiarF.                                                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmDiarF ----> Impresión Diario de Facturación.                 *
************************************************************************
Procedure SelDiarF
Parameters cCodPro, dFechaDesde, dFechaHasta

Private cSelec, cFromF, cWhere
Local lEstado

Store .T. To lEstado

If Used('F70cSel')
   Use In F70cSel
EndIf

cWhere = "F70cCodPro='" + cCodPro + "' And " + ;
         "F70cFecFac Between " + ;
         _GCD(dFechaDesde) + " And " + _GCD(dFechaHasta)

lEstado = f3_sql("*", "F70c", cWhere, , , "F70cSel")
If !lEstado
   *> No hay datos.
   Use In (Select ("F70cSel"))
   Return
EndIf

*> Generar datos de las facturas a cursor de trabajo.
Select F70cSel
Go Top
Do While !Eof()
   =GenDiarF(F70cNumFac)

   Select F70cSel
   Skip
EndDo

Use In (Select ("F70cSel"))
Return lEstado

************************************************************************
*> Impresión del Diario de Facturación - Generar.                      *
*> Segundo paso: Generar datos.                                        *
*>                                                                     *
*> Recibe: cNumFac -----> Número de Factura                            *
*>                                                                     *
*> Genera: XLGENDIAF --> Cursor con los datos a imprimir.              *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmDiarF ----> Impresión Diario de Facturación.                 *
************************************************************************
Procedure GenDiarF
Parameters cNumFac

Private cSelec, cFromF, cWhere, cOrder, cGroup, cField
Local nConcepto

   *> Leer datos factura.
   cSelec = "*"
   cFromF = "F70c,F70i,F70l,F34n,F01p,F70p"
   cOrder = "F70cNumFac"
   cGroup = ""

   *> Clave principal de lectura de la factura.
   cWhere = "F70cNumFac='" + cNumFac + "' And F70lNumFac=F70cNumFac And F70iNumFac=F70cNumFac"

   *> Clave secundaria de relación con tabla de conceptos.
   cWhere = cWhere + " And F34nCodCon=F70lCodCon"
   
   *> Clave secundaria de relación con tabla de propietarios.
   cWhere = cWhere + " And F01pCodigo=F70cCodPro"

   *> Clave secundaria de relación con tabla de condiciones de pago.
   cWhere = cWhere + " And F70pNumFac=F70cNumFac"

   If !f3_sql(cSelec, cFromF, cWhere, cOrder, cGroup, '+XLGENDIAFCUR')
      _LxErr = 'No se han podido cargar datos para imprimir. Factura: ' + cNumFac
      Do Form St3Inc With .T.
      Return
   EndIf

Return

************************************************************************
*> Impresión del Diario de Facturación - Imprimir.                     *
*> Tercer paso: Imprimir datos.                                        *
*>                                                                     *
*> Recibe:                                                             *
*>	- XLGENDIAF --> Cursor con los datos a imprimir.                   *
*>	- cTipoDiario:                                                     *
*>		1: Diario facturación.                                         *
*>		3: Diario facturación por cliente.                             *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmDiarF ----> Impresión Diario de Facturación.                 *
************************************************************************

*> Historial de modificaciones:
*> 27.05.2008 (AVC) Clasificar por fectura.

Procedure PrtDiarF
Parameters cTipoDiario

	Private loImp, _Label1, cRpt, oCUR, oFAC
	Local lPrimera, cFileSort

	Do Case
		*> Por defecto: Relación de facturas.
		Case Type('cTipoDiario')<>'C'
			cRpt = "FcmDiarFL1"

		*> Relación de facturas.
		Case cTipoDiario=='1'
			cRpt = "FcmDiarFL1"

		*> Relación de facturas por cliente.
		Case cTipoDiario=='3'
			cRpt = "FcmDiarFL3"

		Otherwise
			cRpt = "FcmDiarFL1"
	EndCase

	If Used('XLGENDIAFCUR')
		=CrtCursor('XLGENDIAFCUR', 'XLGENDIAF', 'C')

		*> Generar los acumulados.
		Select XLGENDIAFCUR
		Go Top
		Scatter Name oCUR
		lPrimera = .T.

		Do While !Eof()
			Scatter Name oFAC

			If oCUR.F70cNumFac<>oFAC.F70cNumFac
				Select XLGENDIAF
				Append Blank
				Gather Name oCUR
				oCUR = oFAC
				lPrimera = .T.
			EndIf

			If !lPrimera
				oCUR.F70lBasImp = oCUR.F70lBasImp + oFAC.F70lBasImp
				oCUR.F70lImpIva = oCUR.F70lImpIva + oFAC.F70lImpIva
				oCUR.F70lImpEqv = oCUR.F70lImpEqv + oFAC.F70lImpEqv
			EndIf

			lPrimera = .F.
			Select XLGENDIAFCUR
			Skip
		EndDo

		Select XLGENDIAF				&& Agregar el último acumulado.
		Append Blank
		Gather Name oCUR

		*> Ordenar el listado por Factura.
		cFileSort = AllTrim(Sys(2015)) + ".DBF"
		If cTipoDiario=='1' Or cTipoDiario=='2'
			Sort To (cFileSort) On F70cNumFac /A
		Else
			Sort To (cFileSort) On F70cCodPro, F70cNumFac /A
		EndIf

		Zap
		Append From (cFileSort)
		Delete File (cFileSort)

		*> Impresión.
		Go Top
		_Label1 = "DIARIO DE FACTURACION"

		*> Objeto impresión.
		loImp2 = CreateObject('st3imp2', , [CR=XLGENDIAF, RPT=&cRpt])
		=loImp2.Exec('?', [LBL=&_Label1])
		loImp2.Release
	Else
		_LxErr = "No hay datos para imprimir"
		Do Form St3Inc With .T.
	EndIf

	*> Cerrar cursores generados por SQL.
	Use In (Select ('XLGENDIAF'))
	Use In (Select ('XLGENDIAFCUR'))

Return

************************************************************************
*> Impresión del Diario de I.V.A. - Selección.                         *
*> Primer paso: Selección facturas Propietario/Periodo.                *
*>                                                                     *
*> Recibe: cCodPro -----> Propietario.                                 *
*>         dFechaD -----> Fecha inicial.                               *
*>         dFechaF -----> Fecha final.                                 *
*>                                                                     *
*> Genera: XLGENDIAF --> Cursor con los datos a imprimir.              *
*>                                                                     *
*>  Llama: GenDiarF.                                                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmDiarF ----> Impresión Diario de Facturación.                 *
************************************************************************

Procedure SelDiarI
Parameters cCodPro, dFechaDesde, dFechaHasta

Private cSelec, cFromF, cWhere
Local lEstado

Store .T. To lEstado

If Used('F70cSel')
   Use In F70cSel
EndIf

cWhere = "F70cCodPro='" + cCodPro + "' And " + ;
         "F70cFecFac Between " + ;
         _GCD(dFechaDesde) + " And " + _GCD(dFechaHasta)

lEstado = f3_sql("*", "F70c", cWhere, , , "F70cSel")
If !lEstado
   *> No hay datos.
   Use In (Select ("F70cSel"))
   Return
EndIf

*> Generar datos de las facturas a cursor de trabajo.
Select F70cSel
Go Top
Do While !Eof()
   =GenDiarI(F70cNumFac)

   Select F70cSel
   Skip
EndDo

Use In (Select ("F70cSel"))
Return

************************************************************************
*> Impresión del Diario de I.V.A. - Generar.                           *
*> Segundo paso: Generar datos.                                        *
*>                                                                     *
*> Recibe: cNumFac -----> Número de Factura                            *
*>                                                                     *
*> Genera: XLGENDIAF --> Cursor con los datos a imprimir.              *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmDiarF ----> Impresión Diario de Facturación.                 *
************************************************************************
Procedure GenDiarI
Parameters cNumFac

Private cSelec, cFromF, cWhere, cOrder, cGroup, cField
Local nConcepto

   *> Leer datos factura.
   cSelec = "*"
   cFromF = "F70c,F70i,F70l,F34n,F01p,F70p,F34v"
   cOrder = "F70cNumFac"
   cGroup = ""

   *> Clave principal de lectura de la factura.
   cWhere = "F70cNumFac='" + cNumFac + "' And F70lNumFac=F70cNumFac And F70iNumFac=F70cNumFac"

   *> Clave secundaria de relación con tabla de conceptos.
   cWhere = cWhere + " And F34nCodCon=F70lCodCon"
   
   *> Clave secundaria de relación con tabla de propietarios.
   cWhere = cWhere + " And F01pCodigo=F70cCodPro"

   *> Clave secundaria de relación con tabla de condiciones de pago.
   cWhere = cWhere + " And F70pNumFac=F70cNumFac"

   *> Clave secundaria de relación con tabla de impuestos.
   cWhere = cWhere + " And F34vCodCon=F70lCodIva"

   If !f3_sql(cSelec, cFromF, cWhere, cOrder, cGroup, '+XLGENDIAFCUR')
      _LxErr = 'No se han podido cargar datos para imprimir. Factura: ' + cNumFac
      Do Form St3Inc With .T.
      Return
   EndIf

Return

************************************************************************
*> Impresión del Diario de I.V.A. - Imprimir.                          *
*> Tercer paso: Imprimir datos.                                        *
*>                                                                     *
*> Recibe: XLGENDIAF --> Cursor con los datos a imprimir.              *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmDiarF ----> Impresión Diario de Facturación.                 *
************************************************************************

*> Historial de modificaciones:
*> 27.05.2008 (AVC) Clasificar por fectura.

Procedure PrtDiarI

	Private loImp, _Label1, cRpt, oCUR, oFAC
	Local lPrimera, cFileSort

	cRpt = "FcmDiarFL2"

	If Used('XLGENDIAFCUR')
		=CrtCursor('XLGENDIAFCUR', 'XLGENDIAF', 'C')

		*> Generar los acumulados.
		Select XLGENDIAFCUR
		Go Top
		Scatter Name oCUR
		lPrimera = .T.

		Do While !Eof()
			Scatter Name oFAC

			If oCUR.F70cNumFac<>oFAC.F70cNumFac .Or. oCUR.F70lCodIva<>oFAC.F70lCodIva
				Select XLGENDIAF
				Append Blank
				Gather Name oCUR
				oCUR = oFAC
				lPrimera = .T.
			EndIf

			If !lPrimera
				oCUR.F70lBasImp = oCUR.F70lBasImp + oFAC.F70lBasImp
				oCUR.F70lImpIva = oCUR.F70lImpIva + oFAC.F70lImpIva
				oCUR.F70lImpEqv = oCUR.F70lImpEqv + oFAC.F70lImpEqv
			EndIf

			lPrimera = .F.
			Select XLGENDIAFCUR
			Skip
		EndDo

		Select XLGENDIAF				&& Agregar el último acumulado.
		Append Blank
		Gather Name oCUR

		*> Ordenar el listado por Factura.
		cFileSort = AllTrim(Sys(2015)) + ".DBF"
		Sort To (cFileSort) On F70cNumFac /A
		Zap
		Append From (cFileSort)
		Delete File (cFileSort)

		*> Impresión.
		Go Top
		_Label1 = "DIARIO DE I. V. A."

		*> Objeto impresión.
		loImp2 = CreateObject('st3imp2', , [CR=XLGENDIAF, RPT=&cRpt])
		=loImp2.Exec('?', [LBL=&_Label1])
		loImp2.Release
	Else
		_LxErr = "No hay datos para imprimir"
		Do Form St3Inc With .T.
	EndIf

	*> Cerrar cursores generados por SQL.
	Use In (Select ('XLGENDIAF'))
	Use In (Select ('XLGENDIAFCUR'))
	Return
	
*>---------------------------------------------------------------------------------------
*> Generar datos al cursor que se enviará al report auxiliar.
*>---------------------------------------------------------------------------------------
PROCEDURE GenCursorDocE

    Parameters CodPro, TipDoc, NumDoc

	private lx_sql, lx_select, f_from, f_where, f_order, err

	f_select  = " SELECT F35eCodPro, F01pDescri, F35eFecDoc, F35eNumDoc, F35eNumLin, " + ;
						"F35eTotPal, F35eTotFra, F35eUniFra"

	f_from    = " FROM F35e" + _em + ", F01p" + _em
	
	f_where   = " WHERE  F35eCodPro='" + CodPro + "'" + ;
                " And    F35eTipDoc='" + TipDoc + "'" + ;
                " And    F35eNumDoc='" + NumDoc + "'" + ;
                " And    F01pCodigo=F35eCodPro"

    f_sql = f_select + f_from + f_where
	err=sqlexec(_ASql, f_sql, 'RELDOCE')
	=sqlmoreresults(_asql)

	if err<0
	=f3_sn(1, 1, "Error al generar listado auxiliar (Entradas)")
		return
	endif

	*> Mirar si está vacío
	Select RELDOCE
	Go Top
	If Eof()
       Use In RELDOCE
       Return
    EndIf

    *> Añadir los datos al cursor que se enviará al report.
    If !Used('_XLCurDocE')
       =CrtCursor('RELDOCE', '_XLCurDocE', 'C')
    EndIf
    Select _XLCurDocE
    Append From Dbf('RELDOCE')
    Use In RELDOCE

Return

*>---------------------------------------------------------------------------------------
*> Proceso de listado auxiliar de documentos de entrada propiamente dicho.
*> Se le llama desde: FCMRELDOCE ---> Listado auxiliar de Documentos de Entrada.
*>---------------------------------------------------------------------------------------
Procedure PrtDocE

    _label1 = "LISTADO AUXILIAR FACTURACION (ENTRADAS)"
    If Used('_XLCurDocE')
        Select _XLCurDocE
        Go Top
        Do Form St3Imp2 With _Label1, 'FcmRdeL1', '_XLCurDocE'
        Use In _XLCurDocE
    Else
       =f3_sn(1, 1, "No hay Documentos de Entrada para imprimir")    
    EndIf

Return

*>---------------------------------------------------------------------------------------
*> Seleccionar los documentos de salida de F35s para listado auxiliar.
*>---------------------------------------------------------------------------------------
PROCEDURE FacDocS

   Select FRADOCS
   Go Top
   Do While !Eof()
      If marca <> 1
         Skip
         Loop
      EndIf 

      *> Generar datos de ESTE Documento de Salida.
      Do GenCursorDocS With F35sCodPro, F35sTipDoc, F35sNumDoc

      *> Leer el siguiente Documento de Salida.
      Select FRADOCS
      Skip
      Loop
   EndDo      

   *> Imprimir el listado auxiliar de documentos de salida.
   Do PrtDocS

   If Used('FRADOCS')
      Use In FRADOCS
   EndIf
   Select SysPrg

Return
	
*>---------------------------------------------------------------------------------------
*> Generar datos al cursor que se enviará al report auxiliar.
*>---------------------------------------------------------------------------------------
PROCEDURE GenCursorDocS

    Parameters CodPro, TipDoc, NumDoc

	private lx_sql, lx_select, f_from, f_where, f_order, err

	f_select  = " SELECT F35sCodPro, F01pDescri, F35sFecDoc, F35sNumDoc, F35sNumLin, " + ;
						"F35sTotPal, F35sTotCaj, F35sTotFra, F35sUniFra"

	f_from    = " FROM F35s" + _em + ", F01p" + _em
	
	f_where   = " WHERE  F35sCodPro='" + CodPro + "'" + ;
                " And    F35sTipDoc='" + TipDoc + "'" + ;
                " And    F35sNumDoc='" + NumDoc + "'" + ;
                " And    F01pCodigo=F35sCodPro"

    f_sql = f_select + f_from + f_where
	err=SqlExec(_ASql, f_sql, 'RELDOCS')
	=SqlMoreResults(_asql)

	if err<0
	=f3_sn(1, 1, "Error al generar listado auxiliar (Salidas)")
		return
	endif

	*> Mirar si está vacío
	Select RELDOCS
	Go Top
	If Eof()
       Use In RELDOCS
       Return
    EndIf

    *> Añadir los datos al cursor que se enviará al report.
    If !Used('_XLCurDocS')
       =CrtCursor('RELDOCS', '_XLCurDocS', 'C')
    EndIf
    Select _XLCurDocS
    Append From Dbf('RELDOCS')
    Use In RELDOCS

Return

*>---------------------------------------------------------------------------------------
*> Proceso de listado auxiliar de documentos de salida propiamente dicho.
*> Se le llama desde: FCMRELDOCS ---> Listado auxiliar de Documentos de Salida.
*>---------------------------------------------------------------------------------------
Procedure PrtDocS

    _label1 = "LISTADO AUXILIAR FACTURACION (SALIDAS)"
    If Used('_XLCurDocS')
        Select _XLCurDocS
        Go Top
        Do Form St3Imp2 With _Label1, 'FcmRdsL1', '_XLCurDocS'
        Use In _XLCurDocS
    Else
       =f3_sn(1, 1, "No hay Documentos de Salida para imprimir")    
    EndIf

Return

************************************************************************
*> Relación de Nºs de palet entrados.                                  *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALENT --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Relación de Palets de Entrada.                   *
************************************************************************
Procedure GenPalEnt
Parameters _CodPro, _FecIni, _FecFin
Private _Selec, _From, _Where, _Order, oF36p, cAlbaran

	*> Cursor de trabajo. Datos de Histórico y de diario estancia de palets.
	=CrtMCursor("F36p,F20c", "PALENTCUR")

   *> Leer los palets a procesar.

	_Selec = "*"
	_From  = "F36p"
	_Order = "F36pNumPal"
	_Where = "F36pCodPro='" + _CodPro + "' And "
	_Where = _Where + "F36pFecEnt Between " + _GCD(_FecIni) + " And " + _GCD(_FecFin)

	If !f3_sql(_Selec, _From, _Where, _Order, , 'PALENTCUR')
		Return .F.
	EndIf

	*> Agregar los datos necesarios del albarán de entrada.
	Select PALENTCUR
	Go Top
	Do While !Eof()
		Scatter Name oF36p

		_Where = "F20cCodPro='" + oF36p.F36pCodPro + "' And F20cNumPal='" + oF36p.F36pNumPal + "' And "
		_Where = _Where + "F20cTipMov Between '1000' And '1999'"
		_Selec = "Min(F20cNumEnt) As F20cNumEnt"
		_From = "F20c"

		If f3_sql(_Selec, _From, _Where, , , "F20cCUR")
			Select F20cCur
			Go Top
			cAlbaran = Iif(IsNull(F20cNumEnt), "", F20cNumEnt)
		Else
			cAlbaran = ""
		EndIf


		Select PALENTCUR
		Replace F20cNumEnt With cAlbaran
		Skip
	EndDo

	*> Copiar datos a cursor de trabajo. Si no existe, se crea.
	If !Used('XLPALENT')
		=CrtCursor('PALENTCUR', 'XLPALENT', 'C')
	EndIf

	Select XLPALENT
	Append From Dbf('PALENTCUR')

	Use In (Select ("PALENTCUR"))
	Use In (Select ("F20cCUR"))

Return

************************************************************************
*> Relación de Nºs de palet entrados.                                  *
*> Segundo paso: Imprimir los datos generados por GenPalEnt.           *
*>                                                                     *
*> Recibe: XLPALENT --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Relación de Palets de Entrada.                   *
************************************************************************
Procedure PrtPalEnt
Parameters lResumen

If Used('XLPALENT')
	Select XLPALENT

	Select XLPALENT
	Go Top
	_Label1 = "RELACION DE PALETS (ENTRADA)"

	*> Objeto impresión.
	loImp2 = CreateObject('st3imp2', , [CR=XLPALENT, RPT=FcmPalRel1])
	=loImp2.Exec('?', [LBL=&_Label1])
	loImp2.Release
Else
	_LxErr = "No hay palets para imprimir"
	Do Form St3Inc With .T.
EndIf

*> Cerrar cursores generados por SQL.
Use In (Select("XLPALENT"))

Return

************************************************************************
*> Relación de Nºs de palet entrados por día.                          *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALENT --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Relación de Palets de Entrada.                   *
************************************************************************
Procedure GenPalEntDia
Parameters _CodPro, _FecIni, _FecFin
Private _Selec, _From, _Where, _Order, oF36p, cAlbaran

	*> Cursor de trabajo. Datos de Histórico y de diario estancia de palets.
	=CrtMCursor("F36p,F20c", "PALENTCUR")

   *> Leer los palets a procesar.

	_Selec = "*"
	_From  = "F36p"
	_Order = "F36pNumPal"
	_Where = "F36pCodPro='" + _CodPro + "' And "
	_Where = _Where + "F36pFecEnt Between " + _GCD(_FecIni) + " And " + _GCD(_FecFin)

	If !f3_sql(_Selec, _From, _Where, _Order, , 'PALENTCUR')
		Return .F.
	EndIf

	*> Agregar los datos necesarios del albarán de entrada.
	Select PALENTCUR
	Go Top
	Do While !Eof()
		Scatter Name oF36p

		_Where = "F20cCodPro='" + oF36p.F36pCodPro + "' And F20cNumPal='" + oF36p.F36pNumPal + "' And "
		_Where = _Where + "F20cTipMov Between '1000' And '1999'"
		_Selec = "Min(F20cNumEnt) As F20cNumEnt"
		_From = "F20c"

		If f3_sql(_Selec, _From, _Where, , , "F20cCUR")
			Select F20cCur
			Go Top
			cAlbaran = Iif(IsNull(F20cNumEnt), "", F20cNumEnt)
		Else
			cAlbaran = ""
		EndIf


		Select PALENTCUR
		Replace F20cNumEnt With cAlbaran
		Skip
	EndDo

	*> Copiar datos a cursor de trabajo. Si no existe, se crea.
	If !Used('XLPALENT')
		=CrtCursor('PALENTCUR', 'XLPALENT', 'C')
	EndIf

	Select XLPALENT
	Append From Dbf('PALENTCUR')

	Use In (Select ("PALENTCUR"))
	Use In (Select ("F20cCUR"))

Return

************************************************************************
*> Relación de Nºs de palet entrados día.                              *
*> Segundo paso: Imprimir los datos generados por GenPalEntDia         *
*>                                                                     *
*> Recibe: XLPALENT --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Relación de Palets de Entrada.                   *
************************************************************************
Procedure PrtPalEntDia
Parameters lResumen

If Used('XLPALENT')
	Select XLPALENT

	Select XLPALENT
	Go Top
	_Label1 = "RELACION DE PALETS (ENTRADA / DIA)"

	*> Objeto impresión.
	loImp2 = CreateObject('st3imp2', , [CR=XLPALENT, RPT=FcmPalRe11])
	=loImp2.Exec('?', [LBL=&_Label1])
	loImp2.Release
Else
	_LxErr = "No hay palets para imprimir"
	Do Form St3Inc With .T.
EndIf

*> Cerrar cursores generados por SQL.
Use In (Select("XLPALENT"))

Return

************************************************************************
*> Relación de contenido de nºs de palet entrados.                     *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALENTDET --> Cursor con los datos a imprimir.            *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacPalEnC ---> Relación de Palets de Entrada.                   *
************************************************************************
Procedure GenPalEntDet
Parameters _CodPro, _FecIni, _FecFin
Local _Selec, _From, _Where, _Order

   *> Primer paso: Leer los palets del histórico, dado que en el mismo
   *> tenemos los datos básicos, excepto el peso del palet.

   _Selec = "F20cCodPro, F20cNumPal, F20cFecMov, F20cNumEnt, F20cCodArt, F20cCanFis, " + ;
            "Nvl(F16tPesPal, 0) As F16tPesPal, F08cDescri"
   _From  = "F20c,F16t,F08c"
   _Order = "F20cCodPro, F20cFecMov, F20cNumPal,F20cCodArt"
   _Where = "F20cCodPro='" + _CodPro + "' And " + ;
            "SubStr(F20cTipMov, 1, 1)='1' And " + ;
            "F20cFecMov Between To_Date('" + DToC(_FecIni) + "', 'dd-mm-yyyy') And To_Date('" + DToC(_FecFin) + "', 'dd-mm-yyyy') And " + ;
            "F16tNumPal(+)=F20cNumPal And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

   If !f3_sql(_Selec, _From, _Where, _Order, '', 'PALENTDETCUR')
      Return .F.
   EndIf

   Select PALENTDETCUR
   Delete For Empty(F20cNumEnt)
   Go Top

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLPALENTDET')
      =CrtCursor('PALENTDETCUR', 'XLPALENTDET', 'C')
   EndIf

   Select XLPALENTDET
   Append From Dbf('PALENTDETCUR')

   Use In PALENTDETCUR

Return .T.

************************************************************************
*> Relación de contenido de nºs de palet entrados.                     *
*> Segundo paso: Imprimir los datos generados por GenPalEntDet.        *
*>                                                                     *
*> Recibe: XLPALENTDET --> Cursor con los datos a imprimir.            *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacPalEnC ---> Relación de Palets de Entrada.                   *
************************************************************************
Procedure PrtPalEntDet

   If Used('XLPALENTDET')
      Select XLPALENTDET

      Select XLPALENTDET
      Go Top
      _Label1 = "RELACION DE CONTENIDO DE PALETS (ENTRADA)"
      Do Form St3Imp2 With _Label1, 'FacPalRel2', 'XLPALENTDET'
   Else
      _LxErr = "No hay palets para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('XLPALENTDET')
      Use in XLPALENTDET
   EndIf

Return

************************************************************************
*> Relación de Nºs de palet de salida.                                 *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALSAL --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Relación de Palets de Salida.                    *
************************************************************************
Procedure GenPalSal
Parameters _CodPro, _FecIni, _FecFin
Private _Selec, _From, _Where, _Order, oF36p, cAlbaran

   *> Leer los palets a procesar.
	_Selec = "*"
	_From  = "F36p"
	_Order = "F36pNumPal"
	_Where = "F36pCodPro='" + _CodPro + "' And "
	_Where = _Where + "F36pFecSal Between " + _GCD(_FecIni) + " And " + _GCD(_FecFin)

	If !f3_sql(_Selec, _From, _Where, _Order, , 'PALSALCUR')
		Return .F.
	EndIf

	*> Copiar datos a cursor de trabajo. Si no existe, se crea.
	If !Used('XLPALSAL')
		=CrtCursor('PALSALCUR', 'XLPALSAL', 'C')
	EndIf

	Select XLPALSAL
	Append From Dbf('PALSALCUR')

	Use In (Select ("PALSALCUR"))

Return

************************************************************************
*> Relación de Nºs de palet salidos.                                   *
*> Segundo paso: Imprimir los datos generados por GenPalSal.           *
*>                                                                     *
*> Recibe: XLPALSAL --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Relación de Palets de Salida.                    *
*>                                                                     *
*> Añadir listado detalle ó resumen. AVC - 01.06.2001                  *
************************************************************************
Procedure PrtPalSal
Parameters lResumen

If Used('XLPALSAL')
	Select XLPALSAL

	Select XLPALSAL
	Delete For F36pFecSal > Date()
	Go Top
	_Label1 = "RELACION DE PALETS (SALIDA)"

	*> Objeto impresión.
	loImp2 = CreateObject('st3imp2', , [CR=XLPALSAL, RPT=FcmPalRel3])
	=loImp2.Exec('?', [LBL=&_Label1])
	loImp2.Release
Else
	_LxErr = "No hay palets para imprimir"
	Do Form St3Inc With .T.
EndIf

*> Cerrar cursores generados por SQL.
Use In (Select("XLPALSAL"))

Return

************************************************************************
*> Relación de contenido de nºs de palet salidos.                      *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALSALDET --> Cursor con los datos a imprimir.            *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacPalSaC ---> Relación de Palets de Salida.                    *
*>                                                                     *
*> Tomar peso movimiento de F14S, si existe. AVC - 31.05.2001          *
************************************************************************
Procedure GenPalSalDet
Parameters _CodPro, _FecIni, _FecFin
Local _Selec, _From, _Where, _Order
Local nCanEnt
Local nPeso

   *> Primer paso: Leer los palets del histórico, dado que en el mismo
   *> tenemos los datos básicos, excepto el peso del palet.

   _Selec = "F20cCodPro, F20cNumPal, F20cFecMov, F20cNumDoc, F20cCodArt, F20cCanFis, F20cNumMov, " + ;
            "Nvl(F16tPesPal, 0) As F16tPesPal, F08cDescri, 0 As F20cPesSal"
   _From  = "F20c,F16t,F08c"
   _Order = "F20cCodPro, F20cFecMov, F20cNumPal,F20cCodArt"
   _Where = "F20cCodPro='" + _CodPro + "' And " + ;
            "F20cTipMov='2999' And " + ;
            "Trunc(F20cFecMov) Between To_Date('" + DToC(_FecIni) + "', 'dd-mm-yyyy') And To_Date('" + DToC(_FecFin) + "', 'dd-mm-yyyy') And " + ;
            "F16tNumPal(+)=F20cNumPal And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

   If !f3_sql(_Selec, _From, _Where, _Order, '', 'PALSALDETCUR')
      Return .F.
   EndIf

   *> Para cada movimiento leído, leer el peso de F14s y, si no existe,
   *> Leer la cantidad entrada para calcular el peso de la cantidad de salida.
   Select PALSALDETCUR
   Go Top
   Do While !Eof()
      Scatter MemVar

      *> Obtener peso de F14s. Sino existe, se crea.
      nPeso = ObtenerPesoMovimiento(PALSALDETCUR.F20cCodPro, ;
                                    PALSALDETCUR.F20cNumPal, ;
                                    PALSALDETCUR.F20cCanFis, ;
                                    PALSALDETCUR.F20cNumMov)
      *>
      Select PALSALDETCUR
      Replace F20cPesSal With nPeso
      Skip
   EndDo

   Select PALSALDETCUR
   Go Top

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLPALSALDET')
      =CrtCursor('PALSALDETCUR', 'XLPALSALDET', 'C')
   EndIf

   Select XLPALSALDET
   Append From Dbf('PALSALDETCUR')

   Use In PALSALDETCUR

Return .T.

************************************************************************
*> Relación de contenido de nºs de palet salidos.                      *
*> Segundo paso: Imprimir los datos generados por GenPalSalDet.        *
*>                                                                     *
*> Recibe: XLPALSALDET --> Cursor con los datos a imprimir.            *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacPalSaC ---> Relación de Palets de Salida.                    *
************************************************************************
Procedure PrtPalSalDet

   If Used('XLPALSALDET')
      Select XLPALSALDET

      Select XLPALSALDET
      Go Top
      _Label1 = "RELACION DE CONTENIDO DE PALETS (SALIDA)"
      Do Form St3Imp2 With _Label1, 'FacPalRel4', 'XLPALSALDET'
   Else
      _LxErr = "No hay palets para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('XLPALSALDET')
      Use in XLPALSALDET
   EndIf

Return

************************************************************************
*> Diario de entrada/salida de palets de facturación.                  *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALFAC --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Diario de Palets Facturación.                    *
************************************************************************

*> Historial de modificaciones:
*> 04.12.2007 (AVC) modificado el rango de selección por fecha.

Procedure GenPalFac
Parameters _CodPro, _FecIni, _FecFin
Private _Selec, _From, _Where, _Order, oPal, nNumDia

	*> Cargar los palets seleccionados del diario de palets.
	_Where = "F36pCodPro='" + _CodPro + "' And "
	_Where = _Where + "F36pFecEnt<=" + _GCD(_FecIni) + " Or F36pFecSal>=" + _GCD(_FecFin)
	_Order = "F36pCodPro,F36pNumPal"
	_From  = "F36p"
	_Selec = "*"

	If !f3_sql(_Selec, _From, _Where, _Order, , 'PALFACCUR')
		Return .F.
	EndIf

	*> Agregar campos de cálculo del detalle de factura.
	Select PALFACCUR
	=AddFldToCursor("PALFACCUR", [NAME=FecIni,TYPE=D])
	=AddFldToCursor("PALFACCUR", [NAME=FecFin,TYPE=D])
	=AddFldToCursor("PALFACCUR", [NAME=NumDia,TYPE=N,LENGTH=4,DECIMALS=0])

	Replace All FecIni With _FecIni
	Replace All FecFin With _FecFin
	Replace All NumDia With 0

	*> Copiar datos a cursor de trabajo. Si no existe, se crea.
	If !Used('XLPALFAC')
		=CrtCursor('PALFACCUR', 'XLPALFAC', 'C')
	EndIf

	Select XLPALFAC
	Append From Dbf('PALFACCUR')
	Use In (Select ("PALFACCUR"))

	_Order = "F35dFecCal Desc"
	_From  = "F35d"
	_Selec = "*"

	Select XLPALFAC
	Go Top
	Do While !Eof()
		Scatter Name oPal
		nNumDia = 0

		*> Cargar los días estancia palet.
		_Where = "F35dCodPro='" + _CodPro + "' And F35dNumPal='" + oPal.F36pNumPal + "'"

		If f3_sql(_Selec, _From, _Where, _Order, , '___F35D')
			Select ___F35D
			nNumDia = F35dNumDia
		EndIf


		Select XLPALFAC
		Replace NumDia With nNumDia
		Skip
	EndDo
	
	Go Top
	Use In (Select ("___F35D"))

Return

************************************************************************
*> Diario de entrada/salida de palets de facturación.                  *
*> Segundo paso: Imprimir los datos generados por GenPalFac.           *
*>                                                                     *
*> Recibe: XLPALFAC --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FcmPalRel ---> Diario de Palets Facturación.                    *
************************************************************************
Procedure PrtPalFac

Private _Label1, loImp

If Used('XLPALFAC')
	Select XLPALFAC

	Select XLPALFAC
	Go Top
	_Label1 = "DIARIO DE PALETS (FACTURACION)"

	*> Objeto impresión.
	loImp2 = CreateObject('st3imp2', , [CR=XLPALFAC, RPT=FcmPalRel5])
	=loImp2.Exec('?', [LBL=&_Label1])
	loImp2.Release
Else
	_LxErr = "No hay palets para imprimir"
	Do Form St3Inc With .T.
EndIf

*> Cerrar cursores generados por SQL.
Use In (Select("XLPALFAC"))

Return

************************************************************************
*> Relación de entradas / Salidas de palets.                           *
*> Primer paso: Generar datos del propietario/periodo actual.          *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _FecIni ---> Fecha inicio periodo.                          *
*>         _FecFin ---> Fecha final periodo.                           *
*>                                                                     *
*> Genera: XLPALENTSAL ----> Cursor con los datos a imprimir.          *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacPalEntSal ---> Diario de Entradas/Salidas de palets.         *
************************************************************************
Procedure GenPalEntSal
Parameters _CodPro, _FecIni, _FecFin
Local _Selec, _From, _Where, _Order

   *> Cargar los palets entrados del propietario/fecha recibidos.
   *> Utiliza GenPalEnt, que genera el cursor XLPALENT.
   Wait Window 'Cargando datos entradas propietario: ' + _CodPro NoWait
   =GenPalEnt(_CodPro, _FecIni, _FecFin)

   *> Cargar los palets salidos del propietario/fecha recibidos.
   *> Utiliza GenPalSal, que genera el cursor XLPALSAL.
   Wait Window 'Cargando datos salidas propietario: ' + _CodPro NoWait
   =GenPalSal(_CodPro, _FecIni, _FecFin)

   *> Crear, si no existe, el cursor de trabajo.
   If !Used('XLPALENTSAL')
      Create Cursor XLPALENTSAL ( ;
         CodPro C(6), ;
         FecMov D, ;
         PalEnt C(10), ;
         NumAlb C(9), ;
         KgsEnt N(10, 3), ;
         PalSal C(10), ;
         DocSal C(13), ;
         KgsSal N(10, 3))
   EndIf

   *> Añadir los palets entrados en el XLPALENTSAL.
   If Used('XLPALENT')
      Select XLPALENT
      Go Top
      Do While !Eof()
         Select XLPALENTSAL
         Append Blank
         Replace CodPro With _CodPro, ;
                 FecMov With XLPALENT.F20cFecMov, ;
                 NumAlb With XLPALENT.F20cNumEnt, ;
                 PalEnt With XLPALENT.F20cNumPal, ;
                 KgsEnt With XLPALENT.F16tPesPal, ;
                 KgsSal With 0

         *>
         Select XLPALENT
         Skip
      EndDo

      Use In XLPALENT
   EndIf

   *> Añadir los palets salidos en el XLPALENTSAL.
   If Used('XLPALSAL')
      Select XLPALSAL
      Go Top
      Do While !Eof()
         Select XLPALENTSAL
         Locate For FecMov==XLPALSAL.F20cFecMov .And. ;
                    PalSal==XLPALSAL.F20cNumPal .And. ;
                    DocSal==XLPALSAL.F20cNumDoc
         If Found()
           Replace KgsSal With KgsSal + XLPALSAL.F16tPesPal
         Else
            Locate For FecMov==XLPALSAL.F20cFecMov .And. Empty(PalSal)
            If !Found()
               Append Blank
               Replace CodPro With _CodPro, ;
                       FecMov With XLPALSAL.F20cFecMov, ;
                       KgsEnt With 0, ;
                       KgsSal With 0
            EndIf
            Replace PalSal With XLPALSAL.F20cNumPal, ;
                    DocSal With XLPALSAL.F20cNumDoc, ;
                    KgsSal With KgsSal + XLPALSAL.F16tPesPal
         EndIf

         *>
         Select XLPALSAL
         Skip
      EndDo

      Use In XLPALSAL
   EndIf

   Wait Clear

Return .T.

************************************************************************
*> Relación de entradas / Salidas de palets.                           *
*> Segundo paso: Imprimir los datos generados por GenPalEntSal.        *
*>                                                                     *
*> Recibe: XLPALENTSAL --> Cursor con los datos a imprimir.            *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacPalEntSal ---> Diario de Entradas/Salidas de palets.         *
************************************************************************
Procedure PrtPalEntSal
Local cOldNull

   cOldNull = Set('NULLDISPLAY')
   Set NullDisplay To Space(1)

   If Used('XLPALENTSAL')
      Select XLPALENTSAL

      Select XLPALENTSAL
      Go Top
      _Label1 = "DIARIO DE ENTRADAS/SALIDAS DE PALETS"
      Do Form St3Imp2 With _Label1, 'FacPalRel6', 'XLPALENTSAL'
   Else
      _LxErr = "No hay palets para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('XLPALENTSAL')
      Use in XLPALENTSAL
   EndIf

   Set NullDisplay To cOldNull

Return

************************************************************************
*> Listado relación aseguradora.                                       *
*> Primer paso: Generar datos del año/mes.                             *
*>                                                                     *
*> Recibe: cAnyo ----> Año a calcular.                                 *
*>         cMes -----> Mes a calcular.                                 *
*>                                                                     *
*> Genera: XLSEGMES ----> Cursor con los datos a imprimir.             *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacSegMes ---> Relación aseguradora/mes.                        *
************************************************************************
Procedure GenSeguroMes
Parameters cAnyo, cMes

Private cWhere, cFromF, cField, cOrder, cGroup
Local lEstado, nSaldo
Local cFechaInicial, dFechaActual, cFechaSaldoInicial, nSaldoAnterior
Local nInx

lEstado = .T.

If Used('F30DPREV')
   Use In F30DPREV
EndIf
If Used('XLSEGMES')
   Use In XLSEGMES
EndIf

*> Obtener el saldo anterior, que corresponde al cierre del último
*> día del mes anterior a cMes.
cFechaInicial = "01/" + cMes + "/" + cAnyo
cWhere = "F30dFecha<To_Date('" + cFechaInicial + "', 'DD-MM-YYYY')"
cFromF = "F30d"
cField = "Max(F30dFecha) As F30dFecha"

Wait Window 'Obteniendo saldo inicial ...' NoWait

lEstado = f3_sql(cField, cFromF, cWhere, , , 'F30DPREV')
If lEstado
   *> Existe ficha de cierre: Obtener saldo anterior.
   Select F30DPREV
   Go Top
   cFechaSaldoInicial = DToC(F30dFecha)
   nSaldoAnterior = ObtenerSaldoDia(CToD(cFechaSaldoInicial))
Else
   *> No existe ningún cierre anterior: Saldo inicial a cero.
   Store 0 To nSaldoAnterior
EndIf

dFechaCalculo = CToD(cFechaInicial) - 1

*> Borrar datos temporales.
Use In F30DPREV

*> Crear cursor de trabajo.
Create Cursor XLSEGMES (;
   cAnCalc C(4), ;
   cMeCalc C(2), ;
   cDia C(2), ;
   nSldDia N(13, 3), ;
   nSalAnt N(13, 3), ;
   nEntDia N(13, 3), ;
   nSalDia N(13, 3), ;
   nRegDia N(13, 3))

For nInx = 1 To 31
   *> Obtener la fecha de cálculo.
   dFechaCalculo = dFechaCalculo + 1
   If Month(dFechaCalculo) # Val(cMes)
      *> Cambio de mes: Fin de proceso. (Hay meses que no tienen 31 días ...)
      Exit
   EndIf

   Select XLSEGMES
   Append Blank
   Replace cAnCalc With cAnyo, ;
           cMeCalc With cMes, ;
           cDia    With PadL(AllTrim(Str(nInx, 2, 0)), 2, '0')

   *> Obtener el saldo del día actual.
   Wait Window 'Obteniendo saldo día: ' + DToC(dFechaCalculo) + ' ...' NoWait

   nSaldo = ObtenerSaldoDia(dFechaCalculo)
   If nSaldo = 0
      *> No hay cierre para la fecha solicitada: Tomar saldo anterior en curso.
      nSaldo = nSaldoAnterior
   EndIf

   Select XLSEGMES
   Replace nSldDia With nSaldo, ;
           nRegDia With 0

   *> Obtener el saldo entradas del día actual.
   Wait Window 'Obteniendo entradas día: ' + DToC(dFechaCalculo) + ' ...' NoWait

   nSaldo = ObtenerEntradasDia(dFechaCalculo)
   Select XLSEGMES
   Replace nEntDia With nSaldo

   *> Obtener el saldo salidas del día actual.
   Wait Window 'Obteniendo salidas día: ' + DToC(dFechaCalculo) + ' ...' NoWait

   nSaldo = ObtenerSalidasDia(dFechaCalculo)
   Select XLSEGMES
   Replace nSalDia With nSaldo

   Replace nSalAnt With nSaldoAnterior

   *> El acumulado cierre del día actual es el saldo anterior del próximo día (do you understand, stupid?)
   nSaldoAnterior = XLSEGMES.nSldDia

   *> Actualizar la columna de regularizaciones. Esto es una trampa para que cuadre ...
   Replace nRegDia With nSaldoAnterior - (XLSEGMES.nSalAnt + XLSEGMES.nEntDia - XLSEGMES.nSalDia)
EndFor

Wait Clear

Return lEstado

************************************************************************
*> Listado relación aseguradora.                                       *
*> Segundo paso: Imprimir datos del año/mes.                           *
*>                                                                     *
*> Recibe: XLSEGMES, cursor con los datos a imprimir                   *
*>         cTipo, tipo listado (normal o con ajustes inventario).      *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacSegMes ---> Relación aseguradora/mes.                        *
************************************************************************
Procedure PrtSeguroMes
Parameters cTipo

Local cReport

   If Type('cTipo') # 'C'
      cTipo = '1'
   EndIf

   cReport = 'FacSegMes2'
   If cTipo # '2'
      cReport = 'FacSegMes1'
   EndIf
   
   If Used('XLSEGMES')
      Select XLSEGMES

      Select XLSEGMES
      Go Top
      _Label1 = "RELACION ASEGURADORA"
      Do Form St3Imp2 With _Label1, cReport, 'XLSEGMES'
   Else
      _LxErr = "No hay datos para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('XLSEGMES')
      Use in XLSEGMES
   EndIf

Return

************************************************************************
*> Obtener saldo a una fecha concreta.                                 *
*> Recibe: dFecha -----> Fecha de cálculo.                             *
*>         cCProI -----> Propietario inicial.                          *
*>         cCProF -----> Propietario final.                            *
*>         cCArtI -----> Artículo inicial.                             *
*>         cCArtF -----> Artículo final.                               *
************************************************************************
Function ObtenerSaldoDia
Parameters dFecha, cCProI, cCProF, cCArtI, cCArtF

Local nSaldo, lEstado, nUnidades
Private cWhere, cFromF, cField, cOrder, cGroup

If Type('cCProI')=='L'
   cCProI = Space(6)
EndIf
If Type('cCProF')=='L'
   cCProF = Replicate('z', 6)
EndIf
If Type('cCArtI')=='L'
   cCArtI = Space(13)
EndIf
If Type('cCArtF')=='L'
   cCArtF = Replicate('z', 13)
EndIf

Store 0 To nSaldo

If Used("F30DSEG")
   Use In F30DSEG
EndIf

*> Selección de datos de la tabla de cierre diario.
cWhere = ""
cWhere = cWhere + "F30dFecha=To_Date('" + DToC(dFecha) + "', 'DD-MM-YYYY') And "
cWhere = cWhere + "F30dCodPro Between '" + cCProI + "' And '" + cCProF + "' And "
cWhere = cWhere + "F30dCodArt Between '" + cCArtI + "' And '" + cCArtF + "' And "

*> Relación con la tabla de artículos.
cWhere = cWhere + "F08cCodPro=F30dCodPro And F08cCodArt=F30dCodArt"

cOrder = "F30dFecha, F30dCodPro, F30dCodArt"
cGroup = ""
cFromF = "F30d,F08c"
cField = "*"

*> Cargar datos del día a calcular.
lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F30DSEG")
If !lEstado
   Use In F30DSEG
   Return nSaldo
EndIf

*> Acumular valor saldo día.
Select F30DSEG
Go Top
Do While !Eof()
   nSaldo = nSaldo + (F30dStock * F08cPCoste)
   Select F30DSEG
   Skip
EndDo

Use In F30DSEG

Return Int(nSaldo + .5)

************************************************************************
*> Obtener valor entradas a una fecha concreta.                        *
*> Recibe: dFecha -----> Fecha de cálculo.                             *
*>         cCProI -----> Propietario inicial.                          *
*>         cCProF -----> Propietario final.                            *
*>         cCArtI -----> Artículo inicial.                             *
*>         cCArtF -----> Artículo final.                               *
************************************************************************
Function ObtenerEntradasDia
Parameters dFecha, cCProI, cCProF, cCArtI, cCArtF

Local nSaldo, lEstado, nUnidades
Private cWhere, cFromF, cField, cOrder, cGroup

If Type('cCProI')=='L'
   cCProI = Space(6)
EndIf
If Type('cCProF')=='L'
   cCProF = Replicate('z', 6)
EndIf
If Type('cCArtI')=='L'
   cCArtI = Space(13)
EndIf
If Type('cCArtF')=='L'
   cCArtF = Replicate('z', 13)
EndIf

Store 0 To nSaldo

If Used("F20CSEG")
   Use In F20CSEG
EndIf

*> Selección de datos de la tabla de cierre diario.
cWhere = ""
cWhere = cWhere + "F20cFecMov=To_Date('" + DToC(dFecha) + "', 'DD-MM-YYYY') And "
cWhere = cWhere + "F20cCodPro Between '" + cCProI + "' And '" + cCProF + "' And "
cWhere = cWhere + "F20cCodArt Between '" + cCArtI + "' And '" + cCArtF + "' And "
cWhere = cWhere + "F20cTipMov Like '1%' And "
cWhere = cWhere + "F20cNumEnt>'0' And "

*> Relación con la tabla de artículos.
cWhere = cWhere + "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

cOrder = "F20cFecMov, F20cCodPro, F20cCodArt"
cGroup = ""
cFromF = "F20c,F08c"
cField = "*"

*> Cargar datos del día a calcular.
lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F20CSEG")
If !lEstado
   Use In F20CSEG
   Return nSaldo
EndIf

*> Acumular valor saldo entradas día.
Select F20CSEG
Go Top
Do While !Eof()
   nSaldo = nSaldo + (F20cCanFis * F08cPCoste)
   Select F20CSEG
   Skip
EndDo

Use In F20CSEG

Return Int(nSaldo + .5)

************************************************************************
*> Obtener valor salidas a una fecha concreta.                         *
*> Recibe: dFecha -----> Fecha de cálculo.                             *
*>         cCProI -----> Propietario inicial.                          *
*>         cCProF -----> Propietario final.                            *
*>         cCArtI -----> Artículo inicial.                             *
*>         cCArtF -----> Artículo final.                               *
************************************************************************
Function ObtenerSalidasDia
Parameters dFecha, cCProI, cCProF, cCArtI, cCArtF

Local nSaldo, lEstado, nUnidades
Private cWhere, cFromF, cField, cOrder, cGroup

If Type('cCProI')=='L'
   cCProI = Space(6)
EndIf
If Type('cCProF')=='L'
   cCProF = Replicate('z', 6)
EndIf
If Type('cCArtI')=='L'
   cCArtI = Space(13)
EndIf
If Type('cCArtF')=='L'
   cCArtF = Replicate('z', 13)
EndIf

Store 0 To nSaldo

If Used("F20CSEG")
   Use In F20CSEG
EndIf

*> Selección de datos de la tabla de cierre diario.
cWhere = ""
cWhere = cWhere + "F20cFecMov=To_Date('" + DToC(dFecha) + "', 'DD-MM-YYYY') And "
cWhere = cWhere + "F20cCodPro Between '" + cCProI + "' And '" + cCProF + "' And "
cWhere = cWhere + "F20cCodArt Between '" + cCArtI + "' And '" + cCArtF + "' And "
cWhere = cWhere + "F20cTipMov='2999' And "

*> Relación con la tabla de artículos.
cWhere = cWhere + "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

cOrder = "F20cFecMov, F20cCodPro, F20cCodArt"
cGroup = ""
cFromF = "F20c,F08c"
cField = "*"

*> Cargar datos del día a calcular.
lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F20CSEG")
If !lEstado
   Use In F20CSEG
   Return nSaldo
EndIf

*> Acumular valor saldo salidas día.
Select F20CSEG
Go Top
Do While !Eof()
   nSaldo = nSaldo + (F20cCanFis * F08cPCoste)
   Select F20CSEG
   Skip
EndDo

Use In F20CSEG

Return Int(nSaldo + .5)

************************************************************************
*> Impresión de tarifas almacenaje - Generar.                          *
*> Primer paso: Generar datos.                                         *
*>                                                                     *
*> Recibe: cCodPro -----> Propietario.                                 *
*> Recibe: cAnyo   -----> A;o periodo.                                 *
*> Recibe: cMes    -----> Mes periodo.                                 *
*> Recibe: cQuince -----> Quincena.                                    *
*>                                                                     *
*> Genera: XLGENTAR --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacTarAl ----> Aplicación Tarifas Almacenaje.                   *
************************************************************************
Procedure GenTarF
Parameters cCodPro, cAnyo, cMes, cQuince

Private cSelec, cFromF, cWhere, cOrder, cGroup, cField
Local SUMF34GCNT

   *> Leer datos de la ficha de facturación del propietario.
   m.F32cCodPro = cCodPro
   If !f3_seek("F32c")
      *_LxErr = 'No se han podido cargar datos ficha propietario' + cr
      *Do Form St3Inc With .T.
      Return
   EndIf

   *> Leer datos para imprimir.
   cSelec = "F34g" + _em + ".*, F34f" + _em + ".*, " + ;
            "F08cTipPro, F08cPesUni, F08cVolUni, 0 As Ratio, " + ;
            "0 As CanMin, 0 As PrecioS, '" + Space(3) + "' As UniFac, " + ;
            "0 As F34gCantid, 0 As F34fCantid, " + ;
            "'" + Space(6) + "' As F34gTipo, 0 As PrecioT, 0 As SUMF34gCantid"

   cFromF = "F34g,F34f,F08c"
   cOrder = "F34gCodPro, F34gAnyo, F34fMes, F34fQuince, F34fCodArt"
   cWhere = ""

   *> Clave principal de lectura de F34g.
   cWhere = cWhere + "F34gCodPro='" + cCodPro + "' And "
   cWhere = cWhere + "F34gAnyo='" + cAnyo + "' And "
   cWhere = cWhere + "F34gMes='" + cMes + "' And "
   cWhere = cWhere + "F34gQuince='" + cQuince + "' "

   *> Clave secundaria de relación con F34f.
   cWhere = cWhere + "And F34fCodPro=F34gCodPro "
   cWhere = cWhere + "And F34fAnyo=F34gAnyo "
   cWhere = cWhere + "And F34fMes=F34gMes "
   cWhere = cWhere + "And F34fQuince=F34gQuince "
   cWhere = cWhere + "And F34fCodArt=F34gCodArt "
   cWhere = cWhere + "And F34fSitStk=F34gSitStk "

   *> Clave secundaria de relación con F08c.
   cWhere = cWhere + "And F08cCodPro=F34gCodPro And F08cCodArt=F34fCodArt"

   If !f3_sql(cSelec, cFromF, cWhere, cOrder, , 'XLGENTARCUR')
      _LxErr = _LxErr + 'No se han podido cargar datos: ' + cCodPro + ' Quincena: ' + cQuince + cr
      Return
   EndIf

   *> Leer cantidad mínima a facturar por producto.
   cSelec = "*"
   cFromF = "F34t"
   cWhere = "F34tCodPro='" + cCodPro + "' And F34tCodTar='ALMS'"
   cOrder = "F34tCodPro, F34tCodArt"

   If !f3_sql(cSelec, cFromF, cWhere, cOrder, , 'F34TCUR')
      _LxErr = _LxErr + 'No se han podido cargar tarifas: ' + cCodPro + ' Quincena: ' + cQuince + cr
      Return
   EndIf


   Select XLGENTARCUR
   Go Top
   Do While !Eof()
      *> Buscar la tarifa del artículo, si la hay.
      Select F34TCUR
      Locate For F34tCodArt==XLGENTARCUR.F34gCodArt
      If !Found()
         *> Tomar por defecto la tarifa general del propietario.
         Go Top
      EndIf

      Select XLGENTARCUR
      Replace CanMin   With F34TCUR.F34tMinUni, ;
              PrecioS  With F32c.F32cPreMin, ;
              PrecioT  With F34TCUR.F34tEuros, ;
              UniFac   With F32c.F32cUniFac, ;
              F34gTipo With F34TCUR.F34tTipo

      *> Obtener la cantidad real (Media o Punta).
      Do Case
         Case F34TCUR.F34tTipo='MEDIA'
            Replace F34gCantid With F34gCanMed
         Case F34TCUR.F34tTipo='PUNTA'
            Replace F34gCantid With F34gCanMax
         Otherwise
            Replace F34gCantid With F34gCanMed
      EndCase

      *> Obtener la cantidad teórica (KGS o DM3 / Media o Punta).
      Do Case
         Case F34TCUR.F34tUnitat='KGS'
			Replace Ratio With F08cPesUni

            Do Case
               Case F34TCUR.F34tTipo='MEDIA'
                  Replace F34fCantid With F34fMedKgs
               Case F34TCUR.F34tTipo='PUNTA'
                  Replace F34fCantid With F34fMaxKgs
               Otherwise
                  Replace F34fCantid With F34fMedKgs
            EndCase

         Case F34TCUR.F34tUnitat='DM3'
			Replace Ratio With F08cVolUni

            Do Case
               Case F34TCUR.F34tTipo='MEDIA'
                  Replace F34fCantid With F34fMedVol
               Case F34TCUR.F34tTipo='PUNTA'
                  Replace F34fCantid With F34fMaxVol
               Otherwise
                  Replace F34fCantid With F34fMedVol
            EndCase

         Otherwise
			Replace Ratio With F08cPesUni

            Do Case
               Case F34TCUR.F34tTipo='MEDIA'
                  Replace F34fCantid With F34fMedKgs
               Case F34TCUR.F34tTipo='PUNTA'
                  Replace F34fCantid With F34fMaxKgs
               Otherwise
                  Replace F34fCantid With F34fMedKgs
            EndCase
      EndCase

      *>
      Skip
   EndDo

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLGENTAR')
      =CrtCursor('XLGENTARCUR', 'XLGENTAR', 'C')
   EndIf

   *> Agregar registros al cursor de impresión.
   *> Borrar los productos en estado laboratorio (SSTK='8000') And (TIPPRO='3000')

   Select XLGENTARCUR
   Delete For F34gSitStk=='8000' .And. F08cTipPro=='3000'

   Sum(F34gCantid) To SUMF34GCNT
   Replace All SUMF34gCan With SUMF34GCNT

   Select XLGENTAR
   Append From Dbf('XLGENTARCUR')
*   Delete For F34fCantid = 0
   Delete For F34gCantid = 0

   Use In XLGENTARCUR
   Use In F34TCUR

Return

************************************************************************
*> Impresión de tarifas almacenaje - Generar.                          *
*> Segundo paso: Imprimir datos.                                       *
*>                                                                     *
*> Recibe: XLGENTAR --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacTarAl ----> Aplicación Tarifas Almacenaje.                   *
************************************************************************
Procedure PrtTarF

   If Used('XLGENTAR')
      Select XLGENTAR
      Go Top

      _Label1 = "APLICACION TARIFAS-ALMACENAJE"
      Do Form St3Imp2 With _Label1, 'FacTarAlL1', 'XLGENTAR'
   Else
      _LxErr = "No hay tarifas para imprimir" + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('XLGENTAR')
      Use in XLGENTAR
   EndIf
   
Return

************************************************************************
*> Listado de conceptos directos de facturación.                       *
*> Primer paso: Generar datos a imprimir.                              *
*>                                                                     *
*> Recibe: CodPro ----> Propietario.                                   *
*>         dDate   ---> Fecha de cálculo. (def=Date).                  *
*>                                                                     *
*> Genera: XLFRAD--> Cursor con los datos a imprimir.                  *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FCMFRAD -------> Listado de conceptos directos de facturación.  *
************************************************************************
Procedure GenFraD

Parameters cCodPro, dDate, dDateH
Local cWhere, cFormF, cOrder, cGroup, cField
Local lEstado

If Type('dDate')<>'D'
	*> Opcionalmente, fecha de cálculo = fecha del día.
   dDate = Date()
EndIf

If Type('dDateH')<>'D'
	*> Opcionalmente, fecha de cálculo = fecha del día.
   dDateH = Date()
   If dDateH < dDate
      dDateH = dDate
   EndIf
EndIf

cField = "*"
cGroup = ""
cFromF = "F36m,F01p"
cOrder = "F36mCodPro, F36mFecCal, F36mRowId"
cWhere = "F36mCodPro='" + cCodPro + "' And " + ;
         "F36mFecCal Between " + _GCD(dDate)  + " And " + _GCD(dDateH) + " And F01pCodigo=F36mCodPro"

lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "XLFRADCUR")
If !lEstado
   Return .F.
EndIf

*> Copiar datos a cursor de trabajo. Si no existe, se crea.
If !Used('XLFRAD')
   =CrtCursor('XLFRADCUR', 'XLFRAD', 'C')
EndIf

Select XLFRAD
Append From Dbf('XLFRADCUR')
Use In XLFRADCUR

Return

************************************************************************
*> Listado de conceptos directos de facturación.                       *
*> Segundo paso: Impresión de datos.                                   *
*>                                                                     *
*> Recibe: XLFRAD  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FCMDIRE -------> Listado de conceptos directos de facturación.  *
************************************************************************
Procedure PrtFraD

   If Used('XLFRAD')
      Select XLFRAD
      Go Top
      _Label1 = "CONCEPTOS DIRECTOS DE FACTURACION"
      Do Form St3Imp2 With _Label1, 'FCMDIRE1', 'XLFRAD'
   Else
      _LxErr = "No hay datos para imprimir"
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLFRAD')
      Use in XLFRAD
   EndIf

Return

************************************************************************
*> Listado de conceptos manuales de facturación.                       *
*> Primer paso: Generar datos a imprimir.                              *
*>                                                                     *
*> Recibe: CodPro ----> Propietario.                                   *
*>         dDate   ---> Fecha de cálculo. (def=Date).                  *
*>                                                                     *
*> Genera: XLFRAM--> Cursor con los datos a imprimir.                  *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FCMFRAM -------> Listado de conceptos manuales de facturación.  *
************************************************************************
Procedure GenFraM

Parameters cCodPro, dDate, dDateH
Local cWhere, cFormF, cOrder, cGroup, cField
Local lEstado

If Type('dDate')<>'D'
	*> Opcionalmente, fecha de cálculo = fecha del día.
   dDate = Date()
EndIf

If Type('dDateH')<>'D'
	*> Opcionalmente, fecha de cálculo = fecha del día.
   dDateH = Date()
   If dDateH < dDate
      dDateH = dDate
   EndIf
EndIf

cField = "*"
cGroup = ""
cFromF = "F36d,F01p"
cOrder = "F36dCodPro, F36dFecCal, F36dNumDoc, F36dRowId"
cWhere = "F36dCodPro='" + cCodPro + "' And " + ;
         "F36dFecCal Between " + _GCD(dDate)  + " And " + _GCD(dDateH) + " And F01pCodigo=F36dCodPro"

lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "XLFRAMCUR")
If !lEstado
   Return .F.
EndIf

*> Copiar datos a cursor de trabajo. Si no existe, se crea.
If !Used('XLFRAM')
   =CrtCursor('XLFRAMCUR', 'XLFRAM', 'C')
EndIf

Select XLFRAM
Append From Dbf('XLFRAMCUR')
Use In XLFRAMCUR

Return

************************************************************************
*> Listado de conceptos manuales de facturación.                       *
*> Segundo paso: Impresión de datos.                                   *
*>                                                                     *
*> Recibe: XLFRAM  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FCMMANU -------> Listado de conceptos manuales de facturación.  *
************************************************************************
Procedure PrtFraM

   If Used('XLFRAM')
      Select XLFRAM
      Go Top
      _Label1 = "CONCEPTOS MANUALES DE FACTURACION"
      Do Form St3Imp2 With _Label1, 'FCMMANU1', 'XLFRAM'
   Else
      _LxErr = "No hay datos para imprimir"
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLFRAM')
      Use in XLFRAM
   EndIf

Return

************************************************************************
*> Listado relación seguro.		                                       *
*> Primer paso: Generar datos del año/mes.                             *
*>                                                                     *
*> Recibe: cAnyo ----> Año a calcular.                                 *
*>         cMes -----> Mes a calcular.                                 *
*>                                                                     *
*> Genera: XLVALMES ----> Cursor con los datos a imprimir.             *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacSegPro ---> Relación seguro.			                       *
************************************************************************
Procedure ValGenSeguroMes
Parameters cAnyo, cMes

Private cWhere, cFromF, cField, cOrder, cGroup
Local lEstado, nSaldo
Local cFechaInicial, dFechaActual, cFechaSaldoInicial, nSaldoAnterior
Local nInx

lEstado = .T.

If Used('F30DPREV')
   Use In F30DPREV
EndIf
If Used('XLVALMES')
   Use In XLVALMES
EndIf

Create Cursor Maximas  (CodPro C(6), CodArt C(13) , FechaP D , ;
						Peso N(10,3),;
						FechaV D ,;
						Volumen N(10,3),;
						StockP N(10,3),;
						StockV N(10,3),;
						SitStk C(4))

Create Cursor XLVALMES (CodPro C(6) , F08cCodArt C(13) ,;
						F08cDescri C(40), Fecha D ,;
						Cantid N(10,3), Canfis N(10,3),;
						CanRea N(10,3),;
						Tarifa C(4) ,;
						Precio N(10,6),;
						Quince C (1),;
						SitStk C (4))

*> Cuantas Quincenas Calculamos.------------------------------------------------
If m.Labs = 0
	_Sentencia = "Select * from F01p001 Where F01pCodigo Between '" + Inc_Cod + ;
				 "' And '" + Fin_cod + "'" + ;
				 " Order By F01pCodigo "
Else
	Inc_cod ='      '
	Fin_cod ='zzzzzz'

	_Sentencia = "Select * from F01p001 Where F01pCodigo Between '" + Inc_Cod + ;
				 "' And '" + Fin_cod + "'" + ;
				 " Order By F01pCodigo "

EndIf

_Ok = SqlExec(_Asql,_Sentencia,'Propi')
=SqlMoreResults(_Asql)			 


Select Propi
Go Top
Do While !Eof()

		Wait Window 'Tratando Propietario ' + Propi.F01pCodigo NoWait

		*> Cuantas Quincenas.----------------------------------------------------
		_Sentencia = " Select * from f34f001 Where F34fCodPro = '" + Propi.F01pCodigo + ;
					 "' And F34fMes = '" + cMes + ;
					 "' And F34fAnyo = '" + cAnyo + "'" + ;
					 " Order By F34fQuince,F34fCodArt,F34fSitStk "

		_Ok = SqlExec(_Asql,_Sentencia,'Quincena')
		=SqlMoreResults(_Asql)			 



		Select Quincena
		Go Top
		Do While !Eof()


			*> Haremos la discriminación.----------------------------------------------
			*> los tipos de producto 3000 no cobran otra cosa q no sea SitStk = 1000.--
			_Sentencia = " Select F08cTipPro From F08c001 " + ;
						 " Where F08cCodPro = '" + Propi.F01pCodigo + ;
						 "' And F08cCodArt = '" + Quincena.F34fCodArt + "'"
						 
			_Ok = SqlExec(_Asql,_Sentencia,'TipPro')
			If _Ok < 1
			   =MessageBox('Error consultando Tipos de producto',16,'TUNDEN')
			   Return .F.	
			EndIf

			Select Quincena

			*> Si es de Laboratorio que lo paguen ellos.--------------------
			If m.Labs = 0
				If Quincena.F34fSitStk <> '1000' .And. TipPro.F08cTipPro = '3000'
				   Skip
				   Loop
				EndIf
			Else
				If Quincena.F34fSitStk <> '8000' .Or. TipPro.F08cTipPro <> '3000'
				   Skip
				   Loop
				EndIf
			EndIf

			Fechad = IIF(F34fQuince = '1','01/'+cMes+'/'+cAnyo,'16'+cMes+'/'+cAnyo)
			Fechah = IIF(F34fQuince = '2','01/'+Right('00'+Alltrim(Str(Val(cMes)+1)),2)+'/'+cAnyo,'15/'+cMes+'/'+cAnyo)
			Fechah = Ctod(Fechah)
			Fechah = IIF(F34fQuince = '2',Dtoc(fechah-1),Dtoc(fechah))

			*> Recogemos los dias de ese mes en el cierre diario.------------------
			_Sentencia  = " Select * " + ;
						  " From F30d001 Where F30dFecha between to_Date('" + Fechad + "','dd-mm-yyyy')" + ;
						  " And To_date('" + Fechah + "','dd-mm-yyyy')" + ;
						  " And F30dCodPro = '" + Quincena.F34fCodPro + "'" + ;
						  " And F30dCodArt = '" + Quincena.F34fCodArt + "'" + ;
						  " And F30dSitStk = '" + Quincena.F34fSitStk + "'"

*!*				_Sentencia  = " Select F30dCodPro,F30dCodArt,F30dFecha " + ;
*!*							  ", Sum(Nvl(F30dTotVol,0)) As F30dTotVol " + ;
*!*							  ", Sum(Nvl(F30dTotPes,0)) As F30dTotPes " + ;
*!*							  ", Sum(Nvl(F30dStock,0)) As F30dStock " + ;
*!*							  " From F30d001 Where F30dFecha between to_Date('" + Fechad + "','dd-mm-yyyy')" + ;
*!*							  " And To_date('" + Fechah + "','dd-mm-yyyy')" + ;
*!*							  " And F30dCodPro BetWeen '" + Inc_Cod + "' And '" + Fin_cod + "'" + ;
*!*							  " Group By F30dCodpro,F30dCodArt,F30dFEcha "

*						  " And F30dSitStk = '1000' " 

		*			  " Order By F30dCodPro,F30dCodArt "

			_Ok = SqlExec(_Asql,_Sentencia,'DiasCierre')
			=SqlMoreResults(_Asql)
			If _Ok < 1
				=MessageBox('Error consultando cierre',16,'TUNDEN')
				Return .F.
			EndIf

		    *> Reclasificar el cursor de trabajo, dado que se pueden haber insertado registros al final.
			Select DiasCierre
			Sort To 'F30dSort' On F30dCodPro/A, F30dCodArt/A,  F30dSitStk/A, F30dFecha/A
			Zap
			Append From F30dSort
			Delete File 'F30dSort.Dbf'


			Select DiasCierre
			Go Top
			If Eof()
*				=MessageBox('No hay cierres de este mes',16,'TUNDEN')
*				Return .F.
			EndIF



			Select DiasCierre
			Go Top
			Store '' To _CodPro,_CodArt,_SitStk
			Store 0 To _Pes,_Vol
			Do While !Eof()
				
				If _codPro <> DiasCierre.F30dCodPro .Or. _CodArt <> DiasCierre.F30dCodArt .Or. _SitStk <> DiasCierre.F30dSitStk
					Select Maximas
					Append Blank
					Replace CodPro With DiasCierre.F30dCodPro, ;
							CodArt With DiasCierre.F30dCodArt, ;
							SitStk With DiasCierre.F30dSitStk
					_CodPro = DiasCierre.F30dCodPro
					_CodArt = DiasCierre.F30dCodArt
					_SitStk = DiasCierre.F30dSitStk
					Select DiasCierre
				EndIf

				If DiasCierre.F30dTotPes > Maximas.Peso .Or. DiasCierre.F30dTotpes <= 0
				   Select Maximas
				   Replace Peso With IIF(DiasCierre.F30dTotPes<0,0,DiasCierre.F30dtotPes), ;
				   		   Fechap With DiasCierre.F30dFecha,;
				   		   StockP With DiasCierre.F30dStock
			       Select DiasCierre
				EndIf

				If DiasCierre.F30dTotVol > Maximas.Volumen .Or. DiasCierre.F30dTotVol <= 0
				   Select Maximas
				   Replace Volumen With IIF(DiasCierre.F30dTotVol<0,0,DiasCierre.F30dTotVol), ;
				   		   Fechav With DiasCierre.F30dFecha	,;
				   		   Stockv With DiasCierre.F30dStock
			       Select DiasCierre
				EndIf

				Select DiasCierre
			   Skip
			EndDo						
	     Select Quincena
		 Skip
		 EndDo									



		Select Maximas
		Go Top

		Do While !Eof()
				
			*> Datos del Artículo.---------------------------------------
			_Sentencia = " Select F08cTipUni,F08cDescri,F08cPcoste,Nvl(F08cVolUni,0) As F08cVolUni,Nvl(F08cPesUni,0) As F08cPesUni From F08c001 " + ;
						 " Where F08cCodPro = '" + Maximas.CodPro + ;
						 "' And F08cCodArt = '" + Maximas.CodArt + "'"
						 
			_Ok = SqlExec(_Asql,_Sentencia,'DatosArt')
			=SqlMoreResults(_Asql)
			If _Ok < 1
				=MessageBox('Error leyendo datos artículo',16,'TUNDEN')
				Return .F.
			EndIf
			
			*> Leemos el F32c.-----------------------------------------
			_Sentencia = " Select * from F32c001 Where F32cCodPro = '" + Maximas.CodPro + "'"
			_Ok = SqlExec(_Asql,_Sentencia,'F32cMas')
			=SqlMoreResults(_Asql)

		*	m.F32cCodPro = Maximas.CodPro
		*	=f3_seek('F32c')

			_Selec= "F34tCodPro='" + Maximas.CodPro + "' And F34tCodTar='ALMS'"

			If !f3_sql('*', 'F34t', _Selec, , , 'F34tCur')
		   	   _LxErr = _LxErr + 'No hay tarifas seguro propietario: ' + Maximas.CodPro + cr
		   	   Return
			EndIf

			Select F34tCur
			Locate For F34tCodArt=Maximas.CodArt
			If !Found()
				Go Top
			EndIf

			*> El seguro se calcula sobre la punta !!!!!!!

			*> Como puede ser kilos o unidades.-------------------(LUIS 01/03/2002)
		    Cantidad = IIF(F34tCur.F34tUnitat = 'DM3', Maximas.Volumen, Maximas.Peso)
			Canfisi = IIF(F34tCur.F34tUnitat = 'DM3', Maximas.StockV, Maximas.StockP)
		    Fechac = IIF(F34tCur.F34tUnitat = 'DM3', Maximas.FechaV, Maximas.FechaP)

			*> Como puede ser kilos o unidades.-------------------(LUIS 03/04/2002)
			If F34tCur.F34tUnitat = 'DM3'
				If DatosArt.F08cVolUni <= 0
			   	   _LxErr = _LxErr + 'Volumen unitario cero: ' + Maximas.CodPro + Space(1) + Maximas.CodArt + cr
				   Return .F.
				EndIf
			Else
				If DatosArt.F08cPesUni <= 0
			   	   _LxErr = _LxErr + 'Peso unitario cero: ' + Maximas.CodPro + Space(1) + Maximas.CodArt + cr
				   Return .F.
				EndIf
			EndIf

			Select XLVALMES
			Append Blank
			Replace CodPro With Maximas.CodPro,;
					F08cCodArt With Maximas.CodArt,;
					F08cDescri With DatosArt.F08cDescri,;
					Fecha With Fechac,;
					Cantid With 0,;
					Canfis With CanFisi,;
					Canrea With Cantidad,;
					Precio With DatosArt.F08cPCoste,;
					Tarifa With F32cMas.F32cUniFac,;
					Quince With IIF(Val(Substr(Dtoc(Fecha),1,2))>15,'2','1'),;
					SitStk With Maximas.SitStk
			
					If Empty(f32cmas.F32cFijVar)
				   	   _LxErr = _LxErr + 'Tipo Calculo seguro vacio: ' + Maximas.CodPro + Space(1) + Maximas.CodArt + cr
					   Return .F.
					EndIf

					*> Aqui distribuimos los campos.--------------------------------------
					Do Case
						Case F32cmas.F32cFijVar = 'FIJO'
							 Select XLVALMES
							 Replace Canrea With Cantidad / IIF(F34tCur.F34tUnitat = 'DM3',DatosArt.F08cVolUni,DatosArt.F08cPesUni),;
							 Tarifa With DatosArt.F08cTipUni


						OtherWise
							 Select XLVALMES
							 Replace CanRea With Cantidad

					EndCase					

					*> Como puede ser kilos o unidades.-------------------(LUIS 03/04/2002)
					If F32cmas.F32cFijVar = 'FIJO'
						Replace Cantid With Cantid + (Cantidad * DatosArt.F08cPCoste) / IIF(F34tCur.F34tUnitat = 'DM3',DatosArt.F08cVolUni,DatosArt.F08cPesUni)
					Else
						Replace Cantid With Cantid + (Cantidad * DatosArt.F08cPCoste)
					EndIf

			Select Maximas
			Skip
		EndDo

	Select Maximas 
	Delete all
	Select Propi
	Skip
EndDo

Wait Clear


Return lEstado



************************************************************************
*> Listado relación seguro.		                                       *
*> Primer paso: Generar datos del año/mes.                             *
*>                                                                     *
*> Recibe: cAnyo ----> Año a calcular.                                 *
*>         cMes -----> Mes a calcular.                                 *
*>                                                                     *
*> Genera: XLVALMES ----> Cursor con los datos a imprimir.             *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FacSegPro ---> Relación seguro.			                       *
************************************************************************
Procedure ValLabSeguroMes
Parameters cAnyo, cMes

Private cWhere, cFromF, cField, cOrder, cGroup
Local lEstado, nSaldo
Local cFechaInicial, dFechaActual, cFechaSaldoInicial, nSaldoAnterior
Local nInx

lEstado = .T.

If Used('F30DPREV')
   Use In F30DPREV
EndIf
If Used('XLVALMES')
   Use In XLVALMES
EndIf

Create Cursor Maximas  (CodPro C(6), CodArt C(13) , FechaP D , ;
						Peso N(10,3),;
						FechaV D ,;
						Volumen N(10,3),;
						StockP N(10,3),;
						StockV N(10,3),;
						SitStk C(4))

Create Cursor XLVALMES (CodPro C(6) , F08cCodArt C(13) ,;
						F08cDescri C(40), Fecha D ,;
						Cantid N(10,3), Canfis N(10,3),;
						CanRea N(10,3),;
						Tarifa C(4) ,;
						Precio N(10,6),;
						Quince C (1),;
						SitStk C (4))

*> Cuantas Quincenas Calculamos.------------------------------------------------
If m.Labs = 0
	_Sentencia = "Select * from F01p001 Where F01pCodigo Between '" + Inc_Cod + ;
				 "' And '" + Fin_cod + "'" + ;
				 " Order By F01pCodigo "
Else
	Inc_cod ='      '
	Fin_cod ='zzzzzz'

	_Sentencia = "Select * from F01p001 Where F01pCodigo Between '" + Inc_Cod + ;
				 "' And '" + Fin_cod + "'" + ;
				 " Order By F01pCodigo "

EndIf

_Ok = SqlExec(_Asql,_Sentencia,'Propi')
=SqlMoreResults(_Asql)			 


Select Propi
Go Top
Do While !Eof()

		Wait Window 'Tratando Propietario ' + Propi.F01pCodigo NoWait

		*> Cuantas Quincenas.----------------------------------------------------
		_Sentencia = " Select * from f34f001 Where F34fCodPro = '" + Propi.F01pCodigo + ;
					 "' And F34fMes = '" + cMes + ;
					 "' And F34fAnyo = '" + cAnyo + "'" + ;
					 " Order By F34fQuince,F34fCodArt,F34fSitStk "

		_Ok = SqlExec(_Asql,_Sentencia,'Quincena')
		=SqlMoreResults(_Asql)			 



		Select Quincena
		Go Top
		Do While !Eof()


			*> Haremos la discriminación.----------------------------------------------
			*> los tipos de producto 3000 no cobran otra cosa q no sea SitStk = 1000.--
			_Sentencia = " Select F08cTipPro From F08c001 " + ;
						 " Where F08cCodPro = '" + Propi.F01pCodigo + ;
						 "' And F08cCodArt = '" + Quincena.F34fCodArt + "'"
						 
			_Ok = SqlExec(_Asql,_Sentencia,'TipPro')
			If _Ok < 1
			   =MessageBox('Error consultando Tipos de producto',16,'TUNDEN')
			   Return .F.	
			EndIf

			Select Quincena

			*> Si es de Laboratorio que lo paguen ellos.--------------------
			If m.Labs = 0
				If Quincena.F34fSitStk <> '1000' .And. TipPro.F08cTipPro = '3000'
				   Skip
				   Loop
				EndIf
			Else
				If Quincena.F34fSitStk <> '8000' .Or. TipPro.F08cTipPro <> '3000'
				   Skip
				   Loop
				EndIf
			EndIf

			Fechad = IIF(F34fQuince = '1','01/'+cMes+'/'+cAnyo,'16'+cMes+'/'+cAnyo)
			Fechah = IIF(F34fQuince = '2','01/'+Right('00'+Alltrim(Str(Val(cMes)+1)),2)+'/'+cAnyo,'15/'+cMes+'/'+cAnyo)
			Fechah = Ctod(Fechah)
			Fechah = IIF(F34fQuince = '2',Dtoc(fechah-1),Dtoc(fechah))

			*> Recogemos los dias de ese mes en el cierre diario.------------------
			_Sentencia  = " Select * " + ;
						  " From F30d001 Where F30dFecha between to_Date('" + Fechad + "','dd-mm-yyyy')" + ;
						  " And To_date('" + Fechah + "','dd-mm-yyyy')" + ;
						  " And F30dCodPro = '" + Quincena.F34fCodPro + "'" + ;
						  " And F30dCodArt = '" + Quincena.F34fCodArt + "'" + ;
						  " And F30dSitStk = '" + Quincena.F34fSitStk + "'"

*!*				_Sentencia  = " Select F30dCodPro,F30dCodArt,F30dFecha " + ;
*!*							  ", Sum(Nvl(F30dTotVol,0)) As F30dTotVol " + ;
*!*							  ", Sum(Nvl(F30dTotPes,0)) As F30dTotPes " + ;
*!*							  ", Sum(Nvl(F30dStock,0)) As F30dStock " + ;
*!*							  " From F30d001 Where F30dFecha between to_Date('" + Fechad + "','dd-mm-yyyy')" + ;
*!*							  " And To_date('" + Fechah + "','dd-mm-yyyy')" + ;
*!*							  " And F30dCodPro BetWeen '" + Inc_Cod + "' And '" + Fin_cod + "'" + ;
*!*							  " Group By F30dCodpro,F30dCodArt,F30dFEcha "

*						  " And F30dSitStk = '1000' " 

		*			  " Order By F30dCodPro,F30dCodArt "

			_Ok = SqlExec(_Asql,_Sentencia,'DiasCierre')
			=SqlMoreResults(_Asql)
			If _Ok < 1
				=MessageBox('Error consultando cierre',16,'TUNDEN')
				Return .F.
			EndIf

		    *> Reclasificar el cursor de trabajo, dado que se pueden haber insertado registros al final.
			Select DiasCierre
			Sort To 'F30dSort' On F30dCodPro/A, F30dCodArt/A,  F30dSitStk/A, F30dFecha/A
			Zap
			Append From F30dSort
			Delete File 'F30dSort.Dbf'


			Select DiasCierre
			Go Top
			If Eof()
*				=MessageBox('No hay cierres de este mes',16,'TUNDEN')
*				Return .F.
			EndIF



			Select DiasCierre
			Go Top
			Store '' To _CodPro,_CodArt,_SitStk
			Store 0 To _Pes,_Vol
			Do While !Eof()
				
				If _codPro <> DiasCierre.F30dCodPro .Or. _CodArt <> DiasCierre.F30dCodArt .Or. _SitStk <> DiasCierre.F30dSitStk
					Select Maximas
					Append Blank
					Replace CodPro With DiasCierre.F30dCodPro, ;
							CodArt With DiasCierre.F30dCodArt, ;
							SitStk With DiasCierre.F30dSitStk
					_CodPro = DiasCierre.F30dCodPro
					_CodArt = DiasCierre.F30dCodArt
					_SitStk = DiasCierre.F30dSitStk
					Select DiasCierre
				EndIf

				If DiasCierre.F30dTotPes > Maximas.Peso .Or. DiasCierre.F30dTotpes <= 0
				   Select Maximas
				   Replace Peso With IIF(DiasCierre.F30dTotPes<0,0,DiasCierre.F30dtotPes), ;
				   		   Fechap With DiasCierre.F30dFecha,;
				   		   StockP With DiasCierre.F30dStock
			       Select DiasCierre
				EndIf

				If DiasCierre.F30dTotVol > Maximas.Volumen .Or. DiasCierre.F30dTotVol <= 0
				   Select Maximas
				   Replace Volumen With IIF(DiasCierre.F30dTotVol<0,0,DiasCierre.F30dTotVol), ;
				   		   Fechav With DiasCierre.F30dFecha	,;
				   		   Stockv With DiasCierre.F30dStock
			       Select DiasCierre
				EndIf

				Select DiasCierre
			   Skip
			EndDo						
	     Select Quincena
		 Skip
		 EndDo									



		Select Maximas
		Go Top
		Do While !Eof()
				
			*> Datos del Artículo.---------------------------------------
			_Sentencia = " Select F08cTipUni,F08cDescri,F08cPcoste,Nvl(F08cVolUni,0) As F08cVolUni,Nvl(F08cPesUni,0) As F08cPesUni From F08c001 " + ;
						 " Where F08cCodPro = '" + Maximas.CodPro + ;
						 "' And F08cCodArt = '" + Maximas.CodArt + "'"
						 
			_Ok = SqlExec(_Asql,_Sentencia,'DatosArt')
			=SqlMoreResults(_Asql)
			If _Ok < 1
				=MessageBox('Error leyendo datos artículo',16,'TUNDEN')
				Return .F.
			EndIf
			
			*> Leemos el F32c.-----------------------------------------
			_Sentencia = " Select * from F32c001 Where F32cCodPro = '" + Maximas.CodPro + "'"
			_Ok = SqlExec(_Asql,_Sentencia,'F32cMas')
			=SqlMoreResults(_Asql)

		*	m.F32cCodPro = Maximas.CodPro
		*	=f3_seek('F32c')

			_Selec= "F34tCodPro='" + Maximas.CodPro + "' And F34tCodTar='ALMS'"

			If !f3_sql('*', 'F34t', _Selec, , , 'F34tCur')
		   	   _LxErr = _LxErr + 'No hay tarifas seguro propietario: ' + Maximas.CodPro + cr
		   	   Return
			EndIf

			Select F34tCur
			Locate For F34tCodArt=Maximas.CodArt
			If !Found()
				Go Top
			EndIf

			*> El seguro se calcula sobre la punta !!!!!!!

			*> Como puede ser kilos o unidades.-------------------(LUIS 01/03/2002)
		    Cantidad = IIF(F34tCur.F34tUnitat = 'DM3', Maximas.Volumen, Maximas.Peso)
			Canfisi = IIF(F34tCur.F34tUnitat = 'DM3', Maximas.StockV, Maximas.StockP)
		    Fechac = IIF(F34tCur.F34tUnitat = 'DM3', Maximas.FechaV, Maximas.FechaP)

			*> Como puede ser kilos o unidades.-------------------(LUIS 03/04/2002)
			If F34tCur.F34tUnitat = 'DM3'
				If DatosArt.F08cVolUni <= 0
			   	   _LxErr = _LxErr + 'Volumen unitario cero: ' + Maximas.CodPro + Space(1) + Maximas.CodArt + cr
				   Return .F.
				EndIf
			Else
				If DatosArt.F08cPesUni <= 0
			   	   _LxErr = _LxErr + 'Peso unitario cero: ' + Maximas.CodPro + Space(1) + Maximas.CodArt + cr
				   Return .F.
				EndIf
			EndIf

			Select XLVALMES
			Append Blank
			Replace CodPro With Maximas.CodPro,;
					F08cCodArt With Maximas.CodArt,;
					F08cDescri With DatosArt.F08cDescri,;
					Fecha With Fechac,;
					Cantid With 0,;
					Canfis With CanFisi,;
					Canrea With Cantidad,;
					Precio With DatosArt.F08cPCoste,;
					Tarifa With F32cMas.F32cUniFac,;
					Quince With IIF(Val(Substr(Dtoc(Fecha),1,2))>15,'2','1'),;
					SitStk With Maximas.SitStk
			
					If Empty(f32cmas.F32cFijVar)
				   	   _LxErr = _LxErr + 'Tipo Calculo seguro vacio: ' + Maximas.CodPro + Space(1) + Maximas.CodArt + cr
					   Return .F.
					EndIf

					*> Aqui distribuimos los campos.--------------------------------------
					Do Case
						Case F32cmas.F32cFijVar = 'FIJO'
							 Select XLVALMES
							 Replace Canrea With Cantidad / IIF(F34tCur.F34tUnitat = 'DM3',DatosArt.F08cVolUni,DatosArt.F08cPesUni),;
							 Tarifa With DatosArt.F08cTipUni


						OtherWise
							 Select XLVALMES
							 Replace CanRea With Cantidad

					EndCase					

					*> Como puede ser kilos o unidades.-------------------(LUIS 03/04/2002)
					If F32cmas.F32cFijVar = 'FIJO'
						Replace Cantid With Cantid + (Cantidad * DatosArt.F08cPCoste) / IIF(F34tCur.F34tUnitat = 'DM3',DatosArt.F08cVolUni,DatosArt.F08cPesUni)
					Else
						Replace Cantid With Cantid + (Cantidad * DatosArt.F08cPCoste)
					EndIf

			Select Maximas
			Skip
		EndDo

	Select Maximas 
	Delete all
	Select Propi
	Skip
EndDo


Wait Clear


Return lEstado

