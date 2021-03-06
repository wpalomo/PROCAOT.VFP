*> Cap�alera

   *> Descripci� ................. Listados de albaranes.
   *> Modul ...................... Salialbc.Prg
   *> Llenguatge ................. Visual FoxPro 6.0
   *> Sistema Operatiu ........... Windows'95
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Diseny ..................... Abraham Vi�as
   *> Programador ................ Abraham Vi�as
   *> Data d'inici ............... 07.11.2000
   *> Data de fi ................. 07.11.2000

*> M�dulo......................... SaliAlbCNew()      (Impresi�n albaranes cliente, selecci�n)
*>                                 GenAlbCli()        (Impresi�n albaranes cliente, generar)
*>                                 PrtAlbCli()        (Impresi�n albaranes cliente, imprimir - interactivo)
*>                                 PrtAlbCliB()       (Impresi�n albaranes cliente, imprimir - BATCH)

******************************************************************
*> Impresi�n de Albaranes de Clientes.
*> Recibe: ALBCLI, documentos a imprimir.
*> Llamado desde SALIALBCN.

*> Historial de modificaciones:
*>	- Permitir expediciones parciales. AVC - 13.06.2007
******************************************************************
Procedure SaliAlbCN
Private oAlb

   *> Imprimir albaranes.
   Select ALBCLI
   Locate For Marca==1
   Do While Found()
      Scatter Name oAlb

      =WaitWindow ('Calculando datos albar�n: ' + oAlb.F24cCodPro + '-' + oAlb.F24cTipDoc + '-' + oAlb.F24cNumDoc)

      *> Generar datos del documento actual.
      Do GenAlbCli With oAlb.F24cCodPro, oAlb.F24cTipDoc, oAlb.F24cNumDoc, oAlb.F27cNumAlb

      *> Leer el siguiente documento.
      Select ALBCLI
      Continue
   EndDo      

   *> Imprimir los albaranes
   Do PrtAlbCli
   Use In (Select("XLSaliAlbC"))

Return

************************************************************************
*> Procedimiento de impresi�n del albar�n de UN documento de salida.
*> Primer paso: Generar datos del albar�n actual.
*> Adaptado a procesos BATCH. AVC - 09.11.2006

*> Recibe:
*>	_CodPro ---> C�digo de propietario.
*>	_TipDoc ---> Tipo de documento.
*>	_NumDoc ---> N� de Documento.
*>	_NumAlb ---> N� de Albar�n (opcional).

*> Devuelve:
*>	- NCURSOR, cursor con los datos a imprimir.
*>	- cEstado, resultado de la operaci�n.
*>		'S': OK
*>		'N': No hay datos.
*>		'C': Error en operaci�n BBDD.

*> M�dulo llamado desde:
*>   - SaliAlbCN ----> Impresi�n de albaranes. (Por SaliAlbC3)
*>   - BATCH.VCX ----> Procesos BATCH, impresi�n de albaranes.

*> Historial de modificaciones:
*>	- Permitir expediciones parciales. AVC - 13.06.2007
************************************************************************

Procedure GenAlbCli

   Parameters _CodPro, _TipDoc, _NumDoc, _NumAlb

   Local lStado, cEstado
   Private f_from, f_where, f_orden
   Private oF01t, oF24t, oDoc

   *> Crear macro cursor de trabajo.
   =CrtMCursor([F24l,F27c,F27l,F01p,F01t,F08c,F00h], 'NCursor', 'C')

   *> Dado el l�mite impuesto por VFP6 al n� m�ximo de columnas (255), las de F24c/F24t se a�aden aparte.
   =CrtFCursor('NCursor', [TBL=F24c,FLD=F24cCodPro,FLD=F24cNumDoc,FLD=F24cNumPed,FLD=F24cDirAso,FLD=F24cFecDoc,FLD=F24cCodTra,FLD=F24cPortes])
   =CrtFCursor('NCursor', [TBL=F24t,FLD=F24tNomAso,FLD=F24tNumNif,FLD=F24t1erDir,FLD=F24t2ndDir,FLD=F24t3rdDir])
   =CrtFCursor('NCursor', [TBL=F24t,FLD=F24tCodPos,FLD=F24tDPobla,FLD=F24tDProvi])

   *> Agregar campos de trabajo adicionales.
   =AddFldToCursor('NCursor', [NAME=CabObs,TYPE=C,LENGTH=60])
   =AddFldToCursor('NCursor', [NAME=PieObs,TYPE=C,LENGTH=60])
   =AddFldToCursor('NCursor', [NAME=DetObs,TYPE=C,LENGTH=60])

   f_from = "F24c,F24l,F27c,F27l,F08c,F01p"

   f_where =           "F24cCodPro='" + _CodPro + "'And F24cTipDoc='" + _TipDoc + "' And F24cNumDoc='" + _NumDoc + "' And "
   f_where = f_where + "F24lCodPro=F24cCodPro And F24lTipDoc=F24cTipDoc And F24lNumDoc=F24cNumDoc And "
   f_where = f_where + "F27cCodPro=F24cCodPro And F27cTipDoc=F24cTipDoc And F27cNumDoc=F24cNumDoc And "
   f_where = f_where + "F27lCodPro=F24lCodPro And F27lTipDoc=F24lTipDoc And F27lNumDoc=F24lNumDoc And F27lLinDoc=F24lLinDoc And F27lNumAlb=F27cNumAlb And "
   f_where = f_where + "F01pCodigo=F24cCodPro And "
   f_where = f_where + "F08cCodPro=F27lCodPro And F08cCodArt=F27lCodArt"

   If Type('_NumAlb')=='C'
      f_where = f_where + " And F27cNumAlb='" + _NumAlb + "'"
   EndIf

   f_orden = "F24cCodPro, F24cTipDoc, F24cNumDoc, F24lLinDoc, F24lNumLot, F24lFecCad"

   lStado = f3_sql('*', f_from, f_where, f_orden, , 'NCursor')
   If !lStado
      *> No hay datos de este documento.
      Use In (Select("NCursor"))
      cEstado = 'N'
      Return cEstado
   EndIf

   *> Cargar datos de direcci�n.
   f_where = "F24tCodPro='" + _CodPro + "' And F24tTipDoc='" + _TipDoc + "' And F24tNumDoc='" + _NumDoc + "'"

   lStado = f3_sql('*', 'F24t', f_where, , , 'F24tCur')
   If lStado
      Select F24tCur
      Go Top
      Scatter Name oF24t
   Else
      Select F24t
      Scatter Name oF24t Blank
   EndIf
   
   *> Cargar datos de observaciones (Cabecera), si hay. Solo toma la primera l�nea de observaciones.
   f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='0000'"
   lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
   If lStado
      Select F24oCur
      Go Top
      Select NCursor
      Replace All CabObs With F24oCur.F24oDesObs
   EndIf

   *> Cargar datos de observaciones (Pie), si hay. Solo toma la primera l�nea de observaciones.
   f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='9999'"
   lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
   If lStado
      Select F24oCur
      Go Top
      Select NCursor
      Replace All PieObs With F24oCur.F24oDesObs
   EndIf

   *> Cargar datos transportista.
   Select NCursor
   Go Top
   Scatter Name oDoc

   m.F01tCodigo = oDoc.F24cCodTra
   If f3_seek('F01t')
      Select F01t
      Scatter Name oF01t
   Else
      Select F01t
      Scatter Name oF01t Blank
   EndIf

   *> Cargar datos asociados a las l�neas de detalle.
   Select NCursor
   Go Top
   Do While !Eof()
      Scatter Name oDoc

      *> Cargar datos de observaciones (L�nea), si hay. Solo toma la primera l�nea de observaciones.
      f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='" + oDoc.F24lLinDoc + "'"
      lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
      If lStado
         Select F24oCur
         Go Top
         Select NCursor
         Replace DetObs With F24oCur.F24oDesObs
      EndIf

      *> Cargar datos de transportista, le�dos con anterioridad.
      Select NCursor
      Gather Name oF01t
      Gather Name oF24t

      Skip
   EndDo

   *> Pasar los datos al cursor de impresi�n.
   If !Used("XLSaliAlbC")
      =CrtCursor("NCursor", "XLSaliAlbC", "C")
   EndIf

   Select XLSaliAlbC
   Append From Dbf('NCursor')

   Use In (Select ("F24tCur"))
   Use In (Select ("F24oCur"))
   Use In (Select ("NCursor"))

   cEstado = "S"

Return cEstado

************************************************************************
*> Procedimiento de impresi�n de albaranes de cliente.
*> Segundo paso: Imprimir los datos generados por GenAlbCli.

*> Recibe:
*>	- XLSALIALBC, cursor con los datos para imprimir.

*> M�dulo llamado desde:
*>   - SaliAlbCN ----> Impresi�n de albaranes.
************************************************************************
Procedure PrtAlbCli

Local _Inx
Private loImp2, cRpt, cLbl, cCrs

	If Used("XLSaliAlbC")
		Select XLSaliAlbC
		Go Top

		*> Objeto impresi�n.
		cRpt = 'SaliAlbCL1'
		cLbl = 'IMPRESION DE ALBARAN DE CLIENTE'
		cCrs = 'XLSaliAlbC'

		*> Impresi�n por defecto.
		loImp2 = CreateObject('st3imp2', , [RPT=&cRpt, CRS=&cCrs])
		=loImp2.Exec('?', [LBL=&cLbl])

		*> Impresi�n de las copias. Utiliza la variable p�blica m.NumCop
		For _Inx = 1 To m.NumCop
			=loImp2.Exec('P')
		EndFor

	   loImp2.Release
	EndIf

	Use In (Select('XLSaliAlbC'))

Return

************************************************************************
*> Procedimiento de impresi�n de albaranes de cliente.
*> Segundo paso: Imprimir los datos generados por GenAlbCli.
*> Versi�n para llamadas en BATCH.

*> Recibe:
*>	- XLSALIALBC, cursor con los datos para imprimir.
*>	- cTarea, Tarea que llama (para gesti�n de colas).
*>	- cReport, report de salida.
*>	- nCopias, n� de copias de salida.
*>	- cDevice, Dispositivo de salida.

*> Devuelve:
*>	- cEstado, resultado de la operaci�n.
*>		'S': OK
*>		'N': No hay colas.
*>		'C': Error en operaci�n BBDD.

*> M�dulo llamado desde:
*>   - Procesos BATCH.
************************************************************************
Procedure PrtAlbCliB
Parameters cTarea, cReport, cCopias, cDevice

Local _Inx, cEstado, nCopias
Private loImp2, cCrs

	If Used("XLSaliAlbC")
		Select XLSaliAlbC
		Go Top

		*> Objeto impresi�n.
		cCrs = 'XLSaliAlbC'
		nCopias = Val(cCopias)

		*> Impresi�n por defecto.
		loImp2 = CreateObject('st3imp2', , [RPT=&cReport, CRS=&cCrs, PRINTER=&cDevice])
		=loImp2.Exec('P')

		*> Impresi�n de las copias.
		For _Inx = 1 To nCopias
			=loImp2.Exec('P')
		EndFor

	   loImp2.Release
	EndIf

	cEstado = "S"

	Use In (Select('XLSaliAlbC'))

Return cEstado





******************************************************************
*> Impresi�n de Albaranes de Clientes.
*> Recibe: ALBCLI, documentos a imprimir.
*> Llamado desde SALIALBCN.

*> Historial de modificaciones:
*>	- Permitir expediciones parciales. AVC - 13.06.2007
******************************************************************
Procedure SaliAlbCN_Repaso
Private oAlb

   *> Imprimir albaranes.
   Select EXTDOC
   Scatter Name oAlb

      =WaitWindow ('Calculando datos albar�n: ' + m.CodPro + '-' + m.TipDoc + '-' + m.NumDoc)

      *> Generar datos del documento actual.
      Do GenAlbCli_Repaso With m.CodPro, m.TipDoc, m.NumDoc


   *> Imprimir los albaranes
   Do PrtAlbCli_Repaso
   Use In (Select("XLSaliAlbC"))

Return

************************************************************************
*> Procedimiento de impresi�n del albar�n de UN documento de salida.
*> Primer paso: Generar datos del albar�n actual.
*> Adaptado a procesos BATCH. AVC - 09.11.2006

*> Recibe:
*>	_CodPro ---> C�digo de propietario.
*>	_TipDoc ---> Tipo de documento.
*>	_NumDoc ---> N� de Documento.
*>	_NumAlb ---> N� de Albar�n (opcional).

*> Devuelve:
*>	- NCURSOR, cursor con los datos a imprimir.
*>	- cEstado, resultado de la operaci�n.
*>		'S': OK
*>		'N': No hay datos.
*>		'C': Error en operaci�n BBDD.

*> M�dulo llamado desde:
*>   - SaliAlbCN ----> Impresi�n de albaranes. (Por SaliAlbC3)
*>   - BATCH.VCX ----> Procesos BATCH, impresi�n de albaranes.

*> Historial de modificaciones:
*>	- Permitir expediciones parciales. AVC - 13.06.2007
************************************************************************

Procedure GenAlbCli_Repaso

   Parameters _CodPro, _TipDoc, _NumDoc

   Local lStado, cEstado
   Private f_from, f_where, f_orden
   Private oF01t, oF24t, oDoc

   *> Crear macro cursor de trabajo.
   =CrtMCursor([F24l,F27c,F27l,F01p,F01t,F08c,F00h], 'NCursor', 'C')

   *> Dado el l�mite impuesto por VFP6 al n� m�ximo de columnas (255), las de F24c/F24t se a�aden aparte.
   =CrtFCursor('NCursor', [TBL=F24c,FLD=F24cCodPro,FLD=F24cNumDoc,FLD=F24cNumPed,FLD=F24cDirAso,FLD=F24cFecDoc,FLD=F24cCodTra,FLD=F24cPortes,FLD=F24cNumBul,FLD=F24cTotKgs,FLD=F24cTotVol])
   =CrtFCursor('NCursor', [TBL=F24t,FLD=F24tNomAso,FLD=F24tNumNif,FLD=F24t1erDir,FLD=F24t2ndDir,FLD=F24t3rdDir])
   =CrtFCursor('NCursor', [TBL=F24t,FLD=F24tCodPos,FLD=F24tDPobla,FLD=F24tDProvi])

   *> Agregar campos de trabajo adicionales.
   =AddFldToCursor('NCursor', [NAME=CabObs,TYPE=C,LENGTH=60])
   =AddFldToCursor('NCursor', [NAME=PieObs,TYPE=C,LENGTH=60])
   =AddFldToCursor('NCursor', [NAME=DetObs,TYPE=C,LENGTH=60])

*!*	   f_where = f_where + "F27cCodPro=F24cCodPro And F27cTipDoc=F24cTipDoc And F27cNumDoc=F24cNumDoc And "
*!*	   f_where = f_where + "F27lCodPro=F24lCodPro And F27lTipDoc=F24lTipDoc And F27lNumDoc=F24lNumDoc And F27lLinDoc=F24lLinDoc And F27lNumAlb=F27cNumAlb And "



   f_from = "F24c,F24l,F08c,F01p"

   f_where =           "F24cCodPro='" + _CodPro + "'And F24cTipDoc='" + _TipDoc + "' And F24cNumDoc='" + _NumDoc + "' And "
   f_where = f_where + "F24lCodPro=F24cCodPro And F24lTipDoc=F24cTipDoc And F24lNumDoc=F24cNumDoc And "
   f_where = f_where + "F01pCodigo=F24cCodPro And "
   f_where = f_where + "F08cCodPro=F24lCodPro And F08cCodArt=F24lCodArt"

*!*	   If Type('_NumAlb')=='C'
*!*	      f_where = f_where + " And F27cNumAlb='" + _NumAlb + "'"
*!*	   EndIf

   f_orden = "F24cCodPro, F24cTipDoc, F24cNumDoc, F24lLinDoc, F24lNumLot, F24lFecCad"

   lStado = f3_sql('*', f_from, f_where, f_orden, , 'NCursor')
   If !lStado
      *> No hay datos de este documento.
      Use In (Select("NCursor"))
      cEstado = 'N'
      Return cEstado
   EndIf

   *> Cargar datos de direcci�n.
   f_where = "F24tCodPro='" + _CodPro + "' And F24tTipDoc='" + _TipDoc + "' And F24tNumDoc='" + _NumDoc + "'"

   lStado = f3_sql('*', 'F24t', f_where, , , 'F24tCur')
   If lStado
      Select F24tCur
      Go Top
      Scatter Name oF24t
   Else
      Select F24t
      Scatter Name oF24t Blank
   EndIf
   
   *> Cargar datos de observaciones (Cabecera), si hay. Solo toma la primera l�nea de observaciones.
   f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='0000'"
   lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
   If lStado
      Select F24oCur
      Go Top
      Select NCursor
      Replace All CabObs With F24oCur.F24oDesObs
   EndIf

   *> Cargar datos de observaciones (Pie), si hay. Solo toma la primera l�nea de observaciones.
   f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='9999'"
   lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
   If lStado
      Select F24oCur
      Go Top
      Select NCursor
      Replace All PieObs With F24oCur.F24oDesObs
   EndIf

   *> Cargar datos transportista.
   Select NCursor
   Go Top
   Scatter Name oDoc

   m.F01tCodigo = oDoc.F24cCodTra
   If f3_seek('F01t')
      Select F01t
      Scatter Name oF01t
   Else
      Select F01t
      Scatter Name oF01t Blank
   EndIf

   *> Cargar datos asociados a las l�neas de detalle.
   Select NCursor
   Go Top
   Do While !Eof()
      Scatter Name oDoc

      *> Cargar datos de observaciones (L�nea), si hay. Solo toma la primera l�nea de observaciones.
      f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='" + oDoc.F24lLinDoc + "'"
      lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
      If lStado
         Select F24oCur
         Go Top
         Select NCursor
         Replace DetObs With F24oCur.F24oDesObs
      EndIf

      *> Cargar datos de transportista, le�dos con anterioridad.
      Select NCursor
      Gather Name oF01t
      Gather Name oF24t

      Skip
   EndDo

   *> Pasar los datos al cursor de impresi�n.
   If !Used("XLSaliAlbC")
      =CrtCursor("NCursor", "XLSaliAlbC", "C")
   EndIf

   Select XLSaliAlbC
   Append From Dbf('NCursor')

   Use In (Select ("F24tCur"))
   Use In (Select ("F24oCur"))
   Use In (Select ("NCursor"))

   cEstado = "S"

Return cEstado

************************************************************************
*> Procedimiento de impresi�n de albaranes de cliente.
*> Segundo paso: Imprimir los datos generados por GenAlbCli.

*> Recibe:
*>	- XLSALIALBC, cursor con los datos para imprimir.

*> M�dulo llamado desde:
*>   - SaliAlbCN ----> Impresi�n de albaranes.
************************************************************************
Procedure PrtAlbCli_Repaso

Local _Inx
Private loImp2, cRpt, cLbl, cCrs

	If Used("XLSaliAlbC")
		Select XLSaliAlbC
		Go Top

		*> Objeto impresi�n.
		cRpt = 'SaliAlbCL1'
		cLbl = 'IMPRESION DE PRE-ALBARAN DE CLIENTE'
		cCrs = 'XLSaliAlbC'

		*> Impresi�n por defecto.
		loImp2 = CreateObject('st3imp2', , [RPT=&cRpt, CRS=&cCrs])
		=loImp2.Exec('?', [LBL=&cLbl])

*!*			*> Impresi�n de las copias. Utiliza la variable p�blica m.NumCop
*!*			For _Inx = 1 To m.NumCop
*!*				=loImp2.Exec('P')
*!*			EndFor

	   loImp2.Release
	EndIf

	Use In (Select('XLSaliAlbC'))

Return
