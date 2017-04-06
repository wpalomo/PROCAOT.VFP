*> Capçalera

   *> Descripció ................. Listados de albaranes.
   *> Modul ...................... Salialbc.Prg
   *> Llenguatge ................. Visual FoxPro 6.0
   *> Sistema Operatiu ........... Windows'95
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Diseny ..................... Abraham Viñas
   *> Programador ................ Abraham Viñas
   *> Data d'inici ............... 07.11.2000
   *> Data de fi ................. 07.11.2000

*> Módulo......................... SaliAlbCNew()      (Impresión albaranes cliente, selección)
*>                                 GenAlbCli()        (Impresión albaranes cliente, generar)
*>                                 PrtAlbCli()        (Impresión albaranes cliente, imprimir - interactivo)
*>                                 PrtAlbCliB()       (Impresión albaranes cliente, imprimir - BATCH)

******************************************************************
*> Impresión de Albaranes de Clientes.
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

      =WaitWindow ('Calculando datos albarán: ' + oAlb.F24cCodPro + '-' + oAlb.F24cTipDoc + '-' + oAlb.F24cNumDoc)

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
*> Procedimiento de impresión del albarán de UN documento de salida.
*> Primer paso: Generar datos del albarán actual.
*> Adaptado a procesos BATCH. AVC - 09.11.2006

*> Recibe:
*>	_CodPro ---> Código de propietario.
*>	_TipDoc ---> Tipo de documento.
*>	_NumDoc ---> Nº de Documento.
*>	_NumAlb ---> Nº de Albarán (opcional).

*> Devuelve:
*>	- NCURSOR, cursor con los datos a imprimir.
*>	- cEstado, resultado de la operación.
*>		'S': OK
*>		'N': No hay datos.
*>		'C': Error en operación BBDD.

*> Módulo llamado desde:
*>   - SaliAlbCN ----> Impresión de albaranes. (Por SaliAlbC3)
*>   - BATCH.VCX ----> Procesos BATCH, impresión de albaranes.

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

   *> Dado el límite impuesto por VFP6 al nº máximo de columnas (255), las de F24c/F24t se añaden aparte.
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

   *> Cargar datos de dirección.
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
   
   *> Cargar datos de observaciones (Cabecera), si hay. Solo toma la primera línea de observaciones.
   f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='0000'"
   lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
   If lStado
      Select F24oCur
      Go Top
      Select NCursor
      Replace All CabObs With F24oCur.F24oDesObs
   EndIf

   *> Cargar datos de observaciones (Pie), si hay. Solo toma la primera línea de observaciones.
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

   *> Cargar datos asociados a las líneas de detalle.
   Select NCursor
   Go Top
   Do While !Eof()
      Scatter Name oDoc

      *> Cargar datos de observaciones (Línea), si hay. Solo toma la primera línea de observaciones.
      f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='" + oDoc.F24lLinDoc + "'"
      lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
      If lStado
         Select F24oCur
         Go Top
         Select NCursor
         Replace DetObs With F24oCur.F24oDesObs
      EndIf

      *> Cargar datos de transportista, leídos con anterioridad.
      Select NCursor
      Gather Name oF01t
      Gather Name oF24t

      Skip
   EndDo

   *> Pasar los datos al cursor de impresión.
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
*> Procedimiento de impresión de albaranes de cliente.
*> Segundo paso: Imprimir los datos generados por GenAlbCli.

*> Recibe:
*>	- XLSALIALBC, cursor con los datos para imprimir.

*> Módulo llamado desde:
*>   - SaliAlbCN ----> Impresión de albaranes.
************************************************************************
Procedure PrtAlbCli

Local _Inx
Private loImp2, cRpt, cLbl, cCrs

	If Used("XLSaliAlbC")
		Select XLSaliAlbC
		Go Top

		*> Objeto impresión.
		cRpt = 'SaliAlbCL1'
		cLbl = 'IMPRESION DE ALBARAN DE CLIENTE'
		cCrs = 'XLSaliAlbC'

		*> Impresión por defecto.
		loImp2 = CreateObject('st3imp2', , [RPT=&cRpt, CRS=&cCrs])
		=loImp2.Exec('?', [LBL=&cLbl])

		*> Impresión de las copias. Utiliza la variable pública m.NumCop
		For _Inx = 1 To m.NumCop
			=loImp2.Exec('P')
		EndFor

	   loImp2.Release
	EndIf

	Use In (Select('XLSaliAlbC'))

Return

************************************************************************
*> Procedimiento de impresión de albaranes de cliente.
*> Segundo paso: Imprimir los datos generados por GenAlbCli.
*> Versión para llamadas en BATCH.

*> Recibe:
*>	- XLSALIALBC, cursor con los datos para imprimir.
*>	- cTarea, Tarea que llama (para gestión de colas).
*>	- cReport, report de salida.
*>	- nCopias, nº de copias de salida.
*>	- cDevice, Dispositivo de salida.

*> Devuelve:
*>	- cEstado, resultado de la operación.
*>		'S': OK
*>		'N': No hay colas.
*>		'C': Error en operación BBDD.

*> Módulo llamado desde:
*>   - Procesos BATCH.
************************************************************************
Procedure PrtAlbCliB
Parameters cTarea, cReport, cCopias, cDevice

Local _Inx, cEstado, nCopias
Private loImp2, cCrs

	If Used("XLSaliAlbC")
		Select XLSaliAlbC
		Go Top

		*> Objeto impresión.
		cCrs = 'XLSaliAlbC'
		nCopias = Val(cCopias)

		*> Impresión por defecto.
		loImp2 = CreateObject('st3imp2', , [RPT=&cReport, CRS=&cCrs, PRINTER=&cDevice])
		=loImp2.Exec('P')

		*> Impresión de las copias.
		For _Inx = 1 To nCopias
			=loImp2.Exec('P')
		EndFor

	   loImp2.Release
	EndIf

	cEstado = "S"

	Use In (Select('XLSaliAlbC'))

Return cEstado





******************************************************************
*> Impresión de Albaranes de Clientes.
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

      =WaitWindow ('Calculando datos albarán: ' + m.CodPro + '-' + m.TipDoc + '-' + m.NumDoc)

      *> Generar datos del documento actual.
      Do GenAlbCli_Repaso With m.CodPro, m.TipDoc, m.NumDoc


   *> Imprimir los albaranes
   Do PrtAlbCli_Repaso
   Use In (Select("XLSaliAlbC"))

Return

************************************************************************
*> Procedimiento de impresión del albarán de UN documento de salida.
*> Primer paso: Generar datos del albarán actual.
*> Adaptado a procesos BATCH. AVC - 09.11.2006

*> Recibe:
*>	_CodPro ---> Código de propietario.
*>	_TipDoc ---> Tipo de documento.
*>	_NumDoc ---> Nº de Documento.
*>	_NumAlb ---> Nº de Albarán (opcional).

*> Devuelve:
*>	- NCURSOR, cursor con los datos a imprimir.
*>	- cEstado, resultado de la operación.
*>		'S': OK
*>		'N': No hay datos.
*>		'C': Error en operación BBDD.

*> Módulo llamado desde:
*>   - SaliAlbCN ----> Impresión de albaranes. (Por SaliAlbC3)
*>   - BATCH.VCX ----> Procesos BATCH, impresión de albaranes.

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

   *> Dado el límite impuesto por VFP6 al nº máximo de columnas (255), las de F24c/F24t se añaden aparte.
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

   *> Cargar datos de dirección.
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
   
   *> Cargar datos de observaciones (Cabecera), si hay. Solo toma la primera línea de observaciones.
   f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='0000'"
   lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
   If lStado
      Select F24oCur
      Go Top
      Select NCursor
      Replace All CabObs With F24oCur.F24oDesObs
   EndIf

   *> Cargar datos de observaciones (Pie), si hay. Solo toma la primera línea de observaciones.
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

   *> Cargar datos asociados a las líneas de detalle.
   Select NCursor
   Go Top
   Do While !Eof()
      Scatter Name oDoc

      *> Cargar datos de observaciones (Línea), si hay. Solo toma la primera línea de observaciones.
      f_where = "F24oCodPro='" + _CodPro + "' And F24oTipDoc='" + _TipDoc + "' And F24oNumDoc='" + _NumDoc + "' And F24oLinObs='" + oDoc.F24lLinDoc + "'"
      lStado = f3_sql('*', 'F24o', f_where, , , 'F24oCur')
      If lStado
         Select F24oCur
         Go Top
         Select NCursor
         Replace DetObs With F24oCur.F24oDesObs
      EndIf

      *> Cargar datos de transportista, leídos con anterioridad.
      Select NCursor
      Gather Name oF01t
      Gather Name oF24t

      Skip
   EndDo

   *> Pasar los datos al cursor de impresión.
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
*> Procedimiento de impresión de albaranes de cliente.
*> Segundo paso: Imprimir los datos generados por GenAlbCli.

*> Recibe:
*>	- XLSALIALBC, cursor con los datos para imprimir.

*> Módulo llamado desde:
*>   - SaliAlbCN ----> Impresión de albaranes.
************************************************************************
Procedure PrtAlbCli_Repaso

Local _Inx
Private loImp2, cRpt, cLbl, cCrs

	If Used("XLSaliAlbC")
		Select XLSaliAlbC
		Go Top

		*> Objeto impresión.
		cRpt = 'SaliAlbCL1'
		cLbl = 'IMPRESION DE PRE-ALBARAN DE CLIENTE'
		cCrs = 'XLSaliAlbC'

		*> Impresión por defecto.
		loImp2 = CreateObject('st3imp2', , [RPT=&cRpt, CRS=&cCrs])
		=loImp2.Exec('?', [LBL=&cLbl])

*!*			*> Impresión de las copias. Utiliza la variable pública m.NumCop
*!*			For _Inx = 1 To m.NumCop
*!*				=loImp2.Exec('P')
*!*			EndFor

	   loImp2.Release
	EndIf

	Use In (Select('XLSaliAlbC'))

Return
