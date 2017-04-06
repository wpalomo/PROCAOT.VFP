*> Capçalera

   *> Descripció ................. Listados de albaranes y facturas.
   *> Modul ...................... Salialbc.Prg
   *> Llenguatge ................. Visual FoxPro 6.0
   *> Sistema Operatiu ........... Windows'95
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Diseny ..................... Abraham Viñas
   *> Programador ................ Abraham Viñas
   *> Data d'inici ............... 07.11.2000
   *> Data de fi ................. 07.11.2000

*> Notes

*> Módulo......................... SaliAlbC3()        (Impresión albaranes cliente, selección)
*>                                 SaliProf1()        (Impresión facturas proforma, selección)
*>                                 GenAlbCli()        (Impresión albaranes cliente, generar)
*>                                 GenFraPro()        (Impresión facturas proforma, generar)
*>                                 PrtAlbCli()        (Impresión albaranes cliente, imprimir)
*>                                 PrtFraPro()        (Impresión facturas proforma, imprimir)
*>                                 CalcDatosBultos()  (Impresión albaranes, bultos)
*>                                 AgruparPorLinea()  (Impresión albaranes, totalizar)


#Define COPIA_CLIENTE 'COPIA PARA EL CLIENTE'
#Define COPIA_DISTRIBUCION 'ALMACEN DISTRIBUCION (ORDEN NUMERICO)'
#Define COPIA_SERVICIO 'SERVICIO CLIENTES (ORDEN NUMERICO)'

******************************************************************
*> Impresión de Albaranes de Clientes.                           *
*> Según el flag F24cValAlb imprime albarán normal o valorado.   *
*> Llamado desde SALIALBC.                                       *
*> Tratamiento de Nº de copias. AVC - 05.04.2000                 *
******************************************************************
Procedure SaliAlbC3
Private _Inx, _inxold

   *> Primera pasada: Imprimir albarán SIN los estupefacientes.
   Select ALBCLI
   Locate For Marca==1
   Do While Found()
      Scatter MemVar

      Wait Window 'Calculando datos albarán: ' + ;
                   F24cCodPro + '-' + ;
                   F24cTipDoc + '-' + ;
                   F24cNumDoc + '-' + ;
                   F27cNumAlb NoWait

      *> Generar datos del documento, copia clientes.
	  _Inx = 1
*      For _Inx = 1 To F24cNumCop
         If ALBCLI.F24cCCliSn==1
            Do GenAlbCli With F24cCodPro, F24cTipDoc, F24cNumDoc, F27cNumAlb, _Inx, COPIA_CLIENTE
         EndIf
*      EndFor

      _InxOld = _Inx   

      *> Generar datos del documento, copia almacén.
*      If ALBCLI.F24cCAlmSn==1
*         Do GenAlbCli With F24cCodPro, F24cTipDoc, F24cNumDoc, F27cNumAlb, _InxOld+1, COPIA_DISTRIBUCION
*      EndIf

      *> Generar datos del documento, copia servicio clientes.
*      If ALBCLI.F24cCSerSn==1
*         Do GenAlbCli With F24cCodPro, F24cTipDoc, F24cNumDoc, F27cNumAlb, _InxOld+2, COPIA_SERVICIO
*      EndIf

      *> Leer el siguiente documento.
      Select ALBCLI
      Continue
         Documento=F24cNumDoc
      Modificado=.F.
      Temporal=0

   EndDo      

   Wait Clear

   *> Imprimir los albaranes.---------------------------------------
   Do PrtAlbCli

   If Used('XLSaliAlbC')
      Use In XLSaliAlbC
   EndIf

Return

******************************************************************
*> Impresión de Facturas proforma.                               *
*> Llamado desde SALIPROF.                                       *
******************************************************************
PROCEDURE SaliProf1

   Select FRAPRO
   Locate For Marca==1
   Do While Found()
      *> Generar datos de ESTE documento.
      Do GenFraPro With F24cCodPro, F24cTipDoc, F24cNumDoc

      *> Leer el siguiente documento.
      Select FRAPRO
      Continue
   EndDo

   *> Imprimir las facturas proforma. -----------------------------------
   Do PrtFraPro

   If Used('XLSaliProf')
      Use In XLSaliProf
   EndIf

Return

************************************************************************
*> Procedimiento de impresión del albarán de UN documento de salida.   *
*> Primer paso: Generar datos del albarán actual.                      *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _TipDoc ---> Tipo de documento.                             *
*>         _NumDoc ---> Nº de Documento.                               *
*>         _NumAlb ---> Nº de Albarán.                                 *
*>         _CurCop ---> Nº de copia actual.                            *
*>         _TxtCop ---> Texto copia actual.                            *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaliAlbC ----> Impresión de albaranes. (Por SaliAlbC3)          *
*>   - ProcVeri ----> Verificación TUNDEN.                             *
************************************************************************
Procedure GenAlbCli
   Parameters _CodPro, _TipDoc, _NumDoc, _NumAlb, _CurCop, _TxtCop

   Private lx_sql, lx_select, f_from, f_where, f_orden
   Private Err
   Private _Bucle

   Private Lin          && Línea del documento
   Private reg          && Registro al que volver  
   Private MpDoc        && Macs por Documento
   Private AuxCantLin   && Cantidad de Línea de Documento  
   Private AuxCantDoc   && Cantidad de Documento

   Public _IIDto

   Set NullDisplay To ''

   Store .T. To _Bucle
   Do While _Bucle == .T.
      *> Formar strings de conexión ORACLE.
      lx_select= " Select Distinct F26lCodPro, F26lTipDoc, F26lNumDoc, F26lLinDoc, F26lCodArt, " + ;
                         " F26lNumLot, F26lFecCad, F26lUniVen, F26lCajPal, F26lPacCaj, " + ;
                         " F26lUniPac, F26lNumMov, F26lUbiOri, " + ; 
                         " F24cNumDoc, F24cNumPed, F24cDirAso, F24cFecDoc, " + ;
                         " F27cNumAlb, F27cFecAlb, F27cCajPic, F27cBultos, F27cPesoKg, F27cVolume,F27lNumAlb,F27lLinDoc, F27lPesBru," + ;
                         " F24cValAlb, F24cDesPpp, F24cPortes," + ;
                         " F24cTotKgs, F24cCodPro, F24cTotVol, F24cNumBul, F24cFraTot, " + ;
                         " F24lCanDoc, F24lCanRes, F24lNumDrg, F24lFlag1, " + ;
                         " F24lPrecio, F24lIvaLin, F24lEqvLin, F24lDtoLin, " + ;
                         " LinObs.F24oLinObs F24oLinObs, LinObs.F24oDesObs F24oDesObs, " + ;
                         " CabObs.F24oDesObs As Cabecera, " + ;
                         " PeuObs.F24oDesObs As Pie, " + ;
                         " F01pDescri, F01pNumNif, F01pDirecc, F01pNumTel, F01pLogo, F01pPoblac, " + ;
                         " F01pCodPos, F01pProvin, F01pNumNif, F01tDescri, " + ;
                         " F24tNomAso, F24tNumNif, " + ;
                         " F24t1erDir, F24t2ndDir, F24t3rdDir, F24tCodPos, F24tDPobla, F24tDProvi, " + ;
                         " F22pNomAso, F22p1erDir, F22p2ndDir, F22p3rdDir, " + ;
                         " F22pCodPos, F22pDPobla, F22pDProvi, " + ;
                         " RPad(SubStr(F08cDescri, 1, 25), 60, ' ') As F08cDescri, " +;
                         "(F08cPesUni / 1000.000) As F08cPesUni,  " + ;
                         " F00gAbrevi, " + ;
                         " F26lCanFis, " + ;
                         " 0 As PNeto, 0 As Bultis, 0 As IDtoLin, 0 As IIvaLin, 0 As IIDto, " + ;
                         " 0 As CanLin, 0 As CanLot, 0 As CanDet, " + ;
                         " 0 As CanUni, 0 As CanSub, 0 As CanGrp, 0 As CanCaj, 0 As CanPal," + ;
                         " 0 As BulIni, 0 As BulFin, '*' As TipMac , 0 As UniMac, 0 As TotMac," + ;
                         " 0 As NoOfLine,F08cPesuni As PesTotal," + ;
                         " LPad(' ',60, ' ') As DetObs," + ;
                         " LPad(' ',60, ' ') As CabObs," + ;
                         " RPad(' ',35, ' ') As CopiaTxt," + ;
                         " RPad(' ', 3, '0') As CopiaNum," + ;
                         " 'N' As TipLin"

      f_from = " From F24c" + _em + ", " + ;
               "      F27c" + _em + ", " + ;  
               "      F24l" + _em + ", " + ;  
               "      F01p" + _em + ", " + ;
               "      F01t" + _em + ", " + ;
               "      F24o" + _em + " LinObs, " + ;
               "      F24o" + _em + " CabObs, " + ;
               "      F24o" + _em + " PeuObs, " + ;
               "      F24t" + _em + ", " + ;
               "      F22p" + _em + ", " + ;
               "      F26l" + _em + ", " + ;
               "      F27l" + _em + ", " + ;
               "      F08c" + _em + ", " + ;
               "      F00g" + _em

      f_where= " Where F24cCodPro=?_CodPro" + ;
               "   And F24cTipDoc=?_TipDoc" + ;
               "   And F24cNumDoc=?_NumDoc" + ;
               "   And F27cCodPro=F24cCodPro" + ;
               "   And F27cTipDoc=F24cTipDoc" + ;
               "   And F27cNumDoc=F24cNumDoc" + ;
               "   And F27cNumAlb=?_NumAlb" + ;
               "   And F01pCodigo=?_CodPro" + ;
               "   And F01tCodigo(+)=F27cCodTra" + ;
               "   And F24tCodPro(+)=F24cCodPro" + ;
               "   And F24tTipDoc(+)=F24cTipdoc" + ;
               "   And F24tNumDoc(+)=F24cNumDoc" + ;   
               "   And F22pCodPro(+)=F24cCodPro" + ;
               "   And F22pDirAso(+)=F24cDirAso" + ;
               "   And F27lNumAlb=F27cNumAlb" + ;
               "   And F27llinDoc=F26lLinDoc" + ;
               "   And F26lCodPro=F27lCodPro" + ;
               "   And F26lTipDoc=F27lTipDoc" + ;
               "   And F26lNumDoc=F27lNumDoc" + ;
               "   And F26lCodArt=F27lCodArt" + ;
               "   And F26lNumLot=F27lNumLot" + ;
               "   And F24lCodPro=F26lCodPro" + ;
               "   And F24lTipDoc=F26lTipDoc" + ;
               "   And F24lNumDoc=F26lNumDoc" + ;
               "   And F24lLinDoc=F26lLinDoc" + ; 
               "   And LinObs.F24oCodPro(+)=F26lCodPro" + ;
               "   And LinObs.F24oTipDoc(+)=F26lTipDoc" + ;
               "   And LinObs.F24oNumDoc(+)=F26lNumDoc" + ;
               "   And LinObs.F24oLinObs(+)=F26lLinDoc" + ;
               "   And CabObs.F24oCodPro(+)=F26lCodPro" + ;
               "   And CabObs.F24oTipDoc(+)=F26lTipDoc" + ;
               "   And CabObs.F24oNumDoc(+)=F26lNumDoc" + ;
               "   And CabObs.F24oLinObs(+)='0000'" + ;
               "   And PeuObs.F24oCodPro(+)=F26lCodPro" + ;
               "   And PeuObs.F24oTipDoc(+)=F26lTipDoc" + ;
               "   And PeuObs.F24oNumDoc(+)=F26lNumDoc" + ;
               "   And PeuObs.F24oLinObs(+)='9999'" + ;
               "   And F08cCodPro=F26lCodPro" + ;
               "   And F08cCodArt=F26lCodArt" + ;
               "   And F00gCodFam(+)=F08cTipFam"

      f_Orden = " Order By F26lCodPro, F26lTipDoc, F26lNumDoc, F26lLinDoc, F26lNumLot," + ;
                " F26lFecCad, F26lNumMov"

      lx_sql=lx_select + f_from + f_where + f_orden

      Wait Window 'Cargando datos para calcular albaranes.' NoWait

      Err=SqlExec(_ASql, lx_sql, 'NCursor')
      If Err <= 0
         _LxErr = 'Error al generar impresión albaranes' + cr + ;
                  'MENSAJE: ' + Message() + cr
         Do Form St3Inc
         _LxErr = ''
         Store .F. To _Bucle
         Loop
      EndIf

      =SqlMoreResults(_ASql)

      *> Si no hay datos a tratar, abandonar el proceso.
      Select NCursor
      Go Top
      If Eof()
         Store .F. To _Bucle
         Use In NCursor
         Return    &&&&& Loop
      EndIf

	  *> Cahpuzilla.------------------
	  Replace all Pestotal With 0
	  Go Top

      *> Buscar tipo bulto, y nº bulto inicial y final.---------------
      Wait Window 'Calculando datos bultos.' NoWait

      Do CalcDatosBultos

      *> Crear cursor _XL para grabar líneas acumuladas.--------------
      If Used('_XL')
         Use In _XL
      EndIf
      =CrtCursor('NCursor', '_XL', 'C')            

      *> Crear índice para cursor.------------------------------------
      Wait Window 'Indexando cursor de trabajo.' NoWait

      Select NCursor
      Delete Tag All
      f_Orden = "Str(F26lLinDoc) + F26lNumLot + DToS(F26lFecCad) + Str(BulIni)"
      Index On &f_Orden Tag LINDOC
      Set Order To 1

***   f_Index = AllTrim(Sys(2015))           && f_Index = FnTemp(1)
***   Index On &f_Orden To (f_Index)

      *> A partir de cursor NCursor, conseguir cantidad total por Lote y traspasar a 
      *> Cursor _XL eliminando mvtos. repetidos provocados por Join Cab y Pie de Línea
      Wait Window 'Agrupando datos por línea.' NoWait

      =AgruparPorLinea()

      Select _XL
      Go Top
      If Eof()
         _LxErr = 'No hay datos de agrupación de líneas' + cr
         Do Form St3Inc
         _LxErr = ''
         Use In _XL
         Store .F. To _Bucle
         Loop
      EndIf

      Replace All CanDet With F26lCanFis

      **************************************
      * Cantidad total de línea de documento 
      **************************************
      Select _XL
      Go Top
      Do While !Eof()
         Lin = F26lLinDoc
         reg = RecNo()
         Sum F26lCanFis To AuxCantLin While F26lLinDoc = Lin
         Go reg
         Replace CanLin With AuxCantLin While F26lLinDoc = Lin
      EndDo
         
      ****************************************
      *> Cantidad total de lote del documento.
      ****************************************
      Select _XL
      Go Top
      Do While !Eof()
         Lin = F26lLinDoc
         Lot = F26lNumLot
         Cad = F26lFecCad
         reg = RecNo()
         Sum F26lCanFis To AuxCantLin While F26lLinDoc = Lin .And. F26lNumLot = Lot ;
                     .And. F26lFecCad = Cad
         Go reg
         Replace CanLot With AuxCantLin While F26lLinDoc = Lin .And. F26lNumLot = Lot ;
                     .And. F26lFecCad = Cad
      EndDo

      *> Acumular las cantidades bonificadas en la línea no bonificada.
      *> Como estas líneas vienen a precio cero, actualizar de las no bonificadas.
      Wait Window 'Calculando líneas bonificadas.' NoWait

      Select _XL
      Go Top
      Do While !Eof()
         If F24lFlag1=='S'
            Replace F26lCanFis With Abs(F26lCanFis) * (-1)
            _RecNo = RecNo()
            _CodArt = F26lCodArt
            _CanFis = Abs(F26lCanFis)
            _Precio = F24lPrecio
            _DtoLin = F24lDtoLin

            *> Acumula cantidad en línea no bonificada.
            Locate For F26lCodPro = _CodPro .And. ;
                       F26lTipDoc = _TipDoc .And. ;
                       F26lNumDoc = _NumDoc .And. ;
                       F26lCodArt = _CodArt .And. ;
                       F24lFlag1 # 'S'
*            If Found()
*               Replace F26lCanFis With F26lCanFis + _CanFis
*               _Precio = F24lPrecio
*               _DtoLin = F24lDtoLin
*            EndIf

            *> Actualizar precio en línea bonificada, solo para el listado.
            Go _RecNo
*            Replace F24lPrecio With _Precio, ;
*                    F24lDtoLin With _DtoLin
         EndIf

         Select _XL
         Skip
      EndDo

      *> Calcular el descuento por línea. F24lDtoLin es el precio neto, NO el % descuento.
      Wait Window 'Calculando datos valoración.' NoWait

      Select _XL
      Replace All IDtoLin With F26lCanFis * (F24lPrecio - F24lDtoLin)

      *> Calcular el importe IVA por línea.
      Replace All IIvaLin With (F26lCanFis * F24lDtoLin) * (F24lIvaLin + F24lEqvLin) / 100 ;
         For TipLin == 'N' .Or. TipLin == 'S'

      *> Calcular el importe neto por línea.
      Replace All PNeto   With ((F26lCanFis * F24lDtoLin) + IIvaLin) / F26lCanFis

      *> Calcular el importe bruto total.
      Sum(F26lCanFis * F24lPrecio) To _IBruto ;
         For F26lCodPro = _CodPro .And. ;
             F26lTipDoc = _TipDoc .And. ;
             F26lNumDoc = _NumDoc .And. ;
             F27cNumAlb = _NumALb

      *> Calcular importe neto total.
      Sum(F26lCanFis * F24lDtoLin) To _INeto ;
         For F26lCodPro = _CodPro .And. ;
             F26lTipDoc = _TipDoc .And. ;
             F26lNumDoc = _NumDoc .And. ;
             F27cNumAlb = _NumALb

      *> Calcular % descuento por línea.
      Replace All IIDto With 100 - Round((_INeto * 100 / _IBruto), 0) ;
         For F26lCodPro = _CodPro .And. ;
             F26lTipDoc = _TipDoc .And. ;
             F26lNumDoc = _NumDoc .And. ;
             F27cNumAlb = _NumALb

      *> Calcular % descuento promedio.
      _IIDto = Round((_IBruto - _INeto) * 100.0 / _INeto, 1)

      *> Descripción fija si es bonificación.
      Replace All F08cDescri With 'BONIF. EN GENERO', ;
                  TipLin     With 'S' ;
         For F24lFlag1=='S'

      *> Clasificar el cursor por artículo.
      Select _XL
      _lx1 = _Tmp + AllTrim(Sys(2015)) + '.Dbf'
      Sort To &_lx1 On F26lCodPro, F26lTipDoc, F26lNumDoc, F26lCodArt, F26lNumLot
      Zap      
      Append From &_lx1
      Delete File(_lx1)
      Go Top

      Store .F. To _Bucle
   EndDo

   *> Si hay observaciones, generar líneas adicionales.
   Wait Window 'Generando observaciones.' NoWait

   Select _XL
   Go Top
   Scatter MemVar Memo

   For _Inx = 1 To 6
      _DesObs = MLine(Cabecera, _Inx)
      If !Empty(_DesObs) .And. !IsNull(_DesObs)
         Append Blank
         Gather MemVar Memo
         Replace F26lCodArt With Replicate('Z', 8), ;
                 DetObs     With _DesObs, ;
                 F26lNumLot With Replicate('9', 8) + '1' + Str(_Inx, 1, 0), ;
                 TipLin     With 'O'
      EndIf

      Go Top
   EndFor

   *> Eliminar las observaciones de los registros de datos.
   Select _XL
   Replace DetObs     With Space(6) For TipLin # 'O'
   Replace F08cDescri With DetObs   For TipLin = 'O'

	Select _XL
	Go Top
   _CodPro = F26lCodPro
   _TipDoc = F26lTipDoc
   _NumDoc = F26lNumDoc

   _Sentencia = " Select * From F24o001 " + ;
   				" Where F24oCodPro='" + _CodPro + "' And " + ;
	            "F24oTipDoc='" + _TipDoc + "' And " + ;
    	        "F24oNumDoc='" + _NumDoc + "' And " + ;
        	    "F24oLinObs='9999'"

	_Ok = SqlExec(_Asql,_Sentencia,'F24oCur')
	=SqlMoreResults(_Asql)

*   _Campos = '*'
*   _Selec = "F24oCodPro='" + F26lCodPro + "' And " + ;
*            "F24oTipDoc='" + F26lTipDoc + "' And " + ;
*            "F24oNumDoc='" + F26lNumDoc + "' And " + ;
*            "F24oLinObs='9999'"

*   =!f3_sql(_Campos, ;
*            'F24O', ;
*            _Selec, ;
*            '', ;
*            '', ;
*            'F24oCur')

   Select _XL
   Replace All DetObs With Iif(SubStr(F24oCur.F24oImpObs, 1, 1)=='S', ;
                           F24oCur.F24oDesObs, Space(60))


   _Sentencia = " Select * From F24o001 " + ;
   				" Where F24oCodPro='" + _CodPro + "' And " + ;
	            "F24oTipDoc='" + _TipDoc + "' And " + ;
    	        "F24oNumDoc='" + _NumDoc + "' And " + ;
        	    "F24oLinObs='0000'"
	_Ok = SqlExec(_Asql,_Sentencia,'F24oCur')
	=SqlMoreResults(_Asql)

*   _Campos = '*'
*   _Selec = "F24oCodPro='" + F26lCodPro + "' And " + ;
*            "F24oTipDoc='" + F26lTipDoc + "' And " + ;
*            "F24oNumDoc='" + F26lNumDoc + "' And " + ;
*            "F24oLinObs='0000'"

*   =!f3_sql(_Campos, ;
*            'F24O', ;
*            _Selec, ;
*            '', ;
*            '', ;
*            'F24oCur')

   Select _XL
   Replace All CabObs With Iif(SubStr(F24oCur.F24oImpObs, 1, 1)=='S', ;
                           F24oCur.F24oDesObs, Space(60))

   *> Generar registros adicionales para registros con nº de vale estupefacientes.
   _Inx = 0
   Select _XL
   Scan For !Empty(F24lNumDrg) .And. TipLin=='N'
      _Inx = _Inx + 1
      Scatter MemVar Memo
      Replace F24lNumDrg With Space(15)

      Append Blank
      Gather MemVar Memo
      Replace F08cDescri With 'Nº Vale estupefacientes : ' + m.F24lNumDrg, ;
              F24lNumDrg With m.F24lNumDrg, ;
              TipLin     With 'S'

      *> Necesario, pues el Append Blank nos posiciona al final del fichero.
      Go Top
   EndScan

   *> Texto de cada copia de albarán.
   Replace All CopiaTxt With _TxtCop, ;
               CopiaNum With PadL(AllTrim(Str(_CurCop, 3, 0)), 3, '0')

   ******************************************************************
   *> Calcular número de Macs por línea y traspaso a cursor listado.
   ******************************************************************
   Wait Window 'Traspasando datos a cursor de impresión.' NoWait

   If !Used('XLSaliAlbC')
      =CrtCursor('_XL', 'XLSaliAlbC', 'C')
   EndIf 

   Select _XL
   Go Top
   Do While !Eof()
      Replace TotMac With BulFin - BulIni + 1
      Skip
   EndDo

   Select XLSaliAlbC
   Append From Dbf('_XL')

*----------
*> Peso total
if Modificado=.F.
	Temporal = 0
    Select XLSalialbC
    Go Top
    Do While !Eof() 
    	if F26lNumDoc=Documento
			If TipLin=='N'
    	   		Temporal = Temporal + F27lPesBru
			EndIf
       	EndIf
		Select XLSalialbC
     	Skip
    EndDo
*   Modificado=.T.
EndIf

Select XLSalialbC
Go top
Do while !Eof()
	if Empty(PesTotal)
		Replace PesTotal with Temporal
	EndIf
	Select XLSalialbC
	skip
EndDo
*	Temporal=0

*---------

	*> Ponemos el Numero de Bultos.--------------------------------------
	Select XLSalialbC
	Go Top
	_NumAlb = XlSalialbc.F27cNumAlb
	_Sentencia = " Select F30cBultos From F30c001 Where F30cAlbRep Like '" + _NumAlb + "%'"
	_Ok = SqlExec(_Asql,_Sentencia,'BulAlb')
	=SqlMoreResults(_Asql)
	If _ok < 1

	EndIf

	Select XLSalialbC
	Replace All Bultis With BulAlb.F30cBultos

   *> Eliminar cursores temporales de trabajo.
   Use In _XL

   Select NCursor
   Delete Tag All
   Use In NCursor

   If Type('f_Index')=='C'
      If File(f_Index)
         Delete File &f_Index
      EndIf
   EndIf

   Wait Clear
Return

************************************************************************
*> Procedimiento de la factura proforma de UN documento de salida.     *
*> Primer paso: Generar datos del albarán actual.                      *
*>                                                                     *
*> Recibe: _CodPro ---> Código de propietario.                         *
*>         _TipDoc ---> Tipo de documento.                             *
*>         _NumDoc ---> Nº de Documento.                               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaliProf ----> Impresión de albaranes. (Por SaliProf1)          *
************************************************************************
Procedure GenFraPro
   Parameters _CodPro, _TipDoc, _NumDoc

   Private lx_sql, lx_select, f_from, f_where, f_orden
   Private Err
   Private _Bucle

   Private Lin          && Línea del documento
   Private reg          && Registro al que volver  
   Private MpDoc        && Macs por Documento
   Private AuxCantLin   && cantidad de Línea de Documento  
   Private AuxCantDoc   && Cantidad de Documento

   Set NullDisplay To ''

   Store .T. To _Bucle
   Do While _Bucle == .T.
      *> Formar strings de conexión ORACLE.
      lx_select= " Select F24c" + _em + ".*, " + ;
                         "F24l" + _em + ".*, " + ;
                         "LinObs.F24oLinObs F24oLinObs, LinObs.F24oDesObs F24oDesObs, " + ;
                         "CabObs.F24oDesObs Cabecera, " + ;
                         "PeuObs.F24oDesObs Pie, " + ; 
                         "F01p" + _em + ".*, " + ;
                         "F01tDescri, " + ;
                         "F24t" + _em + ".*, " + ;
                         "F22p" + _em + ".*, " + ;
                         "F08c" + _em + ".*, (F08cPesUni / 1000.000) F08cPesUni,  " + ;
                         "F00gAbrevi, " + ;
                         "RPad(' ', 10, ' ') As Partida, 0 As Importe, '1' As Nivel"

      f_from = " From F24c" + _em + ", " + ;
                     "F24l" + _em + ", " + ;  
                     "F24o" + _em + " LinObs, " + ;
                     "F24o" + _em + " CabObs, " + ;
                     "F24o" + _em + " PeuObs, " + ;                         
                     "F01p" + _em + ", " + ;
                     "F01t" + _em + ", " + ;
                     "F24t" + _em + ", " + ;
                     "F22p" + _em + ", " + ;
                     "F08c" + _em + ", " + ;
                     "F00g" + _em

      f_where= " Where F24cCodPro='" + _CodPro + "'" + ;
                 " And F24cTipDoc='" + _TipDoc + "'" + ;
                 " And F24cNumDoc='" + _NumDoc + "'" + ;
                 " And F01pCodigo='" + _CodPro + "'" + ;
                 " And F01tCodigo(+)=F24cCodTra" + ;
                 " And F24tCodPro(+)=F24cCodPro" + ;
                 " And F24tTipDoc(+)=F24cTipdoc" + ;
                 " And F24tNumDoc(+)=F24cNumDoc" + ;
                 " And F22pCodPro(+)=F24cCodPro" + ;
                 " And F22pDirAso(+)=F24cDirAso" + ;
                 " And F24lCodPro=F24cCodPro" + ;
                 " And F24lTipDoc=F24cTipDoc" + ;
                 " And F24lNumDoc=F24cNumDoc" + ;
                 " And LinObs.F24oCodPro(+)=F24lCodPro" + ;
                 " And LinObs.F24oTipDoc(+)=F24lTipDoc" + ;
                 " And LinObs.F24oNumDoc(+)=F24lNumDoc" + ;
                 " And LinObs.F24oLinObs(+)=F24lLinDoc" + ;
                 " And CabObs.F24oCodPro(+)=F24cCodPro" + ;
                 " And CabObs.F24oTipDoc(+)=F24cTipDoc" + ;
                 " And CabObs.F24oNumDoc(+)=F24cNumDoc" + ;
                 " And CabObs.F24oLinObs(+)='0000'" + ;
                 " And PeuObs.F24oCodPro(+)=F24cCodPro" + ;
                 " And PeuObs.F24oTipDoc(+)=F24cTipDoc" + ;
                 " And PeuObs.F24oNumDoc(+)=F24cNumDoc" + ;
                 " And PeuObs.F24oLinObs(+)='9999'" + ;
                 " And F08cCodPro=F24lCodPro" + ;
                 " And F08cCodArt=F24lCodArt" + ;
                 " And F00gCodFam(+)=F08cTipFam"

      f_Orden = " Order By F24cCodPro, F24cTipDoc, F24cNumDoc"

      lx_sql=lx_select + f_from + f_where + f_orden
      Err=SqlExec(_aSql, lx_sql, 'NCursor')
      If Err <= 0
         _LxErr = 'Error al generar impresión proformas' + cr + ;
                  'MENSAJE: ' + Message() + cr
         Do Form St3Inc
         _LxErr = ''
         Store .F. To _Bucle
         Loop
      EndIf

      =SqlMoreResults(_aSql)

      *> Si no hay datos a tratar, abandonar el proceso.
      Select NCursor
      Go Top
      If Eof()
         Store .F. To _Bucle
         Use In NCursor
         Loop
      EndIf

      *> Pasar datos a cursor de impresión.
      If !Used('XLSaliProf')
         =CrtCursor('NCursor', 'XLSaliProf', 'C')            
      EndIf 

      *> Acumulados por partida estadística.
      Create Cursor PProfCur ;
         (Partida C(10), ;
          Importe N(10,0))

      Select NCursor
      Replace All F00gAbrevi With Space(10) For IsNull(F00gAbrevi)

      Go Top
      Do While !Eof()
         Scatter MemVar
         Select XLSaliProf
         Append Blank
         Gather MemVar

         *> Acumular en partida estadística.
         Select PProfCur
         Locate For Partida = NCursor.F00gAbrevi
         If !Found()
            Append Blank
            Replace Partida With NCursor.F00gAbrevi, ;
                    Importe With 0
         EndIf
         Replace Importe With Importe + (NCursor.F24lCanRes * NCursor.F24lPrecio)

         *>
         Select NCursor
         Skip
      EndDo

      *> Pasar los acumulados partidas al cursor de impresión.
      Select PProfCur
      Go Top
      If !Eof()
         *> Crear línea de título.
         Select XLSaliProf
         Append Blank
         Replace F24cCodPro With _CodPro, ;
                 F24cTipDoc With _TipDoc, ;
                 F24cNumDoc With _NumDoc, ;
                      Nivel With '2'
      EndIf

      Select PProfCur
      Go Top
      Do While !Eof()
         Scatter MemVar
         Select XLSaliProf
         Append Blank
         Replace F24cCodPro With _CodPro, ;
                 F24cTipDoc With _TipDoc, ;
                 F24cNumDoc With _NumDoc, ;
        	        Partida With m.Partida, ;
                    Importe With m.Importe, ;
                      Nivel With '3'

         Select PProfCur
         Skip      
      EndDo

      Use In NCursor
      Use In PProfCur

      Store .F. To _Bucle
   EndDo

Return

************************************************************************
*> Procedimiento de impresión del albarán de UN documento de salida.   *
*> Segundo paso: Imprimir los datos generados por GenAlbCli.           *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaliAlbC ----> Impresión de albaranes. (Por SaliAlbC3)          *
*>   - ProcVeri ----> Verificación TUNDEN.                             *
*> Imprimir albarán valorado o normal. AVC - 30.03.2000                *
************************************************************************
Procedure PrtAlbCli

   If Used('XLSaliAlbC')
      Select XLSalialbC

      *> Albarán sin valorar.
      Locate For F24cValAlb=='N'
      If Found()
         *> Pasar los registros a procesar a un cursor.
         If Used('_XL')
            Select _XL
            Delete Tag All
            Use In _XL
         EndIf

         Wait Window 'Crear cursor temporal de impresión. (N)' NoWait

         =CrtCursor('XLSaliAlbC', '_XL', 'C')            
         Select _XL
         Append From Dbf('XLSaliAlbC') For F24cValAlb=='N'

         *> Ordenar el cursor a imprimir.
         Wait Window 'Indexando cursor temporal de impresión. (N)' NoWait

         f_Orden = "F27cNumAlb + F26lCodPro + F26lTipDoc + F26lNumDoc + CopiaNum + F26lCodArt + F26lNumLot + Str(F26lLinDoc, 4, 0) + TipLin"
         Select _XL
         Delete Tag All
         Index On &f_Orden Tag NUMALB
         Set Order To 1

***      f_Index = AllTrim(Sys(2015))               && f_Index = FnTemp(1)
***      Index On &f_Orden To (f_Index)

         *> Acumular el nº de registros de cada albarán, para rellenar el cursor del report,
         *> de manera que la última página tenga 32 líneas. (¡¡ Vaya chapuza !!).
         Select F27cNumAlb, CopiaNum, Count(*) As F27cNumReg ;
            From _XL ;
            Group By F27cNumAlb, CopiaNum ;
            Order By F27cNumAlb, CopiaNum ;
            Into Cursor _XLGroup

         Select _XLGroup
         Go Top
         Do While !Eof()
            _Inx = F27cNumReg - (Floor(F27cNumreg / 32) * 32)
            _Inx = Mod(_Inx, 32)
*****       _Inx = Iif(_Inx = 0, 32, _Inx)

            Select _XL
            Locate For F27cNumAlb==_XLGroup.F27cNumAlb .And. ;
                       CopiaNum  ==_XLGroup.CopiaNum
            Scatter MemVar Memo

            For _Inx = _Inx + 1 To 32
               Append Blank
               Gather MemVar Memo
               Replace F26lCodArt With Replicate('Z', 8), ;
                       F08cDescri With Space(30), ;
                       F26lNumLot With Replicate('9', 8) + '2' + Str(_Inx, 2, 0), ;
                       TipLin     With 'O'
            EndFor

            *> Grabar contador de nº registro en cursor para control de cambio de página
            *> en report. No podemos utilizar RecNo(), porque es un valor ya asignado y
            *> no depende del index.
            _InxOk = 0
            Locate For F27cNumAlb==_XLGroup.F27cNumAlb .And. ;
                       CopiaNum  ==_XLGroup.CopiaNum
            Do While Found()
               _InxOk = _InxOk + 1
               Replace NoOfLine With _InxOk               && Antes RecNo().
               Continue
            EndDo

            *>
            Select _XLGroup
            Skip
         EndDo

         Use In _XLGroup

         *> Líneas ficticias.
         Wait Window 'Añadir líneas complementarias. (N)' NoWait

         Select _XL
         Replace All F26lCanFis With 0 For TipLin # 'N' .And. TipLin # 'S'
         Replace All F24lPrecio With 0 For TipLin # 'N'
         Replace All F24lDtoLin With 0 For TipLin # 'N'
         Replace All F08cDescri With LTrim(F08cDescri)

         *> Limpiar valores de las líneas ficticias.
         Replace All IIvaLin With 0, ;
                     IDtoLin With 0, ;
                     IIDto   With 0, ;
                     PNeto   With 0 For TipLin # 'N' .And. TipLin # 'S'
         
         *> Calcular el importe IVA por línea.
         Replace All IIvaLin With (F26lCanFis * F24lDtoLin) * (F24lIvaLin + F24lEqvLin) / 100

         *> Calcular el importe neto por línea.
         Replace All PNeto   With ((F26lCanFis * F24lDtoLin) + IIvaLin) / F26lCanFis

         Wait Window 'Proceso de impresión. (N)' NoWait

         Go Top
         _Label1 = "IMPRESION DE ALBARAN DE CLIENTE (NO VALORADO)"
         Do Form St3Imp2 With _Label1, 'SaliAlbC11', '_XL', , "F24cValAlb=='N'"

         Select _XL
         Delete Tag All
         Use In _XL
      EndIf

      Wait Clear

      *> Albarán valorado a precio venta laboratorio.
      Select XLSaliAlbC
      Locate For F24cValAlb=='S'
      If Found()
         *> Pasar los registros a procesar a un cursor.
         If Used('_XL')
            Select _XL
            Delete Tag All
            Use In _XL
         EndIf

         Wait Window 'Crear cursor temporal de impresión. (S)' NoWait

         =CrtCursor('XLSaliAlbC', '_XL', 'C')            
         Select _XL
         Append From Dbf('XLSaliAlbC') For F24cValAlb=='S'

         *> Ordenar el cursor a imprimir.
         Wait Window 'Indexando cursor temporal de impresión. (S)' NoWait

         f_Orden = "F27cNumAlb + F26lCodPro + F26lTipDoc + F26lNumDoc + CopiaNum + F26lCodArt + F26lNumLot + Str(F26lLinDoc, 4, 0) + TipLin"
         Select _XL
         Delete Tag All
         Index On &f_Orden Tag NUMALB
         Set Order To 1

***      f_Index = AllTrim(Sys(2015))                && f_Index = FnTemp(1)
***      Index On &f_Orden To (f_Index)

         *> Acumular el nº de registros de cada albarán, para rellenar el cursor del report,
         *> de manera que la última página tenga 19 líneas. (¡¡ Vaya chapuza !!).
         Select F27cNumAlb, CopiaNum, Count(*) As F27cNumReg ;
            From _XL ;
            Group By F27cNumAlb, CopiaNum ;
            Order By F27cNumAlb, CopiaNum ;
            Into Cursor _XLGroup

         Select _XLGroup
         Go Top
         Do While !Eof()
            _Inx = F27cNumReg - (Floor(F27cNumreg / 32) * 32)
            _Inx = Mod(_Inx, 19)
            _Inx = Iif(_Inx = 0, 19, _Inx)

            Select _XL
            Locate For F27cNumAlb==_XLGroup.F27cNumAlb .And. ;
                       CopiaNum  ==_XLGroup.CopiaNum
            Scatter MemVar Memo

            For _Inx = _Inx + 1 To 19   &&   32
               Append Blank
               Gather MemVar Memo
               Replace F26lCodArt With Replicate('Z', 8), ;
                       F08cDescri With Space(30), ;
                       F26lNumLot With Replicate('9', 8) + '2' + Str(_Inx, 2, 0), ;
                       TipLin     With 'O'
            EndFor

            *> Grabar contador de nº registro en cursor para control de cambio de página
            *> en report. No podemos utilizar RecNo(), porque es un valor ya asignado y
            *> no depende del index.
            _InxOk = 0
            Locate For F27cNumAlb==_XLGroup.F27cNumAlb .And. ;
                       CopiaNum  ==_XLGroup.CopiaNum
            Do While Found()
               _InxOk = _InxOk + 1
               Replace NoOfLine With _InxOk               && Antes RecNo().
               Continue
            EndDo

            *>
            Select _XLGroup
            Skip
         EndDo

         Use In _XLGroup

         *> Líneas ficticias.
         Wait Window 'Añadir líneas complementarias. (S)' NoWait

         Select _XL
         Replace All F26lCanFis With 0 For TipLin # 'N' .And. TipLin # 'S'
         Replace All F24lPrecio With 0 For TipLin # 'N'
         Replace All F24lDtoLin With 0 For TipLin # 'N'
         Replace All F08cDescri With LTrim(F08cDescri)

         *> Limpiar valores de las líneas ficticias.
         Replace All IIvaLin With 0, ;
                     IDtoLin With 0, ;
                     IIDto   With 0, ;
                     PNeto   With 0 For TipLin # 'N' .And. TipLin # 'S'

         *> Calcular el importe IVA por línea.
         Replace All IIvaLin With (F26lCanFis * F24lDtoLin) * (F24lIvaLin + F24lEqvLin) / 100

         *> Calcular el importe neto por línea.
         Replace All PNeto   With ((F26lCanFis * F24lDtoLin) + IIvaLin) / F26lCanFis

         Wait Window 'Proceso de impresión. (S)' NoWait

         Go Top

         _Label1 = "IMPRESION DE ALBARAN DE CLIENTE (VALORADO PVL)"
         Do Form St3Imp2 With _Label1, 'SaliAlbC12', '_XL', , "F24cValAlb=='S'"

         Select _XL
         Delete Tag All
         Use In _XL
      EndIf

      Wait Clear

      *> Albarán valorado a precio venta neto.
      Select XLSaliAlbC
      Locate For F24cValAlb=='T'
      If Found()
         *> Pasar los registros a procesar a un cursor.
         If Used('_XL')
            Select _XL
            Delete Tag All
            Use In _XL
         EndIf

         Wait Window 'Crear cursor temporal de impresión. (T)' NoWait

         =CrtCursor('XLSaliAlbC', '_XL', 'C')            
         Select _XL
         Append From Dbf('XLSaliAlbC') For F24cValAlb=='T'

         *> Ordenar el cursor a imprimir.
         Wait Window 'Indexando cursor temporal de impresión. (T)' NoWait

         f_Orden = "F27cNumAlb + F26lCodPro + F26lTipDoc + F26lNumDoc + CopiaNum + F26lCodArt + F26lNumLot + Str(F26lLinDoc, 4, 0) + TipLin"
         Select _XL
         Delete Tag All
         Index On &f_Orden Tag NUMALB
         Set Order To 1

***      f_Index = AllTrim(Sys(2015))              && f_Index = FnTemp(1)
***      Index On &f_Orden To (f_Index)

         *> Acumular el nº de registros de cada albarán, para rellenar el cursor del report,
         *> de manera que la última página tenga 22 líneas. (¡¡ Vaya chapuza !!).
         Select F27cNumAlb, CopiaNum, Count(*) As F27cNumReg ;
            From _XL ;
            Group By F27cNumAlb, CopiaNum ;
            Order By F27cNumAlb, CopiaNum ;
            Into Cursor _XLGroup

         Select _XLGroup
         Go Top
         Do While !Eof()
            _Inx = F27cNumReg - (Floor(F27cNumreg / 32) * 32)
            _Inx = Mod(_Inx, 22)
            _Inx = Iif(_Inx = 0, 22, _Inx)

            Select _XL
            Locate For F27cNumAlb==_XLGroup.F27cNumAlb .And. ;
                       CopiaNum  ==_XLGroup.CopiaNum
            Scatter MemVar Memo

            For _Inx = _Inx + 1 To 22   &&     32
               Append Blank
               Gather MemVar Memo
               Replace F26lCodArt With Replicate('Z', 8), ;
                       F08cDescri With Space(30), ;
                       F26lNumLot With Replicate('9', 8) + '2' + Str(_Inx, 2, 0), ;
                       TipLin     With 'O'
            EndFor

            *> Grabar contador de nº registro en cursor para control de cambio de página
            *> en report. No podemos utilizar RecNo(), porque es un valor ya asignado y
            *> no depende del index.
            _InxOk = 0
            Locate For F27cNumAlb==_XLGroup.F27cNumAlb .And. ;
                       CopiaNum  ==_XLGroup.CopiaNum
            Do While Found()
               _InxOk = _InxOk + 1
               Replace NoOfLine With _InxOk               && Antes RecNo().
               Continue
            EndDo

            *>
            Select _XLGroup
            Skip
         EndDo

         Use In _XLGroup

         *> Líneas ficticias.
         Wait Window 'Añadir líneas complementarias. (T)' NoWait

         Select _XL
         Replace All F26lCanFis With 0 For TipLin # 'N' .And. TipLin # 'S'
         Replace All F24lPrecio With 0 For TipLin # 'N'
         Replace All F24lDtoLin With 0 For TipLin # 'N'
         Replace All F08cDescri With LTrim(F08cDescri)

         *> Limpiar valores de las líneas ficticias.
         Replace All IIvaLin With 0, ;
                     IDtoLin With 0, ;
                     IIDto   With 0, ;
                     PNeto   With 0 For TipLin # 'N' .And. TipLin # 'S'

         *> Calcular el importe IVA por línea.
         Replace All IIvaLin With (F26lCanFis * F24lDtoLin) * (F24lIvaLin + F24lEqvLin) / 100 ;
            For TipLin=='N' .Or. TipLin=='S'

         *> Calcular el importe neto por línea.
         Replace All PNeto   With ((F26lCanFis * F24lDtoLin) + IIvaLin) / F26lCanFis ;
            For TipLin=='N' .Or. TipLin=='S'

         Wait Window 'Proceso de impresión. (T)' NoWait

         Go Top
         _Label1 = "IMPRESION DE ALBARAN DE CLIENTE (VALORADO P. NETO)"
         Do Form St3Imp2 With _Label1, 'SaliAlbC13', '_XL', , "F24cValAlb=='T'"

         Select _XL
         Delete Tag All
         Use In _XL
      EndIf

      Wait Clear
   Else
      _LxErr = "No hay albaranes de cliente para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLSALIALBC')
      Use In XLSALIALBC
   EndIf
   
   If Used('_XL')
      Select _XL
      Delete Tag All
      Use In _XL
   EndIf
   
   If Used('NCursor')
      Use In NCursor
   EndIf

   If Type('f_Index')=='C'
      If File(f_Index)
         Delete File &f_Index
      EndIf
   EndIf

*>
Wait Clear
Return

************************************************************************
*> Procedimiento de impresión de proforma de UN documento de salida.   *
*> Segundo paso: Imprimir los datos generados por GenFraPro.           *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaliProf ----> Impresión de proformas. (Por SaliProf1)          *
************************************************************************
Procedure PrtFraPro

   _Label1 = "IMPRESION DE FACTURAS PROFORMA"
   If Used('XLSaliProf')
      Select XLSaliProf
      Go Top
      Do Form St3Imp2 With _Label1, 'SaliProfL1', 'XLSaliProf'
   Else
      _LxErr = "No hay Proformas para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursores generado por SQL.
   If Used('XLSALIPROF')
      Use in XLSALIPROF
   EndIf
   
   If Used('_XL')
      Use in _XL
   EndIf
   
   If Used('_XL')
      Use in _XL
   EndIf

   If Used('PProfCur')
      Use in PProfCur
   EndIf

Return

************************************************************************************
* Eliminar duplicados en líneas de observaciones de documentos.                    *
************************************************************************************
Function AgruparPorLinea

   Private SumCFis, SumCUni, SumCSub, SumCGrp, SumCCaj, SumCPal
   Private Lin, Lote, FecCad, mov

   Store 0 To SumCFis, SumCUni, SumCSub, SumCGrp, SumCCaj, SumCPal

   *> Borrar movimientos duplicados.
   w_NumMov = Space(10)

   Select NCursor
   Go Top
   Do While !Eof()
      If F26lNumMov = w_NumMov
         Delete Next 1
      EndIf

      w_NumMov = F26lNumMov

      Select NCursor
      Skip
   EndDo

   Select NCursor
   Go Top
   new_BulIni = BulIni
   new_BulFin = BulFin
   Do While !Eof()
*      If F26lNumMov = w_NumMov           && Si mismo movimiento, saltar.
*         Skip
*         Loop
*      EndIf
  
      SumCFis = SumCFis + F26lCanFis
      SumCUni = SumCUni + CanUni
      SumCSub = SumCSub + CanSub
      SumCGrp = SumCGrp + CanGrp
      SumCCaj = SumCCaj + CanCaj
      SumCPal = SumCPal + CanPal

      *> Leer siguiente movimiento.-----------------------------------
      Select NCursor
      w_LinDoc = F26lLinDoc
      w_NumLot = F26lNumLot
      w_FecCad = F26lFecCad
      w_TipMac = TipMac
      w_BulIni = BulIni
      w_BulFin = BulFin

      *> Guardar copia del registro actual.---------------------------
      Scatter MemVar Memo
      Skip

      Do Case
         Case w_LinDoc <> F26lLinDoc .Or. w_NumLot <> F26lNumLot

            *> En nuevo cursor se generará una línea por Lote con su cantidad
            Select _XL
            Append Blank
            Gather MemVar Memo
            Replace F26lCanFis With SumCFis, ;
                    CanUni     With SumCUni, ;
                    CanSub     With SumCSub, ;
                    CanGrp     With SumCGrp, ;
                    CanCaj     With SumCCaj, ;
                    CanPal     With SumCPal, ;
                    BulIni     With new_BulIni, ;
                    BulFin     With new_BulFin

            Select NCursor
            Store 0 To SumCFis, SumCUni, SumCSub, SumCGrp, SumCCaj, SumCPal
            new_BulIni = BulIni
            new_BulFin = BulFin

         Case new_BulFin + 1 = BulIni     && Si el bulto es correlativo, enlazar bultos.
            new_BulFin = BulFin
      EndCase
   EndDo
Return .T.         

*>==============================================================
*> CalcDatosBultos ............... Calcular datos de bultos
*>==============================================================
Procedure CalcDatosBultos
Private f_Selec, f_Where, f_Order

   Select NCursor
   Go Top

   Do While !Eof()
      f_Selec = "F26wNumBul, F26vTipOri"
      f_Where = "F26wNMovMP=?NCursor.F26lNumMov And F26vNumMac=F26wNumMac"
      f_Order = "F26wNumBul"

      =f3_SqlTun(f_Selec, 'F26w,F26v', f_Where, f_Order, , , 'AlbCF26w')

      Select AlbCF26w
      Go Top
      If !Eof()
         Replace NCursor.BulIni With Val(F26wNumBul), ;
                 NCursor.TipMac With F26vTipOri
         Do Case 
            *> Unidades en palets completos.
            Case F26vTipOri=="N"   && Antes 'P'
               Select NCursor
               Replace NCursor.UniMac With NCursor.F26lUniPac * NCursor.F26lPacCaj * NCursor.F26lCajPal, ;
                       NCursor.CanPal With 1

            *> Unidades en cajas completas.
            Case F26vTipOri=="S"   && Antes 'C'
               Select NCursor
               Replace NCursor.UniMac With NCursor.F26lUniPac * NCursor.F26lPacCaj, ;
                       NCursor.CanCaj With 1

            *> Unidades en grupos.
            Case F26vTipOri = "G"
               Select NCursor
               Replace NCursor.UniMac With NCursor.F26lUniPac * NCursor.F26lPacCaj

               *> Ver si acumula en grupos o en subgrupos:
               *> F08gLvlAgr = 1? = Subgrupo.
               *> F08gLvlAgr > 1? = Grupo.
               _Selec = "Select * From F08g" + _em + ;
                        " Where F08gCodPro=?NCursor.F26lCodPro" + ;
                        " And   F08gCodArt=?NCursor.F26lCodArt" + ;
                        " Order By F08gCodPro, F08gCodArt, F08gLvlAgr"

               _xier = SqlExec(_ASql, _Selec, 'F08gCur')
               If _xier <= 0
                  _LxErr = 'Error leyendo nivel de agrupación' + cr + ;
                           'MENSAJE: ' + Message() + cr
                  Do Form St3Inc
                  _LxErr = ''
                  Return
               EndIf

               =SqlMoreResults(_ASql)

               Select F08gCur
               Go Top
               Locate For F08gCodUbi = NCursor.F26lUbiOri
               Do Case
                  Case !Found()
                     Select NCursor
                     Replace NCursor.CanGrp With 1
                  Case F08gLvlAgr <= 10
                     Select NCursor
                     Replace NCursor.CanGrp With 1
                  Case F08gLvlAgr >= 20
                     Select NCursor
                     Replace NCursor.CanSub With 1
               EndCase

               Use In F08gCur

            *> Unidades en fracciones.
            Case F26vTipOri = "U"
               Select NCursor
               Replace NCursor.UniMac With NCursor.F26lCanFis, ;
                       NCursor.CanUni With NCursor.F26lCanFis
         EndCase
      EndIf

      Select AlbCF26w
      Do While !Eof()
         Replace NCursor.BulFin With Val(F26wNumBul)
         Skip
      EndDo

      *> Eliminar cursor de trabajo.
      If Used('AlbCF26w')
         Use In AlbCF26w
      EndIf
   
      Select NCursor
      Skip
   EndDo

Return
