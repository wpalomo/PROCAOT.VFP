Define Class oraprmresv as custom



PROCEDURE inicializar

*> Inicializaci�n del objeto de reserva.

With This
	*> Inicializar datos del documento.
	.DscCPro = Space(6)
	.DscTDoc = Space(4)
	.DscNDoc = Space(13)

	.DsdCPro = Space(6)
	.DsdTDoc = Space(4)
	.DsdNDoc = Space(13)
	.DsdLDoc = Space(4)
	.DsdCart = Space(13)
	.DsdTPro = Space(4)

	*> Inicializar par�metros ubicaci�n.
	.UbiCUbi = Space(14)

	*> Inicializar par�metros reserva.
	.PrgCnfAut = Space(1)
	.PrgDejPic = Space(1)
	.PrgImpAut = Space(1)
	.PrgOrdExt = Space(1)
	.PrgSerPic = Space(1)
	.PrgTipMov = Space(1)
	.PrgTipRes = Space(1)
	.PrgRsvCD  = Space(1)

	*> Inicializar otros datos.
	.PwAsql = _ASql
	.PwUser = _UsrCod
	.PwPrgm = Space(20)
	.PwEmpr = _em
	.PwAlma = _Alma
	.PwCrLf = Chr(13) + Chr(10)

	*> Datos volumetr�a.
	.PtStVo   = Space(2)
	.VolNBu   = Space(9)
	.VolCtrF  = 'N'
	.VolCtrG  = 'N'
	.VolFriA  = Space(4)
	.VolDrgA  = Space(4)
EndWith

Return

ENDPROC


EndDefine 
Define Class orafncresv as custom
Height = 15
Width = 100



PROCEDURE reserva

*> Reserva de material de un documento.
*> Recibe: Pedido: Pedido a reservar.
*> Opcional: L�nea de pedido a reservar

Parameters cCodPro, cTipDoc, cNumDoc, cLinDoc

Private cCodPro, cTipDoc, cNumDoc, cTexto
Private _Field, _Sentencia, _Where, _Order, oF24cl
Local lStado, _Ok, oArti, oF24c
Local _FPac, _FCaj, _FPal, _QPal, _QCaj, _QUni
Local _CanRes, _CanResTot, _CRest, _CanMinRes
Local nRecNo

    *> Guardar par�metros como propiedades.
    With This.ObjParm
       .DscCPro = cCodPro
       .DscTDoc = cTipDoc
       .DscNDoc = cNumDoc
    EndWith

	*> Inicializar propiedades privadas de la clase.
	=This.InitLocals()

	*> Leer la cabecera del pedido.
	_Sentencia = "F24cCodPro,F24cTipDoc,F24cNumDoc"
    _Where = "F24cCodPro='" + cCodPro + "' And " + ;
             "F24cTipDoc='" + cTipDoc + "' And " + ;
             "F24cNumDoc='" + cNumDoc + "'"


**             "F24cFlgEst < '4' "

    lStado = f3_sql(_Sentencia, 'F24c', _Where, , , 'ResF24c')
    If !lStado
       This.UsrError = 'Documento no existe o no procesable'
       Return lStado
    EndIf

    Select ResF24c
    Go Top
    Scatter Name oF24c

	*> Leer las l�neas de detalle.
    _Where = "F24cCodPro='" + ResF24c.F24cCodPro + "'" + ;
             " And F24cTipDoc='" + ResF24c.F24cTipDoc + "'" + ;
             " And F24cNumDoc='" + ResF24c.F24cNumDoc + "'" + ;
             " And F24lCodPro=F24cCodPro" + ;
             " And F24lTipDoc=F24cTipDoc" + ;
             " And F24lNumDoc=F24cNumDoc" + ;
             " And F24lCanDoc > F24lCanRes"

    _Order = "F24lCodPro, F24lCodArt, F24lNumLot DESC "

    lStado = f3_sql('*', 'F24c,F24l', _Where, _Order, , 'c_F24cl')
    If !lStado
       This.UsrError = 'El documento no tiene l�neas pendientes de reservar'
       Return lStado
    EndIf

    Select c_F24cl

	This.ActzPrm = CreateObject("OraPrmActz")        && Par�metros.
	This.ActzObj = CreateObject("OraFncActz")        && Procedimientos.
	This.ActzObj.ObjParm = This.ActzPrm

	If Type('cLindoc')=='C'
		If !Empty(cLinDoc)
			Delete For F24lLinDoc <> cLinDoc
		EndIf
	EndIf
	
	*>Buscamos la relaci�n tipos de documentos-ubicaciones 10p
	*> Leer las l�neas de detalle.
    _Where = "F10pTipDoc='" + ResF24c.F24cTipDoc + "'"

    _Order = "F10pPriori"

    lStado = f3_sql('*', 'F10p', _Where, _Order, , 'c_F10p')

   Select c_F10p
   Go Top

    Select c_F24cl
    *> Comenzamos la reserva real.-----------------------------------------
    Go Top
    Do While !Eof()
       Scatter Name oF24cl

       *> Guardar propiedades de detalle.
       With This.ObjParm
          .DsdCPro = oF24cl.F24lCodPro
          .DsdTDoc = oF24cl.F24lTipDoc
          .DsdNDoc = oF24cl.F24lNumDoc
          .DsdLDoc = oF24cl.F24lLinDoc
          .DsdCArt = oF24cl.F24lCodArt
       EndWith

       *> Obtener los datos de la ficha del art�culo.
       lStado = f3_seek('F08c', '[m.F08cCodPro=oF24cl.F24lCodPro,m.F08cCodArt=oF24cl.F24lCodArt]')
       If !lStado
		  This.UsrError = 'Error buscando ficha de art�culo: ' + c_F24cl->F24lCodArt
		  Return lStado
       EndIf

       Select F08c
       Go Top
       Scatter Name oArti

       This.CtrCad = oArti.F08cCaduca

       This.ObjParm.DsdTPro = oArti.F08cTipPro	&& Guardo el tipo de producto de la referencia a reservar.
	
       If !This.GetRsvParm()
		  This.UsrError = 'No se encontraron los par�metros de reserva'
		  Return .F.
       EndIf

       *> Orden de la reserva (Lifo,Fifo etc..)
       This.ObtenerOrden()

	    *> Calcular los factores del producto.
		_FPac = oArti.F08cUniPac
		_FCaj = _FPac * oArti.F08cPacCaj
		_FPal = _FCaj * oArti.F08cCajPal
		_FPac = IIf(Empty(_FPac), 1, _FPac)
		_FCaj = IIf(Empty(_FCaj), 1, _FCaj)
		_FPal = IIf(Empty(_FPal), 1, _FPal)

        *> Cantidad a reservar.------------------------------------
        _CanRes = c_F24cl->F24lCanDoc - c_F24cl->F24lCanRes

		*> Dividir la cantidad a reservar en Palets, Cajas y Unidades Sueltas.
		_QPal = Iif(_FPal <= 1, 0, Floor(_CanRes / _FPal) * _FPal)
		_QCaj = Iif(_FCaj < 1, 0, Floor((_CanRes - _QPal) / _FCaj) * _FCaj)
		_QUni = _CanRes - _QPal - _QCaj

		*> Probamos a reservar por cantidad.-----------------------
		*> Si es 'P' probamos reservar por palets.-----------------
		If This.ObjParm.PrgDejPic # 'P'
			_QPal = 0
			_QCaj = _CanRes
			_Quni = 0
		EndIf

        Store 0 to _ResTot

		*> Resevamos Palets.--------------------------------------------------
        _Crest = _QPal

        *> Cantidad m�nima a reservar: Cantidad palet.
        _CanMinRes = _FPal

		Do While _Crest > 0
		   *> Buscamos la ocupaci�n.------------------------------------------
           _Where = "F16cCodPro = '" + c_f24cl->F24lCodPro + ;
                    "' And F16cCodArt = '" + c_f24cl->F24lCodArt + ;
                    "' And F16cSitStk = '" + c_f24cl->F24lSitStk + ;
                    "' And F16cCanFis-F16cCanRes >=" + Str(_CanMinRes) + ;
                    " And F10cCodUbi = F16cCodUbi "

           If !Empty(c_F10p.F10pTipDoc)
           		_Where= _Where + " And F10pCodNum=F10cPickSn And F10pTipDoc='" + c_f24cl->F24lTipDoc + "' "
           Else
                _Where= _Where + " And F10cPickSN In ('N')"
		   EndIf
                _Where= _Where + " And F10cEstSal <> 'S' "

           *> Si hay lote.----------------------------------------------------
           If !Empty(c_f24cl->F24lNumLot)
              _Where = _Where + " And F16cNumLot = '" + c_F24cl->F24lNumLot + "'"
           EndIf
			
           If !Empty(c_F10p.F10pTipDoc)
		       _Order = "F10pPriori ," + This.Orden
	           _Field = "F16c" + _em + ".*,F10p" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"
    	       lStado = f3_sql(_Field, 'F16c,F10p,F10c', _Where, _Order, , 'OcuF16c')
	       Else
	           _Order = This.Orden + ", F10cPickSN"
	           _Field = "F16c" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"
    	       lStado = f3_sql(_Field, 'F16c,F10c', _Where, _Order, , 'OcuF16c')
		   EndIf

		   Select OcuF16c

		   If This.ObjParm.PrgTipMov=="M"
			  *> Realizar el m�nimo n� de movimientos.
			  Locate For F16cCanDis>=_CRest
			  If !Found()
				 Go Bottom
				 If !Eof()
				 	Skip -1
				 EndIf
			  EndIf
		   Else
		   	  Go Top
		   EndIf

		   If Eof()
			  This.UsrError = "No se encuentra lote para este pedido"
      	      This.GrabarIncidencias(c_F24cl.F24lLinDoc,c_f24cl->F24lCodArt,c_f24cl->F24lCanDoc)
              Exit
		   EndIf

           *> El resto se decrementa.---------------------------------------
           _CanResTot = IIF((F16cCanfis - F16cCanres)>=_Crest,_Crest,(F16cCanfis - F16cCanres))
		   _ResTot = _ResTot + IIF((F16cCanfis - F16cCanres)>=_Crest,_Crest,(F16cCanfis - F16cCanres))
 		   _Crest = IIF(_Crest - (F16cCanfis - F16cCanres)<0,0,_Crest - (F16cCanfis - F16cCanres))

           *> Generamos el movimiento.--------------------------------------
		   _Ok = This.GenMov(_CanResTot, 'P')
		   If _Ok = .F.
			  This.UsrError="Error generando movimientos pendientes (Palet)"
     	      This.GrabarIncidencias(c_F24cl.F24lLinDoc,c_f24cl->F24lCodArt,_Crest)  && Display error.
			  Return .F.
		   EndIf
		EndDo

		*> Resevamos cajas.--------------------------------------------------
        _Crest = _Crest + _QCaj

        *> Cantidad m�nima a reservar.
        _CanMinRes = _FCaj

		Do While _Crest > 0
		   *> Buscamos la ocupaci�n.------------------------------------------
		   _Where = "F16cCodPro = '" + c_f24cl->F24lCodPro + ;
                    "' And F16cCodArt = '" + c_f24cl->F24lCodArt + ;
                    "' And F16cSitStk = '" + c_f24cl->F24lSitStk + ;
                    "' And F16cCanFis-F16cCanRes > 0 " + ;
                    " And F10cCodUbi = F16cCodUbi "

           If !Empty(c_F10p.F10pTipDoc)
           		_Where= _Where + " And F10pCodNum=F10cPickSn And F10pTipDoc='" + c_f24cl->F24lTipDoc + "' "
           Else
           		_Where= _Where + " And F10cPickSN In ('S', 'N') "
           EndIf
				_Where= _Where + " And F10cEstSal <> 'S'"

           *> Si hay lote.----------------------------------------------------
           If !Empty(c_f24cl->F24lNumLot)
              _Where = _Where + " And F16cNumLot = '" + c_F24cl->F24lNumLot + "'"
           EndIf

		   If !Empty(c_F10p.F10pTipDoc)
		       _Order = "F10pPriori ," + This.Orden
	           _Field = "F16c" + _em + ".*,F10p" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"
    	       lStado = f3_sql(_Field, 'F16c,F10p,F10c', _Where, _Order, , 'OcuF16c')
	       Else
	           _Order = This.Orden + ", F10cPickSN DESC"

	           _Field = "F16c" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"

	           lStado = f3_sql(_Field, 'F16c,F10c', _Where, _Order, , 'OcuF16c')
		   EndIf

		   Select OcuF16c
		   If This.ObjParm.PrgTipMov=="M"
			  *> Realizar el m�nimo n� de movimientos.
			  Locate For F16cCanDis>=_CRest
			  If !Found()
				 Go Bottom
				 If !Eof()
				 	Skip -1
				 EndIf
			  EndIf
		   Else
		   	  Go Top
		   EndIf

		   If Eof()
			  This.UsrError = "Algunas l�neas no se reservaron (cajas) "
		      This.GrabarIncidencias(c_F24cl.F24lLinDoc,c_f24cl->F24lCodArt,0)
              Exit
		   EndIf

           *> El resto se decrementa.---------------------------------------
           _CanResTot = IIF((F16cCanfis - F16cCanres)>=_Crest,_Crest,(F16cCanfis - F16cCanres))
		   _ResTot = _ResTot + IIF((F16cCanfis - F16cCanres)>=_Crest,_Crest,(F16cCanfis - F16cCanres))
 		   _Crest = IIF(_Crest - (F16cCanfis - F16cCanres)<0,0,_Crest - (F16cCanfis - F16cCanres))
		
           *> Generamos el movimiento.----------------------------------------
		   This.GenMov(_CanResTot, 'C')
		EndDo

		*> Resevamos unidades.--------------------------------------------------
        _CRest = _Crest + _QUni

        *> Cantidad m�nima a reservar
        _CanMinRes = 1

		Do While _Crest > 0
		   *> Buscamos la ocupaci�n.--------------------------------------------
           _Where = "F16cCodPro = '" + c_f24cl->F24lCodPro + ;
                    "' And F16cCodArt = '" + c_f24cl->F24lCodArt + ;
                    "' And F16cSitStk = '" + c_f24cl->F24lSitStk + ;
                    "' And F16cCanFis-F16cCanRes > 0 " + ;
                    " And F10cCodUbi = F16cCodUbi "
           If !Empty(c_F10p.F10pTipDoc)
           		_Where= _Where + " And F10pCodNum=F10cPickSn And F10pTipDoc='" + c_f24cl->F24lTipDoc + "' "
           Else
                _Where= _Where + " And F10cPickSN In ('U', 'S', 'N') "
		   EndIf
                _Where= _Where + " And F10cEstSal <> 'S'"

           *> Si hay lote.----------------------------------------------------
           If !Empty(c_f24cl->F24lNumLot)
              _Where = _Where + " And F16cNumLot = '" + c_F24cl->F24lNumLot + "'"
           EndIf

		   If !Empty(c_F10p.F10pTipDoc)
		       _Order = "F10pPriori ," + This.Orden

	           _Field = "F16c" + _em + ".*,F10p" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"

    	       lStado = f3_sql(_Field, 'F16c,F10p,F10c', _Where, _Order, , 'OcuF16c')
	       Else
	           _Order = " F10cPickSN DESC," +  This.Orden

	           _Field = "F16c" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"

	           lStado = f3_sql(_Field, 'F16c,F10c', _Where, _Order, , 'OcuF16c')
		   EndIf
		   Select OcuF16c
		   If This.ObjParm.PrgTipMov=="M"
			  *> Realizar el m�nimo n� de movimientos.
			  Locate For F16cCanDis>=_CRest
			  If !Found()
				 Go Bottom
				 If !Eof()
				 	Skip -1
				 EndIf
			  EndIf
		   Else
		   	  Go Top
		   EndIf

		   If Eof()
			  This.UsrError="Algunas l�neas no se reservaron (unidades)"
		      This.GrabarIncidencias(c_F24cl.F24lLinDoc,c_f24cl->F24lCodArt,0) && Display error.
              Exit
		   EndIf

           *> El resto se decrementa.---------------------------------------
           _CanResTot = IIF((F16cCanfis - F16cCanres)>=_Crest,_Crest,(F16cCanfis - F16cCanres))
		   _ResTot = _ResTot + IIF((F16cCanfis - F16cCanres)>=_Crest,_Crest,(F16cCanfis - F16cCanres))
 		   _Crest = IIF(_Crest - (F16cCanfis - F16cCanres)<0,0,_Crest - (F16cCanfis - F16cCanres))

           *> Generamos el movimiento.--------------------------------------
		   This.GenMov(_CanResTot, 'U')
		EndDo

        Select c_F24cl
        nRecNo = Recno()

        *> Actualizar el documento (l�neas).-----------------------
		_Ok = This.UpdLin(_ResTot)
	    If _Ok = .F.
    	   This.UsrError = "Error dando de alta la l�nea "
		   This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CanResTot)  && Display error.
		   Return .F.
	    EndIf

		*> Actualizar el documento.---------------------------------
		_Ok = This.UpdDoc()
	    If _Ok = .F.
	       This.UsrError = "Error dando de alta la cabecera "
		   This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CanResTot) && Display error.
		   Return .F.
	    EndIf

	    Select c_F24cl
	    Goto nRecNo

	  *> Inicio tratamiento de Pol�tica de servicio (F24cPolSer)
      If _CRest > 0 .And. !IsNull(c_F24cl->F24cPolSer)
        Do Case
          *> Documento completo: Cancelar toda la reserva.
          Case This.ObjParm.PrgTipRes=='D' .Or. c_F24cl->F24cPolSer=='2'
             cTexto = "Reserva de documento cancelada : Pol�tica Servicio 2"
             =WaitWindow(cTexto, , 1)
             =This.CancelarReserva (This.ObjParm.DscCPro, This.ObjParm.DscTDoc, This.ObjParm.DscNDoc, .F.)

			 *> Incidencia de reserva.
			 This.UsrError = cTexto
		     This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CRest)
             Exit

          *> L�nea completa: Cancelar reserva de la l�nea.
          Case (This.ObjParm.PrgTipRes=='L' .Or. c_F24cl->F24cPolSer=='1')
             cTexto = "Reserva de l�nea cancelada : Pol�tica Servicio 1"
             =WaitWindow (cTexto, , 1)
             =This.CancelarReserva (This.ObjParm.DscCPro, This.ObjParm.DscTDoc, This.ObjParm.DscNDoc, This.ObjParm.DsdLDoc)

		     *> Incidencia de reserva.
		     This.UsrError = cTexto
    	     This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CRest)

          *> Reserva parcial de documento.
          Otherwise
             cTexto = "Reserva de l�nea parcial : Pol�tica Servicio 0"
             =WaitWindow(cTexto, , 1)

		     *> Incidencia de reserva.
		     This.UsrError = cTexto
    	     This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CRest)

             =SqlCommit(_ASql)
        EndCase
      EndIf 	

      *> Fin tratamiento pol�tica de servicio.

      Select c_F24cl
      Skip
	EndDo

	Use In (Select("ResF24c"))
	Use In (Select("c_F24cl"))

Return

ENDPROC
PROCEDURE getrsvparm

*> Obtener los par�metros generales de reserva.

Private _Where
Local lStado

   *> Obtener par�metros del art�culo, si hay.
   _Where = "F25cCodPro='" +  This.ObjParm.DsdCPro + "' And F25cCodArt='" + This.ObjParm.DsdCArt + "'"
   lStado = f3_sql('*', 'F25c', _Where, , , "F25cPrm")

   If !lStado
	   *> Obtener los par�metros del tipo de producto, si hay.
	   _Where = "F25cCodPro='" +  This.ObjParm.DsdCPro + "' And F25cTipPro='" + This.ObjParm.DsdTPro + "'"
	   lStado = f3_sql('*', 'F25c', _Where, , , "F25cPrm")
   EndIf

   If !lStado
	   *> Obtener los par�metros del propietario, si hay.
	   _Where = "F25cCodPro='" +  This.ObjParm.DsdCPro + "' And F25cCodArt='" + Space(13) + "'"
	   lStado = f3_sql('*', 'F25c', _Where, , , "F25cPrm")
   EndIf

   If !lStado
	   *> Obtener los par�metros generales.
	   _Where = "F25cCodPro='" + Space(6) + "' And F25cCodArt='" + Space(13) + "'"
	   lStado = f3_sql('*', 'F25c', _Where, , , "F25cPrm")
   EndIf

   Select F25cPrm
   Go Top
   With This.ObjParm
	  .PrgSerPic = F25cSerPic
	  .PrgTipMov = F25cTipMov
	  .PrgOrdExt = F25cOrdExt
	  .PrgDejPic = F25cDejPic
	  .PrgTipRes = F25cTipRes
	  .PrgCnfAut = F25cConAut		&& No utilizado
	  .PrgImpAut = F25cImpEti		&& No utilizado
   EndWith

Use In (Select("F25cPrm"))
Return lStado

ENDPROC
PROCEDURE obtenerorden

*> Seleccionar el orden de reserva (FIFO/LIFO/...)

Local _OrderCant

If This.ObjParm.PrgTipMov=="M"
	_OrderCant = " F16cCanDis DESC "
Else
	_OrderCant = " F16cCanDis ASC "
EndIf

Do Case
	*> Ordenar por Fecha de Caducidad. (FEFO). Solo si el producto tiene control de caducidad.
	Case This.ObjParm.PrgOrdExt=='C' .And. This.CtrCad=="S"
		This.Orden = "F16cFecCad ASC, F16cCanDis ASC "

	*> Ordenar por FIFO.
	Case This.ObjParm.PrgOrdExt=='F'
		This.Orden = "F16cFecEnt ASC, F16cCanDis ASC "

	*> Ordenar por Alfab�tico
	Case This.ObjParm.PrgOrdExt=='A'
		This.Orden = "F16cNumLot ASC, F16cCanDis ASC "

	*> Ordenar por LIFO (opci�n por defecto).
	OtherWise
		This.Orden = "F16cFecEnt DESC, F16cCanDis ASC "
EndCase

Return

ENDPROC
PROCEDURE genmov

*> Generar MP de reserva.
*> Los datos de art�culo se han le�do en This.Reserva().

*> Modificaciones:
*>		AVC - 02.06.2005 : Unificar c�lculo de muelle con el la clase de tratamiento de listas,
*>						   pues este proceso se utilizar� aqu� y en confirmaci�n de listas.
*>		AVC - 13.06.2006 : Validar la ubicaci�n de muelle.


Parameters _CanResTot, cOriRes

   Private cWhere
   Local lStado

   #Define _P This.ObjParm
   #Define _J This.ActzObj
   #Define _Z This.ActzObj.ObjParm

   This.WRowId = OcuF16c->F16cIdOcup

   lStado = .T.

   *> Inicializar par�metros de actualizaci�n.
   _Z.Inicializar
   _Z.PTAcc = '04'                        && Operaci�n: Reserva

   *> Datos actualizaci�n ubicaciones.
   _Z.PUbOld = OcuF16c->F16cCodUbi        && Ubicaci�n antigua
   _Z.PUFlag = 'S'                        && Flag actualizar ubicaci�n

   *> Datos actualizaci�n ocupaciones.
   _Z.POTDoc = _P.DsdTDoc                 && Tipo documento
   _Z.PONDoc = _P.DsdNDoc                 && N� documento
   _Z.POLDoc = Val(_P.DsdLDoc)            && L�nea documento
   _Z.POTEnt = c_F24cl->F24cTipEnt        && Tipo entidad
   _Z.POCEnt = c_F24cl->F24cCodEnt        && C�digo entidad
   _Z.PONPed = c_F24cl->F24cNumPed        && N� pedido
   _Z.POLPed = 0                          && L�nea pedido
   _Z.POFMov = c_F24cl->F24cFecDoc        && Fecha documento
   _Z.POFMovOra = c_F24cl->F24cFecDoc     && Fecha documento Oracle
   _Z.POCPro = OcuF16c->F16cCodPro        && C�digo propietario
   _Z.POCArt = OcuF16c->F16cCodArt        && C�digo art�culo
   _Z.PONLot = OcuF16c->F16cNumLot        && N�mero lote
   _Z.POSStk = OcuF16c->F16cSitStk        && Situaci�n stock
   _Z.POFCad = OcuF16c->F16cFecCad        && Fecha caducidad
   _Z.POFCadOra = OcuF16c->F16cFecCad     && Fecha caducidad Oracle
   _Z.POCFis = 0                          && Cantidad f�sica
*   _Z.POPAct = _AliasF08c->F08cPCoste    && Precio actual
   _Z.POPAct = 0	    				  && Precio actual
   _Z.POCAlm = _Alma                      && C�digo almac�n
   _Z.POCUbi = OcuF16c->F16cCodUbi        && C�digo ubicaci�n
   _Z.PONPal = OcuF16c->F16cNumPal        && N�mero palet
   _Z.POTPal = OcuF16c->F16cTamPal        && Tipo palet
   _Z.POFUni = OcuF16c->F16cUniVen        && Unidades venta
   _Z.POFSer = OcuF16c->F16cUniPac        && Unidades pack
   _Z.POFEnv = OcuF16c->F16cPacCaj        && Packs caja
   _Z.POFPal = OcuF16c->F16cCajPal        && Cajas palet

***_Z.POFFab = CToD(Space(8))             && Fecha fabricaci�n
***_Z.POFFabOra = CToD(Space(8))          && Fecha fabricaci�n Oracle
***_Z.PONEnt = Space(10)                  && N� entrada
***_Z.POFEnt = CToD(Space(8))             && Fecha entrada
***_Z.POFEntOra = CToD(Space(8))          && Fecha entrada Oracle
***_Z.POFCal = CToD(Space(8))             && Fecha calidad
***_Z.POFCalOra = CToD(Space(8))          && Fecha calidad Oracle
***_Z.PONAna = Space(6)                   && N� an�lisis
***_Z.PONotE = Space(10)                  && Nota entrega

   _Z.POCRes = _CanResTot				  && Cantidad reservada
   _Z.PORowId= This.WRowId                && Identificador de ocupaci�n
   _Z.POFlag = 'S'                        && Flag actualizar ocupaci�n

   *> Datos actualizaci�n situaciones stock
   _Z.PSCPro = OcuF16c->F16cCodPro        && C�digo propietario
   _Z.PSCArt = OcuF16c->F16cCodArt        && C�digo art�culo
   _Z.PSCAlm = _Alma                      && C�digo almac�n
   _Z.PStOld = OcuF16c->F16cSitStk        && Situaci�n stock antigua
   _Z.PStNew = '2000'                     && Situaci�n stock nueva
   _Z.PSCFis = _CanResTot				  && Cantidad
   _Z.PSFlag = 'S'                        && Flag actualizar situaci�n stock

   *> Datos actualizaci�n MPs
   _Z.PMTMov = '2000'                     && Tipo movimiento
   _Z.PMFMov = Date()                     && Fecha movimiento
   _Z.PMFMovOra = Date()                  && Fecha movimiento Oracle
   _Z.PMFDoc = c_F24cl->F24cFecDoc        && Fecha documento
   _Z.PMFDocOra = c_F24cl->F24cFecDoc     && Fecha documento Oracle
   _Z.PMRHab = c_F24cl->F24cRutHab        && Ruta habitual
   _Z.PMStat = '0'                        && Estado movimiento
   _Z.PMORes = cOriRes                    && Para saber si son cajas unidades o palets
   _Z.PMTUbi = OcuF16c->F10cTipAlm        && Tipo ubicaci�n

   *_Z.PMORut = c_F24cl->F24cOrdRut        && Orden ruta
   _Z.PMORut = IIf(c_F24cl->F24cOrdRut==0, 9, _Z.PMORut)  && LRC. 12/2/9. Prioridad de LT

   _Z.PMTERe = c_F24cl->F24cTenTre        && Tipo entidad reexpedici�n
   _Z.PMCERe = c_F24cl->F24cCenTre        && C�digo entidad reexpedici�n
   _Z.PMVHab = c_F24cl->F24cVenHab        && Vendedor habitual
   _TMac = Space(4)
   _Z.PMTMac = _TMac                      && Tipo MAC
   _Z.PMTMac = 'MSTD'                     && Tipo MAC
   _Z.PMSecc = F08c.F08cSeccio    		  && Secci�n

***_Z.PMTMov = This.GetTMov               && Tipo movimiento
***_Z.PMCOpe = ''                         && C�digo operario
***_Z.PMNLst = ''                         && N�mero lista
***_Z.PMNExp = ''                         && N�mero expedici�n
***_Z.PMORec = ''                         && Orden recorrido
***_Z.PMORes = OcuF16c->F10cPickSn        && Origen reserva
***_Z.PMORes = This.Calculaori()          && Para saber si son cajas unidades o palets
***_Z.PMTMAs = ''                         && Tipo MAC asociado
***_Z.PMNMAs = ''                         && N�mero MAC asociado
***_Z.PMMUni = ''                         && MAC unido
***_Z.PMNMac = ''                         && N�mero MAC
***_Z.PMFlg2 = ''                         && Flag 2

   *> Si la ocupaci�n est� pendiente de reposici�n, MP bloqueado.-----
   If OcuF16c->F16cFlag1 = 'R'
      _Z.PMFlg1 = 'B'                     && Flag 1
   Else
      _Z.PMFlg1 = ' '                     && Flag 1
   EndIf

   _Z.PMFgMP = 'S'                        && Flag actualizar MP

   *> M�s campos (para hist�rico de movimientos)
   _Z.PMDAso = c_F24cl->F24cDirAso        && Direcci�n asociada
   _Z.PMUDes = c_F24cl->F24cUbiExp        && Ubicaci�n expedici�n

	If !This.ValidarMuelle(_Z.PMUDes)
		_Z.PMUDes = This.ListObj.Muelle
	EndIf

***_Z.PMEnSa = ''                         && Entrada / Salida
***_Z.PMCTra = ''                         && C�digo transportista
***_Z.PMBloq = ''                         && Bloqueado
***_Z.PMFlgE = ''                         && Flag env�o
***_Z.PMFEnv = ''                         && Fecha env�o
***_Z.PMFEnvOra = ''                      && Fecha env�o Oracle
***_Z.PMNEnv = ''                         && N�mero env�o
***_Z.PMFOrd = ''                         && Fecha orden
***_Z.PMFOrdOra = ''                      && Fecha orden Oracle
   _Z.PMFgHM = 'N'                        && Flag actualizar hist�rico

   *> Otros Datos
***_Z.PWUser = ''                         && Usuario
***_Z.PWPrgm = ''                         && Programa
***_Z.PWFree = ''                         && Liberar recursos (inactivo, viene del AS/400)
***_Z.PWCRtn = ''                         && C�digo retorno
***_Z.PWFill = ''                         && Filler

   *> Llamada a la funci�n de actualizaci�n.-----------------------------
   _J.Ejecutar
   If _Z.PWCRtn > '49'
      lStado = .F.
      Exit
   Else
      _Z.POCFis = _CanResTot              && Cantidad f�sica
      This.GenMP                          && Generar MP's con desglose P, C y U.
   EndIf

Return lStado

ENDPROC
PROCEDURE genmp

*> Generar MP's por POCFIS, desglosando en MP's de palets, cajas y unidades si es necesario.

#Define _P This.ObjParm
#Define _J This.ActzObj
#Define _Z This.ActzObj.ObjParm

Private n_UniCaj, n_UniPal, n_CanTot, n_CanPal, n_CanCaj, n_CanUni, n_TotPal, n_TotCaj

n_UniCaj = _Z.POFSer * _Z.POFEnv
n_UniPal = _Z.POFSer * _Z.POFEnv * _Z.POFPal
n_UniCaj = IIf(n_UniCaj = 0, 1, n_UniCaj)
n_UniPal = IIf(n_UniPal = 0, 1, n_UniPal)
*n_CanTot = This.ActzObj.ObjParm.POCFis
n_CanTot = _Z.POCFis

*> Desglosar cantidades en palets, cajas y unidades.------------------
n_CanUni = n_CanTot

n_TotPal = Int(n_CanUni / n_UniPal)              && Palets
n_CanPal = n_TotPal * n_UniPal
n_CanUni = n_CanUni - n_CanPal

n_TotCaj = Int(n_CanUni / n_UniCaj)              && Cajas
n_CanCaj = n_TotCaj * n_UniCaj
n_CanUni = n_CanUni - n_CanCaj

*> Generar movimientos para palets.-----------------------------------
*If n_CanPal > 0
*   _Z.POCFis = n_CanPal
*   _Z.PMNMov = Ora_NewMp()
*   _J.ActMP
*EndIf

*> Generar movimientos para cajas.------------------------------------
*If n_CanCaj > 0
*   _Z.POCFis = n_CanCaj
*   _Z.PMNMov = Ora_NewMp()
*   _J.ActMP
*EndIf

*> Generar movimientos para unidades.---------------------------------
*If n_CanUni > 0
*   _Z.POCFis = n_CanUni
*   _Z.PMNMov = Ora_NewMp()
*   _J.ActMP
*EndIf

*> Recuperar cantidad total del movimiento en POCFIS.-----------------
_Z.POCFis = n_CanTot
_J.ActMP

Return

ENDPROC
PROCEDURE calculaori
*> Calculamos.-----------------------

   _Z.POFUni = OcuF16c->F16cUniVen    && Unidades venta
   _Z.POFSer = OcuF16c->F16cUniPac    && Unidades pack
   _Z.POFEnv = OcuF16c->F16cPacCaj    && Packs caja
   _Z.POFPal = OcuF16c->F16cCajPal    && Cajas palet

   _Unidades = OcuF16c->F16cPacCaj
   _Cajas    = OcuF16c->F16cCajPal
   _Palet    = OcuF16c->F16cPacCaj*OcuF16c->F16cCajPal

   Do Case
      Case Mod((OcuF16c->F16cCanFis-OcuF16c->F16cCanRes),_Palet)=0
      		_Ori = 'P'
      Case Mod((OcuF16c->F16cCanFis-OcuF16c->F16cCanRes),_Unidades)=0
			_Ori = 'C'
      OtherWise
			_Ori = 'U'
   EndCase

Return _Ori

ENDPROC
PROCEDURE upddoc

*> Procedimiento para actualizar la cabecera del documento.

#Define _P This.ObjParm

Private _XSelec, _XSet, _XWhere, _XFromF
Private _Flg2
Local lStado

*> Primer paso: actualizar el cursor de documentos.
Select c_F24cl
Count For F24lFlgEst<>'2' To _Flg2

Replace All F24cFlgEst With Iif(Empty(_Flg2), '3', '2')
Go Top

*> Formar la instrucci�n SQL de actualizaci�n.
_XWhere =           " WHERE F24cCodPro='" + _P.DsdCPro + "'"
_XWhere = _XWhere + " And F24cTipDoc='" + _P.DsdTDoc + "'"
_XWhere = _XWhere + " And F24cNumDoc='" + _P.DsdNDoc + "'"

_XFromF = "F24C" + _em
_XSet = " Set F24cFlgEst='" + c_F24cl->F24cFlgEst + "'"
_XSelec = "UPDATE " + _XFromF + _XSet + _XWhere

lStado = f3_SqlExec(_Asql, _XSelec)

Return

ENDPROC
PROCEDURE updlin

*> Actualizar la l�nea del documento de salida.

Parameters _ResTot

#Define _P This.ObjParm

Private _XSelec, _XSet, _XWhere, _XFromF
Local lStado

_oldPoint = Set('Point')
Set Point To '.'

*> Primer paso: actualizar el cursor de documentos.
Select c_F24cl
Locate For F24lCodPro = _P.DsdCPro .And. ;
           F24lTipDoc = _P.DsdTDoc .And. ;
           F24lNumDoc = _P.DsdNDoc .And. ;
           F24lLinDoc = _P.DsdLDoc
If Found()
   Replace F24lCanRes With F24lCanRes + _ResTot, ;
           F24lFlgEst With Iif(F24lCanRes>=F24lCanDoc, '2', Iif(F24lCanRes==0, '0', '1'))
EndIf

*> Formar la instrucci�n SQL de actualizaci�n.
_XWhere =           " WHERE F24lCodPro='" + _P.DsdCPro + "'"
_XWhere = _XWhere + " And F24lTipDoc='" + _P.DsdTDoc + "'"
_XWhere = _XWhere + " And F24lNumDoc='" + _P.DsdNDoc + "'"
_XWhere = _XWhere + " And F24lLinDoc='" + _P.DsdLDoc + "'"

_XFromF = "F24L" + _em
_XSet =         " Set F24lCanRes=" + Str(c_F24cl->F24lCanRes,12,3) + ","
_XSet = _XSet + "    F24lFlgEst='" + c_F24cl->F24lFlgEst + "'"
_XSelec = "UPDATE " + _XFromF + _XSet + _XWhere

lStado = f3_SqlExec(_Asql, _XSelec)

Set Point To (_oldPoint)

Return

ENDPROC
PROCEDURE grabarincidencias

*> Grabar archivo de incidencias de reserva.
*> Si no existe, se habr� creado en This.CrearFicheroIncidencias.
Parameters cLinAct,cCodArt,intCanFis

This.CrearFicheroIncidencias
Select 0
Use F99r
Append Blank
Replace F99rFecInc With DateTime(), ;
        F99rCodUsr With _UsrCod, ;
        F99rCodAlm With _Alma, ;
        F99rCodPro With This.ObjParm.DscCPro, ;
        F99rCodEmp With _em, ;
        F99rTipDoc With This.ObjParm.DscTDoc, ;
        F99rNumDoc With This.ObjParm.DscNDoc, ;
        F99rLinDoc With cLinAct, ;
        F99rCodArt With cCodArt, ;
        F99rCanFis With intCanFis, ;
        F99rNumLot With Space(1), ;
        F99rUbiOri With Space(1), ;
        F99rCodInc With '000', ;
        F99rTipInc With 'GENERAL', ;
        F99rTxtInc With This.UsrError, ;
        F99rTxtPrg With 'reserva.reserva'

Use In F99r
Return

ENDPROC
PROCEDURE crearficheroincidencias

*> Crea el fichero de incidencias de reserva, si no existe.
If !File('F99r.Dbf')
   Create Table F99r ;
     (F99rFecInc DateTime, ;
      F99rCodUsr C(6), ;
      F99rCodAlm C(4), ;
      F99rCodPro C(6), ;
      F99rCodEmp C(3), ;
      F99rTipDoc C(4), ;
      F99rNumDoc C(13), ;
      F99rLinDoc C(4), ;
      F99rCodArt C(13), ;
      F99rCanFis N(8, 0), ;
      F99rNumLot C(15), ;
      F99rUbiOri C(14), ;
      F99rCodInc C(3), ;
      F99rTipInc C(10), ;
      F99rTxtInc Memo, ;
      F99rTxtPrg Memo)
EndIf

If Used('F99r')
   Use In F99r
EndIf

Return

ENDPROC
PROCEDURE volbul
*>
*> Recalcular un bulto concreto.
*>    Cabecera: Estado del bulto.
*>    Detalle : N� de bulto relativo.

Private _Selec
Private _TotalP, _TotalD, _TotalT
Private EstadoBulto
Private f_where
Local strBultActual,intNumBul

_Selec = "Select F26v" + _em + ".*, F26w" + _em + ".* " + ;
         " From F26v" + _em + ", F26w" + _em + ;
         " Where F26vNumMac='" + This.ObjParm.VolNBu + "'" + ;
         " And   F26wNumMac=F26vNumMac" + ;
         " Order By F26vNumMac, F26wNMovMp"

*> El control de errores lo debe de realizar la funci�n que llama a este m�todo.
_xier = f3_SqlExec(_ASql, _Selec, 'BultosCur')
If _xier <= 0
   ErrVol = '55'
   This.VolErr('X', ErrVol)
   Return .F.
EndIf

*> Actualizar el n� de bulto relativo (si fracciones).
Select BultosCur
Replace All F26wNumBul With PadL(AllTrim(Str(RecNo())), 4, '0')

Count To _TotalD                           && N� total de l�neas de detalle del bulto.
Count For F26wEstLin = '1' To _TotalP      && N� total de l�neas de detalle en proceso.
Count For F26wEstLin = '3' To _TotalT      && N� total de l�neas de detalle procesadas.

*> Actualizar la cabecera del bulto.
EstadoBulto = '0'
If _TotalP > 0
   EstadoBulto = '1'
Else
   If _TotalT >= _TotalD
      EstadoBulto = '3'
   EndIf
EndIf

f_where = "F26vNumMac='" + This.ObjParm.VolNBu + "'"
=f3_UpdTun('F26v', , 'F26vEstBul', 'EstadoBulto', , f_where, 'N', 'N')

*> Actualizar las l�neas de detalle del bulto.
Select BultosCur
Go Top
Do While !Eof()
   f_where = "F26WNMOVMP='" + BultosCur.F26WNMOVMP + "'"
   =f3_UpdTun('F26w', , 'F26wNumBul', 'BultosCur.F26wNumBul', , f_where, 'N', 'N')

   Select BultosCur
   Skip
EndDo

Return

ENDPROC
PROCEDURE volbuldoc
*>
*> Recalcular los bultos de un documento concreto.
*> Llama a This.VolBul()

Private cSelec, cFromF, cWhere, cOrder, cGroup, cBultoRelativo
Local lStado, nCurrentBulto

cSelec = "*"
cFromF = "F26v"
cWhere = "F26vCodPro='" + This.ObjParm.DscCPro + "' And " + ;
         "F26vTipDoc='" + This.ObjParm.DscTDoc + "' And " + ;
         "F26vNumDoc='" + This.ObjParm.DscNDoc + "'"
cOrder = ""
cGroup = ""

Use In (Select('BultosDocCur'))

*> El control de errores lo debe de realizar la funci�n que llama a este m�todo.
lStado = f3_sql(cSelec, cFromF, cWhere, cOrder, cGroup, 'BultosDocCur')
If _xier <= 0
   ErrVol = '55'
   This.VolErr('U', ErrVol)
   Return .F.
EndIf

Store 0 To nCurrentBulto

Select BultosDocCur
Go Top

Do While!Eof()
   *> Actualizar estado del bulto y del detalle.
   This.ObjParm.VolNBu = F26vNumMac
   lStado = This.VolBul()

   *> Calcular n� de bulto relativo.
   nCurrentBulto = nCurrentBulto + 1
   cWhere = "F26vNumMac='" + BultosDocCur.F26vNumMac + "'"
   cBultoRelativo = PadL(AllTrim(Str(nCurrentBulto, 4, 0)), 4, '0')
   =f3_UpdTun('F26v', , 'F26vNumBul', cBultoRelativo, , cWhere, 'N', 'N')

   Select BultosDocCur
   Skip
EndDo

Use In (Select('BultosDocCur'))

Return lStado

ENDPROC
PROCEDURE volcan
***> Volumetr�a - Cancelaci�n de Mac's por documento.-----------------

*> Observaciones:
*>      - Actualizar cabecera de documento de salida. AVC - 09.02.2000
*>

*> Control de errores en cancelaci�n.---------------------------------
Private ErrVol, f_Where
ErrVol = "00"

*> Comprobar que no hay nada del documento en lista.------------------
*f_Where = "F26lCodPro = '" + This.ObjParm.DscCPro + "' And F26lTipDoc = '" + ;
*      This.ObjParm.DscTDoc + "' And F26lNumDoc = '" + This.ObjParm.DscNDoc + "'"
*l_Enc = F3_SeekTun('F26l', f_Where)
*If l_Enc = .T.
*   ErrVol = "60"                 && El documento tiene movimientos en lista.
*EndIf

*> Comprobar que tiene volumetr�a generada.---------------------------
f_Where = "F14cCodPro = '" + This.ObjParm.DscCPro + "' And F14cTipDoc = '" + ;
      This.ObjParm.DscTDoc + "' And F14cNumDoc = '" + This.ObjParm.DscNDoc + "' And " + ;
      "F14cNumMac <> LPad(' ', 9)"
l_Enc = F3_SeekTun('F14c', f_Where)
If l_Enc = .F.  .And.  ErrVol < "50"
   ErrVol = "65"                 && El documento no tiene volumetr�a generada.
EndIf

*> Cargar cursor con bultos del documento.----------------------------
=CrtCursor('F26v', 'VCanF26v')
f_Where = "F26vCodPro = '" + This.ObjParm.DscCPro + "' And F26vTipDoc = '" + ;
      This.ObjParm.DscTDoc + "' And F26vNumDoc = '" + This.ObjParm.DscNDoc + "'"
l_Enc = F3_Sql('*', 'F26v', f_Where, , , 'VCanF26v')
If l_Enc = .F.  .And.  ErrVol < "50"
   ErrVol = "70"                 && El documento no tiene volumetr�a generada.
EndIf

Select VCanF26v
Go Top
Do While !EOF()  .And.  ErrVol < "50"
   *> Borrar detalle del bulto actual.--------------------------------
   f_Where = "F26wNumMac = '" + VCanF26v.F26vNumMac + "'"
   Sw = F3_DelTun('F26w', , f_Where, 'N')
   If Sw = .F.
      ErrVol = "50"         && Error en borrado de detalle bulto.
      Loop
   EndIf

   *> Borrar cabecera del bulto actual.-------------------------------
   f_Where = "F26vNumMac = '" + VCanF26v.F26vNumMac + "'"
   Sw = F3_DelTun('F26v', , f_Where, 'N')
   If Sw = .F.
      ErrVol = "51"         && Error en borrado de cabecera bulto.
      Loop
   EndIf

   *> Vaciar n�mero de Mac en movimiento pendiente.-------------------
   c_NumMac = Space(9)
   f_Where = "F14cNumMac = '" + VCanF26v.F26vNumMac + _cm
   Sw = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
   If Sw = .F.
      ErrVol = "52"         && Error en vaciar n�mero Mac en MP.
      Loop
   EndIf

   *> Vaciar n�mero de Mac en detalle lista.--------------------------
   c_NumMac = Space(9)
   f_Where = "F26lNumMac='" + VCanF26v.F26vNumMac + _cm
   Sw = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
   If Sw = .F.
      ErrVol = "53"         && Error en vaciar n�mero Mac en MP.
      Loop
   EndIf

   *> Siguiente bulto.------------------------------------------------
   Select VCanF26v
   Skip
EndDo

*> Actualizar el documento de salida.---------------------------------
This.VolDoc

*> Tratamiento mensajes de error.-------------------------------------
This.VolErr('C', ErrVol)

Use In VCanF26v

ENDPROC
PROCEDURE voldlg
*>
*> Desglose de MPs en uno o mas MACs, seg�n ficha de ar�culos/soportes, F08s.
*> Procesa el registro actual del VPicF14c, dejando la cantidad que NO se ha
*> desglosado, para que siga el proceso normal de generaci�n de volumetr�a.

Private _Where, _Order, _Group
Private nCanMac, cNumMov

*> Leer los datos de art�culos/soporte.
_Where = "F08sCodPro='" + VPicF14c.F14cCodPro + "' And " + ;
         "F08sCodArt='" + VPicF14c.F14cCodArt + "'"
_Order = 'F08sCodPro, F08sCodArt, F08sCantid'
_Group = ''

If !f3_sql('*', 'F08s', _Where, _Order, _Group, 'F08sCur')
   Return
EndIf

Select F08sCur
Go Top
Locate For F08sCantid >= VPicF14c.F14cCanFis

*> No hay ficha art�culo/soporte o bien, no hay desglose.
If !Found() .Or. F08sLvlAgr # 'S'
   Return
EndIf

Store VPicF14c.F14cCanFis To nCanMac

*> Si la cantidad es igual o menor a la de desglose, volver.
If nCanMac <= F08sCur.F08sCanMac
   Return
EndIf

Do While nCanMac > F08sCur.F08sCanMac
   *> Pero NO nCanMac >= F08sCur.F08sCanMac, para que la cantidad restante nunca
   *> sea cero lo que obligar�a a borrar el MP, la lista, ..., en fin, un rollo.

   *> Calcular volumen del movimiento.
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VPicF14c.F14cCodPro, ;
                                VPicF14c.F14cCodArt, ;
                                F08sCur.F08sCanMac, ;
                                n_Peso, n_Volu

   *> Desglosar el movimiento (MP y/o lista).
   _LxErr = ''
   cNumMov = DividirMvt(VPicF14c.F14cNumMov, F08sCur.F08sCanMac)
   If !Empty(_LxErr)
      Do Form St3Inc With .T.
      Return .F.
   EndIf

   *> Crear la cabecera MAC.
   This.VolNewMac
   Replace Macs.MaVolLib With 0               && MAC �nico.

   *> Crear detalle de bulto.
   Select VGenF26w
   Append Blank
   Replace F26wTipMac With VPicF14c->F14cTipMac, ;
           F26wNumMac With c_NumMac, ;
           F26wNumBul With PadL(AllTrim(Str(n_BulAct)), 4, '0'), ;
           F26wNMovMP With cNumMov, ;
           F26wNMovHM With Space(10), ;
           F26wEstLin With '0'

   l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
   If l_Enc = .F.
      b_Err = .T.
      ErrVol = "91"
      Return .F.
   EndIf

   If b_Err = .F.
      *> Grabar n�mero de MAC al MP.----------------------------------
      f_Where = "F14cNumMov = '" + cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "92"
         Return .F.
      EndIf

      *> Grabar n�mero de MAC en detalle lista.-----------------------
      f_Where = "F26lNumMov = '" + cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "93"
         Return .F.
      EndIf
   EndIf

   *> Actualiza la cantidad restante en cursor de MPs.
   Select VPicF14c
   Replace F14cCanFis With F14cCanFis - F08sCur.F08sCanMac

   *> Actualizar cantidad restante para desglosar.
   nCanMac = nCanMac - F08sCur.F08sCanMac
EndDo

*> Actualizar la cabecera de la lista.

*>
If Used('F08sCur')
   Use In F08sCur
EndIf

ENDPROC
PROCEDURE voldoc
*>
*> Recalcular acumulados de bultos en cabecera del documento.

Private _Selec
Private n_BulDoc, n_PalDoc, n_CajDoc, n_FraDoc, n_KgsDoc, n_VolDoc
Private n_PalAct, n_CajAct, n_FraAct
Private n_Peso, n_Volu
Private f_Asigna, f_Valor, f_Where
Local lStado

_Selec = "Select * From F26v" + _em + ;
         " Where F26vCodPro='" + This.ObjParm.DscCPro + "'" + ;
         " And   F26vTipDoc='" + This.ObjParm.DscTDoc + "'" + ;
         " And   F26vNumDoc='" + This.ObjParm.DscNDoc + "'"

*> El control de errores lo debe de realizar la funci�n que llama a este m�todo.
lStado = f3_SqlExec(_ASql, _Selec, 'BultosCur')
If _xier <= 0
   ErrVol = '55'
   This.VolErr('G', ErrVol)
   Return .F.
EndIf

Store 0 To n_BulDoc, n_PalDoc, n_CajDoc, n_FraDoc, n_KgsDoc, n_VolDoc
Store 0 To n_PalAct, n_CajAct, n_FraAct

Select BultosCur
Go Top

Do While!Eof()
   *> Bultos totales.
   n_BulDoc = n_BulDoc + 1

   Do Case
      *> Palets completos totales.
      Case F26vTipOri = 'P'
         n_PalDoc = n_PalDoc + 1
         n_PalAct = Iif(F26vEstBul>'0',  n_PalAct + 1, n_PalAct)

      *> Cajas completas totales.
      Case F26vTipOri = 'C'
         n_CajDoc = n_CajDoc + 1
         n_CajAct = Iif(F26vEstBul>'0',  n_CajAct + 1, n_CajAct)

      *> Cajas de fracciones totales.
      Case F26vTipOri = 'U'
         n_FraDoc = n_FraDoc + 1
         n_FraAct = Iif(F26vEstBul>'0',  n_FraAct + 1, n_FraAct)

      *> Cajas grupo totales. (Se tratan como cajas completas).
      Case F26vTipOri = 'G'
         n_CajDoc = n_CajDoc + 1
         n_CajAct = Iif(F26vEstBul>'0',  n_CajAct + 1, n_CajAct)
   EndCase

   *> Leer el contenido de este bulto para calcular el peso y el volumen.
   _Selec = "Select * " + ;
            " From  F26w" + _em + "," + ;
            "       F14c" + _em +  ;
            " Where F26wNumMac='" + BultosCur.F26vNumMac + "'" + ;
            " And   F14cNumMov=F26wNMovMP"

   *> El control de errores lo debe de realizar la funci�n que llama a este m�todo.
   lStado = f3_SqlExec(_ASql, _Selec, 'BultosCurDet')
   If _xier <= 0
      ErrVol = '55'
      This.VolErr('G', ErrVol)
      Return .F.
   EndIf

   *> Peso y volumen totales.
   Select BultosCurDet
   Go Top
   Do While !Eof()
      Store 0 To n_Peso, n_Volu
      Do PesVolAr In Ora_Ca00 With BultosCurDet.F14cCodPro, ;
                                   BultosCurDet.F14cCodArt, ;
                                   BultosCurDet.F14cCanFis, ;
                                   n_Peso, n_Volu
      n_KgsDoc = n_KgsDoc + n_Peso
      n_VolDoc = n_VolDoc + n_Volu

      Select BultosCurDet
      Skip
   EndDo

   Select BultosCur
   Skip
EndDo

If Used('BultosCur')
   Use In BultosCur
EndIf

If Used('BultosCurDet')
   Use In BultosCurDet
EndIf

*> Campos a actualizar en cabecera de documento.
f_Asigna = 'F24cNumBul, F24cPalTot, F24cPalAct, F24cCajTot, F24cCajAct, ' + ;
           'F24cFraTot, F24cFraAct, F24cTotKgs, F24cTotVol'

*> Valores a asignar.
f_Valor  = 'n_BulDoc, n_PalDoc, n_PalAct, n_CajDoc, n_CajAct, ' + ;
           'n_FraDoc, n_FraAct, n_KgsDoc, n_VolDoc'

*> Condici�n de b�squeda.
f_Where = "F24cCodPro = '" + This.ObjParm.DscCPro + "' And " + ;
          "F24cTipDoc = '" + This.ObjParm.DscTDoc + "' And " + ;
          "F24cNumDoc = '" + This.ObjParm.DscNDoc + "'"

*> El control de errores lo debe de realizar la funci�n que llama a este m�todo.
Sw = F3_UpdTun('F24c', , f_Asigna, f_Valor, , f_Where, 'N', 'N')
If Sw = .F.
   ErrVol = '55'
   This.VolErr('G', ErrVol)
   Return .F.
EndIf

ENDPROC
PROCEDURE volerr
*>
*> Tratamiento mensajes de error.-------------------------------------
LParameter _Operacion, ErrVol

ErrTxt=''

Do Case
   *> Generaci�n de volumetr�a.
   Case _Operacion = 'G'
      Do Case
         Case ErrVol = "55"
            ErrTxt = "Error en actualizaci�n de cabecera de documento" + Cr
         Case ErrVol = "51"
            ErrTxt = "El documento ya tiene bultos confirmados" + Cr
         Case ErrVol = "70"
            ErrTxt = "Error en creaci�n cabecera de bulto - Palets" + Cr
         Case ErrVol = "71"
            ErrTxt = "Error en creaci�n detalle de bulto - Palets" + Cr
         Case ErrVol = "72"
            ErrTxt = "Error en actualizaci�n movimiento pendiente - Palets" + Cr
         Case ErrVol = "73"
            ErrTxt = "Error en actualizaci�n detalle lista - Palets" + Cr
         Case ErrVol = "80"
            ErrTxt = "Error en creaci�n cabecera de bulto - Cajas Completas" + Cr
         Case ErrVol = "81"
            ErrTxt = "Error en creaci�n detalle de bulto - Cajas Completas" + Cr
         Case ErrVol = "82"
            ErrTxt = "Error en actualizaci�n movimiento pendiente - Cajas Completas" + Cr
         Case ErrVol = "83"
            ErrTxt = "Error en actualizaci�n detalle lista - Cajas Completas" + Cr
         Case ErrVol = "90"
            ErrTxt = "Error en creaci�n cabecera de bulto - Fracciones" + Cr
         Case ErrVol = "91"
            ErrTxt = "Error en creaci�n detalle de bulto - Fracciones" + Cr
         Case ErrVol = "92"
            ErrTxt = "Error en actualizaci�n movimiento pendiente - Fracciones" + Cr
         Case ErrVol = "93"
            ErrTxt = "Error en actualizaci�n detalle lista - Fracciones" + Cr
      EndCase

   *> Cancelaci�n de volumetr�a.
   Case _Operacion = 'C'
      Do Case
         Case ErrVol = "50"
            ErrTxt = "Error en borrado de detalle del bulto" + Cr
         Case ErrVol = "51"
            ErrTxt = "Error en borrado de cabecera del bulto" + Cr
         Case ErrVol = "52"
            ErrTxt = "Error en vaciado de n�mero de Mac del movimiento pendiente" + Cr
         Case ErrVol = "53"
            ErrTxt = "Error en vaciado de n�mero de Mac de detalle lista" + Cr
         Case ErrVol = "55"
            ErrTxt = "Error en actualizaci�n de cabecera de documento" + Cr
         Case ErrVol = "60"
            ErrTxt = "El documento tiene movimientos en lista" + Cr
         Case ErrVol = "65"
            ErrTxt = "El documento no tiene la volumetr�a generada" + Cr
         Case ErrVol = "66"
            ErrTxt = "Error al actualizar acumulados en documento" + Cr
         Case ErrVol = "67"
            ErrTxt = "Error al actualizar acumulados en documento" + Cr
         Case ErrVol = "70"
            ErrTxt = "No se encuentran los bultos de generados del documento" + Cr

   *> Llamadas externas a esta funci�n.
   Case _Operacion = 'X'
         Case ErrVol = "55"
            ErrTxt = "Error al recalcular el bulto" + Cr
      EndCase
EndCase

This.ObjParm.PtStVo = ErrVol
This.ObjParm.PtText = ErrTxt

ENDPROC
PROCEDURE volgen
***> Volumetr�a - Generaci�n de Mac's por documento.------------------

*> Observaciones:
*>    - Grabar F26vFlag1. AVC - 28.02.1999
*>    - Grabar MAC tambi�n el detalle lista. AVC - 16.08.1999
*>    - A�adir c�lculo de bultos de grupos. AVC - 09.02.2000
*>    - F26vTipOri = F14cOriRes. AVC _ 09.02.2000
*>    - En fracciones, separar fr�o, estupefacientes y ambiente. AVC - 25.07.2000
*>    - Actualizar Flag2 de F26v. AVC - 25.07.2000
*>

*> Procedimiento para dividir un MP en dos (desglose MACS).
Set Procedure To ProcMacs Additive

*> Control de errores en volumetr�a.----------------------------------
Private ErrVol, f_Where, _Selec
Private l_HayFri, l_HayDrg
Local b_Err

l_HayFri = .F.
l_HayDrg = .F.
ErrVol = "00"

*> Crear cursor con los movimientos de un documento.------------------
*******=CrtCursor('F14c', 'VGenF14c')
f_Where = "F14cCodPro='" + This.ObjParm.DscCPro + "' And " + ;
          "F14cTipDoc='" + This.ObjParm.DscTDoc + "' And " + ;
          "F14cNumDoc='" + This.ObjParm.DscNDoc + "' And " + ;
          "F08cCodPro=F14cCodPro And F08cCodArt=F14cCodArt"

f_Order = "F14cTipMac, F14cLinDoc, F14cCodArt, F14cNumLot, F14cFecCad"
f_Campos = "F14c" + _em + ".*, F08cTipPro, 'N' As TipBul"

l_Enc = F3_Sql(f_Campos, 'F14c,F08c', f_Where, f_Order, , 'VGenF14c')
If l_Enc = .F.
   Use In VGenF14c
   This.VolErr('G', ErrVol)
   Return
EndIf

w_TipMac = Space(4)
c_NumMac = Space(9)
Store 0 To n_VolMac, n_VolOcu
Store 0 To n_BulDoc, n_BulAct
Store 0 To n_PalDoc, n_CajDoc, n_FraDoc, n_KgsDoc, n_VolDoc

=CrtCursor('F26v', 'VGenF26v')
=CrtCursor('F26w', 'VGenF26w')

*> Ver si el documento ya tiene volumetr�a generada y, si la tiene,
*> coger el �ltimo n�mero de bulto generado.--------------------------
*> En realidad, este proceso no es necesario, pues la volumetr�a se genera
*> y se cancela para el documento completo.

*>
*> ANTES
*>
*f_Where = "F24cCodPro = '" + This.ObjParm.DscCPro + "' And F24cTipDoc = '" + ;
*      This.ObjParm.DscTDoc + "' And F24cNumDoc = '" + This.ObjParm.DscNDoc + "'"
*f_Asigna= "n_BulDoc=F24cNumBul, n_PalDoc=F24cPalTot, n_CajDoc=F24cCajTot, " + ;
*          "n_FraDoc=F24cFraTot, n_KgsDoc=F24cTotKgs, n_VolDoc=F24cTotVol"
*
*l_Enc = F3_SeekTun('F24c', f_Where, , f_Asigna)
*If l_Enc = .F.
*   n_BulDoc = 0
*EndIf

Store 0 To n_BulDoc, n_BulAct
Store 0 To n_PalDoc, n_CajDoc, n_FraDoc, n_KgsDoc, n_VolDoc

f_Where = "F26vCodPro = '" + This.ObjParm.DscCPro + "' And F26vTipDoc = '" + ;
      This.ObjParm.DscTDoc + "' And F26vNumDoc = '" + This.ObjParm.DscNDoc + "'"

*> Comprobar que no haya ning�n bulto ya procesado.
l_Enc = F3_SeekTun('F26v', f_Where)
If l_Enc
   Select F26v
   Locate For F26vEstBul > '0'
   If Found()
      b_Err = .T.
      ErrVol = "51"
   EndIf
EndIf

*>
*> Calcular el valor de Flag2 del bulto:
*>   'N' : Normal (Ambiente).
*>   'F' : Fr�o (Si This.ObjParm.CrtF=='S').
*>   'E' : Estupefacientes (Si This.ObjParm.CrtG=='S').

Select VGenF14c

If This.ObjParm.VolCtrF=='S'
   Replace All TipBul With 'F' For F08cTipPro==This.ObjParm.VolFriA
EndIf

If This.ObjParm.VolCtrG=='S'
   Replace All TipBul With 'E' For F08cTipPro==This.ObjParm.VolDrgA
EndIf

*>
*> Generar los bultos de fracciones de FRIO.
If This.ObjParm.VolCtrF=='S'
   =CrtCursor('VGenF14c', 'VPicF14c', 'C')
   Select VPicF14c
   Append From Dbf('VGenF14c') For F14cOriRes=='U' .And. F08cTipPro==This.ObjParm.VolFriA
   This.VolGenFri
   Use In VPicF14c
   l_HayFri = .T.
EndIf

*> Generar los bultos de fracciones de ESTUPEFACIENTES.
If This.ObjParm.VolCtrg=='S'
   =CrtCursor('VGenF14c', 'VPicF14c', 'C')
   Select VPicF14c
   Append From Dbf('VGenF14c') For F14cOriRes=='U' .And. F08cTipPro==This.ObjParm.VolDrgA
   This.VolGenDrg
   Use In VPicF14c
   l_HayDrg = .T.
EndIf

*> Generar los bultos de fracciones de AMBIENTE.
=CrtCursor('VGenF14c', 'VPicF14c', 'C')
Select VPicF14c
Append From Dbf('VGenF14c') For F14cOriRes=='U'

*> Borrar, si cal, los MPs de FRIO.
If l_HayFri
   Delete For F08cTipPro==This.ObjParm.VolFriA
EndIf

*> Borrar, si cal, los MPs de ESTUPEFACIENTES.
If l_HayDrg
   Delete For F08cTipPro==This.ObjParm.VolDrgA
EndIf

This.VolGenNor
Use In VPicF14c

******************************************************************

***> Tratar los bultos de caja completa.------------------------------
Select VGenF14c
Locate For F14cOriRes = 'C'
Do While Found()
   b_Err = .F.
   If !Empty(F14cNumMac) .Or. Empty(F14cTipMac) .Or. F14cTipMov <> '2000'
      Continue
      Loop
   EndIf

   *> Calcular volumen del movimiento.--------------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VGenF14c.F14cCodPro, ;
                                VGenF14c.F14cCodArt, ;
                                VGenF14c.F14cCanFis, ;
                                n_Peso, n_Volu
   n_KgsDoc = n_KgsDoc + n_Peso
   n_VolDoc = n_VolDoc + n_Volu

   n_NumCaj = Ceiling(F14cCanFis / (F14cUniPac * F14cPacCaj))
   c_TipOri = VGenF14c.F14cOriRes           && "C"

   c_PriMac = Space(9)

   *> Crear cabecera de bulto.----------------------------------------
   For n = 1 To n_NumCaj
      *> Obtener nuevo n�mero de Mac.---------------------------------
      c_NumMac = Ora_NewMac('N')
      If n = 1
         c_PriMac = c_NumMac
      EndIf

      n_BulDoc = n_BulDoc + 1
      n_CajDoc = n_CajDoc + 1

      Select VGenF26v
      Append Blank
      Replace F26vTipMac With VGenF14c->F14cTipMac, ;
              F26vNumMac With c_NumMac, ;
              F26vNumBul With PadL(AllTrim(Str(n_BulDoc)), 4, '0'), ;
              F26vCodPro With VGenF14c->F14cCodPro, ;
              F26vTipDoc With VGenF14c->F14cTipDoc, ;
              F26vNumDoc With VGenF14c->F14cNumDoc, ;
              F26vFecCre With Date(), ;
              F26vNumLis With VGenF14c->F14cNumLst, ;
              F26vTipOri With c_TipOri, ;
              F26vFlag1  With '0', ;
              F26vFlag2  With VGenF14c->TipBul, ;
              F26vEstBul With '0'

      l_Enc = F3_InsTun('F26v', 'VGenF26v','N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "80"
      EndIf

      *> Crear detalle de bulto.--------------------------------------
      Select VGenF26w
      Append Blank
      Replace F26wTipMac With VGenF14c->F14cTipMac, ;
              F26wNumMac With c_NumMac, ;
              F26wNumBul With PadL(AllTrim(Str(n_BulDoc)), 4, '0'), ;
              F26wNMovMP With VGenF14c->F14cNumMov, ;
              F26wNMovHM With Space(10), F26wEstLin With '0'

      l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "81"
       EndIf
   Next

   If b_Err = .F.
      *> Grabar n�mero de Mac al MP.----------------------------------
      c_NumMac = c_PriMac
      f_Where = "F14cNumMov = '" + VGenF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "82"
      EndIf

      *> Grabar n�mero de Mac en detalle lista.-----------------------
      c_NumMac = c_PriMac
      f_Where = "F26lNumMov = '" + VGenF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "83"
      EndIf
   EndIf

   *> Leer siguiente movimiento.--------------------------------------
   Select VGenF14c
   Continue
EndDo

***> Tratar los bultos de agrupaciones.-------------------------------
Select VGenF14c
Locate For F14cOriRes = 'G'
Do While Found()
   b_Err = .F.
   If !Empty(F14cNumMac) .Or. Empty(F14cTipMac) .Or. F14cTipMov <> '2000'
      Continue
      Loop
   EndIf

   *> Calcular volumen del movimiento.--------------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VGenF14c.F14cCodPro, ;
                                VGenF14c.F14cCodArt, ;
                                VGenF14c.F14cCanFis, ;
                                n_Peso, n_Volu
   n_KgsDoc = n_KgsDoc + n_Peso
   n_VolDoc = n_VolDoc + n_Volu

   n_NumCaj = Ceiling(F14cCanFis / (F14cUniPac * F14cPacCaj))
   c_TipOri = VGenF14c.F14cOriRes

   c_PriMac = Space(9)

   *> Crear cabecera de bulto.----------------------------------------
   For n = 1 To n_NumCaj
      *> Obtener nuevo n�mero de Mac.---------------------------------
      c_NumMac = Ora_NewMac('N')
      If n = 1
         c_PriMac = c_NumMac
      EndIf
      n_BulDoc = n_BulDoc + 1
      n_CajDoc = n_CajDoc + 1

      Select VGenF26v
      Append Blank
      Replace F26vTipMac With VGenF14c->F14cTipMac, ;
              F26vNumMac With c_NumMac, ;
              F26vNumBul With PadL(AllTrim(Str(n_BulDoc)), 4, '0'), ;
              F26vCodPro With VGenF14c->F14cCodPro, ;
              F26vTipDoc With VGenF14c->F14cTipDoc, ;
              F26vNumDoc With VGenF14c->F14cNumDoc, ;
              F26vFecCre With Date(), ;
              F26vNumLis With VGenF14c->F14cNumLst, ;
              F26vTipOri With c_TipOri, ;
              F26vFlag1  With '0', ;
              F26vFlag2  With VGenF14c->TipBul, ;
              F26vEstBul With '0'

      l_Enc = F3_InsTun('F26v', 'VGenF26v','N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "80"
      EndIf

      *> Crear detalle de bulto.--------------------------------------
      Select VGenF26w
      Append Blank
      Replace F26wTipMac With VGenF14c->F14cTipMac, ;
              F26wNumMac With c_NumMac, ;
              F26wNumBul With PadL(AllTrim(Str(n_BulDoc)), 4, '0'), ;
              F26wNMovMP With VGenF14c->F14cNumMov, ;
              F26wNMovHM With Space(10), ;
              F26wEstLin With '0'

      l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "81"
       EndIf
   Next

   If b_Err = .F.
      *> Grabar n�mero de Mac al MP.----------------------------------
      c_NumMac = c_PriMac
      f_Where = "F14cNumMov = '" + VGenF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "82"
      EndIf

      *> Grabar n�mero de Mac en detalle lista.-----------------------
      c_NumMac = c_PriMac
      f_Where = "F26lNumMov = '" + VGenF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "83"
      EndIf
   EndIf

   *> Leer siguiente movimiento.--------------------------------------
   Select VGenF14c
   Continue
EndDo

***> Tratar los bultos de palet.--------------------------------------
Select VGenF14c
Locate For F14cOriRes = 'P'
Do While Found()
   b_Err = .F.
   If !Empty(F14cNumMac) .Or. Empty(F14cTipMac) .Or. F14cTipMov <> '2000'
      Continue
      Loop
   EndIf

   *> Calcular volumen del movimiento.--------------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VGenF14c.F14cCodPro, ;
                                VGenF14c.F14cCodArt, ;
                                VGenF14c.F14cCanFis, ;
                                n_Peso, n_Volu
   n_KgsDoc = n_KgsDoc + n_Peso
   n_VolDoc = n_VolDoc + n_Volu

   n_NumPal = 1
   c_TipOri = VGenF14c.F14cOriRes              && "P"
   c_PriMac = Space(9)

   *> Crear cabecera de bulto.----------------------------------------
   For n = 1 To n_NumPal
      *> Obtener nuevo n�mero de Mac.---------------------------------
      c_NumMac = Ora_NewMac('N')
      If n = 1
         c_PriMac = c_NumMac
      EndIf
      n_BulDoc = n_BulDoc + 1
      n_PalDoc = n_PalDoc + 1

      *> Crear cabecera de bulto.-------------------------------------
      Select VGenF26v
      Append Blank
      Replace F26vTipMac With VGenF14c->F14cTipMac, ;
              F26vNumMac With c_NumMac, ;
              F26vNumBul With PadL(AllTrim(Str(n_BulDoc)), 4, '0'), ;
              F26vCodPro With VGenF14c->F14cCodPro, ;
              F26vTipDoc With VGenF14c->F14cTipDoc, ;
              F26vNumDoc With VGenF14c->F14cNumDoc, ;
              F26vFecCre With Date(), ;
              F26vNumLis With VGenF14c->F14cNumLst, ;
              F26vTipOri With c_TipOri, ;
              F26vFlag1  With '0', ;
              F26vFlag2  With VGenF14c->TipBul, ;
              F26vEstBul With '0'

      l_Enc = F3_InsTun('F26v', 'VGenF26v','N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "70"
      EndIf

      *> Crear detalle de bulto.--------------------------------------
      Select VGenF26w
      Append Blank
      Replace F26wTipMac With VGenF14c->F14cTipMac, ;
              F26wNumMac With c_NumMac, ;
              F26wNumBul With PadL(AllTrim(Str(n_BulDoc)), 4, '0'), ;
              F26wNMovMP With VGenF14c->F14cNumMov, ;
              F26wNMovHM With Space(10), ;
              F26wEstLin With '0'

      l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "71"
      EndIf
   Next

   If b_Err = .F.
      *> Grabar n�mero de Mac al MP.----------------------------------
      c_NumMac = c_PriMac
      f_Where = "F14cNumMov = '" + VGenF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "72"
      EndIf

      *> Grabar n�mero de Mac en detalle lista.-----------------------
      c_NumMac = c_PriMac
      f_Where = "F26lNumMov = '" + VGenF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "73"
      EndIf
   EndIf

   *> Leer siguiente movimiento.--------------------------------------
   Select VGenF14c
   Continue
EndDo

*> Actualizar datos bultos en cabecera de documento.
If n_BulDoc > 0
   This.VolDoc
EndIf

*> Tratamiento mensajes de error.-------------------------------------
This.VolErr('G', ErrVol)

Use In VGenF14c
Use In VGenF26v
Use In VGenF26w

*> Liberar procedimientos.
Release Procedure ProcMacs

ENDPROC
PROCEDURE volgendrg
*>
*> Generar los bultos de fracciones - ESTUPEFACIENTES.
*>
*> Recibe cursor VPicF14c, con los Mps de aquellos productos de estupefacientes.
*> Este cursor se genera en This.VolGen.
*>

*> Crear cursor de bultos para reparto de picking.--------------------
Create Cursor Macs (MaTipMac Char(4), ;
                    MaTipPro Char(4), ;
                    MaNumLst Char(6), ;
                    MaNumMac Char(9), ;
                    MaVolTot N(9, 3), ;
                    MaVolLib N(9, 3), ;
                    MaNumBul N(5, 0))

Select VPicF14c
Go Top
Do While !Eof()
   b_Err = .F.
   If !Empty(F14cNumMac) .Or. Empty(F14cTipMac) .Or. SubStr(F14cTipMov, 1, 1) # '2'
      Skip
      Loop
   EndIf

   *> Desglosar el MP actual en MACs, seg�n ficha art�culos/soportes.
   *> La F14cCanFis restante, si hay, pasa al proceso normal de volumetr�a.
   This.VolDlg

   *> Calcular volumen del movimiento.--------------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VPicF14c.F14cCodPro, ;
                                VPicF14c.F14cCodArt, ;
                                VPicF14c.F14cCanFis, ;
                                n_Peso, n_Volu
   n_KgsDoc = n_KgsDoc + n_Peso
   n_VolDoc = n_VolDoc + n_Volu

   *> Buscar primer Mac con volumen suficiente.-----------------------
   Select Macs
   Locate For MaVolLib >= n_Volu .And. MaTipMac == VPicF14c.F14cTipMac
   If !Eof()
      *> Si hay alguno, meterlo en ese Mac.---------------------------
      c_NumMac = Macs.MaNumMac
      n_BulAct = Macs.MaNumBul
      Replace Macs.MaVolLib With Macs.MaVolLib - n_Volu
   Else
      *> Si no hay ninguno, crear un Mac nuevo.-----------------------
      This.VolNewMac
   EndIf

   *> Crear detalle de bulto.-----------------------------------------
   Select VGenF26w
   Append Blank
   Replace F26wTipMac With VPicF14c->F14cTipMac, ;
           F26wNumMac With c_NumMac, ;
           F26wNumBul With PadL(AllTrim(Str(n_BulAct)), 4, '0'), ;
           F26wNMovMP With VPicF14c->F14cNumMov, ;
           F26wNMovHM With Space(10), F26wEstLin With '0'

   l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
   If l_Enc = .F.
      b_Err = .T.
      ErrVol = "91"
   EndIf

   If b_Err = .F.
      *> Grabar n�mero de Mac al MP.----------------------------------
      f_Where = "F14cNumMov = '" + VPicF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "92"
      EndIf

      *> Grabar n�mero de Mac en detalle lista.-----------------------
      f_Where = "F26lNumMov = '" + VPicF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "93"
      EndIf
   EndIf

   *> Leer siguiente movimiento.--------------------------------------
   w_TipMac = F14cTipMac
   Select VPicF14c
   Skip
EndDo

*>
Use In Macs

ENDPROC
PROCEDURE volgenfri
*>
*> Generar los bultos de fracciones - FRIO.
*>
*> Recibe cursor VPicF14c, con los Mps de aquellos productos de fr�o.
*> Este cursor se genera en This.VolGen.
*>

*> Crear cursor de bultos para reparto de picking.--------------------
Create Cursor Macs (MaTipMac Char(4), ;
                    MaTipPro Char(4), ;
                    MaNumLst Char(6), ;
                    MaNumMac Char(9), ;
                    MaVolTot N(9, 3), ;
                    MaVolLib N(9, 3), ;
                    MaNumBul N(5, 0))

Select VPicF14c
Go Top
Do While !Eof()
   b_Err = .F.
   If !Empty(F14cNumMac) .Or. Empty(F14cTipMac) .Or. SubStr(F14cTipMov, 1, 1) # '2'
      Skip
      Loop
   EndIf

   *> Desglosar el MP actual en MACs, seg�n ficha art�culos/soportes.
   *> La F14cCanFis restante, si hay, pasa al proceso normal de volumetr�a.
   This.VolDlg

   *> Calcular volumen del movimiento.--------------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VPicF14c.F14cCodPro, ;
                                VPicF14c.F14cCodArt, ;
                                VPicF14c.F14cCanFis, ;
                                n_Peso, n_Volu
   n_KgsDoc = n_KgsDoc + n_Peso
   n_VolDoc = n_VolDoc + n_Volu

   *> Buscar primer Mac con volumen suficiente.-----------------------
   Select Macs
   Locate For MaVolLib >= n_Volu .And. MaTipMac == VPicF14c.F14cTipMac
   If !Eof()
      *> Si hay alguno, meterlo en ese Mac.---------------------------
      c_NumMac = Macs.MaNumMac
      n_BulAct = Macs.MaNumBul
      Replace Macs.MaVolLib With Macs.MaVolLib - n_Volu
   Else
      *> Si no hay ninguno, crear un Mac nuevo.-----------------------
      This.VolNewMac
   EndIf

   *> Crear detalle de bulto.-----------------------------------------
   Select VGenF26w
   Append Blank
   Replace F26wTipMac With VPicF14c->F14cTipMac, ;
           F26wNumMac With c_NumMac, ;
           F26wNumBul With PadL(AllTrim(Str(n_BulAct)), 4, '0'), ;
           F26wNMovMP With VPicF14c->F14cNumMov, ;
           F26wNMovHM With Space(10), F26wEstLin With '0'

   l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
   If l_Enc = .F.
      b_Err = .T.
      ErrVol = "91"
   EndIf

   If b_Err = .F.
      *> Grabar n�mero de Mac al MP.----------------------------------
      f_Where = "F14cNumMov = '" + VPicF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "92"
      EndIf

      *> Grabar n�mero de Mac en detalle lista.-----------------------
      f_Where = "F26lNumMov = '" + VPicF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "93"
      EndIf
   EndIf

   *> Leer siguiente movimiento.--------------------------------------
   w_TipMac = F14cTipMac
   Select VPicF14c
   Skip
EndDo

*>
Use In Macs

ENDPROC
PROCEDURE volgennor
*>
*> Generar los bultos de fracciones - AMBIENTE.
*>
*> Recibe cursor VPicF14c, con los Mps de aquellos productos de ambiente.
*> Este cursor se genera en This.VolGen.
*>

*> Crear cursor de bultos para reparto de picking.--------------------
Create Cursor Macs (MaTipMac Char(4), ;
                    MaTipPro Char(4), ;
                    MaNumLst Char(6), ;
                    MaNumMac Char(9), ;
                    MaVolTot N(9, 3), ;
                    MaVolLib N(9, 3), ;
                    MaNumBul N(5, 0))

Select VPicF14c
Go Top
Do While !Eof()
   b_Err = .F.
   If !Empty(F14cNumMac) .Or. Empty(F14cTipMac) .Or. SubStr(F14cTipMov, 1, 1) # '2'
      Skip
      Loop
   EndIf

   *> Desglosar el MP actual en MACs, seg�n ficha art�culos/soportes.
   *> La F14cCanFis restante, si hay, pasa al proceso normal de volumetr�a.
   This.VolDlg

   *> Calcular volumen del movimiento.--------------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With VPicF14c.F14cCodPro, ;
                                VPicF14c.F14cCodArt, ;
                                VPicF14c.F14cCanFis, ;
                                n_Peso, n_Volu
   *** n_KgsDoc = n_KgsDoc + n_Peso
   *** n_VolDoc = n_VolDoc + n_Volu

   *> Buscar primer MAC con volumen suficiente.-----------------------
   Select Macs
   Locate For MaVolLib >= n_Volu .And. MaTipMac == VPicF14c.F14cTipMac
   If !Eof()
      *> Si hay alguno, meterlo en ese MAC.---------------------------
      c_NumMac = Macs.MaNumMac
      n_BulAct = Macs.MaNumBul
      Replace Macs.MaVolLib With Macs.MaVolLib - n_Volu
   Else
      *> Si no hay ninguno, crear un MAC nuevo.-----------------------
      This.VolNewMac
   EndIf

   *> Crear detalle de bulto.-----------------------------------------
   Select VGenF26w
   Append Blank
   Replace F26wTipMac With VPicF14c->F14cTipMac, ;
           F26wNumMac With c_NumMac, ;
           F26wNumBul With PadL(AllTrim(Str(n_BulAct)), 4, '0'), ;
           F26wNMovMP With VPicF14c->F14cNumMov, ;
           F26wNMovHM With Space(10), F26wEstLin With '0'

   l_Enc = F3_InsTun('F26w', 'VGenF26w','N')
   If l_Enc = .F.
      b_Err = .T.
      ErrVol = "91"
   EndIf

   If b_Err = .F.
      *> Grabar n�mero de MAC al MP.----------------------------------
      f_Where = "F14cNumMov = '" + VPicF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F14c', '', 'F14cNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "92"
      EndIf

      *> Grabar n�mero de MAC en detalle lista.-----------------------
      f_Where = "F26lNumMov = '" + VPicF14c.F14cNumMov + "'"
      l_Enc = F3_UpdTun('F26l', '', 'F26lNumMac', 'c_NumMac', , f_Where, 'N', 'N')
      If l_Enc = .F.
         b_Err = .T.
         ErrVol = "93"
      EndIf
   EndIf

   *> Leer siguiente movimiento.--------------------------------------
   w_TipMac = F14cTipMac
   Select VPicF14c
   Skip
EndDo

*>
Use In Macs

ENDPROC
PROCEDURE volnewmac
*>
*> Crear nuevo MAC.

n_BulDoc = n_BulDoc + 1
n_BulAct = n_BulDoc
n_FraDoc = n_FraDoc + 1

*> Obtener nuevo n�mero de MAC y Volumen �til del MAC.----------
c_NumMac = Ora_NewMac('N')
m.F46cCodigo = VPicF14c.F14cTipMac
=F3_Seek('F46c', , , ;
         'n_VolMac = ((F46cAltSop * F46cAncSop * F46cPrfSop) / 1000)' + ;
         ' * F46cPctUti / 100')

*> Generar MAC en fichero de reparto.---------------------------
Select Macs
Append Blank
Replace MaTipMac With VPicF14c.F14cTipMac, ;
        MaTipPro With VPicF14c.F08cTipPro, ;
        MaNumMac With c_NumMac, ;
        MaNumLst With VPicF14c.F14cNumLst, ;
        MaVolTot With n_VolMac, ;
        MaVolLib With n_VolMac - n_Volu, ;
        MaNumBul With n_BulAct

*> Crear cabecera de MAC.---------------------------------------
c_TipOri = VPicF14c.F14cOriRes      &&       "U"
Select VGenF26v
Append Blank
Replace F26vTipMac With VPicF14c->F14cTipMac, ;
        F26vNumMac With c_NumMac, ;
        F26vNumBul With PadL(AllTrim(Str(n_BulAct)), 4, '0'), ;
        F26vCodPro With VPicF14c->F14cCodPro, ;
        F26vTipDoc With VPicF14c->F14cTipDoc, ;
        F26vNumDoc With VPicF14c->F14cNumDoc, ;
        F26vFecCre With Date(),     ;
        F26vNumLis With VPicF14c->F14cNumLst, ;
        F26vTipOri With c_TipOri, ;
        F26vFlag1  With '0', ;
        F26vFlag2  With VPicF14c->TipBul, ;
        F26vEstBul With '0'

l_Enc = F3_InsTun('F26v', 'VGenF26v','N')
If l_Enc = .F.
   b_Err = .T.
   ErrVol = "90"
EndIf

ENDPROC
PROCEDURE initlocals

*> Inicializar propiedades que se utilizan como variables locales y/o de trabajo.
With This
	.Orden = ""			&& Orden reserva
	.WRowId = ""		&& RowId ocupaci�n
	.ActzObj = ""		&& Objeto clase actualizaci�n
	.ActzPrm = ""		&& Par�metros clase actualizaci�n
	.ListObj = ""		&& Objeto clase listas

	. CtrCad = ""		&& Flag control caducidad del art�culo
EndWith

Return

ENDPROC
PROCEDURE cancelarreserva

*> Cancelar los MPs de preparaci�n de un documento completo.
*> Actualiza el estado del documento de salida.
*>
*> Recibe: CodPro ----> Propietario.
*>         TipDoc ----> Tipo de documento.
*>         NumDoc ----> N� de documento.
*>         LinDoc ----> L�nea documento. Si .F., cancelar TODO el documento.

Parameters cCodPro, cTipDoc, cNumDoc, cLinDoc

Private _Where
Local lStado

_Where = "F14cCodPro='" + cCodPro + "' And F14cTipDoc='" + cTipDoc + "' And F14cNumDoc='" + cNumDoc + "' "
If Type('cLinDoc')=='C'
   _Where = _Where + " And F14cLinDoc=" + cLinDoc
EndIf

lStado = f3_sql('*', 'F14c', _Where, '', '', 'F14cCurUPD')
Select F14cCurUPD
Go Top
Do While !Eof()
   =This.DesReserva(F14cCurUPD.F14cNumMov)
   Select F14cCurUPD
   Skip
EndDo

*> Actualizar el documento de salida.
lStado = This.UpdDocRsv(cCodPro, cTipDoc, cNumDoc)

Use In (Select("F14cCurUPD"))
Return

ENDPROC
PROCEDURE desreserva

*> Cancelaci�n de reserva a partir de un movimiento pendiente.
*> Este m�todo NO ACTUALIZA EL ESTADO DEL DOCUMENTO.
*> El MP NO debe estar en lista de trabajo.

*> Recibe: N� de MP a desreservar.

*> Modificaciones:
*>		AVC - 19.10.2006 : Borrar el MP aunque haya incidencia en ocupaci�n, de otro modo no se puede borrar nunca el MP.

Parameters c_NumMov

Private c_LinDoc, oFnc, oPrm, oF14c
Local _Ok

	*> Para la funci�n de actualizaci�n.
	oFnc = CreateObject("OraFncActz")
	oPrm = CreateObject("OraprmActz")
	oFnc.ObjParm = oPrm

	*> Comprobar si existe el movimiento pendiente.
	m.F14cNumMov = c_NumMov
	_Ok = f3_seek("F14c")
	If !_Ok
		This.UsrError = "No se encontr� movimiento [" + c_NumMov + "] en MPs" + cr
		Return .F.
	EndIf

	*> Comprobar que el MP NO est� en listas.
	m.F26lNumMov = c_NumMov
	_Ok = f3_seek("F26l")
	If _Ok
		This.UsrError = "El movimiento [" + c_NumMov + "] est� en listas " + cr
		Return .F.
	EndIf

	*> Guardar datos del MP.
	Select F14c
	Scatter Name oF14c

	*> Desreservar cantidad movimiento en ocupaci�n.
	With oFnc
		With .ObjParm
		    .Inicializar					&& Inicializar propiedades.

			.PuFlag = "N"					&& Actualizar mapa.
			.PsFlag = "N"					&& Actualizar stock.

			.PoRowId = oF14c.F14cIdOcup		&& ID ocupaci�n.
			.PoCPro  = oF14c.F14cCodPro		&& Propietario.
			.PoCArt  = oF14c.F14cCodArt		&& Art�culo.
			.PoCFis  = 0					&& Cantidad a descontar (f�sica).
			.PoCRes  = oF14c.F14cCanFis		&& Cantidad a descontar (reservada).
			.PoCUbi  = oF14c.F14cUbiOri		&& Ubicaci�n.
			.PoFlag = "S"					&& Actualizar ocupaci�n.

			.PtAcc = "06"					&& Salida reservada.
		EndWith
		.Ejecutar
	EndWith

	If oFnc.ObjParm.PWCRtn > "49"
		This.UsrError = "No se pudo actualizar ocupaci�n [" + oF14c.F14cIdOcup + "] en MPs" + cr
		*>>>>>>>>>>>>>>Return .F.
	EndIf

	*> Borrar el movimiento pendiente.
	m.F14cNumMov = oF14c.F14cNumMov
	_Ok = f3_baja("F14c")
	If !_Ok
		This.UsrError = "No se pudo borrar movimiento [" + c_NumMov + "] en MPs" + cr
		*>>>>>>>>>>>>>>Return .F.
	EndIf

Return

ENDPROC
PROCEDURE upddocrsv

*> Actualizar la cantidad reservada de un documento de salida.
*> El documento debe de estar pendiente de confirmaci�n de carga.
*> Parte de la base de que se confirma la carga de todo el documento.

*> Par�metros: Propietario, Tipo documento, N� documento.

*> Historial de modificaciones:
*>	- Modificar la actualizaci�n del estado de la l�nea pedido. AVC - 13.06.2007
*>	- Adaptar para expediciones parciales. AVC - 13.06.2007
*>	- Marcar documento como expedido si est� totalmente servido. AVC - 14.06.2007
*>	- Validar expediciones parciales. AVC - 28.11.2007

Parameters cCodPro, cTipDoc, cNumDoc

Private cWhere, cCampos, cValores, nCanRes, nCanEnv, cEstado
Local lStado, oF14c, oF20c, nStt, nStt0, nStt1, nStt2, cParcial, cEstadoCab

*>Si el documento existe en F24k001, no se hace nada.
*> Leer el documento.
Use In (Select("cBorr"))
_Sentencia="Select * From F24k001 Where F24kNumDoc='" + cNumDoc + "'"
=SqlExec(_Asql,_Sentencia,'cBorr')

Select cBorr
Go Top
If !Eof()
	*> No existe el documento.
	Return
EndIf

Use In (Select("cBorr"))

*> Leer el documento.
cWhere = "F24cCodPro='" + cCodPro + "' And " + ;
         "F24cTipDoc='" + cTipDoc + "' And " + ;
         "F24cNumDoc='" + cNumDoc + "'"

lStado = f3_sql("*", "F24c", cWhere, , , "F24cCursor")
If !lStado
	*> No existe el documento.
	Return
EndIf

Select F24cCursor
Go Top
If F24cFlgEst>='5'
	*> Ya est� expedido.
	Return
EndIf

cEstadoCab = F24cFlgEst

*> Par�metros para validar expediciones parciales. A nivel de propietario.
m.F25cCodPro = cCodPro
m.F25cTipPro = ""
m.F25cCodArt = ""
If f3_seek("F25c")
	Select F25c
	Go Top
	cParcial = F25cConAut
Else
	cParcial = 'N'
EndIf

Store 0 To nCanRes, nCanEnv

*> Cargar los MPs de preparaci�n.
cWhere = "F14cCodPro='" + cCodPro + "' And " + ;
         "F14cTipDoc='" + cTipDoc + "' And " + ;
         "F14cNumDoc='" + cNumDoc + "' And " + ;
         "F14cTipMov Between '2000' And '2999'"

lStado = f3_sql("*", "F14c", cWhere, , , "F14cCursor")

*> Cargar los movimientos ya expedidos.
cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
         "F20cTipDoc='" + cTipDoc + "' And " + ;
         "F20cNumDoc='" + cNumDoc + "' And " + ;
         "F20cTipMov Between '2000' And '2999'"

lStado = f3_sql("*", "F20c", cWhere, , , "F20cCursor")

*> Poner a cero la cantidad reservada en el detalle documento.
cWhere = "F24lCodPro='" + cCodPro + "' And " + ;
         "F24lTipDoc='" + cTipdoc + "' And " + ;
         "F24lNumDoc='" + cNumDoc + "'"

cCampos = "F24lCanRes,F24lCanEnv,F24lFlgEst"
cValores = "0,0,'0'"

lStado = f3_updtun('F24l', , cCampos, cValores, , cWhere)
If !lStado
	*> Error.
	This.UsrError = "Error inicializando reserva en detalle"
	Use In (Select("F14cCursor"))
	Use In (Select("F20cCursor"))
	Return
EndIf

Use In (Select("F24lCursor"))

*> Actualizar la cantidad reservada (por MPs).
Select F14cCursor
Go Top
Do While !Eof()
	Scatter Name oF14c

	cWhere = "F24lCodPro='" + cCodPro + "' And " + ;
	         "F24lTipDoc='" + cTipDoc + "' And " + ;
	         "F24lNumDoc='" + cNumDoc + "' And " + ;
	         "F24lLinDoc='" + PadL(AllTrim(Str(oF14c.F14cLinDoc, 4, 0)), 4, '0') + "'"

	lStado = f3_sql("*", "F24l", cWhere, , , "F24lCursor")
	If lStado
		Select F24lCursor
		Go Top

		nCanRes = F24lCanRes + oF14c.F14cCanFis
        cEstado = Iif(nCanRes>=F24lCanDoc, '2', Iif(nCanRes==0, '0', '1'))

		cCampos = "F24lCanRes,F24lFlgEst"
		cValores= "nCanRes,cEstado"

		lStado = f3_updtun('F24l', , cCampos, cValores, , cWhere)
	EndIf

	Select F14cCursor
	Skip
EndDo

Use In (Select("F24lCursor"))

*> Actualizar la cantidad reservada y expedida.
Select F20cCursor
Go Top
Do While !Eof()
	Scatter Name oF20c

	cWhere = "F24lCodPro='" + cCodPro + "' And " + ;
	         "F24lTipDoc='" + cTipDoc + "' And " + ;
	         "F24lNumDoc='" + cNumDoc + "' And " + ;
	         "F24lLinDoc='" + PadL(AllTrim(Str(oF20c.F20cLinDoc, 4, 0)), 4, '0') + "'"

	lStado = f3_sql("*", "F24l", cWhere, , , "F24lCursor")
	If lStado
		Select F24lCursor
		Go Top

		nCanRes = F24lCanRes + oF20c.F20cCanFis
		nCanEnv = F24lCanEnv + oF20c.F20cCanFis
        cEstado = Iif(nCanEnv>=F24lCanDoc, '3', Iif(nCanRes>=F24lCanDoc, '2', Iif(nCanRes==0, '0', '1')))

		cCampos = "F24lCanRes,F24lCanEnv,F24lFlgEst"
		cValores= "nCanRes,nCanEnv,cEstado"

		lStado = f3_updtun('F24l', , cCampos, cValores, , cWhere)
	EndIf

	Select F20cCursor
	Skip
EndDo

*> Actualizar el estado de la cabecera del documento.
cWhere = "F24lCodPro='" + cCodPro + "' And " + ;
         "F24lTipDoc='" + ctipDoc + "' And " + ;
         "F24lNumDoc='" + cNumDoc + "' And " + ;
         "F24lFlgEst<='2'"

lStado = f3_sql("*", "F24l", cWhere, , , "F24lCursor")
Select F24lCursor
Count To nStt
Count For F24lFlgEst=='0' To nStt0
Count For F24lFlgEst=='1' To nStt1
Count For F24lFlgEst=='2' To nStt2

Do Case
	*> Todo expedido.
	Case !lStado
		cEstado = '6'

	*> Todo preparado y sin expediciones parciales.
	Case cEstadoCab=='4' .And. cParcial=='N'
		cEstado = '6'

	*> Sin preparar.
	Case nStt0 > 0 .And. (nStt1 + nStt2)==0
		cEstado = '0'

	*> Parcialmente preparado.
	Case nStt0 > 0 .Or. nStt1 > 0
		cEstado = '2'

	*> Totalmente preparado.
	Otherwise
		cEstado = '3'
EndCase

*cEstado = Iif(!lStado, '3', Iif(Empty(nStt), '0', '2'))
cCampos = "F24cFlgEst"
cValores= "cEstado"

cWhere = "F24cCodPro='" + cCodPro + "' And " + ;
         "F24cTipDoc='" + cTipDoc + "' And " + ;
         "F24cNumDoc='" + cNumDoc + "'"

lStado = f3_updtun('F24c', , cCampos, cValores, , cWhere)

Use In (Select("F14cCursor"))
Use In (Select("F20cCursor"))
Use In (Select("F24lCursor"))

Return

ENDPROC
PROCEDURE listobj_access

*> Crear objeto de la clase para el tratamiento de listas
If Type('This.ListObj') # 'O'
	This.ListObj = CreateObject('OraFncList')
EndIf

Return This.ListObj

ENDPROC
PROCEDURE validarmuelle

*> Validar la ubicaci�n de expedici�n.
*> M�todo privado de la clase.

*> Recibe:
*>	- cUbicaci�n, ubicaci�n de expedici�n a validar.

*> Devuelve:
*>	- lStado (.T. / .F.), resultado de la operaci�n.

*> Historial de modificaciones:
*> 28.11.2007 (AVC) Adaptar a funcionamiento de partes de montaje.

Parameters cUbicacion

Local lStado
Private cCdoUbi

lStado = .F.
cCodUbi = cUbicacion

If f3_seek("F10c", "[m.F10cCodUbi=cUbicacion]")
	Select F10c
	lStado = (F10cPickSN=="M" .Or. F10cPickSN=="E" .Or. F10cPickSN=='L' .Or. F10cPickSN=='I') .And. F10cEstGen<>"B" .And. F10cEstEnt=="N"
EndIf

If !lStado
	=This.ListObj.AsignarMuelle()
EndIf

Return lStado

ENDPROC
PROCEDURE reserva_crossdocking

*> Reserva de material de un documento.
*> Recibe: Pedido: Pedido a reservar.
*> Opcional: L�nea de pedido a reservar

Parameters cCodPro, cTipDoc, cNumDoc, cLinDoc, cNumPal, cCanFis

Private cCodPro, cTipDoc, cNumDoc, cLinDoc, cNumPal, cCanFis, cTexto
Private _Field, _Sentencia, _Where, _Order, oF24cl
Local lStado, _Ok, oArti, oF24c
Local _CanRes, _CanResTot, _CRest, _CanMinRes
Local nRecNo

	*> Inicializar propiedades privadas de la clase.
	=This.InitLocals()
	
	*> Leer las l�neas de detalle.
	_Where =               "F24lCodPro='" + cCodPro + "'"
	_Where = _Where + " And F24lTipDoc='" + cTipDoc + "'"
	_Where = _Where + " And F24lNumDoc='" + cNumDoc + "'"
	_Where = _Where + " And F24lLinDoc= " + Str(cLinDoc)
	_Where = _Where + " And F24lCanDoc > (F24lCanRes + F24lCanEnv)"
	_Where = _Where + " And F24cCodPro=F24lCodPro And F24cTipDoc=F24lTipDoc And F24cNumDoc=F24lNumDoc"
	_Where = _Where + " And F08cCodPro=F24lCodPro And F08cCodArt=F24lCodArt"
	
	_Order = "F24lCodPro, F24lCodArt, F24lNumLot DESC "
	
    If !f3_sql('*', 'F24l,F24c,F08c', _Where, _Order, , 'c_F24cl')
		This.UsrError = 'El documento no tiene l�neas pendientes de reservar'
		Return lStado
    EndIf

	Select c_F24cl
	
	This.ActzPrm = CreateObject("OraPrmActz")        && Par�metros.
	This.ActzObj = CreateObject("OraFncActz")        && Procedimientos.
	This.ActzObj.ObjParm = This.ActzPrm
		
	*> Comenzamos la reserva real.-----------------------------------------
	Go Top
	Scatter Name oF24cl
	
	*> Guardar propiedades de detalle.
	With This.ObjParm
		.DsdCPro = oF24cl.F24lCodPro
		.DsdTDoc = oF24cl.F24lTipDoc
		.DsdNDoc = oF24cl.F24lNumDoc
		.DsdLDoc = oF24cl.F24lLinDoc
		.DsdCArt = oF24cl.F24lCodArt
		.DsdTPro = oF24cl.F08cTipPro		&& Guardo el tipo de producto de la referencia a reservar.
	EndWith
	      		
	This.CtrCad = oF24cl.F08cCaduca
	
	If !This.GetRsvParm()
		This.UsrError = 'No se encontraron los par�metros de reserva'
		Return .F.
	EndIf
	
	*> Orden de la reserva (Lifo,Fifo etc..)
	This.ObtenerOrden()

	_ResTot = 0	
	_CResT  = IIf((oF24cl.F24lCanDoc - (oF24cl.F24lCanRes + oF24cl.F24lCanEnv)) < cCanFis, (oF24cl.F24lCanDoc - (oF24cl.F24lCanRes + oF24cl.F24lCanEnv)), cCanFis)
	
	*> Buscamos la ocupaci�n.
	_Where =                "F16cCodPro = '" + c_f24cl->F24lCodPro + "'"
	_Where = _Where  + " And F16cCodArt = '" + c_f24cl->F24lCodArt + "'"
	_Where = _Where  + " And F16cSitStk = '" + c_f24cl->F24lSitStk + "'"
	_Where = _Where  + " And F16cNumPal = '" + cNumPal             + "'"
	_Where = _Where  + " And F10cCodUbi = F16cCodUbi And F10cEstSal <> 'S'"
	
	_Order = This.Orden + ", F10cPickSN"
	
	_Field = "F16c" + _em + ".*, F10c" + _em + ".*, F16cCanFis-F16cCanRes As F16cCanDis"
	
	If f3_sql(_Field, 'F16c,F10c', _Where, _Order, , 'OcuF16c')
	
		Select OcuF16c
		
		_CanResTot = IIF((F16cCanfis - F16cCanres) < _CResT, (F16cCanfis - F16cCanres), _CResT)
		_ResTot    = _ResTot + _CanResTot
		_CResT     = _CResT  - _CanResTot
		
		*> Generamos el movimiento.--------------------------------------
		_Ok = This.GenMov(_CanResTot, 'P')
		If _Ok = .F.
			This.UsrError="Error generando movimientos pendientes (CD)"
			This.GrabarIncidencias(c_F24cl.F24lLinDoc,c_f24cl->F24lCodArt,_Crest)  && Display error.
			Return .F.
		EndIf
		
		*> Actualizar el documento (l�neas).-----------------------
		_Ok = This.UpdLin(_ResTot)
		If _Ok = .F.
			This.UsrError = "Error dando de alta la l�nea "
			This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CanResTot)  && Display error.
			Return .F.
		EndIf

		*> Actualizar el documento.---------------------------------
		_Ok = This.UpdDoc()
		If _Ok = .F.
			This.UsrError = "Error dando de alta la cabecera "
			This.GrabarIncidencias(c_F24cl.F24lLinDoc, c_f24cl->F24lCodArt, _CanResTot) && Display error.
			Return .F.
		EndIf

	EndIf
	
	*> ---------------------------------------------------------------
	*> Crear lista de trabajo.
	_Elim = CreateObject('Procaot')
		
	If !Used('F26c')
		_Elim.OpenTabla('F26c')
	EndIf
	
	If !Used('F26l')
		_Elim.OpenTabla('F26l')
	EndIf
	
	*> Objeto para tratamientos de listas de trabajo.
	oLst = CreateObject("OraFncList")
	oLst.Inicializar
	
	With oLst
		.CodPro = cCodPro
		.TipDoc = cTipDoc
		.NumDoc = cNumDoc
		.CodOpe = "0000"
		.UpdLst = "S"
		lStado = .GenLstDoc()
	EndWith
	*> ---------------------------------------------------------------

	*> ---------------------------------------------------------------
	*> Confirmar movimiento de lista de trabajo.

	_Sentencia =              " Select *"
	_Sentencia = _Sentencia + "   From F26l001"
	_Sentencia = _Sentencia + "  Where F26lCodPro='" + cCodPro + "'"
	_Sentencia = _Sentencia + "    And F26lTipDoc='" + cTipDoc + "'"
	_Sentencia = _Sentencia + "    And F26lNumDoc='" + cNumDoc + "'"
	_Sentencia = _Sentencia + "  Order By F26lNumLst Desc"

	=SqlExec(_Asql,_Sentencia,'cConFirmar')
	Select cConfirmar
	Go Top
	If !Eof()			
	
		With oLst
			.Inicializar
			
			.NumMovLS = cConfirmar.F26lNumMov				&& MP a confirmar.
			.FrzCnf = "N"									&& Forzar confirmaci�n.
			.UpdLst = "N"									&& Actualizar lista.
			.TMovExp = "2999"								&& Movimiento expedici�n (2999).
			.TMovOrg = "3500"								&& Movimiento en HM (origen).
			.TMovDst = "3000"								&& Movimiento en HM (destino).
		     lStado = .CnfLstMP_Cross()
		  EndWith
		
	EndIf
	*> ---------------------------------------------------------------
	
	Use In (Select("c_F24cl"))
	
Return
ENDPROC
PROCEDURE Init

*> Historial de modificaciones:
*>	13.06.2007 (AVC) (UpdDocRsv), Modificar la actualizaci�n del estado de la l�nea pedido.
*>	13.06.2007 (AVC) (UpdDocRsv), Adaptar a expediciones parciales.

=DoDefault()

ENDPROC


EndDefine 
