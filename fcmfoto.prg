*>
*> Proceso de foto del almacén un propietario/fecha.
*>
*> Calcular el peso a partir de F16t y, si no existe, tomar de F08c.
*> Debe estar FACGENC.PRG en la lista de procedimientos activos.

Parameters cPropietarioDesde, cPropietarioHasta, dFechaCierre,cArtIni,cArtFin

Private cWhere, cField, cGroup, cOrder, cFromF
Local lEstado, dFechaInicial, nPeso, nVolumen, nSigno
dFechaBusqueda = (dFechaCierre - 1)


Store .T. To lEstado

*> Cursores de trabajo temporales.
If Used("F20cInfo")
   Use In F20cInfo
EndIf
If Used("F30dInfo")
   Use In F30dInfo
EndIf
Wait Window 'Cargando datos cierre anterior FOTO: ' + DToC(dFechaCierre) NoWait

*> Calcular la fecha de cierre anterior mas cercana al día a calcular.
cWhere = "F30lFecha=" + _GCD(dFechaBusqueda)
cFromF = "F30l"
cField = "Max(F30lFecha) As F30lFecha"

lEstado = f3_sql(cField, cFromF, cWhere, , , 'F30lPREV')
If lEstado
   *> Existe fecha de cierre: Obtener los datos de cierre de ese día.
   Select F30lPREV
   Go Top
   dFechaInicial = F30lFecha

   If dFechaInicial < _FecMin
      dFechaInicial = _FecMin
   EndIf

   cFromF = "F30l"
   cWhere = "F30lCodPro Between '" + cPropietarioDesde + "' And '" + cPropietarioHasta + "' And " + ;
   			"F30lCodArt Between '" + cArtIni + "' And '" + cArtFin + "' And " + ;	
            "F30lFecha=" + _GCD(dFechaInicial)
   lEstado = f3_sql("*", cFromF, cWhere, , , 'F30lInfo')
Else
   *> No hay cierre anterior a la fecha a calcular: Partir de un F30D vacío.
   =CrtCursor("F30l", "F30lInfo", "C")
   dFechaInicial = _FecMin
EndIf

If Used('F30lPREV')
   Use In F30lPREV
EndIf

*> Cargar los movimientos producidos desde la fecha del último cierre hasta el día de cálculo.
cFromF = "F20c"
cOrder = "F20cCodPro, F20cCodArt, F20cFecMov"
cWhere = "F20cCodPro Between'" + cPropietarioDesde + "' And '" + cPropietarioHasta + "' And " + ;
         "F20cCodArt Between'" + cArtIni + "' And '" + cArtFin + "' And " + ;
         "F20cFecMov > " + _GCD(dFechaInicial) + " And " + ;
         "F20cFecMov <= " + _GCD(dFechaCierre)

lEstado = f3_sql("*", cFromF, cWhere, cOrder, , "F20cInfo")

*> Eliminar movimientos entre ubicaciones.
Select F20cInfo
*Delete For SubStr(F20cTipMov, 1, 1) = '3'
Go Top

Do While !Eof()
   Wait Window 'Calculando: ' + + DToC(dFechaCierre) + Space(1) + F20cCodPro + Space(1) + F20cCodArt NoWait

   nSigno = Iif(F20cEntSal=='E', 1, -1)

   *> Obtener datos de la ficha de artículo.
   m.F08cCodPro = F20cCodPro
   m.F08cCodArt = F20cCodArt
   =f3_seek('F08c')

   *> Obtener el peso del F16t.
   nPeso = CalcPesoMovimiento(F20cInfo.F20cCodPro, F20cInfo.F20cNumPal, F20cInfo.F20cCanFis)
   If nPeso <= 0
      *> Si no se ha pesado el palet, calcular peso a partir de la ficha del artículo.
      nPeso = F20cInfo.F20cCanFis * F08c.F08cPesUni
   EndIf

   *> El volumen se calcula siempre a partir de la ficha del artículo.
   nVolumen = F20cInfo.F20cCanFis * F08c.F08cVolUni

   *> Añadir peso, volumen y cantidad a cursor de F30d.
   Select F30lInfo
   Locate For F30lCodPro==F20cInfo.F20cCodPro .And. ;
              F30lCodArt==F20cInfo.F20cCodArt .And. ;
              F30lSitStk==F20cInfo.F20cSitStk .And. ;
              F30lNumPal==F20cInfo.F20cNumPal 

*             F30lNumLot==F20cInfo.F20cNumLot 

*              F30lCodUbi==F20cInfo.F20cUbiOri

   *> Chapuza al canto.----------------------------
*   If !Found()
*	  If nSigno = -1
*		   Select F30dInfo
*		   Locate For F30dCodArt==F20cInfo.F20cCodArt 
*	  EndIf
*   EndIf

*   If F20cInfo.F20cEntSal = 'E'	
	   *> Añadir peso, volumen y cantidad a cursor de F30d.
*!*		   Select F30lInfo
*!*		   Locate For F30lCodPro==F20cInfo.F20cCodPro .And. ;
*!*		   			  F30lCodArt==F20cInfo.F20cCodArt .And. ;
*!*		              F30lSitStk==F20cInfo.F20cSitStk .And. ;
*!*		              F30lNumPal==F20cInfo.F20cNumPal .And. ;
*!*		              F30lNumLot==F20cInfo.F20cNumLot 

*	              F30lCodUbi==F20cInfo.F20cUbiOri

	   If !Found()
	      Append Blank
	      Replace F30lCodPro With F20cInfo.F20cCodPro, ;
	              F30lCodArt With F20cInfo.F20cCodArt, ;
	              F30lSitStk With F20cInfo.F20cSitStk, ;
	              F30lFecha  With dFechaCierre, ;
	              F30lNumPal With F20cInfo.F20cNumPal, ;
	              F30lNumLot With F20cInfo.F20cNumLot, ;
	              F30lCodUbi With F20cInfo.F20cUbiOri
	   EndIf

*!*		              F30lTotVol With nVolumen, ;
*!*		              F30lTotPes With nPeso, ;
*!*		              F30lStock  With F20cInfo.F20cCanFis, ;

*!*		Else

*!*		   *> Añadir peso, volumen y cantidad a cursor de F30d.
*!*		   Select F30lInfo
*!*		   Locate For F30lCodPro==F20cInfo.F20cCodPro .And. ;
*!*		   			  F30lCodArt==F20cInfo.F20cCodArt .And. ;
*!*		              F30lSitStk==F20cInfo.F20cSitStk .And. ;
*!*		              F30lNumPal==F20cInfo.F20cNumPal .And. ;
*!*		              F30lNumLot==F20cInfo.F20cNumLot 

*!*	*	              F30lCodUbi==F20cInfo.F20cUbiOri
*!*		
*!*			If Found()

		   Replace F30lTotVol  With F30lTotVol + (nVolumen * nSigno), ;
		           F30lTotPes  With F30lTotPes + (nPeso * nSigno), ;
		           F30lStock   With F30lStock  + (F20cInfo.F20cCanFis * nSigno)

*		   Delete

*!*			EndIf

*    EndIf

   *>
   Select F20cInfo
   Skip
EndDo

*> Borrar los datos del día a calcular, si los hay.
cFromF = "F30l"
cWhere = "F30lCodPro Between '" + cPropietarioDesde + "' And '" + cPropietarioHasta + "' And " + ;
         "F30lCodArt Between '" + cArtIni + "' And '" + cArtFin + "' And " + ;
         "F30lFecha=" + _GCD(dFechaCierre)

lEstado = f3_DelTun(cFromF, , cWhere, 'N')
If !lEstado
   _LxErr = 'Error borrando datos existentes' + cr
   Do Form St3Inc With .T.
   =SqlRollBack(_ASql)
   Return
EndIf

*> Insertar los datos del cursor en la tabla de cierre, F30D.
Select F30lInfo

Delete For F30lStock <= 0
Replace All F30lFecha With dFechaCierre

Go Top
Do While !Eof()
   Wait Window 'Generando: ' + + DToC(dFechaCierre) + Space(1) + F30lCodPro + Space(1) + F30lCodArt NoWait

   If IsNull(F30lInfo.F30lCodUbi)
	  Replace F30lInfo.F30lCodUbi With Space(14)
   EndIf

   =f3_InsTun('F30l', 'F30lInfo', 'N')

   Select F30lInfo
   Skip
EndDo

Wait Clear

*> Eliminar datos temporales.
If Used("F20cInfo")
   Use In F20cInfo
EndIf
If Used("F30dInfo")
   Use In F30dInfo
EndIf

=SqlCommit(_ASql)
Return lEstado
