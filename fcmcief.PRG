*>
*> Proceso de cierre diario para un propietario/fecha.
*>
*> Calcular el peso a partir de F16t y, si no existe, tomar de F08c.
*> Debe estar FACGENC.PRG en la lista de procedimientos activos.

Parameters cPropietarioDesde, cPropietarioHasta, dFechaCierre,cArtIni, cArtFin
Private cWhere, cField, cGroup, cOrder, cFromF
Local lEstado, dFechaInicial, nPeso, nVolumen, nSigno

Store .T. To lEstado

*> Cursores de trabajo temporales.
If Used("F20cInfo")
   Use In F20cInfo
EndIf
If Used("F30dInfo")
   Use In F30dInfo
EndIf

Wait Window 'Cargando datos cierre anterior : ' + DToC(dFechaCierre) NoWait

*> Calcular la fecha de cierre anterior mas cercana al día a calcular.
cWhere = "F30dFecha < " + _GCD(dFechaCierre)
cFromF = "F30d"
cField = "Max(F30dFecha) As F30dFecha"

lEstado = f3_sql(cField, cFromF, cWhere, , , 'F30DPREV')

Select F30DPREV
If Day(F30dFecha)<1
   lEstado = .F.
EndIf
If lEstado
   *> Existe fecha de cierre: Obtener los datos de cierre de ese día.
   Select F30DPREV
   Go Top
   dFechaInicial = F30dFecha

   cFromF = "F30d"
   cWhere = "F30dCodPro Between '" + cPropietarioDesde + "' And '" + cPropietarioHasta + "' And " + ;
			"F30dCodArt Between '" + cArtIni + "' And '" + cArtFin + "' And " + ;
            "F30dFecha=" + _GCD(dFechaInicial)
   lEstado = f3_sql("*", cFromF, cWhere, , , 'F30dInfo')
Else
   *> No hay cierre anterior a la fecha a calcular: Partir de un F30D vacío.
   =CrtCursor("F30d", "F30dInfo", "C")
   dFechaInicial = _FecMin
EndIf

If Used('F30DPREV')
   Use In F30DPREV
EndIf

*> Cargar los movimientos producidos desde la fecha del último cierre hasta el día de cálculo.
cFromF = "F20c"
cOrder = "F20cCodPro, F20cCodArt, F20cFecMov"
cWhere = "F20cCodPro Between'" + cPropietarioDesde + "' And '" + cPropietarioHasta + "' And " + ;
		 "F20cCodArt Between'" + cArtIni + "' And '" + cArtFin + "' And " + ;
         "F20cFecMov > " + _GCD(dFechaInicial) + " And F20cFecMov <= " + _GCD(dFechaCierre)

lEstado = f3_sql("*", cFromF, cWhere, cOrder, , "F20cInfo")

*> Eliminar movimientos entre ubicaciones.
Select F20cInfo
*Delete For SubStr(F20cTipMov, 1, 1) = '3'
*Go Top

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
   Select F30dInfo
   Locate For F30dCodPro==F20cInfo.F20cCodPro .And. ;
   			  F30dCodArt==F20cInfo.F20cCodArt .And. ;
              F30dSitStk==F20cInfo.F20cSitStk

   If !Found()
      Append Blank
      Replace F30dCodPro With F20cInfo.F20cCodPro, ;
              F30dCodArt With F20cInfo.F20cCodArt, ;
              F30dSitStk With F20cInfo.F20cSitStk, ;
              F30dFecha  With dFechaCierre, ;
              F30dTotVol With 0, ;
              F30dTotPes With 0, ;
              F30dStock  With 0
   EndIf

   Replace F30dTotVol  With F30dTotVol + (nVolumen * nSigno), ;
           F30dTotPes  With F30dTotPes + (nPeso * nSigno), ;
           F30dStock   With F30dStock  + (F20cInfo.F20cCanFis * nSigno)

   *> Recalculamos el volumen total.--------------------------------
   Replace F30dTotVol With F30dStock * F08c.F08cVolUni

   *> Si la tarifa es fija recalculamos el peso.----------------------
   cField = "F32cFijVar"
   cFromF = "F32c"
   cWhere = "F32cCodPro = '" + F20cInfo.F20cCodPro + "'"
   lEstado = f3_sql(cField, cFromF, cWhere, , , 'Tarifa')
   If lEstado
      Select Tarifa
      Go Top
      If Tarifa.F32cFijVar = 'FIJO'
	     Select F30dInfo
	     Replace F30dTotPes With F30dStock * F08c.F08cPesUni
      EndIf
   EndIf

   *>
   Select F20cInfo
   Skip
EndDo

*> Borrar los datos del día a calcular, si los hay.
cFromF = "F30d"
cWhere = "F30dCodPro Between '" + cPropietarioDesde + "' And '" + cPropietarioHasta + "' And " + ;
		 "F30dCodArt Between '" + cArtIni + "' And '" + cArtFin + "' And " + ; 	
         "F30dFecha=" + _GCD(dFechaCierre)

lEstado = f3_DelTun(cFromF, , cWhere, 'N')
If !lEstado
   _LxErr = 'Error borrando datos existentes' + cr
   Do Form St3Inc With .T.
   =SqlRollBack(_ASql)
   Return
EndIf

*> Insertar los datos del cursor en la tabla de cierre, F30D.
Select F30dInfo
Delete For F30dStock <= 0
Replace All F30dFecha With dFechaCierre

Go Top
Do While !Eof()
   Wait Window 'Generando: ' + + DToC(dFechaCierre) + Space(1) + F30dCodPro + Space(1) + F30dCodArt NoWait

   =f3_InsTun('F30D', 'F30dInfo', 'N')

   Select F30dInfo
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
