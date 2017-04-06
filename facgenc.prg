*> GENERAR CALCULOS DE FACTURAS - "FACGENC.prg"

*****	Procedimiento Principal => GenCalc => Realiza las siguientes tareas:
*****										- Comprueba la viabilidad de la generación de cálculos
*****										- Crea los archivos temporales del F16,F20,F34 y F35
*****										- Gestiona la toma de decisiones : cuándo hay que
*****										  actualizar una tabla, etc, etc
*****
*****	ActualizarRegistroDiario => Idem
*****	CalcularDatosGlobalesDiarios	=> Es una parte del proceso anterior encargada de
*****              los campos que dependen del estado del F16temp para cada día del mes
*****	ActualizarRegistroMensual => Idem
*****	ActualizarRegistroSemanal => Idem
*****	ActualizarDataBase => Guarda los registros temporales y luego los borra
*****	Busqueda => Esta función devuelve si se ha encontrado algo como resultado de la consulta
*****				contenida en la variable Lx_Select
*****	Agregar_Error => Gestiona si se debe meter un nuevo error en _lxerr y lo mete si se tercia
*****	=> Calcula tabla F35S, nº de bultos por documento de salida
*****	CalcularDetalleEntradas => Calcula tabla F35E, nº de palets por documento de entrada

*> Actualizar datos de F35E/F35S en campos nuevos de F34C. AVC - 27.04.1999
*> Generar conceptos SEMANALES. AVC - 29.04.1999
*> Calcular máximo y promedio ubicaciones SEMANALES(F36C). AVC - 27.05.1999
*> Generar registros diarios, aunque no haya movimientos. AVC - 01.06.1999
*> Semanas incompletas, pasar al mes siguiente. JA - 07.06.1999
*> Incorporar tratamiento palets. AVC - 25.04.2001

***** Procedimiento principal del módulo
PROCEDURE GenCalc
Parameters codpro, ancalc, mecalc
Private cWhere, cField, cFromF, cOrder, cGroup

    MesT = PadL(AllTrim(Str(mecalc)), 2, '0')
    AnoT = PadL(AllTrim(Str(ancalc)), 4, '0')

    ** Miro si existe la ficha cliente para ese propietario
    m.F32cCodPro = CodPro
    =f3_seek('F32c')
    Select F32c
    Go Top
    If Eof()
       *> Horror!! No hay datos de facturación de este propietario.
       Do Agregar_Error With -1, "No hay datos facturación del propietario: " + CodPro + cr
       Return .F.
    EndIf
	
        ** Miro que no se haya generado el cálculo para este codpro+ancalc+mecalc
        Private ano, mes, fecha
		ano = Str(ancalc, 4)
		mes = Str(mecalc, 2)
		If left(mes,1)=" "
		   mes = "0" + Right(mes, 1)
		EndIf
		fecha = ano + mes + "0"

		Lx_Select = "Select * From F34c" + _em + Space(1) + ;
		            "Where F34cCodPro = '" + CodPro + "' And " + ;
					"F34cFecVal='" + fecha+ _cm

        _ok = Busqueda()
*        If _ok
*            _LxErr = _LxErr + "Ya se han generado los cálculos para el propietario: " + CodPro + cr
*            Return .F.
*        EndIf


		Set ClassLib To FACSTD Additive
		Ora = CreateObject('FacSpace.calculafac')
		=Ora.Inicio(CodPro, Fecha)
		Return .T.

        *> Leer valoración stock mes anterior.
        cWhere = "F34cCodPro='" + CodPro + "' And " + ;
                 "F34cFecVal<'" + Fecha + "'"
        =f3_sql("F34cValStk", "F34c", cWhere, "F34cFecVal", "", "F34cSeguro")
        Select F34cSeguro
        Go Bottom
        nSeguro = Iif(Eof(), 0, F34cValStk)
        Use In F34cSeguro

        ** Creo una versión del F16c para reconstruir las ocupaciones
        Wait Window 'Generando datos de trabajo de OCUPACIONES ...' NoWait
		cWhere = "F16cCodPro='" + CodPro + "'"
		=f3_sql("*", "F16c", cWhere, , , "F16TEMP")

		Select F16TEMP
        Replace All F16cFlag1 With Space(1)
		Go Top

        ** Creo el índice f16ind
        Wait Clear
        Wait Window 'Indexando datos de trabajo de OCUPACIONES ...' NoWait
		Index On F16cCodUbi + F16cCodArt + F16cNumPal + F16cNumLot + F16cCodPro TAG f16ind
        Set Order To Tag f16ind

        ** Creo un cursor con los artículos del propietario
        Wait Window 'Generando datos de trabajo de ARTICULOS ...' NoWait
		Lx_Select = "Select * From F08c" + _em + ", F00e" + _em + ;
                    " Where F08cCodPro = '" + CodPro + _cm + ;
                    " And   F08cTipPro=F00eTipPro"

		Err = f3_SqlExec(_aSql,Lx_Select,'F08TEMP')
		=SqlMoreResults(_aSql)
		Do Agregar_Error With err,"No se ha podido acceder a la tabla de artículos F08"
		Select F08TEMP
		If Eof()
		    _LxErr = _LxErr + "No hay ningún producto del propietario: " + CodPro + cr
   			Return .F.
	   	EndIf

        *> Crear índice a la tabla temporal de artículos.
        Wait Clear
        Wait Window 'Indexando datos de trabajo de ARTICULOS ...' NoWait
		Index On F08cCodPro + F08cCodArt TAG F08Ind
        Set Order To Tag F08Ind

        *> Creo un cursor con todos los movimientos implicados.
        Wait Clear
        Wait Window 'Generando datos de trabajo de HISTORICO de MOVIMIENTOS ...' NoWait
		fechat = "01/" + mes + "/" + ano

        *> Tomar todos los movimientos de fecha mayor o igual al mes a calcular, no solo los
        *> del mes, pues hay que calcular la ocupación, hacia atrás, desde el día del cálculo.
		cWhere =          "F20cCodPro='" + CodPro + "' And "
		cWhere = cWhere + "F20cFecMov >=" + _GCD(fechat) + " And "

        cWhere = cWhere + _GCSS("F20cTipMov", 1, 1) + " In ('1', '2', '4')"

		=f3_sql("*", "F20c", cWhere, , , "F20TEMP")

        *> Clasificar el cursor por fecha.
        Select F20TEMP
        cSort = FnTemp(3)
        Sort To (cSort) On F20cFecMov /D
        
        Select F20TEMP
        Delete All
        Append From (cSort)
        Delete File (cSort)
					
        Select F20TEMP
        Go Top

        ** Creo un cursor con la estructura del F34c.
        =CrtCursor('F34C', 'F34TEMP', 'C')

        Select F34TEMP
        Delete All
        Append Blank
        Replace F34cCodPro With CodPro, ;
                F34cFecVal With Fecha

        ** Creo un cursor con la estructura del f35c
        =CrtCursor('F35C', 'F35TEMP', 'C')

        *> Primer paso: Generar (vacíos) todos los registros diarios.
        Do GenerarF35c

        *> Generar acumulados y diario de palets para facturación.
        =ActualizarRegistroPalets(CodPro, AnoT, MesT)
        =ActualizarTotalesPalets(CodPro, AnoT, MesT)

        ** Creo un cursor con la estructura del f36c
        =CrtCursor('F36C', 'F36TEMP', 'C')

        ** Aquí monto el pasado
		vacio = .T.      &&&&& Controlo si hay algún registro activo en el F35TEMP.
		Select F20TEMP
        Count To _Total
        Store 0 To _NReg
        Store 'RECALCULAR OCUPACIONES PERIODO' To TITULO
        Do Form St3Term
        =F3_Term('TITULO', TITULO)

        Wait Clear
        SwInicio = .F.
        Select F20TEMP

        dia_tratamiento = _FecMin

        Go Top
        Do While !Eof()
            _NReg = _NReg + 1
            =F3_Term("LINEA", _NReg, _Total)

			* Actualizo el F35 si ya estoy en el mes que me han pedido
			If mes=(SubStr(DToC(F20TEMP.F20cFecMov), 4, 2))
                If !SwInicio 
                   =CalcularOcupacionFinal()
                   SwInicio = .T.
                EndIf
                =ActualizarRegistroDiario()

                If dia_tratamiento = _FecMin .Or. F20TEMP.F20cFecMov <> dia_tratamiento
                   dia_tratamiento = F20TEMP.F20cFecMov    && Para f35c.
                   Select F35TEMP
                   Locate For F35cCodPro = CodPro .And. F35cFecVal = DToS(dia_tratamiento)
                   =CalcularDatosGlobalesDiarios()
                EndIf
			EndIf

			* Busco el registro del F20 en el F16
            Select F16TEMP
            Seek F20TEMP.F20cUbiOri + ;
                 F20TEMP.F20cCodArt + ;
                 F20TEMP.F20cNumPal + ;
                 F20TEMP.F20cNumLot + ;
                 F20TEMP.F20cCodPro

			* Si no existe creo uno nuevo en el F16TEMP.
			If !Found()
				Append Blank
				Replace F16cCodPro With F20TEMP.F20cCodPro, ;
						F16cCodArt With F20TEMP.F20cCodArt, ;
						F16cCodUbi With F20TEMP.F20cUbiOri, ;
						F16cCanFis With F20TEMP.F20cCanFis, ;
						F16cCanFis With 0, ;
						F16cFecEnt With F20TEMP.F20cFecMov, ;
						F16cNumLot With F20TEMP.F20cNumLot, ;
						F16cNumPal With F20TEMP.F20cNumPal, ;
						F16cSitStk With F20TEMP.F20cSitStk, ;
						F16cFecCad With F20TEMP.F20cFecCad
			EndIf

			* Actualizar la ocupación con +/- el movimiento
			Do Case
               *> Entradas: Restar.
               Case Left(F20TEMP.F20cTipMov, 1)="1" .Or. ;
                    Left(F20TEMP.F20cTipMov, 2)="40" .Or. ;
                    Between(F20TEMP.F20cTipMov, '3000', '3499')
                  Replace F16cCanFis With F16cCanFis - F20TEMP.F20cCanFis

               *> Salidas: Sumar.
               Case Left(F20TEMP.F20cTipMov, 1)="2" .Or. ;
                    Left(F20TEMP.F20cTipMov, 2)="45" .Or. ;
                    Between(F20TEMP.F20cTipMov, '3500', '3999')
                  Replace F16cCanFis With F16cCanFis + F20TEMP.F20cCanFis
			EndCase

            Replace F16cFlag1 With Space(1)

			*> Voy al siguiente registro del histórico de movimientos.
			Select F20TEMP
			Skip
        EndDo

        *> Cerrar el termómetro.
        =F3_Term('FIN')

        *> Guardar ocupación al final del periodo.
        If !SwInicio
           =CalcularOcupacionFinal()
           SwInicio = .T.
        EndIf

        *> Calcular la ocupación al principio del periodo.
        =CalcularOcupacionInicial()
        
       ** Actualizo la información del último día.
       Select F20TEMP
       Go Top
       If !Eof()
*         =CalcularDatosGlobalesDiarios()        &&&&& Se hacen los cálculos del F16
	   EndIf
	
       ** Aquí actualizo el registro del F35s.
       =CalcularDetalleSalidas()
       Wait Clear

       ** Aquí actualizo el registro del F35e
       =CalcularDetalleEntradas()
       Wait Clear

       ** Actualizar F35c para los días sin movimientos.
       =ActualizarF35c()

       ** Aquí actualizo el registro del F34c
       =ActualizarRegistroMensual()
       Wait Clear

       ** Grabo los temporales y luego los borro
       =ActualizarDataBase()
       Wait Clear

RETURN .T.

*> Actualizar los acumulados DIARIOS.
PROCEDURE ActualizarRegistroDiario
Private nPesoMovimiento, nValorMovimiento

    *> Leer el artículo del cursor con índice que se creó al principio.
    Select F08Temp
    Seek CodPro + F20TEMP.F20cCodArt
        
    *> Calcular el peso y valor del movimiento.
    nPesoMovimiento = CalcularPesoMovimiento(F20TEMP.F20cCodPro, ;
                                             F20TEMP.F20cNumPal, ;
                                             F20TEMP.F20cCanFis)

    nValorMovimiento = nPesoMovimiento * F08TEMP.F08cPCoste

    ** De paso voy actualizando el registro del F34c
    Select F34TEMP
    Do Case
       *> Kilos entradas.
       Case Left(F20TEMP.F20cTipMov,1)="1" .Or. Left(F20TEMP.F20cTipMov,2)="40"
*         Replace F34cKilEnt With F34cKilEnt + (F20TEMP.F20cCanFis * F08TEMP.F08cPesUni / 1000.0), ;

          Replace F34cKilEnt With F34cKilEnt + nPesoMovimiento, ;
                  F34cVolEnt With F34cVolEnt + (F20TEMP.F20cCanFis * F08TEMP.F08cVolUni / 1000.0), ;
                  F34cMovEnt With F34cMovEnt + 1, ;
                  F34cUniEnt With (F34cUniEnt + F20TEMP.F20cCanFis)

       *> Kilos salidas.
       Case Left(F20TEMP.F20cTipMov,1)="2" .Or. Left(F20TEMP.F20cTipMov,2)="45"
*         Replace F34cKilSal With F34cKilSal + (F20TEMP.F20cCanFis * F08TEMP.F08cPesUni / 1000.0), ;

          Replace F34cKilSal With F34cKilSal + nPesoMovimiento, ;
                  F34cVolSal With F34cVolSal + (F20TEMP.F20cCanFis * F08TEMP.F08cVolUni)/ 1000.0, ;
                  F34cMovSal With F34cMovSal + 1, ;
                  F34cUniSal With (F34cUniSal + F20TEMP.F20cCanFis)
    EndCase

    ** Relleno los campos pertinentes al F35C.
    Select F35TEMP
    If F20TEMP.F20cEntSal="E"
       Replace F35cMovEnt With F35cMovEnt + 1, ;
               F35cVolEnt With F35cVolEnt + (F20TEMP.F20cCanFis*F08TEMP.F08cVolUni / 1000.0), ;
               F35cKilEnt With F35cKilEnt + (F20TEMP.F20cCanFis*F08TEMP.F08cPesUni / 1000.0)
    Else
       Replace F35cMovSal With F35cMovSal + 1, ;
               F35cVolSal With F35cVolSal + (F20TEMP.F20cCanFis*F08TEMP.F08cVolUni / 1000.0), ;
               F35cKilSal With F35cKilSal + (F20TEMP.F20cCanFis*F08TEMP.F08cPesUni / 1000.0)
    EndIf

		Replace F35cTotMov With F35cTotMov + 1
Return

*> Actualizar el diario de palets para facturación del mes en curso.
*> F36P - Diario palets facturación.
*>
*> Recibe (variables globales):
*>   CodPro -----> Propietario.
*>    cAnoT -----> Año a calcular.
*>    cMesT -----> Mes a calcular.
*>
*> Calcula, si cal, la valoración de los palets entrados, a partir del peso y de PCoste.

Procedure ActualizarRegistroPalets
Parameters cCodPro, cAnoT, cMesT

Private cSelec, cFromF, cWhere, cOrder, cGroup
Private dFechaDeCierre, cFechaSalida
Local nRecNo, nPesUniTotal, nPesoParcial, cPaletActual

   Store '' To cOrder, cGroup

   Wait Window 'Generando registro de palets mes. Un momento ...' NoWait

   *> Borrar los registros del mes actual, si hay.
   cWhere = "F36pCodPro='" + cCodPro + "' And " + ;
            "F36pFecVal='" + cAnoT + cMesT + "'"
   =f3_DelTun('F36P', , cWhere, 'N')

   *> Crear un cursor de trabajo como F36P.
   =CrtCursor('F36P', 'F36PTEMP', 'C')

   *>
   *> Anulado AFRISA. Solo carga los palets entrados en el mes en curso. AVC - 28.06.2001
   *>
   *> Leer y traspasar los palets abiertos de periodos anteriores.
   *> Se toman los que tengan fecha de salida > mes actual.
   *> Para estos palets, calcular la fecha de cierre, para los que han salido desde
   *> la facturación anterior.
   *cWhere = "F36pCodPro='" + cCodPro + "' And " + ;
   *         "F36pFecSal>=" + _GCD(CToD("01" + "/" + cMesT + "/" + cAnoT)) + " And " + ;
   *         "F36pFecVal<='" + cAnoT + cMesT + "' And " + ;
   *         "F36pFlgEst='0'"
   *
   *=f3_sql('*', 'F36P', cWhere, cOrder, cGroup, 'F36pCur')
   *
   *cFromF = 'F16c,F10c'
   *Store '' To cOrder, cGroup
   *
   *If Used('F16cCur')
   *   Use In F16cCur
   *EndIf

   *Select F36pCur
   *Go Top
   *Do While !Eof()
      *> Pasar los palets abiertos al mes actual.
      *Select F36PTEMP
      *Locate For F36pCodPro==F36pCur.F36pCodPro .And. F36pNumPal==F36pCur.F36pNumPal
      *If !Found()
      *   Select F36pCur
      *   Scatter Name oF36
      *   Select F36PTEMP
      *   Append Blank
      *   Gather Name oF36
      *   Replace F36pFecVal With cAnoT + cMesT
      *
      *   If F36pFecSal = _FecMax
      *      *> Buscar palet en ocupaciones.
      *      *>       "F16cNumDoc>'0' And "
      *
      *      cWhere = "F16cNumPal='" + F36PTEMP.F36pNumPal + "' And " + ;
      *               "F10cCodUbi=F16cCodUbi And F10cPickSn<>'E'"
      *
      *      =f3_sql('*', cFromF, cWhere, cOrder, cGroup, 'F16cCur')
      *      Select F16cCur
      *      Go Top
      *
      *      *> Calcular la nueva fecha de cierre para los palets abiertos.
      *      dFechaDeCierre = Iif(!Eof(), _FecMax, FechaSalidaPalet(F36PTEMP.F36pNumPal))
      *
      *      Select F36PTEMP
      *      If !IsNull(dFechaDeCierre)
      *         Replace F36pFecSal With dFechaDeCierre
      *      EndIf
      *   EndIf
      *EndIf

      *>
   *   Select F36pCur
   *   Skip
   *EndDo
   *
   *Use In F36pCur

   Wait Window 'Calculando datos detalle palets. Un momento ...' NoWait

   *> Leer y traspasar los palets entrados del mes a calcular.
   cSelec = "Unique F20cCodPro, F20cCodArt, F20cNumPal, F20cFecMov, F20cNumEnt, " + ;
            _GCN("F16tPesPal", 0) + " As F16tPesPal, " + ;
            _GCN("F08cPCoste", 0) + " As F08cPCoste, " + _GCN("F08cPesUni", 0) + " As F08cPesUni"
   cFromF = "F20c,F16t,F08c"
   cOrder = ""                 && "F20cCodPro, F20cFecMov, F20cNumPal"
   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
            "To_Char(F20cFecMov, 'YYYYMM')='" + cAnoT + cMesT + "' And " + ;
            "F16tNumPal(+)=F20cNumPal And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

   =f3_sql(cSelec, cFromF, cWhere, cOrder, cGroup, 'F20cCur')

   cFromF = 'F16c,F10c'
   Store '' To cOrder, cGroup, cPaletActual

   *> Incorporar los palets entrados en el mes actual.
   *> Tener en cuenta que pueden ser palets multireferencia.
   Select F20cCur
   Delete For Empty(F20cNumEnt)
   Go Top
   Do While !Eof()
      If F20cNumPal==cPaletActual
         Skip
         Loop
      EndIf

      Wait Window 'Actualizando palet Nº: ' + F20cCur.F20cNumPal NoWait

      cPaletActual = F20cNumPal

      *> Calcular el valor del stock.
      If Type('nSeguro')=='N'
         *> Guardar posición actual.
         nRecNo = RecNo()

         *> Calculo proporcional del peso para cada producto, en palets multireferencia.
         Sum(F08cPesUni) To nPesUniTotal For F20cNumPal=cPaletActual
         nPesUniTotal = Iif(Empty(nPesUniTotal), 1, nPesUniTotal)     && Porsiaca.

         Locate For F20cNumPal==cPaletActual
         Do While Found()
            *> Calcular la parte proporcional del peso palet que coresponde al artículo.
            nPesoParcial = F16tPesPal * F08cPesUni / nPesUniTotal
            nSeguro = nSeguro + (nPesoParcial * F20cCur.F08cPCoste)

            Continue
         EndDo

         *> Recuperar posición actual.
         Go nRecNo
      EndIf

      *> Buscar palet en ocupaciones.
      cWhere = "F16cNumPal='" + F20cCur.F20cNumPal + "' And " + ;
               "F10cCodUbi=F16cCodUbi And F10cPickSn<>'E'"

      =f3_sql('*', cFromF, cWhere, cOrder, cGroup, 'F16cCur')
      Select F16cCur
      Go Top

      *> Si existe, el palet está abierto, poner fecha máxima. En caso contrario,
      *> el palet está cerrado, poner fecha de salida del almacén.
      *> Si el palet no está, y NO ha salido por preparación (2999), por ejemplo por
      *> una regularización, este palet no se considera a efectos de facturación.
      dFechaDeCierre = Iif(!Eof(), _FecMax, FechaSalidaPalet(F20cCur.F20cNumPal))

      *> Generar registro en F36P de trabajo.
      If IsNull(dFechaDeCierre)
         dFechadeCierre = F20cCur.F20cFecMov
      EndIf

      Select F36PTEMP
      Locate For F36pNumPal==F20cCur.F20cNumPal
      If !Found()
         Append Blank
         Replace F36pCodPro With cCodPro, ;
                 F36pNumPal With F20cCur.F20cNumPal, ;
                 F36pFecVal With cAnoT + cMesT, ;
                 F36pFecEnt With F20cCur.F20cFecMov , ;
                 F36pFecSal With dFechaDeCierre, ;
                 F36pFlgEst With '0', ;
                 F36pFlg2   With '0', ;
                 F36pFlg1   With '0'
      EndIf

      *>
      Select F20cCur
      Skip
   EndDo

   Wait Window 'Actualizando datos detalle palets. Un momento ...' NoWait

   *> Actualizar F36P de trabajo en la base de datos.
   Select F36PTEMP
   Go Top
   Do While !Eof()
      =f3_InsTun('F36P', 'F36PTEMP', 'S')

      *>
      Select F36PTEMP
      Skip
   EndDo

   Wait Clear

   *> Eliminar tablas temporales.
   If Used('F16cCur')
      Use In F16cCur
   EndIf

   If Used('F20cCur')
      Use In F20cCur
   EndIf

   If Used('F36PTEMP')
      Use In F36PTEMP
   EndIf

Return

*> Actualizar el histórico de palets del mes.
*> F35P - Histórico palets facturación.
*> Trabaja a partir de F36P, que contiene el diario de palets del mes en curso,
*> que se debe de haber generado con anterioridad.
*>
*> Recibe:
*>   cCodPro -----> Propietario.
*>     cAnoT -----> Año a calcular.
*>     cMesT -----> Mes a calcular.
*>
*> Calcula, si cal, la valoración de los palets entrados, a partir del peso y de PCoste.

Procedure ActualizarTotalesPalets
Parameters cCodPro, cAnoT, cMesT
Private cField, cSelec, cFromF, cWhere, cOrder, cGroup
Private nKgs, nKgF
Private dFechaMedio
Private nKilosSalidaQu1, nQilosSalidaQu2, nKilosSalidaMes, nPesoMovimiento
Private cPaletActual
Private nRecNo

   Wait Window 'Generando acumulado de palets quincena/mes. Un momento ...' NoWait

   *> Obtener los datos de facturación del propietario.
   m.F32cCodPro = cCodPro
   =f3_seek('F32c')
   Select F32c
   Go Top
   If Eof()
      *> Horror!! No hay datos de facturación de este propietario.
      Do Agregar_Error With -1, "No hay datos facturación del propietario: " + CodPro + cr
      Return .F.
   EndIf

   *> Borrar los registros del mes actual, si hay.
   cWhere = "F35pCodPro='" + cCodPro + "' And " + ;
            "F35pFecVal='" + cAnoT + cMesT + "'"
   =f3_DelTun('F35P', , cWhere, 'N')

   *>
   *> Versión CHACHI del programa.
   *> NO BORRAR este trozo de código, porfa !!!!!!
   *>
   *> Crear cursor de trabajo de F35P, acumulado palets.
   *> =CrtCursor('F35P', 'F35PTEMP', 'C')
   *> Select F35PTEMP
   *> Append Blank
   *>Replace F35pCodPro With cCodPro, ;
   *>        F35pFecVal With cAnoT + cMesT, ;
   *>        F35pPalQu1 With 0, ;
   *>       F35pKgsQu1 With 0, ;
   *>       F35pKgFQu1 With 0, ;
   *>       F35pPalQu2 With 0, ;
   *>       F35pKgsQu2 With 0, ;
   *>       F35pKgFQu2 With 0, ;
   *>       F35pPalMes With 0, ;
   *>       F35pKgsMes With 0, ;
   *>       F35pKgFMes With 0, ;
   *>       F35pPalRsv With F32c.F32cPalRsv

   *> Agregar el saldo del mes anterior. Normalmente se añadirían los palets abiertos
   *> en F36p del mes anterior pero, debido al descontrol del almacén, se parte del
   *> saldo anterior, si existe, del mes anterior. AVC - 28.06.2001

   cWhere = "F35pCodPro='" + cCodPro + "' And " + ;
            "F35pFecVal<'" + cAnoT + cMesT + "'"
   cFromF = "F35p"
   cOrder = "F35pCodPro, F35pFecVal"
   cGroup = ""
   cField = "*"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F35PTEMP')
   Select F35PTEMP
   Go Bottom
   If Eof()
      Append Blank
      Replace F35pPalQu1 With 0, ;
              F35pKgsQu1 With 0, ;
              F35pKgFQu1 With 0, ;
              F35pPalQu2 With 0, ;
              F35pKgsQu2 With 0, ;
              F35pKgFQu2 With 0, ;
              F35pPalMes With 0, ;
              F35pKgsMes With 0, ;
              F35pKgFMes With 0
   Else
      Scatter Name oF35p
      Delete All
      Append Blank

      Replace F35pPalQu1 With oF35p.F35pPalQu2 + oF35p.F35pPalRsv, ;
              F35pKgsQu1 With oF35p.F35pKgsQu2, ;
              F35pKgFQu1 With oF35p.F35pKgFQu2, ;
              F35pPalQu2 With oF35p.F35pPalQu2 + oF35p.F35pPalRsv, ;
              F35pKgsQu2 With oF35p.F35pKgsQu2, ;
              F35pKgFQu2 With oF35p.F35pKgFQu2, ;
              F35pPalMes With oF35p.F35pPalQu2 + oF35p.F35pPalRsv, ;
              F35pKgsMes With oF35p.F35pKgsQu2, ;
              F35pKgFMes With oF35p.F35pKgFQu2
   EndIf

   Replace F35pCodPro With cCodPro, ;
           F35pFecVal With cAnoT + cMesT, ;
           F35pPalRsv With F32c.F32cPalRsv

   cOrder = 'F36pFecEnt'
   Store '' To cGroup

   *> Agregar los palets abiertos de F36p.
   cFromF = "F36p,F16t"

   cField = "F36p" + _em + ".*, " + _GCN("F16tPesPal", 0) + " As F36pPesPal"
   cWhere = "F36pCodPro='" + cCodPro + "' And " + ;
            "F36pFecVal='" + cAnoT + cMesT + "' And " + ;
            "F36pFlgEst='0' And " + ;
            "F16tNumPal(+)=F36pNumPal"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F36PTEMP')

   *> Calcular la fecha de final quincenas.
   dFechaMedio = CToD('15/' + cMesT + '/' + cAnoT)

   Do Case
      *> Sin reserva de palets.
      *Case F32c.F32cPalRsv==0
      *   =CalcularAcumuladoPaletsNormal()

      *> Con reserva mensual de palets.
      Case F32c.F32cTipRsv=='M'
         =CalcularAcumuladoPaletsMensual()

      *> Con reserva diaria de palets.
      Case F32c.F32cTipRsv=='D'
         =CalcularAcumuladoPaletsDiario()

      *> Con reserva mensual de palets.
      Otherwise
         =CalcularAcumuladoPaletsMensual()
   EndCase

   *> Reserva de palets.
   Select F35PTEMP
   * Replace F35pPalMes With F35pPalMes + F32c.F32cPalRsv

   If F32c.F32cKgsMed > 0
      *> Kilos facturables, quincena 1.
      nKgs = F35pKgsQu1
      nKgF = F32c.F32cKgsMed * F35pPalQu1
      Replace F35pKgFQu1 With Iif(nKgF > nKgs, nKgF, nKgs)

      *> Kilos facturables, quincena 2.
      nKgs = F35pKgsQu2
      nKgF = F32c.F32cKgsMed * F35pPalQu2
      Replace F35pKgFQu2 With Iif(nKgF > nKgs, nKgF, nKgs)

      *> Kilos facturables, mensual.
      nKgs = F35pKgsMes
      nKgF = F32c.F32cKgsMed * F35pPalMes
      Replace F35pKgFMes With Iif(nKgF > nKgs, nKgF, nKgs)
   EndIf

   *> Actualizar histórico palets en base de datos.
   =f3_InsTun('F35P', 'F35PTEMP', 'S')

   Wait Clear

   *> Eliminar tablas temporales.
   If Used('F35PTEMP')
      Use In F35PTEMP
   EndIf

   If Used('F36PTEMP')
      Use In F36PTEMP
   EndIf

Return

*> Calcular acumulados quincenales y mensual de palets.
*> SIN reserva de palets.
*>     A) Calcular entradas periodo.
Procedure CalcularAcumuladoPaletsNormal

   Local lEstado
   Private dFechaMedio

   Store .T. To lEstado

   *> Calcular la fecha de final quincenas.
   dFechaMedio = CToD('15/' + cMesT + '/' + cAnoT)

   *> Acumular los palets del diario en el histórico palets.
   Select F36PTEMP
   Go Top
   Do While !Eof()
      Select F35PTEMP

      *> Acumular en segunda quincena y totales mes.
      Replace F35pPalMes With F35pPalMes + 1, ;
              F35pKgsMes With F35pKgsMes + F36PTEMP.F36pPesPal, ;
              F35pPalQu2 With F35pPalQu2 + 1, ;
              F35pKgsQu2 With F35pKgsQu2 + F36PTEMP.F36pPesPal

      *> Acumular en totales primera quincena.
      If F36PTEMP.F36pFecEnt <= dFechaMedio
         Replace F35pPalQu1 With F35pPalQu1 + 1, ;
                 F35pKgsQu1 With F35pKgsQu1 + F36PTEMP.F36pPesPal
      EndIf

      *>
      Select F36PTEMP
      Skip
   EndDo

   *> Restar las salidas del periodo.
   =CalcularSalidasNormal()

Return lEstado

*> Calcular acumulados quincenales y mensual de palets.
*> SIN reserva de palets.
*>     B) Calcular salidas periodo.
*>
*>    Recibe: cAnoT
*>            cMesT
*>
Procedure CalcularSalidasNormal

   Local lEstado
   Private nPesoMovimiento
   Private cField, cSelec, cFromF, cWhere, cOrder, cGroup
   Private nKilosSalidaQu1, nQilosSalidaQu2, nKilosSalidaMes, nPesoMovimiento
   Private cPaletActual, dFechaSalida

   Store .T. To lEstado

   *> Calcular las salidas del periodo.
   Store 0 To nKilosSalidaQu1, nKilosSalidaQu2, nKilosSalidaMes

   cFromF = "F20c,F08c"
   cField = "F20c" + _em + ".*, " + _GCN("F08cPCoste", 0) + "As F08cPCoste"
   cOrder = 'F20cFecMov DESC'
   cGroup = ''
   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
            "F20cTipMov='2999' And " + ;
            "To_Char(F20cFecMov, 'YYYYMM')='" + cAnoT + cMesT + "' And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F2036TEMP')
   Select F2036TEMP
   Replace All F20cFlag2 With Space(1)

   Go Top
   Do While !Eof()
      cPaletActual = F2036TEMP.F20cNumPal
      nPesoMovimiento = ObtenerPesoMovimiento(F2036TEMP.F20cCodPro, ;
                                              F2036TEMP.F20cNumPal, ;
                                              F2036TEMP.F20cCanFis, ;
                                              F2036TEMP.F20cNumMov)
      *> Calcular el valor del stock.
      If Type('nSeguro')=='N'
         nSeguro = nSeguro - (nPesoMovimiento * F2036TEMP.F08cPCoste)
      EndIf

      *> Salidas total 2ª quincena y mes.
      nKilosSalidaMes = nKilosSalidaMes + nPesoMovimiento
      nKilosSalidaQu2 = nKilosSalidaQu2 + nPesoMovimiento

      *> Salidas de la 1ª quincena.
      Select F2036TEMP
      If F20cFecMov <= dFechaMedio
         nKilosSalidaQu1 = nKilosSalidaQu1 + nPesoMovimiento

         *> Ver si el palet todavía está en el almacén.
         If Empty(F20cFlag2)
            cWhere = "F16cNumPal='" + cPaletActual + "' And " + ;
                     "F10cCodUbi=F16cCodUbi And F10cPickSn<>'E'"

            =f3_sql('*', "F16c,F10c", cWhere, , , 'F16cCur')
            Select F16cCur
            Go Top
            If Eof()
               dFechaSalida = FechaSalidaPalet(cPaletActual)
               If dFechaSalida <= dFechaMedio
                  *> El palet ya no está: Restar de acumulados.
                  Select F35PTEMP
                  Replace F35pPalQu2 With F35pPalQu2 - 1
                  If F35pPalQu2 < 0
                     Replace F35pPalQu2 With 0
                 EndIf
              EndIf
            EndIf

            Use In F16cCur
         EndIf
      EndIf

      *> Tratar el siguiente movimiento de salida.
      Select F2036TEMP
      nRecNo = RecNo()
      Replace All F20cFlag2 With '*' For F20cNumPal==cPaletActual
      Go nRecNo
      Skip
   EndDo

   Use In F2036TEMP

   *> Actualizar los kilos facturables, según parámetros facturación del propietario.
   Select F35PTEMP
   Replace F35pKgsQu2 With F35pKgsQu2 - nKilosSalidaQu1
   If F35pKgsQu2 < 0
      Replace F35pKgsQu2 With 0
   EndIf

Return lEstado

*> Calcular acumulados quincenales y mensual de palets.
*> CON reserva mensual de palets.
Procedure CalcularAcumuladoPaletsMensual

   Local lEstado
   Private nRsv
   Private dFechaMedio

   Store .T. To lEstado

   *> Calcular la fecha de final quincenas.
   dFechaMedio = CToD('15/' + cMesT + '/' + cAnoT)

   *> Palets reserva mensuales para cálculos.
   nRsv = F32c.F32cPalRsv

   *> Acumular los palets del diario en el histórico palets.
   *> Tener en cuenta los palets reserva que tenga el propietario.
   Select F36PTEMP
   Go Top
   Do While !Eof()
      Select F35PTEMP

      *> Acumular en segunda quincena y totales mes.
      Replace F35pPalMes With F35pPalMes + 1, ;
              F35pKgsMes With F35pKgsMes + F36PTEMP.F36pPesPal, ;
              F35pPalQu2 With F35pPalQu2 + 1, ;
              F35pKgsQu2 With F35pKgsQu2 + F36PTEMP.F36pPesPal

      *> Acumular en totales primera quincena.
      If F36PTEMP.F36pFecEnt <= dFechaMedio
         Replace F35pPalQu1 With F35pPalQu1 + 1, ;
                 F35pKgsQu1 With F35pKgsQu1 + F36PTEMP.F36pPesPal
      EndIf

      *>
      Select F36PTEMP
      Skip
   EndDo

   *> Restar las salidas del periodo.
   =CalcularSalidasMensual()

   Select F35PTEMP
   If F35pPalMes > F32c.F32cPalRsv
      Replace F35pPalMes With F35pPalMes - F32c.F32cPalRsv
   EndIf
   If F35pPalQu1 > F32c.F32cPalRsv
      Replace F35pPalQu1 With F35pPalQu1 - F32c.F32cPalRsv
   EndIf
   If F35pPalQu2 > F32c.F32cPalRsv
      Replace F35pPalQu2 With F35pPalQu2 - F32c.F32cPalRsv
   EndIf

Return lEstado

*> Calcular acumulados quincenales y mensual de palets.
*> CON reserva mensual de palets.
*>     B) Calcular salidas periodo.
*>
*>    Recibe: cAnoT
*>            cMesT
*>
Procedure CalcularSalidasMensual

   Local lEstado
   Private nPesoMovimiento
   Private cField, cSelec, cFromF, cWhere, cOrder, cGroup
   Private nKilosSalidaQu1, nKilosSalidaMes, nPesoMovimiento
   Private cPaletActual, dFechaSalida

   Store .T. To lEstado

   *> Calcular las salidas del periodo.
   Store 0 To nKilosSalidaQu1, nKilosSalidaMes

   cFromF = "F20c,F08c"
   cField = "F20c" + _em + ".*, " + _GCN("F08cPCoste", 0) + " As F08cPCoste"
   cOrder = 'F20cFecMov DESC'
   cGroup = ''
   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
            "F20cTipMov='2999' And " + ;
            "To_Char(F20cFecMov, 'YYYYMM')='" + cAnoT + cMesT + "' And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F2036TEMP')
   Select F2036TEMP
   Replace All F20cFlag2 With Space(1)

   Go Top
   Do While !Eof()
      cPaletActual = F2036TEMP.F20cNumPal
      nPesoMovimiento = ObtenerPesoMovimiento(F2036TEMP.F20cCodPro, ;
                                              F2036TEMP.F20cNumPal, ;
                                              F2036TEMP.F20cCanFis, ;
                                              F2036TEMP.F20cNumMov)
      *> Calcular el valor del stock.
      If Type('nSeguro')=='N'
         nSeguro = nSeguro - (nPesoMovimiento * F2036TEMP.F08cPCoste)
      EndIf

      *> Salidas de la 1ª quincena.
      If F2036TEMP.F20cFecMov <= dFechaMedio
         nKilosSalidaQu1 = nKilosSalidaQu1 + nPesoMovimiento

         *> Ver si el palet todavía está en el almacén.
         Select F2036TEMP
         If Empty(F20cFlag2)
            cWhere = "F16cNumPal='" + cPaletActual + "' And " + ;
                     "F10cCodUbi=F16cCodUbi And F10cPickSn<>'E'"

            =f3_sql('*', "F16c,F10c", cWhere, , , 'F16cCur')
            Select F16cCur
            Go Top
            If Eof()
               dFechaSalida = FechaSalidaPalet(cPaletActual)
               If dFechaSalida <= dFechaMedio
                  *> El palet ya no está: Restar de acumulados.
                  Select F35PTEMP
                  Replace F35pPalQu2 With F35pPalQu2 - 1
                  If F35pPalQu2 < 0
                     Replace F35pPalQu2 With 0
                 EndIf
              EndIf
           EndIf

            Use In F16cCur
         EndIf
      EndIf

      *> Tratar el siguiente movimiento de salida.
      Select F2036TEMP
      nRecNo = RecNo()
      Replace All F20cFlag2 With '*' For F20cNumPal==cPaletActual
      Go nRecNo
      Skip
   EndDo

   Use In F2036TEMP

   *> Actualizar los kilos facturables.
   Select F35PTEMP
   Replace F35pKgsQu2 With F35pKgsQu2 - nKilosSalidaQu1

   If F35pPalQu2 < 0
      Replace F35pPalQu2 With 0
   EndIf
   If F35pKgsQu2 < 0
      Replace F35pKgsQu2 With 0
   EndIf

Return lEstado

*> Calcular acumulados quincenales y mensual de palets.
*> CON reserva diaria de palets.
*>     A) Calcular entradas periodo.
Procedure CalcularAcumuladoPaletsDiario

   Local lEstado
   Private dFechaMedio
   Private dDiaEnCurso, dDiaCalculo
   Private nPaletsDiaEnCurso, nKilosDiaEnCurso
   Private nPalets

   Store .T. To lEstado

   *> Calcular la fecha de final quincenas.
   dFechaMedio = CToD('15/' + cMesT + '/' + cAnoT)

   *> Día de cálculo.
   dDiaCalculo = CToD('01' +  '/' + MesT + '/' + AnoT)

   Select F36PTEMP
   Go Top
   If !Eof()
      *> Obtener valores iniciales del día en curso.
      Select F35PTEMP
      nPaletsDiaEnCurso = F35PTEMP.F35pPalQu1        && F35PTEMP.F35pPalMes
      nKilosDiaEnCurso =  F35PTEMP.F35pKgsQu1        && F35PTEMP.F35pKgsMes

      Replace F35pPalMes With 0, ;
              F35pKgsMes With 0, ;
              F35pPalQu1 With 0, ;
              F35pKgsQu1 With 0, ;
              F35pPalQu2 With 0, ;
              F35pKgsQu2 With 0

      nRsv = F32c.F32cPalRsv - nPaletsDiaEnCurso
      If nRsv < 0
         nRsv = 0
      EndIf
   Else
      Return lEstado
   EndIf

   Do While Month(dDiaCalculo) = Val(MesT)
      dDiaEnCurso = dDiaCalculo

      *> Acumular los palets del diario en el histórico palets.
      *> Tener en cuenta los palets reserva que tenga el propietario.
      Select F36PTEMP
      Locate For F36pFecEnt==dDiaEnCurso
      Do While Found()
         If nRsv <= 0
            *> Acumular en totales día en curso.
            nPaletsDiaEnCurso = nPaletsDiaEnCurso + 1
            nKilosDiaEnCurso = nkilosDiaEnCurso + F36PTEMP.F36pPesPal
         Else
            nRsv = nRsv - 1
         EndIf

         Select F36PTEMP
         Continue
      EndDo

      *> Restar las salidas del día en curso.
      =CalcularSalidasDiario()

      If nKilosDiaEnCurso < 0
         nKilosDiaEnCurso = 0
      EndIf
      If nPaletsDiaEnCurso < 0
         nPaletsDiaEnCurso = 0
      EndIf

      *> Acumular en totales mes.
      nPalets = nPaletsDiaEnCurso - F32c.F32cPalRsv
      If nPalets < 0
         nPalets = 0
      EndIf

      Select F35PTEMP
      Replace F35pPalMes With F35pPalMes + nPalets, ;
              F35pKgsMes With f35pKgsMes + nKilosDiaEnCurso

      If F36PTEMP.F36pFecEnt <= dFechaMedio
         *> Acumular en totales primera quincena.
         Replace F35pPalQu1 With F35pPalQu1 + nPalets, ;
                 F35pKgsQu1 With F35pKgsQu1 + nKilosDiaEnCurso
      Else
         *> Acumular en totales segunda quincena.
         Replace F35pPalQu2 With F35pPalQu2 + nPalets, ;
                 F35pKgsQu2 With F35pKgsQu2 + nKilosDiaEnCurso
      EndIf

      *> Inicializar acumulados del día en curso.
      nRsv = F32c.F32cPalRsv - nPaletsDiaEnCurso
      If nRsv < 0
         nRsv = 0
      EndIf

      *> Calcular el siguiente día.
      dDiaCalculo = dDiaCalculo + 1
   EndDo

Return lEstado

*> Calcular acumulados quincenales y mensual de palets.
*> CON reserva diaria de palets.
*>     B) Calcular salidas periodo.
*>
*>    Recibe: dDiaEnCurso
*>
*> Actualiza: nPaletsDiaEnCurso
*>            nKilosDiaEnCurso

Procedure CalcularSalidasDiario

   Local lEstado
   Private nPesoMovimiento
   Private cField, cSelec, cFromF, cWhere, cOrder, cGroup
   Private cPaletActual, dFechaSalida

   Store .T. To lEstado

   cFromF = "F20c,F08c"
   cField = "F20c" + _em + ".*, " + _GCN("F08cPCoste", 0) + "As F08cPCoste"
   cGroup = ''
   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
            "F20cTipMov='2999' And " + ;
            "To_Char(F20cFecMov, 'YYYYMMDD')='" + DToS(dDiaEnCurso - 1) + "' And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F2036TEMP')
   Select F2036TEMP
   Replace All F20cFlag2 With Space(1)

   Go Top
   Do While !Eof()
      cPaletActual = F2036TEMP.F20cNumPal
      nPesoMovimiento = ObtenerPesoMovimiento(F2036TEMP.F20cCodPro, ;
                                              F2036TEMP.F20cNumPal, ;
                                              F2036TEMP.F20cCanFis, ;
                                              F2036TEMP.F20cNumMov)
      *> Calcular el valor del stock.
      If Type('nSeguro')=='N'
         nSeguro = nSeguro - (nPesoMovimiento * F2036TEMP.F08cPCoste)
      EndIf

      *> Salidas día en curso.
      nKilosDiaEnCurso = nKilosDiaEnCurso - nPesoMovimiento

      *> Ver si el palet todavía está en el almacén.
      Select F2036TEMP
      If Empty(F20cFlag2)
         cWhere = "F16cNumPal='" + cPaletActual + "' And " + ;
                  "F10cCodUbi=F16cCodUbi And F10cPickSn<>'E'"

         =f3_sql('*', "F16c,F10c", cWhere, , , 'F16cCur')
         Select F16cCur
         Go Top
         If Eof()
            dFechaSalida = FechaSalidaPalet(cPaletActual)
            If dFechaSalida < dDiaEnCurso
               *> El palet ya no está: Restar de acumulados.
               nPaletsDiaEnCurso = nPaletsDiaEnCurso - 1
            EndIf
         EndIf

         Use In F16cCur
      EndIf

      *> Tratar el siguiente movimiento de salida.
      Select F2036TEMP
      nRecNo = RecNo()
      Replace All F20cFlag2 With '*' For F20cNumPal==cPaletActual
      Go nRecNo
      Skip
   EndDo

   Use In F2036TEMP

Return lEstado

*>
Procedure CalcularDatosGlobalesDiarios       &&&&& Incumben al F16 en un determinado día

    Private UbicAnt && Ubicación anterior - Sirve para ver si se repiten
    Private nPesoMovimiento, nValorMovimiento
    Local nRegistrosTotales

    Wait Clear
	UbicAnt = Space(14)

    *> Relacionar registro de ocupaciones con ficha artículo.
	Select F16TEMP
    Set Relation To F16cCodPro + F16cCodArt Into F08Temp

    Count For F16cCanFis <> 0 To nRegistrosTotales

	Locate For F16cCanFis <> 0
	Do While Found()
        Wait Window 'Actualizando día: ' + DToC(dia_tratamiento) + ' Registro: ' + Str(nRegistrosTotales) NoWait

		* NOTA : voy incrementando los datos del F34(mensual) para al final tan solo tener
		*		 que dividir entre el número de días
		Do Case
            *> Ocupaciones de FRIO.
			Case F08TEMP.F00eCodEst = TipPro(1)
				Replace F35TEMP.F35cOcuFri With F35TEMP.F35cOcuFri + 1, ;
						F34TEMP.F34cOcuFrm With F34TEMP.F34cOcuFrm + 1
				If F16cCodUbi<>UbicAnt
 					Replace F35TEMP.F35cUbiFri With F35TEMP.F35cUbiFri + 1, ;
							F34TEMP.F34cUbiFrm With F34TEMP.F34cUbiFrm + 1
				EndIf

            *> Ocupaciones de AMBIENTE.
			Case F08TEMP.F00eCodEst = TipPro(2)
				Replace F35TEMP.F35cOcuAmb With F35TEMP.F35cOcuAmb + 1, ;
						F34TEMP.F34cOcuAmm With F34TEMP.F34cOcuAmm + 1
				If F16cCodUbi<>UbicAnt
					Replace F35TEMP.F35cUbiAmb With F35TEMP.F35cUbiAmb + 1, ;
							F34TEMP.F34cUbiAmm With F34TEMP.F34cUbiAmm + 1
				EndIf

            *> Ocupaciones de ESTUPEFACIENTES.
			Case F08TEMP.F00eCodEst = TipPro(3)
				Replace F35TEMP.F35cOcuDrg With F35TEMP.F35cOcuDrg + 1, ;
						F34TEMP.F34cOcuDrm With F34TEMP.F34cOcuDrm + 1 
				If F16cCodUbi<>UbicAnt
					Replace F35TEMP.F35cUbiDrg With F35TEMP.F35cUbiDrg + 1, ;
							F34TEMP.F34cUbiDrm With F34TEMP.F34cUbiDrm + 1
				EndIf

            *> Ocupaciones de RESTO.
			OtherWise
				Replace F35TEMP.F35cOcuRes With F35TEMP.F35cOcuRes + 1, ;
						F34TEMP.F34cOcuRem With F34TEMP.F34cOcuRem + 1
				If F16cCodUbi<>UbicAnt
					Replace F35TEMP.F35cUbiRes With F35TEMP.F35cUbiRes + 1, ;
							F34TEMP.F34cUbiRem With F34TEMP.F34cUbiRem + 1
				EndIf
		EndCase

		Replace F34TEMP.F34cOcuOcu With F34TEMP.F34cOcuOcu + 1
		If F16cCodUbi<>UbicAnt
		   Replace F34TEMP.F34cUbiUbi With F34TEMP.F34cUbiUbi + 1
		EndIf

        *> Calcular el peso y valor del movimiento.
        nPesoMovimiento = CalcularPesoMovimiento(F16cCodPro, F16cNumPal, F16cCanFis)
        nValorMovimiento = nPesoMovimiento * F08TEMP.F08cPCoste

        Replace F35TEMP.F35cVolOcu With F35TEMP.F35cVolOcu + (F16cCanFis*F08TEMP.F08cVolUni / 1000.0), ;
                F35TEMP.F35cKilOcu With F35TEMP.F35cKilOcu + (F16cCanFis*F08TEMP.F08cPesUni / 1000.0), ;
                F35TEMP.F35cKilStk With F35TEMP.F35cKilStk + nPesoMovimiento, ;
                F35TEMP.F35cValStk With F35TEMP.F35cValStk + nValorMovimiento


        * Aprovecho para ir actualizando el Volumen y Kilos Ocupados del mes.
        Replace F34TEMP.F34cVolOcu With F34TEMP.F34cVolOcu + F35TEMP.F35cVolOcu, ;
                F34TEMP.F34cKilOcu With F34TEMP.F34cKilOcu + F35TEMP.F35cKilOcu, ;
                F34TEMP.F34cValStk With F34TEMP.F34cValStk + nValorMovimiento

        *>
        nRegistrosTotales = nRegistrosTotales - 1

		Select F16TEMP
        Replace F16cFlag1 With '*'
        UbicAnt = F16cCodUbi
        Continue
	EndDo

	* No dejo ningún campo vacío para que no se produzcan errores por NULLs.
    Select F35TEMP
	If Empty(F35cUbiFri)
		Replace F35cUbiFri With 0, ;
				F35cOcuFri With 0
	EndIf
	If Empty(F35cUbiAmb)
		Replace F35cUbiAmb With 0, ;
				F35cOcuAmb With 0
	EndIf
	If Empty(F35cUbiDrg)
		Replace F35cUbiDrg With 0, ;
				F35cOcuDrg With 0
	EndIf
	If Empty(F35cUbiRes)
		Replace F35cUbiRes With 0, ;
				F35cOcuRes With 0
	EndIf
	If Empty(F35cMovEnt)
		Replace F35cMovEnt With 0
	EndIf
	If Empty(F35cMovSal)
		Replace F35cMovSal With 0
	EndIf
	If Empty(F35cTotMov)
		Replace F35cTotMov With 0
	EndIf
	If Empty(F35cVolEnt)
		Replace F35cVolEnt With 0
	EndIf
	If Empty(F35cVolSal)
		Replace F35cVolSal With 0
	EndIf
	If Empty(F35cVolReg)
		Replace F35cVolReg With 0
	EndIf
	If Empty(F35cVolOcu) 
		Replace F35cVolOcu With 0
	EndIf
	If Empty(F35cKilEnt)
		Replace F35cKilEnt With 0
	EndIf
	If Empty(F35cKilSal)
		Replace F35cKilSal With 0
	EndIf
	If Empty(F35cKilOcu)
		Replace F35cKilOcu With 0
	EndIf
	If Empty(F35cKilEnp)
		Replace F35cKilEnp With 0
	EndIf
	If Empty(F35cValStk)
		Replace F35cValStk With 0
	EndIf
	If Empty(F35cKilStk)
		Replace F35cKilStk With 0
	EndIf

RETURN

*> Guarda en F34c la ocupación al final del periodo de cálculo.
Function CalcularOcupacionFinal

    Private UbicAnt

    Wait Clear
	UbicAnt = Space(14)

    *> Relacionar registro de ocupaciones con ficha artículo.
	Select F16TEMP
    Set Relation To F16cCodPro + F16cCodArt Into F08Temp

	Go Top
	Locate For F16cCanFis <> 0
	Do While Found()
		Do Case
            *> Ocupaciones de FRIO.
			Case F08TEMP.F00eCodEst = TipPro(1)
				Replace F34TEMP.F34cOcuFrf With F34TEMP.F34cOcuFrf + 1
				If F16cCodUbi<>UbicAnt
					Replace F34TEMP.F34cUbiFrf With F34TEMP.F34cUbiFrf + 1
				EndIf

            *> Ocupaciones de AMBIENTE.
			Case F08TEMP.F00eCodEst = TipPro(2)
				Replace F34TEMP.F34cOcuAmf With F34TEMP.F34cOcuAmf + 1
				If F16cCodUbi<>UbicAnt
					Replace F34TEMP.F34cUbiAmf With F34TEMP.F34cUbiAmf + 1
				EndIf

            *> Ocupaciones de ESTUPEFACIENTES.
			Case F08TEMP.F00eCodEst = TipPro(3)
				Replace F34TEMP.F34cOcuDrf With F34TEMP.F34cOcuDrf + 1
				If F16cCodUbi<>UbicAnt
					Replace F34TEMP.F34cUbiDrf With F34TEMP.F34cUbiDrf + 1
				EndIf

            *> Ocupaciones de RESTO.
			OtherWise
				Replace F34TEMP.F34cOcuRef With F34TEMP.F34cOcuRef + 1
				If F16cCodUbi<>UbicAnt
					Replace F34TEMP.F34cUbiRef With F34TEMP.F34cUbiRef + 1
				EndIf
		EndCase

        *>
		Select F16TEMP
        UbicAnt = F16cCodUbi
        Continue
	EndDo

Return

*> Guarda en F34c la ocupación al inicio del periodo de cálculo.
Function CalcularOcupacionInicial

    Private UbicAnt

    Wait Clear
	UbicAnt = Space(14)

    *> Relacionar registro de ocupaciones con ficha artículo.
	Select F16TEMP
    Set Relation To F16cCodPro + F16cCodArt Into F08Temp

	Go Top
	Locate For F16cCanFis <> 0
	Do While Found()
		Do Case
            *> Ocupaciones de FRIO.
			Case F08TEMP.F00eCodEst = TipPro(1)
				Replace F34TEMP.F34cOcuFri With F34TEMP.F34cOcuFri + 1
				If F16cCodUbi<>UbicAnt
					Replace F34TEMP.F34cUbiFri With F34TEMP.F34cUbiFri + 1
				EndIf

            *> Ocupaciones de AMBIENTE.
			Case F08TEMP.F00eCodEst = TipPro(2)
				Replace F34TEMP.F34cOcuAmi With F34TEMP.F34cOcuAmi + 1
				If F16cCodUbi<>UbicAnt
					Replace F34TEMP.F34cUbiAmi With F34TEMP.F34cUbiAmi + 1
				EndIf

            *> Ocupaciones de ESTUPEFACIENTES.
			Case F08TEMP.F00eCodEst = TipPro(3)
				Replace F34TEMP.F34cOcuDri With F34TEMP.F34cOcuDri + 1
				If F16cCodUbi<>UbicAnt 
					Replace F34TEMP.F34cUbiDri With F34TEMP.F34cUbiDri + 1
				EndIf

            *> Ocupaciones de RESTO.
			OtherWise
				Replace F34TEMP.F34cOcuRei With F34TEMP.F34cOcuRei + 1
				If F16cCodUbi<>UbicAnt 
					Replace F34TEMP.F34cUbiRei With F34TEMP.F34cUbiRei + 1
				EndIf
		EndCase

        *>
		Select F16TEMP
        UbicAnt = F16cCodUbi
        Continue
	EndDo

Return

*> Actualiza los totales por propietario/mes.
PROCEDURE ActualizarRegistroMensual

	* Cantidad de movimientos
	Select F20TEMP
	Count To NRegs
    Select F34TEMP
    Replace F34cMovMov With NRegs

	* Documentos de Entrada
    Wait Clear
    Wait Window 'Calculando Nº de DOCUMENTOS de ENTRADA ...' NoWait
    Lx_Select = "Select " + _GCN("Count(Distinct(F20cNumDoc))") + " As Cuenta From F20c" + _em + ;
                " Where F20cCodPro='" + CodPro + "' And " + ;
                " To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "'" + ;
                " And " + _GCSS("F20cTipMov", 1, 1) + "='1'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F20TEMPDE')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F20 para recoger documentos de entrada"
	Replace F34TEMP.F34cDocEnt With Cuenta
	Use In F20TEMPDE

	* Documentos de Salida
    Wait Clear
    Wait Window 'Calculando Nº de DOCUMENTOS de SALIDA ...' NoWait
	Lx_Select = "Select Count(Distinct(F24cNumDoc)) As Cuenta From F24c" + _em + ;
	            " Where F24cCodPro='" + codpro + "' And " + ;
                "To_Char(F24cFecDoc, 'YYYYMM')='" + + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F20TEMPDS')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F20 para recoger documentos de salida"
	Replace F34TEMP.F34cDocSal With Cuenta
	Use In F20TEMPDS

	* Líneas de Entrada
    Wait Clear
    Wait Window 'Calculando Nº de LINEAS de ENTRADA ...' NoWait
	Lx_Select = "Select " + _GCN("Count(Distinct(F20cLinDoc))") + " As Cuenta From F20c" + _em + ;
	            " Where F20cCodPro='" + codpro + "' And " + ;
                " To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "'" + ;
                " And " + _GCSS("F20cTipMov", 1, 1) + "='1'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F20TEMPLE')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F20 para recoger líneas de entrada"
	Replace F34TEMP.F34cLinEnt With Cuenta
	Use In F20TEMPLE

	* Líneas de Salida
    Wait Clear
    Wait Window 'Calculando Nº de LINEAS de SALIDA ...' NoWait
	Lx_Select = "Select " + _GCN("Count(F24lLinDoc)") + " As Cuenta From f24l" + _em + " Where F24lCodPro='" + codpro + "' and " + ;
                " To_Char(F24lFecDoc, 'YYYYMM')='" + AnoT + MesT + _cm

	Err = f3_SqlExec(_aSql,Lx_Select,'F20TEMPLS')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F20 para recoger líneas de salida"
	Replace F34TEMP.F34cLinSal With Cuenta
	Use In F20TEMPLS

    *> Totales de F35E - Entradas.
    Wait Clear
    Wait Window 'Calculando acumulados de ENTRADA ...' NoWait
    MesT = AllTrim(Mes)
    MesT = PadL(MesT, 2, '0')

    Lx_Select = "Select " + _GCN("Count(*)")   + " As DOCE, " + ;
                       _GCN("Sum(F35eNumLin)") + " As LINE, " + ;
                       _GCN("Sum(F35eTotPal)") + " As PALE, " + ;
                       _GCN("Sum(F35eTotFra)") + " As FRAE, " + ;
                       _GCN("Sum(F35eUniFra)") + " As UNFE " + ;
                "From F35e" + _em + Space(1) + ;
                "Where F35eCodPro='" + CodPro + "'" + " And " + ;
                "To_Char(F35eFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql, Lx_Select, 'F35eTemp')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err, "No se han podido recoger los datos de F35E"

    Replace F34TEMP.F34cDocPrv With F35eTemp.DOCE, ;
            F34TEMP.F34cPalPrv With F35eTemp.PALE, ;
            F34TEMP.F34cFraPrv With F35eTemp.FRAE, ;
            F34TEMP.F34cUniPrv With F35eTemp.UNFE, ;
            F34TEMP.F34cLinPrv With F35eTemp.LINE
	Use In F35eTemp

    *> Totales de F35S - Salidas.
    Wait Clear
    Wait Window 'Calculando acumulados de SALIDA ...' NoWait

    Lx_Select = "Select " + GCN("Count(*)")    + " As DOCS, " + ;
                       _GCN("Sum(F35sNumLin)") + " As LINS, " + ;
                       _GCN("Sum(F35sTotPal)") + " As PALS, " + ;
                       _GCN("Sum(F35sTotCaj)") + " As CAJS, " + ;
                       _GCN("Sum(F35sTotGrp)") + " As GRPS, " + ;
                       _GCN("Sum(F35sTotFra)") + " As FRAS, " + ;
                       _GCN("Sum(F35sUniFra)") + " As UNFS, " + ;
                       _GCN("Sum(F35sTotPk1)") + " As CAJ1, " + ;
                       _GCN("Sum(F35sTotPk2)") + " As CAJ2 " + ;
                "From F35s" + _em + Space(1) + ;
                "Where F35sCodPro='" + CodPro + "'" + " And " + ;
                "To_Char(F35sFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql, Lx_Select, 'F35sTemp')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err, "No se han podido recoger los datos de F35S"

    Replace F34TEMP.F34cDocCli With F35sTemp.DOCS, ;
            F34TEMP.F34cPalCli With F35sTemp.PALS, ;
            F34TEMP.F34cCajCli With F35sTemp.CAJS, ;
            F34TEMP.F34cGrpCli With F35sTemp.GRPS, ;
            F34TEMP.F34cFraCli With F35sTemp.FRAS, ;
            F34TEMP.F34cUniCli With F35sTemp.UNFS, ;
            F34TEMP.F34cLinCli With F35sTemp.LINS, ;
            F34TEMP.F34cPk1Cli With F35sTemp.CAJ1, ;
            F34TEMP.F34cPk2Cli With F35sTemp.CAJ2

	Use In F35sTemp

	*> Divido los valores contenidos en los medios entre el nº de días para obtener la media.
	*> Obtengo el nº de días de proceso.
	*> Aclarar si debe tomar nº de días proceso o nº de días del mes.
	Select F35TEMP
	Count To NRegs

	Select F34TEMP
	If NRegs > 0
		Replace F34cUbiFrm With Ceiling(F34cUbiFrm/NRegs), ;
				F34cOcuFrm With Ceiling(F34cOcuFrm/NRegs), ;
				F34cUbiAmm With Ceiling(F34cUbiAmm/NRegs), ;
				F34cOcuAmm With Ceiling(F34cOcuAmm/NRegs), ;
				F34cUbiDrm With Ceiling(F34cUbiDrm/NRegs), ;
				F34cOcuDrm With Ceiling(F34cOcuDrm/NRegs), ;
				F34cUbiRem With Ceiling(F34cUbiRem/NRegs), ;
				F34cOcuRem With Ceiling(F34cOcuRem/NRegs)
	Else
		Replace F34cUbiFrm With 0, ;
				F34cOcuFrm With 0, ;
				F34cUbiAmm With 0, ;
				F34cOcuAmm With 0, ;
				F34cUbiDrm With 0, ;
				F34cOcuDrm With 0, ;
				F34cUbiRem With 0, ;
				F34cOcuRem With 0
	EndIf
	
	* Nº de documentos de salida.
	Lx_Select = "Select " + _GCN("Count(Distinct(F24cNumPed))") + " As Cuenta From F24c" + _em + ;
	            " Where F24cCodPro='" + CodPro + "' And " + ;
                "To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F24TEMPNP')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F24C para recoger pedidos"
	Replace F34TEMP.F34cNpeRec With Cuenta
	Use In F24TEMPNP

	* Nº de líneas de documentos de salida.
	Lx_Select = "Select " + _GCN("Count(Distinct(F24lLinDoc))") + " As Cuenta From F24l" + _em + ;
	            " Where F24lCodPro='" + CodPro + "' And " + ;
                "To_Char(F24lFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F24TEMPLP')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F24L para recoger líneas de pedidos"
	Replace F34TEMP.F34cLpeRec With Cuenta
	Use In F24TEMPLP

	* Nº de Albaranes
	Lx_Select = "Select " + _GCN("Count(Distinct(F30cAlbRep))") + " As Cuenta From F30c" + _em + ;
	            " Where F30cCodEnt='" + CodPro + "' And " + ;
                " To_Char(F30cFecAlb, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F30TEMPAA')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F30C para recoger albaranes"
	Replace F34TEMP.F34cAlbAlb With Cuenta
	Use In F30TEMPAA

	* Líneas de Albaranes
	Lx_Select = "Select " + _GCN("Count(*)") + " As Cuenta From F30c" + _em + ;
	            " Where F30cCodEnt='" + CodPro + "' And " + ;
                " To_Char(F30cFecAlb, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F30TEMPLA')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F30C para recoger líneas de albaranes"
	Replace F34TEMP.F34cLinAlb With Cuenta
	Use In F30TEMPLA

	* Kilos de Albaranes
	Lx_Select = "Select " + _GCN("Sum(F30cPesoKg)") + " As Cuenta From F30c" + _em + ;
	            " Where F30cCodEnt='" + CodPro + "' And " + ;
                " To_Char(F30cFecAlb, 'YYYYMM')='" + AnoT + MesT + "'"

	Err = f3_SqlExec(_aSql,Lx_Select,'F30TEMPPA')
	=SqlmoreResults(_aSql)
	do Agregar_Error With err,"No se ha podido acceder al F30C para recoger kilos de albaranes"
	If not IsNull(Cuenta) 
		Replace F34TEMP.F34cKilAlb With Cuenta
	else
		Replace F34TEMP.F34cKilAlb With 0
	EndIf
	Use In F30TEMPPA

	* Volumen de Albaranes
	Lx_Select = "Select " + _GCN("Sum(F30cVolume)") + " As Cuenta From F30c" + _em + ;
	            " Where F30cCodEnt='" + CodPro + "' And " + ;
                " To_Char(F30cFecAlb, 'YYYYMM')='" + AnoT + MesT + "'"

    Err = f3_SqlExec(_aSql,Lx_Select,'F30TEMPVA')
    =SqlmoreResults(_aSql)
    Do Agregar_Error With err,"No se ha podido acceder al F30C para recoger volumen de albaranes"
    If not IsNull(Cuenta) 
       Replace F34TEMP.F34cVolAlb With Cuenta
    Else
       Replace F34TEMP.F34cVolAlb With 0
	EndIf
    Use In F30TEMPVA

    *> Actualizar el peso palet/día por pesaje-F16T.
    Wait Window 'Leyendo acumulado peso palets ...' NoWait

    Lx_Select = "Select "+ _GCN("Sum(F16tPesPal)") + " As PesoPalet " + ;
                "From   F16t" + _em + Space(1) + ;
                "Where  F16tNumPal In " + ;
                "   (Select Unique F20cNumPal From F20c" + _em + ;
                "    Where F20cCodPro='" + CodPro + "'" + ;
                "    And   To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "'" + ;
                "    And   " + _GCSS("F20cTipMov", 1, 1) + "='1'" + ;
                "    And   F20cNumEnt > '0')"         && + "    And   F20cNumPal=F16tNumPal)"

    Err = f3_SqlExec(_aSql,Lx_Select,'F16TTEMP')
    =SqlmoreResults(_aSql)
    Do Agregar_Error With err,"No se ha podido acceder al F16T para recoger peso palets mensual"
    Replace F34TEMP.F34cKilEnP With PesoPalet

    Use In F16TTEMP
    Wait Clear

    *> Valorar el stock mensual.
    Select F35TEMP
    Go Bottom

    Select F34TEMP
    Replace F34cValStk With F35TEMP.F35cValStk       && Valor antiguo.
    Replace F34cValStk With nSeguro                  && Valor actual.

    *> Sum(F35cValStk) To nValStk
    *> Select F34TEMP
    *> Replace F34cValStk With nValStk

    *> Controlar que NO haya campos NULL.
	Select F34TEMP
	If Empty(F34cKilEnt) 
		Replace F34cKilEnt With 0
	EndIf
	If Empty(F34cVolEnt) 
		Replace F34cVolEnt With 0
	EndIf
	If Empty(F34cMovEnt) 
		Replace F34cMovEnt With 0
	EndIf
	If Empty(F34cUniEnt) 
		Replace F34cUniEnt With 0
	EndIf
	If Empty(F34cDocEnt) 
		Replace F34cDocEnt With 0
	EndIf
	If Empty(F34cLinEnt) 
		Replace F34cLinEnt With 0
	EndIf
	If Empty(F34cKilSal) 
		Replace F34cKilSal With 0
	EndIf
	If Empty(F34cVolSal) 
		Replace F34cVolSal With 0
	EndIf
	If Empty(F34cMovSal) 
		Replace F34cMovSal With 0
	EndIf
	If Empty(F34cUniSal) 
		Replace F34cUniSal With 0
	EndIf
	If Empty(F34cDocSal) 
		Replace F34cDocSal With 0
	EndIf
	If Empty(F34cLinSal) 
		Replace F34cLinSal With 0
	EndIf
	If Empty(F34cValStk) 
		Replace F34cValStk With 0
	EndIf
		
RETURN

*> Comprueba si la semana está completa o no
PROCEDURE SemanaCompleta

   Parameters MesT, NSem 
 
   *> Si es la primera semana del año no cuenta
   If Val(MesT) >= 12
      Return .T.
   EndIf

   Select DiasSem
   Go Bottom
   If Eof() .Or. DiasSem.Sema # NSem
      Return .T.
   Else
      Return Iif(DiasSem.Dias = 7, .T., .F.)
   EndIf

Return

*> Calcula los acumulados por propietario/semana.
PROCEDURE ActualizarRegistroSemanal

    MesT = AllTrim(Mes)
    MesT = PadL(MesT, 2, '0')

   *> Crear tabla temporal con datos semanales, a partir de F35c.
   Wait Clear
   Wait Window 'Actualizando conceptos SEMANALES ...' NoWait
   Lx_Select = "Select F35cCodPro, " + ;
               "To_Char(To_Date(F35cFecVal, 'YYYYMMDD'), 'WW') As SEMA, " + ;
               "Sum(F35cOcuFri+F35cOcuAmb+F35cOcuDrg+F35cOcuRes) As OCUP, " + ;
               "Sum(F35cUbiFri+F35cUbiAmb+F35cUbiDrg+F35cUbiRes) As UBIC, " + ;
               "Max(F35cOcuFri+F35cOcuAmb+F35cOcuDrg+F35cOcuRes) As MAXI, " + ;
               "Max(F35cUbiFri+F35cUbiAmb+F35cUbiDrg+F35cUbiRes) As MAXU, " + ;
               "Avg(F35cOcuFri+F35cOcuAmb+F35cOcuDrg+F35cOcuRes) As AVER, " + ;
               "Avg(F35cUbiFri+F35cUbiAmb+F35cUbiDrg+F35cUbiRes) As AVEU, " + ;
               "Sum(F35cTotMov) As MOVI, " + ;
               "Sum(F35cVolOcu) As VOLU, " + ;
               "Sum(F35cKilOcu) As KILO, " + ;
               "Sum(F35cKilEnP) As KILP " + ;
               "From F35c" + _em + Space(1) + ;
               "Where " + _GCSS("F35cFecVal", 5, 2) + "='" + MesT + "' " + ;
               "And F35cCodPro='" + CodPro + "' " + ;
               "Group By F35cCodPro, To_Char(To_Date(F35cFecVal, 'YYYYMMDD'), 'WW')"

        Err = f3_SqlExec(_aSql,Lx_Select,'F36cTemp')
        =SqlMoreResults(_aSql)
        Do Agregar_Error With err,"No se ha podido acceder a la tabla F35C"

  *> Creo el cursor para control de semanas incompletas       
   Select  = "F35cCodPro, To_Char(To_Date(F35cFecVal, 'YYYYMMDD'), 'WW') As Sema, Count(*) As Dias"
   From    = "F35c"	 
   Where   = _GCSS("F35cFecVal", 5, 2) + "= '" + MesT + "' And F35cCodPro = '" + CodPro + _cm	
   Order   = "F35cCodPro, Sema"
   GroupBy = "F35cCodPro, To_Char(To_Date(F35cFecVal, 'YYYYMMDD'), 'WW')"         
 
   =f3_sql(Select, From, Where, Order, GroupBy, "DiasSem")

   *> Actualización de F36C.
   Mes = MesT
   Select F36cTemp
   Go Top
   NumSemana = 0
   Do While !Eof()
      Wait Clear
      Wait Window 'Procesando acumulados SEMANALES ' + F35cCodPro + '-' + SEMA NoWait

      If Val(MesT) < 12
      	If !SemanaCompleta (MesT, F36cTemp.SEMA)
      	    Mes = Val(MesT) + 1
      	    Mes = PadL(AllTrim(Str(Mes)), 2, '0')
         Else
      	    Mes = MesT
      	 EndIf
      EndIf

      Select F36TEMP
      Append Blank
      Replace F36cCodPro With CodPro, ;
              F36cFecVal With Ano + Mes + F36cTemp.SEMA, ;
              F36cOcuOcu With F36cTemp.OCUP, ;
              F36cUbiUbi With F36cTemp.UBIC, ;
              F36cPalMax With F36cTemp.MAXI, ;
              F36cUbiMax With F36cTemp.MAXU, ;
              F36cPalAvg With F36cTemp.AVER, ;
              F36cUbiAvg With F36cTemp.AVEU, ;
              F36cTotMov With F36cTemp.MOVI, ;
              F36cVolOcu With F36cTemp.VOLU, ;
              F36cKilOcu With F36cTemp.KILO, ;
              F36cKilOcP With F36cTemp.KILP

      *> Leer el siguiente acumulado semanal.
      Select F36cTemp
      Skip
      NumSemana = NumSemana + 1
   EndDo

RETURN

*> Calcular tabla F35S - Información de bultos en SALIDAS. - Versión standard.
PROCEDURE Calcular_Detalle_Salidas_Old

   *> Crear tabla temporal con datos de MACs.
   Wait Clear
   Wait Window 'Recuperando datos de detalle de bultos por tipo SALIDAS ...' NoWait

   Lx_Select = "Select F26vCodPro, F26vTipDoc, F26vNumDoc, F26vTipOri, " + _GCN("Count(*)") + " As Total " + ;
               "From F26v" + _em + Space(1) + ;
               "Where Exists " + ;
               "(Select * From F24c" + _em + Space(1) + ;
               "Where F26vCodPro = F24cCodPro " + ;
               "And   F26vTipDoc = F24cTipDoc " + ;
               "And   F26vNumDoc = F24cNumDoc " + ;
               "And   F26vCodPro = '" + CodPro + "'" + Space(1) + ;
               "And To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "') " + ;
               "Group by F26vCodPro, F26vTipDoc, F26vNumDoc, F26vTipOri"

   Err = f3_SqlExec(_ASql, Lx_Select, 'F35sTemp')
   =SqlMoreResults(_aSql)
   Do Agregar_Error With err, "No se ha podido calcular detalle bultos SALIDAS"
   Select F35sTemp
   Go Top

   *> Crear tabla temporal con datos de cabecera de documentos.
   Wait Clear
   Wait Window 'Recuperando datos de cabecera de documentos SALIDAS ...' NoWait
   Lx_Select = "Select * From F24c" + _em + Space(1) + ;
               "Where F24cCodPro='" + CodPro + "' " + ;
               "And To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "') " + ;
               "Order by F24cCodPro, F24cTipDoc, F24cNumDoc"

   Err = f3_SqlExec(_ASql, Lx_Select, 'F24cTemp')
   =SqlMoreResults(_aSql)
   Do Agregar_Error With err, "No se ha podido calcular detalle documentos SALIDAS"
   Select F24cTemp
   Go Top

   *> Crear tabla temporal con datos de líneas de documentos.
   Wait Clear
   Wait Window 'Recuperando datos de detalle de líneas SALIDAS ...' NoWait
   Lx_Select = "Select F24lCodPro, F24lTipDoc, F24lNumDoc, " + _GCN("Count(*)") + " As Lineas " + ;
               "From F24l" + _em + ", F24c" + _em + Space(1) + ;
               "Where F24lCodPro='" + CodPro + "' " + ;
               "And   F24cCodPro = F24lCodPro " + ;
               "And   F24cTipDoc = F24lTipDoc " + ;
               "And   F24cNumDoc = F24lNumDoc " + ;
               "And To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "') " + ;
               "Group by F24lCodPro, F24lTipDoc, F24lNumDoc"

   Err = f3_SqlExec(_ASql, Lx_Select, 'F24lTemp')
   =SqlMoreResults(_aSql)
   Do Agregar_Error With err, "No se ha podido leer detalle líneas documentos SALIDAS"
   Select F24lTemp
   Go Top

   *> Crear tabla temporal con datos de unidades de fracciones.
   Wait Clear
   Wait Window 'Recuperando datos de unidades fracciones SALIDAS ...' NoWait
   Lx_Select = "Select F20cCodPro, F20cTipDoc, F20cNumDoc, " + _GCN("Sum(F20cCanFis)") + " As CanFra " + ;
               "From F20c" + _em + ", F26v" + _em + ", F26w" + _em + ", F24c" + _em + Space(1) + ;
               "Where F26vCodPro = '" + CodPro + "'" + ;
               "And   F26vTipOri='U' " + ;
               "And   F26vNumMac = F26wNumMac " + ;
               "And   F20cNMovMP = F26wNMovMP " + ;
               "And   F20cTipMov Like '2%'" + ;
               "And   F24cCodPro = F26vCodPro " + ;
               "And   F24cTipDoc = F26vTipDoc " + ;
               "And   F24cNumDoc = F26vNumDoc " + ;
               "And To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "') " + ;
               "Group by F20cCodPro, F20cTipDoc, F20cNumDoc"

   Err = f3_SqlExec(_ASql, Lx_Select, 'F20lTemp')
   =SqlMoreResults(_aSql)
   Do Agregar_Error With err, "No se ha podido leer unidades fracciones SALIDAS"
   Select F20lTemp
   Go Top

   *> Actualización de F35S.
   Select F35sTemp
   Do While !Eof()
      Select F24cTemp
      Locate For F24cCodPro = CodPro .And. ;
                 F24cTipDoc = F35sTemp.F26vTipDoc .And. ;
                 F24cNumDoc = F35sTemp.F26vNumDoc
      If !Found()
         Select F35sTemp
         Skip
         Loop
      EndIf

      Wait Clear
      Wait Window 'Procesando bultos documento ' + F35sTemp.F26vNumDoc NoWait

      *> Leer registro de SALIDAS.
      m.F35sCodPro = CodPro
      m.F35sTipDoc = F35sTemp.F26vTipDoc
      m.F35sNumDoc = F35sTemp.F26vNumDoc
      _ok = f3_seek('F35S')

      Select F35s
      If !_ok
         Zap
         Append Blank
         Replace F35sCodPro With CodPro, ;
                 F35sTipDoc With F35sTemp.F26vTipDoc, ;
                 F35sNumDoc With F35sTemp.F26vNumDoc, ;
                 F35sFecDoc With F24cTemp.F24cFecDoc
      EndIf

      *> Actualizar nº de bultos (palets, cajas, fracciones).
      Do Case
         *> Palet completo.
         Case F35sTemp.F26vTipOri=='N'
            Replace F35sTotPal With F35sTemp.Total

         *> Cajas completas.
         Case F35sTemp.F26vTipOri=='S'
            Replace F35sTotCaj With F35sTemp.Total

         *> Grupos.
         Case F35sTemp.F26vTipOri=='G'
            Replace F35sTotGrp With F35sTemp.Total

         *> Fracciones.
         Case F35sTemp.F26vTipOri=='U'
            Replace F35sTotFra With F35sTemp.Total

         *> Otros.
         OtherWise
            Replace F35sTotVar With F35sTemp.Total
      EndCase

      *> Actualizar Nº de líneas del documento.
      Select F24lTemp
      Locate For F24lCodPro = CodPro .And. ;
                 F24lTipDoc = F35sTemp.F26vTipDoc .And. ;
                 F24lNumDoc = F35sTemp.F26vNumDoc
      If Found()
         Select F35s
         Replace F35sNumLin With F24lTemp.Lineas
      EndIf

      *> Actualizar unidades de fracciones del documento.
      Select F20lTemp
      Locate For F20cCodPro = CodPro .And. ;
                 F20cTipDoc = F35sTemp.F26vTipDoc .And. ;
                 F20cNumDoc = F35sTemp.F26vNumDoc
      If Found()
         Select F35s
         Replace F35sUniFra With F20lTemp.CanFra
      EndIf

      *> Actualizar tabla en ORACLE.
      Select F35s
      If !_ok
         =f3_InsTun('F35S', 'F35S', 'S')
      Else
         _where = "F35sCodPro='" + CodPro + "' " + ;
                  "And F35sTipDoc='" + F35sTipDoc + "' " + ;
                  "And F35sNumDoc='" + F35sNumDoc + "'"
         Err = f3_UpdTun('F35S', , , , , _where, 'N')
         If !Err
            _LxErr = _LxErr + 'Error actualizando F35S' + cr
            =SqlRollBack(_ASql)
            Return .F.
         EndIf
      EndIf

      *> Leer el siguiente documento.
      Select F35sTemp
      Skip
   EndDo

   *>
   Set Relation To

RETURN

*> Calcular tabla F35S - Información de bultos en SALIDAS.
Procedure CalcularDetalleSalidas
Private cWhere, cFromF, cField, cOrder, cGroup
Private nPesoMovimiento, nCantidadEntrada
Private nPesoPalet, nPesoCaja
Private nTotalRegistros

   *> Borrar los registros del mes actual, si hay.
   cWhere = "F35sCodPro='" + CodPro + "' And " + ;
            "To_Char(F35sFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"
   =f3_DelTun('F35S', , cWhere, 'N')

   *> Crear tabla temporal con los bultos de salida.
   Wait Clear
   Wait Window 'Recuperando datos de detalle de bultos por tipo SALIDAS ...' NoWait

   If Used('F35sTemp')
      Use In F35sTemp
   EndIf

   cField = "F20cCodPro, F20cTipDoc, F20cNumDoc, F20cLinDoc, F20cCodArt, F20cNumPal, F20cOrires, " + ;
            "F20cUniPac*F20cPacCaj*F20cCajPal As Factor, " + ;
            "F20cFecMov, F20cCanFis, F20cNumMov"
   cFromF = "F20c"
   cWhere = "F20cCodPro = '" + CodPro + "' And F20cTipMov='2999' And " + ;
            "To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "'"

   *cGroup = "F20cCodPro, F20cTipDoc, F20cNumDoc, F20cLinDoc, F20cNumPal, F20cOriRes, " + ;
   *         "F20cUniPac*F20cPacCaj*F20cCajPal"

   cGroup = ''
   cOrder = "F20cCodPro, F20cTipDoc, F20cNumDoc, F20cLinDoc, F20cNumPal"

   Wait Window 'Cargando datos para cálculo de bultos. Un momento ...' NoWait

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F35sDet')
   Do Agregar_Error With err, "No se ha podido calcular detalle bultos SALIDAS"

   *> Calcular los kilos salida por día.
   Select F35sDet
   Count To nTotalRegistros

   Go Top
   Do While !Eof()
      Wait Window 'Calculando peso movimiento: ' + Str(nTotalRegistros, 6, 0) NoWait

      Select F35TEMP
      Locate For F35cCodPro = CodPro .And. F35cFecVal = DToS(F35sDet.F20cFecMov)
      If Found()
*        nPesoMovimiento = CalcularPesoMovimiento(F35sDet.F20cCodPro, ;
                                                  F35sDet.F20cNumPal, ;
                                                  F35sDet.F20cCanFis)

         nPesoMovimiento = ObtenerPesoMovimiento(F35sDet.F20cCodPro, ;
                                                 F35sDet.F20cNumPal, ;
                                                 F35sDet.F20cCanFis, ;
                                                 F35sDet.F20cNumMov)

         Replace F35cKilSaP With F35cKilSaP + nPesoMovimiento
      EndIf      
      
      nTotalRegistros = nTotalRegistros - 1

      Select F35sDet
      Skip
   EndDo

   *> Preparar cursor para calcular tabla F35s.
   Select F20cCodPro, F20cTipDoc, F20cNumDoc, F20cLinDoc, F20cCodArt, F20cNumPal, F20cOrires, Factor, ;
          Sum(F20cCanFis) As F20cCanFis ;
      From F35sDet ;
      Group By F20cCodPro, F20cTipDoc, F20cNumDoc, F20cLinDoc, F20cCodArt, F20cNumPal, F20cOriRes, Factor ;
      Order By F20cCodPro, F20cTipDoc, F20cNumDoc, F20cLinDoc, F20cNumPal ;
      Into Table F35sTemp
     
   Wait Clear

   Use In F35sDet
   Select F35sTemp

   *> Crear tabla temporal con las cabeceras de los documentos de salida.
   Wait Window 'Recuperando datos de cabeceras de documento SALIDAS ...' NoWait

   Store '' To cOrder, cGroup
   cField = "*"
   cFromF = "F24c"
   cWhere = "F24cCodPro = '" + CodPro + "' And " + ;
            "To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F24cTemp')
   Do Agregar_Error With err, "No se ha podido calcular cabecera documentos SALIDAS"
   Select F24cTemp
   Go Top

   *> Crear tabla temporal con datos de líneas de documentos.
   Wait Clear
   Wait Window 'Recuperando datos de detalle de líneas SALIDAS ...' NoWait

   Store '' To cOrder
   cField = "F24lCodPro, F24lTipDoc, F24lNumDoc, " + _GCN("Count(*)") + " As Lineas"
   cFromF = "F24l,F24c"
   cWhere = "F24lCodPro='" + CodPro + "' And " + ;
            "F24cCodPro = F24lCodPro And " + ;
            "F24cTipDoc = F24lTipDoc And " + ;
            "F24cNumDoc = F24lNumDoc And " + ;
            "To_Char(F24cFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"
   cGroup = "F24lCodPro, F24lTipDoc, F24lNumDoc"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F24lTemp')
   Do Agregar_Error With err, "No se ha podido leer detalle líneas documentos SALIDAS"
   Select F24lTemp
   Go Top

   *>
   Select F35s
   Zap

   *> Actualización de F35S.
   Select F35sTemp
   Go Top
   Do While !Eof()
      Wait Clear
      Wait Window 'Procesando bultos documento ' + F35sTemp.F20cNumDoc NoWait

      *> Datos de documento.
      Select F24cTemp
      Locate For F24cCodPro = F35sTemp.F20cCodPro .And. ;
                 F24cTipDoc = F35sTemp.F20cTipDoc .And. ;
                 F24cNumDoc = F35sTemp.F20cNumDoc
      If !Found()
         *> Documento de salida de otro mes: cargar.
         cWhere = "F24cCodPro='" + F35sTemp.F20cCodPro + "' And " + ;
                  "F24cTipDoc='" + F35sTemp.F20cTipDoc + "' And " + ;
                  "F24cNumDoc='" + F35sTemp.F20cNumDoc + "'"

         If f3_sql("*", "F24c", cWhere, , , 'F24c')
            Select F24c
            Go Top
            Select F24cTemp
            Append From Dbf('F24c')
         EndIf

         *> Cargar las líneas de detalle.
         cWhere = "F24lCodPro='" + F35sTemp.F20cCodPro + "' And " + ;
                  "F24lTipDoc='" + F35sTemp.F20cTipDoc + "' And " + ;
                  "F24lNumDoc='" + F35sTemp.F20cNumDoc + "'"

         If f3_sql("*", "F24l", cWhere, , , 'F24l')
            Select F24l
            Go Top
            Select F24lTemp
            Append From Dbf('F24l')
         EndIf
      EndIf
      
      *> Obtener la cantidad original, pues son palets irregulares.
      nCantidadEntrada = LeerCantidadEntrada(F35sTemp.F20cCodPro, F35sTemp.F20cNumPal)
      If nCantidadEntrada > 0
         Replace F35sTemp.Factor With nCantidadEntrada
      EndIf

      Select F35s
      Locate For F35sCodPro = F35sTemp.F20cCodPro .And. ;
                 F35sTipDoc = F35sTemp.F20cTipDoc .And. ;
                 F35sNumDoc = F35sTemp.F20cNumDoc

      If !Found()
         Append Blank
         Replace F35sCodPro With F35sTemp.F20cCodPro, ;
                 F35sTipDoc With F35sTemp.F20cTipDoc, ;
                 F35sNumDoc With F35sTemp.F20cNumDoc, ;
                 F35sFecDoc With F24cTemp.F24cFecDoc, ;
                 F35sNumLin With 0, ;
                 F35sTotPal With 0, ;
                 F35sTotCaj With 0, ;
                 F35sTotGrp With 0, ;
                 F35sTotFra With 0, ;
                 F35sTotVar With 0, ;
                 F35sUniFra With 0, ;
                 F35sTotPk1 With 0, ;
                 F35sTotPk2 With 0
      EndIf

      *> Actualizar nº de bultos (palets, cajas, grupos, fracciones).
      Do Case
         *> Fracciones.
         *Case F35sTemp.F20cOriRes=='U'
         *   Replace F35sTotFra With F35sTotFra + F35sTemp.F20cCanFis

         *> Grupos.
         *Case F35sTemp.F20cOriRes=='G'
         *   Replace F35sTotGrp With F35sTotGrp + F35sTemp.F20cCanFis

         *> Palets completos.
         Case F35sTemp.F20cCanFis >= F35sTemp.Factor
            Replace F35sTotPal With F35sTotPal + F35sTemp.F20cCanFis

         *> Picking.
         Case F35sTemp.F20cCanFis < F35sTemp.Factor
            Replace F35sTotCaj With F35sTotCaj + F35sTemp.F20cCanFis

            *> Calcular cajas picking tipos 1 y 2.
            If F32c.F32cKilMin > 0
               Do Case
                  *> Calcular por peso palet (F16T)
                  Case F32c.F32cTipKil=='P'
                     nPesoPalet = CalcularPesoMovimiento(F35sTemp.F20cCodPro, ;
                                                         F35sTemp.F20cNumPal, ;
                                                         nCantidadEntrada)

                     nPesoCaja = Round(nPesoPalet / nCantidadEntrada, 0)

                  *> Calcular por peso unitario de artículo.
                  Case F32c.F32cTipKil=='A'
                     Select F08Temp
                     Seek CodPro + F35sTemp.F20cCodArt
                     nPesoCaja = Round(F08cPesUni / 1000, 0)
               EndCase

               Select F35s
               If nPesoCaja <= F32c.F32cKilMin
                  Replace F35sTotPk1 With F35sTotPk1 + F35sTemp.F20cCanFis
               Else
                  Replace F35sTotPk2 With F35sTotPk2 + F35sTemp.F20cCanFis
               EndIf
            EndIf

         *> Otros.
         OtherWise
            Replace F35sTotVar With F35sTotVar + F35sTemp.F20cCanFis
      EndCase

      *> Leer el siguiente documento.
      Select F35sTemp
      Skip
   EndDo

   *> Actualizar el nº de líneas de detalle de los documentos de salida.
   Select F35s
   Go Top
   Do While !Eof()   
      Select F24lTemp
      Locate For F24lCodPro = F35sTemp.F20cCodPro .And. ;
                 F24lTipDoc = F35sTemp.F20cTipDoc .And. ;
                 F24lNumDoc = F35sTemp.F20cNumDoc
      If Found()
         Select F35s
         Replace F35sNumLin With F24lTemp.Lineas
      EndIf

      *> Actualizar la tabla en ORACLE.
      Select F35s
      =f3_InsTun('F35S', 'F35S', 'S')

      *>
      Select F35s
      Skip
   EndDo

   *>
   Wait Clear
   Set Relation To

   *> Borrar tablas temporales.
   If Used('F35sTemp')
      Use In F35sTemp
      Delete File 'F35sTemp.Dbf'
   EndIf
   If Used('F24lTemp')
      Use In F24lTemp
   EndIf
   If Used('F24cTemp')
      Use In F24cTemp
   EndIf

RETURN

*> Calcular tabla F35e - Información de palets en ENTRADAS.
PROCEDURE Calcular_Detalle_Entradas_Old

   *> Crear tabla temporal con datos de documentos de entrada.
   Wait Clear
   Wait Window 'Recuperando datos de documentos ENTRADAS ...' NoWait
   Lx_Select = "Select F20cCodPro, F20cNumEnt, F20cTipDoc, F20cNumDoc, F20cFecMov, F20cLinDoc, " + ;
               "       F20cCodArt, F20cNumLot, F20cNumPal, F20cCanFis, " + ;
               _GCN("F08cUniPac * F08cPacCaj * F08cCajPal") + " As FacPal " + ;
               "From F20c" + _em + ", F08c" + _em + ", F16t" + _em + Space(1) + ;
               "Where F20cCodPro='" + CodPro + "' " + ;
               "And   F20cTipMov Like '1%'" + ;
               "And   F20cFecMov >= To_Date('" + fechat + "', 'dd/mm/yyyy') " + ;
               "And F08cCodPro=F20cCodPro " + ;
               "And F08cCodArt=F20cCodArt " + ;
               "And F16tNumPal=F20cNumPal"

   Err = f3_SqlExec(_ASql, Lx_Select, 'F20lTemp')
   =SqlMoreResults(_aSql)
   Do Agregar_Error With err, "No se ha podido leer datos documentos ENTRADAS"

   *> Eliminar registros que NO sean del mes de cálculo.
   Select F20lTemp
   Delete For Month(F20cFecMov) # Month(CToD(fechat)) .Or. ;
          Empty(F20cNumEnt) .Or. ;
          IsNull(F20cTipDoc)

   *> Relacionar registro de ocupaciones con ficha artículo.
   Go Top
   Set Relation To F20cCodPro + F20cCodArt Into F08Temp

   *> Crear cursor para contar las líneas de documento.
   Select F20cCodPro, ;
          F20cTipDoc, ;
          F20cNumDoc, ;
          F20cFecMov, ;
          Count(*) As LinEnt ;
      From F20lTemp ;
      Into Cursor _CF20L ;
      Group By F20cCodPro, ;
               F20cTipDoc, ;
               F20cNumDoc, ;
               F20cFecMov

   *> Crear cursor para contar los palets de cada documento.
   Select F20cCodPro, ;
          F20cTipDoc, ;
          F20cNumDoc, ;
          F20cNumPal ;
      From F20lTemp ;
      Into Cursor _CF20P ;
      Group By F20cCodPro, ;
               F20cTipDoc, ;
               F20cNumDoc, ;
               F20cNumPal ;
      Order By F20cCodPro, ;
               F20cTipDoc, ;
               F20cNumDoc, ;
               F20cNumPal

   *> Actualizar tabla de ENTRADAS.
   Select _CF20L
   Go Top
   Do While !Eof()
      Wait Clear
      Wait Window 'Procesando detalle entradas documento ' + _CF20L.F20cNumDoc NoWait

      *> Contar palets completos, picos y unidades en picos.
      Select F20lTemp

      *> Este valor no es válido aquí. Se calcula con _CF20P.
      Count For F20cCodPro = CodPro .And. ;
                F20cTipDoc = _CF20L.F20cTipDoc .And. ;
                F20cNumDoc = _CF20L.F20cNumDoc ;
         To _TotPal

      Count For F20cCodPro = CodPro .And. ;
                F20cTipDoc = _CF20L.F20cTipDoc .And. ;
                F20cNumDoc = _CF20L.F20cNumDoc .And. ;
                F20cCanFis < FacPal ;
         To _TotFra

      Sum F20cCanFis ;
         For F20cCodPro = CodPro .And. ;
             F20cTipDoc = _CF20L.F20cTipDoc .And. ;
             F20cNumDoc = _CF20L.F20cNumDoc .And. ;
             F20cCanFis < FacPal ;
         To _UniFra

      *> Leer registro de ENTRADAS.
      m.F35eCodPro = CodPro
      m.F35eTipDoc = _CF20L.F20cTipDoc
      m.F35eNumDoc = _CF20L.F20cNumDoc
      _ok = f3_seek('F35E')

      Select F35e
      If !_ok
         Zap
         Append Blank
         Replace F35eCodPro With CodPro, ;
                 F35eTipDoc With _CF20L.F20cTipDoc, ;
                 F35eNumDoc With _CF20L.F20cNumDoc, ;
                 F35eFecDoc With _CF20L.F20cFecMov
      EndIf

      *> Actualizar F35E.
      Replace F35eNumLin With F35eNumLin + _CF20L.LinEnt, ;
              F35eTotPal With _TotPal, ;
              F35eTotFra With _TotFra, ;
              F35eUniFra With _UniFra

      *> Actualizar el Nº de palets del documento.
      Select _CF20P
      Count For F20cCodPro = CodPro .And. ;
                F20cTipDoc = _CF20L.F20cTipDoc .And. ;
                F20cNumDoc = _CF20L.F20cNumDoc To _TotPal

      Select F35E
      Replace F35eTotPal With _TotPal

      *> Actualizar tabla en ORACLE.
      If !_ok
         =f3_InsTun('F35E', 'F35E', 'N')
      Else
         _where = "F35eCodPro='" + CodPro + "' " + ;
                  "And F35eTipDoc='" + F35eTipDoc + "' " + ;
                  "And F35eNumDoc='" + F35eNumDoc + "'"
         Err = f3_updtun('F35E', , , , , _where, 'N')
         If !Err
            _LxErr = _LxErr + 'Error actualizando F35E' + cr
            =SqlRollBack(_ASql)
            Return .F.
         EndIf
      EndIf

      *> Leer el siguiente documento.
      Select _CF20L
      Skip
   EndDo

   *>
   Set Relation To

RETURN

*> Calcular tabla F35e - Información de palets en ENTRADAS.
PROCEDURE CalcularDetalleEntradas
Private cWhere, cFromF, cField, cOrder, cGroup

   *> Borrar los registros del mes actual, si hay.
   cWhere = "F35eCodPro='" + CodPro + "' And " + ;
            "To_Char(F35eFecDoc, 'YYYYMM')='" + AnoT + MesT + "'"
   =f3_DelTun('F35E', , cWhere, 'N')

   *> Crear tabla temporal con datos de documentos de entrada.
   Wait Clear
   Wait Window 'Recuperando datos de documentos ENTRADAS ...' NoWait

   Store '' To cOrder, cGroup
   cField = "F20cCodPro, F20cNumEnt, F20cTipDoc, F20cNumDoc, F20cFecMov, F20cLinDoc, " + ;
            "F20cCodArt, F20cNumLot, F20cNumPal, F20cCanFis, " + ;
            _GCN("F08cUniPac * F08cPacCaj * F08cCajPal") + " As FacPal"
   cFromF = "F20c,F08c,F16t"
   cWhere = "F20cCodPro='" + CodPro + "' And " + ;
            "F20cTipMov Like '1%' And " + ;
            "To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "' And " + ;
            "F08cCodPro=F20cCodPro And " + ;
            "F08cCodArt=F20cCodArt And " + ;
            "F16tNumPal(+)=F20cNumPal"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F20lTemp')
   Do Agregar_Error With err, "No se ha podido leer datos documentos ENTRADAS"

   *> Eliminar registros que NO sean del mes de cálculo.
   Select F20lTemp
   Delete For Empty(F20cNumEnt) .Or. IsNull(F20cTipDoc)

   *> Relacionar registro de ocupaciones con ficha artículo.
   Go Top
   Set Relation To F20cCodPro + F20cCodArt Into F08Temp

   *> Crear cursor para contar las líneas de documento.
   Select F20cCodPro, ;
          F20cTipDoc, ;
          F20cNumDoc, ;
          F20cFecMov, ;
          Count(*) As LinEnt ;
      From F20lTemp ;
      Into Cursor _CF20L ;
      Group By F20cCodPro, ;
               F20cTipDoc, ;
               F20cNumDoc, ;
               F20cFecMov

   *> Crear cursor para contar los palets de cada documento.
   Select F20cCodPro, ;
          F20cTipDoc, ;
          F20cNumDoc, ;
          F20cNumPal ;
      From F20lTemp ;
      Into Cursor _CF20P ;
      Group By F20cCodPro, ;
               F20cTipDoc, ;
               F20cNumDoc, ;
               F20cNumPal ;
      Order By F20cCodPro, ;
               F20cTipDoc, ;
               F20cNumDoc, ;
               F20cNumPal

   *>
   Store 0 To _TotPal
   Select F35e
   Zap

   *> Actualizar tabla de ENTRADAS.
   Select _CF20L
   Go Top
   Do While !Eof()
      Wait Clear
      Wait Window 'Procesando detalle entradas documento ' + _CF20L.F20cNumDoc NoWait

      *> Contar palets completos, picos y unidades en picos.
      Select F20lTemp

      Count For F20cCodPro = CodPro .And. ;
                F20cTipDoc = _CF20L.F20cTipDoc .And. ;
                F20cNumDoc = _CF20L.F20cNumDoc .And. ;
                F20cCanFis < FacPal ;
         To _TotFra

      Sum F20cCanFis ;
         For F20cCodPro = CodPro .And. ;
             F20cTipDoc = _CF20L.F20cTipDoc .And. ;
             F20cNumDoc = _CF20L.F20cNumDoc .And. ;
             F20cCanFis < FacPal ;
         To _UniFra

      Select F35e
      Locate For F35eCodPro = _CF20L.F20cCodPro .And. ;
                 F35eTipDoc = _CF20L.F20cTipDoc .And. ;
                 F35eNumDoc = _CF20L.F20cNumDoc
      If !Found()
         Append Blank
         Replace F35eCodPro With _CF20L.F20cCodPro, ;
                 F35eTipDoc With _CF20L.F20cTipDoc, ;
                 F35eNumDoc With _CF20L.F20cNumDoc, ;
                 F35eFecDoc With _CF20L.F20cFecMov
      EndIf

      *> Actualizar F35E.
      Replace F35eNumLin With F35eNumLin + _CF20L.LinEnt, ;
              F35eTotPal With _TotPal, ;
              F35eTotFra With _TotFra, ;
              F35eUniFra With _UniFra

      *> Actualizar el Nº de palets del documento.
      Select _CF20P
      Count For F20cCodPro = _CF20L.F20cCodPro .And. ;
                F20cTipDoc = _CF20L.F20cTipDoc .And. ;
                F20cNumDoc = _CF20L.F20cNumDoc To _TotPal

      Select F35E
      Replace F35eTotPal With _TotPal

      *> Leer el siguiente documento.
      Select _CF20L
      Skip
   EndDo

   *> Actualizar tabla en ORACLE.
   Select F35E
   Go Top
   Do While !Eof()
      =f3_InsTun('F35E', 'F35E', 'S')

      Select F35E
      Skip
   EndDo

   *>
   Set Relation To

   *> Eliminar las tablas temporales.
   If Used('F20lTemp')
      Use In F20lTemp
   EndIf
   If Used('_CF20P')
      Use In _CF20P
   EndIf
   If Used('_CF20L')
      Use In _CF20L
   EndIf

RETURN

*> Generar todos los registros del F35c correspondientes al mes de cálculo.
Procedure GenerarF35c

   Select F35TEMP
   Delete All

   _Date = CToD('01' +  '/' + MesT + '/' + AnoT)

   Do While Month(_Date) = Val(MesT)
      Select F35TEMP
      Append Blank
      Replace F35cCodPro With CodPro, ;
              F35cFecVal With DToS(_Date)

      *> Actualizar el peso palet/día por pesaje-F16T.
      Wait Window 'Leyendo pesaje palets día: ' + DToC(_Date) NoWait

***   "    And   To_Char(F20cFecMov, 'YYYYMMDD')='" + DToS(_Date) + "'" + ;

      Lx_Select = "Select " + _GCN("Sum(F16tPesPal)") + " As PesoPalet " + ;
                  "From   F16t" + _em + Space(1) + ;
                  "Where  F16tNumPal In " + ;
                  "   (Select Unique F20cNumPal From F20c" + _em + ;
                  "    Where F20cCodPro='" + CodPro + "'" + ;
                  "    And   F20cFecMov=To_Date('" + DToS(_Date) + "'" + ", 'YYYYMMDD')" + ;
                  "    And   " + _GCSS("F20cTipMov", 1, 1) + "='1'" + ;
                  "    And   F20cNumEnt > '0'" + ;
                  "    And   F20cNumPal=F16tNumPal)"

      Err = f3_SqlExec(_aSql,Lx_Select,'F16TTEMP')
      =SqlmoreResults(_aSql)
      Do Agregar_Error With err,"No se ha podido acceder al F16T para recoger pesos de palet(2)"

      Select F35TEMP
      Replace F35cKilEnP With F16TTEMP.PesoPalet

      Use In F16TTEMP
      Wait Clear
      _Date = _Date + 1
   EndDo

Return

*>
*> Calcular el F35c para los días SIN movimientos.
Procedure ActualizarF35c

   Private dFechaMinima
   Local nValStk, nKilStk

   If Type('oF35c') # 'U'
      Release oF35c
   EndIf

   *> Obtener la fecha del primer movimiento de entrada, para actualizar
   *> solo hasta dicha fecha, si ésta es del mes de cálculo.
   dFechaMinima = ObtenerFechaMinima(CodPro)
   If IsNull(dFechaMinima)
      dFechaMinima = _FecMin
   EndIf

   *> Valores iniciales de F35cValStk y F35cKilStk.
   Store 0 To nValStk, nKilStk

   Select F35TEMP
   Locate For F35cKilStk > 0    && F35cTotMov > 0
   Do While Found()
      nValStk = F35cValStk
      nKilStk = F35cKilStk
      Continue
   EndDo

   Go Bottom
   Do While !Bof() .And. F35cFecVal >= DToS(dFechaMinima)
      If F35cTotMov = 0
         If Type('oF35c') # 'O'
            Select F34TEMP
            Go Top
            Replace F35TEMP.F35cOcuFri With F34cOcuFrf, ;
                    F35TEMP.F35cUbiFri With F34cUbiFrf, ;
                    F35TEMP.F35cOcuAmb With F34cOcuAmf, ;
                    F35TEMP.F35cUbiAmb With F34cUbiAmf, ;
                    F35TEMP.F35cOcuDrg With F34cOcuDrf, ;
                    F35TEMP.F35cUbiDrg With F34cUbiDrf, ;
                    F35TEMP.F35cOcuRes With F34cOcuRef, ;
                    F35TEMP.F35cUbiRes With F34cUbiRef
         Else
            Gather Name oF35c
         EndIf
      EndIf

      Select F35TEMP
      If F35cValStk = 0
         Replace F35cValStk With nValStk, ;
                 F35cKilStk With nKilStk
      EndIf

      Scatter Name oF35c Fields F35cOcuFri, ;
                                F35cUbiFri, ;
                                F35cOcuAmb, ;
                                F35cUbiAmb, ;
                                F35cOcuDrg, ;
                                F35cUbiDrg, ;
                                F35cOcuRes, ;
                                F35cUbiRes, ;
                                F35cValStk, ;
                                F35cKilStk
      Select F35TEMP
      Skip -1
   EndDo

   If Type('oF35c') # 'U'
      Release oF35c
   EndIf

Return

***********************

   _Date = CToD('01' +  '/' + MesT + '/' + AnoT)
   If Type('RF35c') == 'O'
      Release RF35c
   EndIf

   Do While Month(_Date) = Val(MesT)
      Select F35TEMP
      Locate For F35cCodPro = CodPro .And. ;
                 F35cFecVal = DToS(_Date)

      If Found()
         *> Memoriza registro a "arrastrar".
         If F35cTotMov > 0
            Scatter Name RF35c Fields F35cOcuFri, ;
                                      F35cUbiFri, ;
                                      F35cOcuAmb, ;
                                      F35cUbiAmb, ;
                                      F35cOcuDrg, ;
                                      F35cUbiDrg, ;
                                      F35cOcuRes, ;
                                      F35cUbiRes
         Else
            If Type('RF35c') == 'O'
               Gather Name RF35c
            Else
               *> Calcular ocupación a partir de F34TEMP
               Select F34TEMP
               Go Top
               Replace F35TEMP.F35cOcuFri With F34cOcuFri, ;
                       F35TEMP.F35cUbiFri With F34cUbiFri, ;
                       F35TEMP.F35cOcuAmb With F34cOcuAmi, ;
                       F35TEMP.F35cUbiAmb With F34cUbiAmi, ;
                       F35TEMP.F35cOcuDrg With F34cOcuDri, ;
                       F35TEMP.F35cUbiDrg With F34cUbiDri, ;
                       F35TEMP.F35cOcuRes With F34cOcuRei, ;
                       F35TEMP.F35cUbiRes With F34cUbiRei

               *> Memoriza registro a "arrastrar".
               Select F35TEMP
               Scatter Name RF35c Fields F35cOcuFri, ;
                                         F35cUbiFri, ;
                                         F35cOcuAmb, ;
                                         F35cUbiAmb, ;
                                         F35cOcuDrg, ;
                                         F35cUbiDrg, ;
                                         F35cOcuRes, ;
                                         F35cUbiRes
            EndIf
         EndIf
      Else
         *> Error !!!
      EndIf

      *> Siguiente día
      _Date = _Date + 1
   EndDo

Return

*> Actualizar datos en las tablas reales de ORACLE.
PROCEDURE ActualizarDataBase

    *> Actualizar acumulados mensuales.
	Select F34TEMP
	go top
	err=F3_InsTun("F34c", "F34TEMP", "N")
	If err=.F. 
		do Agregar_Error With 0,"No se ha podido guardar la información en el F34C"
	EndIf

    *> Actualizar registro ocupaciones diarias.
	Select F35TEMP
	Go Top
    Do While !Eof()
        err=F3_InsTun("F35c", "F35TEMP", "N")
        If err=.F.
           Do Agregar_Error With 0,"No se ha podido guardar la información en el F35C"
		EndIf
        Select F35TEMP
		Skip
	EndDo

    *> Generar temporal de acumulados semanales.
    =ActualizarRegistroSemanal()
    Use In DiasSem
    Wait Clear

    *> Actualizar registro de acumulados semanales.
 	Select F36TEMP
	Go Top

	*> Proceso de búsqueda si la semana era incompleta y se ha sumado al mes siguiente
	Do While !Eof()
		m.F36cCodPro = F36TEMP.F36cCodPro
		m.F36cFecVal = F36TEMP.F36cFecVal

		Ok = f3_seek("F36C")
		If Ok
            Select F36c
            Go Top
			Where = "F36cCodPro = '" + F36TEMP.F36cCodPro + _cm + ;
			        " And F36cFecVal = '" + F36TEMP.F36cFecVal + _cm			

            *> Se hacen los reemplazamientos de datos
			Select F36TEMP
			Replace F36cOcuOcu With F36cOcuOcu + F36C.F36cOcuOcu,  ; 
			F36cUbiUbi With F36cUbiUbi + F36C.F36cUbiUbi,  ; 
			F36cPalMax With Iif(F36cPalMax>F36C.F36cPalMax,F36cPalMax,F36C.F36cPalMax)  ; 
			F36cUbiMax With Iif(F36cUbiMax>F36C.F36cUbiMax,F36cUbiMax,F36C.F36cUbiMax)  ; 
			F36cPalAvg With (F36cPalAvg + F36C.F36cPalAvg) / 2,  ; 
			F36cUbiAvg With (F36cUbiAvg + F36C.F36cUbiAvg) / 2,  ; 
			F36cTotMov With F36cTotMov + F36C.F36cTotMov,  ; 
			F36cVolOcu With F36cVolOcu + F36C.F36cVolOcu,  ; 
			F36cKilOcu With F36cKilOcu + F36C.F36cKilOcu

			err= F3_UpdTun("F36C", , , , "F36TEMP", where, "N")			
		Else
			err=F3_InsTun("F36C", "F36TEMP", "N")
		EndIf

		If err=.F. 
			Do Agregar_Error With 0,"No se ha podido guardar la información en el F36C"
		EndIf
        Select F36TEMP
		Skip
	EndDo

	Use In F08TEMP
	Use In F16TEMP
	Use In F20TEMP
	Use In F34TEMP
	Use In F35TEMP
	Use In F36TEMP

*   Use In F2016TEMP

    =SqlCommit(_ASql)

Return

PROCEDURE Agregar_Error
parameters valor_error,nuevo_error

	If valor_error < 1 
		_lxerr = _lxerr + nuevo_error + cr
        =SqlRollBack(_ASql)
	EndIf
	
RETURN

FUNCTION Busqueda

	Err = f3_SqlExec(_aSql,Lx_Select,'BUSQUEDA')
	=SqlmoreResults(_aSql)
	If Err <= 0
  		Return .F.
	EndIf

	Select BUSQUEDA
	Go Top
	If Eof()
        Use In BUSQUEDA
		Return .F.
   	EndIf
   	Use In BUSQUEDA

RETURN .T.

*> Obtener la fecha de salida del almacén de un palet que ya no está en ocupaciones.
*> Devuelve NULL si no se encuentra el movimiento de salida, o si éste se ha
*> producido por una regularización de inventario.
*>
*> No tener en cuenta la regularización. AVC - 26.05.2001

Function FechaSalidaPalet
Parameters cNumeroPalet
Private cWhere, cOrder, cGroup
Local dFecha

   Store '' To cGroup

   *>   cWhere = "F20cNumPal='" + cNumeroPalet + "' And F20cEntSal='S' And F20cTipMov Like '2%'"

   cWhere = "F20cNumPal='" + cNumeroPalet + "' And F20cEntSal='S'"
   cOrder = "F20cFecMov DESC"
   =f3_sql('*', 'F20c', cWhere, cOrder, cGroup, 'F20cDate')

   Select F20cDate
   Go Top
   dFecha = Iif(Eof(), Null, F20cFecMov)

   Use In F20cDate

Return dFecha

*>
*> Obtener el peso de un movimiento (en principio de salida) de la tabla F14s.
*> Si no existe se calcula y se graba en F14s. AVC - 31.05.2001
*>
*> Recibe: cNumMov ---> Nº movimiento en F20c.

Function ObtenerPesoMovimiento
Parameters cCodPro, cNumPal, nCanFis, cNumMov

Private cField, cFromF, cWhere
Local lEstado
Local nPesoMovimiento
Local cOldSelect, nF20cCanFis

   cOldSelect = Select()

   Store 0 To nPesoMovimiento

   *> Leer el peso del F14s, si existe.
   m.F14sNumMov = cNumMov
   lEstado = f3_seek('F14S')
   If lEstado
      *> Existe: Tomar peso de F14s.
      Select F14s
      Go Top
      nPesoMovimiento = F14sPesMov
   Else
      *> No existe: Calcular peso movimiento y grabar.
      nPesoMovimiento = CalcularPesoMovimiento(cCodPro, cNumPal, nCanFis)
      Select F14s
      Zap
      Append Blank
      Replace F14sNumMov With cNumMov, ;
              F14sPesMov With nPesoMovimiento, ;
              F14sEstado With '0', ;
              F14sFlag1  With '0', ;
              F14sFlag2  With '0'
      =f3_InsTun('F14S', 'F14S', 'S')
   EndIf

   Select (cOldSelect)

Return nPesoMovimiento

*>
*> Calcular el peso (aproximado) de un movimiento, generalmente de salida.
*> Busca el peso de entrada (de F16t), el movimiento de entrada (F20c), para
*> obtener la cantidad inicial, y calcula el peso de la cantidad recibida.
*>
*> Recibe: cCodPro ---> Propietario.
*>         cNumPal ---> Nº de palet.
*>         nCanFis ---> Cantidad movimiento.

Function CalcularPesoMovimiento
Parameters cCodPro, cNumPal, nCanFis

Private cField, cFromF, cWhere
Local nPesoMovimiento
Local cOldSelect, nF20cCanFis

   cOldSelect = Select()
   Store 0 To nPesoMovimiento

   If Type('FechaT') # 'C'
      FechaT = DToC(Date())
   EndIf

   *> Generar un cursor con el peso de los palets del propietario/periodo.
   If !Used('F2016TEMP')
      =CargarPesoPalets(cCodPro, FechaT)
   EndIf

   Select F2016TEMP
   Locate For F20cCodPro==cCodPro .And. F20cNumPal==cNumPal
   If !Found()
      *> Agregar el nuevo palet a la lista de palets.
      =CargarPesoDeUnPalet(cCodPro, cNumPal)
   EndIf

   *> Calcular el peso de la cantidad recibida como parámetro.
   Scatter Name oF16t
   Sum(F20cCanFis) To oF16t.F20cCanFis For F20cCodPro==cCodPro .And. F20cNumPal==cNumPal
   nPesoMovimiento = Iif(oF16t.F20cCanFis==0, 0, Round((nCanFis * oF16t.F16tPesPal / oF16t.F20cCanFis), 0))

   *> Recuperar entorno de trabajo.
   Select (cOldSelect)

Return nPesoMovimiento

*>
*> Calcular el peso (aproximado) de los palets del propietario/mes de cálculo.
*>
*> Genera el cursor F2016TEMP.

Function CargarPesoPalets
Parameters cCodPro, cFecha

Private cField, cFromF, cWhere
Local cOldSelect

   cOldSelect = Select()

   If Used('F2016TEMP')
      Use In F2016TEMP
   EndIf

   Wait Window 'Cargando datos peso entrada palets. Un momento ...' NoWait

   *> "To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "' And " + ;

   *> Leer el peso del palet cuando entró.
   cField = "F20cCodPro, F20cNumPal, F20cCanFis, " + _GCN("F16tPesPal") + " As F16tPesPal"
   cFromF = "F20c,F16t"
*!*	   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
*!*	            "F20cFecMov >=" + _GCD(cFecha) + " And " + ;
*!*	            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
*!*	            "F20cNumEnt > '0' And " + ;
*!*	            "F16tNumPal(+)=F20cNumPal"

   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
            "F20cFecMov >=" + _GCD(cFecha) + " And " + ;
            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
            "F20cNumEnt > '0' And " + ;
            "F16tNumPal=F20cNumPal"

   =f3_sql(cField, cFromF, cWhere, , , 'F2016TEMP')

   Select F2016TEMP
   Go Top

   Wait Clear

   *> Recuperar entorno de trabajo.
   Select (cOldSelect)

Return

*>
*> Calcular el peso (aproximado) de un palet y lo añade al cursor F2016TEMP.

Function CargarPesoDeUnPalet
Parameters cCodPro, cNumPal

Private cField, cFromF, cWhere
Local cOldSelect

   cOldSelect = Select()

   *> Leer el peso del palet cuando entró.
   cField = "F20cCodPro, F20cNumPal, F20cCanFis, " + _GCN("F16tPesPal") + " As F16tPesPal"
   cFromF = "F20c,F16t"

   *   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;

   cWhere = "F20cNumPal='" + cNumPal + "' And " + ;
            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
            "F20cNumEnt > '0' And " + ;
            "F16tNumPal=F20cNumPal"

   =f3_sql(cField, cFromF, cWhere, , , '_F16t')
   Select _F16t
   Go Top

   *> No existe el palet. Esto debería de ser un error.
   If Eof()
      Append Blank
      Replace F20cCodPro With cCodPro, ;
              F20cNumPal With cNumPal, ;
              F20cCanFis With 0, ;
              F16tPesPal With 0
   EndIf

   If !Used('F2016TEMP')
      Copy Structure To F2016TEMP
      Select 0
      Use F2016TEMP
   EndIf

   Select F2016TEMP
   Append From Dbf('_F16t')

   *> Recuperar entorno de trabajo.
   Use In _F16t
   Select (cOldSelect)

Return

*>
*> Calcular el peso (aproximado) de un palet y lo añade al cursor F2016TEMP.

Function CargarPalet
Parameters cCodPro, cNumPal

Private cField, cFromF, cWhere
Local cOldSelect

   cOldSelect = Select()

   *> Leer el peso del palet cuando entró.
   cField = "F20cCodPro, F20cNumPal, F20cCanFis," +  _GCN("F16tPesPal", 0) + " As F16tPesPal"
   cFromF = "F20c,F16t"

   *   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;

*!*	   cWhere = "F20cNumPal='" + cNumPal + "' And " + ;
*!*	            "F20cEntSal = 'E' And " + ;
*!*	            "F16tNumPal(+)=F20cNumPal"

   cWhere = "F20cNumPal='" + cNumPal + "' And " + ;
            "F20cEntSal = 'E' And " + ;
            "F16tNumPal =F20cNumPal"

   cGroup = "F20cCodPro,F20cNumPal,F20cCanfis,F16tPesPal"

   =f3_sql(cField, cFromF, cWhere, ,cGroup , '_F16t')
   Select _F16t
   Go Top

   *> No existe el palet. Esto debería de ser un error.
   If Eof()
      Append Blank
      Replace F20cCodPro With cCodPro, ;
              F20cNumPal With cNumPal, ;
              F20cCanFis With 0, ;
              F16tPesPal With 0
   EndIf

   If !Used('F2016TEMP')
      Copy Structure To F2016TEMP
      Select 0
      Use F2016TEMP
   EndIf

   Select F2016TEMP
   Append From Dbf('_F16t')

   *> Recuperar entorno de trabajo.
   Use In _F16t
   Select (cOldSelect)

Return

*>
*> Obtener la fecha mas pequeña de movimientos de entrada de un propietario.
*>   Recibe: cCodPro -----> Propietario.
*>           cCodArt -----> Artículo (Opcional).
*>
*> Devuelve: dFechaM -----> Fecha mínima (o NULL, si no hay movimientos).

Procedure ObtenerFechaMinima
Parameters cCodPro, cCodArt

Private dFechaM
Private cWhere, cOrder

   cOrder = "F20cFecMov"
   cWhere = "F20cCodPro='" + cCodPro + "'" + ;
            Iif(Type('cCodArt')=='C', " And F20cCodArt='" + cCodArt + "'", "") + " And " + ;
            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
            "F20cNumEnt > '0'"
   =f3_sql("*", "F20c", cWhere, cOrder, , '_F20cFecha')

   Select _F20cFecha
   Go Top
   dFechaM = IIf(!Eof(), F20cFecMov, Null)

   Use In _F20cFecha

Return dFechaM

*>
*> Obtener la cantidad con la que entró un palet.
*>
*> Recibe: cCodPro ---> Propietario.
*>         cNumPal ---> Nº de palet.

Function LeerCantidadEntrada
Parameters cCodPro, cNumPal

Private cField, cFromF, cWhere
Local nCantidad
Local cOldSelect, nF20cCanFis

   cOldSelect = Select()
   Store 0 To nCantidad

   *> Leer el peso del palet cuando entró.
   cField = "F20cCodPro, F20cNumPal, F20cCanFis"
   cFromF = "F20c"

   *> Eliminar referencia al propietario, pues enlentece mucho. AVC - 23.07.2001

   *   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
   *            "F20cNumPal='" + cNumPal + "' And " + ;
   *            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
   *            "F20cNumEnt > '0'"

   cWhere = "F20cNumPal='" + cNumPal + "' And " + ;
            _GCSS("F20cTipMov", 1, 1) + "='1' And " + ;
            "F20cNumEnt > '0'"

   =f3_sql(cField, cFromF, cWhere, , , '_F20c')
   Select _F20c
   Go Top

   *> No se encontró el palet de entrada. (Debería se un error).
   If Eof()
      Append Blank
      Replace F20cCodPro With cCodPro, ;
              F20cNumPal With cNumPal, ;
              F20cCanFis With 0
   Else
      Sum F20cCanFis To nF20cCanFis
      Go Top
      Replace F20cCanFis With nF20cCanFis
   EndIf

   *> Calcular el peso de la cantidad recibida como parámetro.
   Scatter Name oF20c
   nCantidad = oF20c.F20cCanFis

   *> Recuperar entorno de trabajo.
   If Used('_F20c')
      Use In _F20c
   EndIf

   Select (cOldSelect)

Return nCantidad

*>
*> Calcular el peso (aproximado) de un movimiento, generalmente de salida.
*> Busca el peso de entrada (de F16t), el movimiento de entrada (F20c), para
*> obtener la cantidad inicial, y calcula el peso de la cantidad recibida.
*>
*> Recibe: cCodPro ---> Propietario.
*>         cNumPal ---> Nº de palet.
*>         nCanFis ---> Cantidad movimiento.

Function CalcPesoMovimiento
Parameters cCodPro, cNumPal, nCanFis

Private cField, cFromF, cWhere
Local nPesoMovimiento
Local cOldSelect, nF20cCanFis

   cOldSelect = Select()
   Store 0 To nPesoMovimiento

   If Type('FechaT') # 'C'
      FechaT = DToC(Date())
   EndIf

   *> Generar un cursor con el peso de los palets del propietario/periodo.
   If !Used('F2016TEMP')
      =PesoPalets(cCodPro, FechaT)
   EndIf

   Select F2016TEMP
   Locate For F20cCodPro==cCodPro .And. F20cNumPal==cNumPal
   If !Found()
      *> Agregar el nuevo palet a la lista de palets.
      =CargarPalet(cCodPro, cNumPal)
   EndIf

   *> Calcular el peso de la cantidad recibida como parámetro.
   Scatter Name oF16t
   Sum(F20cCanFis) To oF16t.F20cCanFis For F20cCodPro==cCodPro .And. F20cNumPal==cNumPal
*   nPesoMovimiento = Iif(oF16t.F20cCanFis==0, 0, Round((nCanFis * oF16t.F16tPesPal / oF16t.F20cCanFis), 0))

	nPesoMovimiento = oF16t.F16tPesPal

   *> Recuperar entorno de trabajo.
   Select (cOldSelect)

Return nPesoMovimiento

Function PesoPalets
Parameters cCodPro, cFecha

Private cField, cFromF, cWhere
Local cOldSelect

   cOldSelect = Select()

   If Used('F2016TEMP')
      Use In F2016TEMP
   EndIf

   Wait Window 'Cargando datos peso entrada palets. Un momento ...' NoWait

   *> "To_Char(F20cFecMov, 'YYYYMM')='" + AnoT + MesT + "' And " + ;

   *> Leer el peso del palet cuando entró.
   cField = "F20cCodPro, F20cNumPal, F20cCanFis," + _GCN("F16tPesPal", 0) + " As F16tPesPal"
   cFromF = "F20c,F16t"
   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
            "F20cFecMov >=" + _GCD(cFecha) + " And " + ;
            "F20cEntSal = 'E' And " + ;
            "F16tNumPal=F20cNumPal"
            
*!*	   cWhere = "F20cCodPro='" + cCodPro + "' And " + ;
*!*	            "F20cFecMov >=" + _GCD(cFecha) + " And " + ;
*!*	            "F20cEntSal = 'E' And " + ;
*!*	            "F16tNumPal(+)=F20cNumPal"

   cGroup = "F20cCodPro,F20cNumPal,F20cCanfis,F16tPesPal"

   =f3_sql(cField, cFromF, cWhere, ,cGroup , 'F2016TEMP')
	Wait Window '1' NoWait
   Select F2016TEMP
   Go Top

   Wait Clear

   *> Recuperar entorno de trabajo.
   Select (cOldSelect)

Return
