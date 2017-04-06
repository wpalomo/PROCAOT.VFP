*> GENERAR FACTURAS - "FACGENF.prg"

*****	Procedimiento Principal => GenFac => Realiza las siguientes tareas:
*****									- Comprueba la viabilidad de la generación de facturas
*****									- Crea los archivos temporales del
*****									- Gestiona la toma de decisiones
*****
*****	ActualizarFactura => Guarda los registros temporales y luego los borra
*****	Busqueda => Esta función devuelve si se ha encontrado algo como resultado de la consulta
*****				contenida en la variable Lx_Select
*****	Agregar_Error => Gestiona si se debe meter un nuevo error en _lxerr y lo mete si se tercia
*****	Encontrado => Gestiona el resultado del seek. Si no se ha encontrado da error y sale.

*> Añadir tratamiento de conceptos DIARIOS (F35c) y SEMANALES (F36c). AVC - 29.04.1999
*> Tratamiento precio mínimo y máximo por documento o por total línea, según concepto. AVC - 04.05.1999
*> Recibe fecha factura. AVC - 01.06.1999
*> Añadir conceptos quincenales. AVC - 26.04.2001

***** Procedimiento principal del módulo
*> Recibe: CodPro -----> Propietario.
*>         AnFac  -----> Año para cálculo.
*>         MeFac  -----> Mes para cálculo.
*>         FecFra -----> Fecha de la factura.

Procedure GenFac
Parameters CodPro, AnFac, MeFac, FecFra, nOperacion

        Private fecha, fechastring, ano, mes  && fechastring está adaptada al formato de F34cFecVal
        Private nDiasCalculo, nDiasSegundaQuincena, nDiasNaturalesMes

        Do Borrar_Temporales

		** Saco dos formatos de fechas a necesarios en la creación de dos cursores
		** No intentes comprenderlo, simplemente funciona.

		mes=mefac+1		&&&&& Esto es para que salga el día 1 del mes siguiente

        mefac = PadL(AllTrim(Str(mefac)), 2, '0')
		anfac = AllTrim(Str(anfac))
		fechastring=anfac+mefac+"0"
		if mes=13
			mes=1
			ano=ano+1
		endif

		ano=anfac
        mes = PadL(AllTrim(Str(mes)), 2, '0')
		fecha='01/' + mes + '/' + ano
        AnyoMes = anfac + mefac

		** Miro los conceptos del propietario válidos en esta fecha.
		Lx_Select = "Select * From F33c" + _em + " Where F33cCodPro = '" + CodPro + "' And " + ;
                    " Exists (Select * From F34u" + _em + ;
                    " Where F34uCodCon=F33cCodUni) And" + ;
					" F33cFecVal<=To_Date('" + fecha + "','dd/mm/yyyy')"

		Err = SqlExec(_ASql, Lx_Select, 'F33CTEMP')
		=SqlmoreResults(_aSql)
		_ok =Agregar_Error(err,"No se ha podido acceder al fichero F33C")
		if !_ok
			return
		endif
		Select F33CTEMP
		if eof()
            _LxErr = _LxErr + "No hay ningún concepto para el propietario: " + CodPro + cr
			Return .F.
		endif

        Wait Window 'Cargando ficheros de facturación ...' NoWait

        ***** Crear un cursor con la estructura del F70l
        =CrtCursor('F70L', 'F70LTEMP', 'C')

        *> Cargar acumulados MENSUALES.
		Lx_Select = "Select * from f34c" + _em + " where F34cCodPro='" + codpro + "' and " + ;
					"F34cfecVal='" + fechastring + _cm
		Err = SqlExec(_aSql,Lx_Select,'F34CTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero F34C")
		if !_ok
			return
		endif

        *> Cargar acumulados DIARIOS.
        Lx_Select = "Select * From F35c" + _em + " Where F35cCodPro='" + CodPro + _cm + ;
                    " And SubStr(F35cFecVal, 1, 6)='" + AnyoMes + _cm + " Order By 1, 2"
		Err = SqlExec(_aSql,Lx_Select,'F35CTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero F35C")
		if !_ok
			return
		endif

        *> Calcular los días de la segunda quincena del mes.
        Select F35CTEMP
        Go Bottom
        nDiasSegundaQuincena = Val(SubStr(F35cFecVal, 7, 2)) - 15
        nDiasNaturalesMes    = Val(SubStr(F35cFecVal, 7, 2))

        *> Cargar acumulados SEMANALES.
        Lx_Select = "Select * From F36c" + _em + " Where F36cCodPro='" + CodPro + _cm + ;
                    " And SubStr(F36cFecVal, 1, 6)='" + AnyoMes + _cm
		Err = SqlExec(_aSql,Lx_Select,'F36CTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero F36C")
		if !_ok
			return
		endif

        *> Cargar acumulados documentos de SALIDA.
        Lx_Select = "Select * From F35s" + _em + " Where F35sCodPro='" + CodPro + _cm + ;
                    " And To_Char(F35sFecDoc, 'MM')='" + MeFac + _cm
		Err = SqlExec(_aSql,Lx_Select,'F35STEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero F35S")
		if !_ok
			return
		endif

        *> Cargar acumulados QUINCENALES.
        Lx_Select = "Select * From F35p" + _em + " Where F35pCodPro='" + CodPro + _cm + ;
                    " And F35pFecVal='" + AnyoMes + _cm
		Err = SqlExec(_aSql,Lx_Select,'F35PTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero F35P")
		if !_ok
			return
		endif

        *> Cargar tabla de UNIDADES.
		Lx_Select = "Select * From F34u" + _em

		Err = SqlExec(_aSql, Lx_Select, 'F34UTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se han podido cargar unidades facturación (F34U) - " + CodPro)
		if !_ok
			return
		endif
		Select F34UTEMP
		Index On F34uCodCon TAG F34uInd
		Set Order To Tag F34uInd
		
        *> Cargar archivo general de CONCEPTOS.
		Lx_Select = "Select * From C34c" + _em
		Err = SqlExec(_aSql,Lx_Select,'C34CTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero C34C")
		if !_ok
			return
		endif

		Select C34CTEMP
		Index On C34cCodCon TAG C34cInd
        Set Order To Tag C34cInd

        *> Cargar datos de facturación del PROPIETARIO.
*        cWhere = "F32cCodPro='" + CodPro + "'"
*        =f3_sql("*", "F32c", cWhere, , , "F32c")
*        Select F32c
*        Go Top
*        If Eof()
*           _LxErr = _LxErr + "Error al cargar datos facturación F32C" + cr
*           Return
*        EndIf

        *> Cargar tabla de tipos de I.V.A.
		Lx_Select = "Select * from F34v" + _em
		Err = SqlExec(_aSql,Lx_Select,'F34VTEMP')
		=SqlmoreResults(_aSql)
		_ok=Agregar_Error(err,"No se ha podido acceder al fichero F34V")
		if !_ok
			return
		endif
		Select F34VTEMP
		Index On F34vCodCon TAG f34vind
		
        Wait Clear
        Wait Window 'Generando Factura - Espere Por Favor ...' NoWait

	***** Para cada registro del F33CTEMP se genera uno ó varios registros en F70l,
	***** dependiendo de la periodicidad del concepto (F34UPeriod)
	Private cantidad		&&&&& Guarda el contenido del campo del concepto a facturar
	Private final			&&&&& Guarda el final(totlin)
	Private bruto			&&&&& Guarda el bruto(implin)
	Private brutiva,bruteqv,brutdto		&&&&& Guarda parciales

    Select F33CTEMP
    Go Top
    Do While !Eof()
		*> Ver si existe el concepto en tabla conceptos.
		Select F34UTEMP
		seek F33CTEMP.F33cCodUni
		_ok=Encontrado("No se ha encontrado concepto " +  F33CTEMP.F33cCodUni + " (F34U) - " + CodPro)
		if !_ok
			return
		endif

        *> Ver si existe el concepto en archivo general.
        If F34UTEMP.F34uNumCon > 0
   	       Select C34CTEMP
           seek F34UTEMP.F34uNumCon
           _ok=Encontrado("No se ha encontrado concepto " +  Str(F34UTEMP.F34uNumCon) + " (C34C) - " + CodPro)
           if !_ok
              return
           endif
        EndIf

        *> Obtener los días para cálculo, según el concepto.
        Do Case
           Case F33CTEMP.F33cTipCal=='0'
              nDiasCalculo = 1
           Case F33CTEMP.F33cTipCal=='1'
              nDiasCalculo = 15
           Case F33CTEMP.F33cTipCal=='2'
              nDiasCalculo = nDiasSegundaQuincena
           Case F33CTEMP.F33cTipCal=='3'
              nDiasCalculo = 30
           Case F33CTEMP.F33cTipCal=='4'
              nDiasCalculo = nDiasNaturalesMes
           Otherwise
              nDiasCalculo = 1
        EndCase

        *> Aquí seleccionamos el fichero de acumulados, según que el concepto
        *> que se va a calcular sea Mensual, Semanal ó Diario.
        Select F34UTEMP
        Scatter MemVar
        Do Case
           *> Calcular conceptos SEMANALES.
           Case F34uPeriod=='S'
              Select F36CTEMP
              Go Top
              Do While !Eof()
                 campo=alltrim(C34CTEMP.C34CNomCam)
                 cantidad=&campo
                 Do GenLineaFactura With F34uPeriod

                 Select F36CTEMP
                 Skip
              EndDo

           *> Calcular conceptos DIARIOS.
           Case F34uPeriod=='D'
              Select F35CTEMP
              Go Top
              Do While !Eof()
                 campo=alltrim(C34CTEMP.C34CNomCam)
                 cantidad=&campo
                 Do GenLineaFactura With F34uPeriod

                 Select F35CTEMP
                 Skip
              EndDo
                 
           *> Calcular conceptos MENSUALES.
           Case F34uPeriod=='M'
              If F34uNumCon > 0
                 Select F34CTEMP
                 campo=alltrim(C34CTEMP.C34CNomCam)
                 cantidad=&campo
              Else
                 *> Conceptos no calculados.
                 Cantidad = 1
              EndIf
              Do GenLineaFactura With F34uPeriod

           *> Calcular conceptos QUINCENALES.
           Case F34uPeriod=='Q'
              Select F35PTEMP
              campo=alltrim(C34CTEMP.C34CNomCam)
              cantidad=&campo
              Do GenLineaFactura With F34uPeriod

           *> Por defecto, conceptos mensuales.
           OtherWise
              If F34uNumCon > 0
                 Select F34CTEMP
                 campo=alltrim(C34CTEMP.C34CNomCam)
                 cantidad=&campo
              Else
                 *> Conceptos no calculados.
                 Cantidad = 1
              EndIf
              Do GenLineaFactura With F34uPeriod
        EndCase

		*> Leer el siguiente concepto del propietario actual.
        Select F33CTEMP
        Skip
     EndDo
	
     ***** Guardo los registros generados
     =ActualizarFactura(nOperacion)

RETURN

*> Generar UNA línea de factura.
Procedure GenLineaFactura

   Parameters Periodo
   Private Descri

        Do Case
           Case Periodo == 'D'
              Descri = AllTrim(F33CTEMP.F33cDescri) + Space(1) + Right(F35CTEMP.F35cFecVal, 4)
           Case Periodo == 'S'
              Descri = AllTrim(F33CTEMP.F33cDescri) + Space(1) + Right(F36CTEMP.F36cFecVal, 2)
           Case Periodo == 'M'
              Descri = F33CTEMP.F33cDescri
           OtherWise
              Descri = F33CTEMP.F33cDescri
        EndCase

		** Añado registro a detalle de factura.
        Select F70LTEMP
        Append Blank
		Replace F70lCodPro With CodPro, ;
				F70lCodSrv With F33CTEMP.F33cCodSrv, ;
				F70lCodCon With F33CTEMP.F33cCodCon, ;
				F70lCodUni With F33CTEMP.F33cCodUni, ;
				F70lDescri With Descri, ;
				F70lUniCal With cantidad * nDiasCalculo, ;
				F70lUniCor With cantidad * nDiasCalculo, ;
				F70lPreUni With F33CTEMP.F33cPtsUni, ;
				F70lPreCor With F33CTEMP.F33cPtsUni, ;
				F70lImpCnt With F33CTEMP.F33cImpCnt, ;
				F70lImpPvp With F33CTEMP.F33cImpPvp

        *> Se aplica importe fijo al concepto.
        If F33CTEMP.F33cPtsFix>0
           bruto=F33CTEMP.F33cPtsFix
        Else
           *> Importe máximo y/o mínimo del concepto.
		   bruto=F33CTEMP.F33cPtsUni * cantidad * nDiasCalculo
           If F33CTEMP.F33cPtsMin > 0 .Or. F33CTEMP.F33cPtsMax > 0
              bruto = CalcBruto()
           EndIf
        EndIf
		
		** HAGO LOS CALCULOS
		Select F32c
		brutdto=bruto-(bruto*(F32cDtodc1/100))
		brutdto=brutdto-(brutdto*(F32cDtodc2/100))
		brutdto=brutdto-(brutdto*(F32cDtodpp/100))
		Select F34VTEMP
		seek F33CTEMP.F33cCodImp
		_ok=Encontrado("No se ha encontrado el concepto en el fichero de Impuestos F34V")
		if !_ok
			return
		endif
		brutiva=brutdto*(F34vValIva/100)
		bruteqv=brutdto*(F34vValEqv/100)
		final=brutdto+brutiva+bruteqv
		
		*> Rellenar resto de campos de la línea de detalle de la factura.
		Select F70LTEMP
		Replace F70lImpLin With bruto, ;
		        F70lCodImp With F33CTEMP.F33cCodImp, ;
				F70lValIva With F34VTEMP.F34vValIva, ;
				F70lTotIva With brutiva, ;
				F70lValEqv With F34VTEMP.F34vValEqv, ;
				F70lTotEqv With bruteqv, ;
				F70lTotDto With bruto - brutdto, ;
				F70lBasImp With brutdto, ;
				F70lTotLin With final,F70lTipLin With 'ALMA'
Return

*> Calcular el importe cuando hay importe mínimo y máximo.
PROCEDURE CalcBruto

   Private BrutoReal
   Private Parcial

   Do Case
      *> Líneas de fracciones, aplicar mínimo y/o máximo A CADA DOCUMENTO.
      Case F34UTEMP.F34uNumCon == 86
         BrutoReal = 0
         Select F35STEMP
         Go Top
         Do While !Eof()
            Parcial = F35sNumLin * F33CTEMP.F33cPtsUni
            Parcial = Iif(Parcial < F33CTEMP.F33cPtsMin, F33CTEMP.F33cPtsMin, Parcial)
            Parcial = Iif(Parcial > F33CTEMP.F33cPtsMax, F33CTEMP.F33cPtsMax, Parcial)
            BrutoReal = BrutoReal + Parcial
         
            Select F35STEMP
            Skip
         EndDo

      *> Resto conceptos, aplicar mínimo y/o máximo AL TOTAL LINEA.
      OtherWise
         BrutoReal = cantidad * F33CTEMP.F33cPtsUni * nDiasCalculo
         BrutoReal = Iif(BrutoReal < F33CTEMP.F33cPtsMin, F33CTEMP.F33cPtsMin, BrutoReal)
         BrutoReal = Iif(BrutoReal > F33CTEMP.F33cPtsMax, F33CTEMP.F33cPtsMax, BrutoReal)
   EndCase

RETURN BrutoReal

*> Actualiza los cambios en los ficheros reales de facturación.
*> Recibe: nOperacion ---> Operación a realizar si la factura ya existe.
Procedure ActualizarFactura
Parameters nOperacion

    Private cWhere, cFactura
    Private lEstado

	*> Vaciar cursores de trabajo.
	Select F70c
	Delete All
	Select F70l
	Delete All
	Select F70i
	Delete All
	Select F70o
	Delete All
	Select F70p
	Delete All

    *> Eliminar las líneas con cantidad cero.
    Select F70LTEMP
    Delete For F70lUniCal = 0
    Go Top
    If Eof()
       _LxErr = _LxErr + "No hay conceptos para generar factura del propietario: " + CodPro + cr
       Return
    EndIf

    *> Miro si ya existe una cabecera para esas líneas.
    cWhere = "F70cCodPro='" + CodPro + "' And " + ;
             "F70cMesFac='" + MeFac  + "' And " + ;
             "F70cAnyFac='" + AnFac  + "'"

    lEstado = f3_sql("*", "F70c", cWhere, , , "F70c")
    Select F70c
    Go Top
    If !Eof()
      *> Hay mas de una factura del mes: Escoger la que se actualizará.
      cFactura = EscogerFactura()                               && FACEDIF1.
      If Empty(cFactura)
         Go Top
         cFactura = F70cNumFac
      EndIf

      Delete For F70cNumFac # cFactura
      Go Top

      *> Recoge los datos de la factura actual.
      =CargarDatosPagoFactura(cFactura)                         && FACEDIF1.
      =CargarDetalleFactura(cFactura)                           && FACEDIF1.

      Select F70p
      Go Top
      If Eof()
         =GenerarDatosPago(cFactura)
      EndIf

      Select F70i
      Go Top
      If Eof()
         =GenerarAcumulados(cFactura)
      EndIf
      
      *> Ya existe la factura: Operación a realizar.
      Do Case
         *> Añadir líneas a las ya existentes.
         Case nOperacion==1
            =AgregarLineas(.F., cFactura)
            =CalcularAcumuladosFactura()                        && FACEDIF1.
            =GrabarFactura(.F.)                                 && FACEDIF1.
            =SqlCommit(_ASql)
            Return

         *> Substituir las líneas ya existentes.
         Case nOperacion==2
            =SubstituirLineas(cFactura)
            =CalcularAcumuladosFactura()                        && FACEDIF1.
            =GrabarFactura(.F.)                                 && FACEDIF1.
            =SqlCommit(_ASql)
            Return

         *> Cancelar la operación.
         Case nOperacion==3
            _LxErr = _LxErr + "Ya se ha generado factura para propietario: " + CodPro + cr
            =SqlRollBack(_ASql)
            Return

         *> Crear una nueva factura.
         Case nOperacion==4
            cFactura = GenerarCabecera()
            =GenerarDatosPago(cFactura)
            =GenerarAcumulados(cFactura)
            =AgregarLineas(.T., cFactura)
            =CalcularAcumuladosFactura()                        && FACEDIF1.
            =GrabarFactura(cFactura)                            && FACEDIF1.
            =SqlCommit(_ASql)
            Return

         *> Por defecto, añadir líneas a las ya existentes.
         OtherWise
            =AgregarLineas(.F., cFactura)
            =CalcularAcumuladosFactura()                        && FACEDIF1.
            =GrabarFactura(.F.)                                 && FACEDIF1.
            =SqlCommit(_ASql)
            Return
      EndCase
	Else
	  *> No existe la factura: Generar.
	  cFactura = GenerarCabecera()
	  =GenerarDatosPago(cFactura)
	  =GenerarAcumulados(cFactura)
	  =AgregarLineas(.T., cFactura)
	  =CalcularAcumuladosFactura()                        && FACEDIF1.
	  =GrabarFactura(cFactura)                            && FACEDIF1.
	  =SqlCommit(_ASql)
	  Return
	EndIf

Return

*>
*> Procedimiento para preparar datos de cabecera.
*>
*> Devuelve: Nº de factura.
Procedure GenerarCabecera

   Private cFactura

   *> Obtener el nº de factura.
   cFactura = Ora_NewNum('NFAC', 'N', 'N', 10, 1)
   
   Select F70c
   Delete All
   Append Blank
   Replace F70cCodPro With CodPro, ;
           F70cAnyFac With AnFac, ;
           F70cMesFac With MeFac, ;
           F70cNumFac With cFactura, ;
           F70cFecFac With FecFra, ;
           F70cEstado With '0'

   Scatter MemVar

Return cFactura

*>
*> Procedimiento para preparar datos de cabecera.
Procedure GenerarDatosPago
Parameters cFactura

   Local lEstado

   Store .T. To lEstado

   Select F70p
   Delete All
   Append Blank
   Replace F70pCodPro With CodPro, ;
           F70pNumFac With cFactura, ;
           F70pDiapg1 With F32c.F32cDiaPg1, ;
           F70pDiapg2 With F32c.F32cDiaPg2, ;
           F70pDiapg3 With F32c.F32cDiaPg3, ;
           F70pDia1er With F32c.F32cDia1er, ;
           F70pDiaiev With F32c.F32cDiaIEv, ;
           F70pFecFra With F32c.F32cFecFra, ;
           F70pNumRec With F32c.F32cNumRec, ;
           F70pDtoDc1 With F32c.F32cDtoDc1, ;
           F70pDtoDc2 With F32c.F32cDtoDc2, ;
           F70pDtoDpp With F32c.F32cDtoDpp, ;
           F70pCodFpg With F32c.F32cCodFpg, ;
           F70pDesFpg With F32c.F32cDesFpg, ;
           F70pDomFpg With F32c.F32cDomFpg, ;
           F70pPobFpg With F32c.F32cPobFpg, ;
           F70pCtaBan With F32c.F32cCtaBan, ;
           F70pNbaNco With F32c.F32cNbanco

   Scatter MemVar

Return lEstado

*>
*> Procedimiento para preparar datos de cabecera.
Procedure GenerarAcumulados
Parameters cFactura

   Local lEstado

   Store .T. To lEstado

   Select F70i
   Delete All
   Append Blank
   Replace F70iCodPro With CodPro, ;
           F70iNumFac With cFactura

   Scatter MemVar

Return lEstado

*>
*> Procedimiento para agregar líneas a la factura.
*>
*> Recibe: lBorrar (.T./.F.) Borrar líneas existentes.
*>         cFactura (Opcional), para actualizar / Borrar.
Procedure AgregarLineas
Parameters lBorrar, cFactura

   Local lEstado

   Store .T. To lEstado

   *> Agregar la líneas generadas por el proceso.
   Select F70l
   If lBorrar
      Delete For F70lNumFac==cFactura .And. F70lTipLin=='ALMA'

      cWhere = "F70lNumFac='" + cFactura + "' And F70lTipLin='ALMA'"
      =f3_DelTun('F70L', , cWhere, 'N')
   EndIf

   Select F70l
   Append From Dbf('F70LTEMP')
   Replace All F70lNumFac With cFactura
   Go Top

Return lEstado

*>
*> Procedimiento para substituir las líneas ya existentes en la factura.
Procedure SubstituirLineas
Parameters cFactura

   Local lEstado

   Store .T. To lEstado

   *> Substituir por las generadas por el proceso.
   cWhere = "F70lNumFac='" + cFactura + "' And F70lTipLin='ALMA'"
   =f3_DelTun('F70L', , cWhere, 'N')

   Select F70l
   Delete For F70lNumFac==cFactura .And. F70lTipLin=='ALMA'

   Append From Dbf('F70LTEMP')
   Replace All F70lNumFac With cFactura
   Go Top

Return lEstado

FUNCTION Agregar_Error
Parameters valor_error,texto_error

	if valor_error < 1
		_lxerr =texto_error + cr
		=SqlRollBack(_ASql)
		Do Borrar_Temporales
		RETURN .F.
	endif
	
RETURN .T.

FUNCTION Encontrado
Parameters texto

	if !found()
		_ok=Agregar_Error(0,texto)
		RETURN .F.
	endif

RETURN .T.

PROCEDURE Borrar_Temporales

	if used('F34cTEMP')
		use in F34cTEMP
	endif
	if used('F33CTEMP')
		use in F33CTEMP
	endif
	if used('F34UTEMP')
		use in F34UTEMP
	endif
	if used('C34CTEMP')
		use in C34CTEMP
	endif
	if used('F34VTEMP')
		use in F34VTEMP
	endif
	if used('F70LTEMP')
		use in F70LTEMP
	endif
	if used('F70CTEMP')
		use in F70CTEMP
	endif
	if used('F70PTEMP')
		use in F70PTEMP
	endif
	if used('IMPTEMP')
		use in IMPTEMP
	endif

RETURN
