*>
*> GENERAR CALCULOS DE FACTURAS PARA ALMACENAJES- "FACGENCA.prg"
*>
*> Arrastrar saldo paras los días que carezcan de cierre, bien por error, o bien
*> porque se trata de festivos o fines de semana. AVC - 25.01.2002

***** Procedimiento principal del módulo para el cálculo
PROCEDURE GenCalc
Parameters codpro, ancalc, mecalc, quince

Private cWhere, cField, cFromF, cOrder, cGroup
Local lEstado, nInx, nDiaAnterior

    MesT = PadL(AllTrim(Str(mecalc)), 2, '0')
    AnoT = PadL(AllTrim(Str(ancalc)), 4, '0')
    MH = PadL(AllTrim(Str(mecalc+1)), 2, '0')
    AH=AnoT
    If MH="13"
    	MH="01"
    	AH=PadL(AllTrim(Str(ancalc+1)), 4, '0')
    EndIf

    FD1="01/" + MesT+"/"+ AnoT
    FD2="16/" + MesT+"/"+ AnoT
    FH1="15/" + MesT+"/"+ AH
    FH2="01/" + MH+"/"+ AH

    If quince = 1
    	FechaDsd = CtoD(FD1)
    	FechaHta = CTOD(FH1)
    Else
	    FechaDsd = CtoD(FD2)
    	FechaHta = CTOD(FH2)-1
    EndIf

	*> Obtener el nombre de propietario.
	m.F01pCodigo = m.CodPro
	_ok = f3_seek('F01P')
	m.DesPro = Iif(_ok, F01pDescri, '')

	*> Cursor de trabajo para cálculos.
	If Used("F30dCur")
	   Use In F30dCur
	EndIf

	*> Cursor de trabajo para arrastre de saldos.
	If Used("F30dPrevious")
	   Use In F30dPrevious
	EndIf


	*> Cargar los datos de cierre correspondientes al periodo a calcular (quincena).
	cFromF = "F30d"
	cField = "*"
	cOrder = "F30dCodArt,F30dSitStk,F30dFecha"
	cGroup = ""
	cWhere = "F30dCodPro='" + CodPro + "' And " + ;
			 "F30dFecha Between To_Date('" + DToC(FechaDsd) + "', 'dd/mm/yyyy') And " +;
			 "To_Date('" + DToC(FechaHta) + "', 'dd/mm/yyyy')"

	Wait Window 'Cargando datos a procesar quincena: ' + Str(quince, 1, 0) + ' ...' NoWait

	lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F30dCur")
    If !lEstado
	   	 _LxErr = _LxErr + 'No hay movimientos del propietario: ' + m.codpro + ' quincena: ' + Str(quince, 1, 0) + cr
		=Sqlrollback(_Asql)
        Return
	EndIf

	*> Arrastrar cierre para aquellos días en los que no exista.
	nDiaAnterior = 0
	=CrtCursor('F30dCur', 'F30dPrevious', 'C')

	Wait Window 'Recalculando datos cierre ...' NoWait

	For nInx = Day(FechaDsd) To Day(FechaHta)
		Select F30dCur
		Locate For Day(F30dFecha)==nInx
		If !Found()
*!*				Select F30dPrevious
*!*				Delete All

*!*				*> No hay cierre de este día: Arrastrar cierre día anterior.
*!*				If nDiaAnterior > 0
*!*					*> Hay arrastre memorizado de saldo.
*!*					Append From Dbf('F30dCur') For Day(F30dFecha)==nDiaAnterior
*!*				Else
*!*					*> No hay memorizado el arrastre de cierre: Tomar el del día más cercano.
*!*					cWhere = "F30dFecha<To_Date('" + DToC(FechaDsd) + "', 'DD-MM-YYYY')"
*!*					cFromF = "F30d"
*!*					cField = "Max(F30dFecha) As FechaPrev"
*!*					lEstado = f3_sql(cField, cFromF, cWhere, , , 'F30DPREV')

*!*					If lEstado
*!*						*> Cargar registros de cierre del día más cercano.
*!*						Select F30dPREV
*!*						Go Top

*!*						cFromF = "F30d"
*!*						cField = "*"
*!*						cOrder = "F30dCodArt,F30dSitStk,F30dFecha"
*!*						cGroup = ""
*!*						cWhere = "F30dCodPro='" + CodPro + "' And " + ;
*!*								 "F30dFecha = To_Date('" + DToC(F30dPREV.FechaPrev) + "', 'dd/mm/yyyy')"

*!*						lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F30dPrevious")
*!*					EndIf
*!*				EndIf

*!*				*> Cambia la fecha por el día actual.
*!*				Select F30dPrevious
*!*				Replace All F30dFecha With CToD(Str(nInx, 2, 0) + "/" + Str(Month(FechaDsd), 2, 0) + "/" + Str(Year(FechaDsd), 4, 0))
*!*				
*!*				*> Agregar al cursor de trabajo.
*!*				Select F30dCur
*!*				Append From Dbf('F30dPrevious')
		EndIf

		*> Memorizar el día con el último cierre obtenido.
		nDiaAnterior = nInx
	EndFor

	*> Si es control de calidad, considerar como disponible a efectos de punta y media.
	*> Si es laboratorio, mantener la situación de stock.
	*> Laboratorio: SSTK='8000' y TIPPRO=='3000'.
	*> Control calidad: SSTK=='8000' y TIPPRO # '3000'.
	*> Resto: SSTK # '1000': Poner a '1000'.
	Select F30dCur
	Scan For F30dSitStk # '1000'
		Do Case
			Case F30dSitStk=='8000'
				m.F08cCodPro = F30dCur.F30dCodPro
				m.F08cCodArt = F30dCur.F30dCodArt
				=f3_seek('F08c')

				Select F30dCur
				If F08c.F08cTipPro # '3000'
					Replace F30dSitStk With '1000'
				EndIf

			Otherwise
				Replace F30dSitStk With '1000'
		EndCase
	EndScan

    *> Reclasificar el cursor de trabajo, dado que se pueden haber insertado registros al final.
	Select F30dCur
	Sort To 'F30dSort' On F30dCodArt/A, F30dSitStk/A, F30dFecha/A
	Zap
	Append From F30dSort
	Delete File 'F30dSort.Dbf'

	*> Eliminar los datos ya existentes del propietario/año/mes/quincena.
	cWhere = "F34fCodPro='" + CodPro + "' And " + ;
	         "F34fAnyo='" + AnoT + "' And " + ;
	         "F34fMes='" + MesT + "' And " + ;
	         "F34fQuince='" + Str(Quince, 1, 0) + "'"

	lEstado = f3_DelTun("F34f", , cWhere, 'N')
	If !lEstado
	   _LxErr = 'Error borrando datos existentes' + cr
	   Do Form St3Inc With .T.
	   =SqlRollBack(_ASql)
	   Return
	EndIf

	*>
	Select F30dCur
    Go Top
	Do While !Eof() 
		Articulo = F30dCur.F30dCodArt
		Stok = F30dCur.F30dSitStk
		Pes = F30dCur.F30dTotPes			&& Para punta peso.
		Vol = F30dCur.F30dTotVol			&& Para punta peso.

		Wait Window 'Procesando: ' + CodPro + Space(1) + Articulo + Space(1) + 'Quincena: ' + Str(Quince, 1, 0) NoWait

		Select F30dCur
		Do While (stok = F30dCur.F30dSitStk) And (Articulo=F30dCur.F30dCodArt)
			AcumPes  = AcumPes + F30dCur.F30dTotPes
			AcumVol  = AcumVol + F30dCur.F30dTotVol

			If F30dCur.F30dTotPes >= Pes
				Pes = F30dCur.F30dTotPes
			EndIf
			If F30dCur.F30dTotVol >= Vol
				Vol = F30dCur.F30dTotVol
			EndIf
			
			*> Leer siguiente registro de cierre día del artículo.
			Select F30dCur
			r2 = RecNo()
			Skip
		EndDo

		MediaVol = AcumVol / (Day(FechaHta) - Day(FechaDsd) + 1)
		MediaKgs = AcumPes / (Day(FechaHta) - Day(FechaDsd) + 1)

		*> Miramos si ya se ha hecho antes este cálculo con este artículo este mismo periodo.
		_Sentencia = "SELECT * FROM F34F" +_em + ; 
					 " WHERE F34fCodPro='" + CodPro + "' And" + ;
					 " F34fCodArt='" + Articulo + "' And" + ;
					 " F34fSitStk='" + stok + "' And" +;
					 " F34fMes='" + MesT + "' And F34fAnyo = '" + AnoT + "' And" +;
					 " F34fQuince=" + str(quince,1, 0)+ ""
				 
		_Err = SqlExec(_Asql,_Sentencia,'F34fCur')
		If _Err < 0
	 		=MessageBox(' Error al consultar F34F ' + Chr(13) + message() + ' ',16,'TUNDEN')
			=Sqlrollback(_Asql)
			Return   
		EndIf			 

		=sqlmoreresults(_Asql)
		Select F34fCur
		Go Top
		If Eof()
			_Sentencia = "INSERT INTO F34F" +_em +	; 
				 		 "(F34fAnyo,F34fMes,F34fQuince,F34fCodPro,F34fCodArt,F34fSitStk,F34fMedVol,F34fMedKgs,F34fMaxVol,F34fMaxKgs) " + ;  
			    		 " VALUES('" + AnoT + "','" + MesT + "','" + str(quince,1) + "', '" + CodPro + "'" +;
			 			 " ,'" + Articulo + "' ,'" + stok + "'," + str(MediaVol,13,3) + "," + str(MediaKgs,13,3) + ;
				 		 " ," + str(Vol,13,3) + "," + str(Pes,13,3) + " )"				 
			_Err = SqlExec(_Asql,_Sentencia)
			If _Err < 0
				=MessageBox(' Error al insertar la información en F34F ' + Chr(13) + message() + ' ',16,'TUNDEN')
    			=Sqlrollback(_Asql)
	    		Return   
			EndIf			 
			=sqlcommit(_Asql)
		Else
			_Sentencia = "UPDATE F34F" + _em + ;
						 " SET F34fMedVol = " + str(MediaVol,13,3) + ",F34fMedKgs = " + str(MediaKgs,13,3) + ;
						 " ,F34fMaxVol = " + str(Vol,13,3) + ", F34fMaxKgs = " + str(Pes,13,3) + ;
						 " WHERE F34fCodPro='" + CodPro + "' And" + ;
						 " F34fCodArt='" + Articulo + "' And" + ;
						 " F34fSitStk='" + stok + "' And" + ;
				 		 " F34fMes='" + MesT + "' And F34fAnyo='" + AnoT + "' And" + ;
					     " F34fQuince='" + str(quince,1,1)+ "'"
		 	_Err = SqlExec(_Asql,_Sentencia)
			If _Err < 0
				=MessageBox(' Error al modificar la información en F34F' + Chr(13) + message() + ' ',16,'TUNDEN')
    			=Sqlrollback(_Asql)
	    		Return   
			EndIf			 
			=sqlcommit(_Asql)
		EndIf

		AcumPes = 0
		AcumVol = 0
		Select F30dCur
	EndDo

	If Used("F30dCur")
		Use In F30dCur
	EndIf

	If Used("F30dPrevious")
		Use In F30dPrevious
	EndIf

	Wait Clear
Return

***** Procedimiento para los cálculos de aplicación de tarifa de Almacenaje
PROCEDURE GenTari
Parameters codpro, ancalc, mecalc, quince
Private cWhere, cField, cFromF, cOrder, cGroup,MediaKgs,MediaVol,CanTotalMax,CanTotalMed,ValorTotal

Store 0 to MediaKgs,MediaVol,CanTotalMax,CanTotalMed,ValorTotal

Create Cursor CURSOR_TOTAL(;
	CodCli c(6), ;
	Articulo c(13), ;
	SitStk c(4), ;
	PreMins n(13,3), ;
	Cant n(13,3), ;
	Unidad c(3), ;
	Precios n(13,3), ;
	Calculus c(6), ;
	Quincen   n(1), ;
	Mes      c(2), ;
	Anyo     c(4))

    MesT = PadL(AllTrim(Str(mecalc)), 2, '0')
    AnoT = PadL(AllTrim(Str(ancalc)), 4, '0')

    ** Miro si existe la ficha cliente para ese propietario
   m.F32cCodPro = CodPro
   _ok = f3_seek('F32C')
   If !_ok
		_LxErr = _LxErr + 'No hay datos de facturación ' + CodPro + cr
		=Sqlrollback(_Asql)
		Return
	EndIf

	Unitat = F32cUniFac
	PreMin = F32cPreMin
	m.CodPro = F32cCodPro
	m.F01pCodigo = m.CodPro
	_ok = f3_seek('F01P')
	m.DesPro = Iif(_ok, F01pDescri, '')
   
	*> Miramos si se ha hecho la generación de cálculos-almacenaje
	_Sentencia= " Select F34FCodPro,F34fCodArt,Nvl(F34fMedVol,0) As F34fMedVol," + ;
				" Nvl(F34fMedKgs,0) As F34fMedKgs,Nvl(F34fMaxVol,0) As F34fMaxVol," + ;
				" Nvl(F34fMaxKgs,0) As F34fMaxKgs,F34fSitStk,F34fQuince,F34fAnyo,F34fMes from F34F" + _em + ;
				" where F34fCodPro= '" + CodPro + "'" + ;
				" And F34fAnyo = '"+ AnoT +"' and F34fMes = '" + MesT + "'" + ; 
				" And F34fquince=" + str(quince, 1, 0) + ;
    			" order by F34fCodArt, F34fSitStk"

	_Err = SqlExec(_Asql,_Sentencia,'F34fCur')
	If _Err < 0
     	=MessageBox(' Error al consultar F34f ' + Chr(13) + message() + ' ',16,'TUNDEN')
		=Sqlrollback(_Asql)
		Return   
	EndIf			 
	=sqlmoreresults(_Asql)
	
	*> Miramos los datos de la tarifa del propietario.
	_Sentencia = "Select * from F34T" + _em + ;
				 " Where F34tCodPro = '" + CodPro + "'" + ;
				 " Order By F34tCodArt "
	_Err = SqlExec(_Asql,_Sentencia,'F34tCur')
	If _Err < 0
 		=MessageBox(' Error al consultar F34T ' + Chr(13) + message() + ' ',16,'TUNDEN')
   		=Sqlrollback(_ASql)
		Return   
	EndIf			 
	=sqlmoreresults(_ASql)

	Select F34tCur
	Go Top
	If Eof()
		 _LxErr =_LxErr + 'No hay información de tarifa del propietario: ' + CodPro + Space(1) + cr
		=SqlRollBack(_ASql)
		Return
	EndIf

	Select F34fCur
	Go Top
	If eof()
		_LxErr =_LxErr +  'No hay cálculos para el propietario: '+ m.codpro + ' quincena: ' + Str(quince, 1, 0) + cr
		=Sqlrollback(_ASql)
		Return
	endif

	*> Eliminar los datos ya existentes del propietario/año/mes/quincena.
	cWhere = "F34gCodPro='" + CodPro + "' And " + ;
	         "F34gAnyo='" + AnoT + "' And " + ;
	         "F34gMes='" + MesT + "' And " + ;
	         "F34gQuince='" + Str(Quince, 1, 0) + "'"

	lEstado = f3_DelTun("F34g", , cWhere, 'N')
	If !lEstado
	   _LxErr = _LxErr + 'Error borrando datos existentes' + cr
	   =SqlRollBack(_ASql)
	   Return
	EndIf

	*> Empezamos con los cálculos para cada artículo de este propietario
	Select F34fCur
	Go Top
	Do While !Eof()
   		Wait Window "Procesando cálculos: " + CodPro + Space(1) + F34fCur.F34fCodArt + Space(1) + 'Quincena: ' + Str(Quince, 1, 0) NoWait

		Select CURSOR_TOTAL
		Append Blank

		CantFinMax = 0
		CantFinMed = 0

		*> Si hay tarifa para un artículo la cogemos.
		Select F34tCur
		Locate For F34tCodArt==F34fCur.F34fCodArt .And. F34tSitStk==F34fCur.F34fSitStk
		If !Found()
			*> Por defecto, cogemos los datos de la tarifa "genérica".
			Go Top
		EndIf

		Cantidad = F34tMinUni
		Unidades = F34tUnitat
		Precio   = F34tEuros
		Tipo     = F34tTipo

		Select CURSOR_TOTAL		
		Replace	CodCli   with F34fCur.F34fCodPro, ;
		        Articulo with F34fCur.F34fCodArt, ;
		        SitStk   with F34fCur.F34fSitStk, ;
		        Cant     with Cantidad, ;
	        	Unidad   with Unidades, ;
		       	Precios  with Precio	, ;
		       	PreMins  with PreMin, ;
		       	Calculus with Tipo, ;
		        Mes      with MesT, ;
		        Anyo     with AnoT, ;
		        Quincen  with quince

		*> Calcular por punta de valor.
		If unidades = "KGS"
			*> Acumular peso (máximo).
			If F34fCur.F34fMaxKgs = 0
				CantFinMax = 0
			Else
				If F34fCur.F34fMaxKgs >= Cantidad
					CantFinMax = F34fCur.F34fMaxKgs
				Else
					CantFinMax = Cantidad
				EndIf
			EndIf

			*> Acumular peso (media).
			If F34fCur.F34fMedKgs = 0
					CantFinMed = 0
			Else
				If F34fCur.F34fMedKgs >= Cantidad
					CantFinMed= F34fCur.F34fMedKgs
				Else
					CantFinMed = Cantidad
				EndIf
			EndIf
		Else
			*> Acumular volumen (máximo).
			If F34fCur.F34fMaxVol = 0
					CantFinMax = 0
			Else
				If F34fCur.F34fMaxVol >= Cantidad
					CantFinMax = F34fCur.F34fMaxVol
				Else
					CantFinMax = Cantidad
				EndIf
			EndIf

			*> Acumular volumen (media).
			If F34fCur.F34fMedVol = 0
					CantFinMed = 0
			Else
				If F34fCur.F34fMedVol>= Cantidad
					CantFinMed = F34fCur.F34fMedVol
				Else
					CantFinMed = Cantidad
				EndIf
			EndIf
		EndIf
	
		CanTotalMax = CanTotalMax + CantFinMax
		CanTotalMed = CanTotalMed + CantFinMed

		*> Miramos si ya se ha hecho antes este cálculo con este artículo este mismo periodo
		_Sentencia = "SELECT * FROM F34G" +_em + ; 
					 " WHERE F34gCodPro = '" + F34fCur.F34fCodPro + "' and" + ;
					 " F34gCodArt = '" + F34fCur.F34fCodArt + "' and" + ;
					 " F34gSitStk = '" + F34fCur.F34fSitStk + "' And" + ;
					 " F34gMes = '" + MesT + "' and F34gAnyo = '" + AnoT + "' And" + ;
					 " F34gQuince = " + str(quince,1)+ ""
			 
   		_Err = SqlExec(_Asql,_Sentencia,'F34gCur')
		If _Err < 0
			=MessageBox(' Error al consultar F34G ' + Chr(13) + message() + ' ',16,'TUNDEN')
			=Sqlrollback(_Asql)
			Return
		EndIf
		=sqlmoreresults(_Asql)

		*> Si está vacío F34gcur para este propietario lo insertamos
		Select F34gCur
		If Eof()
			_Sentencia = "INSERT INTO F34G" +_em +	; 
				 		 "(F34gAnyo,F34gMes,F34gQuince,F34gCodPro,F34gCodArt,F34gSitStk,F34gCanMax,F34gCanMed) " + ;
			    		 " VALUES('" + AnoT + "','" + MesT + "','" + str(quince,1) + "', '" + CodPro + "'" + ;
			 			 " ,'" + F34fCur.F34fCodArt + "' ,'" + F34fCur.F34fSitStk + "'" + ;
				 		 " ," + str(CantFinMax,13,3) + "," + str(CantFinMed,13,3) + " )"

			_Err = SqlExec(_Asql,_Sentencia)
			If _Err < 0
				=MessageBox(' Error al insertar la información en F34G ' + Chr(13) + message() + ' ',16,'TUNDEN')
				=Sqlrollback(_Asql)
	    		Return   
			EndIf			 
			=sqlcommit(_Asql)
		Else
			*> Sino, lo modificamos	
			_Sentencia = "UPDATE F34G" + _em + ;
						 " SET F34GCanMed = " + str(CantFinMed,13,3) + ",F34GCanMax = " + str(CantFinMax,13,3) + ;
			 			 " WHERE F34gCodPro = '" + F34fCur.F34fCodPro + "' and" + ;
			 			 " F34gCodArt = '" + F34fCur.F34fCodArt + "' And" + ;
			 			 " F34gSitStk = '" + F34fCur.F34fSitStk + "' And" +;
			 			 " F34gMes = '" + MesT + "' and F34gAnyo = '" + AnoT + "' And" +;
				  		 " F34gQuince = '" + str(quince,1)+ "'"
		 	_Err = SqlExec(_Asql,_Sentencia)
			If _Err < 0
				=MessageBox(' Error al modificar la información en F34G' + Chr(13) + message() + ' ',16,'TUNDEN')
    			=Sqlrollback(_Asql)
	    		Return   
			EndIf			 
			=sqlcommit(_Asql)
		EndIf

		Select F34fCur
		skip	
	EndDo

	Store 0 To MediaKgs, MediaVol, CanTotalMax, CanTotalMed, ValorTotal
	
    *> Generar datos para listado.
    Do GenTarF With CodPro, AnoT, MesT, Str(quince, 1, 0) In FacLst

Return
