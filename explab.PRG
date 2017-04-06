Function ALaboratorio
Parameters Marca

Create Cursor Temp(Temp C(100))

*>Fichero para la exportación hacia laboratorio
	_LxErr=''
	
	_sel="F20cCodPro As CodCli, " + ;
	     "SubStr(F20cNumLot, 1, 10) As NUMBOL, " + ;
		 "SubStr(F20cNumLot, 11, 15) As NUMBAL, F20cFecMov As FECPES, " + ;
		 "Nvl(F16tPesEnt,0) As PESLOT, F16tAcond As ACONDI, F20cNumMov As NumMov" 
	
*>Si el check está marcado filtrar por propietario y fecha 	
	If Marca=1
		f_Where="F20cNumPal = F16tNumPal And F20cCodPro Between '" + ProDes + "' And '" + ProHas + "' " + ; 
				"And F20cFecMov Between To_Date('" + DToC(FALDes) + "', 'dd-mm-yyyy') And To_Date('" + DToC(FALHas) + "', 'dd-mm-yyyy') " + ;  
				"And F20cTipDoc='1400' " + ;
				"And F20cFlgEnv='S'"

*>Si check no está marcado filtrar sólo po propietario
    Else
    	f_Where="F20cCodPro Between '" + ProDes + "' And '" + ProHas + "' " + ;
				"And F20cTipDoc='1400' And F20cFlgEnv<>'S' And F20cNumPal=F16tNumPal" 
    EndIf
    
    
    If !f3_sql(_sel,'F20c,F16t',f_Where,,,'ALabor')
       	_LxErr = 'No hay Datos entre los límites seleccionados' + cr
   		Do Form St3Inc
   		_LxErr = ''
   		Return
   	EndIf
    
*>Abrir el fichero en el que guardaremos los datos por defecto un .TXT
 	Dir=PutFile()

    If Empty(Dir)
	   Return	
    EndIf 

	 Select ALabor
	 c=Dbf()
	
 	Select ALabor
 	Go Top
	Do While !Eof()
		Wait Window 'Generando Fichero ' + ALabor.NumBol NoWait  
*		Codigo=PadL(AllTrim(ALabor.CODCLI), 6, '0')
		NueBol=PadL(AllTrim(ALabor.NUMBOL), 10, '0')
		NueBal=PadL(AllTrim(ALabor.NUMBAL), 5, '0')
		Cadena=Dtos(ALabor.FECPES)
		Cadena1=Substr(Cadena, 7, 2)
		Cadena2=Substr(Cadena, 5, 2)
		Cadena3=Substr(Cadena, 1, 4)
		Peso=PadL(Alltrim(Str(ALabor.PESLOT, 11, 3)), 11, '0')
		
		If ALabor.PESLOT=0
*			_var=Codigo+NueBol+NueBal+Cadena1+Cadena2+Cadena3+Str(ALabor.PESLOT, 11, 3)+ALabor.ACONDI
			_var=NueBol+NueBal+Cadena1+Cadena2+Cadena3+Str(ALabor.PESLOT, 11, 3)
		Else
*			_var=Codigo+NueBol+NueBal+Cadena1+Cadena2+Cadena3+Peso+ALabor.ACONDI
			_var=NueBol+NueBal+Cadena1+Cadena2+Cadena3+Peso
		EndIf
		*+Alltrim(Str(ALabor.ACONDI))
		Select Temp
		Append Blank
		Replace Temp With _var
		
		Select ALabor
		Skip
	EndDo	
	
	Select Temp
	Copy To &Dir Type Sdf

	*>Actualizar con 'S' todos los registros ya procesados
	Select Alabor
	Go Top
	Do While !Eof()
		Estado = 'S'
 		x_Where = "F20cCodPro='" + ALabor.CodCli + "' And " + ;
    	          "F20cNumMov='" + ALabor.NumMov + "'"
    
   		Sw = F3_UpdTun('F20c', , 'F20cFlgEnv', 'Estado', , x_Where, 'N')
		Select ALabor
		Skip
	EndDo	
	
Return 
 

    