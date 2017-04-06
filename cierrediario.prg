
PUBLIC _COMVFP

_ComVfp = 'AUTOMATICO'
*_ComVfp = 'Algevasa06'

* Set Default To &_JobDir

Set Exclusive off
*>
*> Obtener conexión con ORACLE para el caso de llamada externa.
Open DataBase Conexion
=Sys(3053)
_ASql = SqlConnect(_ComVfp)
Close Database

_tmp=Sys(2023) + "\"
_sys=Sys(5) + CurDir()

_xier=0
_xerrac=0
_lxerr=''
select 100
use _SYS + 'SYSPRG' order tag idx1

_xier=0
select 101
use _sys + 'SYSTXT' order tag idx1

_xier=0
select 102
use _sys + 'SYSFC' order tag idx1

_xier=0
select 103
use _sys + 'SYSMEN' order tag idx1

_xier=0
select 106
use _sys + 'SYSTXT30' order tag idx1

Set Point To .
Set Date british

Set Procedure to FacGenc Additive
Set Procedure To St3Rt Additive
Set Procedure To Ora_Proc  Additive
Set Procedure to Wproc001  Additive
Set Procedure To WProcOra  Additive
Public _Entorno,_em

_Entorno = 'ORACLE'
_Em = '001'
Fecha = Date()

*> Deberíamos hacer la foto del almacén.---------------------------------------------------------
_Sentencia = "SELECT F16cCodPro,F16cCodArt,F16cSitStk,F16cNumPal," + ;
			 " F16cNumLot, " + GetCvtNvl(_ENTORNO, _VERSION, "F16tPesEnt") + " As F16tPesEnt," + ;
			 " F08cPesUni,F08cVolUni,F16cCanFis As Contador " + ;
			 " FROM F16C" + _em + ",F16t" + _em + ",F08c" + _em + ;
			 " Where F16tNumPal(+) = F16cNumPal" + ;
			 " And F08cCodPro = F16cCodPro " + ;
			 " And F08cCodArt = F16cCodArt " 

_Ok = Sqlexec(_Asql,_Sentencia,'Fotoprix')
If _Ok < 1
 	 =MessageBox(' Error al extraer información de la F16C ' + Chr(13) + message() + ' ',16,'TUNDEN')
   	 =Sqlrollback(_Asql)
     Return   
EndIf

=SqlMoreREsults(_ASql)
Select FotoPrix
Go Top
Do While !Eof()


    Pes = IIF(FotoPrix.F16tPesEnt=0,Str(FotoPrix.Contador * Fotoprix.F08cPesUni,10,3),Str(FotoPrix.F16tPesEnt))
	Vol = str(FotoPrix.Contador * Fotoprix.F08cVolUni,10,3)

	*> Buscar si existe.--------------------------------------
	_Sentencia = " SELECT * FROM F30L" + _em + ;
			 	 " WHERE F30LCodPro = '" + Fotoprix.F16cCodPro + "' And F30lCodArt = '" + Fotoprix.F16cCodArt + "'" + ;
			 	 " AND F30lSitStk = '" + Fotoprix.F16cSitStk + "'" + ;
			 	 " And F30lFecha = " + GetCvtDate(_ENTORNO, _VERSION, Fecha) + ;
			 	 " And F30lNumPal = '" + Fotoprix.F16cNumPal + "' And F30lNumLot = '" + Fotoprix.F16cnumlot + "'"
	_Err = SqlExec(_Asql,_Sentencia,'SEEKF30L')

	If _Err < 0
		=MessageBox(' Error al buscar la información en F30L ' + Chr(13) + message() + ' ',16,'TUNDEN')
 		=Sqlrollback(_Asql)
   		Return   
	EndIf
	
	SELECT SEEKF30L
	Go Top
	if !Eof()

		_Sentencia = " DELETE FROM F30L" + _em + ;
				 	 " WHERE F30LCodPro = '" + Fotoprix.F16cCodPro + "' And F30lCodArt = '" + Fotoprix.F16cCodArt + "'" + ;
				 	 " AND F30lSitStk = '" + Fotoprix.F16cSitStk + "'" + ;
				 	 " And F30lFecha = " + GetCvtDate(_ENTORNO, _VERSION, Fecha) + ;
				 	 " And F30lNumPal = '" + Fotoprix.F16cNumPal + "' And F30lNumLot = '" + Fotoprix.F16cnumlot + "'"
		_Err = SqlExec(_Asql,_Sentencia)
		If _Err < 0
   			=MessageBox(' Error al borrar la información en F30L ' + Chr(13) + message() + ' ',16,'TUNDEN')
   	 		=Sqlrollback(_Asql)
     		Return   
		EndIf
		=SqlCommit(_Asql)
	EndIf


	_Sentencia = " INSERT INTO F30l" + _em + "(F30lCodPro,F30lCodArt,F30lFecha,F30lTotVol," + ;
				 " F30lTotPes,F30lSitStk,F30lStock,F30lNumPal,F30lNumLot)" + ;
				 " VALUES( '" + Fotoprix.F16cCodPro + "','" + Fotoprix.F16cCodArt + "'," + ;
				 GetCvtDate(_ENTORNO, _VERSION, Fecha) + ;
				 " ," + Vol +  ;
				 " ," + Pes +  ;				 
				 " ,'" + Fotoprix.F16cSitStk + "'," + str(Fotoprix.Contador,10) + ;
				 " ,'" + Fotoprix.F16cNumPal + "','" + Fotoprix.F16cNumLot + "')"

	_Err = SqlExec(_Asql,_Sentencia)
	If _Err < 0
   		=MessageBox(' Error al insertar la información en F30L ' + Chr(13) + message() + ' ',16,'TUNDEN')
   	 	=Sqlrollback(_Asql)
     	Return   
	EndIf


	Select FotoPrix
	Skip
EndDo

*> Borrar la información existente del día de cierre.
_Sentencia = " DELETE FROM F30d" + _em + ;
             " WHERE F30dFecha=" + GetCvtDate(_ENTORNO, _VERSION, Fecha)

_Err = SqlExec(_Asql,_Sentencia)
If _Err <= 0
	=MessageBox(' Error al borrar la información en F30D ' + Chr(13) + message() + ' ',16,'TUNDEN')
	=Sqlrollback(_Asql)
	Return   
EndIf
=SqlCommit(_Asql)


*> Extraemos información de las ocupaciones.-----------------------------------------------------
_Sentencia =''
_Sentencia = "SELECT F30lCodPro As F16cCodPro,F30lCodArt As F16cCodArt ,F30lSitStk As F16cSitStk,F30lNumPal As F16cNumPal,SUM(F30lStock)As Contador FROM F30l" + _em + ;
		 	 " Where F30lFecha = " + GetCvtDate(_ENTORNO, _VERSION, Fecha) + ;
 			 " GROUP BY F30lCodPro,F30lCodArt,F30lSitStk,F30lNumPal "

_Err = SqlExec(_Asql,_Sentencia,'F16cInfo')
If _Err < 0
   	 =MessageBox(' Error al extraer información de la F16C ' + Chr(13) + message() + ' ',16,'TUNDEN')
   	 =Sqlrollback(_Asql)
     Return   
EndIf			 
=sqlmoreresults(_Asql)

		  
Select F16cInfo
Go Top
Do While !Eof()

	*> Sacamos información del artículo
	_Sentencia = "SELECT * FROM F08C" + _em + ;
				 " WHERE F08cCodPro = '" + F16cInfo.F16cCodPro + "' And F08cCodArt = '" + F16cInfo.F16cCodArt + "'"
	_Err = SqlExec(_Asql,_Sentencia,'F08cInfo')
	If _Err < 0
   		=MessageBox(' Error al extraer información de la F08C ' + Chr(13) + message() + ' ',16,'TUNDEN')
   	 	=Sqlrollback(_Asql)
     	Return   
	EndIf			 
	=sqlmoreresults(_Asql)
	
    Select F16cInfo
	*> Obtener el peso del F16t.

    Pes = CalcPesoMovimiento(F16cCodPro, F16cNumPal, Contador)
    If Pes <= 0
       *> Si no se ha pesado el palet, calcular peso a partir de la ficha del artículo.
	   Pes = Str(F16cInfo.Contador * F08cInfo.F08cPesUni, 10, 3)
	EndIf

	*> Hacemos el cálculo para peso y volumen total.
	Vol = str(F16cInfo.Contador * F08cInfo.F08cVolUni,10,3)
*	Pes = str(F16cInfo.Contador * F08cInfo.F08cPesUni,10,3)
	
	*> Al ser un campo de longitud fija, si Peso o Volumen es un valor elevado, lo asignamos para ahorrarnos el error
	If Val(Vol) > 9999999
		Vol = '9999999'
	EndIf
	
	If Type('PES') = 'N'
		If Pes > 9999999
			Pes = '9999999'
		EndIf
	Else
		If Val(Pes) > 9999999
			Pes = '9999999'
		EndIf
	EndIF
	

	If Type('PES') = 'N'
	   Pes = Str(Pes,10,3)
	EndIf

	If Type('VOL') = 'N'
	   Vol = Str(Vol,10,3)
	EndIf

	*> Buscamos si esta información	para este día existe en la F30D
	*> en caso que la información con fecha de hoy ya existiera
	*> lo borramos del F30D e insertamos la nueva información.

	_Sentencia = ''
	_Sentencia = " SELECT * FROM F30D" + _em + ;
			 	 " WHERE F30dCodPro = '" + F16cInfo.F16cCodPro + "' And F30dCodArt = '" + F16cInfo.F16cCodArt + "'" + ;
			 	 " AND F30dSitStk = '" + F16cInfo.F16cSitStk + "'" + ;
			 	 " And F30dFecha = " + GetCvtDate(_ENTORNO, _VERSION, Fecha)

	_Err = SqlExec(_Asql,_Sentencia,'SEEKF30D')
	If _Err <= 0
		=MessageBox(' Error al buscar la información en F30D ' + Chr(13) + message() + ' ',16,'TUNDEN')
 		=Sqlrollback(_Asql)
   		Return   
	EndIf
		
	SELECT SEEKF30D
	Go Top
	If !Eof()
		*> Ya existe: añadir peso, volumen y la cantidad.
		_Sentencia = " UPDATE F30d" + _em + ;
					 " SET F30dTotVol = F30dTotVol + " + Vol + ", " + ;
					 "     F30dTotPes = F30dTotPes + " + Pes + ", " + ;
					 "     F30dStock  = F30dStock  + " + str(F16cInfo.Contador,10) + ;
				 	 " WHERE F30dCodPro = '" + F16cInfo.F16cCodPro + "' And F30dCodArt = '" + F16cInfo.F16cCodArt + "'" + ;
				 	 " AND F30dSitStk = '" + F16cInfo.F16cSitStk + "'" + ;
				 	 " And F30dFecha = " + GetCvtDate(_ENTORNO, _VERSION, Fecha)

		_Err = SqlExec(_Asql,_Sentencia)
		If _Err <= 0
   			=MessageBox(' Error al actualizar la información en F30D ' + Chr(13) + message() + ' ',16,'TUNDEN')
   	 		=Sqlrollback(_Asql)
     		Return
		EndIf
		=SqlCommit(_Asql)
	Else
		*> No existe: Insertar registro.
		_Sentencia = " INSERT INTO F30d" + _em + ;
		             " (F30dCodPro,F30dCodArt,F30dFecha,F30dTotVol,F30dTotPes,F30dSitStk,F30dStock)" + ;
					 " VALUES( '" + F16cInfo.F16cCodPro + "','" + F16cInfo.F16cCodArt + "'," + ;
					 GetCvtDate(_ENTORNO, _VERSION, Fecha) + ;
					 " ," + Vol +  ;
					 " ," + pes +  ;
					 " ,'" + F16cInfo.F16cSitStk + "'," + str(F16cInfo.Contador,10) + " )"

		_Err = SqlExec(_Asql,_Sentencia,'INSERTADO')
		If _Err <= 0
	   		=MessageBox('Error al insertar información en F30D ' + Chr(13) + message() + ' ',16,'TUNDEN')
	   	 	=Sqlrollback(_Asql)
	     	Return
		EndIf
		=SqlCommit(_Asql)
	EndIf

	Vol = 0
	Pes = 0
	Select F16cInfo
	Skip
EndDo
=sqlcommit(_Asql)



