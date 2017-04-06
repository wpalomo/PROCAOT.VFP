
*> Ordeñar ubicaciones según calle.

Procedure OrdenarUbicacion
Parameters nLista
Private Valor, Contador, OrdRec, x

Valor=9999
Contador=0
OrdRec=0
Cuenta=0

Wait Window 'Obteniendo Ubicaciones.....' NoWait 

_Sentencia="Select * From F26l001 Where F26lNumLst='" + nLista + "' Order by Substring(F26lUbiOri, 7, 8)"
Sw=SqlExec(_Asql,_Sentencia,'Ubic')

Create Cursor OrUbi (NumMov C(10),Calles C(2), Rango N(2), Ubicac C(14))

Select Ubic
Go Top
Do While !Eof()
	Select OrUbi
	Append Blank
	Replace NumMov With Ubic.F26lNumMov, ; 
			Calles With SubStr(Ubic.F26lUbiOri, 7, 2), ;
			Ubicac With Ubic.F26lUbiOri
	Select Ubic
	Skip
EndDo

Cuenta=1
Select OrUbi
Go Top
CalleAnterior=OrUbi.Calles
Do While !Eof()
	If CalleAnterior<>OrUbi.Calles
		Cuenta=Cuenta + 1 
		CalleAnterior=OrUbi.Calles
	EndIf
	Replace OrUbi.Rango With Cuenta
	Select OrUbi
	Skip
EndDo

Select OrUbi
Go Top
x=RecCount()

Select OrUbi
Go Top
Do While !Eof()
	Wait Window 'Ordenando ubicaciónes ' + OrUbi.Ubicac +  Str(Contador) + ' de ' + Str(x) NoWait 
	Contador=Contador + 1
	Resultado=Mod(Rango, 2)
	
	If Resultado=0
		OrdRec=(Rango * 10000) + Contador
	Else
		OrdRec=(Rango * 10000) + (Valor - Contador)
	EndIf
			
	_Sel="Update F14c001 Set F14cVenHab=" + Str(OrdRec) + " Where F14cNumMov='" + OrUbi.NumMov + "'"
	 Sw=SqlExec(_Asql,_Sel)
	 
	 _Sel="Update F26l001 Set F26lVenHab=" + Str(OrdRec)+ " Where F26lNumMov='" + OrUbi.NumMov + "'"
	 Sw=SqlExec(_Asql,_Sel)
	
	Select OrUbi
	Skip
EndDo

Return
 
