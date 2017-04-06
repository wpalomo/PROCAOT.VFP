*>Recalcular peso y bultos cada vez que se pese un Mac
*>Mirar tambien que todo el documento tiene mac.....
Parameters cNumMac
Private _Ok, _PesoKg, _Bultos

If Used('cDocs')
	Use In cDocs
EndIf


If Used('cMovi')
	Use In cMovi
EndIf


If Used('cPesos')
	Use In cPesos
EndIf


*>Quiero saber en cuantos documentos distintos se encuentra el Mac.
_Sentencia="Select Distinct F14cCodPro,F14cTipDoc,F14cNumDoc " + ;
		   "From F14c001 Where " + ;
		   "F14cNumMac='" + cNumMac + "' Order By F14cCodPro,F14cTipDoc,F14cNumDoc "
		   
_Ok=SqlExec(_Asql,_Sentencia,'cDocs')
=SqlMoreResults(_Asql)		   

If _Ok < 0
	*> De momento no se hace nada...
	Return
EndIf

Select cDocs
Go Top
Do While !Eof()
	*>Busco si el documento esta todo a '2999'
	_Sentencia="Select * From F14c001 Where " + ;
			   "F14cCodPro='" + cDocs.F14cCodPro + "' And " + ;	
			   "F14cTipDoc='" + cDocs.F14cTipDoc + "' And " + ;
			   "F14cNumDoc='" + cDocs.F14cNumDoc + "' And " + ;			   			   
			   "F14cTipMov<>'2999' "

	_Ok=SqlExec(_Asql,_Sentencia,'cMovi')
	=SqlMoreResults(_Asql)		   
	
	If _Ok < 0
		*> De momento no se hace nada...
		Return
	EndIf

	
	Select cMovi
	Go Top
	If Eof()
		*>Esta todo preparado, ya podemos recalcular.........
		_Sentencia="Select Distinct F14cNumMac,F26pPesMac,F26pVolume  From F14c001,F26p001 Where " + ;
				   "F14cCodPro='" + cDocs.F14cCodPro + "' And " + ;	
				   "F14cTipDoc='" + cDocs.F14cTipDoc + "' And " + ;
				   "F14cNumDoc='" + cDocs.F14cNumDoc + "' And " + ;			   			   
				   "F14cNumMac=F26pNumMac "

		_Ok=SqlExec(_Asql,_Sentencia,'cPesos')
		=SqlMoreResults(_Asql)		   

		If _Ok < 0
			*> De momento no se hace nada...
			Return
		EndIf
		
		Select cPesos
		Go Top
		If !Eof()
			*>Calculo de Peso
			Store 0 To _PesoKg, _Bultos, _Volumen
			Select cPesos
			Go Top
			Sum cPesos.F26pVolume To _Volumen

			Select cPesos
			Go Top
			Sum cPesos.F26pPesMac To _PesoKg

			
			Select cPesos
			Go Top
			_Bultos=RecCount()
			
			*>Por fin update en F24c001.
		    _Sentencia = "Update F24c001 Set F24cTotKgs=" + Str(_PesoKg) + ", F24cTotVol=" + Str(_Volumen,9,3) + ", F24cNumBul=" + Str(_Bultos) + " Where " + ;
		    			 "F24cCodPro = '" + cDocs.F14cCodPro + "' And " + ;
		    			 "F24cTipDoc = '" + cDocs.F14cTipDoc + "' And " + ;
		    			 "F24cNumDoc = '" + cDocs.F14cNumDoc + "'"

		    _Ok = SqlExec(_Asql,_Sentencia)

			If _Ok < 0
				*> De momento no se hace nada...
				Return
			EndIf
		    

		EndIf
	EndIf

	Select cDocs
	Skip
EndDo

Return
