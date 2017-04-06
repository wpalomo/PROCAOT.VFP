_Asql = SqlConnect()

	_Sentencia="Select F16cNumPal,F16cCodUbi From F16c001 Where F16cCodPro = '000148' Order by F16cCodUbi "
	Sw=SqlExec(_Asql,_Sentencia, 'OcuPal')
	=SqlMoreResults(_Asql)
 
 	Select OcuPal
 	NUbi=OcuPal.F16cCodUbi
	NPal=OcuPal.F16cNumPal
 	Do While !Eof()
 		If NUbi=OcuPal.F16cCodUbi
			_Sel = "Update F16c001 Set F16cNumPal='" + NPal + "' Where F16cCodUbi='" + NUbi + "'"
			Sw=SqlExec(_Asql,_Sel)
 		Else
		 	NUbi=OcuPal.F16cCodUbi
 			NPal=OcuPal.F16cNumPal
 		EndIf
        Select OcuPal 		
		Skip
     EndDo	
     =SqlCommit(_Asql)
     
     
     
     
     	