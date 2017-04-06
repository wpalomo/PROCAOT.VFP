set step on
_Asql=SqlConnect('PROCAOT','sa','sa')
_AsqlPre=SqlConnect('PROCAOT_FICHEROS','sa','sa')

=SqlExec(_AsqlPre,"Select * From F08c001 Where F08cUltLot<>''",'cNada')

Select cNada
Go Top
Do While !Eof()
	_Sentencia="Update F08c001 Set F08cUltLot='" + cNada.F08cUltLot + "' Where " + ;
			   "F08cCodPro='" + cNada.F08cCodPro + "' And F08cCodArt='" + cNada.F08cCodArt + "'"
	=SqlExec(_Asql,_Sentencia)			   

	Select cNada
	Skip
EndDo

Return