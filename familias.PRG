_Asql=SqlConnect('Torsesa','sa','sa')

set step on
_Sentencia="Select Distinct F08cTipPro,F08cTipFam From F08c001 Where F08cTipPro<>'9999'"
=SqlExec(_Asql,_Sentencia,'cartis')


Select cArtis
Go Top
Do While !Eof()
	_Sentencia="Update F08c001 Set F08cTipPro='" + cArtis.F08cTipPro + "' Where " + ;
			   "F08cTipFam='" + cArtis.F08cTipFam + "' And F08cTipPro='9999' "
	=SqlExec(_Asql,_Sentencia)
				    
	Select cArtis
	Skip
EndDo


Return