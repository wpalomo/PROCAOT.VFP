_Asql=SqlConnect('Procaot','sa','sa')
set step on
_Sentencia="select * from f18n001 where f18nlinped=0 order by f18nnumdoc"
_ok=SqlExec(_Asql,_Sentencia,'cEntr')

Select cEntr
Go Top
Do While !Eof()
	_Sentencia="select * from f18l001 where " + ;
		       "f18lcodpro='" + cEntr.F18nCodPro + "' and " + ;
		       "f18ltipdoc='" + cEntr.F18nTipDoc + "' and " + ;
			   "f18lnumdoc='" + cEntr.F18nNumDoc + "' and " + ;
		 	   "f18lcodart='" + cEntr.F18nCodArt + "'"

	_ok=SqlExec(_Asql,_Sentencia,'cEntr2')
	Select cEntr2
	Go Top
	If !Eof()
		Select cEntr
		_Sentencia="Update f18n001 Set F18nLinPed=" + Str(Val(cEntr2.f18lcodcli)) + " where  " +;
			       "f18ncodpro='" + cEntr.F18nCodPro + "' and " + ;
			       "f18ntipdoc='" + cEntr.F18nTipDoc + "' and " + ;
				   "f18nnumdoc='" + cEntr.F18nNumDoc + "' and " + ;
			 	   "f18ncodart='" + cEntr.F18nCodArt + "'"

		_ok=SqlExec(_Asql,_Sentencia)
			
	EndIf
	
	Select cEntr
	Skip
EndDo

