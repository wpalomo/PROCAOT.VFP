_Asql=SqlConnect('Torsesa','sa','sa')

set step on
_Sentencia="SELECT DISTINCT F24LNUMDOC FROM F24L001 WHERE F24LCANENV=F24LCANDOC"
=SqlExec(_Asql,_Sentencia,'cTodo')

Select cTodo
Go Top
Do While !Eof()
	_Sentencia="Select * From F24l001 Where F24lNumDoc='" + cTodo.F24lNumDoc + "'"
	_ok=SqlExec(_Asql,_Sentencia,'cTodos')
	
	Select cTodos
	Go Top
	If Eof()
		Select cTodo
		Skip
		Loop
	EndIf
	
	Locate For F24lCanDoc<>F24lCanEnv
	If !Found()
		*>Cerramos cabecera a 6
		_Sentencia="Update F24c001 Set F24cFlgest='8' Where F24cNumDoc='" + cTodo.F24lNumDoc + "'"
		_ok=SqlExec(_Asql,_Sentencia)
	EndIf
	Select cTodo
	Skip
EndDo
c=0
Return