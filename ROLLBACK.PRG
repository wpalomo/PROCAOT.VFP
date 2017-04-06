SET STEP ON
_Asql=SqlConnect('TORSESA','','')



IF (SQLSetProp(_Asql,"Transactions",2) > 0)
	*** Inician las transacciones
	_Sentencia="Insert Into F05c001 Values ('0007','OPERARIO ALISFOUR','OPEALIS','S','S','CONN')"
	_ok=SqlExec(_Asql,_Sentencia)
	If _ok > 0
		=SQlCommit(_Asql)
	Else
		=SQlRollback(_Asql)
	EndIf		
ELSE
	*** Error al intentar las transacciones
	IF (AERROR(laError) > 0)
	ENDIF
ENDIF 

Return