
Set ClassLib  To St3Class
Set ClassLib  To Alisfour    Additive
Set ClassLib  To Procaot     Additive
Set ClassLib  To Apilamiento Additive

Set ClassLib  To Actualizar  Additive
Set ClassLib  To Inventario  Additive
Set ClassLib  To Reserva     Additive
Set ClassLib  To Preparacion Additive
Set ClassLib  To Reposicion  Additive
Set ClassLib  To Ubicar      Additive
Set ClassLib  To Stocks      Additive
Set ClassLib  To ProBATCH    Additive

Set Procedure To St3Rt
Set Procedure To Ora_Proc    Additive
Set Procedure to Wproc001    Additive
Set Procedure To WProcOra    Additive
Set Procedure To Ora_Proc    Additive
Set Procedure To FncList     Additive
Set Procedure To ProcRepos   Additive
Set Procedure To Procesos    Additive

Private oBatch, cParams
Public cr, _asql, _em, _debugsql, _esc, _cm

cr = Chr(13) + Chr(10)
_cm = "'"
_em = '001'
_asql = SqlConnect()

Use SYSFC In 0 Order 1 Shared NoUpdate
set step on


oBatch = CreateObject("Batch")

With oBatch
	.Inicializar

*!*		cParams = "PROPIETARIO='000000',TIPODOCUMENTO='2000',DOCUMENTO='060613001    '"
*!*		lStado = .Execute("CPS2000", "PENTAT", cParams)
*!*	set step on

*!*		cParams = "ENTRADA='0000000267',IDOCUPACION='0000001607'"
*!*		lStado = .Execute("PAL1000", "PENTAT", cParams)
*!*	set step on

*!*		lStado = .ExecuteTask(.TaskID)
*!*		set step on
*!*		lStado = .ExecuteAllTasks()

	set step on
	lStado = .ExecuteTask('_2KZ0L47E3')

EndWith
	
=SqlDisconnect(_asql)

Use In (Select ("sysfc"))