
*>----------------------------------------------------------------
*> Test de exportaci�n de movimientos de almac�n al WS - Torsesa.
*>----------------------------------------------------------------

Local oWS, oErr, cMessage, cTitle, vMovimiento, nAsql
Public _Entorno, _Version

Set Step On

*>>>>>>>> Uso en SGA.

Try
	_Entorno = 'SQLSERVER'
	_Verson = '8.0'
	nASql = SQLConnect()

	cTitle = "OK"
	cMessage = "OK"
	vMovimiento = '0000900093'

	Set Classlib To procaot Additive
	Set Classlib To WS Additive

	oWS = CreateObject("WSExport", nASql, 'SQLSERVER', '8.0')

  	With oWS
		.Inicializar
		=.WSLocationEntry(vMovimiento)
		.Finalizar
	EndWith

Catch To oErr
	cMessage = "Error No. " + Alltrim(Str(oErr.ErrorNo)) + Chr(13) + Chr(10)
	cMessage = cMessage + "Error: " + oErr.Message
	cTitle = "ERROR"

Finally
	=MessageBox(cMessage, 48, cTitle)

EndTry

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*>>>>>>>> Uso Test.

*!*	Try
*!*		*> oWS = NewObject("WSExport.WSEClass", "http://192.168.1.8:7057/DynamicsNAV80_Pruebas/WS/PRUEBAS%20TORSESA/Codeunit/WSSGA", "lreyes", "lreyes2015")

*!*		oWS = NewObject("WSExport.WSEClass")
*!*		? oWS.SetCredenciales("http://192.168.1.8:7057/DynamicsNAV80_Pruebas/WS/PRUEBAS%20TORSESA/Codeunit/WSSGA", "lreyes", "lreyes2015")

*!*		? oWS.SendMovimiento("1234567890","2993","1001","L1000","31/12/2016","AZUL","1000",300,"03/09/2015")
*!*		*? oWS.SendMovimientoExt("1234567890","2999","0001A 01001040","1000","L1000","31/12/16","AZUL","1000",300,"03/09/15","123456","AC13165","N")
*!*		*? oWS.SendMovimientoTest()

*!*		cMessage = "Mje No. " + Alltrim(Str(oWS.StatusCode)) + Chr(13) + Chr(10)
*!*		cMessage = cMessage + "Mje: " + Mline(oWS.StatusText, 1)
*!*		cTitle = oWS.Result

*!*	Catch To oErr
*!*		cMessage = "Error No. " + Alltrim(Str(oErr.ErrorNo)) + Chr(13) + Chr(10)
*!*		cMessage = cMessage + "Error: " + oErr.Message
*!*		cTitle = "ERROR"

*!*	Finally
*!*		=MessageBox(cMessage, 48, cTitle)

*!*	EndTry

Release oWS
