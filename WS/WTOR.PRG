
*!* EJEMPLO

Local tcUser, tcPassword, tcNumero, lcError

tcUser = "Torsesa\wsuser"
tcPassword = "T0rs3s4"

If fWSPrueba(tcUser, tcPassword, 2000, @tcNumero, @lcError) > 0
	? "Número: " + tcNumero
Else
	=MessageBox(lcError, 0+16+256+4096, "Error")
EndIf

**************************************************************************
*-- Funcion: fWSPrueba
*--    almacena en un array por referencia el estado del clima

*-- Parametros tcPais: String, Nombre del pais
*--    tcUser, String. Usuario.
*--    tcPassword, String. Contraseña.
*--    tcNumero: Double. Número a validar.
*--    tcResultado: String. Texto informando del número recibido.
*--    tcError: String por referencia, almacena error.
*-- Retorna  0: Error, >1: WebService consumido correctamente.

*-- Fecha:    2015/08/05
*-- (c) Jose Guillermo Ortiz Hernandez & Abbe
**************************************************************************
Function fWSPrueba

	Lparameters tcuser, tcPassword, lnNumero, tcResultado, tcError
 
	Local loSoapClient, loSoapClientUrl, lnResult
	Local tcResultado, loError

	lnResult = 0
	tcResultado = ""

	*-- Creando web Service.
	Try
		loSoapClient   = CreateObject("MSSOAP.SoapClient30")
		loSoapClientUrl  = [http://62.82.59.121:7047/DynamicsNAV80/WS/CRONUS%20España%20S.A./Codeunit/PruebasSGA]

		If Vartype(loSoapClient)=='O'
			loSoapClient.MSSoapInit(loSoapClientUrl, tcUser, tcPassword)
			tcResultado = loSoapClient.WSprueba(lnNumero)
		Else
   			tcError = "Error: " + lcXml
   			lnResult = -1
		EndIf
 
	Catch To loError
		tcError = "Error: " + loError.Message
   		lnResult = -2
	EndTry

	Return lnResult
 
 EndFunc
 