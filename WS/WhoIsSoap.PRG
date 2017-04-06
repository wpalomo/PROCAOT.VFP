
*!* EJEMPLO

Local laTexto[1], lcError

If fGetWhoIs("renovatio2k9.net", @laTexto, @lcError) > 0
	? "Texto: " + laTexto[1]
Else
	=MessageBox(lcError, 0+16+256+4096, "Error")
EndIf

**************************************************************************
*-- Funcion: fGetWhoIs
*--    almacena en un array por referncia los datos de dominio
*--
*-- Parametros tcDomain: String, Nombre del dominio
*--    taResultado: Array por referencia, valores
*--    tcError: String por referencia, almacena error
*-- Retorna  0:Error, >1:webService consumido correctamente

*-- Fecha:	2015/07/10
*--			Abbe Cortes
**************************************************************************
Function fGetWhoIs

	Lparameters tcDomain, taResultado, tcError
 
	Local loSoapClient, loSoapClientUrl, lnResult
	Dimension taResultado[5]

	lnResult = 0

	*!* Creando web Service
	Try
		loSoapClient   = CreateObject("MSSOAP.SoapClient30")
		loSoapClientUrl  = [http://www.webservicex.net/whois.asmx?WSDL]

		If Vartype(loSoapClient)=='O'
			loSoapClient.MSSoapInit(loSoapClientUrl)
			lcXml = loSoapClient.GetWhoIS(tcDomain)
		EndIf
  
		taResultado[1] = lcXml
		lnResult = 1
 
	Catch To loError
		tcError = "Error: " + loError.Message
   		lnResult = -2
	EndTry

	Return lnResult
 
 EndFunc
 