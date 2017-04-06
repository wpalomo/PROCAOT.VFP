
*!* EJEMPLO

Local laTiempo[1], lcError

If fGetClima("Spain", "Madrid", @laTiempo, @lcError) > 0
	? "Ubicacion: "+laTiempo[1]
	? "Fecha y Hora: "+laTiempo[2]
	? "Humedad relativa: "+laTiempo[3]
	? "Estado: "+laTiempo[4]
	? "Temperatura: "+laTiempo[5]
Else
	=MessageBox(lcError, 0+16+256+4096, "Error")
EndIf

**************************************************************************
*-- Funcion: fGetClima
*--    almacena en un array por referncia el estado del clima
*--
*-- Parametros tcPais: String, Nombre del pais
*--    tcCiudad: String, Nombre de la ciudad
*--    taResultado: Array por referencia, estado del clima
*--    tcError: String por referencia, almacena error
*-- Retorna  0:Error, >1:webService consumido correctamente
*--
*-- Fecha:    2012/01/10
*-- Creo  Jose Guillermo Ortiz Hernandez
**************************************************************************
Function fGetClima

	Lparameters tcPais, tcCiudad, taResultado, tcError
 
	Local loSoapClient, loSoapClientUrl, lnResult
	Dimension taResultado[5]

	lnResult = 0

	*!* Creando web Service
	Try
		loSoapClient   = CreateObject("MSSOAP.SoapClient30")
		loSoapClientUrl  = [http://www.webservicex.net/globalweather.asmx?WSDL]

		If Vartype(loSoapClient)=='O'
			loSoapClient.MSSoapInit(loSoapClientUrl)
			lcXml = loSoapClient.GetWeather(tcCiudad,tcPais)
		EndIf
  
		If "CURRENTWEATHER" $ Upper(lcXml)
			lnResult = Alen(taResultado,1)
			taResultado[1]=StrExtract(lcXml,"<Location>","</Location>")
			taResultado[2]=StrExtract(lcXml,"<Time>","</Time>")
			taResultado[3]=StrExtract(lcXml,"<RelativeHumidity>","</RelativeHumidity>")
			taResultado[4]=StrExtract(lcXml,"<Status>","</Status>")
			taResultado[5]=StrExtract(StrExtract(lcXml,"<Temperature>","</Temperature>"),"(",")")
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
 