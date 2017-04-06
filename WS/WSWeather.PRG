
Local oWS, lcRespuesta

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>------------- Datos del tiempo atmosférico de una ciudad.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

oWS = CreateObject("VFP_WebService", "http://www.webservicex.net/globalweather.asmx?WSDL")
lcRespuesta = oWS.GetWeather("Madrid", "Spain")
If oWS.iStatus != 0
	MessageBox(oWS.sError,16,"Error al ejecutar WS")
Else
	MessageBox(lcRespuesta,64,"EjecutandoWS desde VFP - PortalFOX")
EndIf

Release oWS

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Define Class VFP_WebService As Custom

	* --- Definimos las propiedades ---
	sError = ""
	iStatus = 0	
	sURL_WS = ""

	* --- Definimos la función GetWeather del WebService ---
	*---------------------------------------------------
	Function GetWeather (tcCiudad, tcPais)
	*---------------------------------------------------

		* --- Paso 1. Creo el XML Request ---
		sXMLRequest = This.CreaRequest(tcCiudad, tcPais)

		pXMLResponse = Addbs(Sys(2023)) + Sys(2015) + [.xml]

		* --- Paso 2. Ejecuto el WS | Paso 3. Obtengo el Response ---
		This.iStatus =  This.EjecutaWS(This.sURL_WS, sXMLRequest, pXMLResponse)

		If This.iStatus != 0  && Ocurrió un error el cual está especificado en sError.
			Return ""
		EndIf

		sXMLResponse = FileToStr(pXMLResponse)
		* --- Parseamos el XML Response ---
		* --- Para el ejemplo está así, manejando texto, ustedes deben manejar XML (falta de tiempo, perdón) ---

		sRespuestaWS = "Location:" + Chr(9) + StrExtract(sXMLResponse,'&lt;Location&gt;','&lt;/Location&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "Time:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;Time&gt;','&lt;/Time&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "Wind:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;Wind&gt;','&lt;/Wind&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "Visibility:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;Visibility&gt;','&lt;/Visibility&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "SkyConditions:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;SkyConditions&gt;','&lt;/SkyConditions&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "Temperature:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;Temperature&gt;','&lt;/Temperature&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "DewPoint:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;DewPoint&gt;','&lt;/DewPoint&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "RelativeHumidity:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;RelativeHumidity&gt;','&lt;/RelativeHumidity&gt;')+CHR(13)+CHR(10)
		sRespuestaWS = sRespuestaWS + "Pressure:"+CHR(9)+ StrExtract(sXMLResponse,'&lt;Pressure&gt;','&lt;/Pressure&gt;')

		This.BorraArchivo(pXMLResponse)

		Return sRespuestaWS
	
	EndFunc

	*-------------------------------------------------------------
	Function EjecutaWS (pURL_WSDL, pFileRequest , pFileResponse)
	*-------------------------------------------------------------
		Try
			oHTTP = CreateObject('Msxml2.ServerXMLHTTP.6.0')
			oHTTP.Open("POST", pURL_WSDL, .F.)
			oHTTP.setRequestHeader("User-Agent", "EjecutandoWS desde VFP - PortalFOX")
			oHTTP.setRequestHeader("Content-Type", "text/xml;charset=utf-8")
			oHTTP.SEND(pFileRequest)

		Catch To loErr
			This.sError = "Error: " + Transform(loErr.ErrorNo) +  " Mensaje: " + loErr.Message
			This.iStatus = -1	   		
		EndTry
 
	   If This.iStatus != 0
			Return -1
	   EndIf

	    * --- Si el status es diferente a 200, ocurrió algún error de conectividad con el WS ---
	    If oHTTP.Status = 200
	        RespuestaWS = oHTTP.responseText
		    * --- Se genera el XML del response | Este es el paso 3!! ---
		    StrToFile(Strconv(RespuestaWS, 9), pXMLResponse)
		    This.iStatus = 0
		    This.sError = ""
		    Return 0
	    Else
	        This.sError = "Error: No se logró la conexión con el Web Service."
	        This.iStatus = -1
			Return -1
	    EndIf

	EndFunc
	*---------------------------------------------------

	*---------------------------------------------------
	Function CreaRequest(tcCiudad, tcPais)
	*---------------------------------------------------
		Text To sXMLRequest TEXTMERGE NOSHOW
		<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
		   xmlns:web="http://www.webserviceX.NET">
		   <soapenv:Header/>
		   <soapenv:Body>
		      <web:GetWeather>
		         <web:CityName><<tcCiudad>></web:CityName>
		         <web:CountryName><<tcPais>></web:CountryName>
		      </web:GetWeather>
		   </soapenv:Body>
		</soapenv:Envelope>
		EndText 

		Return sXMLRequest

	EndFunc
	*---------------------------------------------------

	*---------------------------------------------------
	Function BorraArchivo(pFile)
	*---------------------------------------------------
		If File(pFile)
			Delete File (pFile)
		EndIf

	EndFunc
	*---------------------------------------------------

	*---------------------------------------------------
	* Evento constructor
	Procedure Init
	*---------------------------------------------------
		Lparameters tcURLWS

        This.sURL_WS = tcURLWS
        This.iStatus = 0
        This.sError = ""

	EndProc
	*---------------------------------------------------

EndDefine
