
Local oWS, lcRespuesta

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>------------- Datos de un dominio web.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Set Step On

oWS = CreateObject("VFP_WebService", "http://merlyna.com/merlyna/abc/webserviceSRI-RegistroCivil.php?wsdl")
lcRespuesta = oWS.GetWhoIs("renovatio2k9.net")
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

	* --- Definimos la función GetWhoIs del WebService ---
	*---------------------------------------------------
	Function GetWhoIs (tcDominio)
	*---------------------------------------------------

		Local nPosIni, nPosFin
		Local sRespuestaWS, pXMLResponse, sXMLRequest

		* --- Paso 1. Creo el XML Request ---
		*> sXMLRequest = This.CreaRequest11(tcDominio)		&& SOAP 1.1
		sXMLRequest = This.CreaRequest12(tcDominio)			&& SOAP 1.2

		pXMLResponse = Addbs(Sys(2023)) + Sys(2015) + [.xml]

		* --- Paso 2. Ejecuto el WS | Paso 3. Obtengo el Response ---
		This.iStatus =  This.EjecutaWS(This.sURL_WS, sXMLRequest, pXMLResponse)

		If This.iStatus != 0  && Ocurrió un error el cual está especificado en sError.
			Return ""
		EndIf

		sXMLResponse = FileToStr(pXMLResponse)
		* --- Parseamos el XML Response ---
		* --- Para el ejemplo está así, manejando texto, ustedes deben manejar XML (falta de tiempo, perdón) ---

		nPosIni = Atc("domain name:", sXMLResponse)
		nPosFin = Atc(Chr(10), Substr(sXMLResponse, nPosIni))
		sRespuestaWS = Chr(9) + Substr(sXMLResponse, nPosIni, nPosFin) + Chr(13) + Chr(10)

		nPosIni = Atc("whois server:", sXMLResponse)
		nPosFin = Atc(Chr(10), Substr(sXMLResponse, nPosIni))
		sRespuestaWS = sRespuestaWS + Substr(sXMLResponse, nPosIni, nPosFin) + Chr(13) + Chr(10)

		nPosIni = Atc("updated date:", sXMLResponse)
		nPosFin = Atc(Chr(10), Substr(sXMLResponse, nPosIni))
		sRespuestaWS = sRespuestaWS + Substr(sXMLResponse, nPosIni, nPosFin) + Chr(13) + Chr(10)

		nPosIni = Atc("creation date:", sXMLResponse)
		nPosFin = Atc(Chr(10), Substr(sXMLResponse, nPosIni))
		sRespuestaWS = sRespuestaWS + Substr(sXMLResponse, nPosIni, nPosFin) + Chr(13) + Chr(10)

		nPosIni = Atc("expiration date:", sXMLResponse)
		nPosFin = Atc(Chr(10), Substr(sXMLResponse, nPosIni))
		sRespuestaWS = sRespuestaWS + Substr(sXMLResponse, nPosIni, nPosFin) + Chr(13) + Chr(10)

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
			Return -1
		EndTry
 
		If This.iStatus != 0
			This.sError = "Error: "  + Transform(oHTTP.Status) +  " Mensaje: " + oHTTP.StatusText
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
			This.sError = "Error: "  + Transform(oHTTP.Status) +  " Mensaje: " + oHTTP.StatusText
			This.iStatus = -1
			Return -1
		EndIf

	EndFunc
	*---------------------------------------------------

	*---------------------------------------------------
	Function CreaRequest11(tcDominio)
	*---------------------------------------------------
		Text To sXMLRequest TEXTMERGE NOSHOW
<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GetWhoIS xmlns="http://www.webservicex.net">
      <HostName><<tcDominio>></HostName>
    </GetWhoIS>
  </soap:Body>
</soap:Envelope>
		EndText

		Return sXMLRequest

	EndFunc
	*---------------------------------------------------

	*---------------------------------------------------
	Function CreaRequest12(tcDominio)
	*---------------------------------------------------
		Text To sXMLRequest TEXTMERGE NOSHOW
<?xml version="1.0" encoding="utf-8"?>
<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"
xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">
  <soap12:Body>
    <GetWhoIS xmlns="http://www.webservicex.net">
      <HostName><<tcDominio>></HostName>
    </GetWhoIS>
  </soap12:Body>
</soap12:Envelope>
		EndText

		Return sXMLRequest

	EndFunc
	*---------------------------------------------------

	*---------------------------------------------------
	Function CreaRequestOld(tcDominio)
	*---------------------------------------------------
		Text To sXMLRequest TEXTMERGE NOSHOW 
		<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
		   xmlns:web="http://www.webserviceX.NET">
		   <soapenv:Header/>
		   <soapenv:Body>
		      <web:GetWhoIS>
		         <web:HostName><<tcDominio>></web:HostName>
		      </web:GetWhoIS>
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
