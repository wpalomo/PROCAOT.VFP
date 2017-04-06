
Local oWS, lcRespuesta

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>------------- Datos Test de Torsesa.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Set Step On

oWS = CreateObject("VFP_WebService", "http://192.168.1.18:88/WSFoxPro/WS.asmx?wsdl")
lcRespuesta = oWS.GetTor("Torsesa\wsuser", "T0rs3s4", "00000000003", "8400000000003")
If oWS.iStatus != 0
	MessageBox(oWS.sError,16,"Error al ejecutar WS")
Else
	MessageBox(lcRespuesta,64,"EjecutandoWS desde VFP - R2K9")
EndIf

Release oWS

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Define Class VFP_WebService As Custom

	* --- Definimos las propiedades ---
	sError = ""
	iStatus = 0	
	sURL_WS = ""

	* --- Definimos la función GetTor del WebService ---
	*---------------------------------------------------------
	Function GetTor (tcUsuario, tcPassword, pCodigo, pCBarra)
	*---------------------------------------------------------

		* --- Paso 1. Creo el XML Request ---
		sXMLRequest = This.CreaRequest(pCodigo, pCBarra)		&& SOAP 1.2

		pXMLResponse = Addbs(Sys(2023)) + Sys(2015) + [.xml]

		* --- Paso 2. Ejecuto el WS | Paso 3. Obtengo el Response ---
		This.iStatus =  This.EjecutaWS(This.sURL_WS, sXMLRequest, pXMLResponse, tcUsuario, tcPassword)

		If This.iStatus != 0  && Ocurrió un error el cual está especificado en sError.
			Return ""
		EndIf

		sXMLResponse = FileToStr(pXMLResponse)
		* --- Parseamos el XML Response ---
		* --- Para el ejemplo está así, manejando texto, ustedes deben manejar XML (falta de tiempo, perdón) ---

		sRespuestaWS = sXMLResponse

		This.BorraArchivo(pXMLResponse)

		Return sRespuestaWS
	
	EndFunc

	*-------------------------------------------------------------------------------
	Function EjecutaWS (pURL_WSDL, pFileRequest , pFileResponse, tcUser, tcPassword)
	*-------------------------------------------------------------------------------
		Try
			oHTTP = CreateObject('Msxml2.ServerXMLHTTP.6.0')
			oHTTP.Open("GET", pURL_WSDL, .F., tcUser, tcPassword)
			oHTTP.setRequestHeader("User-Agent", "EjecutandoWS desde VFP - R2K9")
			oHTTP.setRequestHeader("Content-Type", "text/xml;charset=utf-8")
			oHTTP.SEND(pFileRequest)

		Catch To loErr
			This.sError = "Error: " + Transform(loErr.ErrorNo) +  " Mensaje: " + loErr.Message
			This.iStatus = -1	   		
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
	Function CreaRequest(pCodigo, pCBarra)
	*---------------------------------------------------
		Text To sXMLRequest TEXTMERGE NOSHOW
<?xml version="1.0" encoding="utf-8"?>
<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">
  <soap12:Body>
    <Articulos_CCBB xmlns="http://tempuri.org/">
      <Codigo><<pCodigo>></Codigo>
      <CBarra><<pCBarra>></CBarra>
    </Articulos_CCBB>
  </soap12:Body>
</soap12:Envelope>
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
