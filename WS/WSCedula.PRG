
Local oWS, lcRespuesta

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>------------- Datos de una cédula del Registro Civil.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Set Step On

oWS = CreateObject("VFP_WebService", "http://merlyna.com/merlyna/abc/webserviceSRI-RegistroCivil.php?wsdl")
lcRespuesta = oWS.GetCedula("1713027611",0,0,'','','','')
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

	* --- Definimos la función GetCedula del WebService ---
	*-----------------------------------------------------------------------------------------------
	Function GetCedula (tcCedula, tiNumero1, tiNumero2, tcCadena1, tcCadena2, tcCadena3, tcCadena4)
	*-----------------------------------------------------------------------------------------------

		Local sRespuestaWS, pXMLResponse, sXMLRequest

		* --- Paso 1. Creo el XML Request - SOAP 1.2 con parámetros genéricos.
		sXMLRequest = This.CreaRequest(tcCedula, tiNumero1, tiNumero2, tcCadena1, tcCadena2, tcCadena3, tcCadena4)

		pXMLResponse = Addbs(Sys(2023)) + Sys(2015) + [.xml]

		* --- Paso 2. Ejecuto el WS | Paso 3. Obtengo el Response ---
		This.iStatus =  This.EjecutaWS(This.sURL_WS, sXMLRequest, pXMLResponse)

		If This.iStatus != 0  && Ocurrió un error el cual está especificado en sError.
			Return ""
		EndIf

		sXMLResponse = FileToStr(pXMLResponse)
		* --- Parseamos el XML Response ---
		* --- Para el ejemplo está así, manejando texto, ustedes deben manejar XML (falta de tiempo, perdón) ---
		sRespuestaWS = "return1: " + Chr(9)+ StrExtract(sXMLResponse,'&lt;return&gt;','&lt;/return&gt;') + Chr(13) + Chr(10)

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
			StrToFile(Strconv(RespuestaWS, 9), pFileResponse)
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
	Function CreaRequest(tcCedula, tiNumero1, tiNumero2, tcCadena1, tcCadena2, tcCadena3, tcCadena4)
	*---------------------------------------------------
		Text To sXMLRequest TEXTMERGE NOSHOW
<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:web="http://merlyna.com/.ajax/abc/webserviceSRI-RegistroCivil.php">
   <soapenv:Header/>
   <soapenv:Body>

      <web:nombreCedulaRegistroCivil soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<arg0 xsi:type="xsd:string"><<tcCedula>></arg0>
         <arg1 xsi:type="xsd:int">tiNumero1</arg1>
         <arg2 xsi:type="xsd:int">tiNumero2</arg2>
         <arg3 xsi:type="xsd:string">tcCadena1</arg3>
         <arg4 xsi:type="xsd:string">tcCadena2</arg4>
         <arg5 xsi:type="xsd:string">tcCadena3</arg5>
         <arg6 xsi:type="xsd:string">tcCadena4</arg6>
      </web:nombreCedulaRegistroCivil>
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
