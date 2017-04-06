
*!* EJEMPLO

Local laTexto[1], lcError

If fTranslateText("SpanishTOEnglish", "Donde esta micarro", @laTexto, @lcError) > 0
	? "Texto: " + laTexto[1]
Else
	=MessageBox(lcError, 0+16+256+4096, "Error")
EndIf

**************************************************************************
*-- Funcion: fTranslateText
*--    almacena en un array por referncia el texto traducido
*--
*-- Parametros tcModo: String, Modo idioma
*--    tcTexto: String, Texto a traducir
*--    taResultado: Array por referencia, valores
*--    tcError: String por referencia, almacena error
*-- Retorna  0:Error, >1:webService consumido correctamente
*--
*-- tcModo:
*--		SpanishTOEnglish, EnglishTOSpanish, SpanishTOFrench, etc
*-- Fecha:	2015/07/10
*--			Abbe Cortes
**************************************************************************
Function fTranslateText

	Lparameters tcModo, tcTexto, taResultado, tcError
 
	Local loSoapClient, loSoapClientUrl, lnResult
	Dimension taResultado[5]

	lnResult = 0

	*!* Creando web Service
	Try
		loSoapClient   = CREATEOBJECT("MSSOAP.SoapClient30")
		loSoapClientUrl  = [http://www.webservicex.net/TranslateService.asmx?WSDL]

		If Vartype(loSoapClient)=='O'
			loSoapClient.MSSoapInit(loSoapClientUrl)
			lcXml = loSoapClient.Translate(tcModo, tcTexto)
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
 