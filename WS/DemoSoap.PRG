
* Ejemplo consumo webservice
LOCAL lodemowebservice AS "MSSOAP.SoapClient30"
* LOCAL lodemowebservice AS "MSSOAP.SoapClient30?
* Do not remove or alter following line. It is used to support IntelliSense for your XML Web service.
*__VFPWSDef__: lodemowebservice = http://windows/demowebservice/demowebservice.wsdl , demowebservice , demowebserviceSoapPort
LOCAL loException, lcErrorMsg, loWSHandler

Try
	loWSHandler = NewObject("WSHandler",IIF(VERSION(2)=0,"",HOME()+"FFC\")+"_ws3client.vcx")
	lodemowebservice = loWSHandler.SetupClient("http://windows/demowebservice/demowebservice.wsdl&#8221", "demowebservice", "demowebserviceSoapPort")

	* Call your XML Web service here. ex: leResult = lodemowebservice.SomeMethod()
	lodemowebservice.XMLHolaTxt("ParametroEnviadoEjemplo")
	lodemowebservice.XMLHolaXML("ParametroEnviadoEjemplo")
	lodemowebservice.XMLHolaXML2Tablas("ParametroEnviadoEjemplo")

Catch To loException
	lcErrorMsg="Error: "+TRANSFORM(loException.Errorno)+" – "+loException.Message
	DO CASE
		CASE VARTYPE(lodemowebservice)<>"O"
			* Handle SOAP error connecting to web servicea
		CASE !EMPTY(lodemowebservice.FaultCode)
			* Handle SOAP error calling method
			lcErrorMsg=lcErrorMsg+CHR(13)+lodemowebservice.Detail
		OTHERWISE
	* Handle other error
	ENDCASE

	* Use for debugging purposes
	MESSAGEBOX(lcErrorMsg)
Finally

EndTry

* Demostración de creación de webservice
Define Class DemoWebService As Session OlePublic

	Procedure XMLHolaTxt(lcMensaje As Character) As Character
		* La rutina devuelve un texto plano con el literal Hola Mundo + lo que se le pase como parámetro
		lcResultado = "Hola Mundo: " + lcMensaje
		Return lcResultado
	Endproc

	Procedure XMLHolaXML(lcMensaje As Character) As Character
		* La rutina devuelve un dataset
		Create Cursor tmpRespuesta (campo1 c(10), campo2 N(10))
		Insert Into tmpRespuesta (campo1, campo2) Values (lcMensaje, 8)
		* Generamos el XML convirtiendo un cursor en XML, ver help de vfp.
		Cursortoxml("tmpRespuesta", "lcResultado", 1, 8, 0, "1")
		Return lcResultado
	Endproc

	Procedure XMLHolaXML2Tablas(lcMensaje As Character) As Character
		* La rutina devuelve un dataset
		Create Cursor tmpTabla1 (codigo c(2), campo1 c(10), campo2 N(10))
		Insert Into tmpTabla1 (codigo, campo1, campo2) Values ("1",lcMensaje, 8)
		Insert Into tmpTabla1 (codigo, campo1, campo2) Values ("2",lcMensaje, 6)
		Insert Into tmpTabla1 (codigo, campo1, campo2) Values ("3",lcMensaje, 11)
		Index On codigo Tag codigo
		Set Order To codigo
		Create Cursor tmpTabla2 (codigo c(2), descripcion c(10))
		Insert Into tmpTabla2 (codigo, descripcion) Values ("1","Teléfonos")
		Insert Into tmpTabla2 (codigo, descripcion) Values ("1","Ordenadores")
		Insert Into tmpTabla2 (codigo, descripcion) Values ("2","Relojes")
		Index On codigo Tag codigo
		Set Order To codigo
		* creamos una relacion entre ambas tablas
		Sele tmpTabla1
		Set Relation To codigo Into tmpTabla2
		* Generamos el XML convirtiendo un cursor en XML, ver help de vfp.
		*****************************
		*Generar XML
		*****************************
		Local loXMLAdapter As Xmladapter
		Local lcXMLCustomers As String

		loXMLAdapter = Createobject("XMLAdapter")
		loXMLAdapter.RespectNesting= .T.
		loXMLAdapter.AddTableSchema("tmptabla1")
		loXMLAdapter.AddTableSchema("tmptabla2")
		loXMLAdapter.UTF8Encoded = .T.
		loXMLAdapter.ToXML("lcResultado")

		Select tmpTabla1
		Use
		Select tmpTabla2
		Use
		Return lcResultado
	EndProc
Enddefine
