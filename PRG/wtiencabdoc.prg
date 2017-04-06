
*> Funciones anexas a importación de pedidos de venta - cabeceras.

*> Recibe:
*>	- cModo, origen de la llamada:
*>                    'I', al empezar el proceso de exportación.
*>                    'A', antes de procesar el registro activo.
*>                    'L', al procesar una columna con tratamiento especial.
*>                    'G', al grabar un registro.
*>                    'D', después de procesar el registro activo.
*>                    'F', al finalizar el proceso de exportación.
*>	- oRegFichero, registro actual fichero destino.
*>	- oRegOrigen, registro actual fichero origen.
*>	- oClass, referencia a la propia clase.
*>	- cPrm1, cPrm2, cPrm3, ..., parámetros adicionales:
*>		cPrm1: Modo 'L' --> Columna a validar.

*> Devuelve:
*>	- lStado (.T. / .F.)
*>	- cEstado, texto de error.

Function WTienCabDoc
Parameters cModo, cEstado, oRegistroFichero, oRegistroOrigen, oClass, cPrm1

lStado = .T.
cEstado = ""

*> Selección del momento en que se realiza la llamada.
Do Case
	*> Al empezar la importación de pedidos de venta - cabeceras.
	Case cModo=='I'

	*> Antes de procesar un registro.
	Case cModo=='A'
		With oRegistroFichero
			*> Asignar valores por defecto.
			.F24cCodPro = oClass.__get__propietario__()	&& Propietario.
			.F24cAlmSer = oClass.__get__almacen__()		&& Almacén entrada.
			.F24cFecCre = date()						&& Fecha creación.
			.F24cRutHab = '0000'						&& Ruta.
			.F24cFlgEst = '0'							&& Estado.
		EndWith

		*> Añadir el pedido a la lista de control de pedidos.
		select __DOCS
		locate for __codpro==oRegistroFichero.F24cCodPro and __tipdoc==oRegistroOrigen.TIPDOC and __numdoc==oRegistroOrigen.NUMDOC
		if !found()
			append blank
			replace __codpro with oRegistroFichero.F24cCodPro
			replace __tipdoc with oRegistroOrigen.TIPDOC
			replace __numdoc with oRegistroOrigen.NUMDOC
			replace __oper with oRegistroOrigen.CODOPE
		endif

	*> Procesar una columna con tratamiento especial.
	Case cModo=='L'
		*> Validar columnas en blanco.
		Do Case
			*> Tipo de documento.
			Case cPrm1=='F24CTIPDOC'
				If Empty(oRegistroFichero.F24cTipDoc)
					cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000001]), oRegistroFichero.F24cNumDoc)
					lStado = .F.
				endif

			*> Nº de documento.
			Case cPrm1=='F24CNUMDOC'
				if empty(oRegistroFichero.F24cNumDoc)
					cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000002]))
					lStado = .F.
				endif

			*> Fecha de documento. Convertir de char (DDMMAAAA) a fecha.
			Case cPrm1=='F24CFECDOC'
				oRegistroFichero.F24cFecDoc = ctod(substr(oRegistroOrigen.FECDOC, 1, 2) + "/" + substr(oRegistroOrigen.FECDOC, 3, 2) + "/" + substr(oRegistroOrigen.FECDOC, 5, 4))

			*> Fecha de entrega. Convertir de char (DDMMAAAA) a fecha.
			Case cPrm1=='F24CFECPRE'
				oRegistroFichero.F24cFecPre = ctod(substr(oRegistroOrigen.FECPRE, 1, 2) + "/" + substr(oRegistroOrigen.FECPRE, 3, 2) + "/" + substr(oRegistroOrigen.FECPRE, 5, 4))
		EndCase

	*> Al grabar un registro.
	Case cModo=='G'
		With oRegistroFichero
			*> Validar el tipo documento.
			if !ValidarTipoDocumento(.F24cTipDoc)
				cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000003]), .F24cTipDoc, .F24cNumDoc)
				return .F.
			endif

			*> Validar el transportista.
			If !Empty(.F24cCodTra)
				If !ValidarTransportista(.F24cCodTra)
					cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000004]), .F24cTipDoc, .F24cNumDoc, .F24cCodTra)
					Return .F.
				EndIf
			EndIf

			*> Guardar la referencia de cliente.
			Select __ODOCS
			Append Blank
			Replace __codpro With .F24cCodPro
			Replace __tipdoc With .F24cTipDoc
			Replace __numdoc With .F24cNumDoc
			*Replace __refcli With oRegistroOrigen.REFCLI
		EndWith

	*> Después de procesar un registro.
	Case cModo=='D'

	*> Al finalizar la importación de pedidos de venta - cabeceras.
	Case cModo=='F'

EndCase

return lStado
