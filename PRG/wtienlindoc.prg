
*> Funciones anexas a importación de pedidos de venta - líneas de detalle.

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

*> Historial de modificaciones:
*> 18.07.2007 (AVC) Modificar filtro artículos, tipo producto y destino carrusel (Modificación provisional).
*>					Se importan las líneas que, aunque no pasen el filtro de tipo producto, se deben preparar en carrusel.

Function WTienLinDoc
Parameters cModo, cEstado, oRegistroFichero, oRegistroOrigen, oClass, cPrm1

Private lStado, cWhere
Local oLocal

lStado = .T.
cEstado = ""

*> Selección del momento en que se realiza la llamada.
Do Case
   *> Al empezar la importación de líneas.
   Case cModo=='I'

   *> Antes de procesar un registro.
   Case cModo=='A'
		With oRegistroFichero
			*> Asignar valores por defecto. Recibe el propietario activo y almacén.
			.F24lCodPro = oClass.__get__propietario__()	&& Propietario.
			.F24lAlmSer = oClass.__get__almacen__()		&& Almacén entrada.
			.F24lSitStk = '1000'						&& SSTK por defecto.
			.F24lFlgEst = '0'							&& Estado.
		EndWith

   *> Procesar una columna con tratamiento especial.
   Case cModo=='L'
		*> Validar columnas en blanco.
		Do Case
			*> Código de artículo.
			Case cPrm1=='F24LCODART'
				If Empty(oRegistroFichero.F24lCodArt)
					cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000011]), oRegistroFichero.F24lNumDoc)
					lStado = .F.
				EndIf

			*> Cantidad pedida.
			Case cPrm1=='F24LCANDOC'
				if empty(oRegistroFichero.F24lCanDoc)
					cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000012]), oRegistroFichero.F24lNumDoc)
					lStado = .F.
				endif

			*> Cantidad ya servida.
			Case cPrm1=='F24LCANRES'
				if oRegistroFichero.F24lCanRes >= oRegistroFichero.F24lCanDoc
					cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000013]), oRegistroFichero.F24lNumDoc, oRegistroFichero.F24lCodArt)
					lStado = .F.
				endif

			*> Fecha de entrega. Convertir de char (DDMMAAAA) a fecha.
			Case cPrm1=='F24LFECPRE'
				oRegistroFichero.F24lFecPre = Ctod(Substr(oRegistroOrigen.FECPRE, 1, 2) + "/" + Substr(oRegistroOrigen.FECPRE, 3, 2) + "/" + Substr(oRegistroOrigen.FECPRE, 5, 4))

			*> Calcular el nº de línea a partir del ID línea.
*!*				Case cPrm1=='F24LLINDOC'
*!*					cWhere = "F24lIdLDoc='" + oRegistroOrigen.IDLDOC + "'"
*!*					If f3_sql("*", "F24l", cWhere, , , "__F24LIDLDOC")
*!*						*> Cambiar el nº de línea recibido, que puede ser variable, por el real del SGA.
*!*						Select __F24lIDLDOC
*!*						Go Top
*!*						Scatter Name oLocal
*!*						oRegistroFichero.F24lLinDoc = oLocal.F24lLinDoc
*!*					EndIf
*!*					Use In (Select("__F24LIDLDOC"))
		EndCase

   *> Antes de grabar un registro.
   Case cModo=='G'
		With oRegistroFichero
			*> Validar el artículo.
			If !ValidarArticulo(.F24lCodPro, .F24lCodArt)
				cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000014]), oRegistroFichero.F24lNumDoc, oRegistroFichero.F24lCodArt)
				Return .F.
			EndIf

			*> Solo acepta aquellas líneas que cumplan el filtro de tipo de producto.
			*> En el caso de reserva carrusel, se acepta la línea si lleva la marca de reserva Carrusel ('R').
			If !FiltroArticulo(.F24lCodPro, .F24lCodArt)
				cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000015]), oRegistroFichero.F24lNumDoc, oRegistroFichero.F24lCodArt)
				Return .F.
*!*				Else
*!*					*> LRC. 20/7/2007. En Fase III habrá que eliminar este else ya que se deberán comunicar todas las referencias sean de SILO o CARRUSEL.
*!*					If Alltrim(F08c.F08cTipPro)=Alltrim(oClass._tipo_carrusel) And oRegistroOrigen.SERTIR<>'R'
*!*						cEstado = oClass.__setmessage_(oClass.__getmessage_([GRP=DOCS,MJC=000015]), oRegistroFichero.F24lNumDoc, oRegistroFichero.F24lCodArt)
*!*						Return .F.
*!*					EndIf
			EndIf

			*> Tomar la descripción corta por defecto, si no viene.
*!*			If Empty(.F24lAbrevi)
*!*				.F24lAbrevi = F08c.F08cAbrevi
*!*			EndIf
		EndWith

   *> Después de procesar un registro.
   Case cModo=='D'
		*> Realizar baja de línea.
		*> Atención !! El flag de operación se recibe en la cabecera, no en las líneas, a diferencia de la importación de entradas.
		select __DOCS
		locate for __codpro==oRegistroFichero.F24lCodPro and __tipdoc==oRegistroFichero.F24lTipDoc and __numdoc==oRegistroFichero.F24lNumDoc
		if found()
			If __oper=='B'
				=BajaLineaSalida(oRegistroFichero, @cEstado)
				*> OJO !! Siempre debe devolver .F., para que el proceso principal borre la línea,
				*> y así, ésta no se incorpore al documento de entrada (N. de P.)
				*> Los mensajes ya vienen asignados.
				Return .F.
			EndIf
		EndIf

		if !ValidarEstadoLineaSalida(@oRegistroFichero, @cEstado)
			*> Los mensajes ya vienen asignados.
			return .F.
		endif

   *> Al finalizar la importación de líneas.
   Case cModo=='F'
EndCase

return lStado
