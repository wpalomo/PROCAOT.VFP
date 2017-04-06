
*> Funciones anexas a importación de pedidos de venta - observaciones.

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

Function WTienObsDoc
Parameters cModo, cEstado, oRegistroFichero, oRegistroOrigen, oClass, cPrm1

lStado = .T.
cEstado = ""

*> Selección del momento en que se realiza la llamada.
Do Case
   *> Al empezar la importación de observaciones.
   Case cModo=='I'

   *> Antes de procesar un registro.
   Case cModo=='A'
		With oRegistroFichero
			*> Asignar valores por defecto. Recibe el propietario activo y almacén.
			.F24oCodPro = oClass.__get__propietario__()		&& Propietario.
		EndWith

   *> Procesar una columna con tratamiento especial.
   Case cModo=='L'

   *> Antes de grabar un registro.
   Case cModo=='G'

   *> Después de procesar un registro.
   Case cModo=='D'

   *> Al finalizar la importación de direcciones.
   Case cModo=='F'
EndCase

Return lStado

*********************************************

*> Generar registros de observaciones a partir de la referencia de cliente, que viene en la cabecera.

Function GenerarReferenciaCliente

Local lStado
Private oDoc

lStado = .T.

Select __ODOCS
Go Top
Do While !Eof()
	Scatter Name oDoc

	If !Empty(oDoc.__RefCli)
		Select __F24oDOCOBS
		Locate For F24oCodPro==oDoc.__CodPro And ;
				   F24oTipDoc==oDoc.__TipDoc And ;
				   F24oNumDoc==oDoc.__NumDoc And ;
				   F24oLinObs=='0000'
		If !Found()
			Append Blank
			Replace F24oCodPro With oDoc.__CodPro
			Replace F24oTipDoc With oDoc.__TipDoc
			Replace F24oNumDoc With oDoc.__NumDoc
			Replace F24oLinObs With '0000'
			Replace F24oDesObs With oDoc.__RefCli
		EndIf
	EndIf 

	Select __ODOCS
	Skip
EndDo

Return
