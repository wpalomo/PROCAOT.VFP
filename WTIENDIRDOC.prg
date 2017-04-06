
*> Funciones anexas a importación de pedidos de venta - direcciones.

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

Function WTienDirDoc
Parameters cModo, cEstado, oRegistroFichero, oRegistroOrigen, oClass, cPrm1

lStado = .T.
cEstado = ""

*> Selección del momento en que se realiza la llamada.
Do Case
   *> Al empezar la importación de direcciones.
   Case cModo=='I'

   *> Antes de procesar un registro.
   Case cModo=='A'
		With oRegistroFichero
			*> Asignar valores por defecto. Recibe el propietario activo y almacén.
			.F24tCodPro = oClass.__get__propietario__()		&& Propietario.
			.F24tCProvi = '00' + Left(oRegistroOrigen.CODPOS, 2)
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

return lStado
