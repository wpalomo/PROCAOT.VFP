
*> Gestión general de errores del BATCH.

*> Recibe:
*>	- Nº error.
*>	- Mensaje de error.
*>	- Sentencia que genera el error.
*>	- Programa donde se produce el error.
*>	- Línea del programa.
*>	- Variable pública __batch (opcional)

Procedure ErrHnd

Parameters nError, cMessage, cParentMessage, cPrograma, nLinea

	Private nHandle, cLog, cErrPath, cErrFile
	Local cOldError

	*> Se anula momentaneamente el control de errores.
	cOldError = On ("Error")
	On Error

	cErrFile = Iif(Type('__batch')=='O', __batch._errfile, "")

	*> Formar la cadena a grabar.
	cLog = 		  "ERRNO: " + Str(nError) + ";"	
	cLog = cLog + "MESSAGE(0): " + cMessage + ";"	
	cLog = cLog + "MESSAGE(1): " + cParentMessage + ";"	
	cLog = cLog + "PROGRAM: " + cPrograma + ";"	
	cLog = cLog + "LINENO: " + Str(nLinea) + ";"

	cErrPath = Sys(5) + CurDir() + Iif(!Empty(cErrFile), cErrFile, "BATCH.ERR")

	*> Si el fichero no existe, se crea.
	If !File(cErrPath)
		nHandle = FCreate(cErrPath)
		=FClose(nHandle)
	EndIf

	*> Abrir LOG para escritura.
	nHandle = FOpen(cErrPath, 11)

	If nHandle > 0
		=FSeek(nHandle, 0, 2)			&& Posicionar al final.

		*> Completar la cadena a grabar.
		cLog = DToC(Date()) + ";" + Time() + ";" + cLog

		=FPutS(nHandle, cLog)
		=FClose(nHandle)
	EndIf

	*> Se restaura el control de errores.
	On Error &cOldError

Return
