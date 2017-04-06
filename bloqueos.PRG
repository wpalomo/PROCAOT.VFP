_asql=SqlConnect('Procaot','sa','sa')


_Ruta="\\Servidor2\Ficheros\SgaOut\Blq\"

set step on
lnFileCount = ADir(laFiles, _Ruta + "*.txt")
 For lnFile = 1 to lnFileCount    	
	FichCab = _Ruta + laFiles(lnFile, 1)
	_Pedido=JustStem(laFiles(lnFile, 1))
	If lnFileCount > 0
	 	Use In (Select("cExiste"))
	 	*>Procesamos los ficheros........
		_Sentencia="Select * From F14c001 Where F14cNumDoc='" + _Pedido + "'"
		=SqlExec(_Asql,_Sentencia,'cExiste')
		
		Select cExiste
		Go Top
		If Eof()
			Delete File(FichCab)
		EndIf
	EndIf		
 Next     


Return
