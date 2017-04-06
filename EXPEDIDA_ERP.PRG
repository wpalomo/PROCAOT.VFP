
_SCREEN.VISIBLE = .F.

_Asql = SqlConnect('PROCAOT','sa','sa')
set step on
*>Hace importación del fichero ARTXXXX.TXT
Use In (Select("ARTICULOS"))

Create Cursor ARTICULOS (Documento C(13),LinErp C(11),Cantidad N(9))

_Ruta = "\\Servidor2\Ficheros\"

lnFileCount = ADir(laFiles, _Ruta + "SERVIDAS*.txt")

If lnFileCount < 1
	=MessageBox('No hay fichero para procesar',64+0,'AVISO')
	Return
EndIf

FichCab = _Ruta + laFiles(1, 1)

Dir = FichCab

_Procaot='000000'


Select ARTICULOS
Append From &Dir Delimited With Character ";"

Select ARTICULOS
Go Top
_R =  RecCount()
_J = 0

Use In (Select("MALOS"))
Create Cursor MALOS (Pedido C(13))

Select ARTICULOS
Go Top
Do While !Eof()
    Wait Window 'Importando '  + AllTrim(Str(_J)) + ' de ' + 	AllTrim(Str(_R)) NoWait
    
    *>Insertar dentro de la tabla F24k001
    _Sentencia="INSERT INTO F24K001 VALUES ('" + ARTICULOS.Documento + "')"
	=SqlExec(_Asql,_Sentencia)
	
    *>Ver si existe el articulo
	_Sentencia="Select * From F24L001 Where " + ;
			   "F24lNumDoc='" + ARTICULOS.Documento + "' And F24lNumDrg='" + ARTICULOS.LinErp + "'"
	=Sqlexec(_Asql,_Sentencia,'F24l')		   

	Select F24l
	Go Top
	If !Eof()
		Select ARTICULOS
	  	If ARTICULOS.Cantidad > 0
			_Sentencia="Update F24L001 Set F24lCanEnv=" + Str(ARTICULOS.Cantidad) + " Where " + ;
					   "F24lNumDoc='" + ARTICULOS.Documento + "' And F24lNumDrg='" + ARTICULOS.LinErp + "'"
		  	=SqlExec(_Asql,_Sentencia)
	  	Else
			Select MALOS
			Append Blank
			Replace Pedido With ARTICULOS.Documento
	  	EndIf
	Else
		Select MALOS
		Append Blank
		Replace Pedido With ARTICULOS.Documento
	EndIf	   
   
    _J = _J + 1
   
    Select ARTICULOS
    Skip
EndDo

Delete File &Dir

Select MALOS
Go Top
Copy To C:\Pedidos.Txt Type SDF

Return