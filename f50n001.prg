
_SCREEN.VISIBLE = .F.

_Asql = SqlConnect('PROCAOT','sa','sa')

*>Hace importación del fichero ARTXXXX.TXT
Use In (Select("ARTICULOS"))

Create Cursor ARTICULOS (Articulo C(13),Cantidad N(9))

_Ruta = "\\Servidor2\Ficheros\"

lnFileCount = ADir(laFiles, _Ruta + "INV*.txt")

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
Create Cursor MALOS (CodArt C(13))

Select ARTICULOS
Go Top
Do While !Eof()
    Wait Window 'Importando '  + AllTrim(Str(_J)) + ' de ' + 	AllTrim(Str(_R)) NoWait
    *>Ver si existe el articulo
	_Sentencia="Select * From F08c001 Where " + ;
			   "F08cCodPro='" + _Procaot + "' And F08cCodArt='" + ARTICULOS.Articulo + "'"
	=Sqlexec(_Asql,_Sentencia,'F08c')		   

	Select F08c
	Go Top
	If !Eof()
		Select ARTICULOS
	  	If ARTICULOS.Cantidad > 0
		  	_Sentencia="INSERT INTO F50N001 VALUES('" + ARTICULOS.Articulo + "'," + Str(ARTICULOS.Cantidad) + ")"
		  	=SqlExec(_Asql,_Sentencia)
	  	Else
			Select MALOS
			Append Blank
			Replace CodArt With ARTICULOS.Articulo
	  	EndIf
	Else
		Select MALOS
		Append Blank
		Replace CodArt With ARTICULOS.Articulo
	EndIf	   
   
    _J = _J + 1
   
    Select ARTICULOS
    Skip
EndDo

Delete File &Dir

Select MALOS
Go Top
Copy To C:\ArtInv.Txt Type SDF

Return