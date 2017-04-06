_ASql=SqlConnect()

_Sentencia = " Select F10cCodUbi As Ubicacion, " + ;
			 " ' '  As Propietario, " + ;
			 " ' ' As Articulo, " + ;
			 " ' ' As Lote , " + ;
			 " ' ' As Cantidad, " + ;
			 " ' ' As Observaciones, " + ;
			 " ' ' As Marca " + ;
			 "from F10c001 " 


_Ok = SqlExec(_Asql,_Sentencia,'F10c')
=SqlMoreResults(_Asql)

_Sentencia = " Select F16cCodUbi " + ;
			 "from F16c001 " 
_Ok = SqlExec(_Asql,_Sentencia,'F16c')

set step on
Select F10c
Go top
Do While !Eof()
   _CodUbi = Ubicacion

	Select F16c
	Locate For F16cCodUbi = _CodUbi
	If Found()
	   Replace F10c.Marca With 'N'
	EndIf
	Select F10c
	Skip
EndDo


set step on
Select F10c
Copy to c:\f10c Type Sdf

