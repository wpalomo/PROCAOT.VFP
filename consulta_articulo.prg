LParameters cCodArt,cAgrupado,cIdSesion
_Screen.Visible = .F.
Set Safety Off
*!*	Public _ASql
*!*	Public _BD
*!*	Public _PathProcaot
*!*	Public _PathModula
*!*	Public _pNumPal

*!*	Public _pMovExp 
*!*	Public _pMovOri 
*!*	Public _pMovDes
*!*	Public _pMovEnt
*!*	Public _UBICACION_TIENDA
*!*	Public _RUTA_ARTICULO

*!*	Public TMovSalida
*!*	Public TMovEntrada
*!*	Public TDOCSalida
*!*	Public TDOCEntrada
*!*	Public SSTKDisponible
*!*	Public cTransporte
*!*	Public cRuta

*!*	Public _SITSTKE
*!*	Public _SITSTKB
*!*	Public _TIPMOVE
*!*	Public _TIPMOVX
*!*	Public _TIPMOVR
*!*	Public _TIPMOVO
*!*	Public _TIPMOVD
*!*	Public _TMOVINVE
*!*	Public _TMOVINVS
*!*	Public _TIPMOVR
*!*	Public _TIPMOVCARROS
*!*	Public _TIPMOVCARROE
*!*	Public _TIPMOVMOSTRADORS
*!*	Public _TIPMOVMOSTRADORE
*!*	Public _TIPMOVDEVOLUCIONS
*!*	Public _TIPMOVDEVOLUCIONE


*!*	Set Safety Off

*!*	set procedure to conexiones additive
*!*	set procedure to conexion_procaot additive



*!*	*Establecer Conexión
*!*	_Conectado = Conexion()

*!*	If _Conectado=.F.
*!*		Return
*!*	EndIf

*!*	if !conexion_procaot() then
*!*		= desconexion()
*!*		return
*!*	endif

_Asql=SqlConnect('PROCAOT','sa','sa')


Set Point To ','
*>Reservar de forma automática los pedidos, teniendo en cuenta la tabla de tipos de documento.

Use In (Select ("cStockOcu"))

_Sentencia="Select * From F16c001 Where F16cCodArt='" + cCodArt + "'"

=SqlExec(_Asql,_Sentencia,'cStockOcu')


Select cStockOcu
Go Top


Use In (Select ("cStockOcu2"))


If Type('cAgrupado')=='L'
	*>Por defecto, que venga no agrupado.
	cAgrupado = 'N'
EndIf

If cAgrupado<>'S'
	*>Por si las moscas le pongo el valor.
	cAgrupado = 'N'
EndIf




If cAgrupado = 'S'
	Select F16cCodArt,';' As C1, Sum(F16cCanFis - F16cCanRes) As F16cCanFis,';' As C2, F16cSitStk, ';' As C3;
	From cStockOcu ;
	Into Cursor cStockOcu2 ;
	Group By F16cCodArt,F16cSitStk,C1,C2,C3;
	Order By F16cSitStk
Else	
	Select F16cCodArt,';' As C1,Sum(F16cCanFis - F16cCanRes) As F16cCanFis,';' As C2,F16cSitStk,';' As C3,F16cCodUbi,';' As C4 ;
	From cStockOcu ;
	Into Cursor cStockOcu2 ;
	Group By F16cCodArt,C1,F16cSitStk,F16cCodUbi,C2,C3,C4 ;
	Order By F16cCodUbi,F16cSitStk
EndIf

Select cStockOcu2
Go Top
If Type('cIdSesion')#'L'
	Copy To "\\SERVIDOR2\FICHEROS\CONSULTA_STOCK\" + AllTrim(cIdSesion) + AllTrim(cCodArt) + '.TXT'   Type SDF
Else
	Copy To "\\SERVIDOR2\FICHEROS\CONSULTA_STOCK\" + AllTrim(cCodArt) + '.TXT'  Type SDF
EndIf	
*!*	If Type('cIdSesion')#'L'
*!*		Copy To _RUTA_ARTICULO + AllTrim(cIdSesion) + AllTrim(cCodArt) + '.TXT'   Type SDF
*!*	Else
*!*		Copy To _RUTA_ARTICULO + AllTrim(cCodArt) + '.TXT'  Type SDF
*!*	EndIf	

Close All
Quit
