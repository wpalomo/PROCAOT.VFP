PARAMETERS pCodPro, pTipDoc, pNumDoc

Set Safety Off
Set Talk Off NoWindow
_SCREEN.Visible = .F.

Public _ASql
Public _BD
Public _PathProcaot
Public _PathModula
Public _pNumPal

Public _pMovExp 
Public _pMovOri 
Public _pMovDes
Public _pMovEnt
Public _UBICACION_TIENDA

Public TMovSalida
Public TMovEntrada
Public TDOCSalida
Public TDOCEntrada
Public SSTKDisponible
Public cTransporte
Public cRuta

Public _SITSTKE
Public _SITSTKB
Public _TIPMOVE
Public _TIPMOVX
Public _TIPMOVR
Public _TIPMOVO
Public _TIPMOVD
Public _TMOVINVE
Public _TMOVINVS
Public _TIPMOVR
Public _TIPMOVCARROS
Public _TIPMOVCARROE
Public _TIPMOVMOSTRADORS
Public _TIPMOVMOSTRADORE
Public _TIPMOVDEVOLUCIONS
Public _TIPMOVDEVOLUCIONE

Set Safety Off

set procedure to conexiones additive
set procedure to conexion_procaot additive

Set Step On 

*Establecer Conexión
_Conectado = Conexion()

If _Conectado=.F.
	Return
EndIf

if !conexion_procaot() then
	= desconexion()
	return
endif


*> ---------------------------------------------------------------------------------------------------------------------
*> Eliminar las ocupaciones con cantidad fisica 0
_Sentencia="Delete F16c001 Where F16cCanFis=0"
=SqlExec(_Asql,_Sentencia)
*> ---------------------------------------------------------------------------------------------------------------------

*>Reservar de forma automática los pedidos, teniendo en cuenta la tabla de tipos de documento.

Use In (Select ("cReservar"))

_Sentencia="Select * From F00k001 Where F00kReserv='S'"
=SqlExec(_Asql,_Sentencia,'cReservar')


*> Crear objeto de la clase reserva.
oPro = CreateObject('OraFncResv')
oPrm = CreateObject('OraPrmResv')
oPro.ObjParm = oPrm


*> Objeto para tratamientos de listas de trabajo.
oLst = CreateObject("OraFncList")


Select cReservar
Go Top
Do While !Eof()

	*>Buscamos documentos que esten pendientes de reservar.
	_Sentencia =              "Select * From F24c001"
	_Sentencia = _Sentencia + " Where F24cTipDoc='" + cReservar.F00kCodDoc + "'"
	_Sentencia = _Sentencia + "   And F24cFlgEst<='2'"


	*> Si se ha pasado un pedido en concreto, reservar solo ese.
	*> Esto permite lanzar la reserva desde la importación de pedidos.
	_Sentencia = _Sentencia + IiF (TYPE('pCodPro')='C', " And F24cCodPro='" + pCodPro + "'", '')
	_Sentencia = _Sentencia + IiF (TYPE('pTipDoc')='C', " And F24cTipDoc='" + pTipDoc + "'", '')
	_Sentencia = _Sentencia + IiF (TYPE('pNumDoc')='C', " And F24cNumDoc='" + pNumDoc + "'", '')

	=SqlExec(_Asql,_Sentencia,'cTipDoc')

	Select cTipDoc
	Go Top
	Do While !Eof()
		*>Si el flag de listas está a 'S' es imprescindible que el campo F24cCodMon, esté informado, de lo contrario, no se reserva.
		If cReservar.F00kListas='S' .And. Empty(F24cCodMon)
			Select cTipDoc
			Skip
			Loop
		EndIf
		

		*>Intentamos reservar documento a documento
		lStado = oPro.ObjParm.Inicializar()

		Select cTipDoc
		If oPro.Reserva(cTipDoc.F24cCodPro, cTipDoc.F24cTipDoc, cTipDoc.F24cNumDoc) 
			If cReservar.F00kListas='S'
				*>Generamos la lista y reposición a tienda, si hace falta..
				Select cTipDoc
				=Generar_Lista(F24cCodPro,F24cTipDoc,F24cNumDoc,F24cCodMon)
			EndIf
		EndIf
		
		Select cTipDoc
		Skip
	EndDo
	
	
	
	
	
	Select cReservar
	Skip
EndDo

Use In (Select ("cReservar"))



   *> Cursor de trabajo para cargar los MPs de cada documento.
   =CrtCursor('F14c', 'CRutF14c')

   *> Para confirmación de carga.
   oLst = CreateObject("OraFncList")

   *> Para generar albaranes.
   oAlb = CreateObject("OraFncAlbS")
   
*>Finalmente expedimos la carga.
_Sentencia="Select * From F00k001 Where F00kExtrac='S'"
=SqlExec(_Asql,_Sentencia,'cExtraer')

Select cExtraer
Go Top
Do While !Eof()
	*>Confirmación de Listas.
	*>Buscamos documentos que esten pendientes de confirmar.
	_Sentencia="Select * From F14c001 Where " + ;
		       "F14cTipDoc='" + cExtraer.F00kCodDoc + "' And " + ;
			   "F14cTipMov='2000' And F14cNumLst<>' '"

	=SqlExec(_Asql,_Sentencia,'cExtDoc')


	Select cExtDoc
	Go Top
	Do While !Eof()
		*>Confirmar Listas.
		*If cExtDoc.F14cTipDoc<>'2150'
		    With oLst
			     .Inicializar
			     
			     .NumMovLS = cExtDoc.F14cNumMov	&& MP a confirmar.
			     .FrzCnf = "N"					&& Forzar confirmación.
			     .UpdLst = "N"					&& Actualizar lista.
			     .TMovExp = _pMovExp			&& Movimiento expedición (2999).
			     .TMovOrg = _pMovOri			&& Movimiento en HM (origen).
			     .TMovDst = _pMovDes			&& Movimiento en HM (destino).

			     lStado = .CnfLstMP()
			EndWith      
		*EndIf
		Select cExtDoc
		Skip
	EndDo
	
	Select cExtraer
	Skip
EndDo

Use In (Select("cExtDoc"))


Select cExtraer
Go Top
Do While !Eof()
	*>Confirmación de carga.
	*>Buscamos documentos que esten pendientes de reservar.
	_Sentencia="Select * From F14c001 Where " + ;
		       "F14cTipDoc='" + cExtraer.F00kCodDoc + "' And " + ;
			   "F14cTipMov='2999' And F14cCodPro+F14cTipDoc+F14cNumDoc Not In (Select F14cCodPro+F14cTipDoc+F14cNumDoc From F14c001 Where F14cTipMov='2000')"



	=SqlExec(_Asql,_Sentencia,'cExtDoc')


	Select cExtDoc
	Go Top
	Do While !Eof()
         Scatter Name oF14c
		 With oLst
		 	.Inicializar

		 	.NumMovMP = oF14c.F14cNumMov			&& Movimiento a confirmar.
		 	.UpdDoc = "S"							&& Actualizar documento.

		 	lStado = .CnfCrgMP()
		 EndWith

		 If !lStado
		 	Return .F.
		 EndIf


*!*	        With oAlb
*!*				. Inicializar
*!*				.CodPro = oF14c.F14cCodPro
*!*				.TipDoc = oF14c.F14cTipDoc
*!*				.NumDoc = oF14c.F14cNumDoc

*!*				lStado = .AlbaranDocumento()
*!*	        EndWith

		Select cExtDoc
		Skip
	EndDo	
	
	Select cExtraer
	Skip
EndDo

Close All
Quit


***************************************************************************************************************************************************
*>Generar lista asignada a operario
*>Tener en cuenta las ubicaciones en la queel operario puede trabajar.
*>Si es necesario también lanzará una reposición a tienda.
***************************************************************************************************************************************************
Procedure Generar_Lista
LParameters _CodPro, _TipDoc, _NumDoc, _CodOpe


Private _Selec, _List, _Where, _From, _Order
Private _Err, _Inx
Private _TipCrt, _TotDoc
Private _CodPro, _TipDoc, _NumDoc
Private _NumMac, _RecNo, _OldOpe

Use In (Select("ASTMPG"))

Create Cursor ASTMPG (CodOpe C4, NumMov C10, NRegMPS N5)


*>Buscamos documentos que esten pendientes de reservar.
_Sentencia="Select * From F14c001 Where " + ;
	       "F14cCodPro='" + _CodPro + "' And " + ;
	       "F14cTipDoc='" + _TipDoc + "' And " + ;
	       "F14cNumDoc='" + _NumDoc + "' And " + ;
	       "F14cTipMov='2000' And F14cNumLst='' "

=SqlExec(_Asql,_Sentencia,'ASTMPS')


Use In (Select("curOrden"))
=SqlExec(_Asql,'Select * From F25c001','curOrden')



_Selec ="SELECT F05cCodOpe, F05mTipMov, F05mPickSn, F05uUbiDsd, F05uUbiHst, F05uTipMov, F05sCodSop, F05sCntSop"

_From  =" FROM F05C" + _em + " a, " + ;
        "      F05M" + _em + " b, " + ;
        "      F05U" + _em + " c, " + ;
        "      F05S" + _em + " d "

_Where =" WHERE a.F05cCodOpe = '" + _CodOpe + "' " + ;
        " AND   a.F05cCodOpe = b.F05mCodOpe" + ;
        " AND   b.F05mCodOpe = c.F05uCodOpe" + ;
        " AND   c.F05uCodope = d.F05sCodOpe" + ;
        " AND   a.F05cActivo = 'S'" + ;
        " AND   b.F05mSel = 'S'" + ;
        " AND   c.F05uSel = 'S'" + ;
        " AND   d.F05sSel = 'S'"

_Order =' ORDER BY F05cCodOpe' 

_Selec = _Selec + _From + _Where+ _Order
_Err = f3_SqlExec(_ASql, _Selec, 'F05cCur')

If _Err <= 0
	Return .F.
EndIf

*> Ver que existan parámetros de operarios.
Select F05cCur
Go Top
If Eof()
	Return .F.
EndIf

*> Crear cursor de trabajo, que contendrá los MPs que puede realizar cada operario.
=CrtCursor('ASTMPG', 'WAstMpg', 'C')



Select ASTMPS
Go Top
Do While !Eof()
	*> Explorar parámetros de operarios para ver quién puede realizar este MP.
	_OldOpe = ''
	_RecNo = RecNo()


	   Select F05cCur
	   Locate For F05mTipMov==ASTMPS.F14cTipMov .And. ;
				SubStr(ASTMPS.F14cUbiOri, 1, 4)==_Alma And ;
				F05sCodSop==ASTMPS.F14cTipMac .And. ;
					(F05uTipMov=='H' .And. ;
						(Between(SubStr(ASTMPS.F14cUbiOri, 5, 2), SubStr(F05uUbiDsd, 5, 2), SubStr(F05uUbiHst, 5, 2)) .And. ;
						 Between(SubStr(ASTMPS.F14cUbiOri, 7, 2), SubStr(F05uUbiDsd, 7, 2), SubStr(F05uUbiHst, 7, 2)) .And. ;
						 Between(SubStr(ASTMPS.F14cUbiOri, 9, 3), SubStr(F05uUbiDsd, 9, 3), SubStr(F05uUbiHst, 9, 3)) .And. ;
						 Between(SubStr(ASTMPS.F14cUbiOri,12, 2), SubStr(F05uUbiDsd,12, 2), SubStr(F05uUbiHst,12, 2)) .And. ;
						 Between(SubStr(ASTMPS.F14cUbiOri,14, 1), SubStr(F05uUbiDsd,14, 1), SubStr(F05uUbiHst,14, 1))) .Or. ;
					(F05uTipMov=='V' .And. ;
						F05uUbiDsd<=ASTMPS.F14cUbiOri .And. F05uUbiHst>=ASTMPS.F14cUbiOri)) .And. ;
				(F05mPickSn=='T' .Or. ;
				(F05mPickSn=='N' .And. ASTMPS.F14cOriRes=='P') .Or. ;
				(F05mPickSn=='S' .And. ASTMPS.F14cOriRes=='C') .Or. ;
				(F05mPickSn=='G' .And. ASTMPS.F14cOriRes=='G') .Or. ;
				(F05mPickSn=='U' .And. ASTMPS.F14cOriRes=='U') .Or. ;
				(F05mPickSn=='K' .And. AtC(ASTMPS.F14cOriRes, 'CGU')>0))

	   Do While Found()
		  If F05cCodOpe==_OldOpe
		     Continue
		     Loop
		  EndIf

		  *> Grabar cursor que relaciona MPs seleccionados con operarios que los pueden hacer.
		  Select WAstMpg
		  Append Blank
		  Replace CodOpe With F05cCur.F05cCodOpe, ;
		          NumMov With ASTMPS.F14cNumMov, ;
		          NRegMPS With _RecNo


		  Store CodOpe To _OldOpe


          Select ASTMPS
		  Replace F14cCodOpe With _CodOpe For RecNo() = _RecNo
		
		  *> Ver si este movimiento lo puede hacer otro operario.
		  Select F05cCur
		  Continue
	   EndDo


    Select ASTMPS	
	Go _Recno
	
	Select ASTMPS
	Skip
EndDo


Use In (Select("F14cUpd"))
Use In (Select("F26cUpd"))
Use In (Select("F26lUpd"))

*> Cursores de trabajo.
 =CrtCursor("F14c", "F14cUpd", "C")
 =CrtCursor("F26c", "F26cUpd", "C")
 =CrtCursor("F26l", "F26lUpd", "C")

_NewLst =''
Select ASTMPS
Go Top
Do While !Eof()

	Scatter Name oMP
	With oLst
		.NumMovMP = oMP.F14cNumMov
		.NumLst = Iif(Empty(_NewLst), "", _NewLst)
		.CodOpe = _CodOpe
		.AgrDoc = Space(1)
		.UpdLst = "S"

		lStado = .GenLstMP()
		If !lStado
			Return .F.
		EndIf
	EndWith
	_NewLst = oLst.NumLst

*!*		Select ASTMPS
*!*		If Empty(F14cCodOpe)
*!*			*>Generar reposición a tienda.
*!*			Select ASTMPS
*!*			=Generar_Reposicion(F14cNumMov)
*!*		EndIf

	Select ASTMPS
	Skip
EndDo

If !Empty(_NewLst)
	*>Llamada a la función de orden ubicación.
	If curOrden.F25cOrdRec='S'
		Do OrdenarUbicacion With _NewLst
	EndIf
EndIf
	
	*>Con los movimientos que no se han podido hacer, generar una repo a tienda.		
	
	
Return .T.	




***************************************************************************************************************************************************
*>Generar lista asignada a operario
*>Tener en cuenta las ubicaciones en la queel operario puede trabajar.
*>Si es necesario también lanzará una reposición a tienda.
***************************************************************************************************************************************************
Procedure Generar_Reposicion
LParameters _NumMov


_Rep   = CreateObject('OrafncRepos')
AczPrm = CreateObject('OraPrmActz')
AczFnc = CreateObject('OraFncActz')
AczFnc.ObjParm = AczPrm





m.F14cNumMov=_NumMov
If !F3_Seek('F14c')
	Return .F.
EndIf



Select F14c
Go Top

*> Leer la ocupación origen sobre la que se realizará la reposición.
cWhere = "F16cIdOcup='" + F14c.F14cIdOcup + "'"
lStado = f3_sql("*", "F16c", cWhere, , , "F16cIdOrg")
If !lStado
	This.UsrError = "No existe ocupación origen"
	Use In (Select("F16cIdOrg"))
	Return .F.
EndIf

Select F16cIdOrg
Go Top
If Eof()
	Return .F.
EndIf

Scatter Name oF16c

*> Leer la ocupación origen sobre la que se realizará la reposición.
_Sentencia = "Select Sum(F14cCanFis) As F14cCanFis From F14c001 Where F14cIdOcup='" + F14c.F14cIdOcup + "' And F14cTipDoc='" + F14c.F14cTipDoc + "'"
_ok=SqlExec(_Asql,_Sentencia, "F14cIdOrg")

If _Ok < 0
	Return .F.
EndIf

Select F14cIdOrg
Go Top
If F14cCanFis < 0 .Or. IsNull(F14cCanFis)
	Return .F.
EndIf

*>Mirar si existe reposición de esa IDOCUP.
_Sentencia = "Select Sum(F14cCanFis) As F14cCanFis From F14c001 Where F14cIdOcup='" + F14c.F14cIdOcup + "' And F14cTipMov='3560'"

_ok=SqlExec(_Asql,_Sentencia, "F14cExiste")

If _Ok < 0
	Return .F.
EndIf



Select F14cExiste
Go Top
If F14cCanFis < 0 .Or. IsNull(F14cCanFis) .Or. Empty(F14cCanFis)
	*> Asignar propiedades de la función de actualización.
	

	*> Movimiento de salida origen.
	With AczFnc.ObjParm
		.Inicializar
		.CargarParametros("O", oF16c)

		.PuFlag = "N"
		.PoFlag = "N"
		.PsFlag = "N"
		.PmFgHM = "N"
		.PmFgMP = "S"

		.PuBOld = oF16c.F16cCodUbi
		.PoCFis = F14cIdOrg.F14cCanFis
		.PoCRes = 0
		.PoRowId = oF16c.F16cIdOcup

		.PmUDes = _UBICACION_TIENDA
		.PmTMov = '3560'
		.PmFMov = Date()
		.PmTMac = Space(1)
		.PmORes = "N"
		.PmEnSa = "S"
		.POTDoc = Space(4)
		.PONDoc = Space(13)
	EndWith

	*> Generar el MP.
	AczFnc.ActMP
	If AczFnc.ObjParm.PwCrtn >= '50'
		Return .F.
	EndIf

	*> Establecer el estado de bloqueo.
	=_Rep.SetBloqMP(oF16c.F16cIdOcup, "B")		&& MPs y listas.
	*=_Rep.SetBloqOC(oF16c.F16cIdOcup)			&& Ocupación.
Else
	_Sentencia="Update F14c001 Set F14cCanFis =" + Str(F14cIdOrg.F14cCanFis) + " Where F14cNumMov='" + F14c.F14cNumMov + "'"
	=SqlExec(_Asql,_Sentencia)
EndIf



Return