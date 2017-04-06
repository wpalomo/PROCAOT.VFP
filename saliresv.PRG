*> Capcelera

   *> Descripcio ................. Procesos del módulo de reserva de material.
   *> Módulo ..................... SaliResv.
   *> Llenguatge ................. FoxPro 2.0
   *> Sistema Operatiu ........... MS-DOS Ver 3.30 o superior
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... Abraham Viñas Cortés
   *> Programador ................ Abraham Viñas Cortés
   *> Data d'inici ............... 08.11.1998
   *> Data de fi ................. 12.11.1998

*> Utilitats

   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*>

*>           (C)Copyright ALISFOUR 2002 BARCELONA

*> Procedimiento para reservar los documentos seleccionados.
*> Valores generales, se asignan en .Inicializar.

Procedure GrbDocS

	Private Estado, bb, nHnd, oPro, oPrm
	Local lStado

	*> Comprobar que hay registros para procesar.----------------------
	If !f3_sn(2, 1, 'Realizar reserva de material')
		Return .F.
	EndIf

	_LxErr=''
	_Mensaje = ''
	
	*> Crear objeto de la clase reserva.
	oPro = CreateObject('OraFncResv')
	oPrm = CreateObject('OraPrmResv')
	oPro.ObjParm = oPrm

	
	*> Llamamos al proceso de reserva.--------------------------- 
	Select CABRES
	Replace All Estado With "NO"
	Locate For Marca==1 And Estado=='NO'

	Do While Found()
		lStado = oPro.ObjParm.Inicializar()
	    oComm = CreateObject('commstd', , _Procaot, _Alma)
		*>Comprobar el estadp de pedido en el ERP.
		oComm.Inicializar
		lStado = oComm._pbp_estadopedido_erp('DOCS',CABRES.F24cTipDoc, CABRES.F24cNumDoc)
		If !Empty(lStado)
			*> Pedido bloqueado en el ERP, no poner en Albarán.
			_Mensaje = _Mensaje  + 'Pedido : ' + CABRES.F24cNumDoc + cr
			Release oComm   
			Select CABRES
			Continue
			Loop
			Release oComm   
		Else
			oComm.Inicializar
			lStado = oComm.ProcesarBloqueoPedido('B', 'DOCS', CABRES.F24cTipDoc, CABRES.F24cNumDoc)
			If !Empty(lStado)
				*> Pedido no disponible.
				_LxErr = oComm.UsrError
			EndIf
		
			If !oPro.Reserva(CABRES.F24cCodPro, CABRES.F24cTipDoc, CABRES.F24cNumDoc)
			   _LxErr = _LxErr + 'Incidencias en reserva documento ' + CABRES.F24cNumDoc + cr + oPro.UsrError + cr
			Else
				Select CABRES
				Replace Estado With "SI"
			EndIf
		EndIf


		*> -------------------------------------------------------------------------------------------
		*> LRC. 6.2.9. Si no quedan MP de tipo 2000 se debe liberar el pedido.
		_Where =               "F14cTipMov='2000'"
		_Where = _Where + " And F14cCodPro='" + CABRES.F24cCodPro + "'"
		_Where = _Where + " And F14cTipDoc='" + CABRES.F24cTipDoc + "'"
		_Where = _Where + " And F14cNumDoc='" + CABRES.F24cNumDoc + "'"

		lStado=f3_sql('*', 'F14c', _Where, , , 'F14c2000')

		If !lStado and _xier > 0
		    oComm = CreateObject('commstd', , _Procaot, _Alma)

			oComm.Inicializar
			lStado = oComm.ProcesarBloqueoPedido('L', 'DOCS', CABRES.F24cCodPro, CABRES.F24cNumDoc)
			If !Empty(lStado)
				*> Pedido no disponible.
				_LxErr = oComm.UsrError
			EndIf
		EndIf
		
		Use In (Select("F14c2000"))
		*> -------------------------------------------------------------------------------------------
		
		Select CABRES
		Continue
	EndDo

	If !Empty(_LxErr)
		Do Form St3Inc With .T.
	EndIf
	
	If !Empty(_Mensaje)
		=MessageBox(_Mensaje + ' están bloqueados por el ERP',32+0,'AVISO')
	EndIf
	   
	=SqlCommit(_ASql)

	*> Impresión de incidencias de reserva.
	If File('F99r.Dbf')
		Use In (Select('F99r'))

		Select 0
		Use F99r
		Locate For F99rCodUsr==_UsrCod
		If Found()
			Go Top
			_Label1 = "IMPRESION DE INCIDENCIAS DE RESERVA"
			Do Form St3Imp2 With _Label1, 'SaliResvL1', 'F99R', , "F99rCodUsr==_UsrCod"

			If f3_sn(2, 1, 'Borrar incidencias de reserva')
				Select F99r
				Delete For F99rCodUsr==_UsrCod
			EndIf
		EndIf

		Use In F99r
	EndIf

Return
