
*> Programa: PROCRVOL.PRG
*> Descripci�n  .............. Procesos varios de rec�lculo de vol�menes.

*> M�dulos ................... PrcRVol       (Rec�lculo de volumen de UNA ubicaci�n)
*>                             PrcRVolRng    (Rec�lculo de volumen de un rango)

*>-----------------------------------------------------------------------------------------
*>   Recibe: cUbicacionDesde ---------> Ubicaci�n inicial.
*>           cUbicacionHasta ---------> Ubicaci�n final.
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta funci�n. 
*>-----------------------------------------------------------------------------------------
Procedure ProcRVolRng
Parameters cUbicacionDesde, cUbicacionHasta

Private cWhere
Local lStado

cWhere = "F10cCodUbi Between '" + cUbicacionDesde + "' And '" + cUbicacionHasta + "'"
lStado = f3_sql("*", "F10c", cWhere, "F10cCodUbi", , "F10cTOT")

Select F10cTOT
Go Top
Do While !Eof()
   lStado = ProcRVol(F10cCodUbi)
   Select F10cTOT
   Skip
EndDo

Use In F10cTOT
Return

*>-----------------------------------------------------------------------------------------
*>   Recibe: cUbicacion ---------> Ubicaci�n a recalcular.
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta funci�n. 
*> Corregir c�lculos con MPs. AVC - 18.08.2004
*>-----------------------------------------------------------------------------------------
Procedure ProcRVol
Parameters cUbicacion

Private _Peso, _Volumen
Private cWhere, cField
Local lStado

    *> Cargo la ubicaci�n.
   	cWhere = "F10cCodUbi='" + cUbicacion + _cm
    lStado = f3_sql('*', 'F10c', cWhere, , , 'CF10c')
    If !lStado
       _LxErr = 'Error cargando ubicaciones a procesar' + cr
       Return .F.
    EndIf

    *> Inicializar los valores a recalcular de la ubicaci�n.
    Select CF10c
	Go Top
    Replace F10cNumOcu With 0, ;
            F10cPesOcu With 0, ;
            F10cVolLib With F10cVolTot

	** Busco todas las ocupaciones que hay en una ubicaci�n
	cField = "F16cCodPro, F16cCodArt, F16cCanFis"
	cWhere = "F16cCodUbi='" + CF10c.F10cCodUbi + _cm

	lStado = f3_sql(cField, 'F16c', cWhere, , , 'OCUPS')

    =WaitWindow ('Calculando ocupaciones ubicaci�n: ' + cUbicacion)
		
	Select OCUPS
	Go Top
	Do While !Eof()
		m.F08cCodPro = OCUPS.F16cCodPro
		m.F08cCodArt = OCUPS.F16cCodArt
		If !f3_seek('F08c')
		   Select F08c
		   Append Blank
		   Replace F08cPesUni With 0, ;
		           F08cVolUni With 0
		EndIf

    	*> Tomar peso y volumen unitario de fichero temporal de art�culos.
        Select F08c
        Go Top

		Select CF10c
        Replace F10cNumOcu With F10cNumOcu + 1, ;
                F10cPesOcu With F10cPesOcu + (OCUPS.F16cCanFis * F08c.F08cPesUni) / 1000, ;
                F10cVolLib With F10cVolLib - (OCUPS.F16cCanFis * F08c.F08cVolUni) / 1000

        *> Leer siguiente ocupaci�n de la ubicaci�n.
        Select OCUPS
        Skip
    EndDo

    *> Cargar los MPs de entrada de esta ubicaci�n.
    cField = "F14cCodPro, F14cCodArt, F14cCanFis"
    cWhere = "F14cUbiOri='" + CF10c.F10cCodUbi + _cm + " And F14cTipMov Like '1%'"
	lStado = f3_sql(cField, 'F14c', cWhere, , , 'EMEPES')

    =WaitWindow ('Calculando entradas Ubicaci�n: ' + cUbicacion)

    Select EMEPES
    Go Top
    Do While !Eof()
		m.F08cCodPro = EMEPES.F14cCodPro
		m.F08cCodArt = EMEPES.F14cCodArt
		If !f3_seek('F08c')
		   Select F08c
		   Append Blank
		   Replace F08cPesUni With 0, ;
		           F08cVolUni With 0
		EndIf

    	*> Tomar peso y volumen unitario de fichero temporal de art�culos.
        Select F08c
        Go Top

		Select CF10c
        Replace F10cNumOcu With F10cNumOcu + 1, ;
                F10cPesOcu With F10cPesOcu + (EMEPES.F14cCanFis * F08c.F08cPesUni) / 1000, ;
                F10cVolLib With F10cVolLib - (EMEPES.F14cCanFis * F08c.F08cVolUni) / 1000

        *> Leer el siguiente MP de entrada de esta ubicaci�n.
        Select EMEPES
        Skip	
	EndDo

    *> Corregir posibles errores de desbordamiento.
    Select CF10c
    Replace F10cPesOcu With Iif(F10cPesOcu > F10cPesTot, F10cPesTot, F10cPesOcu)
    Replace F10cPesOcu With Iif(F10cPesOcu < 0, 0, F10cPesOcu)
    Replace F10cVolLib With Iif(F10cVolLib < 0, 0, F10cVolLib)
    Replace F10cNumOcu With Iif(F10cNumOcu > F10cMaxPal, F10cMaxPal, F10cNumOcu)

    *> Actualiza el estado de la ubicaci�n, solo aquellas que NO est�n bloqueadas.
	If F10cestGen # 'B'
	    Replace F10cEstGen With Iif(F10cNumOcu > 0, 'O', 'L')
	EndIf

    =WaitWindow ('Actualizando peso y volumen Ubicaci�n: ' + cUbicacion)

    Select CF10c
    Scatter MemVar
    lStado =  f3_updtun('F10C', , ;
 	                    'F10cPesOcu,F10cVolLib,F10cNumOcu,F10cEstgen', ;
	                    'F10cPesOcu,F10cVolLib,F10cNumOcu,F10cEstGen', ;
	                    'CF10c', ,'N')
    If !lStado
       _LxErr = 'Error actualizando peso y volumen en ubicaciones' + cr + ;
                'MENSAJE: ' + Message() + cr
       Return .F.
    EndIf

	*> Eliminar cursores de trabajo.
    Use In (Select('CF08c'))
    Use In (Select('CF10c'))
    Use In (Select('OCUPS'))
    Use In (Select('EMEPES'))

Return
