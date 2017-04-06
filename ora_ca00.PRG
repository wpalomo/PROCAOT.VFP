*> Cabecera

   *> Descripcio ................. Llibreria per tractament fisic del magatzem - Oracle
   *> Modul ...................... Ora_Ca00.PRG
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 23.10.98
   *> Data de fi .................

*> Notes


**************************************************************
*     Funciones standard para ProDis / Procaot en Oracle     *
**************************************************************

*> Funciones ............................ CargaUbi         Cargar ubicación.
*>                                        DesUbi           Descargar ubicación.
*>                                        PesVolAr         Obtener peso y volumen por art./cant.
*>										  PesVolPal        Obtener peso y volumen por palet
*>                                        PesVolUb         Obtener peso y volumen por ubicación.

*>=========================================================
*> CargaUbi ................. Rutina cargar ubicación
*>=========================================================

Procedure CargaUbi
Parameters c_CodUbi, n_PesoKg, n_VoluM3, n_NumPal, l_Bien
     *> Entrar : Codigo ubicacion
     *>          Peso del material a ubicar
     *>          Volumen del material a ubicar
     *>          Cantidad de palets = 0 si no es palet completo
     *>                             = 1 si es palet completo o se completa
     *> retorna : l_Bien = .T. Se ubicó correctamente
     *>                  = .F. No se consiguió ubicar 
     
   Private l_Openf10, f_Fichero, Codi_Ubi, f_Campos, f_Valores
   f_Fichero = Select()
   l_Bien    = .F.

	If _Lenguaje <> 'VB'
	   *> Mensaje de Ubicar.------------------------------------------
	   Wait Window "Cargando palet en la ubicación " + c_CodUbi NoWait
   EndIf   

   *> Intentamos abrir el fichero de ubicaciones.---------------------
   =CrtCursor('F10c', 'CUbiF10c')
   f_Where = "F10cCodUbi='" + c_CodUbi + "'"
   =F3_SQL('*', 'F10c', f_Where, , , 'CUbiF10c')

   *> Reemplazamos los valores.---------------------------------------
   Sele CUbiF10c
   Go Top
   If !Eof()
      *> Controlamos los estados.--------------------------------------
      If ((F10cNumOcu + n_NumPal)>=F10cMaxPal) .And. (F10cTipAlm = "M" .Or. F10cTipAlm = "P")
         Replace F10cEstEnt With "S", F10cEstSal With "N"
      EndIf

      *> Reemplazar peso, teniendo en cuenta peso negativo.-----------
      If F10cVolLib > F10cVolTot
         Replace F10cVolLib With F10cVolTot
      EndIf
      If F10cVolLib - N_VoluM3  <  0
         Replace F10cVolLib With 0
      Else
         Replace F10cVolLib With F10cVolLib - n_VoluM3
      EndIf

      *> Reemplazar volumen, teniendo en cuenta overflow.-------------
      If F10cPesOcu > F10cPesTot
         Replace F10cPesOcu With F10cPesTot
      EndIf
      If F10cPesOcu + N_PesoKg  >  9999999
         Replace F10cPesOcu With 9999999
      Else
         Replace F10cPesOcu With F10cPesOcu + N_PesoKg
      EndIf

      Replace F10cNumOcu With Iif(F10cNumOcu>=F10cMaxPal, F10cMaxPal, F10cNumOcu + n_NumPal),;
              F10cEstGen With IIf(F10cNumOcu>=F10cMaxPal,"O",F10cEstGen)

      f_Where = "F10cCodUbi='" + c_CodUbi + "'"

      l_Bien  = F3_UpdTun('F10c', , , , 'CUbiF10c', f_Where)
   EndIf

   *> Activamos el fichero anterior.----------------------------------
   Use In CUbiF10c
   Select (f_Fichero)
Return l_Bien

*>=====================================================
*> DesUbi ................. Rutina descargar ubicación
*>=====================================================

Procedure DesUbi
Parameters c_CodUbi, n_PesoKg, n_VoluM3, n_NumPal, l_Bien
*> Entrar : Codigo ubicacion
*>          Peso del material a desubicar
*>          Volumen del material a desubicar
*>          Cantidad de palets = 0 si no es palet completo
*>                             = 1 si es palet completo o se completa
*> Retorna : l_Bien = .T. Se desubicó correctamente
*>                  = .F. No se consiguió

Private l_Openf08, l_Openf10, f_Fichero, l_Bien
   f_Fichero = Select()
   l_Bien    = .F.

   *> Mensaje de desubicar.-------------------------------------------
	If _Lenguaje <> 'VB'
	   Wait Window "Rehaciendo la ubicación " + c_CodUbi NoWait
	EndIf
   =CrtCursor('F10c', 'DUbiF10c')
   f_Where = "F10cCodUbi='" + c_CodUbi + "'"
   =f3_sql('*', 'F10c', f_Where, , , 'DUbiF10c')

   *> Reemplazamos los valores.---------------------------------------
   Select DUbiF10c
   Go Top
   If !EOF()
      *> Controlamos los estados.-------------------------------------
      If ((F10cNumOcu - n_NumPal) = 0) .And. (F10cTipAlm = "M" .Or. F10cTipAlm = "P")
         Replace F10cEstSal With "S", F10cEstEnt With "N"
      EndIf

      *> Reemplazamos volumen teniendo en cuenta Overflow.------------
      Do Case
         Case F10cVolLib > F10cVolTot
            Replace F10cVolLib With F10cVolTot
         Case F10cVolLib < 0
            Replace F10cVolLib With 0
      EndCase

      If F10cVolLib + n_VoluM3 > F10cVolTot
         Replace F10cVolLib With F10cVolTot
      Else
         Replace F10cVolLib With F10cVolLib + n_VoluM3
      EndIf

      *> Reemplazamos peso teniendo en cuenta Overflow.---------------
      Do Case
         Case F10cPesOcu > F10cPesTot
            Replace F10cPesOcu With F10cPesTot
         Case F10cPesOcu < 0
            Replace F10cPesOcu With 0
      EndCase

      If F10cPesOcu - n_PesoKg  <  0
         Replace F10cPesOcu With 0
      Else
         Replace F10cPesOcu With F10cPesOcu - n_PesoKg
      EndIf

      *> Reemplazamos valores generales.------------------------------
      Replace F10cNumOcu With F10cNumOcu - n_NumPal

      Replace F10cEstGen With IIf(F10cNumOcu < F10cMaxPal,"L", F10cEstGen),;
              F10cNumOcu With IIf(F10cNumOcu >= 0, F10cNumOcu, 0)

      f_Where = "F10cCodUbi='" + c_CodUbi + "'"

      l_Bien  = F3_UpdTun('F10c', , , , 'DUbiF10c', f_Where, 'N', 'N')
   EndIf

   *> Activamos el fichero anterior.----------------------------------
   Select (f_Fichero)
Return l_Bien

*>===================================================================================
*> PesVolAr ................. Obtener peso y volumen a partir de artículo y cantidad
*>===================================================================================

Procedure PesVolAr
Parameters c_CodPro, c_CodArt, n_Cantid, n_Peso, n_Volu
Private c_TamAbi, n_TotPal, f_Replace
   *> Guardamos los valores del artículo.--------------------------
   n_Peso = 0
   n_Volu = 0
   c_TamAbi = ''
   n_TotPal = 0

   f_Replace = "n_Peso = F08cPesUni * n_Cantid/1000, n_Volu = F08cVolUni * n_Cantid/1000, " + ;
       "c_TamAbi = F08cTamAbi, n_TotPal = F08cUniVen * F08cUniPac * F08cPacCaj * F08cCajPal"
   m.F08cCodPro = c_CodPro
   m.F08cCodArt = c_CodArt
   =f3_seek('F08c',,, f_Replace)
   If n_TotPal = 0
      n_TotPal = 1
   EndIf

   *> Si el volumen es cero lo calculamos.-------------------------
   If n_Volu = 0
      *> Selecionamos y buscamos tamaño.------------------------------
      m.F00mCodTam = c_TamAbi
      f_Replace = "n_Volu = (F00mDimVol / n_TotPal) * n_Cantid / 1000"
      =F3_Seek('F00m',,, f_Replace)
   EndIf
Return

*>===================================================================================
*> PesVolPal ................. Obtener peso y volumen a partir de artículo y cantidad
*>===================================================================================

Procedure PesVolPal
Parameters c_CodPro, c_NumPal, n_Peso, n_Volu
Private c_TamAbi, n_TotPal, f_Replace, _Sentencia
Local lStado

   *> Guardamos los valores del artículo.--------------------------
   n_Peso = 0
   n_Volu = 0
   c_TamAbi = ''
   n_TotPal = 0

   _Sentencia = "F14cNumPal = '"+ c_NumPal+"'"
   lStado = f3_sql('*', 'F14c', _Sentencia, , , 'CurF14c')
   If !lStado
   		_LxErr = " No se ha encontrado información del palet" + cr
   		Do Form St3Inc
   		Return
   EndIf
   
   Select CurF14c
   Go Top
   Do while !Eof()
      n_Cant = F14cCanFis
      f_Replace = "n_Peso = n_Peso+F08cPesUni * n_Cant/1000, n_Volu = n_Volu+F08cVolUni * n_Cant/1000, " + ;
          "c_TamAbi = F08cTamAbi, n_TotPal = F08cUniVen * F08cUniPac * F08cPacCaj * F08cCajPal"
      m.F08cCodPro = c_CodPro
      m.F08cCodArt = F14cCodArt
      =f3_seek('F08c',,, f_Replace)
      If n_TotPal = 0
         n_TotPal = 1
      EndIf

   *> Si el volumen es cero lo calculamos.-------------------------
      If n_Volu = 0
      *> Selecionamos y buscamos tamaño.------------------------------
         m.F00mCodTam = c_TamAbi
         f_Replace = "n_Volu = (F00mDimVol / n_TotPal) * n_Cant / 1000"
         =F3_Seek('F00m',,, f_Replace)
      EndIf
      Select CurF14c
      Skip
   EndDo
Return

*>===================================================================================
*> PesVolUb ................. Obtener peso, volumen y ocupaciones de una ubicación
*>===================================================================================

Procedure PesVolUb
Parameters c_CodUbi, n_Ocup, n_Peso, n_Volu
Private _Selec, _oldAlias

   _oldAlias = Select()

   *> Inicializar los valores a devolver.--------------------------
   n_Peso = 0
   n_Volu = 0
   n_Ocup = 0

   *> Si volumen es cero, tomarlo de tabla de tamaños de palet.
   _Selec = "Select " + _GCN("Count(F16cCodUbi)") + " As n_Ocup, " + ;
            _GCN("Sum(F16cCanFis * F08cPesUni) / 1000.0") + " As n_Peso, " + ;
            _GCN("Sum(F16cCanFis * F08cVolUni) / 1000.0") + " As n_Volu " + ;
            "From F16c" + _em + ", F08c" + _em + Space(1) + ;
            "Where F16cCodUbi='" + c_CodUbi + "' And " + ;
            "F08cCodPro=F16cCodPro And F08cCodArt=F16cCodArt "

   _xier = SqlExec(_ASql, _Selec, 'F16cVolCur')
   If _xier <= 0
      _LxErr = 'Error calculando estado ubicación: ' + c_CodUbi + cr + ;
               'MENSAJE: ' + cr
      Do Form St3Inc
      _LxErr = ''
      Return
   EndIf

   =SqlMoreResults(_ASql)
   Select F16cVolCur
   Go Top
   Scatter MemVar

   If !Empty(_oldAlias)
      Select (_oldAlias)
   EndIf

Return

*>===================================================================================
*> Procedimiento de confirmación general de un movimiento.
*>===================================================================================

PROCEDURE Prc_ConF
PARAMETERS n_posf14,L_Bien
PRIVATE f_fichero, l_openf14, l_bien, l_openf20, l_openf16

   *> Guardamos el fichero anterior.----------------------------------
   f_fichero = Select()

   *> Abrimos el fichero movimientos pendientes.----------------------
   l_openf14 = f_selec("f14c","codigo","0")

   *> Inicializamos la variable de salida.----------------------------
   l_bien = .F.

   IF n_posf14 <= RecCount()

      *> Abrimos el fichero ocupaciones.------------------------------
      l_openf16 = f_selec("f16c","codigo","0")

      *> Posicionamos el puntero en la posición indicada.-------------
      SELE f14c
      GOTO n_posf14
   
      DO CASE
         *> Entrada.--------------------------------------------------
         CASE !EMPTY(f14cubicaz) .AND. EMPTY(f14cubiori)
            Do Prc_ConE with n_PosF14,l_Bien

         *> Salida.---------------------------------------------------
         CASE EMPTY(f14cubicaz) .AND. !EMPTY(f14cubiori)
            Do Prc_ConS with n_PosF14,l_Bien

         *> Salida - Entrada.-----------------------------------------
         CASE !EMPTY(f14cubicaz) .AND. !EMPTY(f14cubiori)
            Do Prc_ConS with n_PosF14,l_Bien
            IF l_bien
               Do Prc_ConE with n_PosF14,l_Bien
            ENDIF

      ENDCASE

      *> Si todo el proceso es correcto.------------------------------
      IF l_bien

         *> Actualizar lista de trabajo. -----------------------------
         IF !Empty(f14cnumdes)
            Num_Des=F14cnumdes
            Do Prc_List with Num_des,l_Bien
         ENDIF

         *> Borramos el registro del f14c si es necesario.------------
         SELECT f14c
         GOTO n_posf14
         DELETE
      ENDIF

      *> Cerrar la ventana de proceso.--------------------------------
      WAIT CLEAR
   ENDIF

   *> Abrimos fichero anterior.---------------------------------------
   SELE (f_fichero)

RETURN 

*-----------------------*
 PROCEDURE Prc_ConS
*-----------------------*
PARAMETERS n_posicion,L_Fin_Exit
   *> Descripcio ................. Confirmacion extraccion mercancia
   *> Modul ...................... PRC_CONS.PRG
   *> Dise¤o ..................... Santiago Rivas i Tardiu
PRIVATE f_anterior, l_openf14, l_openf20
PRIVATE wndname, c_articulo, c_ubicacion, n_cantidad, numocu

*> Confirmacion extraccion de la mercancia

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = SELE()

   *> Inicializamos la variable de salida.----------------------------
   l_fin_exit = .F.

   *> Abrimos el fichero de movimientos.------------------------------
   l_openf14 = f_selec("f14c","codigo","0")

   *> Controlamos que no sea mayor.-----------------------------------
   IF n_posicion <= RECCOUNT()
   
      *> Situamos el puntero.-----------------------------------------
      GOTO n_posicion

      *> Asignamos la variable de salida.-----------------------------
      c_articulo  = f14ccodart
      c_NumLote   = f14cnumlot
      c_ubicacion = f14cubiori
      n_cantidad  = f14ccanfis
      c_num_palet = F14cnumpal
      
      *> Mensaje de proceso.------------------------------------------
	If _Lenguaje <> 'VB'
      Wait window "Confirmación extracción del palet "+f14cnumpal nowait
    EndIf
      *> Abrimos el fichero historico de movimientos.-----------------
      l_openf20 = f_selec("F20c", "codigo", "0")


      *> Liberamos las ocupaciones.-----------------------------------
      do DesOcu with c_Ubicacion,c_Num_Palet,c_Articulo,c_NumLote,n_Cantidad,l_Fin_Exit

      if l_Fin_Exit
         
******   *> Calcular si debemos restar o no N§Ocupaciones.------------
         Select F16c
         If Seek(c_Ubicacion+c_Num_Palet+c_Articulo+c_NumLote)
            NumOcu = 0
         Else
            NumOcu = 1
         EndIf
         Select F20c

         *> Liberamos las ubicaciones.--------------------------------
         Store 0 to n_Peso,N_Volu
         do PesVolAr with c_Articulo, n_Cantidad, n_Peso, n_Volu
         store .f. to L_B
         do DesUbi with c_Ubicacion, n_Peso, n_Volu, NumOcu, l_B
         
         *> Generamos un nuevo registro en historico.-----------------
         SELECT f20c
         APPEND BLANK
         REPLACE F20cTipAmo WITH f14c.f14cTipAmo,;
                 F20cCodAmo WITH f14c.f14cCodAmo,;
                 F20ccodart WITH f14c.f14ccodart,;
                 F20cnpalet WITH f14c.f14cnumpal,;
                 F20ccantid WITH f14c.f14ccanfis,;
                 F20ctipmov WITH f14c.f14ctipmov,;
                 F20csitstk WITH f14c.f14csitstk,;
                 F20cfecmov WITH Date(),;
                 F20chormov WITH time(),;
                 F20ctipdoc WITH f14c.f14ctipdoc,;
                 F20cnumdoc WITH f14c.f14cnumped,;
                 F20ctentra WITH "S",;
                 F20ctipent WITH f14c.f14ctipent,;
                 F20ccodent WITH f14c.f14ccodent,;
                 F20cnulote WITH f14c.f14cnumlot,;
                 F20cubiori WITH '',;
                 F20cubides WITH f14c.f14cubiori,;
                 F20ctipcom WITH f14c.f14ctipamo,;
                 F20ccodcom WITH f14c.f14ccodamo,;
                 F20ccodope WITH f14c.f14ccodope,;
                 F20ctampal WITH f14c.f14csizpal,;
                 F20cruthab WITH f14c.f14cruthab,;
                 F20cespico WITH f14c.f14cespico,;
                 F20cnumped WITH f14c.f14cnumped,;
                 F20cCodAlm With F14c.F14cCodAlm,;
                 F20cNumEnt WITH ''
      ENDIF
      

   ENDIF

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_anterior)

RETURN

*-----------------------*
 PROCEDURE Prc_ConE
*-----------------------*
PARAMETERS n_posicion,l_Fin_Exit
   *> Confirmaci¢n de la entrada de la mercancia
PRIVATE f_anterior, l_openf14, l_openf16, l_openf20
*> Confirmacion de la mercancia

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = SELE()

   *> Inicializamos la variable de salida.----------------------------
   l_fin_exit = .F.

   *> Abrimos el fichero de movimientos.------------------------------
   l_openf14 = f_selec("f14c","codigo","0")

   *> Controlamos que no sea mayor.-----------------------------------
   IF n_posicion <= RECCOUNT()
   
      *> Situamos el puntero.-----------------------------------------
      GOTO n_posicion

      *> Mensaje de movimiento.---------------------------------------
	If _Lenguaje <> 'VB'
    	  WAIT WINDOW "Confirmando el palet "+f14cnumpal NOWAIT
	EndIf
      *> Asignamos la variable de salida.-----------------------------
      l_fin_exit = .T.

      *> Abrimos el fichero historico de movimientos.-----------------
      l_openf20 = f_selec("f20c","codigo","0")

      *> Abrimos el fichero ocupaciones.------------------------------
      l_openf16 = f_selec("f16c","codigo","0")
      set order to Tag Ind01
      seek F14c.F14cubicaz+f14c.f14cnumpal+f14c.f14ccodart+f14c.f14cnumlot
      if !Found()
         Append blank
         REPLACE f16ccodart WITH f14c.f14ccodart,;
                 f16ccodubi WITH f14c.f14cubicaz,;
                 f16csitstk WITH f14c.f14csitstk,;
                 f16cnumlot WITH f14c.f14cnumlot,;
                 f16cfcaduc WITH f14c.f14cfeccad,;
                 f16cfcalda WITH f14c.f14cfeccal,;
                 f16cnumana WITH f14c.f14cnumana,;
                 f16ccanres WITH 0,;
                 f16cfctsrv WITH f14c.f14cunipac,;
                 f16cfctenv WITH f14c.f14cpaccaj,;
                 f16cfctpal WITH f14c.f14ccajpal,;
                 f16ctipent WITH f14c.f14ctipamo,;
                 f16cnument WITH f14c.f14ccodamo,;
                 f16cnotent WITH f14c.f14cnotaes,;
                 f16ctipdoc WITH f14c.f14ctipdoc,;
                 f16cnumped WITH f14c.f14cnumped,;
                 f16cfecfab WITH f14c.f14cfecfab,;
                 f16cfecent WITH f14c.f14cfechas,;
                 f16cvolocu WITH 0,;
                 f16csizpal WITH f14c.f14csizpal,;
                 f16cnumpal WITH f14c.f14cnumpal,;
                 f16cespico WITH f14c.f14cespico,;
                 F16cLinPro WITH f14c.F14cLinPro
      Endif   
      *> Generamos un nuevo registro en ocupaciones.------------------
      REPLACE f16ccanfis WITH f16ccanfis+f14c.f14ccanfis
   
      *> Generamos un nuevo registro en historico.--------------------
      SELE f20c
      APPEND BLANK
      REPLACE f20cTipAmo WITH f14c.f14cTipAmo,;
              f20cCodAmo WITH f14c.f14cCodAmo,;
              f20ccodart WITH f14c.f14ccodart,;
              f20cnpalet WITH f14c.f14cnumpal,;
              f20ccantid WITH f14c.f14ccanfis,;
              f20ctipmov WITH IIF(Left(f14c.f14ctipmov,1)="2","1"+Right(f14c.f14ctipmov,3),f14c.f14ctipmov),;
              f20csitstk WITH f14c.f14csitstk,;
              f20cfecmov WITH Date(),;
              f20chormov WITH time(),;
              f20ctipdoc WITH f14c.f14ctipdoc,;
              f20cnumdoc WITH f14c.f14cnumped,;
              f20ctentra WITH "E",;
              f20ctipent WITH f14c.f14ctipent,;
              f20ccodent WITH f14c.f14ccodent,;
              f20cnulote WITH f14c.f14cnumlot,;
              f20cubiori WITH '',;
              f20cubides WITH f14c.f14cubicaz,;
              f20ctipcom WITH f14c.f14ctipamo,;
              f20ccodcom WITH f14c.f14ccodamo,;
              f20ccodope WITH f14c.f14ccodope,;
              f20ctampal WITH f14c.f14csizpal,;
              f20cruthab WITH f14c.f14cruthab,;
              f20cespico WITH f14c.f14cespico,;
              f20cnumped WITH f14c.f14cnumped,;
              f20cCodAlm With F14c.F14cCodAlm,;
              f20cNumEnt WITH ''
   ENDIF

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_anterior)

RETURN 

PROCEDURE prc_list
PARAMETERS c_num_ord, l_Fin_exit
PRIVATE f_FICHERO, l_openf26
*> Confirmacion de la linea de lista de trabajo

   *> Guardamos el fichero anterior.----------------------------------
   f_fichero = SELEC()
   l_Fin_Exit=.t.

   *> Abrimos los ficheros.-------------------------------------------
*  l_openf27 = f_selec("f27l","codigo","0")
   l_openf24 = f_selec("f24l","codigo","0")
   l_openf16 = f_selec("f16c","ind01","0")
   l_openf10 = f_selec("f10c","codigo","0")
   l_openf26 = f_selec("f26c","codigo","0")

   IF SEEK(c_num_ord)

      SELE f26c
      IF EMPTY(f26cubicaz)

         *> Rebajamos del documento.----------------------------------
         SELE f24l
         IF SEEK(f26c.F26cTipdoc+f26c.f26cnumped+f26c.f26cnumlin)
            REPLACE f24lcanres WITH f24lcanres - f26c.f26ccanfis,;
                    f24lcanser WITH f24lcanser + f26c.f26ccanfis
		If _Lenguaje <> 'VB'
    	      WAIT WINDOW "Cantidad Extraida "+str(f24lcanser) NOWAIT
        EndIf
         ENDIF

      ENDIF

      *> Borramos la linea ld.----------------------------------------
      SELE f26c
      DELETE
   ENDIF

   *> Se define el array de datos.------------------------------------
   DECLARE a_datos(31)

   Set order to Tag Ind01
   *> Si otras lineas dependen de ella todas abajo.-------------------
   SEEK c_num_ord 
*   LOCATE FOR f26cnumdes = c_num_ord

   DO WHILE !EOF().AND.f26cnumdes = c_num_ord

      *> Inicializamos los datos.-------------------------------------
      a_datos(01) = f26c.f26cnumpal
      a_datos(02) = f26c.f26csizpal
      a_datos(03) = f26c.f26ctipmov
      a_datos(04) = f26c.f26ctipent
      a_datos(05) = f26c.f26ccodent
      a_datos(06) = f26c.f26cnumped
      a_datos(07) = f26c.f26ctipdoc
      a_datos(08) = f26c.f26cnotaes
      a_datos(09) = f26c.f26cfechas
      a_datos(10) = f26c.f26cespico
      a_datos(11) = f26c.f26cTipAmo
      a_datos(12) = f26c.f26cCodAmo
      a_datos(13) = f26c.f26ccodart
      a_datos(14) = f26c.f26csitstk
      a_datos(15) = f26c.f26ccanfis
      a_datos(16) = f26c.f26cfecfab
      a_datos(17) = f26c.f26cfeccad
      a_datos(18) = f26c.f26cfeccal
      a_datos(19) = f26c.f26cnumlot
      a_datos(20) = f26c.f26cunipac
      a_datos(21) = f26c.f26cpaccaj
      a_datos(22) = f26c.f26ccajpal
      a_datos(23) = ""
      a_datos(24) = f26c.f26ccodope
      a_datos(25) = f26c.f26cnumana
      a_datos(26) = f26c.f26cfecmov
      a_datos(27) = f26c.f26cubicaz
      a_datos(28) = f26c.f26cubiori
      a_datos(29) = f26c.f26cnumord
      a_datos(30) = F26c.F26cLinpro
      a_datos(31) = F26c.F26cCodAlm
      
      *> Reservar en Ubicacion origen.--------------------------------
      if !Empty(a_Datos(28))

		If _Lenguaje <> 'VB'
         *> Mensaje.--------------------------------------------------
         WAIT WINDOW "Reservando el art¡culo "+ALLTRIM(A_Datos(13)) NOWAIT
		EndIf

         *> Buscamos la ocupacion.------------------------------------
	     sele f16c
         set order to Tag Ind01
         seek A_Datos(28)+A_Datos(01)+A_Datos(13)+A_Datos(11)
         n_can_extr = iif(Eof(),0,(f16ccanfis - f16ccanres))

         *> Encuentra la ocupacion se¤alada para este articulo.-------
         IF n_can_extr >= a_datos(15) .AND. !EOF() .AND. ;
            (f16ccodart = A_Datos(13))
            replace f16ccanres with f16ccanres+A_Datos(15)
         ELSE

         *> Nunca dara el error.-------------------------------------
         IF n_can_extr < a_datos(15) .AND. !EOF() .AND. ;
         (f16ccodart = A_Datos(13))
            n_can_extr = A_datos(15)
         Endif

         IF n_can_extr >= a_datos(15) .AND. !EOF() .AND. ;
         (f16ccodart = A_Datos(13))
            replace f16ccanres with f16ccanres+A_Datos(15)

            *> Misteriosamento no hay cantidad física suficiente.-----
            *> para reservar, Algun gazapo ha ocurrido, no se.--------
            *> puede validar.-----------------------------------------
            
*           wait window "Fatal error en la confirmación "

   
            *> Activamos el fichero anterior.-------------------------
            SELE (f_FICHERO)

            *> Salimos.-----------------------------------------------
            L_Fin_Exit = .f.
            RETURN
            ENDIF
         ENDIF
      ENDIF
      
      *> Cargar en ubicación destino.---------------------------------
      IF !EMPTY(a_datos(27))

         *> Calculamos el peso y el volumen.--------------------------
         Store 0 to n_Peso, n_Volu
         do PesVolAr with A_Datos(13), A_Datos(15), n_Peso,n_Volu

         *> Ocupa la ubicacion.---------------------------------------
         l_bien=.f.
         Do CargaUbi with a_Datos(27), n_Peso, n_Volu, 1, l_Bien

      ENDIF
      *> Generar nuevo movimiento pdte
 
      *> Generamos el movimiento.-------------------------------------
      n_Status=0
      do genmov with a_datos, n_Status
      IF n_status = 2
         =F3_Sn(4,1,"Sit. stock erronea en palet " + a_datos(01))
      ENDIF

      *> Eliminar numero de dependencia.------------------------------
      SELE f26c
      REPLACE f26cnumdes WITH ""

      *> Localizar otra linea.----------------------------------------
*      LOCATE FOR f26cnumdes = c_num_ord
      SEEK c_num_ord 
   ENDDO

   
   *> Activamos el fichero anterior.----------------------------------
   SELE (f_FICHERO)
RETURN 





*-----------------------*
 PROCEDURE GenMov
*-----------------------*
   *> Descripcio ................. Crea el mov. en F14c
PARAMETERS a_f14, n_retorna
   *> Entra un array con todos los datos de f14c
   *> La funcion retorna un codigo de control:
   *>
   *> Retorna 0 : Todo correcto.
   *>         1 : Numero de palet existente.
   *>         2 : Error en la situacion de stock.
   *>
PRIVATE f_fichero, l_f14open, c_numpalet
*> Genera un movimiento pendiente con los datos del array.

   EXTERNAL ARRAY a_f14
   
   *> Guardamos entorno.----------------------------------------------
   f_fichero = SELEC()
   l_f14open = USED("f14c")
   n_retorna = 0
   
   *> Inicializamos el array.-----------------------------------------
   a_f14(01) = IIF(EMPTY(a_f14(01)),SPACE(10) ,a_f14(01)) && N§ de palet.
   a_f14(02) = IIF(EMPTY(a_f14(02)),SPACE( 4) ,a_f14(02)) && Tamanyo palet.
   a_f14(03) = IIF(EMPTY(a_f14(03)),SPACE( 4) ,a_f14(03)) && Tipo mov.
   a_f14(04) = IIF(EMPTY(a_f14(04)),SPACE( 4) ,a_f14(04)) && Tipo ent.
   a_f14(05) = IIF(EMPTY(a_f14(05)),SPACE( 6) ,a_f14(05)) && Codigo ent.
   a_f14(06) = IIF(EMPTY(a_f14(06)),SPACE(10) ,a_f14(06)) && Num doc.
   a_f14(07) = IIF(EMPTY(a_f14(07)),SPACE( 4) ,a_f14(07)) && Tipo doc.
   a_f14(08) = IIF(EMPTY(a_f14(08)),SPACE(10) ,a_f14(08)) && Nota e/s.
   a_f14(09) = IIF(EMPTY(a_f14(09)),{  .  .  },a_f14(09)) && Fecha entrada.
   a_f14(10) = IIF(EMPTY(a_f14(10)),SPACE( 1) ,a_f14(10)) && Es pico?.
   a_f14(11) = IIF(EMPTY(a_f14(11)),SPACE( 4) ,a_f14(11)) && Tipo amo.
   a_f14(12) = IIF(EMPTY(a_f14(12)),SPACE( 6) ,a_f14(12)) && Codigo amo.
   a_f14(13) = IIF(EMPTY(a_f14(13)),SPACE(10) ,a_f14(13)) && Codigo artic.
   a_f14(14) = IIF(EMPTY(a_f14(14)),SPACE( 4) ,a_f14(14)) && Sit. stock.
   a_f14(15) = IIF(EMPTY(a_f14(15)),0         ,a_f14(15)) && Cantidad fis.
   a_f14(16) = IIF(EMPTY(a_f14(16)),{  .  .  },a_f14(16)) && Fech. fabr.
   a_f14(17) = IIF(EMPTY(a_f14(17)),{  .  .  },a_f14(17)) && Fech. cad.
   a_f14(18) = IIF(EMPTY(a_f14(18)),{  .  .  },a_f14(18)) && Fech. c.c.
   a_f14(19) = IIF(EMPTY(a_f14(19)),SPACE(15) ,a_f14(19)) && Num. lote.
   a_f14(20) = IIF(EMPTY(a_f14(20)),0         ,a_f14(20)) && Uni - pack.
   a_f14(21) = IIF(EMPTY(a_f14(21)),0         ,a_f14(21)) && Pack - caj.
   a_f14(22) = IIF(EMPTY(a_f14(22)),0         ,a_f14(22)) && Caj - Pale.
   a_f14(23) = IIF(EMPTY(a_f14(23)),SPACE( 4) ,a_f14(23)) && Rut. Hab.
   a_f14(24) = IIF(EMPTY(a_f14(24)),SPACE( 4) ,a_f14(24)) && Codigo ope.
   a_f14(25) = IIF(EMPTY(a_f14(25)),SPACE(10) ,a_f14(25)) && N§ anal.
   a_f14(26) = IIF(EMPTY(a_f14(26)),Date()    ,a_f14(26)) && Fech. mov.
   a_f14(27) = IIF(EMPTY(a_f14(27)),SPACE(16) ,a_f14(27)) && Ubi. destino.
   a_f14(28) = IIF(EMPTY(a_f14(28)),SPACE(16) ,a_f14(28)) && Ubi. origen.
   a_f14(29) = IIF(EMPTY(a_f14(29)),SPACE( 6) ,a_f14(29)) && N§ LD.
   a_f14(30) = IIF(EMPTY(a_f14(30)),SPACE( 1) ,a_f14(30)) && N§ Line de producci¢n
   a_f14(31) = IIF(EMPTY(a_f14(31)),SPACE( 2) ,a_f14(31)) && Almacen

   *> Abrimos el fichero movimientos pendientes.----------------------
   DO fselec WITH "f14c","Ind01","0"

   *> Buscamos el numero de palet.------------------------------------
   c_numpalet = a_f14(01)
   IF SEEK(c_numpalet)
      n_retorna = 1
   ENDIF

	If _Lenguaje <> 'VB'
	   *> Mensaje de movimiento.------------------------------------------
	   WAIT WINDOW "Generando movimiento para el palet "+c_numpalet NOWAIT
	EndIf

   *> Creamos el registro en el fichero de movimientos.---------------
   SELE f14c
   APPEND from array a_f14

   *> Podemos cerrar el fichero de movimientos.-----------------------
   IF !l_f14open
      Sele f14c
      unlock
   ENDIF
   SELE (f_fichero)
RETURN n_retorna



*-----------------------*
 PROCEDURE Prc_CanF
*-----------------------*
PARAMETERS n_posf14,L_Bien
*> Procedimiento de cancelaci¢n general de un 
*> movimiento pendiente en F14c.

   PRIVATE f_fichero, l_openf14, l_openf20, l_openf16

   *> Guardamos el fichero anterior.----------------------------------
   f_fichero = SELEC()

   *> Abrimos el fichero movimientos pendientes.----------------------
   l_openf14 = f_selec("f14c","codigo","0")

   *> Inicializamos la variable de salida.----------------------------
   l_bien = .F.

   IF n_posf14 <= RECCOUNT()

      *> Abrimos el fichero ocupaciones.------------------------------
      l_openf16 = f_selec("f16c","codigo","0")

      *> Posicionamos el puntero en la posicion indicada.-------------
      SELE f14c
      GOTO n_posf14
   
      DO CASE
         *> Entrada.-----------------------------------------------------
         CASE !EMPTY(f14cubicaz) .AND. EMPTY(f14cubiori)
            Do Prc_CanE with n_PosF14,l_Bien

         *> Salida.------------------------------------------------------
         CASE EMPTY(f14cubicaz) .AND. !EMPTY(f14cubiori)
            Do Prc_CanS with n_PosF14,l_Bien

         *> Salida - Entrada.--------------------------------------------
         CASE !EMPTY(f14cubicaz) .AND. !EMPTY(f14cubiori)

****>NO se puede cancelar la reposicion. 04/07/95.
         = F3_Sn(1,1,"La reposici¢n debe ser confirmada..") && Aviso
           RETURN 

*            Do Prc_CanS with n_PosF14,l_Bien
*            IF l_bien
*               Do Prc_CanE with n_PosF14,l_Bien
*            ENDIF
      ENDCASE

      *> Si todo el proceso es correcto.------------------------------
      *> Si hubiera lista deber¡a cancelarla.-------------------------
      IF l_bien
         SELE f14c
         IF !Empty(f14cnumdes)
            *> Actualizar lista de trabajo, eliminar movimientos.-----
            Num_Des=F14cnumdes
            Do Prc_canl with Num_des,l_Bien
         Endif   

         *> Borramos el registro del f14c si es necesario.------------
         SELE f14c
         GOTO n_posf14
         DELETE

      ENDIF

      *> Cerrar la ventana de proceso.--------------------------------
      WAIT CLEAR

   ENDIF

   *> Abrimos fichero anterior.---------------------------------------
   SELE (f_fichero)

RETURN 

*-----------------------*
 PROCEDURE Prc_CanS
*-----------------------*
PARAMETERS n_posicion,L_Fin_Exit
PRIVATE f_anterior, l_openf14, l_openf20
PRIVATE wndname, c_articulo, c_ubicacion, n_cantidad
*> Cancelaci¢n de una reserva de salida anterior

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = SELE()

   *> Inicializamos la variable de salida.----------------------------
   l_fin_exit = .F.

   *> Abrimos el fichero de movimientos.------------------------------
   l_openf14 = f_selec("f14c","codigo","0")

   *> Controlamos que no sea mayor.-----------------------------------
   IF n_posicion <= RECCOUNT()
   
      *> Situamos el puntero.-----------------------------------------
      GOTO n_posicion

      *> Asignamos la variable de salida.-----------------------------
      c_articulo  = f14ccodart
      c_NumLote   = f14cnumlot
      c_ubicacion = f14cubiori
      n_cantidad  = f14ccanfis
      c_num_palet = F14cnumpal
      
	If _Lenguaje <> 'VB'
      *> Mensaje de proceso.------------------------------------------
      Wait window "Cancelando la extracción del palet "+f14cnumpal nowait
	EndIf

      *> Cancelamos las ocupaciones.-----------------------------------
      DO CanOcu with c_Ubicacion,c_Num_Palet,c_Articulo,c_NumLote,n_Cantidad,l_Fin_Exit

      IF l_Fin_Exit


      ENDIF
   
      *> Cerrar la ventana de proceso.--------------------------------
      WAIT CLEAR

   ENDIF

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_anterior)

RETURN

*-----------------------*
 PROCEDURE Prc_CanE
*-----------------------*
PARAMETERS n_posicion,l_Fin_Exit
PRIVATE f_anterior, l_openf14, l_openf16, l_openf20
*> Confirmaci¢n de la entrada de la mercancia

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = SELE()

   *> Inicializamos la variable de salida.----------------------------
   l_fin_exit = .F.

   *> Abrimos el fichero de movimientos.------------------------------
   l_openf14 = f_selec("f14c","codigo","0")

   *> Controlamos que no sea mayor.-----------------------------------
   IF n_posicion <= RECCOUNT()
   
      *> Situamos el puntero.-----------------------------------------
      GOTO n_posicion

	If _Lenguaje <> 'VB'
      *> Mensaje de movimiento.---------------------------------------
      WAIT WINDOW "Cancelando entrada del palet "+f14cnumpal NOWAIT
	EndIf

      *> Calculamos el peso y volumen.--------------------------------
      Store 0 to n_Peso,N_Volu
      DO PesVolAr with f14ccodart, f14ccanfis, n_Peso, n_Volu

      *> Liberamos las ubicaciones.-----------------------------------
      DO DesUbi with f14c.f14cubicaz, n_Peso, n_Volu, 1, l_fin_exit
         
      *> Si es una entrada desde la calle.----------------------------
      SELE f14c

      *> Cancelamos la ventana.---------------------------------------
      WAIT CLEAR

	   ENDIF

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_anterior)

RETURN 

*-----------------------*
 PROCEDURE CanOcu
*-----------------------*
    *> 
    *> Entrada : C¢digo de ubicaci¢n 
    *>           Palet
    *>           Art¡culo
    *>           Lote
    *> Resultado : L_bien = .t. Se pudo desocupar la ubicacion de  estos datos
    *>                    = .f. No se consiguio
PARAMETERS codigo_ubi, c_numpalet, c_articulo, c_num_Lot,n_cantidad,l_Bien
PRIVATE l_openf16, f_fichero, l_bien
*> Cancelar una ocupacion POR UNA CANTIDAD DETERMINADA 

   f_fichero = SELEC()
   l_bien    = .F.

	If _Lenguaje <> 'VB'
	   *> Mensaje de desocupar.-------------------------------------------
	   WAIT WINDOW "Cancelando la ocupación "+codigo_ubi NOWAIT
	EndIf

   *> Abrimos el fichero ocupaciones.---------------------------------
   l_openf16 = f_selec("f16c","codigo","0")
   SET ORDER TO Tag Ind01
               
   *> Buscamos el registro.-------------------------------------------
   IF SEEK(codigo_ubi+c_numpalet+c_articulo+c_num_Lot)

      *> Si la cantidad reservada es mayor.---------------------------
      IF (f16ccanres >= n_cantidad) 
         REPLACE f16ccanres WITH f16ccanres - n_cantidad
         l_bien = .T.
      ENDIF
   ENDIF
    
   *> Borramos la ocupacion si es necesario.--------------------------
   IF l_bien .AND. EMPTY(f16ccanres) .AND. EMPTY(f16ccanfis)
      DELETE
   ENDIF

   *> Cancelamos la ventana.------------------------------------------
   WAIT CLEAR

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_fichero)

RETURN 

*-----------------------*
 FUNCTION prc_CanL
*-----------------------*
PARAMETERS c_num_ord,l_Fin_exit
     *>---
     *> Procedimeinto de cancelaci¢n
     *>
PRIVATE f_FICHERO, l_openf26

*> Confirmacion de la linea de lista de trabajo

   *> Guardamos el fichero anterior.----------------------------------
   f_fichero = SELEC()
   l_Fin_Exit=.t.
   *> Abrimos el fichero movimientos pendientes.----------------------
   l_openf24 = f_selec("f24l","codigo","0")
   l_openf16 = f_selec("f16c","ind01","0")
   l_openf10 = f_selec("f10c","codigo","0")
   l_openf26 = f_selec("f26c","codigo","0")

   *> Inicializamos la variable de salida.----------------------------
   l_bien = .t.

   IF SEEK(c_num_ord)
      *> Borramos la linea ld.----------------------------------
      *> Abrimos el fichero de pedidos---------------------------------
      if empty(f26c.f26cubicaz)
         SELE f24l
         IF SEEK(f26c.F26cTipdoc+f26c.f26cnumped+f26c.f26cnumlin)
            REPLACE f24lcanres WITH f24lcanres - f26c.f26ccanfis
         ENDIF

         *> Por si cantidad reservada es negativo.-2/05/95---------
         IF F24Lcanres < 0
            REPLACE f24lcanres WITH 0
         ENDIF

      ENDIF
      sele f26c
      DELETE
   ENDIF
   *> Si otras lineas dependen de ella todas abajo.-------------
   locate for f26cnumdes=c_num_ord .and. !Deleted()
   do while !Eof()
      *>Localizada linea de lista de trabajo
      sele f26c
      sTORE F26CNUMORD TO NUM3NIV
      if empty(f26c.f26cubicaz)
         SELE f24l
         IF SEEK(f26c.F26cTipdoc+f26c.f26cnumped+f26c.f26cnumlin)
            REPLACE f24lcanres WITH f24lcanres - f26c.f26ccanfis
         ENDIF

        *> Por si cantidad reservada es negativo.-2/05/95---------
        IF F24Lcanres < 0
           REPLACE f24lcanres WITH 0
        ENDIF

      ENDIF
      sele f26c
      delete
      locate for f26cnumdes=num3NIV .and. !Deleted()
      do while !Eof()
      *>Localizada linea de lista de trabajo DE TERCER NIVEL
         sele f26c
         if empty(f26c.f26cubicaz)
            SELE f24l
            IF SEEK(f26c.F26cTipdoc+f26c.f26cnumped+f26c.f26cnumlin)
               REPLACE f24lcanres WITH f24lcanres - f26c.f26ccanfis
            ENDIF

          *> Por si cantidad reservada es negativo.-2/05/95---------
          IF F24Lcanres < 0
             REPLACE f24lcanres WITH 0
          ENDIF

         ENDIF
         sele f26c
         delete
         locate for f26cnumdes=num3NIV .and. !Deleted()
      enddo   
      locate for f26cnumdes=C_num_ord .and. !Deleted()
   enddo   
   *> Activamos el fichero anterior.----------------------------------
   SELE (f_FICHERO)
RETURN 

*-------PROSAN----------*
*-----------------------*
 PROCEDURE Actualgf
*-----------------------*
PARAMETERS C_Almacen,C_Zona,c_articulo, n_cantidad, c_stock, l_operacion,l_bien
PRIVATE l_bien, l_cerramos
*> Actualiza el stock del articulo

   *> Guardamos datos comunes.----------------------------------------
   f_fichero  = SELE()
   l_cerramos = USED("F13L")
   l_bien     = .T.

   *> Abrimos el fichero de stock.------------------------------------
   Sele F13l

   *> Buscamos el codigo especial.------------------------------------
   IF !SEEK(C_Almacen+C_Zona+c_articulo+c_stock)
      APPEND BLANK
      REPLACE f13lcodalm WITH C_almacen,;
              f13lcodigo WITH c_articulo,;
              f13lsitstk WITH c_stock   ,;
              f13lcantid WITH 0,;
              F13lCodZon With C_Zona
   ENDIF

   *> Reemplazamos el valor por el acumulado.-------------------------
   IF l_operacion
      REPLACE f13lcantid WITH f13lcantid + n_cantidad
   ELSE
      l_bien = (f13lcantid >= n_cantidad)
      REPLACE f13lcantid WITH f13lcantid - n_cantidad
   ENDIF
   
   *> Restauramos el entorno.-----------------------------------------
   SELE (f_fichero)

RETURN 

*-----------------------*
 PROCEDURE GenAlb
*-----------------------*
*> Genera un albarán de entrada con los datos del array.

   *> Descripcio ................. Crea el alb. en F18m, F18n.
   *> Grabar/Actualizar detalle albar n. AVC - 20.06.1998

PARAMETERS a_f18m, a_f18n, n_retorna

   *> a_f18m ---> Datos cabecera albarán de entrada.
   *> a_f18n ---> Datos detalle albarán de entrada.
   *> n_retorna > Estado.

   *> Entra arrays con los datos de f18m, f18n.
   *> La funci¢n retorna un c¢digo de control:
   *>
   *> Retorna 0 : Todo correcto.
   *>         1 : Entrada en estado no válido.
   *>         2 :
   *>

   *>   a_f18m(01)             && Numero Pedido.
   *>   a_f18m(02)             && Nº entrada.
   *>   a_f18m(03)             && Albarán propietario.
   *>   a_f18m(04)             && Operario.
   *>   a_f18m(05)             && Fecha entrada.
   *>   a_f18m(06)             && Hora entrada.
   *>   a_f18m(07)             && Almacén.
   *>   a_f18m(08)             && Propietario.
   *>   a_f18m(09)             && Origen (M/D).
   *>   a_f18m(10)             && Estado.
   *>   a_f18m(11)             && Transportista.
   *>   a_f18m(12)             && Matrícula.
   *>   a_f18m(13)             && Conductor.
   *>   a_f18m(14)             && Muelle.
   *>   a_f18m(15)             && Booking In.
   *>   a_f18m(16)             && Centro asociado.

      *> Inicializamos los datos del detalle.----------------------
   *>   a_f18n(01)             && Numero Pedido.
   *>   a_f18n(02)             && Linea Pedido.
   *>   a_f18n(03)             && Linea Recuento.
   *>   a_f18n(04)             && Nº entrada.
   *>   a_f18n(05)             && Tipo documento.
   *>   a_f18n(06)             && Nº documento.
   *>   a_f18n(07)             && Fecha documento.
   *>   a_f18n(08)             && Almacén.
   *>   a_f18n(09)             && Propietario.
   *>   a_f18n(10)             && Artículo.
   *>   a_f18n(11)             && Lote.
   *>   a_f18n(12)             && Caducidad.
   *>   a_f18n(13)             && Situación stock.
   *>   a_f18n(14)             && Cantidad pedida.
   *>   a_f18n(15)             && Cantidad albarán.
   *>   a_f18n(16)             && Cantidad recuento.
   *>   a_f18n(17)             && Nº de cajas.
   *>   a_f18n(18)             && Unidades sueltas.
   *>   a_f18n(19)             && Estado.
   *>   a_f18n(20)             && Cantidad a paletizar.


   PRIVATE f_fichero, l_f18open, c_numpalet

   EXTERNAL ARRAY a_f18m, a_f18n
   
   *> Guardamos entorno.----------------------------------------------
   f_fichero = SELEC()
   l_f18mopen = USED("f18m")
   l_f18nopen = USED("f18n")
   n_retorna = 0
   
   *> Abrimos los ficheros de albaranes de entrada.-------------------
   Do FSelec With "F18m", "CODIGO", "0"       && Albaranes-Cab.
   Do FSelec With "F18n", "IND01",  "0"       && Albaranes-Det

   *> Mensaje de movimiento.------------------------------------------
   c_NumAlb = a_f18m(02)

	If _Lenguaje <> 'VB' 
	  Wait Window "Generando albarán de entrada Nº " + c_NumAlb NoWait
	EndIf

   *> Grabar/actualizar cabecera de albarán de entrada.---------------
   Select F18m
   Seek c_NumAlb

   Do Case
      *> Albarán ya existe y no se puede modificar.
      Case Found() .And. F18mEstado > a_f18m(10)
         n_retorna = 1
      *> Albarán ya existe cabecera: Actualizar.
      Case Found()
         Gather From a_f18m
      *> Albarán no existe: Crear.
      Case !Found()
         Append From Array a_f18m
   EndCase

   *> Grabar la línea de detalle del albarán de entrada.-----------
   If n_retorna = 0
      Select F18n
      
      *> Núm. Entrada + Núm. Pedido + Linea Pedido + Linea Recuento
      Seek a_f18n(04) + a_f18n(01) + a_f18n(02) + a_f18n(03)
           
      If Found()
         Gather From a_f18n
      Else
         Append From Array a_f18n
      EndIf
   EndIf

   *> Podemos cerrar los ficheros de albaranes de entrada.---------
   If !l_f18mopen
      Select f18m
      UnLock
      Use In f18m
   EndIf

   If !l_f18nopen
      Select f18n
      UnLock
      Use In f18n
   EndIf

   Select (f_fichero)

RETURN n_retorna
