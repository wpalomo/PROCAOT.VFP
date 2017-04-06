*> Cap‡alera
   *> Llibreria per tractament fisic del magatzem
   *> Programador ................ Ignasi
   *> Data d'inici ............... 05.03.94
   *> Data de fi ................. 12.01.94


*> Notes


PROCEDURE Prc_ConF
PARAMETERS n_posf14,L_Bien
PRIVATE f_fichero, l_openf14, l_bien, l_openf20, l_openf16
*> Procedimiento de confirmaci¢n general de un movimiento.
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
      Wait window "Confirmaci¢n extracción del palet "+f14cnumpal nowait

      *> Abrimos el fichero historico de movimientos.-----------------
      l_openf20 = f_selec("f20c","codigo","0")


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
         SELE f20c
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
                 F20cFecTrp WITH CToD(''),;
                 F20cHorTrp WITH '',;
                 F20CLinPro With F14c.F14cLinPro,;
                 F20cCodAlm With F14c.F14cCodAlm,;
                 F20cNumTrp WITH '',;
                 F20cAlbPro With F14c.F14cNotaes,;
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
      WAIT WINDOW "Confirmando el palet "+f14cnumpal NOWAIT

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
              f20cFecTrp WITH CToD(''),;
              f20cHorTrp WITH '',;
              f20cLinPro WITH f14c.F14cLinPro,;
              f20cCodAlm With F14c.F14cCodAlm,;
              f20cNumTrp WITH '',;
              f20cAlbPro With F14c.F14cNotaes,;
              f20cNumEnt WITH F14c.F14cNumAna



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
          WAIT WINDOW "Cantidad Extraida "+str(f24lcanser) NOWAIT
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

         *> Mensaje.--------------------------------------------------
         WAIT WINDOW "Reservando el art¡culo "+ALLTRIM(A_Datos(13)) NOWAIT

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

            *> Misteriosamento no hay cantidad f¡sica suficiente.-----
            *> para reservar, Algun gazapo ha ocurrido, no se.--------
            *> puede validar.-----------------------------------------
            
*           wait window "Fatal error en la confirmacion "

   
            *> Activamos el fichero anterior.-------------------------
            SELE (f_FICHERO)

            *> Salimos.-----------------------------------------------
            L_Fin_Exit = .f.
            RETURN
            ENDIF
         ENDIF
      ENDIF
      
      *> Cargar en ubicacion destino.---------------------------------
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
 PROCEDURE ActualSt
*-----------------------*
PARAMETERS c_articulo, n_cantidad, c_stock, l_operacion,l_bien
PRIVATE l_bien, l_cerramos
*> Actualiza el stock del articulo

   *> Guardamos datos comunes.----------------------------------------
   f_fichero  = SELE()
   l_cerramos = USED("F13C")
   l_bien     = .T.

   *> Abrimos el fichero de stock.------------------------------------
   Sele F13c

   *> Buscamos el codigo especial.------------------------------------
   IF !SEEK(_alma+c_articulo+c_stock)
      APPEND BLANK
      REPLACE f13ccodalm WITH _alma,;
              f13ccodigo WITH c_articulo,;
              f13csitstk WITH c_stock   ,;
              f13ccantid WITH 0
   ENDIF

   *> Reemplazamos el valor por el acumulado.-------------------------
   IF l_operacion
      REPLACE f13ccantid WITH f13ccantid + n_cantidad
   ELSE
      l_bien = (f13ccantid >= n_cantidad)
      REPLACE f13ccantid WITH f13ccantid - n_cantidad
   ENDIF
   
   *> Restauramos el entorno.-----------------------------------------
   SELE (f_fichero)

RETURN 

PROCEDURE DesOcu
*> DESHACER UNA OCUPACION POR UNA CANTIDAD DETERMINADA 
*> Entrada : C¢digo de ubicaci¢n 
*>           Palet
*>           Art¡culo
*>           Lote
*> Resultado : L_bien = .t. Se pudo desocupar la ubicacion de  estos datos
*>                    = .f. No se consiguio
PARAMETERS codigo_ubi, c_numpalet, c_articulo, c_num_Lot,n_cantidad,l_Bien
PRIVATE l_openf16, f_fichero, l_bien
*> Se desocupa una ubicacion.

   f_fichero = SELEC()
   l_bien    = .F.

   *> Mensaje de deshubicar.------------------------------------------
   WAIT WINDOW "Rehaciendo la ocupación "+codigo_ubi NOWAIT

   *> Abrimos el fichero ocupaciones.---------------------------------
   l_openf16 = f_selec("f16c","codigo","0")
   SET ORDER TO Tag Ind01
               
   *> Buscamos el registro.-------------------------------------------
   IF SEEK(codigo_ubi+c_numpalet+c_articulo+c_num_Lot)
      *> Si la cantidad reservada es mayor.---------------------------
*     IF (f16ccanres >= n_cantidad) 
*        REPLACE f16ccanres WITH f16ccanres - n_cantidad,;
*                f16ccanfis WITH f16ccanfis - n_cantidad,;
*                f16cespico WITH "S"
*        l_bien = .T.
*     ENDIF
      *> Avanzamos puntero.-------------------------------------------
*  ENDIF

**************************************************> Antes
*     SELE(NORMAL) 

      *> Es posible que no modifique la cantidad.---------------------
      IF (f16c.F16cCanRes < n_cantidad)
         Replace F16c.F16cCanRes With N_Cantidad 
      ENDIF 

      IF (f16c.f16ccanres >= n_cantidad) 
           REPLACE f16c.f16ccanres WITH f16c.f16ccanres - n_cantidad,;
                   f16c.f16ccanfis WITH f16c.f16ccanfis - n_cantidad,;
                   f16c.f16cespico WITH "S"
           l_bien = .T.
      ENDIF
      *> Avanzamos puntero.-------------------------------------------
   ENDIF
**********************************************************************

   *> Borramos la ocupacion si es necesario.--------------------------
   IF l_bien .AND. EMPTY(f16c.f16ccanres) .AND. EMPTY(f16c.f16ccanfis).OR. l_bien .AND. F16C.F16CCANRES=0 .AND. F16C.F16CCANFIS=0
      SELE F16C
      DELETE
   ENDIF

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_fichero)

RETURN 

*-----------------------*
 PROCEDURE CargaUbi
*-----------------------*
PARAMETERS codi_ubi, n_PesoKg, n_VoluM3, n_NPal, l_Bien
     *>
     *>PROCEDIMIENTO PARA CARGAR UNA UBICACION 
     *> Entrar : Codigo ubicacion
     *>          Peso del material a ubicar
     *>          Volumen del material a ubicar
     *>          Cantidad de palets = 0 si no es palet completo
     *>                             = 1 si es palet completo o se completa
     *> retorna : l_Bien = .t. Se ubic¢ correctamente
     *>                  = .f. No se consigui¢ 
     
    PRIVATE l_openf10, f_fichero ,Codi_ubi
    *> Se ubica en una ubicacion.

   f_fichero = SELEC()
   l_bien    = .F.

   *> Mensaje de Ubicar.------------------------------------------
   WAIT WINDOW "Cargando palet en la ubicaci¢n "+codi_ubi NOWAIT
   
   *> Intentamos abrir el fichero de ubicaciones.---------------------
   l_openf10 = f_selec("f10c","codigo","0")

   *> Reemplazamos los valores.---------------------------------------
   Sele f10c
   
   SEEK codi_ubi
   
   if !Eof()
      *> Controlamos los estados.--------------------------------------
      IF ((f10ccanpal + n_NPal)>=f10cMaxPal) .AND. (f10ctipalm = "M" .or. f10ctipalm = "P")
         REPLACE f10cestent WITH "S", f10cestsal WITH "N"
      ENDIF

      *> Reemplazamos valores generales.------------------------------

      *> Reemplazar peso, teniendo en cuenta peso negativo.-----------
      If F10cEspLib > F10cVolume
         Replace F10cEspLib With F10cVolume
      EndIf
      If F10cEspLib - N_VoluM3  <  0
         Replace F10cEspLib With 0
      Else
         Replace F10cEspLib With F10cEspLib - N_VoluM3
      EndIf

      *> Reemplazar volumen, teniendo en cuenta overflow.-------------
      If F10cOcupKg > F10cResiKg
         Replace F10cOcupKg With F10cResiKg
      EndIf
      If F10cOcupKg + N_PesoKg  >  9999999
         Replace F10cOcupKg With 9999999
      Else
         Replace F10cOcupKg With F10cOcupKg + N_PesoKg
      EndIf

      Replace F10cCanPal With F10cCanPal + n_NPal,;
              F10cEstGen With IIF(F10cCanPal>=F10cMaxPal,"O",F10cEstGen)


*      REPLACE f10cesplib WITH f10cesplib - n_voluM3,;
*              f10cocupkg WITH f10cocupkg + n_pesoKg,;
*              f10ccanpal WITH f10ccanpal + n_NPal,;
*              f10cestgen WITH IIF(f10ccanpal>=f10cmaxpal,"O",f10cestgen)

      *> Asignamos la variable de salida.-----------------------------
      l_bien = .T.

   ENDIF


   *> Activamos el fichero anterior.----------------------------------
   SELE (f_fichero)

RETURN l_bien

PROCEDURE DesUbi
PARAMETERS codigo_ubi, n_PesoKg, n_VoluM3, n_NPal, l_Bien
*> Entrar : Codigo ubicacion
*>          Peso del material adesubicar
*>          Volumen del material a desubicar
*>          Cantidad de palets = 0 si no es palet completo
*>                             = 1 si es palet completo o se completa
*> retorna : l_Bien = .t. Se desubic¢ correctamente
*>                  = .f. No se consigui¢ 
PRIVATE l_openf08, l_openf10, f_fichero
*> Se desubica una ubicacion.

   f_fichero = SELEC()
   l_bien    = .F.

   *> Mensaje de deshubicar.------------------------------------------
   WAIT WINDOW "Rehaciendo la ubicación "+codigo_ubi NOWAIT

   *> Intentamos abrir el fichero de ubicaciones.---------------------
   l_openf10 = f_selec("f10c","codigo","0")

   *> Reemplazamos los valores.---------------------------------------
   IF SEEK(codigo_ubi)

      *> Controlamos los stados.--------------------------------------
      IF ((f10ccanpal - n_NPal) = 0) .AND. (f10ctipalm = "M" .or. F10cTipAlm="P")
         REPLACE f10cestsal WITH "S", f10cestent WITH "N"
      ENDIF

      *> Reemplazamos volumen teniendo en cuenta Overflow.------------
      Do Case
         Case F10cEspLib > F10cVolume
            Replace F10cEspLib With F10cVolume
         Case F10cEspLib < 0
            Replace F10cEspLib With 0
      EndCase

      If F10cEspLib + n_VoluM3  >  F10cVolume
         Replace F10cEspLib With F10cVolume
      Else
         Replace F10cEspLib With F10cEspLib + n_VoluM3
      EndIf

      *> Reemplazamos peso teniendo en cuenta Overflow.---------------
      Do Case
         Case F10cOcupKg > F10cResiKg
            Replace F10cOcupKg With F10cResiKg
         Case F10cOcupKg < 0
            Replace F10cOcupKg With 0
      EndCase

      If F10cOcupKg - n_PesoKg  <  0
         Replace F10cOcupKg With 0
      Else
         Replace F10cOcupKg With F10cOcupKg - n_PesoKg
      EndIf

*     REPLACE f10cesplib WITH f10cesplib + n_voluM3,;
*             f10cocupkg WITH f10cocupkg - n_pesoKg,;


      *> Reemplazamos valores generales.------------------------------
      REPLACE f10ccanpal WITH f10ccanpal - n_NPal

      REPLACE f10cestgen WITH IIF(f10ccanpal<f10cmaxpal," ",f10cestgen),;
              f10ccanpal WITH IIF(f10ccanpal>=0,f10ccanpal,0)

      *> Asignamos la variable de salida.-----------------------------
      l_bien = .T.
   ENDIF

   *> Activamos el fichero anterior.----------------------------------
   SELE (f_fichero)

RETURN

*-----------------------*
 PROCEDURE PesVolAr
*-----------------------*
PARAMETERS c_Propietario, c_Articulo, c_Canti, n_Peso,n_Volu
*>     PROCEDIMIENTO DE DADO UN ARTICULO Y UNA CANTIDAD DEL MISMO
*>     Devuelve Su Peso y Volumen Total

      *> Intentamos abrir el fichero de articulos.--------------------
*      l_openf04 = f_selec("f04c","codigo","0")
*      l_openf08 = f_selec("f08c","codigo","0")

      *> Guardamos los valores del articulo.--------------------------
      n_peso = 0
      n_volu = 0

      m.F08cCodPro=c_Propietario
      m.F08cCodArt=c_Articulo
      IF f3_seek('F08C')
         n_peso = f08cpesokg*c_Canti
         n_volu = f08cvolume*c_Canti
      ENDIF
RETURN

       *> Si el volumen es cero lo calculamos.-------------------------
       IF n_volu = 0

          *> Inicializamos variables.-------------------------------------
          c_tamany = F08Ctamabi
          n_univen = f08cuniven
          n_unipac = f08cunipac
          n_paccaj = f08cpaccaj
          n_cajpal = f08ccajpal

          *> Selecionamos y buscamos tama¤o.------------------------------
          SELE F04C
          SEEK c_tamany
          n_artvol = F04CDIMVOL 
          *> Calculamos el volumen.---------------------------------------
          n_tot = (n_univen*n_unipac*n_paccaj*n_cajpal)
          If N_Tot = 0
             N_Tot = 1
          EndIf
          n_volu = (n_artvol/n_tot)*c_Canti          
       ENDIF

return

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

   *> Mensaje de movimiento.------------------------------------------
   WAIT WINDOW "Generando movimiento para el palet "+c_numpalet NOWAIT

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
      
      *> Mensaje de proceso.------------------------------------------
      Wait window "Cancelando la extracción del palet "+f14cnumpal nowait

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

      *> Mensaje de movimiento.---------------------------------------
      WAIT WINDOW "Cancelando entrada del palet "+f14cnumpal NOWAIT

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

   *> Mensaje de desocupar.-------------------------------------------
   WAIT WINDOW "Cancelando la ocupación "+codigo_ubi NOWAIT

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

   *>   a_f18m(21)             && Factores.
   *>   a_f18m(22)             && Factores.
   *>   a_f18m(23)             && Factores.
   *>   a_f18m(24)             && Factores.


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
   Wait Window "Generando albarán de entrada Nº " + c_NumAlb NoWait

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
