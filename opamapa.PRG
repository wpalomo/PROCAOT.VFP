*> Cabecera

   *> Descripcio ................. Procesos varios de mapa de almacén.
   *> Modul ...................... OpaMapa.PRG
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 18.02.98
   *> Data de fi .................

*> Utilitats

   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*> Módulos........................ GenUbi      (Generación masiva de Ubicaciones)
*>                                 GeneraUbic  (Generación de Ubicaciones)
*>                                 UpdateMain  (Dar de alta las Ubicaciones)
*>                                 Changeubic  (Cambiando los datos de las Ubicaciones)
*>                                 BorraUbic   (Borrando Ubicaciones)
                                   

*>           (C)Copyright ALISFOUR 2002 BARCELONA


*> Creación masiva de Ubicaciones.----------------------------------------------------
PROCEDURE GenUbi
PRIVATE f_temp
PRIVATE c_ubi_ini, c_ubi_fin
*
PRIVATE c_alm_ini, c_alm_fin
PRIVATE c_zon_ini, c_zon_fin
PRIVATE c_cal_ini, c_cal_fin
PRIVATE c_fil_ini, c_fil_fin
PRIVATE c_pis_ini, c_pis_fin
PRIVATE c_pro_ini, c_pro_fin
*
PRIVATE n_alm_ini, n_alm_fin
PRIVATE n_zon_ini, n_zon_fin
PRIVATE n_cal_ini, n_cal_fin
PRIVATE n_fil_ini, n_fil_fin
PRIVATE n_pis_ini, n_pis_fin
PRIVATE n_pro_ini, n_pro_fin
*
PRIVATE l_copia_z, l_copia_f
*> Rutina llamada desde el WR0000000

   EXTERNAL ARRAY mvar()

   *> Ubicaciones iniciales.------------------------------------------
   c_ubi_ini = _alma+" "+C_Zon+" "+C_Cal+" "+C_Fil+" "+C_Pis+" "+C_Pro
   c_ubi_fin = _alma+" "+C_Zon+" "+D_Cal+" "+D_Fil+" "+D_Pis+" "+D_Pro

   *> Almacén.--------------------------------------------------------
   c_alm_ini = _Alma
   c_alm_fin = _Alma
   n_alm_ini = VAL(_Alma)
   n_alm_fin = VAL(_Alma)

   *> Zona.-----------------------------------------------------------
   c_zon_ini = C_Zon
   c_zon_fin = C_Zon
   n_zon_ini = VAL(c_zon)
   n_zon_fin = VAL(c_zon)

   *> Calle.----------------------------------------------------------
   c_cal_ini = C_Cal
   c_cal_fin = D_Cal
   n_cal_ini = Val(C_Cal)
   n_cal_fin = Val(D_cal)

   *> Fila.-----------------------------------------------------------
   c_fil_ini = C_Fil
   c_fil_fin = D_Fil
   n_fil_ini = VAL(C_fil)
   n_fil_fin = VAL(D_fil)

   *> Piso.-----------------------------------------------------------
   c_pis_ini = C_Pis
   c_pis_fin = D_Pis
   n_pis_ini = VAL(C_pis)
   n_pis_fin = VAL(D_pis)

   *> Profundidad.----------------------------------------------------
   c_pro_ini = C_Pro
   c_pro_fin = D_Pro
   n_pro_ini = VAL(C_pro)
   n_pro_fin = VAL(D_pro)

   *> Asignamos la variable de copia de zona.-------------------------
   l_copia_z = .F.

   IF BETWEEN(LEFT(c_zon_ini, 1), "A", "Z") ;
      OR BETWEEN(RIGHT(c_zon_ini, 1), "A", "Z")
      
      *> Asignamos la variable de copia de zona.----------------------
      l_copia_z = .T.
      n_zon_ini = 1
      n_zon_fin = 1
      
   ENDIF

   *> Asignamos la variable de copia de calle.------------------------
   l_copia_c = .F.

   *> Chequeo a la fila.----------------------------------------------
   IF BETWEEN(LEFT(c_CAl_ini,1),"A","Z")
      
      *> Asignamos la variable de copia de fila.----------------------
      l_copia_c = .T.

      n_cal_ini = Asc(C_Cal)
      n_cal_fin = Asc(D_cal)
*     n_cal_ini = VAL(RIGHT(c_cal_ini,1))
*     n_cal_fin = VAL(RIGHT(c_cal_fin,1))
      
   ENDIF

   *> Asignamos la variable de copia de fila.-------------------------
   l_copia_f = .F.

   *> Chequeo a la fila.----------------------------------------------
   IF BETWEEN(LEFT(c_fil_ini,1),"A","Z")
      
      *> Asignamos la variable de copia de fila.----------------------
      l_copia_f = .T.
      n_fil_ini = VAL(RIGHT(c_fil_ini,1))
      n_fil_fin = VAL(RIGHT(c_fil_fin,1))

      n_cal_ini = VAL(RIGHT(c_cal_ini,1))
      n_cal_fin = VAL(RIGHT(c_cal_fin,1))
      
   ENDIF

   *> Se define el mensaje de entrada.--------------------------------
   DECLARE a_texto(7)
   a_texto(01) = "El proceso que está a punto de comenzar"
   a_texto(02) = "crea las ubicaciones entre "+C_Ubi_ini
   a_texto(03) = "y "+C_Ubi_Fin+". En el caso de que ya se"
   a_texto(04) = "tengan ubicaciones generadas entre ambos"
   a_texto(05) = "códigos, estas se modificaran con los valores"
   a_texto(06) = "actuales."
   a_texto(07) = "Desea proceder a la generación?"

   *> Controlamos si se quiere ejecutar.------------------------------
   SwResp=F3_sn(2,3,"Este proceso crea las ubicaciones entre "," "+C_Ubi_Ini+"   Y   "+C_Ubi_Fin,"Desea proceder a la generación?","","","")
   If SwResp

      *> Nombre al fichero temporal.----------------------------------
      f_temp = FNTEMP(3)

      *> Copiamos la estructura y liberamos.--------------------------
      Select F10c
      Copy Structure To (f_temp)
   
      *> Abrimos el fichero temporal.---------------------------------
      _aaa = Sys(2001, 'SAFETY')
      SELECT 0
      USE (f_temp) ALIAS datos EXCLUSIVE
      Set Safety Off
      ZAP
      Set Safety &_aaa

      *> Generar las ubicaciones entre los limites.-------------------
      DO generaubic

      *> Actualizamos el fichero de ubicaciones.----------------------
      DO updatemain

      *> Borrar los archivos temporales.------------------------------
      SELECT datos
      USE
      DELETE FILE (f_temp)

      *> Abrimos los ficheros del proceso.----------------------------
 *    DO FSelect WITH "f10c", "CODIGO", "0"
      Select F10c
   ENDIF

RETURN

PROCEDURE generaubic
PRIVATE n_total, n_genera, n_media
PRIVATE w_ubica, c_codubi
PRIVATE n_alm, n_zon, n_cal, n_fil, n_pis, n_pro, c_zon, c_fil
*> Genera la ubicaciones propiamente.

   *> Chequeo al incremento de filas.---------------------------------
   m.IniFil = IIF(EMPTY(m.IniFil), 1, m.IniFil)

   *> Calculo de cantidad de registros.-------------------------------
   n_total =           ((n_alm_fin - n_alm_ini) + 1)
   n_total = n_total * ((n_zon_fin - n_zon_ini) + 1)
   n_total = n_total * ((n_cal_fin - n_cal_ini) + 1)
   n_total = n_total * CEILING(((n_fil_fin - n_fil_ini) + 1)/m.IniFil)
   n_total = n_total * ((n_pis_fin - n_pis_ini) + 1)
   n_total = n_total * ((n_pro_fin - n_pro_ini) + 1)

   *> Mensaje de proceso.---------------------------------------------
   DECLARE a_ubi[1]
   TEXTO = 'Generando '+ALLTRIM(TRANSFORM(n_total,"99,999,999")) + ' Ubicaciones'

   do form ST3TERM
   =f3_term('TEXTO')
   i=inkey(.2)

   *> Inicializamos el contador de registros.-------------------------
   n_genera = 0
   n_media  = INT(n_total / 48)
   n_media  = IIF(EMPTY(n_media), 1, n_media)

   *> Generar la ubicaciones.-----------------------------------------
   SELECT datos
   FOR n_alm = n_alm_ini TO n_alm_fin
      FOR n_zon = n_zon_ini TO n_zon_fin
         FOR n_cal = n_Cal_Ini To n_Cal_Fin
            FOR n_fil = n_fil_ini TO n_fil_fin STEP m.IniFil
               FOR n_pis = n_pis_ini TO n_pis_fin
                  FOR n_pro = n_pro_ini TO n_pro_fin

                     *> Muestra el porcentaje.------------------------
                     n_genera = n_genera + 1
                     =f3_term('LINEA',n_genera,n_total)

                     *> Inicializa la zona.---------------------------
                     c_zon = IIF(l_copia_z,c_zon_ini,RIGHT(STR(100+n_zon,3),2))
                     
                     *> Inicializa la fila.---------------------------
                     c_fil = RIGHT(STR(1000+n_fil,4),3)
                     c_fil = IIF(l_copia_f,LEFT(c_fil_ini,1)+RIGHT(c_fil,1),c_fil)

                     *> Inicializa la calle.--------------------------
*                    c_cal = IIF(l_copia_c,c_cal_ini,RIGHT(STR(100+n_cal,3),2))
                     c_cal = IIF(l_copia_c,Chr(n_cal)+Space(1),RIGHT(STR(100+n_cal,3),2))

                     *> Genera el codigo.-----------------------------
                     c_codubi = _alma+;
                                c_zon+;
                                c_cal+;
                                c_fil+;
                                RIGHT(STR(100+n_pis,3),2)+;
                                RIGHT(STR(100+n_pro,3),1)
****RIGHT(STR(100+n_cal,3),2)

                     *> Genera el registro.---------------------------
                     m.F10cCodUbi=C_CodUbi
                     Select Datos
                     Append Blank
                     REPLACE f10cCodUbi With c_codubi, ;
                             f10cTIPALM With M.F10cTipALm,;
                             f10cTAMUBI With M.F10cTamUbi,;
                             f10cVOLTOT With M.F10cVolTot,;
                             f10cVOLLIB With M.F10cVolLib,;
                             f10cPESTOT With M.F10cPesTot,;
                             f10cTIPPRO With M.F10cTipPro,;
                             f10cPICKSN With M.F10cPickSn,;
                             f10cCOORDX With M.F10cCoordX,;
                             f10cCOORDY With M.F10cCoordY,;
                             f10cPRIORI With M.F10cPriori,;
                             f10cORDENT With M.F10cOrdent,;
                             f10cORDSAL With M.F10cOrdSal,;
                             f10cMAXPAL With M.F10cMaxPal,;
                             f10cFECDES With M.F10cFecDes,;
                             f10cESTENT WITH M.F10cEstEnt,;
                             f10cESTSAL WITH M.F10cEstSal,;
                             f10cNUMOCU WITH M.F10cNumOcu,;
                             F10cEstGen With M.F10cEstGen
                  NEXT
               NEXT
            NEXT
         NEXT
      NEXT
   NEXT

   *> Cerramos la ventana de porcentaje.------------------------------
    =f3_term('FIN')

RETURN

PROCEDURE updatemain
PRIVATE w_mensaje
PRIVATE n_total, n_actual
Private C_Param, nFila

       m.IniFil = IIF(EMPTY(m.IniFil), 1, m.IniFil)

       *> Mensaje de espera.----------------------------------------------
       DECLARE a_texto(2)
       a_texto(1) = "Se están añadiendo las nuevas ubicaciones"
       a_texto(2) = "en el fichero, espere un momento."

       Wait Window "Se están añadiendo las nuevas ubicaciones ..." NoWait

       *> Abrimos los ficheros del proceso.-------------------------------
       Select F10c

       *> Borramos los registros ya generados.----------------------------
       C_Param =           "(" + _GCSS("F10ccodubi" ,1, 4) + " Between "+_cm+c_alm_ini+_cm+" And " +_cm+c_alm_fin+_cm+") And "
       C_Param = C_Param + "(" + _GCSS("F10ccodubi", 5, 2) + " Between "+_cm+c_zon_ini+_cm+" And " +_cm+c_zon_fin+_cm+") And "
       C_Param = C_Param + "(" + _GCSS("F10ccodubi", 7, 2) + " Between "+_cm+c_cal_ini+_cm+" And " +_cm+c_cal_fin+_cm+") And "
       C_Param = C_Param + "(" + _GCSS("F10ccodubi", 9, 3) + " Between "+_cm+c_fil_ini+_cm+" And " +_cm+c_fil_fin+_cm+") And "
       C_Param = C_Param + "(" + _GCSS("F10ccodubi",12, 2) + " Between "+_cm+c_pis_ini+_cm+" And " +_cm+c_pis_fin+_cm+") And "
       C_Param = C_Param + "(" + _GCSS("F10ccodubi",14, 1) + " Between "+_cm+c_pro_ini+_cm+" And " +_cm+c_pro_fin+_cm+")"

       If Used('F10cCur')
          Use In F10cCur
       EndIf

       =f3_sql('*', 'F10c', C_Param, 'F10cCodUbi', , 'F10cCur')
       Select F10cCur
       Go Top

       nFila = m.IniFil

       insertado = .T.
       Wait Clear

       *> Se añaden los registros.----------------------------------------
       If insertado
          Select Datos
          Count To n_total
          TEXTO = 'Actualizando ' + ALLTRIM(TRANSFORM(n_total, "99,999,999")) + ' Ubicaciones'

          do form ST3TERM
          =f3_term('TEXTO')
          i=inkey(.2)

          n_actual = 0

          Select Datos
          Go Top
          Do While !Eof()
             Scatter MemVar
             n_actual = n_actual + 1
             =f3_term('LINEA', n_actual, n_total)

             *> Utilizamos el F3_Ins con modificación cassolana.----------------
             m.F10cCodUbi = F10cCodUbi
             _ok=f3_seek('F10C')
             If !_ok
                Select F10c
                Append Blank
                Gather MemVar

                _ok = f3_InsTun("F10C")
                If !_ok
                   _LxErr = 'Error insertando ubicaciones ya existentes' + cr
                   _LxErr = _LxErr + Message() + cr
                   Do Form St3Inc With .T.
                   insertado = .F.
                   Exit Do
                EndIf
             EndIf

             *>
             Select Datos
             Skip
          EndDo 
       EndIf

       *> Cerramos la ventana de porcentaje.------------------------------
        =f3_term('FIN')

       *> Cerramos el fichero de ubicaciones.-----------------------------
       Select F10c
       Delete All

       Use In F10cCur
       If Used('Temp')
          Use In Temp
       EndIf

RETURN

*> Rutina para borrado o modificación de Ubicaciones.------------------
PROCEDURE ChangeUbic

   Private w_mensaje, c_set_del, n_cantidad, n_actual
   Private c_campos, c_campos1, c_fecha, _aaa, _bbb
   Private n_genera, C_Param, nFila, c_Intr
   Private lStado, cUbicacionInicial, cUbicacionFinal

   *> Declaro el array de campos.-------------------------------------
   Declare a_campos(FCount('F10C'))

   m.IniFil = IIF(EMPTY(m.IniFil), 1, m.IniFil)

   *> Inicializo el array.--------------------------------------------
   Select F10c
   For n_cantidad = 1 To ALen(a_campos)
      _aaa = '_Screen.ActiveForm.' + AllTrim(Field(n_cantidad)) + '.Name'
      _aaa = Type('&_aaa')
      If _aaa == 'C'
         _bbb = '_Screen.ActiveForm.' + AllTrim(Field(n_cantidad)) + '.BackColor'
         _bbb = &_bbb
         a_campos(n_cantidad) = Iif(_bbb = Rgb(255, 0, 0), .T., .F.)
      EndIf
   Next

   c_ubi_ini = _alma + " " + C_Zon + " " + C_Cal + " " + C_Fil + " " + C_Pis + " " + C_Pro
   c_ubi_fin = _alma + " " + C_Zon + " " + D_Cal + " " + D_Fil + " " + D_Pis + " " + D_Pro

   *> Almacén.--------------------------------------------------------
   c_alm_ini = _Alma
   c_alm_fin = _Alma
   n_alm_ini = Val(_Alma)
   n_alm_fin = Val(_Alma)

   *> Zona.-----------------------------------------------------------
   c_zon_ini = C_Zon
   c_zon_fin = C_Zon
   n_zon_ini = Val(c_zon)
   n_zon_fin = Val(c_zon)

   *> Calle.----------------------------------------------------------
   c_cal_ini = C_Cal
   c_cal_fin = D_Cal
   n_cal_ini = Val(C_cal)
   n_cal_fin = Val(D_cal)

   *> Asignamos la variable de copia de calle.------------------------
   l_copia_c = .F.

   *> Chequeo a la fila.----------------------------------------------
   IF BETWEEN(LEFT(c_Cal_ini,1),"A","Z")
      *> Asignamos la variable de copia de fila.----------------------
      l_copia_c = .T.

      n_cal_ini = C_Cal
      n_cal_fin = D_cal
   ENDIF

   *> Fila.-----------------------------------------------------------
   c_fil_ini = C_Fil
   c_fil_fin = D_Fil
   n_fil_ini = VAL(C_fil)
   n_fil_fin = VAL(D_fil)

   *> Piso.-----------------------------------------------------------
   c_pis_ini = C_Pis
   c_pis_fin = D_Pis
   n_pis_ini = VAL(C_pis)
   n_pis_fin = VAL(D_pis)

   *> Profundidad.----------------------------------------------------
   c_pro_ini = C_Pro
   c_pro_fin = D_Pro
   n_pro_ini = VAL(C_pro)
   n_pro_fin = VAL(D_pro)

   SwResp=f3_sn(2,3,"Se modificará el fichero de ubicaciones", ;
                    "Con todos los campos que tengan algún valor", ;
                    "Desea continuar con el proceso", "", "", "")
   If !SwResp
      Return
   EnDif

   *> Preparar sentencia SQL para cargar las ubicaciones a modificar, según recorrido.
   If m.Recorrido=='V'
      cUbicacionInicial = c_alm_ini + c_zon_ini + c_cal_ini + c_fil_ini + c_pis_ini + c_pro_ini
      cUbicacionFinal = c_alm_fin + c_zon_fin + c_cal_fin + c_fil_fin + c_pis_fin + c_pro_fin
      C_Param = "(F10cCodUbi Between '" + cUbicacionInicial + "' And '" + cUbicacionFinal + "')"
   Else
      C_Param =           "(" + _GCSS("F10ccodubi" ,1, 4) + " Between " + _cm + c_alm_ini + _cm + " And " + _cm + c_alm_fin +_cm + ") And "
      C_Param = C_Param + "(" + _GCSS("F10ccodubi", 5, 2) + " Between " + _cm + c_zon_ini + _cm + " And " + _cm + c_zon_fin +_cm + ") And "   
      C_Param = C_Param + "(" + _GCSS("F10ccodubi", 7, 2) + " Between " + _cm + c_cal_ini + _cm + " And " + _cm + c_cal_fin +_cm + ") And "   
      C_Param = C_Param + "(" + _GCSS("F10ccodubi", 9, 3) + " Between " + _cm + c_fil_ini + _cm + " And " + _cm + c_fil_fin +_cm + ") And "
      C_Param = C_Param + "(" + _GCSS("F10ccodubi",12, 2) + " Between " + _cm + c_pis_ini + _cm + " And " + _cm + c_pis_fin +_cm + ") And "
      C_Param = C_Param + "(" + _GCSS("F10ccodubi",14, 1) + " Between " + _cm + c_pro_ini + _cm + " And " + _cm + c_pro_fin +_cm + ")"
   EndIf

   *> Asignamos los campos y los valores.-----------------------------
   c_campos = ""

   c_campos = c_campos + Iif(_Screen.ActiveForm.Almace.BackColor = Rgb(255, 0, 0), ", F10cTipAlm="+_cm+ m.F10cTipAlm +_cm, "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.F10cTamUbi.BackColor = Rgb(255, 0, 0), ", F10ctamubi="+_cm+ m.F10cTamubi +_cm, "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.F10cVolTot.BackColor = Rgb(255, 0, 0), ", F10cVolTot="+ Str(m.F10cVolTot,10), "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.F10cVolLib.BackColor = Rgb(255, 0, 0), ", F10cVolLib="+ Str(m.F10cVolLib,10), "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.F10cPesTot.BackColor = Rgb(255, 0, 0), ", F10cPesTot="+ Str(m.F10cPesTot,10), "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.F10cPesOcu.BackColor = Rgb(255, 0, 0), ", F10cPesOcu="+ Str(m.F10cPesOcu,10), "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.F10cTipPro.BackColor = Rgb(255, 0, 0), ", F10cTipPro="+_cm+ m.F10cTipPro +_cm, "")
   c_campos = c_campos + Iif(_Screen.ActiveForm.TipUbi.BackColor = Rgb(255, 0, 0), ", F10cPickSn="+_cm+ m.F10cPickSn +_cm, "")

   *> Elimina la primera coma del string.
   c_campos = Iif(Empty(c_campos), "", Right(c_campos, Len(c_campos)-1))

   c_campos1 = ""
   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cPriori.BackColor = Rgb(255, 0, 0), ",F10cPriori="+ Str(m.F10cPriori,5), "")
   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cMaxPal.BackColor = Rgb(255, 0, 0), ",F10cMaxPal="+ Str(m.F10cMaxPal,5), "")

   *> Tratamiento de fecha desocupación.
   c_fecha = _GCD(m.F10cFecDes)

   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cFecDes.BackColor = Rgb(255, 0, 0), ",F10cFecDes="+ c_fecha,"")
   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cEstEnt.BackColor = Rgb(255, 0, 0), ",F10cEstEnt="+_cm+ m.F10cEstEnt +_cm, "")
   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cEstSal.BackColor = Rgb(255, 0, 0), ",F10cEstSal="+_cm+ m.F10cEstSal +_cm, "")
   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cEstGen.BackColor = Rgb(255, 0, 0), ",F10cEstGen="+_cm+ m.F10cEstGen +_cm, "")
   c_campos1 = c_campos1 + Iif(_Screen.ActiveForm.F10cNumOcu.BackColor = Rgb(255, 0, 0), ",F10cNumOcu="+ Str(m.F10cNumOcu,5), "")

   *> Elimina la primera coma del string.
   c_campos1 = Iif(Empty(c_campos1), "", Right(c_campos1, Len(c_campos1)-1))

	Use In (Select("F10cCur"))
        
    nFila = m.IniFil

	*> Cargar el cursor con las ubicaciones candidatas.
    lStado = f3_sql('*', 'F10c', C_Param, 'F10cCodUbi', , 'F10cCur')
	If !lStado
		If _xier <= 0
			_LxErr = "Error cargando las ubicaciones a modificar"
			Do Form St3Inc With .T.
			Return
		EndIf
	EndIf

    Select F10cCur
    Go Top

*!*		nFilaActiva = 0
*!*		nFilaActual = -1

	*> Procesar las ubicaciones.
	Do While !Eof()
*!*			If nFilaActual < 0
*!*				nFilaActual = Val(SubStr(F10cCodUbi, 9, 3))
*!*			EndIf

*!*			*> Si hay cambio de fila, ver si es procesable ó no.
*!*			nFilaActiva = Val(SubStr(F10cCodUbi, 9, 3))

*!*			If nFilaActiva # nFilaActual
*!*				Do While nFilaActual < nFilaActiva
*!*					nFilaActual = nFilaActual + nFila
*!*				EndDo

*!*				If nFilaActiva # nFilaActual
*!*					Select F10cCur
*!*					Skip
*!*				EndIf
*!*				Loop
*!*			EndIf

		*> Actualizar la ubicación activa.
        =WaitWindow ('Modificando ubicación: ' + F10cCur.F10cCodUbi)

        If !Empty(c_campos)
           C_intr = "Update F10c" + _em + " Set " + C_campos + " Where F10cCodUbi='" + F10cCur.F10cCodUbi + "'"
           _xier = f3_SqlExec(_ASql, C_intr)

           If _xier<=0
              _LxErr = 'Error modificando ubicaciones (1)' + cr
              _LxErr = _LxErr + 'MENSAJE: ' + Message() + cr
              _LxErr = _LxErr + 'SENTENCIA: ' + C_intr + cr
              Do Form St3Inc With .T.
              Exit
           EndIf 
		EndIf

        If !Empty(c_campos1)
           C_intr = "Update F10c" + _em + " Set " + C_campos1 + " Where F10cCodUbi='" + F10cCur.F10cCodUbi + "'"
           _xier = f3_SqlExec(_ASql, C_intr)
           If _xier<=0
              _LxErr = 'Error modificando ubicaciones (2)' + cr
              _LxErr = _LxErr + 'MENSAJE: ' + Message() + cr
              _LxErr = _LxErr + 'SENTENCIA: ' + C_intr + cr
              Do Form St3Inc With .T.
              Exit
           EndIf 
        EndIf

		*> Tomar la siguiente ubicación a modificar.
		Select F10cCur
		Skip
	EndDo

    Use In (Select("F10cCur"))
    Use In (Select("Temp"))

Return

Procedure BorraUbic

Private w_mensaje, c_set_del, n_cantidad, n_actual
Private C_Param, nFila
Private c_campos, c_campos1
Private lStado, nFilaActual, nFilaActiva, cWhere

   *> Declaro el array de campos.-------------------------------------
   DECLARE a_campos(22)
   
   *> Inicializo el array.--------------------------------------------
   FOR n_cantidad = 1 TO ALEN(a_campos)
      a_campos(n_cantidad) = .F.
   NEXT

   m.IniFil = IIF(EMPTY(m.IniFil), 1, m.IniFil)

   c_ubi_ini = _alma+" "+C_Zon+" "+C_Cal+" "+C_Fil+" "+C_Pis+" "+C_Pro
   c_ubi_fin = _alma+" "+C_Zon+" "+D_Cal+" "+D_Fil+" "+D_Pis+" "+D_Pro

   *> Almacén.--------------------------------------------------------
   c_alm_ini = _Alma
   c_alm_fin = _Alma
   n_alm_ini = VAL(_Alma)
   n_alm_fin = VAL(_Alma)

   *> Zona.-----------------------------------------------------------
   c_zon_ini = C_Zon
   c_zon_fin = C_Zon
   n_zon_ini = VAL(c_zon)
   n_zon_fin = VAL(c_zon)

   *> Calle.----------------------------------------------------------
   c_cal_ini = C_Cal
   c_cal_fin = D_Cal
   n_cal_ini = VAL(C_cal)
   n_cal_fin = VAL(D_cal)

   *> Fila.-----------------------------------------------------------
   c_fil_ini = C_Fil
   c_fil_fin = D_Fil
   n_fil_ini = VAL(C_fil)
   n_fil_fin = VAL(D_fil)

   *> Piso.-----------------------------------------------------------
   c_pis_ini = C_Pis
   c_pis_fin = D_Pis
   n_pis_ini = VAL(C_pis)
   n_pis_fin = VAL(D_pis)

   *> Profundidad.----------------------------------------------------
   c_pro_ini = C_Pro
   c_pro_fin = D_Pro
   n_pro_ini = VAL(C_pro)
   n_pro_fin = VAL(D_pro)

   SwResp = f3_sn(2, 3, "Se modificará el fichero de Ubicaciones", ;
                        "borrándose las ubicaciones que estén entre los límites", ;
                        "Desea continuar con el proceso.", "", "", "")
   If !SwResp
      Return
   EnDif

   *> Preparar sentencia SQL.
   C_Param =           "(" + _GCSS("F10ccodubi" ,1, 4) + " Between "+_cm+c_alm_ini+_cm+" And " +_cm+c_alm_fin+_cm+") And "
   C_Param = C_Param + "(" + _GCSS("F10ccodubi", 5, 2) + " Between "+_cm+c_zon_ini+_cm+" And " +_cm+c_zon_fin+_cm+") And "
   C_Param = C_Param + "(" + _GCSS("F10ccodubi", 7, 2) + " Between "+_cm+c_cal_ini+_cm+" And " +_cm+c_cal_fin+_cm+") And "
   C_Param = C_Param + "(" + _GCSS("F10ccodubi", 9, 3) + " Between "+_cm+c_fil_ini+_cm+" And " +_cm+c_fil_fin+_cm+") And "
   C_Param = C_Param + "(" + _GCSS("F10ccodubi",12, 2) + " Between "+_cm+c_pis_ini+_cm+" And " +_cm+c_pis_fin+_cm+") And "
   C_Param = C_Param + "(" + _GCSS("F10ccodubi",14, 1) + " Between "+_cm+c_pro_ini+_cm+" And " +_cm+c_pro_fin+_cm+")"
      
	Use In (Select("F10cCur"))

    nFila = m.IniFil
    
	*> Cargar el cursor con las ubicaciones candidatas.
    =f3_sql('*', 'F10c', C_Param, 'F10cCodUbi', , 'F10cCur')
    Select F10cCur
    Go Top

	nFilaActiva = 0
	nFilaActual = -1

	*> Procesar las ubicaciones.
	Do While !Eof()
		If nFilaActual < 0
			nFilaActual = Val(SubStr(F10cCodUbi, 9, 3))
		EndIf

		*> Si hay cambio de fila, ver si es procesable ó no.
		nFilaActiva = Val(SubStr(F10cCodUbi, 9, 3))

		If nFilaActiva # nFilaActual
			Do While nFilaActual < nFilaActiva
				nFilaActual = nFilaActual + nFila
			EndDo

			If nFilaActiva # nFilaActual
				Select F10cCur
				Skip
			EndIf
			Loop
		EndIf

        =WaitWindow ('Borrando ubicación: ' + F10cCur.F10cCodUbi)
        cWhere = "F10cCodUbi='" + F10cCur.F10cCodUbi + "'"
        lStado = f3_DelTun('F10c', , cWhere, 'N')
        If !lStado
        	_LxErr = "Error borrando ubicación: " + F10cCur.F10cCodUbi
        	Do Form St3Inc With .T.
        	Exit
        EndIf

		*> Tomar la siguiente ubicación a modificar.
		Select F10cCur
		Skip
	EndDo

*    =f3_sql('*', 'F10c', C_Param, 'F10cCodUbi', , 'F10cCur')
*    Select F10cCur
*    Go Top

*    Do While !Eof()
       
*       If nFila < m.IniFil
*          nFila = nFila + 1
*          Skip
*          Loop
*       EndIf

 *      Wait Window 'Borrando ubicación: ' + F10cCur.F10cCodUbi NoWait

*       cWhere = "F10cCodUbi='" + F10cCur.F10cCodUbi + "'"
*       _borrado = f3_DelTun('F10c', , cWhere, 'N')
*       If !_borrado
*          Exit
*       EndIf

       *>
*       nFila = 1
*
*       Select F10cCur
*       Skip
*    EndDo

*!*	Procedure BorraUbic

*!*	Private w_mensaje, c_set_del, n_cantidad, n_actual
*!*	Private C_Param, nFila
*!*	Private c_campos, c_campos1
*!*	Private lStado, cUbicacionInicial, cUbicacionFinal

*!*	   *> Declaro el array de campos.-------------------------------------
*!*	   DECLARE a_campos(22)
*!*	   
*!*	   *> Inicializo el array.--------------------------------------------
*!*	   FOR n_cantidad = 1 TO ALEN(a_campos)
*!*	      a_campos(n_cantidad) = .F.
*!*	   NEXT

*!*	   m.IniFil = IIF(EMPTY(m.IniFil), 1, m.IniFil)

*!*	   c_ubi_ini = _alma+" "+C_Zon+" "+C_Cal+" "+C_Fil+" "+C_Pis+" "+C_Pro
*!*	   c_ubi_fin = _alma+" "+C_Zon+" "+D_Cal+" "+D_Fil+" "+D_Pis+" "+D_Pro

*!*	   *> Almacén.--------------------------------------------------------
*!*	   c_alm_ini = _Alma
*!*	   c_alm_fin = _Alma
*!*	   n_alm_ini = VAL(_Alma)
*!*	   n_alm_fin = VAL(_Alma)

*!*	   *> Zona.-----------------------------------------------------------
*!*	   c_zon_ini = C_Zon
*!*	   c_zon_fin = C_Zon
*!*	   n_zon_ini = VAL(c_zon)
*!*	   n_zon_fin = VAL(c_zon)

*!*	   *> Calle.----------------------------------------------------------
*!*	   c_cal_ini = C_Cal
*!*	   c_cal_fin = D_Cal
*!*	   n_cal_ini = VAL(C_cal)
*!*	   n_cal_fin = VAL(D_cal)

*!*	   *> Fila.-----------------------------------------------------------
*!*	   c_fil_ini = C_Fil
*!*	   c_fil_fin = D_Fil
*!*	   n_fil_ini = VAL(C_fil)
*!*	   n_fil_fin = VAL(D_fil)

*!*	   *> Piso.-----------------------------------------------------------
*!*	   c_pis_ini = C_Pis
*!*	   c_pis_fin = D_Pis
*!*	   n_pis_ini = VAL(C_pis)
*!*	   n_pis_fin = VAL(D_pis)

*!*	   *> Profundidad.----------------------------------------------------
*!*	   c_pro_ini = C_Pro
*!*	   c_pro_fin = D_Pro
*!*	   n_pro_ini = VAL(C_pro)
*!*	   n_pro_fin = VAL(D_pro)

*!*	   SwResp = f3_sn(2, 3, "Se modificará el fichero de Ubicaciones", ;
*!*	                        "borrándose las ubicaciones que estén entre los límites", ;
*!*	                        "Desea continuar con el proceso.", "", "", "")
*!*	   If !SwResp
*!*	      Return
*!*	   EnDif


*!*	   *> Preparar sentencia SQL para cargar las ubicaciones a modificar, según recorrido.
*!*	   If m.Recorrido=='V'
*!*	      cUbicacionInicial = c_alm_ini + c_zon_ini + c_cal_ini + c_fil_ini + c_pis_ini + c_pro_ini
*!*	      cUbicacionFinal = c_alm_fin + c_zon_fin + c_cal_fin + c_fil_fin + c_pis_fin + c_pro_fin
*!*	      C_Param = "(F10cCodUbi Between '" + cUbicacionInicial + "' And '" + cUbicacionFinal + "')"
*!*	   Else
*!*	      C_Param =           "(" + _GCSS("F10ccodubi" ,1, 4) + " Between " + _cm + c_alm_ini + _cm + " And " + _cm + c_alm_fin +_cm + ") And "
*!*	      C_Param = C_Param + "(" + _GCSS("F10ccodubi", 5, 2) + " Between " + _cm + c_zon_ini + _cm + " And " + _cm + c_zon_fin +_cm + ") And "   
*!*	      C_Param = C_Param + "(" + _GCSS("F10ccodubi", 7, 2) + " Between " + _cm + c_cal_ini + _cm + " And " + _cm + c_cal_fin +_cm + ") And "   
*!*	      C_Param = C_Param + "(" + _GCSS("F10ccodubi", 9, 3) + " Between " + _cm + c_fil_ini + _cm + " And " + _cm + c_fil_fin +_cm + ") And "
*!*	      C_Param = C_Param + "(" + _GCSS("F10ccodubi",12, 2) + " Between " + _cm + c_pis_ini + _cm + " And " + _cm + c_pis_fin +_cm + ") And "
*!*	      C_Param = C_Param + "(" + _GCSS("F10ccodubi",14, 1) + " Between " + _cm + c_pro_ini + _cm + " And " + _cm + c_pro_fin +_cm + ")"
*!*	   EndIf
*!*	      
*!*		Use In (Select("F10cCur"))

*!*	    nFila = m.IniFil
*!*	    
*!*		*> Cargar el cursor con las ubicaciones candidatas.
*!*	    lStado = f3_sql('*', 'F10c', C_Param, 'F10cCodUbi', , 'F10cCur')
*!*		If !lStado
*!*			If _xier <= 0
*!*				_LxErr = "Error cargando las ubicaciones a borrar"
*!*				Do Form St3Inc With .T.
*!*				Return
*!*			EndIf
*!*		EndIf

*!*	    Select F10cCur
*!*	    Go Top

*!*	*!*		nFilaActiva = 0
*!*	*!*		nFilaActual = -1

*!*		*> Procesar las ubicaciones.
*!*		Do While !Eof()
*!*	*!*			If nFilaActual < 0
*!*	*!*				nFilaActual = Val(SubStr(F10cCodUbi, 9, 3))
*!*	*!*			EndIf

*!*	*!*			*> Si hay cambio de fila, ver si es procesable ó no.
*!*	*!*			nFilaActiva = Val(SubStr(F10cCodUbi, 9, 3))

*!*	*!*			If nFilaActiva # nFilaActual
*!*	*!*				Do While nFilaActual < nFilaActiva
*!*	*!*					nFilaActual = nFilaActual + nFila
*!*	*!*				EndDo

*!*	*!*				If nFilaActiva # nFilaActual
*!*	*!*					Select F10cCur
*!*	*!*					Skip
*!*	*!*				EndIf
*!*	*!*				Loop
*!*	*!*			EndIf

*!*			*> Validar si hay ocupaciones.
*!*	        cWhere = "F16cCodUbi='" + F10cCur.F10cCodUbi + "'"
*!*	        If f3_sql("*", "F16c", cWhere, , , "_F16cOcup")
*!*				=WaitWindow ('La ubicación: ' + F10cCur.F10cCodUbi + " está ocupada")
*!*			Else
*!*				*> Borrar la ubicación.
*!*		        =WaitWindow ('Borrando ubicación: ' + F10cCur.F10cCodUbi)
*!*		        cWhere = "F10cCodUbi='" + F10cCur.F10cCodUbi + "'"
*!*		        lStado = f3_DelTun('F10c', , cWhere, 'N')
*!*		        If !lStado
*!*		        	_LxErr = "Error borrando ubicación: " + F10cCur.F10cCodUbi
*!*		        	Do Form St3Inc With .T.
*!*		        	Exit
*!*		        EndIf
*!*			EndIf

*!*			*> Tomar la siguiente ubicación a modificar.
*!*			Select F10cCur
*!*			Skip
*!*		EndDo

*!*		Use In (Select("F10cCur"))
*!*		Use In (Select ("_F16cOcup"))

*!*	Return
