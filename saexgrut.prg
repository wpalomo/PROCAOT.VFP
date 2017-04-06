*> Capcelera

   *> Descripcio ................. Procesos del generación hojas de ruta.
   *> Módulo ..................... SaExGRut
   *> Llenguatge ................. FoxPro 2.0
   *> Sistema Operatiu ........... MS-DOS Ver 3.30 o superior
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... Javier Garrido
   *> Programador ................ Javier Garrido
   *> Data d'inici ............... 04.02.99
   *> Data de fi ................. 

*> Utilitats

   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*>        - Enlace con Confirmación de carga. AVC - 01.12.2000
*>        - Enlace en BATCH con Confirmación de carga. AVC - 17.01.2001
*>
*>
*>

*>           (C)Copyright ALISFOUR 2004 BARCELONA

************************************************************************************************
*> Procedimiento para generar hoja de ruta.                                                    *
*> Realizar confirmación de carga desde aquí, si no se ha hecho ya. AVC - 18.10.2000           *
************************************************************************************************
Procedure GrbAlbS
Private c_HojRut, b_Err

   Set Procedure To SaExCfCa Additive

   =CrtCursor('F30c', 'GRutF30c')
   =CrtCursor('F31c', 'GRutF31c')
   =CrtCursor('F31l', 'GRutF31l')

   c_HojRut = ""
   b_Err = .F.
   n_PalCmp = 0
   n_CajLib = 0
   n_CajPic = 0
   n_Bultos = 0
   n_PesoKg = 0
   n_Volume = 0
   n_NumAlb = 0

   *> Bucle de lectura del GRID de documentos.------------------------
   Select CABGRU
   Locate For Marca==1
   Do While Found() .And. b_Err=.F.

      *> Realizar confirmación de carga del documento actual.

      *>
      *> Anular Confirmación de Carga On Line. AVC - 17.01.2001
      *>
*     If !CnfC_ProcDoc(F30cCodEnt, F30cTipDoc, F30cNumDoc)
*        Do Form St3Inc With .T.
*        =SqlRollBack(_ASql)
*        Select CABGRU
*        Skip
*        Loop
*     EndIf

      *> Obtener nuevo número de hoja de ruta.------------------------
      If Empty(c_HojRut)
         c_HojRut = Ora_NewHRu()

         *> Generar cabecera de hoja de ruta (F31c).------------------
         Select GRutF31c
         Zap
         Append Blank
         Replace F31cHojRut With c_HojRut, ;
                 F31cFecRut With Date(), ;
                 F31cCodVeh With "000000", ;
                 F31cCodCho With "000000", ;
                 F31cCodTra With m.CodTra, ;
                 F31cFecSal With Date(), ;
                 F31cFecIni With Date(), ;
                 F31cFecFin With Date(), ;
                 F31cFecLiq With _FecMin

         Sw = f3_InsTun('F31C', 'GRutF31c', 'N')
         If Sw =.F.  .And.  b_Err=.F.
            _LxErr = _LxErr + 'Error insertando cabecera de hoja de ruta: ' + c_HojRut + cr
            b_Err=.T.
         EndIf
      EndIf

      *> Generar línea de hoja de ruta (F31l).------------------------
      Select GRutF31l
      Zap
      Append Blank
      Replace F31lHojRut With c_HojRut,          F31lTipEnt With CabGRu.F30cTipEnt, ;
              F31lCodEnt With CabGRu.F30cCodEnt, F31lTipAlb With CabGRu.F30cTipAlb, ;
              F31lTipDoc With CabGRu.F30cTipDoc, F31lAlbRep With CabGRu.F30cAlbRep, ;
              F31lCodCnf With Space(4),          F31lFecCnf With Date(), ;
              F31lRutHab With CabGRu.F30cRutHab, F31lCodTra With m.CodTra, ;
              F31lFecEnt With Date()
      Sw = F3_InsTun('F31L', 'GRutF31l', 'N')
      If Sw=.F.  .And.  b_Err=.F.
         b_Err=.T.
      EndIf

      *> Acumular totales a variables.--------------------------------
      n_PalCmp = n_PalCmp + CabGRu.F30cPalCmp
      n_CajLib = n_CajLib + CabGRu.F30cCajLib
      n_CajPic = n_CajPic + CabGRu.F30cCajPic
      n_Bultos = n_Bultos + CabGRu.F30cBultos
      n_PesoKg = n_PesoKg + CabGRu.F30cPesoKg
      n_Volume = n_Volume + CabGRu.F30cVolume
      n_NumALb = n_NumAlb + 1
      
      *> Actualizar albarán F30c.-------------------------------------
      c_CodSit= "1"
      lxWhere = "F30cTipEnt = '" + CabGRu.F30cTipEnt + "' And " + ;
                "F30cCodEnt = '" + CabGRu.F30cCodEnt + "' And " + ;
                "F30cTipAlb = '" + CabGRu.F30cTipAlb + "' And " + ;
                "F30cTipDoc = '" + CabGRu.F30cTipDoc + "' And " + ;
                "F30cAlbRep = '" + CabGRu.F30cAlbRep + "'"
      Sw = F3_UpdTun('F30C',, 'F30cHojRut, F30cCodSit', 'c_HojRut, c_CodSit',, lxWhere, 'N','N')
      If Sw=.F.  .And.  b_Err=.F.
         _LxErr = _LxErr + 'Error actualizando cabecera de albarán (F30C)' + cr + ;
                           'Documento: ' + F30cCodEnt + '-' + F30cTipDoc + '-' + F30cNumDoc + cr
         b_Err=.T.
      EndIf

      *> Siguiente documento de salida a procesar.--------------------
      Select CABGRU
      Continue
   EndDo

   If b_Err = .F.
      *> Actualizar cabecera hoja ruta con totales.-------------------
      f_Campos = 'F31cAlbRut, F31cPalCmp, F31cCajLib, F31cCajPic, F31cBultos, F31cPesoKg, F31cVolume'
      f_Asigna = 'n_NumAlb, n_PalCmp, n_CajLib, n_CajPic, n_Bultos, n_PesoKg, n_Volume'
      lxWhere = "F31cHojRut = '" + c_HojRut + "'"
      Sw = F3_UpdTun('F31C', , f_Campos, f_Asigna, , lxWhere, 'N', 'N')
      If Sw=.F.  .And.  b_Err=.F.
         _LxErr = _LxErr + 'Error actualizando totales cabecera de hoja de ruta: ' + c_HojRut + cr
         b_Err=.T.
      EndIf
   EndIf
   
   If b_Err = .F.
      =SqlCommit(_ASql)
      =f3_sn(1, 1, "Se ha generado la hoja de ruta " + c_HojRut)

      If f3_sn(2, 1, 'Desea imprimir hoja de ruta')
         NombreReportG = 'SaExAlrL11'
         Do GenAlrG With c_HojRut In Listados
         Do PrtAlrG With NombreReportG In Listados
      EndIf
   Else
      Do Form St3Inc With .T.
      =SqlRollBack(_ASql)
   EndIf


   *> Generar fichero de pedidos procesados para enlace con SAP.
   If Type('FicheroDeEnlace')=='C'
      nHnd = .F.

      Select CABGRU
      Locate For Marca = 1
      Do While Found()
         *> Abrir el fichero de confirmación de carga en BATCH.
         If Type('nHnd')=='L'
            If !File(FicheroDeEnlace)
               nHnd = FCreate(FicheroDeEnlace, 0)      && No existe: Crear fichero.
               =FClose(nHnd)
            EndIf

            nHnd = FOpen(FicheroDeEnlace, 2)           && Existe: Abrir fichero.
            If nHnd <= 0
               _LxErr = 'Error al abrir el fichero de confirmación de carga' + cr
               Do Form St3Inc With .T.
               Exit
            EndIf
         EndIf

         Wait Window 'Preparando confirmación de carga de documento: ' + CABGRU.F30cNumDoc NoWait

         *> Grabar Propietario + tipo documento + nº documento en el fichero.
         nBytes = FSeek(nHnd, 0, 2)                         && Posicionarse al final.
         =FPutS(nHnd, CABGRU.F30cCodEnt + CABGRU.F30cTipDoc + CABGRU.F30cNumDoc + '0')

         *>
         Select CABGRU
         Continue
      EndDo

      *> Cerrar fichero de confirmación de carga en BATCH.
      =FClose(nHnd)
   EndIf

   Wait Clear

   *> Borrar cursores de trabajo.
   Use In GRutF30c
   Use In GRutF31c
   Use In GRutF31l

   Release Procedure SaExCfCa

Return
