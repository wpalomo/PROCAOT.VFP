*> Cabecera

   *> Descripcio ................. Librería para ubicar la mercancía en Fox - Oracle.
   *> Modul ...................... Ora_Cu00.PRG
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 18.09.98
   *> Data de fi .................

*> Notes


**************************************************************
*     Funciones standard para ProDis / Procaot en Oracle     *
**************************************************************

*> Funciones ............................ Prc_Com          Rutina ubicación en compacto.
*>                                        Prc_Ubi          Rutina ubicación principal.
*>                                        Prc_Ubi_Pal
*>                                        Prc_Alm          Rutina ubicación caótica.
*>                                        Prc_AlmTipPro    Rutina ubicación caótica por tipo prod.
*>                                        Prc_Pic          Ubicaciones picking.
*>                                        Prc_PicArt       Ubicaciones picking artículo.
*>                                        Prc_PicRng       Ubicaciones picking rango.
*>                                        Prc_PicGen       Ubicaciones picking almacén.
*>                                        Prc_PicPicos     Ubicaciones picking a picos.

*>                                        UbiPicAsignada   Comprobar ubic. asignada a otro prod.
*>                                        UbiPicOcupada    Comprobar ubic. ocupada por otro prod.
*>                                        UbiPicOcupadaMP  Comprobar ubic. ocupada por MPs.

*>=========================================================
*> Prc_Com .................. Rutina ubicación en compacto
*>=========================================================

Procedure Prc_Com
Parameters c_CodPro, c_CodArt, f_FecCad, c_TamUbi, n_Cantid, c_Retorna, l_PrimeraVez
   EXTERNAL ARRAY aubparmz()

   *> Guardamos el entorno.-------------------------------------------
   f_anterior = SELECT()
   w_anterior = WOUTPUT()

   *> Variable que contendrá la ubicación.----------------------------
   c_retorna = ""

   *> Abrimos el fichero de artículos.--------------------------------
   =CrtCursor('F08c', 'PComF08c')
   lxWhere = "F08cCodPro='" + c_CodPro + "' And F08cCodArt='" + c_CodArt + "'"
   =f3_sql('*', 'F08c', lxWhere, , , 'PComF08c')
   Select PComF08c
   Go Top
   IF EOF()
      Use In PComF08c
      Return
   Endif   

   *> Guardamos datos comunes.----------------------------------------
   c_TipAlm = F08cTipAlm
   c_TipPro = F08cTipPro
   c_MulLot = F08cMulLot
   c_MulPro = F08cMulPro
   n_CoeRot = F08cCoeRot

   Store 0 to n_TotPes, n_TotVol
   Do PesVolAr In Ora_Ca00 With c_CodPro, c_CodArt, n_Cantid, n_TotPes, n_TotVol

   *> Ubicar solo compactos.------------------------------------------
   If c_TipAlm <> "P"
      Use In PComF08c
      Return
   EndIf

   *> Ubicaciones iniciales.------------------------------------------
   c_UbiI = _Alma + "          "
   c_UbiF = _Alma + "ZZZZZZZZZZ"

   *> Buscamos las ubicaciones de la mercancía.-----------------------
   *> Devuelve c_IbiI, c_UbiF.----------------------------------------
   Do Busca_Zona With c_CodPro, c_CodArt

   *> Buscamos la zona.-----------------------------------------------
   Do Busca_ZnPk With c_CodPro, c_CodArt, _Alma

   *> Zona.-----------------------------------------------------------
   ZI = SUBSTR(c_UbiI, 5, 2)
   ZF = SUBSTR(c_UbiF, 5, 2)
   *> Calle.----------------------------------------------------------
   CI = SUBSTR(c_UbiI, 7, 2)
   CF = SUBSTR(c_UbiF, 7, 2)
   *> Fila.-----------------------------------------------------------
   FI = SUBSTR(c_UbiI,9, 3)
   FF = SUBSTR(c_UbiF,9, 3)
   *> Piso.-----------------------------------------------------------
   PI = SUBSTR(c_UbiI,12, 2)
   PF = SUBSTR(c_UbiF,12, 2)
   *> Profundidad.----------------------------------------------------
   RI = SUBSTR(c_UbiI,14, 1)
   RF = SUBSTR(c_UbiF,14, 1)

   If _Lenguaje <> 'VB'
	   *> Mensaje de espera.----------------------------------------------
	   Wait Window "Intentando ubicar el artículo " + AllTrim(c_CodArt) NoWait
   EndIf
 
   *> Activamos el fichero de ubicaciones.----------------------------
   =CrtCursor('F10c', 'PComF10c')
*   lxOrder = "F10cTipAlm, F10cTipPro, F10cTamUbi, Str(F10cPriori,5), F10cCodUbi"
   lxOrder = "F10cCodUbi"
   =f3_sql('*', 'F10c', , , , 'PComF10c')

   If l_PrimeraVez
      c_FilSec = " .And. F10cNumOcu < F10cMaxPal .And. F10cEstGen <> 'O' .And. " + ;
                 "F10cEstEnt <> 'S' .And. F10cTamUbi == c_Tamanyo"
   Else
      c_FilSec = " .And. F10cNumOcu < F10cMaxPal .And. F10cEstGen <> 'O' .And. " + ;
                 "F10cTamUbi >= c_Tamanyo"
   EndIf

   c_Filter = ""

   *> Activamos el filtro por coef. rotación.-------------------------
   If aubparmz[5] = 'S'
      *> Inicializa prioridades.--------------------------------------
      n_ProI = 999999
      n_ProF = 0

      *> Retorna las proximidades.------------------------------------
      Do Busca_Prox With n_CoeRot

      c_Filter = " .And. Between(F10cPriori, n_ProF, n_ProI)"
   EndIf

   *> Búsqueda sobre F14c.--------------------------------------------
   =CrtCursor('F14c', 'PComF14c')
   lxWhere = "F14cCodPro = '" + c_CodPro + "' And F14cCodArt = '" + c_CodArt + "'"
   =f3_sql('*', 'F14c', lxWhere, , , 'PComF14c')
   
   *> Rastreo en movimientos pendientes (f14c).-----------------------
   Select PComF14c
   Go Top
   Scan While Empty(c_Retorna)
      If !Empty(F14cUbiOri) .And. (f_FecCad = F14cFecCad)
         Select F10c
         If Seek(F14c.F14cUbiOri)
            c_Retorna = F10cCodUbi
         EndIf
         Select F14c
      EndIf
   EndScan   

   *> Si encontramos la ubicación nos piramos.------------------------
   If !Empty(c_Retorna)
      Use In PComF08c
      Use In PComF10c
      Use In PComF14c

      *> Actualizamos el fichero de ubicaciones.----------------------
      l_Ubicado=.f.
      n_Npal=1
      Do CargaUbi In Ora_Ca00 With c_Retorna, n_Peso, n_Volu, n_NPal, l_Ubicado
      If !L_Ubicado
         c_Retorna=""
      EndIf   

      Return c_Retorna
   EndIf

   *> Activamos el fichero de ocupaciones.----------------------------
   =CrtCursor('F16c', 'PComF14c')
   lxWhere = "F16cCodPro = '" + c_CodPro + "' And F16cCodArt = '" + c_CodArt + "'"
   lxOrder = "F16cFecCad"
   =f3_sql('*', 'F16c', lxWhere, lxOrder, , 'PComF16c')

   *> Establecemos relaciones entre ocupaciones y ubicaciones.--------
   Set Relation To F16cCodUbi Into PComF10c
   
   *> Filtramos para los registros que cumplan condiciones del F10c.--
*   Select PComF16c
*   Set Filter To !Eof() Additive

   *> Buscamos el registro.-------------------------------------------
   If l_primeravez
      Seek DToS(f_FecCad)
   Else
      Go Top
   EndIf

   *> De las ocupadas no existe ningun registro.----------------------
   If EOF()
      If l_primeravez
         *> Cancelo filtros y relaciones entre ficheros.-----------------
         Set Filter To
         Set Relation To
      
         *> Busco sobre ubicaciones.-------------------------------------
         Select PComF10c
         Set Filter To

         *> Activamos el filtro.-----------------------------------------
         c_filsec = ".AND. f10ccanpal = 0 .and. f10cestgen<>'O' "
         
                  Set Filter To ((f10cresikg-f10cocupkg)>=n_peso).AND.;
                       BETW(SUBS(f10ccodUbi,5,2),ZI,ZF).AND.;
                       BETW(SUBS(f10ccodUbi,7,2),CI,CF).AND.;
                       BETW(SUBS(f10ccodUbi,9,3),FI,FF).AND.;
                       BETW(SUBS(f10ccodUbi,12,2),PI,PF).AND.;
                       BETW(SUBS(f10ccodUbi,14,1),RI,RF).AND.;
                       (f10ctippro==c_TipPr.OR.F10ctippro=='    ').AND.;
                       (f10ctamubi==c_tamanyo).AND.(f10ctipalm="P");
                       &c_filsec&c_filter
   
         GOTO TOP

         IF EOF()
            c_retorna = ""
         ELSE
            c_retorna = f10ccodUbi
         ENDIF
      ELSE

         *> ???-------------------------------------------------------
         *> Falta el saber si se puede reintentar por todas las
         *> zonas y los tama¤os.
         *> ----------------------------------------------------------

         *> Ubicaciones iniciales.------------------------------------
         c_UbiI = _Alma + "         "
         c_UbiF = _Alma + "ZZZZZZZZZ"

         *> Zona.-----------------------------------------------------
         c_ZonI = SUBSTR(c_UbiI, 3, 2)
         c_ZonF = SUBSTR(c_UbiF, 3, 2)
         *> Calle.----------------------------------------------------
         c_CalI = SUBSTR(c_UbiI, 5, 2)
         c_CalF = SUBSTR(c_UbiF, 5, 2)
         *> Fila.-----------------------------------------------------
         c_FilI = SUBSTR(c_UbiI,7, 2)
         c_FilF = SUBSTR(c_UbiF,7, 2)
         *> Piso.-----------------------------------------------------
         c_PisI = SUBSTR(c_UbiI,9, 2)
         c_PisF = SUBSTR(c_UbiF,9, 2)
         *> Profundidad.----------------------------------------------
         c_ProI = SUBSTR(c_UbiI,11, 1)
         c_ProF = SUBSTR(c_UbiF,11, 1)

         *> Activamos el filtro.-----------------------------------------
         Select F10c
         c_filsec = ".AND. f10ccanpal = 0 .and. f10cestgen<>'O' "
         
         SET FILTER TO ((f10cresikg-f10cocupkg)>=n_peso).AND.;
                       BETW(SUBS(f10ccodUbi,3,2),ZI,ZF).AND.;
                       BETW(SUBS(f10ccodUbi,5,2),CI,CF).AND.;
                       BETW(SUBS(f10ccodUbi,7,2),FI,FF).AND.;
                       BETW(SUBS(f10ccodUbi,9,2),PI,PF).AND.;
                       BETW(SUBS(f10ccodUbi,11,1),RI,RF).AND.;
                       (f10ctippro==c_TipPr.OR.F10ctippro=='    ').AND.;
                       (f10ctamubi>=c_tamanyo).AND.(f10ctipalm="P");
                       &c_filsec&c_filter
   
         GOTO TOP

         IF EOF()
            c_retorna = ""
         ELSE
            c_retorna = f10ccodUbi
         ENDIF

*         IF SEEK(c_articulo)
*            c_retorna = f16ccodubi
*         ENDIF

         *> Si no encontrado, forzamos zona y buscar en ubicación de igual tamañoo.
         If Empty(c_Retorna)
            *> Zona.-----------------------------------------------------
            ZI = c_ZonI
            ZF = c_ZonF
            *> Calle.----------------------------------------------------
            CI = c_CalI
            CF = c_CalF
            *> Fila.-----------------------------------------------------
            FI = c_FilI
            FF = c_FilF
            *> Piso.-----------------------------------------------------
            PI = c_PisI
            PF = c_PisF
            *> Profundidad.----------------------------------------------
            RI = c_ProI
            RF = c_ProF
            c_filsec = ".AND. f10ccanpal = 0 .and. f10cestgen<>'O' "
         
            SET FILTER TO ((f10cresikg-f10cocupkg)>=n_peso).AND.;
                       BETW(SUBS(f10ccodUbi,5,2),ZI,ZF).AND.;
                       BETW(SUBS(f10ccodUbi,7,2),CI,CF).AND.;
                       BETW(SUBS(f10ccodUbi,9,3),FI,FF).AND.;
                       BETW(SUBS(f10ccodUbi,12,2),PI,PF).AND.;
                       BETW(SUBS(f10ccodUbi,14,1),RI,RF).AND.;
                       (f10ctippro==c_TipPr.OR.F10ctippro=='    ').AND.;
                       (f10ctamubi==c_tamanyo).AND.(f10ctipalm="P");
                       &c_filsec&c_filter
   
            GOTO TOP
            IF EOF()
               c_retorna = ""
            ELSE
               c_retorna = f10ccodUbi
            ENDIF

            *> Si no encontrado, forzar zona y buscar en ubicación mayor.
            If Empty(C_Retorna)
               c_filsec = ".AND. f10ccanpal = 0 .and. f10cestgen<>'O' "
         
               SET FILTER TO ((f10cresikg-f10cocupkg)>=n_peso).AND.;
                       BETW(SUBS(f10ccodUbi,3,2),ZI,ZF).AND.;
                       BETW(SUBS(f10ccodUbi,5,2),CI,CF).AND.;
                       BETW(SUBS(f10ccodUbi,7,2),FI,FF).AND.;
                       BETW(SUBS(f10ccodUbi,9,2),PI,PF).AND.;
                       BETW(SUBS(f10ccodUbi,11,1),RI,RF).AND.;
                       (f10ctippro==c_TipPr.OR.F10ctippro=='    ').AND.;
                       (f10ctamubi>=c_tamanyo).AND.(f10ctipalm="P");
                       &c_filsec&c_filter
   
               GOTO TOP
               IF EOF()
                  c_retorna = ""
               ELSE
                  c_retorna = f10ccodUbi
               ENDIF
            EndIf
         EndIf
      ENDIF
   ELSE
      c_retorna = f16ccodubi   
   ENDIF      

   *> Cancelamos de todo.---------------------------------------------
   SELE F16c
   SET RELATION TO
   SET FILTER TO

   *> Cancelamos ubicaciones.-----------------------------------------   
   SELE F10c
   SET FILTER TO
   SET ORDER TO Tag Codigo

   *> Ocupamos.-------------------------------------------------------
   IF !EMPTY(c_retorna)
      *> Actualizamos el fichero de ubicaciones.----------------------
      l_Ubicado=.f.
      n_Npal=1
      Do CargaUbi in Ora_Ca00 with c_Retorna, n_Peso, n_Volu, n_NPal, l_Ubicado
      if !L_Ubicado
         c_Retorna=""
      Endif   
   ENDIF
   
   *> Fin del mensaje de espera.--------------------------------------
   wait clear

   *> Restauramos el entorno.-----------------------------------------
   SELE (f_anterior)
RETURN

*>=============================================================================
*> Prc_Ubi .................. Proporciona ubicación destino a entrada material
*>                            según parámetros de ubicación.
*>
*> aubparmz[01] = Artículo (no utilizado)
*> aubparmz[02] = Tipo de ubicación
*> aubparmz[03] = Ubicar en picking
*> aubparmz[04] = Criterio de ubicación
*> aubparmz[05] = Coeficiente de rotación
*> aubparmz[06] = Forzar tamaño
*> aubparmz[07] = Forzar zona
*> aubparmz[08] = Forzar ubicación
*> aubparmz[09] = Confirmación automática
*> aubparmz[10] = Desglose automático
*> aubparmz[11] = Imprimir etiqueta bulto
*> aubparmz[12] = Imprimir etiqueta palet
*>
*>=============================================================================
Procedure Prc_Ubi
*Parameter c_CodPro, ;
*          c_CodArt, ;
*          c_TamUbi, ;
*          n_Cantid, ;
*          c_Ubifor, ;
*          c_NumPal, ;
*          c_PicoSN, ;
*          b_LAutom, ;
*          b_BlqUbi, ;
*          c_CodUbi, ;
*          c_TxtErr, ;
*          f_Fecha
Parameter c_CodPro, ;
          c_CodArt, ;
          c_TamUbi, ;
          n_Cantid, ;
          c_Ubifor, ;
          c_NumPal, ;
          c_PicoSN, ;
          b_LAutom, ;
          b_BlqUbi, ;
          c_CodUbi, ;
          c_TxtErr, ;
          c_NumLot

   c_CodUbi = ""
   c_TxtErr = "" 



*> Para buscar en la 'BRECHA' si el parametro lo permite.---------
   If _UbicaMolada = 'S' .And. Empty(c_UbiFor) 

      UbiFnc = CreateObject("UbiFncMol")        && Procedimientos.

      With UbiFnc
         .Inicializar                            && Poner a blancos.

         *> Asignar parámetros para poder ubicar.----------------
         *> Necesitariamos el Tono-Calibre-Lote(TCL).------------
         .UbiCodPro = C_CodPro                   && Propietario.
         .UbiCodArt = c_CodArt                   && Código de Artículo.
         .UbiCanFis = n_Cantid                   && Cantidad a Ubicar.
         .UbiNumPal = C_NumPal                   && Número de Palet.
         .UbiNumLot = c_NumLot					 && Número de TCL.

      EndWith

      *> Ubicar el Palet.----------------------------------------
      UbiFnc.UbiMol

      *> Forzariamos la ubicación.-------------------------------
      If !Empty(UbiFnc.UbiCodUbi)
		 C_UbiFor = UbiFnc.UbiCodUbi
      EndIf

      *> Eliminar el Objeto creado.------------------------
      Release UbiFnc

   EndIf

   *> Si se fuerza la ubicación.--------------------------------------
   If !Empty(c_UbiFor)
      L_Actu = .T.
      L_Ubic = .F.
      Do Prc_For With c_UbiFor, c_CodPro, c_CodArt, c_TamUbi, n_Cantid, l_Actu, l_Ubic
      If !l_Ubic
         c_TxtErr = "En ubicación forzada " + c_UbiFor
      Else
         c_CodUbi = c_UbiFor
      EndIf
   Else
      *> Ubicar primero en picking.---------------------------------
      *> 'N' -----> Ubicar primero caoticamente.
      *> 'S' -----> Ubicar primero en picking.
      If aubparmz[3] = 'S'
         *> Buscar ubicaciones de picking.
*         Do Prc_Pic With c_CodPro, ;
*                         c_CodArt, ;
*                         n_Cantid, ;
*                         c_PicoSN, ;
*                         b_BlqUbi, ;
*                         b_LAutom, ;
*                         c_CodUbi
   
         If Empty(c_CodUbi)
            *> Buscar ubicaciones de picking asignadas al producto.
            Do Prc_PicArt With c_CodPro, ;
                               c_CodArt, ;
                               n_Cantid, ;
                               c_PicoSN, ;
                               c_CodUbi
         EndIf

         If Empty(c_CodUbi)
            *> Buscar ubicaciones de picking rangos.
            Do Prc_PicRng With c_CodPro, ;
                               c_CodArt, ;
                               n_Cantid, ;
                               c_PicoSN, ;
                               c_CodUbi
         EndIf

         If Empty(c_CodUbi)
            *> Buscar ubicaciones de picking generales.
            Do Prc_PicGen With c_CodPro, ;
                               c_CodArt, ;
                               n_Cantid, ;
                               c_PicoSN, ;
                               c_CodUbi
         EndIf
      EndIf    && If aubparzm[3] = 'S'

      If Empty(c_CodUbi)
         *> Si no se puede, ubicar por proximidad a picking.--------
         *> 'C' ----> Ubicar caoticamente.
         *> 'P' ----> Ubicar por proximidad a picking.
         If (aubparmz[4] = 'P')
            Do Prc_PxPi With c_CodPro, ;
                             c_CodArt, ;
                             c_TamUbi, ;
                             n_Cantid, ;
                             b_BlqUbi, ;
                             b_lAutom, ;
                             c_CodUbi
         EndIf

         If Empty(c_CodUbi)           
            *> Si no se puede, ubicar en compacto.-----------------
*            Do Prc_Com With c_CodPro, ;
*                            c_CodArt, ;
*                            f_Fecha, ;
*                            c_TamUbi, ;
*                            n_Cantid, ;
*                            c_CodUbi, .T.

            If Empty(c_CodUbi)
               *> Localizamos peso y volumen del artículo
               Store 0 to n_Peso, n_Volu
               Do PesVolAr In Ora_Ca00 With c_CodPro, ;
                                            c_CodArt, ;
                                            n_Cantid, ;
                                            n_Peso, ;
                                            n_Volu

*>  Standard.--------------------------------
*              *> Criterio de ubicación caótico.-------------------
*              c_PicCaot = "N"    && Ubicar en palet.
*              Do Prc_Alm With c_CodPro, ;
*                              c_CodArt, ;
*                              c_TamUbi, ;
*                              n_Cantid, ;
*                              b_BlqUbi, ;
*                              b_LAutom, ;
*                              c_CodUbi, ;
*                              c_PicCaot, ;
*                              n_Peso, ;
*                              n_Volu


               *> Criterio de ubicación caótico por tipo de producto.
               c_PicCaot = c_PicoSN
               Do Prc_AlmTipPro With c_CodPro, ;
                                     c_CodArt, ;
                                     n_Cantid, ;
                                     c_CodUbi, ;
                                     c_PicCaot, ;
                                     n_Peso, ;
                                     n_Volu

               If Empty(c_CodUbi)
                  c_PicCaot = Iif(c_PicCaot=='N', 'S', 'N')
                  Do Prc_AlmTipPro With c_CodPro, ;
                                        c_CodArt, ;
                                        n_Cantid, ;
                                        c_CodUbi, ;
                                        c_PicCaot, ;
                                        n_Peso, ;
                                        n_Volu
               EndIf

*               If Empty(c_CodUbi)
*                  c_PicCaot = "S"    && Ubicar en picking.
*                  Do Prc_Alm With c_CodPro, ;
*                                  c_CodArt, ;
*                                  c_TamUbi, ;
*                                  n_Cantid, ;
*                                  b_BlqUbi, ;
*                                  b_LAutom, ;
*                                  c_CodUbi, ;
*                                  c_PicCaot, ;
*                                  n_Peso, ;
*                                  n_Volu
*               EndIf

               *> Busco en compacto sin fecha.------------------
               If Empty(c_CodUbi)
*                  Do Prc_Com With c_CodPro, c_CodArt, f_Fecha, c_TamUbi, n_Cantid, c_CodUbi, .F.
               EndIf
            EndIf

            *> En el caso de que no se pueda ubicar.------------------
            If Empty(c_CodUbi)
               c_TxtErr = "No hay ubicaciones libres en el almacén"
            EndIf
         EndIf
      EndIf
   EndIf
Return

*>--------------------------------------------------------------
*>=============================================================================
*> Prc_Ubi_Pal .................. Proporciona ubicación destino a entrada material
*>                            según parámetros de ubicación.
*>
*> aubparmz[01] = Artículo (no utilizado)
*> aubparmz[02] = Tipo de ubicación
*> aubparmz[03] = Ubicar en picking
*> aubparmz[04] = Criterio de ubicación
*> aubparmz[05] = Coeficiente de rotación
*> aubparmz[06] = Forzar tamaño
*> aubparmz[07] = Forzar zona
*> aubparmz[08] = Forzar ubicación
*> aubparmz[09] = Confirmación automática
*> aubparmz[10] = Desglose automático
*> aubparmz[11] = Imprimir etiqueta bulto
*> aubparmz[12] = Imprimir etiqueta palet
*>
*>=============================================================================
Procedure Prc_Ubi_Pal
Parameter c_CodPro, ;
          c_CodArt, ;
          c_TamUbi, ;
          n_Cantid, ;
          c_Ubifor, ;
          c_NumPal, ;
          c_PicoSN, ;
          b_LAutom, ;
          b_BlqUbi, ;
          c_CodUbi, ;
          c_TxtErr, ;
          f_Fecha

   c_CodUbi = ""
   c_TxtErr = "" 

   *> Si se fuerza la ubicación.--------------------------------------
   If !Empty(c_UbiFor)
      L_Actu = .T.
      L_Ubic = .F.
      Do Prc_For With c_UbiFor, c_CodPro, c_CodArt, c_TamUbi, n_Cantid, l_Actu, l_Ubic
      If !l_Ubic
         c_TxtErr = "En ubicación forzada " + c_UbiFor
      Else
         c_CodUbi = c_UbiFor
      EndIf
   Else
      *> Ubicar primero en picking.---------------------------------
      *> 'N' -----> Ubicar primero caoticamente.
      *> 'S' -----> Ubicar primero en picking.
      If aubparmz[3] = 'S'

         *> Buscar ubicaciones de picking.
*         Do Prc_Pic With c_CodPro, ;
*                         c_CodArt, ;
*                         n_Cantid, ;
*                         c_PicoSN, ;
*                         b_BlqUbi, ;
*                         b_LAutom, ;
*                         c_CodUbi
   
         If Empty(c_CodUbi)
            *> Buscar ubicaciones de picking asignadas al producto.
            Do Prc_PicArt With c_CodPro, ;
                               c_CodArt, ;
                               n_Cantid, ;
                               c_PicoSN, ;
                               c_CodUbi
         EndIf

         If Empty(c_CodUbi)
            *> Buscar ubicaciones de picking rangos.
            Do Prc_PicRng With c_CodPro, ;
                               c_CodArt, ;
                               n_Cantid, ;
                               c_PicoSN, ;
                               c_CodUbi
         EndIf

         If Empty(c_CodUbi)
            *> Buscar ubicaciones de picking generales.
            Do Prc_PicGen With c_CodPro, ;
                               c_CodArt, ;
                               n_Cantid, ;
                               c_PicoSN, ;
                               c_CodUbi
         EndIf
      EndIf    && If aubparzm[3] = 'S'

      If Empty(c_CodUbi)
         *> Si no se puede, ubicar por proximidad a picking.--------
         *> 'C' ----> Ubicar caoticamente.
         *> 'P' ----> Ubicar por proximidad a picking.
         If (aubparmz[4] = 'P')
            Do Prc_PxPi With c_CodPro, ;
                             c_CodArt, ;
                             c_TamUbi, ;
                             n_Cantid, ;
                             b_BlqUbi, ;
                             b_lAutom, ;
                             c_CodUbi
         EndIf

         If Empty(c_CodUbi)           
            *> Si no se puede, ubicar en compacto.-----------------
*            Do Prc_Com With c_CodPro, ;
*                            c_CodArt, ;
*                            f_Fecha, ;
*                            c_TamUbi, ;
*                            n_Cantid, ;
*                            c_CodUbi, .T.

            If Empty(c_CodUbi)
               *> Localizamos peso y volumen del artículo
               Store 0 to n_Peso, n_Volu
   *            Do PesVolAr In Ora_Ca00 With c_CodPro, ;
   *                                         c_CodArt, ;
   *                                         n_Cantid, ;
   *                                         n_Peso, ;
   *                                         n_Volu

*>  Standard.--------------------------------
*              *> Criterio de ubicación caótico.-------------------
              c_PicCaot = "N"    && Ubicar en palet.
              Do Prc_Alm With c_CodPro, ;
                              c_CodArt, ;
                              c_TamUbi, ;
                              n_Cantid, ;
                              b_BlqUbi, ;
                              b_LAutom, ;
                              c_CodUbi, ;
                             c_PicCaot, ;
                              n_Peso, ;
                              n_Volu

               *> Criterio de ubicación caótico por tipo de producto.
*               c_PicCaot = c_PicoSN
*               Do Prc_Alm_Pal With c_CodPro, ;
*                                     c_NumPal, ;
*                                     c_TamUbi, ;
*                                     n_Cantid, ;
*                                     b_BlqUbi, ;
*                                     c_CodUbi, ;
*                                     c_PicCaot, ;
*                                     n_Peso, ;
*                                     n_Volu

               If Empty(c_CodUbi)
*                  c_PicCaot = Iif(c_PicCaot=='N', 'S', 'N')
*                  Do Prc_Alm_Pal With c_CodPro, ;
*                                        c_NumPal, ;
*                                        c_TamUbi, ;
*                                        n_Cantid, ;
*                                        b_BlqUbi, ;
*                                        c_CodUbi, ;
*                                        c_PicCaot, ;
*                                        n_Peso, ;
*                                        n_Volu
               EndIf

               If Empty(c_CodUbi)
                  c_PicCaot = "S"    && Ubicar en picking.
                  Do Prc_Alm With c_CodPro, ;
                                  c_CodArt, ;
                                  c_TamUbi, ;
                                  n_Cantid, ;
                                  b_BlqUbi, ;
                                  b_LAutom, ;
                                  c_CodUbi, ;
                                  c_PicCaot, ;
                                  n_Peso, ;
                                  n_Volu
               EndIf

               *> Busco en compacto sin fecha.------------------
               If Empty(c_CodUbi)
*                  Do Prc_Com With c_CodPro, c_CodArt, f_Fecha, c_TamUbi, n_Cantid, c_CodUbi, .F.
               EndIf
            EndIf

            *> En el caso de que no se pueda ubicar.------------------
            If Empty(c_CodUbi)
               c_TxtErr = "No hay ubicaciones libres en el almacén"
            EndIf
         EndIf
      EndIf
   EndIf
Return

*>=============================================================================
*> Prc_For .................. Proceso de ubicación forzada.
*>=============================================================================

Procedure Prc_For
Parameters c_Ubica, c_Propie, c_articulo, c_tamanyo, n_canti, l_Actuali, L_Ubicado

PRIVATE f_anterior
PRIVATE c_tipo_al, c_TipPr, n_peso, n_volu, c_texto, l_openf08, l_openf10

*> Proceso de ubicación forzada.
   EXTERNAL ARRAY aubparmz()

   *> Guardamos el entorno.-------------------------------------------
   f_anterior = SELECT()

   *> Variable que indicará si se consiguió la ubicación.-------------
   l_Ubicado = .f.

   If _Lenguaje <> 'VB'
	   *> Mensaje.--------------------------------------------------------
	   Wait Window "Forzando ubicación en " + c_Ubica NoWait
   EndIf

   *> Abrimos el fichero de artículos.--------------------------------
   m.F08cCodPro = c_Propie
   m.F08cCodArt = c_Articulo
   Store '' To c_TipAlm, c_TipPro
   Store 0  To n_PesUni, n_VolUni

   f_Replace = "c_TipAlm = F08cTipAlm, c_TipPro = F08cTipPro"
   =F3_Seek('F08c', , , f_Replace)

   Store 0 to n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With c_Propie, c_Articulo, n_Canti, n_Peso, n_Volu
*   n_Peso = Round(n_Peso, 0)
*   n_Volu = Round(n_Volu, 0)

   *> Abrimos el fichero de ubicaciones.------------------------------
   =CrtCursor('F10c', 'PForF10c')
   lxWhere = "F10cCodUbi='" + c_Ubica + "'"
   =F3_SQL('*', 'F10c', lxWhere, , , 'PForF10c')
   Select PForF10c
   Go Top
   If EOF()
      Use In PForF10c
      l_Ubicado = .F.
      Return
   Else
      l_Ubicado = .T.
   Endif   

   *> Bloquear el registro de ubicación-------------------------------
*   If !FChkLk("UBICACION : " + c_Ubica + " en uso por otro usuario")
*      l_Ubicado=.f.
*      Return
*   EndIf

   *> Inicializamos la variable.--------------------------------------
   Select PForF10c
   c_texto = ""

   *> Controlamos el tipo de almacenamiento.--------------------------
   If c_TipAlm <> F10cTipAlm
      c_texto = "El tipo de almacenamiento no coincide." + cr
   EndIf

   *> Controlamos el tamaño de la ubicación.--------------------------
   If c_tamanyo <> f10ctamubi
      c_texto = c_texto + "El tamaño de la ubicación no coincide." + cr
   EndIf

   *> Controlamos el tipo de producto.--------------------------------
   If (c_TipPro <> f10ctippro) .AND. (f10ctippro <> " ")
      c_texto = c_texto + "El tipo de producto no coincide." + cr
   EndIf

   *> Controlamos el peso.--------------------------------------------
   If (n_peso+f10cpesocu) > f10cpestot
      c_texto = c_texto + "El peso excede la resistencia." + cr
   EndIf

   *> Controlamos el volumen.-----------------------------------------
   If (f10cVolLib-n_volu) < 0
      c_texto = c_texto + "No se tiene suficiente volumen libre." + cr
   EndIf

   *> Controlamos el estado.------------------------------------------
   If F10cNumOcu >= F10cMaxPal 
      c_texto = c_texto + "El número de palets está excedido." + cr
   EndIf

   *> Controlamos si se produjeron errores.---------------------------
   c_texto = ""
   If !Empty(c_Texto) .And. _Lenguaje <> 'VB'
      If !f3_sn(2, 3, c_Texto, , "Desea continuar pese a las inconsistencias?")
         L_Ubicado = .F.
      EndIf
   EndIf

   If l_Actuali .and. l_Ubicado
      *> Actualizamos el fichero de ubicaciones.----------------------
      n_Npal = 1
      Do CargaUbi In Ora_Ca00 With c_Ubica, ;
                                   n_Peso, ;
                                   n_Volu, ;
                                   n_NPal, ;
                                   l_Ubicado
   EndIf

   *> Desbloqueamos el fichero de ubicaciones.------------------------
*  SELE f10c
*  UNLOCK

   *> Seleccionamos el fichero anterior.------------------------------
   Use In PForF10c
   Select (f_Anterior)
Return

*>================================================================================
*> Prc_Pic .................. Proceso de ubicación en ubic.picking del artículo.
*>================================================================================

Procedure Prc_Pic
Parameters c_propie, c_articul, n_cantida, EsPico, l_actuali, l_automat, c_Ubica
Private f_anterior, n_peso, n_volu, n_PosRec, lxWhere, f_from, l_order

   *> Guardamos el fichero anteriormente abierto.---------------------
   f_anterior = Select()

   If _Lenguaje <> 'VB'
	   *> Mensaje.--------------------------------------------------------
	   Wait Window "Intentando ubicar en picking ..." NoWait
   EndIf

   *> Si no es pico, no ubica en picking.-----------------------------
   If EsPico # 'S' .And. EsPico # 'U'
      Select (f_Anterior)
      Wait Clear
      Return c_Ubica
   EndIf

   *> Variable que retorna la ubicación.------------------------------
   c_ubica = ""
   n_MaxPick = 0

   *> Buscamos el artículo para saber si tiene picking.---------------
   lxWhere = "F12cCodPro = '" + c_Propie + "' And F12cCodArt = '" + c_Articul + ;
       "' And F12cCodAlm = '" + _Alma + "'"
   f_Replace = "n_MaxPick = F12cCanMax, c_Ubica = F12cCodUbi"

   l_Enc = F3_SeekTun('F12c', lxWhere, , f_Replace)
   If l_Enc = .F.
      Select (f_Anterior)
      *> Cerrar ventana.----------------------------------------------
      Wait Clear
      Return c_Ubica
   EndIf

   *> Si se sobrepasa máximo, salir y hacer Prc_Alm caótico.----------
   *> Calcular total del artículo en picking.-------------------------
   n_TotPick = 0

   *> Buscar cantidad en picking en MPs de entrada.
   =CrtCursor('F14c', 'PPicF14c')
   lxWhere = "F14cCodPro='" + c_Propie + "' And " + ;
             "F14cCodArt='" + c_Articul + "' And " + ;
             "And F14cCodAlm='" + _Alma + "'"

   =f3_sql('*', 'F14c', lxWhere, , , 'PPicF14c')
   
   Select PPicF14c
   Go Top
   Do While !EOF()
      *> Buscar si la ubicación destino es de picking.----------------
      w_PickSN = ""
      m.F10cCodUbi = F14cUbiOri
      l_Enc = F3_Seek('F10c', , 'w_PickSN = F10cPickSN')
      If l_Enc = .T. .And. w_PickSN = "S"
         n_TotPick = n_TotPick + PPicF14c.F14cCanFis
      EndIf

      Select PPicF14c
      Skip
   EndDo

   *> Buscar cantidad en picking en ocupaciones.----------------------
   =CrtCursor('F16c', 'PPicF16c')
   lxWhere = "F16cCodPro='" + c_Propie + "' And F16cCodArt='" + c_Articul + "'"
   =f3_sql('*', 'F16c', lxWhere, , , 'PPicF16c')

   Select PPicF16c
   Go Top
   Do While !EOF()
      *> Buscar si la ubicación destino es de picking.----------------
      w_PickSN = ""
      m.F10cCodUbi = F16cCodUbi
      l_Enc = F3_Seek('F10c', F16cCodUbi, , 'w_PickSN = F10cPickSN')
      If l_Enc = .T. .And. w_PickSN = "S"
         n_TotPick = n_TotPick + PPicF16c.F16cCanFis
      EndIf
      
      Select PPicF16c
      Skip
   EndDo

   If n_TotPick + n_Cantida > n_MaxPick
      Sele (F_Anterior)
      Wait Clear
      c_Ubica = ""
      Use In PPicF14c
      Use In PPicF16c
      Return c_Ubica
   EndIf

   *> Localizamos peso y volumen del articulo.------------------------
   Store 0 To n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With c_Propie, c_Articul, n_Cantida, n_Peso, n_Volu

   If Empty(c_Ubica)
      *> Criterio de ubicación caótico.-------------------------------
      c_PicCaot = "S"
      SwUbic = .T.
      Do Prc_Alm with c_CodPro, ;
                      c_articul, ;
                      c_tamUbi, ;
                      n_cantida, ;
                      l_Actuali, ;
                      L_Automat, ;
                      c_ubica, ;
                      c_PicCaot, ;
                      n_peso, ;
                      n_volu
      Wait Clear
      Use In PPicF10c
      Use In PPicF14c
      Use In PPicF16c
      Return c_Ubica
   EndIf

   *> Buscamos la ubicación.------------------------------------------
   =CrtCursor('F10c', 'PPicF10c')
   lxWhere = "F10cCodUbi = '" + c_Ubica + "'"
   l_Enc = F3_Sql('*', 'F10c', lxWhere, , , 'PPicF10c')
   If l_Enc = .F.
      c_Ubica = ""
      Wait Clear
      Use In PPicF10c
      Use In PPicF14c
      Use In PPicF16c
      Return
   EndIf

   *> Bloquear la ubicación .-----------------------------------------
   *> No usamos mensaje de bloqueo porque se llamará desde radiofrecuencia
   Sebloquea=.f.

   *> Inicializamos variables.----------------------------------------
   n_Encontr = 0

   lxWhere = "F14cUbiOri='" + c_Ubica + "' And F14cCodPro='" + c_Propie + ;
        "' And F14cCodArt='" + c_Articul + "' And " + _GCSS("F14cCodAlm", 1, 1) + "='1'"
   lxOrder = "F14cUbiOri, F14cCodPro, F14cCodArt"
   =F3_SQL('*', 'F14c', lxWhere, lxOrder, , 'PPicF14c')

   Select PPicF14c
   Go Top

   *> Efectuamos bucle sobre fichero.---------------------------------
   Do While !EOF()
      n_Encontr = n_Encontr + (F14cCanFis)      
      Skip
   EndDo

   *> Indexamos fichero f16c.-----------------------------------------
   lxWhere = "F16cCodUbi='" + c_Ubica + "' And F16cCodPro='" + c_Propie + ;
        "' And F16cCodArt='" + c_Articul + "'"
   lxOrder = "F16cCodUbi, F16cCodPro, F16cCodArt"
   =F3_SQL('*', 'F16c', lxWhere, lxOrder, , 'PPicF16c')

   *> Buscamos la cantidad para acumular.-----------------------------
   Sele PPicF16c
   Go Top

   *> Efectuamos bucle sobre fichero.----------------------------------
   Do While !EOF()
      n_Encontr = n_Encontr + (F16cCanFis-F16cCanRes)      
      Skip
   EndDo   

   *> Ver si no se sobrepasa limite de cant. de la ubic. en picking.--
   n_MaxPick = 0
   lxWhere = "F12cCodUbi = '" + c_Ubica + "'"
   =F3_SeekTun('F12c', lxWhere, , 'n_MaxPick = F12cCanMax')
   If n_Cantida + n_Encontr > n_MaxPick
      *> Retornamos vacio porque esta llena---------------------------
      C_Ubica=""
      Wait Clear
      Use In PPicF10c
      Use In PPicF14c
      Use In PPicF16c
      Return
   EndIf

   *> Ver si cabe en picking la cantidad
   Select PPicF10c

   *>Si es de picking pasamos de volúmenes e historias.-11/07/95.-------
   IF f10cpicksn = "N"
      IF f10cestgen = "O"
         if !((F10cespLib>=n_Volu) .and. (f10cresikg>=(f10cocupKg+n_Peso)) .and. EsPico=="S") 
            *> Si a pesar de que dice que no cabe mas tenemos prou volumen
            *> y resiste el peso y es pico Podremos ubicar
            *> En caso contrario, retornar vacío porque está llena.
            C_Ubica=""
            Wait Clear
            Use In PPicF10c
            Use In PPicF14c
            Use In PPicF16c
            RETURN
         Endif  
      Endif   
   ENDIF

   IF l_Actuali
      *> Actualizamos el fichero de ubicaciones.-------------------
      l_Ubicado=.f.
      n_NPal=1
      Do CargaUbi in Ora_Ca00 with c_Ubica, n_Peso, n_Volu, n_NPal, l_Ubicado
      if !L_Ubicado
         c_Ubica=""
      endif
   ENDIF
           
   *> Cerramos los fichero.-------------------------------------------
   Use In PPicF10c
   Use In PPicF14c
   Use In PPicF16c
   
   *> Cerrar ventana.-------------------------------------------------
   WAIT CLEAR
   SELE (f_anterior)
RETURN c_ubica

*>================================================================================
*> Prc_PicArt ............... Proceso de ubicación en ubic.picking del artículo
*> Busca en las ubicaciones de picking asignadas al producto (F12c).
*>================================================================================

Procedure Prc_PicArt
Parameters c_propie, ;
           c_articul, ;
           n_cantida, ;
           EsPico, ;
           c_Ubica

Private f_anterior, n_peso, n_volu, n_PosRec
Private lxWhere, f_from, l_order

   *> Guardamos el alias de la tabla activa.
   f_anterior = Select()

   If _Lenguaje <> 'VB'
		Wait Window "Intentando ubicar en picking asignado a producto ..." NoWait
   EndIf

   *> Si no es pico, no ubica en picking.
   If EsPico # 'S' .And. EsPico # 'U'
      If !Empty(f_Anterior)
         Select (f_Anterior)
      EndIf

      Wait Clear
      Return
   EndIf

   *> Inicializar valores.
   c_ubica = ""
   n_MaxPick = 0

   *> Buscamos las ubicaciones de picking de este producto.
   lxWhere = "F12cCodPro='" + c_Propie + "' And " + ;
             "F12cCodArt='" + c_Articul + "' And " + ;
             "F12cCodAlm='" + _Alma + "' And " + ;
             "F10cCodUbi=F12cCodUbi And F10cPickSn='" + EsPico + "'"

   f_from = 'F12c,F10c'
   l_order = "F12cCodPro, F12cCodArt, F12cPriori"

   l_enc = f3_sql('*', f_from, lxWhere, l_order, , 'PPicF12c')

   Select PPicF12c
   Go Top
   Do While !Eof()
      *> Acumular la cantidad en MPs de entrada, la ya existente en ocupaciones
      *> y la cantidad pre-ubicada durante el proceso.
      n_TotPick = CanPickingMP(c_propie, c_articul, PPicF12c.F12cCodUbi) + ;
                  CanPickingOC(c_propie, c_articul, PPicF12c.F12cCodUbi) + ;
                  CanPickingTMP(c_propie, c_articul, PPicF12c.F12cCodUbi)

      *> Si cabe y no sobrepasamos el máximo, devolver la ubicación.
      If n_TotPick < PPicF12c.F12cCanMax
         If (n_TotPick + n_cantida) <= PPicF12c.F12cCanMax
            c_ubica = PPicF12c.F12cCodUbi
            Exit
         EndIf
      EndIf

      *> Intentarlo con otra ubicación.
      Select PPicF12c
      Skip
   EndDo

   *> Calcular peso y volumen de la cantidad a ubicar.
   If !Empty(c_ubica)
      Store 0 to n_TotPes, n_TotVol
      Do PesVolAr In Ora_Ca00 With c_propie, ;
                                   c_articul, ;
                                   n_cantida, ;
                                   n_TotPes, ;
                                   n_TotVol

      *> Actualizar el estado de la ubicación seleccionada.
      l_Ubicado = .F.
      n_NPal = 1
      Do CargaUbi In Ora_Ca00 With c_Ubica, n_TotPes, n_TotVol, n_NPal, l_Ubicado

      *> Memorizar cantidad ubicada temporalmente.
      =UpdateCanPickingTMP(c_propie, c_articul, PPicF12c.F12cCodUbi, n_cantida)
   EndIf
           
   *> Recuperar entorno de trabajo anterior.
   Use In PPicF12c
   If !Empty(f_anterior)
      Select (f_anterior)
   EndIf

   Wait Clear

Return c_ubica

*>================================================================================
*> Prc_PicRng ............... Proceso de ubicación en ubic.picking del artículo
*> Busca en las ubicaciones de picking dentro del rango de ubicaciones asignadas
*> a este producto (F08u).
*>================================================================================

Procedure Prc_PicRng
Parameters c_propie, ;
           c_articul, ;
           n_cantida, ;
           EsPico, ;
           c_Ubica

Private f_anterior, n_pestot, n_voltot
Private lxWhere, f_from, l_order

   *> Guardamos el alias de la tabla activa.
   f_anterior = Select()

   If _Lenguaje <> 'VB'
	 Wait Window "Intentando ubicar en picking rango ubicaciones ..." NoWait
   EndIf	

   *> Si no es pico, no ubica en picking.
   If EsPico # 'S' .And. EsPico # 'U'
      If !Empty(f_Anterior)
         Select (f_Anterior)
      EndIf

      Wait Clear
      Return
   EndIf

   *> Inicializar valores.
   c_ubica = ""
   n_MaxPick = 0

   *> Leer los datos de la ficha de artículo.
   m.F08cCodPro = c_propie
   m.F08cCodArt = c_articul
   =f3_seek('F08C')

   *> Buscamos las ubicaciones de picking dentro del rango de ubicaciones.
   *> Teoricamente, para cada artículo se pueden definir varios rangos.
   lxWhere = "F08uCodPro='" + c_Propie + "' And " + ;
             "F08uArtDsd<='" + c_Articul + "' And " + ;
             "F08uArtHst>='" + c_Articul + "'"

   f_from = 'F08u'
   l_order = "F08uCodPro, F08uArtDsd"

   l_enc = f3_sql('*', f_from, lxWhere, l_order, , 'PPicF08u')

   Select PPicF08u
   Go Top
   Do While !Eof()
      *> Cargar las ubicaciones de picking de cada rango seleccionado.
      lxWhere = "F10cCodUbi Between '" + PPicF08u.F08uUbiDsd + ;
                "' And '" + PPicF08u.F08uUbiHst + "' And " + ;
                "F10cPickSn='" + EsPico + "'"

      f_from = 'F10c'
      l_order = "F10cCodUbi"

      l_enc = f3_sql('*', f_from, lxWhere, l_order, , 'PPicF10c')

      Select PPicF10c
      Go Top
      Do While !Eof()
         *> Comprobar que esta ubicación NO esté asignada a otro producto.
         If UbiPicAsignada(c_propie, c_articul, PPicF10c.F10cCodUbi)
            Select PPicF10c
            Skip
            Loop
         EndIf

         *> Comprobar que esta ubicación NO esté ocupada por otro producto,
         *> Controlando multiproducto/multilote.
         If UbiPicOcupada(c_propie, c_articul, PPicF10c.F10cCodUbi)
            Select PPicF10c
            Skip
            Loop
         EndIf

         *> Comprobar que esta ubicación NO tenga MPs de entrada de otro producto,
         *> Controlando multiproducto/multilote.
         If UbiPicOcupadaMP(c_propie, c_articul, PPicF10c.F10cCodUbi)
            Select PPicF10c
            Skip
            Loop
         EndIf

         *> Acumular la cantidad en MPs de entrada, la ya existente en ocupaciones
         *> y la cantidad pre-ubicada durante el proceso.
         n_TotPick = CanPickingMP(c_propie, c_articul, PPicF10c.F10cCodUbi) + ;
                     CanPickingOC(c_propie, c_articul, PPicF10c.F10cCodUbi) + ;
                     CanPickingTMP(c_propie, c_articul, PPicF10c.F10cCodUbi)

         *> Si cabe y no sobrepasamos el máximo por palet, devolver la ubicación encontrada.
         If (n_TotPick + n_cantida) <= (F08c.F08cUniPac * F08c.F08cPacCaj * F08c.F08cCajPal)
            c_ubica = PPicF10c.F10cCodUbi
            Exit
         EndIf

         *> Intentarlo con otra ubicación.
         Select PPicF10c
         Skip
      EndDo

      *> Si ha encontrado ubicación, actualizar y volver.
      If !Empty(c_ubica)
         *> Calcular peso y volumen de la cantidad a ubicar.
         Store 0 to n_TotPes, n_TotVol
         Do PesVolAr In Ora_Ca00 With c_propie, ;
                                      c_articul, ;
                                      n_cantida, ;
                                      n_TotPes, ;
                                      n_TotVol

         *> Actualizar el estado de la ubicación seleccionada.
         l_Ubicado = .F.
         n_NPal = 1
         Do CargaUbi In Ora_Ca00 With c_Ubica, n_TotPes, n_TotVol, n_NPal, l_Ubicado

         *> Memorizar cantidad ubicada temporalmente.
         =UpdateCanPickingTMP(c_propie, c_articul, PPicF10c.F10cCodUbi, n_cantida)

         *> Abandonar bucle de rangos ubicaciones.
         Exit
      EndIf

      Use In PPicF10c

      *> Probar con el siguiente rango.      
      Select PPicF08u
      Skip
   EndDo

   *> Recuperar entorno de trabajo anterior.
   Use In PPicF08u
   If !Empty(f_anterior)
      Select (f_anterior)
   EndIf

   Wait Clear

Return c_ubica

*>================================================================================
*> Prc_PicGen ............... Proceso de ubicación en ubic.picking del artículo
*> Busca en las ubicaciones de picking dentro de todo el mapa (F10c).
*>================================================================================

Procedure Prc_PicGen
Parameters c_propie, ;
           c_articul, ;
           n_cantida, ;
           EsPico, ;
           c_Ubica

Private f_anterior, n_pestot, n_voltot
Private lxWhere, f_from, l_order

   *> Guardamos el alias de la tabla activa.
   f_anterior = Select()

   If _Lenguaje <> 'VB'
	Wait Window "Intentando ubicar en picking general ..." NoWait
   EndIf	
 
   *> Si no es pico, no ubica en picking.
   If EsPico # 'S' .And. EsPico # 'U'
      If !Empty(f_Anterior)
         Select (f_Anterior)
      EndIf

      Wait Clear
      Return
   EndIf

   *> Inicializar valores.
   c_ubica = ""
   n_MaxPick = 0

   *> Leer los datos de la ficha de artículo.
   m.F08cCodPro = c_propie
   m.F08cCodArt = c_articul
   =f3_seek('F08C')

   *> Buscamos las ubicaciones de picking dentro del mapa de almacén.
   lxWhere = "F10cPickSn='" + EsPico + "' And " + _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "'"
   f_from = 'F10c'
   l_order = "F10cCodUbi"

   l_enc = f3_sql('*', f_from, lxWhere, l_order, , 'PPicF10c')

   Select PPicF10c
   Go Top
   Do While !Eof()
      *> Comprobar que esta ubicación NO esté asignada a otro producto.
      If UbiPicAsignada(c_propie, c_articul, PPicF10c.F10cCodUbi)
         Select PPicF10c
         Skip
         Loop
      EndIf

      *> Comprobar que esta ubicación NO esté ocupada por otro producto,
      *> Controlando multiproducto/multilote.
      If UbiPicOcupada(c_propie, c_articul, PPicF10c.F10cCodUbi)
         Select PPicF10c
         Skip
         Loop
      EndIf

      *> Comprobar que esta ubicación NO tenga MPs de entrada de otro producto,
      *> Controlando multiproducto/multilote.
      If UbiPicOcupadaMP(c_propie, c_articul, PPicF10c.F10cCodUbi)
         Select PPicF10c
         Skip
         Loop
      EndIf

      *> Acumular la cantidad en MPs de entrada, la ya existente en ocupaciones
      *> y la cantidad pre-ubicada durante el proceso.
      n_TotPick = CanPickingMP(c_propie, c_articul, PPicF10c.F10cCodUbi) + ;
                  CanPickingOC(c_propie, c_articul, PPicF10c.F10cCodUbi) + ;
                  CanPickingTMP(c_propie, c_articul, PPicF10c.F10cCodUbi)

      *> Si cabe y no sobrepasamos el máximo por palet, devolver la ubicación encontrada.
      If (n_TotPick + n_cantida) <= (F08c.F08cUniPac * F08c.F08cPacCaj * F08c.F08cCajPal)
         c_ubica = PPicF10c.F10cCodUbi
         Exit
      EndIf

      *> Intentarlo con otra ubicación.
      Select PPicF10c
      Skip
   EndDo

   *> Si ha encontrado ubicación, actualizar y volver.
   If !Empty(c_ubica)
      *> Calcular peso y volumen de la cantidad a ubicar.
      Store 0 to n_TotPes, n_TotVol
      Do PesVolAr In Ora_Ca00 With c_propie, ;
                                   c_articul, ;
                                   n_cantida, ;
                                   n_TotPes, ;
                                   n_TotVol

      *> Actualizar el estado de la ubicación seleccionada.
      l_Ubicado = .F.
      n_NPal = 1
      Do CargaUbi In Ora_Ca00 With c_Ubica, n_TotPes, n_TotVol, n_NPal, l_Ubicado

      *> Memorizar cantidad ubicada temporalmente.
      =UpdateCanPickingTMP(c_propie, c_articul, PPicF10c.F10cCodUbi, n_cantida)
   EndIf

   *> Recuperar entorno de trabajo anterior.
   Use In PPicF10c
   If !Empty(f_anterior)
      Select (f_anterior)
   EndIf

   Wait Clear

Return c_ubica

*>================================================================================
*> Prc_PicPicos
*> Proceso de ubicación en picking donde ya hay cantidad.
*> Permite ubicar parcialmente la cantidad recibida.
*> Intenta ubicar de en el pico mayor.
*> Añadir lote, de cara a tratamiento acabados. AVC - 28.02.2002
*> Tratar solo con ocupaciónes con SSTK='1000'. AVC - 28.02.2002
*>
*> Parámetros: cCodPro (I) : Propietario.
*>             cCodArt (I) : Artículo.
*>             cNumLot (I) : Nº de Lote.
*>             nCantidadEntrada (I) : Cantidad a ubicar en picking.
*>             cParcial (I) : Ubicar parcialmente (S/N).
*>
*>             nCantidadUbicada (O) : Cantidad que ha podido ubicar.
*>             cUbicacion (O) : Ubicación destino.
*>             cPalet (O) : Palet destino.
*>
*> Devuelve: 'S' : OK, ha ubicado todo.
*>           'N' : No ha podido ubicar en picking.
*>           'P' : Ha ubicado parcialmente.
*>           'C' : Error.
*>================================================================================

Procedure Prc_PicPicos
Parameters cCodPro, cCodArt, cNumLot, nCantidadEntrada, cParcial, nCantidadUbicada, cUbicacion, cPalet

Private cWhere, cFromF, cField, cGroup, cOrder
Private oPRC, cAlmacen, nPeso, nVolumen
Local lEstado, cEstado, cOldSelect, lIsOpen, nPalets
Local nTotalPicking, nMaximo

   *> Inicializar valores
   Store 'N' To cEstado
   Store 0 To nCantidadUbicada
   Store '' To cUbicacion, cNumPal

   *> Guardamos el alias de la tabla activa.
   cOldSelect = Select()

   If Used("F16cPicos")
      Use In F16cPicos
   EndIf

   oPRC = CreateObject("procaot")

   *> Tomar datos de la ficha de artículos.
   lIsOpen = Used('F08c')
   If !lIsOpen
      =oPRC.OpenTabla('F08c')
   EndIf

   m.F08cCodPro = cCodPro
   m.F08cCodArt = cCodArt
   =f3_seek('F08c')

   *> Obtener las ocupaciones de picking donde esté este producto.
   cFromF = "F16c,F10c"
   cOrder = "F16cCodPro, F16cCodArt, F16cCanFis Desc"

   cWhere =          "F16cCodPro='" + cCodPro + "' And "
   cWhere = cWhere + "F16cCodArt='" + cCodArt + "' And "
   cWhere = cWhere + "F16cNumLot='" + cNumLot + "' And "
   cWhere = cWhere + _GCSS("F16cCodUbi", 1, 4) + "='" + _Alma + "' And "
   cWhere = cWhere + "F16cSitStk='1000' And "
   cWhere = cWhere + "F10cCodUbi=F16cCodUbi And F10cPickSN='S'"

   lEstado = f3_sql('*', cFromF, cWhere, cOrder, , "F16cPicos")

   Select F16cPicos
   Go Top
   Do While !Eof()
      *> Acumular la cantidad en MPs de entrada y la ya pre-ubicada durante el proceso.
      nTotalPicking = CanPickingMP(cCodPro, cCodArt, F16cPicos.F16cCodUbi) + ;
                      CanPickingTMP(cCodPro, cCodArt, F16cPicos.F16cCodUbi)

      *> Decidir la cantidad que realmente se puede ubicar.
      nMaximo = F08c.F08cUniPac * F08c.F08cPacCaj * F08c.F08cCajPal
      nCantidadUbicada = nMaximo - F16cPicos.F16cCanFis

      Do Case
         *> No cabe: Buscar otra.
         Case nCantidadUbicada <= 0
            Select F16cPicos
            Skip
            Loop

         *> Cabe todo: (Yuppppiiii!).
         Case nCantidadUbicada >= nCantidadEntrada
            cEstado = 'S'
            nCantidadUbicada = nCantidadEntrada
            cUbicacion = F16cPicos.F16cCodUbi
            cPalet = F16cPicos.F16cNumPal
            Exit

         *> Solo cabe una parte (Algo es algo).
         Case cParcial=='S' .And. nCantidadUbicada < nCantidadEntrada
            cEstado = 'P'
            cUbicacion = F16cPicos.F16cCodUbi
            cPalet = F16cPicos.F16cNumPal
            Exit

         *> No permite ubicación parcial (Mala suerte, chico).
         Case cParcial # 'S' .And. nCantidadUbicada < nCantidadEntrada
            Select F16cPicos
            Skip
            Loop

         *> Error: (No debería llegar aquí).
         Otherwise
            cEstado = 'C'
            Exit
      EndCase

      *> Intentarlo con otra ocupación.
      Select F16cPicos
      Skip
   EndDo

   *> Calcular peso y volumen de la cantidad a ubicar.
   If !Empty(cUbicacion)
      Store 0 To nPeso, nVolumen
      Do PesVolAr In Ora_Ca00 With cCodPro, cCodArt, nCantidadUbicada, nPeso, nVolumen

      *> Actualizar el estado de la ubicación seleccionada.
      lEstado = .F.
      nPalets = 0
      Do CargaUbi In Ora_Ca00 With cUbicacion, nPeso, nVolumen, nPalets, lEstado

      *> Memorizar cantidad ubicada temporalmente.
      =UpdateCanPickingTMP(cCodPro, cCodArt, cUbicacion, nCantidadUbicada)
   EndIf
           
   *> Recuperar entorno de trabajo anterior.
   Use In F16cPicos
   Release oPRC

   If !lIsOpen
      Use In F08c
   EndIf
   If !Empty(cOldSelect)
      Select (cOldSelect)
   EndIf

Return cEstado

*>==============================================================================
*> Prc_PxPi.................. Buscar ubicación por proximidad a picking.
*>==============================================================================

Procedure Prc_Pxpi
Parameters c_CodPro, c_Articulo, c_Tama, n_cantidad, l_actuali, l_automat, c_Ubica
Private f_Anterior, n_Peso, n_Volu

   If _Lenguaje <> 'VB'
	   Wait Window "Ubicando por proximidad a picking ..." NoWait
   EndIf	

   *> Guardamos el fichero anteriormente abierto.---------------------
   f_anterior = SELECT()
   c_Ubica = ""

   *> Buscamos el artículo para saber si tiene picking.---------------
   m.F12cCodPro = c_CodPro
   m.F12cCodArt = c_Articulo
   m.F12cCodAlm = _Alma
   f_Replace = "c_Ubica = F12cCodUbi"
   l_Enc = F3_Seek('F12c', , , f_Replace)
   If l_Enc = .F.
      Select (f_Anterior)
      Return c_Ubica
   EndIf

****> Ubicaciones asignadas a artículo.-------------------------------
   lxWhere = "F08uCodPro = '" + c_CodPro + "' And F08uArtDsd <= '" + c_Articulo + ;
        "' And F08uArtHst >= '" + c_Articulo + "'"
   f_Replace = "c_Ubica = F08uUbiDsd, c_UbiF = F08uUbiHst"
   l_Enc = F3_SeekTun('F08u', lxWhere, , f_Replace)

***-------------------------------------------------------------------
   =CrtCursor('F08c', 'PPrxF08c')
   lxWhere = "F08cCodPro='" + c_CodPro + "' And F08cCodArt='" + c_Articulo + "'"
   l_Enc = F3_Sql('*', 'F08c', lxWhere, , , 'PPrxF08c')
   IF l_Enc = .F.
      Use In PPrxF08c
      SELE (f_anterior)
      RETURN ""
   ENDIF
   Select PPrxF08c
   c_tipo_al = f08ctipalm
   c_TipPr = f08ctippro
   c_tipo_ml = F08cMulLot
   c_tipo_mp = F08cMulPro
   n_peso = ROUND(n_cantidad * f08cPesUni,0)
   n_volu = ROUND(n_cantidad * f08cVolUni,0)
   
   *> Si la ubicación no está vacía.----------------------------------
   IF !EMPTY(c_ubica)
	   If _Lenguaje <> 'VB'

    	  Wait window "Ubicando por proximidad a picking artículo "+ALLTRIM(c_CodPro +" - "+ c_Articulo) nowait
      EndIf
      UbicaMas = ""
      UbicaMen = ""

      m.F10cCodUbi = c_Ubica
      l_Enc = F3_Seek('F10c')
      If l_Enc = .F.
         *> Buscamos la ubicación.------------------------------------
         =CrtCursor('F10c', 'PPrxF10c')
         lxWhere = "F10cEstGen<>'O' And F10cVolLib - " + Str(n_Volu) + " >= 0 " + ;
              "And F10cPesOcu + " + Str(n_Peso) + " <= F10cPesTot " + ;
              "And F10cNumOcu + 1 <= F10cMaxPal And F10cPickSN = 'N' And F10cEstEnt<>'S' " + ;
              "And (F10cTipPro='" + c_TipPr + "' Or F10cTipPro='    ') " + ;
              "And SibStr(F10cCodUbi, 1, 4) = '" + _Alma + "'"

***> Cuidado con bloqueos a ubicaciones en Oracle.--------------------
         *> Buscar ubicacion superior.--------------------------------
         lxWhereSup = lxWhere + " And F10cCodUbi >= '" + c_Ubica + "'"
         l_Enc = F3_Sql('Min(F10cCodUbi)', 'F10c', lxWhereSup, 'F10cCodUbi', , 'PPrxF10c')
         Select PPrxF10c
         Go Top
         If !EOF()
            UbicaMas = F10cCodUbi
         EndIf

         *> Buscar ubicación inferior.--------------------------------
         lxWhereInf = lxWhere + " And F10cCodUbi <= '" + c_Ubica + "'"
         l_Enc = F3_Sql('Max(F10cCodUbi)', 'F10c', lxWhereInf, 'F10cCodUbi', , 'PPrxF10c')
         Select PPrxF10c
         Go Top
         If !EOF()
            UbicaMen = F10cCodUbi
         EndIf

         if Empty(UbicaMas)
            if empty(UbicaMen)
               c_Ubica=""
            Else 
               c_Ubica=UbicaMen
            Endif
         Else
            If Empty(UbicaMen)      
               c_Ubica=UbicaMas
            Else
               if UbicaMen>C_Ubica
                  c_Ubica=Ubicamas
               else
                  if UbicaMas<C_Ubica
                     C_Ubica=UbicaMen
                  Else
                     PesMen=Pesoubi(UbicaMen)
                     PesMas=Pesoubi(UbicaMas)
                     PesUbi=PesoUbi(c_Ubica)
                     PesMen=Abs(PesUbi-PesMen)
                     PesMas=Abs(PesMas-PesUbi)
                     c_Ubica=UbicaMas
                     if PesMen<PesMas
                        c_Ubica=Ubicamen
                     Endif   
                  Endif   
               endif
            Endif
         Endif         
         *> Actualizamos el fichero de ubicaciones.----------------
         if !Empty(c_Ubica)
            IF l_Actuali 
               *> Actualizamos el fichero de ubicaciones.-------------------
               l_Ubicado=.f.
               n_NPal=1
               Do CargaUbi in Ora_Ca00 with c_Ubica, n_Peso, n_Volu, n_NPal, l_Ubicado
               if !L_Ubicado
                  c_Ubica=""
               Endif   
            endif
         ENDIF
      ENDIF
      *> Cerramos el fichero de ubicaciones.--------------------------
      Wait Clear           
   ENDIF

   Use In PPrxF08c
   If Used('PPrxF10c')
      Use In PPrxF10c
   EndIf
   Select (f_Anterior)
Return 

*>==============================================================================
*> Prc_Alm .................. Buscar ubicación caótica del palet
*>==============================================================================
*> Cuando la mercancía no va a picking. Retorna el código de 
*> ubicación si la encuentra.
*>
*> Se necesitan tener definidas las variables :
*>    aubparmz[?]... Array de parámetros de ubicación.

*> aubparmz[01] = Artículo (no utilizado)
*> aubparmz[02] = Tipo de ubicación
*> aubparmz[03] = Ubicar en picking
*> aubparmz[04] = Criterio de ubicación
*> aubparmz[05] = Coeficiente de rotación
*> aubparmz[06] = Forzar tamaño
*> aubparmz[07] = Forzar zona
*> aubparmz[08] = Forzar ubicación
*> aubparmz[09] = Confirmación automática
*> aubparmz[10] = Desglose automático
*> aubparmz[11] = Imprimir etiqueta bulto
*> aubparmz[12] = Imprimir etiqueta palet

*> Si c_PickCaot="S" ubica según Picking Caótico, y si c_PickCaot="N"
*> hace ubicación caótica.
Procedure Prc_Alm
Parameters c_CodPro, ;
           c_articulo, ;
           c_tamanyo, ;
           n_cantidad, ;
           l_actualiza, ;
           l_automatico, ;
           c_Retorna, ;
           c_PickCaot, ;
           n_peso1, ;
           n_volu1

Private f_anterior, w_anterior, l_encontrado, l_zona, l_tamany, w_mensaje, f_inaux2
Private c_UbiI, c_UbiF, c_alm_ini, c_alm_fin, c_ZonI, c_ZonF, c_CalI, c_CalF, ;
        c_FilI, c_FilF, c_PisI, c_PisF, c_ProI, c_ProF, n_ProI, n_ProF
Private c_tipo_al, c_TipPr, n_peso, n_volu, n_coef
Private c_retorna, l_openf08, c_tipo_ml, c_tipo_mp, l_openf14, l_openf16, l_exito

   EXTERNAL ARRAY aubparmz()

   *> Guardamos el entorno.-------------------------------------------
   f_anterior = SELECT()
   w_anterior = WOUTPUT()

   *> Variable que contendrá la ubicación.----------------------------
   c_retorna = ""

   *> Abrimos el fichero de artículos.--------------------------------
   =CrtCursor('F08c', 'PAlmF08c')
   lxWhere = "F08cCodPro = '" + c_CodPro + "' And F08cCodArt = '" + c_Articulo + "'"
   l_Enc = F3_Sql('*', 'F08c', lxWhere, , , 'PAlmF08c')
   IF l_Enc = .F.
      Use In PAlmF08c
      Return
   Endif   

   *> Guardamos datos comunes.----------------------------------------
   c_tipo_al = F08ctipalm
   c_TipPr   = F08ctippro
   c_tipo_ml = F08cMulLot
   c_tipo_mp = F08cMulPro
   n_coef    = F08cCoeRot
   c_Tamany  = c_Tamanyo

   Store 0 to n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With c_CodPro, c_Articulo, n_Cantidad, n_Peso, n_Volu
   IF n_volu = 0
      n_volu = n_volu1
   ENDIF

   *> Variables de formas de ubicación.-------------------------------
   l_tamany  = (aubparmz[6] = "N")              && Forzar tamaño.
   l_zona    = (aubparmz[7] = "N")              && Forzar zona.

   *> Ubicaciones iniciales.------------------------------------------
   c_UbiI = _Alma + "          "
   c_UbiF = _Alma + "ZZZZZZZZZZ"

   *> Buscamos las ubicaciones de la mercancía.-----------------------
   *> Devuelve c_IbiI, c_UbiF.----------------------------------------
   Do Busca_Zona With c_CodPro, c_Articulo

*   *> Buscamos la zona.-----------------------------------------------
*   Do Busca_ZnPk With c_CodPro, c_Articulo, MainCodAlm

   *> Zona.-----------------------------------------------------------
   c_ZonI = SUBSTR(c_UbiI, 5, 2)
   c_ZonF = SUBSTR(c_UbiF, 5, 2)

   *> Calle.----------------------------------------------------------
   c_CalI = SUBSTR(c_UbiI, 7, 2)
   c_CalF = SUBSTR(c_UbiF, 7, 2)

   *> Fila.-----------------------------------------------------------
   c_FilI = SUBSTR(c_UbiI, 9, 3)
   c_FilF = SUBSTR(c_UbiF, 9, 3)

   *> Piso.-----------------------------------------------------------
   c_PisI = SUBSTR(c_UbiI, 12, 2)
   c_PisF = SUBSTR(c_UbiF, 12, 2)

   *> Profundidad.----------------------------------------------------
   c_ProI = SUBSTR(c_UbiI, 14, 1)
   c_ProF = SUBSTR(c_UbiF, 14, 1)

   *> Mensaje de espera.----------------------------------------------
    If _Lenguaje <> 'VB'
	   Wait Window "Intentando ubicar el artículo " + AllTrim(c_CodPro +" - "+ c_Articulo) NoWait
	EndIf
   *> Establecemos orden y filtro para fichero de ubicaciones.--------
   lxOrder = 'F10cTipAlm, F10cTipPro, F10cTamUbi, F10cPriori, F10cCodUbi'

   If c_PickCaot="N"                    && Ubicación en Palet Caótico.
      lxWhereIni = "F10cEstGen<>'O' And F10cPickSN='N'"                    && Picking="N"
   Else                                 && Ubicación por Picking Caótico.
      lxWhereIni = "F10cEstGen<>'O' And F10cPickSN In ('S', 'G', 'U')"     && Picking="S"
   EndIf

   lxWhereIni = lxWhereIni + " And F10cEstEnt='N'"

   *> Buscar en tipos de producto alternativos, según prioridades.
   _Selec = "Select * From F11t" + _em + ;
            " Where F11tTipPro='" + c_TipPr + _cm + ;
            " Order By F11tPriori"

   _xier = f3_SqlExec(_ASql, _Selec, 'F11t')
   If _xier <= 0
       _LxErr = 'Error leyendo prioridades por tipo de artículo (1)' + cr + ;
                'MENSAJE: ' + Message() + cr
       Do Form St3Inc
       _LxErr = ''
    Else
       =SqlMoreResults(_ASql)
       Select F11t
       Go Top
    EndIf

   *> Inicializa prioridades.-----------------------------------------
   n_ProI = 999999
   n_ProF = 0

   *> Activamos el filtro por coeficiente de rotación.----------------
   If aubparmz[5] = 'S'
      *> Retorna las proximidades.------------------------------------
      *> Devuelve n_ProI, n_ProF.
      Do Busca_Prox With n_Coef
   EndIf

*>      
*> Standard.
*   lxWhereIni = lxWhereIni + " And (F10cPesTot-F10cPesOcu) >= " + Str(n_Peso) + " And " + ;
*       "F10cNumOcu < F10cMaxPal And (F10cPriori Between " + Str(n_ProF) + " And " + ;
*       Str(n_ProI) + ") And " + ;
*       "(SubStr(F10cCodUbi, 5, 2) Between '" + c_ZonI + "' And '" + c_ZonF + "') And " + ;
*       "(SubStr(F10cCodUbi, 7, 2) Between '" + c_CalI + "' And '" + c_CalF + "') And " + ;
*       "(SubStr(F10cCodUbi, 9, 3) Between '" + c_FilI + "' And '" + c_FilF + "') And " + ;
*       "(SubStr(F10cCodUbi,12, 2) Between '" + c_PisI + "' And '" + c_PisF + "') And " + ;
*       "(SubStr(F10cCodUbi,14, 1) Between '" + c_ProI + "' And '" + c_ProF + "') And " + ;
*       "F10cTamUbi >= '" + c_Tamany + "' And SubStr(F10cCodUbi, 1, 4)='" + _Alma + "'"

   lxWhereIni = lxWhereIni + " And (F10cPesTot-F10cPesOcu) >= " + Str(n_Peso) + " And " + ;
       "F10cNumOcu < F10cMaxPal And (F10cPriori Between " + Str(n_ProF) + " And " + ;
       Str(n_ProI) + ") And F10cCodUbi Between '" + c_UbiI + "' And '" + c_UbiF + "' And " + ;
       "F10cTamUbi >= '" + c_Tamany + "' And " + _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "'"

   *> Busco por tipo almacenaje, tipo producto, tamaño palet.---------
   =CrtCursor('F10c', 'PAlmF10c')
   lxWhere = lxWhereIni + " And F10cTipAlm = '" + c_Tipo_Al + ;
             "' And (F10cTipPro = '" + c_TipPr + "' Or F10cTipPro='    ') " + ;
             " And F10cTamUbi = '" + c_Tamanyo + "' And " + _GCSS("F10cCodUbi", 1, 4) + " = '" + _Alma + "'"
   l_Enc = F3_Sql('*', 'F10c', lxWhere, lxOrder, , 'PAlmF10c')
   
   *> Bucle al fichero.-----------------------------------------------
   Do While l_Enc = .F.
      *> Reintento por tipo de producto ubicación.--------------------
      Select F11t
      If !Eof()
         c_TipPr = F11tTipDes
         Skip
      Else
         c_TipPr = Space(4)
      EndIf

      If !Empty(c_TipPr)
         lxWhere = lxWhereIni + " And F10cTipAlm = '" + c_Tipo_Al + ;
            "' And (F10cTipPro = '" + c_TipPr + "' Or F10cTipPro='    ') " + ;
            " And " + _GCSS("F10cCodUbi", 1, 4) + " = '" + _Alma + "'"
         l_Enc = F3_Sql('*', 'F10c', lxWhere, lxOrder, , 'PAlmF10c')
         Loop
      EndIf

      *> Si es posible, las de tamaño mayor.--------------------------
      If l_Tamany
         l_Tamany  = .F.
         lxWhere = lxWhereIni + " And F10cTipAlm = '" + c_Tipo_Al + ;
              "' And (F10cTipPro = '" + c_TipPr + "' Or F10cTipPro='    ') " + ;
              " And " + _GCSS("F10cCodUbi", 1, 4) + " = '" + _Alma + "'"
         l_Enc = F3_Sql('*', 'F10c', lxWhere, lxOrder, , 'PAlmF10c')
         Loop
      EndIf
      
      *> Si es posible distintas zonas.-------------------------------
      If l_Zona
         l_Zona = .F.
         c_UbiI = _Alma + "          "
         c_UbiF = _Alma + "ZZZZZZZZZZ"       && Las mayores zonas.

         *> Zona.-----------------------------------------------------
         c_ZonI = SUBSTR(c_UbiI, 5, 2)
         c_ZonF = SUBSTR(c_UbiF, 5, 2)

         *> Calle.----------------------------------------------------
         c_CalI = SUBSTR(c_UbiI, 7, 2)
         c_CalF = SUBSTR(c_UbiF, 7, 2)

         *> Fila.-----------------------------------------------------
         c_FilI = SUBSTR(c_UbiI, 9, 3)
         c_FilF = SUBSTR(c_UbiF, 9, 3)

         *> Piso.-----------------------------------------------------
         c_PisI = SUBSTR(c_UbiI,12, 2)
         c_PisF = SUBSTR(c_UbiF,12, 2)

         *> Profundidad.----------------------------------------------
         c_ProI = SUBSTR(c_UbiI,14, 1)
         c_ProF = SUBSTR(c_UbiF,14, 1)

         Loop
      EndIf

      *> Salida del bucle.--------------------------------------------
      Exit
   EndDo

   If l_Enc = .F.
      *> Mensaje de error.--------------------------------------------
*      = F3_sn(4,1,"El artículo '"+ALLTRIM(c_artículo)+"' no se puede ubicar")
*      = LibError(NIL, "El art¡culo '"+ALLTRIM(c_articulo)+"' no se puede "+;
*                          "ubicar", TRUE)
   Else
      *> Ubicación automática.----------------------------------------
*      If aubparmz[2] = 'A' .Or. l_Automático
         *> Guardamos el primer código de ubicación.------------------
         Select PAlmF10c
         Go Top
         If !EOF()
            c_Retorna = F10cCodUbi
         EndIf

      If !Empty(c_Retorna)
         If l_Actualiza
            *> Actualizamos el fichero de ubicaciones.----------------
            l_Ubicado=.f.
            n_Npal=1
            Do CargaUbi In Ora_Ca00 With c_Retorna, n_Peso, n_Volu, n_NPal, l_Ubicado
            If !l_Ubicado
               c_Retorna = ""
            EndIf
         EndIf
      EndIf
   EndIf

   *> Fin del mensaje de espera.---------------------------------------
   Wait Clear

   *> Restauramos el entorno.--------------------------------------
   Use In PAlmF08c
   Use In PAlmF10c
   Select (f_Anterior)

Return c_Retorna

*>=====================================================================================
*> Prc_Alm_Pal .................. Buscar ubicación caótica del palet para Multiproducto
*>=====================================================================================
*> Cuando la mercancía no va a picking. Retorna el código de 
*> ubicación si la encuentra.
*>
*> Se necesitan tener definidas las variables :
*>    aubparmz[?]... Array de parámetros de ubicación.

*> aubparmz[01] = Artículo (no utilizado)
*> aubparmz[02] = Tipo de ubicación
*> aubparmz[03] = Ubicar en picking
*> aubparmz[04] = Criterio de ubicación
*> aubparmz[05] = Coeficiente de rotación
*> aubparmz[06] = Forzar tamaño
*> aubparmz[07] = Forzar zona
*> aubparmz[08] = Forzar ubicación
*> aubparmz[09] = Confirmación automática
*> aubparmz[10] = Desglose automático
*> aubparmz[11] = Imprimir etiqueta bulto
*> aubparmz[12] = Imprimir etiqueta palet

*> Si c_PickCaot="S" ubica según Picking Caótico, y si c_PickCaot="N"
*> hace ubicación caótica.
Procedure Prc_Alm_Pal
Parameters c_CodPro, ;
           c_NumPal, ;
           c_Tamanyo, ;
           n_cantidad, ;
           l_actualiza, ;
           c_CodUbi, ;
           c_PickCaot, ;
           n_peso,	 ; 
           n_volu

Private f_anterior, w_anterior, l_encontrado, l_zona, l_tamany, w_mensaje, f_inaux2
Private c_UbiI, c_UbiF, c_alm_ini, c_alm_fin, c_ZonI, c_ZonF, c_CalI, c_CalF, ;
        c_FilI, c_FilF, c_PisI, c_PisF, c_ProI, c_ProF, n_ProI, n_ProF
Private c_tipo_al, c_TipPr, n_peso, n_volu, n_coef
Private c_retorna, l_openf08, c_tipo_ml, c_tipo_mp, l_openf14, l_openf16, l_exito
   EXTERNAL ARRAY aubparmz()

   *> Guardamos el entorno.-------------------------------------------
   f_anterior = SELECT()
   w_anterior = WOUTPUT()

   *> Variable que contendrá la ubicación.----------------------------
   c_retorna = ""

   *> Abrimos el fichero de artículos.--------------------------------
*   =CrtCursor('F08c', 'PAlmF08c')
*   lxWhere = "F08cCodPro = '" + c_CodPro + "' And F08cCodArt = '" + c_Articulo + "'"
*   l_Enc = F3_Sql('*', 'F08c', lxWhere, , , 'PAlmF08c')
*   IF l_Enc = .F.
*      Use In PAlmF08c
*      Return
*   Endif   

   *> Guardamos datos comunes.----------------------------------------
*   c_tipo_al = F08ctipalm
*   c_TipPr   = F08ctippro
*   c_tipo_ml = F08cMulLot
*   c_tipo_mp = F08cMulPro
*   n_coef    = F08cCoeRot
*   c_Tamany  = c_Tamanyo

   Store 0 to n_Peso, n_Volu
   Do PesVolPal In Ora_Ca00 With c_CodPro, c_NumPal, n_Peso, n_Volu
   IF n_volu = 0
      n_volu = 0 && n_volu1
   ENDIF

   *> Variables de formas de ubicación.-------------------------------
   l_tamany  = (aubparmz[6] = "N")              && Forzar tamaño.
   l_zona    = (aubparmz[7] = "N")              && Forzar zona.

   *> Ubicaciones iniciales.------------------------------------------
   c_UbiI = _Alma + "          "
   c_UbiF = _Alma + "ZZZZZZZZZZ"

   *> Buscamos las ubicaciones de la mercancía.-----------------------
   *> Devuelve c_IbiI, c_UbiF.----------------------------------------
   *Do Busca_Zona With c_CodPro, c_Articulo

*   *> Buscamos la zona.-----------------------------------------------
*   Do Busca_ZnPk With c_CodPro, c_Articulo, MainCodAlm

   *> Zona.-----------------------------------------------------------
   c_ZonI = SUBSTR(c_UbiI, 5, 2)
   c_ZonF = SUBSTR(c_UbiF, 5, 2)

   *> Calle.----------------------------------------------------------
   c_CalI = SUBSTR(c_UbiI, 7, 2)
   c_CalF = SUBSTR(c_UbiF, 7, 2)

   *> Fila.-----------------------------------------------------------
   c_FilI = SUBSTR(c_UbiI, 9, 3)
   c_FilF = SUBSTR(c_UbiF, 9, 3)

   *> Piso.-----------------------------------------------------------
   c_PisI = SUBSTR(c_UbiI, 12, 2)
   c_PisF = SUBSTR(c_UbiF, 12, 2)

   *> Profundidad.----------------------------------------------------
   c_ProI = SUBSTR(c_UbiI, 14, 1)
   c_ProF = SUBSTR(c_UbiF, 14, 1)

   *> Mensaje de espera.----------------------------------------------
   If _Lenguaje <> 'VB'
	   Wait Window "Intentando ubicar el palet " + AllTrim(c_CodPro +" - "+ c_NumPal) NoWait
   EndIf
   	
   *> Establecemos orden y filtro para fichero de ubicaciones.--------
   lxOrder = 'F10cTipAlm, F10cTipPro, F10cTamUbi, F10cPriori, F10cCodUbi'

   If c_PickCaot="N"                    && Ubicación en Palet Caótico.
      lxWhereIni = "F10cEstGen<>'O' And F10cPickSN='N'"                    && Picking="N"
   Else                                 && Ubicación por Picking Caótico.
      lxWhereIni = "F10cEstGen<>'O' And F10cPickSN In ('S', 'G', 'U')"     && Picking="S"
   EndIf

   lxWhereIni = lxWhereIni + " And F10cEstEnt='N'"

   *> Buscar en tipos de producto alternativos, según prioridades.
*   _Selec = "Select * From F11t" + _em + ;
*            " Where F11tTipPro='" + c_TipPr + _cm + ;
*            " Order By F11tPriori"

*   _xier = f3_SqlExec(_ASql, _Selec, 'F11t')
*   If _xier <= 0
*       _LxErr = 'Error leyendo prioridades por tipo de artículo (1)' + cr + ;
*                'MENSAJE: ' + Message() + cr
*       Do Form St3Inc
*       _LxErr = ''
*    Else
*       =SqlMoreResults(_ASql)
*       Select F11t
*       Go Top
*    EndIf

   *> Inicializa prioridades.-----------------------------------------
   n_ProI = 999999
   n_ProF = 0

   *> Activamos el filtro por coeficiente de rotación.----------------
 *  If aubparmz[5] = 'S'
      *> Retorna las proximidades.------------------------------------
      *> Devuelve n_ProI, n_ProF.
*      Do Busca_Prox With n_Coef
*   EndIf

*>      
*> Standard.
*   lxWhereIni = lxWhereIni + " And (F10cPesTot-F10cPesOcu) >= " + Str(n_Peso) + " And " + ;
*       "F10cNumOcu < F10cMaxPal And (F10cPriori Between " + Str(n_ProF) + " And " + ;
*       Str(n_ProI) + ") And " + ;
*       "(SubStr(F10cCodUbi, 5, 2) Between '" + c_ZonI + "' And '" + c_ZonF + "') And " + ;
*       "(SubStr(F10cCodUbi, 7, 2) Between '" + c_CalI + "' And '" + c_CalF + "') And " + ;
*       "(SubStr(F10cCodUbi, 9, 3) Between '" + c_FilI + "' And '" + c_FilF + "') And " + ;
*       "(SubStr(F10cCodUbi,12, 2) Between '" + c_PisI + "' And '" + c_PisF + "') And " + ;
*       "(SubStr(F10cCodUbi,14, 1) Between '" + c_ProI + "' And '" + c_ProF + "') And " + ;
*       "F10cTamUbi >= '" + c_Tamany + "' And SubStr(F10cCodUbi, 1, 4)='" + _Alma + "'"

   lxWhereIni = lxWhereIni + " And (F10cPesTot-F10cPesOcu) >= " + Str(n_Peso) + " And " + ;
       "F10cNumOcu < F10cMaxPal And (F10cPriori Between " + Str(n_ProF) + " And " + ;
       Str(n_ProI) + ") And F10cCodUbi Between '" + c_UbiI + "' And '" + c_UbiF + "' And " + ;
       "F10cTamUbi >= '" + c_Tamanyo + "' And " + _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "'"

   lxWhereIni = lxWhereIni +  " And F10cTipAlm = 'C'"

   *> Busco por tipo almacenaje, tipo producto, tamaño palet.---------
   =CrtCursor('F10c', 'PAlmF10c')
*   lxWhere = lxWhereIni + " And F10cTamUbi = '" + c_Tamanyo + "' And SubStr(F10cCodUbi, 1, 4) = '" + _Alma + "'"
   l_Enc = F3_Sql('*', 'F10c', lxWhereIni, lxOrder, , 'PAlmF10c')
   
   *> Bucle al fichero.-----------------------------------------------
   Do While l_Enc = .F.
      *> Reintento por tipo de producto ubicación.--------------------
  *    Select F11t
  *    If !Eof()
  *       c_TipPr = F11tTipDes
  *       Skip
  *    Else
  *       c_TipPr = Space(4)
  *    EndIf

  *    If !Empty(c_TipPr)
  *       lxWhere = lxWhereIni + " And F10cTipAlm = '" + c_Tipo_Al + ;
  *          "' And (F10cTipPro = '" + c_TipPr + "' Or F10cTipPro='    ') " + ;
  *          " And SubStr(F10cCodUbi, 1, 4) = '" + _Alma + "'"
  *       l_Enc = F3_Sql('*', 'F10c', lxWhere, lxOrder, , 'PAlmF10c')
  *       Loop
  *    EndIf

      *> Si es posible, las de tamaño mayor.--------------------------
      If l_Tamany
         l_Tamany  = .F.
         lxWhere = lxWhereIni + " And F10cTipAlm = '" + c_Tipo_Al + ;
              "' And (F10cTipPro = '" + c_TipPr + "' Or F10cTipPro='    ') " + ;
              " And " + _GCSS("F10cCodUbi", 1, 4) + " = '" + _Alma + "'"
         l_Enc = F3_Sql('*', 'F10c', lxWhere, lxOrder, , 'PAlmF10c')
         Loop
      EndIf
      
      *> Si es posible distintas zonas.-------------------------------
      If l_Zona
         l_Zona = .F.
         c_UbiI = _Alma + "          "
         c_UbiF = _Alma + "ZZZZZZZZZZ"       && Las mayores zonas.

         *> Zona.-----------------------------------------------------
         c_ZonI = SUBSTR(c_UbiI, 5, 2)
         c_ZonF = SUBSTR(c_UbiF, 5, 2)

         *> Calle.----------------------------------------------------
         c_CalI = SUBSTR(c_UbiI, 7, 2)
         c_CalF = SUBSTR(c_UbiF, 7, 2)

         *> Fila.-----------------------------------------------------
         c_FilI = SUBSTR(c_UbiI, 9, 3)
         c_FilF = SUBSTR(c_UbiF, 9, 3)

         *> Piso.-----------------------------------------------------
         c_PisI = SUBSTR(c_UbiI,12, 2)
         c_PisF = SUBSTR(c_UbiF,12, 2)

         *> Profundidad.----------------------------------------------
         c_ProI = SUBSTR(c_UbiI,14, 1)
         c_ProF = SUBSTR(c_UbiF,14, 1)

         Loop
      EndIf

      *> Salida del bucle.--------------------------------------------
      Exit
   EndDo

   If l_Enc = .F. .And. _Lenguaje <> 'VB'
      *> Mensaje de error.--------------------------------------------
      = F3_sn(4,1,"El palet '"+ALLTRIM(c_NumPal)+"' no se puede ubicar")
      = LibError(NIL, "El palet '"+ALLTRIM(c_NumPal)+"' no se puede "+;
                          "ubicar", TRUE)
   Else
      *> Ubicación automática.----------------------------------------
*      If aubparmz[2] = 'A' .Or. l_Automatico
         *> Guardamos el primer código de ubicación.------------------
         Select PAlmF10c
         Go Top
         If !EOF()
            c_Retorna = F10cCodUbi
            c_CodUbi = c_Retorna
         EndIf
*      EndIf

      If !Empty(c_Retorna)
         If l_Actualiza
            *> Actualizamos el fichero de ubicaciones.----------------
            l_Ubicado=.f.
            n_Npal=1
            Do CargaUbi In Ora_Ca00 With c_Retorna, n_Peso, n_Volu, n_NPal, l_Ubicado
            If !l_Ubicado
               c_Retorna = ""
            EndIf
         EndIf
      EndIf
   EndIf

   *> Fin del mensaje de espera.---------------------------------------
   Wait Clear

   *> Restauramos el entorno.--------------------------------------
*   Use In PAlmF08c
   Use In PAlmF10c
   Select (f_Anterior)

Return c_Retorna

*>==============================================================================
*> Prc_AlmTipPro. Ubicar por tipo producto.
*>==============================================================================
*> Si c_PickCaot="S" ubica según Picking Caótico, y si c_PickCaot="N"
*> hace ubicación caótica.
Procedure Prc_AlmTipPro
Parameters c_CodPro, ;
           c_articulo, ;
           n_cantidad, ;
           c_Retorna, ;
           c_PickCaot, ;
           n_peso1, ;
           n_volu1

Private f_anterior, w_anterior, l_encontrado, l_zona, l_tamany, w_mensaje, f_inaux2
Private c_UbiI, c_UbiF, c_alm_ini, c_alm_fin, c_ZonI, c_ZonF, c_CalI, c_CalF, ;
        c_FilI, c_FilF, c_PisI, c_PisF, c_ProI, c_ProF, n_ProI, n_ProF
Private c_tipo_al, c_TipPr, n_peso, n_volu, n_coef
Private c_retorna, l_openf08, c_tipo_ml, c_tipo_mp, l_openf14, l_openf16, l_exito

   EXTERNAL ARRAY aubparmz()

   *> Guardamos el entorno.-------------------------------------------
   f_anterior = Select()
   w_anterior = WOutput()

   *> Variable que contendrá la ubicación.----------------------------
   c_retorna = ""

   *> Obtener datos de la ficha del artículo.
   =CrtCursor('F08c', 'PAlmF08c')
   lxWhere = "F08cCodPro = '" + c_CodPro + "' And F08cCodArt = '" + c_Articulo + "'"
   l_Enc = F3_Sql('*', 'F08c', lxWhere, , , 'PAlmF08c')
   IF l_Enc = .F.
      Use In PAlmF08c
      Return .F.
   Endif   

   Select PAlmF08c
   *> Guardamos datos comunes.----------------------------------------
   c_tipo_al = F08ctipalm
   c_TipPr   = F08ctippro
   c_tipo_ml = F08cMulLot
   c_tipo_mp = F08cMulPro
   n_coef    = F08cCoeRot

   *> Calcular peso y volumen de la cantidad a ubicar.
   Store 0 to n_Peso, n_Volu
   Do PesVolAr In Ora_Ca00 With c_CodPro, c_Articulo, n_Cantidad, n_Peso, n_Volu
   IF n_volu = 0
      n_volu = n_volu1
   ENDIF

   *> Mensaje de espera.----------------------------------------------
   If _Lenguaje <> 'VB'
	   Wait Window "Intentando ubicar el artículo " + AllTrim(c_CodPro +" - "+ c_Articulo) NoWait
	EndIf

   If c_PickCaot="N"                    && Ubicación en Palet Caótico.
      lxWhereIni = "F10cEstGen<>'O' And F10cPickSN='N'"                    && Picking="N"
   Else                                 && Ubicación por Picking Caótico.
      lxWhereIni = "F10cEstGen<>'O' And F10cPickSN='S'"                    && Picking="S"
   EndIf

   lxWhereIni = lxWhereIni + " And F10cEstEnt='N'"
   lxWhereIni = lxWhereIni + " And F10cTipAlm = '" + c_Tipo_Al + "'"
   lxWhereIni = lxWhereIni + " And " + _GCSS("F10cCodUbi", 1, 4) + " = '" + _Alma + "'"

   *> Inicializa prioridades.-----------------------------------------
   n_ProI = 999999
   n_ProF = 0

   *> Activamos el filtro por coeficiente de rotación.----------------
   If aubparmz[5] = 'S'
      *> Retorna las proximidades.------------------------------------
      *> Devuelve n_ProI, n_ProF.
      Do Busca_Prox With n_Coef

      *> Ordenar por proximidad.
      lxOrder = 'F10cTipAlm, F10cPriori, F10cCodUbi'
   Else
      *> Ordenar por ubicación.
      lxOrder = 'F10cTipAlm, F10cCodUbi'
   EndIf

   lxWhereIni = lxWhereIni + ;
                " And (F10cPesTot-F10cPesOcu) >= " + ;
                Str(n_Peso) + " And F10cNumOcu < F10cMaxPal And (F10cPriori Between " + ;
                Str(n_ProF) + " And " + Str(n_ProI) + ")"

   *> Crear cursor de ubicaciones.
   =CrtCursor('F10c', 'PAlmF10c')
   
   *> Buscar en tipos de producto alternativos, según prioridades.
   _Selec = "Select * From F11t" + _em + ;
            " Where F11tTipPro='" + c_TipPr + _cm + ;
            " Order By F11tPriori"

   _xier = f3_SqlExec(_ASql, _Selec, 'F11t')
   If _xier <= 0
       _LxErr = 'Error leyendo prioridades por tipo de artículo (2)' + cr + ;
                'MENSAJE: ' + Message() + cr
       Do Form St3Inc
       _LxErr = ''
       Return .F.
   EndIf

   =SqlMoreResults(_ASql)

   *> Bucle de búsqueda de ubicación por tipo de producto.
   l_Enc = .F.

   Select F11t
   Go Top
   Do While !Eof()
      c_TipPr = F11tTipDes
      lxWhere = lxWhereIni + " And F10cTipPro = '" + c_TipPr + ;
            "' And " + _GCSS("F10cCodUbi", 1, 4) + " = '" + _Alma + "'"

      l_Enc = F3_Sql('*', 'F10c', lxWhere, lxOrder, , 'PAlmF10c')
      If l_Enc
         Exit
      EndIf

      Select F11t
      Skip
   EndDo

   *> Si encuentra ubicaciones que cumplan. Devolver la primera.
   If l_Enc
      Select PAlmF10c
      Go Top
      If !Eof()
         c_Retorna = F10cCodUbi
      EndIf

      *> Actualizar la ubicación.
      If !Empty(c_Retorna)
         l_Ubicado = .F.
         n_Npal = 1
         Do CargaUbi In Ora_Ca00 With c_Retorna, n_Peso, n_Volu, n_NPal, l_Ubicado
         If !l_Ubicado
            c_Retorna = ""
         EndIf
      EndIf
   EndIf

   *> Fin del mensaje de espera.---------------------------------------
   Wait Clear

   *> Restauramos el entorno.--------------------------------------
   Use In PAlmF08c
   Use In PAlmF10c
   Select (f_Anterior)

Return c_Retorna

*>==============================================================================
*> Busca_Zona ............... Buscar zona de un artículo
*>==============================================================================
Procedure Busca_Zona
Parameter c_CodPro, c_CodArt
Private f_Fichero
   *> Guardamos el entorno.-------------------------------------------
   f_Fichero = Select()

   *> Ubicaciones asignadas a artículo.-------------------------------
   lxWhere = "F08uCodPro = '" + c_CodPro + "' And F08uArtDsd <= '" + c_CodArt + ;
        "' And F08uArtHst >= '" + c_CodArt + "'"
   f_Replace = "c_UbiI = F08uUbiDsd, c_UbiF = F08uUbiHst"
   l_Enc = F3_SeekTun('F08u', lxWhere, , f_Replace)
   l_enc = f3_sql('*', 'F08u', lxWhere, , , '_F08uCur')

   Select (f_Fichero)
Return

*>==============================================================================
*> Busca_ZnPk ............... Buscar zona de picking de un artículo
*>==============================================================================
Procedure Busca_ZnPk
Parameter c_CodPro, c_CodArt, c_Almacen
Private f_Fichero, c_Ubica
   *> Guardamos el entorno.-------------------------------------------
   f_Fichero  = SELECT()

   c_Ubica = ""
   m.F12cCodPro = c_CodPro
   m.F12cCodArt = c_CodArt
   m.F12cCodAlm = c_Almacen
   f_Replace = "c_Ubica = F12cCodUbi"
   l_Enc = F3_Seek('F12c', , , f_Replace)
   If l_Enc = .F.
      c_UbiI = Left(c_Ubica, 6) + Space(8)
      c_UbiF = Left(c_Ubica, 6) + Replicate('Z', 8)
   EndIf

   *> Restauramos el entorno.-----------------------------------------
   Select (f_Fichero)
Return

*>==============================================================================
*> Busca_Prox ............... Busca las proximidades
*>==============================================================================
Procedure Busca_Prox
Parameters n_Rotacion
Private f_Fichero, lxWhere, lxReplace
   f_Fichero = Select()
   m.F11cCoefRt = n_Rotacion
   lxReplace = "n_ProI=F11cPriIni, n_ProF=F11cPriFin"
   =F3_Seek('F11c', , , lxReplace)

   Select (f_Fichero)
Return

*>==============================================================================
*> PesoUbi .................. Busca peso de una ubicación
*>==============================================================================
Procedure PesoUbi
Parameters Ubicacion
Private Peso
   *> Miraremos solo calle y fila.------------------------------------
   PesoCall = Val(Substr(Ubicacion,7,2))*1000
   fila1 = Substr(Ubicacion,10,1)
   fila2 = Substr(Ubicacion,11,1)
   if asc(Fila1)>64
      PesoFila=(Asc(Fila1)-65)+10
      PesoFila=PesoFila*10
   else   
      Pesofila=Val(Fila1)*10
   Endif
   PesoFila=Pesofila+val(fila2)
   Peso=Pesocall+PesoFila
Return Peso      

*>=========================================================================
*> Calcular la cantidad de un artículo/ubicación en MPs de entrada.
*>=========================================================================
Procedure CanPickingMP
Parameters CodPro, CodArt, CodUbi
Local TotalMP, _Selec

Store 0 To TotalMP

_Selec = " Select " + _GCN("Sum(F14cCanFis)") + " As TotalMP From F14c" + _em + ;
         " Where F14cUbiOri='" + CodUbi + _cm + ;
         " And   F14cCodPro='" + CodPro + _cm + ;
         " And   F14cCodArt='" + CodArt + _cm + ;
         " And " + _GCSS("F14cTipMov", 1, 1) + "='1'"

Err = f3_SqlExec(_ASql, _Selec, 'PicF14cCur')
If Err <=0
   _LxErr = 'Error calculando cantidad picking en MPs' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
Else
   =SqlMoreResults(_ASql)
   Select PicF14cCur
   Go Top
   Scatter MemVar
   Use In PicF14cCur
EndIf

Return TotalMP

*>=========================================================================
*> Calcular la cantidad de un artículo/ubicación en ocupaciones.
*>=========================================================================
Procedure CanPickingOC
Parameters CodPro, CodArt, CodUbi
Local TotalOC, _Selec

Store 0 To TotalOC

_Selec = " Select " + _GCN("F16cCanFis") + " As TotalOC From F16c" + _em + ;
         " Where F16cCodUbi='" + CodUbi + _cm + ;
         " And   F16cCodPro='" + CodPro + _cm + ;
         " And   F16cCodArt='" + CodArt + _cm + ;
         " And " + _GCSS("F16cSitStk", 1, 1) + "='1'"

Err = f3_SqlExec(_ASql, _Selec, 'PicF16cCur')
If Err <=0
   _LxErr = 'Error calculando cantidad picking en ocupaciones' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
Else
   =SqlMoreResults(_ASql)
   Select PicF16cCur
   Go Top
   Scatter MemVar
   Use In PicF16cCur
EndIf

Return TotalOC

*>=========================================================================
*> Calcular la cantidad pre-ubicada de un artículo.
*> Durante el proceso de entradas se genera un cursor donde se van
*> almacenando los palets para los que se ha encontrado ubicación.
*>=========================================================================
Procedure CanPickingTMP
Parameters CodPro, CodArt, CodUbi
Local TotalTMP

Store 0 To TotalTMP

If Used('_F14cTmpUbic')
   Select _F14cTmpUbic
   Locate For _F14cCodPro = CodPro .And. ;
              _F14cCodArt = CodArt .And. ;
              _F14cCodUbi = CodUbi

   If Found()
      TotalTMP = _F14cCanFis
   EndIf
EndIf

Return TotalTMP

*>=========================================================================
*> Actualizar palets ubicados para memorizar cantidad pre-ubicada.
*>=========================================================================
Procedure UpdateCanPickingTMP
Parameters CodPro, CodArt, CodUbi, CanFis

If !Used('_F14cTmpUbic')
   Create Cursor _F14cTmpUbic ;
      (_F14cCodPro C(6), ;
       _F14cCodArt C(13), ;
       _F14cCodUbi C(14), ;
       _F14cCanFis N(10, 0))
EndIf

Select _F14cTmpUbic
Locate For _F14cCodPro = CodPro .And. ;
           _F14cCodArt = CodArt .And. ;
           _F14cCodUbi = CodUbi

If !Found()
   Append Blank
   Replace _F14cCodPro With CodPro, ;
           _F14cCodArt With CodArt, ;
           _F14cCodUbi With CodUbi, ;
           _F14cCanFis With 0
EndIf
Replace _F14cCanFis With _F14cCanFis + CanFis

Return

*>=========================================================================
*> Ver si una ubicación de picking está asignada a otro producto.
*>=========================================================================
Procedure UbiPicAsignada
Parameters CodPro, CodArt, CodUbi
Local _ok, lxWhere, f_from

   lxWhere = "F12cCodAlm='" + _Alma + "' And F12cCodUbi='" + CodUbi + "'"
   f_from = 'F12c'
   l_enc = f3_sql('*', f_from, lxWhere, , , 'PPicAsiF12c')

   Select PPicAsiF12c
   Locate For F12cCodPro # CodPro .Or. F12cCodArt # CodArt
   _ok = Found()

   Use In PPicAsiF12c

Return _ok

*>=========================================================================
*> Ver si una ubicación de picking está ocupada por otro producto y, en
*> este caso, si es multilote/multiproducto.
*>=========================================================================
Procedure UbiPicOcupada
Parameters CodPro, CodArt, CodUbi
Local _ok, lxWhere, f_from
Local _MulPro, _MulLot

   *> Guardar los datos de la ficha de producto.
   m.F08cCodPro = CodPro
   m.F08cCodArt = CodArt
   =f3_seek('F08C')
   Select F08c
   _MulPro = F08c.F08cMulPro
   _MulLot = F08c.F08cMulLot

   *> Resultado de la función.
   _ok = .T.

   *> String de selección de ocupaciones.
   lxWhere = "F16cCodUbi='" + CodUbi + "'"
   f_from = 'F16c'
   l_enc = f3_sql('*', f_from, lxWhere, , , 'PPicAsiF16c')

   Select PPicAsiF16c
   Scan For F16cCodPro # CodPro .Or. F16cCodArt # CodArt
      Do Case
         *> La ubicación NO es multiproducto.
         Case _MulPro=='N'
            _ok = .F.
            Exit

         *> La ubicación SI es multiproducto.
         Case _MulPro=='S'
            m.F08cCodPro = F16cCodPro
            m.F08cCodArt = F16cCodArt
            =f3_seek('F08C')

            *> El producto que hay NO es multiproducto.
            If F08c.F08cMulPro=='N'
               _ok = .F.
               Exit
            EndIf
      EndCase      
   EndScan

   Use In PPicAsiF16c

Return _ok

*>=========================================================================
*> Ver si una ubicación de picking ya tiene algún MP de entrada asignada.
*> este caso, si es multilote/multiproducto.
*>=========================================================================
Procedure UbiPicOcupadaMP
Parameters CodPro, CodArt, CodUbi
Local _ok, lxWhere, f_from
Local _MulPro, _MulLot

   *> Guardar los datos de la ficha de producto.
   m.F08cCodPro = CodPro
   m.F08cCodArt = CodArt
   =f3_seek('F08C')
   _MulPro = F08c.F08cMulPro
   _MulLot = F08c.F08cMulLot

   *> Resultado de la función.
   _ok = .T.

   *> String de selección de ocupaciones.
   lxWhere = "F14cUbiOri='" + CodUbi + "' And " + ;
             _GCSS("F14cTipMov", 1, 1) + "='1'"
   f_from = 'F14c'
   l_enc = f3_sql('*', f_from, lxWhere, , , 'PPicAsiF14c')

   Select PPicAsiF14c
   Scan For F14cCodPro # CodPro .Or. F14cCodArt # CodArt
      Do Case
         *> La ubicación NO es multiproducto.
         Case _MulPro=='N'
            _ok = .F.
            Exit

         *> La ubicación SI es multiproducto.
         Case _MulPro=='S'
            m.F08cCodPro = F14cCodPro
            m.F08cCodArt = F14cCodArt
            =f3_seek('F08C')

            *> El producto que hay NO es multiproducto.
            If F08c.F08cMulPro=='N'
               _ok = .F.
               Exit
            EndIf
      EndCase      
   EndScan

   Use In PPicAsiF14c

Return _ok
