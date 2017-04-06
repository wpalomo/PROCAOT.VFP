*> Cabecera

   *> Descripcio ................. Procesos varios.
   *> Modul ...................... Proceso.PRG
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 8.01.99
   *> Data de fi .................

*> Utilitats

   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*>---------------------TRATAMIENTO LISTAS----------------------------------
*> Modulos........................ ExistF14c
*>                                 ExistF26l
*>                                 ExistF16c
*>                                 UpdQMovF16c
*>                                 NroMovF26l
*>                                 DelMovF26l
*>                                 DelCabF26c
*>                                 DelMvtF14c
*>                                 ExistF24l
*>                                 UpdMvtF24l
*>                                 ExistF13c
*>                                 updateF13c
*>                                 InsertF13c
*>                                 ActSSF13c
*>                                 DesReservar
*>                                 CancRepos
*>                                 CamposHM
*>                                 CanLis
*>                                 ECanFisF16c
*>                                 GrabHMF20c
*>                                 RegLis
*>                                 UpdF14c
*>                                 UpdF26l
*>                                 ChgUbi
*>                                 ChgUbiPal
*>                                 ChgOpe
*>                                 ChgLst
*>                                 ConLis
*>                                 UpdOperF14c
*>                                 DesOper
*>                                 DesLis
*>                                 DivMov
*>                                 VerificarMac
*>                                 DividirMac

*>-----------------------REPOSICIÓN POR MÍNIMOS------------------
*>                                 RepCajToUni
*>                                 OcuRep
*>                                 ActOcuCand
*>                                 ActOcup
*>                                 RepCajas

*>------------------------------ESTADISTICAS--------------------------
*>                                 GenEst
*>                                 GenEstDis

*>           (C)Copyright ALISFOUR 2004 BARCELONA


*>Reserva de documentos.----------------------------------------------
Procedure Reserva

    *> Crear fichero para listado de incidencias.---------------------
    Select XlIncid
    LinIncid = 1
    Select LinRes

    *>Cargaremos las lineas a reservar.-------------------------------
    Do Case

       *>Iremos por lineas.-------------------------------------------
       Case !Eof()
       Do ResLin

       *>Iremos por Cabeceras.----------------------------------------
       Case Eof()
       Do ResCab

    EndCase

    Select F25c
    If F25cLisRpo = "S"
       *>Listado de reposiciones pendientes.--------------------------
       _Label1 = "LISTADO DE REPOSICIONES PENDIENTES"
       Do Form Pntw\LisRpo1 With _Label1
       
    EndIf

    If F25cRepAut = "S"
       *>Confirmación automática de reposiciones.---------------------
       Do ConfRep 
    EndIf
    select SYSPRG
    
Return

Procedure ResCab

    *>Leemos los registros de cabecera.-------------------------------
    select LINRES
    zap
    Select CabRes
    Go top

    *>Pendiente del sql
    *>Posible Solución cargar en un temporal Linres y mandarlo al Procedure Linres.
    *>con todas las lineas marcadas.
    Do While !Eof()
       If Cabres.Marca=1
          =f3_sql('recno(),F24lTipDoc,F24lNumDoc,F24lNumLin,F24lCodArt,F24lNumLot,' + ;
            'F24lSitStk,F24lCantid,F24lCanREs,F24lCanser','f24l', + ;
            'F24lTipDoc+F24lNumDoc=CabREs.f24cTipDoc+Cabres.f24cNumDoc.And.' + ;
            '(F24lCantid-(F24lCanres+F24lCanSer))>0.And.F24lCanser=0','F24lTipDoc','','+LINRES')
       EndIf 
       Select CabRes
       Skip
    EndDo
    Select linres
    Replace all Marca With 1
    Do ResLin
Return


Procedure ResLin
Private n_CanPal,n_cantidad,C_articu
Private n_Cant,N_CanExt,C_TipDoc,C_NumDoc,C_NumLin
Private C_NumLot,C_sitSto,C_UbiDes

    *>Leemos los registros de Lineas.----------------------------------
    Select LinRes
    Go top
    Do While !Eof()
       If Marca = 1
          Pico=.f.

            *> Inicializamos variables.-------------------------------
            n_cantidad = f24lcantid
            c_articu   = f24lcodart
            n_canpal   = 0

            *> Abrimos el fichero de articulos.-----------------------
            Select F08c
            IF SEEK(c_articu)
               n_canpal = f08cunipac*f08cpaccaj*f08ccajpal
            ENDIF

            *> Controlamos si es un pico.-----------------------------
            pico = (n_canpal > n_cantidad)

            *> Reservamos.--------------------------------------------
            Select LinRes

            *> Guardamos datos comunes.----------------------------------------
            n_cant   = f24lcantid
            c_tipdoc = f24ltipdoc
            c_numdoc = f24lnumdoc
            c_numlin = f24lnumlin
            c_numlot = f24lnumlot
            c_sitsto = f24lsitstk
            c_ubides = ""
            n_CanExt = 0
            TxtErr = ""

            Select F24c
            Seek C_tipDoc+C_NumDoc
            C_TipEnt = F24cTipEnt
            C_NumEnt = F24cCodEnt

            Select LinRes
            Do PRC_RES with n_cant, c_tipdoc, c_numdoc, c_numlin,c_TipEnt,c_NumEnt,; 
                            c_articu,c_NumLot,c_sitsto, c_ubides, Pico, ;
                            n_canext, TxtErr

            Select LinRes
            *> En el caso de que no se pueda.-------------------------
            IF !EMPTY(txterr)
               *> Grabar incidencia.----------------------------------
               Select XlIncid
               TxtIncid = "Pedido : "+c_NumDoc+", Linea : "+(C_numLin)+", ;
                       Cantidad : "+Tran(n_cant,"9,999,999")
               Append Blank
               Replace InNumLin With LinIncid, InTxtInc With TxtIncid
               Append Blank
               Replace InNumLin With LinIncid, InTxtInc With TxtErr
               Append Blank
               Replace InNumLin With LinIncid, InTxtInc With "NO PUDO RESERVARSE"
               Select LinRes
               REPLACE Marca WITH 0
            Else
               if n_CanExt>0

                  *> Modifico la cantidad que extraigo.---------------------------
                  Select LinRes
                  if n_Cant>n_CanExt
                     SCAT TO a_r
                     *> Genero el nuevo registro.-----------------------------------
                     INSERT BLANK after
                     GATH FROM a_r
                     REPLACE f24lcantid WITH f24lcantid-n_CanExt
                     *> Situamos el puntero de nuevo en donde estaba.---------------
                     Skip -1
                     rele a_r
                  Endif   
                  REPLACE f24lcantid WITH n_CanExt
               else   
*                  = liberror(NIL,"Pedido : "+c_NumDoc+" Linea : "+C_numLin+" Cantidad : "+Tran(n_cant,"9,999,999")+"|"+TxtErr+"|NO PUDO RESERVARSE",FALSE)
                  *> Grabar incidencia.----------------------------------
                  Select XlIncid
                  TxtIncid = "Pedido : "+c_NumDoc+", Linea : "+(C_numLin)+", Cantidad : "+Tran(n_cant,"9,999,999")
                  Append Blank
                  Replace InNumLin With LinIncid, InTxtInc With TxtIncid
                  Append Blank
                  Replace InNumLin With LinIncid, InTxtInc With TxtErr
                  Append Blank
                  Replace InNumLin With LinIncid, InTxtInc With "NO PUDO RESERVARSE"
                  Select Linres
                  REPLACE Marca WITH 0
               Endif    
            Endif
         else   
            Select LinRes
            REPLACE Marca WITH 0
         ENDIF

       *> Avanzar el puntero.------------------------------------------
       LinIncid = LinIncid + 1
      Select LinRes
      Skip
   ENDDO

   *> Imprimir listado de incidencias de reserva.---------------------
   Select XlIncid
   Go Top
   If !EOF()
       *> Sacar listado de incidencias.--------------------------------
       *>Teoricamente Llamaremos a un report para la impresión.-----------
*>     Do Form Pntw\IncRes1 With _Label1
       Do form st3imp2 with 'LISTADO DE INCIDENCIAS DE RESERVA','IncRes1','XlIncid'
       select SYSPRG
   EndIf

Return


Procedure Prc_Res
Parameters n_CantOri, c_Tip_Doc, c_Num_Doc, c_Num_Lin,c_Tip_Ent,c_Num_Ent,;
           c_Articulo, c_Num_Lote,;
           c_Sit_Stoc, c_Ubi_Dest, Pico, n_CanExtr, Txt_Err

Private c_Ubi_Pick, c_UbiRepos, n_Cantidad

   *> Mirar que valor dejamos en situaci¢n de stock.-------------------
   c_Sit_Stoc = IIF(Empty(c_Sit_Stoc),"1000",c_Sit_stoc)

   *> Establecer la clave e indice según venga lote o no.--------------
   If Empty(c_Num_Lote)
      c_Clave = c_Articulo
      Indice = "IND02"
   Else
      c_Clave = c_Articulo + c_Num_Lote
      Indice = "IND06"
   EndIf

   =WaitWindow("Reservando el material "+AllTrim(c_Articulo))

   *> Abrimos el fichero de lineas de pedido.-------------------------
   Select F24l

   *> Contemplamos las dos posibilidades basicas.---------------------
   Do Case
      *> Lo que pedimos es igual o superior a la unidad de palet.-----
      Case Pico = .F. 
         *> Flag de cantidad encontrada.------------------------------
           SwCarga = .F.

           *> Buscamos en ubicaciones de carga.-----------------------
           =WaitWindow("Reservando en carga el material "+AllTrim(c_Articulo))
           Do Carga

           *> Si no encontramos nada en carga.------------------------
           If SwCarga = .F.
              *> Buscamos en ubicaciones de picking.------------------
              =WaitWindow("Reservando en Picking el material " +AllTrim(c_Articulo))
              Do Picking
           EndIf              
 
      *> Lo que pedimos es menor a la unidad de palet.----------------
      Case Pico = .T.
         *> Flag de cantidad encontrada.------------------------------
         SwCarga = .T.
           
         *> Buscamos en ubicaciones de carga.-------------------------
         =WaitWindow("Reservando en Picking el material "+AllTrim(c_Articulo))
         Do Picking
   EndCase

   *> Buscamos el código en las líneas reales.------------------------
   Select F24l
   If Seek(c_Tip_Doc + c_Num_Doc + c_Num_Lin)
      *> Añadimos la cantidad a la reservada.-------------------------
      Replace F24lCanRes With F24lCanRes + n_CanExtr
            
      *> Restamos cantidad reservada de Situaci¢n 7500.---------------
       l_ValRet = .F.
       Do ActualSt In Lib_Ca00 with F24lCodArt,   ;
                                    n_CanExtr, "7500",.F., l_ValRet
   EndIf

   *> Buscamos cabecera para actualizar el estado.--------------
   Select F24c
   If Seek(c_Tip_Doc + c_Num_Doc)
      If n_CanExtr>0
         *> Añadimos la cantidad a la reservada.-------------------
         Replace F24cRestSn With "R"
      Else 
         *> Añadimos la cantidad a la reservada.-------------------
         Replace F24cRestSn With "P"
      EndIf
   EndIf 

   *> Si no encontramos material.-------------------------------------
   If n_CanExtr = 0
      Select F08c
      Seek c_Articulo
      c_Descri = F08cDescri
      c_Marca  = F08cTipPro
      Select F26c
      Txt_Err = "No Encontrada cant. artículo :"+ c_Articulo+ ;
           " ("+ c_Descri +")"
   EndIf

   *> Cerramos ficheros.----------------------------------------------
   Select F16c
   UnLock
   Set Filter To
   UnLock In F10c
   UnLock In F26c
   UnLock In F14c
Return

*> Buscar ubicación reposición.---------------------------------------
Procedure Bus_Ubica
Parameters c_CodArt, n_Cantid

Private f_Anterior, w_Anterior
Private c_UbiI, c_UbiF
Private c_ZonI, c_ZonF
Private c_CalI, c_CalF
Private c_FilI, c_FilF
Private c_PisI, c_PisF
Private c_ProI, c_ProF
Private n_ProI, n_ProF
Private l_Encontrado, l_Zona, l_Tamany
Private w_Mensaje
Private f_InAux2
Private c_Tipo_Al, c_TipPr, n_Peso, n_Volu, n_Coef
Private c_Tipo_Ml, c_Tipo_Mp, l_OpenF14, l_OpenF16, l_Exito

*>
*> Cuando la mercancia no va a picking. Retorna el codigo de 
*> ubicacion si la encuentra.
*>
*> Se necesitan tener definidas las variables :
*>    aubparmz[?]... Array de parametros de ubicacion.
*>

   *> Variable que contendra la ubicacion.----------------------------
   c_Ubi_Pick = ""

   *> Abrimos el fichero de articulos.--------------------------------
   Select F08c

   *> Buscamos el articulo.-------------------------------------------
   Seek c_CodArt

**********
   If EOF()
      Return
   EndIf
**********

   *> Guardamos datos comunes.----------------------------------------
   c_Tipo_Al = F08cTipAlm
   c_TipPr   = F08cTipPro
   c_Tipo_Ml = F08cMxLote
   c_Tipo_Mp = F08cUbiMix
   n_Peso    = F08cPesoKg * n_Cantid
   n_Volu    = F08cVolume * n_Cantid
   n_Coef    = F08cCoefRt
   c_Tamanyo = F08cTamAbi
   
   If n_Volu = 0
      Do PesVolAr With c_CodArt, n_Cantid, n_Peso, n_Volu
   EndIf

   ***Abrir Fichero de parametros de Ubicación.-----------------------
   *> Variables de formas de ubicacion.-------------------------------
   l_Tamany  = (aubparmz[6] = "N")
   l_Zona    = (aubparmz[7] = "N")

   *> Ubicaciones iniciales.------------------------------------------
   c_UbiI = _Alma + "         "
   c_UbiF = _Alma + "ZZZZZZZZZ"

   *> Buscamos las ubicaciones de la mercancia.-----------------------
   Do Busca_Zona In Lib_Cu00 With c_CodArt

   *> Buscamos la zona.-----------------------------------------------
*  Do Busca_ZnPk With c_CodAmo, c_CodArt, MainCodAlm

   *> Zona.-----------------------------------------------------------
   c_ZonI = SubStr(c_UbiI, 3, 2)
   c_ZonF = SubStr(c_UbiF, 3, 2)

   *> Calle.----------------------------------------------------------
   c_CalI = SubStr(c_UbiI, 5, 2)
   c_CalF = SubStr(c_UbiF, 5, 2)

   *> Fila.-----------------------------------------------------------
   c_FilI = SubStr(c_UbiI,7, 2)
   c_FilF = SubStr(c_UbiF,7, 2)

   *> Piso.-----------------------------------------------------------
   c_PisI = SubStr(c_UbiI,9, 2)
   c_PisF = SubStr(c_UbiF,9, 2)

   *> Profundidad.----------------------------------------------------
   c_ProI = SubStr(c_UbiI,11, 1)
   c_ProF = SubStr(c_UbiF,11, 1)

   *> Mensaje de espera.----------------------------------------------
   =WaitWindow("Buscando ubicación para reposición de "+AllTrim(c_CodArt))

   *> Activamos el fichero de ubicaciones.----------------------------
   Select F10c

   *> Ubicación por Picking Caótico.
   Set Order To Ind03    && Filtro por Picking="S"

   *>Añadimos F10cEstSal = "N" .------(PROSAN)-----------------------
   *>----------------------------------------------------------------

   *> Activamos el filtro por coef. rotacion.-------------------------
   **Parametros de Ubicación.-----------------------------------------
   If aubparmz[5] = 'S'

      *> Inicializa prioridades.--------------------------------------
      n_ProI = 999999
      n_ProF = 0

      *> Retorna las proximidades.------------------------------------
      Do Busca_Prox In Lib_Cu00 With n_Coef

      Set Filter To ((F10cResiKg-F10cOcupKg)>=n_Peso)  .And. ;
           F10cCanPal < F10cMaxPal  .And. ;
           Between(F10cPriori,n_ProF,n_ProI)  .And. ;
           Between(SubStr(F10cCodigo, 4,2), c_ZonI, c_ZonF)  .And. ;
           Between(SubStr(F10cCodigo, 7,2), c_CalI, c_CalF)  .And. ;
           Between(SubStr(F10cCodigo,10,2), c_FilI, c_FilF)  .And. ;
           Between(SubStr(F10cCodigo,13,2), c_PisI, c_PisF)  .And. ;
           Between(SubStr(F10cCodigo,16,1), c_ProI, c_ProF)  .And. ;
           F10cEstGen<>"O"  .And.  F10cEstEnt<>"S"  .And. ;
          (F10cTipPro==c_TipPr .Or. F10cTipPro=="    ").And. ;
          Left(F10cCodigo,2)=_Alma .And. F10cEstSal = "N"
   Else
      Set Filter To ((F10cResiKg-F10cOcupKg)>=n_Peso)  .And. ;
           F10cCanPal < F10cMaxPal  .And. ;
           Between(SuBStr(F10cCodigo, 4,2), c_ZonI, c_ZonF)  .And. ;
           Between(SubStr(F10cCodigo, 7,2), c_CalI, c_CalF)  .And. ;
           Between(SubStr(F10cCodigo,10,2), c_FilI, c_FilF)  .And. ;
           Between(SubStr(F10cCodigo,13,2), c_PisI, c_PisF)  .And. ;
           Between(SubStr(F10cCodigo,16,1), c_ProI, c_ProF)  .And. ;
           F10cEstGen<>"O"  .And.  F10cEstEnt<>"S"  .And. ;
          (F10cTipPro==c_TipPr .Or. F10cTipPro=="    ").And.;
          Left(F10cCodigo,2)=_Alma .And. F10cEstSal = "N"
   EndIf

   *> Busco el tip.almac, tip.prod, tam.palet.--------------------------
   Seek c_Tipo_Al + c_TipPr + c_Tamanyo   && +maincodalm
   Do While !Lock() .And. !EOF()
      Skip
      If !(F10cTipAlm + F10cTipPro + F10cTamUbi + Left(F10cCodigo,2)== ;
         c_Tipo_Al + c_TipPr + c_Tamanyo + _Alma)
         Go Bott
         Skip
       EndIf
   EndDo
         
   *> Bucle al fichero.-----------------------------------------------
   CTPr=c_TipPr
   Do While EOF()
      *> Reintento por tipo de producto ubicacion.--------------------
      If !Empty(c_TipPr)
         c_TipPr = "    "
         Seek c_Tipo_Al + c_TipPr + c_Tamanyo     && +maincodalm
         Do While !Lock() .And. !EOF()
            Skip
            If !(F10cTipAlm +F10cTipPro +F10cTamUbi +Left(F10cCodigo,2)==;
               c_Tipo_Al + c_TipPr + c_Tamanyo + _Alma)
               Go Bott
               Skip  
            EndIf
         EndDo      
         Loop
      EndIf

      *> Si es posible las de tama¤o mayor.---------------------------
      If l_Tamany
         l_Tamany  = .F.
         Set Near On
         Seek c_Tipo_Al + c_TipPr + c_Tamanyo
         Set Near Off
         Do While !Lock() .And. !EOF()
            Skip
            If !(F10cTipAlm==c_Tipo_Al).Or.!(Left(F10cCodigo,2)==_Alma)
               Go Bott
               Skip  
            EndIf
         EndDo
         Loop
      EndIf
      
      *> Si es posible distintas zonas.-------------------------------
      If l_Zona
         l_Zona = .F.
         c_UbiI = _Alma+"           "
         c_UbiF = _Alma+"ZZZZZZZZZ"  && Las mayores zonas.

         *> Zona.-----------------------------------------------------
         c_ZonI = SubStr(c_UbiI, 3, 2)
         c_ZonF = SubStr(c_UbiF, 3, 2)

         *> Calle.----------------------------------------------------
         c_CalI = SubStr(c_UbiI, 5, 2)
         c_CalF = SubStr(c_UbiF, 5, 2)

         *> Fila.-----------------------------------------------------
         c_FilI = SubStr(c_UbiI,7, 2)
         c_FilF = SubStr(c_UbiF,7, 2)

         *> Piso.-----------------------------------------------------
         c_PisI = SubStr(c_UbiI,9, 2)
         c_PisF = SubStr(c_UbiF,9, 2)

         *> Profundidad.----------------------------------------------
         c_ProI = SubStr(c_UbiI,11, 1)
         c_ProF = SubStr(c_UbiF,11, 1)

         *> Reintentamos la zona.-------------------------------------
         Go Top
         c_TipPr = CTPr
         Loop
      EndIf
      
      *> Salida del bucle.--------------------------------------------
      Exit
   EndDo

   If !EOF()
      c_Ubi_Pick = F10cCodigo
   EndIf
   Set Filter To
Return c_Ubi_Pick

Procedure Carga      
   *> Ordenaci¢n por Propietario y Art¡culo.--------------------------
   Select F16c
   Set Order To &Indice

   *> Flag de cantidad encontrada.------------------------------------
   SwCarga = .F.

   *> Si hay ocupación.-----------------------------------------------
   If Seek(c_Clave)
      Do While !EOF()  .And.c_Articulo == F16cCodArt  ;
                       .And. SwCarga = .F.     
         If Empty(c_Num_Lote) .Or. c_Num_Lote = F16cNumLot
            *> Controlamos que la ubicaci¢n no sea de Picking.--------
            Select F10c
            Seek F16c.F16cCodubi
            If F10cPickSn != "S" .And. F10CEstSal = "N" .And. Left(F10cCodigo,2)=_Alma
               Select F16c
               If Rlock()
                If F16cCanfis - F16cCanres > 0  .And.  F16cSitStk = c_Sit_stoc
                   SwCarga = .T.
                   c_NumDes = " "
                   w_UbiDes = " "
                   Do Genera
                EndIf
               EndIf
            EndIf
         EndIf
         Select F16c
         Skip
      EndDo
   EndIf
Return


Procedure Genera

   SwSaldo = .T.

   *> Comprobamos cantidad que podemos extraer.-----------------------
   Select F16c
   w_UbiEst = IIf(!Empty(w_UbiDes), w_UbiDes, F16cCodUbi)
   n_CanPos = F16cCanFis - F16cCanRes

   *> Si no venimos de Saldos.----------------------------------------
   If Empty(n_CanExtr)
      n_CanExtr = IIf(n_CantOri > n_CanPos, n_CanPos, n_CantOri)
      
      *> Actualizamos cantidad reservada.-----------------------------
      Replace F16cCanRes With F16cCanRes + n_CanExtr
   Else
      SwSaldo = .F.
      Select F26c
      Replace F26cSaldos With F26cSaldos - n_CanExtr 
   EndIf

*> Generar el movimiento f¡sico.--------------------------------------
   *> Nuevo número de orden.------------------------------------------
   c_Numero = Prc_nld()
      
   *> Si viene de Saldos.---------------------------------------------
   c_NumRes = IIf(!Empty(c_NumDes), c_NumDes, " ")    

   *> Sabemos si es pico.---------------------------------------------
   c_TipMov = IIF(n_CanExtr # F16c.F16cFctPal,"P","E")

   Select F26c
   Append Blank

   *> Generamos un registro.------------------------------------------
   Replace F26cNumOrd With c_Numero,;
           F26cNumPal With F16c.F16cNumPal,;
           F26cSizPal With F16c.F16cSizPal,;
           F26cTipMov With "2000",;
           F26cTipEnt With c_Tip_Ent,;
           F26cCodEnt With c_Num_Ent,;
           F26cNumPed With c_Num_Doc,;
           F26cTipDoc With c_Tip_Doc,;
           F26cNumLin With c_Num_Lin,;
           F26cNotaEs With F16c.F16cNotEnt,;
           F26cFechas With F16c.F16cFecEnt,;
           F26cEsPico With F16c.F16cEsPico,;
           F26cTipAmo With F16c.F16cTipEnt,;
           F26cCodAmo With F16c.F16cNumEnt,;
           F26cCodArt With f16c.F16cCodArt,;
           F26cSitStk With f16c.F16cSitStk,;
           F26cCanFis With n_CanExtr ,;
           F26cFecFab With F16c.F16cFecFab,;
           F26cFecCad With F16c.F16cFCaduc,;
           F26cFecCal With F16c.F16cFCalda,;
           F26cNumLot With F16c.F16cNumLot,;
           F26cUniPac With F16c.F16cFctSrv,;
           F26cPacCaj With F16c.F16cFctEnv,;
           F26cCajPal With F16c.F16cFctPal,;
           F26cRutHab With "",;
           F26cCodOpe With "",;
           F26cNumAna With F16c.F16cNumAna,;
           F26cFecMov With Date(),;
           F26cUbicaz With " ",;
           F26cUbiOri With w_UbiEst,;
           F26cNumDes With c_NumRes,;
           F26cTipoMv With c_TipMov,;
           F26cSaldos With 0,;
           F26cCodAlm With _Alma

   *> Lo hacemos si no viene de saldos.-------------------------------
   If Swsaldo = .T.
      *> Se define el array de datos.------------------------------------
      Declare a_datos(31)

      *> Inicializamos los datos.----------------------------------------
      a_datos(01) = F16c.F16cnumpal
      a_datos(02) = F16c.F16csizpal
      a_datos(03) = "2000"
      a_datos(04) = c_tip_ent
      a_datos(05) = c_num_ent
      a_datos(06) = c_num_doc
      a_datos(07) = c_tip_doc
      a_datos(08) = f16c.f16cnotent
      a_datos(09) = f16c.f16cfecent
      a_datos(10) = f16c.f16cespico
      a_datos(11) = F16c.F16cTipEnt
      a_datos(12) = F16c.F16cNumEnt
      a_datos(13) = f16c.f16ccodart
      a_datos(14) = f16c.f16csitstk
      a_datos(15) = n_canExtr 
      a_datos(16) = f16c.f16cfecfab
      a_datos(17) = f16c.f16cfcaduc
      a_datos(18) = f16c.f16cfcalda
      a_datos(19) = f16c.f16cnumlot
      a_datos(20) = f16c.f16cfctsrv
      a_datos(21) = f16c.f16cfctenv
      a_datos(22) = f16c.f16cfctpal
      a_datos(23) = ""
      a_datos(24) = ""
      a_datos(25) = f16c.f16cnumana
      a_datos(26) = DATE()
      a_datos(27) = " "            
      a_datos(28) = f16c.f16ccodubi
      a_datos(29) = c_numero
      a_datos(30) = f16c.f16clinpro
      a_datos(31) = _Alma
      
      *> Generamos el movimiento.----------------------------------------
      n_status = 0
      Do GenMov in Lib_Ca00 With a_datos, n_Status

      *> Actualizamos situacion reservada.--------------------------
      L_Resultad=.f.
      Do actualst In Lib_ca00 with A_Datos(13),A_datos(15),"4000",.T.,l_Resultad

      *> Actualizamos situacion que corresponda.--------------------
      L_Resultad=.f.
      Do actualst In Lib_ca00 with A_Datos(13),A_datos(15),A_datos(14),.F.,l_Resultad

   EndIf
Return

Procedure Picking
   *> Comenzamos la busqueda.-----------------------------------------
   *> Primero buscamos en saldos.-------------------------------------
   *> Utilizamos orden propietario+artículo.--------------------------
   Swpico =.F.
   Select F26c
   Set Order to Ind04
   Go top

   If Seek(c_articulo)
      Do While !EOF() .And.F26cCodArt == c_Articulo .And. Swpico =.F.
         If F26cSaldos > 0 .And. Left(F26cUbiOri,2)=_Alma
            If Empty(c_num_lote) .OR. C_num_lote = F26cNumLot
             If Rlock()
               *> Sabremos la cantidad que extraemos.--------------------
               n_CanExtr = IIF(n_Cantori > F26cSaldos,F26cSaldos,n_Cantori)
               Swpico = .T. 
               c_numdes = F26cNumOrd
               w_ubides = F26cUbiCaz 
               Select F16c
               Set order to Ind01
               Seek f26c.f26cubiori+f26c.f26cnumpal+F26c.F26ccodart
               Do Genera
             EndIf 
            EndIf
         Endif
         Select F26c
         Skip
      EndDo    
   EndIf       
      
   *> Si no hemos encontrado en saldos.-------------------------------
   *> Buscaremos en ubicaciones físicas.------------------------------
   If Swpico = .F. 
      *> Ordenaci¢n por Propietario y Art¡culo.-----------------------
      Select F16c
      Set order to &Indice

      *> Si hay ocupaci¢n.--------------------------------------------
      If Seek(c_Clave)
         Do While !EOF() .AND. (c_articulo == F16cCodArt) .AND. Swpico = .F. 
            If Empty(c_num_lote) .OR. C_num_lote = F16cNumLot
               *> Controlamos que la ubicaci¢n sea de Picking.--------------
               Select F10c
               Seek F16c.F16cCodUbi
               If F10cPickSn = "S" .AND. F10CEstSal = "N" .And. Left(F10cCodigo,2)=_Alma                     
                  Select F16c
                  If F16cCanfis - F16cCanres > 0 .AND. F16cSitStk = c_Sit_Stoc
                   If Rlock()
                     c_numdes = " "
                     w_ubides = " " 
                     Swpico = .T. 
                     Do Genera
                   EndIf
                  EndIf      
               EndIf
            EndIF  
            Select F16c
            Skip
         EndDo   
      EndIf
   EndIf

   *> Para hacer reposici¢n.------------------------------------------
   *> Si no hemos encontrado y no venimos de Carga.-------------------
   *> buscamos en carga para hacer reposici¢n.------------------------
   If SwCarga = .T. .AND. Swpico = .F.
      *> buscamos y si no esta vamos a carga.-------------------------
      Select F12c
      If Seek(c_articulo+_Alma)
         *> Igualamos la variable de ubicaci¢n de reposici¢n.---------
         c_UbiRepos = F12cUbicpk    
         CanPic     = F12cCantPk
 
         *> Si Esta vacia la ubicaci¢n buscamos una para reponer.-----
         If Empty(c_UbiRepos)
            c_UbiRepos = Bus_Ubica(C_Articulo, n_Cantori)
         EndIf
            
         Select F16c
         Set order to Ind01
         Seek c_UbiRepos
         If (F16cCanFis-F16cCanres) > 0
            c_UbiRepos = " "
         EndIf   

         *> Si no esta vacia hacemos reposici¢n.----------------------
         If !Empty(c_UbiRepos)
            *> Buscamos en ocupaciones.----------------------------------
            Select F16c
            Set order to &Indice
            If Seek (c_Clave)
               Do While !eof() .And. c_articulo = F16cCodArt .And. Swpico = .F.
                  If Empty(c_num_lote) .OR. C_num_lote = F16c.F16cNumLot
                   If Rlock()
                     *> Controlamos que la ubicaci¢n sea de Picking.--------------
                     Select F10c
                     Seek F16c.F16cCodUbi
                     If F10cPickSn = "N" .AND. F10CEstSal = "N" .And. Left(F10cCodigo,2)=_Alma                     
                        If (F16c.F16cCanFis-F16c.F16cCanres) > 0 .and. F16c.F16cSitStk = c_Sit_stoc

                          *> Si las ubicaciones son iguales.------------------------
                          If c_UbiRepos # F16c.F16cCodUbi  

                           *> Si encontramos todo lo necesario.----------------------
                           Swpico = .T. 
                           Do GenRes
                          Else
                           Swpico = .T. 
                           Do Carga
                          EndIf  
                        EndIf 
                     Endif
                   EndIf
                  EndIf
                  Select F16c
                  Skip
               EndDo

            EndIf
         Else
            Do Carga
         EndIf
      Else
         Do Carga
      EndIf
   EndIf
Return


Procedure GenRes

   *> Comprobamos cantidad que podemos extraer.-----------------------
   Select F16c
   w_ubides = F16cCodUbi 
   n_Canpos =  IIF((F16cCanfis - F16cCanres) > CanPic,CanPic,F16cCanfis - F16cCanres)
   n_CanExtr = IIF(n_Cantori > n_Canpos,n_Canpos,n_Cantori)

   *> Actualizamos cantidad reservada.--------------------------------
   Replace F16cCanres With F16cCanres + n_canpos

   *> Generar El movimiento f¡sico.-----------------------------------
   *> Nuevo número de orden.------------------------------------------
   c_numero = prc_nld()
      
   =WaitWindow("Generando Reposición")

   *> Generar Movimiento de reposici¢n en F26c y F14c.----------------
   Select f26c
   APPEND BLANK

   *> Generamos un registro.------------------------------------------
   REPLACE f26cnumord With c_numero,;
           f26cnumpal With f16c.f16cnumpal,;
           f26csizpal With f16c.f16csizpal,;
           f26ctipmov With "3000",;
           f26ctipent With c_tip_ent,;
           f26ccodent With c_num_ent,;
           f26cnumped With c_num_doc,;
           f26ctipdoc With c_tip_doc,;
           f26cnumlin With c_num_lin,;
           f26cnotaes With f16c.f16cnotent,;
           f26cfechas With f16c.f16cfecent,;
           f26cespico With f16c.f16cespico,;
           f26ctipamo With "",;
           f26ccodamo With "",;
           f26ccodart With f16c.f16ccodart,;
           f26csitstk With f16c.f16csitstk,;
           f26ccanfis With n_canpos,;
           f26cfecfab With f16c.f16cfecfab,;
           f26cfeccad With f16c.f16cfcaduc,;
           f26cfeccal With f16c.f16cfcalda,;
           f26cnumlot With f16c.f16cnumlot,;
           f26cunipac With f16c.f16cfctsrv,;
           f26cpaccaj With f16c.f16cfctenv,;
           f26ccajpal With f16c.f16cfctpal,;
           f26cruthab With "",;
           f26ccodope With "",;
           f26cnumana With f16c.f16cnumana,;
           f26cfecmov With DATE(),;
           f26cubicaz With c_ubirepos,;
           f26cubiori With f16c.f16ccodubi,;
           f26cnumdes With " ",;
           f26cTipoMv With "R",;
           f26csaldos With (n_canpos-n_canextr),;
           F26cCodAlm With _alma

   *> Se define el array de datos.------------------------------------
   DECLARE a_datos(31)

   *> Inicializamos los datos.----------------------------------------
   a_datos(01) = f16c.f16cnumpal
   a_datos(02) = f16c.f16csizpal
   a_datos(03) = "3000"
   a_datos(04) = c_tip_ent
   a_datos(05) = c_num_ent
   a_datos(06) = c_num_doc
   a_datos(07) = c_tip_doc
   a_datos(08) = f16c.f16cnotent
   a_datos(09) = f16c.f16cfecent
   a_datos(10) = f16c.f16cespico
   a_datos(11) = ""
   a_datos(12) = ""
   a_datos(13) = f16c.f16ccodart
   a_datos(14) = f16c.f16csitstk
   a_datos(15) = n_canPos 
   a_datos(16) = f16c.f16cfecfab
   a_datos(17) = f16c.f16cfcaduc
   a_datos(18) = f16c.f16cfcalda
   a_datos(19) = f16c.f16cnumlot
   a_datos(20) = f16c.f16cfctsrv
   a_datos(21) = f16c.f16cfctenv
   a_datos(22) = f16c.f16cfctpal
   a_datos(23) = ""
   a_datos(24) = ""
   a_datos(25) = f16c.f16cnumana
   a_datos(26) = DATE()
   a_datos(27) = c_UbiRepos            
   a_datos(28) = f16c.f16ccodubi
   a_datos(29) = c_numero
   a_datos(30) = f16c.f16clinpro
   a_datos(31) = _Alma

   *> Generar Movimiento que cuelga de la reposici¢n.-----------------
   Select f26c
   APPEND BLANK

   c_numdes = C_numero 

   *> Generar El movimiento f¡sico.-----------------------------------
   *> Nuevo número de orden.------------------------------------------
   c_numero = prc_nld()

   *> Sabemos si es pico.---------------------------------------------
   c_TipMov = IIF(n_CanExtr # F16c.F16cFctPal,"P","E")

   *> Generamos un registro.------------------------------------------
   REPLACE f26cnumord With c_numero,;
           f26cnumpal With f16c.f16cnumpal,;
           f26csizpal With f16c.f16csizpal,;
           f26ctipmov With "2000",;
           f26ctipent With c_tip_ent,;
           f26ccodent With c_num_ent,;
           f26cnumped With c_num_doc,;
           f26ctipdoc With c_tip_doc,;
           f26cnumlin With c_num_lin,;
           f26cnotaes With f16c.f16cnotent,;
           f26cfechas With f16c.f16cfecent,;
           f26cespico With f16c.f16cespico,;
           f26ctipamo With "",;
           f26ccodamo With "",;
           f26ccodart With f16c.f16ccodart,;
           f26csitstk With f16c.f16csitstk,;
           f26ccanfis With n_canExtr,;
           f26cfecfab With f16c.f16cfecfab,;
           f26cfeccad With f16c.f16cfcaduc,;
           f26cfeccal With f16c.f16cfcalda,;
           f26cnumlot With f16c.f16cnumlot,;
           f26cunipac With f16c.f16cfctsrv,;
           f26cpaccaj With f16c.f16cfctenv,;
           f26ccajpal With f16c.f16cfctpal,;
           f26cruthab With "",;
           f26ccodope With "",;
           f26cnumana With f16c.f16cnumana,;
           f26cfecmov With DATE(),;
           f26cubicaz With " ",;
           f26cubiori With c_ubirepos,;
           f26cnumdes With c_numdes,;
           f26cTipoMv With c_TipMov,;
           f26csaldos With 0       ,;          
           f26cCodAlm With _Alma

   *> Generamos el movimiento.----------------------------------------
   n_status = 0
   Do GenMov in Lib_Ca00 With a_datos, n_Status

   *> Actualizamos situación reservada.--------------------------
   L_Resultad=.f.
   Do actualst In Lib_ca00 with A_Datos(13),n_canExtr,"4000",.T.,l_Resultad

   *> Actualizamos situacion que corresponda.--------------------
   L_Resultad=.f.
   Do actualst In Lib_ca00 with A_Datos(13),n_CanExtr,A_datos(14),.F.,l_Resultad

Return

*Confirmación de reposiciones.-----------------------------------------
Procedure ConfRep
   Select F26c 
   Set Filter to !Empty(f26cUbicaz).AND.!EMPTY(f26cUbiori) 

   Go Top
   Do While !Eof()

         *> Guardamos el número de palet.-----------------------------
         C_numpalet = f26cnumpal
         C_CodArt   = F26cCodArt
         C_Cantid   = F26cCanfis
         Reg26 = Recno()
         Set filter to
         *> Buscamos el palet en el f14c.-----------------------------
         Select f14c

         *>Posible fallo....9/2/96.-----------------------------------
         Locate For F14cNumPal == c_NumPalet  .And.  ;
                    !Empty(F14cUbiOri)  .And.  !Empty(F14cUbicaz) 
            
         IF !EOF()           
            lRet = .f.
            lReg = RECNO()
                               
            *> Si se confirma.----------------------------------------
            do prc_ConF in Lib_Ca00 with lReg, lRet
               IF !lRet
                  =F3_Sn(1,4,"Error en la confirmación de la reposición "+c_numpalet)
               ENDIF
         ELSE
            =F3_Sn(1,4,"El palet "+c_numpalet+" no se encuentra")
         ENDIF

      *> Avanzamos el registro.---------------------------------------
      Select F26c
      SET filter to !Empty(f26cUbicaz).AND.!EMPTY(f26cUbiori) 
      Set Order To Codigo
      GOTO Reg26
      Skip
   ENDDO

Return

Procedure BegConf

    *>Bucle para confirmar mas de uno.--------------------------------
    Select ConDoc
    Go Top
    Do While !Eof()

       *>si Esta marcado para confirmar.-------------------------------
       If ConDoc.Marca=1

          *>Genero F27.--------------------------------------------------
          Do GenAlb With ConDoc.F24cTipDoc,ConDoc.F24cNumDoc,Condoc.F24cTipEnt,ConDoc.F24cCodEnt 

          *>Confirmo movimiento.-----------------------------------------
          Do Confirma With ConDoc.F24cTipDoc,ConDoc.F24cNumDoc        

          *>Genero fichero de traspaso.----------------------------------
          *>Do ExpSal With ConDoc.F24cTipDoc,ConDoc.F24cNumDoc in Traspaso
          *> Ejecución remota.
          *>Run Rsh 193.147.157.194 -l Gestion \usr\compras\salidas.sh 

          *> ACTUALIZO PLAN DIAS -----------------------------------------------------------
*>        C_Texto = 'Documento ' + Alltrim(ConDoc.F24cTipDoc) + ' ' + Alltrim(ConDoc.F24cNumDoc)
*>        If F3_Sn(2,3,"Se va a realizar la actualización de Plan Dias",C_Texto,"Desea continuar ?")
          
             *> Recupero el Nº de Albarán
             Do FSelect With 'F27c', 'IND01', '0'
             Seek ConDoc.F24cTipDoc+ConDoc.F24cNumDoc
             If Found()
                C_NumAlb = F27cNumAlb
                
                Do FSelect With 'F27l', 'CODIGO', '0'
                Seek C_NumAlb
                Do While !Eof() .and. F27lNumAlb=C_NumAlb
                
                   *> Obtengo el número de traspaso
                   Do FSelect With 'F00c', 'CODIGO', '0'
                   Seek 'TNU' + 'TRP'
                   If Found()
                      C_NumTrp = F00c.F00cNumero
                      C_NumTrp = Right('0000000000' + Alltrim(Str(F00c.F00cNumero)), 10)
                      Replace F00c.F00cNumero With F00c.F00cNumero + 1
                   Else
                      C_NumTrp = '0000000000'
                   EndIf
                   
                   *> Genero movimiento de SALIDA en histórico de Traspasos
                   Do FSelect With 'EnvPla', 'CODIGO', '0'
                   Append Blank
                   
                   Replace EnvError  With 'S'
                   Replace EnvEntSal With 'S'
                   Replace EnvNumTrp With C_NumTrp
                   
                   Replace EnvTipMov With '2000'
                   
                   Replace EnvReint  With 1
                   
                   *> El Código de Artículo debe ser el de PLAN DIAS
                   _Ok=f3_seek("F08c", F27l.F27lCodart, '', '', '', 'CODIGO')
                   Replace EnvCodArt With IIf(_Ok, F08c.F08cCodPla, '')
                   
                   Replace EnvCantid With F27l.F27lCanSer
                   Replace EnvFecha  With Date()

                   Replace EnvAnyCon With ''
                   Replace EnvTipCon With ''
                   Replace EnvNumCon With ''
                   Replace EnvCodPro With ''
                   Replace EnvNumPed With ''
                   Replace EnvNumAlb With ''

                   *> Ejecución remota.
 *> Run Rsh 193.147.157.194 -l Gestion \usr\compras\salidas.sh F27l.F27lCodart F27l.F27lCanSer Date() 

                   *> Actualizo el histórico de traspasos, según si hay o no error
                   *> Select EnvPla
                   *> Do FSelect With 'EnvPla', 'IND01', '0'
                   *> Replace EnvError With IIf(Error, 'S', 'N')
                   
                   Select F27l
                   Skip
                EndDo
             EndIf
          
*>        EndIF

       EndIf
       Select Condoc
       Skip
    EndDo   

Return

*>Confirmación de documentos.------------------------------------------
Procedure Confirma
PARAMETERS c_tip_doc, c_num_doc
PRIVATE c_num_pal, Pos_palet, c_num_ord, Ubidesti, l_Result
*> Confirmacion de la mercancia.

   Select F14c

   *> Buscamos lineas para el pedido.---------------------------------
   Select f14c
   Set order to Tag Ind02
   SEEK c_tip_doc+c_num_doc
*  LOCATE FOR (f14ctipdoc = c_tip_doc) .AND. (f14cnumped = c_num_doc)
   If !Found()
      =F3_Sn(1,4,"No Existen Mov. Pendientes del pedido Nº "+C_num_doc)
      Return
   EndIf   

   *> Si las encontramos.---------------------------------------------
   DO WHILE !EOF() .AND. (f14ctipdoc = c_tip_doc) .AND. (f14cnumped = c_num_doc)
   
      *> Confirmamos el movimiento.-----------------------------------
      LStock = .T.   
      c_num_pal  = f14cnumpal
      Pos_palet  = Recno()
      C_articul  = F14ccodart
      c_num_ord  = f14cnumdes
      Ubidesti   = F14cubicaz
      n_canF14   = F14cCanFis

      *>Para saber si es un GFH.--(PROSAN)----------------------------------------
      Declare A_Datos(31)
      Scat to A_Datos(31)
      C_TipEnt   = F14cTipEnt
      C_CodEnt   = F14cCodEnt 
      *>--(PROSAN)----------------------------------------------------------------

      If !Empty(F14cUbiCaz).And.!Empty(F14cUbiOri)
         LStock = .F.
      Endif
      store .f. to l_Result

      do Prc_ConF in Lib_Ca00 with Pos_Palet, l_Result
      
      *> Buscamos lineas para el pedido.------------------------------
      Select f14c
      IF l_Result

         If LStock = .T.
            *> Actualizamos situacion reservada.--------------------------
            L_Resultad=.f.
            Do actualst In Lib_ca00 with C_articul,n_Canf14,"4000",.F.,l_Resultad

            *> Actualizamos situacion Pend. Cliente.----------------------
*           L_Resultad=.f.
*           Do actualst In Lib_ca00 with c_Articul,n_CAnF14,"7500",.F.,l_Resultad

            *>.--------------------------------------------------------------
            *>Deberiamos hacer entrada en ubicación de GFH.----(PROSAN)------ 
            *>.--------------------------------------------------------------
*>            If Alltrim(C_TipEnt) = "GFH"
*>               Select F01c
*>               Seek C_TipEnt+C_CodEnt
*>               C_CodZon = F01cCodZon
*>               C_CodUbi = _alma+C_CodZon+"0000000"
*>               Select F12c
*>               Seek C_Articul+_Alma+C_CodZon               
*>               If Found()
*>                  C_CodUbi = F12cUbicPk
*>               EndIf 
         
*>               *>Inicializamos el array del ubicaciónes.--------------------------
*>               A_Datos(27)=C_CodUbi
*>               A_datos(28)=""
               
*>               *> Generamos el movimiento.----------------------------------------
*>              n_status = 0
*>              Do GenMov in Lib_Ca00 With a_datos, n_Status

*>               *> Actualizamos situacion disponible.--------------------------
*>               L_Resultad=.f.
*>               Do actualgf In Lib_ca00 with _Alma,C_CodZon,A_Datos(13),A_datos(15),"1000",.T.,l_Resultad
 
*>               *> Actualizamos situacion que corresponda.--------------------
*>               L_Resultad=.f.
*>               Do actualgf In Lib_ca00 with _Alma,C_CodZon,A_Datos(13),A_datos(15),"7500",.F.,l_Resultad

*>               Select f14c
*>               LRet = .f.
*>               NReg = Recno()
*>               Do Prc_Conf in Lib_Ca00 with NReg, LRet
*>               IF !LRet
*>                  =F3_Sn(1,4,"Error en la confirmación automatica del palet "+a_datos(01))
*>               ENDIF

*>            EndIf
            *>.--------------------------------------------------------------
            *>Deberiamos hacer entrada en ubicación de GFH.----(PROSAN)------ 
            *>.--------------------------------------------------------------

         EndIf

         Set order to IND02
         SEEK c_tip_doc+c_num_doc
*        LOCATE FOR (f14ctipdoc = c_tip_doc) .AND. (f14cnumped = c_num_doc)

      ELSE   
         =F3_Sn(1,4,"Error en la confirmación del palet : "+C_Num_Pal+"Articulo "+C_articul)
         exit
      ENDIF
   EndDo

   If l_Result
      Select F24c
      If Seek(c_Tip_Doc + c_Num_Doc)

         *> Añadimos la A de Confirmada.------------------------------
         Replace F24cRestSn With "A"
      EndIf 
   EndIf

RETURN

*>Procesos De Ubicación.----------------------------------------------
Procedure Ubica
PRIVATE n_total, c_cantidad, c_tamubica, codigo_ubi, c_ubifor
PRIVATE c_cod_pal, c_articulo, Reg_Ant
*> Proporciona la ubicacion a cada una de las lineas

   *> Declara el array.-----------------------------------------------
   DECLARE aubparmz[17]
   DO carga_datos IN prment.prg

   *> Procedemos.-----------------------------------------------------
   Select EntMAt
   GOTO TOP
   DO WHILE !EOF()

      *> Guardamos datos.---------------------------------------------
      Pico     = EntMat.PicoSn
      c_articu = EntMat.CodArt
      c_tamubi = EntMat.TamAbi
      n_total  = EntMat.CanFis
      
      c_ubifor = IIf(Empty(EntMat.UbiFor),'',_ALMA+EntMat.UbiFor)
      c_Cod_Pa = EntMat.NumPal
      codi_ubi = ""          && Retorno de la ubicacion destino
      Txt_Err  = ""          && Texto de porque no se puede ubicar
      Blq_Ubi  = .t.         && Si bloquea la ubicacion o no
      
      LAutom = .f.

      Reg_Ant = RecNo()      && Guardar puntero en Normal (lo pierde).

      Do Prc_Ubi in Lib_Cu00 With c_Articu,c_TamUbi,n_Total,c_Ubifor,c_Cod_Pa,Pico,LAutom,Blq_Ubi,Codi_Ubi,Txt_err

      If Empty(Codi_ubi)
         =F3_sn(1,4,"Imposible ubicar el palet "+C_Cod_Pa)
      Endif   

      *> Grabamos la ubicacion.---------------------------------------
      Select EntMat
      Go Reg_Ant         && Recuperar puntero en Normal.
      REPLACE EntMat.UbiFor WITH Right(codi_ubi,9)

      *> Avanzamos el puntero.----------------------------------------
      Skip
   ENDDO

   *> Posicionamos el puntero en el fichero de lineas.----------------
   Select EntMat
   GOTO TOP

RETURN 

PROCEDURE aceptar
PRIVATE n_status, puerto, l_okport
*> Se aceptan las ubicaciones

   *> Declara el array.-----------------------------------------------
   DECLARE aubparmz[17]
   DO carga_datos IN prment.prg

   *> Se define el array de datos.------------------------------------
*   DECLARE a_datos(31)

   *> Si tenemos que imprimir.----------------------------------------
   IF (aubparmz[11] = "S") .OR. (aubparmz[12] = "S")
       Select EntMat
       Go top
       Set Relation to CodArt into F08c
       _Label1 = "Listado Entrada de Material"
       Do form st3imp2 with _Label1,'EntMatl1','EntMat'
       Set relation to
   ENDIF

   *> Abrimos el fichero movimientos pendientes.----------------------
*   DO fselec WITH "f14c","codigo","0"
   
   *> Crea los movimientos.-------------------------------------------
   Select EntMat
   GOTO TOP
   DO WHILE !EOF()
      if Empty(UbiFor)
         Skip
         loop
      Endif   
      
      *> Inicializamos los datos.-------------------------------------
      a_datos(01) = NumPal
      a_datos(02) = TamAbi
      a_datos(03) = "1000"
      a_datos(04) = "PROV"
      a_datos(05) = CodEnt
      a_datos(06) = NumPed
      a_datos(07) = TipDoc
      a_datos(08) = NumEnt
      a_datos(10) = PicoSn
      a_datos(11) = "PROV"
      a_datos(12) = CodEnt
      a_datos(13) = CodArt
      a_datos(15) = CanFis
      a_datos(16) = {  .  .  }
      a_datos(17) = FecCad
      a_datos(18) = {  .  .  }
      a_datos(19) = NumLot
      a_datos(23) = ""
      a_datos(25) = ""     
      a_datos(26) = {  .  .  }
      a_datos(27) = CodAlm+UbiFor
      a_datos(28) = ""
      a_datos(29) = ""
      a_datos(30) = ""
      a_datos(31) = CodAlm

      Do Case
         Case Upper(_XWin)='ENTMAT'
           a_datos(09) = m.Fecha
           a_datos(14) = m.Codigo
           a_datos(20) = m.unipac
           a_datos(21) = m.paccaj
           a_datos(22) = m.cajpal
           a_datos(24) = m.CodOpe
           
         Case Upper(_XWin)='PALENT'
           a_datos(09) = Date()
           a_datos(14) = '1000'
           
           *> Recupero factores de ficha de artículo (Entidad + Art.Proveedor)
           _Ok=f3_seek("F08c", a_datos(5)+a_datos(13), '', '', '', 'IND03')
           a_datos(20) = F08c.F08cUniPac
           a_datos(21) = F08c.F08cPacCaj
           a_datos(22) = F08c.F08cCajPal
           
           a_datos(24) = ''
      EndCase 

      *> Generamos el movimiento.-------------------------------------
      n_status = 0
      Do GenMov in Lib_Ca00 with a_datos, n_Status

      *> Actualizamos situacion de stock Pendiente de ubicar.(Sumamos).---
      L_Resultad=.f.
      Do actualst In Lib_Ca00 with A_Datos(13), A_Datos(15), "5000", .T.,l_Resultad
  
      IF n_status = 2
         =F3_Sn(1,1,"Sit. stock erronea en palet "+a_datos(01))
      ENDIF

      *> Desglose automatico.-----------------------------------------
      IF aubparmz[10] = "S"

      ENDIF

      *> Imprimir etiqueta bulto.-------------------------------------
      IF aubparmz[11] = "S"

      ENDIF

      *> Confirmacion automatica.-------------------------------------
      IF aubparmz[09] = "S" 
         Select f14c
         LRet = .f.
         NReg = Recno()
         Do Prc_Conf in Lib_Ca00 with NReg, LRet
         IF !LRet
            =F3_Sn(1,4,"Error en la confirmación automatica del palet "+a_datos(01))
         ENDIF

         *> Actualizamos situacion de stock Pendiente de ubicar.(ResTamos).---
         L_Resultad=.f.
         Do actualst In Lib_ca00 with A_Datos(13), A_Datos(15), "5000", .F.,l_Resultad

         *> Actualizamos situación de stock EnTrada.(Sumamos).---
         L_Resultad=.f.
         Do actualst In Lib_ca00 with A_Datos(13), A_Datos(15), A_Datos(14), .T.,l_Resultad

         *> ACTUALIZACIÓN DE FICHERO DE ENVIOS
                
         *> Obtengo el número de traspaso
         Do FSelect With 'F00c', 'CODIGO', '0'
         Seek 'TNU' + 'TRP'
         If Found()
            C_NumTrp = F00c.F00cNumero
            C_NumTrp = Right('0000000000' + Alltrim(Str(F00c.F00cNumero)), 10)
            Replace F00c.F00cNumero With F00c.F00cNumero + 1
         Else
            C_NumTrp = '0000000000'
         EndIf
                   
         *> Genero movimiento de ENTRADA en histórico de Traspasos
         Do FSelect With 'EnvPla', 'CODIGO', '0'
         Append Blank
                   
         Replace EnvError  With 'S'
         Replace EnvEntSal With 'E'
         Replace EnvNumTrp With C_NumTrp
                   
         Replace EnvTipMov With '1000'
                   
         Replace EnvReint  With 1
                   
         *> El Código de Artículo debe ser el de PLAN DIAS
         _Ok=f3_seek("F08c", a_datos(5)+a_datos(13), '', '', '', 'IND03')
     
         If _Ok
            Replace EnvCodArt With IIf(_Ok, F08c.F08cCodPla, '')
                   
            Replace EnvNumPed With EntMat.NumPed
            Replace EnvNumAlb With EntMat.AlbPro
            Replace EnvCantid With EntMat.CanFis
            Replace EnvFecha  With Date()

            Replace EnvAnyCon With ''
            Replace EnvTipCon With ''
            Replace EnvNumCon With ''
            Replace EnvCodPro With ''
         EndIf
      ENDIF
      
      Do Case
      
      Case Upper(_XWin) = 'ENTMAT'
         *> Actualizo en Nº de albarán en F18c
         Do FSelect With 'F18c', 'CODIGO', '0'
         Seek m.TipDes + m.AnyCon + m.TipoCon + m.NumCon
         If Found()
            Replace F18cAlbOri With m.AlbPro
         EndIf 
         
      Case Upper(_XWin) = 'PALENT'
         *> Actualizo Línea del fichero de recuento F18n y la cantidad pendiente a paletizar
         Do FSelect With 'F18n', 'IND01', '0'
         Seek EntMat.NumEnt + EntMat.NumPed + EntMat.LinPed + EntMat.LinRec
         If Found()
            Replace F18nCanPal With F18nCanPal - EntMat.CanFis
            *> Replace F18nEstado With IIf(F18nCanPal=0, '3', '2')
         EndIf 
         
      EndCase
      
      *> Seleccionamos y avanzamos el fichero de lineas.--------------
      Select EntMat
      Skip
   ENDDO

   *> Actualizo Línea del fichero de recuento F18m
   *> Si no hay ninguna línea diferente de '3', eso indica que todas estan totalmente paletizadas
*>   IF Upper(_XWin) = 'PALENT'
*>      Do FSelect With 'F18n', 'CODIGO', '0'
*>      Count for F18nNumEnt=EntMat.NumEnt .and. F18nEstado<>'3' to N_Total
*>      If N_Total = 0
*>         Do FSelect With 'F18m', 'CODIGO', '0'
*>         Seek EntMat.NumEnt
*>         If Found()
*>            Replace F18mEstado With '3'
*>         EndIf 
*>      EndIf 
*>    EndIf 

   *> Cerramos el puerto.---------------------------------------------
   IF ((aubparmz[11] = "S") .OR. (aubparmz[12] = "S")) 
*      =FPUTS(Puerto,"#FC")
*      =FCLOSE(Puerto)
   ENDIF

   *> Variable de control, en rutina de impresión de etiquetas.
   SelImp=.T.

   *> Imprimir etiqueta palet.-------------------------------------
   IF F08c.F08cEtiPal = "S"
   
      Select EntMat
      Go Top
      
      Select F08c
      Seek EntMat.CodArt
   
      Select EntMat
      Go Top
      Do While !Eof()
         If !Empty(UbiFor)
         
              NumEti = "1"
              DO ImpPro With F08c.F08cCodigo, NumEti In Imprime
              
         EndIf 
         Select EntMat
         Skip
      EndDo   

   ENDIF

   *> Imprimir etiqueta caja.-------------------------------------
   IF F08c.F08cEtiCaj = "S" 
   
      Select EntMat
      Go Top
      
      Select F08c
      Seek EntMat.CodArt
   
      Select EntMat
      Go Top
      Do While !Eof()
         If !Empty(UbiFor)
           
            Do Case
            Case _XWin='ENTMAT'
              NumEti=  Str(Alltrim(Ceiling(CanFis/m.UniVen*m.UniPac*m.PacCaj)))
            Case _XWin='PALENT'
              NumEti=  Str(Alltrim(Ceiling(CanFis/F08c.F08cUniVen*F08c.F08cUniPac*F08c.F08cPacCaj)))
            EndCase
            
            DO ImpPro With F08c.F08cCodigo, NumEti In Imprime
   
         EndIf 
           
         Select EntMat
         Skip
      EndDo
 
   ENDIF
RETURN

PROCEDURE cancelar
*> Se procesa el fichero y se desubica

   *> Declara el array.-----------------------------------------------
   DECLARE aubparmz[17]
   DO carga_datos IN prment.prg

   *> Retornamos al fichero de líneas de palets.----------------------
   Select EntMat
   GOTO TOP
   DO WHILE !EOF()
      if Empty(UbiFor)
         Skip
         loop
      Endif   

      *> Desubicamos.-------------------------------------------------
      Store 0 to n_Peso, N_Volu
      Do PesVolAr in Lib_Ca00 with CodArt, CanFis, n_Peso, N_Volu
      l_Bi=.f.
      Select EntMat
      CodUbi = CodAlm+UbiFor
      do DesUbi in Lib_Ca00 with CodUbi, n_Peso, n_Volu, 1, l_bi

      *> Avanzamos el puntero.----------------------------------------
      Select EntMat
      Skip

   ENDDO
      
RETURN

Procedure ConfMan
*> Se Confirman los movimientos.--------------------------------------
PRIVATE c_numpalet, l_openf20, l_openf16, l_openf26, n_Cant7000, c_OrdF26c
*> Confirma la lineas seleccionadas.

   *> Bucle para procesar las líneas.---------------------------------
   Select ConMov
   GOTO TOP
   DO WHILE !EOF()

      *> Controlamos si se tiene que confirmar.-----------------------
      IF Marca=1
         *> Guardamos el número de palet.-----------------------------
         c_numpalet = f14cnumpal
         C_Numord   = F14cNumDes

         *> Buscamos el palet en el f14c.-----------------------------
         Select f14c
*        IF SEEK(c_numpalet)
  
         *>Posible fallo....9/2/96.-----------------------------------
         Do Case

            *>Si es Reposición.---------------------------------------
            Case Left(m.TipMov,1) ="R"
                 Locate For F14cNumDes == c_NumOrd  .And.  ;
                        !Empty(F14cUbiOri)  .And.  !Empty(F14cUbicaz) 
            
            *>Si es Salida.-------------------------------------------
            Case Left(m.TipMov,1) ="S"
                Locate For F14cNumDes == c_NumOrd

                C_CodArt = F14cCodArt
                C_Canfis = F14cCanFis
                C_SitStk = F14cSitStk 
                L_SumRes = .F.

*               Locate For F14cNumPal == c_NumPalet  .And.  ;
*                    !Empty(F14cUbiOri)  .And.  Empty(F14cUbicaz) 


            *>Si es Entrada.------------------------------------------
            Case Left(m.TipMov,1) ="E"
                Locate For F14cNumPal == c_NumPalet  

                C_TipDoc = F14cTipDoc
                C_NumDoc = F14cNumPed
                C_CodPro = F14cCodEnt
                C_CodArt = F14cCodArt
                C_Canfis = F14cCanFis
                C_SitStk = F14cSitStk 
                L_SumRes = .T.

                C_AlbPro = F14cNotaes
                
*>                *> Recupero los datos del F18c
*>                Do FSelect With 'F18c', 'CODIGO', '0'
*>                Seek C_TipDoc + C_NumDoc
*>                If Found()
*>                   C_AlbPro = F18cAlbOri
*>                EndIf


         EndCase

          IF !EOF()           
            lRet = .f.
            lReg = RECNO()
            Select ConMov
         
            do prc_ConF in Lib_Ca00 with lReg, lRet
            IF !lRet
               =F3_Sn(1,1,"Error en la confirmación del palet "+C_NumPalet)
            Else

               Do Case

                  *>Si es Entrada.------------------------------------------
                  Case Left(m.TipMov,1) ="E"

                     *> Actualizamos situacion que corresponda.-------------------
                     L_Resultad=.f.
                     Do actualst In Lib_ca00 with C_CodArt,C_Canfis,C_SitStk,L_SumRes,l_Resultad
 
                     *> Actualizamos situacion Pendiente de ubicar.----------------
                     L_Resultad=.f.
                     Do actualst In Lib_ca00 with C_CodArt,C_Canfis,"5000",.F.,l_Resultad

                     Select F18c 
                     Seek C_TipDoc+C_NumDoc 
                     If Found()
                        *> Actualizamos situacion Pendiente de entrar.----------------
                        L_Resultad=.f.
                        Do actualst In Lib_ca00 with C_CodArt,C_Canfis,"7000",.F.,l_Resultad
                     EndIf

                     *> Si el movimiento es de entrada
                     If C_TipDoc='1300'
                     
                        *> ACTUALIZACIÓN DE FICHERO DE ENVIOS
                
                        *> Obtengo el número de traspaso
                        Do FSelect With 'F00c', 'CODIGO', '0'
                        Seek 'TNU' + 'TRP'
                        If Found()
                           C_NumTrp = F00c.F00cNumero
                           C_NumTrp = Right('0000000000' + Alltrim(Str(F00c.F00cNumero)), 10)
                           Replace F00c.F00cNumero With F00c.F00cNumero + 1
                        Else
                           C_NumTrp = '0000000000'
                        EndIf

                        *> Genero movimiento de ENTRADA en histórico de Traspasos
                        Do FSelect With 'EnvPla', 'CODIGO', '0'
                        Append Blank
                   
                        Replace EnvError  With 'S'
                        Replace EnvEntSal With 'E'
                        Replace EnvNumTrp With C_NumTrp
                   
                        Replace EnvTipMov With '1000'
                   
                        Replace EnvReint  With 1
                   
                        *> El Código de Artículo debe ser el de PLAN DIAS
                        _Ok=f3_seek("F08c", C_CodPro + C_CodArt, '', '', '', 'IND03')
                        If _Ok
                           Replace EnvCodArt With IIf(_Ok, F08c.F08cCodPla, '')
                        EndIf
                   
                           Replace EnvCantid With C_CanFis
                           Replace EnvFecha  With Date()
                           Replace EnvNumPed With C_NumDoc
                           Replace EnvNumAlb With C_AlbPro
                           
                           Replace EnvAnyCon With ''
                           Replace EnvTipCon With ''
                           Replace EnvNumCon With ''
                           Replace EnvCodPro With ''
                           
                        
                     EndIf



                 *>Si es Salida.-------------------------------------------------
                 Case Left(m.TipMov,1) ="S"
               

                     *> Actualizamos situacion reservada.--------------------------
                     L_Resultad=.f.
                     Do actualst In Lib_ca00 with C_CodArt,C_Canfis,"4000",.F.,l_Resultad

                     *> Actualizamos situacion Pend.Cliente.--------------------------
                     L_Resultad=.f.
                     Do actualst In Lib_ca00 with C_CodArt,C_Canfis,"7500",.F.,l_Resultad
                   
               EndCase

            ENDIF
 
          ELSE
            =F3_Sn(1,1,"El Palet "+C_NumPalet+" no se encuentra")
          ENDIF
     ENDIF

      *> Avanzamos el registro.---------------------------------------
      Select ConMov
      Skip
   ENDDO

Return

Procedure CanMov

   *> Bucle para procesar las lineas.---------------------------------
   Select ConMov
   GOTO TOP
   DO WHILE !EOF()

      *> Controlamos si se tiene que confirmar.-----------------------
      IF Marca=1
         *> Guardamos el número de palet.-----------------------------
         c_numpalet = f14cnumpal
         C_Numord   = F14cNumDes

         *> Buscamos el palet en el f14c.-----------------------------
         Select f14c
*        IF SEEK(c_numpalet)
  
         *>Posible fallo....9/2/96.-----------------------------------
         Do Case

            *>Si es Reposición.---------------------------------------
            Case Left(m.TipMov,1) ="R"
                 Locate For F14cNumDes == c_NumOrd  .And.  ;
                        !Empty(F14cUbiOri)  .And.  !Empty(F14cUbicaz)
            
            *>Si es Salida.-------------------------------------------
            Case Left(m.TipMov,1) ="S"
                Locate For F14cNumDes == c_NumOrd

                C_CodArt = F14cCodArt
                C_Canfis = F14cCanFis
                C_SitStk = F14cSitStk 
                L_SumRes = .F.

*               Locate For F14cNumPal == c_NumPalet  .And.  ;
*                    !Empty(F14cUbiOri)  .And.  Empty(F14cUbicaz) 


            *>Si es Entrada.------------------------------------------
            Case Left(m.TipMov,1) ="E"
                Locate For F14cNumPal == c_NumPalet  

                C_TipDoc = F14cTipDoc
                C_NumDoc = F14cNumPed
                C_CodArt = F14cCodArt
                C_Canfis = F14cCanFis
                C_SitStk = F14cSitStk 
                L_SumRes = .T.

         EndCase

         IF !EOF()           
            lRet = .f.
            lReg = RECNO()
            Select ConMov
            do prc_CanF in Lib_Ca00 with lReg, lRet
            IF !lRet
               =F3_Sn(1,1,"Error en la cancelación del palet "+C_NumPalet)
            Else

            Do Case
             *>Si es Salida.-------------------------------------------
             Case Left(m.TipMov,1) ="S"
              C_CodArt = F14cCodArt
              C_Canfis = F14cCanFis
              C_SitStk = F14cSitStk 
              L_SumRes = .T.

              *> Actualizamos situacion que corresponda.-------------------
              L_Resultad=.f.
              Do actualst In Lib_ca00 with C_CodArt,C_Canfis,C_SitStk,L_SumRes,l_Resultad
 
              *> Actualizamos situacion Pendiente de ubicar.----------------
              L_Resultad=.f.
              Do actualst In Lib_ca00 with C_CodArt,C_Canfis,"4000",.F.,l_Resultad


         *>Si es Entrada.------------------------------------------
         Case Left(m.TipMov,1) ="E"
              Locate For F14cNumPal == c_NumPalet  
              C_CodArt = F14cCodArt
              C_Canfis = F14cCanFis
              C_SitStk = F14cSitStk 
              L_SumRes = .F.

              *> Actualizamos situacion Pendiente de ubicar.----------------
              L_Resultad=.f.
              Do actualst In Lib_ca00 with C_CodArt,C_Canfis,"5000",.F.,l_Resultad

     EndCase

     EndIf
    Endif
    EndIf
    Select ConMov
    Skip
  EndDo  
Return

*> Se Confirman los movimientos.--------------------------------------
Procedure ConLis
PRIVATE c_numpalet, l_openf20, l_openf16, l_openf26, n_Cant7000, c_OrdF26c

   *> Bucle para procesar las líneas.---------------------------------
   Select TraLis
   Scan For Marca==1
      l_Bien     = .F.

      c_NumPal   = F26lNumPal
      c_UbiDes   = F26lUbiDes
      c_NumOrd   = F26lNumOrd
      C_CodArt   = F26lCodArt
      C_Cantid   = F26lCanFis
      C_NumLin   = F26lNumLin
      C_TipDoc   = F26lTipDoc
      C_NumDoc   = F26lNumDoc

       *> Buscamos el palet en el f14c.-----------------------------
      Select F14c
      If Empty(c_NumDes)
         *> Buscamos en el fichero de mov.pendientes.--------------
         Select F14c
         Locate For F14cNumDes==c_NumOrd
         If !Eof()
            lRet = .f.
            lReg = RecNo()
            Select TraLis
            Do Case
               Case Empty(c_UbiDes)
                    =f3_sn(1,4,"No se puede confirmar un movimiento de","salida directo, estos movimientos se","confirman desde el pedido.")
    
               *> Confirmación.------------------------------------
               Case !Empty(c_UbiDes)
                    Do Prc_Conf In Lib_Ca00 With LReg, l_Bien
                    If !l_Bien 
                       =F3_Sn(1,1,"Error en la confirmación del palet "+C_NumPal)
                    EndIf
            EndCase  
         Else 
            =F3_sn(1,4,"No se puede tratar el palet número "+C_NumPal,"ya que no se encuentra movimiento pendiente") 
         EndIf
      Else
         =F3_sn(1,4,"No se puede tratar el palet número "+C_NumPal,"ya que tiene movimientos pendientes") 
      EndIf

      *>
      Select TRALIS
   EndScan
Return

***********************************************************
*  Funciones relacionadas con Proceso CanLis  y RegLis    *
***********************************************************

*> Existe F14c del Mvt.(F26l)----------------------------------------------
Function ExistF14c
Parameter NMov
Private _ok

m.F14cNumMov = NMov
_ok = f3_seek('F14c')

Return _ok 

*> Existe F26l del Mvt.(F26l)----------------------------------------------
Function ExistF26l
Parameter NMov
Private _ok

m.F26lNumMov = NMov
_ok = f3_seek('F26l')

Return _ok 

*> Existe F16c del Mvt.(F26l)-----------------------------------------------
Function ExistF16c
Private f_select, f_where, Err, ok

Select TRALIS

f_select = " Select F16cCanFis,F16cCanRes From F16c" + _em

f_where  = " Where  F16cCodPro='" + F26lCodPro + "'" + ;
           " And    F16cCodArt='" + F26lCodArt + "'" + ;
           " And    F16cCodUbi='" + F26lUbiOri + "'" + ;
           " And    F16cNumLot='" + F26lNumLot + "'" + ;
           " And    F16cSitStk='" + F26lSitStk + "'" + ;
           " And    F16cNumPal='" + F26lNumPal + "'" + ;
           " And    F16cFecCad=" + _GCD(F26lFecCad)

Err = f3_SqlExec(_ASql,f_select+f_where)
If Err > 0
   Go Top
   Err=IIF(!eof(),1,-1)
EndIf 

_ok=IIF(Err > 0,.T.,.F.) 

return _ok

*> Act. Qtat. reservada F16c--------------------------------------------------
Function UpdQMovF16c
Parameters Accion, Oper

*>-------------------
*>  Acción--> 'F' Afecta a cantidad Física
*>        --> 'R' Afecta a cantidad Reservada
*>  Oper  --> '+' S'afegeix quantitat física mov. F26l
*>        --> '-' Es resta  quantitat física mov. F26l
*>-------------------

Private f_update, f_where, Err, _ok, signo

Select TRALIS
signo=IIF(Oper='+',1,-1)

If Accion=='R'
   f_update = " Update F16c" + _em + ;
              " Set F16cCanRes= " + ;
   Str(IIF(SqlResult.F16cCanRes+(signo*F26lCanFis)>=0,SqlResult.F16cCanRes+(signo*F26lCanFis),0))
Else
   f_update = " Update F16c" + _em + ;
              " Set F16cCanFis= " + ;
   Str(IIF(SqlResult.F16cCanFis+(signo*F26lCanFis)>=0,SqlResult.F16cCanFis+(signo*F26lCanFis),0))
EndIf

f_where  = " Where  F16cCodPro='" + F26lCodPro + "'" + ;
           " And    F16cCodArt='" + F26lCodArt + "'" + ;
           " And    F16cCodUbi='" + F26lUbiOri + "'" + ;
           " And    F16cNumPal='" + F26lNumPal + "'" + ;
           " And    F16cNumLot='" + F26lNumLot + "'" + ;
           " And    F16cSitStk='" + F26lSitStk + "'" + ;
           " And    F16cFecCad=" + _GCD(F26lFecCad)

Err=f3_SqlExec(_ASql,f_update + f_where)
_ok = IIF(Err > 0, .T., .F.)                      

return _ok

*> Calcula número de mvts de la Lista del mvt. Tratado-----------------------------
Function NroMovF26l

Private f_Select, f_where, contt

Select TRALIS

f_select = " Select Count(*) contt From F26l" + _em
f_where  = " Where F26lNumLst='" + F26lNumLst + _cm 

contt=f3_SqlExec(_ASql,f_select + f_where) 
If contt >= 0
   contt=SqlResult.contt
EndIf

return contt

*> Borra actual Mvt. del F26l----------------------------------------------------
Function DelMovF26l

Private f_delete, Err, _ok

Select TRALIS

f_delete = "Delete From F26l" + _em + " Where F26lNumMov='" + F26lNumMov + _cm

Err = f3_SqlExec(_ASql,f_delete) 
_ok=IIF(Err>0,.T.,.F.)

Return _ok


*> Borra Actual Mvt. del F26c----------------------------------------------------
Function DelCabF26c
Private f_delete, Err, _ok

   Select TRALIS
   f_delete = "Delete From F26c" + _em + " Where F26cNumLst='" + F26lNumLst + _cm 
   Err = f3_SqlExec(_ASql,f_delete)
   _ok=IIF(Err>0,.T.,.F.)

Return _ok 

*> Borra actual Mvt. del F14c----------------------------------------------------
Function DelMvtF14c
Parameter NMov
Private f_delete, Err, _ok

f_delete = " Delete From F14c" + _em + " Where F14cNumMov='" + NMov + _cm

Err = f3_SqlExec(_ASql, f_delete) 

_ok = Iif(Err > 0, .T., .F.)

Return _ok

*> Borra actual Mvt. del F26l----------------------------------------------------
Function DelMvtF26l
Parameter NMov
Private f_delete, Err, _ok

f_delete = " Delete From F26l" + _em + " Where F26lNumMov='" + NMov + _cm

Err = f3_SqlExec(_ASql, f_delete) 

_ok = Iif(Err > 0, .T. ,.F.)

Return _ok

*> Actualización de Líneas de Documentos
*> Si Existe Línea de Documento en f24l de mvto. (f26l)-------------------------- 
Function ExistF24l

Private f_Select, f_where, Err

Select TRALIS

f_select = " Select F24lCanRes From F24l" + _em

f_where  = " Where F24lCodPro='" + F26lCodPro + _cm + ;
           " And   F24lTipdoc='" + F26lTipDoc + _cm + ;
           " And   F24lNumDoc='" + F26lNumDoc + _cm + ;
           " And   F24lLinDoc=" + Str(F26lLinDoc) 

Err = f3_SqlExec(_ASql,f_select+f_where)
If Err >= 0
   Go Top
   Err=IIF(!eof(),1,-1)
EndIf 

_ok=IIF(Err > 0,.T.,.F.) 

return _ok 

*> Quitar  la cantidad reservada del act. Mov. f26l a la línea del  F24l.................
Function UpdMvtF24l

Private f_update, f_where, Err, _ok, NewCanRes

Select TRALIS

NewCanRes=Iif(SqlResult.F24lCanRes-F26lCanFis >= 0,SqlResult.F24lCanRes-F26lCanFis,0)

f_update = " Update F24l" + _em + ;
           " Set F24lCanRes=" + Str(NewCanRes)

If NewCanRes==0
   f_update = f_update + ", F24lFlgEst='0'"
EndIf
            
f_where  = " Where F24lCodPro='" + F26lCodPro + _cm + ;
           " And   F24lTipdoc='" + F26lTipDoc + _cm + ;
           " And   F24lNumDoc='" + F26lNumDoc + _cm + ;
           " And   F24lLinDoc=" + Str(F26lLinDoc, 4, 0)
           
Err = f3_SqlExec(_ASql,f_update+f_where)           

_ok= Iif(Err > 0,.T.,.F.)

Return _ok

*>
*> Modificar Situación de Stocks  
*>

*> Existe F13c. 
Function ExistF13c

Parameters SitStk

Private f_select, f_where, Cont

Select TRALIS

f_select = " Select F13cCantid from F13c"+ _em

f_where  = " Where F13cCodAlm='" + _Alma + _cm + ;
           " And   F13cCodPro='" + F26lCodPro + _cm + ;
           " And   F13cCodArt='" + F26lCodArt + _cm + ;
           " And   F13cSitStk='" + SitStk + _cm  
   
Cont = f3_SqlExec(_ASql,f_select+f_where)
If Cont >= 0
   Go Top
   Cont=IIF(!eof(),1,0)
EndIf 


return Cont 

*> actualizar Situación de Stock porque existe
Function UpdateF13c

Parameters SitStk, Cantid
Private f_update, f_where, Err, _ok

Select TRALIS

f_update = " Update F13c" + _em + ;
           " Set F13cCantid=" +  Str(Cantid) 

f_where  = " Where F13cCodAlm='" + _Alma + _cm + ;
           " And   F13cCodPro='" + F26lCodPro + _cm + ;
           " And   F13cCodArt='" + F26lCodArt + _cm + ;
           " And   F13cSitStk='" + SitStk + _cm  

Err = f3_SqlExec(_ASql,f_update+f_where)           

_ok= IIF(Err > 0,.T.,.F.) 

Return _ok

*> Insertar Situación de Stock si no existe
Function InsertF13c

Parameters SitStk, Cantid
Private f_insert, f_valores, Err, _ok


f_insert  = " Insert into F13c" + _em
f_valores = " values('" + _Alma  + "','" + F26lCodPro + "','" + F26lCodArt + "'," + ;
                    "'" + SitStk + "'," + Str(Cantid) + ")"   
Err = f3_SqlExec(_ASql,f_insert+f_valores)           

_ok= IIF(Err > 0,.T.,.F.) 

Return _ok

*> Actualizar Sit. Stock que hay en mvto. del f26l Actual----------------

Function ActSSF13c

*>------------
*>  SitStk --> Situación Stock
*>  Acción --> 'A' Añadir a S.S. Cantidad Movimiento
*>         --> 'E' Extraer de S.S. Cantidad Movimiento   
*>------------
Parameters SitStk, Accion
Private Nro, NewCantid

Select TRALIS


Nro=ExistF13c(SitStk)

*> Si Error al llamar a Oracle
If Nro < 0
  return .F. 
EndIf  


If Nro > 0    && Si Existe S.S. Artículo en F13c  Actualizarla.

  If Accion == 'A'
     NewCantid=IIF(SqlResult.F13cCantid>=0,SqlResult.F13cCantid+TRALIS.F26lCanFis,TRALIS.F26lCanFis)
  Else
     NewCantid=IIF(SqlResult.F13cCantid-TRALIS.F26lCanFis>=0,SqlResult.F13cCantid-TRALIS.F26lCanFis,0)
  EndIf
  _okk=updateF13c(SitStk,NewCantid)
   
Else          && Si NO Existe S.S. artículo en F13c se crea con cantidad igual a F26lCanFis
    NewCAntid=IIF(Accion=='A',TRALIS.F26lCanFis,0)
   _okk=InsertF13c(SitStk,NewCantid)
EndIf

Return _okk

*> Proceso de desreserva de un movimiento pendiente.
Function DesReservar
Parameters NMov
Private Nro, _okk

     *> Comprobar si existe F14c
     If !ExistF14c(NMov)
        _lxerr=_lxerr + "No se encontró Movimiento [" + NMov + "] en F14c." + cr
     Else
        *>  Borrar el Mvt. en el F14c
        If !DelMvtF14c(NMov)
           _lxerr=_lxerr + "No se pudo borrar Movimiento [" + NMov + "] en F14c." + cr
        EndIf  
     EndIf  

     *> Comprobar si existe F26l
     If !ExistF26l(NMov)
        _lxerr=_lxerr + "No se encontró Movimiento [" + NMov + "] en F26l." + cr
     Else
        *>  Borrar el Mvt. en el F26l
        If !DelMvtF26l(NMov)
           _lxerr=_lxerr + "No se pudo borrar Movimiento [" + NMov + "] en F26l." + cr
           Return .F.
        EndIf
     EndIf  

     *> DesReservar Cantidad movto. en f16c.........................  
     *> Existe Ocupación
     If !ExistF16c() 
        _lxerr=_lxerr + "No se encontró Ocupación Movimiento [" + NMov + "] en F16c." + cr
        Return .F.
     EndIf

     If !UpdQMovF16c('R', '-')
        _lxerr=_lxerr + "No se pudo desreservar Ocupación Movimiento [" + NMov + "] en F16c." + cr
        Return .F.
     EndIf  

     *>  Poner Estado de línea de documento (F24l) a 0, y, Cantidad Reservada. 
     *>  Existe F24l del movimiento F26l
     If !ExistF24l()
        _lxerr=_lxerr + "No Existe Línea Documento Movimiento [" + NMov + "] en F24l." + cr
        Return .F.
     EndIf       
           
     If !UpdMvtF24l()
        _lxerr=_lxerr + "No se pudo actualizar Lín. Documento Mvto. [" + NMov + "] en F24l." + cr
        Return .F.
     EndIf  

     *>   Proceso que actualiza la situación de Stocks por mvto actual
     *>   Resta de Reservada por Mvto. actual...................................
     If !ActSSF13c('2000','E')
        _lxerr=_lxerr + "No se act. Stock Movto. [" + NMov+"] en F13c. S" + cr
        Return .F. 
     EndIf  
return .T.

*> Proceso de desreserva de una reposición.
*> Recibe : NMov  (Nº movimiento origen)
*>          NMovR (Nº movimiento destino)
Function CancReposicOld
Parameters NMov, NMovR
Private Nro, _okk

     *> Comprobar si existe F14c (origen)
     _okk=ExistF14c(NMov)
     IF !_okk
        _lxerr=_lxerr + "No se encontró movimiento ORIGEN [" + NMov + "] en F14c." + Cr
        return .F.
     EndIf  

     *> Guardar datos para actualizar los MPs y listas afectados por la reposición.
     Scatter MemVar
     f_where14 = "F14cCodPro='" + m.F14cCodPro + "' And " + ;
                 "F14cCodArt='" + m.F14cCodArt + "' And " + ;
                 "F14cUbiOri='" + m.F14cUbiOri + "' And " + ;
                 "F14cNumPal='" + m.F14cNumPal + "' And " + ;
                 "F14cFecCad=" + _GCD(m.F14cFecCad) + " And " + ;
                 "F14cNumLot='" + m.F14cNumLot + "' And " + ;
                 "F14cSitStk='" + m.F14cSitStk + "' And " + ;
                 "F14cTipMov Between '2000' And '2998'"

     f_where26 = "F26lCodPro='" + m.F14cCodPro + "' And " + ;
                 "F26lCodArt='" + m.F14cCodArt + "' And " + ;
                 "F26lUbiOri='" + m.F14cUbiOri + "' And " + ;
                 "F26lNumPal='" + m.F14cNumPal + "' And " + ;
                 "F26lFecCad=" + _GCD(m.F14cFecCad) + " And " + ;
                 "F26lNumLot='" + m.F14cNumLot + "' And " + ;
                 "F26lSitStk='" + m.F14cSitStk + "' And " + ;
                 "F26lTipMov Between '2000' And '2998'"

     f_where16 = "F16cCodPro='" + m.F14cCodPro + "' And " + ;
                 "F16cCodArt='" + m.F14cCodArt + "' And " + ;
                 "F16cCodUbi='" + m.F14cUbiOri + "' And " + ;
                 "F16cNumPal='" + m.F14cNumPal + "' And " + ;
                 "F16cFecCad=" + _GCD(m.F14cFecCad) + " And " + ;
                 "F16cNumLot='" + m.F14cNumLot + "' And " + ;
                 "F16cSitStk='" + m.F14cSitStk + "'"

     *> Comprobar si existe F14c (destino)
     _okk=ExistF14c(NMovR)
     IF !_okk
        _lxerr=_lxerr + "No se encontró movimiento DESTINO [" + NMovR + "] en F14c." + Cr
        return .F.
     EndIf  

     *>  Borrar MP origen.
     _okk=DelMvtF14c(NMov)
     IF !_okk
        _lxerr=_lxerr + "No se pudo borrar movimiento DESTINO [" + NMov + "] en F14c." + cr
        return .F. 
     EndIf

     *> Borrar MP destino.
     _okk=DelMvtF14c(NMovR)
     If !_okk
        _lxerr=_lxerr + "No se pudo borrar movimiento DESTINO[" + NMovR + "] en F14c." + cr
        return .F. 
     EndIf  

     *> Actualizar MPs de preparación asociados a esta reposición.
     FlgBlq = Space(1)
     Sw = F3_UpdTun('F14c', , 'F14cFlag1', 'FlgBlq', , f_where14, 'N')
     If Sw = .F.
        _lxerr=_lxerr + "No se han podido actualizar MPs de preparación[" + NMovR + "] en F14c." + cr
        Return .F. 
     EndIf

     *> Actualizar listas, si estos MPs están asignados.
     FlgBlq = Space(1)
     Sw = F3_UpdTun('F26l', , 'F26lFlag1', 'FlgBlq', , f_where26, 'N')
     If Sw = .F.
        _lxerr=_lxerr + "No se han podido actualizar listas de preparación[" + NMovR + "] en F14c." + cr
        Return .F. 
     EndIf
     
     *> Actualizar estado ocupación de reposición.
     FlgBlq = Space(1)
     Sw = F3_UpdTun('F16c', , 'F16cFlag1', 'FlgBlq', , f_where16, 'N')
     If Sw = .F.
        _lxerr=_lxerr + "No se han podido actualizar ocupación [" + NMovR + "] en F14c." + cr
        return .F. 
     EndIf

return .T.

*> Rellenar campos del Objeto OraFncActz.
Function CamposHM

Private f_select, f_where, Err, _ok

Select TRALIS

f_select = " Select * from F26l" + _em + ", F10c" + _em

f_where  = " Where F26lNumMov = '" + F26lNumMov + _cm + ;
           " And   F10cCodUbi = F26lUbiOri"

Err= f3_SqlExec(_ASql,f_select+f_where)
If Err < 0
   return .F.
EndIf

Go Top
IF eof()
   return .F.
EndIf

*****>FncA.ObjParm.=F26LNUMMOV   
FncA.ObjParm.PONEnt=F26LNUMENT   
FncA.ObjParm.PMTMov=F26LTIPMOV     
FncA.ObjParm.POTDoc=F26LTIPDOC   
FncA.ObjParm.PONdoc=F26LNUMDOC   
FncA.ObjParm.POLDoc=F26LLINDOC   
FncA.ObjParm.PMFDoc=F26LFECDOC   
FncA.ObjParm.PMDAso=F26LDIRASO   
FncA.ObjParm.PONPed=F26LNUMPED   
FncA.ObjParm.POLPed=F26LLINPED   
*****>FncA.ObjParm.=F26LFECMOV   
*****>FncA.ObjParm.=F26LFECLST   
FncA.ObjParm.POCArt=F26LCODART   
FncA.ObjParm.PONLot=F26LNUMLOT   
FncA.ObjParm.POSStk=F26LSITSTK   
FncA.ObjParm.POFCad=F26LFECCAD   
FncA.ObjParm.POCFis=F26LCANFIS   
FncA.ObjParm.PMRHab=F26LRUTHAB   
FncA.ObjParm.PUbOld=F26LUBIORI   
FncA.ObjParm.PUBNew=F26LUBIORI   
*****>FncA.ObjParm.=F26LUBIDES   
FncA.ObjParm.PONPal=F26LNUMPAL   
FncA.ObjParm.POTPal=F26LTIPPAL   
FncA.ObjParm.POFUni=F26LUNIVEN   
FncA.ObjParm.POFSer=F26LUNIPAC   
FncA.ObjParm.POFEnv=F26LPACCAJ   
FncA.ObjParm.POFPAL=F26LCAJPAL   
FncA.ObjParm.POFFab=F26LFECFAB   
FncA.ObjParm.POFCal=F26LFECCAL   
FncA.ObjParm.PONAna=F26LNUMANA   
FncA.ObjParm.POCPro=F26LCODPRO   
FncA.ObjParm.PMCOpe=F26LCODOPE   
*****>FncA.ObjParm.=F26LTIPLST   
FncA.ObjParm.PMNLst=F26LNUMLST   
FncA.ObjParm.PMNExp=F26LNUMEXP   
*****>FncA.ObjParm.=F26LORDREC   
*****>FncA.ObjParm.=F26LESTMOV  
FncA.ObjParm.PMORes=F26LORIRES  
FncA.ObjParm.PMTUbi=F26LTIPUBI 
FncA.ObjParm.PMTMas=F26LTIPMAS 
FncA.ObjParm.PMNMas=F26LNUMMAS 
FncA.ObjParm.PMMUni=F26LMACUNI   
FncA.ObjParm.PMORut=F26LORDRUT   
FncA.ObjParm.PMTERe=F26LTENTRE   
FncA.ObjParm.PMCERe=F26LCENTRE   
FncA.ObjParm.PMVHab=F26LVENHAB   
FncA.ObjParm.PMTMac=F26LTIPMAC   
FncA.ObjParm.PMNMac=F26LNUMMAC   
FncA.ObjParm.PMSecc=F26LSECCIO
FncA.ObjParm.POPico=F10CPICKSN &&¿no?  
FncA.ObjParm.POCAlm=_Alma
FncA.ObjParm.PMFlg1=F26LFLAG1    
FncA.ObjParm.PMFlg2=F26LFLAG2    

return .T.

*>-------------------------------------
*> cancelación de movimientos de listas
*>-------------------------------------
Procedure CanLis

   Parameters EntSal
   Private Nro, _okk
   
   *> Bucle para procesar las lineas.---------------------------------
   _lxerr=''
   Select TRALIS

   Scan For Marca==1     
     _okk=DesReservar(TRALIS.F26lNumMov)
     If _okk 
        *> Devuelvo cantidad DesReservada al Stock Lógico determinado por el mvto.
        _okk=ActSSF13c(TRALIS.F26lSitStk, 'A')
        If !_okk
           _lxerr=_lxerr + "No se act. Stock Movto. ["+TRALIS.F26lNumMov+"] en F13c. E" + Cr             
        EndIf
     EndIf   

     *> Y si todo ha ido bien Hacemos commit
     If _okk
        =SqlCommit (_ASql)
     Else
        =SqlRollBack(_ASql)
     EndIf
             
     *>
     Select TRALIS
   EndScan

If !Empty(_lxerr)
   Do Form St3inc With .T.
EndIf

Return

************************************************
*> Funciones de Salida por cantidad cancelada  * 
************************************************

*> Extraer Cantidad Física de Mvto. a F16c 
Function ECanFisF16c

Private _ok

Select TRALIS
*>  Comprovar si existe Ubicación
_ok=ExistF16c()
If !_ok
   Return .F.
EndIf 

*> Extraer cantidad física mvto. a Ubicación.
_ok=UpdQMovF16c('F','-')
If !_ok
   Return .F.
EndIf  

Return _ok


*>
*> Grabar movimiento en histórico de movimientos
*> EntSal --> E de Entrada
*>        --> S de salida
*>
Function GrabHMF20c

Parameters EntSal, TipMov
Private _ok

FncA.ObjParm.PMEnSa = EntSal    
FncA.ObjParm.PMFgHM = 'S'
FncA.ObjParm.PMTMov = TipMov
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM

_ok=IIF(FncA.ObjParm.PWCRtn >= '50',.F.,.T.)

return _ok

*>----------------------------------------
*> Regularización Movimientos Lista
*>---------------------------------------- 
Procedure RegLis

   Private FncA, PrmA
   Private Nro, _okk

   *> Crear objetos actualización.---------------------------------------
   FncA = CreateObject('OraFncActz')
   PrmA = CreateObject('OraPrmActz')
   FncA.ObjParm = PrmA

   _lxerr=''

   *> Bucle para procesar las lineas.---------------------------------
   Select TRALIS
   Scan For Marca==1

    *> Llenar los campos del objeto OraPrmActz.
     FncA.ObjParm.Inicializar

     _okk=CamposHM()
     If !_okk
        _lx_err=_lxerr + "No  se pudo actualizar campos del Objeto" + Cr
        Select TRALIS
        Skip 
        Loop 
     EndIf

     *> Primero desreservamos movimiento.
     _okk=DesReservar(TRALIS.F26lNumMov)
     If !_okk
        =SqlRollBack(_ASql)
        Select TRALIS
        Skip
        Loop     
     EndIf

     *> Acabamos de regularizar. Ubicación F16c Del Mvto. de F26l  
     _okk=ECanFisF16c()
     If !_okk
        =SqlRollBack(_ASql)
        _lxerr=_lxerr + "No finalizó Regularización Ocupación MP [" + TRALIS.F26lNumMov+ "] en F16c" + cr
        Select TRALIS
        Skip
        Loop     
     EndIf

     *> Grabar HM de Salida en F20c 
     _okk=GrabHMF20c('S', '4500')
     If _okk 
        =SqlCommit (_ASql)
     Else
        =SqlRollBack(_ASql)
     EndIf
             
     *> Actualizar la cabecera de la lista.
     =Ora_AcCbLs(TRALIS.F26lNumLst)

     *>
     Select TRALIS
   EndScan

   If !Empty(_lxerr)
      Do Form St3inc
   EndIf

   Release FncA
   Release PrmA

Return

**************************************************************
*> Actualización mvto. del F14c y F26l para la nueva Ubicación 
**************************************************************

*> Actualización F14c-----------------------------------------
Function UpdF14c
Parameters NMov

Private f_update, f_where, Err, _ok

Select TRALIS

f_update = " Update F14c" + _em + ;
           " Set F14cUbiOri = '" + F26lUbiOri + "'," + ;
           "     F14cNumLot = '" + F26lNumLot + "'," + ;
           "     F14cNumPal = '" + F26lNumPal + "'," + ;
           "     F14cFecCad = " + _GCD(F26lFecCad)

f_where  = " Where F14cNumMov='" + NMov + "'"

Err = f3_SqlExec(_ASql,f_update+f_where)           

_ok= IIF(Err > 0,.T.,.F.) 

Return _ok

*> Actualización F26l------------------------------------------
Function UpdF26l

Private f_update, f_where, Err, _ok

Select TRALIS

f_update = " Update F26l" + _em + ;
           " Set F26lUbiOri = '" + F26lUbiOri + "'," + ;
           "     F26lNumLot = '" + F26lNumLot + "'," + ;
           "     F26lNumPal = '" + F26lNumPal + "'," + ;
           "     F26lFecCad = " + _GCD(F26lFecCad)
           

f_where  = " Where F26lNumMov='" + F26lNumMov + _cm 

Err = f3_SqlExec(_ASql,f_update+f_where)           

_ok= IIF(Err > 0,.T.,.F.) 

Return _ok

*>--------------------------------------------------------------------
*> Cambio de Ubicación Origen en listas.
*>--------------------------------------------------------------------
Procedure ChgUbi

Private Nro, _okk 

     *> Comprobar si existe F14c
     Select TRALIS
     Scatter MemVar

     _okk=ExistF14c(F26lNumMov) 
     IF !_okk
        =SqlRollBack(_ASql)
        =f3_sn(1, 1, "No se encontró movimiento pendiente ["+TRALIS.F26lNumMov+"] en F14c")
        Return .F.
     EndIf  

     *> Si Existe Antigua Ubic del Mvto extraer a ubic. cantidad física de este.
     *> Existe Ocupación
     _okk=ExistF16c() 
     If !_okk
        =SqlRollBack(_ASql)     
        =f3_sn(1,1,"No se encontró ocupación origen ["+TRALIS.F26lNumMov+"] en F16c")
        Return .F.
     EndIf

     *> Extraer cantidad física mvto. a  cant. Reservada de Ubicación.
     _ok=UpdQMovF16c('R','-')
     If !_ok
        =SqlRollBack(_ASql)     
        =f3_sn(1,1,"No se liberó cantidad reservada en origen ["+TRALIS.F26lNumMov+"] en F16c")
        Return .F.
     EndIf  

     *> Si Existe Nueva Ubic. del Mvto añadir a ubic. cantidad física de este.
     Select TRALIS
     Replace F26lUbiOri With m.UbiNueva, ;
             F26lNumLot With m.LotNuevo, ;
             F26lNumPal With m.PalNuevo, ;
             F26lFecCad With m.FCDNuevo

     *> Existe Ocupación
     _okk=ExistF16c() 
     If !_okk
        =SqlRollBack(_ASql)     
        =f3_sn(1,1,"No se encontró  Ocupación nueva ["+TRALIS.F26lNumMov+"] en F16c.")     
        Return .F.
     EndIf  

     *> Añadir cantidad física del mvto. a cant. Reservada de Ubicación, si se puede.
     _ok=UpdQMovF16c('R','+')
     If!_ok
        =SqlRollBack(_ASql)     
        =f3_sn(1,1,"No se reservó C. Res.  Ocup. Movimiento ["+TRALIS.F26lNumMov+"] en F16c.")             
        Return .F.
     EndIf  

     *> Actualización mvto. del F14c y F26l para la nueva Ubicación 
     _ok=UpdF14c(TRALIS.F26lNumMov)
     If !_ok
        =SqlRollBack(_ASql)     
        =f3_sn(1,1,"No se act. Movimiento ["+TRALIS.F26lNumMov+"] con nueva Ocup. en F14c.")
        Return .F.
     EndIf  

     _ok=UpdF26l()
     IF !_ok
        =SqlRollBack(_ASql)
        =f3_sn("No se act. Movimiento ["+TRALIS.F26lNumMov+"] con nueva Ocup. en F26l.")             
        return .F.
     EndIf  
     
     =SqlCommit (_ASql)
Return

*>--------------------------------------------------------------------
*> Cambio de Ubicación en entradas pendientes de confirmar.
*> Parámetros:
*>    NumMov ----> Nº movimiento en MP.
*>    UbiNew ----> Ubicación nueva.
*>--------------------------------------------------------------------
Procedure ChgUbiPal
Parameters NumMov, UbiNew
Private n_Peso, n_Volu, l_Bi

   *> Buscar el MP a actualizar.
   m.F14cNumMov = NumMov
   If !f3_seek('F14c')
      _LxErr = 'No se encontro el MP: ' + NumMov + cr
      Do Form St3Inc
      _LxErr = ''
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   Select F14c
   Go Top
   CodPro = F14cCodPro
   CodArt = F14cCodArt
   CodUbi = F14cUbiOri
   CanFis = F14cCanFis
   
   *> Calcular peso y volumen del movimiento a cambiar de ubicación.
   Store 0 to n_Peso, N_Volu
   Do PesVolAr In Ora_Ca00 With CodPro, CodArt, CanFis, n_Peso, N_Volu

   *> Desubicar ubicación actual, si existe. Se puede reubicar un MP pendiente
   *> de asignar ubicación, o bien que ha sido desubicado.
   If !Empty(CodUbi)
      l_Bi = .F.
      Do DesUbi In Ora_Ca00 With CodUbi, n_Peso, n_Volu, 1, l_bi
   EndIf

   *> Reubicar ubicación nueva.
   l_Bi = .F.
   Do CargaUbi In Ora_Ca00 With UbiNew, n_Peso, n_Volu, 1, l_Bi

   *> Actualizar nueva ubicación en el MP.
   _Where = "F14cNumMov='" + NumMov + "'"

   If !f3_UpdTun('F14c', , 'F14cUbiOri', 'UbiNew', , _Where, 'N')
      _LxErr = 'No se ha podido actualizar nueva ubicación en el MP' + cr
      Do Form St3Inc
      _LxErr = ''
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   =SqlCommit (_ASql)
Return

*>--------------------------------------------------------------------
*> Cambio de Operario en listas.
*> Llamado desde: SALTTRA -----> Tratamiento de listas.
*> Parámetros:
*>     NumLst ----> Nº de lista.
*>     NewOpe ----> Nuevo operario.
*>--------------------------------------------------------------------
Procedure ChgOpe
Parameters NumLst, NewOpe
Private _Where

     *> Reemplazar el operario en el detalle de la lista.
     _Where = "F26lNumLst='" + NumLst + "'"

     If !f3_UpdTun('F26l', , 'F26lCodOpe', 'NewOpe', , _Where, 'N')
        _LxErr = 'No se ha podido actualizar nuevo operario en la lista' + cr
        =Anomalias()
        =SqlRollBack(_ASql)
        Return .F.
     EndIf

     *> Reemplazar el operario en movimientos pendientes.
     _Where = "F14cNumLst='" + NumLst + "'"

     If !f3_UpdTun('F14c', , 'F14cCodOpe', 'NewOpe', , _Where, 'N')
        _LxErr = 'No se ha podido actualizar nuevo operario en los MPs' + cr
        =Anomalias()
        =SqlRollBack(_ASql)
        Return .F.
     EndIf

     *> Reemplazar el operario en la cabecera de la lista.
     _Where = "F26cNumLst='" + NumLst + "'"

     If !f3_UpdTun('F26c', , 'F26cCodOpe', 'NewOpe', , _Where, 'N')
        _LxErr = 'No se ha podido actualizar nuevo operario en la cabecera de la lista' + cr
        =Anomalias()
        =SqlRollBack(_ASql)
        Return .F.
     EndIf

     *> Actualizar estado de la lista.
     =Ora_AcCbLs(NumLst)

     =SqlCommit (_ASql)
Return

*>--------------------------------------------------------------------
*> Asignar movimientos de listas a otro Nº de lista.
*> Llamado desde: SALTTRA -----> Tratamiento de listas.
*> Parámetros:
*>     NewLst -----> Nueva lista a asignar a los MPs seleccionados.
*>--------------------------------------------------------------------
Procedure ChgLst
Parameters NewLst
Private MvtDestino
Local lStado

     Select TRALIS
     Locate For Marca==1
     Do While Found()
        _LxErr = ''

        If !ChgMvtLst(TRALIS.F26lNumMov, NewLst)
           _LxErr = _LxErr + 'No se ha podido cambiar lista al MP ' + TRALIS.F26lNumMov + cr
           =Anomalias()
           =SqlRollBack(_ASql)
           Return .F.
        EndIf

        *> Si es reposición, cambiar la lista al MP destino.
        If TRALIS.F26lTipLst=='R'
           MvtDestino = PadL(AllTrim(Str(Val(TRALIS.F26lNumMov))), 10, '0')
           If !ChgMvtLst(MvtDestino, NewLst)
              _LxErr = _LxErr + 'No se ha podido cambiar lista al MP destino ' + MvtDestino + cr
              =Anomalias()
              =SqlRollBack(_ASql)
              Return .F.
           EndIf
        EndIf

        *>
        Select TRALIS
        Continue
     EndDo

     =SqlCommit(_ASql)

Return

*>--------------------------------------------------------------------
*> Asignar UN movimiento de lista a otro Nº de lista.
*> Debe recibir _LxErr inicializado.
*> Parámetros:
*>      NumMov ----> Nº movimiento (en F26l/F14c).
*>      NewLst ----> Nueva lista.
*>--------------------------------------------------------------------
Procedure ChgMvtLst
Parameters NumMov, NewLst
Private OldLst, _Where, NuevoOperario
        
     *> Obtener Nº movimiento en lista.
     If !ExistF26l(NumMov)
        _LxErr = _LxErr + 'No existe el movimiento en la lista' + cr
        Return .F.
     EndIf
     OldLst = F26lNumLst

     *> Obtener el operario de la nueva lista.
     m.F26cNumLst = NewLst
     If !f3_seek('F26C')
        _LxErr = _LxErr + 'No se ha encontrado la nueva lista' + cr
        Return .F.
     Else
        If F26c.F26cEstLst = '3'
           _LxErr = _LxErr + 'Lista de trabajo en estado no válido' + cr
           Return .F.
        EndIf
     EndIf
     NuevoOperario = F26c.F26cCodOpe

     *> Reemplazar el nº de lista y el operario en la lista actual.
     _Where = "F26lNumMov='" + NumMov + "'"

     If !f3_UpdTun('F26l', , 'F26lNumLst,F26lCodOpe', 'NewLst,NuevoOperario', , _Where, 'N')
        _LxErr = _LxErr + 'No se ha podido actualizar nuevo nº de lista en la lista' + cr
        Return .F.
     EndIf

     *> Reemplazar el nº de lista y el operario en el MP.
     _Where = "F14cNumMov='" + NumMov + "'"

     If !f3_UpdTun('F14c', , 'F14cNumLst,F14cCodOpe', 'NewLst,NuevoOperario', , _Where, 'N')
        _LxErr = _LxErr + 'No se ha podido actualizar nuevo nº de lista en el MP' + cr
        Return .F.
     EndIf

     *> Actualizar estado de la lista nueva.
     =Ora_AcCbLs(NewLst)

     *> Actualizar estado de la lista antigua.
     =Ora_AcCbLs(OldLst)

     =SqlCommit (_ASql)
Return

*>*****************************************
*> Funciones De Desasignar Listas Oper.
*>*****************************************

*>En F14c Quitar mov. de lista y quitarle operario y lista.

Function UpdOperF14c
Parameter NMov

Private f_update, f_where, _ok

Select TRALIS

f_update = " Update F14c" + _em + ;
           " Set F14cCodOpe = '" + space(4) + "'," + ;
           "     F14cNumLst = '" + space(6) + "'" 

f_where  = " Where F14cNumMov='" + NMov + _cm 

Err = f3_SqlExec(_ASql,f_update+f_where)           

_ok= IIF(Err > 0,.T.,.F.) 
    

return _ok

*********************************************
*> Desasignar UN movimiento de una lista.
*********************************************
Function DesOper
Private NMov

  *> Comprobar si existe F14c
  Select TRALIS
  _okk=ExistF14c(F26lNumMov)
  If !_okk
     _lxerr=_lxerr + "No se encontró Movimiento [" + TRALIS.F26lNumMov + "] en F14c." + Cr
     Return .F.
  Else
     *> Si es reposición, comprobar movimiento destino.
     If TRALIS.F26lTipLst == 'R'
        NMov = PadL(AllTrim(Str(Val(TRALIS.F26lNumMov) + 1)), 10, '0')
        _okk=ExistF14c(F26lNumMov)
        If !_okk
           _lxerr=_lxerr + "No se encontró Movimiento [" + NMov + "] en F14c." + Cr
           Return .F.
        EndIf        
     EndIf        
  EndIf  
         
  *> Eliminar movimiento en F26l.................................
  Select TRALIS
  _okk=DelMovF26l()    && Borra el mvto. del F26l
  IF !_okk
     _lxerr=_lxerr + "No se pudo borrar Movimiento [" + TRALIS.F26lNumMov + "] en F26l." + Cr     
     return .F.
  EndIf  

  *> Actualizar cabecera de la lista, borrando si no quedan líneas de detalle.
  _okk = ORA_AcCbLs(TRALIS.F26lNumLst)
  If !_okk
     _lxerr=_lxerr + "No se pudo actualizar cabecera lista [" + TRALIS.F26lNumMov + "] en F26c" + Cr
     Return .F.
  EndIf
   
  *> Desasignar Operario y Lista del MP.
  _okk=UpdOperF14c(TRALIS.F26lNumMov) 
  If !_okk
     _lxerr=_lxerr + "No se pudo desasignar Movimiento [" + TRALIS.F26lNumMov + "] en F14c" + Cr
     Return .F.
  EndIf  

  *> Si es reposición, desasignar operario y lista de MP destino.
  If TRALIS.F26lTipLst == 'R'
     _okk=UpdOperF14c(NMov) 
     If !_okk
        _lxerr=_lxerr + "No se pudo desasignar Movimiento [" + NMov + "] en F14c" + Cr
        Return .F.
     EndIf  
  EndIf

Return .T.

*>------------------------------------------
*> Proceso de Desasignar Operario a Lista
*>------------------------------------------
Procedure DesLis
Private _ok

*>Proceso de desasignar movimientos de una lista.---------------------

      *> Inicializamos errores.---------------------------------------
      _lxerr= ""

      Select TRALIS
      Scan For Marca==1
         =WaitWindow("Desasignando Lista: " + TRALIS.F26lNumLst + Space(1) + ;
                     "Artículo: " + TRALIS.F26lCodArt + Space(1) + ;
                     "Nº Lote: " + TRALIS.F26lNumLot + Space(1) + ;
                     "Ubicación: " + TRALIS.F26lUbiOri + "-" + TRALIS.F26lUbiDes)

          _ok=DesOper()
         If _ok
            =SqlCommit (_ASql) 
         Else
            =SqlRollBack(_ASql)
         EndIf

         Select TRALIS
      EndScan

      If !Empty(_lxerr)
         Do Form St3inc
         _LxErr = ''
      EndIf

      Wait Clear

Return

*> Dividir un movimiento de una lista en dos.
Procedure DivMov
Lparameters Cant1, Cant2

Private f_update, f_where, f_insert, f_select 
Private _okk

**>División del movimiento.--------------------------------------------

*> Buscar el MP.
m.F14cNumMov = TRALIS.F26lNumMov
If !f3_seek('F14c')
   _LxErr = 'No existe Movimiento en MP [' + TRALIS.f26lNumMov + '] en F14c ' + cr
   Do Form St3Inc With .T.
   Return 
EndIf

*> Buscar la lista.
m.F26lNumMov = TRALIS.F26lNumMov
If !f3_seek('F26l')
   _LxErr = 'No existe movimiento en lista [' + TRALIS.f26lNumMov + '] en F26l ' + cr
   Do Form St3Inc With .T.
   Return 
EndIf

*> Actualizar la nueva cantidad en el MP.
f_update = " Update F14c" + _em + ;
           " Set    F14cCanFis=" + Str(Cant1) 
f_where  = " Where  F14cNumMov='" + TRALIS.F26lNumMov + _cm

_okk = f3_SqlExec(_ASql, f_update+f_where)
If _okk <= 0
   _LxErr = 'No se puede actualizar MP con cantidad ' + Str(Cant1) + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   =SqlRollBack(_ASql)
   Return 
EndIf 

*> Actualizar la nueva cantidad en la lista de trabajo.
f_update = " Update F26l" + _em + ;
           " Set    F26lCanFis=" + Str(Cant1) 
f_where  = " Where  F26lNumMov='" + TRALIS.F26lNumMov + _cm           
           
_okk = f3_SqlExec(_ASql, f_update+f_where)               
If _okk <= 0
   _LxErr = 'No se puede actualizar lista de trabajo con cantidad ' + Str(Cant1) + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   =SqlRollBack(_ASql)
   Return 
EndIf 

*>
*>Insertar en f14c y f26l mov. con Cant2. -----------------------------------------------
NumMov = Ora_NewMP()
Select F14c
Go Top
Replace F14cNumMov With NumMov, ;
        F14cCanFis With Cant2
If !f3_InsTun('F14c', , 'N')
   _LxErr = 'No se puede insertar MP con cantidad ' + Str(Cant2) + cr + ;
            'MENSAJE: ' + Message()
   Do Form St3Inc With .T.
   Return 
EndIf 

Select F26l
Go Top
Replace F26lNumMov With NumMov, ;
        F26lCanFis with Cant2
If !f3_InsTun('F26l', , 'N')
   _LxErr = 'No se puede insertar lista con cantidad ' + Str(Cant2) + cr + ;
            'MENSAJE: ' + Message()
   Do Form St3Inc With .T.
   Return 
EndIf 

*> Actualizar la cabecera de la lista.
=Ora_AcCbLs(TRALIS.F26lNumLst)

Return

*>Movimiento Masivo de Ubicaciones.---------------------------------
Procedure MovMas

   *> Declara el array.----------------------------------------------------
   DECLARE aubparmz(17)
   DO Carga_Datos IN prment.prg

   *> Bucle al fichero temporal.--------------------------------------
   Select MovMas
   GOTO TOP
   DO WHILE !EOF()

      *> Controlamos si queremos mover el registro.-------------------
      IF Marca=1

         *> Inicializamos variables.----------------------------------
         c_codpal = f16cnumpal
         codi_ubi = ""          && Retorno de la ubicacion destino
         Txt_Err  = ""          && Texto de porque no se puede ubicar
         UbiFor = IIf (!Empty(UbiDes),AlmUbi+UbiDes ,"" )

         *> Ubica la mercancia.---------------------------------------
         DO prc_ubi IN lib_cu00 WITH f16ccodart, f16csizpal,;
         f16ccanfis, UbiFor, f16cnumpal, f16cespico,;
         .F., .T., Codi_Ubi, Txt_err
      
         Select MovMas
         IF EMPTY(codi_ubi)
            =f3_Sn(1,4,"Imposible ubicar el palet "+c_codpal)
            Replace MovMas.Marca With 0
         ELSE
            REPLACE MovMas.UbiDes WITH Right(codi_ubi,9)
         ENDIF

      ENDIF
      
      *> Avanzamos puntero.-------------------------------------------
      Select MovMas
      Skip

   ENDDO

Return

*>
*> Dividir un movimiento de un MAC de fracciones en dos, dentro del mismo MAC.
*>   Recibe: NNmov - Nº movimiento del MAC.
*> Devuelve: .T.   - El MAC se puede procesar.
*>           .F.   - El MAC no se puede procesar (También devuelve _LxErr).

Procedure DividirMAC
Parameters NMov

   *> Leer los datos de movimiento.
   m.F26lNumMov = NMov
   If !f3_seek('F26L')
      _LxErr = _LxErr + 'No se pudo leer el movimiento en la lista' + cr
      Return .F.
   EndIf

   Select F26l
   Go Top

   *> Comprobar que el movimiento es de fracciones.
   If F26lOriRes # 'U'
      _LxErr = _LxErr + 'Este movimiento NO es de fracciones' + cr
      Return .F.
   EndIf

   *> Comprobar que el movimiento no esté confirmado.
   If F26lEstMov > '0'
      _LxErr = _LxErr + 'Este movimiento ya ha sido confirmado' + cr
      Return .F.
   EndIf

   *> Leer el MAC.
   m.F26vNumMac = F26l.F26lNumMac
   If !f3_seek('F26V')
      _LxErr = _LxErr + 'No existe este MAC' + cr
      Return .F.
   EndIf

   Select F26v
   Go Top

   *> Comprobar el estado general del MAC.
   If F26vEstBul=='3'
      _LxErr = _LxErr + 'Este MAC ya ha sido totalmente procesado.' + cr
      Return .F.
   EndIf

   *> Leer el movimiento en el MAC.
   m.F26wNMovMp = ThisForm.NMovMp
   If !f3_seek('F26W')
      _LxErr = _LxErr + 'No se pudo leer el movimiento en el MAC' + cr
      Return .F.
   EndIf

   Select F26w
   Go Top

   *> Comprobar el estado del esta línea del MAC.
   If F26wEstLin # '0'
      _LxErr = _LxErr + 'Esta línea ya ha sido procesada.' + cr
      Return .F.
   EndIf

Return .T.

*>
*> Verificar si un MAC (de fracciones) puede ser manipulado (editar, dividir, ...).
Procedure VerificarMAC
Parameters NMov


Return



PROCEDURE AceMas
PRIVATE n_status, c_ubiori, c_articulo, c_codpal, c_numlot
*> Se aceptan las ubicaciones

   *> Declara el array.----------------------------------------------------
   DECLARE aubparmz(17)
   DO Carga_Datos IN prment.prg

   *> Se define el array de datos.------------------------------------
   DECLARE a_datos(31)

   *> Crea los movimientos.-------------------------------------------
   Select MovMas
   GOTO TOP
   DO WHILE !EOF()

      IF !EMPTY(UbiDes)
      
         *> Inicializamos variables.----------------------------------
         c_ubiori   = m.Ps3_Ubic
         c_articulo = f16ccodart
         c_codpal   = f16cnumpal
         c_numlot   = f16cnumlot

         *> Ocupa la ubicacion destino.-------------------------------
         Select f16c
         SET ORDER TO Ind01
         SEEK(c_ubiori+c_codpal+c_articulo+c_numlot)

         *> Inicializamos los datos.-------------------------------
         a_datos(01) = f16cnumpal
         a_datos(02) = f16csizpal
         a_datos(03) = "3000"
         a_datos(04) = f16ctipent
         a_datos(05) = f16cnument
         a_datos(06) = f16cnumped
         a_datos(07) = f16ctipdoc
         a_datos(08) = f16cnotent
         a_datos(09) = f16cfecent
         a_datos(10) = f16cespico
         a_datos(11) = f16ctipent
         a_datos(12) = f16numpent
         a_datos(13) = f16ccodart
         a_datos(14) = f16csitstk
         a_datos(15) = f16ccanfis
         a_datos(16) = f16cfecfab
         a_datos(17) = f16cfcaduc
         a_datos(18) = f16cfcalda
         a_datos(19) = f16cnumlot
         a_datos(20) = f16cfctsrv
         a_datos(21) = f16cfctenv
         a_datos(22) = f16cfctpal
         a_datos(23) = ""
         a_datos(24) = ""
         a_datos(25) = f16cnumana
         a_datos(26) = Date()
         a_datos(27) = _Alma+MovMas.UbiDes
         a_datos(28) = f16ccodubi
         a_datos(29) = ""
         a_Datos(30) = ""
         a_Datos(31) = _Alma
         
         *> Generamos el movimiento.----------------------------------
         n_status = 0
         DO genmov IN lib_ca00 WITH a_datos, n_status

         *> Controlamos el movimiento.--------------------------------
         IF n_status = 2
            =f3_Sn(1,1,"Sit. stock erronea en palet "+a_datos(01))
         ENDIF

         *> Ocupa la ubicacion destino.-------------------------------
         Select f16c
         SET ORDER TO Ind01
         IF SEEK(c_ubiori+c_codpal+c_articulo+c_numlot)
            REPLACE f16ccanres WITH f16ccanres + f16ccanfis
         ENDIF

         *> Confirmacion automatica.----------------------------------
         IF aubparmz[09] = "S" 
            Select f14c
            LRet = .f.
            NReg = Recno()
            Do Prc_Conf in Lib_Ca00 with NReg, LRet
            IF !LRet
               =f3_Sn(1,4,"Error en la confirmación automática del","palet número : "+a_datos(01))
            ENDIF
         ENDIF
      ENDIF

      *> Seleccionamos y avanzamos el fichero de lineas.--------------
      Select MovMas
      Skip

   ENDDO

RETURN

PROCEDURE CanMas
*> Se procesa el fichero y se desubica

   *> Retornamos al fichero de lineas de palets.----------------------
   Select MovMas
   GOTO TOP
   DO WHILE !EOF()

      IF !EMPTY(UbiDes)
         *> Desubicamos.----------------------------------------------
         Store 0 To n_Peso, N_Volu
         Do PesVolAr In Lib_Ca00 With f16ccodart, f16ccanfis, n_Peso, N_Volu
         l_Bi=.f.
         Select MovMas
         DO DesUbi In Lib_Ca00 With _Alma+MovMas.UbiDes, n_Peso, n_Volu, 1, l_bi
      ENDIF

      *> Avanzamos el puntero.----------------------------------------
      Select MovMas
      Replace MovMas.UbiDes With Space(9)
      Skip

   ENDDO
   
RETURN

Procedure ListEtiq
Parameters N_registro
Private Lc_Descrip,lc_pres,lc_codin,lc_ean13,lc_Caduc,Lc_lote
*>Impresión de la Etiqueta

    Select F08c
    Seek m.CodArt
    C_Descri = F08cDescri 
    N_Serie = .F.
    ?'^XA'                      &&START DATA ENTRY
    ?'^BY2,2'           &&SET DEFAULT BAR RATIO
    ?'^CI0^CF0,16,20'   && SET DEFAULT FONT
    ?'^XZ'  
    Select EntMat
    LC_DESCRIP=TRIM(c_DesCri)
    LC_PRES=trim(F08c.F08cPresen)
    LC_CODIN=Alltrim(m.CodArt)
    lc_ean13=F08c.F08cCodEan
    LC_CADUC=DTOC(m.FecCad)
    LC_Lote = Alltrim(m.Lote)
	Texto=' Lot: '

    If F08c.F08cNumSer = "S"
       Select F51c
       Seek m.CodArt+m.TipDes+m.NumDoc
       Lc_Lote = Alltrim(F51cNumSer)
       Texto = '  Serial n.: '
       N_serie = .T.
    EndIf
    ?'^XA'
    ?'^ADN,8,10^FO150,340^FWN^FDHospital "Severo Ochoa"^FS'
    ?'^ADN,8,10^FO30,30^FWN^FD'+Lc_Descrip+'    '+lc_pres+'^FS'
    ?'^ADN,8,10^FO30,90^FWN^FDRef: '+LC_CODIN+Texto+Lc_lote
    If !Empty(m.FecCad)
       ??'  Exp.: '+lc_caduc
    EndIf 
    ??'^FS'

*	??Texto
*   ??lc_lotE
*   ??'  Exp.: '+lc_caduc+'^FS'

    *
    * BARCODE ean13
    ?'^BY2,,5^FO30,110^BC,80,N,N^FD>;>8010'+LC_EAN13+'^FS'

    * texto de EAN13 
    ?'^ADN,8,10^FO30,200^FWN^FD(01) 0'+LC_EAN13+'^FS'
    If !Empty(Lc_lote)
     If N_serie = .T.
      ?'^BY2,2^FO30,225^BC,80,N,N^FD>;>821>6'+lc_lote+'^FS'
      ?'^ADN,8,10^FO30,315^FWN^FD(21) '+lc_lote 
     Else
      If !Empty(Lc_Caduc)
         If len(lc_lote)>7     && ver si el lote tiene más que 7 carácteres
	        ?'^BY1,2'            && más que 7, barcode comprimida
         Else
	        ?'^BY2,2'            && si tiene <7 siete barcode normal
	     EndIf
         ??'^FO30,225^BC,80,N,N^FD>;>817'+LC_Caduc
         ??'10>6'+LC_LOTE
         ??'^FS'
         ?'^ADN,8,10^FO30,315^FWN^FD (17) '+lc_Caduc
         ??'  (10) '+lc_lote

       Else
         * si no hay fecha, barcode lote,modif. antes 85 ahora 80
	     ?'^BY2,2^FO30,225^BC,80,N,N^FD>;>810>6'+LC_LOTE+'^FS'
	     * si no hay fecha, texto de lote,modif. antes 260 ahora 250
         ?'^ADN,8,10^FO30,315^FWN^FD (10) '+LC_LOTE+'^FS'
      EndIf
    EndIf 
    EndIf
    ?'^PQ1,0,1,Y^XZ'

Return

Procedure ConCal
*>Control de Calidad.--------------------------------------------

   *> Abrimos los ficheros.-------------------------------------------
   Select f16c
   set order to Ind01

   *> Bucle al fichero temporal.--------------------------------------
   Select ConCal
   Go Top
   Scan 
      =WaitWindow("Realizando cambio de situación")
      c_clave = f16ccodubi+f16cnumpal+f16ccodart+f16cnumlot
      IF !Empty(marca)
*        replace f16csitstk with m.SitDes
         Select f16c
         IF seek(c_clave)
          If Rlock() = .T.
            replace f16csitstk with m.SitDes
            Do actualst in lib_ca00 with f16ccodart, f16ccanfis, m.SitOri, .F.
            Do actualst in lib_ca00 with f16ccodart, f16ccanfis, m.SitDes, .T.

            *> Dar de alta en el f20c por cambio de SitStk.-----------
            *> Generamos un nuevo registro en historico.-----------------
            Select f20c
            APPEND BLANK
            REPLACE f20ccodart WITH f16c.f16ccodart,;
                    f20cnpalet WITH f16c.f16cnumpal,;
                    f20ccantid WITH f16c.f16ccanfis,;
                    f20ctipmov WITH "3400",;
                    f20csitstk WITH m.SitOri,;
                    f20cfecmov WITH Date(),;
                    f20chormov WITH time(),;
                    f20ctipdoc WITH f16c.f16ctipdoc,;
                    f20cnumdoc WITH f16c.f16cnumped,;
                    f20ctentra WITH "S",;
                    f20ctipent WITH f16c.f16ctipent,;
                    f20ccodent WITH f16c.f16cNumEnt,;
                    f20cnulote WITH f16c.f16cnumlot,;
                    f20cubides WITH f16c.f16cCodUbi,;
                    f20ctipcom WITH " ",;
                    f20ccodcom WITH "",;
                    f20ccodope WITH "",;
                    f20ctampal WITH f16c.f16csizpal,;
                    f20cruthab WITH "",;
                    f20cespico WITH f16c.f16cespico,;
                    f20cnumped WITH f16c.f16cnumped,;
                    f20CLinPro With F16c.F16cLinPro,;
                    F20cCodALm With _Alma

              *>Registro de cambio.-----------------------------------
              Scat To R
              Append Blank
              Gath From r
              Replace F20cSitStk With m.SitDes,;
                      F20cTentra With "E"

          Else
              =f3_sn(1,4,"El movimiento del Palet "+F16cNumPal+" no se ha podido crear.")
          EndIf
          Select F16c
          UnLock
         EndIf 
         Select ConCal
      ENDIF
   EndScan   

   *> Imprimimos.-----------------------------------------------------
   Select ConCal
   Go top
   Set Relation to F16cCodArt into F08c
   _Label1 = "Listado Control Calidad"
   Do form st3imp2 with _Label1,'ConCAll1','ConCal'
   Set relation to

Return

PROCEDURE GenAlb
PARAMETERS c_tip_doc, c_num_doc,C_Tip_Ent,C_Cod_Ent

   *> Abrir ficheros.---------------------------------------------------
   DO FSelec WITH "F27C","Codigo","0"
   DO FSelec WITH "F27l","codigo","0"
   DO FSelec WITH "F08c","codigo","0"

   STORE 1 to n_ant

   *> Obtengo el número de traspaso
   Do FSelect With 'F00c', 'CODIGO', '0'
   Seek 'TNU' + 'ALBA'
   If Found()
      NAlbaran = Right('0000000000' + Alltrim(Str(F00c.F00cNumero)), 10)
      Replace F00c.F00cNumero With F00c.F00cNumero + 1
   EndIf
   
   *> Buscamos el albar n.---------------------------------------------
   Select F27c
*   IF !SEEK(Mvar(2)+Mvar(3)+NAlbaran)
    IF !SEEK(AllTrim(NAlbaran))
      n_ant = 0
      APPEND BLANK
      REPLACE F27cNumAlb WITH NAlbaran,;
              F27cTipDoc WITH C_Tip_Doc,;
              F27cNumDoc WITH C_Num_Doc,;
              F27cTipEnt WITH C_Tip_Ent,;
              F27cCodEnt WITH C_Cod_Ent
   ENDIF

   *> Cambiamos datos comunes.----------------------------------------
*   REPLACE F27cCodOpe With Mvar(1),   F27cNumKgs With Mvar(14), ;
*           F27cNumBul With Mvar(15),  F27cNumEti With Mvar(16), ;
*           F27cCodRut With Mvar(17),  F27cFecEmb With Mvar(19)

   *> Generamos Indice por artículo+línea en L.D.-----------------------
   IndAux = FnTemp(1)
   Select F26c
   INDEX ON F26cCodArt+F26cNumLin TO (IndAux) FOR !DELETED() .AND. ;
            F26cTipDoc==C_Tip_Doc .AND. F26cNumPed==C_Num_Doc
***>11/05/95 Empty(f26cnumdes)
   
   *> Acumulamos la cantidad de cada Artículo-Línea y lo añadimos al----
   *> fichero de líneas de albaranes.-----------------------------------
   GO TOP
   DO WHILE !EOF() .AND. n_ant = 0

      Articulo = F26cCodArt
      Linea    = F26cNumLin
      Palet    = F26cnumpal
      Cantidad = 0
      do WHILE (Articulo+Linea == F26cCodArt+F26cNumLin) .AND. !EOF()
         IF F26cTipMov # "3000"
            store Cantidad+(F26cCanFis)  TO Cantidad
         Endif
         Skip
      enddo   
      
      *> Buscamos el articulo.----------------------------------------
      Select f08c
      SEEK articulo

      *> Buscamos cabecera.-------------------------------------------
      Select F27l
      IF !SEEK(NAlbaran+Articulo+Linea)
         APPEND BLANK
         REPLACE F27lNumAlb WITH NAlbaran,;
                 F27lCodArt WITH Articulo,;
                 F27lNumLin WITH Linea
      ENDIF
      
      *> Grabamos datos comunes.--------------------------------------
      REPLACE F27lCanSer WITH F27lCanSer + Cantidad,;
              F27lPesBru WITH F27lPesBru + (Cantidad*F08c.F08cPesEnv),;
              F27lPesNet WITH F27lPesNet + (Cantidad*F08c.F08cPesoKg)
      Select F26c
   ENDDO

   n_ant = 1

   *> Cancelamos Indice.----------------------------------------------
   Select F26c
   SET INDEX TO
   DELETE FILE (IndAux)

RETURN

*> Ocupaciones candidatas a reponer Ubic Picking Cajas--------------------------
*> Estarán ordenadas por Lote, Fec.Cad y Fec. Ent.------------------------------ 
Function OcuRep
Parameters TipUbi
Private f_select, f_from, f_where, f_orden, lx_sql, _ok

Select REPPICK

f_select = " Select F16cCodPro, F16cCodArt, F16cNumLot, F16cFecCad, F16cFecEnt," + ; 
           " F16cCodUbi, F16cSitStk, F16cNumPal, " + ;
           _GCN("F16cCanFis-F16cCanRes") + " As CanDis," + ;
           _GCN("F16cUniPac") + " As F16cUniPac, " + _GCN("F16cPacCaj") + " As F16cPacCaj," + ;
           "F16cCanFis-F16cCanRes As Disponible, F16cUniPac*F16cPacCaj As FactorCaja, 0 As Sueltas"
 
f_from   = " From F16c" + _em + ", F10c" + _em

f_where  = " Where F16cCodPro='" + F12cCodPro + _cm + ;
           " And   F16cCodArt='" + F12cCodArt + _cm + ;
           " And   F10cCodubi=F16cCodUbi" + ;
           " And   F10cPickSN='" + TipUbi + _cm + ; 
           " And   F16cCanFis-F16cCanRes > 0"

f_orden  = " Order by F16cCodPro, F16cCodArt, F16cFecCad, F16cFecEnt, Disponible"

lx_sql = f_select + f_from + f_where + f_orden

Err = f3_SqlExec(_ASql,lx_sql,'OCUREP')
If Err <= 0
   Return .F.
EndIf

Select OCUREP
Go Top
Do While !Eof()
   Replace Sueltas With Mod(Disponible, FactorCaja)
   Skip
EndDo

Go Top
_ok = !Eof()
Return _ok

*> Comprobar si existe una ocupación.
Function ExisteOcup
Parameters Ubica
Private f_select,  f_where
Select OCUREP

f_select  = " Select * From F16c" + _em 

f_Where   = " Where  F16cCodPro = '" + F16cCodPro + _cm + ;
            " And    F16cCodArt = '" + F16cCodArt + _cm + ;
            " And    F16cCodUbi = '" + Ubica + _cm + ;
            " And    F16cNumPal = '" + F16cNumPal + _cm + ;
            " And    F16cNumLot = '" + F16cNumLot + _cm + ;
            " And    F16cSitStk = '" + F16cSitStk + _cm + ;
            " And    F16cFecCad = " + _GCD(F16cFecCad)

Err =f3_SqlExec(_ASql,f_select+f_where)             
IF Err >= 0
   Go Top
   Err=IIF(eof(),0,1)
EndIf

return Err

*>------------------------------------------------------------
*> Rellenar las propiedades para la función de actualización.
*>------------------------------------------------------------
Procedure LlenarAcFnActz

*****>FncA.ObjParm.PONEnt=
*****>FncA.ObjParm.PMTMov=
FncA.ObjParm.POTDoc=F16CTIPDOC   
FncA.ObjParm.PONdoc=F16CNUMDOC   
FncA.ObjParm.POLDoc=F16CLINDOC   
*****>FncA.ObjParm.PMFDoc=
*****>FncA.ObjParm.PMDAso=
*****>FncA.ObjParm.PONPed=
*****>FncA.ObjParm.POLPed=
FncA.ObjParm.POCArt=F16CCODART   
FncA.ObjParm.PONLot=F16CNUMLOT   
FncA.ObjParm.POSStk=F16CSITSTK   
FncA.ObjParm.POFCad=F16CFECCAD   
FncA.ObjParm.POCFis=F16CCANFIS   
*****>FncA.ObjParm.PMRHab=
FncA.ObjParm.PUbOld=F16CCODUBI   
*****>FncA.ObjParm.PUBNew=
FncA.ObjParm.PONPal=F16CNUMPAL   
FncA.ObjParm.POTPal=F16CTAMPAL   
FncA.ObjParm.POFUni=F16CUNIVEN   
FncA.ObjParm.POFSer=F16CUNIPAC   
FncA.ObjParm.POFEnv=F16CPACCAJ   
FncA.ObjParm.POFPAL=F16CCAJPAL   
FncA.ObjParm.POFFab=F16CFECFAB   
****>FncA.ObjParm.POFCal=
FncA.ObjParm.PONAna=F16CNUMANA   
FncA.ObjParm.POCPro=F16CCODPRO   
*****>FncA.ObjParm.PMCOpe=
*****>FncA.ObjParm.PMNLst=
*****>FncA.ObjParm.PMNExp= 

*****>FncA.ObjParm.PMORes=
*****>FncA.ObjParm.PMTUbi= 
*****>FncA.ObjParm.PMTMas=
*****>FncA.ObjParm.PMNMas=
*****>FncA.ObjParm.PMMUni=
*****>FncA.ObjParm.PMORut=
*****>FncA.ObjParm.PMTERe=
*****>FncA.ObjParm.PMCERe=
*****>FncA.ObjParm.PMVHab=
*****>FncA.ObjParm.PMTMac=
*****>FncA.ObjParm.PMNMac=
*****>FncA.ObjParm.PMSecc=
FncA.ObjParm.POPico=F16cEsPico
FncA.ObjParm.POCAlm=_Alma
FncA.ObjParm.PMFlg1=F16CFLAG1    
FncA.ObjParm.PMFlg2=F16CFLAG2    

Return

*>----------------------------------------------------------
*> Grabar Movimientos pendientes de reposiciones.
*> Recibe: Tipo movimiento(E/S), Tipo movimiento en MP.
*>----------------------------------------------------------
Function GrabMPF14c
Parameters EntSal, TipMov

Private _ok

FncA.ObjParm.PMEnSa = EntSal    
FncA.ObjParm.PMFgMP = 'S'
FncA.ObjParm.PMTMov = TipMov
*   FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMTMac = 'MSTD'
FncA.ObjParm.PMFMov = Date()
FncA.ActMP

_ok = Iif(FncA.ObjParm.PWCRtn >= '50', .F., .T.)

Return _ok

*>-----------------------------------------------------------------
*> Aumentar Cantidad reservada en la cantidad que pasará a picking.
*>-----------------------------------------------------------------
Function OutCand 
Parameters CanOut

Private f_update , f_delete, f_where

Select SQLRESULT

f_Where   = " Where  F16cCodPro = '" + F16cCodPro + _cm + ;
            " And    F16cCodArt = '" + F16cCodArt + _cm + ;
            " And    F16cCodUbi = '" + F16cCodUbi + _cm + ;
            " And    F16cNumPal = '" + F16cNumPal + _cm + ;
            " And    F16cNumLot = '" + F16cNumLot + _cm + ;
            " And    F16cSitStk = '" + F16cSitStk + _cm + ;
            " And    F16cFecCad = " + _GCD(F16cFecCad)


IF NCantidad > 0 
   
   f_update = " Update F16c" + em + ;
              " Set F16cCanRes="  + Str(CanOut)
              
   Err = f3_SqlExec(_ASql,f_update + f_where)                           
EndIf

_ok=IIF(Err < 0,.F.,.T.) 

return _ok

*>------------------------------------------------------------------------------------
*> Aumentar cantidad reservada candidata, en la cantidad que se cede a Ubic. Picking.
*>------------------------------------------------------------------------------------
Function ActOcuCand

Parameters Cantidad
Private f_update, f_delete, f_where, _ok, NCantidad 

*> seleccionar Ocupación que aumentará su cantidad reservada para Ocup. de Picking donde añadir
*> Cantidad física 

Err= ExisteOcup(OCUREP.F16cCodUbi)   
IF Err < 1  && Se ha Producido un error o ocupación no existe 
   return .F.
Else
   do LlenarAcfnActz && rellena campos que se usarán al grabar movtos en histórico
EndIf   

NCantidad = IIF(SQLRESULT.F16cCanRes<0,0,SQLRESULT.F16cCanRes) +  Cantidad

*> Aumentar la CanRes que se pasará a Picking
_ok=OutCand(NCantidad)

return _ok


*> actualizar Ocup.  actual de Picking.  
Function ActOcup
Private _ok
Private SOcuCan && suma la cantidad de las ocupaciones candidatas para llegar a llenar 
                && la ocupación de Picking
Private CFalta  && La cantidad suficiente para llenar picking en caso que no se necesite toda 
                && la disponible de Ocup. candidata
                                 
SOcuCan=0
=f3_SqlExec(_ASql,"SAVEPOINT MARCA") 

Select OCUREP
Go Top

Do While !eof() .and. REPPICK.CanRep > SOcuCan

  FncA.ObjParm.Inicializar && Inicializar campos nuevo registro histórico.  

  If REPPICK.CanRep >= SOcuCan + OCUREP.CanDis 

     *> Como la Ocu. Candidata no tiene Cant. suficiente ó es justa,---------------
     *> Se la quitamos de su antigua ocupación, si CanFis fuera <=0 Delete Ocup.---  
     _ok=ActOcuCand(OCUREP.CanDis) 
     
     If _ok && grabar movimiento salida en histórico
        FncA.ObjParm.POCFis=OCUREP.CanDis
        _OldMP = Ora_NewMP()                     && Nº movimiento salida.
        _NewMP = Ora_NewMP()                     && Nº movimiento entrada.
        FncA.ObjParm.PMNMov = _OldMP
        _ok=GrabMPF14c('S', TipMovS)
        If _ok
           FncA.ObjParm.PUbOld=REPPICK.F12cCodUbi
           FncA.ObjParm.PMNMov = _NewMP
           _ok=GrabMPF14c('E', TipMovE)
        EndIf
     EndIf


     IF _ok
        SOcuCan = SOcuCan + OCUREP.CanDis
     EndIf

  Else

     *> Cuando la ocupación cantidata a reponer ubic. de picking tiene más cantidad disponible 
     *> de la necesaria.

     CFalta=int((REPPICK.CanRep-SOcuCan)/(OCUREP.F16cUniPac*OCUREP.F16cPacCaj)) && 1/0=?
     CFalta=CFalta*(OCUREP.F16cUniPac*OCUREP.F16cPacCaj)
     
     If CFalta > 0 && Si candidata cederá alguna  caja, tratarla 
     

        _ok=ActOcuCand(CFalta)

        If _ok && grabar movimiento salida en histórico
           FncA.ObjParm.POCFis=CFalta
           _OldMP = Ora_NewMP()                  && Nº movimiento salida.
           _NewMP = Ora_NewMP()                  && Nº movimiento entrada.
           FncA.ObjParm.PMNMov = _OldMP
           _ok=GrabMPF14c('S',TipMovS)
           If _ok
              FncA.ObjParm.PUbOld=REPPICK.F12cCodUbi
              FncA.ObjParm.PMNMov = _NewMP
              _ok=GrabMPF14c('E',TipMovE) 
           EndIf
        EndIf

        IF _ok
           SOcuCan = SOcuCan + CFalta
        EndIf

     Else   
        _ok=.F.
     EndIf   

  EndIf                           

  IF _ok 
     *> por si e de deshacer cambios en transacciones sig. Ocup. Candidata,---------- 
     *> que no afecte a esta que acabo satisfactoriamente.
     =f3_SqlExec(_ASql,"SAVEPOINT MARCA") 
  Else
     *> para que Rollback no afecte a las anteriores. 
     =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT MARCA") 
  EndIf
                           
  Select OCUREP
  Skip
EndDo

Return _ok

*>------------------------------------------------------------------------------
*> Se reponen las ubicaciones de picking bajo mínimo con ocupaciones candidatas.
*>------------------------------------------------------------------------------
Procedure RepCajas

Public TipMovS, TipMovE && tipos de mvto. de Salida y de entrada
Private FncA, PrmA
Private _okk

TipMovS='3570' && Salida Reposición de cajas  
TipMovE='3070' && Entrada Reposición de Cajas

*> Crear objetos actualización.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
FncA.ObjParm = PrmA

_lx_err=''

Select REPPICK
Go Top

Do While !Eof()
   *> Comprobar si ya hay alguna reposición para este Propietario/Artículo/Ubicación.
   If !ERepAnt(F12cCodPro, F12cCodArt, F12cCodUbi)
      Select _F12cCajToUni
      Skip
      Loop
   EndIf

   *> diferencia hasta máximo que se pueda reponer en actual ubicación picking
   _okk=CRepUbic()
   IF !_okk
      Select REPPICK
      _lxerr= _lxerr + "Fallo calcular Cant. a reponer["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr 
      Skip
      Loop
   EndIf

   *> Buscar Ocupaciones candidatas para actual ubicación de Picking Cajas-----------
  _okk=OcuRep('N')
  IF !_okk
    =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT PICK")
    Select REPPICK
    _lxerr= _lxerr + "Fallo buscar Ocup. candidatas["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr 
    Skip
    Loop
  EndIf
  
  *> Proceso que rellena la ocup. de Picking Cajas con ocupaciones candidatas-----
  _okk=ActOcup()
  IF !_okk
     =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT PICK")
    Select REPPICK
   _lxerr= _lxerr + "Fallo al poner cant. ubic. Cand. en Pick.["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr     
    Skip
    Loop
  EndIf

  =f3_SqlExec(_ASql,"SAVEPOINT PICK") 
  
  Select REPPICK
  Skip
EndDo

=SqlCommit (_ASql)

IF !empty(_lxerr)
   do Form St3Inc
EndIf

Return 

*>-----------------------------------------------------------------------------------
*> Generación de reposiciones de CAJAS a UNIDADES (Fracciones).
*> Genera cursor REPPICK y llama a proceso de generación de reposiciones.
*>
*> Llamado desde: SALIREPU ---> Generación de reposiciones a UNIDADES.
*>-----------------------------------------------------------------------------------
Procedure RepCajToUni

Private f_select, f_from, f_where, f_orden, lx_sql
Private Err

=f3_sn(1,1,"Este proceso bloqueará las ubicaciones de picking bajo mínimos,",;
           "y las ocupaciones candidatas a reponer a dichas ubicaciones")

If !f3_sn(2, 1, 'Generar reposiciones de UNIDADES')
   Return
EndIf

*> Selecciona todas las ocupaciones de picking unidades que no lleguen al stock mínimo.
*> Condición de selección: Artículos dentro de los rangos, con ubicación que tenga
*> Picking = 'U' (F10cPuickSn), y con stock es esas ubicaciones F16cCanFis), menor que
*> el stock mínimo establecido (F12cCanMin).
f_select = " Select F08cCodPro, F08cCodArt"
 
f_from   = " From F08c" + _em
 
f_where  = " Where F08cCodPro Between '" + m.CodProI + "' And '" + m.CodProF + _cm + ;
           " And   F08cCodArt Between '" + m.CodArtI + "' And '" + m.CodArtF + _cm

f_orden  = " Order By F08cCodPro, F08cCodArt"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F08cCajToUni')
If Err <= 0
   _LxErr = 'Error cargando artículos para generar reposiciones' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   =SqlRollBack(_ASql)
   Return .F.
EndIf

Select _F08cCajToUni
Go Top
Do While !Eof()
   Do RepCajUni With F08cCodPro, F08cCodArt

   *>
   Select _F08cCajToUni
   Skip
EndDo

If Used('_F08cCajToUni')
   Use In _F08cCajToUni
EndIf

If Used('_F16cOcuRep')
   Use In _F16cOcuRep
EndIf

*>------------------------------------------------------------
*> Reposición a UNIDADES de un Propietario/Artículo.
*> Opcionalmente recibe una ubicación concreta donde realizar
*> la reposición, p.ej., si se llama desde reservas. (no impl).
*> Recibe: Propietario/Artículo a generar reposiciones.
*>------------------------------------------------------------
Procedure RepCajUni
Parameters CodPro, CodArt, UbiUni

Public TipMovS, TipMovE
Private FncA, PrmA
Private _okk
Private f_select, f_from, f_where, f_orden, lx_sql
Private CanRepAct

TipMovS = '3560'                      && Salida origen.
TipMovE = '3060'                      && Entrada destino.

*> Comprobar que existe el tipo de movimiento ORIGEN.
m.F00bCodMov = TipMovS
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento ORIGEN (3560)' + cr
   Do Form St3Inc
   _LxErr = ''
   =SqlRollBack(_ASql)
   Return .F.
EndIf

*> Comprobar que existe el tipo de movimiento DESTINO.
m.F00bCodMov = TipMovE
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento DESTINO (3060)' + cr
   Do Form St3Inc
   _LxErr = ''
   =SqlRollBack(_ASql)
   Return .F.
EndIf

*> Selecciona todas las ocupaciones de picking unidades de este artículo que no lleguen
*> al stock mínimo. Selección: ubicaciones que tengan Picking = 'U' (F10cPickSn), y con
*> stock es esas ubicaciones F16cCanFis), menor que el stock mínimo establecido (F12cCanMin).

f_select = " Select F12cCodPro, F12cCodArt, F12cCodAlm, F12cCodUbi, F12cPriori," + ;
           " F12cCanMin, F12cCanMax, 0 CanRep"
 
f_from   = " From F12c" + _em + "," + ;
           "      F10c" + _em    
 
f_where  = " Where F12cCodPro='" + CodPro + "'" + ;
           " And   F12cCodArt='" + CodArt + "'" + ;
           " And   F10cCodUbi=F12cCodUbi" + ;
           " And   F10cPickSN='U'" + ; 
           " And   " + _GCN("F12cCanMin") + " >=(Select " + ;
           _GCN("Sum(F16cCanFis)") + ;
           " From F16c" + _em + ; 
           " Where F16cCodUbi=F12cCodUbi" + ;
           " And   F16cCodPro=F12cCodPro" + ;
           " And   F16cCodArt=F12cCodArt)"

f_orden  = " Order By F12cPriori, F12cCodPro, F12cCodArt, F12cCodUbi"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F12cCajToUni')
If Err <= 0
   _LxErr = 'Error cargando datos para generar reposiciones' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   =SqlRollBack(_ASql)
   Return .F.
EndIf

*> Crear objetos actualización.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
FncA.ObjParm = PrmA

Select _F12cCajToUni
Go Top

Do While !Eof()
   *> Si ya existe reposición para esta ubicación, no hacerla esta vez.
   Scatter MemVar
   If !ERepAnt(F12cCodPro, F12cCodArt, F12cCodUbi, TipMovE)
      Select _F12cCajToUni
      Skip
      Loop
   EndIf
  
   *> Cantidad actual en ocupaciones de este artículo/ubicación.
   CanRepAct = CRepUbic(F12cCodPro, F12cCodArt, F12cCodUbi)
   Select _F12cCajToUni
   If CanRepAct >= F12cCanMin
      Skip
      Loop
   EndIf

   *> Buscar ocupaciones candidatas para reposición.
   *> Recupera cursor _F16cOcuRep, con las ocupaciones encontradas.
   Select _F12cCajToUni
   If !GetOcuRep(F12cCodPro, F12cCodArt, 'S')
      Select _F12cCajToUni
      Skip
      Loop
   EndIf

   *> Proceso que genera las reposiciones de cajas a unidades.
   *> Repone por cantidad máxima.
   Select _F12cCajToUni
   Replace CanRep With F12cCanMax - CanRepAct

   *> Actualizar las reposiciones a unidades del Artículo/Ubicación actual.
   If ActCajToUni()
      =SqlCommit(_ASql)
   Else
      =SqlRollBack(_ASql)
   EndIf

   *>
   Select _F12cCajToUni
   Skip
EndDo

=SqlCommit (_ASql)

Return 

*>-----------------------------------------------------------------------------------
*>      REPOSICIONES DINÁMICAS
*>-----------------------------------------------------------------------------------

*> Crea movimientos en F14c que reflejan reposiciones dinámicas
Function HRepDin
Parameters Cantidad     

Private _ok, Nmovs, CantMov, Cont 

If Cantidad <= 0 
   return .T.
EndIf


Select OCUREP

Nmovs=1     && número de mvtos. que genera ésta candidata. 1 si basta con sueltas. 
CantMov=F16cUniPac*F16cPacCaj  && unidades por movto. por defecto las que componene 1 caja.
If Mod(cantidad,F16cUniPac*F16cPacCaj)<> 0
   CantMov = Cantidad
Else
   Nmovs = Cantidad / CantMov   
EndIf

FncA.ObjParm.POCFis=CantMov

*> Coger número de caja
FOR Cont = 1 TO Nmovs

   FncA.ObjParm.PUbOld=F16cCodUbi
   _OldMP = Ora_NewMP()                          && Nº movimiento salida.
   _NewMP = Ora_NewMP()                          && Nº movimiento entrada.
   FncA.ObjParm.PMNMov = _OldMP
   _ok=GrabMPF14c('S',TipMovS)
   If _ok
      FncA.ObjParm.PUbOld=REPPICK.F12cCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok=GrabMPF14c('E',TipMovE)
   EndIf
   
   If !_ok
      return _ok   
   EndIf

ENDFOR


return _ok

*> actualizar Ocupación dinámica
Function ActOcupDin
Private _ok
Private SOcuCan && suma la cantidad de las ocupaciones candidatas para llegar a llenar 
                && la ocupación de Picking
Private CFalta  && La cantidad suficiente para llenar picking en caso que no se necesite toda 
                && la disponible de Ocup. candidata
                                 
SOcuCan=0
_ok = .F.               

=f3_SqlExec(_ASql,"SAVEPOINT MARCA") 

Select OCUREP
Go Top

Do While !eof() .and. REPPICK.CanRep > SOcuCan  && seguir trantando candidatas mientras 
                                                && pueda seguir llenando picking unidades.
   FncA.ObjParm.Inicializar && Inicializar campos nuevo registro histórico.  

   If OCUREP.sueltas >= REPPICK.CanRep - SOcuCan    && si con unidades sueltas bastante. 

      _ok=ActOcuCand(REPPICK.CanRep - SOcuCan)      && Aumentar cantidad reservada de Candidata.
      If _ok
         _ok=HRepDin(REPPICK.CanRep - SOcuCan)      && reflejar Reposición a Ubic. Din. en F14c.
         If _ok
            SOcuCan=REPPICK.CanRep 
         EndIf   
      EndIf

   Else                                             && si necesito alguna caja. 

      If REPPICK.CanRep >= SOcuCan + OCUREP.CanDis    && Si cantidad total Candidata no sufic.
         _ok=ActOcuCand(OCUREP.CanDis) 
         If _ok
            _ok=HRepDin(OCUREP.Sueltas)  && 1 caja de más de 1 unidad suelta.       
            If _ok
               _ok=HRepDin(OCUREP.CanDis - OCUREP.sueltas) && n cajas completas.
               If _ok
                  SOcuCan = SOcuCan + OCUREP.CanDis      
               EndIf   
            EndIf
         EndIf
      Else                               && basta con las sueltas más alguna caja.
         CFalta=int((REPPICK.CanRep - SOcuCan - OCUREP.sueltas)/(OCUREP.F16cUniPac*OCUREP.F16cPacCaj)) && 1/0=?
         CFalta=CFalta*(OCUREP.F16cUniPac*OCUREP.F16cPacCaj)
         IF CFalta > 0 
            _ok=ActOcuCand(CFalta + OCUREP.Sueltas)
            If _ok
               _ok=HRepDin(OCUREP.Sueltas)  && 1 caja de unidades sueltas.         
               If _ok
                  _ok=HRepDin(CFalta)      
                  If _ok
                     SOcuCan = REPPICK.CanRep
                  EndIf 
               EndIf   
            EndIf
         Else 
            _ok=.F.
         EndIf
      EndIf

   EndIf

   IF _ok 
      *> por si e de deshacer cambios en transacciones sig. Ocup. Candidata,---------- 
      *> que no afecte a esta que acabo satisfactoriamente.
      =f3_SqlExec(_ASql,"SAVEPOINT MARCA") 
   Else
      *> para que Rollback no afecte a las anteriores. 
      =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT MARCA") 
   EndIf
                           
   Select OCUREP
   Skip
EndDo


Return _ok

*> Reposiciones dinámicas
Procedure RepDin

Public TipMovS, TipMovE && tipos de mvto. de Salida y de entrada
Private FncA, PrmA
Private _okk

TipMovS='3550' && Salida Reposiciones dinámicas
TipMovE='3050' && Entrada Reposiciones dinámicas

*> Crear objetos actualizacion.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
FncA.ObjParm = PrmA


_lx_err=''

=f3_SqlExec(_ASql,"SAVEPOINT PICK") 

Select REPPICK
Go Top

Do While !eof()
  *> Si Ya existe Reposición de Ubicación, no hacerla esta vez  
  _okk=ERepAnt()
    IF !_okk
     =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT PICK")
     Select REPPICK
     _lxerr= _lxerr + "Hay Reposición Pdte. en["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr 
     Skip
     Loop
  EndIf

  *> diferencia hasta máximo que se pueda reponer en actual ubicación dinámica
  _okk=CRepUbic()
  IF !_okk
     =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT PICK")
     Select REPPICK
     _lxerr= _lxerr + "Error al calcular cantidad a reponer["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr 
     Skip
     Loop
  EndIf

   *> Buscar Ocupaciones candidatas para actual ubicación dinámica-----------
  _okk=OcuRep('S')
  IF !_okk
    =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT PICK")
    Select REPPICK
    _lxerr= _lxerr + "Fallo buscar Ocup. candidatas["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr 
    Skip
    Loop
  EndIf

  *> Proceso que rellena la Ubicación dinámica con ocupaciones candidatas-----
  _okk=ActOcupDin()
  IF !_okk
     =f3_SqlExec(_ASql,"ROLLBACK TO SAVEPOINT PICK")
    Select REPPICK
    _lxerr= _lxerr + "Fallo al poner cant. ubic. Cand. en Pick.["+f12cCodPro+","+F12cCodArt+","+F12cCodUbi+"]" + Cr     
    Skip
    Loop
  EndIf

  =f3_SqlExec(_ASql,"SAVEPOINT PICK") 

  Select REPPICK
  Skip
EndDo


=SqlCommit (_ASql)

IF !empty(_lxerr)
   do Form St3Inc
EndIf

Return 

*>------------------------------------------------------------------
*> Actualizar reposiciones de cajas a unidades.
*> Trabaja con los datos del cursor _F16cOcuRep (ocupaciones candidatas),
*> y el cursor _F12cCajToUni(Artículo/ubicación/cantidad actual).
*>------------------------------------------------------------------
Function ActCajToUni

Private _ok
Private SOcuCan && Sumatorio cantidad repuenta.
Private CFalta  && Cantidad a actualizar cada vez.
Private _Selec
                                 
Store 0 To SOcuCan, CFalta

Select _F16cOcuRep
Go Top

Do While !Eof() .And. _F12cCajToUni.CanRep > SOcuCan

   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Calcular la cantidad a reponer.
   CFalta = _F16cOcuRep.CanDis

   If _F12cCajToUni.CanRep < SOcuCan + _F16cOcuRep.CanDis
      *> Ver si la cantidad necesaria es mayor de una caja.
      *> Intentar reponer siempre múltiplos de caja completa.
      _CNec = _F12cCajToUni.CanRep - SOcuCan
      _FCaj = _F16cOcuRep.F16cUniPac * _F16cOcuRep.F16cPacCaj
      CFalta = Ceiling(_CNec  / _FCaj) * _FCaj
      CFalta = IIf(CFalta > _CNec, _CNec, CFalta)
   EndIf              

   *> Actualizar la reposición a cajas.
   _Selec = "Update F16c" + _em + ;
            " Set F16cCanRes=F16cCanRes+" +  Str(CFalta, 7, 0) + ;
            " Where F16cCodPro='" + _F16cOcuRep.F16cCodPro + "' And " + ;
            "       F16cCodArt='" + _F16cOcuRep.F16cCodArt + "' And " + ;
            "       F16cCodUbi='" + _F16cOcuRep.F16cCodUbi + "' And " + ;
            "       F16cSitStk='" + _F16cOcuRep.F16cSitStk + "' And " + ;
            "       F16cNumLot='" + _F16cOcuRep.F16cNumLot + "' And " + ;
            "       F16cFecCad='" + _GCD(_F16cOcuRep.F16cFecCad) + "' And " + ;
            "       F16cNumPal='" + _F16cOcuRep.F16cNumPal + "'"

   _err = f3_SqlExec(_ASql, _Selec)
   If _err <= 0
      _LxErr = 'Error actualizando cantidad reservada en ocupación' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   *> Actualizar parámetros para la función de actualización.
   *> Tomará los datos del cursor de ocupaciones activo.
   Select _F16cOcuRep
   Do LlenarAcFnActz

   *> Generar Movimientos Pendientes.
   *> TipMovE y TipMovS son variables públicas definidas en procedimiento principal.
   FncA.ObjParm.POCFis = CFalta
   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMPF14c('S', TipMovS)
   If _ok
      FncA.ObjParm.PUbOld = _F12cCajToUni.F12cCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMPF14c('E', TipMovE)
      If _ok
         SOcuCan = SOcuCan + CFalta
      EndIf
   EndIf

   *>
   Select _F16cOcuRep
   Skip
EndDo

Return _ok

*>------------------------------------------------------------------
*> Procesos Reposición Cajas.
*> Comprueba si ya existe una reposición anterior de esta ubicación.
*> Recibe Propietario, Artículo, Ubicación, Tipo movimiento.
*>------------------------------------------------------------------
Function ERepAnt
Parameters CodPro, CodArt, CodUbi, TipMov

Private f_select, f_where, _ok 

f_select = " Select * From F14c" + _em
f_where  = " Where F14cUbiOri='" + CodUbi + _cm + ;
           " And   F14cCodPro='" + CodPro + _cm + ;
           " And   F14cCodArt='" + CodArt + _cm + ;
           " And   F14cTipMov='" + TipMov + "'"

Err = f3_SqlExec(_ASql, f_select + f_where)
IF Err > 0
   Go Top
   Err = Iif(Eof(), 1, -1)
EndIf
   
Return Iif(Err > 0, .T., .F.)

*>-----------------------------------------------------------
*> Cantidad actual en ocupación a reponer.
*> Recibe Propietario, Artículo, Ubicación.
*> Devuelve cantidad ya existente en la ubicación a reponer.
*>-----------------------------------------------------------
Function CRepUbic
Parameters CodPro, CodArt, CodUbi

Private f_select, _where, _from, _ok

f_select = " Select " + _GCN("Sum(F16cCanFis)") + " As CanUbic"
_from =    " From F16c" + _em 

f_where  = " Where F16cCodUbi='" + CodUbi + _cm + ;
           " And   F16cCodPro='" + CodPro + _cm + ;
           " And   F16cCodArt='" + CodArt + _cm

Err = f3_SqlExec(_ASql, f_select + _from + f_where)
If Err <= 0
   _LxErr = 'Error cargando stock artículo en ocupaciones' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   Return 0
EndIf

Return CanUbic

*>---------------------------------------------------------
*> Cargar ocupaciones candidatas para reponer.
*> Estarán ordenadas por Lote, caducidad y fecha entrada.
*> Recibe: Propietario, Artículo y tipo ubicación.
*>---------------------------------------------------------
Function GetOcuRep
Parameters CodPro, CodArt, TipUbi

Private f_select, f_from, f_where, f_orden, lx_sql, _ok

f_select = " Select F16cCodPro, F16cCodArt, F16cNumLot, F16cFecCad, F16cFecEnt," + ; 
           " F16cCodUbi, F16cSitStk, F16cNumPal, " + ;
           _GCN("F16cCanFis-F16cCanRes") + " As CanDis," + ;
           _GCN("F16cUniPac") + " As F16cUniPac, " + ;
           _GCN("F16cPacCaj") + " As F16cPacCaj, " + ;
           "F16cCanFis-F16cCanRes As Disponible, F16cUniPac*F16cPacCaj As FactorCaja 0 As Sueltas"
 
f_from   = " From F16c" + _em + ",F10c" + _em

f_where  = " Where F16cCodPro='" + CodPro + _cm + ;
           " And   F16cCodArt='" + CodArt + _cm + ;
           " And   F10cCodubi=F16cCodUbi" + ;
           " And   F10cPickSN='" + TipUbi + _cm + ; 
           " And   F16cCanFis-F16cCanRes > 0"
           
f_orden  = " Order by F16cCodPro, F16cCodArt, F16cNumLot, F16cFecCad, F16cFecEnt, Disponible"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql,lx_sql,'_F16cOcuRep')
IF Err <= 0
   _LxErr = 'Error cargando ocupaciones candidatas a reposición' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

Select OCUREP
Go Top
Do While !Eof()
   Replace Sueltas With Mod(Disponible, FactorCaja)
   Skip
EndDo

Go Top
Return !Eof()

********************************************************************************
*> Generar tablas de chequeo de documentos entre PROCAOT y SAP.
*>   DOC_VS_ALB : Comparación entre documentos de salida y albaranes.
*>
*> Recibe:
*>    FechaProcesos: Fecha de los datos a chequear.
*>    BorrarFichero: .T. - Borrar datos existente.
*>                   .F. - Añadir datos a los ya existentes.
********************************************************************************

Procedure CheckAlbProcaotSap
Parameters FechaProceso, BorrarFichero
Private _Select, _FromF, _Donde, _DropT, _Create, _Insert, _Delete
Private _f3_SqlExec
Private _CrearTabla

Store .F. To CrearTabla

_DropT  = "Drop Table DOC_VS_ALB "

_Delete = "Delete From DOC_VS_ALB "

_Create = "Create Table DOC_VS_ALB As "

_Insert = "Insert Into DOC_VS_ALB "

_Select = "Select F24cNumDoc, F24cFecDoc, F24cFecPre, F24cDirAso, " + ;
          "F24lLinDoc, F24lCodArt, F24lCanDoc, F24lCanRes, F24lCanEnv, " + ;
          "F27cNumAlb, F27cFecAlb, " + ;
          "F27lCodArt, F27lNumLot, F27lCanPed, F27lCanSer "

_FromF  = "From F24c" + _em + ", F24l" + _em + ", F27c" + _em + ", F27l" + _em + Space(1)

_Donde  = "Where F24lCodPro=F24cCodPro And " + ;
          "F24lTipDoc=F24cTipDoc And " + ;
          "F24lNumDoc=F24cNumDoc And " + ;
          "F27cCodPro(+)=F24cCodPro And " + ;
          "F27cTipDoc(+)=F24cTipDoc And " + ;
          "F27cNumDoc(+)=F24cNumDoc And " + ;
          "F27cFecAlb = " + _GCD(FechaProceso) + " And " + ;
          "F27lCodPro=F24lCodPro And " + ;
          "F27lTipDoc=F24lTipDoc And " + ;
          "F27lNumDoc=F24lNumDoc And " + ;
          "F27lLinDoc=F24lLinDoc And " + ;
          "F27lNumAlb(+)=F27cNumAlb "

Do Case
   *> Primera pasada: Inicializar tablas.
   Case BorrarFichero==.T.
      *> Cargar los nombres de las tablas del programa.
      _err = SqlTables(_ASql, 'TABLE', '_SqlTables')
      If _err <= 0
         _LxErr = 'No se pueden generar las tablas de chequeo' + cr + ;
                  'MENSAJE: ' + Message() + cr
         Do Form St3Inc With .t.
         Return .F.
      EndIf

      Select _SqlTables
      Locate For Table_Name = 'DOC_VS_ALB'
      If Found()
         *> Borrar los datos de la tabla.
         _f3_SqlExec = _Delete
         _err = f3_SqlExec(_ASql, _f3_SqlExec)
         If _err <= 0
           _LxErr = 'No se ha podido borrar la tabla DOC_VS_ALB' + cr + ;
                     'MENSAJE: ' + Message() + cr
            Do Form St3Inc With .t.
            Return .F.
         EndIf
         Store .F. To _CrearTabla
      Else
         *> Crear la tabla.
         Store .T. To _CrearTabla
      EndIf
      Use In _SqlTables

   Otherwise
      *> Añadir registros a la tabla.
      Store .F. To _CrearTabla
EndCase

*> Formar sentencia SQL, condicionada a crear tabla/añadir registros.
_f3_SqlExec = _Select + _FromF + _Donde
If _CrearTabla
   _f3_SqlExec = _Create + _f3_SqlExec
Else
   _f3_SqlExec = _Insert + _f3_SqlExec
EndIf

_err = f3_SqlExec(_ASql, _f3_SqlExec)
If _err <= 0
   _LxErr = 'No se han podido cargar los datos solicitados' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .t.
   Return .F.
EndIf

Return

********************************************************************************
*> Generar tablas de chequeo de listas entre PROCAOT y SAP.
*>   DOC_VS_LST : Comparación entre documentos de salida y listas de trabajo.
*>
*> Recibe:
*>    FechaProcesos: Fecha de los datos a chequear.
*>    BorrarFichero: .T. - Borrar datos existente.
*>                   .F. - Añadir datos a los ya existentes.
********************************************************************************

Procedure CheckLstProcaotSap
Parameters FechaProceso, BorrarFichero
Private _Select, _FromF, _Donde, _DropT, _Create, _Insert, _Delete
Private _f3_SqlExec
Private _CrearTabla

Store .F. To CrearTabla

_DropT  = "Drop Table DOC_VS_LST "

_Delete = "Delete From DOC_VS_LST "

_Create = "Create Table DOC_VS_LST As "

_Insert = "Insert Into DOC_VS_LST "

_Select = "Select F24cNumDoc, F24cFecDoc, F24cFecPre, F24cDirAso, " + ;
          "F24lLinDoc, F24lCodArt, F24lCanDoc, F24lCanRes, F24lCanEnv, " + ;
          "F26cNumLst, F26cFecCre, " + ;
          "F26lCodArt, F26lNumLot, F26lCanFis "

_FromF  = "From F24c" + _em + ", F24l" + _em + ", F26c" + _em + ", F26l" + _em + Space(1)

_Donde  = "Where F24lCodPro=F24cCodPro And " + ;
          "F24lTipDoc=F24cTipDoc And " + ;
          "F24lNumDoc=F24cNumDoc And " + ;
          "F26cFecCre = " + _GCD(FechaProceso) + " And " + ;
          "F26lCodPro=F24lCodPro And " + ;
          "F26lTipDoc=F24lTipDoc And " + ;
          "F26lNumDoc=F24lNumDoc And " + ;
          "F26lLinDoc=F24lLinDoc And " + ;
          "F26lNumLst(+)=F26cNumLst "

Do Case
   *> Primera pasada: Inicializar tablas.
   Case BorrarFichero==.T.
      *> Cargar los nombres de las tablas del programa.
      _err = SqlTables(_ASql, 'TABLE', '_SqlTables')
      If _err <= 0
         _LxErr = 'No se pueden generar las tablas de chequeo' + cr + ;
                  'MENSAJE: ' + Message() + cr
         Do Form St3Inc With .t.
         Return .F.
      EndIf

      Select _SqlTables
      Locate For Table_Name = 'DOC_VS_LST'
      If Found()
         *> Borrar los datos de la tabla.
         _f3_SqlExec = _Delete
         _err = f3_SqlExec(_ASql, _f3_SqlExec)
         If _err <= 0
           _LxErr = 'No se ha podido borrar la tabla DOC_VS_LST' + cr + ;
                     'MENSAJE: ' + Message() + cr
            Do Form St3Inc With .t.
            Return .F.
         EndIf
         Store .F. To _CrearTabla
      Else
         *> Crear la tabla.
         Store .T. To _CrearTabla
      EndIf
      Use In _SqlTables

   Otherwise
      *> Añadir registros a la tabla.
      Store .F. To _CrearTabla
EndCase

*> Formar sentencia SQL, condicionada a crear tabla/añadir registros.
_f3_SqlExec = _Select + _FromF + _Donde
If _CrearTabla
   _f3_SqlExec = _Create + _f3_SqlExec
Else
   _f3_SqlExec = _Insert + _f3_SqlExec
EndIf

_err = f3_SqlExec(_ASql, _f3_SqlExec)
If _err <= 0
   _LxErr = 'No se han podido cargar los datos solicitados' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .t.
   Return .F.
EndIf

Return

********************************************************************************
*> 
*>   GenEst : Generación de estadísticas
*>
*>     Estadística mensual por artículo
*>     Estadística diaria por artículo
*>     Estadística mensual por operario
*>
*>   Recibe:
*>      Propietario.
*>      Año
*>      Mes
*>
********************************************************************************
*>
*>   Para los calculos de ocupaciones no se debe contemplar las ubicacones sin cantidad
*>   ni la ubicaciones de expedición:
*>
*>   	Eliminar ocupaciones sin cantidad 
*>   	Eliminar ubicaciones de tipo 'E' excepto '0001DED 000000' y '0001DET 000000'
*>
********************************************************************************
Procedure GenEst
Parameters CPro, Anyo, Mes

Private f_select, f_where, f_from, f_group, _ok
Private FecAct, anyofin, mesfin
Private MvtTot, MvtEnt, MvtDev, MvtSal, MvtInt, MvtPal, MvtCaj, MvtGrp, MvtSgr, MvtUni, OriRes, GrpSgr
Private N_Reg, N_Total
Private UbiOri

Store 0 To MvtTot, MvtEnt, MvtDev, MvtSal, MvtInt, MvtPal, MvtCaj, MvtGrp, MvtSgr, MvtUni

*> Monto la fecha inicial y la final
FecIni = '01' + '/' + Right('00' + Alltrim(Str(Mes)),2) + '/' + Right('0000' + Alltrim(Str(Anyo)),4)

If Mes=12
   MesFin = 1
   AnyoFin = Anyo + 1
Else
   MesFin = Mes + 1
   AnyoFin = Anyo
EndIf

FecFin = '01' + '/' + Right('00' + Alltrim(Str(MesFin)),2) + '/' + Right('0000' + Alltrim(Str(AnyoFin)),4)

FecAct = {  /  /  }
UltAct = CToD('01' + '/' + Right('00' + Alltrim(Str(MesFin)),2)  + '/' + Right('0000' + Alltrim(Str(AnyoFin)),4))-1

TotDias = Day(UltAct)

*> Total de ocupaciones
TotOcu = 0

*> Cargo Cursor con los Movimientos del mes
f_select= "*"
f_from  = "F20c"
f_where = "F20cFecMov>=" + _GCD(FecIni)
f_order = "F20cFecMov, F20cNumMov" 

If !f3_sql(f_select, f_from, f_where, f_order, '', 'F20cCur')
   _LxErr = 'No se han seleccionado registros del fichero de movimientos'
   Do Form St3Inc With .T.
   Return 0
EndIf

*> Cargo Existencias
f_select= "F16cCodUbi,F16cCodPro,F16cCodArt,F16cSitStk,sum(F16cCanFis) as F16cCanFis"
f_from  = "F16c"
f_where = "F16cCodPro='" + CPro + _cm
f_group = "F16cCodUbi,F16cCodPro,F16cCodArt,F16cSitStk"

If !f3_sql(f_select, f_from, f_where, '', f_group, 'F16cCur')
   _LxErr = 'No se han seleccionado registros del fichero de ocupaciones'
   Do Form St3Inc With .T.
EndIf

IndAux = FnTemp(1)
Select F16cCur
INDEX ON F16cCodPro+F16cCodArt+F16cCodUbi+F16cSitStk TO (IndAux)

*> Creo Cursor Documentos de entrada
IndAux = FnTemp(1)
Create cursor CurDE (DocEnt C(13), CodPro C(6), CodArt C(13), CanFis N(6,0))
INDEX ON DocEnt+CodPro+CodArt TO (IndAux)

*> Creo Cursor Documentos de salida
IndAux = FnTemp(1)
Create cursor CurDS (DocSal C(13), CodPro C(6), CodArt C(13), CanFis N(6,0))
INDEX ON DocSal+CodPro+CodArt TO (IndAux)

*> Creo Cursor artículos movidos en el mes.
IndAux = FnTemp(1)
Create cursor F08cCur (F08cCodPro C(6), F08cCodArt C(13), MvtArt N(4,0)) 
INDEX ON F08cCodPro+F08cCodArt TO (IndAux)
ArtMes = 0

*> Creo Cursor con acumulado de doctos de entrada y salida, y ocupaciones.
IndAux = FnTemp(1)
Create cursor CurDoc (CodPro C(6),     CodArt C(13),     ;
                      DocEnt N(3,0),   DocSal N(3,0),    ;
                      OcuMes N(6,0))
INDEX ON CodPro+CodArt TO (IndAux)

*> Creo Cursor F61cDia
IndAux = FnTemp(1)
Create cursor F61cDia (F61cCodPro C(6),   ;
                       F61cCodArt C(13),  ;
                       F61cFecMov C(8),   ;
                       F61cTipPro C(4),   ;
                       F61cPalEnt N(9,0), ;
                       F61cCajEnt N(9,0), ;
                       F61cGrpEnt N(9,0), ;
                       F61cSgrEnt N(9,0), ;
                       F61cUniEnt N(9,0), ;
                       F61cPalDev N(9,0), ;
                       F61cCajDev N(9,0), ;
                       F61cGrpDev N(9,0), ;
                       F61cSgrDev N(9,0), ;
                       F61cUniDev N(9,0), ;
                       F61cPalSal N(9,0), ;
                       F61cCajSal N(9,0), ;
                       F61cGrpSal N(9,0), ;
                       F61cSgrSal N(9,0), ;
                       F61cUniSal N(9,0), ;
                       F61cPalInt N(9,0), ;
                       F61cCajInt N(9,0), ;
                       F61cGrpInt N(9,0), ;
                       F61cSgrInt N(9,0), ;
                       F61cUniInt N(9,0), ;
                       F61cNumOcu N(6,0), ;
                       F61cFlag1  C(1),   ;
                       F61cFlag2  C(1),   ;
                       F61cFlag3  C(1))
INDEX ON F61cCodPro+F61cCodArt+F61cFecMov TO (IndAux)

*> Creo Cursor F60cCur  Estadísticas Mensual/Artículo
IndAux = FnTemp(1)
=CrtCursor('F60c', 'F60cCur' ,'C') 
INDEX ON F60cCodPro+F60cCodArt TO (IndAux)

*> Creo Cursor F61cCur  Estadísticas Diario/Artículo
=CrtCursor('F61c', 'F61cCur' ,'C') 

*> Creo Cursor F61cCur  Estadísticas Operario/Mes
IndAux = FnTemp(1)
=CrtCursor('F62c', 'F62cCur' ,'C') 
INDEX ON F62cCodPro+F62cCodOpe+F62cFecMov+F62cTipPro TO (IndAux)

*> Creo Cursor con acumulado de listas de trabajo por operario.
IndAux = FnTemp(1)
Create cursor CurLst (CLstCodPro C(6),     CLstCodOpe C(4),  ;
                      CLstFecMov C(6),     CLstNumLst C(6),  ;
                      CLstCanFis N(6,0))
INDEX ON CLstCodPro+CLstCodOpe+CLstFecMov+CLstNumLst TO (IndAux)

*> Creo Cursor con acumulado de listas de trabajo por operario.
Create cursor F26cCur (DifFec  N(9,0))

Select F20cCur

N_Reg=0
Count to N_Total

*> Recorro movimientos y actualizo stock y estadísticas
Go Bottom

Do While !Bof()

   N_Reg = N_Reg + 1
   =WaitWindow("Procesando registro número " + Alltrim(Str(N_Reg)) + " de un total de " + Alltrim(Str(N_Total)))

   AnyHM = Right('0000'+ AllTrim(Str(Year(F20cCur.F20cFecMov))), 4)
   MesHM = Right('00'  + AllTrim(Str(Month(F20cCur.F20cFecMov))), 2)
   DiaHm = Right('00'  + AllTrim(Str(Day(F20cCur.F20cFecMov))), 2)   

   *> Verifico si se ha alcanzado el mes a calcular.
   If Year(F20cCur.F20cFecMov)=Anyo And Month(F20cCur.F20cFecMov)=Mes

      *> Determino que tipo de movimiento tratamos según criterio de agrupación
      OriRes = ' '
      If !Empty(F20cOriRes)
         OriRes = F20cCur.F20cOriRes
      Else
	     f_select = "*"
         f_from =   "F10c"
	     f_where  = "F10cCodUbi='" + F20cCur.F20cUbiOri + "'"

	     If f3_sql(f_select, f_from, f_where, '', '', 'F10cCur')
            Select F10cCur
            Go Top
            If !Eof()
               OriRes = F10cCur.F10cPickSN
               *> Si la ubicación es de expedición no se acumulará en ninguna columna
            EndIf
	     EndIf
      EndIf

      *> Accedo a F64c para determinar si la situación de stock se descarta.
      DesStk = 'N'   && Descartar Sit. Stock
      _Selec = "Select * From F64c" + _em + Space(1) + ;
      "Where F64cCodAlm='" + F20cCur.F20cCodAlm  + "' And" + ;
      "      F64cSitStk='" + F20cCur.F20cSitStk  + "'"
      _xier = f3_SqlExec(_ASql, _Selec, 'F64cCur')
      If _xier > 0
	      Select F64cCur
	      Go Top
	      If !Eof() and  f64cCur.F64cActivo='S'
	         DesStk = 'S'   && Descartar Sit. Stock
	      EndIf
      EndIf

      *> Accedo a F08g para determinar si se trata de una ub. de grupos o de subgrupos. 
      *> F08gLvlAgr <=  20 -> Grupo          GrpSgr = 'G'
      *> F08gLvlAgr >   20 -> Sugrupo        GrpSgr = 'S'

      If OriRes = 'G' 
         GrpSgr = ''
         UbiOri = ''

      	 If F20cCur.F20cTipMov='2999'
	         *> Accedo al HM para recupera la ubicación origen.
	         _Selec = "Select * From F20c" + _em + Space(1) + ;
	         "Where F20cNMovMp='" + F20cCur.F20cNMovMP  + "' And F20cTipMov='3500'"
	         _xier = f3_SqlExec(_ASql, _Selec, 'F20cTmp')
            If _xier > 0
	           Select F20cTmp
	           Go Top
  	           If !Eof()
	              UbiOri = F20cTmp.F20cUbiOri
      	       EndIf
      	    EndIf
      	 Else
            UbiOri = F20cCur.F20cUbiOri
      	 EndIf

         _Selec = "Select * From F08g" + _em + Space(1) + ;
         "Where F08gCodPro='" + F20cCur.F20cCodPro  + "' And" + ;
         "      F08gCodArt='" + F20cCur.F20cCodArt  + "' And" + ;
         "      F08gCodUbi='" + UbiOri + "'"
         _xier = f3_SqlExec(_ASql, _Selec, 'F08gCur')
         If _xier > 0
	         Select F08gCur
	         Go Top
	         If !Eof()
	            If F08gCur.F08gLvlAgr <= 20 
	               GrpSgr = 'G'
	            Else
	               GrpSgr = 'S'
	            EndIf
	         EndIf
         EndIf
         
         If Empty(GrpSgr)
             *> Busco coincidencias en cantidades.
	         _Selec = "Select * From F08g" + _em + Space(1) + ;
	         "Where F08gCodPro='" + F20cCur.F20cCodPro  + "' And" + ;
	         "      F08gCodArt='" + F20cCur.F20cCodArt  + "'"
	         _xier = f3_SqlExec(_ASql, _Selec, 'F08gCur')
	         If _xier > 0
		         Select F08gCur
		         Go Top
		         Do While !Eof() .and. Empty(GrpSgr)
		         
		            If (F20cCur.F20cCanFis = F08gCur.F08gUniAgr)
			            If F08gCur.F08gLvlAgr <= 20 
			               GrpSgr = 'G'
			            Else
			               GrpSgr = 'S'
			            EndIf
			        EndIf

		            Select F08gCur
		            Skip
		         EndDo
	         EndIf
         EndIf
      EndIf

      *> Recupero volumen y peso unitario
      f_select = "F08cPesUni, F08cVolUni, F08cTipPro"
      f_from =   "F08c"
      f_where  = "F08cCodPro='" + F20cCur.F20cCodPro + _cm + ;
                 " and F08cCodArt='" + F20cCur.F20cCodArt + _cm
      f_group  = ""
      
      If f3_sql(f_select, f_from, f_where, '', f_group, 'F08cTmp')
         Select F08cTmp
         Go Top
         If !Eof()
            VolUni = F08cTmp.F08cVolUni
            PesUni = F08cTmp.F08cPesUni
            TipPro = F08cTmp.F08cTipPro
         Else
            VolUni = 1
            PesUni = 1
            TipPro = ''
         EndIf
      Else
         VolUni = 1
         PesUni = 1
         TipPro = ''
      EndIf
      
      *> Si no se puede recuperar el tipo de producto, se salta el registro.
	      *> Cuando alcanzo el mes de proceso actualizo cursor de estadísticas mensuales. -------------------------------------
	      Select F60cCur
		      Seek F20cCur.F20cCodPro+F20cCur.F20cCodArt
	      If !Found()
	         AnyMov = Right('0000' + Alltrim(Str(Year(F20cCur.F20cFecMov))),4)
	         MesMov = Right(  '00' + Alltrim(Str(Mont(F20cCur.F20cFecMov))),2)
	         Append Blank
	         Replace F60cCur.F60cCodPro With F20cCur.F20cCodPro
	         Replace F60cCur.F60cCodArt With F20cCur.F20cCodArt
	         Replace F60cCur.F60cFecMov With AnyMov+MesMov
	         Replace F60cCur.F60cTipPro With TipPro
	         Replace F60cCur.F60cKgsEnt With 0
	         Replace F60cCur.F60cVolEnt With 0
	         Replace F60cCur.F60cMvtEnt With 0
	         Replace F60cCur.F60cRotEnt With 0
	         Replace F60cCur.F60cKgsDev With 0
	         Replace F60cCur.F60cVolDev With 0
	         Replace F60cCur.F60cMvtDev With 0
	         Replace F60cCur.F60cRotDev With 0
	         Replace F60cCur.F60cKgsSal With 0
	         Replace F60cCur.F60cVolSal With 0
	         Replace F60cCur.F60cMvtSal With 0
	         Replace F60cCur.F60cRotSal With 0
	         Replace F60cCur.F60cKgsInt With 0
	         Replace F60cCur.F60cVolInt With 0
	         Replace F60cCur.F60cMvtInt With 0
	         Replace F60cCur.F60cRotInt With 0
	         Replace F60cCur.F60cKgsPal With 0
	         Replace F60cCur.F60cVolPal With 0
	         Replace F60cCur.F60cMvtPal With 0
	         Replace F60cCur.F60cRotPal With 0
	         Replace F60cCur.F60cKgsCaj With 0
	         Replace F60cCur.F60cVolCaj With 0
	         Replace F60cCur.F60cMvtCaj With 0
	         Replace F60cCur.F60cRotCaj With 0
	         Replace F60cCur.F60cKgsGrp With 0
	         Replace F60cCur.F60cVolGrp With 0
	         Replace F60cCur.F60cMvtGrp With 0
	         Replace F60cCur.F60cRotGrp With 0
	         Replace F60cCur.F60cKgsSgr With 0
	         Replace F60cCur.F60cVolSgr With 0
	         Replace F60cCur.F60cMvtSgr With 0
	         Replace F60cCur.F60cRotSgr With 0
	         Replace F60cCur.F60cKgsUni With 0
	         Replace F60cCur.F60cVolUni With 0
	         Replace F60cCur.F60cMvtUni With 0
	         Replace F60cCur.F60cRotUni With 0
	         Replace F60cCur.F60cCoeRot With 0
	         Replace F60cCur.F60cDocSal With 0
	         Replace F60cCur.F60cDocEnt With 0
	         Replace F60cCur.F60cCanRes With 0
	         Replace F60cCur.F60cMedOcu With 0
	         Replace F60cCur.F60cPerOcu With 0
	         Replace F60cCur.F60cFlag1  With ''
	         Replace F60cCur.F60cFlag2  With ''
	         Replace F60cCur.F60cFlag3  With ''
	      EndIf
	      
	      If !Empty(F20cCur.F20cCodOpe)
	         Select F62cCur
	         Seek F20cCur.F20cCodPro + F20cCur.F20cCodOpe + (AnyHM + MesHM) + TipPro
	         If !Found()
	             Append Blank
	             Replace F62cCodPro With F20cCur.F20cCodPro
	             Replace F62cCodOpe With F20cCur.F20cCodOpe
	             Replace F62cFecMov With (AnyHM + MesHM)
	             Replace F62cTipPro With TipPro
	             Replace F62cLstSal With 0
	             Replace F62cMvtSal With 0
	             Replace F62cKgsSal With 0
	             Replace F62cVolSal With 0
	             Replace F62cTotRep With 0
	             Replace F62cTimSal With 0
	             Replace F62cMvtEnt With 0
		         
		         Replace F62cPalEnt With 0
		         Replace F62cCajEnt With 0
		         Replace F62cGrpEnt With 0
		         Replace F62cUniEnt With 0

		         Replace F62cPalSal With 0
		         Replace F62cCajSal With 0
		         Replace F62cGrpSal With 0
		         Replace F62cUniSal With 0
		         
		         Replace F62cPalInt With 0
		         Replace F62cCajInt With 0
		         Replace F62cGrpInt With 0
		         Replace F62cUniInt With 0
		         
		         Replace F62cFlag1  With ''
		         Replace F62cFlag2  With ''
		         Replace F62cFlag3  With ''
		     EndIf
	      EndIf

	      If F20cCur.F20cTipMov='1000' .or. F20cCur.F20cTipMov='1100' .or. F20cCur.F20cTipMov='1200' .or. ;
	         F20cCur.F20cTipMov='2000' .or. F20cCur.F20cTipMov='2100' .or. F20cCur.F20cTipMov='2999' .or. ;
	         F20cCur.F20cTipMov='3060' .or. F20cCur.F20cTipMov='3070' .or. F20cCur.F20cTipMov='3080' .or. ;
	         F20cCur.F20cTipMov='3100'
		      Do Case
		          Case F20cCur.F20cTipMov = '1000' .or. ;
		               F20cCur.F20cTipMov = '1100' .or. ;
		               F20cCur.F20cTipMov = '1200'
		               
		               If !Empty(F20cCur.F20cCodOpe)
			               Do Case
		  	                  Case F20cCur.F20cTipMov = '1100'   && Devoluciones
				                 
			 	                 *> Contemplo todos los recibidos no solo los recuperados
					             Replace F60cCur.F60cMvtDev With F60cCur.F60cMvtDev + 1
					             Replace F60cCur.F60cKgsDev With F60cCur.F60cKgsDev + (F20cCur.F20cCanFis * PesUni)/1000 
					             Replace F60cCur.F60cVolDev With F60cCur.F60cVolDev + (F20cCur.F20cCanFis * VolUni)/1000 
					             
			 	                 *> Contemplo solo los recuperados
			 	                 *If F20cCur.F20cSitStk='1000'
						         *    Replace F60cCur.F60cMvtDev With F60cCur.F60cMvtDev + 1
						         *    Replace F60cCur.F60cKgsDev With F60cCur.F60cKgsDev + (F20cCur.F20cCanFis * PesUni)/1000 
						         *    Replace F60cCur.F60cVolDev With F60cCur.F60cVolDev + (F20cCur.F20cCanFis * VolUni)/1000 
			 	                 *EndIf
			 	                 
		  	                  Case F20cCur.F20cTipMov = '1000'   && Compras
					             Replace F60cCur.F60cMvtEnt With F60cCur.F60cMvtEnt + 1
					             Replace F60cCur.F60cKgsEnt With F60cCur.F60cKgsEnt + (F20cCur.F20cCanFis * PesUni)/1000 
					             Replace F60cCur.F60cVolEnt With F60cCur.F60cVolEnt + (F20cCur.F20cCanFis * VolUni)/1000 
					             
		  	                  Other                              && Ajustes positivos
		  	                  
		  	               EndCase
	                   EndIf
		             
		               If !Empty(F20cCur.F20cCodOpe)
		                 Do Case
	  	                    Case F20cCur.F20cTipMov = '1100'   && Devoluciones
				         	    *> Descartar DED y DET
						        
				                *> Contemplo todos los recibidos no solo los recuperados
		  		  	            Replace F62cCur.F62cMvtDev With F62cCur.F62cMvtDev + 1
		  		  	               
				                *> Contemplo solo los recuperados
		 	                    *If F20cCur.F20cSitStk='1000'
		  		  	            *   Replace F62cCur.F62cMvtDev With F62cCur.F62cMvtDev + 1
						        *EndIf
	  	                    Case F20cCur.F20cTipMov = '1000'   && Compras
				  	            Replace F62cCur.F62cMvtEnt With F62cCur.F62cMvtEnt + 1
	  	                    Other                              && Ajustes positivos
	  	                 EndCase
		   	           EndIf

		          Case F20cCur.F20cTipMov = '2000' .or. ;
		               F20cCur.F20cTipMov = '2100' .or. ;
		               F20cCur.F20cTipMov = '2999'

		             If !Empty(F20cCur.F20cCodOpe)
			             *> Solo movimientos de preparación
			             If F20cCur.F20cTipMov = '2999'
				             Replace F60cCur.F60cMvtSal With F60cCur.F60cMvtSal + 1
				             Replace F60cCur.F60cKgsSal With F60cCur.F60cKgsSal + (F20cCur.F20cCanFis * PesUni)/1000
				             Replace F60cCur.F60cVolSal With F60cCur.F60cVolSal + (F20cCur.F20cCanFis * VolUni)/1000 
			             EndIf
		             EndIf

		             *> Solo movimientos de preparación
		             If !Empty(F20cCur.F20cCodOpe) .and. F20cCur.F20cTipMov = '2999'
		  	            Replace F62cCur.F62cMvtSal With F62cCur.F62cMvtSal + 1
		  	            Replace F62cCur.F62cKgsSal With F62cCur.F62cKgsSal + (F20cCur.F20cCanFis * PesUni)/1000
		   	            Replace F62cCur.F62cVolSal With F62cCur.F62cVolSal + (F20cCur.F20cCanFis * VolUni)/1000
		   	            
		   	            *>  Contabiliza los MP preparados por el carro
		   	            *>  Replace F62cCur.F62cTimSal With F62cCur.F62cTimSal + F20cCur.F20cOrdRut
		   	         EndIf

		          Case F20cCur.F20cTipMov = '3060' .or. ;
		               F20cCur.F20cTipMov = '3070' .or. ;
	                   F20cCur.F20cTipMov = '3080' .or. ;
	                   F20cCur.F20cTipMov = '3100'

		             If !Empty(F20cCur.F20cCodOpe)
			             Replace F60cCur.F60cMvtInt With F60cCur.F60cMvtInt + 1
			             Replace F60cCur.F60cKgsInt With F60cCur.F60cKgsInt + (F20cCur.F20cCanFis * PesUni)/1000
			             Replace F60cCur.F60cVolInt With F60cCur.F60cVolInt + (F20cCur.F20cCanFis * VolUni)/1000 
		             EndIf
		   	         
		      EndCase
		      
		      *> Acumulo como palet, cajas, grupo o unidades
	          If !Empty(F20cCur.F20cCodOpe)
			      Do Case
			          Case OriRes='N'   && Palet
		                 *> Solo acumula VENTAS
		                 If F20cCur.F20cTipMov='2999'
				             Replace F60cCur.F60cMvtPal With F60cCur.F60cMvtPal + 1
				             Replace F60cCur.F60cKgsPal With F60cCur.F60cKgsPal + (F20cCur.F20cCanFis * PesUni)/1000
				             Replace F60cCur.F60cVolPal With F60cCur.F60cVolPal + (F20cCur.F20cCanFis * VolUni)/1000
		                 EndIf
			             
			             If !Empty(F20cCur.F20cCodOpe)
			  	            
		 	                Do Case
		*!*		                   Case F20cCur.F20cTipMov = '1000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1200'
			                   Case F20cCur.F20cTipMov = '1000' .or. ;
			                        F20cCur.F20cTipMov = '1100'
			  	                    Replace F62cCur.F62cPalEnt With F62cCur.F62cPalEnt + F20cCur.F20cCanFis
		*!*		                   Case F20cCur.F20cTipMov = '2000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2999'
			                   Case F20cCur.F20cTipMov = '2999'
			  	                    Replace F62cCur.F62cPalSal With F62cCur.F62cPalSal + F20cCur.F20cCanFis
			                   Case F20cCur.F20cTipMov = '3060' .or. ;
			                        F20cCur.F20cTipMov = '3070' .or. ;
			                        F20cCur.F20cTipMov = '3080' .or. ;
			                        F20cCur.F20cTipMov = '3100'
			  	                    Replace F62cCur.F62cPalInt With F62cCur.F62cPalInt + F20cCur.F20cCanFis
			                EndCase
			   	         EndIf
			   	         
			          Case OriRes='S'   && Cajas
		                 *> Solo acumula VENTAS
		                 *> If F20cCur.F20cTipMov='2000' .or. F20cCur.F20cTipMov='2100' .or. F20cCur.F20cTipMov='2999'
		                 If F20cCur.F20cTipMov='2999'
				             Replace F60cCur.F60cMvtCaj With F60cCur.F60cMvtCaj + 1
				             Replace F60cCur.F60cKgsCaj With F60cCur.F60cKgsCaj + (F20cCur.F20cCanFis * PesUni)/1000
				             Replace F60cCur.F60cVolCaj With F60cCur.F60cVolCaj + (F20cCur.F20cCanFis * VolUni)/1000
			             EndIf

			             If !Empty(F20cCur.F20cCodOpe)
		 	                Do Case
		*!*		                   Case F20cCur.F20cTipMov = '1000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1200'
			                   Case F20cCur.F20cTipMov = '1000' .or. ;
			                        F20cCur.F20cTipMov = '1100'
			  	                    Replace F62cCur.F62cCajEnt With F62cCur.F62cCajEnt + F20cCur.F20cCanFis
		*!*		                   Case F20cCur.F20cTipMov = '2000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2999'
			                   Case F20cCur.F20cTipMov = '2999'
			  	                    Replace F62cCur.F62cCajSal With F62cCur.F62cCajSal + F20cCur.F20cCanFis
			                   Case F20cCur.F20cTipMov = '3060' .or. ;
			                        F20cCur.F20cTipMov = '3070' .or. ;
			                        F20cCur.F20cTipMov = '3080' .or. ;
			                        F20cCur.F20cTipMov = '3100'
			  	                    Replace F62cCur.F62cCajInt With F62cCur.F62cCajInt + F20cCur.F20cCanFis
			                EndCase
			   	         EndIf
			             
			          Case OriRes='G'   && Grupos
			             *> Separar GRUPOS y SUBGRUPOS
		                 *> Solo acumula VENTAS
		                 *> If F20cCur.F20cTipMov='2000' .or. F20cCur.F20cTipMov='2100' .or. F20cCur.F20cTipMov='2999'
		                 If F20cCur.F20cTipMov='2999'
			 	             Do Case
				                Case GrpSgr = 'G'
				                     Replace F60cCur.F60cMvtGrp With F60cCur.F60cMvtGrp + 1
				                     Replace F60cCur.F60cKgsGrp With F60cCur.F60cKgsGrp + (F20cCur.F20cCanFis * PesUni)/1000
				                     Replace F60cCur.F60cVolGrp With F60cCur.F60cVolGrp + (F20cCur.F20cCanFis * VolUni)/1000
				                Case GrpSgr = 'S'
				                     Replace F60cCur.F60cMvtSgr With F60cCur.F60cMvtSgr + 1
				                     Replace F60cCur.F60cKgsSgr With F60cCur.F60cKgsSgr + (F20cCur.F20cCanFis * PesUni)/1000
				                     Replace F60cCur.F60cVolSgr With F60cCur.F60cVolSgr + (F20cCur.F20cCanFis * VolUni)/1000
			 	             EndCase
		                 EndIf
			             

			             If !Empty(F20cCur.F20cCodOpe)
		 	                Do Case
		*!*		                   Case F20cCur.F20cTipMov = '1000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1200'
			                   Case F20cCur.F20cTipMov = '1000' .or. ;
			                        F20cCur.F20cTipMov = '1100'
			  	                    Replace F62cCur.F62cGrpEnt With F62cCur.F62cGrpEnt + F20cCur.F20cCanFis
		*!*		                   Case F20cCur.F20cTipMov = '2000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2999'
			                   Case F20cCur.F20cTipMov = '2999'
			  	                    Replace F62cCur.F62cGrpSal With F62cCur.F62cGrpSal + F20cCur.F20cCanFis
			                   Case F20cCur.F20cTipMov = '3060' .or. ;
			                        F20cCur.F20cTipMov = '3070' .or. ;
			                        F20cCur.F20cTipMov = '3080' .or. ;
			                        F20cCur.F20cTipMov = '3100'
			  	                    Replace F62cCur.F62cGrpInt With F62cCur.F62cGrpInt + F20cCur.F20cCanFis
			                EndCase
			   	         EndIf
			             
			          Case OriRes='U'   && Unidades
		                 *> Solo acumula VENTAS
		                 *> If F20cCur.F20cTipMov='2000' .or. F20cCur.F20cTipMov='2100' .or. F20cCur.F20cTipMov='2999'
		                 If F20cCur.F20cTipMov='2999'
				             Replace F60cCur.F60cMvtUni With F60cCur.F60cMvtUni + 1
				             Replace F60cCur.F60cKgsUni With F60cCur.F60cKgsUni + (F20cCur.F20cCanFis * PesUni)/1000
				             Replace F60cCur.F60cVolUni With F60cCur.F60cVolUni + (F20cCur.F20cCanFis * VolUni)/1000
			             EndIf
			             
			             If !Empty(F20cCur.F20cCodOpe)
		 	                Do Case
		*!*		                   Case F20cCur.F20cTipMov = '1000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '1200'
			                   Case F20cCur.F20cTipMov = '1000' .or. ;
			                        F20cCur.F20cTipMov = '1100'
			  	                    Replace F62cCur.F62cUniEnt With F62cCur.F62cUniEnt + F20cCur.F20cCanFis
		*!*		                   Case F20cCur.F20cTipMov = '2000' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2100' .or. ;
		*!*		                        F20cCur.F20cTipMov = '2999'
			                   Case F20cCur.F20cTipMov = '2999'
			  	                    Replace F62cCur.F62cUniSal With F62cCur.F62cUniSal + F20cCur.F20cCanFis
			                   Case F20cCur.F20cTipMov = '3060' .or. ;
			                        F20cCur.F20cTipMov = '3070' .or. ;
			                        F20cCur.F20cTipMov = '3080' .or. ;
			                        F20cCur.F20cTipMov = '3100'
			  	                    Replace F62cCur.F62cUniInt With F62cCur.F62cUniInt + F20cCur.F20cCanFis
			                EndCase
			   	         EndIf
			             
			      EndCase
	          EndIf
			      
		      *> Acumulo movimientos (cálculo de coeficientes de rotación)
		      MvtTot = MvtTot + 1   && Total de movimientos del mes
		      Do Case
	             Case F20cCur.F20cTipMov = '1000' .or. ;
	                  F20cCur.F20cTipMov = '1100' .or. ;
	                  F20cCur.F20cTipMov = '1200'
			          If F20cCur.F20cTipMov = '1100'   && Devoluciones
	 	                 
	                     *> Contemplo todos los recibidos no solo los recuperados
	  	                 MvtDev = MvtDev + 1          && Devoluciones del mes de Entrada
	  	                 
	                     *> Contemplo solo los recuperados
	 	                 *If F20cCur.F20cSitStk='1000'
			             *   MvtDev = MvtDev + 1          && Devoluciones del mes de Entrada
	 	                 *EndIf
	 	              Else
			             MvtEnt = MvtEnt + 1          && Movimientos del mes de Entrada
			          EndIf
			          
	             Case F20cCur.F20cTipMov = '2000' .or. ;
	                  F20cCur.F20cTipMov = '2100' .or. ;
	                  F20cCur.F20cTipMov = '2999'
		              MvtSal = MvtSal + 1         && Movimientos del mes de Salida
		              
	             Case F20cCur.F20cTipMov = '3060' .or. ;
	                  F20cCur.F20cTipMov = '3070' .or. ;
	                  F20cCur.F20cTipMov = '3080' .or. ;
	                  F20cCur.F20cTipMov = '3100'
		              MvtInt = MvtInt + 1         && Movimientos del mes de Salida
		      EndCase

	          *> Solo acumula VENTAS
	          *If F20cCur.F20cTipMov='2000' .or. F20cCur.F20cTipMov='2100' .or. F20cCur.F20cTipMov='2999'
	          If F20cCur.F20cTipMov='2999'
			      Do Case
			         Case OriRes='N'   && Palet
			              MvtPal = MvtPal + 1
			         Case OriRes='S'   && Cajas
			              MvtCaj = MvtCaj + 1
			         Case OriRes='G'   && Grupos
			              *> Separar GRUPOS y SUBGRUPOS
		 	              Do Case
			                 Case GrpSgr = 'G'
			                      MvtGrp = MvtGrp + 1
			                 Case GrpSgr = 'S'
			                      MvtSgr = MvtSgr + 1
		 	              EndCase
			         Case OriRes='U'   && Unidades
			              MvtUni = MvtUni + 1
			      EndCase
	          EndIf

		      *> Actualizo número productos movidos
		      Select F08cCur
		*      Locate for F08cCur.F08cCodPro=F20cCur.F20cCodPro and F08cCur.F08cCodArt=F20cCur.F20cCodArt
		      Seek F20cCur.F20cCodPro+F20cCur.F20cCodArt
		      If !Found()
		         Append Blank
		         Replace F08cCur.F08cCodPro With F20cCur.F20cCodPro
		         Replace F08cCur.F08cCodArt With F20cCur.F20cCodArt
		         Replace F08cCur.MvtArt     With 0
		      EndIf
		      ArtMes = ArtMes + 1
		      Replace F08cCur.MvtArt With F08cCur.MvtArt + 1

		      *> Actualizo Documentos de salida o documento de entrada
		      Do Case
	*	         Case F20cCur.F20cTipMov In ('1000','1100','1200')
	*!*	             Case F20cCur.F20cTipMov = '1000' .or. ;
	*!*	                  F20cCur.F20cTipMov = '1100' .or. ;
	*!*	                  F20cCur.F20cTipMov = '1200'
	             Case F20cCur.F20cTipMov = '1000'
		              Select CurDE
		              Seek F20cCur.F20cNumDoc+F20cCur.F20cCodPro+F20cCur.F20cCodArt
		              If !Found()
		                 *> Alta en CurDoc
		                 Select CurDoc
		*                 Locate For CurDoc.CodPro=F20cCur.F20cCodPro and ; 
		*                            CurDoc.CodArt=F20cCur.F20cCodArt
		                 Seek F20cCur.F20cCodPro+F20cCur.F20cCodArt
		                 If !Found()
			                 Append Blank
			                 Replace CurDoc.CodPro With F20cCur.F20cCodPro
			                 Replace CurDoc.CodArt With F20cCur.F20cCodArt
			                 Replace CurDoc.DocEnt With 0
			                 Replace CurDoc.DocSal With 0
			                 Replace CurDoc.OcuMes With 0
		                 EndIf
			             Replace CurDoc.DocEnt With CurDoc.DocEnt + 1
			             
		                 Select CurDE
		                 Append Blank
		                 Replace CurDE.DocEnt With F20cCur.F20cNumDoc
		                 Replace CurDE.CodPro With F20cCur.F20cCodPro
		                 Replace CurDE.CodArt With F20cCur.F20cCodArt
		                 Replace CurDE.CanFis With 0
		              EndIf
		              Replace CurDE.CanFis With CurDE.CanFis + F20cCur.F20cCanFis
		              
	*	         Case F20cCur.F20cTipMov In ('2000','2100','2999')
	*!*	             Case F20cCur.F20cTipMov = '2000' .or. ;
	*!*	                  F20cCur.F20cTipMov = '2100' .or. ;
	*!*	                  F20cCur.F20cTipMov = '2999'
	             Case F20cCur.F20cTipMov = '2999'
		              Select CurDS
		*              Locate for CurDS.DocSal=F20cCur.F20cNumDoc and ;
		*                         CurDS.CodPro=F20cCur.F20cCodPro and ;
		*                         CurDS.CodArt=F20cCur.F20cCodArt
		              Seek F20cCur.F20cNumDoc+F20cCur.F20cCodPro+F20cCur.F20cCodArt
		              If !Found()
			             *> Alta en CurDoc
			             Select CurDoc
		*	             Locate For CurDoc.CodPro=F20cCur.F20cCodPro and ; 
		*	                        CurDoc.CodArt=F20cCur.F20cCodArt
		                 Seek F20cCur.F20cCodPro+F20cCur.F20cCodArt
			             If !Found()
				             Append Blank
				             Replace CurDoc.CodPro With F20cCur.F20cCodPro
				             Replace CurDoc.CodArt With F20cCur.F20cCodArt
				             Replace CurDoc.DocEnt With 0
				             Replace CurDoc.DocSal With 0
			                 Replace CurDoc.OcuMes With 0
			             EndIf
			             Replace CurDoc.DocSal With CurDoc.DocSal + 1
			             
		                 Select CurDS
		                 Append Blank
		                 Replace CurDS.DocSal With F20cCur.F20cNumDoc
		                 Replace CurDS.CodPro With F20cCur.F20cCodPro
		                 Replace CurDS.CodArt With F20cCur.F20cCodArt
		                 Replace CurDS.CanFis With 0
		              EndIf
		              Replace CurDS.CanFis With CurDS.CanFis + F20cCur.F20cCanFis
		      EndCase

	      EndIf
	      
	      *> Acumulo reposiciones
	      If F20cCur.F20cTipMov='3060' or  ;
	         F20cCur.F20cTipMov='3070' or  ;
	         F20cCur.F20cTipMov='3080' 
	         If !Empty(F20cCur.F20cCodOpe)
		         Replace F60cCur.F60cCanRes With F60cCur.F60cCanRes + 1
	         EndIf
	         
	         If !Empty(F20cCur.F20cCodOpe)
	            Replace F62cCur.F62cTotRep With F62cCur.F62cTotRep + 1
	         EndIf
	      EndIf
	      
	      *> Actualizo Listas de Trabajos por operario
	      If F20cCur.F20cTipMov='2999' .and. !Empty(F20cCur.F20cCodOpe)
	          Select CurLst
	*	      Locate for CurLst.CLstCodPro=F20cCur.F20cCodPro and ;
	*	                 CurLst.CLstCodOpe=F20cCur.F20cCodOpe and ;
	*	                 CurLst.CLstFecMov=(AnyHM + MesHM) and ;
	*	                 CurLst.CLstNumLst=F20cCur.F20cNumLst
	          Seek F20cCur.F20cCodPro+F20cCur.F20cCodOpe+(AnyHM + MesHM)+F20cCur.F20cNumLst
		      If !Found()
		         *> Alta en F62cCur
		         Select F62cCur
	*	         Locate For F62cCur.F62cCodOpe=F20cCur.F20cCodOpe and ; 
	*	                    F62cCur.F62cFecMov=(AnyHM + MesHM)
	   	         Seek F20cCur.F20cCodPro + F20cCur.F20cCodOpe + (AnyHM + MesHM) + TipPro
		         If !Found()
		             Append Blank
		             Replace F62cCur.F62cCodPro With F20cCur.F20cCodPro
		             Replace F62cCur.F62cCodOpe With F20cCur.F20cCodOpe
		             Replace F62cCur.F62cFecMov With (AnyHM + MesHM)
		             Replace F62cCur.F62cTipPro With TipPro
	  	             Replace F62cCur.F62cLstSal With 0
	  	             Replace F62cCur.F62cMvtSal With 0
	  	             Replace F62cCur.F62cKgsSal With 0
	  	             Replace F62cCur.F62cVolSal With 0
	  	             Replace F62cCur.F62cTotRep With 0
	  	             Replace F62cCur.F62cTimSal With 0
	  	             Replace F62cCur.F62cMvtEnt With 0

	    	         Replace F62cCur.F62cPalEnt With 0
	    	         Replace F62cCur.F62cCajEnt With 0
	    	         Replace F62cCur.F62cGrpEnt With 0
		             Replace F62cCur.F62cUniEnt With 0

	    	         Replace F62cCur.F62cPalSal With 0
	    	         Replace F62cCur.F62cCajSal With 0
	    	         Replace F62cCur.F62cGrpSal With 0
		             Replace F62cCur.F62cUniSal With 0
		         
	    	         Replace F62cCur.F62cPalInt With 0
	    	         Replace F62cCur.F62cCajInt With 0
	    	         Replace F62cCur.F62cGrpInt With 0
		             Replace F62cCur.F62cUniInt With 0
		             
	  	             Replace F62cCur.F62cFlag1  With ''
	  	             Replace F62cCur.F62cFlag2  With ''
	  	             Replace F62cCur.F62cFlag3  With ''
		         EndIf
		         Replace F62cCur.F62cLstSal With F62cCur.F62cLstSal + 1
		                           
		         Select CurLst
		         Append Blank
		         Replace CurLst.CLstCodPro With F20cCur.F20cCodPro
		         Replace CurLst.CLstCodOpe With F20cCur.F20cCodOpe
		         Replace CurLst.CLstFecMov With (AnyHM + MesHM)
		         Replace CurLst.CLstNumLst With F20cCur.F20cNumLst
		         Replace CurLst.CLstCanFis With 0
		      EndIf
		      Replace CurLst.CLstCanFis With CurLst.CLstCanFis + F20cCur.F20cCanFis
	      EndIf
      *EndIf

   EndIf
      
   *> Verifico si se ha alcanzado el mes a calcular.
   If Year(F20cCur.F20cFecMov)=Anyo and Mont(F20cCur.F20cFecMov)=Mes

      If Year(F20cCur.F20cFecMov) <> Year(FecAct) .or. ;
         Mont(F20cCur.F20cFecMov) <> Mont(FecAct) .or. ;
         Day(F20cCur.F20cFecMov)  <> Day(FecAct)
         *> Nº de días desde la última actualización hasta la fecha del último movimiento
         If FecAct = {}
            FecAct  = F20cCur.F20cFecMov
         Else
*            UltAct  = UltAct - 1
            FecAct  = F20cCur.F20cFecMov
         EndIf
         NumDias = Day(UltAct) - Day(FecAct) 
         
         AnyF61 = Right('0000' + Alltrim(Str(year(UltAct))),4)
         MesF61 = Right('00' + Alltrim(Str(month(UltAct))),2)
         DiaF61 = Right('00' + Alltrim(Str(day(UltAct))),2)

         *> Elimino las ocupaciones no válidas (cantidad a 0)
	     Select F16cCur
         Delete all for f16cCur.F16cCanFis<=0
         
         *> Calculo las ocupaciones diarias
	     Select F16cCur
	     Go Top
	     Do While !Eof()

		    *> Accedo a F64c para determinar si la situación de stock se descarta.
		    DesOcu = 'N'   && No Descartar Ocupación
		    _Selec = "Select * From F64c" + _em + Space(1) + ;
		    "Where F64cCodAlm='" + Left(F16cCur.F16cCodUbi,4)  + "' And" + ;
		    "      F64cSitStk='" + F16cCur.F16cSitStk  + "'"
		    _xier = f3_SqlExec(_ASql, _Selec, 'F64cCur')
		    If _xier > 0
			    Select F64cCur
			    Go Top
			    If !Eof() and  f64cCur.F64cActivo='S'
			       DesOcu = 'S'   && Si Descartar Sit. Stock
			    EndIf
		    EndIf


			If DesOcu = 'N'
		        Select F61cDia
	*	        Locate for F61cDia.F61cCodPro=F16cCur.F16cCodPro and F61cDia.F61cCodArt=F16cCur.F16cCodArt and ;
	*		               F61cDia.F61cFecMov=(AnyF61 + MesF61 + DiaF61)
			    Seek F16cCur.F16cCodPro+F16cCur.F16cCodArt+(AnyF61 + MesF61 + DiaF61)
		        If !Found()
		           Append Blank
		           Replace F61cDia.F61cCodPro With F16cCur.F16cCodPro
		           Replace F61cDia.F61cCodArt With F16cCur.F16cCodArt
		           Replace F61cDia.F61cFecMov With AnyF61 + MesF61 + DiaF61
		           
	               *> Recupero tipo de producto
	               f_select= "F08cTipPro"
	               f_from  = "F08c"
	               f_where = "F08cCodPro='" + F16cCur.F16cCodPro + _cm + ;
	                         " and F08cCodArt='" + F16cCur.F16cCodArt + _cm
	               f_group = ""
	      
	               If f3_sql(f_select, f_from, f_where, '', f_group, 'F08cTmp')
	                  Select F08cTmp
	                  Go Top
	                  If !Eof()
	                     TipPro = F08cTmp.F08cTipPro
	                  Else
	                     TipPro = ''
	                  EndIf
	               Else
	                  TipPro = ''
	               EndIf

		           Replace F61cDia.F61cTipPro With TipPro
		           
			       Replace F61cDia.F61cPalEnt With 0
			       Replace F61cDia.F61cCajEnt With 0
			       Replace F61cDia.F61cGrpEnt With 0
			       Replace F61cDia.F61cSgrEnt With 0
			       Replace F61cDia.F61cUniEnt With 0

			       Replace F61cDia.F61cPalDev With 0
			       Replace F61cDia.F61cCajDev With 0
			       Replace F61cDia.F61cGrpDev With 0
			       Replace F61cDia.F61cSgrDev With 0
			       Replace F61cDia.F61cUniDev With 0
			      
			       Replace F61cDia.F61cPalSal With 0
			       Replace F61cDia.F61cCajSal With 0
			       Replace F61cDia.F61cGrpSal With 0
			       Replace F61cDia.F61cSgrSal With 0
			       Replace F61cDia.F61cUniSal With 0
			      
			       Replace F61cDia.F61cPalInt With 0
			       Replace F61cDia.F61cCajInt With 0
			       Replace F61cDia.F61cGrpInt With 0
			       Replace F61cDia.F61cSgrInt With 0
			       Replace F61cDia.F61cUniInt With 0
		           
		           Replace F61cDia.F61cNumOcu With 0
		           Replace F61cDia.F61cFlag1  With ''
		           Replace F61cDia.F61cFlag2  With ''
		           Replace F61cDia.F61cFlag3  With ''
		        EndIf

		        Replace F61cDia.F61cNumOcu With F61cDia.F61cNumOcu + 1
		           
			    *> Actualizo el total de ocupaciones mensuales por artículo
			    Select CurDoc
	*		    Locate for CurDoc.CodPro=F16cCur.F16cCodPro and CurDoc.CodArt=F16cCur.F16cCodArt
			    Seek F16cCur.F16cCodPro+F16cCur.F16cCodArt
	            If !Found()
	               Append Blank
	               Replace CurDoc.CodPro With F16cCur.F16cCodPro
	               Replace CurDoc.CodArt With F16cCur.F16cCodArt
	               Replace CurDoc.DocEnt With 0
	               Replace CurDoc.DocSal With 0
	               Replace CurDoc.OcuMes With 0
	            EndIf
	            
				Replace CurDoc.OcuMes With CurDoc.OcuMes + NumDias
			    
			    *> Total de ocupaciones
			    TotOcu = TotOcu + NumDias
			EndIf
		    
		    	           
	        Select F16cCur
	        Skip
	     EndDo
	     
         *> Genero registros para cada día del mes
         For i = 0 To (NumDias - 1)
         
             AnyTmp = Right('0000' + Alltrim(Str(year(UltAct))),4)
             MesTmp = Right('00' + Alltrim(Str(month(UltAct))),2)
             DiaTmp = Right('00' + Alltrim(Str(day(UltAct) - i)),2)   
         
	       	 Select F61cDia
	       	 Replace All F61cDia.F61cFecMov With AnyTmp + MesTmp + DiaTmp 
	       	 
	       	 If i>0
	       	   *> Sólo el primer día tiene movimientos, el resto sólo mantiene los huecos ocupados.
		       Replace All F61cDia.F61cPalEnt With 0
		       Replace All F61cDia.F61cCajEnt With 0
		       Replace All F61cDia.F61cGrpEnt With 0
		       Replace All F61cDia.F61cSgrEnt With 0
		       Replace All F61cDia.F61cUniEnt With 0
		       
		       Replace All F61cDia.F61cPalDev With 0
		       Replace All F61cDia.F61cCajDev With 0
		       Replace All F61cDia.F61cGrpDev With 0
		       Replace All F61cDia.F61cSgrDev With 0
		       Replace All F61cDia.F61cUniDev With 0
		      
		       Replace All F61cDia.F61cPalSal With 0
		       Replace All F61cDia.F61cCajSal With 0
		       Replace All F61cDia.F61cGrpSal With 0
		       Replace All F61cDia.F61cSgrSal With 0
		       Replace All F61cDia.F61cUniSal With 0
		      
		       Replace All F61cDia.F61cPalInt With 0
		       Replace All F61cDia.F61cCajInt With 0
		       Replace All F61cDia.F61cGrpInt With 0
		       Replace All F61cDia.F61cSgrInt With 0
		       Replace All F61cDia.F61cUniInt With 0
	       	 EndIf
	       	 
	       	 Select F61cCur
             Append From Dbf('F61cDia')
             
         EndFor
         
         UltAct = FecAct
         
 	     Select F61cDia
	     Delete all
	     
      EndIf
      
	  *If DesStk = 'N'

	*	  If F20cCur.F20cTipMov In ('1000','1100','1200','2000','2100','2999','3060','3070','3080','3100')
	      If F20cCur.F20cTipMov='1000' .or. F20cCur.F20cTipMov='1100' .or. F20cCur.F20cTipMov='1200' .or. ;
	         F20cCur.F20cTipMov='2000' .or. F20cCur.F20cTipMov='2100' .or. F20cCur.F20cTipMov='2999' .or. ;
	         F20cCur.F20cTipMov='3060' .or. F20cCur.F20cTipMov='3070' .or. F20cCur.F20cTipMov='3080' .or. ;
	         F20cCur.F20cTipMov='3100'
		   
			   Select F61cDia
			   * Locate for F61cDia.F61cCodPro=F20cCur.F20cCodPro and F61cDia.F61cCodArt=F20cCur.F20cCodArt
			   Seek F20cCur.F20cCodPro+F20cCur.F20cCodArt
			   If !Found()
			      Append Blank
			      Replace F61cDia.F61cCodPro With F20cCur.F20cCodPro
			      Replace F61cDia.F61cCodArt With F20cCur.F20cCodArt
			      Replace F61cDia.F61cFecMov With AnyHM + MesHM + DiaHM
			      Replace F61cDia.F61cTipPro With TipPro
			      
			      Replace F61cDia.F61cPalEnt With 0
			      Replace F61cDia.F61cCajEnt With 0
			      Replace F61cDia.F61cGrpEnt With 0
			      Replace F61cDia.F61cSgrEnt With 0
			      Replace F61cDia.F61cUniEnt With 0

			      Replace F61cDia.F61cPalDev With 0
			      Replace F61cDia.F61cCajDev With 0
			      Replace F61cDia.F61cGrpDev With 0
			      Replace F61cDia.F61cSgrDev With 0
			      Replace F61cDia.F61cUniDev With 0
			      
			      Replace F61cDia.F61cPalSal With 0
			      Replace F61cDia.F61cCajSal With 0
			      Replace F61cDia.F61cGrpSal With 0
			      Replace F61cDia.F61cSgrSal With 0
			      Replace F61cDia.F61cUniSal With 0
			      
			      Replace F61cDia.F61cPalInt With 0
			      Replace F61cDia.F61cCajInt With 0
			      Replace F61cDia.F61cGrpInt With 0
			      Replace F61cDia.F61cSgrInt With 0
			      Replace F61cDia.F61cUniInt With 0
			      
			      Replace F61cDia.F61cNumOcu With 0
			      Replace F61cDia.F61cFlag1  With ''
			      Replace F61cDia.F61cFlag2  With ''
			      Replace F61cDia.F61cFlag3  With ''
			   EndIf
			   
			   *> Acumulo sólo en UNIDADES
			   If F20cCur.F20cTipMov = '1100'
				  Replace F61cDia.F61cUniDev With F61cDia.F61cUniDev + F20cCur.F20cCanFis    && Palets de Devolucion
			   EndIf
			   
			   Do Case
			      Case OriRes='N'   && Palet
			           Do Case
	*		              Case F20cCur.F20cTipMov In ('1000','1100','1200')
			              Case F20cCur.F20cTipMov = '1000' .or. ;
			                   F20cCur.F20cTipMov = '1100' .or. ;
			                   F20cCur.F20cTipMov = '1200'
	*!*			                   If F20cCur.F20cTipMov = '1100'
	*!*					              
	*!*					              *> Contemplo todos los recibidos no solo los recuperados
	*!*				                  Replace F61cDia.F61cPalDev With F61cDia.F61cPalDev + F20cCur.F20cCanFis    && Palets de Devolucion
	*!*				                     
	*!*	                              *> Contemplo solo los recuperados
	*!*	 	                          *If F20cCur.F20cSitStk='1000'
	*!*				                  *   Replace F61cDia.F61cPalDev With F61cDia.F61cPalDev + F20cCur.F20cCanFis    && Palets de Devolucion
	*!*				                  *EndIf
	*!*				               EndIf
			                   If F20cCur.F20cTipMov = '1000'
				                   Replace F61cDia.F61cPalEnt With F61cDia.F61cPalEnt + F20cCur.F20cCanFis    && Palets de Entrada
			                   EndIf
	*		              Case F20cCur.F20cTipMov In ('2000','2100','2999')
	*!*			              Case F20cCur.F20cTipMov = '2000' .or. ;
	*!*			                   F20cCur.F20cTipMov = '2100' .or. ;
	*!*			                   F20cCur.F20cTipMov = '2999'
			              Case F20cCur.F20cTipMov = '2999'
			                   Replace F61cDia.F61cPalSal With F61cDia.F61cPalSal + F20cCur.F20cCanFis    && Palets de Salida
	*		              Case F20cCur.F20cTipMov In ('3060','3070','3080','3100')      
			              Case F20cCur.F20cTipMov = '3060' .or. ;
			                   F20cCur.F20cTipMov = '3070' .or. ;
			                   F20cCur.F20cTipMov = '3080' .or. ;
			                   F20cCur.F20cTipMov = '3100'
			                   Replace F61cDia.F61cPalInt With F61cDia.F61cPalInt + F20cCur.F20cCanFis    && Palets de Internos
			           EndCase
			           
			      Case OriRes='S'   && Cajas
			           Do Case
	*		              Case F20cCur.F20cTipMov In ('1000','1100','1200')
			              Case F20cCur.F20cTipMov = '1000' .or. ;
			                   F20cCur.F20cTipMov = '1100' .or. ;
			                   F20cCur.F20cTipMov = '1200'
	*!*			                   If F20cCur.F20cTipMov = '1100'
	*!*					              
	*!*					              *> Contemplo todos los recibidos no solo los recuperados
	*!*				                  Replace F61cDia.F61cCajDev With F61cDia.F61cCajDev + F20cCur.F20cCanFis    && Cajas de Devolucion
	*!*					              
	*!*					              *> Contemplo solo los recuperados
	*!*	 	                          *If F20cCur.F20cSitStk='1000'
	*!*				                  *   Replace F61cDia.F61cCajDev With F61cDia.F61cCajDev + F20cCur.F20cCanFis    && Cajas de Devolucion
	*!*				                  *EndIf
	*!*				               EndIf
			                   If F20cCur.F20cTipMov = '1000'
				                   Replace F61cDia.F61cCajEnt With F61cDia.F61cCajEnt + F20cCur.F20cCanFis    && Cajas de Entrada
			                   EndIf
	*		              Case F20cCur.F20cTipMov In ('2000','2100','2999')
	*!*			              Case F20cCur.F20cTipMov = '2000' .or. ;
	*!*			                   F20cCur.F20cTipMov = '2100' .or. ;
	*!*			                   F20cCur.F20cTipMov = '2999'
			              Case F20cCur.F20cTipMov = '2999'
			                   Replace F61cDia.F61cCajSal With F61cDia.F61cCajSal + F20cCur.F20cCanFis    && Cajas de Salida
	*		              Case F20cCur.F20cTipMov In ('3060','3070','3080','3100')      
			              Case F20cCur.F20cTipMov = '3060' .or. ;
			                   F20cCur.F20cTipMov = '3070' .or. ;
			                   F20cCur.F20cTipMov = '3080' .or. ;
			                   F20cCur.F20cTipMov = '3100'
			                   Replace F61cDia.F61cCajInt With F61cDia.F61cCajInt + F20cCur.F20cCanFis    && Cajas de Internos
			           EndCase
			           
			      Case OriRes='G'   && Grupos
		               *> Separar GRUPOS y SUBGRUPOS
	    	           Do Case
		                  Case GrpSgr = 'G'
		                  
					           Do Case
			*		              Case F20cCur.F20cTipMov In ('1000','1100','1200')
					              Case F20cCur.F20cTipMov = '1000' .or. ;
					                   F20cCur.F20cTipMov = '1100' .or. ;
					                   F20cCur.F20cTipMov = '1200'
	*!*					                   If F20cCur.F20cTipMov = '1100'
	*!*					                      
	*!*					                      *> Contemplo todos los recibidos no solo los recuperados
	*!*						                  Replace F61cDia.F61cGrpDev With F61cDia.F61cGrpDev + F20cCur.F20cCanFis    && Grupos de Devolución
	*!*					                      
	*!*					                      *> Contemplo solo los recuperados
	*!*	 	                                  *If F20cCur.F20cSitStk='1000'
	*!*						                  *   Replace F61cDia.F61cGrpDev With F61cDia.F61cGrpDev + F20cCur.F20cCanFis    && Grupos de Devolución
	*!*					                      *EndIf
	*!*						               EndIf
					                   If F20cCur.F20cTipMov = '1000'
						                   Replace F61cDia.F61cGrpEnt With F61cDia.F61cGrpEnt + F20cCur.F20cCanFis    && Grupos de Entrada
					                   EndIf
			*		              Case F20cCur.F20cTipMov In ('2000','2100','2999')
			*!*			              Case F20cCur.F20cTipMov = '2000' .or. ;
			*!*			                   F20cCur.F20cTipMov = '2100' .or. ;
			*!*			                   F20cCur.F20cTipMov = '2999'
					              Case F20cCur.F20cTipMov = '2999'
					                   Replace F61cDia.F61cGrpSal With F61cDia.F61cGrpSal + F20cCur.F20cCanFis    && Grupos de Salida
			*		              Case F20cCur.F20cTipMov In ('3060','3070','3080','3100')      
					              Case F20cCur.F20cTipMov = '3060' .or. ;
					                   F20cCur.F20cTipMov = '3070' .or. ;
					                   F20cCur.F20cTipMov = '3080' .or. ;
					                   F20cCur.F20cTipMov = '3100'
					                   Replace F61cDia.F61cGrpInt With F61cDia.F61cGrpInt + F20cCur.F20cCanFis    && Grupos de Internos
					           EndCase
		                  
		                  Case GrpSgr = 'S'
					           Do Case
			*		              Case F20cCur.F20cTipMov In ('1000','1100','1200')
					              Case F20cCur.F20cTipMov = '1000' .or. ;
					                   F20cCur.F20cTipMov = '1100' .or. ;
					                   F20cCur.F20cTipMov = '1200'
	*!*					                   If F20cCur.F20cTipMov = '1100'
	*!*					                      
	*!*					                      *> Contemplo todos los recibidos no solo los recuperados
	*!*						                  Replace F61cDia.F61cSgrDev With F61cDia.F61cSgrDev + F20cCur.F20cCanFis    && Grupos de Devolución
	*!*						                  
	*!*					                      *> Contemplo todos los recibidos no solo los recuperados
	*!*	 	                                  *If F20cCur.F20cSitStk='1000'
	*!*						                  *   Replace F61cDia.F61cSgrDev With F61cDia.F61cSgrDev + F20cCur.F20cCanFis    && Grupos de Devolución
	*!*					                      *EndIf
	*!*						               EndIf
					                   If F20cCur.F20cTipMov = '1000'
						                   Replace F61cDia.F61cSgrEnt With F61cDia.F61cSgrEnt + F20cCur.F20cCanFis    && Grupos de Entrada
					                   EndIf
			*		              Case F20cCur.F20cTipMov In ('2000','2100','2999')
			*!*			              Case F20cCur.F20cTipMov = '2000' .or. ;
			*!*			                   F20cCur.F20cTipMov = '2100' .or. ;
			*!*			                   F20cCur.F20cTipMov = '2999'
					              Case F20cCur.F20cTipMov = '2999'
					                   Replace F61cDia.F61cSgrSal With F61cDia.F61cSgrSal + F20cCur.F20cCanFis    && Grupos de Salida
			*		              Case F20cCur.F20cTipMov In ('3060','3070','3080','3100')      
					              Case F20cCur.F20cTipMov = '3060' .or. ;
					                   F20cCur.F20cTipMov = '3070' .or. ;
					                   F20cCur.F20cTipMov = '3080' .or. ;
					                   F20cCur.F20cTipMov = '3100'
					                   Replace F61cDia.F61cSgrInt With F61cDia.F61cSgrInt + F20cCur.F20cCanFis    && Grupos de Internos
			 	               EndCase
			           EndCase

			      Case OriRes='U'   && Unidades
			           Do Case
	*		              Case F20cCur.F20cTipMov In ('1000','1100','1200')
			              Case F20cCur.F20cTipMov = '1000' .or. ;
			                   F20cCur.F20cTipMov = '1100' .or. ;
			                   F20cCur.F20cTipMov = '1200'
	*!*			                   If F20cCur.F20cTipMov = '1100'
	*!*					              
	*!*					              *> Contemplo todos los recibidos no solo los recuperados
	*!*				                  Replace F61cDia.F61cUniDev With F61cDia.F61cUniDev + F20cCur.F20cCanFis    && Unidades de Devolucion
	*!*				                  
	*!*					              *> Contemplo todos los recibidos no solo los recuperados
	*!*	                              *If F20cCur.F20cSitStk='1000'
	*!*				                  *   Replace F61cDia.F61cUniDev With F61cDia.F61cUniDev + F20cCur.F20cCanFis    && Unidades de Devolucion
	*!*				                  *EndIf
	*!*				               EndIf
			                   If F20cCur.F20cTipMov = '1000'
				                   Replace F61cDia.F61cUniEnt With F61cDia.F61cUniEnt + F20cCur.F20cCanFis    && Unidades de Entrada
			                   EndIf
	*		              Case F20cCur.F20cTipMov In ('2000','2100','2999')
	*!*			              Case F20cCur.F20cTipMov = '2000' .or. ;
	*!*			                   F20cCur.F20cTipMov = '2100' .or. ;
	*!*			                   F20cCur.F20cTipMov = '2999'
			              Case F20cCur.F20cTipMov = '2999'
			                   Replace F61cDia.F61cUniSal With F61cDia.F61cUniSal + F20cCur.F20cCanFis    && Unidades de Salida
	*		              Case F20cCur.F20cTipMov In ('3060','3070','3080','3100')      
			              Case F20cCur.F20cTipMov = '3060' .or. ;
			                   F20cCur.F20cTipMov = '3070' .or. ;
			                   F20cCur.F20cTipMov = '3080' .or. ;
			                   F20cCur.F20cTipMov = '3100'
			                   Replace F61cDia.F61cUniInt With F61cDia.F61cUniInt + F20cCur.F20cCanFis    && Unidades de Internos
			           EndCase
			   EndCase
		  EndIf
	  *EndIf
   EndIf
   
   *> Actualizo cursor con existencias
   Select F16cCur
*   Locate for F16cCur.F16cCodPro=F20cCur.F20cCodPro and F16cCur.F16cCodArt=F20cCur.F20cCodArt and ;
*              F16cCur.F16cCodUbi=F20cCur.F20cUbiOri
   Seek F20cCur.F20cCodPro+F20cCur.F20cCodArt+F20cCur.F20cUbiOri+F20cCur.F20cSitStk
   
   If !Found()
      Append Blank
      Replace F16cCur.F16cCodPro With F20cCur.F20cCodPro
      Replace F16cCur.F16cCodArt With F20cCur.F20cCodArt
      Replace F16cCur.F16cCodUbi With F20cCur.F20cUbiOri
      Replace F16cCur.F16cSitStk With F20cCur.F20cSitStk
      Replace F16cCur.F16cCanFis With 0
   EndIf
   
   Do Case
      Case F20cCur.F20cEntSal='E'
        Replace F16cCur.F16cCanFis With F16cCur.F16cCanFis - F20cCur.F20cCanFis
      Case F20cCur.F20cEntSal='S'
        Replace F16cCur.F16cCanFis With F16cCur.F16cCanFis + F20cCur.F20cCanFis
   EndCase

   Select F20cCur
   Skip -1
EndDo

*> Actualizo los últimos movimientos. ----------------------------------------------------
*> Actualizo en F61cCur el nº de palet entrados y salidos.

*> Nº de días desde la última actualización hasta la fecha del último movimiento
If FecAct = {}
*   NumDias = Day(UltAct) - Day(CToD(FecIni))
    NumDias = Day(UltAct)
Else
*   UltAct  = UltAct - 1
*   NumDias = Day(UltAct) - Day(CToD(FecIni)) 
    NumDias = Day(UltAct)
EndIf

AnyF61 = Right('0000' + Alltrim(Str(year(UltAct))),4)
MesF61 = Right('00' + Alltrim(Str(month(UltAct))),2)
DiaF61 = Right('00' + Alltrim(Str(day(UltAct))),2)

 *> Elimino las ocupaciones no válidas (cantidad a 0)
 Select F16cCur
 Delete all for f16cCur.F16cCanFis<=0

 *> Calculo las ocupaciones diarias
 Select F16cCur
 Go Top
 Do While !Eof()

    *> Accedo a F64c para determinar si la situación de stock se descarta.
    DesOcu = 'N'   && No Descartar Ocupación
    _Selec = "Select * From F64c" + _em + Space(1) + ;
    "Where F64cCodAlm='" + Left(F16cCur.F16cCodUbi,4)  + "' And" + ;
    "      F64cSitStk='" + F16cCur.F16cSitStk  + "'"
    _xier = f3_SqlExec(_ASql, _Selec, 'F64cCur')
    If _xier > 0
	    Select F64cCur
	    Go Top
	    If !Eof() and f64cCur.f64cActivo='S'
	       DesOcu = 'S'   && Si Descartar Sit. Stock
	    EndIf
    EndIf


	If DesOcu = 'N'
	    Select F61cDia
	*    Locate for F61cDia.F61cCodPro=F16cCur.F16cCodPro and F61cDia.F61cCodArt=F16cCur.F16cCodArt and ;
	*		       F61cDia.F61cFecMov=(AnyF61 + MesF61 + DiaF61)
	    Seek F16cCur.F16cCodPro+F16cCur.F16cCodArt+(AnyF61 + MesF61 + DiaF61)
	    If !Found()
	       Append Blank
	       Replace F61cDia.F61cCodPro With F16cCur.F16cCodPro
	       Replace F61cDia.F61cCodArt With F16cCur.F16cCodArt
	       Replace F61cDia.F61cFecMov With AnyF61 + MesF61 + DiaF61
	       
	       *> Recupero tipo de producto
	       f_select = "F08cTipPro"
	       f_from =   "F08c"
	       f_where  = "F08cCodPro='" + F16cCur.F16cCodPro + _cm + ;
	                  " and F08cCodArt='" + F16cCur.F16cCodArt + _cm
	       f_group  = ""
	      
	       If f3_sql(f_select, f_from, f_where, '', f_group, 'F08cTmp')
	          Select F08cTmp
	          Go Top
	          If !Eof()
	             TipPro = F08cTmp.F08cTipPro
	          Else
	             TipPro = ''
	          EndIf
	       Else
	          TipPro = ''
	       EndIf

		   Replace F61cDia.F61cTipPro With TipPro
		   
		   Replace F61cDia.F61cPalEnt With 0
		   Replace F61cDia.F61cCajEnt With 0
		   Replace F61cDia.F61cGrpEnt With 0
		   Replace F61cDia.F61cSgrEnt With 0
		   Replace F61cDia.F61cUniEnt With 0

		   Replace F61cDia.F61cPalDev With 0
		   Replace F61cDia.F61cCajDev With 0
		   Replace F61cDia.F61cGrpDev With 0
		   Replace F61cDia.F61cSgrDev With 0
		   Replace F61cDia.F61cUniDev With 0
			      
		   Replace F61cDia.F61cPalSal With 0
		   Replace F61cDia.F61cCajSal With 0
	       Replace F61cDia.F61cGrpSal With 0
	       Replace F61cDia.F61cSgrSal With 0
		   Replace F61cDia.F61cUniSal With 0
			      
		   Replace F61cDia.F61cPalInt With 0
		   Replace F61cDia.F61cCajInt With 0
		   Replace F61cDia.F61cGrpInt With 0
		   Replace F61cDia.F61cSgrInt With 0
		   Replace F61cDia.F61cUniInt With 0
		   
	       Replace F61cDia.F61cNumOcu With 0
	       Replace F61cDia.F61cFlag1  With ''
	       Replace F61cDia.F61cFlag2  With ''
	       Replace F61cDia.F61cFlag3  With ''
	    EndIf

		    Replace F61cDia.F61cNumOcu With F61cDia.F61cNumOcu + 1
		       
		    *> Actualizo el total de ocupaciones mensuales por artículo
		    Select CurDoc
		    Seek F16cCur.F16cCodPro+F16cCur.F16cCodArt
		    If !Found()
		        Append Blank
		        Replace CurDoc.CodPro With F16cCur.F16cCodPro
		        Replace CurDoc.CodArt With F16cCur.F16cCodArt
		        Replace CurDoc.DocEnt With 0
		        Replace CurDoc.DocSal With 0
		        Replace CurDoc.OcuMes With 0
		    EndIf
			Replace CurDoc.OcuMes With CurDoc.OcuMes + NumDias

			*> Total de ocupaciones
		    TotOcu = TotOcu + NumDias
	EndIf

    Select F16cCur
    Skip
 EndDo

*> Genero registros para cada día del mes (hasta el primer dia del mes)
*> Genero registros para cada día del mes
For i = 0 To (NumDias - 1) 
         
    AnyTmp = Right('0000' + Alltrim(Str(year(UltAct))),4)
    MesTmp = Right('00' + Alltrim(Str(month(UltAct))),2)
    DiaTmp = Right('00' + Alltrim(Str(day(UltAct) - i)),2)   

  	Select F61cDia
  	Replace All F61cDia.F61cFecMov With AnyTmp + MesTmp + DiaTmp 
  	
	If i>0
	   *> Sólo el primer día tiene movimientos, el resto sólo mantiene los huecos ocupados.
	   Replace All  F61cDia.F61cPalEnt With 0
	   Replace All  F61cDia.F61cCajEnt With 0
	   Replace All  F61cDia.F61cGrpEnt With 0
	   Replace All  F61cDia.F61cSgrEnt With 0
	   Replace All  F61cDia.F61cUniEnt With 0

	   Replace All  F61cDia.F61cPalDev With 0
	   Replace All  F61cDia.F61cCajDev With 0
	   Replace All  F61cDia.F61cGrpDev With 0
	   Replace All  F61cDia.F61cSgrDev With 0
	   Replace All  F61cDia.F61cUniDev With 0
		      
	   Replace All  F61cDia.F61cPalSal With 0
	   Replace All  F61cDia.F61cCajSal With 0
       Replace All  F61cDia.F61cGrpSal With 0
       Replace All  F61cDia.F61cSgrSal With 0
	   Replace All  F61cDia.F61cUniSal With 0
		      
	   Replace All  F61cDia.F61cPalInt With 0
	   Replace All  F61cDia.F61cCajInt With 0
	   Replace All  F61cDia.F61cGrpInt With 0
	   Replace All  F61cDia.F61cSgrInt With 0
	   Replace All  F61cDia.F61cUniInt With 0
	   
       Replace All  F61cDia.F61cNumOcu With 0
       Replace All  F61cDia.F61cFlag1  With ''
       Replace All  F61cDia.F61cFlag2  With ''
       Replace All  F61cDia.F61cFlag3  With ''
	EndIf
  	
  	Select F61cCur
    Append From Dbf('F61cDia')
EndFor

*> NUEVO - > Se anulan aquellos registros sin movimientos
*> Esto supone no aprovechar el cálculo de ocupaciones que se ha hecho anteriormente
Select F61cCur
Delete all for (F61cPalEnt=0 and F61cCajEnt=0 and F61cGrpEnt=0 and F61cSgrEnt=0 and F61cUniEnt=0  ;
            and F61cPalDev=0 and F61cCajDev=0 and F61cGrpDev=0 and F61cSgrDev=0 and F61cUniDev=0  ;
            and F61cPalSal=0 and F61cCajSal=0 and F61cGrpSal=0 and F61cSgrSal=0 and F61cUniSal=0  ;
            and F61cPalInt=0 and F61cCajInt=0 and F61cGrpint=0 and F61cSgrInt=0 and F61cUniInt=0)
           
Select F61cDia
Delete all

*> -----------------------------------------------------------------------------------

*> Actualizo nº de documentos de salida y de entrada, y ocupaciones
Select F60cCur
Go Top
Do While !Eof()
    *> Rotaciones
    If MvtEnt<>0
       Replace F60cCur.F60cRotEnt With F60cCur.F60cMvtEnt/MvtEnt * 100
    EndIf
    If MvtDev<>0
       Replace F60cCur.F60cRotDev With F60cCur.F60cMvtDev/MvtDev * 100
    EndIf
    If MvtSal<>0
       Replace F60cCur.F60cRotSal With F60cCur.F60cMvtSal/MvtSal * 100
    EndIf
    If MvtInt<>0
       Replace F60cCur.F60cRotInt With F60cCur.F60cMvtInt/MvtInt * 100
    EndIf
    If MvtPal<>0
       Replace F60cCur.F60cRotPal With F60cCur.F60cMvtPal/MvtPal * 100
    EndIf
    If MvtCaj<>0
       Replace F60cCur.F60cRotCaj With F60cCur.F60cMvtCaj/MvtCaj * 100
    EndIf
    If MvtGrp<>0
       Replace F60cCur.F60cRotGrp With F60cCur.F60cMvtGrp/MvtGrp * 100
    EndIf
    If MvtSgr<>0
       Replace F60cCur.F60cRotSgr With F60cCur.F60cMvtSgr/MvtSgr * 100
    EndIf
    If MvtUni<>0
       Replace F60cCur.F60cRotUni With F60cCur.F60cMvtUni/MvtUni * 100
    EndIf

    Select F08cCur
    Seek F60cCur.F60cCodPro+F60cCur.F60cCodArt
    If Found() .and. MvtArt<>0 
       Replace F60cCur.F60cCoeRot With F08cCur.MvtArt/MvtTot * 100
    EndIf

    *> Actualizo documentos de entrada y de salida
    Select CurDoc
    Seek F60cCur.F60cCodPro+F60cCur.F60cCodArt
    If Found()
       Replace F60cCur.F60cDocEnt With CurDoc.DocEnt
       Replace F60cCur.F60cDocSal With CurDoc.DocSal
       Replace F60cCur.F60cMedOcu With CurDoc.OcuMes/TotDias
       Replace F60cCur.F60cPerOcu With (CurDoc.OcuMes/TotOcu)*100
    EndIf

    Select F60cCur
    Skip
EndDo

*> Calculo tiempo empleado en listas. ------------------------------------------------------
Select CurLst
Go Top
Do While !Eof()

   *> Recupero la diferencia entre la hora inicial y final por lista de trabajo.
   *> Se transforman las fechas a juliano y se restan.
   *> En ORACLE, la diferencia entre fechas devuelve un número.
   *> En SQLSERVER, la diferencia entre fechas devuelve otra fecha.

   f_select = "F20cCodPro, F20cCodOpe, "
   f_select = f_select + "Max(F20cFecMov) As MaxFecMov, Min(F20cFecMov) As MinFecMov, 0 As DifFec"

   f_from =   "F20c"
   f_where  = "F20cNumLst='" + CurLst.CLstNumLst + "' " + ;
              " And F20cTipMov Like '3%' And F20cEntSal ='S'"
   f_group  = "F20cCodPro, F20cCodOpe"

   If f3_sql(f_select, f_from, f_where, '', f_group, 'F20cTim')
      Select F20cTim
      Go Top
      Do While !Eof()
         Replace DifFec With Val(Sys(11, MaxFecMov)) - Val(Sys(11, MinFecMov))
         Skip
      EndDo

      Go Top
      Do While !Eof()
         Tiempo = DifFec       && Valor en segundos
         *Tiempo = DifFec / 60  && Valor en minutos

         Select F62cCur
	     *Seek F20cCur.F20cCodPro + F20cCur.F20cCodOpe + (AnyHM + MesHM) + TipPro
         Seek F20cTim.F20cCodPro + F20cTim.F20cCodOpe + (AnyHM + MesHM)
         If !Found()
	        Append blank
	        Replace F62cCur.F62cCodPro With F20cTim.F20cCodPro
	        Replace F62cCur.F62cCodOpe With F20cTim.F20cCodOpe
	        Replace F62cCur.F62cFecMov With (AnyHM + MesHM)
	        Replace F62cCur.F62cTipPro With TipPro
	        Replace F62cCur.F62cLstSal With 0
	        Replace F62cCur.F62cMvtSal With 0
	        Replace F62cCur.F62cKgsSal With 0
	        Replace F62cCur.F62cVolSal With 0
	        Replace F62cCur.F62cTotRep With 0
	        Replace F62cCur.F62cTimSal With 0
	        Replace F62cCur.F62cMvtEnt With 0
	        
	        Replace F62cCur.F62cPalEnt With 0
	        Replace F62cCur.F62cCajEnt With 0
	        Replace F62cCur.F62cGrpEnt With 0
	        Replace F62cCur.F62cUniEnt With 0

	        Replace F62cCur.F62cPalSal With 0
	        Replace F62cCur.F62cCajSal With 0
	        Replace F62cCur.F62cGrpSal With 0
	        Replace F62cCur.F62cUniSal With 0
	 
	        Replace F62cCur.F62cPalInt With 0
	        Replace F62cCur.F62cCajInt With 0
	        Replace F62cCur.F62cGrpInt With 0
	        Replace F62cCur.F62cUniInt With 0
	     
	        Replace F62cCur.F62cFlag1  With ''
	        Replace F62cCur.F62cFlag2  With ''
	        Replace F62cCur.F62cFlag3  With ''
	     EndIf
	     
	     Replace F62cCur.F62cTimSal With F62cCur.F62cTimSal + Tiempo

         Select F20cTim
         Skip
      EndDo
   EndIf

   
   Select CurLst
   Skip
EndDo

*> NUEVO - > Se anulan aquellos registros sin datos
Select F62cCur
Delete all for (F62cLstSal=0 and F62cMvtSal=0 and F62cKgsSal=0 and F62cVolSal=0  ;
            and F62cTotRep=0 and F62cTimSal=0 and F62cMvtEnt=0 and F62cMvtDev=0  ;
            and F62cPalEnt=0 and F62cCajEnt=0 and F62cGrpEnt=0 and F62cUniEnt=0  ;
            and F62cPalSal=0 and F62cCajSal=0 and F62cGrpSal=0 and F62cUniSal=0  ;
            and F62cPalInt=0 and F62cCajInt=0 and F62cGrpInt=0 and F62cUniInt=0)

*> Actualizar bases de datos F60c, estadísticas mensual por artículo  ----------------------
*> Eliminar estadísticas existentes
cWhere = " F60cCodPro='" + CPro + "'" + ;
         " and F60cFecMov='" + Right('0000' + Alltrim(Str(Anyo)),4) + Right('00' + Alltrim(Str(Mes)),2) + "'"
=f3_DelTun('F60c', , cWhere, 'N')

*> Insertar estadísticas calculadas
Select F60cCur
Go Top
Do While !Eof()
	If F60cCodPro==CPro
		If !F3_InsTun("F60c", "F60cCur", "N")
		   _LxErr = 'NO se ha podido insertar registro'
		   Do Form St3Inc With .T.
		   Return .F.
		EndIf
	EndIf

	Select F60cCur
	Skip
EndDo

*> Actualizar bases de datos F61c, estadísticas diarias por artículo  ----------------------------------------

*> Eliminar estadísticas existentes
cWhere = " F61cCodPro='" + CPro + "'" + ;
         " And " + _GCSS("F61cFecMov", 1, 4) + "='" + Right('0000' + Alltrim(Str(Anyo)),4) + "'" + ;
         " And " + _GCSS("F61cFecMov", 5, 2) + "='" + Right('00' + Alltrim(Str(Mes)),2) + "'"
=f3_DelTun('F61c', , cWhere, 'N')

*> Insertar estadísticas calculadas

Select F61cCur
Go Top
Set filter to f61ccodpro=' ' 
delete all
Set filter to

Select F61cCur
Go Top
Do While !Eof()
	If F61cCodPro==CPro
		If !F3_InsTun("F61c", "F61cCur", "N")
			_LxErr = 'NO se ha podido insertar registro'
			Do Form St3Inc With .T.
			Return .F.
		EndIf
	EndIf

	Select F61cCur
	Skip
EndDo
		
*> Actualizar bases de datos F62c, estadísticas mensuales por operario  --------------------------------------

*> Eliminar estadísticas existentes
cWhere = " F62cCodPro='" + CPro + "'" + ;
         " And F62cFecMov='" + Right('0000' + Alltrim(Str(Anyo)),4) + Right('00' + Alltrim(Str(Mes)),2) + "'"
=f3_DelTun('F62c', , cWhere, 'N')

*> Insertar estadísticas calculadas
Select F62cCur
Go Top
Do While !Eof()
	If F62cCodPro==CPro
		If !F3_InsTun("F62c", "F62cCur", "N")
			_LxErr = 'NO se ha podido insertar registro'
			Do Form St3Inc With .T.
			Return .F.
		EndIf
	EndIf

	Select F62cCur
	Skip
EndDo

*> Borrar cursores
If Used('F08cCur')
   Use In F08cCur
EndIf
If Used('F16cCur')
   Use In F16cCur
EndIf
If Used('F20cCur')
   Use In F20cCur
EndIf
If Used('F26cCur')
   Use In F26cCur
EndIf
If Used('F60cCur')
   Use In F60cCur
EndIf
If Used('F61cCur')
   Use In F61cCur
EndIf
If Used('F62cCur')
   Use In F62cCur
EndIf
If Used('F08cTmp')
   Use In F08cTmp
EndIf
If Used('F16cDia')
   Use In F08cDia
EndIf
If Used('CurDE')
   Use In CurDE
EndIf
If Used('CurDS')
   Use In CurDS
EndIf
If Used('CurLst')
   Use In CurLst
EndIf

Return

********************************************************************************
*> 
*>   GenEstDis : Generación de estadísticas Distribución
*>
*>     Estadística mensual de distribución
*>
*>   Recibe:
*>      Propietario.
*>      Año
*>      Mes
*>
********************************************************************************
Procedure GenEstDis
Parameters CPro, Anyo, Mes

Private f_select, f_where, f_from, f_group, _ok
Private FecAct, anyofin, mesfin
Private N_Reg, N_Total

Store 0 To MvtTot, MvtEnt, MvtSal, MvtPal, MvtCaj, MvtGrp, MvtUni
Store 0 To N_Reg, N_Total

*> Monto la fecha inicial y la final
FecIni = '01' + '/' + Right('00' + Alltrim(Str(Mes)),2) + '/' + Right('0000' + Alltrim(Str(Anyo)),4)

If Mes=12
   MesFin = 1
   AnyoFin = Anyo + 1
Else
   MesFin = Mes + 1
   AnyoFin = Anyo
EndIf

FecFin = '01' + '/' + Right('00' + Alltrim(Str(MesFin)),2) + '/' + Right('0000' + Alltrim(Str(AnyoFin)),4)

=WaitWindow('Importando datos del mes  ' + ;
             Alltrim(Str(Mes)) +  ' de ' + Alltrim(Str(Anyo)) + ;
             ', pulse una tecla para continuar ...')

*> Creo Cursor F63cCur  Estadística Mensual/Distribución
IndAux = FnTemp(1)
=CrtCursor('F63c', 'F63cCur' ,'C') 
INDEX ON F63cCodPro+Str(F63cPriori)+F63cFecMov TO (IndAux)

*> Creo Cursor MAC's
IndAux = FnTemp(1)
Create cursor CurMAC (NumMAC C(9), MACVta N(2, 0), MACPro N(2,0))
INDEX ON NumMac TO (IndAux)

*> Cargo Cursor con las Hojas de Ruta del mes
f_select = "*"
f_from =   "F31c"
f_where  = "F31cFecRut>=" + _GCD(FecIni) + " And F31cFecRut < " + _GCD(FecFin)

If !f3_sql(f_select, f_from, f_where, '', '', 'F31cCur')
   If _xier <= 0
	   _LxErr = 'No se han seleccionado registros del fichero de hojas de ruta'
	   Do Form St3Inc
	   _LxErr = ''
	   Return 0
   EndIf
EndIf

Select F31cCur
Count to N_Total
N_Reg=0

*> Recorro movimientos y actualizo stock y estadísticas
Select F31cCur
Go Top
Do While !Eof()

    N_Reg = N_Reg + 1
    =WaitWindow("Procesando registro número " + Alltrim(Str(N_Reg)) + " de un total de " + Alltrim(Str(N_Total)))

	*> Obtengo el detalle, para conocer el propietario.
	f_select = "*"
	f_from =   "F31l"
	f_where  = "F31lHojRut='"  + F31cCur.F31cHojRut + _cm

	If !f3_sql(f_select, f_from, f_where, '', '', 'F31lCur')
	   _LxErr = 'No se han seleccionado registros del fichero de hojas de ruta'
	   Do Form St3Inc
	   _LxErr = ''
	   
       Select F31cCur
	   Skip
	   Loop
	EndIf

	AnyHR = Right('0000' + Alltrim(Str(year (F31cCur.F31cFecRut))),4)
	MesHR = Right('00'   + Alltrim(Str(month(F31cCur.F31cFecRut))),2)
	
	Select F31lCur
	Go Top
	Do While !Eof()

		*> Para cada albarán accedo a sus pedidos.
		f_select = "*"
		f_from =   "F27c"
		f_where  = "F27cCodPro='" + F31lCur.F31lCodEnt + _cm + ;
		           " And F27cNumAlb='" + F31lCur.F31lAlbRep + _cm

		If !f3_sql(f_select, f_from, f_where, '', '', 'F27cCur')
		   _LxErr = 'No se han seleccionado registros de cabecera de albaranes'
		   Do Form St3Inc With .T.
		   
	       Select F31lCur
		   Skip
		   Loop
		EndIf

		Select F27cCur
		Go Top
		Do While !Eof()
		
			*> Variable utilizada para controlar si el tipo de albarán
			*> 1 = Venta
			*> 2 = Promoción
			*> 3 = Mixto o ambos
	        TipAlb = 0

			*> Accedo al detalle del albarán
			f_select = "*"
			f_from =   "F27l"
			f_where  = "F27lCodPro='" + F27cCur.F27cCodPro + _cm + ;
			           " And F27lNumAlb='" + F27cCur.F27cNumAlb + _cm

			If !f3_sql(f_select, f_from, f_where, '', '', 'F27lCur')
			   _LxErr = 'No se han seleccionado registros de detalle de albaranes'
			   Do Form St3Inc With .T.
			   
		       Select F27cCur
			   Skip
			   Loop
			EndIf

            *> Recorro el detalle de los albaranes.
			Select F27lCur
			Go Top
			Do While !Eof()
			
	            *> Accedo a F26l y calculo el total de bultos
				* f_select = " count(unique F26lnummac) as NumBul"
				f_select = "F26lNumMac"
				f_from =   "F26l"
				f_where  = "F26lCodPro='" + F27lCur.F27lCodPro + _cm + ;
				           " and F26lTipDoc='" + F27lCur.F27lTipDoc + _cm + ;
				           " and F26lNumDoc='" + F27lCur.F27lNumDoc + _cm + ;
				           " and F26lLinDoc='" + F27lCur.F27lLinDoc + _cm
				f_group  = "F26lNumMac"
				           
	            If !f3_sql(f_select, f_from, f_where, '', f_group, 'F26lMac')
				   _LxErr = 'No se han seleccionado registros de listas de trabajo'
				   Do Form St3Inc With .T.
				   
			       Select F27lCur
				   Skip
				   Loop
				EndIf

	            *> Accedo a F26l y calculo el total de bultos
				* f_select = " count(unique F26lnummac) as NumBul"
				f_select = "*"
				f_from =   "F26l"
				f_where  = "F26lCodPro='" + F27lCur.F27lCodPro + _cm + ;
				           " And F26lTipDoc='" + F27lCur.F27lTipDoc + _cm + ;
				           " And F26lNumDoc='" + F27lCur.F27lNumDoc + _cm + ;
				           " And F26lLinDoc='" + F27lCur.F27lLinDoc + _cm
				f_group = ''

	            If !f3_sql(f_select, f_from, f_where, '', f_group, 'F26lCur')
				   _LxErr = 'No se han seleccionado registros de listas de trabajo'
				   Do Form St3Inc
				   _LxErr = ''
				   
			       Select F27lCur
				   Skip
				   Loop
				EndIf

				*> Determino el tipo de bulto, venta, promoción o ambos
				f_select = "*"
				f_from =   "F24l"
				f_where  = "F24lCodPro='" + F27lCur.F27lCodPro + _cm + ;
				           " And F24lTipDoc='" + F27lCur.F27lTipDoc + _cm + ;
				           " And F24lNumDoc='" + F27lCur.F27lNumDoc + _cm + ;
				           " And F24lLinDoc='" + F27lCur.F27lLinDoc + _cm
				           
				If !f3_sql(f_select, f_from, f_where, '', '', 'F24lCur')
				   _LxErr = 'No se han seleccionado registros de detalle de pedidos'
				   Do Form St3Inc
				   _LxErr = ''
				   
			       Select F27lCur
				   Skip
				   Loop
				EndIf
			
				*> Recupero la prioridad del pedido que determina el tipo de expedición
				*>  F24cPriori  ->   Convencional
				*>                   Hospitales
				*>                   Frio
				*>                   Estupefacientes
				
				f_select = "*"
				f_from =   "F24c"
				f_where  = "F24cCodPro='" + F27cCur.F27cCodPro + _cm + ;
				           " And F24cTipDoc='" + F27cCur.F27cTipDoc + _cm + ;
				           " And F24cNumDoc='" + F27cCur.F27cNumDoc + _cm

				If !f3_sql(f_select, f_from, f_where, '', '', 'F24cCur')
				   _LxErr = 'No se han seleccionado registros de detalle de pedidos'
				   Do Form St3Inc
				   _LxErr = ''

			       Select F27lCur
				   Skip
				   Loop
				EndIf
			
			    Priori = F24cCur.F24cPriori

				*> Actualizo fichero F63c
				Select F24lCur  && Recupero Precio
				Go Top
				Locate For F24lCur.F24lCodPro = F27lCur.F27lCodPro and  ;
				           F24lCur.F24lTipDoc = F27lCur.F27lTipDoc and  ;
				           F24lCur.F24lNumDoc = F27lCur.F27lNumDoc and  ;
				           F24lCur.F24lLinDoc = F27lCur.F27lLinDoc
				If Found()
				    *> Recupero peso unitario
					f_select = "*"
					f_from =   "F08c"
					f_where  = "F08cCodPro='" + F24lCur.F24lCodPro + _cm + ;
					           " And F08cCodArt='" + F24lCur.F24lCodArt + _cm

					If !f3_sql(f_select, f_from, f_where, '', '', 'F08cCur')
					   _LxErr = 'No se ha encontrado la ficha del artículo'
					   Do Form St3Inc
					   _LxErr = ''

				       Select F27lCur
					   Skip
					   Loop
					EndIf
				    
				    Eur  = F27lCur.F27lCanSer * F24lCur.F24lPrecio
				    Kgs  = F27lCur.F27lCanSer * F08cCur.F08cPesUni/1000
				    
					Select F63cCur
*					Locate For F63cCur.F63cCodPro=F31lCur.F31lCodEnt and ;
*							   F63cCur.F63cFecMov=AnyHR+MesHR
					Seek F31lCur.F31lCodEnt + Str(Priori) + AnyHR+MesHR
					If !Found()
					   Append Blank
					   Replace F63cCodPro With F31lCur.F31lCodEnt
					   Replace F63cPriori With Priori
					   Replace F63cFecMov With AnyHR+MesHR
					   
					   Replace F63cExpVta With 0
					   Replace F63cExpPro With 0
					   Replace F63cExpAmb With 0
					   
					   Replace F63cBulVta With 0
					   Replace F63cBulPro With 0
					   Replace F63cBulAmb With 0
					   Replace F63cPalAmb With 0
					   Replace F63cCajAmb With 0
					   Replace F63cGrpAmb With 0
					   Replace F63cUniAmb With 0
					   
					   Replace F63cPalFri With 0
					   Replace F63cCajFri With 0
					   Replace F63cGrpFri With 0
					   Replace F63cUniFri With 0
					   
					   Replace F63cPalPsi With 0
					   Replace F63cCur.F63cCajPsi With 0
					   Replace F63cCur.F63cGrpPsi With 0
					   Replace F63cCur.F63cUniPsi With 0
					   
					   Replace F63cPalVar With 0
					   Replace F63cCajVar With 0
					   Replace F63cGrpVar With 0
					   Replace F63cUniVar With 0
					   
					   Replace F63cKgsVta With 0
					   Replace F63cKgsPro With 0
					   
					   Replace F63cEurVta With 0
					   Replace F63cEurPro With 0
					   
					   Replace F63cLinVta With 0
					   Replace F63cLinPro With 0
					   
					   Replace F63cFlag1  With ""
					   Replace F63cFlag2  With ""
					   Replace F63cFlag3  With ""
					EndIf
					
				    Do Case
*				       Case F24lCur.F24lFlag1=' '
				       Case F24lCur.F24lPrecio<>0
						   Replace F63cKgsVta With F63cKgsVta + Kgs
						   Replace F63cEurVta With F63cEurVta + Eur
                           Replace F63cLinVta With F63cLinVta + 1
				       Otherwise
						   Replace F63cKgsPro With F63cKgsPro + Kgs
						   Replace F63cEurPro With F63cEurPro + Eur
                           Replace F63cLinPro With F63cLinPro + 1
				    EndCase
				EndIf

				*> Actualizo fichero con nº de MACS
				Select F26lCur
				Do While !Eof()
				    *> Actualizo bultos expedidos según el origen y el tipo.

				    *> Recupero peso unitario
					f_select = "*"
					f_from =   "F08c"
					f_where  = "F08cCodPro='" + F26lCur.F26lCodPro + _cm + ;
					           " And F08cCodArt='" + F26lCur.F26lCodArt + _cm

					If !f3_sql(f_select, f_from, f_where, '', '', 'F08cCur')
					   _LxErr = 'No se ha encontrado la ficha del artículo'
					   Do Form St3Inc With .T.

     				   Select F26lCur
					   Skip
					   Loop
					EndIf

					Select F26lCur

				    Do Case
				       Case F08cCur.F08cTipPro='AMB'  && Ambiente o convencional
						    Do Case
						       Case F26lCur.F26lOriRes='N'   && Palet
		                            Replace F63cCur.F63cPalAmb With F63cCur.F63cPalAmb + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='S'   && Cajas
		                            Replace F63cCur.F63cCajAmb With F63cCur.F63cCajAmb + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='G'   && Grupos
		                            Replace F63cCur.F63cGrpAmb With F63cCur.F63cGrpAmb + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='U'   && Unidades
		                            Replace F63cCur.F63cUniAmb With F63cCur.F63cUniAmb + F26lCur.F26lCanFis
						    EndCase

				       Case F08cCur.F08cTipPro='FRI'  && Frio
						    Do Case
						       Case F26lCur.F26lOriRes='N'   && Palet
		                            Replace F63cCur.F63cPalFri With F63cCur.F63cPalFri + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='S'   && Cajas
		                            Replace F63cCur.F63cCajFri With F63cCur.F63cCajFri + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='G'   && Grupos
		                            Replace F63cCur.F63cGrpFri With F63cCur.F63cGrpFri + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='U'   && Unidades
		                            Replace F63cCur.F63cUniFri With F63cCur.F63cUniFri + F26lCur.F26lCanFis
						    EndCase

				       Case F08cCur.F08cTipPro='PSI'  && Psicotrópicos
						    Do Case 
						       Case F26lCur.F26lOriRes='N'   && Palet
		                            Replace F63cCur.F63cPalPsi With F63cCur.F63cPalPsi + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='S'   && Cajas
		                            Replace F63cCur.F63cCajPsi With F63cCur.F63cCajPsi + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='G'   && Grupos
		                            Replace F63cCur.F63cGrpPsi With F63cCur.F63cGrpPsi + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='U'   && Unidades
		                            Replace F63cCur.F63cUniPsi With F63cCur.F63cUniPsi + F26lCur.F26lCanFis
						    EndCase

				       Otherwise                      && Varios
						    Do Case
						       Case F26lCur.F26lOriRes='N'   && Palet
		                            Replace F63cCur.F63cPalVar With F63cCur.F63cPalVar + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='S'   && Cajas
		                            Replace F63cCur.F63cCajVar With F63cCur.F63cCajVar + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='G'   && Grupos
		                            Replace F63cCur.F63cGrpVar With F63cCur.F63cGrpVar + F26lCur.F26lCanFis
						       Case F26lCur.F26lOriRes='U'   && Unidades
		                            Replace F63cCur.F63cUniVar With F63cCur.F63cUniVar + F26lCur.F26lCanFis
						    EndCase
				    EndCase
				    
					Select F26lCur
					Skip
				EndDo

				*> Actualizo fichero con nº de MACS
				Select F26lMac
				Go Top
				Do While !Eof()

					If !Empty(F26lMac.F26lNumMac)
	                    *> Me posiciono 				
						Select F24lCur
						Go Top
						
						Select CurMAC
						Seek F26lMac.F26lNumMac
					    If !Found()
					       Append Blank
					       Replace CurMAC.NumMac With F26lMac.F26lNumMac
					       Replace CurMAC.MACVta With 0
					       Replace CurMAC.MACPro With 0
					    EndIf
					EndIf
					
				    Do Case
*				       Case F24lCur.F24lFlag1=' '
				       Case F24lCur.F24lPrecio<>0
				            Replace CurMAC.MACVta With 1
				       Otherwise
				            Replace CurMAC.MACPro With 1
				    EndCase
					
					Select F26lMac
					Skip
				EndDo

			    *> Determino el tipo de expedición (venta, mixta o promoción)
				Select F24lCur
				Go Top
					
			    Do Case
*			       Case F24lCur.F24lFlag1=' '
			       Case F24lCur.F24lPrecio<>0    && Venta
			            Do Case
			            	Case TipAlb=1  && Venta
			            	Case TipAlb=2  && Promoción
			            	     TipAlb=3  && Ambos (mixto)
			            	Case TipAlb=3
			            	Case TipAlb=0
			            	     TipAlb=1
			            EndCase 
			       Otherwise                     && Promoción
			            Do Case
			            	Case TipAlb=1  && Venta
			            	     TipAlb=3  && Ambos (mixto)
			            	Case TipAlb=2  && Promoción
			            	Case TipAlb=3
			            	Case TipAlb=0
			            	     TipAlb=2
			            EndCase
			    EndCase
					

				Select F27lCur
				Skip
			EndDo

			*> Actualizo el número de expediciones
			If TipAlb>0
				*> Actualizo fichero F63c
				Select F63cCur
                Seek F31lCur.F31lCodEnt + Str(Priori) + AnyHR+MesHR

				If !Found()
				   Append Blank
					   Replace F63cCodPro With F31lCur.F31lCodEnt
					   Replace F63cPriori With Priori
					   Replace F63cFecMov With AnyHR+MesHR
					   
					   Replace F63cExpVta With 0
					   Replace F63cExpPro With 0
					   Replace F63cExpAmb With 0
					   
					   Replace F63cBulVta With 0
					   Replace F63cBulPro With 0
					   Replace F63cBulAmb With 0
					   
					   Replace F63cPalAmb With 0
					   Replace F63cCajAmb With 0
					   Replace F63cGrpAmb With 0
					   Replace F63cUniAmb With 0
					   
					   Replace F63cPalFri With 0
					   Replace F63cCajFri With 0
					   Replace F63cGrpFri With 0
					   Replace F63cUniFri With 0
					   
					   Replace F63cPalPsi With 0
					   Replace F63cCajPsi With 0
					   Replace F63cGrpPsi With 0
					   Replace F63cUniPsi With 0
					   
					   Replace F63cPalVar With 0
					   Replace F63cCajVar With 0
					   Replace F63cGrpVar With 0
					   Replace F63cUniVar With 0
					   
					   Replace F63cKgsVta With 0
					   Replace F63cKgsPro With 0
					   
					   Replace F63cEurVta With 0
					   Replace F63cEurPro With 0
					   
					   Replace F63cLinVta With 0
					   Replace F63cLinPro With 0
					   
					   Replace F63cFlag1  With ""
					   Replace F63cFlag2  With ""
					   Replace F63cFlag3  With ""
				EndIf

				Do Case
				   Case TipAlb=1  && Venta
					   Replace F63cExpVta With F63cExpVta + 1
				   Case TipAlb=2  && Promoción
					   Replace F63cExpPro With F63cExpPro + 1
				   Case TipAlb=3  && Ambos
					   Replace F63cExpAmb With F63cExpAmb + 1
				EndCase
			EndIf
			
			*> Actualizo el número número de MACS
			Select CurMAC
			Go Top
			Do While !Eof()
				*> Actualizo fichero F63c
				Select F63cCur
				Seek F31lCur.F31lCodEnt + Str(Priori) + AnyHR+MesHR
				If !Found()
				   Append Blank
					   Replace F63cCodPro With F31lCur.F31lCodEnt
					   Replace F63cPriori With Priori
					   Replace F63cFecMov With AnyHR+MesHR
					   
					   Replace F63cExpVta With 0
					   Replace F63cExpPro With 0
					   Replace F63cExpAmb With 0
					   
					   Replace F63cBulVta With 0
					   Replace F63cBulPro With 0
					   Replace F63cBulAmb With 0
					   
					   Replace F63cPalAmb With 0
					   Replace F63cCajAmb With 0
					   Replace F63cGrpAmb With 0
					   Replace F63cUniAmb With 0
					   
					   Replace F63cPalFri With 0
					   Replace F63cCajFri With 0
					   Replace F63cGrpFri With 0
					   Replace F63cUniFri With 0
					   
					   Replace F63cPalPsi With 0
					   Replace F63cCajPsi With 0
					   Replace F63cGrpPsi With 0
					   Replace F63cUniPsi With 0
					   
					   Replace F63cPalVar With 0
					   Replace F63cCajVar With 0
					   Replace F63cGrpVar With 0
					   Replace F63cUniVar With 0
					   
					   Replace F63cKgsVta With 0
					   Replace F63cKgsPro With 0
					   
					   Replace F63cEurVta With 0
					   Replace F63cEurPro With 0
					   
					   Replace F63cLinVta With 0
					   Replace F63cLinPro With 0
					   
					   Replace F63cFlag1  With ""
					   Replace F63cFlag2  With ""
					   Replace F63cFlag3  With ""
				EndIf

				Do Case
				   Case CurMAC.MACVta=1  and CurMAC.MACPro=1  && Ambos
					   Replace F63cBulAmb With F63cBulAmb + 1
				   Case CurMAC.MACVta=1  and CurMAC.MACPro=0  && Venta
					   Replace F63cBulVta With F63cBulVta + 1
				   Case CurMAC.MACVta=0  and CurMAC.MACPro=1  && Promoción
					   Replace F63cBulPro With F63cBulPro + 1
				EndCase

				Select CurMAC
				Skip
			EndDo

   		    Select CurMAC
   		    Delete all
		
			Select F27cCur
			Skip
		EndDo
		
		Select F31lCur
		Skip
	EndDo
	
	Select F31cCur
	Skip
EndDo

Select F63cCur
Count to N_Total
N_Reg=0

*> Eliminar estadísticas existentes
cWhere = " F63cCodPro='" + CPro + "'" + ;
         " and F63cFecMov='" + Right('0000' + Alltrim(Str(Anyo)),4) + Right('00' + Alltrim(Str(Mes)),2) + "'"
=f3_DelTun('F63c', , cWhere, 'N')

*> Actualizo base de datos
*> Insertar estadísticas calculadas
Select F63cCur
Go Top
Do While !Eof()
	If F63cCodPro==CPro
	    N_Reg = N_Reg + 1
	    =WaitWindow("Actualizando registro número " + Alltrim(Str(N_Reg)) + " de un total de " + Alltrim(Str(N_Total)))
		If !F3_InsTun("F63c", "F63cCur", "N")
			_LxErr = 'NO se ha podido insertar registro' + cr
			Do Form St3Inc With .T.
			Return .F.
		EndIf
	EndIf
	
	Select F63cCur
	Skip
EndDo

*> Borrar cursores
If Used('F31cCur')
   Use In F31cCur
EndIf
If Used('F31lCur')
   Use In F31lCur
EndIf
If Used('F27cCur')
   Use In F27cCur
EndIf
If Used('F27lCur')
   Use In F27lCur
EndIf
If Used('F26lCur')
   Use In F26lCur
EndIf
If Used('F24lCur')
   Use In F24lCur
EndIf
If Used('F08cCur')
   Use In F08cCur
EndIf
If Used('F10cCur')
   Use In F10cCur
EndIf
If Used('F63cCur')
   Use In F63cCur
EndIf
If Used('CurMAC')
   Use In CurMAC
EndIf

Return
