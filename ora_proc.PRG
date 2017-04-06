*> Cabecera

   *> Descripcio ................. Procesos y funciones varios para VFox - Oracle.
   *> Modul ...................... OraProc.PRG
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 18.09.98
   *> Data de fi .................

*> Notes:
*>     - Numeradores: cambiar F00A por acceso a secuencias de ORACLE. AVC - 02.07.2000

**************************************************************
*     Funciones standard para ProDis / Procaot en Oracle     *
**************************************************************

*> Funciones ............................ Ora_NewPal      Extraer nuevo número palet.
*>                                        Ora_NewMP       Extraer nuevo número MP.
*>                                        Ora_NewHM       Extraer nuevo número HM.
*>                                        Ora_NewLst      Extraer nuevo número lista de trabajo.
*>                                        Ora_NewLCC      Extraer nuevo número lista de carga.
*>                                        Ora_NewEnt      Extraer nuevo número Entrada.
*>                                        Ora_NewInv      Extraer nuevo número Inventario.
*>                                        Ora_NewMac      Extraer nuevo número Mac.
*>                                        Ora_NewHRu      Extraer nuevo número Hoja Ruta.
*>                                        Ora_NewTRa      Extraer nuevo número Traspaso albaranes.
*>                                        Ora_NewTRm      Extraer nuevo número Traspaso mvtos.
*>                                        Ora_NewOCID     Extraer nuevo número de ID ocupaciones.
*>                                        Ora_NewDoc      Extraer nuevo número de documento.fnewdoc

*>                                        Ora_DigCtl      Obtener dígito control ubicación.
*>                                        Ora_AcCbLs      Actualiza campos lista salida (F26c).

**************************************************************
*              Procesos varios de Actualización              *
**************************************************************

*> Módulos........................ Carga_Datos        (Cargar parámetros ubic. en array)
*>                                 EMat_Ubicar        (Ubicar palets en Entrada Material)
*>                                 EMat_AceptarUbic   (Aceptar ubic. en Entrada Material)
*>                                 EMat_CancelarUbic  (Cancela ubic. en Entrada Material)
*>                                 Pale_Ubicar        (Ubicar palets en Paletización)
*>                                 CnfE_Ubicar        (Ubicar palets en Confirmación)
*>                                 Pale_AceptarUbic   (Aceptar ubic. en Paletización)
*>                                 Pale_Acepale       (Aceptar ubic. en Paletización )
*>                                 Pale_CancelarUbic  (Cancela ubic. en Paletización)
*>                                 CnfE_ConfMov       (Confirm. mov. en Conf.Entradas)
*>                                 ConfirmarMP        (Confirmar UN Mp de entrada)
*>                                 CfCa_Chiste        (Chiste rutas en Conf.Carga)
*>                                 CfCa_GrabRut       (Grabar cursor rutas en Conf.Carga)
*>                                 CfCa_GrabDoc       (Grabar cursor docs. en Conf.Carga)
*>                                 CfCa_ActStk        (Actualizar stock, ocupac. y ubic.)
*>                                 CfCa_ActTrn        (Actualizar stock en tránsito)
*>                                 CfCa_GenAlb        (Generar albarán para rutas)
*>                                 MoUb_Ubicar        (Ubicar palets en Movimiento Ubicaciones)
*>                                 MoUb_AceptarUbic   (Aceptar ubic. en Movimiento Ubicaciones)
*>                                 MoUb_CancelarUbic  (Cancela ubic. en Movimiento Ubicaciones)

*>                                 DelNulls     (Eliminar valores NULL)
*>                                 CResF14c     (Cantidad reservada de una ocupación)

*>========================================================
*> Ora_NewPal ............... Extraer el número de palet.
*>========================================================
Function Ora_NewPal
Private f_anterior, c_NPalet

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NPalet = Ora_NewNum('NPAL', 'N', 'N', 10, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NPalet

*>====================================================================
*> Ora_NewMP ................ Extraer el número de movimiento para MP
*>====================================================================
Function Ora_NewMP
Parameters _Commit, _Block
Private f_anterior, c_MovMP

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_MovMP = Ora_NewNum('NMMP', 'N', 'N', 10, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_MovMP

*>====================================================================
*> Ora_NewHM ................ Extraer el número de movimiento para HM
*>====================================================================
Function Ora_NewHM
Parameters _Commit, _Block
Private f_anterior, c_MovHM

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_MovHM = Ora_NewNum('NMHM', 'N', 'N', 10, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_MovHM

*>==================================================================
*> Ora_NewLst ............... Extraer el número de lista de trabajo
*>==================================================================
Function Ora_NewLst
Private f_anterior, c_NumLst

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumLst = Ora_NewNum('NLST', 'N', 'N', 6, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumLst

*>================================================================
*> Ora_NewLCC ............... Extraer el número de lista de carga
*>================================================================
Function Ora_NewLCC
Private f_anterior, c_NumLCC

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumLCC = Ora_NewNum('NLCC', 'N', 'N', 6, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumLCC

*>=========================================================
*> Ora_NewEnt ............... Extraer el número de entrada
*>=========================================================
Function Ora_NewEnt
Private f_anterior, c_NumEnt

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumEnt = Ora_NewNum('NENT', 'N', 'N', 10, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumEnt

*>===========================================================
*> Ora_NewInv ............... Extraer el número de Inventario
*>===========================================================
Function Ora_NewInv
Private f_anterior, c_NumInv

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumInv = Ora_NewNum('NINV', 'N', 'N', 6, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumInv

*>===========================================================
*> Ora_NewMac ............... Extraer el número de Mac
*>===========================================================
Function Ora_NewMac
Parameters _Commit, _Block
Private f_anterior, c_NumMac

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumMac = Ora_NewNum('NMAC', 'N', 'N', 9, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumMac

*>===========================================================
*> Ora_NewHRu ............... Extraer el número de Hoja Ruta
*>===========================================================
Function Ora_NewHRu
Parameters _Commit, _Block
Private f_anterior, c_NumHRu

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumHRu = Ora_NewNum('NHRU', 'N', 'N', 10, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumHRu

*>=========================================================
*> Ora_NewTra ............... Extraer el número de traspaso
*>=========================================================
Function Ora_NewTra
Parameters _Commit, _Block
Private f_anterior, c_NumTra

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumTra = Ora_NewNum('NTRA', 'N', 'N', 6, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumTra

*>====================================================================
*> Ora_NewTRm ................ Extraer el Nº de traspaso movimientos
*>====================================================================
Function Ora_NewTRm
Parameters _Commit, _Block, _Len
Private f_anterior, c_MovTrm

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_MovTrm = Ora_NewNum('NTRM', 'N', 'N', _Len, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_MovTrm

*>====================================================================
*> Ora_NewOCID ................ Obtener ID ocupación.
*>====================================================================
Function Ora_NewOCID
Private f_anterior, c_MovOCID

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_MovOCID = Ora_NewNum('NMOC', 'N', 'N', 10, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_MovOCID

*>====================================================================
*> Ora_NewDoc ................ Obtener nº de documento.
*>====================================================================
Function Ora_NewDoc
Private f_anterior, c_NumDoc

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   c_NumDoc = Ora_NewNum('NDOC', 'N', 'N', 13, 1)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_NumDoc

*>====================================================================
*> Ora_NewNum ................ Extraer una numeración.
*> Permite devolver uno o varios números.
*> Recibe: _CodNum ----> Numeración. Acceso a F00A.
*>         _Commit ----> Hacer commit. Default='N'.
*>         _Block -----> Bloquear. Default='N'.
*>         _Len -------> Longitud a devolver.
*>         _Cuantos ---> Números a devolver. Default=1.
*>====================================================================
Function Ora_NewNum
Parameters _CodNum, _Commit, _Block, _Len, _Cuantos
Private f_anterior, cValMov
Private cEntornL

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Commit') <> 'C'
      _Commit = 'N'
   EndIf

   If Type('_Block') <> 'C'
      _Block = 'N'
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
      
   *> Porsiaca, si no está definida la variable de tipo de entorno.
   If Type('_ENTORNO') # 'C'
      _ENTORNO = Space(1)
   EndIf
   If Type('_VERSION') # 'C'
      _VERSION = Space(1)
   EndIf

   cEntornL = GetEntornID(_ENTORNO, _VERSION)

   *> Obtener numeración, según el entorno de base de datos:
   *>    - Oracle: Utilizar secuencias (F99A).
   *>    - As/400: Utilizar numeraciones (F00A).

   Do Case
      *> Sentencia para entorno ORACLE.
      Case cEntornL=='O'
         cValMov = Ora_NewSequenceOracle(_CodNum, _Len, _Cuantos)

      *> Sentencia para entorno AS/400.
      Case cEntornL=='A'
         cValMov = Ora_NewSequenceDB2(_CodNum, _Len, _Cuantos)

      *> Sentencia para entorno SQLSERVER.
      Case cEntornL=='S'
         cValMov = Ora_NewSequenceSQS(_CodNum, _Len, _Cuantos)

      *> Sentencia para entorno VFP.
      Case cEntornL=='V'
         cValMov = Ora_NewSequenceVFP(_CodNum, _Len, _Cuantos)

      *> Sentencia para entorno ACCESS.
      Case cEntornL=='M'
         cValMov = Ora_NewSequenceMS(_CodNum, _Len, _Cuantos)

      *> Resto de casos: Error.
      Otherwise
         cValMov = Space(1)
         _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
   EndCase

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return cValMov

*>====================================================================
*> Ora_NewSequenceOracle ............ Extraer una numeración.
*> Permite devolver uno o varios números.
*> Versión para ORACLE.
*>
*>   Recibe: _CodNum ----> Código de secuencia.
*>           _Len -------> Longitud a devolver.
*>           _Cuantos ---> Números a devolver. Default=1.
*> Devuelve: c_Sequence -> Numeración solicitada.
*>====================================================================

FUNCTION Ora_NewSequenceOracle
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Comprobar que la secuencia esté creada en F99A.
   m.F99aCodNum = _CodNum
   If !f3_seek('F99A')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F99A' + cr
      =Anomalias()
      Return .Null.
   EndIf

   *> Comprobar que la secuencia esté creada en ORACLE.
   _Selec = "*"
   _Where = "Sequence_Name='" + _CodNum + "'"
   If !f3_sql(_Selec, ;
              'USER_SEQUENCES', ;
              _Where, ;
              '', ;
              '', ;
              'SequenceCur')

      _LxErr = 'No se ha podido encontrar la secuencia '+ _CodNum + cr
      =Anomalias()
      Return .Null.
   EndIf

   Use In SequenceCur

   *> Formar string de conexión con ORACLE.
   _Selec = "LPad(To_Char(" + _CodNum + ".NextVal), " + AllTrim(Str(_Len)) + ", '0') As c_Sequence"
   If !f3_sql(_Selec, ;
              'DUAL ', ;
              '', ;
              '', ;
              '', ;
              'DualCur')

      _LxErr = 'No se ha podido obtener la numeración '+ _CodNum + cr
      =Anomalias()
      Return .Null.
   EndIf

Select DualCur
Go Top
Scatter MemVar
Use In DualCur

If Type('c_Sequence')=='N'
   c_Seq = PadL(AllTrim(Str(c_Sequence, _Len, 0)), _Len, '0')
   c_Sequence = c_Seq
EndIf
   
Return c_Sequence

*>====================================================================
*> Ora_NewSequenceDB2 ............ Extraer una numeración.
*> Permite devolver uno o varios números.
*> Versión para DB2.
*> Agregar control de tiempo de espera. AVC - 25.02.2003
*> Versión con control local de bloqueos. AVC - 21.03.2003
*>====================================================================
*>   Recibe: _CodNum ----> Código de numeración.
*>           _Len -------> Longitud a devolver. Default = 15.
*>           _Cuantos ---> Números a devolver. Default = 1.
*>
*> Devuelve: c_Sequence -> Numeración solicitada.
*>====================================================================
Function Ora_NewSequenceDB2
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where
Local lStado, lLCKIsOpen, cOldSelec
Local nTimeOut, nSecondsForWait

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Guardar entorno anterior.
   cOldSelec = Select()

   *> Establecer el tiempo de espera.
   nTimeOut = Iif(Type('_STANDARDTIMEOUT')=='N', _STANDARDTIMEOUT, 60)
   nSecondsForWait = Seconds()

   *> Comprobar que la numeración esté creada en F00A.
   m.F00aCodNum = _CodNum
   If ! f3_seek('F00A')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()

      *> Restaurar el entorno de trabajo.
      If !Empty(cOldSelec)
         Select (cOldSelec)
      EndIf
      Return .Null.
   EndIf

   *> Control de bloqueo de numeración por otro usuario.
   Do While .T.
      Select F00a
      Go Top

      Do Case
         *> Eureka! La numeración es nuestra.
         Case F00aUsrLck==_UsrCod
            Exit

         *> La numeración está libre: Grabar nuestro código y validar.
         Case Empty(F00aUsrLck)
            Replace F00aUsrLck With _UsrCod
            Scatter MemVar
            =f3_upd('F00A')
            =f3_seek('F00A')

         *> Tiempo de espera sobrepasado.
         Case Seconds() > nSecondsForWait + nTimeOut
            _LxErr = 'Sobrepasado el tiempo máximo de espera para obtener numeración' + cr + ;
                     'Usuario: ' + _UsrCod + ' Numeración: ' + _CodNum + cr
            =Anomalias()

            *> Restaurar el entorno de trabajo.
            If !Empty(cOldSelec)
               Select (cOldSelec)
            EndIf
            Return  .Null.

         *> La numeración es de otro usuario: Reintentar.
         Case F00aUsrLck # _UsrCod
            =f3_seek('F00A')

         *> Resto de casos (¿Puede haberlos?): Reintentar.
         Otherwise
            =f3_seek('F00A')
      EndCase
   EndDo

   *> Actualizar la numeración.
   Select F00a
   c_Sequence = PadL(AllTrim(Str(F00aNumero + 1, _Len, 0)), _Len, '0')

   Replace F00aNumero With F00aNumero + _Cuantos, ;
           F00aUsrLck With Space(6)
   Scatter MemVar

   *> Actualizar la numeración en F00A.
   If !f3_upd('F00A')
      _LxErr = 'No se ha podido actualizar la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()
      c_Sequence = .Null.
   EndIf

   *> Recuperar el entorno anterior.
   If !Empty(cOldSelec)
      Select (cOldSelec)
   EndIf

Return c_Sequence

*> Versión con acceso a DTAARAs de AS400.
Function Ora_NewSequenceDB2AS400
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where
Private oPRC
Local lStado, lLCKIsOpen, cOldSelec
Local nTimeOut, nSecondsForWait

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Guardar entorno anterior.
   cOldSelec = Select()

   *> Abrir, si cal, la tabla auxiliar de numeraciones.
   lLCKIsOpen = Used('F00ALCK')
   If !lLCKIsOpen
      oPRC = CreateObject('procaot')
      =oPRC.OpenTabla('F00ALCK')
   EndIf

   *> Comprobar que la numeración esté creada en F00A.
   m.F00aCodNum = _CodNum
   If !f3_seek('F00A')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()

      *> Restaurar el entorno de trabajo.
      If !lLCKIsOpen
         Use In F00ALCK
         Release oPRC
      EndIf

      If !Empty(cOldSelec)
         Select (cOldSelec)
      EndIf

      Return .Null.
   EndIf

   *> Establecer el tiempo de espera.
   nTimeOut = Iif(Type('_STANDARDTIMEOUT')=='N', _STANDARDTIMEOUT, 60)
   nSecondsForWait = Seconds()

   *> Buscar, si existe, el registro asociado al usuario activo de esta sesión.
   lStado = f3_seek('F00ALCK', '[m.F00aCodNum=_CodNum,m.F00aUsrCod=_UsrCod]')
   If !lStado
      *> No hay registro asociado.
      Select F00ALCK
      Append Blank
      Replace F00aCodNum With _CodNum, ;
              F00aUsrCod With _UsrCod, ;
              F00aValor  With _Cuantos, ;
              F00aIncrem With _Cuantos, ;
              F00aEstado With 'P'

      Scatter MemVar
      =f3_ins('F00ALCK')
   EndIf

   lStado = f3_seek('F00ALCK', '[m.F00aCodNum=_CodNum,m.F00aUsrCod=_UsrCod]')
   Select F00ALCK
   Go Top

   Replace F00aEstado With 'P', ;
           F00aIncrem With _Cuantos

   Do While lStado==.T.
      Do Case
         *> Incidencias en procesos de AS400.
         Case F00aEstado=='D' .Or. F00aEstado=='U' .Or. F00aEstado=='R'
            _LxErr = 'Incidencias de AS400 en proceso numeraciones' + cr + ;
                     'Usuario: ' + _UsrCod + ' Numeración: ' + _CodNum + ' Estado: ' + F00aEstado + cr
            =Anomalias()

            *> Restaurar el entorno de trabajo.
            If !lLCKIsOpen
               Use In F00ALCK
               Release oPRC
            EndIf

            If !Empty(cOldSelec)
               Select (cOldSelec)
            EndIf
            Return  .Null.

         *> Numeración múltiple insuficiente.
         Case F00aEstado=='T' .And. F00aIncrem < _Cuantos
            Replace F00aEstado With 'P', ;
                    F00aIncrem With _Cuantos
            Scatter MemVar
            =f3_upd('F00ALCK')

          *> Numeración OK.
          Case F00aEstado=='T'
             Exit

          *> Tiempo de espera sobrepasado.
          Case Seconds() < nSecondsForWait + nTimeOut
            _LxErr = 'Sobrepasado el tiempo máximo de espera para obtener numeración' + cr + ;
                     'Usuario: ' + _UsrCod + ' Numeración: ' + _CodNum + cr
            =Anomalias()

            *> Restaurar el entorno de trabajo.
            If !lLCKIsOpen
               Use In F00ALCK
               Release oPRC
            EndIf

            If !Empty(cOldSelec)
               Select (cOldSelec)
            EndIf
            Return  .Null.

          *> Poner en estado de petición pendiente.
          Otherwise
            Replace F00aEstado With 'P', ;
                    F00aIncrem With _Cuantos
            Scatter MemVar
            =f3_upd('F00ALCK')
       EndCase

      lStado = f3_seek('F00ALCK', '[m.F00aCodNum=_CodNum,m.F00aUsrCod=_UsrCod]')
      Select F00ALCK
      Go Top
   EndDo

   *> Preparar la numeración a devolver.
   Scatter MemVar
   c_Sequence = PadL(AllTrim(Str(F00aValor, _Len, 0)), _Len, '0')

   *> En el caso de pedir varios números, actualizar contador.
   Replace F00aIncrem With F00aIncrem - 1
   Replace F00aEstado With Iif(F00aIncrem <= 0, 'C', F00aEstado)
   Scatter MemVar
   =f3_upd('F00ALCK')

   *> Cerrar, si cal, tabla auxiliar de numeraciones.
   If !lLCKIsOpen
      Use In F00ALCK
      Release oPRC
   EndIf

   *> Recuperar el entorno anterior.
   If !Empty(cOldSelec)
      Select (cOldSelec)
   EndIf

Return c_Sequence

*> Versión SERVICE PACK de numeraciones de AS400.
FUNCTION Ora_NewSequenceDB2OLD
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where
Local nReintentos, nMaxReintentos

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   If !Used('F00ALCK')
     Use F00ALCK In 0 Again Shared
   EndIf

   *> Nº de reintentos en caso de bloqueo de numeración por otro usuario.
   Store 0 To nReintentos
   Store 999 To nMaxReintentos

   *> Validar que la numeración no esté cogida por otro usuario.
   Select F00ALCK
   Locate For F00aCodNum==_CodNum .And. F00aUsrCod # _UsrCod
   Do While Found()
      nReintentos = nReintentos + 1
      If nReintentos > nMaxReintentos
         *> Supera el máximo de reintentos: Error.
         _LxErr = 'Superado nº de reintentos de la numeración ' + _CodNum + ' en tabla F00A' + cr
         =Anomalias()
         Return .Null.
      Else
         Locate For F00aCodNum==_CodNum .And. F00aUsrCod # _UsrCod
      EndIf
   EndDo

   Locate For F00aCodNum==_CodNum
   If !Found()
      *> No existe la clave: Crear el registro único.
      Append Blank
   EndIf

   *> Bloquear numeración para otros usuarios.
   Replace F00aUsrCod With _UsrCod, ;
           F00aCodNum With _CodNum

   *> Comprobar que la numeración esté creada en F00A.
   m.F00aCodNum = _CodNum
   If !f3_seek('F00A')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()

      *> Elimina los registros de control de bloqueo generados por nosotros.
      Select F00ALCK
      Delete For F00aUsrCod==_UsrCod
      Return .Null.
   EndIf

   *> Actualizar la numeración.
   Select F00a
   Replace F00aNumero With F00aNumero + 1
   Scatter MemVar
   c_Sequence = PadL(AllTrim(Str(F00aNumero, _Len, 0)), _Len, '0')

   *> Actualizar la numeración en F00A.
   If !f3_upd('F00A')
      _LxErr = 'No se ha podido actualizar la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()
      c_Sequence = .Null.
   EndIf

   *> Elimina los registros de control de bloqueo generados por nosotros.
   Select F00ALCK
   Delete For F00aUsrCod==_UsrCod

Return c_Sequence

*>====================================================================
*> Ora_NewSequenceSQS ............ Extraer una numeración.
*> Permite devolver uno o varios números.
*> Versión para SQLSERVER.
*>
*>   Recibe: _CodNum ----> Código de secuencia.
*>           _Len -------> Longitud a devolver.
*>           _Cuantos ---> Números a devolver. Default=1.
*> Devuelve: c_Sequence -> Numeración solicitada.
*>====================================================================

FUNCTION Ora_NewSequenceSQS
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where, _FromF, _Delet, _Inser
Local _xier

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Comprobar que la secuencia esté creada en F99I.
   m.F99iCodNum = _CodNum
   If !f3_seek('F99i')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F99I' + cr
      =Anomalias()
      Return .Null.
   EndIf

   Select F99i
   Go Top
   Scatter Name oF99i

   *> Anular registro con el valor actual de la numeración.
   _Delet = "Delete From"
   _FromF = "F99I" + AllTrim(_CodNum)
   _Selec = "*"
   _Where = "F99iCodNum='" + _CodNum + "' And F99iUsrCod='" + _UsrCod + "'"
   _Selec = _Delet + Space(1) + _FromF + Space(1) + "Where " + _Where

   _xier  = f3_SqlExec(_ASql, _Selec)
   If _xier <= 0
      _LxErr = 'Error eliminando valor previo de la numeración '+ _CodNum + cr + ;
               'MENSAJE: ' + Message() + cr + ;
               'SENTENCIA: ' + _Selec + cr
      =Anomalias()
      Return .Null.
   EndIf

   *> Formar string de actualización de la numeración.
   _Inser = "Insert Into"
   _Selec = _Inser + Space(1) + _FromF
   _Selec = _Selec + Space(1) + "(F99iCodNum, F99iDescri, F99iUsrCod)"
   _Selec = _Selec + Space(1) + "Values('" + _CodNum + "',"
   _Selec = _Selec + Space(1) + "'" + oF99i.F99iDescri + "'," + Space(1) + "'" + _UsrCod + "')"

   _xier  = f3_SqlExec(_ASql, _Selec)
   If _xier <= 0
      _LxErr = 'Error actualizando valor de la numeración '+ _CodNum + cr + ;
               'MENSAJE: ' + Message() + cr + ;
               'SENTENCIA: ' + _Selec + cr
      =Anomalias()
      Return .Null.
   EndIf

   *> Obtener y devolver el valor de la numeración.
   If !f3_sql("*", _FromF, _Where, , , _FromF)
      _LxErr = 'Error obteniendo valor actual de la numeración '+ _CodNum + cr
      =Anomalias()
      Return .Null.
   EndIf

   Select (_FromF)
   Go Top
   c_Sequence = PadL(AllTrim(Str(F99iNumero, _Len, 0)), _Len, '0')

   *> Actualizar numeración en F99I.
   Select F99I
   Go Top
   Replace F99iCurVal With Val(c_Sequence)
   Scatter MemVar

   *> Actualizar la numeración en F99I.
   If !f3_upd('F99I')
      _LxErr = 'No se ha podido actualizar la numeración ' + _CodNum + ' en tabla F99I' + cr
      =Anomalias()
   EndIf

Return c_Sequence

*>====================================================================
*> Ora_NewSequenceVFP ............ Extraer una numeración.
*> Permite devolver uno o varios números.
*> Versión para VFP.
*>
*>   Recibe: _CodNum ----> Código de secuencia.
*>           _Len -------> Longitud a devolver.
*>           _Cuantos ---> Números a devolver. Default=1.
*> Devuelve: c_Sequence -> Numeración solicitada.
*>====================================================================

FUNCTION Ora_NewSequenceVFP
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Comprobar que la numeración esté creada en F00A.
   m.F00aCodNum = _CodNum
   If !f3_seek('F00A')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()
      Return .Null.
   EndIf

   *> Formar la cadena con el valor.
   Select F00A
   Go Top
   Replace F00aNumero With F00aNumero + 1
   Scatter MemVar

   c_Sequence = PadL(AllTrim(Str(F00aNumero, _Len, 0)), _Len, '0')

   *> Actualizar la numeración en F00A.
   If !f3_upd('F00A')
      _LxErr = 'No se ha podido actualizar la numeración ' + _CodNum + ' en tabla F00A' + cr
      =Anomalias()
      Return .Null.
   EndIf

Return c_Sequence

*>====================================================================
*> Ora_NewSequenceMS ............ Extraer una numeración.
*> Permite devolver uno o varios números.
*> Versión para MS ACCESS.
*>
*>   Recibe: _CodNum ----> Código de secuencia.
*>           _Len -------> Longitud a devolver.
*>           _Cuantos ---> Números a devolver. Default=1.
*> Devuelve: c_Sequence -> Numeración solicitada.
*>====================================================================

FUNCTION Ora_NewSequenceMS
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence
Private _Selec, _Where, _FromF, _Delet, _Inser
Local _xier

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Comprobar que la secuencia esté creada en F99I.
   m.F99iCodNum = _CodNum
   If !f3_seek('F99i')
      _LxErr = 'No existe la numeración ' + _CodNum + ' en tabla F99I' + cr
      =Anomalias()
      Return .Null.
   EndIf

   Select F99i
   Go Top
   Scatter Name oF99i

   *> Anular registro con el valor actual de la numeración.
   _Delet = "Delete From"
   _FromF = "F99I" + AllTrim(_CodNum)
   _Selec = "*"
   _Where = "F99iCodNum='" + _CodNum + "' And F99iUsrCod='" + _UsrCod + "'"
   _Selec = _Delet + Space(1) + _FromF + Space(1) + "Where " + _Where

   _xier  = f3_SqlExec(_ASql, _Selec)
   If _xier <= 0
      _LxErr = 'Error eliminando valor previo de la numeración '+ _CodNum + cr + ;
               'MENSAJE: ' + Message() + cr + ;
               'SENTENCIA: ' + _Selec + cr
      =Anomalias()
      Return .Null.
   EndIf

   *> Formar string de actualización de la numeración.
   _Inser = "Insert Into"
   _Selec = _Inser + Space(1) + _FromF
   _Selec = _Selec + Space(1) + "(F99iCodNum, F99iDescri, F99iUsrCod)"
   _Selec = _Selec + Space(1) + "Values('" + _CodNum + "',"
   _Selec = _Selec + Space(1) + "'" + oF99i.F99iDescri + "'," + Space(1) + "'" + _UsrCod + "')"

   _xier  = f3_SqlExec(_ASql, _Selec)
   If _xier <= 0
      _LxErr = 'Error actualizando valor de la numeración '+ _CodNum + cr + ;
               'MENSAJE: ' + Message() + cr + ;
               'SENTENCIA: ' + _Selec + cr
      =Anomalias()
      Return .Null.
   EndIf

   *> Obtener y devolver el valor de la numeración.
   If !f3_sql("*", _FromF, _Where, , , _FromF)
      _LxErr = 'Error obteniendo valor actual de la numeración '+ _CodNum + cr
      =Anomalias()
      Return .Null.
   EndIf

   Select (_FromF)
   Go Top
   c_Sequence = PadL(AllTrim(Str(F99iNumero, _Len, 0)), _Len, '0')

   *> Actualizar numeración en F99I.
   Select F99I
   Go Top
   Replace F99iCurVal With Val(c_Sequence)
   Scatter MemVar

   *> Actualizar la numeración en F99I.
   If !f3_upd('F99I')
      _LxErr = 'No se ha podido actualizar la numeración ' + _CodNum + ' en tabla F99I' + cr
      =Anomalias()
   EndIf

Return c_Sequence

*>==========================================================================
*> Ora_CurSequence ............ Devolver el valor actual de una numeración.
*> Permite devolver uno o varios números.
*> Versión para ORACLE.
*>
*> Recibe: _CodNum ----> Código de secuencia.
*>         _Len -------> Longitud a devolver.
*>         _Cuantos ---> Números a devlver. Default=1.
*>==========================================================================

Function Ora_CurSequenceOracle
Parameters _CodNum, _Len, _Cuantos
Private Sw, f_anterior, c_Sequence, _Selec

   If Type('_CodNum') <> 'C'
      Return .Null.
   EndIf

   If Type('_Len') <> 'N'
      _Len = 10
   EndIf

   If Type('_Cuantos') <> 'N'
      _Cuantos = 1
   EndIf

   *> Formar string de conexión con ORACLE.
   _Selec = "LPad(To_Char(" + _CodNum + ".CurrVal), " + Str(_Len) + ", '0') As c_Sequence"
   If !f3_sql(_Selec, ;
              'DUAL ', ;
              '', ;
              '', ;
              '', ;
              'DualCur')

      _LxErr = 'No se ha podido leer el valor actual de la numeración '+ _CodNum + cr
      =Anomalias()
      Return .Null.
   EndIf

Select DualCur
Return c_Sequence

****************************************************************************************
*	BUSCA UN IDENTIFICADOR EN LA TABLA DE NUMERACIONES
****************************************************************************************	       
*          Devuelve una cadena de 10 posiciones con el número.
*          Se le pasan como parámetros Propietario y Sí o No para actualizar; así como
*		   el código del numerador a obtener.
*          Su uso es:
*              Primero buscar en f34r al propietario sino lo encuentra buscar en f00a
*              El parámetro T/F determina si se debe sumar uno al código anterior


Function Obtener_Num
Parameters Actualizar, Prop, CodNumerador
	
    l_open34r = Used('F34r')
    l_open00a = Used('F00a')
	* Buscar en F34r
	_ok=.F.
	If !Empty(Prop)
		Lx_Select = "Select F34rNumero From F34r" + _em + " Where f34rCodPro='"+Prop+"' AND "+;
	            "F34rCodCon = '" + CodNumerador + _cm  
    	Err = f3_SqlExec(_ASql, Lx_Select,'F34r') 	     	 
    	= SqlMoreResults(_ASql) 	 
    	Sele F34r
    	if !eof()
    		_ok=.T.
    	endif
    EndIf

	* Si encontrado entonces
	 If _ok=.T.
	    * Si Actualizar entoces
	      If Actualizar 
	        * Guarda en tabla
             Replace F34rNumero With Val(PadL(Str(F34rNumero),10,'0')) + 1
             Where =  "f34rCodPro = '" + Prop + _Cm
             =F3_UpdTun("F34r",,"F34rNumero",,"F34r",Where)
          EndIf
          Select f70c
          =TableUpdate(.T.)
          Select f34r
          Num = F34rNumero
          If !l_open34r
             use in F34r
          EndIf
 
          *> Devuelve el valor
	      if actualizar
	      	Return PadL(AllTrim(Str(Num)),10,'0') 
	      else
 			Return PadL(AllTrim(Str(Num + 1)),10,'0') 
 		  endif
 		  
	* Si no encontrado entonces
	 Else 
	    * Coge el número actual de F00a para CodNum = CodNumerador
	     Lx_Select = "Select F00aNumero From F00a" + _em + " Where f00aCodNum='" + CodNumerador + _cm    
         Err = f3_SqlExec(_aSql, Lx_Select,'F00a')     	     	 
         = SqlMoreResults(_aSql) 	 
         Sele F00a         
	     
	       * Si acutalizar entonces
	        If Actualizar 
	           * Guarda en tabla 
               Replace F00aNumero With Val(Padl(Str(F00aNumero),10,'0')) + 1
               Where =  "f00aCodNum = '" + CodNumerador + _cm    	      	 
               =F3_UpdTun("F00a",,"F00aNumero",,"F00a",Where)
               =TableUpdate(.T.)
            EndIf
            Num = F00aNumero
            If !l_open00a
               use in F00a
            EndIf
   	    	if actualizar
	      	  Return PadL(AllTrim(Str(Num)),10,'0') 
	        else
 			  Return PadL(AllTrim(Str(Num + 1)),10,'0') 
 		    endif
    EndIf
Return

FUNCTION Ora_DigCtl
Parameters c_CodUbi
Private Alm, Zona, Calle, Calle1, Calle2, Fila, Fila1, Fila2, Piso, Prof, TAlm, TZona, TCalle, ;
        TFila, Total, x
Private f_anterior, txtSQL, Sw, ErrSQL, c_DigCtl

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
   
   xCalle  = SubStr(c_CodUbi, 7, 2)
   xFila   = SubStr(c_CodUbi, 9, 3)
   xPiso   = SubStr(c_CodUbi,12, 2)
   xProf   = SubStr(c_CodUbi,14, 1)

   C_Cadena = xCalle + xFila + xPiso + xProf
   Store 0 to Suma
   For Hh = 1 To len(C_Cadena)
	   *> Cogemos el caracter ASCII	   .------------------------------
	   C_CharAsc = Asc(Substr(C_Cadena,Hh,1))
	   C_CharAsc = C_CharAsc * Hh
	   Suma = Suma + C_CharAsc
   Next Hh

   x = Ceiling(Mod(Suma, 25))

*!*	   If x > 25
*!*	      x = Mod(x, 25)
*!*	   EndIf
*!*	      
*!*	   If x <= 0
*!*	      x = 25
*!*	   EndIf
      
   c_DigCtl = Chr(x + 65)

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return c_DigCtl

*>================================================================
*> Ora_DigArt ............... Obtener ????????????????????????
*>================================================================
FUNCTION Ora_DigArt
Parameters c_CodArt
Private Alm, Zona, Calle, Calle1, Calle2, Fila, Fila1, Fila2, Piso, Prof, TAlm, TZona, TCalle, ;
        TFila, Total, x
Private f_anterior, txtSQL, Sw, ErrSQL, c_DigCtl

   *> Guardamos el fichero anterior.----------------------------------
   f_anterior = Select()
   
   C_Cadena = c_CodArt
   n_Longitud = Len(C_Cadena)
   Store 0 to P,I
   For Hh = n_longitud To 1 Step -1

	   Num = Val(Substr(c_Cadena,Hh,1))
	   *> Cogemos el caracter ASCII	   .------------------------------
       If Mod(Hh,2) = 0
       	  P = P + Num
       Else
    	  I = I + Num
       EndIf
   Next Hh

   dc = I+(P*3)
   If dc > 100 
   	  dc = dc-100
   EndIf

   dc = Ceiling(mod(dc,10))  
   dc = 10 - dc
   dc = Iif(dc = 10,0,dc) 

   *> Seleccionamos el fichero anterior.------------------------------
   If !Empty(f_Anterior)
      Select (f_anterior)
   EndIf

Return dc

*>================================================================
*> Ora_AcCbLs ...............Actualiza campos lista salida (f26c).
*> Optimizar cálculo totales. AVC - 22.04.1999
*>================================================================

FUNCTION Ora_AcCbLs
Parameters NLista, _Commit
Private b_Ok
b_Ok = .T.

*> Buscar la cabecera de la lista. Si no existe, salir.
If ActCL_BusLst(NLista) < 0
   b_Ok = .F.
   Return b_Ok
EndIf

*> Fecha 1er y ult mov. lista, Nº movs. , PesTot y VolTot de lo asignado y pendiente.
*> Si resultado<0, lista sin líneas de detalle: Borrar cabecera y salir.
If ActCL_PUMovPesVol(NLista) < 0
   m.F26cNumLst = NLista
   =f3_baja('F26C')
   b_Ok = .T.
   Return b_Ok
EndIf

*> Calcular MACs asignados, pendientes y estado lista
=ActCL_NMacL(NLista)
=ActCL_EstLst(NLista)

Return b_Ok

*> Calcular acumulados de la lista actual, peso, volumen, mvtos, ...
*> Debe haberse leído la lista a actualizar ('F26c') para realizar los updates.
FUNCTION ActCL_PUMovPesVol
Parameters p_Lista
Private f_select, f_from, f_where
Local lStado

If Used('UPDCAB')
   Use In UPDCAB
EndIf

*> Calcular acumulados de lo asignado.
f_Select = "Min(F26lFecMov) As pfecmov, " + ;
           "Max(F26lFecMov) As ufecmov, " + ;
           _GCN("Count(*)") + " As contmov, " + ;
           _GCN("Sum(F26lCanfis*F08cPesUni/1000)") + " As totpes, " + ;
           _GCN("Sum(F26lCanfis*F08cVolUni/1000)") + " As totvol"

f_From   = "F26l,F08c"

f_where  = "F26lNumLst='" + p_Lista + "' And " + ;
           "F08cCodPro=F26lCodPro And F08cCodArt=F26lCodArt " + ;
           "Having Count(*) > 0"
lStado = f3_sql(f_Select, f_From, f_Where, , , 'UPDCAB')

If !lStado
   *> Lista sin líneas de detalle.
   Return -1
EndIf

*> Actualización de acumulados asignados de la lista.
Select UPDCAB
Go Top

Select F26c
Go Top
Replace F26cFPrMov With IIf(IsNull(UPDCAB.PFecMov), Date(), UPDCAB.PFecMov), ;
        F26cFUlMov With IIf(IsNull(UPDCAB.UFecMov), Date(), UPDCAB.UFecMov), ;
        F26cMovAsg With IIf(IsNull(UPDCAB.ContMov), 0, UPDCAB.ContMov), ;
        F26cKgsAsg With IIf(IsNull(UPDCAB.TotPes), 0, UPDCAB.TotPes), ;
        F26cVolAsg With IIf(IsNull(UPDCAB.TotVol), 0, UPDCAB.TotVol)

f_where = "F26cNumLst='" + p_Lista + "'"
lEstado = f3_UpdTun('F26c', , , , , f_Where, 'N')
If !lStado
   Return -1
EndIf

Use In UPDCAB

*> Calcular acumulados de lo pendiente.
f_select = _GCN("Count(f26lnummov)") + " As ContMov, " + ;
           _GCN("Sum(f26lcanfis*f08cpesuni/1000)") + " As TotPes, " + ;
           _GCN("Sum(f26lcanfis*f08cvoluni/1000)") + " As TotVol"

f_where  = "F26lNumLst='" + p_Lista + "' And " + ;
           "F08cCodPro=F26lCodPro And F08cCodArt=F26lCodArt And " + ;
           "F26lEstMov='0'"

lStado = f3_sql(f_Select, f_From, f_Where, , , 'UPDCAB')

If !lStado
   *> Lista sin líneas de detalle.
   Return -1
EndIf

*> Actualización de acumulados pendientes de la lista.
Select UPDCAB
Go Top

Select F26c
Go Top
Replace F26cMovPte With IIf(IsNull(UPDCAB.ContMov), 0, UPDCAB.ContMov), ;
        F26cKgsPte With IIf(IsNull(UPDCAB.TotPes), 0, UPDCAB.TotPes), ;
        F26cVolPte With IIf(IsNull(UPDCAB.TotVol), 0, UPDCAB.TotVol)

f_where = "F26cNumLst='" + p_Lista + "'"
lEstado = f3_UpdTun('F26c', , , , , f_Where, 'N')
If !lStado
   Return -1
EndIf

Use In UPDCAB

Return 0

*> --------------------------------------------------------------------

FUNCTION ActCL_BusLst
parameters p_Lista
Private f_select,f_from,f_where,lx_sql 
Private Err
Local lStado

*> Si no existe lista, salir.------------------------------------------
m.F26cNumLst = p_Lista
lStado = f3_seek('F26c')
Err = Iif(lStado, 0, -1)

Return Err 

*> -----------------------------------------------------------------

*> Macs asignados en Lista.------------------------------------------
*> Actualiza sobre cabecera lista ('F26c').
FUNCTION ActCL_NMacL
Parameters P_Lista
Private f_select, f_from, f_where
Local lStado

If Used('UPDCAB')
   Use In UPDCAB
EndIf

*> Número total de MACs de la lista.
f_select = _GCN("Count(*)") + " As NumMac"
f_from   = "F26v"
f_where  = "F26vNumLis='" + p_Lista + "'"

lStado = f3_sql(f_select, f_from, f_where, , , 'UPDCAB')
If !lStado
   Return 0
EndIf

Select UPDCAB
Go Top

*> Actualización Macs asignados en la lista.
Select F26c
Go Top
Replace F26cMacAsg With UPDCAB.NumMac

Use In UPDCAB

*> Número de MACs pendientes de la lista.
f_select = _GCN("Count(*)") + " As NumMac"
f_from   = "F26v"
f_where  = "F26vNumLis='" + p_Lista + "' And F26vEstBul<'3'"

lStado = f3_sql(f_select, f_from, f_where, , , 'UPDCAB')
If !lStado
   Return 0
EndIf

Select UPDCAB
Go Top

*> Actualización Macs pendientes en la lista.
Select F26c
Go Top
Replace F26cMacPte With UPDCAB.NumMac

*> Actualizar la cabecera de la lista.
f_where = "F26cNumLst='" + p_Lista + "'"
lEstado = f3_UpdTun('F26c', , , , , f_Where, 'N')

Use In UPDCAB

Return 0

***> Mirar si todos Estados de movs. iguales (count(x)=1) o diferentes(count(x)>1) 
FUNCTION ActCL_EstLst
Parameters P_Lista
Private f_select, f_from, f_where
Private cCampos, cValores, cEstado
Local lStado, nMovs, nMovs0

f_select = '*'
f_from   = "F26l"
f_where  = "F26lNumLst='" + P_Lista + _cm

If Used('UPDCAB')
   Use In UPDCAB
EndIf

lStado = f3_sql(f_select, f_from, f_where, , , 'UPDCAB')
If !lStado
   Return 0
EndIf

Select UPDCAB
Count To nMovs
Count For F26lEstMov=='0' To nMovs0

cEstado = Iif(nMovs==nMovs0, '0', Iif(nMovs0==0, '3', '1'))

*> Actualización Macs pendientes en la lista.

*> Actualizar la cabecera de la lista.
f_where = "F26cNumLst='" + p_Lista + "'"
cCampos  = "F26cEstLst"
cValores = "cEstado"
=f3_UpdTun('F26c', , cCampos, cValores, , f_where, 'N', 'N')

Use In UPDCAB
Return 0

*> MODULOS.-----------------------------------------------------------

*>====================================================================
*> Carga_Datos .............. Cargar parámetros de ubicación en array
*>====================================================================

Procedure Carga_Datos
Parameters CodPro, CodArt
Private txtSQL, lxWhere
*> Carga los datos del fichero F15 en el array.

   If Type('CodPro') <> 'C'
      CodPro = ''
   EndIf
   If Type('CodArt') <> 'C'
      CodArt = ''
   EndIf

   *> Buscar datos de artículo para acabar de cargar datos array.
   =CrtCursor('F08c', 'ActzF08c')
   lxWhere = "F08cCodPro='" + CodPro + "' And F08cCodArt='" + CodArt + "'"
   =F3_SQL('*', 'F08c', lxWhere, , , 'ActzF08c')

   Select ActzF08c
   aubparmz[13] = F08cCaduca            && control de caducidad
   aubparmz[14] = F08cNumDia            && dias control
   aubparmz[15] = F08cCalida            && control de calidad
   aubparmz[16] = F08cMulPro            && ubicación mixta
   aubparmz[17] = F08cMulLot            && multilote

   *> Parámetros ubicación
   =CrtCursor('F15c', 'ActzF15c')
   lxWhere = "F15cCodPro='" + CodPro + "' And F15cCodArt='" + CodArt + "'"
   =F3_SQL('*', 'F15c', lxWhere, , , 'ActzF15c')
   
   Select ActzF15c
   Go Top
   If EOF()
      CodArt = Space(13)
      lxWhere = "F15cCodPro='" + CodPro + "' And F15cCodArt='" + CodArt + "'"
      =f3_sql('*', 'F15c', lxWhere, , , 'ActzF15c')
   EndIf
      
   Select ActzF15c
   Go Top
   If EOF()
      CodPro = Space(6)
      CodArt = Space(13)
      lxWhere = "F15cCodPro='" + CodPro + "' And F15cCodArt='" + CodArt + "'"
      =f3_sql('*', 'F15c', lxWhere, , , 'ActzF15c')
   EndIf
      
   *> Inicializar la primera parte del array de parámetros de ubicación 
   *> segun los datos del archivo f15c.--------------------------------

   Select ActzF15c
   aubparmz[01] = F15cCodArt            && codigo (no utilizado)
   aubparmz[02] = F15cTipUbi            && tipo de ubicación
   aubparmz[03] = F15cPickin            && ubicar en picking
   aubparmz[04] = F15cCriUbi            && criterio de ubicación
   aubparmz[05] = F15cCoeRot            && coeficiente de rotación
   aubparmz[06] = F15cForTam            && forzar tama¤o
   aubparmz[07] = F15cForZon            && forzar zona
   aubparmz[08] = F15cForUbi            && forzar ubicación
   aubparmz[09] = F15cConAut            && confirmación automática
   aubparmz[10] = F15cDesAut            && desglose automatico
   aubparmz[11] = F15cEtiBul            && imprimir etiqueta bulto
   aubparmz[12] = F15cEtiPal            && imprimir etiqueta palet
   
   *> Cerrar cursores creados
   Use In ActzF08c
   Use In ActzF15c
RETURN


*>=============================================================
*> CnfE_ConfMov ............. Confirmar movimientos de Entrada
*>=============================================================

Procedure CnfE_ConfMovOLD
Private n_Status, Fnc, Prm, b_ErrLin, b_ErrEnt

*> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   *> Crea los movimientos.-------------------------------------------
   b_ErrEnt = .F.
   =CrtCursor('F14c', 'CnfEF14c')
   Select CNFENT
   Go Top
   Do While !Eof() .And. b_ErrEnt = .F.
      If Empty(Sel) .Or. Empty(CodUbi)
         Skip
         Loop
      EndIf
      b_ErrLin = .F.

      =WaitWindow('Actualizando movimiento: ' + CNFENT.CodArt + Space(1) + CNFENT.CodUbi)

      *> Comprobar que el movimiento existe y no está bloqueado.----
      f_Where = "F14cNumMov = '" + CnfEnt.NumMov + "'"
      err = F3_Sql('*', 'F14c', f_Where, , , 'CnfEF14c')
      If !err
         _LxErr = 'Error generando cursor de Movimientos Pendientes' + cr + ;
                  'MENSAJE: ' + Message() + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf
      
      *> Obtener volumen según tamaño palet.--------------------------
      Store 0 To n_UniVen, n_UniPac, n_PacCaj, n_CajPal, n_VolPal
      c_TipPal = ''
      m.F08cCodPro = CnfEF14c.F14cCodPro
      m.F08cCodArt = CnfEF14c.F14cCodArt
      Sw = F3_Seek('F08c', , , 'n_UniVen = m.F08cUniVen, n_UniPac = m.F08cUniPac, ' + ;
               'n_PacCaj = m.F08cPacCaj, n_CajPal = m.F08cCajPal, c_TipPal = m.F08cTipPal')
      If !Sw
         _LxErr = 'Error leyendo datos de la Ficha de Artículos' + cr + ;
                  'MENSAJE: ' + Message() + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      n_UniVen = IIf(n_UniVen <= 0, 1, n_UniVen)
      n_UniPac = IIf(n_UniPac <= 0, 1, n_UniPac)
      n_PacCaj = IIf(n_PacCaj <= 0, 1, n_PacCaj)
      n_CajPal = IIf(n_CajPal <= 0, 1, n_CajPal)

      Select CnfEF14c
      Replace F14cUniVen With IIf(F14cUniVen <= 0, n_UniVen, F14cUniVen), ;
              F14cUniPac With IIf(F14cUniPac <= 0, n_UniPac, F14cUniPac), ;
              F14cPacCaj With IIf(F14cPacCaj <= 0, n_PacCaj, F14cPacCaj), ;
              F14cCajPal With IIf(F14cCajPal <= 0, n_CajPal, F14cCajPal)
      If Empty(F14cTipPal)
         Replace F14cTipPal With c_TipPal
      EndIf

      Store 0 to n_PesOcu, n_VolOcu
      Do PesVolAr In Ora_Ca00 With CnfEF14c.F14cCodPro, ;
                                   CnfEF14c.F14cCodArt, ;
                                   CnfEF14c.F14cCanFis, ;
                                   n_PesOcu, ;
                                   n_VolOcu
      
      *> Decrementar stock 5000 (pendiente ubicar).-------------------
      Fnc.ObjParm.Inicializar
      With Fnc.ObjParm
         .PTAcc = '08'
         .PSCAlm = F14cCodAlm
         .PSCPro = F14cCodPro
         .PSCArt = F14cCodArt
         .PStOld = "5000"
         .PSCFis = F14cCanFis
         .PUFlag = 'N'
         .POFlag = 'N'
         .PSFlag = 'S'
      EndWith

      Fnc.Ejecutar
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         Select CnfEF14c
         _LxErr = 'Error actualizando stock pendiente de ubicar.' + cr + ;
                  'DOCUMENTO: ' + F14cTipDoc + '-' + F14cNumDoc + cr + ;
                  ' ARTICULO: ' + F14cCodArt + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Actualizar situación stock y ocupaciones.--------------------
      Select CnfEF14c
      Fnc.ObjParm.Inicializar
      With Fnc.ObjParm
      	 .PTAcc = '07'			 && Entrada de material

         .PUbOld = F14cUbiOri
         .PUFlag = "N"           && Ubicación se actualiza al buscarla

         .POTDoc = F14cTipDoc
         .PONDoc = F14cNumDoc
         .POLDoc = F14cLinDoc
         .PONPed = F14cNumPed
         .POLPed = F14cLinPed
         .POFMov = Date()
         .POCPro = F14cCodPro
         .POCArt = F14cCodArt
         .PONLot = F14cNumLot
         .POSStk = F14cSitStk
         .POFCad = F14cFecCad
         .POCFis = F14cCanFis
         .POCAlm = F14cCodAlm
         .POCUbi = F14cUbiOri
         .PONPal = F14cNumPal
         .POTPal = F14cTipPal
         .POFUni = F14cUniVen
         .POFSer = F14cUniPac
         .POFEnv = F14cPacCaj 
         .POFPal = F14cCajPal
         .POFFab = F14cFecFab
         .PONEnt = F14cNumEnt
         .POFEnt = m.Fecha
         .POFCal = F14cFecCal
         .PONAna = F14cNumAna
         .POPico = CNFENT.PicoSn
         .POFlag = "S"

         .PSCAlm = F14cCodAlm
         .PSCPro = F14cCodPro
         .PSCArt = F14cCodArt
         .PStOld = F14cSitStk
         .PSCFis = F14cCanFis
         .PSFlag = 'S'

         .PMNMov = Space(10)
         .PMEnSa = "E"
         .PMTMov = F14cTipMov
         .PMFDoc = F14cFecDoc
         .PMDAso = F14cDirAso
         .PMFMov = Date()
         .PMRHab = F14cRutHab
         .PMCTra = CNFENT.CodTra
         .PMCOpe = F14cCodOpe
         .PMNLst = F14cNumLst
         .PMORes = F14cOriRes
         .PMTUbi = F14cTipUbi
         .PMTMAs = F14cTipMAs
         .PMNMAs = F14cNumMAs
         .PMMUni = F14cMacUni
         .PMORut = F14cOrdRut
         .PMTERe = F14cTEntRe
         .PMCERe = F14cCEntRe
         .PMVHab = F14cVenHab
         .PMTMac = F14cTipMac
         .PMNMac = F14cNumMac
         .PMNExp = F14cNumExp
         .PMFgMP = "N"

         .PMFgHM = "S"
      EndWith

      *> Realizar entrada actualizando ocupación, stock y ubicación.
      Fnc.Ejecutar
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         Select CnfEF14c
         _LxErr = 'Error realizando la ubicación de material' + cr + ;
                  'DOCUMENTO: ' + F14cTipDoc + '-' + F14cNumDoc + cr + ;
                  ' ARTICULO: ' + F14cCodArt + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Borrar Movimiento Pendiente.------------------------------
	  Fnc.PMNMov = CnfEnt.NumMov
      Fnc.DelMP
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         Select CnfEF14c
         _LxErr = 'Error borrando Movimiento Pendiente' + cr + ;
                  'DOCUMENTO: ' + F14cTipDoc + '-' + F14cNumDoc + cr + ;
                  ' ARTICULO: ' + F14cCodArt + cr
         =Anomalias()
      EndIf
 
      *> Actualizar Albarán de Entrada, si lo hay.----------------------
      If !Empty(Fnc.ObjParm.PONEnt)
         *> Actualizar línea detalle del albarán entrada.----------------
         strSql = "Update F18n" + _em + " Set F18nEstado='3'" + ;
                  " Where F18nNumEnt='" + Fnc.ObjParm.PONEnt + "'" + ;
                  " And   F18nCodPro='" + Fnc.ObjParm.POCPro + "'" + ;
                  " And   F18nTipDoc='" + Fnc.ObjParm.POTDoc + "'" + ;
                  " And   F18nNumDoc='" + Fnc.ObjParm.PONDoc + "'" + ;
                  " And   F18nCodArt='" + Fnc.ObjParm.POCArt + "'" + ;
                  " And   F18nSitStk='" + Fnc.ObjParm.POSStk + "'" + ;
                  " And   F18nLinDoc='" + PadL(Fnc.ObjParm.PolDoc, 4, '0') + _cm 

         _xier = f3_SqlExec(_aSql, strSql)
         If _xier <= 0
            _LxErr = 'Error actualizando detalle Albarán de Entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf

         *> Ver si quedan líneas de detalle pendientes de este albarán.
         strSql = "Select " + GetCvtNvl(_ENTORNO, _VERSION, "Count(*)") + " As FLinAlb From F18n"  + _em + ;
                  " Where F18nNumEnt='" + Fnc.ObjParm.PONEnt + "'" + ;
                  " And   F18nEstado<'3'"
 
         _xier = f3_SqlExec(_aSql, strSql)
         If _xier <= 0
            _LxErr = 'Error en control líneas pendientes Albarán de Entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf

         *> Actualizar CABECERA Albarán de Entrada.
         =SqlMoreResults(_ASql)
         strSql = "Update F18m"  + _em + ;
                  " Set F18mEstado='" + Iif(FLinAlb > 0, '1', '3') + "'" + ;
                  " Where F18mNumEnt='" + Fnc.ObjParm.PONEnt + "'"
 
         _xier = f3_SqlExec(_aSql, strSql)
         If _xier <= 0
            _LxErr = 'Error actualizando cabecera Albarán de Entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf

      *> Seleccionamos y avanzamos el fichero de líneas.-------------
      Select CNFENT
      Skip
   EndDo

   Wait Clear

   *> Actualizar los movimientos confirmados.-------------------------
   =SqlCommit(_aSql)
   Use In CnfEF14c
  
   *> Elimina objetos función actualización.--------------------------
   Release Prm
   Release Fnc
Return

*>=========================================================================
*> EMat_Ubicar .............. Forzar o buscar ubicación en entrada material
*>
*> Recibe       : ENTMAT -----> Cursor con los palets/picos a ubicar.
*>
*> Llamado desde: ENTREMAT ---> Entrada de material.
*>                ENTREPAL ---> Entrada de palet. (No implementado).
*>                ENTRDIRE ---> Entrada rápida. (No implementado).
*>=========================================================================

Procedure EMat_Ubicar
Private n_Cantid, c_TamUbi, c_CodUbi, c_UbiFor, c_NumPal, c_CodArt, c_PicoSN, n_RegAnt

*> Proporciona la ubicación a cada una de las líneas.

   *> Declara el array de parámetros de ubicación.--------------------
   Declare aubparmz[17]
   Select ENTMAT
   Go Top
  
  
   Do Carga_Datos With CodPro, CodArt    && Cargar parámetros ubicación.

   *> Procedemos.-----------------------------------------------------
   Select ENTMAT
   Go Top
   Do While !EOF()
      If EntMat.UbicSN = "S"
         Skip
         Loop
      EndIf
       
      *> Guardamos datos.---------------------------------------------
      c_CodPro = ENTMAT.CodPro
      c_CodArt = ENTMAT.CodArt
      c_TamUbi = ENTMAT.TamAbi
      n_Cantid = ENTMAT.CanFis
      c_UbiFor = IIf(Empty(ENTMAT.UbiFor), '', ENTMAT.UbiFor)
      c_NumPal = ENTMAT.NumPal
      c_PicoSN = ENTMAT.PicoSn
      b_LAutom = .F.
      b_BlqUbi = .T.         && Si bloquea la ubicación o no
      c_CodUbi = ""          && Retorno de la ubicación destino
      c_TxtErr = ""  
      f_fecha  = .F.        && Texto de porque no se puede ubicar
      c_NumLot = ENTMAT.NumLot
      

      n_RegAnt = RecNo()      && Guardar puntero en Normal (lo pierde).

      c_PicoSN = ENTMAT.PicoSn
*      Do Prc_Ubi In Ora_Cu00 With c_CodPro, ;
*                                  c_CodArt, ;
*                                  c_TamUbi, ;
*                                  n_Cantid, ;
*                                  c_UbiFor, ;
*                                  c_NumPal, ;
*                                  c_PicoSN, ;
*                                  b_LAutom, ;
*                                  b_BlqUbi, ;
*                                  c_CodUbi, ;
*                                  c_TxtErr, ; 
*                                  f_Fecha, ;
*                                  c_NumLot
                                  
                                  
   Do Prc_Ubi In Ora_Cu00 With c_CodPro, ;
                                  c_CodArt, ;
                                  c_TamUbi, ;
                                  n_Cantid, ;
                                  c_UbiFor, ;
                                  c_NumPal, ;
                                  c_PicoSN, ;
                                  b_LAutom, ;
                                  b_BlqUbi, ;
                                  c_CodUbi, ;
                                  c_TxtErr, ; 
                                  c_NumLot                                  

      If Empty(c_CodUbi)
         _LxErr = _LxErr + 'Imposible ubicar el palet N: ' + c_NumPal + cr
         =SqlRollback(_asql)
      Else
         =SqlCommit(_asql)

         *> Grabamos la ubicación.------------------------------------
         Select EntMat
         Go n_RegAnt         && Recuperar puntero en Normal.
         Replace EntMat.UbiFor With c_CodUbi
         If !Empty(EntMat.UbiFor)
            Replace EntMat.UbicSN With "S"
         EndIf
      EndIf

      *> Avanzamos el puntero.----------------------------------------
      Select ENTMAT
      Skip
   EndDo

   *> Posicionamos el puntero en el fichero de líneas.----------------
   Select ENTMAT
   Go Top
Return

*>====================================================================
*> EMat_AceptarUbic ......... Aceptar ubicación en Entrada de Material
*>
*> Recibe       : ENTMAT -----> Cursor con los palets/picos a ubicar.
*>
*> Llamado desde: ENTREMAT ---> Entrada de material.
*>                ENTREPAL ---> Entrada de palet. (No implementado).
*>                ENTRDIRE ---> Entrada rápida. (No implementado).
*>
*> Añadir impresión de etiquetas. AVC - 12.01.1999
*>====================================================================

Procedure EMat_AceptarUbic

Private Fnc, Prm, FncEti
Private n_Status, b_ErrLin, b_ErrEnt

   *> Comprobar que todos los palet con ubicación han actualizado la ubicación de entrada
   Select ENTMAT
   Locate For Empty(UbiFor) .Or. UbicSN=="N"
   If Found()
      If !f3_sn(2, 1, 'Quedan palets pendientes de comprobar ubicación')
	      Return
      EndIf
   EndIf

   *> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   *> Crear objeto parámetros impresión de etiquetas.
   FncEti = CreateObject("OraFncEtiq")
   FncEti.Inicializar

   *> Declara el array y carga parámetros ubicación.------------------
   Declare aubparmz[17]
   Select ENTMAT
   Go Top
   Do Carga_Datos With CodPro, CodArt    && Cargar parámetros ubicación del artículo

   *> Si tenemos que imprimir.----------------------------------------
   If (aubparmz[11] = "S") .OR. (aubparmz[12] = "S")
       Select EntMat
       _Label1 = "Listado Entrada de Material"
*       Do Form St3Imp2 With _Label1,'EntrEMatL1','EntMat'
   EndIf

   *> Crea los movimientos.-------------------------------------------
   b_ErrEnt = .F.
   Select EntMat
   Go Top
   Do While !Eof() .And. b_ErrEnt = .F.

      *> Inicializamos los datos.-------------------------------------
      b_ErrLin = .F.
      With Fnc.ObjParm
         If !Empty(UbiFor)
            .PUbOld = UbiFor
         Else
            .PUbOld = Space(14)
         EndIf
         .PUFlag = "N"           && Ubicación se actualiza al buscarla

         .POTDoc = TipDoc
         .PONDoc = NumDoc
         .PONPed = NumPed
         .POFMov = Date()
         .POCPro = CodPro
         .POCArt = CodArt
         .PONLot = NumLot
         .POSStk = SitStk
         .POFCad = FecCad
         .POCFis = CanFis
         .POCAlm = _Alma
         If !Empty(UbiFor)
            .POCUbi = UbiFor
         Else
            .POCUbi = Space(14)
         EndIf
         .PONPal = NumPal
         .POTPal = TamAbi
         .POFUni = UniVen
         .POFSer = UniPac
         .POFEnv = PacCaj 
         .POFPal = CajPal
         .POFFab = Date()
         .PONEnt = NumEnt
         .POFEnt = m.Fecha
         .POFCal = {  .  .  }
         .POPico = PicoSn
         .PORowID = ""
         .PORowIdNew = ""
         .POFlag = "N"

         .PSCPro = CodPro
         .PSCArt = CodArt
         .PSCAlm = _Alma
         .PStOld = "5000"          && Pendiente de ubicar
         .PStNew = Space(4)
         .PSCFis = CanFis
         .PSFlag = "S"

         .PMTMov = "1000"
         .PMNMov = Space(10)
         .PMFMov = Date()
         .PMCOpe = CodOpe
         .PMFgMP = "S"
         .PMFgHM = "N"
      EndWith

      *> Generamos el movimiento.-------------------------------------
      Fnc.ActMP
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= "50"
         b_ErrLin = .T.
      EndIf

      *> Imprimir etiqueta bulto.-------------------------------------
      If aubparmz[11] = "S"
         If f3_sn(2, 1, "Realizar la impresión de etiquetas")
            With FncEti
               .PEOrig = 'MP'                           && Origen datos: MPs.
               .PMNmov = Prm.PMNmov                     && Nº movimiento del MP.
            EndWith
            FncEti.EtiqArtic
         EndIf
      EndIf

      *> Actualizar línea detalle del albarán entrada.----------------
      If !Empty(Fnc.ObjParm.PONEnt)  And  b_ErrLin = .F.
         strSql = "Update F18n" + _em + " Set F18nEstado='2' Where F18nNumEnt='" + ;
            Fnc.ObjParm.PONEnt + "' And F18nCodPro='" + Fnc.ObjParm.POCPro + ;
            "' And F18nTipDoc='" + Fnc.ObjParm.POTDoc + "' And F18nNumDoc='" + ;
            Fnc.ObjParm.PONDoc + "' And F18nCodArt='" + Fnc.ObjParm.POCArt + ;
            "' And F18nNumLot='" + Fnc.ObjParm.PONLot + "' And F18nSitStk='" + ;
            Fnc.ObjParm.POSStk + "'"

         _xier = f3_SqlExec(_aSql, strSql)

         If _xier <= 0
            _LxErr = _LxErr + 'MENSAJE: ' + Message() + cr + ' PROGRAMA: ' + Prog() + cr
            b_ErrLin = .T.
         EndIf
      EndIf

      If b_ErrLin = .T.
         b_ErrEnt = .T.
         _LxErr = _LxErr + 'Se ha producido un error al aceptar el palet Nº: ' + ENTMAT.NumPal + cr
      EndIf

      *> Seleccionamos y avanzamos el fichero de líneas.--------------
      Select ENTMAT
      Skip
   EndDo

   If !Empty(Fnc.ObjParm.PONEnt)
      *> Actualizar cabecera del albarán entrada.---------------------
      strSql = "Update F18m"  + _em + " Set F18mEstado='2' Where F18mNumEnt='" +  Fnc.ObjParm.PONEnt + "'"
      _xier = f3_SqlExec(_aSql, strSql)
      If _xier <= 0
         _LxErr = _LxErr + 'MENSAJE: ' + Message() + cr + ' PROGRAMA: ' + Prog() + cr
         b_ErrEnt = .T.
      EndIf
   EndIf

   If b_ErrEnt
      _LxErr = _LxErr +  'Se han producido errores durante la confirmación de la entrada' + cr
   EndIf

Return

*>====================================================================
*> EMat_CancelarUbic ........ Cancelar ubicación en Entrada de Material
*>
*> Recibe       : ENTMAT -----> Cursor con los palets/picos a ubicar.
*>
*> Llamado desde: ENTREMAT ---> Entrada de material.
*>                ENTREPAL ---> Entrada de palet. (No implementado).
*>                ENTRDIRE ---> Entrada rápida. (No implementado).
*>====================================================================

Procedure EMat_Cancelar
Private c_CodUbi, n_Peso, n_Volu, b_Err
*> Se procesa el fichero y se desubica
   *> Declara el array.-----------------------------------------------
   Declare aubparmz[17]
   Select EntMat
   Go Top
   Do Carga_Datos With CodPro, CodArt

   Select EntMat
   Go Top
   b_Err = .F.
   Do While !EOF()
      If Empty(UbiFor) .Or. UbicSN = "N"
         Skip
         Loop
      EndIf   

      *> Desubicamos.-------------------------------------------------
      Store 0 to n_Peso, n_Volu
      Do PesVolAr In Ora_Ca00 With EntMat.CodPro, EntMat.CodArt, EntMat.CanFis, n_Peso, N_Volu
      l_Bi=.f.
      Select EntMat
      c_CodUbi = EntMat.UbiFor
      Do DesUbi In Ora_Ca00 With c_CodUbi, n_Peso, n_Volu, 1, l_bi
      If l_Bi = .F.
         =SqlRollback(_aSql)
         _LxErr = _LxErr + 'Se ha producido un error durante la cancelación del palet Nº: ' + EntPal.NumPal + cr
         b_Err = .T.
      Else
         =SqlCommit(_aSql)
      EndIf
         
      Select EntMat
      Skip
   EndDo

   *> La confirmación se hace, de momento, por toda la entrada.-------
   If b_Err = .F.
      =SqlCommit(_aSql)
   Else
      =SqlRollback(_aSql)
      _LxErr = _LxErr + 'Se han producido errores durante la cancelación' + cr
   EndIf
Return

*>=============================================================
*> CnfE_ConfMov ............. Confirmar movimientos de Entrada
*>=============================================================

Procedure CnfE_ConfMov
Private n_Status, Fnc, Prm, b_ErrEnt

   *> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   Store .T. To b_ErrEnt

   Select CNFENT
   Go Top
   Do While !Eof() .And. b_ErrEnt = .T.
      If Empty(Sel) .Or. Empty(CodUbi)
         Skip
         Loop
      EndIf

      =WaitWindow('Actualizando movimiento: ' + CNFENT.CodArt + Space(1) + CNFENT.CodUbi)

      b_ErrEnt = ConfirmarMP(CNFENT.NumMov, CNFENT.CodTra)

      *> Seleccionamos y avanzamos el fichero de líneas.-------------
      Select CNFENT
      Skip
   EndDo

   Wait Clear

   *> Actualizar los MPs confirmados.
   =SqlCommit(_ASql)
  
   *> Elimina objetos función actualización.--------------------------
   Release Prm
   Release Fnc

Return

*>=========================================================================
*> ConfirmarMP .............. Confirma UN movimiento pendiente de entrada.
*>
*>   Recibe: cNumMov ----> Nº de MP a confirmar.
*>           cCodTra ----> Transportista (Opcional).
*>
*> Devuelve: .T. /.F.
*>
*> Llamado desde: CnfE_ConfMov ---> Confirmación de entradas (ENTRCNFE).
*>=========================================================================

Procedure ConfirmarMP
Parameters cNumMov, cCodTra
Private n_Status, Fnc, Prm

   If Type('cCodTra') # 'C'
      Store Space(6) To cCodTra
   EndIf

   *> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   *> Comprobar que el movimiento existe y no está bloqueado.----
   f_Where = "F14cNumMov='" + cNumMov + "'"
   err = F3_Sql('*', 'F14c', f_Where, , , 'CnfEF14c')
   If !err
      _LxErr = 'Error generando cursor de Movimientos Pendientes' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      =SqlRollBack(_ASql)
      Return .F.
   EndIf
      
   Store 0 To n_UniVen, n_UniPac, n_PacCaj, n_CajPal, n_VolPal
   c_TipPal = ''

   *> Obtener datos del artículo.
   m.F08cCodPro = CnfEF14c.F14cCodPro
   m.F08cCodArt = CnfEF14c.F14cCodArt
   Sw = F3_Seek('F08c', , , ;
                'n_UniVen=m.F08cUniVen, n_UniPac=m.F08cUniPac, ' + ;
                'n_PacCaj=m.F08cPacCaj, n_CajPal=m.F08cCajPal, ' + ;
                'c_TipPal = m.F08cTipPal')

   If !Sw
      _LxErr = 'Error leyendo datos de la Ficha de Artículos' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   n_UniVen = IIf(n_UniVen <= 0, 1, n_UniVen)
   n_UniPac = IIf(n_UniPac <= 0, 1, n_UniPac)
   n_PacCaj = IIf(n_PacCaj <= 0, 1, n_PacCaj)
   n_CajPal = IIf(n_CajPal <= 0, 1, n_CajPal)

   Select CnfEF14c
   Replace F14cUniVen With IIf(F14cUniVen <= 0, n_UniVen, F14cUniVen), ;
           F14cUniPac With IIf(F14cUniPac <= 0, n_UniPac, F14cUniPac), ;
           F14cPacCaj With IIf(F14cPacCaj <= 0, n_PacCaj, F14cPacCaj), ;
           F14cCajPal With IIf(F14cCajPal <= 0, n_CajPal, F14cCajPal)

   If Empty(F14cTipPal)
      Replace F14cTipPal With c_TipPal
   EndIf

   *> Calcular peso y volumen.
   Store 0 to n_PesOcu, n_VolOcu
   Do PesVolAr In Ora_Ca00 With CnfEF14c.F14cCodPro, ;
                                CnfEF14c.F14cCodArt, ;
                                CnfEF14c.F14cCanFis, ;
                                n_PesOcu, ;
                                n_VolOcu
      
   *> Actualizar situación de stock '5000', pendiente de ubicar (-).
   Select CnfEF14c

   Fnc.ObjParm.Inicializar
   With Fnc.ObjParm
      .PTAcc = '08'
      .PSCAlm = F14cCodAlm
      .PSCPro = F14cCodPro
      .PSCArt = F14cCodArt
      .PStOld = '5000'
      .PSCFis = F14cCanFis
      .PUFlag = 'N'
      .POFlag = 'N'
      .PSFlag = 'S'
   EndWith

   Fnc.Ejecutar
   n_Status = Fnc.ObjParm.PWCRtn
   If n_Status >= '50'      
      Select CnfEF14c
      _LxErr = 'Error actualizando stock pendiente de ubicar.' + cr + ;
               'DOCUMENTO: ' + F14cTipDoc + '-' + F14cNumDoc + cr + ;
               ' ARTICULO: ' + F14cCodArt + cr
      =Anomalias()
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   *> Actualizar ocupaciones y stock disponible (+).
   Select CnfEF14c

   cEsPico = Iif(F14cCanFis > (F14cUniPac*F14cPacCaj*F14cCajPal), 'N', 'S')

   Fnc.ObjParm.Inicializar
   With Fnc.ObjParm
      .PTAcc = '07'					&& Entrada

      .PUbOld = F14cUbiOri
      .PUFlag = "N"

      .POTDoc = F14cTipDoc
      .PONDoc = F14cNumDoc
      .POLDoc = F14cLinDoc
      .PONPed = F14cNumPed
      .POLPed = F14cLinPed
      .POFMov = Date()
      .POCPro = F14cCodPro
      .POCArt = F14cCodArt
      .PONLot = F14cNumLot
      .POSStk = F14cSitStk
      .POFCad = F14cFecCad
      .POCFis = F14cCanFis
      .POCAlm = F14cCodAlm
      .POCUbi = F14cUbiOri
      .PONPal = F14cNumPal
      .POTPal = F14cTipPal
      .POFUni = F14cUniVen
      .POFSer = F14cUniPac
      .POFEnv = F14cPacCaj 
      .POFPal = F14cCajPal
      .POFFab = F14cFecFab
      .PONEnt = F14cNumEnt
      .POFEnt = Date()
      .POFCal = F14cFecCal
      .PONAna = F14cNumAna
      .POPico = cEsPico
      .POFlag = "S"
      .PSCAlm = F14cCodAlm
      .PSCPro = F14cCodPro
      .PSCArt = F14cCodArt
      .PStOld = F14cSitStk
      .PSCFis = F14cCanFis
      .PSFlag = 'S'

      .PMNMov = Space(10)
      .PMEnSa = "E"
      .PMTMov = F14cTipMov
      .PMFDoc = F14cFecDoc
      .PMDAso = F14cDirAso
      .PMFMov = Date()
      .PMRHab = F14cRutHab
      .PMCTra = cCodTra
      .PMCOpe = F14cCodOpe
      .PMNLst = F14cNumLst
      .PMORes = F14cOriRes
      .PMTUbi = F14cTipUbi
      .PMTMAs = F14cTipMAs
      .PMNMAs = F14cNumMAs
      .PMMUni = F14cMacUni
      .PMORut = F14cOrdRut
      .PMTERe = F14cTEntRe
      .PMCERe = F14cCEntRe
      .PMVHab = F14cVenHab
      .PMTMac = F14cTipMac
      .PMNMac = F14cNumMac
      .PMNExp = F14cNumExp
      .PMFgMP = "N"
      .PMFgHM = "S"
   EndWith

   *> Realizar entrada actualizando ocupación, stock y ubicación.
   Fnc.Ejecutar
   n_Status = Fnc.ObjParm.PWCRtn
   If n_Status >= '50'      
      Select CnfEF14c
      _LxErr = 'Error realizando la ubicación de material' + cr + ;
               'DOCUMENTO: ' + F14cTipDoc + '-' + F14cNumDoc + cr + ;
               ' ARTICULO: ' + F14cCodArt + cr
      =Anomalias()
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   *> Borrar el MP. Si hay error, aviso, pero permite seguir.
   Fnc.ObjParm.PMNMov = cNumMov
   Fnc.DelMP
   n_Status = Fnc.ObjParm.PWCRtn
   If n_Status >= '50'
      Select CnfEF14c
      _LxErr = 'Error borrando Movimiento Pendiente' + cr + ;
               'DOCUMENTO: ' + F14cTipDoc + '-' + F14cNumDoc + cr + ;
               ' ARTICULO: ' + F14cCodArt + cr
      =Anomalias()
   EndIf
 
   *>Miramos si queda algun MP que no se haya podido confirmar
   _Sel="Select * From F14c" + _em + " Where F14cNumEnt='" + Fnc.ObjParm.PONEnt + "' " + ;
  		" And   F14cCodPro='" + Fnc.ObjParm.POCPro + "'" + ;
        " And   F14cTipDoc='" + Fnc.ObjParm.POTDoc + "'" + ;
        " And   F14cNumDoc='" + Fnc.ObjParm.PONDoc + "'" + ;
        " And   F14cCodArt='" + Fnc.ObjParm.POCArt + "'" + ;
        " And   F14cNumLot='" + Fnc.ObjParm.PONLot + "'" + ;
        " And   F14cSitStk='" + Fnc.ObjParm.POSStk + "'" + ;
        " And   F14cLinDoc=" + Str(Fnc.ObjParm.POLDoc) + ""    	 
   Sw = f3_SqlExec(_Asql,_Sel,'CERRAR')
   
   Select CERRAR
   Go Top
   If Empty(CERRAR.F14cNumEnt)   
   		*> Actualizar Albarán de Entrada, si lo hay.----------------------
   		If !Empty(Fnc.ObjParm.PONEnt)
      		*> Actualizar línea detalle del albarán entrada.----------------
      		strSql = "Update F18n" + _em + " Set F18nEstado='3'" + ;
            	     " Where F18nNumEnt='" + Fnc.ObjParm.PONEnt + "'" + ;
               		 " And   F18nCodPro='" + Fnc.ObjParm.POCPro + "'" + ;
               		 " And   F18nTipDoc='" + Fnc.ObjParm.POTDoc + "'" + ;
               		 " And   F18nNumDoc='" + Fnc.ObjParm.PONDoc + "'" + ;
               		 " And   F18nCodArt='" + Fnc.ObjParm.POCArt + "'" + ;
               		 " And   F18nNumLot='" + Fnc.ObjParm.PONLot + "'" + ;
               		 " And   F18nSitStk='" + Fnc.ObjParm.POSStk + "'"

      		_xier = f3_SqlExec(_aSql, strSql)
      		If _xier <= 0
         		_LxErr = 'Error actualizando detalle Albarán de Entrada' + cr + ;
            	         'MENSAJE: ' + Message() + cr
        		=Anomalias()
         		=SqlRollBack(_ASql)
         		Return .F.
      		EndIf

      		*> Ver si quedan líneas de detalle pendientes de este albarán.
      		strSql = "Select " + _GCN("Count(*)") + " As FLinAlb From F18n"  + _em + ;
               		 " Where F18nNumEnt='" + Fnc.ObjParm.PONEnt + "'" + ;
               		 " And   F18nEstado<'3'"
 
      		_xier = f3_SqlExec(_aSql, strSql)
      		If _xier <= 0
         		_LxErr = 'Error en control líneas pendientes Albarán de Entrada' + cr + ;
                		 'MENSAJE: ' + Message() + cr
         		=Anomalias()
         		=SqlRollBack(_ASql)
         		Return .F.
      		EndIf

      		*> Actualizar CABECERA Albarán de Entrada.
      		=SqlMoreResults(_ASql)
      		strSql = "Update F18m"  + _em + ;
            		 " Set F18mEstado='" + Iif(FLinAlb > 0, '1', '3') + "'" + ;
               		 " Where F18mNumEnt='" + Fnc.ObjParm.PONEnt + "'"
 
     		 _xier = f3_SqlExec(_aSql, strSql)
      		If _xier <= 0
         		_LxErr = 'Error actualizando cabecera Albarán de Entrada.' + cr + ;
                		 'MENSAJE: ' + Message() + cr
         		=Anomalias()
         		=SqlRollBack(_ASql)
         		Return .F.
      		EndIf
   		EndIf
EndIf

*> Actualizar los cambios realizados.
=SqlCommit(_aSql)
Use In CnfEF14c
  
*> Elimina objetos función actualización.--------------------------
Release Prm
Release Fnc

Return

*>======================================================================
*> Pale_Ubicar .............. Forzar o buscar ubicación en Paletización
*>======================================================================

Procedure Pale_Ubicar
Parameters cCursor
Private n_Cantid, c_TamUbi, c_CodUbi, c_UbiFor, c_NumPal, c_CodArt, c_PicoSN, n_RegAnt
Local lAgain

*> Proporciona la ubicación a cada una de las líneas
*> A.- Unidades: Busca ubicación para toda la cantidad de LINPAL.CanFis
*> B.-    Cajas: Busca ubicación para CADA UNA de las cajas de LINPAL.CanFis
*> C.-   Palets: Busca ubicación para toda la cantidad de LINPAL.CanFis

   *> Como se llama desde varios programas, renombrar el alias.
   If Type('cCursor')=='C' .And. !Empty(cCursor)
      Select (cCursor)
      Use Dbf() In 0 Alias LINPAL Again
      Store .T. To lAgain
   EndIf

   *> Declara el array de parámetros ubicación.-----------------------
   Declare aubparmz[17]
   Select LINPAL
   Go Top
   Do Carga_Datos With CodPro, CodArt    && Cargar parámetros ubicación del artículo

   *> Procedemos.-----------------------------------------------------
   Select LINPAL
*   Locate For UbicSN # 'S'
*   Do While Found()
    Do While !Eof()

       *> Si ya está ubicado.-----------------------------------------
       If LinPal.UbicSn = 'S'
          *> Obtener el Nº de Palet, si no se ha asignado ya.
          If Empty(NumPal)
             Replace NumPal With Ora_NewPal()
          EndIf

		  Skip
		  Loop	
       EndIf
    
      *> Guardamos datos.---------------------------------------------
      c_CodPro = LINPAL.CodPro
      c_CodArt = LINPAL.CodArt
      c_TamUbi = LINPAL.TamAbi
      n_Cantid = LINPAL.CanFis
      c_UbiFor = IIf(Empty(LINPAL.UbiFor), '', LINPAL.UbiFor)
      c_NumPal = LINPAL.NumPal
      c_PicoSN = LINPAL.PicoSn
      b_LAutom = .F.
      b_BlqUbi = .T.         && Si bloquea la ubicación o no
      c_CodUbi = ""          && Retorno de la ubicación destino
      c_TxtErr = ""
      c_NumLot = LINPAL.NumLot          && Texto de porque no se puede ubicar

      n_RegAnt = RecNo()      && Guardar puntero en Normal (lo pierde).

      Do Prc_Ubi In Ora_Cu00 With c_CodPro, ;
                                  c_CodArt, ;
                                  c_TamUbi, ;
                                  n_Cantid, ;
                                  c_UbiFor, ;
                                  c_NumPal, ;
                                  c_PicoSN, ;
                                  b_LAutom, ;
                                  b_BlqUbi, ;
                                  c_CodUbi, ;
                                  c_TxtErr, ;
                                  c_NumLot
      If Empty(c_CodUbi)
         _LxErr = _LxErr + 'Imposible ubicar el palet Nº: ' + c_NumPal + cr
         =SqlRollback(_asql)
      Else
         =SqlCommit(_asql)

         *> Grabamos la ubicación.------------------------------------
         Select LINPAL
         Replace LINPAL.UbiFor With c_CodUbi, ;
                 LINPAL.UbicSN With "S"

         *> Obtener el Nº de Palet, si no se ha asignado ya.
         If Empty(NumPal)
            Replace NumPal With Ora_NewPal()
         EndIf
      Endif   

      *> Avanzamos el puntero.----------------------------------------
      Select LINPAL
      Skip
      Loop
   EndDo

   *> Posicionamos el puntero en el fichero de líneas.----------------
   Select LINPAL
   Go Top

   If lAgain
      Use In LINPAL
   EndIf

Return 

*>======================================================================
*> CnfE_Ubicar .............. Forzar o buscar ubicación en Confirmación
*>======================================================================

Procedure CnfE_Ubicar
Private n_Cantid, c_TamUbi, c_CodUbi, c_UbiFor, c_NumPal, c_CodArt, c_PicoSN, n_RegAnt
Private cWhere, cUbiOri

*> Proporciona la ubicación a cada una de las líneas:
*> A.- Unidades: Busca ubicación para toda la cantidad de CNFENT.CanFis
*> B.-    Cajas: Busca ubicación para CADA UNA de las cajas de CNFENT.CanFis
*> C.-   Palets: Busca ubicación para toda la cantidad de CNFENT.CanFis

   *> Declara el array de parámetros ubicación.-----------------------
   Declare aubparmz[17]
   Select CNFENT
   Go Top

   
   Do Carga_Datos With CodPro, CodArt    && Cargar parámetros ubicación del artículo

   *> Procedemos.-----------------------------------------------------
   Select CNFENT
   Locate For !Empty(Sel)
   Do While Found()
      *> Guardamos datos.---------------------------------------------
      c_CodPro = CNFENT.CodPro
      c_CodArt = CNFENT.CodArt
      c_TamUbi = CNFENT.TamAbi
      n_Cantid = CNFENT.CanFis
      c_UbiFor = IIf(Empty(CNFENT.CodUbi), '', CNFENT.CodUbi)
      c_NumPal = CNFENT.NumPal
      c_PicoSN = CNFENT.PicoSn
      b_LAutom = .F.
      b_BlqUbi = .T.         && Si bloquea la ubicación o no
      c_CodUbi = ""          && Retorno de la ubicación destino
      c_TxtErr = ""
      c_NumLot = CNFENT.Numlot          && Texto de porque no se puede ubicar

      n_RegAnt = RecNo()      && Guardar puntero en Normal (lo pierde).

      Do Prc_Ubi In Ora_Cu00 With c_CodPro, ;
                                  c_CodArt, ;
                                  c_TamUbi, ;
                                  n_Cantid, ;
                                  c_UbiFor, ;
                                  c_NumPal, ;
                                  c_PicoSN, ;
                                  b_LAutom, ;
                                  b_BlqUbi, ;
                                  c_CodUbi, ;
                                  c_TxtErr, ;
                                  c_NumLot
      If Empty(c_CodUbi)
         _LxErr = _LxErr + 'Imposible ubicar el palet Nº: ' + c_NumPal + cr
         =SqlRollBack(_ASql)
      Else
         =SqlCommit(_ASql)

         *> Grabamos la ubicación.------------------------------------
         Select CNFENT
*         Go n_RegAnt         && Recuperar puntero en Normal.
         Replace CNFENT.CodUbi With c_CodUbi

         *> Obtener el Nº de Palet, si no se ha asignado ya.
         If Empty(NumPal)
            Replace NumPal With Ora_NewPal()
         EndIf

         *> Actualizar ubicación en el MP.
         cWhere = "F14cNumMov='" + CNFENT.NumMov + "'"
         cUbiOri = CNFENT.CodUbi
         If !f3_UpdTun('F14c', , 'F14cUbiOri', 'cUbiOri', , cWhere, 'N')
            _LxErr = _LxErr + 'No se ha podido actualizar ubicación en MP ' + CNFENT.NumMov + cr
         EndIf
      Endif   

      *> Avanzamos el puntero.----------------------------------------
      Select CNFENT
*      Continue
      Skip 
      Loop
   EndDo

   *> Posicionamos el puntero en el fichero de líneas.----------------
   Select CNFENT
   Go Top
Return 

*>====================================================================
*> Pale_AceptarUbic ......... Aceptar ubicación de Paletización.
*> Añadir impresión de etiquetas. AVC - 12.01.1999
*> Llamado desde:
*>   - ENTRPALE - Paletización de entradas.
*>   - ENTRACPP - Propuesta de paletización.
*>====================================================================

Procedure Pale_AceptarUbic
Parameters cCursor, cCursor2

Private n_Status, Fnc, Prm, HayAlb
Private _HayEtiArt, _HayEtiPal
Local lAgain, lAgain2

   *> Como se llama desde varios programas, renombrar aliases.
   If Type('cCursor')=='C' .And. !Empty(cCursor)
      Select (cCursor)
      Use Dbf() In 0 Alias LINPAL Again
      Store .T. To lAgain
   EndIf

   If Type('cCursor2')=='C' .And. !Empty(cCursor2)
      Select (cCursor2)
      Use Dbf() In 0 Alias LINART Again
      Store .T. To lAgain2
   EndIf

   *> Comprobar que todos los palet con ubicación han actualizado la ubicación de entrada
   Store .F. To HayAlb

   Select LINPAL
   Go Top
  
   Do While !EOF()
      If Empty(LINPAL.UbiFor) .Or. LINPAL.UbicSN # "S"
         _LxErr = 'Quedan palets pendientes de comprobar ubicación. Sino utiliza el botón UBICAR' + cr + ;
                  'deber.a ubicar el material mediante la confirmacion de entradas' + cr
*        Return
      EndIf
      Skip
   EndDo

   *> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   *> Crear objeto parámetros impresión de etiquetas.
   FncEti = CreateObject("OraFncEtiq")
   FncEti.Inicializar

   *> Declara el array y carga parámetros ubicación.------------------
   Declare aubparmz[17]
   Select LINPAL
   Go Top
   Do Carga_Datos With CodPro, CodArt    && Cargar parámetros ubicación del artículo

   *> Si tenemos que imprimir.----------------------------------------
   If (aubparmz[11] = "S") .OR. (aubparmz[12] = "S")
       Select LINPAL
       _Label1 = "Listado de Paletización"
*       Do Form St3Imp2 With _Label1,'EntrPaleL1','LINPAL'
   EndIf

   *> Crea los movimientos.-------------------------------------------
   Select LINPAL
   Go Top
   Do While !Eof()
      If Empty(NumPal)
         Replace Numpal With Ora_NewPal()
      EndIf

      *> Inicializamos los datos.-------------------------------------
      Select LINART
      Go LINPAL.RegArt
      Select LINPAL

      With Fnc.ObjParm
         If !Empty(UbiFor)
            .PUbOld = UbiFor
         Else
            .PUbOld = Space(14)
         EndIf
         .PUFlag = "N"           && Ubicación se actualiza al buscarla

         .POTDoc = LINART.F18nTipDoc
         .PONDoc = LINART.F18nNumDoc
         .POLDoc = Val(LINART.F18nLinDoc)
         .PONPed = Space(15)
         .POFMov = Date()
         .POCPro = CodPro
         .POCArt = CodArt
         .PONLot = NumLot
         .POSStk = SitStk
         .POFCad = FecCad
         .POCFis = CanFis
         .POCAlm = _Alma
         If !Empty(UbiFor)
            .POCUbi = UbiFor
         Else
            .POCUbi = Space(14)
         EndIf
         .PONPal = NumPal
         .POTPal = TamAbi
         .POFUni = LINART.F08cUniVen
         .POFSer = LINART.F08cUniPac
         .POFEnv = LINART.F08cPacCaj
         .POFPal = LINART.F08cCajPal
         .POFFab = Date()
         .PONEnt = NumEnt
         .POFEnt = m.Fecha
         .POFCal = {  .  .  }
         .POPico = PicoSn
         .POFlag = "N"

         .PSCPro = CodPro
         .PSCArt = CodArt
         .PSCAlm = _Alma
         .PStOld = "5000"          && Pendiente de ubicar
         .PStNew = Space(4)
         .PSCFis = CanFis
         .PSFlag = "S"

         .PMTMov = "1000"
         .PMFDoc = LINART.F18cFecPed
         .PMNMov = Space(10)
         .PMFMov = Date()
         .PMCOpe = CodOpe
         .PMFgMP = "S"
         .PMFgHM = "N"
      EndWith

      *> Generamos el movimiento.-------------------------------------
      =WaitWindow('Generando Movimiento Pendiente: ' + UbiFor)

      Fnc.ActMP
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         _LxErr = 'Error generando Movimiento Pendiente.' + cr + ;
                  'DOCUMENTO: ' + LINART.F18nTipDoc + '-' + LINART.F18nNumDoc + cr + ;
                  ' ARTICULO: ' + F18nCodArt + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Actualizamos situación de stock Pendiente de ubicar.(Sumamos).---
      Fnc.ObjParm.PTAcc = "07"    && Entrada
      Fnc.Ejecutar
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         _LxErr = 'Error actualizando stock pendiente de ubicar.' + cr + ;
                  'DOCUMENTO: ' + LINART.F18nTipDoc + '-' + LINART.F18nNumDoc + cr + ;
                  ' ARTICULO: ' + F18nCodArt + cr

         =Anomalias()
         =SqlRollBack(_ASql)

         If lAgain
            Use In LINPAL
         EndIf
         If lAgain2
            Use In LINART
         EndIf
         Return
      EndIf
  
      *> Desglose automático.-----------------------------------------
      If aubparmz[10] = "S"

      EndIf

      *> Confirmación automática.-------------------------------------
      aubparmz[09] = "N"
      If aubparmz[09] = "S"
         Select f14c
         LRet = .f.
         NReg = RecNo()

         Do Prc_Conf in Lib_Ca00 with NReg, LRet
         If !LRet
            _LxErr = 'Error en la confirmación automatica del palet Nº: ' + a_datos(01) + cr
            =Anomalias()
            =SqlRollBack(_ASql)

            If lAgain
               Use In LINPAL
            EndIf
            If lAgain2
               Use In LINART
            EndIf
            Return
         EndIf

         *> Actualizamos situación de stock Pendiente de ubicar.(Restamos).---
         L_Resultad=.f.
         Do actualst In Lib_ca00 With A_Datos(13), ;
                                      A_Datos(15), ;
                                      "5000", ;
                                      .F., ;
                                      l_Resultad
         If !L_Resultad
            _LxErr = 'Error actualizando stock pendiente de ubicar (Automático) ' + a_datos(01) + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf

         *> Actualizamos situación de stock Entrada.(Sumamos).---
         L_Resultad=.f.
         Do actualst In Lib_ca00 With A_Datos(13), ;
                                      A_Datos(15), ;
                                      A_Datos(14), ;
                                      .T., ;
                                      l_Resultad
         If !L_Resultad
            _LxErr = 'Error actualizando stock pendiente de entrada (Automático) ' + a_datos(01) + cr
            =Anomalias()
            =SqlRollBack(_ASql)

            If lAgain
               Use In LINPAL
            EndIf
            If lAgain2
               Use In LINART
            EndIf
            Return
         EndIf
      EndIf

      *> Actualizar línea detalle del albarán entrada.----------------
      If !Empty(Fnc.ObjParm.PONEnt)
         strSql = "Update F18n" + _em + " Set F18nEstado='2'" + ;
                  " Where F18nNumEnt='" + Fnc.ObjParm.PONEnt + "'" + ;
                  " And   F18nCodPro='" + Fnc.ObjParm.POCPro + "'" + ;
                  " And   F18nTipDoc='" + Fnc.ObjParm.POTDoc + "'" + ;
                  " And   F18nNumDoc='" + Fnc.ObjParm.PONDoc + "'" + ;
                  " And   F18nCodArt='" + Fnc.ObjParm.POCArt + "'" + ;
                  " And   F18nNumLot='" + Fnc.ObjParm.PONLot + "'" + ;
                  " And   F18nSitStk='" + Fnc.ObjParm.POSStk + "'"
         _xier = f3_SqlExec(_aSql, strSql)
         If _xier <= 0
            _LxErr = 'Error actualizando detalle Albarán de Entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr
            =Anomalias()
            =SqlRollBack(_ASql)

            If lAgain
               Use In LINPAL
            EndIf
            If lAgain2
               Use In LINART
            EndIf
            Return
         EndIf
      EndIf

      *> Actualizar la CABECERA del ALBARAN. Leer el estado de las líneas.
      lx_upd = "Select " + GetCvtNvl(_ENTORNO, _VERSION, "Count(*)") + " As FLinPed From F18n" + _em
      lx_Where=" Where F18nNumEnt='" + NumEnt + _cm + ;
               " And   F18nEstado<'2'"
      _xier = f3_SqlExec(_Asql, lx_Upd+lx_Where)
      If _xier < 0
         _LxErr = 'Error control estado líneas (2) Documento Nº: ' + LINART.F18nNumDoc + cr + ;
                  'MENSAJE: ' + Message() + cr
         =Anomalias()
         =SqlRollBack(_ASql)

         If lAgain
            Use In LINPAL
         EndIf
         If lAgain2
            Use In LINART
         EndIf
         Return
      EndIf
         
      =SqlMoreResults(_ASql)
      lx_Upd = "Update F18m" + _em + ;
               " Set F18mEstado='" + ;
               Iif(FLinPed==0, '2', '1') + "'"
      lx_Where=" Where F18mNumEnt='" + NumEnt + _cm
      _xier = f3_SqlExec(_Asql, lx_Upd+lx_Where)
      If _xier < 0
         _LxErr = 'Error actualizando cabecera albarán Nº: ' + NumEnt + cr + ;
                  'MENSAJE: ' + Message() + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Imprimir etiqueta artículo.----------------------------------
*!*	      If aubparmz[11] = "S"
*!*	         _HayEtiArt = Iif (Type('_HayEtiArt')=='U', f3_sn(2, 1, 'Imprimir etiquetas de artículo'), _HayEtiArt)
*!*	         If _HayEtiArt
*!*	            With FncEti
*!*	               .PEOrig = 'MP'                              && Origen datos: F14c
*!*	               .PMNmov = Prm.PMNmov                        && Nº movimiento F14c
*!*	               Controlar=.T.
*!*	            EndWith
*!*	            FncEti.EtiqArtic
*!*	         EndIf
*!*	      EndIf

*!*	      *> Imprimir etiqueta palet.----------------------------------------
*!*	      If aubparmz[12] = "S"
         _HayEtiPal = Iif (Type('_HayEtiPal')=='U', f3_sn(2, 1, 'Imprimir etiquetas de Palet'), _HayEtiPal)
         If _HayEtiPal
            With FncEti
               .Inicializar
               .PEOrig = "MP"
               .PMNmov = Prm.PMNmov
               Controlar=.T.
            EndWith
            FncEti.OpenPrinter
            FncEti.MPDatosEnt
            FncEti.EtiqPaletOcp
            FncEti.ClosePrinter
         EndIf
*!*	      EndIf

      *> Imprimir etiqueta caja.-------------------------------------
      If "N" = "S"
*     If F08c.F08cEtiCaj = "S" 
         Select LINPAL
         Go Top

         Select F08c
         Seek LINPAL.CodArt

         Select LINPAL
         Go Top
         Do While !Eof()
            If !Empty(UbiFor)
               NumEti=  Str(Alltrim(Ceiling(CanFis/m.UniVen*m.UniPac*m.PacCaj)))
               Do ImpPro With F08c.F08cCodigo, NumEti In Imprime
            EndIf 

            Select LINPAL
            Skip
         EndDo
      EndIf

      *> Seleccionamos y avanzamos el fichero de líneas.--------------
      Store .T. To HayAlb
      Select LINPAL
      Skip
   EndDo

   *>
   If HayAlb
      =SqlCommit(_ASql)
      =f3_sn(1, 1, 'Realizada paletización de material Albarán de Entrada Nº: ' + m.NumEnt)
   Else
      _LxErr = 'No se ha podido realizar la Paletización de Material' + cr
      =Anomalias()
      =SqlRollBack(_ASql)
   EndIf   

   *> Elimina objetos función actualización y etiquetas.--------------
   Release Prm
   Release Fnc
   Release FncEti

   If lAgain
      Use In LINPAL
   EndIf
   If lAgain2
      Use In LINART
   EndIf
Return

*>====================================================================
*> Pale_CancelarUbic ........ Cancelar ubicac. de Entrada de Material
*>====================================================================

Procedure Pale_Cancelar
Parameters cCursor

Private c_CodUbi, n_Peso, n_Volu
Local lAgain

   *> Como se llama desde varios programas, renombrar el alias.
   If Type('cCursor')=='C' .And. !Empty(cCursor)
      Select (cCursor)
      Use Dbf() In 0 Alias LINPAL Again
      Store .T. To lAgain
   EndIf

   *> Declara el array.-----------------------------------------------
   Declare aubparmz[17]

   Select LINPAL
   Go Top
   Do Carga_Datos With CodPro, CodArt

   Select LINPAL
   Go Top
   Do While !EOF()
      If Empty(UbiFor) .Or. UbicSN = "N"
         Skip
         Loop
      EndIf   

      *> Desubicamos.-------------------------------------------------
      Store 0 to n_Peso, n_Volu
      Do PesVolAr In Ora_Ca00 With LINPAL.CodPro, ;
                                   LINPAL.CodArt, ;
                                   LINPAL.CanFis, ;
                                   n_Peso, ;
                                   N_Volu
      l_Bi=.f.
      Select LINPAL
      c_CodUbi = LINPAL.UbiFor
      Do DesUbi In Ora_Ca00 With c_CodUbi, n_Peso, n_Volu, 1, l_bi
      If l_Bi = .F.
         =SqlRollback(_aSql)
         _LxErr = _LxErr + 'Se ha producido un error durante la cancelación del palet Nº: ' + + EntPal.NumPal + cr
         b_Err = .T.
      Else
         =SqlCommit(_aSql)
      EndIf
         
      Select LINPAL
      Skip
   EndDo

   If lAgain
      Use In LINPAL
   EndIf

Return

*>===============================================================
*> CfCa_Chiste .............. Chiste rutas de Confirmación Carga
*>===============================================================
Function CfCa_Chiste
   Store "0" To In51, In52, In53
   Store "1" To In99
   
   =CrtCursor('F14c', 'ChstF14c')
   f_Where = "F14cCodPro = '" + ProAct + "' And F14cTipDoc = '" + TDoAct + "' And " + ;
             "F14cNumDoc = '" + NDoAct + "'"
   =f3_sql('*', 'F14c', f_Where, , , 'ChstF14c')
   
   Select ChstF14c
   Go Top
   Do While !EOF()
      If F14cTipMov = '2999'
         Skip
         Loop
      EndIf
      
      If Empty(F14cNumLst)
         In51 = "1"
      Else
         In53 = "1"
      EndIf
      
      If F14cEstMov = "B"
         In52 = "1"
      EndIf
      
      Select ChstF14c
      Skip
   EndDo
   
   Use In ChstF14c
Return

*>======================================================================
*> CfCa_GrabRut ............. Grabar cursor rutas en Confirmación Carga
*>======================================================================
Procedure CfCa_GrabRut
Private c_DesRut
   Select CnfRut
   Locate For RutHab = RutAct .And. FecEnt = FEnAct
   If !Found()
      *> Buscar descripción de la ruta.-------------------------------
      c_DesRut = ''
      m.F00lCodRut = RutAct
      =f3_seek('F00l', , , 'c_DesRut = m.F00lDescri')
      
      Append Blank
      Replace Sel With 0, ;
              RutHab With RutAct, ;
              DesRut With c_DesRut, ;
              FecEnt With FEnAct, ;
              UbiExp With c_UbiExp, ;
              DocRut With 0, ;
              RecNum With RecNo()
   EndIf
 
   Replace DocRut With DocRut + n_DocRut,  ;
                       NoAsig With IIf(In54 = '1', 'X', NoAsig), ;
                       MovBlq With IIf(In55 = '1', 'X', MovBlq), ;
                       PteLst With IIf(In56 = '1', 'X', PteLst)

   _RecNo = RecNo()

   *> Actualizar cursor documentos asociados a la ruta.
   Select CNFRUTDOC
   Replace Sel With 0, ;
           RutHab With RutAct, ;
           FecEnt With FEnAct, ;
           UbiExp With c_UbiExp, ;
           DocRut With 0, ;
           RecNum With _RecNo ;
      For RecNum = 0
Return

*>===========================================================================
*> CfCa_GrabDoc ............. Grabar cursor documentos en Confirmación Carga
*>===========================================================================
Procedure CfCa_GrabDoc

   Select CNFDOC
   Locate For CodPro = ProAct .And. TipDoc = TDoAct .And. NumDoc = NDoAct

   If !Found()
      Append Blank
      Replace Sel With 0, ;
              CodPro With ProAct, ;
              TipDoc With TDoAct, ;
              NumDoc With NDoAct, ;
              CodCli With CliAct, ;
              OrdRut With n_OrdRut, ;
              RutHab With CNFRUT.RutHab, ;
              CodTra With TraAct, ;
              UbiExp With c_UbiExp, ;
              FecEnt With CNFRUT.FecEnt
   EndIf
 
   Replace NoAsig With IIf(In51 = '1', 'X', NoAsig), ;
           MovBlq With IIf(In52 = '1', 'X', MovBlq), ;
           PteLst With IIf(In53 = '1', 'X', PteLst)
Return

*>===========================================================================
*> CfCa_MovParm ............. Mover parámetros a función de ubicación
*>===========================================================================
Procedure CfCa_MovParm
Parameters FichOri
Private CfCaF16c, n_Peso, n_Volu
Select (FichOri)

*>Datos Ubicaciones
FncA.ObjParm.PUbOld=F14cUbiOri	        && Ubicación nueva
FncA.ObjParm.PUFlag='N'	                && Flag actualizar ubicación ('S' o 'N')

*>Datos Ocupaciones
FncA.ObjParm.POTDoc=F14cTipDoc			&& Tipo documento
FncA.ObjParm.PONDoc=F14cNumDoc			&& Número documento
FncA.ObjParm.POLDoc=F14cLinDoc			&& Línea documento
* FncA.ObjParm.POTEnt=F14cTipEnt			&& Tipo entidad
* FncA.ObjParm.POCEnt=F14cCodEnt			&& Código entidad
FncA.ObjParm.PONPed=F14cNumPed			&& Número pedido
FncA.ObjParm.POLPed=F14cLinPed			&& Línea pedido
FncA.ObjParm.POFMov=Date()    			&& Fecha movimiento

FncA.ObjParm.POCPro=F14cCodPro			&& Código propietario
FncA.ObjParm.POCArt=F14cCodArt			&& Código artículo
FncA.ObjParm.PONLot=F14cNumLot			&& Número lote
FncA.ObjParm.POSStk=F14cSitStk			&& Situación stock
FncA.ObjParm.POFCad=F14cFecCad			&& Fecha caducidad
FncA.ObjParm.POCFis=F14cCanFis			&& Cantidad física
FncA.ObjParm.POCAlm=F14cCodAlm			&& Código almacén
FncA.ObjParm.POCUbi=F14cUbiOri			&& Código ubicación
FncA.ObjParm.PONPal=F14cNumPal			&& Número palet
FncA.ObjParm.POTPal=F14cTipPal			&& Tipo palet

FncA.ObjParm.POFUni=F14cUniVen			&& Factor unidad (unidades venta)
FncA.ObjParm.POFSer=F14cUniPac			&& Factor servicio (unidades / pack)
FncA.ObjParm.POFEnv=F14cPacCaj			&& Factor envase (packs / cajas)
FncA.ObjParm.POFPal=F14cCajPal			&& Factor palet (cajas / palet)
FncA.ObjParm.POFFab=F14cFecFab  		&& Fecha fabricación
FncA.ObjParm.POFCal=F14cFecCal			&& Fecha calidad
FncA.ObjParm.PONAna=F14cNumAna			&& Número análisis
FncA.ObjParm.PONEnt=F14cNumEnt			&& Número entrada
FncA.ObjParm.POCRes=F14cCanFis			&& Cantidad reservada

*> Calcular volumen ocupado y si es pico.-----------------------------
Store 0 To n_Pes, n_Vol
Do PesVolAr In Ora_Ca00 With F14cCodPro, F14cCodArt, F14cCanFis, n_Pes, n_Vol
FncA.ObjParm.POVOcu=n_Vol				&& Volumen ocupado
	
Select (FichOri)
CanMov=FncA.ObjParm.POFUni * FncA.ObjParm.POFSer * FncA.ObjParm.POFEnv * FncA.ObjParm.POFPal
FncA.ObjParm.POPico='N'
If CanMov > F14cCanFis  
   FncA.ObjParm.POPico='S'				&& Pico S/N
EndIf
FncA.ObjParm.POFlag='N'	&&Flag actualizar ocupación ('S' o 'N')

*>Datos Situaciones Stock
FncA.ObjParm.PSCPro=F14cCodPro			&& Código propietario
FncA.ObjParm.PSCArt=F14cCodArt			&& Código artículo
FncA.ObjParm.PSCAlm=F14cCodAlm			&& Código almacén
FncA.ObjParm.PStOld=F14cSitStk			&& Situación stock antigua
FncA.ObjParm.PStNew=Space(4)			&& Situación stock nueva
FncA.ObjParm.PSCFis=F14cCanFis			&& Cantidad
FncA.ObjParm.PSFlag='N'					&& Flag actualizar situación stock ('S' o 'N')

*>Datos Movimientos
FncA.ObjParm.PMTMov=F14cTipMov			&& Tipo movimiento
FncA.ObjParm.PMNMov=Space(10)			&& Número movimiento
FncA.ObjParm.PMFMov=F14cFecMov			&& Fecha movimiento
FncA.ObjParm.PMFDoc=F14cFecDoc			&& Fecha documento
FncA.ObjParm.PMRHab=F14cRutHab			&& Ruta habitual
FncA.ObjParm.PMCOpe=F14cCodOpe			&& Código operario
FncA.ObjParm.PMNLst=F14cNumLst			&& Número lista
FncA.ObjParm.PMNExp=F14cNumExp			&& Número expedición
FncA.ObjParm.PMORec=F14cOrdRec			&& Orden recorrido
FncA.ObjParm.PMStat=F14cEstMov			&& Estado movimiento
FncA.ObjParm.PMORes=F14cOriRes			&& Origen reserva
FncA.ObjParm.PMTUbi=F14cTipUbi			&& Tipo ubicación
FncA.ObjParm.PMTMAs=F14cTipMAs			&& Tipo MAC asociado
FncA.ObjParm.PMNMAs=F14cNumMAs			&& Número MAC asociado
FncA.ObjParm.PMMUni=F14cMacUni			&& MAC unido
FncA.ObjParm.PMORut=F14cOrdRut			&& Orden ruta
FncA.ObjParm.PMTERe=F14cTEntRe			&& Tipo entidad reexpedición
FncA.ObjParm.PMCERe=F14cCEntRe			&& Código entidad reexpedición
FncA.ObjParm.PMVHab=F14cVenHab			&& Vendedor habitual
FncA.ObjParm.PMTMac=F14cTipMac			&& Tipo MAC
FncA.ObjParm.PMNMac=F14cNumMac			&& Número MAC
FncA.ObjParm.PMSecc=F14cSeccio			&& Sección
FncA.ObjParm.PMFgMP='N'					&& Flag actualizar movimiento pendiente ('S' o 'N')

*>Más campos (para histórico de movimientos)

FncA.ObjParm.PMEnSa='S'       			&& Entrada / Salida
FncA.ObjParm.PMMvMP=F14cNumMov			&& Número Movimiento MP para HM
FncA.ObjParm.PMDAso=F14cDirAso			&& Dirección asociada
FncA.ObjParm.PMFEnv=_FecMin				&& Fecha envío
FncA.ObjParm.PMFOrd=_FecMin				&& Fecha orden
FncA.ObjParm.PMFgHM='N'					&& Flag actualizar histórico de movimientos ('S' o 'N')
Return

*>===========================================================================
*> CfCa_ActStk............... Actualizar stocks, ocupaciones y ubicaciones
*>===========================================================================
Procedure CfCa_ActStk
Private c_Error
   c_Error = Space(2)
   
   *> Restar de stock reservado.--------------------------------------
   Select CRutF14c
   FncA.ObjParm.PWCRtn = Space(2)
   With FncA.ObjParm
      .PTAcc='08'
      .PUbOld=F14cUbiOri
      .PUFlag='N'
      .POSStk=F14cSitStk
      .POCFis=0
      .POCRes=F14cCanFis
      .POFlag='S'
      .PSTOld='2000'
      .PSCFis=F14cCanFis
      .PSFlag='S'
   EndWith
   FncA.Ejecutar
   c_Error = FncA.ObjParm.PWCRtn

   *> Restar de stock físico.-----------------------------------------
   Select CRutF14c
   FncA.ObjParm.PWCRtn = Space(2)
   With FncA.ObjParm
      .PTAcc='08'
      .PUbOld=F14cUbiOri
      .PUFlag='S'
      .POSStk=F14cSitStk
      .POCFis=F14cCanFis
      .POCRes=0
      .POFlag='S'
*      .PSTOld='3000'
*      .PSCFis=F14cCanFis
      .PSFlag='N'             && ***** No actualiza stock????
   EndWith
   FncA.Ejecutar

   If FncA.ObjParm.PWCRtn < c_Error
      FncA.ObjParm.PWCRtn = c_Error
   EndIf
Return 

*>===========================================================================
*> CfCa_ActTrn............... Actualizar stocks tránsito
*>===========================================================================
Procedure CfCa_ActTrn
   Select CRutF14c
   With FncA.ObjParm
      .PTAcc='07'
      .PUFlag='N'
      .POCFis=0
      .POCRes=0
      .POFlag='N'
      .PSTOld='6000'
      .PSCFis=F14cCanFis
      .PSFlag='S'
   EndWith
   FncA.Ejecutar
Return

*>===========================================================================
*> CfCa_GenAlb............... Generar albarán para rutas
*>===========================================================================
Function CfCa_GenAlb
Parameters c_CodPro, c_TipDoc, c_NumDoc

*   FncA.ObjParm.PMNMov=Space(10)
*   FncA.ObjParm.PMFgHM='S'
*   FncA.ActHM
Return .T.

*>====================================================================
*> MoUb_Ubicar .............. Forzar o buscar ubicación
*>====================================================================

Procedure MoUb_Ubicar
Private n_Cantid, c_TamUbi, c_CodUbi, c_UbiDes, c_NumPal, c_CodArt, c_PicoSN, n_RegAnt
*> Proporciona la ubicación a cada una de las líneas

   *> Declara el array.-----------------------------------------------
   Declare aubparmz[17]
   Select MovUbi
   Go Top
   Do Carga_Datos With CodPro, CodArt    && Cargar parámetros ubicación del artículo

   *> Procedemos.-----------------------------------------------------
   Select MovUbi
   Go Top
   Do While !EOF()
      If ((MovUbi.UbicSN = "S") .or. (MovUbi.UbicSN = "")) .and. (MovUbi.Marca = 0)
         Skip
         Loop
      EndIf
       
      *> Guardamos datos.---------------------------------------------
      c_CodPro = MovUbi.CodPro
      c_CodArt = MovUbi.CodArt
      c_TamUbi = MovUbi.TamPal
      n_Cantid = MovUbi.CanFis
      c_UbiDes = IIf(Empty(MovUbi.UbiDes), '', MovUbi.UbiDes)
      c_NumPal = MovUbi.NumPal
      c_PicoSN = MovUbi.EspIco
      b_LAutom = .F.
      b_BlqUbi = .T.         && Si bloquea la ubicación o no
      c_CodUbi = ""          && Retorno de la ubicación destino
      c_TxtErr = ""          && Texto de porque no se puede ubicar

      n_RegAnt = RecNo()      && Guardar puntero en Normal (lo pierde).

      Do Prc_Ubi In Ora_Cu00 With c_CodPro, c_CodArt, c_TamUbi, n_Cantid, c_UbiDes, c_NumPal, ;
                 c_PicoSN, b_LAutom, b_BlqUbi, c_CodUbi, c_TxtErr

      If Empty(c_CodUbi)
         =F3_sn(1,4,"Imposible ubicar el palet " + c_NumPal)
      Endif   

      *> Grabamos la ubicación.---------------------------------------
      Select MovUbi
      Go n_RegAnt         && Recuperar puntero en Normal.
      Replace MovUbi.UbiDes With c_CodUbi
      If !Empty(MovUbi.UbiDes)
         Replace MovUbi.UbicSN With "S"
      EndIf

      *> Avanzamos el puntero.----------------------------------------
      Skip
   EndDo

   *> Posicionamos el puntero en el fichero de líneas.----------------
   Select MovUbi
   Go Top
    
Return 

Procedure HistOcup

*****>FncA.ObjParm.PONEnt=
*****>FncA.ObjParm.PMTMov=
Fnc.ObjParm.POTDoc=TIPDOC   
Fnc.ObjParm.PONdoc=NUMDOC   
Fnc.ObjParm.POLDoc=LINDOC   
*****>FncA.ObjParm.PMFDoc=
*****>FncA.ObjParm.PMDAso=
*****>FncA.ObjParm.PONPed=
*****>FncA.ObjParm.POLPed=
Fnc.ObjParm.POCArt=CODART   
Fnc.ObjParm.PONLot=NUMLOT   
Fnc.ObjParm.POSStk=SITSTK   
Fnc.ObjParm.POFCad=FECCAD   
Fnc.ObjParm.POCFis=CANFIS   
*****>FncA.ObjParm.PMRHab=
Fnc.ObjParm.PUbOld=CODUBI
*****>FncA.ObjParm.PUBNew=
Fnc.ObjParm.PONPal=NUMPAL   
Fnc.ObjParm.POTPal=TAMPAL   
Fnc.ObjParm.POFUni=UNIVEN   
Fnc.ObjParm.POFSer=UNIPAC   
Fnc.ObjParm.POFEnv=PACCAJ   
Fnc.ObjParm.POFPAL=CAJPAL   
Fnc.ObjParm.POFFab=FECCAB   
****>FncA.ObjParm.POFCal=
Fnc.ObjParm.PONAna=NUMANA   
Fnc.ObjParm.POCPro=CODPRO   
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
Fnc.ObjParm.POPico=EsPico
Fnc.ObjParm.POCAlm=_Alma
Return


Function GrabMPF16c
Parameters EntSal, TipMov
Private _ok

Fnc.ObjParm.PMEnSa = EntSal    
Fnc.ObjParm.PMFgMP = 'S'
Fnc.ObjParm.PMTMov = TipMov
Fnc.ObjParm.PMNMov = ''
Fnc.ObjParm.PMFMov = Date()
Fnc.ActMP

_ok=IIF(Fnc.ObjParm.PWCRtn >= '50',.F.,.T.)
Return _ok


Function ActOcuCand
private _ok
*>Aquí realiza la e/s , pasándole sólo el tipo de movimiento
   do HistOcup && rellena campos que se usarán al grabar movtos en histórico
   _ok=GrabMPF16c('S','3080')
   If _ok
      Fnc.ObjParm.PUbOld=MOVUBI.UbiDes
      _ok=GrabMPF16c('E','3580')
   EndIf
return _ok


*>====================================================================
*> MoUb_AceptarUbic ......... Aceptar ubicac. de Movimiento de Ubicaciones
*> Añadir impresión de etiquetas. AVC - 12.01.1999
*>====================================================================

PROCEDURE MoUb_AceptarUbic
PRIVATE n_Status, Fnc, Prm, b_ErrLin, b_ErrEnt, _ok
*> Comprobar que todos los palet con ubicación han actualizado la ubicación de entrada

   *> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   Select MovUbi
   Go Top
   Do While !EOF()
      IF Empty(MovUbi.UbiDes)
         skip 
         Loop
      EndIf     
      If !Empty(MovUbi.UbiDes) .And. MovUbi.UbicSN = "N"
         =F3_sn(1, 4, "Quedan palets pendientes de comprobar ubicación. Utilice el botón UBICAR.")
         Return
      EndIf

      _ok=ActOcuCand()
      If _ok
         =SQLCOMMIT(_ASQL)
      Else
         =SQLROLLBACK(_ASQL)
         Select MovUbi
        _lxerr=" Fallo Ubicación ocup. [Pal:" +NumPal+", Pro:"+CodPro+", Art:"  +CodArt+", Lote:"  +;
        	+NumLot+", Cad:" +FecCad+", Stock:"  +SitStk +cr
      EndIf
      
      Select MovUbi
      Skip
   EndDo

    
   *> Elimina objetos función actualización y etiquetas.--------------
   Release Prm
   Release Fnc
 
   If !Empty(_lxerr)
      =Anomalias()
   EndIf

Return

*>====================================================================
*> MoUb_CancelarUbic ........ Cancelar ubicac. de Movimiento de Ubicaciones
*>====================================================================

Procedure MoUb_CancelarUbic
Private c_CodUbi, n_Peso, n_Volu
*> Se procesa el fichero y se desubica
   *> Declara el array.-----------------------------------------------
   Declare aubparmz[17]
   Select MovUbi
   Go Top
   Do Carga_Datos With CodPro, CodArt

   Select MovUbi
   Go Top
   Do While !EOF()
      If Empty(UbiDes) .Or. UbicSN = "N"
         Skip
         Loop
      EndIf   

      *> Desubicamos.-------------------------------------------------
      Store 0 to n_Peso, n_Volu
      Do PesVolAr In Ora_Ca00 With MovUbi.CodPro, MovUbi.CodArt, MovUbi.CanFis, n_Peso, N_Volu
      l_Bi=.f.
      Select MovUbi
      c_CodUbi = MovUbi.UbiDes
      Do DesUbi In Ora_Ca00 With c_CodUbi, n_Peso, n_Volu, 1, l_bi

      Select MovUbi
      Skip
   EndDo
Return


*>====================================================================
*> Can_Movim ................ Cancelar Movimientos 
*>====================================================================


PROCEDURE Can_Movim
PARAMETERS n_posf14,L_Bien

*> Procedimiento de cancelación general de un 
*> movimiento pendiente en F14c.

   PRIVATE f_fichero, l_openf14, l_openf20, l_openf16

   *> Inicializamos la variable de salida.----------------------------
   l_bien = .F.

   *> Sentencia (de muerte) SqL.--------------------------------------
   _xier = f3_SqlExec(_asql,"Select F14cNumMov From F14c" + _em + " Where F14cNumMov ='"+ n_Posf14+_cm,'Result')
   If _xier>0
      =SqlMoreResults(_asql)
   EndIf
   Select Result
   Go Top
   If !Eof()
        
      DO CASE
         *> Entrada.-----------------------------------------------------
         CASE !EMPTY(f14cubides) .AND. EMPTY(f14cubiori)
            Do Prc_CanE with n_PosF14,l_Bien

         *> Salida.------------------------------------------------------
         CASE EMPTY(f14cubides) .AND. !EMPTY(f14cubiori)
            Do Can_MovSal With F14cCodPro,F14cCodArt,F14cUbiOri,F14cNumPal,F14cNumLot,;
                               F14cSitStk,F14cFecCad,l_Bien

         *> Salida - Entrada.--------------------------------------------
         CASE !EMPTY(f14cubides) .AND. !EMPTY(f14cubiori)

            ****>NO se puede cancelar la reposicion. 04/07/95.
            = F3_Sn(1,1,"La reposición debe ser confirmada..") && Aviso
            RETURN 

      ENDCASE

      *> Si todo el proceso es correcto.------------------------------
      *> Si hubiera lista deber¡a cancelarla.-------------------------
      IF l_bien
         Select f14c
         IF !Empty(f14cnumdes)
            *> Actualizar lista de trabajo, eliminar movimientos.-----
            Num_Des=F14cnumdes
            Do Prc_canl with Num_des,l_Bien
         Endif   

         *> Borramos el registro del f14c si es necesario.------------
         Select f14c
         GOTO n_posf14
         DELETE

      ENDIF

      *> Cerrar la ventana de proceso.--------------------------------
      WAIT CLEAR

   ENDIF

   *> Abrimos fichero anterior.---------------------------------------
   Select (f_fichero)

RETURN 

*-----------------------*
 PROCEDURE Can_MovSal
*-----------------------*
    *> 
    *> Entrada : Propietario
    *>           Artículo
    *>           Código de ubicación 
    *>           Palet
    *>           Lote
    *>           Situación Stock
    *>           Fecha de Caducidad
    
    *> Resultado : L_bien = .t. Se pudo desocupar la ubicación de  estos datos
    *>                    = .f. No se consiguio

PARAMETERS C_CodPro,c_articulo,codigo_ubi, c_numpalet, c_num_Lot ,C_sitStk, F_FecCad ,l_Bien
PRIVATE C_CodPro,C_articulo,codigo_ubi,c_numpalet,c_num_lot,c_sitStk,F_fecCad,l_bien
*> Cancelar una ocupación POR UNA CANTIDAD DETERMINADA 

   l_bien    = .F.

   *> Mensaje de desocupar.-------------------------------------------
   =WaitWindow("Cancelando la reserva " + codigo_ubi)

   *> Sentencia (de muerte) SqL.--------------------------------------
   F_Where= " F16cCodPro = '" + c_codPro + _cm + ;
            " And F16cCodArt = '" + c_articulo + _cm + ;
            " And F16cCodubi = '" + codigo_ubi + _cm + ;
            " And F16cNumPal = '" + c_numpalet + _cm + ;
            " And F16cNumLot = '" + c_num_lot  + _cm + ;
            " And F16cSitStk = '" + c_SitStk   + _cm + ;
            " And F16cFecCad = " + _GCD(F_FecCad) + ;
            _Bloqueo

   _xier = f3_SqlExec(_asql,"Select * From F16c" + _em + " Where "+ F_where,'Result') 
   If _xier>0
      =SqlMoreResults(_asql)
   Else
     =Aerror(err)
     If Err(5)= 54
        = F3_Sn(1,1,"La Ocupación está bloqueada..") && Aviso
        Return
     EndIf 
   EndIf
   
   Select Result
   Go Top

   *> Encontramos registro.--------------------------------------------
   If !Eof()
      F_Update = "Update F16c"  + _em + ;
                 "Set F16cCanRes = "  + Str(F16cCanRes - n_cantidad) + F_Where + ;
                 " And F16cCanRes >= " + Str(n_cantidad) 

      _xier = f3_SqlExec(_asql,F_Update) 
      If _xier>0
         l_bien = .T.
 
         F_Delete = "Delete From f16c" + _em + ;
                    F_Where + " and F16cCanres = 0 and F16cCanfis = 0" 

        _xier = f3_SqlExec(_asql,F_Delete) 
               
       EndIf

       If _xier > 0
         _xier=SqlCommit(_asql)
       Else 
         =aError(_odbc_err)
         lx=_odbc_err(2)+_odbc_err(3)+space(180)
         =f3_sn(1,1,left(lx,60),subs(lx,61),subs(lx,121))
         _xier=SqlRollback(_aSql)
       EndIf

   ENDIF
    
   *> Cancelamos la ventana.------------------------------------------
   Wait Clear
Return

*>----------------------------------------------------------
*> Acumula cantidad reservada de MPs de una ubicación.
*>----------------------------------------------------------
Function CResF14c
Parameter CodPro, CodArt, CodUbi, NumPal, NumLot, SitStk

Private f_where, Err, CRes

f_where = "Select " + _GCN("Sum(F14cCanFis)") + " As CRes" + ;
          " From F14c" + _em + ;
          " Where F14cCodPro='" + CodPro + "'" + ;
          " And   F14cCodArt='" + CodArt + "'" + ;
          " And   F14cUbiOri='" + CodUbi + "'" + ;
          " And   F14cNumLot='" + NumLot + "'" + ;
          " And   F14cSitStk='" + SitStk + "'" + ;
          " And " + _GCSS("F14cTipMov", 1, 1) + "='2'"

*         " And   F14cNumPal='" + NumPal + "'" + ;

Err = f3_SqlExec(_ASql, f_where) 
=SqlMoreResults(_ASql)

If Err<=0
   CRes = 0
EndIf

Return CRes

*>
*> Eliminación de NULLs de la tabla activa.----------------------------
*> AVC - 01.07.1999
Function DelNulls

   Parameters _alias
   Private _bb, _cc, _inx

   _oldAlias = Alias()
   If Type('_alias') # 'U'
      Select(_alias)
   EndIf
   
   _bb=FCount()                         && Nº de campos de la tabla
   For _inx = 1 To _bb
      _cc = Field(_inx)                 && Nombre del campo
      Do Case
         Case Type('&_cc')='C'
            Replace &_cc With ' '
         Case Type('&_cc')='D'
            Replace &_cc With {  .  .    }
         Case Type('&_cc')='N'
            Replace &_cc With 0
      EndCase
   EndFor

   Select (_oldAlias)
Return .T.

*>====================================================================
*> Pale_AcePale ......... Aceptar ubicación de Paletización.
*> Añadir impresión de etiquetas. AVC - 12.01.1999
*> Llamado desde ENTRRECP - Montaje de palets.
*>====================================================================

Procedure Pale_AcePale
Parameters NumEnt
Private n_Status, Fnc, Prm
Private _HayEtiArt, _HayEtiPal

*> Comprobar que todos los palet con ubicación han actualizado la ubicación de entrada

   Store .F. To HayAlb


   *> Crear objetos de parámetros y función, e inicializar parámetros
   Prm = CreateObject("OraPrmActz")
   Fnc = CreateObject("OraFncActz")
   Fnc.ObjParm = Prm
   Fnc.ObjParm.Inicializar

   *> Crear objeto parámetros impresión de etiquetas.
   FncEti = CreateObject("OraFncEtiq")
   FncEti.Inicializar

   *> Declara el array y carga parámetros ubicación.------------------
   Declare aubparmz[17]
   Select RECPAL
   Go Top
   Do Carga_Datos With RECPAL.F18nCodPro, RECPAL.F18nCodArt    && Cargar parámetros ubicación del artículo

   *> Si tenemos que imprimir.----------------------------------------
   If (aubparmz[11] = "S") .OR. (aubparmz[12] = "S")
       Select RECPAL
       _Label1 = "Listado de Paletización"
*       Do Form St3Imp2 With _Label1,'EntrPaleL1','LINPAL'
   EndIf

   *> Crea los movimientos.-------------------------------------------
   Select RECPAL
   Go Top
   Do While !Eof()

      *> Inicializamos los datos.-------------------------------------
      Select RECPAL

      With Fnc.ObjParm
         .PUbOld = Space(14)
         .PUFlag = "N"           && Ubicación se actualiza al buscarla
         .POTDoc = RECPAL.F18nTipDoc
         .PONDoc = RECPAL.F18nNumDoc
         .POLDoc = Val(RECPAL.F18nLinDoc)
         .PONPed = Space(15)
         .POFMov = Date()
         .POCPro = RECPAL.F18nCodPro
         .POCArt = RECPAL.F18nCodArt
         .PONLot = RECPAL.F18nNumLot
         .POSStk = RECPAL.F18nSitStk
         .POFCad = RECPAL.F18nFecCad
         .POCFis = RECPAL.F18nCanFis
         .POCUbi = Space(14)
         .PONPal = RECPAL.F18nNumPed
         .POTPal = RECPAL.F08cTamAbi
         .POFUni = 1
         .POFSer = RECPAL.F08cUniPac
         .POFEnv = RECPAL.F08cPacCaj
         .POFPal = RECPAL.F08cCajPal
         .POFFab = Date()
         .PONEnt = NumEnt
         .POFEnt = Date()
         .POFCal = {  .  .  }
         .POPico = "S"
         .POFlag = "N"
         .POCAlm = _Alma

         .PSCPro = RECPAL.F18nCodPro
         .PSCArt = RECPAL.F18nCodArt
         .PSCAlm = _Alma
         .PStOld = "5000"          && Pendiente de ubicar
         .PStNew = Space(4)
         .PSCFis = RECPAL.F18nCanFis
         .PSFlag = "S"

         .PMTMov = "1000"
         .PMFDoc = Date()
         .PMNMov = Space(10)
         .PMFMov = Date()
         .PMCOpe = '0000'
         .PMFgMP = "S"
         .PMFgHM = "N"
      EndWith

      *> Generamos el movimiento.-------------------------------------
      =WaitWindow('Generando Movimiento Pendiente: ' + RECPAL.F18nNumPed)

      Fnc.ActMP
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         _LxErr = 'Error generando Movimiento Pendiente ' + cr + ;
                  'DOCUMENTO: ' + RECPAL.F18nTipDoc + '-' + RECPAL.F18nNumDoc + cr + ;
                  ' ARTICULO: ' + RECPAL.F18nCodArt + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Actualizamos situación de stock Pendiente de ubicar.(Sumamos).---
      Fnc.ObjParm.PTAcc = "07"    && Entrada
      Fnc.Ejecutar
      n_Status = Fnc.ObjParm.PWCRtn
      If n_Status >= '50'      
         _LxErr = 'Error actualizando stock pendiente de ubicar.' + cr + ;
                  'DOCUMENTO: ' + RECPAL.F18nTipDoc + '-' + RECPAL.F18nNumDoc + cr + ;
                  ' ARTICULO: ' + RECPAL.F18nCodArt + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf
  
      *> Desglose automático.-----------------------------------------
      If aubparmz[10] = "S"

      EndIf

      *> Confirmación automática.-------------------------------------
      aubparmz[09] = "N"
      If aubparmz[09] = "S"
         Select f14c
         LRet = .f.
         NReg = RecNo()

         Do Prc_Conf in Lib_Ca00 with NReg, LRet
         If !LRet
            _LxErr = 'Error en la confirmación automatica del palet Nº: ' + a_datos(01) + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf

         *> Actualizamos situación de stock Pendiente de ubicar.(Restamos).---
         L_Resultad=.f.
         Do actualst In Lib_ca00 With A_Datos(13), ;
                                      A_Datos(15), ;
                                      "5000", ;
                                      .F., ;
                                      l_Resultad
         If !L_Resultad
            _LxErr = 'Error actualizando stock pendiente de ubicar (Automático) ' + a_datos(01) + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf

         *> Actualizamos situación de stock Entrada.(Sumamos).---
         L_Resultad=.f.
         Do actualst In Lib_ca00 With A_Datos(13), ;
                                      A_Datos(15), ;
                                      A_Datos(14), ;
                                      .T., ;
                                      l_Resultad
         If !L_Resultad
            _LxErr = 'Error actualizando stock pendiente de entrada (Automático) ' + a_datos(01) + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf

      *> Actualizar línea detalle del albarán entrada.----------------
      If !Empty(Fnc.ObjParm.PONEnt)
         strSql = "Update F18n" + _em + " Set F18nEstado='2'" + ;
                  " Where F18nNumEnt='" + Fnc.ObjParm.PONEnt + "'" + ;
                  " And   F18nCodPro='" + Fnc.ObjParm.POCPro + "'" + ;
                  " And   F18nTipDoc='" + Fnc.ObjParm.POTDoc + "'" + ;
                  " And   F18nNumDoc='" + Fnc.ObjParm.PONDoc + "'" + ;
                  " And   F18nCodArt='" + Fnc.ObjParm.POCArt + "'" + ;
                  " And   F18nNumLot='" + Fnc.ObjParm.PONLot + "'" + ;
                  " And   F18nSitStk='" + Fnc.ObjParm.POSStk + "'"
         _xier = f3_SqlExec(_aSql, strSql)
         If _xier <= 0
            _LxErr = 'Error actualizando detalle Albarán de Entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf

      *> Actualizar la CABECERA del ALBARAN. Leer el estado de las líneas.
      lx_upd = "Select " + _GCN("Count(*)") + " As FLinPed From F18n" + _em
      lx_Where=" Where F18nNumEnt='" + NumEnt + _cm + ;
               " And   F18nEstado<'2'"
      _xier = f3_SqlExec(_Asql, lx_Upd+lx_Where)
      If _xier <= 0
         _LxErr = 'Error control estado líneas (2) Documento Nº: ' + RECPAL.F18nNumDoc + cr + ;
                  'MENSAJE: ' + Message() + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf
         
      =SqlMoreResults(_ASql)
      lx_Upd = "Update F18m" + _em + ;
               " Set F18mEstado='" + ;
               Iif(FLinPed==0, '2', '1') + "'"
      lx_Where=" Where F18mNumEnt='" + NumEnt + _cm
      _xier = f3_SqlExec(_Asql, lx_Upd+lx_Where)
      If _xier < 0
         _LxErr = 'Error actualizando cabecera albarán Nº: ' + NumEnt + cr + ;
                  'MENSAJE: ' + Message() + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Imprimir etiqueta caja.-------------------------------------
      If "N" = "S"
*     If F08c.F08cEtiCaj = "S" 
         Select LINPAL
         Go Top

         Select F08c
         Seek LINPAL.CodArt

         Select LINPAL
         Go Top
         Do While !Eof()
            If !Empty(UbiFor)
               NumEti=  Str(Alltrim(Ceiling(CanFis/m.UniVen*m.UniPac*m.PacCaj)))
               Do ImpPro With F08c.F08cCodigo, NumEti In Imprime
            EndIf 

            Select RECPAL
            Skip
         EndDo
      EndIf

      *> Seleccionamos y avanzamos el fichero de líneas.--------------
      Store .T. To HayAlb
      Select RECPAL
      Skip
   EndDo

   *>
   If HayAlb
      =SqlCommit(_ASql)
      =f3_sn(1, 1, 'Realizada paletización de material Albarán de Entrada Nº: ' + m.NumEnt)
   Else
      _LxErr = 'No se ha podido realizar la Paletización de Material.' + cr
      =Anomalias()
      =SqlRollBack(_ASql)
   EndIf   

   *> Elimina objetos función actualización y etiquetas.--------------
   Release Prm
   Release Fnc
   Release FncEti
Return

*>-----------------------------------------------------------------
*> Grabar mensajes de error.
*> Genera F99c, como OraFncActz.anom, pero con datos restringidos.
*> Recibe _LxErr, variable global con el texto de la incidencia.
*> Display de mensaje, según el modo de trabajo del programa.
*>-----------------------------------------------------------------

Procedure Anomalias
Private oPRC

_oldSele = Alias()

If Type('_BACKGROUND')=='U'
   Store .F. To _BACKGROUND
EndIf

oPRC = CreateObject('procaot')

If !File(Sys(5) + CurDir() + 'F99c.Dbf')
   Create Table F99c(F99cCodUsr C(6), ;
                     F99cCodAlm C(4), ;
                     F99cCodPro C(6), ;
                     F99cCodAno C(2), ;
                     F99cTipAno C(10), ;
                     F99cDesAno Memo, ;
                     F99cPrgAno Memo, ;
                     F99cErrAno Memo, ;
                     F99cProces C(10), ;
                     F99cPtAcc  C(2), ;
                     F99cPEnSa  C(1), ;
                     F99cFecAno D, ;
                     F99cHorAno C(8), ;
                     F99cTipDoc C(4), ;
                     F99cNumDoc C(13), ;
                     F99cLinDoc C(4), ;
                     F99cCodArt C(13), ;
                     F99cNumLst C(6), ;
                     F99cCanFis N(10, 0), ;
                     F99cCanRes N(10, 0), ;
                     F99cCodUbi C(14), ;
                     F99cNumLot C(15), ;
                     F99cFecCad DateTime, ;
                     F99cSitStk C(4), ;
                     F99cNumMov C(10), ;
                     F99cCUbiPr C(14), ;
                     F99cCFisPr N(10, 0), ;
                     F99cCResPr N(10, 0), ;
                     F99cSStkPr C(4), ;
                     F99cCUbiPo C(14), ;
                     F99cCFisPo N(10, 0), ;
                     F99cCResPo N(10, 0), ;
                     F99cSStkPo C(4))
EndIf

If Used('F99c')
   Use In F99c
EndIf

Select 0
Use F99c
Append Blank
Replace F99cCodUsr With _UsrCod, ;
        F99cCodAlm With _Alma, ;
        F99cCodPro With _Procaot, ;
        F99cDesAno With _LxErr, ;
        F99cPrgAno With Sys(16, Program(-1)), ;
        F99cFecAno With Date(), ;
        F99cHorAno With Time()

Use In F99c

=oPRC.WriteIncidencia('P', 'Error en ORA_PROC', _LxErr, )

*> Visualizar en pantalla la incidencia, según el modo de trabajo.
If !_BACKGROUND
   Do Form St3Inc With .T.
EndIf

If !Empty(_oldSele)
   Select (_oldSele)
EndIf
Return

*>-----------------------------------------------------------------
*> Visualización de mensajes tipo Wait Window.
*> Display de mensaje, según el modo de trabajo del programa.
*>-----------------------------------------------------------------

Procedure WaitWindow
Parameters cMensaje, lWait, nTimeOut
Private oPRC

_oldSele = Alias()

If Type('_BACKGROUND')=='U'
   Store .F. To _BACKGROUND
EndIf

If Type('nTimeOut') # 'N'
   nTimeOut = 0
EndIf

*> Visualizar en pantalla el mensaje, según el modo de trabajo.
If !_BACKGROUND
   Do Case
      Case !lWait
         Wait Window cMensaje NoWait

      Case nTimeOut > 0
         Wait Window cMensaje TimeOut nTimeOut

      Otherwise
         Wait Window cMensaje
   EndCase
EndIf

If !Empty(_oldSele)
   Select (_oldSele)
EndIf

Return
