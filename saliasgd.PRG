*> Capçalera

   *> Descripcio ................. Asignar Desasignar documentos de salida.
   *> M¢dulo ..................... SaliAsgD, a partir de OPC06B.
   *> Llenguatge ................. FoxPro 2.0
   *> Sistema Operatiu ........... MS-DOS Ver 3.30 o superior
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Dise¤o ..................... Abraham Vi¤as Cortés
   *> Programador ................ Abraham Vi¤as Cortés
   *> Data d'inici ............... 08.10.1998
   *> Data de fi ................. 08.10.1998

*> Utilitats

   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*>   - Selecci¢n por fecha documento. AVC - 30.07.1998
*>   - Preasignar los documentos seleccionados. AVC - 30.08.1998
*>

*>           (C)Copyright ALISFOUR 2004 BARCELONA

*> Procedimiento para actualizar los documentos seleccionados.-----
*> Los mensajes se tratan en el form que llama a este módulo.------
*> Recibe la operación:
*>    - 'A' --> Asignar documentos.
*>    - 'D' --> Desasignar documentos.
Procedure GrbDocS

   Parameters TipOper

   Private Estado, Stat, bb

   *> Comprobar que hay registros para procesar.-------------------
   If !f3_sn(2, 1, 'Confirma los datos introducidos?')
      Return .F.
   EndIf

   *> Definir el estado a grabar en cabecera documento.
   Do Case
      Case TipOper == 'A'
         Stat = '1'
      Case TipOper == 'D'
         Stat = '0'
      OtherWise
         =f3_sn(1, 1, 'Error en operación a realizar.')
         =SqlRollBack(_ASql)
         Return .F.
   EndCase

   *> Bucle de lectura del grid de documentos.---------------------
   _LxErr = ''
   Select 'ASGDOC'

   Go Top
   Locate For Marca = 1
   Do While !Eof()
      CodPro = F24cCodPro
      TipDoc = F24cTipDoc
      NumDoc = F24cNumDoc

      *> Primer paso: Capturar la cabecera del documento.
      _Selec = 'Select * From F24c' + _em + ' Where'
      _Selec = _Selec + " F24cCodPro='" + CodPro + "'"
      _Selec = _Selec + " And F24cTipDoc='" + TipDoc + "'"
      _Selec = _Selec + " And F24cNumDoc='" + NumDoc + "'"

      _xier = SqlExec(_ASql, _Selec, 'C_F24c')
      If _xier <0
         _LxErr = 'Error leyendo cabecera de documento: ' + F24cNumDoc + cr + ;
                  'MENSAJE: ' + Message() + cr
         Return .F.
      EndIf

      =SqlMoreResults(_ASql)

      *> Comprobar, por última vez, si el documento se puede asignar.
      Estado = Iif(TipOper=='A', StockOk(Alias()), .T.)
      If Estado
         _Selec = 'Select * From F24l' + _em + ' Where'
         _Selec = _Selec + " F24lCodPro='" + CodPro + "'"
         _Selec = _Selec + " And F24lTipDoc='" + TipDoc + "'"
         _Selec = _Selec + " And F24lNumDoc='" + NumDoc + "'"

         _xier = SqlExec(_ASql, _Selec, 'C_F24l')
         If _xier < 0
            _LxErr = 'Error leyendo detalle de documento: ' + F24cNumDoc + cr + ;
                     'MENSAJE: ' + Message() + cr
            Return .F.
         EndIf

         =SqlMoreResults(_ASql)

         *> Crear objeto para actualización.
         Prm = CreateObject("OraPrmActz")              && Parámetros
         Fnc = CreateObject("OraFncActz")              && Procedimientos
         Fnc.ObjParm = Prm
         Fnc.ObjParm.Inicializar

         *> Bucle de actualización del stock.
         Select C_F24l
         Go Top
         Do While !Eof()
            With Fnc
               .ObjParm.Inicializar
               .ObjParm.PUFlag = 'N'
               .ObjParm.POFlag = 'N'

               .ObjParm.PSCPro = CodPro
               .ObjParm.PSCArt = F24lCodArt
               .ObjParm.PSCAlm = _Alma
               .ObjParm.PSTOld = '7000'
               .ObjParm.PSTNew = Space(4)
               .ObjParm.PSCFis = F24lCanDoc
               .ObjParm.PSFlag = 'S'

               .ObjParm.PTAcc = Iif((TipOper=='A'), '07', '08')
               .Ejecutar
            EndWith

            *> Leer la siguiente línea del documento actual.
            Select C_F24l
            Skip
         EndDo

         *> Elimina objetos función actualización.
         Release Prm
         Release Fnc
      Else
         _LxErr = _LxErr + 'Documento: ' + NumDoc + ' no se puede asignar. Stock insuficiente.' + cr
      EndIf

      *> Actualizar la cabecera del documento.---------------------
      If Estado
         _Selec =          "Update F24c" + _em + " Set F24cFlgEst='" + Stat + "' Where"
         _Selec = _Selec + "     F24cCodPro='" + CodPro + "'"
         _Selec = _Selec + " And F24cTipDoc='" + TipDoc + "'"
         _Selec = _Selec + " And F24cNumDoc='" + NumDoc + "'"

         _xier = SqlExec(_ASql, _Selec, 'C_F24l')
         =SqlMoreResults(_ASql)
         If _xier <0
            _LxErr = _LxErr + 'Error en actualización cabecera documento' + cr
            Return .F.
         EndIf
      EndIf

      *> Actualizar string de documentos tratados.
      _LxErr = _LxErr + 'Documento: ' + NumDoc + ' Procesado' + cr

      *> Siguiente documento de salida a procesar.-----------------
      Select 'ASGDOC'
      Skip
   EndDo

   *> Liberar, si cal, ficheros de trabajo.------------------------
   If Used('C_F24c')
      Use In C_F24c
   EndIf
   If Used('C_F24l')
      Use In C_F24l
   EndIf

Return .T.

*> Función para ver el stock de una línea de artículo.-------------
Procedure VerStock

   Parameters LINASG
   StkDis = VerStockDis(LINASG)

   TxtStk =          'Stock Físico: ' + Transform(StkFis, '999,999') + cr
   TxtStk = TxtStk + 'Stock Reserv: ' + Transform(StkRes, '999,999') + cr
   TxtStk = TxtStk + 'Stock Asign.: ' + Transform(StkPed, '999,999') + cr
   TxtStk = TxtStk + 'Stock Dispon: ' + Transform(StkDis, '999,999') + cr
   TxtStk = TxtStk + 'Stock Preasg: ' + Transform(StkPre, '999,999')

   Wait Clear
   Wait Window TxtStk

Return

*> Función para ver si un pedido completo se puede asignar.--------
*> Recibe el alias con cabecera pedidos.
Procedure StockOk

   Parameters WtrLp
   Private Resultado

   Store 0 To StkDis
   Store .F. To Resultado

   *> Leer los datos del documento en cursor F24l.-------------------
   _Selec = 'Select * From F24l' + _em + ' Where'
   _Selec = _Selec + " F24lCodPro='" + &WtrLp->F24cCodPro + "'"
   _Selec = _Selec + " And F24lTipDoc='" + &WtrLp->F24cTipDoc + "'"
   _Selec = _Selec + " And F24lNumDoc='" + &WtrLp->F24cNumDoc + "'"

   _xier = SqlExec(_ASql, _Selec, 'C_F24l')
   If _xier <0
      _LxErr = 'Error calculando stock disponible. Documento: ' + F24lNumDoc + cr
      Return Resultado
   EndIf
   =SqlMoreResults(_ASql)

   *> Ver si este pedido se puede asignar.---------------------------
   Select C_F24l
   Go Top

   Do While !Eof()
      StkDis = VerStockDis(Alias())
      If StkDis < C_F24l.F24lCanDoc
         Store .F. To Resultado
         Exit
      Else
         Store .T. To Resultado
      EndIf

      Select C_F24l
      Skip
   EndDo

   *> Liberar, si cal, ficheros de trabajo.------------------------
   If Used('C_F24l')
      Use In C_F24l
   EndIf

Return Resultado

*> Función para calcular el stock disponible de un artículo.-------
*> Recibe el alias del fichero de detalle.
Function VerStockDis

   Parameters AliasDt

   *> Inicializar los stocks.--------------------------------------
   Store 0 To StkFis                        && Stock físico.
   Store 0 To StkRes                        && Stock reservado.
   Store 0 To StkPed                        && Stock asignado.
   Store 0 To StkPre                        && Stock preasignado.
   Store 0 To StkDis                        && Stock disponible.

   *> Leer el stock físico del artículo.---------------------------
   m.F13cCodAlm = _Alma
   m.F13cCodPro = &AliasDt->F24lCodPro
   m.F13cCodArt = &AliasDt->F24lCodArt
   m.F13cSitStk = '1000'
   _Ok = F3_Seek("F13C", ;
                 '', ;
                 "StkFis", ;
                 "StkFis=F13cCantid")

   *> Leer el stock reservado del artículo.------------------------
   m.F13cSitStk = '5500'
   _Ok = F3_Seek("F13C", ;
                 '', ;
                 "StkRes", ;
                 "StkRes=F13cCantid")

   *> Leer el stock en pedidos asignados.--------------------------
   m.F13cSitStk = '7000'
   _Ok = F3_Seek("F13C", ;
                 '', ;
                 "StkPed", ;
                 "StkPed=F13cCantid")

   *> Leer el stock en pedidos pre-asignados por el programa.------
   m.F13cSitStk = '7001'
   _Ok = F3_Seek("F13C", ;
                 '', ;
                 "StkPre", ;
                 "StkPre=F13cCantid")
 
   *> Calcular el stock disponible real.
   StkDis = StkFis - StkRes - StkPed + StkPre
   StkDis = IIf(StkDis < 0, 0, StkDis)

Return StkDis

*> Procedimiento para actualizar la situación 7001 del pedido actual.
*> Recibe el alias del fichero de cabecera.
Procedure UpdateDoc

   Parameters WtrLp

   *> Leer los datos del documento en cursor F24l.-------------------
   _Selec = "Select * From F24l" + _em + " Where "
   _Selec = _Selec + " F24lCodPro='" + &WtrLp->F24cCodPro + "'"
   _Selec = _Selec + " And F24lTipDoc='" + &WtrLp->F24cTipDoc + "'"
   _Selec = _Selec + " And F24lNumDoc='" + &WtrLp->F24cNumDoc + "'"

   _xier = SqlExec(_ASql, _Selec, 'C_F24l')
   =SqlMoreResults(_ASql)
   If _xier <0
      _LxErr = 'Error en actualización de detalle documentos' + cr
      Do Form St3Inc
      _LxErr = ''
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   *> Actuializar la situación 7001 para cada línea del pedido.------
   Select C_F24l
   Go Top
   Do While !Eof()
      m.F13cCodAlm = _Alma
      m.F13cCodPro = F24lCodPro
      m.F13cCodArt = F24lCodArt
      m.F13cSitStk = '7001'

      _Ok = F3_Seek("F13C")
      If _Ok
         m.F13cCantid = F13cCantid + F24lCanDoc
         =F3_Upd("F13C")
      Else
         m.F13cCantid = F24lCanDoc
         =F3_Ins("F13C")
      EndIf

      Select C_F24l
      Skip
   EndDo

   *> Liberar, si cal, ficheros de trabajo.------------------------
   If Used('C_F24l')
      Use In C_F24l
   EndIf

Return .T.

*> Procedimiento para borrar las situaciones 7001 de todos los ----
*> artículos.------------------------------------------------------
Procedure Clear7001

   _xier = SqlExec(_ASql, "Delete From F13c" + _em + " Where F13cSitStk='7001'")
   If _xier <0
      =f3_sn(1, 1, 'Error en inicialización de stock pre-asignado.')
      =SqlRollBack(_ASql)
   Else
      =SqlCommit(_ASql)
   EndIf

Return

*> Procedimiento para borrar la situación 7001 del pedido actual.
*> Recibe el alias del fichero de cabecera.
Procedure ClearDoc

   Parameters WtrLp

   *> Leer los datos del documento en cursor F24l.-------------------
   _Selec = 'Select * From F24l' + _em + ' Where'
   _Selec = _Selec + " F24lCodPro='" + &WtrLp->F24cCodPro + "'"
   _Selec = _Selec + " And F24lTipDoc='" + &WtrLp->F24cTipDoc + "'"
   _Selec = _Selec + " And F24lNumDoc='" + &WtrLp->F24cNumDoc + "'"

   _xier = SqlExec(_ASql, _Selec, 'C_F24l')
   If _xier <0
      _LxErr = 'Error en borrado stock preasignado' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      =SqlRollBack(_ASql)
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

   *> Actuializar la situación 7001 para cada línea del pedido.------
   Select C_F24l
   Go Top

   Do While !Eof()
      m.F13cCodAlm = _Alma
      m.F13cCodPro = F24lCodPro
      m.F13cCodArt = F24lCodArt
      m.F13cSitStk = '7001'

      _Ok = F3_Seek("F13C")
      If _Ok
         m.F13cCantid = F13cCantid - F24lCanDoc
         If m.F13cCantid > 0
            =F3_Upd("F13C")
         Else
            =F3_Baja("F13C", 'CODIGO')
         EndIf
      EndIf

      Select C_F24l
      Skip
   EndDo

   *> Liberar, si cal, ficheros de trabajo.------------------------
   If Used('C_F24l')
      Use In C_F24l
   EndIf

Return .T.
