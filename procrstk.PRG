*>
*> Programa: PROCRSTK.PRG
*> Descripción  .............. Procesos varios de recálculo de stocks.


*> Módulos ................... ProcRStk      (Recálculo general de stock de UN producto)
*>                             RStkCFis      (Recálculo de stock físico en ocupaciones)
*>                             RStkCRes      (Recálculo de stock reservado en ocupaciones)
*>                             RStkEnt       (Recálculo de stock pendiente de ubicar)
*>                             RStkAsg       (Recálculo de stock preasignado)
*>                             RStkCResUbi   (Recálculo de stock reservado de una ubicación)
*>                             RStkCResOcu   (Recálculo de stock reservado de una ocupación)

*>-----------------------------------------------------------------------------------------
*> Iniciar proceso de recálculo de stocks.
*> Corregir cálculo cantidad reservada en F16c. AVC - 29.12.1999
*> Separar en función aparte. AVC - 03.01.2001
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodArt ----> Artículo.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*>-----------------------------------------------------------------------------------------

Function ProcRStk
Parameters cCodPro, cCodArt, lShowMsg

Private XSelect, XFrom, XWhere, _XGroup, XBlq, F_Selec, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

*> Borrar las situaciones de stock que se van a procesar.
*> '1xxx' ---> Stock físico.
*> '2000' ---> Reservado.
*> '8000' ---> Control de calidad.
*> '5000' ---> Pendiente de ubicar (MPs).
*> '7000' ---> Stock pre-asignado.
*>

*> Leer situaciones de stock.
*XSelect= "SELECT *"
*XFrom  = " FROM F13c" + _em
*XWhere = " WHERE F13cCodPro='" + cCodPro + "' AND F13cCodArt='" + cCodArt + "'"
*XWhere = XWhere + " AND (SubStr(F13cSitStk, 1, 1) IN ('1', '5', '7') OR "
*XWhere = XWhere + " F13cSitStk IN ('2000', '8000'))"
*XGroup = ""
*XBlq   = ""
*
*F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
*_xier = f3_SqlExec(_ASql, F_Selec, 'CF13c')
*If _xier < 0
*   _LxErr = _LxErr + 'Error bloqueando situaciones de stock' + cr + ;
*                     'MENSAJE: ' + Message() + cr
*
*   *> Devolver control al programa.
*   Return .F.
*EndIf
*
*Use In CF13c

*> Borrar los registros del F13c a recalcular.
XSelect= "DELETE"
XFrom  = " FROM F13c" + _em
XWhere = " WHERE F13cCodPro='" + cCodPro + "' AND F13cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND (" + _GCSS("F13cSitStk", 1, 1) + ") IN ('1', '5', '7') OR "
XWhere = XWhere + " F13cSitStk IN ('2000', '8000'))"

If lShowMsg
   Wait Window 'Inicializando situaciones de stock. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando situaciones de stock' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bloquear las ocupaciones que se van a leer en el proceso.
*> Se bloquea previamente, pues el GROUP BY no lo permite.
XSelect= "SELECT * "
XFrom  = " FROM F16c" + _em
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XGroup = ""
XBlq   = _Bloqueo

*>
*> NO bloquear ocupaciones.
*>

*F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
*_xier = f3_SqlExec(_ASql, F_Selec, 'F16C')
*If _xier < 0
*   _LxErr = 'Error bloqueando ocupaciones de almacén' + cr + ;
*            'MENSAJE: ' + Message() + cr
*
*   *> Devolver control al programa.
*   Do Form St3Inc
*   _LxErr = ''
*   =SqlRollBack(_ASql)
*   Return
*EndIf
*

*>
*> Generar el cursor con los acumulados de las ocupaciones.
XSelect= "SELECT F16cCodPro, F16cCodArt, F16cSitStk, Sum(F16cCanFis) F16cCanFis, Sum(F16cCanRes) F16cCanRes"
XFrom  = " FROM F16c" + _em
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XGroup = " GROUP BY F16cCodPro, F16cCodArt, F16cSitStk"
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo ocupaciones de almacén. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF16C')
If _xier < 0
   _LxErr = _LxErr + 'Error leyendo ocupaciones de almacén' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de las ocupaciones.
Select CF16C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock: " + F16cCodPro + " - " + F16cCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF16C.F16cCodPro
   m.F13cCodArt = CF16C.F16cCodArt
   m.F13cSitStk = CF16C.F16cSitStk
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF16C.F16cCanFis
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF16C.F16cCodPro, ;
              F13cCodArt With CF16C.F16cCodArt, ;
              F13cCantid With CF16C.F16cCanFis, ;
              F13cSitStk With CF16C.F16cSitStk
   EndIf

   *> Actualizar el registro de F13c (cantidad física).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar situación de stock' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Leer la siguiente ocupación.
   Select CF16C
   Skip
EndDo

Use In CF16C

*>
*> Borrar la cantidad reservada de las ocupaciones.
*>
XSelect= "UPDATE F16c" + _em
XSet   = " SET F16cCanRes=0"
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"

If lShowMsg
   Wait Window 'Inicializando cantidades reservadas en ocupaciones. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XSet + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando cantidades reservadas' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*>
*> Generar el cursor con los acumulados de los MPs de salida.
XSelect= "SELECT F14cCodPro, F14cCodArt, F14cUbiOri, F14cNumLot, F14cSitStk, Sum(F14cCanFis) F14cCanFis"
XFrom  = " FROM F14c" + _em
XWhere = " WHERE F14cCodPro='" + cCodPro + "' AND F14cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND " + _GCSS("F14cTipMov", 1, 1) + " = '2'"
XGroup = " GROUP BY F14cCodPro, F14cCodArt, F14cUbiOri, F14cNumLot, F14cSitStk"
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo movimientos pendientes de preparación. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF14C')

If _xier < 0
   _LxErr = _LxErr + 'Error leyendo movimientos pendientes de preparación' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de los MPs de salida.
Select CF14C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock reservado: " + F14cCodPro + " - " + F14cCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF14C.F14cCodPro
   m.F13cCodArt = CF14C.F14cCodArt
   m.F13cSitStk = '2000'
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF14C.F14cCanFis
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF14C.F14cCodPro, ;
              F13cCodArt With CF14C.F14cCodArt, ;
              F13cCantid With CF14C.F14cCanFis, ;
              F13cSitStk With '2000'
   EndIf

   *> Actualizar el registro de F13c (cantidad reservada).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar stock reservado' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Actualizar la cantidad reservada de la ocupación.
   XSelect = "SELECT *"
   XFrom   = " FROM F16c" + _em
   XWhere  = " F16cCodPro='" + CF14C.F14cCodPro + "'"
   XWhere  = XWhere + " AND F16cCodArt='" + CF14C.F14cCodArt + "'"
   XWhere  = XWhere + " AND F16cCodUbi='" + CF14C.F14cUbiOri + "'"
*  XWhere  = XWhere + " AND F16cNumPal='" + CF14C.F14cNumPal + "'"
   XWhere  = XWhere + " AND F16cNumLot='" + CF14C.F14cNumLot + "'"
   XWhere  = XWhere + " AND F16cSitStk='" + CF14C.F14cSitStk + "'"
   XGroup  = ""
   XBlq    = _Bloqueo

   F_Selec = XSelect + XFrom + " WHERE " + XWhere + XGroup + XBlq
   _xier = f3_SqlExec(_ASql, F_Selec, 'CF16C')

   If _xier =< 0
      _LxErr = _LxErr + 'Error calculando datos para stock reservado en ocupaciones' + cr + ;
                        'MENSAJE: ' + Message() + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Actualizar stock reservado de la ocupación.
   Select CF16C
   Go Top
   If !Eof()
      Replace F16cCanRes With F16cCanRes + CF14C.F14cCanFis

      *> Actualizar la ocupación.
      _okk = F3_UpdTun('F16C', , 'F16cCanRes', ,'CF16C', XWhere, 'N')
      If !_okk
         _LxErr = _LxErr + 'No se ha podido actualizar el stock reservado en ocupaciones' + cr

         *> Devolver control al programa.
        Return .F.
      EndIf
   EndIf   

   *> Leer el siguiente MP de salida.
   Use In CF16C
   Select CF14C
   Skip
EndDo

Use In CF14C

*>
*> Bloquear los MPs de entrada que se van a leer en el proceso.
*> Se bloquea previamente, pues el GROUP BY no lo permite.
*>
XSelect= "SELECT * "
XFrom  = " FROM F14c" + _em
XWhere = " WHERE F14cCodPro='" + cCodPro + "' AND F14cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND " + _GCSS("F14cTipMov", 1, 1) + " = '1'"
XGroup = ""
XBlq   = _Bloqueo

*>
*> NO bloquear.
*>

*F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
*_xier = f3_SqlExec(_ASql, F_Selec, 'F14C')
*If _xier < 0
*   _LxErr = 'Error bloqueando Movimientos Pendientes de entrada' + cr + ;
*            'MENSAJE: ' + Message() + cr
*
*   *> Devolver control al programa.
*   Do Form St3Inc
*   _LxErr = ''
*   =SqlRollBack(_ASql)
*   Return
*EndIf
*

*>
*> Generar el cursor con los acumulados de los MPs de entrada.
XSelect= "SELECT F14cCodPro, F14cCodArt, Sum(F14cCanFis) F14cCanFis"
XFrom  = " FROM F14c" + _em
XWhere = " WHERE F14cCodPro='" + cCodPro + "' AND F14cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND " + _GCSS("F14cTipMov", 1, 1) + " = '1'"
XGroup = " GROUP BY F14cCodPro, F14cCodArt"
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo movimientos pendientes de entrada. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF14C')
If _xier < 0
   _LxErr = _LxErr + 'Error leyendo movimientos pendientes de entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de los MPs de entrada.
Select CF14C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock pendiente de ubicar: " + F14cCodPro + " - " + F14cCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF14C.F14cCodPro
   m.F13cCodArt = CF14C.F14cCodArt
   m.F13cSitStk = '5000'
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF14C.F14cCanFis
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF14C.F14cCodPro, ;
              F13cCodArt With CF14C.F14cCodArt, ;
              F13cCantid With CF14C.F14cCanFis, ;
              F13cSitStk With '5000'
   EndIf

   *> Actualizar el registro de F13c (cantidad física).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar stock pendiente de entrada' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Leer el siguiente MP de entrada.
   Select CF14C
   Skip
EndDo

Use In CF14C

*> Bloquear los documentos de salida que se van a leer en el proceso.
*> Se bloquea previamente, pues el GROUP BY no lo permite.
*>
XSelect= "SELECT * "
XFrom  = " FROM F24c" + _em + " ,F24l" + _em
XWhere = " WHERE F24cCodPro='" + cCodPro + "'"
XWhere = XWhere + " AND F24cFlgEst<'1'"
XWhere = XWhere + " AND F24cCodPro=F24lCodPro"
XWhere = XWhere + " AND F24cTipDoc=F24lTipDoc"
XWhere = XWhere + " AND F24cNumDoc=F24lNumDoc"
XWhere = XWhere + " AND F24lCodArt='" + cCodArt + "'"
XGroup = ""
XBlq   = _Bloqueo

If lShowMsg
   Wait Window 'Leyendo documentos de salida. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF24C')
If _xier < 0
   _LxErr = _LxErr + 'Error leyendo documentos de salida' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de los documentos de salida.
Select CF24C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock asignado: " + F24cCodPro + " - " + F24lCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF24C.F24cCodPro
   m.F13cCodArt = CF24C.F24lCodArt
   m.F13cSitStk = '7000'
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF24C.F24lCanDoc
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF24C.F24cCodPro, ;
              F13cCodArt With CF24C.F24lCodArt, ;
              F13cCantid With CF24C.F24lCanDoc, ;
              F13cSitStk With '7000'
   EndIf

   *> Actualizar el registro de F13c (cantidad física).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar stock pendiente de servir' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Leer el siguiente Documento de salida.
   Select CF24C
   Skip
EndDo

Use In CF24C

*>
*> Actualizar el stock físico en las ocupaciones de MUELLE.
XSelect= "UPDATE F16c" + _em
XSet   = " SET F16cCanFis=F16cCanRes"
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F16cSitStk='1000'"
XWhere = XWhere + " AND EXISTS (SELECT F10cCodUbi FROM F10c" + _em
XWhere = XWhere + " WHERE F10cCodUbi=F16cCodUbi AND F10cPickSN='E' AND F10cEstSal='N')"

If lShowMsg
   Wait Window 'Actualizando stock físico en ocupaciones de muelle. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XSet + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error actualizando stock físico en muelle' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*>
*> Borrar las ocupaciones con cantidad cero.
XSelect= "DELETE"
XFrom  = " FROM F16c" + _em
XWhere = " WHERE F16cCanFis = 0"
XWhere = XWhere + " AND F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F16cSitStk='1000'"

If lShowMsg
   Wait Window 'Borrando ocupaciones vacías en muelle. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere

_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error borrando ocupaciones vacías en muelle' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*>
If lShowMsg
   Wait Clear
EndIf

*>
Return .T.

*>-----------------------------------------------------------------------------------------
*> Calcular el stock en ocupaciones de un producto.
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodArt ----> Artículo.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*>-----------------------------------------------------------------------------------------

Function RStkCFis
Parameters cCodPro, cCodArt, lShowMsg

Private XSelect, XFrom, XWhere, _XGroup, XBlq, F_Selec, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

*> Borrar las situaciones de stock que se van a procesar.
*> '1xxx' ---> Stock físico.
*> '8xxx' ---> Control de calidad.
*> '9xxx' ---> Bloqueado.

*> Borrar los registros del F13c a recalcular.
XSelect= "DELETE"
XFrom  = " FROM F13c" + _em
XWhere = " WHERE F13cCodPro='" + cCodPro + "' AND F13cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND " + _GCSS("F13cSitStk", 1, 1) + " IN ('1', '8', '9')"

If lShowMsg
   Wait Window 'Inicializando situaciones de stock físico. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando situaciones de stock físico' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

Store Space(0) To m.F13cCodAlm, F13cCodPro, F13cCodArt, F13cSitStk
=f3_seek('F13c')

*> Generar el cursor con los acumulados de las ocupaciones.
XSelect= "SELECT F16cCodPro, F16cCodArt, F16cSitStk, Sum(F16cCanFis) F16cCanFis, Sum(F16cCanRes) F16cCanRes"
XFrom  = " FROM F16c" + _em
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XGroup = " GROUP BY F16cCodPro, F16cCodArt, F16cSitStk"
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo ocupaciones de almacén. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF16C')
If _xier < 0
   _LxErr = _LxErr + 'Error leyendo ocupaciones de almacén' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de las ocupaciones.
Select CF16C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock físico: " + F16cCodPro + " - " + F16cCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF16C.F16cCodPro
   m.F13cCodArt = CF16C.F16cCodArt
   m.F13cSitStk = CF16C.F16cSitStk
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF16C.F16cCanFis
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF16C.F16cCodPro, ;
              F13cCodArt With CF16C.F16cCodArt, ;
              F13cCantid With CF16C.F16cCanFis, ;
              F13cSitStk With CF16C.F16cSitStk
   EndIf

   *> Actualizar el registro de F13c (cantidad física).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar situación de stock físico' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Leer la siguiente ocupación.
   Select CF16C
   Skip
EndDo

*>
Use In CF16C
Return .T.

*>-----------------------------------------------------------------------------------------
*> Calcular el stock reservado en ocupaciones de un producto.
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodArt ----> Artículo.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*>-----------------------------------------------------------------------------------------

Function RStkCRes
Parameters cCodPro, cCodArt, lShowMsg

Private XSelect, XFrom, XWhere, _XGroup, XBlq, F_Selec, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

*> Borrar las situaciones de stock que se van a procesar.
*> '2000' ---> Stock reservado.

*> Borrar los registros del F13c a recalcular.
XSelect= "DELETE"
XFrom  = " FROM F13c" + _em
XWhere = " WHERE F13cCodAlm='" + _Alma + "'"
XWhere = XWhere + " AND F13cCodPro='" + cCodPro + "'"
XWhere = XWhere + " AND F13cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F13cSitStk='2000'"

If lShowMsg
   Wait Window 'Inicializando situaciones de stock reservado. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando situaciones de stock reservado' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

Store Space(0) To m.F13cCodAlm, F13cCodPro, F13cCodArt, F13cSitStk
=f3_seek('F13c')

*> Borrar la cantidad reservada de las ocupaciones.
XSelect= "UPDATE F16c" + _em
XSet   = " SET F16cCanRes=0"
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"

If lShowMsg
   Wait Window 'Inicializando cantidades reservadas en ocupaciones. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XSet + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando cantidades reservadas' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Generar el cursor con los acumulados de los MPs de salida.
XSelect= "SELECT F14cCodPro, F14cCodArt, F14cUbiOri, F14cNumLot, F14cSitStk, "
XSelect= XSelect + _GCN("Sum(F14cCanFis)") + " As F14cCanFis"

XFrom  = " FROM F14c" + _em

XWhere = " WHERE F14cCodPro='" + cCodPro + "'"
XWhere = XWhere + " AND F14cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND " + _GCSS("F14cTipMov", 1, 1) + " = '2'"

XGroup = " GROUP BY F14cCodPro, F14cCodArt, F14cUbiOri, F14cNumLot, F14cSitStk"
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo movimientos pendientes de preparación. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF14C')

If _xier < 0
   _LxErr = _LxErr + 'Error leyendo movimientos pendientes de preparación' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de los MPs de salida.
Select CF14C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock reservado: " + F14cCodPro + " - " + F14cCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF14C.F14cCodPro
   m.F13cCodArt = CF14C.F14cCodArt
   m.F13cSitStk = '2000'
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF14C.F14cCanFis
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF14C.F14cCodPro, ;
              F13cCodArt With CF14C.F14cCodArt, ;
              F13cCantid With CF14C.F14cCanFis, ;
              F13cSitStk With '2000'
   EndIf

   *> Actualizar el registro de F13c (cantidad reservada).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar stock reservado' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Actualizar la cantidad reservada de la ocupación.
   XSelect = "SELECT *"
   XFrom   = " FROM F16c" + _em
   XWhere  = " F16cCodPro='" + CF14C.F14cCodPro + "'"
   XWhere  = XWhere + " AND F16cCodArt='" + CF14C.F14cCodArt + "'"
   XWhere  = XWhere + " AND F16cCodUbi='" + CF14C.F14cUbiOri + "'"
*  XWhere  = XWhere + " AND F16cNumPal='" + CF14C.F14cNumPal + "'"
   XWhere  = XWhere + " AND F16cNumLot='" + CF14C.F14cNumLot + "'"
   XWhere  = XWhere + " AND F16cSitStk='" + CF14C.F14cSitStk + "'"
   XGroup  = ""
   XBlq    = _Bloqueo

   F_Selec = XSelect + XFrom + " WHERE " + XWhere + XGroup + XBlq
   _xier = f3_SqlExec(_ASql, F_Selec, 'CF16C')

   If _xier =< 0
      _LxErr = _LxErr + 'Error leyendo datos para stock reservado en ocupaciones' + cr + ;
                        'MENSAJE: ' + Message() + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Actualizar stock reservado de la ocupación.
   Select CF16C
   Go Top
   If !Eof()
      Replace F16cCanRes With F16cCanRes + CF14C.F14cCanFis

      *> Actualizar la ocupación.
      _okk = F3_UpdTun('F16C', , 'F16cCanRes', ,'CF16C', XWhere, 'N')
      If !_okk
         _LxErr = _LxErr + 'No se ha podido actualizar el stock reservado en ocupaciones' + cr

         *> Devolver control al programa.
        Return .F.
      EndIf
   EndIf   

   Use In CF16C

   *> Leer el siguiente MP de salida.
   Select CF14C
   Skip
EndDo

*> Actualizar el stock físico en las ocupaciones de MUELLE, que debe ser igual al
*> stock reservado. Solo en situación de stock disponible.
XSelect= "UPDATE F16c" + _em
XSet   = " SET F16cCanFis=F16cCanRes"
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F16cSitStk='1000'"
XWhere = XWhere + " AND EXISTS (SELECT F10cCodUbi FROM F10c" + _em
XWhere = XWhere + " WHERE F10cCodUbi=F16cCodUbi AND F10cPickSN='E' AND F10cEstSal='N')"

If lShowMsg
   Wait Window 'Actualizando stock físico en ocupaciones de muelle. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XSet + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error actualizando stock físico en muelle' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Borrar las ocupaciones con cantidad cero. Solo en situación de stock disponible.
XSelect= "DELETE"
XFrom  = " FROM F16c" + _em
XWhere = " WHERE F16cCanFis = 0"
XWhere = XWhere + " AND F16cCodPro='" + cCodPro + "' AND F16cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F16cSitStk='1000'"

If lShowMsg
   Wait Window 'Borrando ocupaciones vacías en muelle. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere

_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error borrando ocupaciones vacías en muelle' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*>
Use In CF14C
Return .T.

*>-----------------------------------------------------------------------------------------
*> Calcular el stock pendiente de ubicar de un producto.
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodArt ----> Artículo.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*>-----------------------------------------------------------------------------------------

Function RStkEnt
Parameters cCodPro, cCodArt, lShowMsg

Private XSelect, XFrom, XWhere, _XGroup, XBlq, F_Selec, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

*> Borrar las situaciones de stock que se van a procesar.
*> '5000' ---> Pendiente de ubicar (MPs).

*> Borrar los registros del F13c a recalcular.
XSelect= "DELETE"
XFrom  = " FROM F13c" + _em
XWhere = " WHERE F13cCodPro='" + cCodPro + "' AND F13cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F13cSitStk='5000'"

If lShowMsg
   Wait Window 'Inicializando situaciones de stock pendiente de ubicar. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando situaciones de stock pendiente de ubicar' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

Store Space(0) To m.F13cCodAlm, F13cCodPro, F13cCodArt, F13cSitStk
=f3_seek('F13c')

*> Generar el cursor con los acumulados de los MPs de entrada.
XSelect= "SELECT F14cCodPro, F14cCodArt, Sum(F14cCanFis) F14cCanFis"
XFrom  = " FROM F14c" + _em
XWhere = " WHERE F14cCodPro='" + cCodPro + "' AND F14cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND " + _GCSS("F14cTipMov", 1, 1) + " = '1'"
XGroup = " GROUP BY F14cCodPro, F14cCodArt"
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo movimientos pendientes de entrada. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF14C')
If _xier < 0
   _LxErr = _LxErr + 'Error leyendo movimientos pendientes de entrada' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de los MPs de entrada.
Select CF14C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock pendiente de ubicar: " + F14cCodPro + " - " + F14cCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF14C.F14cCodPro
   m.F13cCodArt = CF14C.F14cCodArt
   m.F13cSitStk = '5000'
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF14C.F14cCanFis
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF14C.F14cCodPro, ;
              F13cCodArt With CF14C.F14cCodArt, ;
              F13cCantid With CF14C.F14cCanFis, ;
              F13cSitStk With '5000'
   EndIf

   *> Actualizar el registro de F13c (cantidad física).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar stock pendiente de entrada' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Leer el siguiente MP de entrada.
   Select CF14C
   Skip
EndDo

*>
Use In CF14C
Return .T.

*>-----------------------------------------------------------------------------------------
*> Calcular el stock asignado de un producto.
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodArt ----> Artículo.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*>-----------------------------------------------------------------------------------------

Function RStkAsg
Parameters cCodPro, cCodArt, lShowMsg

Private XSelect, XFrom, XWhere, _XGroup, XBlq, F_Selec, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

*> Borrar las situaciones de stock que se van a procesar.
*> '7000' ---> Stock asignado.

*> Borrar los registros del F13c a recalcular.
XSelect= "DELETE"
XFrom  = " FROM F13c" + _em
XWhere = " WHERE F13cCodPro='" + cCodPro + "' AND F13cCodArt='" + cCodArt + "'"
XWhere = XWhere + " AND F13cSitStk='7000'"

If lShowMsg
   Wait Window 'Inicializando situaciones de stock asignado. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere
_xier = f3_SqlExec(_ASql, F_Selec)
If _xier < 0
   _LxErr = _LxErr + 'Error inicializando situaciones de stock asignado' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

Store Space(0) To m.F13cCodAlm, F13cCodPro, F13cCodArt, F13cSitStk
=f3_seek('F13c')

*> Generar el cursor con los documentos de salida asignados a procesar.
XSelect= "SELECT * "
XFrom  = " FROM F24c" + _em + " ,F24l" + _em
XWhere = " WHERE F24cCodPro='" + cCodPro + "'"
XWhere = XWhere + " AND F24cFlgEst<'1'"
XWhere = XWhere + " AND F24cCodPro=F24lCodPro"
XWhere = XWhere + " AND F24cTipDoc=F24lTipDoc"
XWhere = XWhere + " AND F24cNumDoc=F24lNumDoc"
XWhere = XWhere + " AND F24lCodArt='" + cCodArt + "'"
XGroup = ""
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo documentos de salida. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF24C')
If _xier < 0
   _LxErr = _LxErr + 'Error leyendo documentos de salida' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de los documentos de salida.
Select CF24C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock asignado: " + F24cCodPro + " - " + F24lCodArt NoWait
   EndIf

   *> Limpiar cursor F13c.
   Select F13c
   Zap

   *> Buscar en F13c. En teoría no debe de existir, pues se ha borrado al principio.
   m.F13cCodAlm = _Alma
   m.F13cCodPro = CF24C.F24cCodPro
   m.F13cCodArt = CF24C.F24lCodArt
   m.F13cSitStk = '7000'
   _okk = f3_seek('F13C')

   If _okk
      Select F13C
      Replace F13cCantid With F13cCantid + CF24C.F24lCanDoc
   Else
      Select F13C
      Append Blank
      Replace F13cCodAlm With _Alma, ;
              F13cCodPro With CF24C.F24cCodPro, ;
              F13cCodArt With CF24C.F24lCodArt, ;
              F13cCantid With CF24C.F24lCanDoc, ;
              F13cSitStk With '7000'
   EndIf

   *> Actualizar el registro de F13c (cantidad física).
   If !_okk
      _okk = f3_InsTun('F13C',,'N')
   Else
      _okk = F3_UpdTun('F13C',,,,,,'N')
   EndIf
   If !_okk
      _LxErr = _LxErr + 'No se ha podido actualizar stock pendiente de servir' + cr

      *> Devolver control al programa.
      Return .F.
   EndIf

   *> Leer el siguiente Documento de salida.
   Select CF24C
   Skip
EndDo

*>
Use In CF24C
Return .T.

*>-----------------------------------------------------------------------------------------
*> Calcular el stock reservado de una ubicación.
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodUbi ----> Ubicación.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*>-----------------------------------------------------------------------------------------

Function RStkCResUbi
Parameters cCodPro, cCodUbi, lShowMsg

Private XSelect, XFrom, XWhere, _XGroup, XBlq, F_Selec, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

*> Generar el cursor con las ocupaciones de la ubicación a actualizar.
XSelect= "SELECT F16cCodPro, F16cCodArt, F16cNumLot, F16cFecCad, F16cNumPal, F16cSitStk, F16cCodUbi"
XFrom  = " FROM F16c" + _em
XWhere = " WHERE F16cCodPro='" + cCodPro + "' AND F16cCodUbi='" + cCodUbi + "'"
XGroup = ""
XBlq   = ""

If lShowMsg
   Wait Window 'Leyendo ocupaciones de la ubicación a actualizar. Un momento ...' NoWait
EndIf

F_Selec = XSelect + XFrom + XWhere + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF10C')

If _xier < 0
   _LxErr = _LxErr + 'Error leyendo ocupaciones de la ubicación a actualizar' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Bucle de lectura de las ocupaciones.
Select CF10C
Go Top
Do While !Eof()
   If lShowMsg
      Wait Window "Actualizando stock reservado: " + F16cCodPro + " - " + F16cCodUbi NoWait
   EndIf

   *> Actualizar la cantidad reservada de la ocupación.
   _okk = RStkCResOcu(CF10C.F16cCodPro, ;
                      CF10C.F16cCodArt, ;
                      CF10C.F16cNumLot, ;
                      CF10C.F16cFecCad, ;
                      CF10C.F16cNumPal, ;
                      CF10C.F16cSitStk, ;
                      CF10C.F16cCodUbi, ;
                      lShowMsg)

   *> Leer la siguiente ocupación a actualizar.
   Select CF10C
   Skip
EndDo

*>
Use In CF10C
Return .T.

*>-----------------------------------------------------------------------------------------
*> Calcular el stock reservado de una ocupación.
*>
*>   Recibe: cCodPro ----> Propietario.
*>           cCodArt ----> Artículo.
*>           cNumLot ----> Nº de Lote.
*>           dFecCad ----> Caducidad.
*>           cNumPal ----> Nº Palet.
*>           cSitStk ----> Situación stock.
*>           cCodUbi ----> Ubicación.
*>           lShowMsg ---> Mostrar mensajes (opcional).
*>
*> Devuelve: .T. / .F.
*>           _LxErr -----> Mensajes de error.
*>
*> COMMITs y ROLLBACKs se realizan fuera de esta función. 
*> Borrar ocupación solo si es de expedición. AVC - 04.06.2003
*>-----------------------------------------------------------------------------------------

Function RStkCResOcu
Parameters cCodPro, cCodArt, cNumLot, dFecCad, cNumPal, cSitStk, cCodUbi, lShowMsg

Private XSelect, XFrom, XWhere16, XWhere14, _XGroup, XBlq, F_Selec, _Sel, _okk

If Type('lShowMsg') # 'L'
   Store .T. To lShowMsg
EndIf

*> Inicializar variables de trabajo.
Store Space(0) To _LxErr

XSelect = "SELECT *"
XFrom   = " FROM F16c" + _em

XWhere16 = " F16cCodPro='" + cCodPro + "'"
XWhere16 = XWhere16 + " AND F16cCodArt='" + cCodArt + "'"
XWhere16 = XWhere16 + " AND F16cNumLot='" + cNumLot + "'"
XWhere16 = XWhere16 + " AND F16cFecCad=" + _GCD(dFecCad)
XWhere16 = XWhere16 + " AND F16cSitStk='" + cSitStk + "'"
XWhere16 = XWhere16 + " AND F16cCodUbi='" + cCodUbi + "'"
XWhere16 = XWhere16 + " AND F16cNumPal='" + cNumPal + "'"


XBlq = ""
XGroup = ""

F_Selec = XSelect + XFrom + " WHERE " + XWhere16 + XGroup + XBlq
_xier = f3_SqlExec(_ASql, F_Selec, 'CF16C')

If _xier =< 0
   _LxErr = _LxErr + 'Error leyendo datos para stock reservado en ocupación' + cr + ;
                     'MENSAJE: ' + Message() + cr

   *> Devolver control al programa.
   Return .F.
EndIf

*> Actualizar stock reservado de la ocupación.
Select CF16C
Go Top
If !Eof()
   *> Acumular cantidad reservada, según los MPs.
   XSelect= "SELECT F14cCodPro, F14cCodArt, F14cUbiOri, F14cNumLot,F14cNumPal, F14cSitStk, F14cFecCad, "
   XSelect= XSelect + _GCN("Sum(F14cCanFis)") + " As F14cCanFis"
   XFrom  = " FROM F14c" + _em

   XWhere14 = " WHERE F14cCodPro='" + cCodPro + "'"
   XWhere14 = XWhere14 + " AND F14cCodArt='" + cCodArt + "'"
   XWhere14 = XWhere14 + " AND F14cNumLot='" + cNumLot + "'"
   XWhere14 = XWhere14 + " AND F14cFecCad=" + _GCD(dFecCad)
   XWhere14 = XWhere14 + " AND F14cSitStk='" + cSitStk + "'"
   XWhere14 = XWhere14 + " AND F14cUbiOri='" + cCodUbi + "'"
   XWhere14 = XWhere14 + " AND F14cNumPal='" + cNumPal + "'"
   XWhere14 = XWhere14 + " AND F14cTipMov = '2999'"

   XGroup = " GROUP BY F14cCodPro, F14cCodArt, F14cUbiOri, F14cNumLot, F14cNumPal, F14cFecCad, F14cSitStk"

   XBlq   = ""

   If lShowMsg
      Wait Window 'Leyendo movimientos pendientes de preparación. Un momento ...' NoWait
   EndIf

   F_Selec = XSelect + XFrom + XWhere14 + XGroup + XBlq
   _xier = f3_SqlExec(_ASql, F_Selec, 'CF14C')

   If _xier <= 0
      _LxErr = _LxErr + 'Error leyendo movimientos pendientes de preparación' + cr + ;
                        'MENSAJE: ' + Message() + cr

      *> Devolver control al programa.
      Use In CF16C
      Return .F.
   EndIf

	*>Borrar la OOcupación si no hay MPs y la ubicación es de expedición.
	Select CF16C
	Go Top

	If CF14C.F14cCanFis==0
		m.F10cCodUbi = cCodUbi
		If f3_seek('F10c')
			Select F10c
			Go Top

			If F10cPickSn=='E'
				_Sel="Delete From F16c" + _em + " Where F16cCodPro='" + cCodPro + "' " + ;
				"AND F16cCodArt='" + cCodArt + "' " + ;
				"AND F16cNumLot='" + cNumLot + "' " + ;
				"AND F16cFecCad=" + _GCD(dFecCad) + Space(1)+ ;
				"AND F16cSitStk='" + cSitStk + "' " + ;
				"AND F16cCodUbi='" + cCodUbi + "' " + ;
				"AND F16cNumPal='" + cNumPal + "'"

				_okk = f3_SqlExec(_Asql,_Sel,'DOCUP')
				If _okk <= 0
					_LxErr = _LxErr + 'No se ha podido borrar la ocupación' + cr
					Use In CF16C
					Use In CF14C
					Return .F.
				EndIf

				*> Borrado OK de la ocupación: Volver.
				Use In CF16C
				Use In CF14C
				Return
			EndIf
		EndIf
	EndIf

	*> Actualizar cantidad reservada en la ocupación.
	Select CF16C
	Replace F16cCanRes With CF14C.F14cCanFis

	_okk = F3_UpdTun('F16C', , 'F16cCanRes', ,'CF16C', XWhere16, 'N')
	If !_okk
		_LxErr = _LxErr + 'No se ha podido actualizar el stock reservado en la ocupación' + cr
		Use In CF16C
		Use In CF14C
		Return .F.
	EndIf
EndIf

*> Recuperar e entorno de trabajo anterior.
Use In (Select('CF14C'))
Use In (Select('CF16C'))
Return
