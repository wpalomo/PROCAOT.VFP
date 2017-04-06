*>
*> Procedures
*>
*> GenOcuMap .................................... Ubicaciones libres/ocupadas - Mapa(Gr�fico).
*> GenOcuZon .................................... Ubicaciones libres/ocupadas - Zonas(Gr�fico).
*> GenOcuTam .................................... Ubicaciones libres/ocupadas - Tama�os(Gr�fico).
*> GenArtOcuMed ................................. Ocupaci�n media por art�culo - mes(Gr�fico).
*> GenOpeMov .................................... Movimientos operario - mes(Gr�fico).
*> GenOpeLst .................................... Listas operario - mes(Gr�fico).
*> GenOpeKgs .................................... Kilos operario - mes(Gr�fico).
*> GenDisGen .................................... Distribuci�n general(Gr�fico).
*> GenDisExp .................................... Expediciones distribuci�n - mes(Gr�fico).
*> GenDisBul .................................... Bultos distribuci�n - mes(Gr�fico).
*> GenDisKgs .................................... Kilos distribuci�n - mes(Gr�fico).
*> GenDisPts .................................... Pesetas distribuci�n - mes(Gr�fico).
*>
*> CreaOcuMap ................................... Tablas de GenOcuMap.
*> CreaOcuZon ................................... Tablas de GenOcuZon.
*> CreaOcuTam ................................... Tablas de GenOcuTam.
*> CreaArtOcuMed ................................ Tablas de GenArtOcuMed.
*> CreaOpeMov ................................... Tablas de GenOpeMov.
*> CreaOpeLst ................................... Tablas de GenOpeLst.
*> CreaOpeKgs ................................... Tablas de GenOpeKgs.
*> CreaDisGen ................................... Tablas de GenDisGen.
*> CreaDisExp ................................... Tablas de GenDisExp.
*> CreaDisBul ................................... Tablas de GenDisBul.
*> CreaDisKgs ................................... Tablas de GenDisKgs.
*> CreaDisPts ................................... Tablas de GenDisPts.
*>
*> GraficoOcuMap ................................ Gr�fico de GenOcuMap.
*> GraficoOcuZon ................................ Gr�fico de GenOcuZon.
*> GraficoOcuTam ................................ Gr�fico de GenOcuTam.
*> GraficoArtOcuMed ............................. Gr�fico de GenArtOcuMed.
*> GraficoOpeMov ................................ Gr�fico de GenOpeMov.
*> GraficoOpeLst ................................ Gr�fico de GenOpeLst.
*> GraficoOpeKgs ................................ Gr�fico de GenOpeKgs.
*> GraficoDisGen ................................ Gr�fico de GenDisGen.
*> GraficoDisExp ................................ Gr�fico de GenDisExp.
*> GraficoDisBul ................................ Gr�fico de GenDisBul.
*> GraficoDisKgs ................................ Gr�fico de GenDisKgs.
*> GraficoDisPts ................................ Gr�fico de GenDisPts.
*>
*> AbrirConexion ................................ Crear conexi�n con base de datos.

*> OBSERVACIONES:
*>   - A�adir selecci�n de nombre de Hoja de c�lculo destino. AVC - 08.04.2000

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de ubicaciones libres/ocupadas, Mapa.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenOcuMap
LParameters _ASql, _em

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuMap(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Mapa)' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de ubicaciones Libres/Ocupadas (Mapa).
Do GraficoOcuMap

If Used('OcuMap')
   Use In OcuMap
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de ubicaciones libres/ocupadas, por Zonas.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenOcuZon
LParameters _ASql, _em

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuZon(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Zonas)' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de ubicaciones Libres/Ocupadas (Zonas).
Do GraficoOcuZon

If Used('OcuZon')
   Use In OcuZon
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de ubicaciones libres/ocupadas, por Tama�os.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenOcuTam
LParameters _ASql, _em

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuTam(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Tama�os)' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de ubicaciones Libres/Ocupadas (Tama�os).
Do GraficoOcuTam

If Used('OcuTam')
   Use In OcuTam
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de ocupaci�n media por art�culo/mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>   _CodArt -> Art�culo a calcular.
*>-----------------------------------------------------------------------------
Procedure GenArtOcuMed
LParameters _ASql, _em, _CodArt

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaArtOcuMed(_ASql, _em, _CodArt)
   _LxErr = 'Error cargando ocupaci�n media art�culo / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de ocupaci�n media art�culo / mes.
Do GraficoArtOcuMed

If Used('ArtOcuMed')
   Use In ArtOcuMed
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de movimientos por operario / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>   _CodOpe -> Operario a calcular.
*>-----------------------------------------------------------------------------
Procedure GenOpeMov
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOpeMov(_ASql, _em, _CodOpe)
   _LxErr = 'Error cargando movimientos operario / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de movimientos operario / mes.
Do GraficoOpeMov

If Used('OpeMov')
   Use In OpeMov
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de movimientos por operario / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>   _CodOpe -> Operario a calcular.
*>-----------------------------------------------------------------------------
Procedure GenOpeLst
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOpeLst(_ASql, _em, _CodOpe)
   _LxErr = 'Error cargando listas operario / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de listas operario / mes.
Do GraficoOpeLst

If Used('OpeLst')
   Use In OpeLst
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de distribuci�n expediciones / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisExp
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisExp(_ASql, _em)
   _LxErr = 'Error cargando distribuci�n expediciones / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de distribuci�n expediciones / mes.
Do GraficoDisExp

If Used('DisExp')
   Use In DisExp
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de distribuci�n bultos / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisBul
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisBul(_ASql, _em)
   _LxErr = 'Error cargando distribuci�n bultos / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de distribuci�n bultos / mes.
Do GraficoDisBul

If Used('DisBul')
   Use In DisBul
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de distribuci�n kilos / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisKgs
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisKgs(_ASql, _em)
   _LxErr = 'Error cargando distribuci�n kilos / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de distribuci�n kilos / mes.
Do GraficoDisKgs

If Used('DisKgs')
   Use In DisKgs
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de distribuci�n pesetas / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisPts
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisPts(_ASql, _em)
   _LxErr = 'Error cargando distribuci�n pesetas / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de distribuci�n pesetas / mes.
Do GraficoDisPts

If Used('DisPts')
   Use In DisPts
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de kilos preparados por operario / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>   _CodOpe -> Operario a calcular.
*>-----------------------------------------------------------------------------
Procedure GenOpeKgs
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOpeKgs(_ASql, _em, _CodOpe)
   _LxErr = 'Error cargando kilos preparados operario / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de kilos preparados operario / mes.
Do GraficoOpeKgs

If Used('OpeKgs')
   Use In OpeKgs
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generaci�n de gr�ficos (EXCEL) de estad�stica distribuci�n general / mes.
*> Recibe:
*>   _ASql ---> Conexi�n con ORACLE. Si no existe la crea.
*>   _em -----> C�digo de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisGen
LParameters _ASql, _em

*> Crear, si cal, la conexi�n con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisGen(_ASql, _em)
   _LxErr = 'Error cargando estad�stica distribuci�n general' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gr�fico de estad�stica distribuci�n general.
Do GraficoDisGen

If Used('DisGen')
   Use In DisGen
EndIf

Return

*>--------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de ubicaciones libres/ocupadas, mapa.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>--------------------------------------------------------------------------------
Procedure CreaOcuMap
Parameters _ASql, _em
Private err, _Selec

*> Crear la tabla de ubicaciones Libres/Ocupadas.
Create Table OcuMap (UTexto C(10), ;
                     UTotal N(4, 0))

*> Cargar en la tabla las ubicaciones libres.
_Selec = "Select Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi Not In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi)"

Wait Window 'Leyendo ubicaciones libres (Mapa) ...' NoWait

err = f3_SqlExec(_ASql, _Selec, 'Ubi')
If err <= 0
   _LxErr = 'Error cargando ubicaciones libres (Mapa)' + cr + ;
            'MENSAJE: ' + Message() + cr
   =Anomalias()
   Return .F.
EndIf

*> Pasar datos a la tabla.
Select OcuMap
Append Blank
Replace UTotal With Ubi.Ubic, ;
        UTexto With 'LIBRES'

TotalUbics = Ubi.Ubic

*> Cargar en la tabla las ubicaciones ocupadas.
_Selec = "Select Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi)"

Wait Window 'Leyendo ubicaciones ocupadas (Mapa) ...' NoWait

err = f3_SqlExec(_ASql, _Selec, 'Ubi')
If err <= 0
   _LxErr = 'Error cargando ubicaciones ocupadas (Mapa)' + cr + ;
            'MENSAJE: ' + Message() + cr
   =Anomalias()
   Return .F.
EndIf

*> Pasar datos a la tabla.
Select OcuMap
Append Blank
Replace UTotal With Ubi.Ubic, ;
        UTexto With 'OCUPADAS'

*> Cargar en la tabla las ubicaciones reservadas.
_Selec = "Select Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi And F16cCanRes > 0)"

Wait Window 'Leyendo ubicaciones reservadas (Mapa) ...' NoWait

err = f3_SqlExec(_ASql, _Selec, 'Ubi')
If err <= 0
   _LxErr = 'Error cargando ubicaciones reservadas (Mapa)' + cr + ;
            'MENSAJE: ' + Message() + cr
   =Anomalias()
   Return .F.
EndIf

*> Pasar datos a la tabla.
Select OcuMap
Append Blank
Replace UTotal With Ubi.Ubic, ;
        UTexto With 'RESERVADAS'

*> Guardar tabla en formato EXCEL.
Select OcuMap
Copy To OcuMap Type XL5
Return .T.

*>--------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de ubicaciones libres/ocupadas, zonas.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>--------------------------------------------------------------------------------
Procedure CreaOcuZon
Parameters _ASql, _em
Private err, _Selec, _LIBRES, _OCUPADAS, _RESERVADAS

   *> Crear la tabla de ubicaciones Libres/Ocupadas.
   Create Table OcuZon (CZona      C(10), ;
                        LIBRES     N(4, 0), ;
                        OCUPADAS   N(4, 0), ;
                        RESERVADAS N(4, 0))

   *> Cargar en la tabla las ubicaciones libres.
   _Selec = "Select SubStr(F10cCodUbi, 1, 6) As CZona, " + ;
            "Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi Not In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi) " + ;
            "Group By SubStr(F10cCodUbi, 1, 6)"

   Wait Window 'Leyendo ubicaciones libres (Zonas) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Zon')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones libres (Zona)' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      Return .F.
   EndIf

   *> Pasar datos a la tabla.
   Select Zon
   Go Top
   Do While !Eof()
      Select OcuZon
      Append Blank
      Replace CZona      With Zon.CZona, ;
              LIBRES     With Zon.Ubic, ;
              OCUPADAS   With 0, ;
              RESERVADAS With 0
      Select Zon
      Skip
   EndDo

   *> Cargar en la tabla las ubicaciones ocupadas.
   _Selec = "Select SubStr(F10cCodUbi, 1, 6) As CZona, " + ;
            "Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi) " + ;
            "Group By SubStr(F10cCodUbi, 1, 6)"

   Wait Window 'Leyendo ubicaciones ocupadas (Zonas) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Zon')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones ocupadas (Zona)' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      Return .F.
   EndIf

   *> Pasar datos a la tabla.
   Select Zon
   Go Top
   Do While !Eof()
      Select OcuZon
      Locate For CZona = Zon.CZona
      If !Found()
         Append Blank
         Replace CZona      With Zon.CZona, ;
                 LIBRES     With 0, ;
                 RESERVADAS With 0
      EndIf
      Replace OCUPADAS With Zon.Ubic

      Select Zon
      Skip
   EndDo

   *> Cargar en la tabla las ubicaciones reservadas.
   _Selec = "Select SubStr(F10cCodUbi, 1, 6) As CZona, " + ;
            "Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi And F16cCanRes > 0) " + ;
            "Group By SubStr(F10cCodUbi, 1, 6)"

   Wait Window 'Leyendo ubicaciones reservadas (Zonas) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Zon')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones reservadas (Zona)' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      Return .F.
   EndIf

   *> Pasar datos a la tabla.
   Select Zon
   Go Top
   Do While !Eof()
      Select OcuZon
      Locate For CZona = Zon.CZona
      If !Found()
         Append Blank
         Replace CZona    With Zon.CZona, ;
                 LIBRES   With 0, ;
                 OCUPADAS With 0
      EndIf
      Replace RESERVADAS With Zon.Ubic

      Select Zon
      Skip
   EndDo

   *> Calcular los totales.
   Select OcuZon
   Sum(LIBRES) To _LIBRES
   Sum(OCUPADAS) To _OCUPADAS
   Sum(RESERVADAS) To _RESERVADAS

   Append Blank
   Replace CZona      With 'TOTAL', ;
           LIBRES     With _LIBRES, ;
           OCUPADAS   With _OCUPADAS, ;
           RESERVADAS With _RESERVADAS

   *> Guardar tabla en formato EXCEL.
   Select OcuZon
   Copy To OcuZon Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de ubicaciones libres/ocupadas, tama�os.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaOcuTam
Parameters _ASql, _em
Private err, _Selec, _LIBRES, _OCUPADAS, _RESERVADAS

   *> Crear la tabla de ubicaciones Libres/Ocupadas.
   Create Table OcuTam (CTamUbi    C(10), ;
                        LIBRES     N(4, 0), ;
                        OCUPADAS   N(4, 0), ;
                        RESERVADAS N(4, 0))

   *> Cargar en la tabla las ubicaciones libres.
   _Selec = "Select F10cPickSn As CTamUbi, " + ;
            "Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi Not In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi) " + ;
            "Group By F10cPickSn"

   Wait Window 'Leyendo ubicaciones libres (Tama�os) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones libres (Tama�os)' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      Return .F.
   EndIf

   *> Pasar datos a la tabla.
   Select Tam
   Go Top
   Do While !Eof()
      Select OcuTam
      Append Blank
      Replace CTamUbi    With Tam.CTamUbi, ;
              LIBRES     With Tam.Ubic, ;
              OCUPADAS   With 0, ;
              RESERVADAS With 0
      Select Tam
      Skip
   EndDo

   *> Cargar en la tabla las ubicaciones ocupadas.
   _Selec = "Select F10cPickSn As CTamUbi, " + ;
            "Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi) " + ;
            "Group By F10cPickSn"

   Wait Window 'Leyendo ubicaciones ocupadas (Tama�os) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones ocupadas (Tama�os)' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      Return .F.
   EndIf

   *> Pasar datos a la tabla.
   Select Tam
   Go Top
   Do While !Eof()
      Select OcuTam
      Locate For CTamUbi = Tam.CTamUbi
      If !Found()
         Append Blank
         Replace CTamUbi    With Tam.CTamUbi, ;
                 LIBRES     With 0, ;
                 RESERVADAS With 0
      EndIf
      Replace OCUPADAS With Tam.Ubic

      Select Tam
      Skip
   EndDo

   *> Cargar en la tabla las ubicaciones reservadas.
   _Selec = "Select F10cPickSn As CTamUbi, " + ;
            "Count (*) As Ubic " + ;
            "From F10c" + _em + Space(1) + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c" + _em + Space(1) + ;
            "Where F16cCodUbi = F10cCodUbi And F16cCanRes > 0) " + ;
            "Group By F10cPickSn"

   Wait Window 'Leyendo ubicaciones reservadas (Tama�os) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones reservadas (Tama�os)' + cr + ;
               'MENSAJE: ' + Message() + cr
      =Anomalias()
      Return .F.
   EndIf

   *> Pasar datos a la tabla.
   Select Tam
   Go Top
   Do While !Eof()
      Select OcuTam
      Locate For CTamUbi = Tam.CTamUbi
      If !Found()
         Append Blank
         Replace CTamUbi  With Tam.CTamUbi, ;
                 LIBRES   With 0, ;
                 OCUPADAS With 0
      EndIf
      Replace RESERVADAS With Tam.Ubic

      Select Tam
      Skip
   EndDo

   *> Asignar texto al tipo de ubicaci�n.
   Select OcuTam
   Go Top
   Do While !Eof()
      Do Case
         Case CTamUbi = 'N'
            Replace CTamUbi With 'PALETS'
         Case CTamUbi = 'S'
            Replace CTamUbi With 'CAJAS'
         Case CTamUbi = 'U'
            Replace CTamUbi With 'UNIDADES'
         Case CTamUbi = 'G'
            Replace CTamUbi With 'GRUPOS'
         Case CTamUbi = 'D'
            Replace CTamUbi With 'DINAMICAS'
         Case CTamUbi = 'E'
            Replace CTamUbi With 'EXPEDICION'
         Otherwise
            Replace CTamUbi With 'UNKNOWN'
      EndCase

      Skip
   EndDo

   *> Calcular los totales.
   Sum(LIBRES) To _LIBRES
   Sum(OCUPADAS) To _OCUPADAS
   Sum(RESERVADAS) To _RESERVADAS

   Append Blank
   Replace CTamUbi    With 'TOTAL', ;
           LIBRES     With _LIBRES, ;
           OCUPADAS   With _OCUPADAS, ;
           RESERVADAS With _RESERVADAS

   *> Guardar tabla en formato EXCEL.
   Select OcuTam
   Copy To OcuTam Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de ocupaci�n media art�culo / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>         _CodArt, Art�culo a calcular.
*>----------------------------------------------------------------------------------
Procedure CreaArtOcuMed
Parameters _ASql, _em, _CodArt
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de ocupaci�n media art�culo / mes.
   Create Table ArtOcumed (MES       C(10), ;
                           OCUPACION N(6, 0))

   *> Cargar en la tabla la ocupaci�n del art�culo.
   _Selec = "F60cFecMov, F60cMedOcu"
   _FromF = "F60c"
   _Where = "F60cCodPro='" + m.CodPro + "' And  F60cCodArt='" + _CodArt + "' And " + ;
            _GCSS("F60cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo ocupaci�n media art�culo/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Med')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos ocupaci�n media art�culo'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Med
   Go Top
   Do While !Eof()
      Select ArtOcuMed
      Append Blank
      Replace MES       With "01/" + SubStr(Med.F60cFecMov, 5, 2) + "/" + SubStr(Med.F60cFecMov, 1, 4), ;
              OCUPACION With Med.F60cMedOcu

      Select Med
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select ArtOcuMed
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select ArtOcuMed
   Copy To ArtOcuMed Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de movimientos operario / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>         _CodOpe, operario a procesar.
*>----------------------------------------------------------------------------------
Procedure CreaOpeMov
Parameters _ASql, _em, _CodOpe
Private err, _Selec, _FromF, _Where

   Use In (Select ('Ope'))

   *> Crear la tabla de movimientos operario / mes.
   Create Table OpeMov (MES        C(10), ;
                        ENTRADAS   N(6, 0), ;
                        SALIDAS    N(6, 0), ;
                        REPOSICION N(6, 0))

   *> Cargar en la tabla los movimientos del operario.
   _Selec = "F62cFecMov, F62cMvtEnt, F62cMvtSal, F62cTotRep"
   _FromF = "F62c"
   _Where = "F62cCodPro='" + m.CodPro + "' And F62cCodOpe='" + _CodOpe + "' And " + ;
            _GCSS("F62cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo movimientos operario/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Ope')
   If !err .And. _xier <= 0
      _LxErr = 'No hay movimientos operario/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Ope
   Go Top
   Do While !Eof()
      Select OpeMov
      Append Blank
      Replace MES        With "01/" + SubStr(Ope.F62cFecMov, 5, 2) + "/" + SubStr(Ope.F62cFecMov, 1, 4), ;
              ENTRADAS   With Ope.F62cMvtEnt, ;
              SALIDAS    With Ope.F62cMvtSal, ;
              REPOSICION With Ope.F62cTotRep

      Select Ope
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select OpeMov
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select OpeMov
   Copy To OpeMov Type XL5

Return

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de listas de trabajo operario / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>         _CodOpe, operario a procesar.
*>----------------------------------------------------------------------------------
Procedure CreaOpeLst
Parameters _ASql, _em, _CodOpe
Private err, _Selec, _FromF, _Where

   Use In (Select ('Ope'))

   *> Crear la tabla de listas de trabajo operario / mes.
   Create Table OpeLst (MES    C(10), ;
                        LISTAS N(6, 0))

   *> Cargar en la tabla las listas de trabajo del operario.
   _Selec = "F62cFecMov, F62cLstSal"
   _FromF = "F62c"
   _Where = "F62cCodPro='" + m.CodPro + "' And F62cCodOpe='" + _CodOpe + "' And " + ;
            _GCSS("F62cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo listas de trabajo operario/mes ...')

   err = f3_sql(_selec, _FromF, _Where, , , 'Ope')
   If !err .And. _xier <= 0
      _LxErr = 'No hay listas de trabajo operario/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Ope
   Go Top
   Do While !Eof()
      Select OpeLst
      Append Blank
      Replace MES    With "01/" + SubStr(Ope.F62cFecMov, 5, 2) + "/" + SubStr(Ope.F62cFecMov, 1, 4), ;
              LISTAS With Ope.F62cLstSal

      Select Ope
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select OpeLst
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select OpeLst
   Copy To OpeLst Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de kilos preparados operario / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>         _CodOpe, operario a procesar.
*>----------------------------------------------------------------------------------
Procedure CreaOpeKgs
Parameters _ASql, _em, _CodOpe
Private err, _Selec, _FromF, _Where

   Use In (Select ('Ope'))

   *> Crear la tabla de kilos preparados operario / mes.
   Create Table OpeKgs (MES   C(10), ;
                        KILOS N(7, 0))

   *> Cargar en la tabla los kilos preparados del operario.
   _Selec = "F62cFecMov, F62cKgsSal"
   _FromF = "F62c"
   _Where = "F62cCodPro='" + m.CodPro + "' And F62cCodOpe='" + _CodOpe + "' And " + ;
            _GCSS("F62cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo kilos preparados operario/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Ope')
   If !err .And. _xier <= 0
      _LxErr = 'No hay kilos preparados operario/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Ope
   Go Top
   Do While !Eof()
      Select OpeKgs
      Append Blank
      Replace MES   With "01/" + SubStr(Ope.F62cFecMov, 5, 2) + "/" + SubStr(Ope.F62cFecMov, 1, 4), ;
              KILOS With Ope.F62ckGSSal

      Select Ope
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select OpekGS
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select OpekGS
   Copy To OpeKgs Type XL5

Return .T.

*>--------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico estad�stica distribuci�n general/mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>--------------------------------------------------------------------------------
Procedure CreaDisGen
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de estad�stica distribuci�n general.
   Create Table DisGen (CConc     C(15), ;
                        VENTAS    N(10, 0), ;
                        PROMOCION N(10, 0), ;
                        TOTALES   N(10, 0))

   *> Cargar en la tabla los datos de distribuci�n - expediciones.
   _Selec = "'EXPEDICIONES' As CConc, " + ;
            "Sum(F63cExpVta) As VENTAS, " + ;
            "Sum(F63cExpPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n general (Expediciones) ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n general (Expediciones)'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisGen
      Append Blank
      Replace CConc     With Dis.CConc, ;
              VENTAS    With Dis.VENTAS, ;
              PROMOCION With Dis.PROMOCION, ;
              TOTALES   With 0
      Select Dis
      Skip
   EndDo

   *> Cargar en la tabla los datos de distribuci�n - bultos.
   _Selec = "'N� BULTOS' As CConc, " + ;
            "Sum(F63cBulVta) As VENTAS, " + ;
            "Sum(F63cBulPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n general (Bultos) ...')

   err = f3s_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n general (Bultos)'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisGen
      Append Blank
      Replace CConc     With Dis.CConc, ;
              VENTAS    With Dis.VENTAS, ;
              PROMOCION With Dis.PROMOCION, ;
              TOTALES   With 0
      Select Dis
      Skip
   EndDo

   *> Cargar en la tabla los datos de distribuci�n - kilos.
   _Selec = "'N� KILOS' As CConc, " + ;
            "Sum(F63cKgsVta) As VENTAS, " + ;
            "Sum(F63cKgsPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n general (Kilos) ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n general (Kilos)'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisGen
      Append Blank
      Replace CConc     With Dis.CConc, ;
              VENTAS    With Dis.VENTAS, ;
              PROMOCION With Dis.PROMOCION, ;
              TOTALES   With 0
      Select Dis
      Skip
   EndDo

   *> Cargar en la tabla los datos de distribuci�n - pesetas.
   _Selec = "'PESETAS' As CConc, " + ;
            "Sum(F63cPtsVta) As VENTAS, " + ;
            "Sum(F63cPtsPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n general (Pesetas) ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n general (Pesetas)'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisGen
      Append Blank
      Replace CConc     With Dis.CConc, ;
              VENTAS    With Dis.VENTAS, ;
              PROMOCION With Dis.PROMOCION, ;
              TOTALES   With 0
      Select Dis
      Skip
   EndDo

   *> Calcular los totales.
   Select DisGen
   Replace All TOTALES With VENTAS + PROMOCION

   *> Guardar tabla en formato EXCEL.
   Select DisGen
   Copy To DisGen Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de distribuci�n expediciones / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisExp
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribuci�n expediciones / mes.
   Create Table DisExp (MES       C(10), ;
                        VENTAS    N(5, 0), ;
                        PROMOCION N(5, 0), ;
                        TOTALES   N(5, 0))

   *> Cargar en la tabla las expediciones.
   _Selec = "F63cFecMov, F63cExpVta, F63cExpPro, (F63cExpVta + F63cExpPro) As F63cExpTot"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n expediciones/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n expediciones/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisExp
      Append Blank
      Replace MES       With "01/" + SubStr(Dis.F63cFecMov, 5, 2) + "/" + SubStr(Dis.F63cFecMov, 1, 4), ;
              VENTAS    With Dis.F63cExpVta, ;
              PROMOCION With Dis.F63cExpPro, ;
              TOTALES   With Dis.F63cExpTot

      Select Dis
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select DisExp
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select DisExp
   Copy To DisExp Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de distribuci�n bultos / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisBul
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribuci�n bultos / mes.
   Create Table DisBul (MES       C(10), ;
                        VENTAS    N(6, 0), ;
                        PROMOCION N(6, 0), ;
                        TOTALES   N(6, 0))

   *> Cargar en la tabla los bultos.
   _Selec = "F63cFecMov, F63cBulVta, F63cBulPro, (F63cBulVta + F63cBulPro) As F63cBulTot" + ;
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n bultos/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n bultos/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisBul
      Append Blank
      Replace MES       With "01/" + SubStr(Dis.F63cFecMov, 5, 2) + "/" + SubStr(Dis.F63cFecMov, 1, 4), ;
              VENTAS    With Dis.F63cBulVta, ;
              PROMOCION With Dis.F63cBulPro, ;
              TOTALES   With Dis.F63cBulTot

      Select Dis
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select DisBul
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select DisBul
   Copy To DisBul Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de distribuci�n kilos / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisKgs
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribuci�n kilos / mes.
   Create Table DisKgs (MES       C(10), ;
                        VENTAS    N(7, 0), ;
                        PROMOCION N(7, 0), ;
                        TOTALES   N(7, 0))

   *> Cargar en la tabla los kilos.
   _Selec = "F63cFecMov, F63cKgsVta, F63cKgsPro, (F63cKgsVta + F63cKgsPro) As F63cKgsTot"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n kilos/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n kilos/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisKgs
      Append Blank
      Replace MES       With "01/" + SubStr(Dis.F63cFecMov, 5, 2) + "/" + SubStr(Dis.F63cFecMov, 1, 4), ;
              VENTAS    With Dis.F63cKgsVta, ;
              PROMOCION With Dis.F63cKgsPro, ;
              TOTALES   With Dis.F63cKgsTot

      Select Dis
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select DisKgs
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select DisKgs
   Copy To DisKgs Type XL5

Return .T.

*>----------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gr�fico de distribuci�n pesetas / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexi�n con la base de datos.
*>         _em, c�digo de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisPts
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribuci�n pesetas / mes.
   Create Table DisPts (MES       C(10), ;
                        VENTAS    N(9, 0), ;
                        PROMOCION N(9, 0), ;
                        TOTALES   N(9, 0))

   *> Cargar en la tabla las pesetas.
   _Selec = "F63cFecMov, F63cPtsVta, F63cPtsPro, (F63cPtsVta + F63cPtsPro) As F63cPtsTot"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribuci�n pesetas/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribuci�n pesetas/mes'
      =Anomalias()
      Return err
   EndIf

   *> Pasar datos a la tabla.
   Select Dis
   Go Top
   Do While !Eof()
      Select DisPts
      Append Blank
      Replace MES       With "01/" + SubStr(Dis.F63cFecMov, 5, 2) + "/" + SubStr(Dis.F63cFecMov, 1, 4), ;
              VENTAS    With Dis.F63cPtsVta, ;
              PROMOCION With Dis.F63cPtsPro, ;
              TOTALES   With Dis.F63cPtsTot

      Select Dis
      Skip
   EndDo

   *> Calcular el nombre del mes.
   Select DisPts
   Replace All MES With Upper(CMonth(CToD(MES)))

   *> Guardar tabla en formato EXCEL.
   Select DisPts
   Copy To DisPts Type XL5

Return .T.

*------------------------------------------------------------
*> Crear gr�fico de ubicaciones libres/ocupadas (Mapa).
*> Recibe: OcuMap, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaOcuMap.
*------------------------------------------------------------
Procedure GraficoOcuMap

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'OCUMAP.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select OcuMap

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64 + lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustSTem(_lcHoja)
   * lcHoja = JustFName(_lcHoja)
   * lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   * lcPlanilla = Sys(5) + CurDir() + _lcHoja
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico ubicaciones libres/ocupadas (Mapa) ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 70           && Tipo gr�fico: Pastel 3D.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text= "UBICACIONES LIBRES/OCUPADAS (MAPA)"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      *> Con tipo pastel no hay t�tulo.
*      With .ActiveChart
*         .Axes(1, 1).HasTitle = .T.
*         .Axes(1, 1).AxisTitle.Text = "Ubicaciones"
*         .Axes(2, 1).HasTitle = .T.
*         .Axes(2, 1).AxisTitle.Text = "Ocupaci�n"
*         .Deselect
*      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de ubicaciones libres/ocupadas (Zonas).
*> Recibe: OcuZon, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaOcuZon.
*------------------------------------------------------------
Procedure GraficoOcuZon

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'OCUZON.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select OcuZon

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico ubicaciones libres/ocupadas (Zonas) ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 51           && Tipo gr�fico: Barras.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "UBICACIONES LIBRES/OCUPADAS (ZONAS)"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "ZONAS"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "OCUPACION (HUECOS)"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de ubicaciones libres/ocupadas (Tama�os).
*> Recibe: OcuTam, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaOcuTam.
*------------------------------------------------------------
Procedure GraficoOcuTam

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'OCUTAM.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select OcuTam

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico ubicaciones libres/ocupadas (Tama�os) ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 52           && Tipo gr�fico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "UBICACIONES LIBRES/OCUPADAS (TAMA�OS)"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "TAMA�OS"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "OCUPACION (HUECOS)"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de ocupaci�n media art�culo/mes.
*> Recibe: ArtOcuMed, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaArtOcuMed.
*------------------------------------------------------------
Procedure GraficoArtOcuMed

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'ARTOCUMED.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select ArtOcuMed

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico ocupaci�n media art�culo / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 52           && Tipo gr�fico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "OCUPACION MEDIA ARTICULO / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "OCUPACION"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de movimientos operario/mes.
*> Recibe: OpeMov, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaOpeMov.
*------------------------------------------------------------
Procedure GraficoOpeMov

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'OPEMOV.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select OpeMov

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico movimientos operario / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 52           && Tipo gr�fico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "MOVIMIENTOS OPERARIO / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "MOVIMIENTOS"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de listas de trabajo operario/mes.
*> Recibe: OpeMov, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaOpeLst.
*------------------------------------------------------------
Procedure GraficoOpeLst

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'OPELST.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select OpeLst

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico listas de trabajo operario / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 52           && Tipo gr�fico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "LISTAS OPERARIO / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "LISTAS TRABAJO"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de kilos preparados operario/mes.
*> Recibe: OpeMov, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaOpeKgs.
*------------------------------------------------------------
Procedure GraficoOpeKgs

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'OPEKGS.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select OpeKgs

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico kilos preparados operario / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 52           && Tipo gr�fico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "KILOS OPERARIO / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "KILOS PREPARADOS"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de distribuci�n general.
*> Recibe: DisGen, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaDisGen.
*------------------------------------------------------------
Procedure GraficoDisGen

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'DISGEN.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select DisGen

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico distribuci�n general ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 51           && Tipo gr�fico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "ESTADISTICA DISTRIBUCION GENERAL"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "CONCEPTOS"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "V A L O R E S"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de distribuci�n expediciones/mes.
*> Recibe: DisExp, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaDisExp.
*------------------------------------------------------------
Procedure GraficoDisExp

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'DISEXP.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select DisExp

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico distribuci�n expediciones / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 51           && Tipo gr�fico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "EXPEDICIONES / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "EXPEDICIONES"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de distribuci�n bultos/mes.
*> Recibe: DisBul, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaDisBul.
*------------------------------------------------------------
Procedure GraficoDisBul

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'DISBUL.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select DisBul

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico distribuci�n bultos / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 51           && Tipo gr�fico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "N� BULTOS / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "N� BULTOS"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de distribuci�n kilos/mes.
*> Recibe: DisKgs, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaDisKgs.
*------------------------------------------------------------
Procedure GraficoDisKgs

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'DISKGS.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select DisKgs

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico distribuci�n kilos / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 51           && Tipo gr�fico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "N� KILOS / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "N� KILOS"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*------------------------------------------------------------
*> Crear gr�fico de distribuci�n pesetas/mes.
*> Recibe: DisPts, Tabla DBF con los c�lculos, generada
*> en el procedimiento CreaDisPts.
*------------------------------------------------------------
Procedure GraficoDisPts

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de c�lculo.
   _lcHoja = PutFile('GRABAR GRAFICO', 'DISPTS.XLS', 'XLS|XL5')
   If Empty(_lcHoja)
      Return
   EndIf

   Select DisPts

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = JustFName(_lcHoja)
   lcHoja = Left(lcHoja, At('.', lcHoja) - 1)
   lcPlanilla = _lcHoja

   *> Crear el objeto EXCEL.
   loExcel = CreateObject("Excel.Application")

   Wait Window 'Generando gr�fico distribuci�n pesetas / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de c�lculo.

      *---- A�ado gr�fico de l�neas
      .Charts.Add                           && A�adir hoja.
      .ActiveChart.ChartType = 51           && Tipo gr�fico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho l�nea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> T�tulo de gr�fico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "PESETAS / MES"
      .ActiveChart.ChartTitle.Select
      With .Selection.Font
         .Name = "Arial"
         .FontStyle = "Negrita"
         .Size = 16
      EndWith

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.Select
      .Selection.Position = -4160           && xlTop

      With .Selection.Font
         .Name = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .Size = 16
      EndWith

      *> Ejes.
      With .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.Text = "MESES"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.Text = "PESETAS"
         .Deselect
      EndWith

      *> Grabar planilla y salir.
      .Visible = .F.
      .ActiveWorkbook.Save
      .workbooks.Close
   EndWith
   Release loExcel

Return

*>-----------------------------------------------------------------------------
*> Abrir conexi�n con la base de datos.
*> Devuelve: Conexi�n con base de datos, o NULL si no tuvo �xito.
*>-----------------------------------------------------------------------------
Procedure AbrirConexion

Local oProcaot, cInitFile
Local nConexion

oProcaot = CreateObject("FOXAPI")

cInitFile = AllTrim(Sys(5)) + CurDir() + 'CONNDB.INI'

If !File(cInitFile)
   _LxErr = "No existe el fichero de par�metros de conectividad [" + cInitFile + "]" + cr
   =Anomalias()
   Return .Null.
EndIf

nConexion = AbrirConexion(cInitFile)

Return nConexion
