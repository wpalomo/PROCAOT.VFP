*>
*> Procedures
*>
*> GenOcuMap .................................... Ubicaciones libres/ocupadas - Mapa(Gráfico).
*> GenOcuZon .................................... Ubicaciones libres/ocupadas - Zonas(Gráfico).
*> GenOcuTam .................................... Ubicaciones libres/ocupadas - Tamaños(Gráfico).
*> GenArtOcuMed ................................. Ocupación media por artículo - mes(Gráfico).
*> GenOpeMov .................................... Movimientos operario - mes(Gráfico).
*> GenOpeLst .................................... Listas operario - mes(Gráfico).
*> GenOpeKgs .................................... Kilos operario - mes(Gráfico).
*> GenDisGen .................................... Distribución general(Gráfico).
*> GenDisExp .................................... Expediciones distribución - mes(Gráfico).
*> GenDisBul .................................... Bultos distribución - mes(Gráfico).
*> GenDisKgs .................................... Kilos distribución - mes(Gráfico).
*> GenDisPts .................................... Pesetas distribución - mes(Gráfico).
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
*> GraficoOcuMap ................................ Gráfico de GenOcuMap.
*> GraficoOcuZon ................................ Gráfico de GenOcuZon.
*> GraficoOcuTam ................................ Gráfico de GenOcuTam.
*> GraficoArtOcuMed ............................. Gráfico de GenArtOcuMed.
*> GraficoOpeMov ................................ Gráfico de GenOpeMov.
*> GraficoOpeLst ................................ Gráfico de GenOpeLst.
*> GraficoOpeKgs ................................ Gráfico de GenOpeKgs.
*> GraficoDisGen ................................ Gráfico de GenDisGen.
*> GraficoDisExp ................................ Gráfico de GenDisExp.
*> GraficoDisBul ................................ Gráfico de GenDisBul.
*> GraficoDisKgs ................................ Gráfico de GenDisKgs.
*> GraficoDisPts ................................ Gráfico de GenDisPts.
*>
*> AbrirConexion ................................ Crear conexión con base de datos.

*> OBSERVACIONES:
*>   - Añadir selección de nombre de Hoja de cálculo destino. AVC - 08.04.2000

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de ubicaciones libres/ocupadas, Mapa.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenOcuMap
LParameters _ASql, _em

*> Crear, si cal, la conexión con ORACLE.
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

*> Crear gráfico de ubicaciones Libres/Ocupadas (Mapa).
Do GraficoOcuMap

If Used('OcuMap')
   Use In OcuMap
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de ubicaciones libres/ocupadas, por Zonas.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenOcuZon
LParameters _ASql, _em

*> Crear, si cal, la conexión con ORACLE.
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

*> Crear gráfico de ubicaciones Libres/Ocupadas (Zonas).
Do GraficoOcuZon

If Used('OcuZon')
   Use In OcuZon
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de ubicaciones libres/ocupadas, por Tamaños.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenOcuTam
LParameters _ASql, _em

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuTam(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Tamaños)' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de ubicaciones Libres/Ocupadas (Tamaños).
Do GraficoOcuTam

If Used('OcuTam')
   Use In OcuTam
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de ocupación media por artículo/mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>   _CodArt -> Artículo a calcular.
*>-----------------------------------------------------------------------------
Procedure GenArtOcuMed
LParameters _ASql, _em, _CodArt

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaArtOcuMed(_ASql, _em, _CodArt)
   _LxErr = 'Error cargando ocupación media artículo / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de ocupación media artículo / mes.
Do GraficoArtOcuMed

If Used('ArtOcuMed')
   Use In ArtOcuMed
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de movimientos por operario / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>   _CodOpe -> Operario a calcular.
*>-----------------------------------------------------------------------------
Procedure GenOpeMov
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
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

*> Crear gráfico de movimientos operario / mes.
Do GraficoOpeMov

If Used('OpeMov')
   Use In OpeMov
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de movimientos por operario / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>   _CodOpe -> Operario a calcular.
*>-----------------------------------------------------------------------------
Procedure GenOpeLst
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
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

*> Crear gráfico de listas operario / mes.
Do GraficoOpeLst

If Used('OpeLst')
   Use In OpeLst
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de distribución expediciones / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisExp
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisExp(_ASql, _em)
   _LxErr = 'Error cargando distribución expediciones / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de distribución expediciones / mes.
Do GraficoDisExp

If Used('DisExp')
   Use In DisExp
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de distribución bultos / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisBul
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisBul(_ASql, _em)
   _LxErr = 'Error cargando distribución bultos / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de distribución bultos / mes.
Do GraficoDisBul

If Used('DisBul')
   Use In DisBul
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de distribución kilos / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisKgs
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisKgs(_ASql, _em)
   _LxErr = 'Error cargando distribución kilos / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de distribución kilos / mes.
Do GraficoDisKgs

If Used('DisKgs')
   Use In DisKgs
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de distribución pesetas / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisPts
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisPts(_ASql, _em)
   _LxErr = 'Error cargando distribución pesetas / mes' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de distribución pesetas / mes.
Do GraficoDisPts

If Used('DisPts')
   Use In DisPts
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de kilos preparados por operario / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>   _CodOpe -> Operario a calcular.
*>-----------------------------------------------------------------------------
Procedure GenOpeKgs
LParameters _ASql, _em, _CodOpe

*> Crear, si cal, la conexión con ORACLE.
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

*> Crear gráfico de kilos preparados operario / mes.
Do GraficoOpeKgs

If Used('OpeKgs')
   Use In OpeKgs
EndIf

Return

*>-----------------------------------------------------------------------------
*> Generación de gráficos (EXCEL) de estadística distribución general / mes.
*> Recibe:
*>   _ASql ---> Conexión con ORACLE. Si no existe la crea.
*>   _em -----> Código de empresa.
*>-----------------------------------------------------------------------------
Procedure GenDisGen
LParameters _ASql, _em

*> Crear, si cal, la conexión con ORACLE.
_ASql = Iif(Type('_ASql') # 'N', AbrirConexion(), _ASql)
If Type('_ASql') # 'N'
   _LxErr = 'No se pudo conectar con la base de datos' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaDisGen(_ASql, _em)
   _LxErr = 'Error cargando estadística distribución general' + cr
   =Anomalias()
   Return .F.
EndIf

*> Crear gráfico de estadística distribución general.
Do GraficoDisGen

If Used('DisGen')
   Use In DisGen
EndIf

Return

*>--------------------------------------------------------------------------------
*> Crear las tablas de trabajo para gráfico de ubicaciones libres/ocupadas, mapa.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
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
*> Crear las tablas de trabajo para gráfico de ubicaciones libres/ocupadas, zonas.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
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
*> Crear las tablas de trabajo para gráfico de ubicaciones libres/ocupadas, tamaños.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
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

   Wait Window 'Leyendo ubicaciones libres (Tamaños) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones libres (Tamaños)' + cr + ;
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

   Wait Window 'Leyendo ubicaciones ocupadas (Tamaños) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones ocupadas (Tamaños)' + cr + ;
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

   Wait Window 'Leyendo ubicaciones reservadas (Tamaños) ...' NoWait

   err = f3_SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones reservadas (Tamaños)' + cr + ;
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

   *> Asignar texto al tipo de ubicación.
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
*> Crear las tablas de trabajo para gráfico de ocupación media artículo / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
*>         _CodArt, Artículo a calcular.
*>----------------------------------------------------------------------------------
Procedure CreaArtOcuMed
Parameters _ASql, _em, _CodArt
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de ocupación media artículo / mes.
   Create Table ArtOcumed (MES       C(10), ;
                           OCUPACION N(6, 0))

   *> Cargar en la tabla la ocupación del artículo.
   _Selec = "F60cFecMov, F60cMedOcu"
   _FromF = "F60c"
   _Where = "F60cCodPro='" + m.CodPro + "' And  F60cCodArt='" + _CodArt + "' And " + ;
            _GCSS("F60cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo ocupación media artículo/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Med')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos ocupación media artículo'
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
*> Crear las tablas de trabajo para gráfico de movimientos operario / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
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
*> Crear las tablas de trabajo para gráfico de listas de trabajo operario / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
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
*> Crear las tablas de trabajo para gráfico de kilos preparados operario / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
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
*> Crear las tablas de trabajo para gráfico estadística distribución general/mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
*>--------------------------------------------------------------------------------
Procedure CreaDisGen
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de estadística distribución general.
   Create Table DisGen (CConc     C(15), ;
                        VENTAS    N(10, 0), ;
                        PROMOCION N(10, 0), ;
                        TOTALES   N(10, 0))

   *> Cargar en la tabla los datos de distribución - expediciones.
   _Selec = "'EXPEDICIONES' As CConc, " + ;
            "Sum(F63cExpVta) As VENTAS, " + ;
            "Sum(F63cExpPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución general (Expediciones) ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución general (Expediciones)'
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

   *> Cargar en la tabla los datos de distribución - bultos.
   _Selec = "'Nº BULTOS' As CConc, " + ;
            "Sum(F63cBulVta) As VENTAS, " + ;
            "Sum(F63cBulPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución general (Bultos) ...')

   err = f3s_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución general (Bultos)'
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

   *> Cargar en la tabla los datos de distribución - kilos.
   _Selec = "'Nº KILOS' As CConc, " + ;
            "Sum(F63cKgsVta) As VENTAS, " + ;
            "Sum(F63cKgsPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución general (Kilos) ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución general (Kilos)'
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

   *> Cargar en la tabla los datos de distribución - pesetas.
   _Selec = "'PESETAS' As CConc, " + ;
            "Sum(F63cPtsVta) As VENTAS, " + ;
            "Sum(F63cPtsPro) As PROMOCION"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución general (Pesetas) ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución general (Pesetas)'
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
*> Crear las tablas de trabajo para gráfico de distribución expediciones / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisExp
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribución expediciones / mes.
   Create Table DisExp (MES       C(10), ;
                        VENTAS    N(5, 0), ;
                        PROMOCION N(5, 0), ;
                        TOTALES   N(5, 0))

   *> Cargar en la tabla las expediciones.
   _Selec = "F63cFecMov, F63cExpVta, F63cExpPro, (F63cExpVta + F63cExpPro) As F63cExpTot"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución expediciones/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución expediciones/mes'
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
*> Crear las tablas de trabajo para gráfico de distribución bultos / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisBul
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribución bultos / mes.
   Create Table DisBul (MES       C(10), ;
                        VENTAS    N(6, 0), ;
                        PROMOCION N(6, 0), ;
                        TOTALES   N(6, 0))

   *> Cargar en la tabla los bultos.
   _Selec = "F63cFecMov, F63cBulVta, F63cBulPro, (F63cBulVta + F63cBulPro) As F63cBulTot" + ;
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución bultos/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución bultos/mes'
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
*> Crear las tablas de trabajo para gráfico de distribución kilos / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisKgs
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribución kilos / mes.
   Create Table DisKgs (MES       C(10), ;
                        VENTAS    N(7, 0), ;
                        PROMOCION N(7, 0), ;
                        TOTALES   N(7, 0))

   *> Cargar en la tabla los kilos.
   _Selec = "F63cFecMov, F63cKgsVta, F63cKgsPro, (F63cKgsVta + F63cKgsPro) As F63cKgsTot"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución kilos/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución kilos/mes'
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
*> Crear las tablas de trabajo para gráfico de distribución pesetas / mes.
*> Crea un DBF y lo pasa a XLS.
*> Recibe: _ASql, conexión con la base de datos.
*>         _em, código de empresa.
*>----------------------------------------------------------------------------------
Procedure CreaDisPts
Parameters _ASql, _em
Private err, _Selec, _FromF, _Where

   *> Crear la tabla de distribución pesetas / mes.
   Create Table DisPts (MES       C(10), ;
                        VENTAS    N(9, 0), ;
                        PROMOCION N(9, 0), ;
                        TOTALES   N(9, 0))

   *> Cargar en la tabla las pesetas.
   _Selec = "F63cFecMov, F63cPtsVta, F63cPtsPro, (F63cPtsVta + F63cPtsPro) As F63cPtsTot"
   _FromF = "F63c"
   _Where = "F63cCodPro='" + m.CodPro + "' And " + ;
            _GCSS("F63cFecMov", 1, 4) + "='" + AllTrim(Str(Year(Date()))) + "'"

   =WaitWindow ('Leyendo distribución pesetas/mes ...')

   err = f3_sql(_Selec, _FromF, _Where, , , 'Dis')
   If !err .And. _xier <= 0
      _LxErr = 'No hay datos distribución pesetas/mes'
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
*> Crear gráfico de ubicaciones libres/ocupadas (Mapa).
*> Recibe: OcuMap, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOcuMap.
*------------------------------------------------------------
Procedure GraficoOcuMap

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico ubicaciones libres/ocupadas (Mapa) ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 70           && Tipo gráfico: Pastel 3D.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
      *> Con tipo pastel no hay título.
*      With .ActiveChart
*         .Axes(1, 1).HasTitle = .T.
*         .Axes(1, 1).AxisTitle.Text = "Ubicaciones"
*         .Axes(2, 1).HasTitle = .T.
*         .Axes(2, 1).AxisTitle.Text = "Ocupación"
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
*> Crear gráfico de ubicaciones libres/ocupadas (Zonas).
*> Recibe: OcuZon, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOcuZon.
*------------------------------------------------------------
Procedure GraficoOcuZon

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico ubicaciones libres/ocupadas (Zonas) ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 51           && Tipo gráfico: Barras.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de ubicaciones libres/ocupadas (Tamaños).
*> Recibe: OcuTam, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOcuTam.
*------------------------------------------------------------
Procedure GraficoOcuTam

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico ubicaciones libres/ocupadas (Tamaños) ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 52           && Tipo gráfico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "UBICACIONES LIBRES/OCUPADAS (TAMAÑOS)"
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
         .Axes(1, 1).AxisTitle.Text = "TAMAÑOS"
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
*> Crear gráfico de ocupación media artículo/mes.
*> Recibe: ArtOcuMed, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaArtOcuMed.
*------------------------------------------------------------
Procedure GraficoArtOcuMed

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico ocupación media artículo / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 52           && Tipo gráfico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de movimientos operario/mes.
*> Recibe: OpeMov, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOpeMov.
*------------------------------------------------------------
Procedure GraficoOpeMov

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico movimientos operario / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 52           && Tipo gráfico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de listas de trabajo operario/mes.
*> Recibe: OpeMov, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOpeLst.
*------------------------------------------------------------
Procedure GraficoOpeLst

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico listas de trabajo operario / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 52           && Tipo gráfico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de kilos preparados operario/mes.
*> Recibe: OpeMov, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOpeKgs.
*------------------------------------------------------------
Procedure GraficoOpeKgs

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico kilos preparados operario / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 52           && Tipo gráfico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de distribución general.
*> Recibe: DisGen, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaDisGen.
*------------------------------------------------------------
Procedure GraficoDisGen

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico distribución general ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 51           && Tipo gráfico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de distribución expediciones/mes.
*> Recibe: DisExp, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaDisExp.
*------------------------------------------------------------
Procedure GraficoDisExp

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico distribución expediciones / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 51           && Tipo gráfico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Crear gráfico de distribución bultos/mes.
*> Recibe: DisBul, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaDisBul.
*------------------------------------------------------------
Procedure GraficoDisBul

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico distribución bultos / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 51           && Tipo gráfico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "Nº BULTOS / MES"
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
         .Axes(2, 1).AxisTitle.Text = "Nº BULTOS"
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
*> Crear gráfico de distribución kilos/mes.
*> Recibe: DisKgs, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaDisKgs.
*------------------------------------------------------------
Procedure GraficoDisKgs

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico distribución kilos / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 51           && Tipo gráfico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.Text = "Nº KILOS / MES"
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
         .Axes(2, 1).AxisTitle.Text = "Nº KILOS"
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
*> Crear gráfico de distribución pesetas/mes.
*> Recibe: DisPts, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaDisPts.
*------------------------------------------------------------
Procedure GraficoDisPts

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel, _lcHoja

   *> Seleccionar el nombre de la hoja de cálculo.
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

   Wait Window 'Generando gráfico distribución pesetas / mes ...' NoWait

   With loExcel.Application
      .Visible = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.Open(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.Add                           && Añadir hoja.
      .ActiveChart.ChartType = 51           && Tipo gráfico: Columnas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).Range(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).Select
      With .Selection.Border
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      EndWith

      *> Título de gráfico.
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
*> Abrir conexión con la base de datos.
*> Devuelve: Conexión con base de datos, o NULL si no tuvo éxito.
*>-----------------------------------------------------------------------------
Procedure AbrirConexion

Local oProcaot, cInitFile
Local nConexion

oProcaot = CreateObject("FOXAPI")

cInitFile = AllTrim(Sys(5)) + CurDir() + 'CONNDB.INI'

If !File(cInitFile)
   _LxErr = "No existe el fichero de parámetros de conectividad [" + cInitFile + "]" + cr
   =Anomalias()
   Return .Null.
EndIf

nConexion = AbrirConexion(cInitFile)

Return nConexion
