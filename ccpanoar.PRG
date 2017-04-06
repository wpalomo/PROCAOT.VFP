*>
*> Procedures
*>
*> GenOcuMap .................................... Ubicaciones libres/ocupadas - Mapa(Gráfico).
*> GenOcuZon .................................... Ubicaciones libres/ocupadas - Zonas(Gráfico).
*> GenOcuTam .................................... Ubicaciones libres/ocupadas - Tamaños(Gráfico).
*> GenArtOcuMed ................................. Ocupación media por artículo - mes(Gráfico).
*>
*> CreaOcuMap ................................... Tablas de GenOcuMap.
*> CreaOcuZon ................................... Tablas de GenOcuZon.
*> CreaOcuTam ................................... Tablas de GenOcuTam.
*> CreaArtOcuMed ................................ Tablas de GenArtOcuMed.
*>
*> GraficoOcuMap ................................ Gráfico de GenOcuMap.
*> GraficoOcuZon ................................ Gráfico de GenOcuZon.
*> GraficoOcuTam ................................ Gráfico de GenOcuTam.
*> GraficoArtOcuMed ............................. Gráfico de GenArtOcuMed.
*>
*> AbrirConexion ................................ Crear conexión con base de datos.

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
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuMap(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Mapa)' + cr
   Do Form St3Inc
   _LxErr = ''
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
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuZon(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Zonas)' + cr
   Do Form St3Inc
   _LxErr = ''
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
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuTam(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Tamaños)' + cr
   Do Form St3Inc
   _LxErr = ''
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
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaArtOcuMed(_ASql, _em, _CodArt)
   _LxErr = 'Error cargando ocupación media artículo / mes' + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear gráfico de ocupación media artículo / mes.
Do GraficoArtOcuMed

If Used('ArtOcuMed')
   Use In ArtOcuMed
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

err = SqlExec(_ASql, _Selec, 'Ubi')
If err <= 0
   _LxErr = 'Error cargando ubicaciones libres (Mapa)' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Pasar datos a la tabla.
=SqlMoreResults(_ASql)
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

err = SqlExec(_ASql, _Selec, 'Ubi')
If err <= 0
   _LxErr = 'Error cargando ubicaciones ocupadas (Mapa)' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Pasar datos a la tabla.
=SqlMoreResults(_ASql)
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

err = SqlExec(_ASql, _Selec, 'Ubi')
If err <= 0
   _LxErr = 'Error cargando ubicaciones reservadas (Mapa)' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Pasar datos a la tabla.
=SqlMoreResults(_ASql)
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

   err = SqlExec(_ASql, _Selec, 'Zon')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones libres (Zona)' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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

   err = SqlExec(_ASql, _Selec, 'Zon')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones ocupadas (Zona)' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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

   err = SqlExec(_ASql, _Selec, 'Zon')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones reservadas (Zona)' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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

   err = SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones libres (Tamaños)' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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

   err = SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones ocupadas (Tamaños)' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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

   err = SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones reservadas (Tamaños)' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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
Private err, _Selec, _LIBRES, _OCUPADAS, _RESERVADAS

   *> Crear la tabla de ocupación media artículo / mes.
   Create Table ArtOcumed (MES       C(10), ;
                           OCUPACION N(6, 0))

   *> Cargar en la tabla la ocupación del artículo.
   _Selec = "Select F60cFecMov, F60cMedOcu " + ;
            "From F60c" + _em + Space(1) + ;
            "Where F60cCodPro='" + m.CodPro + "'" + ;
            " And  F60cCodArt='" + _CodArt + "'" + ;
            " And  SubStr(F60cFecMov, 1, 4)='" + AllTrim(Str(Year(Date()))) + "'"

   Wait Window 'Leyendo ocupación media artículo/mes ...' NoWait

   err = SqlExec(_ASql, _Selec, 'Med')
   If err <= 0
      _LxErr = 'Error cargando ocupación media artículo' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   =SqlMoreResults(_ASql)

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

*------------------------------------------------------------
*> Crear gráfico de ubicaciones libres/ocupadas (Mapa).
*> Recibe: OcuMap, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOcuMap.
*------------------------------------------------------------
Procedure GraficoOcuMap

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel

   Select OcuMap

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64 + lnCol) + AllTrim(Str(lnFil))
   lcHoja = "OcuMap"
   lcPlanilla = Sys(5) + CurDir() + "OcuMap.Xls"

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

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel

   Select OcuZon

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = "OcuZon"
   lcPlanilla = Sys(5) + CurDir() + "OcuZon.Xls"

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
*> Crear gráfico de ubicaciones libres/ocupadas (Tamaños).
*> Recibe: OcuTam, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaOcuTam.
*------------------------------------------------------------
Procedure GraficoOcuTam

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel

   Select OcuTam

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = "OcuTam"
   lcPlanilla = Sys(5) + CurDir() + "OcuTam.Xls"

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
*> Crear gráfico de ocupación media artículo/mes.
*> Recibe: ArtOcuMed, Tabla DBF con los cálculos, generada
*> en el procedimiento CreaArtOcuMed.
*------------------------------------------------------------
Procedure GraficoArtOcuMed

   Local lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel

   Select ArtOcuMed

   *> Contar las filas.
   lnFil = RecCount() + 1

   *> Contar las columnas.
   lnCol = FCount()

   *> Generar el rango de celdas.
   lcRango = "A1:" + Chr(64+lnCol) + AllTrim(Str(lnFil))
   lcHoja = "ArtOcuMed"
   lcPlanilla = Sys(5) + CurDir() + "ArtOcuMed.Xls"

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
   Do Form St3Inc With .T.
   Return .Null.
EndIf

nConexion = AbrirConexion(cInitFile)

Return nConexion

