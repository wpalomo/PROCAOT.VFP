*>
*> Procedures
*>
*> GenOcuMap .................................... Ubicaciones libres/ocupadas - Mapa(Gr�fico).
*> GenOcuZon .................................... Ubicaciones libres/ocupadas - Zonas(Gr�fico).
*> GenOcuTam .................................... Ubicaciones libres/ocupadas - Tama�os(Gr�fico).
*> GenArtOcuMed ................................. Ocupaci�n media por art�culo - mes(Gr�fico).
*>
*> CreaOcuMap ................................... Tablas de GenOcuMap.
*> CreaOcuZon ................................... Tablas de GenOcuZon.
*> CreaOcuTam ................................... Tablas de GenOcuTam.
*> CreaArtOcuMed ................................ Tablas de GenArtOcuMed.
*>
*> GraficoOcuMap ................................ Gr�fico de GenOcuMap.
*> GraficoOcuZon ................................ Gr�fico de GenOcuZon.
*> GraficoOcuTam ................................ Gr�fico de GenOcuTam.
*> GraficoArtOcuMed ............................. Gr�fico de GenArtOcuMed.
*>
*> AbrirConexion ................................ Crear conexi�n con base de datos.

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
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaOcuTam(_ASql, _em)
   _LxErr = 'Error cargando ubicaciones libres/ocupadas (Tama�os)' + cr
   Do Form St3Inc
   _LxErr = ''
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
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear las tablas de trabajo.
If !CreaArtOcuMed(_ASql, _em, _CodArt)
   _LxErr = 'Error cargando ocupaci�n media art�culo / mes' + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Crear gr�fico de ocupaci�n media art�culo / mes.
Do GraficoArtOcuMed

If Used('ArtOcuMed')
   Use In ArtOcuMed
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

   err = SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones libres (Tama�os)' + cr + ;
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

   Wait Window 'Leyendo ubicaciones ocupadas (Tama�os) ...' NoWait

   err = SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones ocupadas (Tama�os)' + cr + ;
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

   Wait Window 'Leyendo ubicaciones reservadas (Tama�os) ...' NoWait

   err = SqlExec(_ASql, _Selec, 'Tam')
   If err <= 0
      _LxErr = 'Error cargando ubicaciones reservadas (Tama�os)' + cr + ;
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
Private err, _Selec, _LIBRES, _OCUPADAS, _RESERVADAS

   *> Crear la tabla de ocupaci�n media art�culo / mes.
   Create Table ArtOcumed (MES       C(10), ;
                           OCUPACION N(6, 0))

   *> Cargar en la tabla la ocupaci�n del art�culo.
   _Selec = "Select F60cFecMov, F60cMedOcu " + ;
            "From F60c" + _em + Space(1) + ;
            "Where F60cCodPro='" + m.CodPro + "'" + ;
            " And  F60cCodArt='" + _CodArt + "'" + ;
            " And  SubStr(F60cFecMov, 1, 4)='" + AllTrim(Str(Year(Date()))) + "'"

   Wait Window 'Leyendo ocupaci�n media art�culo/mes ...' NoWait

   err = SqlExec(_ASql, _Selec, 'Med')
   If err <= 0
      _LxErr = 'Error cargando ocupaci�n media art�culo' + cr + ;
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
*> Crear gr�fico de ubicaciones libres/ocupadas (Mapa).
*> Recibe: OcuMap, Tabla DBF con los c�lculos, generada
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
*> Crear gr�fico de ubicaciones libres/ocupadas (Tama�os).
*> Recibe: OcuTam, Tabla DBF con los c�lculos, generada
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
*> Crear gr�fico de ocupaci�n media art�culo/mes.
*> Recibe: ArtOcuMed, Tabla DBF con los c�lculos, generada
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
   Do Form St3Inc With .T.
   Return .Null.
EndIf

nConexion = AbrirConexion(cInitFile)

Return nConexion

