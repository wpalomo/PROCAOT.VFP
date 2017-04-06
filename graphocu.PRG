*=========================================================
SET DATE DMY
SET SAFETY OFF

*> Crea las tablas de trabajo.
DO CreaLibOcuMap
DO CreaLibOcuZon

*> Crear gráfico de ubicaciones Libres/Ocupadas (Mapa).
Do GraficoLibOcuMap

*> Crear gráfico de ubicaciones Libres/Ocupadas (Zona).
Do GraficoLibOcuZon

*--- Genera gráfico anual
*DO GraficoAnual
*
*--- Genera gráfico semanal
*DO GraficoSemanal

CLOSE TABLES ALL
RETURN

*============================================================

*------------------------------------------------------------
* PROCEDURE CreaLibOcuMap
*------------------------------------------------------------
* Crea los archivos como DBFs y después se guardan como .XLS
*------------------------------------------------------------
PROCEDURE CreaLibOcuMap

   Local nd
   
   *> Crear la conexión con ORACLE
   Open Database CONEXION
   nd = SqlConnect('TEINFOS400')
   If nd <= 0
      =MessageBox('Error al intentar conectar con ORACLE', 0)
      Close DataBase
      Suspend
   EndIf

   Close DataBase

   *> Crear la tabla de ubicaciones Libres/Ocupadas.
   CREATE TABLE LibOcuMap (UTexto C(10), ;
                           UTotal N(4, 0))

   *> Cargar tabla Libres/Ocupadas.
   _Selec = "Select Count (*) As Ubic " + ;
            "From F10c001 " + ;
            "Where F10cCodUbi Not In " + ;
            "(Select F16cCodUbi From F16c001 " + ;
            "Where F16cCodUbi = F10cCodUbi)"

   err = SqlExec(nd, _Selec, 'Ubi')
   If err <= 0
      =MessageBox('Error cargando ubicaciones libres', 0)
      =SqlDisConnect(nd)
      Suspend
   EndIf

   *> Pasar datos a tabla.
   =SqlMoreResults(nd)
   Select LibOcuMap
   Append Blank
   Replace UTotal With Ubi.Ubic, ;
           UTexto With 'LIBRES'

   TotalUbics = Ubi.Ubic

   *> Cargar tabla Libres/Ocupadas.
   _Selec = "Select Count (*) As Ubic " + ;
            "From F10c001 " + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c001 " + ;
            "Where F16cCodUbi = F10cCodUbi)"

   err = SqlExec(nd, _Selec, 'Ubi')
   If err <= 0
      =MessageBox('Error cargando ubicaciones ocupadas', 0)
      =SqlDisConnect(nd)
      Suspend
   EndIf

   =SqlMoreResults(nd)

   *> Pasar datos a tabla.
   Select LibOcuMap
   Append Blank
   Replace UTotal With Ubi.Ubic, ;
           UTexto With 'OCUPADAS'

   *> Cerrar la conexión con ORACLE.
   =SqlDisConnect(nd)

   *> Guardar tabla en formato EXCEL.
   SELECT LibOcuMap
   COPY TO LibOcuMap TYPE XL5

   RETURN

ENDPROC

*------------------------------------------------------------
* PROCEDURE CreaLibOcuZon
*------------------------------------------------------------
* Crea los archivos como DBFs y después se guardan como .XLS
*------------------------------------------------------------
PROCEDURE CreaLibOcuZon

   Local nd
   
   *> Crear la conexión con ORACLE
   Open Database CONEXION
   nd = SqlConnect('TEINFOS400')
   If nd <= 0
      =MessageBox('Error al intentar conectar con ORACLE', 0)
      Close DataBase
      Suspend
   EndIf

   Close DataBase

   *> Crear la tabla de ubicaciones Libres/Ocupadas.
   CREATE TABLE LibOcuZon (CZona C(10), ;
                           ULibr N(4, 0), ;
                           UOcup N(4, 0), ;
                           UTota N(4,0))

   *> Cargar tabla Libres/Ocupadas.
   _Selec = "Select SubStr(F10cCodUbi, 1, 6) As CZona, " + ;
            "Count (*) As Ubic " + ;
            "From F10c001 " + ;
            "Where F10cCodUbi Not In " + ;
            "(Select F16cCodUbi From F16c001 " + ;
            "Where F16cCodUbi = F10cCodUbi) " + ;
            "Group By SubStr(F10cCodUbi, 1, 6)"

   err = SqlExec(nd, _Selec, 'Ubi')
   If err <= 0
      =MessageBox('Error cargando ubicaciones libres', 0)
      =SqlDisConnect(nd)
      Suspend
   EndIf

   =SqlMoreResults(nd)

   *> Pasar datos a tabla.
   Select Ubi
   Go Top
   Do While !Eof()
      Select LibOcuZon
      Append Blank
      Replace CZona With Ubi.CZona, ;
              ULibr With Ubi.Ubic, ;
              UOcup With 0
      Select Ubi
      Skip
   EndDo

   *> Cargar tabla Libres/Ocupadas.
   _Selec = "Select SubStr(F10cCodUbi, 1, 6) As CZona, " + ;
            "Count (*) As Ubic " + ;
            "From F10c001 " + ;
            "Where F10cCodUbi In " + ;
            "(Select F16cCodUbi From F16c001 " + ;
            "Where F16cCodUbi = F10cCodUbi) " + ;
            "Group By SubStr(F10cCodUbi, 1, 6)"

   err = SqlExec(nd, _Selec, 'Ubi')
   If err <= 0
      =MessageBox('Error cargando ubicaciones ocupadas', 0)
      =SqlDisConnect(nd)
      Suspend
   EndIf

   =SqlMoreResults(nd)

   *> Pasar datos a tabla.
   Select Ubi
   Go Top
   Do While !Eof()
      Select LibOcuZon
      Locate For CZona = Ubi.CZona
      If !Found()
         Append Blank
         Replace CZona With Ubi.CZona, ;
                 ULibr With 0
      EndIf
      Replace UOcup With Ubi.Ubic

      Select Ubi
      Skip
   EndDo

   *> Pasar datos de totales a tabla.
   Select LibOcuZon
   Replace All UTota With ULibr + UOcup
   Go Top

   *> Cerrar la conexión con ORACLE.
   =SqlDisConnect(nd)

   *> Guardar tabla en formato EXCEL.
   SELECT LibOcuZon
   COPY TO LibOcuZon TYPE XL5

   RETURN

ENDPROC

*============================================================

*-------------------------------------------------------
*> Crear gráfico de ubicaciones libres/ocupadas (Mapa).
* PROCEDURE GraficoLibOcuMap
*-------------------------------------------------------

PROCEDURE GraficoLibOcuMap

   LOCAL lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel

   Select LibOcuMap

   *> Contar las filas.
   lnFil = RECCOUNT() + 1

   *> Contar las columnas.
   lnCol = FCOUNT()

   *> Generar el rango de celdas.
   lcRango = "A1:" + CHR(64+lnCol) + ALLTRIM(STR(lnFil))
   lcHoja = "LibOcuMap"
   lcPlanilla = SYS(5) + CURDIR() + "LibOcuMap.Xls"

   *> Crear el objeto EXCEL.
   loExcel = CREATEOBJECT("Excel.Application")

   WITH loExcel.APPLICATION
      .VISIBLE = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.OPEN(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.ADD                           && Añadir hoja.
      .ActiveChart.ChartType = 70           && Tipo gráfico: Pastel 3D.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).RANGE(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).SELECT
      WITH .SELECTION.BORDER
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      ENDWITH

      *> Título de gráfico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.TEXT = "UBICACIONES LIBRES/OCUPADAS (MAPA)"
      .ActiveChart.ChartTitle.SELECT
      WITH .SELECTION.FONT
         .NAME = "Arial"
         .FontStyle = "Negrita"
         .SIZE = 16
      ENDWITH

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.SELECT
      .SELECTION.Position = -4160           && xlTop

      WITH .SELECTION.FONT
         .NAME = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .SIZE = 16
      ENDWITH

      *> Ejes.
      *> Con tipo pastel no hay título.
*      WITH .ActiveChart
*         .Axes(1, 1).HasTitle = .T.
*         .Axes(1, 1).AxisTitle.TEXT = "Ubicaciones"
*         .Axes(2, 1).HasTitle = .T.
*         .Axes(2, 1).AxisTitle.TEXT = "Ocupación"
*         .Deselect
*      ENDWITH

      *> Grabar planilla y salir.
      .VISIBLE = .F.
      .ActiveWorkbook.SAVE
      .workbooks.CLOSE
   ENDWITH
   RELEASE loExcel

   RETURN

ENDPROC

*-------------------------------------------------------
*> Crear gráfico de ubicaciones libres/ocupadas (Zona).
* PROCEDURE GraficoLibOcuZon
*-------------------------------------------------------

PROCEDURE GraficoLibOcuZon

   LOCAL lnFil, lnCol, lcRango, lcPlanilla, lcHoja, loExcel

   Select LibOcuZon

   *> Contar las filas.
   lnFil = RECCOUNT() + 1

   *> Contar las columnas.
   lnCol = FCOUNT()

   *> Generar el rango de celdas.
   lcRango = "A1:" + CHR(64+lnCol) + ALLTRIM(STR(lnFil))
   lcHoja = "LibOcuZon"
   lcPlanilla = SYS(5) + CURDIR() + "LibOcuZon.Xls"

   *> Crear el objeto EXCEL.
   loExcel = CREATEOBJECT("Excel.Application")

   WITH loExcel.APPLICATION
      .VISIBLE = .F.                        && Ocultar trabajo en hoja EXCEL.
      .workbooks.OPEN(lcPlanilla)           && Abrir hoja de cálculo.

      *---- Añado gráfico de líneas
      .Charts.ADD                           && Añadir hoja.
      .ActiveChart.ChartType = 51   && 52           && Tipo gráfico: Barras apiladas.
      .ActiveChart.SetSourceData(.Sheets(lcHoja).RANGE(lcRango), 2)
      .ActiveChart.Location(1, "Grafico")
      .ActiveChart.HasDataTable = .F.

      *--- Estilo y ancho línea
      .ActiveChart.SeriesCollection(1).SELECT
      WITH .SELECTION.BORDER
         .Weight = 4                        && xlThick
         .LineStyle = 1                     && xlContinuous
      ENDWITH

      *> Título de gráfico.
      .ActiveChart.HasTitle = .T.
      .ActiveChart.ChartTitle.TEXT = "UBICACIONES LIBRES/OCUPADAS (ZONAS)"
      .ActiveChart.ChartTitle.SELECT
      WITH .SELECTION.FONT
         .NAME = "Arial"
         .FontStyle = "Negrita"
         .SIZE = 16
      ENDWITH

      *> Leyenda.
      .ActiveChart.HasLegend = .T.
      .ActiveChart.Legend.SELECT
      .SELECTION.Position = -4160           && xlTop

      WITH .SELECTION.FONT
         .NAME = "Bookman Old Style"
         .FontStyle = "Cursiva"
         .SIZE = 16
      ENDWITH

      *> Ejes.
      WITH .ActiveChart
         .Axes(1, 1).HasTitle = .T.
         .Axes(1, 1).AxisTitle.TEXT = "Zonas"
         .Axes(2, 1).HasTitle = .T.
         .Axes(2, 1).AxisTitle.TEXT = "Ocupación"
         .Deselect
      ENDWITH

      *> Grabar planilla y salir.
      .VISIBLE = .F.
      .ActiveWorkbook.SAVE
      .workbooks.CLOSE
   ENDWITH
   RELEASE loExcel

   RETURN

ENDPROC
