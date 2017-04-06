Define Class configenv as form
Top = -2
Left = -4
Height = 306
Width = 566
ShowTips = .T.
Caption = "Configuración PROCAOT"
Closable = .F.
MaxButton = .F.
WindowState = 0

ADD OBJECT Shape1 as shape WITH Top = 50, Left = 9, Height = 49, Width = 544
ADD OBJECT Command1 as commandbutton WITH Top = 275, Left = 444, Height = 25, Width = 85, Caption = "\<Salir", TabIndex = 15, ToolTipText = "Abandonar el programa."
ADD OBJECT PcPC as textbox WITH FontSize = 14, FontUnderline = .F., ControlSource = "ThisForm.Pc", Enabled = .F., Height = 32, Left = 99, TabIndex = 2, Top = 59, Width = 220
ADD OBJECT Label1 as label WITH FontSize = 14, Caption = "Configuración S. G. A.", Height = 22, Left = 172, Top = 18, Width = 219, TabIndex = 1, ForeColor = Rgb(0,0,160)
ADD OBJECT Label2 as label WITH Caption = "PC/Usuario", Height = 13, Left = 23, Top = 69, Width = 66, TabIndex = 4
ADD OBJECT PcUser as textbox WITH FontSize = 14, FontUnderline = .F., ControlSource = "ThisForm.User", Enabled = .F., Height = 32, Left = 326, TabIndex = 3, Top = 59, Width = 220
ADD OBJECT oProcaot as procaot WITH Top = 18, Left = 459, Height = 12, Width = 15
ADD OBJECT Shape3 as shape WITH Top = 116, Left = 9, Height = 49, Width = 544
ADD OBJECT DbUsr as textbox WITH FontSize = 14, FontUnderline = .F., ControlSource = "ThisForm.DbUser", Format = "XXXXXXXXXXXXXXXXXXXX", Height = 32, Left = 133, TabIndex = 5, Top = 125, Width = 200
ADD OBJECT Label3 as label WITH Caption = "Usuario/Password", Height = 13, Left = 23, Top = 135, Width = 108, TabIndex = 7
ADD OBJECT DbPasswd as textbox WITH FontSize = 14, FontUnderline = .F., ControlSource = "ThisForm.DbPassword", Format = "XXXXXXXXXXXXXXXXXXXX", Height = 32, Left = 345, TabIndex = 6, Top = 125, Width = 200, PasswordChar = "*"
ADD OBJECT Shape2 as shape WITH Top = 180, Left = 38, Height = 107, Width = 369
ADD OBJECT Check1 as checkbox WITH Top = 190, Left = 63, Height = 17, Width = 137, Caption = "Configuración ODBC", TabIndex = 9, ToolTipText = "Definir la configuración de la conexión ODBC  con la base de datos. (Una vez por cada PC)"
ADD OBJECT Check2 as checkbox WITH Top = 212, Left = 63, Height = 17, Width = 168, Caption = "Configuración Conexiones", TabIndex = 11, ToolTipText = "Definir las conexiones del programa con la base de datos. (Una vez por cada PC)"
ADD OBJECT Check4 as checkbox WITH Top = 255, Left = 63, Height = 17, Width = 132, Caption = "Edición Conexiones", TabIndex = 14, ToolTipText = "Editar el archivo de configuración de conexiones"
ADD OBJECT PcName as textbox WITH ControlSource = "ThisForm.Pc", Enabled = .F., Height = 21, Left = 243, TabIndex = 8, Top = 188, Width = 148
ADD OBJECT ConnName as textbox WITH ControlSource = "ThisForm.Connection", Enabled = .F., Height = 21, Left = 243, TabIndex = 13, Top = 253, Width = 148
ADD OBJECT Command2 as commandbutton WITH Tag = "0", Top = 205, Left = 444, Height = 29, Width = 80, Caption = "\<Default", TabIndex = 10, ToolTipText = "Configurar conexión por defecto"
ADD OBJECT Check3 as checkbox WITH Top = 234, Left = 63, Height = 17, Width = 132, Caption = "Entorno de trabajo", TabIndex = 12, ToolTipText = "Establecer el entorno de base de datos"


PROCEDURE getpc
*>
*> Devolver el nombre de la computadora. (Rutina de la API).

ThisForm.Pc = ThisForm.oPRC.GetPc()

ENDPROC
PROCEDURE getuser
*>
*> Devolver el nombre del usuario. (Rutina de la API).

ThisForm.User = ThisForm.oPRC.GetUser()

ENDPROC
PROCEDURE getnombreconexion
*>
*> Obtener nombre conexión de archivo configuración (CONNDB).

ThisForm.Connection = ThisForm.oPRC.GetKeyIni("Conexion", ThisForm.Pc, ThisForm.InitFile)

ENDPROC
PROCEDURE oprc_access

*> Generar objeto de la clase PROCAOT para aceesos a la API.
If Type('This.oPRC') # 'O'
   This.oPRC = CreateObject('foxapi')
EndIf

Return This.oPRC

ENDPROC
PROCEDURE odbcapi
****************************************************************************
***
*** ODBCAPI.PRG  - Declares and defines for ODBC API in Visual FoxPro 3.0
***
***        Pablo Almunia Sanz
***        100341.1136@compuserve.com
***
*** Version - 1.0 - first release
***           1.1 - Bugs eliminated in RETCODE
***                                    SQLNativeSql
***                                    SQLInstallDriver
***                                    SQLInstallODBC
***                                    SQLColAttributes
***
****************************************************************************

***  CORE  ***
DECLARE SHORT SQLAllocConnect IN odbc32.dll ;
    INTEGER  nHenv, ;
    INTEGER @nHdbc
DECLARE SHORT SQLAllocEnv IN odbc32.dll ;
    INTEGER @nHenv
DECLARE SHORT SQLAllocStmt IN odbc32.dll ;
    INTEGER  nhdbc, ;
    INTEGER @nHstmt
DECLARE SHORT SQLBindCol IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nCol, ;
    INTEGER  nCType, ;
    STRING  @cValue, ;
    INTEGER  nValueMax, ;
    INTEGER @nValueSize
DECLARE SHORT SQLCancel IN odbc32.dll ALIAS SQLCancelX ;
    INTEGER  nHstmt
DECLARE SHORT SQLColAttributes IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nCol, ;
    INTEGER  nDescType, ;
    STRING  @cDesc, ;
    INTEGER  nDescMax, ;
    INTEGER @nDescSize, ;
    INTEGER @nDesc
DECLARE SHORT SQLConnect IN odbc32.dll ALIAS SQLConnectX ;
    INTEGER  nHdbc, ;
    STRING   cDSN, ;
    INTEGER  nDSNSize, ;
    STRING   cUID, ;
    INTEGER  nUIDSize, ;
    STRING   cAuthStr, ;
    INTEGER  nAuthStrSize
DECLARE SHORT SQLDescribeCol IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nCol, ;
    STRING  @cColName, ;
    INTEGER  nColNameMax, ;
    INTEGER @nColNameSize, ;
    INTEGER @nSqlType, ;
    INTEGER @nColDef, ;
    INTEGER @nScale, ;
    INTEGER @nNullable
DECLARE SHORT SQLDisconnect IN odbc32.dll ALIAS SQLDisconnectX ;
    INTEGER  nHdbc
DECLARE SHORT SQLError IN odbc32.dll ;
    INTEGER  nHenv, ;
    INTEGER  nHdbc, ;
    INTEGER  nHstmt, ;
    STRING  @cSqlState, ;
    INTEGER @nNativeError, ;
    STRING  @cErrorMsg, ;
    INTEGER  nErrorMsgMax, ;
    INTEGER @nErrorMsgSize
DECLARE SHORT SQLExecDirect IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cSqlStr, ;
    INTEGER  nSqlStrSize
DECLARE SHORT SQLExecute IN odbc32.dll ;
    INTEGER nHstmt
DECLARE SHORT SQLFetch IN odbc32.dll ;
    INTEGER  nHstmt
DECLARE SHORT SQLFreeConnect IN odbc32.dll ;
    INTEGER  nHdbc
DECLARE SHORT SQLFreeEnv IN odbc32.dll ;
    INTEGER  nHenv
DECLARE SHORT SQLFreeStmt IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nOption
DECLARE SHORT SQLGetCursorName IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING  @cCursor, ;
    INTEGER  nCursorMax, ;
    INTEGER @nCursorSize
DECLARE SHORT SQLNumResultCols IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER @nCol
DECLARE SHORT SQLPrepare IN odbc32.dll ;
    INTEGER  nHstm, ;
    STRING   cSqlStr, ;
    INTEGER  nSqlStrSize
DECLARE SHORT SQLRowCount IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  @cRow
DECLARE SHORT SQLSetCursorName IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cCursor, ;
    INTEGER  nCursorSize
DECLARE SHORT SQLTransact IN odbc32.dll ;
    INTEGER  nHenv, ;
    INTEGER  nHdbc, ;
    INTEGER  nType

***  LEVEL 1  **
DECLARE SHORT SQLBindParameter IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nPar, ;
    INTEGER  nParamType, ;
    INTEGER  nCType, ;
    INTEGER  nSQLType, ;
    INTEGER  nColDef, ;
    INTEGER  nScale, ;
    STRING  @cValue, ;
    INTEGER  nValueMax, ;
    INTEGER  nValueSize
DECLARE SHORT SQLColumns IN odbc32.dll ALIAS SQLColumnsX ;
    INTEGER  nHstmt, ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize, ;
    STRING   cColumnName, ;
    INTEGER  nColumnNameSize
DECLARE SHORT SQLDriverConnect IN odbc32.dll ;
    INTEGER  nHdbc, ;
    INTEGER  nHwnd, ;
    STRING   cConnStr, ;
    INTEGER  nConnStrSize, ;
    STRING  @cConnStrOut, ;
    INTEGER  nConnStrOutMax, ;
    INTEGER @nConnStrOutSize, ;
    INTEGER  nDriverCompletion
DECLARE SHORT SQLGetConnectOption IN odbc32.dll ;
    INTEGER  nHdbc, ;
    INTEGER  nOption, ;
    STRING  @cParam
DECLARE SHORT SQLGetData IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nCol, ;
    INTEGER  nCType, ;
    STRING  @cValue, ;
    INTEGER  nValueMax, ;
    INTEGER @nValueSize
DECLARE SHORT SQLGetFunctions IN odbc32.dll ;
    INTEGER  nHdbc, ;
    INTEGER  nFunction, ;
    STRING  @cExists
DECLARE SHORT SQLGetInfo IN odbc32.dll ;
    INTEGER  nHdbc, ;
    INTEGER  nInfoType, ;
    STRING  @cInfoValue, ;
    INTEGER  nInfoValueMax, ;
    INTEGER @nInfoValueSize
DECLARE SHORT SQLGetStmtOption IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nOption, ;
    STRING  @cParam
DECLARE SHORT SQLGetTypeInfo IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nSqlType
DECLARE SHORT SQLParamData IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING  @Value
DECLARE SHORT SQLPutData IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING  @cValue, ;
    INTEGER  nValueSize
DECLARE SHORT SQLSetConnectOption IN odbc32.dll ;
    INTEGER  nHdbc, ;
    INTEGER  nOption, ;
    INTEGER  nParam
DECLARE SHORT SQLSetStmtOption IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nOption, ;
    INTEGER  nParam
DECLARE SHORT SQLSpecialColumns IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nColType, ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize, ;
    INTEGER  nScope, ;
    INTEGER  nNullable
DECLARE SHORT SQLStatistics IN odbc32.dll ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize, ;
    INTEGER  nUnique, ;
    INTEGER  nAccuracy
DECLARE SHORT SQLTables IN odbc32.dll ALIAS SQLTablesX ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize, ;
    STRING   cTableType, ;
    INTEGER  nTableTypeSize

***  LEVEL 2  ***
DECLARE SHORT SQLBrowseConnect IN odbc32.dll ;
    INTEGER  nHdbc, ;
    STRING   cConnStr, ;
    INTEGER  nConnStrSize, ;
    STRING  @cConnStrOut, ;
    INTEGER  nConnStrOutMax, ;
    INTEGER @nConnStrOutSize
DECLARE SHORT SQLColumnPrivileges IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize, ;
    STRING   cColumnName, ;
    INTEGER  nColumnNameSize
DECLARE SHORT SQLDataSources IN odbc32.dll ;
    INTEGER  nHenv, ;
    INTEGER  nDirection, ;
    STRING  @cDSN, ;
    INTEGER  nDSNMax, ;
    INTEGER @nDSNSize, ;
    STRING  @cDescription, ;
    INTEGER  nDescriptionMax, ;
    INTEGER @nDescription
DECLARE SHORT SQLDescribeParam IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nPar, ;
    INTEGER @nSqlType, ;
    INTEGER @nColDef, ;
    INTEGER @nScale, ;
    INTEGER @nNullable
DECLARE SHORT SQLDrivers IN odbc32.dll ;
    INTEGER  nHenv, ;
    INTEGER  nDirection, ;
    STRING  @cDriverSec, ;
    INTEGER  nDriverSecMax, ;
    INTEGER @nDriverSecSize, ;
    STRING  @nDriverAtributes, ;
    INTEGER  nDriverAttrMax, ;
    INTEGER @nDriverAttrSize
DECLARE SHORT SQLExtendedFetch IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nFetchType, ;
    INTEGER  nRow, ;
    INTEGER @nRowFeched, ;
    STRING  @cRowStatus
DECLARE SHORT SQLForeignKeys IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cPkTableQualifier, ;
    INTEGER  nPkTableQualifierSize, ;
    STRING   cPkTableOwner, ;
    INTEGER  nPkTableOwnerSize, ;
    STRING   cPkTableName, ;
    INTEGER  nPkTableNameSize, ;
    STRING   cFkTableQualifier, ;
    INTEGER  nFkTableQualifierSize, ;
    STRING   cFkTableOwner, ;
    INTEGER  nFkTableOwnerSize, ;
    STRING   cFkTableName, ;
    INTEGER  nFkTableNameSize
DECLARE SHORT SQLMoreResults IN odbc32.dll ALIAS SQLMoreResultsX ;
    INTEGER nHstmt
DECLARE SHORT SQLNativeSql IN odbc32.dll ;
    INTEGER  nHdbc, ;
    STRING   cSqlStr, ;
    INTEGER  nSqlStrSize, ;
    STRING  @cSqlStrTraslated, ;
    INTEGER  cSqlStrTrasMax, ;
    INTEGER @nSqlStrTrasSize
DECLARE SHORT SQLNumParams IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER @nPar
DECLARE SHORT SQLParamOptions IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nRow, ;
    INTEGER @nRowCurrent
DECLARE SHORT SQLPrimaryKeys IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize
DECLARE SHORT SQLProcedureColumns IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING  @cProcQualifer, ;
    INTEGER  nProcQualiferSize, ;
    STRING  @cProcOwner, ;
    INTEGER  nProcOwnerSize, ;
    STRING  @cProcName, ;
    INTEGER  nProcNameSize, ;
    STRING  @cColumnName, ;
    INTEGER  nColumnNameSize
DECLARE SHORT SQLProcedures IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cProcQualifier, ;
    INTEGER  nProcQualifierSize, ;
    STRING   cProcOwner, ;
    INTEGER  nProcOwnerSize, ;
    STRING   cProcName, ;
    INTEGER  nProcNameSize
DECLARE SHORT SQLSetPos IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nRow, ;
    INTEGER  nOption, ;
    INTEGER  nLock
DECLARE SHORT SQLSetScrollOptions IN odbc32.dll ;
    INTEGER  nHstmt, ;
    INTEGER  nConcurrency, ;
    INTEGER  nRowKeyset, ;
    INTEGER  nRowRowset
DECLARE SHORT SQLTablePrivileges IN odbc32.dll ;
    INTEGER  nHstmt, ;
    STRING   cTableQualifier, ;
    INTEGER  nTableQualifierSize, ;
    STRING   cTableOwner, ;
    INTEGER  nTableOwnerSize, ;
    STRING   cTableName, ;
    INTEGER  nTableNameSize

***  INSTALL  ***
DECLARE SHORT SQLConfigDataSource IN odbccp32.dll ;
    INTEGER  nHwndParent, ;
    INTEGER  nRequest, ;
    STRING   cDriver, ;
    STRING   cAttributes
DECLARE SHORT SQLCreateDataSource IN odbccp32.dll ;
    INTEGER  nHwndParent, ;
    STRING   cDSN
DECLARE SHORT SQLGetAvailableDrivers IN odbccp32.dll ;
    STRING   cInfFile, ;
    STRING  @cBuf, ;
    INTEGER  nBufMax, ;
    INTEGER @nBufSize
DECLARE SHORT SQLGetInstalledDrivers IN odbccp32.dll ;
    STRING  @lpszBuf, ;
    INTEGER  nBufMax, ;
    INTEGER @nBufOut
DECLARE SHORT SQLGetPrivateProfileString IN odbccp32.dll ;
    STRING   cSection, ;
    STRING   cEntry, ;
    STRING   cDefault, ;
    STRING  @cRetBuffer, ;
    INTEGER  nRetBufferSize, ;
    INTEGER  cFilename
DECLARE SHORT SQLGetTranslator IN odbccp32.dll ;
    INTEGER  nHwnd, ;
    STRING  @cName, ;
    INTEGER  nNameMax, ;
    INTEGER @nNameSize, ;
    STRING  @cPath, ;
    INTEGER  nPathMax, ;
    INTEGER @nPathSize, ;
    INTEGER  nOption
DECLARE SHORT SQLInstallDriver IN odbccp32.dll ;
    STRING   cInfFile, ;
    STRING   cDriver, ;
    STRING  @cPath, ;
    INTEGER  nPathMax, ;
    INTEGER @nPathSize, ;
DECLARE SHORT SQLInstallDriverManager IN odbccp32.dll ;
    STRING  @cPath, ;
    INTEGER  nPathMax, ;
    INTEGER @nPathSize
DECLARE SHORT SQLInstallODBC IN odbccp32.dll ;
    INTEGER  nHwndParent, ;
    STRING   cInfFile, ;
    STRING   cSrcPath, ;
    STRING   cDrivers
DECLARE SHORT SQLManageDataSources IN odbccp32.dll ;
    INTEGER  nHwndParent
DECLARE SHORT SQLRemoveDefaultDataSource IN odbccp32.dll
DECLARE SHORT SQLRemoveDSNFromIni IN odbccp32.dll ;
    STRING   cDSN
DECLARE SHORT SQLValidDSN IN odbccp32.dll ;
    STRING   cDSN
DECLARE SHORT SQLWriteDSNToIni IN odbccp32.dll ;
    STRING   cDSN, ;
    STRING   cDriver
DECLARE SHORT SQLWritePrivateProfileString IN odbccp32.dll ;
    STRING   cSection, ;
    STRING   cEntry, ;
    STRING   cString, ;
    STRING   cFilename

*** End of ODBCAPI.PRG ***

ENDPROC
PROCEDURE Init
*>
*> Inicializar propiedades y métodos.

=DoDefault()

Set ClassLib To Procaot Additive

ThisForm.OdbcApi()

With ThisForm
   .DbUser = ""
   .DbPassword = ""
   .InitFile = AllTrim(Sys(5)) + CurDir() + 'CONNDB.INI'

   .GetPc
   .GetUser
   .GetNombreConexion
EndWith

ThisForm.Refresh

ENDPROC


PROCEDURE Command1.Click
*>
*> Abandonar la configuración.

ThisForm.Release

ENDPROC


PROCEDURE Check1.Click
*>
*> Configurar ODBCs con ORACLE.

Do Form OdbcCONFIG To ThisForm.Connection

=MessageBox("Conexión ODBC generada", 64, "CONEXION ODBC")

*Run OdbcAd32.Exe
This.Value = 0

ThisForm.Check2.SetFocus
ThisForm.Refresh

ENDPROC


PROCEDURE Check2.Click
*>
*> Configuración de las conexiones de FOX con ORACLE.

Open Database Conexion Exclusive

*> Aixó no xuta !!!!!!
*Modify Connection
*Close Database
*This.Value = 0
*ThisForm.Refresh

Create Connection (ThisForm.Pc) ;
   DataSource (ThisForm.Connection) ;
   UserId (AllTrim(ThisForm.DbUser)) ;
   Password (AllTrim(ThisForm.DbPassword))

*> Crear las claves en el archivo de configuración (CONNDB).
ThisForm.oPRC.WriteKeyIni("Conexion", ;
                              ThisForm.Pc, ;
                              ThisForm.Pc, ;
                              ThisForm.InitFile)

ThisForm.oPRC.WriteKeyIni("DataBase", ;
                              ThisForm.Pc, ;
                              "Conexion", ;
                              ThisForm.InitFile)

=MessageBox("Conexión ODBC configurada", 64, "CONEXION ODBC")

This.Value = 0
ThisForm.Check3.SetFocus
ThisForm.Refresh

ENDPROC


PROCEDURE Check4.Click
*>
*> Aquí se puede editar el archivo de configuración.

Modify Command (ThisForm.InitFile)

This.Value = 0

ThisForm.Command1.SetFocus
ThisForm.Refresh

ENDPROC


PROCEDURE Command2.Click
*>
*> Cambiar configuración usuario / configuración por defecto.

Do Case
   *> Modo default: Cambio a modo usuario.
   Case This.Tag = '0'
      With This
         .Tag = '1'
         .Caption = '\<User'
         .ToolTipText = 'Configurar conexión del PC'
      EndWith
      ThisForm.Pc = '.DEFAULT'

   *> Modo usuario: Cambio a modo default.
   Case This.Tag # '0'
      With This
         .Tag = '0'
         .Caption = '\<Default'
         .ToolTipText = 'Configurar conexión por defecto'
      EndWith
      ThisForm.Pc = ThisForm.oPRC.GetPc()
EndCase

=ThisForm.GetNombreConexion()
ThisForm.Refresh

ENDPROC


PROCEDURE Check3.Click
*>
*> Establecer el entorno de base de datos.

Public _Entorno, _Version, _esc, _psql, _horizontal, _enter

*> Editar las claves.
Do Sy3Env

*> Crear las claves en el archivo de configuración (CONNDB).
If Type('_Entorno')=='C' .And. Type('_Version')=='C'
   ThisForm.oPRC.WriteKeyIni("_Entorno", ;
                              ThisForm.Pc, ;
                              _Entorno, ;
                              ThisForm.InitFile)

   ThisForm.oPRC.WriteKeyIni("_Version", ;
                              ThisForm.Pc, ;
                              _Version, ;
                             ThisForm.InitFile)
   =MessageBox("Entorno de trabajo configurado", 64, "ENTORNO")
Else
   =MessageBox("Entorno de trabajo cancelado", 64, "ENTORNO")
EndIf

This.Value = 0
ThisForm.Check4.SetFocus
ThisForm.Refresh

ENDPROC


EndDefine 
Define Class st3rt as custom



PROCEDURE ft_t
*>
*> Traduce una línea de texto.

Parameters f3t_linea, _idioma
Private cLocalLinea
Local ft3_ali, ft3_nd1, ft3_nd2

If Type('_idioma') # 'N'
  _idioma = 1
EndIf

cLocalLinea = ft3_linea

If _idioma > 1 .And. !Empty(cLocalLinea)
  f3t_ali = Alias()
  f3t_nd1 = At('[', cLocalLinea)
  f3t_nd2 = At(']', cLocalLinea)

  If f3t_nd1 > 0 .And. f3t_nd2 > f3t_nd1
    f3t_lx1 = SubStr(cLocalLinea, f3t_nd1 + 1, f3t_nd2 - f3t_nd1 - 1)
    cLocalLinea = Stuff(cLocalLinea, f3t_nd1 + 1, f3t_nd2 - f3t_nd1 - 1, '')
  Else
    f3t_lx1=''
  EndIf
  If Empty(cLocalLinea) .Or. cLocalLinea = '[]'

  Else
    If Len(cLocalLinea) <= 30
      Select SYSTXT30
      Seek Left(cLocalLinea + Space(30), 30)
    Else
      Select SYSTXT
      Seek Left(cLocalLinea + Space(80), 80)
    EndIf

    Do Case
       Case Eof()
          Append Blank
          Replace TXT_I1 With cLocalLinea
       Case MemLines(TXT_I2) >= _idioma - 1
          f3t_lx = MLine(TXT_I2, _idioma - 1)
          cLocalLinea = Iif(!Empty(f3t_lx),f3t_lx,cLocalLinea)
    EndCase
  EndIf

  f3t_nd1 = At('[', cLocalLinea)
  If f3t_nd1 > 0
    cLocalLinea = Stuff(cLocalLinea, f3t_nd1, 1, '[' + f3t_lx1)
  EndIf

  If !Empty(f3t_ali)
    Select Alias (f3t_ali)
  EndIf
EndIf

Return Trim(cLocalLinea)

ENDPROC


EndDefine 
Define Class st3hlpf as form
Height = 105
Width = 525
AutoCenter = .T.
Caption = (f3_t('Establecer filtro'))
FontBold = .T.
FontSize = 10
MaxButton = .F.
MinButton = .F.
WindowType = 1
AlwaysOnTop = .T.
ColorSource = 5
BackColor = Rgb(192,192,192)

ADD OBJECT L_normal1 as l_normal WITH FontItalic = .T., FontSize = 10, FontUnderline = .T., Alignment = 0, Left = 10, Top = 5, TabIndex = 1, ForeColor = Rgb(0,0,128)
ADD OBJECT cnd1 as st_combo WITH DisplayValue = , ControlSource = "_cond1", Height = 19, Left = 27, Style = 2, TabIndex = 2, Top = 27, Width = 37
ADD OBJECT _txt1 as st_get WITH ControlSource = "_txt1", Left = 72, TabIndex = 3, Top = 27, Width = 168
ADD OBJECT cnd2 as st_combo WITH DisplayValue = , ControlSource = "_cond2", Height = 19, Left = 279, Style = 2, TabIndex = 4, Top = 27, Width = 37
ADD OBJECT cnd3 as st_combo WITH DisplayValue = , ControlSource = "_cond3", Height = 19, Left = 27, Style = 2, TabIndex = 6, Top = 45, Width = 37
ADD OBJECT cnd4 as st_combo WITH DisplayValue = , ControlSource = "_cond4", Height = 19, Left = 279, Style = 2, TabIndex = 8, Top = 45, Width = 37
ADD OBJECT _txt2 as st_get WITH ControlSource = "_txt2", Left = 333, TabIndex = 5, Top = 27, Width = 168
ADD OBJECT _txt3 as st_get WITH ControlSource = "_txt3", Left = 72, TabIndex = 7, Top = 45, Width = 168
ADD OBJECT _txt4 as st_get WITH ControlSource = "_txt4", Left = 333, TabIndex = 9, Top = 45, Width = 168
ADD OBJECT bot_filtro as commandbutton WITH Top = 72, Left = 27, Height = 28, Width = 94, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "Quitar filtros", TabIndex = 10, TabStop = .F., ColorSource = 0
ADD OBJECT bot_ok as commandbutton WITH Top = 72, Left = 432, Height = 29, Width = 29, FontBold = .T., FontSize = 10, Picture = "bmp\ok.bmp", Caption = "", Default = .T., TabIndex = 11, ToolTipText = "Aceptar", ColorSource = 0
ADD OBJECT bot_esc as commandbutton WITH Top = 72, Left = 468, Height = 29, Width = 29, FontBold = .T., FontSize = 10, Picture = "bmp\close.bmp", Caption = "", TabIndex = 12, ToolTipText = "Salir", ColorSource = 0
ADD OBJECT L_normal2 as l_normal WITH Caption = "Y", Left = 259, Top = 30
ADD OBJECT L_normal3 as l_normal WITH Caption = "Y", Height = 17, Left = 258, Top = 48, Width = 9
ADD OBJECT L_normal4 as l_normal WITH Caption = "O", Height = 17, Left = 9, Top = 48, Width = 11


PROCEDURE Init

=DoDefault()

*> Inicializar los valores para realizar los filtros.
With This
  ._cnd1[1] = '  '
  ._cnd1[2] = '>='
  ._cnd1[3] = '<='
  ._cnd1[4] = '> '
  ._cnd1[5] = '< '
  ._cnd1[6] = '= '
  ._cnd1[7] = '<>'

   *> Asignar origen de datos a los listbox.
   .cnd1.RowSource = '._cnd1'
   .cnd1.RowSourceType = 5
   .cnd2.RowSource = '._cnd1'
   .cnd2.RowSourceType = 5
   .cnd3.RowSource = '._cnd1'
   .cnd3.RowSourceType = 5
   .cnd4.RowSource = '._cnd1'
   .cnd4.RowSourceType = 5
EndWith

ENDPROC
PROCEDURE Activate
ThisForm.l_normal1.Caption = Trim(_txtvar)
_ok = .F.

ENDPROC


PROCEDURE cnd1.Valid
if empty(_cond1)
  store space(4)  to _cond2,_cond3,_cond4
  store space(20) to _txt1,_txt2,_txt3,_txt4
  thisform.refresh
endif

ENDPROC


PROCEDURE cnd2.Valid
if empty(_cond2)
  store space(20) to _txt2
  thisform.refresh
endif

ENDPROC


PROCEDURE cnd3.Valid
if empty(_cond3)
  store space(4)  to _cond4
  store space(20) to _txt3,_txt4
  thisform.refresh
endif

ENDPROC


PROCEDURE cnd4.Valid
if empty(_cond4)
  store space(20) to _txt4
  thisform.refresh
endif

ENDPROC


PROCEDURE bot_filtro.Click
Store '' To _cond1,_txt1,_cond2,_txt2,_cond3,_txt3,_cond4,_txt4
For nd=1 To _xbnum
  Store '' to _xbusca(nd,5),_xbusca(nd,6),_xbusca(nd,7),_xbusca(nd,8)
EndFor

_ok=.T.
thisform.release
ENDPROC


PROCEDURE bot_ok.Click
_ok=.T.
thisform.release
ENDPROC


PROCEDURE bot_esc.Click
_ok=.F.
thisform.release
ENDPROC


EndDefine 
Define Class foxado as custom



PROCEDURE creaters
*>
*> Crear la estructura de un RecordSet a partir de una tabla ó cursor.
*>
*>   Recibe: cCur ---> Tabla/cursor a partir de la cual se genera el RS.
*>                     Por defecto lo crea a partir de la tabla activa.
*>
*> Devuelve: rsRSet -----> RecordSet, si OK.
*>                         '', si error.

#Include "adovfp.h"
#Include "dbf2rs.h"

Parameters cCur
Private rsRSet
Local nCampos, aCampos
Local nInx, cOldSele

*> Guardar entorno de trabajo anterior.
cOldSele = Select()

*> Si se recibe la tabla como parámetro, hacerla activa.
If Type('cCur')=='C'
   Select (cCur)
EndIf

*> Declarar el RecordSet.
rsRSet = NewObject("ADODB.Recordset")

*> Parámetros generales del RecordSet.
*> Constantes definidas en "adovfp.h" y "dbf2rs.h"
rsRSet.cursorLocation = ADUSECLIENT
rsRSet.cursorType = ADOPENSTATIC
rsRSet.lockType = ADLOCKOPTIMISTIC

*> Crear un array con las campos de la tabla con la que se crea el RecordSet.
Declare aCampos(1)
nCampos = AFields(aCampos)

For nInx = 1 To nCampos
   Do Case
      *> Campos char.
      Case aCampos(nInx, 2)=='C'
         rsRSet.Fields.Append(aCampos(nInx - 1, 1), ADCHAR, aCampos(nInx, 3))

      *> Campos numéricos.
      Case aCampos(nInx, 2)=='N' .Or. ;
           aCampos(nInx, 2)=='I' .Or. ;
           aCampos(nInx, 2)=='B'
         rsRSet.Fields.Append(aCampos(nInx - 1, 1), ADDOUBLE, aCampos(nInx, 3))

      *> Campos fecha: Se tratan como Char.
      Case aCampos(nInx, 2)=='D' .Or. ;
           aCampos(nInx, 2)=='T'
         rsRSet.Fields.Append(aCampos(nInx - 1, 1), ADCHAR, aCampos(nInx, 3))

      *> Por defecto, se trata el campo como char.
      Otherwise
         rsRSet.Fields.Append(aCampos(nInx - 1, 1), ADCHAR, aCampos(nInx, 3))
   EndCase
EndFor

*> Recuperar entorno de trabajo anterior.
If !Empty(cOldSele)
   Select (cOldSele)
EndIf

Return rsRSet

ENDPROC
PROCEDURE createdbf
*>
*> Crear la estructura de una tabla ó cursor a partir de un RecordSet.
*>
*>   Recibe: rsRSet ---> RecordSet a transformar.
*>           aCampos --> Array que contendrá la estructura.
*>
*> Devuelve: aCampos --> array con la estructura de la tabla, si OK.
*>                       '', si error.
*>
*> Modificar tratamiento decimales en campos float y double. AVC - 30.06.2003

#Include "adovfp.h"
#Include "dbf2rs.h"

Parameters rsRSet, aCampos
Local nInx, nInx1, cOldSele

*> Guardar entorno de trabajo anterior.
cOldSele = Select()

For nInx = 1 To rsRSet.Fields.Count
   *> Crear un array con las campos de la tabla a crear.
   Dimension aCampos(nInx, 16)

   For nInx1 = 7 To 16
      aCampos(nInx, nInx1) = ""
   EndFor

   *> Nombre del campo.
   aCampos(nInx, 1) = rsRSet.Fields(nInx - 1).Name

   *> Tipo de campo.
   Do Case
      *> Campos char.
      Case rsRSet.Fields(nInx - 1).Type==ADCHAR .Or. ;
           rsRSet.Fields(nInx - 1).Type==ADVARCHAR
         aCampos(nInx, 2) = 'C'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).DefinedSize		&& Longitud.
         aCampos(nInx, 4) = 0

      *> Campos fecha.
      Case rsRSet.Fields(nInx - 1).Type==ADDATE .Or. ;
           rsRSet.Fields(nInx - 1).Type==ADDBDATE .Or. ;
           rsRSet.Fields(nInx - 1).Type==ADDBTIME .Or. ;
           rsRSet.Fields(nInx - 1).Type==ADDBTIMESTAMP
         aCampos(nInx, 2) = 'D'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).DefinedSize		&& Enteros.
         aCampos(nInx, 4) = rsRSet.Fields(nInx - 1).NumericScale	&& Decimales.

      *> Campos decimales.
      Case rsRSet.Fields(nInx - 1).Type==ADDECIMAL .Or. ;
           rsRSet.Fields(nInx - 1).Type==ADNUMERIC
         aCampos(nInx, 2) = 'N'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).Precision		&& Enteros.
         aCampos(nInx, 4) = rsRSet.Fields(nInx - 1).NumericScale	&& Decimales.

      *> Campos enteros.
      Case rsRSet.Fields(nInx - 1).Type==ADINTEGER
         aCampos(nInx, 2) = 'I'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).Precision		&& Enteros.
         aCampos(nInx, 4) = rsRSet.Fields(nInx - 1).NumericScale	&& Decimales.

      *> Campos float.
      Case rsRSet.Fields(nInx - 1).Type==ADSINGLE
         aCampos(nInx, 2) = 'F'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).DefinedSize		&& Enteros.
         aCampos(nInx, 4) = 0										&& Decimales.
         *aCampos(nInx, 4) = rsRSet.Fields(nInx - 1).NumericScale	&& Decimales.

      *> Campos double.
      Case rsRSet.Fields(nInx- 1).Type==ADDOUBLE
         aCampos(nInx, 2) = 'B'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).DefinedSize		&& Enteros.
         aCampos(nInx, 4) = 0										&& Decimales.
         *aCampos(nInx, 4) = rsRSet.Fields(nInx - 1).NumericScale	&& Decimales.

      *> Por defecto, se trata el campo como char.
      Otherwise
         aCampos(nInx, 2) = 'C'										&& Tipo.
         aCampos(nInx, 3) = rsRSet.Fields(nInx - 1).DefinedSize		&& Longitud.
         aCampos(nInx, 4) = 0
   EndCase

   *> Campos NULL.
   If BitAnd(rsRSet.Fields(nInx - 1).Attributes, 0x00000020)==0x00000020
      aCampos(nInx, 5) = .T.
   EndIf
EndFor

*> Recuperar entorno de trabajo anterior.
If !Empty(cOldSele)
   Select (cOldSele)
EndIf

Return

ENDPROC
PROCEDURE addallrecordstors
*>
*> Añadir todos los registros de la tabla activa a un RecordSet.
*>
*>   Recibe: rsRSet --> RecordSet.
*>           cCur ----> Tabla a partir de la cual se genera el RS.
*>                     Por defecto los hace de la tabla activa.
*>
*> Devuelve: rsRSet -----> RecordSet con los datos de la tabla, si OK.
*>                         '', si error.

#Include "adovfp.h"
#Include "dbf2rs.h"

Parameters rsRSet, cCur
Local cAlias, nInx, cCampo

rsRSet.Open

*> Si se recibe la tabla como parámetro, hacerla activa.
If Type('cCur')=='C'
   Select (cCur)
EndIf

cAlias = Alias()

Go Top
Do While !Eof()
   *> Añade registro al RS (Equivalente al Append Blank de FOX).
   rsRSet.AddNew

   *> Añadir el valor de cada una de las columnas de la tabla al RecordSet.
   For nInx = 0 To rsRset.Fields.Count - 1
      cCampo = cAlias + '.' + rsRSet.Fields.Item(nInx).Name
      rsRSet.Fields(nInx) = &cCampo
   EndFor

   *>
   Select (cAlias)
   Skip
EndDo

*>
*> El rsRSet NO se debe de cerrar.
Return rsRSet

ENDPROC
PROCEDURE addrecordfromrs
*>
*> Agregar un registro de un RS a un cursor/tabla.
*>
*>   Recibe: rsRSet ----> RecordSet.
*>           cCursor ---> Tabla / cursor a actualizar.
*>           lNoSort ---> (Opcional), estructura idéntica en RS y tabla.
*>
*> Devuelve:

Parameters rsRSet, cCursor, lNoSort
Local cOldSele, nInx, nScan
Local aCampos

*> Guardar el entorno de trabajo anterior.
cOldSele = Select()

Select (cCursor)
Declare aCampos(1, 16)
=AFields(aCampos)

*> Añadir los campos de RS que correspondan al de la tabla.
Append Blank

For nInx = 0 To rsRset.Fields.Count - 1
   Do Case
      *> Estructura idéntica en RS y tabla.
      Case !lNoSort
         If aCampos(nInx + 1, 1)==Upper(rsRset.Fields(nInx).Name)
            Replace &aCampos(nInx + 1, 1) With rsRset.Fields(nInx).Value
         EndIf

      *> Estructura no clasificada en tabla respecto a RS.
      Case lNoSort
         nScan = AScan(aCampos, Upper(rsRset.Fields(nInx).Name))
         If nScan > 0
            Replace &aCampos(nScan, 1) With rsRset.Fields(nInx).Value
         EndIf
   EndCase
EndFor

*> Restaurar el entorno de trabajo anterior.
If !Empty(cOldSele)
   Select (cOldSele)
EndIf

ENDPROC


EndDefine 
Define Class st3hlp as form
Height = 281
Width = 584
AutoCenter = .T.
BorderStyle = 3
Caption = "Buscar"
Closable = .F.
FontBold = .F.
FontName = "Courier"
FontSize = 10
MaxButton = .T.
Visible = .F.
TabStop = .F.
LockScreen = .F.
AlwaysOnTop = .T.
ColorSource = 5
BackColor = Rgb(255,255,128)

ADD OBJECT Label4 as label WITH AutoSize = .T., FontBold = .T., FontSize = 8, BackStyle = 0, Caption = "", Height = 16, Left = 12, Top = 287, Width = 2, TabIndex = 8, ColorSource = 0
ADD OBJECT Grid1 as grid WITH ColumnCount = 20, FontBold = .F., FontSize = 8, DeleteMark = .F., GridLines = 2, Height = 225, Left = 9, Panel = 1, ReadOnly = .T., RecordMark = .F., RowHeight = 14, TabIndex = 1, Top = 5, Visible = .T., Width = 560, Column1.FontBold = .F., Column1.FontSize = 8, Column1.ControlSource = "", Column1.Width = 200, Column1.ReadOnly = .T., Column1.Visible = .T., Column2.FontBold = .F., Column2.FontSize = 8, Column2.ControlSource = "", Column2.Width = 200, Column2.ReadOnly = .T., Column2.Visible = .T., Column3.FontBold = .F., Column3.FontSize = 8, Column3.ControlSource = "", Column3.Width = 200, Column3.ReadOnly = .T., Column3.Visible = .T., Column4.FontBold = .F., Column4.FontSize = 8, Column4.ControlSource = "", Column4.Width = 200, Column4.ReadOnly = .T., Column4.Visible = .T., Column5.FontBold = .F., Column5.FontSize = 8, Column5.ControlSource = "", Column5.Width = 200, Column5.ReadOnly = .T., Column5.Visible = .T., Column6.FontBold = .F., Column6.FontSize = 8, Column6.ControlSource = "", Column6.Width = 200, Column6.ReadOnly = .T., Column6.Visible = .T., Column7.FontBold = .F., Column7.FontSize = 8, Column7.ControlSource = "", Column7.Width = 200, Column7.ReadOnly = .T., Column7.Visible = .T., Column8.FontBold = .F., Column8.FontSize = 8, Column8.ControlSource = "", Column8.Width = 200, Column8.ReadOnly = .T., Column8.Visible = .T., Column9.FontBold = .F., Column9.FontSize = 8, Column9.ControlSource = "", Column9.Width = 200, Column9.ReadOnly = .T., Column9.Visible = .T., Column10.FontBold = .F., Column10.FontSize = 8, Column10.ControlSource = "", Column10.Width = 200, Column10.ReadOnly = .T., Column10.Visible = .T., Column11.FontBold = .F., Column11.FontSize = 8, Column11.ControlSource = "", Column11.Width = 200, Column11.ReadOnly = .T., Column11.Visible = .T., Column12.FontBold = .F., Column12.FontSize = 8, Column12.ControlSource = "", Column12.Width = 200, Column12.ReadOnly = .T., Column12.Visible = .T., Column13.FontBold = .F., Column13.FontSize = 8, Column13.ControlSource = "", Column13.Width = 200, Column13.ReadOnly = .T., Column13.Visible = .T., Column14.FontBold = .F., Column14.FontSize = 8, Column14.ControlSource = "", Column14.Width = 200, Column14.ReadOnly = .T., Column14.Visible = .T., Column15.FontBold = .F., Column15.FontSize = 8, Column15.ControlSource = "", Column15.Width = 200, Column15.ReadOnly = .T., Column15.Visible = .T., Column16.FontBold = .F., Column16.FontSize = 8, Column16.ControlSource = "", Column16.Width = 200, Column16.ReadOnly = .T., Column16.Visible = .T., Column17.FontBold = .F., Column17.FontSize = 8, Column17.ControlSource = "", Column17.Width = 200, Column17.ReadOnly = .T., Column17.Visible = .T., Column18.FontBold = .F., Column18.FontSize = 8, Column18.ControlSource = "", Column18.Width = 200, Column18.ReadOnly = .T., Column18.Visible = .T., Column19.FontBold = .F., Column19.FontSize = 8, Column19.ControlSource = "", Column19.Width = 200, Column19.ReadOnly = .T., Column19.Visible = .T., Column20.FontBold = .F., Column20.FontSize = 8, Column20.ControlSource = "", Column20.Width = 200, Column20.ReadOnly = .T., Column20.Visible = .T.
ADD OBJECT "Grid1.Column1.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column1.Text1" as textbox WITH FontBold = .F., FontSize = 8, BackStyle = 0, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column2.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column2.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column3.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column3.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column4.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column4.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column5.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column5.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column6.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column6.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column7.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column7.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column8.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column8.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column9.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column9.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column10.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column10.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column11.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column11.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column12.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column12.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column13.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column13.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column14.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column14.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column15.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column15.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column16.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column16.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column17.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column17.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column18.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column18.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column19.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column19.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column20.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column20.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT L_normal1 as l_normal WITH FontSize = 7, Caption = "Click sobre cabecera de una columna, ordena la ayuda por dicha columna", Left = 9, Top = 246, TabIndex = 6, ForeColor = Rgb(0,0,255)
ADD OBJECT L_normal2 as l_normal WITH FontSize = 7, Caption = "Click derecho sobre cabecera de una columna, establece filtro sobre dicha columna", Left = 9, Top = 256, TabIndex = 7, ForeColor = Rgb(0,0,255)
ADD OBJECT bot_ok as st_bot WITH AutoSize = .F., Top = 234, Left = 510, Height = 29, Width = 29, Picture = "bmp\ok.bmp", Caption = "", TabIndex = 3, ToolTipText = "Aceptar [CTRL+ENTER]"
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 234, Left = 540, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 4, ToolTipText = "Salir [ESC]"
ADD OBJECT Bot_prn as st_bot WITH AutoSize = .F., Top = 234, Left = 451, Height = 29, Width = 29, Picture = "bmp\printp.bmp", Caption = "", TabIndex = 2, ToolTipText = "Imprimir"
ADD OBJECT L_normal3 as l_normal WITH FontSize = 7, Caption = "Doble Click sobre una columna, ordena la ayuda por dicha columna en orden inverso", Left = 9, Top = 235, TabIndex = 5, ForeColor = Rgb(0,0,255)


PROCEDURE ordenar
*>
*> Clasificar por una columna, ascendente.
Parameter _n_campo
Local lx1, lx2, lx3, r1

lx1 = Sys(2015)
lx2 = _tmp + lx1

Select (This.Source)
r1 = Iif(Eof(), 0, RecNo())

lx3 = Field(_n_campo)
Sort To (lx2) On &lx3 /A
Zap
Append From (lx2)

lx2 = lx2 + '.DBF'
Delete File (lx2)

If r1 > 0
   Go r1
   ThisForm.Grid1.ActivateCell(r1, _n_campo)
Else
   Go Top
EndIf

*> Memorizar la columna y tipo de clasificación activa.
With ThisForm
   .nColumnOrder = _n_campo
   .cTypeOrder = 'A'
   .Refresh
EndWith

ENDPROC
PROCEDURE filtro
*>
*> Filtrar ayudas.
*> Recibe: Columna sobre la que se realiza el filtro.

Parameters _col

Private _xbusca, nFields
Private _cond1, _cond2, _cond3, _cond4
Private _txt1, _txt2, _txt3, _txt4

Local _hlp_filtro, _hlp_campo, _hlp_tipo
Local lx1, _num, lxx1, lxx2, lxx3, lxx4
Local _xerrac, _xier, _ok

Store Space(2) To _cond1, _cond2, _cond3, _cond4
Store Space(20) To _txt1, _txt2, _txt3, _txt4


Select (This.Source)
nFields = FCount()
Dimension _xbusca(nFields, 8)
=ACopy(This._xbusca, _xbusca)

*> Recuperar las condiciones de filtro anteriores.
If !Empty(_xbusca(_col, 5))
  lx1 = _xbusca(_col, 5)
  _cond1 = Left(lx1, 2)
  _txt1 = SubStr(lx1, 3)

  If !Empty(_xbusca(_col, 6))
    lx1 = _xbusca(_col, 6)
    _cond2 = Left(lx1, 2)
    _txt2 = SubStr(lx1, 3)
  EndIf

  If !Empty(_xbusca(_col, 7))
    lx1 = _xbusca(_col, 7)
    _cond3 = Left(lx1, 2)
    _txt3 = SubStr(lx1, 3)

    If !Empty(_xbusca(_col, 8))
      lx1 = _xbusca(_col, 8)
      _cond4 = Left(lx1, 2)
      _txt4 = SubStr(lx1, 3)
    EndIf
  EndIf
EndIf

_ok = .T.
_txtvar = _xbusca(_col, 3)

_ok = This.oHlpF.Show()

If _ok = .T.
  _xbusca(_col, 5) = _cond1 + _txt1
  _xbusca(_col, 6) = _cond2 + _txt2
  _xbusca(_col, 7) = _cond3 + _txt3
  _xbusca(_col, 8) = _cond4 + _txt4

  _hlp_filtro = ''

  *> Cambiar el color de cabecera de las columnas con filtro.
  For _num = 1 To nFields
    lx1 = 'thisform.grid1.Column' + LTrim(Str(_num)) + '.Header1.Backcolor'
    If Empty(_xbusca(_num, 5))
      &lx1 = Rgb(192, 192, 192)
    Else
      _hlp_campo = Trim(_xbusca(_num, 1))
      _hlp_tipo = _xbusca(_num, 4)

      Store '' To lxx1, lxx2, lxx3, lxx4
      &lx1 = RGB(0, 255, 255)
      lxx1 = _hlp_campo + Trim(Left(_xbusca(_num, 5),2 ))
      lx1 = Trim(SubStr(_xbusca(_num, 5), 3))

      do Case
      Case _hlp_tipo = 'N'
        lxx1 = lxx1 + lx1
      Case _hlp_tipo = 'D'
        lxx1 = lxx1 + '{'+lx1+'}'
      otherwise
        lxx1 = lxx1 + '"'+lx1+'"'
      EndCase
      If !Empty(_xbusca(_num, 6))
        lxx2 = '.and.' + _hlp_campo + Trim(Left(_xbusca(_num, 6), 2))
        lx1 = Trim(SubStr(_xbusca(_num, 6), 3))

        Do Case
	        Case _hlp_tipo = 'N'
	           lxx2 = lxx2 + lx1
	        Case _hlp_tipo = 'D'
	           lxx2 = lxx2 + '{'+lx1+'}'
	        Otherwise
	           lxx2 = lxx2 + '"'+lx1+'"'
	        EndCase
      EndIf

      If !Empty(_xbusca(_num, 7))
        lxx3 = _hlp_campo+Trim(Left(_xbusca(_num, 7), 2))
        lx1 = Trim(SubStr(_xbusca(_num, 7), 3))

        Do Case
	        Case _hlp_tipo = 'N'
	           lxx3 = lxx3 + lx1
	        Case _hlp_tipo = 'D'
	           lxx3 = lxx3 + '{'+lx1+'}'
	        Otherwise
	           lxx3 = lxx3 + '"'+lx1+'"'
        EndCase

        If !Empty(_xbusca(_num, 8))
          lxx4 = '.and.' + _hlp_campo + Trim(Left(_xbusca(_num, 8), 2))
          lx1 = Trim(SubStr(_xbusca(_num, 8), 3))

          Do Case
	          Case _hlp_tipo = 'N'
	             lxx4 = lxx4 + lx1
	          Case _hlp_tipo = 'D'
	             lxx4 = lxx4 + '{'+lx1+'}'
	          Otherwise
	             lxx4 = lxx4 + '"'+lx1+'"'
          EndCase
        EndIf
      EndIf

      ndh = at('**',lxx1)
      If ndh>1
        lxx1 = 'atc('+SubStr(lxx1,ndh+2)+','+Left(lxx1,ndh-1)+')>0'
      EndIf

      ndh = at('**',lxx2)
      If ndh>1
        lxx2 = 'atc('+SubStr(lxx2,ndh+2)+','+Left(lxx2,ndh-1)+')>0'
      EndIf

      If Empty(lxx3)
        _hlp_filtro = _hlp_filtro+'.and.'+lxx1+lxx2
      Else
        ndh = at('**',lxx3)
        If ndh>1
          lxx3 = 'atc('+SubStr(lxx3,ndh+2)+','+Left(lxx3,ndh-1)+')>0'
        EndIf
        ndh = at('**',lxx4)
        If ndh>1
          lxx4 = 'atc('+SubStr(lxx4,ndh+2)+','+Left(lxx4,ndh-1)+')>0'
        EndIf
        _hlp_filtro = _hlp_filtro+'.and.(('+lxx1+lxx2+').or.('+lxx3+lxx4+'))'
      EndIf
    EndIf
  EndFor

  Select (This.Source)
  If Empty(_hlp_filtro)
    Set Filter To
  Else
    _hlp_filtro = SubStr(_hlp_filtro, 6)
    Store 0 to _xerrac, _xier
    Select (This.Source)
    Set Filter To &_hlp_filtro
    If _xier<>0
       = f3_sn(1, 1,'No se ha podido establecer el filtro','Posiblemente las condiciones estén mal definidas')
    EndIf

    _xier = 0
    _xerrac = 1
  EndIf
EndIf

ENDPROC
PROCEDURE ordenardesc
*>
*> Clasificar por una columna, descendente.
Parameter _n_campo
Local lx1, lx2, lx3, r1

lx1 = Sys(2015)
lx2 = _tmp + lx1

Select (This.Source)
r1 = Iif(Eof(), 0, RecNo())

lx3 = Field(_n_campo)
Sort To (lx2) On &lx3 /D
Zap
Append From (lx2)

If r1 > 0
   Go r1
   ThisForm.Grid1.ActivateCell(r1, _n_campo)
Else
   Go Top
EndIf

lx2 = lx2 + '.DBF'
Delete File (lx2)

*> Memorizar la columna y tipo de clasificación activa.
With ThisForm
   .nColumnOrder = _n_campo
   .cTypeOrder = 'D'
   .Refresh
EndWith

ENDPROC
PROCEDURE setparams

*> Actualizar parámetros de configuración de la clase.

*> Parámetros que recibe:

*> cParams ----> Lista de parámetros opcionales, con el formato:
*>              [SOURCE=AYUDA,                                   SRC
*>               SOURCETYPE=1,                                   SRCT
*>               GRIDRECORS=50,                                  GREC
*>               FETCHRECORDS=100,                               FREC
*>               CSQL=SELECT * FROM F01C,
*>               FILE=F01C,
*>               LANGUAGE=1,                                     LANG
*>               ODBCHANDLE=_ASql,                               ODBCH
*>               EMPRESA=_em,
*>               CURSORLOCATION=0,                               CLOC
*>               CURSOROPENTYPE=0,                               COPN
*>               CURSORLOCKTYPE=0,                               CLCK
*>               ADORRECORDSET=oAdo,                             ADORS
*>               ADOCONNECTION=oConn]                            ADOCONN

LParameters cParams

Local nd, cPrm, cVal, cUpd, cTmp

If Type('cParams') # 'C'
   Return
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      Case cPrm=='SOURCE' .Or. cPrm=='SRC'
         This.Source = cVal

      Case cPrm=='SOURCETYPE' .Or. cPrm=='SRCT'
         This.SourceType = cVal

      Case cPrm=='GRIDRECORDS' .Or. cPrm=='GREC'
         This.GridRecords = Val(cVal)

      Case cPrm=='FETCHRECORDS' .Or. cPrm=='FREC'
         This.FetchRecords = Val(cVal)

      Case cPrm=='CSQL'
         This.cSQL = cVal

      Case cPrm=='FILE'
         This.File = cVal

      Case cPrm=='LANGUAGE' .Or. cPrm=='LANG'
         This.nIdioma = Val(cVal)

      Case cPrm=='ODBCHANDLE' .Or. cPrm=='ODBCH'
         This.nASql = Val(cVal)

      Case cPrm=='EMPRESA'
         This.cEmpresa = cVal

      Case cPrm=='ADORECORDSET' .Or. cPrm=='ADORS'
         This.AdoRs = cVal

      Case cPrm=='ADOCONNECTION' .Or. cPrm=='ADOCONN'
         This.AdoConn = cVal

      Case cPrm=='CURSORLOCATION' .Or. cPrm=='CLOC'
         This.CursorLocation = Val(cVal)

      Case cPrm=='CURSOROPENTYPE' .Or. cPrm=='COPN'
         This.CursorOpenType = Val(cVal)

      Case cPrm=='CURSORLOCKTYPE' .Or. cPrm=='CLCK'
         This.CursorLockType = Val(cVal)

      *> No es un parámetro reconocido.
      Otherwise
   EndCase

EndDo

ENDPROC
PROCEDURE generarsql
*>
*> Generar la sentencia SQL a ejecutar.
*>
*> varios: _nvarh
*>         _hlpok
*>         _mensaje_hlp

Local _controlc, _fic
Local _lxsele, _lxfrom, _lxwhere, _lxord, _lxgrup, _replaces, _hlpriv
Local _xfxa, nd, lx, lx1, num

*> Ya se ha generado la sentencia SQL.
If !Empty(This.cSQL)
   Return
EndIf

Store .F. To _controlc
Store '' To _lxsele, _lxfrom, _lxwhere, _lxord, _lxgrup, _replaces, _hlpriv

*> El fichero debe estar abierto.
This.lFileIsOpen = Used(This.File)
If !Used(This.File)
  =This.oPROCAOT.OpenTabla(This.File)
EndIf

*> Buscar si hay una ayuda personalizada para el fichero.
Select SYSHL
_fic = Upper(AllTrim(This.File))
_fic = Left(_fic + Space(10), 10)
Seek This.cWOnTop + _fic
If Eof()
  Seek Space(10) + _fic
EndIf

Do Case
Case !Eof() .and. AtC('/' + Trim(FC_FC) + ']', _xopenfc) = 0
  _controlc=.F.
  _mensaje_hlp=.T.
  Return

*> Hay ayuda definida: montar cláusula sql.
Case !Eof()
  _xfxa = Trim(FC_FC)
  For nd = 1 To MemLines(SYSHL.HL_EXPR)
    lx = MLine(syshl.HL_EXPR,nd)
    lx=ChrTran(lx, "'", '"')

    Do Case
    Case empty(lx)
    Case left(lx,1) = '['
      lx1 = lx
    Case lx1 = '[Selecciona]'
      _lxsele = _lxsele + lx
    Case lx1 = '[Fichero]'
      _lxfrom = _lxfrom + lx
    Case lx1 = '[Cuando]'
      _lxwhere = _lxwhere + lx
    Case lx1 = '[Agrupado]'
      _lxgrup = _lxgrup + lx
    Case lx1 = '[Ordenado]'
      _lxord = _lxord + lx
    Case lx1 = '[Reemplazando]'
      _replaces = _replaces + lx
    Case lx1 = '[Privadas]'
      _hlpriv = _hlpriv + lx
    EndCase
  EndFor

*> Definición directa de la ayuda.
Otherwise
  _xfxa = AllTrim(_fic)
  _lxfrom = AllTrim(_fic)
  Select (_xfxa)
  num = FCount()
  For nd= 1  To num
    If Type(Field(nd))=='N' .Or. Type(Field(nd))=='C' .Or. Type(Field(nd))=='D'
      _lxsele = _lxsele + ',' + Field(nd)
    EndIf
  EndFor

  _lxsele = SubStr(_lxsele, 2)
  _lxsele = '*'
EndCase

If !Empty(_hlpriv)
  _hlpriv = _hlpriv + ',  '
  nd=At(',', _hlpriv)
  Do While nd > 0
    lx = Left(_hlpriv, nd - 1)
    _hlpriv=SubStr(_hlpriv, nd + 1)
    nd = At('*', lx)
    If nd = 0
      Private All Like &lx
    Else
      Private All Like &lx
    EndIf

    nd = At(',', _hlpriv)
  EndDo
EndIf

Select Alias(_xfxa)

_lxfrom = AllTrim(_lxfrom)
if Len(_lxfrom) = 4
  _lxfrom = _lxfrom + This.cEmpresa
EndIf

*> Construcción de la sentencia SQL.
_lxsql = 'select &_lxsele From &_lxfrom '
If !Empty(_lxwhere)
  _lxsql = _lxsql + 'Where ' + _lxwhere + Space(1)
EndIf

If Empty(_lxord)
   _lxord = '1'
EndIf
_lxsql = _lxsql + 'Order By &_lxord '
If !Empty(_lxgrup)
  _lxsql = _lxsql + 'Group by &_lxgrup '
EndIf

This.cSQL = _lxsql

ENDPROC
PROCEDURE dohelp
*>
*> Proceso de consulta de fichero propiamente dicho.
*> Recibe, opcionalmente, una lista de parámetros de inicialización.

Parameters cParams

If PCount()==1
   =This.SetParams(cParams)
EndIf

With This
   .Initialize()			&& Inicializar el proceso de ayudas.
   .GenerarSQL()			&& Preparar sentencia SQL a realizar.
   .InitADO()				&& Preparar conexión ADO con la DB.
   .OpenRS()				&& Crear RS al fichero.
   .OpenAyuda()				&& Crear cursor ayudas.
   .InitForm()				&& Inicializar características del form.
   .InitFiltros()			&& Inicializar filtros por columnas.
   .CargarRegistros()		&& Primera carga de registros.
   .Show(1)					&& Muestra ayuda.
EndWith

ENDPROC
PROCEDURE cargarregistros
*> cDireccion  ----> S (Siguiente); A (Anterior); I (Inicio); F (Fin)

Parameter cDireccion
*> Añadir registros del RecordSet al cursor DBF de ayudas.
Local nInx, nRecNo

*If This.EOF
*   *> Ya no hay más registros.
*   Return
*EndIf

If Type('cDireccion') # 'C'
	cDireccion='S'
EndIf

Select (This.Source)
nRecNo = Iif(Eof(), 0, RecNo())
nInx = 0

If This.AdoRs.Eof() .Or. This.AdoRs.Bof()
   This.EOF = .T.
   Return
EndIf

*> Vaciado del DBF
Select (This.Source)
Zap

do case
	*> Siguiente
	case cDireccion == 'S'
	*> Anterior
	case cDireccion == 'A'
		If (this.adors.Bookmark-(This.FetchRecords)*2)>0
			this.adors.Bookmark=this.adors.Bookmark-((This.FetchRecords)*2)
		Else
			This.AdoRs.movefirst
		EndIf	
	*> Inicio
	case cDireccion == 'I'
		This.AdoRs.movefirst
	*> Fin
	case cDireccion == 'F'
		This.AdoRs.moveLast
endcase	


Do While nInx < This.FetchRecords
   *> Pasa el registro actual del RS al DBF.
   =This.oFOXADO.AddRecordFromRS(This.AdoRs, This.Source, .F.)

   nInx = nInx + 1
   This.GridLastRecord = This.GridLastRecord + 1
   This.AdoRs.MoveNext

   If This.AdoRs.Eof()
      This.EOF = .T.
      Exit
   EndIf
EndDo

*> Reclasificar el GRID, si cal.
Select (This.Source)
If Type('This.nColumnOrder')=='N'
   If This.cTypeOrder=='A'
      =This.Ordenar(This.nColumnOrder)
   Else
      =This.OrdenarDesc(This.nColumnOrder)
   EndIf
Else
   If nRecNo > 0
      Go nRecNo
   Else
      Go Top
   EndIf
EndIf

With This
   .Grid1.RecordSource = .Source
   .Grid1.Refresh
EndWith

ENDPROC
PROCEDURE initado
*>
*> Conectar a la DB mediante ADO a partir de ODBC.

Local cDSN

*> Conexión ADO ya creada.
If Type('This.ADoConn')=='O'
   Return
EndIf

cDSN = SqlGetProp(This.nASql, 'ConnectString')		&& DSN de ODBC.

With This
   .AdoConn = CreateObject('ADODB.Connection')		&& Objeto conexión.
   .AdoRs   = CreateObject('ADODB.RecordSet')		&& Objeto RecordSet.

   *.AdoConn.Open(cDSN)  && Alternativa (N. de P.).
   .AdoConn.ConnectionString = cDSN
   .AdoConn.Open()
EndWith

ENDPROC
PROCEDURE initialize
*>
Local _prghlpx

With This
   .EOF = .F.								&& Control Eof del RS.
   .BOF = .F.								&& Control Bof del RS.
   .lSYSFCIsOpen = Used('SYSFC')        	&& Control open SYSFC.
   .lSYSHLIsOpen = Used('SYSHL')        	&& Control open SYSHL.
   .lSYSVARIsOpen = Used('SYSVAR')			&& Control open SYSVAR.
EndWith

*> Archivo general de definición de ayudas.
If !This.lSYSHLIsOpen
  Use SYSHL Order Tag SYSHL In 0
EndIf
Select SYSHL

*> Inicializar propiedades del grid.
This.Grid1.RecordSource = This.Source
This.Grid1.RecordSourceType = This.SourceType

This.cProgram = 'V' + This.File

*> Código a ejecutar ANTES de cargar la ayuda.
If File(This.cProgram + '.PRG') .Or. File(This.cProgram + '.FXP')
  _prghlpx = "=" + This.cProgram + "('" + This.cWOnTop + "', 'INICIO')"
  &_prghlpx
EndIf

ENDPROC
PROCEDURE nasql_access
*>
*> Valor del handle ODBC a la DB.

Local nASqlLocal

If This.nASql <= 0
   =Sys(3053)
   This.nASql = SqlConnect()
EndIf

Return This.nASql

ENDPROC
PROCEDURE cempresa_access
*>
*> Valor de la empresa activa.

This.cEmpresa = Iif(Empty(This.cEmpresa), '001', This.cEmpresa)
Return This.cEmpresa

ENDPROC
PROCEDURE oprocaot_access
*>
*> Crear objeto de la clase PROCAOT.

If Type('This.oPROCAOT') # 'O'
   This.oPROCAOT = CreateObject('procaot')
EndIf

Return This.oPROCAOT

ENDPROC
PROCEDURE openrs
*>
*> Crear el RS de consulta del fichero.

If This.AdoRs.State > 0
   This.AdoRs.Close
EndIf

=This.AdoRs.Open(This.cSQL, This.AdoConn.ConnectionString, This.CursorOpenType, This.CursorLockType)

ENDPROC
PROCEDURE openayuda
*>
*> Crear estructura del cursor de ayudas (DBF), a partir del RecordSet.
Private aCampos

If Used(This.Source)
   Use In This.Source
EndIf

*> Array que contendrá la definición de la tabla de consulta.
Declare aCampos(1, 16)

*> Obtener la descripción a partir del RecordSet.
=This.oFOXADO.CreateDBF(This.AdoRs, @aCampos)

Create Cursor(This.Source) From Array aCampos

Return

ENDPROC
PROCEDURE gridkeypress
*>
*> Procesa las pulsaciones de teclado en el grid (flecha abajo, Pantalla abajo, ...).

LParameters nKeyCode, nShiftAltCtrl, nColumnOrder
Local lx1, _hlp_car, r1

Select (This.Source)
r1 = Iif(Eof(), 0, RecNo())

Do Case
*> CTRL j
Case nKeyCode==10
  _hlpok = .T.
  ThisForm.Release

*> ESCape
Case nKeyCode==27
  ThisForm.Release

*> Posicionarse.
Case Between(nKeyCode, 32, 255)
  lx1 = Field(nColumnOrder)
  If nKeyCode==127
    _hlp_car = Iif(Len(_hlp_car) > 1, Left(_hlp_car, Len(_hlp_car) - 1), '')
  Else
    _hlp_car = _hlp_car + Chr(nKeyCode)
  EndIf

  Do Case
  Case Type(lx1)=='N'
    Locate For &lx1 >= Val(_hlp_car)
  Case Type(lx1)=='C'
    Locate For &lx1=_hlp_car
  Case Type(lx1)=='D'
    Locate For Left(DToC(&lx1, Len(_hlp_car)) = _hlp_car
  EndCase

  If !Found() .And. r1 > 0
     Go r1
  EndIf

  *> Flecha abajo o Página abajo.
  Case nKeyCode==3
     =ThisForm.CargarRegistros('S')
     Go Top

  *> Flecha Arriba o Página Arriba.
  Case nKeyCode==18
     =ThisForm.CargarRegistros('A')
     Go Top

  *> Tecla Inicio.
  Case nKeyCode==1
     =ThisForm.CargarRegistros('I')
     Go Top

  *> Tecla Fin.
  Case nKeyCode==6
     =ThisForm.CargarRegistros('F')
     Go Top
EndCase

ENDPROC
PROCEDURE ofoxado_access
*>
*> Crear objeto de la clase FOXADO.

If Type('This.oFOXADO') # 'O'
   This.oFOXADO = CreateObject('foxado')
EndIf

Return This.oFOXADO

ENDPROC
PROCEDURE initform
*>
*> Inicializar características del form de ayudas.
Local nd, nd2, num, lx, lx1, lx2, _xbnum, nvar
Local _xbusca, _cnd1

On Key

if at(_cse,_usrmaxp)>0
  on key label 'SHIFT+F2' do =f3_hlpos()
endif

store .F. to _hlpok, _ok, _controlc

if syshl.HL_LIN<>0
  thisform.top=syshl.HL_LIN
endif

if syshl.HL_COL<>0
  thisform.left=syshl.HL_COL
endif

if syshl.HL_ANCHO<>0
  thisform.Width=syshl.HL_ANCHO
endif

if syshl.HL_ALTO<>0
  thisform.Height=syshl.HL_ALTO
endif

if syshl.HL_ALTO<>0 .or. syshl.HL_ANCHO<>0
  thisform.resize
endif

_linea = syshl.HL_TIT
if _xidiom>1
  thisform.caption=f3_t(_linea)
else
  thisform.caption=_linea
endif

*> Asignar origen de datos a cada columna.
Select (This.Source)
_xbnum = FCount()
ThisForm.grid1.ColumnCount = _xbnum

Dimension _xbusca(_xbnum, 8), _cnd1(7)

_cnd1(1) = '  '
_cnd1(2) = '>='
_cnd1(3) = '<='
_cnd1(4) = '> '
_cnd1(5) = '< '
_cnd1(6) = '= '
_cnd1(7) = '<>'

Select (This.Source)
For nd = 1 To _xbnum
   _xbusca(nd, 4) = 'C'
   nvar = Field(nd)
   Select SYSVAR
   Seek nvar
   If !Eof()
      m.VAR_DES=VAR_DES

      Do Case
      Case Left(VAR_FMT,1)=='9'
        lx2 = Trim(VAR_FMT)
        nd2 = Len(lx2) * 6.5 + 7
        _xbusca(nd, 4) = 'N'
      Case Left(VAR_FMT, 1)=='r'
        lx2 = VAR_FMT
        lx2 = &lx2
        nd2 = Len(lx2) * 6.5 + 7
      Case Left(VAR_FMT,1)=='D'
        nd2 = 65
        _xbusca(nd, 4) = 'D'
      EndCase
    Else
      nd2 = 140
      m.VAR_DES = nvar
    EndIf

    Select (This.Source)
    _xbusca(nd, 1) = This.Source + "." + Trim(nvar)
    _xbusca(nd, 2) = nd2
    _xbusca(nd, 3) = VAR_DES
    *if _xidiom>1
    *  _xbusca(nd,3)=f3_t(VAR_DES)
    *endif
    Store '' To _xbusca(nd, 5),_xbusca(nd, 6),_xbusca(nd, 7),_xbusca(nd, 8)
EndFor

For num = 1 To _xbnum
  lx1 = 'ThisForm.grid1.column' + LTrim(Str(num))
  With &lx1
    lx = '.ControlSource = "'+_xbusca(num, 1)+'"'
    &lx
    .Width = _xbusca(num, 2)
    .Header1.Caption = _xbusca(num, 3)
  EndWith
EndFor

*If _ayuda_especial=.T.
*  ThisForm.bot_ok.Visible = .F.
*EndIf

ENDPROC
PROCEDURE ohlpf_access
*>
*> Formulario para realizar filtros sobre la ayuda.

If Type('This.oHlpF') # 'O'
   This.oHlpF = CreateObject('st3hlpf')
EndIf

Return This.oHlpF

ENDPROC
PROCEDURE initfiltros
*>
*> Genera los parámetros para los filtros.
*> Debe llamarse después de crear el cursor de ayudas.

Local nd, nd2, nvar, lx2
Local cVarDes

Select (This.Source)
For nd = 1 To FCount()
   *> Redimensionar el array de parámetros de filtro.
   Dimension This._xbusca[nd, 8]

   *> Establecer el tipo de campo. Por defecto, alfanumérico.
   This._xbusca[nd, 4] = 'C'
   nvar = Field(nd)

   Select SYSVAR
   Seek nvar
   If !Eof()
      cVarDes = VAR_DES					&& Descripción del campo.

      Do Case
         *> Campo numérico.
         Case Left(VAR_FMT, 1)=='9'
           lx2 = Trim(VAR_FMT)
           nd2 = Len(lx2) *6.5 + 7
           This._xbusca[nd, 4] = 'N'

         Case Left(VAR_FMT, 1)=='r'
            lx2 = VAR_FMT
            lx2 = &lx2
            nd2 = Len(lx2) * 6.5 + 7

         *> Campo fecha / hora.
         Case Left(VAR_FMT, 1)=='D' .Or. Left(VAR_FMT, 1)=='T'
            nd2 = 65
            This._xbusca[nd, 4] = 'D'
         EndCase
   Else
      *> No está definido en SYSVAR.
      nd2 = 140
      cVarDes = nvar
   EndIf

   Select (This.Source)
   With This
      ._xbusca[nd, 1] = This.Source + '.' + Trim(nvar)
      ._xbusca[nd, 2] = nd2
      ._xbusca[nd, 3] = Iif(.nIdioma > 1, f3_t(cVarDes), cVarDes)
      ._xbusca[nd, 5] = ''
      ._xbusca[nd, 6] = ''
      ._xbusca[nd, 7] = ''
      ._xbusca[nd, 8] = ''
   EndWith
EndFor

*  store '' to _hlp_filtro,_hlp_car

ENDPROC
PROCEDURE nidioma_access
*>
*> Valor por defecto del idioma de los textos.

If Type('This.nIdioma') # 'N'
   This.nIdioma = 1
EndIf

Return This.nIdioma

ENDPROC
PROCEDURE ost3rt_access
*>
*> Crear objeto de la clase ST3RT.

If Type('This.oST3RT') # 'O'
   This.oST3RT = CreateObject('st3rt')
EndIf

Return This.oST3RT

ENDPROC
PROCEDURE Init
*>
Parameters cFichero, nASql, cEmpresa

*> Definición de constantes generales de ADO.
#Include "adovfp.h"
#Include "dbf2rs.h"

*> Inicializar la clase.
=DoDefault()

*> Valores por defecto de las propiedades.
With ThisForm
   .Source = 'AYUDA'						&& Origen de datos.
   .SourceType = 1							&& Tipo origen de datos: Alias.
   .AdoConn = .F.                       	&& Objeto Connection de ADO.
   .AdoRs = .F.                         	&& Objeto RecordSet de ADO.

   .EOF = .F.								&& Control Eof del RS.
   .BOF = .F.								&& Control Bof del RS.
   .lSYSFCIsOpen = Used('SYSFC')        	&& Control open SYSFC.
   .lSYSHLIsOpen = Used('SYSHL')        	&& Control open SYSHL.
   .lSYSVARIsOpen = Used('SYSVAR')			&& Control open SYSVAR.

   .CursorOpenType = ADOPENSTATIC			&& Modo apertura del cursor ADO.
   .CursorLockType = ADLOCKREADONLY 		&& Modo bloqueo del cursor ADO.
   .CursorLocation = ADUSECLIENT			&& Localización del cursor ADO.
   .GridRecords = 25						&& Registros en el Grid.
   .FetchRecords = 50						&& Registros a cargar desde el RS.
   .GridCurrentRecord = 0					&& Registro actual del Grid.
   .GridLastRecord = 0						&& Ultimo registro actual del Grid.
   .cSQL = ''								&& Sentencia SQL.
   .File = ''								&& Fichero origen da datos.
   .nASql = 0								&& Handle ODBC de la DB.
   .cEmpresa = ''							&& Empresa activa.
   .nIdioma = 1								&& Idioma de los textos.
   .MessageText = ''						&& Mensaje de error.
   .StatusCode = 0							&& Código de error.
   .lFileIsOpen = .F.						&& Control open fichero a consultar.
   .cCurrentAlias = Select()				&& Alias activo al entrar.
   .cWOnTop = Left(WOnTop() + Space(10), 10)&& Ventana activa.
   .cProgram = ''							&& Programa anexo a la ayuda.

   *> Asignar parámetros básicos de PROCAOT.
   If PCount()==3
      .nASql = nASql
      .File = cFichero
      .cEmpresa = cEmpresa
   EndIf
EndWith

ENDPROC
PROCEDURE Resize
ancho=thisform.width
alto=thisform.height
if alto>90
  with thisform
    .grid1.height=alto-40
    .grid1.width=ancho-7
    .bot_ok.top=alto-32
    .bot_esc.top=alto-32
    .bot_ok.left=ancho-76
    .bot_esc.left=ancho-38
    .L_Normal1.top=alto-33
    .L_Normal2.top=alto-21
  endwith
endif

ENDPROC
PROCEDURE Release

*> Eliminar la conexión con ADO.
Local _prghlx

*> Código a ejecutar DESPUES de visualizar la ayuda.
If File(This.cProgram + '.PRG') .Or. File(This.cProgram + '.FXP')
  _prghlpx = "=" + This.cProgram + "('" + This.cWOnTop + "', 'FINAL')"
  &_prghlpx
EndIf

=DoDefault()

If Type('This.AdoRs')=='O'
   If This.AdoRs.State > 0
      This.AdoRs.Close
   EndIf
   This.AdoRs = .Null.
EndIf

With This
   .AdoConn = .Null.
   .oPROCAOT = .Null.
   .oST3RT = .Null.
EndWith

*> Cerrar ficheros de sistema abiertos durante el proceso.
If !This.lSYSFCIsOpen
   Use In Select('SYSFC')
EndIf
If !This.lSYSVARIsOpen
   Use In Select('SYSVAR')
EndIf
If !This.lSYSHLIsOpen
   Use In Select('SYSHL')
EndIf

*> Cerrar el archivo a consultar.
If !This.lFileIsOpen
   Use In Select (This.File)
EndIf

*> Restaurar el entorno de trabajo anterior.
If !Empty(This.cCurrentAlias)
   Select (This.cCurrentAlias)
EndIf

select AYUDA
use

ENDPROC


PROCEDURE Grid1.AfterRowColChange
LPARAMETERS nColIndex
*
_hlp_car=''

ENDPROC
PROCEDURE Grid1.Scrolled
*>
*> Control del desplazamiento a través de la barra vertical.

LParameters nDirection
Local r1

=DoDefault()

Do Case
   *> Flecha abajo.
   Case nDirection==1
      Select (ThisForm.Source)
      KeyBoard '{DNARROW}'

   *> Desplazamiento sobre el cuadro.
   Case nDirection==2

   *> Desplazamiento vertical abajo.
   Case nDirection==3
      Select (ThisForm.Source)
      KeyBoard '{PGDN}'
EndCase

Return

ENDPROC


PROCEDURE Grid1.Column1.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column1.Header1.Click
*>
*> Ordenar por columna, orden ascendente.

ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column1.Header1.DblClick
*>
*> Ordenar por columna, orden descendente.

ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column1.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC
PROCEDURE Grid1.Column1.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 1)

ENDPROC


PROCEDURE Grid1.Column2.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column2.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column2.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column2.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 2)

ENDPROC
PROCEDURE Grid1.Column2.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column3.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column3.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column3.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column3.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 3)

ENDPROC
PROCEDURE Grid1.Column3.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column4.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column4.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column4.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column4.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 4)

ENDPROC
PROCEDURE Grid1.Column4.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column5.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column5.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column5.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column5.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 5)

ENDPROC
PROCEDURE Grid1.Column5.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column6.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column6.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column6.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column6.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC
PROCEDURE Grid1.Column6.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 6)

ENDPROC


PROCEDURE Grid1.Column7.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column7.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column7.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column7.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 7)

ENDPROC
PROCEDURE Grid1.Column7.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column8.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column8.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column8.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column8.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 8)

ENDPROC
PROCEDURE Grid1.Column8.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column9.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column9.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column9.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column9.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 9)

ENDPROC
PROCEDURE Grid1.Column9.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column10.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column10.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column10.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column10.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 10)

ENDPROC
PROCEDURE Grid1.Column10.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column11.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column11.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column11.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column11.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 11)

ENDPROC
PROCEDURE Grid1.Column11.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column12.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column12.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column12.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column12.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 12)

ENDPROC
PROCEDURE Grid1.Column12.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column13.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column13.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column13.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column13.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC
PROCEDURE Grid1.Column13.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 13)

ENDPROC


PROCEDURE Grid1.Column14.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column14.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column14.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column14.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 14)

ENDPROC
PROCEDURE Grid1.Column14.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column15.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column15.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column15.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column15.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 15)

ENDPROC
PROCEDURE Grid1.Column15.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column16.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column16.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column16.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column16.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 16)

ENDPROC
PROCEDURE Grid1.Column16.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column17.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column17.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column17.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column17.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 17)

ENDPROC
PROCEDURE Grid1.Column17.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column18.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column18.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column18.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column18.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 18)

ENDPROC
PROCEDURE Grid1.Column18.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column19.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column19.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column19.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column19.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 19)

ENDPROC
PROCEDURE Grid1.Column19.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Grid1.Column20.Header1.DblClick
ThisForm.ordenardesc(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column20.Header1.RightClick
ThisForm.filtro(This.Parent.ColumnOrder)

ENDPROC
PROCEDURE Grid1.Column20.Header1.Click
ThisForm.ordenar(This.Parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column20.Text1.KeyPress

LParameters nKeyCode, nShiftAltCtrl

=ThisForm.GridKeyPress(nKeyCode, nShiftAltCtrl, 20)

ENDPROC
PROCEDURE Grid1.Column20.Text1.DblClick
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE bot_ok.Click
store .T. to _hlpok,_ok
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE bot_esc.Click
*thisform.release
ThisForm.Hide

ENDPROC


PROCEDURE Bot_prn.Click
*>
List To Printer Prompt Off NoConsole

ENDPROC


EndDefine 
Define Class procaot as custom
Height = 15
Width = 100



PROCEDURE salvaretiqueta
*>
*> Salvar los datos de una etiqueta de artículo en CodBar.
*> Utilizado desde impresión de etiquetas de artículo.
Parameters cNumPal, cCodPro, cCodArt, nCanFis, cNumLot, dFecCad

Local lEstado

Store .T. To lEstado

=This.InitEtiqueta()

If This.ExistsEtiqueta(cNumPal)
   *> Ya existe la etiqueta: Aviso y reemplazar.
   With This
      .nCodigoError = 1
      .cTextoError = "SALVARETIQUETA"
      .cTextoErrorExtendido = "Ya existe el palet " + cNumPal + This.cCrLf
   EndWith
Else
   Select F50X
   Append Blank
EndIf

Replace F50xNumPal With cNumPal, ;
        F50xCodPro With cCodPro, ;
        F50xCodArt With cCodArt, ;
        F50xCanFis With nCanFis, ;
        F50xNumLot With cNumLot, ;
        F50xFecCad With dFecCad, ;
        F50xDate   With Date(), ;
        F50xEstado With '0'

Return lEstado

ENDPROC
PROCEDURE obteneretiqueta
*>
*> Obtener los datos de una etiqueta de CodBar generada con anterioridad.

Parameters cNumPal
Local lEstado

Store .T. To lEstado

=This.InitEtiqueta()

*> Buscar etiqueta en archivo auxiliar.
lEstado = This.ExistsEtiqueta(cNumPal)

If lEstado
   With This
      .UsrPropietario = F50X.F50xCodPro
      .UsrArticulo = F50X.F50xCodArt
      .UsrPalet = F50X.F50xNumPal
      .UsrLote = F50X.F50xNumLot
      .UsrCantidad = F50X.F50xCanFis
      .UsrCaducidad = F50X.F50xFecCad
   EndWith
Else
   With This
      .UsrPropietario = ''
      .UsrArticulo = ''
      .UsrPalet = ''
      .UsrLote = ''
      .UsrCantidad = 0
      .UsrCaducidad = ''

      .nCodigoError = 1
      .cTextoError = "OBTENERETIQUETA"
      .CTextoErrorExtendido = "No existe la etiqueta del palet " + cNumPal + This.cCrLf
   EndWith
EndIf

Return lEstado

ENDPROC
PROCEDURE openetiqueta
*>
*> Abre/crea la tabla F50X, auxiliar de etiquetas de artículos (CodBar).

Local lEstado

Store .T. To lEstado

If Used('F50X')
   Use In F50X
EndIf

If File('F50X.Dbf')
   Use F50X In 0 Order 1
Else
   Create Table F50X (;
      F50xCodPro C(6), ;
      F50xCodArt C(13), ;
      F50xNumPal C(10), ;
      F50xNumLot C(15), ;
      F50xFecCad D, ;
      F50xCanFis N(10, 0), ;
      F50xDate D, ;
      F50xEstado C(1))

      Index On F50xNumPal Tag NUMPAL
EndIf

Return lEstado

ENDPROC
PROCEDURE ccrlf_access
*>
*> Declara retorno de línea.

If Type('This.cCrLf') # 'C'
   This.cCrLf = Chr(13) + Chr(10)
EndIf

Return This.cCrLf

ENDPROC
PROCEDURE existsetiqueta
*>
*> Verificar la existencia o no de una etiqueta a partir del nº de palet.

Parameters cNumPal
Local lEstado

=This.InitEtiqueta()

Select F50X
Seek cNumPal

lEstado = Found()

Return lEstado
ENDPROC
PROCEDURE actualizarestadoetiqueta
*>
*> Actualiza el estado de una etiqueta, a partir del Nº de palet.

Parameters cNumPal
Local lEstado

Store .T. To lEstado

=This.InitEtiqueta()

Select F50X
Seek cNumPal

If !Found()
   With This
      .nCodigoError = 2
      .cTextoError = "ACTUALIZARESTADO"
      .CTextoErrorExtendido = "No existe la etiqueta del palet " + cNumPal + This.cCrLf
   EndWith

   lEstado = .F.
Else
   Replace F50xEstado With '1'
EndIf

Return lEstado

ENDPROC
PROCEDURE opentabla
*>
*> Abrir una tabla definida en SYSFC.
*> Recibe: cTabla ---> Nombre de la tabla a abrir.

Parameters cTabla

Local lEstado, cOldSelect, nConexion
Local lSysfcIsOpen
Local cFcfc, cDirFich, cFile, nError
Private cWhere, Sql_Fc

If Used(cTabla)
   lEstado = .T.
   Return lEstado
EndIf

*> Verificar/abrir la conexión con la base de datos.
nConexion = This.nConexion
If nConexion <= 0
   Store .F. To lEstado
   Return lEstado
EndIf

Store .T. To lEstado, lSysfcIsOPen
cOldSelect = Select()

cDirFich = "fich"

*> Abrir tabla de definición de ficheros.
If !Used('SYSFC')
   Use SYSFC In 0 Order 1 Alias SYSFC SHARED NOUPDATE
   lSysfcIsOPen = .F.
EndIf

Select SYSFC
Seek Upper(cTabla)
If !Eof()
   cFcfc = Iif(Len(cTabla) > 4, cTabla, cTabla + This.cEmpresaPROCAOT)
   cFile = cFcfc

   *> Buscar imagen de la tabla en directoro de configuración.
   If !Empty(FC_FCS) .And. !File(cFcfc + '.DBF')
      cFcfc = Iif(At('\', FC_FCS) = 0, cDirFich + '\' + AllTrim(FC_FCS), AllTrim(FC_FCS))
   EndIf

   Sql_Fc = cFile
   cWhere = FC_INIT
   cWhere = &cWhere

   =CrtCursor(cFile, cTabla, 'C')

   *cWhere = "SELECT * FROM " + cFile + " WHERE 0=1"
   *cWhere = "SELECT * FROM " + cFile + " WHERE " + cWhere
   *cWhere = "SELECT * FROM " + cFile + " WHERE ROWNUM = 1"

   *> Abrir la tabla seleccionada.
   *nError = SqlExec(nConexion, cWhere, cTabla)
   *If nError <= 0
   *   Store .F. To lEstado
   *Else
   *   =SqlMoreResults(nConexion)
   *   Delete All
   *EndIf
Else
   *> La tabla no está definida en SYSFC.
   Store .F. To lEstado
EndIf

*> Restaurar el entorno de trabajo anterior.
If !lSysfcIsOpen
   Use In SYSFC
EndIf

If !Empty(cOldSelect)
   Select (cOldSelect)
EndIf

Return lEstado

ENDPROC
PROCEDURE cempresaprocaot_access
*>
*> Declara la empresa en PROCAOT.

If Type('This.cEmpresaPROCAOT') # 'C'
   This.cEmpresaPROCAOT = Iif(Type('_em')=='C', _em , '001')
EndIf

Return This.cEmpresaPROCAOT

ENDPROC
PROCEDURE existsetiquetanew
*>
*> Verificar la existencia o no de una etiqueta a partir del nº de palet.

Parameters cNumPal
Local lEstado

=This.InitEtiqueta()

Select F50X
Delete All

m.F50xNumPal = cNumPal
lEstado = f3_seek('F50X')

Return lEstado

ENDPROC
PROCEDURE openetiquetanew
*>
*> Abre/crea la tabla F50X, auxiliar de etiquetas de artículos (CodBar).

Local lEstado

Store .T. To lEstado

If !Used('F50X')
   lEstado = This.OpenTabla('F50X', .F.)
EndIf

Return lEstado

ENDPROC
PROCEDURE salvaretiquetanew
*>
*> Salvar los datos de una etiqueta de artículo en CodBar.

Parameters cNumPal, cCodPro, cCodArt, nCanFis, cNumLot, dFecCad

Private cWhere
Local lEstado

=This.InitEtiqueta()

lEstado = This.ExistsEtiquetaNew(cNumPal)

If lEstado
   *> Ya existe la etiqueta: Aviso y reemplazar.
   With This
      .nCodigoError = 1
      .cTextoError = "SALVARETIQUETA"
      .cTextoErrorExtendido = "Ya existe el palet " + cNumPal + This.cCrLf
   EndWith
EndIf

Select F50X
Delete All
Append Blank

Replace F50xNumPal With cNumPal, ;
        F50xCodPro With cCodPro, ;
        F50xCodArt With cCodArt, ;
        F50xCanFis With nCanFis, ;
        F50xNumLot With cNumLot, ;
        F50xFecCad With dFecCad, ;
        F50xDate   With Date(), ;
        F50xEstado With '0'

If lEstado
   cWhere = "F50xNumPal='" + cNumPal + "'"
   =f3_UpdTun('F50X', , , , , cWhere, 'N')
Else
   =f3_InsTun('F50X', 'F50X' , 'N')
EndIf

=SqlCommit(_ASql)

Store .T. To lEstado
Return lEstado

ENDPROC
PROCEDURE obteneretiquetanew
*>
*> Obtener los datos de una etiqueta de CodBar generada con anterioridad.

Parameters cNumPal
Local lEstado

Store .T. To lEstado

=This.InitEtiqueta()

*> Buscar etiqueta en archivo auxiliar.
lEstado = This.ExistsEtiquetaNew(cNumPal)

If lEstado
   *Select F50X
   *Go Top

   With This
      .UsrPropietario = F50X.F50xCodPro
      .UsrArticulo = F50X.F50xCodArt
      .UsrPalet = F50X.F50xNumPal
      .UsrLote = F50X.F50xNumLot
      .UsrCantidad = F50X.F50xCanFis
      .UsrCaducidad = F50X.F50xFecCad
   EndWith
Else
   With This
      .UsrPropietario = ''
      .UsrArticulo = ''
      .UsrPalet = ''
      .UsrLote = ''
      .UsrCantidad = 0
      .UsrCaducidad = ''

      .nCodigoError = 1
      .cTextoError = "OBTENERETIQUETA"
      .CTextoErrorExtendido = "No existe la etiqueta del palet " + cNumPal + This.cCrLf
   EndWith
EndIf

Return lEstado

ENDPROC
PROCEDURE nconexion_access
*>
*> Abrir / Verificar conexión con la base de datos.
If Type('This.nConexion') # 'N'
   If Type('_ASql') == 'N'
      This.nConexion = _ASql
   Else
      =Sys(3053)
      This.nConexion = SqlConnect()
   EndIf
EndIf

Return This.nConexion

ENDPROC
PROCEDURE validararticulo
*>
*> Validar un artículo. Opcionalmente devuelve la descripción en una propiedad.

Parameters cPropietario, cArticulo, lDevolverDescripcion

Local lEstado, lIsOpen

lEstado = .T.
lIsOpen = Used('F08c')

If !lIsOpen
   lEstado = This.OpenTabla('F08c')
EndIf

If lEstado
   m.F08cCodPro = cPropietario
   m.F08cCodArt = cArticulo
   lEstado = f3_seek('F08c')
   If !lEstado
      If !lIsOpen
         Use In F08c
      EndIf

      Return lEstado
   EndIf

   *> Guardar el código de artículo.
   This.cCodigo = F08c.F08cCodArt

   *> Asignar la descripción a una propiedad de la clase.
   If lDevolverDescripcion
      This.cDescripcion = Iif(lEstado, F08c.F08cDescri, '')
   EndIf

   If !lIsOpen
      Use In F08c
   EndIf
EndIf

Return lEstado

ENDPROC
PROCEDURE validarpropietario
*>
*> Validar un propietario. Opcionalmente devuelve el nombre en una propiedad.

Parameters cPropietario, lDevolverNombre

Local lEstado, lIsOpen

lEstado = .T.
lIsOpen = Used('F01p')

If !lIsOpen
   lEstado = This.OpenTabla('F01p')
EndIf

If lEstado
   m.F01pCodigo = cPropietario
   lEstado = f3_seek('F01p')

   *> Asignar el nombre a una propiedad de la clase.
   If lDevolverNombre
      This.cDescripcion = Iif(lEstado, F01p.F01pDescri, '')
   EndIf

   If !lIsOpen
      Use In F01p
   EndIf
EndIf

Return lEstado

ENDPROC
PROCEDURE destroyetiquetas
*>
*> Cerrar, si cal, la tabla de etiquetas inventario.

If This.l_IsOpenF50X
   Use In F50X
EndIf

ENDPROC
PROCEDURE initprocaot
*>
*> Inicializar PROCAOT.

Parameters cEmpresa, xConexion

This.cEmpresaPROCAOT = cEmpresa

If Type('xConexion')=='N'
   This.nConexion = xConexion
Else
   If Type('xConexion')=='C'
      Open DataBase Conexion Shared
      This.nConexion = SqlConnect(xConexion)
      Close DataBase
   Else
      =Sys(3053)
      This.nConexion = SqlConnect()
   EndIf
EndIf

*> Tablas de sistema.
If !Used('SYSPRG')
   Select 0
   Use SysPrg Order 1 Alias SYSPRG SHARED NOUPDATE
   Store .T. To l_IsOpenSYSPRG
EndIf

If !Used('SYSFC')
   Select 0
   Use SysFc Order 1 Alias SYSFC SHARED NOUPDATE
   Store .T. To l_IsOpenSYSFC
EndIf

If !Used('SYSVAR')
   Select 0
   Use SysVar Order 1 Alias SYSVAR SHARED NOUPDATE
   Store .T. To l_IsOpenSYSVAR
EndIf

ENDPROC
PROCEDURE initetiqueta
*>
*> Inicializar la clase.

With This
   .nCodigoError = 0
   .cTextoError = ''
   .cTextoErrorExtendido = ''
EndWith

If !Used('F50X')
   =This.OpenEtiquetaNew()
   Store .T. To This.l_IsOpenF50X
EndIf

ENDPROC
PROCEDURE writeincidencia
*>
*> Grabar una incidencia en la tabla de incidencias de inventario.

Parameters cCodigo, cErr, cErrorExtendido, cSql

Local cOldSelect, lEstado

cOldSelect = Select()
Store .T. To lEstado

*> Valores por defecto.
If Type('cCodigo') # 'C'
   cCodigo = Space(1)
EndIf
If Type('cErr') # 'C'
   cErr = Space(1)
EndIf
If Type('cErrorExtendido') # 'C'
   cErrorExtendido = Space(1)
EndIf
If Type('cSql') # 'C'
   cSql = Space(1)
EndIf

*> Abrir la tabla de incidencias.
If !Used('F99v')
   This.OpenIncidencias()
EndIf

Select F99v
Append Blank
Replace cUsuario   With '', ;
        cLista     With '', ;
        dFecha     With Date(), ;
        cError     With cCodigo, ;
        cTexto     With cErr, ;
        cTextoExt  With cErrorExtendido, ;
        cSentencia With cSql

Use In F99v

*>
If !Empty(cOldSelect)
   Select (cOldSelect)
EndIf

Return lEstado

ENDPROC
PROCEDURE openincidencias
*>
*> Abrir crear tabla F99v, donde se guardarán las incidencias del proceso.

Local lEstado

Store .T. To lEstado

If File('F99v.Dbf')
   Use F99v In 0 SHARED
Else
   Create Table F99v (;
      cUsuario C(4), ;
      cLista C(6), ;
      dFecha D, ;
      cError C(1), ;
      cTexto Memo, ;
      cTextoExt Memo, ;
      cSentencia Memo)
EndIf

*>
Return lEstado

ENDPROC
PROCEDURE ubicdigit
*>
*> Calcular el dígito de control de una ubicación.

Parameters cUbicacion

Private Valor, Inx
Local cDigito

Store 0 To Valor
Store Space(1) to cDigito

For Inx = 5 To Len(cUbicacion)
   Valor = Valor + Asc(SubStr(cUbicacion, Inx + 4, 1)) * Inx
EndFor

***** Valor = Val(SubStr(cUbicacion, 9, 3)) * Val(SubStr(cUbicacion, 12, 2))

*> Obtener la letra correspondiente al valor calculado.
Do While Valor > 25
   Valor = Mod(Valor, 25)
EndDo

If Valor = 22
   Valor = 54
EndIf

*>
cDigito = Chr(Valor + Asc('A'))
Return cDigito

ENDPROC
PROCEDURE f3sql
*>
*> Realizar una consulta SQL bassada en f3_sql de st3rt. Recibe los mismos parámetros.
*>
*> Devuelve un recordset
Parameters cFields, cFromF, cWhere, cOrder, cGroup

Local lEstado
ENDPROC
PROCEDURE executesql
*>
*> Ejecuta una sentencia SQL, recibida como parámetro.
*> Esta sentencia se puede enviar directamente, o se genera a través de This.GetSQL
*>
*>   Recibe: cSql ---> Sentencia SQL a ejecutar.
*>
*> Devuelve: rsSql -------> RecordSet con los datos, si OK.
*>                         '', si error.
*>
*> En caso de error genera registro de incidencias (This.Anom):
*> Código: '42' (Fijo).
*>  Texto: En función del error.

Parameters cSql

Private rsRSet
Local nErrorSQL

*> Inicializar variable a devolver.
Store .F. To rsRSet

*> Ejecutar la sentencia SQL, obteniendo un vulgar cursor.
nErrorSQL = SqlExec(This.nConexion, cSql, 'SqlCur')
If nErrorSQL <= 0

   *> Generar registro de incidencias.
   With This
      .nCodigoError = 1
      .cTextoError = "EXECUETESQL"
      .cTextoErrorExtendido = "Error en sentencia" + This.cCrLf
   EndWith

   Store .F. To rsRSet
   Return rsRSet
EndIf

*> Actualizar buffers.
=SqlMoreResults(This.nConexion)

*> En función de que la sentencia SQL sea tipo SELECT o tipo INSERT/UPDATE/DELETE,
*> la llamada a SqlExec() nos devuelve o no un cursor, en cuyo caso generaremos
*> el RecordSet que se devuelve al programa principal.
If !Used('SqlCur')
   *> NO debe devolver RecordSet.
   Return rsRSet
EndIf

*> Crear el RecordSet de la tabla activa.
*> También se le puede pasar el ALIAS como parámetro.
Select SqlCur
rsRSet = This.CreateRS()
If Type('rsRSet') # 'O'
   *> No se ha podido crear el recordSet. Generar registro de incidencias.
   With This
      .nCodigoError = 1
      .cTextoError = "EXECUETESQL"
      .cTextoErrorExtendido = "No se puede crear el RS" + This.cCrLf
   EndWith
   Store .F. To rsRSet
   Return rsRSet
EndIf

*> Añadir registros de la tabla activa al RecordSet.
*> También se le puede pasar el ALIAS como parámetro.
Select SqlCur
rsRSet = This.AddAllRecordsToRS(rsRSet)
If Type('rsRSet') # 'O'
   *> No se han podido añadir registros al RecordSet. Generar registro de incidencias.
   *> Generar registro de incidencias.
   With This
      .nCodigoError = 1
      .cTextoError = "EXECUETESQL"
      .cTextoErrorExtendido = "Error añadiendo registros al RS" + This.cCrLf
   EndWith

   Store .F. To rsRSet
   Return rsRSet
EndIf

Use In SqlCur

Return rsRSet

ENDPROC
PROCEDURE destroyprocaot
*>
*> Cerrar, si cal, las tablas de sistema de PROCAOT.

If This.l_IsOpenSYSPRG
   Use In SYSPRG
EndIf

If This.l_IsOpenSYSFC
   Use In SYSFC
EndIf

If This.l_IsOpenSYSVAR
   Use In SYSVAR
EndIf

ENDPROC
PROCEDURE executedirect
*>
*> Ejecuta una sentencia SQL, recibida como parámetro.
*> Esta sentencia se puede enviar directamente, o se genera a través de This.GetSQL
*>
*>   Recibe: cSql ---> Sentencia SQL a ejecutar.
*>      nConexion ---> Conexión con la base de datos.
*>        cCursor ---> Cursor (Opcional) a devolver.
*>
*> Devuelve: .T./.F.
*>

Parameters cSql, nConexion, cCursor

Local lEstado, nErrorSQL

Store .T. To lEstado

*> Ejecutar la sentencia SQL.
nErrorSQL = SqlExec(nConexion, cSql, cCursor)
If nErrorSQL <= 0

   *> Generar registro de incidencias.
   With This
      .nCodigoError = 1
      .cTextoError = "EXECUETEDIRECT"
      .cTextoErrorExtendido = "Error en sentencia" + This.cCrLf
   EndWith

   =SqlRollBack(nConexion)

   Store .F. To lEstado
   Return lEstado
EndIf

=SqlCommit(nConexion)
Return lEstado

ENDPROC
PROCEDURE getnumerof99a
*>
*> Obtener numeraciones a partir de secuencias de ORACLE.
Parameters cCodNum

Local cNumeroDevuelto, lIsOpen

lIsOpen = Used('F99A')
If !lIsOpen
   =This.OpenTabla('F99A')
EndIf

cNumeroDevuelto = Ora_NewNum(cCodNum)

If !lIsOpen
   Use In F99A
EndIf

Return cNumeroDevuelto

ENDPROC
PROCEDURE validararticulocb
*>
*> Validar un artículo por código de barras.
*> Busca el CB en F08c (F08cCodBar). Si no existe, busca en F08e.
*> Opcionalmente devuelve la descripción (mediante This.ValidarArticulo()).
*> El parámetro propietario es obsoleto.

Parameters cPropietario, cArticuloCB, lDevolverDescripcion

Private cField, cFromF, cWhere, cOrder, cGroup
Local lEstado

lEstado = .T.

If Used('F08ceCur')
   Use In F08ceCur
EndIf

cField = '*'
cFromF = 'F08c'
cWhere = "F08cCodEAN='" + PadR(cArticuloCB, 13) + "'"
cOrder = ''
cGroup = ''

*> Buscar primero en ficha del artículo.
lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F08ceCur')
If lStado
   Select F08ceCur
   Go Top

   *> Guardar el código de artículo.
   This.cCodigo = F08ceCur.F08cCodArt

   If lDevolverDescripcion
      *> Asignar la descripción a una propiedad de la clase.
      This.cDescripcion = F08cDescri
   EndIf

   Use In F08ceCur
   Return lStado
EndIf

*> Buscar el CB en ficha de artículos/códigos de barras.
If Used('F08ceCur')
   Use In F08ceCur
EndIf

cField = '*'
cFromF = 'F08e'
cWhere = "F08eCodBar='" + PadR(cArticuloCB, 15) + "'"
cOrder = ''
cGroup = ''

*> Buscar primero en ficha del artículo.
lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'F08ceCur')
If lStado
   Select F08ceCur
   Go Top

   *> Validar en ficha de artículo.
   lStado = This.ValidarArticulo(F08cCodPro, F08cCodArt, lDevolverDescripcion)
   Use In F08ceCur
Else
   *> No existe el CB.
EndIf

*> Situar el código de artículo en alguna parte.

Return lEstado

ENDPROC
PROCEDURE Init
*>
*> Inicializar la clase.

Parameters cEmpresa, xConexion

*>
=DoDefault()

If PCount()>=2
   =This.InitPROCAOT(cEmpresa, xConexion)
EndIf

ENDPROC
PROCEDURE Destroy
*>
=This.DestroyEtiquetas()
=This.DestroyPROCAOT()

=DoDefault()
ENDPROC


EndDefine 
Define Class msgdbg as custom



PROCEDURE initialize

*> Dar valores por defecto a la clase.

With This
	.cUser = Iif(Type('_UsrCod')=="C", _UsrCod, "")
	.cFecha = TToC(DateTime())
	.cCursor = ""
	.cSQL = ""
	.cError = ""
	.cMessage = ""
	.cProgram = ""
EndWith

ENDPROC
PROCEDURE exec

*> Grabar propiedades en el fichero LOG.

*> cParams ----> Lista de parámetros opcionales (ver This.SetParams).

Parameters cParams

Local cOldSele

*> Actualizar los parámetros, si cal.
This.SetParams(cParams)
This.SetPrograma

cOldSele = Select()

If !File(Sys(5) + CurDir() + 'F99q.Dbf')
	Create Table F99q( ;
				 F99qCodUsr C(6), ;
                 F99qFecha C(22), ;
                 F99qCursor C(20), ;
                 F99qSQL Memo, ;
				 F99qTxtErr Memo, ;
				 F99qMessg Memo, ;
				 F99qProgrm Memo)
EndIf

Use In (Select("F99q"))
Use F99q In 0 Shared
Select F99q

Append Blank
Replace F99qCodUsr With This.cUser
Replace F99qFecha With This.cFecha
Replace F99qCursor With This.cCursor
Replace F99qSQL With This.cSQL
Replace F99qTxtErr With This.cError
Replace F99qMessg With This.cMessage
Replace F99qProgrm With This.cProgram

Use In (Select("F99q"))
If !Empty(cOldSele)
	Select (cOldSele)
EndIf

Return

ENDPROC
PROCEDURE setparams

*> Actualizar propiedades de la clase.
*> Ver notas en This._setparams()

*> Parámetros que recibe:

*> cParams ----> Lista de parámetros opcionales, con el formato:
*>              [USUARIO/USR			Código de usuario.
*>               DATETIME/DT			Fecha / hora del proceso.
*>               CURSOR/CUR				Nombre del cursor.
*>               SENTENCIA/SQL			Sentencia SQL.
*>               ERROR/ERR				Texto de error general.
*>               MENSAJE/MSG]			Mensaje de error.

LParameters cParams

Local nd, cPrm, cVal, cUpd, cTmp

If Type('cParams') # 'C'
   Return
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      Case cPrm=='USUARIO' .Or. cPrm=='USR'
         This.cUser = cVal

      Case cPrm=='DATETIME' .Or. cPrm=='DT'
         This.cFecha = cVal

      Case cPrm=='CURSOR' .Or. cPrm=='CUR'
         This.cCursor = cVal

      Case cPrm=='SENTENCIA' .Or. cPrm=='SQL'
         This.cSQL = cVal

      Case cPrm=='ERROR' .Or. cPrm=='ERR'
         This.cError = cVal

      Case cPrm=='MENSAJE' .Or. cPrm=='MSG'
         This.cMessage = cVal

      *> No es un parámetro reconocido.
      Otherwise
   EndCase
EndDo

Return

ENDPROC
PROCEDURE setprograma

*> Asignar el árbol de programas a la propiedad.

Local nInx, cPrograma

nInx = 1
cPrograma = Sys(16, nInx)

Do While !Empty(cPrograma)
	This.cProgram = This.cProgram + cPrograma + cr
	nInx = nInx + 1
	cPrograma = Sys(16, nInx)
EndDo

Return

ENDPROC
PROCEDURE Init

*> Inicialización de la clase.
*> Recibe los siguientes parámetros opcionales:
*>    cParams ----> Parámetros especiales.

*> Ver notas en This._setparams()

Parameters cParams

=DoDefault()

With This
   .Initialize()
   ._setparams(cParams)
EndWith

ENDPROC
PROCEDURE _setparams

*> Actualizar propiedades especiales de la clase.
*> Como la longitud de la cadena no puede exceder de 254 caracteres, se subdivide la asignación de
*> parámetros, asignándose aquí los de longitud mayor.
*> A diferencia de This.SetParams(), método público, aquí se asignan variables, no valores.

*> Parámetros que recibe:

*> cParams ----> Lista de parámetros opcionales, con el formato:
*>              [USUARIO/USR			Código de usuario.
*>               DATETIME/DT			Fecha / hora del proceso.
*>               CURSOR/CUR				Nombre del cursor.
*>               SENTENCIA/SQL			Sentencia SQL.
*>               ERROR/ERR				Texto de error general.
*>               MENSAJE/MSG]			Mensaje de error.

LParameters cParams

Local nd, cPrm, cVal, cUpd, cTmp

If Type('cParams') # 'C'
   Return
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      Case cPrm=='USUARIO' .Or. cPrm=='USR'
         This.cUser = &cVal

      Case cPrm=='DATETIME' .Or. cPrm=='DT'
         This.cFecha = &cVal

      Case cPrm=='CURSOR' .Or. cPrm=='CUR'
         This.cCursor = &cVal

      Case cPrm=='SENTENCIA' .Or. cPrm=='SQL'
         This.cSQL = &cVal

      Case cPrm=='ERROR' .Or. cPrm=='ERR'
         This.cError = &cVal

      Case cPrm=='MENSAJE' .Or. cPrm=='MSG'
         This.cMessage = &cVal

      *> No es un parámetro reconocido.
      Otherwise
   EndCase
EndDo

Return

ENDPROC


EndDefine 
Define Class msganom as custom



EndDefine 
Define Class st3imp2 as form
Height = 50
Width = 360
ShowTips = .T.
AutoCenter = .T.
BorderStyle = 2
Caption = "IMPRESION DIRECTA"
MaxButton = .F.
MinButton = .F.
WindowType = 1
BackColor = Rgb(255,255,128)

ADD OBJECT bot_pan as st_bot WITH AutoSize = .F., Top = 12, Left = 12, Height = 29, Width = 29, Picture = "bmp\pantalla.bmp", Caption = "", TabIndex = 2, ToolTipText = "Impresión vista previa"
ADD OBJECT bot_impp as st_bot WITH AutoSize = .F., Top = 12, Left = 48, Height = 29, Width = 29, Picture = "bmp\printp.bmp", Caption = "", TabIndex = 3, ToolTipText = "Impresora predeterminada (Botón derecho, cambiar)"
ADD OBJECT bot_imp as st_bot WITH AutoSize = .F., Top = 12, Left = 84, Height = 29, Width = 29, Picture = "bmp\print.bmp", Caption = "", TabIndex = 4, ToolTipText = "Seleccionar otra impresora"
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 12, Left = 312, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 6, ToolTipText = "Salir"
ADD OBJECT bot_impc as st_bot WITH AutoSize = .F., Top = 12, Left = 120, Height = 29, Width = 29, Picture = "bmp\grafic.bmp", Caption = "", TabIndex = 5, ToolTipText = "Exportar listado"


PROCEDURE setparams

*> Actualizar parámetros de configuración de la impresión.

*> Parámetros que recibe:

*> cParams ----> Lista de parámetros opcionales, con el formato:
*>              [OUTPUT='D',                                     OUT/OT
*>               LABEL='Listado XXX',                            LBL/LB
*>               CAPTION='Impresión directa',                    CPT/CP
*>               REPORT='ENTRBOLE',                              RPT/RP
*>               CURSOR='XLENTRBOL1',                            CRS/CR
*>               WHILE='!Eof()',                                 WHI/WH
*>               FOR='!Deleted()',                               FOR/FR
*>               LANGUAGE=1,                                     LAN/LG
*>               EXPORT=1 (si cOutput='X',                       EXP/EX
*>               PAGEFROM=n,                                     PFR/PF
*>               PAGETO=m,                                       PTO/PT
*>               COPIES=1,                                       NCP/NC
*>               PRINTER='Nombre impresora',                     PRN/PN
*>               HEADING='Encabezado',                           HED/HD
*>               PLAIN=.F.,                                      PLA/PL
*>               SUMMARY=.F.]                                    SUM/SM

LParameters cParams

Local nd, cPrm, cVal, cUpd, cTmp

If Type('cParams') # 'C'
   Return
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      Case cPrm=='OUTPUT' .Or. cPrm=='OUT' .Or. cPrm=='OT'
         This.Output = cVal

      Case cPrm=='LABEL' .Or. cPrm=='LBL' .Or. cPrm=='LB'
         This.Caption = cVal

      Case cPrm=='CAPTION' .Or. cPrm=='CPT' .Or. cPrm=='CP'
         This.Caption = cVal

      Case cPrm=='REPORT' .Or. cPrm=='RPT' .Or. cPrm=='RP'
         This.Report = 'LstW/' + cVal
         If This.Language > 1
            cTmp = 'LstW' + Str(This.Language, 1, 0) + '/' + cVal
            This.Report = Iif(File(cTmp + '.Frx'), cTmp, This.Report)
         EndIf

      Case cPrm=='CURSOR' .Or. cPrm=='CRS' .Or. cPrm=='CR'
         This.Cursor = cVal

      Case cPrm=='WHILE' .Or. cPrm=='WHI' .Or. cPrm=='WH'
         This.While = cVal

      Case cPrm=='FOR' .Or. cPrm=='FR'
         This.For = cVal

      Case cPrm=='LANGUAGE' .Or. cPrm=='LAN' .Or. cPrm=='LG'
         This.Language = Val(cVal)
         If This.Language > 1
            cTmp = 'LstW' + Str(This.Language, 1, 0) + '/' + cVal
            This.Report = Iif(File(cTmp + '.Frx'), cTmp, This.Report)
         EndIf

      Case cPrm=='EXPORT' .Or. cPrm=='EXP' .Or. cPrm=='EX'
         This.Export = cVal

      Case cPrm=='PAGEFROM' .Or. cPrm=='PFR' .Or. cPrm=='PF'
         This.PageFrom = Val(cVal)

      Case cPrm=='PAGETO' .Or. cPrm=='PTO' .Or. cPrm=='PT'
         This.PageTo = Val(cVal)

      Case cPrm=='PRINTER' .Or. cPrm=='PRN' .Or. cPrm=='PN'
         This.Printer = cVal

      Case cPrm=='COPIES' .Or. cPrm=='NCP' .Or. cPrm=='NC'
         This.Copies = Val(cVal)

      Case cPrm=='HEADING' .Or. cPrm=='HED' .Or. cPrm=='HD'
         This.Heading = cVal

      Case cPrm=='PLAIN' .Or. cPrm=='PLA' .Or. cPrm=='PL'
         This.Plain = Iif(cVal=='S', .T., .F.)

      Case cPrm=='SUMMARY' .Or. cPrm=='SUM' .Or. cPrm=='SM'
         This.Summary = Iif(cVal=='S', .T., .F.)

      *> No es un parámetro reconocido.
      Otherwise
   EndCase

EndDo

ENDPROC
PROCEDURE exec

*> Impresión directa sin selección de dispositivo de salida.

*> Parámetros que recibe:

*> cOutput ----> Dispositivo de salida:
*>               'D', Pantalla.
*>               'P', Impresora predeterminada.
*>               'S', Impresora seleccionada.
*>               'X', Exportar (no implementado).
*>               'W', Seleccionar salida (This.Show).
*>               '?', Salida predeterminada.

*> cParams ----> Lista de parámetros opcionales (ver This.SetParams).

LParameters cOutput, cParams

*> Actualizar los parámetros, si cal.
This.SetParams(cParams)

If Type('cOutput') # 'C'
   Return
EndIf

*> Salida predeterminada.
If cOutput=='?'
   cOutput = Iif(Type('This.Output') # 'C', 'W', This.Output)
EndIf

*> Guardar la selección.
This.Output = cOutput

Do Case
   *> Salida por pantalla.
   Case cOutput=='D'
      =This.bot_pan.Click()

   *> Salida por impresora predeterminada.
   Case cOutput=='P'
      =This.bot_impp.Click()

   *> Cambio impresora predeterminada.
   Case cOutput=='S'
      =This.bot_imp.Click()

   *> Pedir selección de dispositivo de salida.
   Case cOutput=='W'
      This.Show
EndCase

ENDPROC
PROCEDURE initialize

*> Dar valores por defecto a la clase.
With This
   .Caption = f3_t(this.caption)
   .While = '!eof()'
   .For = '!deleted()'
   .PageFrom = 1
   .PageTo = 999999
   .Language = 1
   .Copies = 1
   .Heading = ''
   .Plain = .F.
   .Summary = .F.
EndWith

ENDPROC
PROCEDURE Init

*> Inicialización de la clase.
*> Recibe los siguientes parámetros opcionales:
*>    cOutput ----> Dispositivo de salida (ver This.Exec).
*>    cParams ----> Parámetros opcionales (ver This.SetParams).

Parameters cOutput, cParams

=DoDefault()
With This
   .Initialize()
   .Exec(cOutput, cParams)
   .blnsysprgisopen = Used('SYSPRG')

   If !.blnsysprgisopen
		Use SYSPRG Order 1 In 0
   EndIf 	
EndWith

ENDPROC
PROCEDURE Unload
=DoDefault()
If !this.blnsysprgisopen
	Use in SYSPRG
Endif

ENDPROC


PROCEDURE bot_pan.Click

*> Salida por pantalla.

Private _xfc, r1ls, lxlist, expfor, expr, npagef, npaget

_xfc=thisform.Cursor
select alias(_xfc)
go top
r1ls=iif(eof(),0,recno())

lxlist=thisform.Report
expr=thisform.While
expfor=thisform.For
npagef=thisform.PageFrom
npaget=thisform.PageTo

select alias(_xfc)
report form (lxlist) preview while &expr for &expfor noconsole range npagef, npaget
select alias(_xfc)
if r1ls>0
  go r1ls
endif

ThisForm.Hide

ENDPROC


PROCEDURE bot_impp.Click

*> Salida por la impresora predeterminada.
*> Adaptoado a trabajo en BATCH. AVC - 13.11.2006

Private _xfc, r1ls, lxlist, expfor, expr, npagef, npaget
Private cloPrinter, clDefaultPrinter, cNombreReport

_xfc=thisform.Cursor

select alias(_xfc)
go top
r1ls=iif(eof(),0,recno())

lxlist=thisform.Report
expr=thisform.While
expfor=thisform.For
npagef=thisform.PageFrom
npaget=thisform.PageTo

*> Guardar la impresora predeterminada de la sesión para restaurarla luego.
clDefaultPrinter = Set('PRINTER', 2)

*> Mira si hay impresora predeterminada para el report.
If Empty(ThisForm.Printer)
	cNombreReport = Upper(lxlist)
	Select SYSPRG
	Seek cNombreReport
	If !Eof() .And. !Empty(Prg_Report)
	   cloPrinter = Trim(Prg_report)
	Else
	   cloPrinter = clDefaultPrinter
	Endif

	ThisForm.Printer = cloPrinter
EndIf

*> Establezco como predeterminada del sistema la predeterminada del report.
Set Printer To Name (ThisForm.Printer)

select alias(_xfc)
report form (lxlist) To Printer NoConsole NoWait while &expr for &expfor  range npagef, npaget

*> Restauro la impresora predetermina de la sesión.
Set Printer To Name (clDefaultPrinter)

select alias(_xfc)
if r1ls>0
  go r1ls
endif

ThisForm.Hide

ENDPROC
PROCEDURE bot_impp.RightClick

*> Establecer la impresora predeterminada.
Private cDefaultPrinter, cNombreReport

cDefaultPrinter=GetPrinter()

If !Empty(cDefaultPrinter)
	This.Parent.Printer = Trim(cDefaultPrinter)
EndIf	

*> Guardar, si cal, la impressora asociada al report.
cNombreReport = Upper(Substr(This.Parent.Report, 6))

*> Añadir impresora predeterminada para el report en SYSPRG.
Select SYSPRG
Seek cNombreReport
If !Eof()
   Replace prg_report With cDefaultPrinter
EndIf

ENDPROC
PROCEDURE bot_impp.MouseMove
LPARAMETERS nButton, nShift, nXCoord, nYCoord

*This.ToolTipText = This.Parent.Printer

ENDPROC


PROCEDURE bot_imp.Click

*> Seleccionar el dispositivo de salida.

Private _xfc, r1ls, lxlist, expfor, expr, npagef, npaget
local lIsOpen

_xfc=thisform.Cursor
select alias(_xfc)
go top
r1ls=iif(eof(),0,recno())

lxlist=thisform.Report
expr=thisform.While
expfor=thisform.For
select alias(_xfc)
npagef=thisform.PageFrom
npaget=thisform.PageTo

If Type('ThisForm.Printer')=='C' .And. !Empty(ThisForm.Printer)
   report form (lxlist) to printer name ThisForm.Printer while &expr for &expfor noconsole range npagef, npaget
Else
   report form (lxlist) to printer prompt while &expr for &expfor noconsole range npagef, npaget
EndIf

select alias(_xfc)
if r1ls>0
  go r1ls
endif

ThisForm.Hide

ENDPROC


PROCEDURE bot_esc.Click
ThisForm.Hide

* thisform.release

ENDPROC


PROCEDURE bot_impc.Click
*>
*> Exportar a EXCEL.
Private _xfc, r1ls, cfile
Local loMenu

*> Mostrar menú contextual.
* loMenu = NewObject("_shortcutmenu", Home() + "\FFC\_MENU")
loMenu = NewObject("_shortcutmenu", "FFC\_MENU")

With loMenu
   .AddMenuBar("Exportar a \<EXCEL", ;
               [.vresult = 1], ;
               , ;
               .F., ;
               .F., ;
               .F., ;
               .T.)

   .AddMenuBar("Exportar a \<TEXTO", ;
               [.vresult = 2], ;
               , ;
               .F., ;
               .F., ;
               .F., ;
               .F.)

   .AddMenuBar("Exportar a \<DBF", ;
               [.vresult = 3], ;
               , ;
               .F., ;
               .F., ;
               .F., ;
               .F.)
   .ShowMenu()
EndWith

_xfc=thisform.Cursor
select alias(_xfc)
r1ls=iif(eof(),0,recno())

Do Case
    *> Valores no procesables.
    Case Type('loMenu.vresult') # 'N'

	*> Exportar a EXCEL.
	Case loMenu.vresult==1
		cfile = GetFile("Hoja Excel:XLS", "Selección", "", 0, "EXPORTAR A EXCEL")
		If !Empty(cfile)
			lxlist=thisform.Report
			expr=thisform.While
			expfor=thisform.For
			select alias(_xfc)
			Copy To (cfile) while &expr for &expfor Type XL5
		EndIf

	*> Exportar a TEXTO
	Case loMenu.vresult==2
		cfile = GetFile("Archivo de Texto:TXT", "Selección", "", 0, "EXPORTAR A TEXTO")
		If !Empty(cfile)
			lxlist=thisform.Report
			expr=thisform.While
			expfor=thisform.For
			select alias(_xfc)
			Copy To (cfile) while &expr for &expfor Type SDF
		EndIf

	*> Exportar a DBF
	Case loMenu.vresult==3
		cfile = GetFile("Archivo Dbf:DBF", "Selección", "", 0, "EXPORTAR A DBF")
		If !Empty(cfile)
			lxlist=thisform.Report
			expr=thisform.While
			expfor=thisform.For
			select alias(_xfc)
			Copy To (cfile) while &expr for &expfor
		EndIf
EndCase

select alias(_xfc)
if r1ls>0
  go r1ls
endif

Release loMenu

ThisForm.Hide

ENDPROC


EndDefine 
Define Class foxapi as custom
Height = 17
Width = 100



PROCEDURE getpc
*>
*> Obtener el nombre del PC.

Private cActivePC
Private nLen

*> Declaraciones de funciones de la API.
=This.InitIni()

cActivePC  = Space(255)
nLen = 255

=GetComputerName (@cActivePC, @nLen)
cActivePC = AllTrim(Left(cActivePC, At(Chr(0), cActivePC) - 1))

Return cActivePC

ENDPROC
PROCEDURE getuser
*>
*> Obtener el nombre del usuario (PC).

Private cActiveUser
Private nLen

*> Declaraciones de funciones de la API.
=This.InitIni()

cActiveUser  = Space(255)
nLen = 255

=GetUserName (@cActiveUser, @nLen)
cActiveUser = AllTrim(Left(cActiveUser, At(Chr(0), cActiveUser) - 1))

Return cActiveUser

ENDPROC
PROCEDURE initini
*>
*> Declaraciones para uso de funciones de la API.

   *> Leer el valor de una clave.
   Declare Integer GetPrivateProfileString ;
           In Win32Api ;
           String cSeccion, ;
           String cClave, ;
           String cDefault, ;
           String @Valor, ;
           Integer nTama, ;
           String cFichero

   *> Grabar el valor de una clave.
   Declare Integer WritePrivateProfileString ;
           In Win32Api ;
           String cSeccion, ;
           String cClave, ;
           String cValor, ;
           String cFichero

   *> Leer el valor de una sección.
   Declare Integer GetPrivateProfileSection ;
           In Win32Api ;
           String cSeccion, ;
           String @Valor, ;
           Integer nTama, ;
           String cFichero

   *> Obtener el nombre del PC.
   Declare Integer GetComputerName ;
           In Win32Api ;
           String @cComputer, ;
           Integer @nLen

   *> Obtener el nombre del Usuario.
   Declare Integer GetUserName ;
           In Win32Api ;
           String @cUser, ;
           Integer @nLen

ENDPROC
PROCEDURE getkeyini
*>
*> Obtener el valor de una clave en un fichero INI.
*>   Recibe: cKey -------> Clave a consultar.
*>           cSeccion ---> Sección del fichero.
*>           cFile ------> Path + Nombre del fichero INI. (Por defecto WIN.INI)
*>
*> Devuelve: cValor -----> Valor de la clave. NULL si no existe.

Parameters cKey, cSeccion, cFile
Local cValor
Local nRetorno
Local nTama, cDefault

If Type('cFile') # 'C'
   cFile = 'WIN.INI'
EndIf

nTama = 255                          && Tamaño valor a devolver
cDefault = " "                       && Valor por defecto

This.InitIni()                           && Declaraciones API.

*> Rellenar la variable a enviar a la API.
cValor = Replicate(Chr(0), nTama)
nRetorno = GetPrivateProfileString ( ;
               cSeccion, ;
               cKey, ;
               cDefault, ;
               @cValor, ;
               nTama, ;
               cFile)

If nRetorno > 0
   cValor = SubStr(cValor, 1, At(Chr(0), cValor) - 1)
Else
   cValor = ''
EndIf

Return cValor

ENDPROC
PROCEDURE writekeyini
*>
*> Grabar el valor de una clave en un fichero INI.
*>   Recibe: cKey -------> Clave a consultar.
*>           cSeccion ---> Sección del fichero.
*>           cFile ------> Path + Nombre del fichero INI. (Por defecto WIN.INI)
*>
*> Devuelve: nRetorno ---> Código de status.
*>-----------------------------------------------------------------------------------
Parameters cKey, cSeccion, cValor, cFile
Local nRetorno

If Type('cFile') # 'C'
   cFile = 'WIN.INI'
EndIf

This.InitIni()                      && Declaraciones API.

*> Rellenar la variable a enviar a la API.
nRetorno = WritePrivateProfileString ( ;
               cSeccion, ;
               cKey, ;
               cValor, ;
               cFile)

Return nRetorno

ENDPROC
PROCEDURE abrirconexiongenerica
*>
*> Abrir conexión genérica con la base de datos.

Local nConexion

=Sys(3053)
nConexion = SqlConnect()

If nConexion <= 0
   With This
      .nCodigoError = nConexion
      .cTextoError = "SQLCONNECT"
      .cTextoErrorExtendido = Message() + cr
   EndWith
EndIf

Return nConexion

ENDPROC
PROCEDURE abrirconexion
*>
*> Abrir una conexión ODBC con la base de datos.
*> Específica de PROCAOT, utiliza CONNDB.INI
*>
*>   Recibe: Fichero de configuración (con el path completo)
*>           Nombre estación (opcional)
*>
*> Devuelve: Handle conexión ODBC
*>

Parameters cInitFile, cActivePC

Private cDataBase, cConexion
Private nConexion

Store 0 To nConexion

With This
   .nCodigoError = 0
   .cTextoError = ""
   .cTextoErrorExtendido = ""
EndWith

*> Buscar la conexión con la base de datos.
If Type('cActivePC') # 'C'
   cActivePC = This.GetPC()
EndIf

If !File(cInitFile)
   With This
      .nCodigoError = 1
      .cTextoError = cInitFile
      .cTextoErrorExtendido = "No existe archivo CONNDB.INI"
   EndWith

   Return -1
EndIf

cDataBase = This.GetKeyIni("DataBase", cActivePC, cInitFile)
cConexion = This.GetKeyIni("Conexion", cActivePC, cInitFile)

If Empty(cDataBase)
   With This
      .nCodigoError = 2
      .cTextoError = "cDataBase"
      .cTextoErrorExtendido = "No se encuentra DATABASE"
   EndWith

   Return -1
EndIf

If Empty(cConexion)
   With This
      .nCodigoError = 3
      .cTextoError = "cConexion"
      .cTextoErrorExtendido = "No se encuentra CONEXION"
   EndWith

   Return -1
EndIf

Open DataBase &cDataBase Shared
nConexion  = SqlConnect(cConexion)

If nConexion <= 0
   With This
      .nCodigoError = nConexion
      .cTextoError = "SQLCONNECT"
      .cTextoErrorExtendido = Message()
   EndWith
EndIf

Close DataBase

Return nConexion

ENDPROC
PROCEDURE getdsnbyname
*>
*> Obtener la cadena de conexión ODBC a partir de un nombre de conexión.
*> Específica de PROCAOT, utiliza CONNDB.INI
*>
*>   Recibe: Fichero de configuración
*>           Nombre conexión (Opcional)
*>
*> Devuelve: DSN de la conexión
*>

Parameters cInitFile, cNombreConexion

Private cDSN, nConexion

Store 0 To nConexion
Store "" To cDSN

With This
   .nCodigoError = 0
   .cTextoError = ""
   .cTextoErrorExtendido = ""
EndWith

nConexion = This.AbrirConexion(cInitFile, cNombreConexion)
If nConexion <= 0
   *> Los errores se definen en la llamada.
   Return cDSN
EndIf

cDSN = This.GetDsnByHandle(nConexion)
=SqlDisConnect(nConexion)

Return cDSN

ENDPROC
PROCEDURE getdsnbyhandle
*>
*> Obtener la cadena de conexión ODBC a partir de un handle ODBC
*> Específica de PROCAOT, utiliza CONNDB.INI
*>
*>   Recibe: Handle conexión
*>
*> Devuelve: DSN de la conexión
*>

Parameters nASql

Private cDSN

Store "" To cDSN

With This
   .nCodigoError = 0
   .cTextoError = ""
   .cTextoErrorExtendido = ""
EndWith

cDSN = SqlGetProp(nASql, "ConnectString")

Return cDSN

ENDPROC
PROCEDURE initextern
*>
*> Inicializar para llamadas externas a PROCAOT.
*> Necesario para trabajar con rutinas PROCAOT desde, por ejemplo, Visual Basic.
*>
*> Recibe: Directorio de trabajo de VFP.
*>
*> Recordar que un COM no admite interfaz de usuario.

Parameters cDefaultDir

Local lEstado

Store .T. To lEstado

Set Default To &cDefaultDir
Set Date To DMY

Return lEstado

ENDPROC
PROCEDURE initpublicprocaot
*>
*> Declara las variables públicas necesarias en PROCAOT.
*>
*>        _em : Empresa activa.
*>        _cm : Constante utilizada en cadenas.
*>         cr : Retorno de línea.
*>    _XIdiom : Idioma por defecto.
*>       _esc : Tecla escape pulsada.
*>      _PSql : Control Spool.
*>      _ASql : Handle BBDD.
*>     _LxErr : Texto general de errores.
*>    _UsrCod : Código de usuario.
*>      _xier : Errores SQL.
*>  _DEBUGSQL : Log sentencias SQL.

If Type('_em') # 'C'
   Public _em
   _em = "001"
EndIf

If Type('_cm') # 'C'
   Public _cm
   _cm = "'"
EndIf

If Type('_esc')=='U'
   Public _esc
   _esc = .F.
EndIf

If Type('_PSql')=='U'
   Public _PSql
   _PSql = .F.
EndIf

If Type('_ASql')=='U'
   Public _ASql
   _ASql = 0
EndIf

If Type('_XIdiom') # 'N'
   Public _XIdiom
   _XIdiom = 1
EndIf

If Type('cr') # 'C'
   Public cr
   cr = Chr(13) + Chr(10)
EndIf

If Type('_LxErr') # 'C'
   Public _LxErr
   _LxErr = ""
EndIf

If Type('_UsrCod') # 'C'
   Public _UsrCod
   _UsrCod = ""
EndIf

If Type('_xier') # 'N'
   Public _xier
   _xier = 0
EndIf

If Type('_DEBUGSQL')=='U'
   Public _DEBUGSQL
   _DEBUGSQL = .F.
EndIf

Return

ENDPROC
PROCEDURE initpublicprocaot2
*>
*> Declarar variables públicas de PROCAOT no definidas ya en This.InitPublicPROCAOT().
*> Utilizado principalmente para llamadas desde otros lenguajes.
*>
*> Recibe: Directorio de trabajo de VFP.
*>         Nombre de la variable a declarar.
*>         Valor a asignar.
*>         Tipo de la variable (opcional).

Parameters cDefaultDir, cVariable, cValor, cTipo

Local lEstado

Store .T. To lEstado

Set Default To &cDefaultDir

If Type(cVariable) # 'U'
   *> La variable ya está declarada.
   *>Return lEstado
   Release (cVariable)
EndIf

*> Por defecto, la variable es de tipo carácter.
If Type('cTipo')=='L' .Or. Type('cTipo')=='U'
   cTipo = 'C'
EndIf

Public &cVariable

*> Declarar la variable en función del tipo.
Do Case
   *> Carácter.
   Case cTipo=='C'
      &cVariable = cValor
   *> Fecha.
   Case cTipo=='D'
      &cVariable = CToD(cValor)

   *> Numérico.
   Case cTipo=='N'
      &cVariable = Val(cValor)

   *> Boleano.
   Case cTipo=='B'
      &cVariable = (cValor)

   *> Por defecto, carácter.
   Otherwise
      &cVariable = cValor
EndCase

Return lEstado

ENDPROC
PROCEDURE gethandlebydsn

*> Obtener el handel ODBC a partir de la cadena de conexión
*> Recibe:
*>	- Cadena de conexión.

*> Devuelve:
*>	- Handle de la BBDD (negativo si error)
*>	- Código y texto de error.

Parameters cDSN

Private nHandle

Store 0 To nHandle

With This
   .nCodigoError = 0
   .cTextoError = ""
   .cTextoErrorExtendido = ""
EndWith

nHandle = SqlStringConnect(cDSN)
If nHandle <= 0
With This
   .nCodigoError = -1
   .cTextoError = "Error DSN"
   .cTextoErrorExtendido = "No se ha podido conectar con la BBDD"
EndWith
EndIf

Return nHandle

ENDPROC
PROCEDURE getsectionini

*> Obtener el valor de una sección en un fichero INI.
*>   Recibe: vArray -----> Array a devolver (Importante: Se recibe POR REFERENCIA).
*>           cSeccion ---> Sección del fichero.
*>           cFile ------> Path + Nombre del fichero INI. (Por defecto WIN.INI)
*>
*> Devuelve: nCount -----> Nº de elementos cargados (0=Vacío).

Parameters vArray, cSeccion, cFile

Local cValor, cCurValor, nTama, nInx, nPos

If Type('cFile') # 'C'
	cFile = 'WIN.INI'
EndIf

nTama = 1024                         && Tamaño valor a devolver
nInx = 0
nPos = 1

This.InitIni()                       && Declaraciones API.

*> Rellenar la variable a enviar a la API.
cValor = Replicate(Chr(0), nTama)
nRetorno = GetPrivateProfileSection ( ;
               cSeccion, ;
               @cValor, ;
               nTama, ;
               cFile)

If nRetorno <= 0
	Return 0
EndIf

nPos = At(Chr(0), cValor)
cCurValor = SubStr(cValor, 1, nPos - 1)

Do While !Empty(cCurValor)
	nInx = nInx + 1

	Dimension vArray[nInx, 2]
	vArray[nInx, 1] = StrTran(SubStr(cCurValor, 1, At("=", cCurValor) - 1), '"', "")	&& Clave
	vArray[nInx, 2] = StrTran(SubStr(cCurValor, At("=", cCurValor) + 1), '"', "")		&& Valor

	cValor = SubStr(cValor, nPos + 1)
	nPos = At(Chr(0), cValor)
	cCurValor = SubStr(cValor, 1, nPos - 1)
EndDo

Return nInx

ENDPROC
PROCEDURE Init
*>
*> Inicializar la clase.

=DoDefault()

With This
   .nCodigoError = 0
   .cTextoError = ''
   .cTextoErrorExtendido = ''
EndWith

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones.

*> 30.08.2007 (AVC) Agregar método GetSectionIni, que permite devolver una sección entera de un INI.

ENDPROC


EndDefine 
