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
