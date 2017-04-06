****************************************************************************
***
*** ODBCAPI.PRG  - Defines for ODBC API in Visual FoxPro 3.0
***
***        Pablo Almunia Sanz
***        100341.1136@compuserve.com
***
*** Version - 1.0 - First release
***           1.1 - Bugs eliminated
***
****************************************************************************

#define ODBCVER                         513
#define SQL_SPEC_MAJOR                    2
#define SQL_SPEC_MINOR                    1
#define SQL_SPEC_STRING             "02.01"
#define SQL_SQLSTATE_SIZE                 5
#define SQL_MAX_MESSAGE_LENGTH          512
#define SQL_MAX_DSN_LENGTH               32
#define SQL_INVALID_HANDLE              (-2)
#define SQL_ERROR                       (-1)
#define SQL_SUCCESS                       0
#define SQL_SUCCESS_WITH_INFO             1
#define SQL_NO_DATA_FOUND               100
#define SQL_CHAR                          1
#define SQL_NUMERIC                       2
#define SQL_DECIMAL                       3
#define SQL_INTEGER                       4
#define SQL_SMALLINT                      5
#define SQL_FLOAT                         6
#define SQL_REAL                          7
#define SQL_DOUBLE                        8
#define SQL_VARCHAR                      12
#define SQL_TYPE_NULL                     0
#define SQL_TYPE_MAX                     12
#define SQL_C_CHAR                        1
#define SQL_C_LONG                        4
#define SQL_C_SHORT                       5
#define SQL_C_FLOAT                       7
#define SQL_C_DOUBLE                      8
#define SQL_C_DEFAULT                    99
#define SQL_NO_NULLS                      0
#define SQL_NULLABLE                      1
#define SQL_NULLABLE_UNKNOWN              2
#define SQL_NULL_DATA                   (-1)
#define SQL_DATA_AT_EXEC                (-2)
#define SQL_NTS                         (-3)
#define SQL_CLOSE                         0
#define SQL_DROP                          1
#define SQL_UNBIND                        2
#define SQL_RESET_PARAMS                  3
#define SQL_COMMIT                        0
#define SQL_ROLLBACK                      1
#define SQL_COLUMN_COUNT                  0
#define SQL_COLUMN_NAME                   1
#define SQL_COLUMN_TYPE                   2
#define SQL_COLUMN_LENGTH                 3
#define SQL_COLUMN_PRECISION              4
#define SQL_COLUMN_SCALE                  5
#define SQL_COLUMN_DISPLAY_SIZE           6
#define SQL_COLUMN_NULLABLE               7
#define SQL_COLUMN_UNSIGNED               8
#define SQL_COLUMN_MONEY                  9
#define SQL_COLUMN_UPDATABLE             10
#define SQL_COLUMN_AUTO_INCREMENT        11
#define SQL_COLUMN_CASE_SENSITIVE        12
#define SQL_COLUMN_SEARCHABLE            13
#define SQL_COLUMN_TYPE_NAME             14
#define SQL_COLUMN_TABLE_NAME            15
#define SQL_COLUMN_OWNER_NAME            16
#define SQL_COLUMN_QUALIFIER_NAME        17
#define SQL_COLUMN_LABEL                 18
#define SQL_COLATT_OPT_MAX               18
#define SQL_COLUMN_DRIVER_START        1000
#define SQL_COLATT_OPT_MIN                0
#define SQL_ATTR_READONLY                 0
#define SQL_ATTR_WRITE                    1
#define SQL_ATTR_READWRITE_UNKNOWN        2
#define SQL_UNSEARCHABLE                  0
#define SQL_LIKE_ONLY                     1
#define SQL_ALL_EXCEPT_LIKE               2
#define SQL_SEARCHABLE                    3
#define SQL_NULL_HENV                     0
#define SQL_NULL_HDBC                     0
#define SQL_NULL_HSTMT                    0
#define SQL_MAX_OPTION_STRING_LENGTH    256
#define SQL_STILL_EXECUTING               2
#define SQL_NEED_DATA                    99
#define SQL_DATE                          9
#define SQL_TIME                         10
#define SQL_TIMESTAMP                    11
#define SQL_LONGVARCHAR                 (-1)
#define SQL_BINARY                      (-2)
#define SQL_VARBINARY                   (-3)
#define SQL_LONGVARBINARY               (-4)
#define SQL_BIGINT                      (-5)
#define SQL_TINYINT                     (-6)
#define SQL_BIT                         (-7)
#define SQL_TYPE_DRIVER_START          (-80)
#define SQL_SIGNED_OFFSET              (-20)
#define SQL_UNSIGNED_OFFSET            (-22)
#define SQL_C_DATE                         SQL_DATE
#define SQL_C_TIME                         SQL_TIME
#define SQL_C_TIMESTAMP                    SQL_TIMESTAMP
#define SQL_C_BINARY                       SQL_BINARY
#define SQL_C_BIT                          SQL_BIT
#define SQL_C_TINYINT                      SQL_TINYINT
#define SQL_C_SLONG                        SQL_C_LONG+SQL_SIGNED_OFFSET
#define SQL_C_SSHORT                       SQL_C_SHORT+SQL_SIGNED_OFFSET
#define SQL_C_STINYINT                     SQL_TINYINT+SQL_SIGNED_OFFSET
#define SQL_C_ULONG                        SQL_C_LONG+SQL_UNSIGNED_OFFSET
#define SQL_C_USHORT                       SQL_C_SHORT+SQL_UNSIGNED_OFFSET
#define SQL_C_UTINYINT                     SQL_TINYINT+SQL_UNSIGNED_OFFSET
#define SQL_C_BOOKMARK                     SQL_C_ULONG
#define SQL_TYPE_MIN                       SQL_BIT
#define SQL_ALL_TYPES                     0
#define SQL_DRIVER_NOPROMPT               0
#define SQL_DRIVER_COMPLETE               1
#define SQL_DRIVER_PROMPT                 2
#define SQL_DRIVER_COMPLETE_REQUIRED      3
#define SQL_NO_TOTAL                    (-4)
#define SQL_DEFAULT_PARAM               (-5)
#define SQL_IGNORE                      (-6)
#define SQL_LEN_DATA_AT_EXEC_OFFSET   (-100)
#define SQL_API_SQLALLOCCONNECT           1
#define SQL_API_SQLALLOCENV               2
#define SQL_API_SQLALLOCSTMT              3
#define SQL_API_SQLBINDCOL                4
#define SQL_API_SQLCANCEL                 5
#define SQL_API_SQLCOLATTRIBUTES          6
#define SQL_API_SQLCONNECT                7
#define SQL_API_SQLDESCRIBECOL            8
#define SQL_API_SQLDISCONNECT             9
#define SQL_API_SQLERROR                 10
#define SQL_API_SQLEXECDIRECT            11
#define SQL_API_SQLEXECUTE               12
#define SQL_API_SQLFETCH                 13
#define SQL_API_SQLFREECONNECT           14
#define SQL_API_SQLFREEENV               15
#define SQL_API_SQLFREESTMT              16
#define SQL_API_SQLGETCURSORNAME         17
#define SQL_API_SQLNUMRESULTCOLS         18
#define SQL_API_SQLPREPARE               19
#define SQL_API_SQLROWCOUNT              20
#define SQL_API_SQLSETCURSORNAME         21
#define SQL_API_SQLSETPARAM              22
#define SQL_API_SQLTRANSACT              23
#define SQL_NUM_FUNCTIONS                23
#define SQL_EXT_API_START                40
#define SQL_API_SQLCOLUMNS               40
#define SQL_API_SQLDRIVERCONNECT         41
#define SQL_API_SQLGETCONNECTOPTION      42
#define SQL_API_SQLGETDATA               43
#define SQL_API_SQLGETFUNCTIONS          44
#define SQL_API_SQLGETINFO               45
#define SQL_API_SQLGETSTMTOPTION         46
#define SQL_API_SQLGETTYPEINFO           47
#define SQL_API_SQLPARAMDATA             48
#define SQL_API_SQLPUTDATA               49
#define SQL_API_SQLSETCONNECTOPTION      50
#define SQL_API_SQLSETSTMTOPTION         51
#define SQL_API_SQLSPECIALCOLUMNS        52
#define SQL_API_SQLSTATISTICS            53
#define SQL_API_SQLTABLES                54
#define SQL_API_SQLBROWSECONNECT         55
#define SQL_API_SQLCOLUMNPRIVILEGES      56
#define SQL_API_SQLDATASOURCES           57
#define SQL_API_SQLDESCRIBEPARAM         58
#define SQL_API_SQLEXTENDEDFETCH         59
#define SQL_API_SQLFOREIGNKEYS           60
#define SQL_API_SQLMORERESULTS           61
#define SQL_API_SQLNATIVESQL             62
#define SQL_API_SQLNUMPARAMS             63
#define SQL_API_SQLPARAMOPTIONS          64
#define SQL_API_SQLPRIMARYKEYS           65
#define SQL_API_SQLPROCEDURECOLUMNS      66
#define SQL_API_SQLPROCEDURES            67
#define SQL_API_SQLSETPOS                68
#define SQL_API_SQLSETSCROLLOPTIONS      69
#define SQL_API_SQLTABLEPRIVILEGES       70
#define SQL_API_SQLDRIVERS               71
#define SQL_API_SQLBINDPARAMETER         72
#define SQL_EXT_API_LAST                   SQL_API_SQLBINDPARAMETER
#define SQL_API_ALL_FUNCTIONS             0
#define SQL_NUM_EXTENSIONS                (SQL_EXT_API_LAST-SQL_EXT_API_START+1)
#define SQL_API_LOADBYORDINAL           199
#define SQL_INFO_FIRST                    0
#define SQL_ACTIVE_CONNECTIONS            0
#define SQL_ACTIVE_STATEMENTS             1
#define SQL_DATA_SOURCE_NAME              2
#define SQL_DRIVER_HDBC                   3
#define SQL_DRIVER_HENV                   4
#define SQL_DRIVER_HSTMT                  5
#define SQL_DRIVER_NAME                   6
#define SQL_DRIVER_VER                    7
#define SQL_FETCH_DIRECTION               8
#define SQL_ODBC_API_CONFORMANCE          9
#define SQL_ODBC_VER                     10
#define SQL_ROW_UPDATES                  11
#define SQL_ODBC_SAG_CLI_CONFORMANCE     12
#define SQL_SERVER_NAME                  13
#define SQL_SEARCH_PATTERN_ESCAPE        14
#define SQL_ODBC_SQL_CONFORMANCE         15
#define SQL_DBMS_NAME                    17
#define SQL_DBMS_VER                     18
#define SQL_ACCESSIBLE_TABLES            19
#define SQL_ACCESSIBLE_PROCEDURES        20
#define SQL_PROCEDURES                   21
#define SQL_CONCAT_NULL_BEHAVIOR         22
#define SQL_CURSOR_COMMIT_BEHAVIOR       23
#define SQL_CURSOR_ROLLBACK_BEHAVIOR     24
#define SQL_DATA_SOURCE_READ_ONLY        25
#define SQL_DEFAULT_TXN_ISOLATION        26
#define SQL_EXPRESSIONS_IN_ORDERBY       27
#define SQL_IDENTIFIER_CASE              28
#define SQL_IDENTIFIER_QUOTE_CHAR        29
#define SQL_MAX_COLUMN_NAME_LEN          30
#define SQL_MAX_CURSOR_NAME_LEN          31
#define SQL_MAX_OWNER_NAME_LEN           32
#define SQL_MAX_PROCEDURE_NAME_LEN       33
#define SQL_MAX_QUALIFIER_NAME_LEN       34
#define SQL_MAX_TABLE_NAME_LEN           35
#define SQL_MULT_RESULT_SETS             36
#define SQL_MULTIPLE_ACTIVE_TXN          37
#define SQL_OUTER_JOINS                  38
#define SQL_OWNER_TERM                   39
#define SQL_PROCEDURE_TERM               40
#define SQL_QUALIFIER_NAME_SEPARATOR     41
#define SQL_QUALIFIER_TERM               42
#define SQL_SCROLL_CONCURRENCY           43
#define SQL_SCROLL_OPTIONS               44
#define SQL_TABLE_TERM                   45
#define SQL_TXN_CAPABLE                  46
#define SQL_USER_NAME                    47
#define SQL_CONVERT_FUNCTIONS            48
#define SQL_NUMERIC_FUNCTIONS            49
#define SQL_STRING_FUNCTIONS             50
#define SQL_SYSTEM_FUNCTIONS             51
#define SQL_TIMEDATE_FUNCTIONS           52
#define SQL_CONVERT_BIGINT               53
#define SQL_CONVERT_BINARY               54
#define SQL_CONVERT_BIT                  55
#define SQL_CONVERT_CHAR                 56
#define SQL_CONVERT_DATE                 57
#define SQL_CONVERT_DECIMAL              58
#define SQL_CONVERT_DOUBLE               59
#define SQL_CONVERT_FLOAT                60
#define SQL_CONVERT_INTEGER              61
#define SQL_CONVERT_LONGVARCHAR          62
#define SQL_CONVERT_NUMERIC              63
#define SQL_CONVERT_REAL                 64
#define SQL_CONVERT_SMALLINT             65
#define SQL_CONVERT_TIME                 66
#define SQL_CONVERT_TIMESTAMP            67
#define SQL_CONVERT_TINYINT              68
#define SQL_CONVERT_VARBINARY            69
#define SQL_CONVERT_VARCHAR              70
#define SQL_CONVERT_LONGVARBINARY        71
#define SQL_TXN_ISOLATION_OPTION         72
#define SQL_ODBC_SQL_OPT_IEF             73
#define SQL_CORRELATION_NAME             74
#define SQL_NON_NULLABLE_COLUMNS         75

#define SQL_DRIVER_HLIB                  76
#define SQL_DRIVER_ODBC_VER              77
#define SQL_LOCK_TYPES                   78
#define SQL_POS_OPERATIONS               79
#define SQL_POSITIONED_STATEMENTS        80
#define SQL_GETDATA_EXTENSIONS           81
#define SQL_BOOKMARK_PERSISTENCE         82
#define SQL_STATIC_SENSITIVITY           83
#define SQL_FILE_USAGE                   84
#define SQL_NULL_COLLATION               85
#define SQL_ALTER_TABLE                  86
#define SQL_COLUMN_ALIAS                 87
#define SQL_GROUP_BY                     88
#define SQL_KEYWORDS                     89
#define SQL_ORDER_BY_COLUMNS_IN_SELECT   90
#define SQL_OWNER_USAGE                  91
#define SQL_QUALIFIER_USAGE              92
#define SQL_QUOTED_IDENTIFIER_CASE       93
#define SQL_SPECIAL_CHARACTERS           94
#define SQL_SUBQUERIES                   95
#define SQL_UNION                        96
#define SQL_MAX_COLUMNS_IN_GROUP_BY      97
#define SQL_MAX_COLUMNS_IN_INDEX         98
#define SQL_MAX_COLUMNS_IN_ORDER_BY      99
#define SQL_MAX_COLUMNS_IN_SELECT       100
#define SQL_MAX_COLUMNS_IN_TABLE        101
#define SQL_MAX_INDEX_SIZE              102
#define SQL_MAX_ROW_SIZE_INCLUDES_LONG  103
#define SQL_MAX_ROW_SIZE                104
#define SQL_MAX_STATEMENT_LEN           105
#define SQL_MAX_TABLES_IN_SELECT        106
#define SQL_MAX_USER_NAME_LEN           107
#define SQL_MAX_CHAR_LITERAL_LEN        108
#define SQL_TIMEDATE_ADD_INTERVALS      109
#define SQL_TIMEDATE_DIFF_INTERVALS     110
#define SQL_NEED_LONG_DATA_LEN          111
#define SQL_MAX_BINARY_LITERAL_LEN      112
#define SQL_LIKE_ESCAPE_CLAUSE          113
#define SQL_QUALIFIER_LOCATION          114

#define SQL_OJ_CAPABILITIES           65002
#define SQL_INFO_LAST                      SQL_QUALIFIER_LOCATION
#define SQL_INFO_DRIVER_START          1000

#define SQL_CVT_CHAR                     16
#define SQL_CVT_NUMERIC                  32
#define SQL_CVT_DECIMAL                  64
#define SQL_CVT_INTEGER                 128
#define SQL_CVT_SMALLINT                256
#define SQL_CVT_FLOAT                   512
#define SQL_CVT_REAL                   1024
#define SQL_CVT_DOUBLE                 2048
#define SQL_CVT_VARCHAR                4096
#define SQL_CVT_LONGVARCHAR            8192
#define SQL_CVT_BINARY                16384
#define SQL_CVT_VARBINARY             32768
#define SQL_CVT_BIT                   65536
#define SQL_CVT_TINYINT              131072
#define SQL_CVT_BIGINT               262144
#define SQL_CVT_DATE                 524288
#define SQL_CVT_TIME                1048576
#define SQL_CVT_TIMESTAMP           2097152
#define SQL_CVT_LONGVARBINARY       4194304

#define SQL_FN_CVT_CONVERT               16

#define SQL_FN_STR_CONCAT                16
#define SQL_FN_STR_INSERT                32
#define SQL_FN_STR_LEFT                  64
#define SQL_FN_STR_LTRIM                128
#define SQL_FN_STR_LENGTH               256
#define SQL_FN_STR_LOCATE               512
#define SQL_FN_STR_LCASE               1024
#define SQL_FN_STR_REPEAT              2048
#define SQL_FN_STR_REPLACE             4096
#define SQL_FN_STR_RIGHT               8192
#define SQL_FN_STR_RTRIM              16384
#define SQL_FN_STR_SUBSTRING          32768
#define SQL_FN_STR_UCASE              65536
#define SQL_FN_STR_ASCII             131072
#define SQL_FN_STR_CHAR              262144
#define SQL_FN_STR_DIFFERENCE        524288
#define SQL_FN_STR_LOCATE_2         1048576
#define SQL_FN_STR_SOUNDEX          2097152
#define SQL_FN_STR_SPACE            4194304


#define SQL_FN_NUM_ABS                   16
#define SQL_FN_NUM_ACOS                  32
#define SQL_FN_NUM_ASIN                  64
#define SQL_FN_NUM_ATAN                 128
#define SQL_FN_NUM_ATAN2                256
#define SQL_FN_NUM_CEILING              512
#define SQL_FN_NUM_COS                 1024
#define SQL_FN_NUM_COT                 2048
#define SQL_FN_NUM_EXP                 4096
#define SQL_FN_NUM_FLOOR               8192
#define SQL_FN_NUM_LOG                16384
#define SQL_FN_NUM_MOD                32768
#define SQL_FN_NUM_SIGN               65536
#define SQL_FN_NUM_SIN               131072
#define SQL_FN_NUM_SQRT              262144
#define SQL_FN_NUM_TAN               524288
#define SQL_FN_NUM_PI               1048576
#define SQL_FN_NUM_RAND             2097152
#define SQL_FN_NUM_DEGREES          4194304
#define SQL_FN_NUM_LOG10            8388608
#define SQL_FN_NUM_POWER           16777216
#define SQL_FN_NUM_RADIANS         33554432
#define SQL_FN_NUM_ROUND           67108864
#define SQL_FN_NUM_TRUNCATE       134217728

#define SQL_FN_TD_NOW                    16
#define SQL_FN_TD_CURDATE                32
#define SQL_FN_TD_DAYOFMONTH             64
#define SQL_FN_TD_DAYOFWEEK             128
#define SQL_FN_TD_DAYOFYEAR             256
#define SQL_FN_TD_MONTH                 512
#define SQL_FN_TD_QUARTER              1024
#define SQL_FN_TD_WEEK                 2048
#define SQL_FN_TD_YEAR                 4096
#define SQL_FN_TD_CURTIME              8192
#define SQL_FN_TD_HOUR                16384
#define SQL_FN_TD_MINUTE              32768
#define SQL_FN_TD_SECOND              65536
#define SQL_FN_TD_TIMESTAMPADD       131072
#define SQL_FN_TD_TIMESTAMPDIFF      262144
#define SQL_FN_TD_DAYNAME            524288
#define SQL_FN_TD_MONTHNAME         1048576

#define SQL_FN_SYS_USERNAME             16
#define SQL_FN_SYS_DBNAME               32
#define SQL_FN_SYS_IFNULL               64

#define SQL_FN_TSI_FRAC_SECOND          16
#define SQL_FN_TSI_SECOND               32
#define SQL_FN_TSI_MINUTE               64
#define SQL_FN_TSI_HOUR                128
#define SQL_FN_TSI_DAY                 256
#define SQL_FN_TSI_WEEK                512
#define SQL_FN_TSI_MONTH              1024
#define SQL_FN_TSI_QUARTER            2048
#define SQL_FN_TSI_YEAR               4096

#define SQL_OAC_NONE                     0
#define SQL_OAC_LEVEL1                   1
#define SQL_OAC_LEVEL2                   2

#define SQL_OSCC_NOT_COMPLIANT           0
#define SQL_OSCC_COMPLIANT               1

#define SQL_OSC_MINIMUM                  0
#define SQL_OSC_CORE                     1
#define SQL_OSC_EXTENDED                 2

#define SQL_CB_NULL                      0
#define SQL_CB_NON_NULL                  1

#define SQL_CB_DELETE                    0
#define SQL_CB_CLOSE                     1
#define SQL_CB_PRESERVE                  2

#define SQL_IC_UPPER                     1
#define SQL_IC_LOWER                     2
#define SQL_IC_SENSITIVE                 3
#define SQL_IC_MIXED                     4

#define SQL_TC_NONE                      0
#define SQL_TC_DML                       1
#define SQL_TC_ALL                       2
#define SQL_TC_DDL_COMMIT                3
#define SQL_TC_DDL_IGNORE                4

#define SQL_SO_FORWARD_ONLY             16
#define SQL_SO_KEYSET_DRIVEN            32
#define SQL_SO_DYNAMIC                  64
#define SQL_SO_MIXED                   128
#define SQL_SO_STATIC                  256

#define SQL_SCCO_READ_ONLY              16
#define SQL_SCCO_LOCK                   32
#define SQL_SCCO_OPT_ROWVER             64
#define SQL_SCCO_OPT_VALUES            128

#define SQL_FD_FETCH_NEXT               16
#define SQL_FD_FETCH_FIRST              32
#define SQL_FD_FETCH_LAST               64
#define SQL_FD_FETCH_PRIOR             128
#define SQL_FD_FETCH_ABSOLUTE          256
#define SQL_FD_FETCH_RELATIVE          512
#define SQL_FD_FETCH_RESUME           1024
#define SQL_FD_FETCH_BOOKMARK         2048

#define SQL_TXN_READ_UNCOMMITTED        16
#define SQL_TXN_READ_COMMITTED          32
#define SQL_TXN_REPEATABLE_READ         64
#define SQL_TXN_SERIALIZABLE           128
#define SQL_TXN_VERSIONING             256

#define SQL_CN_NONE                      0
#define SQL_CN_DIFFERENT                 1
#define SQL_CN_ANY                       2

#define SQL_NNC_NULL                     0
#define SQL_NNC_NON_NULL                 1

#define SQL_NC_HIGH                      0
#define SQL_NC_LOW                       1
#define SQL_NC_START                     2
#define SQL_NC_END                       4

#define SQL_FILE_NOT_SUPPORTED           0
#define SQL_FILE_TABLE                   1
#define SQL_FILE_QUALIFIER               2

#define SQL_GD_ANY_COLUMN               16
#define SQL_GD_ANY_ORDER                32
#define SQL_GD_BLOCK                    64
#define SQL_GD_BOUND                   128

#define SQL_AT_ADD_COLUMN               16
#define SQL_AT_DROP_COLUMN              32

#define SQL_PS_POSITIONED_DELETE        16
#define SQL_PS_POSITIONED_UPDATE        32
#define SQL_PS_SELECT_FOR_UPDATE        64

#define SQL_GB_NOT_SUPPORTED             0
#define SQL_GB_GROUP_BY_EQUALS_SELECT    1
#define SQL_GB_GROUP_BY_CONTAINS_SELECT  2
#define SQL_GB_NO_RELATION               3

#define SQL_OU_DML_STATEMENTS           16
#define SQL_OU_PROCEDURE_INVOCATION     32
#define SQL_OU_TABLE_DEFINITION         64
#define SQL_OU_INDEX_DEFINITION        128
#define SQL_OU_PRIVILEGE_DEFINITION    256

#define SQL_QU_DML_STATEMENTS           16
#define SQL_QU_PROCEDURE_INVOCATION     32
#define SQL_QU_TABLE_DEFINITION         64
#define SQL_QU_INDEX_DEFINITION        128
#define SQL_QU_PRIVILEGE_DEFINITION    256

#define SQL_SQ_COMPARISON               16
#define SQL_SQ_EXISTS                   32
#define SQL_SQ_IN                       64
#define SQL_SQ_QUANTIFIED              128
#define SQL_SQ_CORRELATED_SUBQUERIES   256

#define SQL_U_UNION                     16
#define SQL_U_UNION_ALL                 32

#define SQL_BP_CLOSE                    16
#define SQL_BP_DELETE                   32
#define SQL_BP_DROP                     64
#define SQL_BP_TRANSACTION             128
#define SQL_BP_UPDATE                  256
#define SQL_BP_OTHER_HSTMT             512
#define SQL_BP_SCROLL                 1024

#define SQL_SS_ADDITIONS                16
#define SQL_SS_DELETIONS                32
#define SQL_SS_UPDATES                  64

#define SQL_LCK_NO_CHANGE               16
#define SQL_LCK_EXCLUSIVE               32
#define SQL_LCK_UNLOCK                  64

#define SQL_POS_POSITION                16
#define SQL_POS_REFRESH                 32
#define SQL_POS_UPDATE                  64
#define SQL_POS_DELETE                 128
#define SQL_POS_ADD                    256

#define SQL_QL_START                    16
#define SQL_QL_END                      32

#define SQL_OJ_LEFT                     16
#define SQL_OJ_RIGHT                    32
#define SQL_OJ_FULL                     64
#define SQL_OJ_NESTED                  128
#define SQL_OJ_NOT_ORDERED             256
#define SQL_OJ_INNER                   512
#define SQL_OJ_ALL_COMPARISON_OPS     1024

#define SQL_QUERY_TIMEOUT                0
#define SQL_MAX_ROWS                     1
#define SQL_NOSCAN                       2
#define SQL_MAX_LENGTH                   3
#define SQL_ASYNC_ENABLE                 4
#define SQL_BIND_TYPE                    5
#define SQL_CURSOR_TYPE                  6
#define SQL_CONCURRENCY                  7
#define SQL_KEYSET_SIZE                  8
#define SQL_ROWSET_SIZE                  9
#define SQL_SIMULATE_CURSOR             10
#define SQL_RETRIEVE_DATA               11
#define SQL_USE_BOOKMARKS               12
#define SQL_GET_BOOKMARK                13
#define SQL_ROW_NUMBER                  14
#define SQL_STMT_OPT_MAX                  SQL_ROW_NUMBER
#define SQL_STMT_OPT_MIN                  SQL_QUERY_TIMEOUT

#define SQL_QUERY_TIMEOUT_DEFAULT        0

#define SQL_MAX_ROWS_DEFAULT             0

#define SQL_NOSCAN_OFF                   0
#define SQL_NOSCAN_ON                    1
#define SQL_NOSCAN_DEFAULT                SQL_NOSCAN_OFF

#define SQL_MAX_LENGTH_DEFAULT           0

#define SQL_ASYNC_ENABLE_OFF             0
#define SQL_ASYNC_ENABLE_ON              1
#define SQL_ASYNC_ENABLE_DEFAULT          SQL_ASYNC_ENABLE_OFF

#define SQL_BIND_BY_COLUMN               0

#define SQL_CONCUR_READ_ONLY             1
#define SQL_CONCUR_LOCK                  2
#define SQL_CONCUR_ROWVER                3
#define SQL_CONCUR_VALUES                4

#define SQL_CURSOR_FORWARD_ONLY          0
#define SQL_CURSOR_KEYSET_DRIVEN         1
#define SQL_CURSOR_DYNAMIC               2
#define SQL_CURSOR_STATIC                3

#define SQL_ROWSET_SIZE_DEFAULT          1

#define SQL_KEYSET_SIZE_DEFAULT          0

#define SQL_SC_NON_UNIQUE                0
#define SQL_SC_TRY_UNIQUE                1
#define SQL_SC_UNIQUE                    2

#define SQL_RD_OFF                       0
#define SQL_RD_ON                        1
#define SQL_RD_DEFAULT                    SQL_RD_ON

#define SQL_UB_OFF                       0
#define SQL_UB_ON                        1
#define SQL_UB_DEFAULT                    SQL_UB_OFF

#define SQL_ACCESS_MODE                101
#define SQL_AUTOCOMMIT                 102
#define SQL_LOGIN_TIMEOUT              103
#define SQL_OPT_TRACE                  104
#define SQL_OPT_TRACEFILE              105
#define SQL_TRANSLATE_DLL              106
#define SQL_TRANSLATE_OPTION           107
#define SQL_TXN_ISOLATION              108
#define SQL_CURRENT_QUALIFIER          109
#define SQL_ODBC_CURSORS               110
#define SQL_QUIET_MODE                 111
#define SQL_PACKET_SIZE                112
#define SQL_CONN_OPT_MAX                  SQL_PACKET_SIZE
#define SQL_CONNECT_OPT_DRVR_START    1000
#define SQL_CONN_OPT_MIN                  SQL_ACCESS_MODE

#define SQL_MODE_READ_WRITE              0
#define SQL_MODE_READ_ONLY               1
#define SQL_MODE_DEFAULT                  SQL_MODE_READ_WRITE

#define SQL_AUTOCOMMIT_OFF               0
#define SQL_AUTOCOMMIT_ON                1
#define SQL_AUTOCOMMIT_DEFAULT            SQL_AUTOCOMMIT_ON

#define SQL_LOGIN_TIMEOUT_DEFAULT       15

#define SQL_OPT_TRACE_OFF                0
#define SQL_OPT_TRACE_ON                 1
#define SQL_OPT_TRACE_DEFAULT             SQL_OPT_TRACE_OFF
#define SQL_OPT_TRACE_FILE_DEFAULT        "\\SQL.LOG"

#define SQL_CUR_USE_IF_NEEDED            0
#define SQL_CUR_USE_ODBC                 1
#define SQL_CUR_USE_DRIVER               2
#define SQL_CUR_DEFAULT                   SQL_CUR_USE_DRIVER

#define SQL_BEST_ROWID                   1
#define SQL_ROWVER                       2
#define SQL_SCOPE_CURROW                 0
#define SQL_SCOPE_TRANSACTION            1
#define SQL_SCOPE_SESSION                2

#define SQL_ENTIRE_ROWSET                0

#define SQL_POSITION                     0
#define SQL_REFRESH                      1
#define SQL_UPDATE                       2
#define SQL_DELETE                       3
#define SQL_ADD                          4

#define SQL_LOCK_NO_CHANGE               0
#define SQL_LOCK_EXCLUSIVE               1
#define SQL_LOCK_UNLOCK                  2

#define SQL_FETCH_NEXT                   1
#define SQL_FETCH_FIRST                  2
#define SQL_FETCH_LAST                   3
#define SQL_FETCH_PRIOR                  4
#define SQL_FETCH_ABSOLUTE               5
#define SQL_FETCH_RELATIVE               6
#define SQL_FETCH_BOOKMARK               8

#define SQL_ROW_SUCCESS                  0
#define SQL_ROW_DELETED                  1
#define SQL_ROW_UPDATED                  2
#define SQL_ROW_NOROW                    3
#define SQL_ROW_ADDED                    4
#define SQL_ROW_ERROR                    5

#define SQL_CASCADE                      0
#define SQL_RESTRICT                     1
#define SQL_SET_NULL                     2

#define SQL_PARAM_TYPE_UNKNOWN           0
#define SQL_PARAM_INPUT                  1
#define SQL_PARAM_INPUT_OUTPUT           2
#define SQL_RESULT_COL                   3
#define SQL_PARAM_OUTPUT                 4
#define SQL_RETURN_VALUE                 5

#define SQL_PARAM_TYPE_DEFAULT            SQL_PARAM_INPUT_OUTPUT
#define SQL_SETPARAM_VALUE_MAX         (-1)

#define SQL_INDEX_UNIQUE                 0
#define SQL_INDEX_ALL                    1
#define SQL_QUICK                        0
#define SQL_ENSURE                       1

#define SQL_TABLE_STAT                   0
#define SQL_INDEX_CLUSTERED              1
#define SQL_INDEX_HASHED                 2
#define SQL_INDEX_OTHER                  3

#define SQL_PT_UNKNOWN                   0
#define SQL_PT_PROCEDURE                 1
#define SQL_PT_FUNCTION                  2

#define SQL_PC_UNKNOWN                   0
#define SQL_PC_NOT_PSEUDO                1
#define SQL_PC_PSEUDO                    2

#define SQL_DATABASE_NAME               16
#define SQL_FD_FETCH_PREV                 SQL_FD_FETCH_PRIOR
#define SQL_FETCH_PREV                    SQL_FETCH_PRIOR
#define SQL_CONCUR_TIMESTAMP              SQL_CONCUR_ROWVER
#define SQL_SCCO_OPT_TIMESTAMP            SQL_SCCO_OPT_ROWVER
#define SQL_CC_DELETE                     SQL_CB_DELETE
#define SQL_CR_DELETE                     SQL_CB_DELETE
#define SQL_CC_CLOSE                      SQL_CB_CLOSE
#define SQL_CR_CLOSE                      SQL_CB_CLOSE
#define SQL_CC_PRESERVE                   SQL_CB_PRESERVE
#define SQL_CR_PRESERVE                   SQL_CB_PRESERVE
#define SQL_FETCH_RESUME                 7
#define SQL_SCROLL_FORWARD_ONLY          0
#define SQL_SCROLL_KEYSET_DRIVEN       (-1)
#define SQL_SCROLL_DYNAMIC             (-2)
#define SQL_SCROLL_STATIC              (-3)
#define SQL_PC_NON_PSEUDO                 SQL_PC_NOT_PSEUDO

#define ODBC_ADD_DSN                     1
#define ODBC_CONFIG_DSN                  2
#define ODBC_REMOVE_DSN                  3

*** End of ODBCAPI.H ***