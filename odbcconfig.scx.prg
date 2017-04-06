Public CONNECTIONS_45T0S51QT

CONNECTIONS_45T0S51QT = CreateObject("CONNECTIONS" )
If CONNECTIONS_45T0S51QT.WindowType == 1
CONNECTIONS_45T0S51QT.Show(1)
else
CONNECTIONS_45T0S51QT.Show()
EndIf

#INCLUDE "odbcapi.h"

Define Class CONNECTIONS as form
DataSession = 1
Height = 228
Width = 300
AutoCenter = .T.
Picture = ""
BorderStyle = 2
Caption = "Seleccionar Conexión o DataSource"
MaxButton = .F.
MinButton = .F.
WindowType = 1
LockScreen = .F.
BackColor = Rgb(192,192,192)
creturn = ""
nmove = 0

ADD OBJECT opgSelect as optiongroup WITH ButtonCount = 2, BackStyle = 0, Value = 2, Height = 48, Left = 11, Top = 172, Width = 173, Option1.FontName = "MS Sans Serif", Option1.FontSize = 8, Option1.BackStyle = 0, Option1.Caption = "\<Conexiones", Option1.Value = 0, Option1.Enabled = .F., Option1.Height = 15, Option1.Left = 6, Option1.Top = 6, Option1.Width = 73, Option1.AutoSize = .T., Option2.FontName = "MS Sans Serif", Option2.FontSize = 8, Option2.BackStyle = 0, Option2.Caption = "Datasources \<Disponibles", Option2.Value = 1, Option2.Height = 15, Option2.Left = 6, Option2.Top = 24, Option2.Width = 135, Option2.AutoSize = .T.
ADD OBJECT lblAvailable as label WITH AutoSize = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Datasources Disponibles", Height = 15, Left = 11, Top = 13, Width = 119
ADD OBJECT lstDatasources as listbox WITH FontName = "MS Sans Serif", FontSize = 8, RowSourceType = 5, RowSource = "ThisForm.aDatasources", Value = 1, Height = 125, Left = 11, Top = 29, Width = 173
ADD OBJECT cmdNew as commandbutton WITH Top = 30, Left = 198, Height = 23, Width = 84, FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Nuevo"
ADD OBJECT cmdOk as commandbutton WITH Top = 96, Left = 198, Height = 23, Width = 84, FontName = "MS Sans Serif", FontSize = 8, Caption = "OK", Default = .T.
ADD OBJECT cmdCancel as commandbutton WITH Top = 129, Left = 198, Height = 23, Width = 84, FontName = "MS Sans Serif", FontSize = 8, Caption = "Cancel"
ADD OBJECT lblSelect as label WITH AutoSize = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Select", Height = 15, Left = 11, Top = 159, Width = 32
ADD OBJECT lblSample2 as label WITH AutoSize = .T., FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = " ODBC API", Height = 15, Left = 190, Top = 188, Width = 55, ForeColor = Rgb(255,255,255)
ADD OBJECT cmdSetup as commandbutton WITH Top = 63, Left = 198, Height = 23, Width = 84, FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Setup"


PROCEDURE availabledsn
*** Initialitation variables
cDSN = REPLICATE( chr(0), SQL_MAX_DSN_LENGTH )
nDSNSize = 0
cDescription = REPLICATE( chr(0), SQL_MAX_DSN_LENGTH )
nDescriptionSize = 0
nCont = 0

*** ODBC Environment Handle
nHenv = VAL( SYS(3053) )

*** ODBC API for get Datasources
DO WHILE INLIST( SQLDataSources( nHenv, ;
                                 SQL_FETCH_NEXT, ;
                                 @cDSN, ;
                                 LEN( cDSN ), ;
                                 @nDSNSize, ;
                                 @cDescription, ;
                                 LEN( cDescription ), ;
                                 @nDescriptionSize ), ;
                 SQL_SUCCESS, SQL_SUCCESS_WITH_INFO )

    nCont = nCont + 1
    DIMENSION This.aDatasources[nCont,2]
    This.aDatasources[nCont,1] = SUBSTR( cDSN, 1, nDSNSize )
    This.aDatasources[nCont,2] = SUBSTR( cDescription, 1, nDescriptionSize )
ENDDO

This.lstDatasources.NumberOfElements = nCont

ENDPROC
PROCEDURE Unload
*** Return the DataSource
RETURN This.cReturn
ENDPROC
PROCEDURE Init
*** ODBC API Declarations
*DO odbcapi

*** WIN32 Declaration
DECLARE INTEGER FindWindow IN WIN32API STRING, STRING

*** Get Availables DataSource Name
This.AvailableDSN()
ENDPROC


PROCEDURE lstDatasources.DblClick
*** Store the Datasource Name for Return
ThisForm.cReturn = ThisForm.aDatasources[ThisForm.lstDatasources.Value,1]
ThisForm.Release()
ENDPROC


PROCEDURE cmdNew.Click
*** Handle of VFP Window
nHwnd = FindWindow( 0, _SCREEN.caption )

*** ODBC API for New Datasource
=SQLCreateDataSource( nHwnd, 0 )

*** Get Availables DataSource Name
ThisForm.AvailableDSN()

ENDPROC


PROCEDURE cmdOk.Click
*** Store the datasource name for return
ThisForm.cReturn = ThisForm.aDatasources[ThisForm.lstDatasources.Value,1]
ThisForm.Release()
ENDPROC


PROCEDURE cmdCancel.Click
*** Store '' for return
ThisForm.cReturn = ''
ThisForm.Release()
ENDPROC


PROCEDURE cmdSetup.Click
*** Handle of VFP Window
nHwnd = FindWindow( 0, _SCREEN.Caption )

*** ODBC API for New Datasource
=SQLConfigDataSource( nHwnd, 2, ;
                      ThisForm.aDatasources[ThisForm.lstDatasources.Value, 2], ;
                      "DSN=" + ThisForm.aDatasources[ThisForm.lstDatasources.Value, 1] )

ENDPROC


EndDefine 
