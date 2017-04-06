Define Class _hyperlinkcommandbutton as _commandbutton
AutoSize = .T.
FontUnderline = .T.
MousePointer = 99
MouseIcon = "graphics\h_point.cur"
ForeColor = Rgb(0,0,255)
ctarget = ""
clocation = ""
cframe = ""
nvisitedforecolor = 8388736
ohyperlink = .NULL.
chyperlinkclass = "_HyperLinkBase"
chyperlinkclasslibrary = "_HyperLink.vcx"
lformsynch = .T.



PROCEDURE follow
this.oHyperLink.NavigateTo(this.cTarget,this.cLocation,this.cFrame)
this.lVisited=.T.

ENDPROC
PROCEDURE goback
RETURN this.oHyperLink.GoBack()

ENDPROC
PROCEDURE goforward
RETURN this.oHyperLink.GoForward()

ENDPROC
PROCEDURE lvisited_assign
LPARAMETERS m.vNewVal

IF this.lVisited=m.vNewVal
	RETURN
ENDIF
this.lVisited=m.vNewVal
IF this.lVisited
	this.ShowVisitedForeColor
ENDIF

ENDPROC
PROCEDURE nvisitedforecolor_assign
LPARAMETERS m.vNewVal

IF this.nVisitedForeColor=m.vNewVal
	RETURN
ENDIF
this.nVisitedForeColor=m.vNewVal
IF this.lVisited
	this.ShowVisitedForeColor
ENDIF

ENDPROC
PROCEDURE showvisitedforecolor
this.ForeColor=this.nVisitedForeColor

ENDPROC
PROCEDURE ohyperlink_access
IF this.lFormSynch AND TYPE("thisform")=="O" AND NOT ISNULL(thisform)
	IF TYPE("thisform.oHyperLinkFormSynch")=="O" AND ;
			NOT ISNULL(thisform.oHyperLinkFormSynch)
		this.oHyperLink=thisform.oHyperLinkFormSynch
	ELSE
		this.SetObjectRef("oHyperLink",this.cHyperLinkClass,this.cHyperLinkClassLibrary)
		thisform.AddProperty("oHyperLinkFormSynch",this.oHyperLink)
	ENDIF
ELSE
	this.SetObjectRef("oHyperLink",this.cHyperLinkClass,this.cHyperLinkClassLibrary)
ENDIF
this.oHyperLink.lNewWindow=this.lNewWindow
IF NOT this.lHyperLinkCreated
	this.lHyperLinkCreated=.T.
	this.cTarget=this.cTarget
ENDIF
RETURN this.oHyperLink

ENDPROC
PROCEDURE ctarget_assign
LPARAMETERS m.vNewVal

IF this.lHyperLinkCreated
	this.cTarget=this.oHyperLink.ValidURL(m.vNewVal)
ELSE
	this.cTarget=ALLTRIM(m.vNewVal)
ENDIF
this.ToolTipText=this.cTarget

ENDPROC
PROCEDURE Destroy
IF NOT DODEFAULT()
	RETURN .F.
ENDIF
this.oHyperLink=.NULL.

ENDPROC
PROCEDURE Click
this.Follow

ENDPROC
PROCEDURE MouseMove
LPARAMETERS nButton, nShift, nXCoord, nYCoord

IF NOT this.lHyperLinkCreated
	=this.oHyperLink
ENDIF

ENDPROC


EndDefine 
Define Class _hyperlinklabel as _label
AutoSize = .T.
FontUnderline = .T.
MousePointer = 99
MouseIcon = "graphics\h_point.cur"
ForeColor = Rgb(0,0,255)
ctarget = ""
clocation = ""
cframe = ""
nvisitedforecolor = 8388736
lformsynch = .T.
ohyperlink = .NULL.
chyperlinkclass = "_HyperLinkBase"
chyperlinkclasslibrary = "_HyperLink.vcx"



PROCEDURE follow
this.oHyperLink.lNewWindow=this.lNewWindow
this.oHyperLink.NavigateTo(this.cTarget,this.cLocation,this.cFrame)
this.lVisited=.T.

ENDPROC
PROCEDURE goback
RETURN this.oHyperLink.GoBack()

ENDPROC
PROCEDURE goforward
RETURN this.oHyperLink.GoForward()

ENDPROC
PROCEDURE lvisited_assign
LPARAMETERS m.vNewVal

IF this.lVisited=m.vNewVal
	RETURN
ENDIF
this.lVisited=m.vNewVal
IF this.lVisited
	this.ShowVisitedForeColor
ENDIF

ENDPROC
PROCEDURE nvisitedforecolor_assign
LPARAMETERS m.vNewVal

IF this.nVisitedForeColor=m.vNewVal
	RETURN
ENDIF
this.nVisitedForeColor=m.vNewVal
IF this.lVisited
	this.ShowVisitedForeColor
ENDIF

ENDPROC
PROCEDURE showvisitedforecolor
this.ForeColor=this.nVisitedForeColor

ENDPROC
PROCEDURE ohyperlink_access
IF this.lFormSynch AND TYPE("thisform")=="O" AND NOT ISNULL(thisform)
	IF TYPE("thisform.oHyperLinkFormSynch")=="O" AND ;
			NOT ISNULL(thisform.oHyperLinkFormSynch)
		this.oHyperLink=thisform.oHyperLinkFormSynch
	ELSE
		this.SetObjectRef("oHyperLink",this.cHyperLinkClass,this.cHyperLinkClassLibrary)
		thisform.AddProperty("oHyperLinkFormSynch",this.oHyperLink)
	ENDIF
ELSE
	this.SetObjectRef("oHyperLink",this.cHyperLinkClass,this.cHyperLinkClassLibrary)
ENDIF
this.oHyperLink.lNewWindow=this.lNewWindow
IF NOT this.lHyperLinkCreated
	this.lHyperLinkCreated=.T.
	this.cTarget=this.cTarget
ENDIF
RETURN this.oHyperLink

ENDPROC
PROCEDURE ctarget_assign
LPARAMETERS m.vNewVal

IF this.lHyperLinkCreated
	this.cTarget=this.oHyperLink.ValidURL(m.vNewVal)
ELSE
	this.cTarget=ALLTRIM(m.vNewVal)
ENDIF
this.ToolTipText=this.cTarget

ENDPROC
PROCEDURE Destroy
IF NOT DODEFAULT()
	RETURN .F.
ENDIF
this.oHyperLink=.NULL.

ENDPROC
PROCEDURE Click
this.Follow

ENDPROC
PROCEDURE MouseMove
LPARAMETERS nButton, nShift, nXCoord, nYCoord

IF NOT this.lHyperLinkCreated
	=this.oHyperLink
ENDIF

ENDPROC


EndDefine 
Define Class _hyperlinkbase as _hyperlink
Height = 27
Width = 25
clocationurl = ""
oie = .NULL.
ctarget = ""
clocation = ""
cframe = ""
cieclass = "InternetExplorer.Application"
cshellexecuteclass = "_ShellExecute"
cshellexecuteclasslibrary = "_Environ.vcx"



PROCEDURE clocationurl_access
IF TYPE("this.oIE.LocationURL")=="C"
	RETURN this.oIE.LocationURL
ENDIF
RETURN this.cLocationURL

ENDPROC
PROCEDURE clocationurl_assign
LPARAMETERS m.vNewVal

this.cTarget=m.vNewVal
this.NavigateTo

ENDPROC
PROCEDURE follow
RETURN this.NavigateTo()

ENDPROC
PROCEDURE validurl
LPARAMETERS tcURL
LOCAL lcURL

IF EMPTY(tcURL)
	RETURN ""
ENDIF
lcURL=ALLTRIM(tcURL)
IF NOT LOWER(LEFT(lcURL,5))=="http:" AND NOT LOWER(LEFT(lcURL,5))=="file:" AND ;
		(LOWER(LEFT(lcURL,4))=="www." OR ;
		INLIST(LOWER(RIGHT(lcURL,4)),".com",".gov",".net") OR ;
		(NOT SUBSTR(lcURL,2,1)==":" AND NOT LEFT(lcURL,2)=="\\"))
	lcURL="http://"+lcURL
ENDIF
IF SUBSTR(PADR(lcURL,5),5,1)==":"
	lcURL=STRTRAN(STRTRAN(lcURl,"\","/"),"///","//")
ELSE
	lcURL="file://"+STRTRAN(STRTRAN(STRTRAN(lcURL,"\","/"),"///","//"),"//","/")
ENDIF
RETURN lcURL

ENDPROC
PROCEDURE getdefaultbrowser
#DEFINE HKEY_CLASSES_ROOT		-2147483648
#DEFINE	HTTP_PATH				"HTTP\shell\open\ddeexec\Application"
#DEFINE ERROR_SUCCESS			0
#DEFINE REG_SZ 					1

LOCAL nHKey,cSubKey,nResult,lpszValueName,dwReserved
LOCAL lpdwType,lpbData,lpcbData,lnErrCode,lnGetKey

DECLARE Integer RegOpenKey IN Win32API ;
	Integer nHKey, String @cSubKey, Integer @nResult
DECLARE Integer RegQueryValueEx IN Win32API ;
	Integer nHKey, String lpszValueName, Integer dwReserved,;
	Integer @lpdwType, String @lpbData, Integer @lpcbData
DECLARE Integer RegCloseKey IN Win32API ;
	Integer nHKey
lnGetKey=0
lnErrCode=RegOpenKey(HKEY_CLASSES_ROOT,HTTP_PATH,@lnGetKey)
IF lnErrCode#ERROR_SUCCESS
	RETURN ""
ENDIF
lpdwType=0
lpbData=SPACE(256)
lpcbData=LEN(lpbData)
lnErrCode=RegQueryValueEx(lnGetKey,"",0,@lpdwType,@lpbData,@lpcbData)
IF lnErrCode#ERROR_SUCCESS OR lpdwType#REG_SZ
	RegCloseKey(nGetKey)
	RETURN ""
ENDIF
RegCloseKey(lnGetKey)
RETURN ALLTRIM(LEFT(lpbData,lpcbData-1))

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine

IF nError=1733 AND LOWER(cMethod)=="navigateto"
	RETURN
ENDIF
RETURN DODEFAULT(nError,cMethod,nLine)

ENDPROC
PROCEDURE NavigateTo
LPARAMETERS cTarget, cLocation, cFrame
LOCAL lcTarget,lcLocation,lcFrame,oShellExecute

lcTarget=ALLTRIM(IIF(EMPTY(cTarget),this.cTarget,cTarget))
lcLocation=ALLTRIM(IIF(EMPTY(cLocation),this.cLocation,cLocation))
lcFrame=ALLTRIM(IIF(EMPTY(cFrame),this.cFrame,cFrame))
NODEFAULT
IF EMPTY(lcTarget)
	RETURN .F.
ENDIF
IF (this.lNewWindow OR TYPE("this.oIE.LocationURL")#"C") AND LOWER(this.GetDefaultBrowser())=="iexplore"
	this.oIE=CREATEOBJECT(this.cIEClass)
ENDIF
IF TYPE("this.oIE.LocationURL")#"C"
	this.SetObjectRef("oShellExecute",this.cShellExecuteClass,this.cShellExecuteClassLibrary)
	IF VARTYPE(this.oShellExecute)=="O"
		RETURN this.oShellExecute.ShellExecute(lcTarget)
	ENDIF
	RETURN DODEFAULT(lcTarget,lcLocation,lcFrame)
ENDIF
IF EMPTY(this.oIE.LocationURL)
	lcFrame=""
ENDIF
this.oIE.Navigate2(lcTarget,0,lcFrame)
this.oIE.Visible=.T.

ENDPROC
PROCEDURE GoBack
IF TYPE("this.oIE.LocationURL")=="C"
	NODEFAULT
	this.oIE.GoBack
	RETURN
ENDIF

ENDPROC
PROCEDURE GoForward
IF TYPE("this.oIE.LocationURL")=="C"
	NODEFAULT
	this.oIE.GoForward
	RETURN
ENDIF

ENDPROC


EndDefine 
Define Class _hyperlinkimage as _image
MousePointer = 99
MouseIcon = "graphics\h_point.cur"
ctarget = ""
clocation = ""
cframe = ""
ohyperlink = .NULL.
chyperlinkclass = "_HyperLinkBase"
chyperlinkclasslibrary = "_HyperLink.vcx"
lformsynch = .T.



PROCEDURE follow
this.oHyperLink.NavigateTo(this.cTarget,this.cLocation,this.cFrame)
this.lVisited=.T.

ENDPROC
PROCEDURE goback
RETURN this.oHyperLink.GoBack()

ENDPROC
PROCEDURE goforward
RETURN this.oHyperLink.GoForward()

ENDPROC
PROCEDURE ohyperlink_access
IF this.lFormSynch AND TYPE("thisform")=="O" AND NOT ISNULL(thisform)
	IF TYPE("thisform.oHyperLinkFormSynch")=="O" AND ;
			NOT ISNULL(thisform.oHyperLinkFormSynch)
		this.oHyperLink=thisform.oHyperLinkFormSynch
	ELSE
		this.SetObjectRef("oHyperLink",this.cHyperLinkClass,this.cHyperLinkClassLibrary)
		thisform.AddProperty("oHyperLinkFormSynch",this.oHyperLink)
	ENDIF
ELSE
	this.SetObjectRef("oHyperLink",this.cHyperLinkClass,this.cHyperLinkClassLibrary)
ENDIF
this.oHyperLink.lNewWindow=this.lNewWindow
IF NOT this.lHyperLinkCreated
	this.lHyperLinkCreated=.T.
	this.cTarget=this.cTarget
ENDIF
RETURN this.oHyperLink

ENDPROC
PROCEDURE ctarget_assign
LPARAMETERS m.vNewVal

IF this.lHyperLinkCreated
	this.cTarget=this.oHyperLink.ValidURL(m.vNewVal)
ELSE
	this.cTarget=ALLTRIM(m.vNewVal)
ENDIF
this.ToolTipText=this.cTarget

ENDPROC
PROCEDURE Click
this.Follow

ENDPROC
PROCEDURE Destroy
IF NOT DODEFAULT()
	RETURN .F.
ENDIF
this.oHyperLink=.NULL.

ENDPROC
PROCEDURE MouseMove
LPARAMETERS nButton, nShift, nXCoord, nYCoord

IF NOT this.lHyperLinkCreated
	=this.oHyperLink
ENDIF

ENDPROC


EndDefine 
