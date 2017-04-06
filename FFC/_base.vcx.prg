Define Class _checkbox as checkbox
Height = 17
Width = 60
Caption = "Check1"
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _combobox as combobox
Height = 24
Width = 100
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _commandbutton as commandbutton
Height = 27
Width = 84
Caption = "Command1"
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _commandgroup as commandgroup
ButtonCount = 2
Value = 1
Height = 66
Width = 94
Command1.Top = 5
Command1.Left = 5
Command1.Height = 27
Command1.Width = 84
COMMAND1.Caption = "Command1"
Command2.Top = 34
Command2.Left = 5
Command2.Height = 27
Command2.Width = 84
Comm
AND2.Caption = "Command2"
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _container as container
Width = 200
Height = 200
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _editbox as editbox
Height = 53
Width = 100
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _grid as grid
Height = 200
Width = 320
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _line as line
Height = 68
Width = 68
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _listbox as listbox
Height = 170
Width = 100
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _optionbutton as optionbutton
Caption = "Option1"
Height = 17
Width = 61
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _optiongroup as optiongroup
ButtonCount = 2
Value = 1
Height = 46
Width = 71
OPTION1.Caption = "Option1"
OPTION1.Value = 1
Option1.Height = 17
Option1.Left = 5
Option1.Top = 5
Option1.Width = 61
OPTION2.Caption = "Option2"
Option2.Height = 17
Option2.Left = 5
Option2.Top = 24
Option2.Width = 61
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _pageframe as pageframe
ErasePage = .T.
PageCount = 2
Width = 241
Height = 169
PAGE1.Caption = "Page1"
PAGE2.Caption = "Page2"
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _separator as separator
Height = 0
Width = 0
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _shape as shape
Height = 68
Width = 68
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _spinner as spinner
Height = 24
Width = 120
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _textbox as textbox
Height = 23
Width = 100
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _timer as timer
Height = 23
Width = 23
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _formset as formset
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")

ADD OBJECT Form1 as form WITH Caption = "Form1", cversion = "", builder = "", builderx = (HOME()+"Wizards\BuilderD,BuilderDForm"), nobjectrefcount = 0, ninstances = 0, ohost = .NULL., vresult = .T., csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")


PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


PROCEDURE Form1.Release
RELEASE this

ENDPROC


EndDefine 
Define Class _activedoc as activedoc
Height = 68
Width = 68
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _hyperlink as hyperlink
Height = 23
Width = 23
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _label as label
Caption = "Label1"
Height = 16
Width = 40
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _projecthook as projecthook
Height = 68
Width = 68
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _image as image
Height = 68
Width = 68
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _control as control
Height = 22
Width = 24
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _form as form
ShowWindow = 1
Caption = "Form1"
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _toolbar as toolbar
Caption = "Toolbar1"
ShowWindow = 1
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ninstances = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
Define Class _custom as custom
Height = 22
Width = 24
cversion = ""
builder = ""
builderx = (HOME()+"Wizards\BuilderD,BuilderDForm")
nobjectrefcount = 0
ohost = .NULL.
vresult = .T.
csetobjrefprogram = (IIF(VERSION(2)=0,"",HOME()+"FFC\")+"SetObjRf.prg")
ninstances = 0



PROCEDURE release
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF

ENDPROC
PROCEDURE setobjectref
LPARAMETERS tcName,tvClass,tvClassLibrary
LOCAL lvResult

this.vResult=.T.
DO (this.cSetObjRefProgram) WITH (this),(tcName),(tvClass),(tvClassLibrary)
lvResult=this.vResult
this.vResult=.T.
RETURN lvResult

ENDPROC
PROCEDURE setobjectrefs
LPARAMETERS toObject

RETURN

ENDPROC
PROCEDURE releaseobjrefs
LOCAL lcName,oObject,lnCount

IF this.nObjectRefCount=0
	RETURN
ENDIF
FOR lnCount = this.nObjectRefCount TO 1 STEP -1
	lcName=this.aObjectRefs[lnCount,1]
	IF EMPTY(lcName) OR NOT PEMSTATUS(this,lcName,5) OR TYPE("this."+lcName)#"O"
		LOOP
	ENDIF
	oObject=this.&lcName
	IF ISNULL(oObject)
		LOOP
	ENDIF
	IF TYPE("oObject")=="O" AND NOT ISNULL(oObject) AND PEMSTATUS(oObject,"Release",5)
		oObject.Release
	ENDIF
	IF NOT ISNULL(oObject) AND PEMSTATUS(oObject,"oHost",5)
		oObject.oHost=.NULL.
	ENDIF
	this.&lcName=.NULL.
	oObject=.NULL.
ENDFOR
DIMENSION this.aObjectRefs[1,3]
this.aObjectRefs=""

ENDPROC
PROCEDURE nobjectrefcount_access
LOCAL lnObjectRefCount

lnObjectRefCount=ALEN(this.aObjectRefs,1)
IF lnObjectRefCount=1 AND EMPTY(this.aObjectRefs[1])
	lnObjectRefCount=0
ENDIF
RETURN lnObjectRefCount

ENDPROC
PROCEDURE nobjectrefcount_assign
LPARAMETERS m.vNewVal

ERROR 1743

ENDPROC
PROCEDURE sethost
this.oHost=IIF(TYPE("thisform")=="O",thisform,.NULL.)

ENDPROC
PROCEDURE newinstance
LPARAMETERS tnDataSessionID
LOCAL oNewObject,lnLastDataSessionID

lnLastDataSessionID=SET("DATASESSION")
IF TYPE("tnDataSessionID")=="N" AND tnDataSessionID>=1
	SET DATASESSION TO tnDataSessionID
ENDIF
oNewObject=NEWOBJECT(this.Class,this.ClassLibrary)
SET DATASESSION TO (lnLastDataSessionID)
RETURN oNewObject

ENDPROC
PROCEDURE addtoproject
*-- Dummy code for adding files to project.
RETURN

DO SetObjRf.prg

ENDPROC
PROCEDURE ninstances_access
LOCAL laInstances[1]
	
RETURN AINSTANCE(laInstances,this.Class)

ENDPROC
PROCEDURE ninstances_assign
LPARAMETERS vNewVal

ERROR 1743

ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
LOCAL lcOnError,lcErrorMsg,lcCodeLineMsg

IF this.lIgnoreErrors
	RETURN .F.
ENDIF
lcOnError=UPPER(ALLTRIM(ON("ERROR")))
IF NOT EMPTY(lcOnError)
	lcOnError=STRTRAN(STRTRAN(STRTRAN(lcOnError,"ERROR()","nError"), ;
			"PROGRAM()","cMethod"),"LINENO()","nLine")
	&lcOnError
	RETURN
ENDIF
lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)+this.Name+CHR(13)+ ;
		"Error:           "+ALLTRIM(STR(nError))+CHR(13)+ ;
		"Method:       "+LOWER(ALLTRIM(cMethod))
lcCodeLineMsg=MESSAGE(1)
IF BETWEEN(nLine,1,100000) AND NOT lcCodeLineMsg="..."
	lcErrorMsg=lcErrorMsg+CHR(13)+"Line:            "+ALLTRIM(STR(nLine))
	IF NOT EMPTY(lcCodeLineMsg)
		lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
	ENDIF
ENDIF
WAIT CLEAR
MESSAGEBOX(lcErrorMsg,16,_screen.Caption)
ERROR nError

ENDPROC
PROCEDURE Init
IF this.lSetHost
	this.SetHost
ENDIF
IF this.lAutoSetObjectRefs AND NOT this.SetObjectRefs(this)
	RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
IF this.lRelease
	RETURN .F.
ENDIF
this.lRelease=.T.
this.ReleaseObjRefs
this.oHost=.NULL.

ENDPROC


EndDefine 
