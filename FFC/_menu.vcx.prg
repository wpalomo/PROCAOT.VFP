Define Class _shortcutmenu as _custom
Height = 22
Width = 24
conselection = ""
cmenu = ""



PROCEDURE deactivatemenu
IF EMPTY(this.cMenu)
	RETURN
ENDIF
DEACTIVATE POPUP (this.cMenu)
this.cMenu=""
DOEVENTS

ENDPROC
PROCEDURE activatemenu
LPARAMETERS tcParentMenu
LOCAL lnArrayColumns,llMultiArray,lnBar,lnSkipCount,lnCount,lnMRow,lnMCol
LOCAL lnMenuCount,lcMenu,lcMenu2,lcMenuItem,luMenuSelection,llSetMark,lcClauses

lnMRow=MAX(MROW(),0)
lnMCol=MAX(MCOL(),0)
IF TYPE("this.aMenu")#"C"
	RETURN .F.
ENDIF
lnMenuCount=ALEN(this.aMenu,1)
IF lnMenuCount=0
	RETURN .F.
ENDIF
lcMenu=IIF(EMPTY(tcParentMenu),SYS(2015),ALLTRIM(tcParentMenu))
this.cMenu=lcMenu
lnArrayColumns=ALEN(this.aMenu,2)
llMultiArray=(lnArrayColumns>0)
DEACTIVATE POPUP (lcMenu)
CLEAR TYPEAHEAD
IF EMPTY(tcParentMenu)
	DEFINE POPUP (lcMenu) ;
			FROM lnMRow,lnMCol ;
			MARGIN ;
			SHORTCUT
	ON SELECTION POPUP (lcMenu) DEACTIVATE MENU (lcMenu)
ENDIF
lnSkipCount=0
FOR lnCount = 1 TO lnMenuCount
	lcMenuItem=IIF(llMultiArray,this.aMenu[lnCount,1],this.aMenu[lnCount])
	IF TYPE("lcMenuItem")#"C" OR EMPTY(lcMenuItem) OR ;
			((lnCount=1 OR lnCount=lnMenuCount) AND ALLTRIM(lcMenuItem)=="\-")
		lnSkipCount=lnSkipCount+1
		LOOP
	ENDIF
	lnBar=lnCount-lnSkipCount
	llSetMark=.F.
	IF LEFT(lcMenuItem,1)=="^"
		lcMenuItem=SUBSTR(lcMenuItem,2)
		llSetMark=.T.
	ENDIF
	IF lnArrayColumns>=3 AND NOT EMPTY(this.aMenu[lnCount,3])
		lcClauses=ALLTRIM(this.aMenu[lnCount,3])
	ELSE
		lcClauses=""
	ENDIF
	IF EMPTY(lcClauses)
		DEFINE BAR lnBar OF (lcMenu) PROMPT (lcMenuItem)
	ELSE
		DEFINE BAR lnBar OF (lcMenu) PROMPT (lcMenuItem) &lcClauses
	ENDIF
	IF llSetMark
		SET MARK OF BAR (lnBar) OF (lcMenu) TO .T.
	ENDIF
	IF NOT llMultiArray
		LOOP
	ENDIF
	luMenuSelection=this.aMenu[lnCount,2]
	IF TYPE("luMenuSelection")=="O" AND NOT ISNULL(luMenuSelection)
		lcMenu2=SYS(2015)
		DEFINE POPUP (lcMenu2) ;
				MARGIN ;
				SHORTCUT
		ON SELECTION POPUP (lcMenu2) DEACTIVATE MENU (lcMenu2)
		ON BAR lnBar OF (lcMenu) ACTIVATE POPUP (lcMenu2)
		IF EMPTY(luMenuSelection.cOnSelection)
			luMenuSelection.cOnSelection=this.cOnSelection
		ENDIF
		luMenuSelection.ActivateMenu(lcMenu2)
		LOOP
	ENDIF
	IF EMPTY(luMenuSelection)
		luMenuSelection=ALLTRIM(this.cOnSelection)
	ENDIF
	IF NOT EMPTY(luMenuSelection)
		ON SELECTION BAR lnBar OF (lcMenu) &luMenuSelection
	ENDIF
ENDFOR
IF lnSkipCount>=lnMenuCount OR NOT EMPTY(tcParentMenu)
	RETURN
ENDIF
ACTIVATE POPUP (lcMenu)
IF NOT EMPTY(this.cMenu)
	DEACTIVATE POPUP (this.cMenu)
ENDIF
this.cMenu=""
DOEVENTS

ENDPROC
PROCEDURE clearmenu
DIMENSION this.aMenu[1]
this.aMenu=""
this.cOnSelection=""

ENDPROC
PROCEDURE newmenu
LOCAL toObject
LOCAL oNewObject,lcClass,lcClassLibrary,lcBaseClass,lcAlias,llAddLibrary

IF TYPE("toObject")#"O" OR ISNULL(toObject)
	toObject=this
ENDIF
lcClass=LOWER(toObject.Class)
lcClassLibrary=LOWER(toObject.ClassLibrary)
lcBaseClass=LOWER(toObject.BaseClass)
IF EMPTY(lcClassLibrary)
	oNewObject=CREATEOBJECT(lcBaseClass)
	RETURN oNewObject
ENDIF
lcAlias=LOWER(SYS(2015))
llAddLibrary=(ATC(lcClassLibrary,SET("CLASSLIB"))=0)
IF llAddLibrary
	SET CLASSLIB TO (lcClassLibrary) ALIAS (lcAlias) ADDITIVE
ENDIF
oNewObject=CREATEOBJECT(lcClass)
IF llAddLibrary
	RELEASE CLASSLIB ALIAS (lcAlias)
ENDIF
RETURN oNewObject

ENDPROC
PROCEDURE addmenubar
LPARAMETERS tcPrompt,tcOnSelection,tcClauses,tnElementNumber,tlMark,tlDisabled,tlBold
LOCAL lcPrompt,lcClauses,lnElementNumber,lnMenuCount,lnArrayColumns,lnIndex,oShortCutMenu

IF EMPTY(tcPrompt)
	RETURN .F.
ENDIF
IF TYPE("tcPrompt")=="O" AND NOT ISNULL(tcPrompt)
	oShortCutMenu=tcPrompt
	tcPrompt=.NULL.
	FOR lnIndex = 1 TO ALEN(oShortCutMenu.aMenu,1)
		this.AddMenuBar(oShortCutMenu.aMenu[lnIndex,1],oShortCutMenu.aMenu[lnIndex,2], ;
				oShortCutMenu.aMenu[lnIndex,3])
	ENDFOR
	RETURN
ENDIF
lcPrompt=tcPrompt
lcClauses=IIF(EMPTY(tcClauses),"",tcClauses)
IF tlMark
	lcPrompt="^"+lcPrompt
ENDIF
IF tlDisabled
	lcClauses=lcClauses+[ SKIP FOR .T.]
ENDIF
IF tlBold
	lcClauses=lcClauses+[ STYLE "B"]
ENDIF
lnMenuCount=ALEN(this.aMenu,1)
lnArrayColumns=ALEN(this.aMenu,2)
IF lnMenuCount<=1 AND EMPTY(this.aMenu[1])
	lnMenuCount=1
	lnArrayColumns=3
ELSE
	lnMenuCount=lnMenuCount+1
ENDIF
lnIndex=lnMenuCount
DIMENSION this.aMenu[lnIndex,lnArrayColumns]
IF TYPE("tnElementNumber")=="N"
	lnElementNumber=MAX(INT(tnElementNumber),1)
	IF lnElementNumber<lnMenuCount
		IF AINS(this.aMenu,lnElementNumber)#1
			RETURN .F.
		ENDIF
		lnIndex=lnElementNumber
	ENDIF
ENDIF
IF lnArrayColumns<=1
	this.aMenu[lnIndex]=lcPrompt
	RETURN
ENDIF
this.aMenu[lnIndex,1]=lcPrompt
this.aMenu[lnIndex,2]=tcOnSelection
IF lnArrayColumns>=3
	this.aMenu[lnIndex,3]=lcClauses
ENDIF

ENDPROC
PROCEDURE addmenuseparator
LPARAMETERS tnElementNumber

this.AddMenuBar("\-",,,tnElementNumber)

ENDPROC
PROCEDURE showmenu
RETURN this.ActivateMenu()

ENDPROC
PROCEDURE setmenu
LPARAMETERS toObject

this.ClearMenu
RETURN .F.

ENDPROC
PROCEDURE Init
this.ClearMenu

ENDPROC


EndDefine 
