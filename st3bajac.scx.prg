Public ST3BAJA_45T0S4SDB
SET CLASSLIB TO "st3class.vcx" ADDITIVE

ST3BAJA_45T0S4SDB = CreateObject("ST3BAJA" )
If ST3BAJA_45T0S4SDB.WindowType == 1
ST3BAJA_45T0S4SDB.Show(1)
else
ST3BAJA_45T0S4SDB.Show()
EndIf



Define Class ST3BAJA as form
Top = 255
Left = 33
Height = 54
Width = 488
ShowTips = .T.
BackColor = Rgb(64,128,128)
BorderStyle = 3
Caption = ""
Closable = .F.
ControlBox = .F.
MaxButton = .F.
MinButton = .F.
Enabled = .T.
WindowType = 1
AlwaysOnTop = .T.
FontBold = .T.
FontSize = 10
ColorSource = 5

ADD OBJECT Shape1 as shape WITH BackStyle = 0, BorderStyle = 1, Height = 47, Left = 352, Top = 3, Width = 117, SpecialEffect = 0, ColorSource = 0
ADD OBJECT bot_si as st_bot WITH AutoSize = .F., Top = 9, Left = 423, Height = 32, Width = 32, Picture = "bmp\ok.bmp", Caption = "", TabIndex = 2, ToolTipText = "Si"
ADD OBJECT bot_no as st_bot WITH AutoSize = .F., Top = 9, Left = 369, Height = 32, Width = 32, Picture = "bmp\cancel.bmp", Caption = "", TabIndex = 1, ToolTipText = "No"
ADD OBJECT L_normal2 as l_normal WITH Caption = "Desea eliminar esta ficha con TODAS sus líneas", ForeColor = Rgb(255,255,255), Left = 25, Top = 15


PROCEDURE Init
thisform.caption=sysfc.FC_NOM
if _xidiom>1
  thisform.caption=f3_t(thisform.caption)
  thisform.bot_si.ToolTipText=f3_t(thisform.bot_si.ToolTipText)
  thisform.bot_no.ToolTipText=f3_t(thisform.bot_no.ToolTipText)
endif
thisform.left=(_screen.width-456)/2
thisform.top=_screen.height-94

ENDPROC
PROCEDURE Unload
_controlc=.F.

ENDPROC


PROCEDURE bot_si.Valid
_ok=.T.
select alias (_xfc)
_xobjw='X'+trim(_xfc)
if  file(_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'OK_BAJA',.T.
  select alias (_xfc)
endif
_xier=0
if _entorno='DBF'
  r1_baja=recno()
  skip
  r1_siguiente=iif(eof(),0,recno())
  go r1_baja
  delete record recno()
  do case
  case _xier<>0
    go r1_baja
  case r1_siguiente=0
    go bottom
  otherwise
    go r1_siguiente
  endcase
else
  =f3_baja(_xfc)
endif
thisform.release
ENDPROC


PROCEDURE bot_no.Valid
_ok=.F.
thisform.release

ENDPROC


EndDefine 
