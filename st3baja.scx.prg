Public ST3BAJA_45T0S4RWB
SET CLASSLIB TO "st3class.vcx" ADDITIVE

ST3BAJA_45T0S4RWB = CreateObject("ST3BAJA" )
If ST3BAJA_45T0S4RWB.WindowType == 1
ST3BAJA_45T0S4RWB.Show(1)
else
ST3BAJA_45T0S4RWB.Show()
EndIf



Define Class ST3BAJA as form
Top = 255
Left = 33
Height = 54
Width = 456
ShowTips = .T.
BackColor = Rgb(255,255,128)
BorderStyle = 2
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

ADD OBJECT Shape1 as shape WITH BackStyle = 0, BorderStyle = 1, Height = 47, Left = 284, Top = 4, Width = 145, SpecialEffect = 0, ColorSource = 0
ADD OBJECT bot_si as st_bot WITH AutoSize = .F., Top = 10, Left = 380, Height = 32, Width = 32, Picture = "bmp\ok.bmp", Caption = "", TabIndex = 2, ToolTipText = "Si"
ADD OBJECT bot_no as st_bot WITH AutoSize = .F., Top = 10, Left = 310, Height = 32, Width = 32, Picture = "bmp\cancel.bmp", Caption = "", TabIndex = 1, ToolTipText = "No"
ADD OBJECT L_normal2 as l_normal WITH Caption = "Desea dar de baja la ficha seleccionada", Left = 27, Top = 18


PROCEDURE Unload
_controlc=.F.

ENDPROC
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


PROCEDURE bot_si.Valid
select alias (_xfc)
_xobjw='X'+trim(_xfc)
if  file(_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'OK_BAJA',.T.
  select alias (_xfc)
endif
_xier=0
=f3_baja(_xfc)
thisform.release

ENDPROC


PROCEDURE bot_no.Valid
_xier=1
thisform.release

ENDPROC


EndDefine 
