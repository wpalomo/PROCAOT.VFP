Public ST3CC_45T0S4W1I
SET CLASSLIB TO "st3class.vcx" ADDITIVE

ST3CC_45T0S4W1I = CreateObject("ST3CC" )
If ST3CC_45T0S4W1I.WindowType == 1
ST3CC_45T0S4W1I.Show(1)
else
ST3CC_45T0S4W1I.Show()
EndIf



Define Class ST3CC as form
Height = 155
Width = 481
ShowTips = .T.
AutoCenter = .T.
BackColor = Rgb(192,192,192)
BorderStyle = 2
Caption = (f3_t('Cambio de Contraseña'))
Closable = .F.
FontBold = .F.
FontName = "Courier"
FontSize = 10
HalfHeightCaption = .F.
MaxButton = .F.
MinButton = .F.
Movable = .T.
FillColor = Rgb(192,192,192)
WindowType = 1
ColorSource = 5

ADD OBJECT shpShape1 as shape WITH BackColor = Rgb(255,255,255), BorderWidth = 2, BorderColor = Rgb(128,128,128), FillColor = Rgb(255,255,255), FillStyle = 0, Height = 90, Left = 11, Top = 14, Width = 453, ReleaseErase = .F., SpecialEffect = 1, ColorSource = 3
ADD OBJECT text2 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, BackColor = Rgb(255,255,255), BorderStyle = 1, ControlSource = "m.cont_ant", Format = "K!", Height = 17, InputMask = "!!!!!!", Left = 359, SpecialEffect = 1, Top = 26, Width = 56, PasswordChar = "*", ColorSource = 0
ADD OBJECT text3 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, BackColor = Rgb(255,255,255), BorderStyle = 1, ControlSource = "m.cont_n1", Format = "K!", Height = 17, InputMask = "!!!!!!", Left = 359, SpecialEffect = 1, Top = 50, Width = 56, ColorSource = 3, PasswordChar = "*"
ADD OBJECT text4 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, BackColor = Rgb(255,255,255), BorderStyle = 1, ControlSource = "m.cont_n2", Format = "K!", Height = 17, InputMask = "!!!!!!", Left = 359, SpecialEffect = 1, Top = 75, Width = 56, ColorSource = 3, PasswordChar = "*"
ADD OBJECT Line1 as line WITH Height = 0, Left = 0, Top = 108, Width = 480
ADD OBJECT Line2 as line WITH BorderColor = Rgb(255,255,255), Height = 0, Left = 0, Top = 109, Width = 480
ADD OBJECT L_normal6 as l_normal WITH Caption = "Contraseña Antigua", Left = 198, Top = 27
ADD OBJECT L_normal7 as l_normal WITH Caption = "Contraseña Nueva", Left = 198, Top = 54
ADD OBJECT L_normal8 as l_normal WITH Caption = "Repetir C. Nueva", Left = 198, Top = 81
ADD OBJECT L_tit10 as l_tit WITH FontName = "Times New Roman", FontSize = 14, FontUnderline = .F., Caption = "Usuario", Left = 27, Top = 27
ADD OBJECT L_tit9 as l_tit WITH FontBold = .T., FontName = "Times New Roman", FontSize = 14, FontUnderline = .F., Alignment = 0, Caption = "[Cod.Usuario]", Left = 27, Top = 48
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 117, Left = 432, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabStop = .F., ToolTipText = "Salir"


PROCEDURE Release
_esc=.F.
ENDPROC
PROCEDURE Init
_controlc=.F.
this.L_tit9.caption=sysuser.USR_COD
store space(6) to m.CONT_N1,m.CONT_N2,m.CONT_ANT
public contador
contador=0

ENDPROC
PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
*
if _esc=.T.
  _esc=.F.
  thisform.release
endif
ENDPROC


PROCEDURE text2.Valid
estado=.T.
do case
case empty(sysuser.USR_PAS) .and. !empty(m.cont_ant)
  estado=.F.
case !empty(sysuser.USR_PAS)
  lxpas=m.cont_ant
  lxpasd=chr(asc(subs(lxpas,1,1))+asc(subs(USR_COD,5,1))-int(asc(subs(USR_COD,3,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,2,1))+asc(subs(USR_COD,2,1))-int(asc(subs(USR_COD,1,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,3,1))+asc(subs(USR_COD,1,1))-int(asc(subs(USR_COD,4,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,4,1))+asc(subs(USR_COD,6,1))-int(asc(subs(USR_COD,3,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,5,1))+asc(subs(USR_COD,4,1))-int(asc(subs(USR_COD,2,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,6,1))+asc(subs(USR_COD,3,1))-int(asc(subs(USR_COD,3,1))/3))
  if lxpasd<>USR_PAS
    estado=.F.
  endif
endcase
do case
case estado=.F. .and. contador=4
  do form st3sn with 1,1,'Compruebe su password'
  thisform.release
case estado=.F.
  contador=contador+1
otherwise
  contador=0
endcase
return estado
ENDPROC


PROCEDURE text4.Valid
if m.CONT_N1=m.CONT_N2
  lxpas=m.CONT_N1
  lxpasd=chr(asc(subs(lxpas,1,1))+asc(subs(USR_COD,5,1))-int(asc(subs(USR_COD,3,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,2,1))+asc(subs(USR_COD,2,1))-int(asc(subs(USR_COD,1,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,3,1))+asc(subs(USR_COD,1,1))-int(asc(subs(USR_COD,4,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,4,1))+asc(subs(USR_COD,6,1))-int(asc(subs(USR_COD,3,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,5,1))+asc(subs(USR_COD,4,1))-int(asc(subs(USR_COD,2,1))/3))
  lxpasd=lxpasd+chr(asc(subs(lxpas,6,1))+asc(subs(USR_COD,3,1))-int(asc(subs(USR_COD,3,1))/3))
  repl sysuser.USR_PAS with lxpasd
  release thisform
endif

ENDPROC


PROCEDURE bot_esc.Click
thisform.release
ENDPROC


EndDefine 
