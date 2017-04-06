Public SY3USR_45T0S4UMZ
SET CLASSLIB TO "st3class.vcx" ADDITIVE

SY3USR_45T0S4UMZ = CreateObject("SY3USR" )
If SY3USR_45T0S4UMZ.WindowType == 1
SY3USR_45T0S4UMZ.Show(1)
else
SY3USR_45T0S4UMZ.Show()
EndIf



Define Class SY3USR as form
ScaleMode = 3
Height = 165
Width = 465
AutoCenter = .T.
BorderStyle = 1
Caption = "Cambio de Usuario"
ControlBox = .F.
FontBold = .T.
FontName = "Courier"
FontSize = 10
MaxButton = .F.
MinButton = .F.
WindowType = 1
LockScreen = .F.
AlwaysOnTop = .T.
WhatsThisHelpID = 555
ColorSource = 5
BackColor = Rgb(192,192,192)

ADD OBJECT Shape2 as shape WITH Top = 120, Left = 72, Height = 37, Width = 277, BackStyle = 0, FillStyle = 1, SpecialEffect = 0, ColorSource = 0
ADD OBJECT Shape1 as shape WITH Top = 52, Left = 24, Height = 61, Width = 421, SpecialEffect = 0, ColorSource = 0, ColorScheme = 1, BackColor = Rgb(255,255,255)
ADD OBJECT usr_nom as label WITH FontBold = .T., FontSize = 10, BorderStyle = 1, Caption = "", Height = 18, Left = 96, Top = 60, Width = 277, TabIndex = 3, ColorSource = 0
ADD OBJECT hora as label WITH FontBold = .T., FontSize = 10, BorderStyle = 1, Caption = "", Height = 18, Left = 96, Top = 84, Width = 109, TabIndex = 5, ColorSource = 0
ADD OBJECT dia as label WITH FontBold = .T., FontSize = 10, BorderStyle = 1, Caption = "", Height = 18, Left = 264, Top = 84, Width = 109, TabIndex = 6, ColorSource = 0
ADD OBJECT L_normal6 as l_normal WITH Caption = "Código del usuario", Left = 108, Top = 18, TabIndex = 1
ADD OBJECT L_normal7 as l_normal WITH Caption = "Hola", Left = 48, Top = 62, TabIndex = 4
ADD OBJECT L_normal8 as l_normal WITH Caption = "son las", Left = 48, Top = 85, TabIndex = 7
ADD OBJECT L_normal9 as l_normal WITH Caption = "del día", Left = 216, Top = 86, TabIndex = 8
ADD OBJECT L_normal10 as l_normal WITH Caption = "Código de seguridad", Left = 85, Top = 130, TabIndex = 9
ADD OBJECT USR_COD as st_get WITH ControlSource = "m.USR_COD", Height = 18, InputMask = "!!!!!!", Left = 234, TabIndex = 2, Top = 18, Width = 55
ADD OBJECT USR_PAS1 as st_get WITH ControlSource = "m.usr_pas1", Height = 18, InputMask = "!!!!!!", Left = 234, TabIndex = 10, Top = 130, Width = 55, PasswordChar = "*"
ADD OBJECT Image1 as image WITH Stretch = 1, Height = 36, Left = 8, Top = 5, Width = 42
ADD OBJECT St_box3 as st_box WITH Top = 0, Left = 1, Height = 44, Width = 56


PROCEDURE Init
if _xidiom>1
  this.caption=f3_t(this.caption)
endif
ENDPROC
PROCEDURE Release
if total<4
  _correcto=.T.
  _usrcod=m.USR_COD
  use _fich+'\EMPRESA' order tag CODIGO in 0
  select EMPRESA
  seek sysuser.USR_EMP
  if eof() .or. sysuser.USR_EMP=0
    thisform.hide
    _em=''
    =f3_ce()
    select EMPRESA
    thisform.show
  endif
  if _xier=0 .and. !eof()
    scatter memvar
    do st3conf
    do sy3caption

****modi windows screen title trim(_titprg)+' ['+_em+' '+trim(_nem)+'] '+_usrcod font 'Courier',10 noclose

    _cse=m.USR_NIV
    _usrcod=m.USR_COD
    _xidiom=sysuser.USR_IDIOM
  endif
  select EMPRESA
  use
  select SYSPRG
else
  _correcto=.F.
endif

ENDPROC
PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
m.USR_COD=_usrcod
*
if _esc=.T.
  _esc=.F.
  total=5
  thisform.release
endif
ENDPROC


PROCEDURE USR_COD.Valid
select SYSUSER
seek ' '+m.USR_COD
_nok=1
if empty(m.USR_COD) .or. eof()
  total=total+1
  if total>=4
    _ok=f3_sn(2,1,'Compruebe su código de usuario','Desea volver a entrarlo')
    if _ok=.F.
      thisform.release
    endif
    total=0
  endif
  _nok=0
else
  scatter memvar
  thisform.hora.caption=' '+time()
  thisform.dia.caption=' '+dtoc(date())
  thisform.usr_nom.caption=' '+USR_NOM
endif
return _nok
ENDPROC


PROCEDURE USR_PAS1.Valid
on key label 'ESC' do =f3_esc()
lxpas=iif(empty(m.USR_PAS1),space(6),m.USR_PAS1)
lxpasd=chr(asc(subs(lxpas,1,1))+asc(subs(USR_COD,5,1))-int(asc(subs(USR_COD,3,1))/3))
lxpasd=lxpasd+chr(asc(subs(lxpas,2,1))+asc(subs(USR_COD,2,1))-int(asc(subs(USR_COD,1,1))/3))
lxpasd=lxpasd+chr(asc(subs(lxpas,3,1))+asc(subs(USR_COD,1,1))-int(asc(subs(USR_COD,4,1))/3))
lxpasd=lxpasd+chr(asc(subs(lxpas,4,1))+asc(subs(USR_COD,6,1))-int(asc(subs(USR_COD,3,1))/3))
lxpasd=lxpasd+chr(asc(subs(lxpas,5,1))+asc(subs(USR_COD,4,1))-int(asc(subs(USR_COD,2,1))/3))
lxpasd=lxpasd+chr(asc(subs(lxpas,6,1))+asc(subs(USR_COD,3,1))-int(asc(subs(USR_COD,3,1))/3))
if lxpasd=sysuser.USR_PAS
  thisform.release
else
  total=total+1
  if total>=4
    _ok=f3_sn(2,1,'Compruebe su código de acceso','Desea volver a entrarlo')
    if _ok=.F.
      thisform.release
    endif
    total=0
  endif
endif

ENDPROC
PROCEDURE USR_PAS1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if nKeyCode=27
  _esc=.F.
  total=5
  thisform.release
endif
ENDPROC
PROCEDURE USR_PAS1.When
on key
estado=iif(empty(m.USR_COD),.F.,.T.)
m.USR_PAS=space(6)
return estado

ENDPROC


PROCEDURE Image1.Init
this.Picture=_logo
ENDPROC


EndDefine 
