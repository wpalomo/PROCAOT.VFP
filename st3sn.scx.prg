Public ST3SN_45T0S4OR4

ST3SN_45T0S4OR4 = CreateObject("ST3SN" )
If ST3SN_45T0S4OR4.WindowType == 1
ST3SN_45T0S4OR4.Show(1)
else
ST3SN_45T0S4OR4.Show()
EndIf



Define Class ST3SN as form
Height = 126
Width = 520
AutoCenter = .T.
BorderStyle = 2
Caption = "AVISO"
ControlBox = .F.
Closable = .F.
FontBold = .T.
FontName = "Courier"
FontSize = 10
MaxButton = .F.
MinButton = .F.
WindowType = 1
LockScreen = .F.
AlwaysOnTop = .T.
ColorSource = 5
BackColor = Rgb(192,192,192)
fn_boton1 = ""
fn_boton2 = ""
fn_boton3 = ""

ADD OBJECT Image1 as image WITH DragMode = 0, Picture = "bmp\caution.bmp", Stretch = 0, BackStyle = 0, Height = 40, Left = 6, Top = 23, Width = 40
ADD OBJECT bot1 as commandbutton WITH AutoSize = .F., Top = 90, Left = 198, Height = 29, Width = 85, FontName = "Arial", FontSize = 10, Cancel = .T., Caption = "\<Aceptar", Default = .T., TabIndex = 4, ColorSource = 0
ADD OBJECT bot2 as commandbutton WITH Top = 90, Left = 297, Height = 29, Width = 85, FontName = "Arial", FontSize = 10, Cancel = .T., Caption = "\<Cancelar", TabIndex = 5, ColorSource = 0
ADD OBJECT Shape1 as shape WITH Top = 10, Left = 59, Height = 73, Width = 444, BackStyle = 1, SpecialEffect = 0, ColorSource = 0, BackColor = Rgb(255,255,255)
ADD OBJECT Label1 as label WITH AutoSize = .F., FontName = "Arial", FontSize = 9, Alignment = 2, BackStyle = 0, Caption = "Label1", Height = 18, Left = 70, Top = 20, Width = 421, TabIndex = 1, ColorSource = 0
ADD OBJECT Label2 as label WITH AutoSize = .F., FontName = "Arial", FontSize = 9, Alignment = 2, BackStyle = 0, Caption = "Label2", Height = 18, Left = 70, Top = 39, Width = 421, TabIndex = 2, ColorSource = 0
ADD OBJECT Label3 as label WITH AutoSize = .F., FontName = "Arial", FontSize = 9, Alignment = 2, BackStyle = 0, Caption = "Label3", Height = 18, Left = 70, Top = 58, Width = 421, TabIndex = 3, ColorSource = 0
ADD OBJECT bot3 as commandbutton WITH Top = 90, Left = 395, Height = 29, Width = 85, FontName = "Arial", FontSize = 10, Cancel = .T., Caption = "", TabIndex = 6, Visible = .F., ColorSource = 0


PROCEDURE Init
Parameter _nbotones,_dibujo,_texto1,_texto2,_texto3,_titulo,_lx1,_lx2,_lx3
*
store '' to _fnbot1,_fnbot2,_fnbot3
_controlc=.F.
_nbotones=iif(type('_nbotones')='N',_nbotones,2)
_dibujo=iif(type('_dibujo')='N',_dibujo,1)
_texto1=iif(type('_texto1')='C',_texto1,'')
_texto2=iif(type('_texto2')='C',_texto2,'')
_texto3=iif(type('_texto3')='C',_texto3,'')
_titulo=iif(type('_titulo')='C',_titulo,'')
conf=''
do case
case _nbotones=1
  thisform.bot1.visible=.F.
  thisform.bot2.left=240
  thisform.bot2.caption='Continuar'
case _nbotones=3
  thisform.bot1.left=120
  thisform.bot2.left=230
  thisform.bot3.left=340
  thisform.bot3.visible=.T.
endcase
thisform.caption=trim(_titulo)
* Por error el botón 2 es el primero en ser visualizado
if type('_lx1')='C' .and. !empty(_lx1)
  nd=at(',',_lx1)
  if nd<>0
    lx1=subs(_lx1,nd+1)
    if !empty(lx1)
      _fnbot2=lx1
    endif
    _lx1=iif(nd=1,'',left(_lx1,nd-1))
  endif
  if !empty(_lx1)
    thisform.bot2.caption=_lx1
  endif
endif
if type('_lx2')='C' .and. !empty(_lx2)
  nd=at(',',_lx2)
  if nd<>0
    lx1=subs(_lx2,nd+1)
    if !empty(lx1)
      _fnbot1=lx1
    endif
    _lx2=iif(nd=1,'',left(_lx2,nd-1))
  endif
  if !empty(_lx2)
    thisform.bot1.caption=_lx2
  endif
endif
if type('_lx3')='C' .and. !empty(_lx3)
  nd=at(',',_lx3)
  if nd<>0
    lx1=subs(_lx3,nd+1)
    if !empty(lx1)
      _fnbot3=lx1
    endif
    _lx3=iif(nd=1,'',left(_lx3,nd-1))
  endif
  if !empty(_lx3)
    thisform.bot3.caption=_lx3
  endif
endif
do case
case _dibujo=1
  _titulo=iif(empty(_titulo),'Atención',_titulo)
case _dibujo=2
  _titulo=iif(empty(_titulo),'Aviso',_titulo)
  this.image1.picture='bmp\info.bmp'
case _dibujo=3
  _titulo=iif(empty(_titulo),'Pregunta',_titulo)
  this.image1.picture='bmp\pregunta.bmp'
case _dibujo=4
  _titulo=iif(empty(_titulo),'Prohibido',_titulo)
  this.image1.picture='bmp\parar.bmp'
endcase
thisform.caption=_titulo
if _xidiom>1
  thisform.caption=f3_t(thisform.caption)
  thisform.bot1.caption=f3_t(thisform.bot1.caption)
  thisform.bot2.caption=f3_t(thisform.bot2.caption)
  thisform.bot3.caption=f3_t(thisform.bot3.caption)
  thisform.label1.caption=f3_t(_texto1)
  thisform.label2.caption=f3_t(_texto2)
  thisform.label3.caption=f3_t(_texto3)
else
  thisform.label1.caption=_texto1
  thisform.label2.caption=_texto2
  thisform.label3.caption=_texto3
endif

ENDPROC
PROCEDURE Release
_controlc=.F.

ENDPROC
PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok()
*
do case
case _esc=.T.
  _esc=.F.
  thisform.bot2.click
case _enter=.T.
  _enter=.F.
  thisform.bot1.click
endcase
ENDPROC


PROCEDURE bot1.Click
if empty(_fnbot1).or._fnbot1='VALOR'
  conf='S'
else
  do &_fnbot1
endif
if !empty(conf)
  thisform.release
endif

ENDPROC


PROCEDURE bot2.Click
if empty(_fnbot2) .or. _fnbot2='VALOR'
  conf='N'
else
  do &_fnbot2
endif
if !empty(conf)
  thisform.release
endif

ENDPROC


PROCEDURE bot3.Click
if empty(_fnbot3) .or. _fnbot3='VALOR'
  conf='X'
else
  do &_fnbot3
endif
if !empty(conf)
  thisform.release
endif

ENDPROC


EndDefine 
