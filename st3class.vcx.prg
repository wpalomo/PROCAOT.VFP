Define Class f_pant as form
Height = 252
Width = 551
ShowTips = .T.
AutoCenter = .T.
BackColor = Rgb(192,192,192)
Caption = ""
LockScreen = .T.

ADD OBJECT Shape1 as shape WITH BackStyle = 0, Height = 29, Left = 33, Top = 219, Width = 91, SpecialEffect = 0
ADD OBJECT bot_top as commandbutton WITH Top = 224, Left = 39, Height = 21, Width = 21, Picture = "bmp\top.bmp", DisabledPicture = "", Caption = "", TabIndex = 1, ToolTipText = "Inicio"
ADD OBJECT bot_ant as commandbutton WITH Top = 224, Left = 59, Height = 21, Width = 21, Picture = "bmp\prior.bmp", Caption = "", TabIndex = 2, ToolTipText = "Anterior"
ADD OBJECT bot_sig as commandbutton
ADD OBJECT l_stit as label WITH AutoSize = .T., FontItalic = .T., FontName = "MS Sans Serif", FontSize = 10, FontUnderline = .T., BackColor = Rgb(255,255,255), BackStyle = 0, Caption = "Subtit1", ForeColor = Rgb(0,0,128), Height = 18, Width = 55


PROCEDURE traduce
Parameter _linea
*
select SYSTXT
seek _linea
if eof()
  append blank
  repl TXT_I1 with _linea
else
  lx=mline(TXT_I2,_xidiom-1)
  _linea=iif(empty(lx),_linea,lx)
endif
select SYSPRG
return trim(_linea)

ENDPROC
PROCEDURE busca
Parameter _nidx,lxkey
*
_nidx=max(1,_nidx)
if !empty(lxkey)
  select (_xfc)
  if _nidx>1
    set order to (_nidx)
  endif
  seek lxkey
  if !eof()
    scatter memvar
    thisform.refresh
  endif
  set order to 1
endif
*
return

ENDPROC
PROCEDURE Unload
select (_xfc)
if !eof()
  unlock
endif
select SYSPRG
lx1=upper(thisform.name)
nd=at('['+lx1+'/',_xopenfc)
do while nd>0
  lx2=subs(_xopenfc,nd)
  nd1=at(']',lx2)
  lx2=left(lx2,nd1)
  _xopenfc=stuff(_xopenfc,nd,nd1,'')
  nd1=at('/',lx2)
  lx2=subs(lx2,nd1)
  nd1=at(lx2,_xopenfc)
  if nd1=0
    lx2=subs(lx2,2,len(lx2)-2)
    select alias (lx2)
    use
  endif
  nd=at('['+lx1+'/',_xopenfc)
enddo
select SYSPRG

ENDPROC
PROCEDURE Load
select SYSPRG
m.c_lock=0
_linea=PRG_TIT
if _xidiom>1
  _linea=thisform.traduce(_linea)
endif
thisform.c_lock.value=0
thisform.caption=_linea
thisform.BorderStyle=2
thisform.l_negra.left=2
thisform.l_negra.width=thisform.width-4
thisform.l_blanca.left=2
thisform.l_blanca.width=thisform.width-4

ENDPROC
PROCEDURE Activate
prog=wontop()
select SYSPRG
seek prog
_xfc=mline(sysprg.PRG_FICH,1)
select alias (_xfc)
scatter memvar
m.c_lock=thisform.c_lock.value
select SYSPRG
thisform.refresh

ENDPROC


PROCEDURE bot_top.Click
_xfc=mline(sysprg.PRG_FICH,1)
select (_xfc)
go top
if !eof()
  scatter memvar
  thisform.refresh
else
  wait window f3_t('ATENCION -> Fichero vacío')
endif
select SYSPRG
ENDPROC


PROCEDURE bot_ant.Click
_xfc=mline(sysprg.PRG_FICH,1)
select (_xfc)
nd=iif(eof(),0,recno())
if !bof()
  skip -1
endif
do case
case !bof()
  scatter memvar
  thisform.refresh
case nd>0
  ?? chr(7)
  wait window f3_t('ATENCION -> Inicio de fichero')
  go nd
otherwise
  ?? chr(7)
  wait window f3_t('ATENCION -> Fichero vacío')
endcase
select SYSPRG
ENDPROC


PROCEDURE Init
if _xidiom>1
  _linea=f3_t(this.caption)
  this.caption=trim(_linea)
endif
ENDPROC


EndDefine 
Define Class l_stit as label
AutoSize = .T.
FontItalic = .T.
FontName = "MS Sans Serif"
FontSize = 10
FontUnderline = .T.
BackColor = Rgb(255,255,255)
BackStyle = 0
Caption = "Subtit1"
ForeColor = Rgb(0,0,128)
Height = 18
Width = 55



PROCEDURE Init
if _xidiom>1
  _linea=f3_t(this.caption)
  this.caption=trim(_linea)
endif
ENDPROC


EndDefine 
Define Class l_tit as label
AutoSize = .T.
FontItalic = .T.
FontName = "MS Sans Serif"
FontSize = 12
FontUnderline = .T.
BackStyle = 0
Caption = "Label1"
ForeColor = Rgb(0,0,128)
Height = 22
Width = 64



PROCEDURE Init
if _xidiom>1
  _linea=f3_t(this.caption)
  this.caption=trim(_linea)
endif

ENDPROC


EndDefine 
Define Class st_list as listbox
FontBold = .F.
FontName = "Courier"
RowSourceType = 0
Height = 68
MoverBars = .T.
MultiSelect = .T.
Width = 68
DisabledItemForeColor = Rgb(0,0,128)
DisabledItemBackColor = Rgb(192,192,192)



PROCEDURE Init
if _psql
  this.RowSourceType=0
  this.RowSource=''
  this.enabled=.F.
endif

ENDPROC


EndDefine 
Define Class st_spin as spinner
FontBold = .F.
FontName = "Courier"
FontSize = 10
Height = 20
Margin = 0
Width = 107



PROCEDURE Init
if _psql
  This.ReadOnly=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.ForeColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif
ENDPROC
PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.ForeColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.ForeColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif

ENDPROC


EndDefine 
Define Class l_normal as label
AutoSize = .T.
FontName = "Arial"
FontSize = 9
BackStyle = 0
Caption = "Label1"
Height = 17
Width = 38



PROCEDURE Init
if _xidiom>1
  _linea=f3_t(this.caption)
  this.caption=trim(_linea)
endif
ENDPROC


EndDefine 
Define Class st_chek as checkbox
Height = 17
Width = 61
FontName = "Arial"
FontSize = 9
AutoSize = .T.
BackStyle = 0
Caption = "Check1"



PROCEDURE Init
if _xidiom>1
  _linea=f3_t(this.caption)
  this.caption=trim(_linea)
endif
if _psql
  this.enabled=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.ForeColor=RGB(255,0,0)
    this.FontBold=.F.
  endif
endif

ENDPROC
PROCEDURE Valid
if _psql
  lx1=this.ControlSource
  &lx1=0
  this.refresh
endif
ENDPROC
PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.FontBold=.F.
    this.ForeColor=RGB(255,0,0)
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.ForeColor=RGB(0,0,0)
    this.FontBold=.T.
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC


EndDefine 
Define Class st_option as optiongroup
AutoSize = .T.
ButtonCount = 2
BackStyle = 0
Value = 1
Height = 48
Width = 78
Option1.FontName = "Arial"
Option1.FontSize = 9
Option1.BackStyle = 0
Option1.Caption = "Option1"
Option1.Value = 1
Option1.Height = 17
Option1.Left = 5
Option1.Top = 5
Option1.Width = 62
Option1.AutoSize = .T.
Option2.FontName = "Arial"
Option2.FontSize = 9
Option2.BackStyle = 0
Option2.Caption = "Option2"
Option2.Value = 0
Option2.Height = 18
Option2.Left = 5
Option2.Top = 25
Option2.Width = 68



PROCEDURE Init
if _psql
  this.enabled=.T.
  this.ButtonCount=1
  lx1=this.ControlSource
  nd=at('.',lx1)
  if nd>0
    lx1=subs(lx1,nd+1)
  endif
  if atc('.'+lx1+',',_lxsele)>0
    this.Option1.SpecialEffect=1
    this.Option1.FontBold=.F.
    this.Option1.ForeColor=RGB(255,0,0)
  endif
endif
if _xidiom>1
  for n_bot=1 to this.ButtonCount
    lx='this.Option'+ltrim(str(n_bot))+'.Caption'
    _linea=&lx
    &lx=f3_t(_linea)
  endfor
endif

ENDPROC
PROCEDURE Option1.Click
if _psql
  lx=this.ControlSource
  nd=at('.',lx)
  if nd>0
    lx=subs(lx,nd+1)
  endif
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.ForeColor=RGB(255,0,0)
    this.FontBold=.F.
    =sq3_camp('I',lx)
  else
    this.SpecialEffect=0
    this.ForeColor=RGB(0,0,0)
    this.FontBold=.T.
    =sq3_camp('E',lx)
  endif
endif
ENDPROC


EndDefine 
Define Class st_bot as commandbutton
AutoSize = .T.
Height = 23
Width = 84
FontName = "MS Sans Serif"
FontSize = 8
Caption = "Command1"



PROCEDURE Init
if _xidiom>1
  _linea=f3_t(this.caption)
  this.caption=trim(_linea)
  _linea=f3_t(this.Tooltiptext)
  this.Tooltiptext=trim(_linea)
endif
if _psql=.T.
  this.enabled=.F.
endif

ENDPROC
PROCEDURE GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC


EndDefine 
Define Class st_getg as textbox
Height = 24
Width = 113



EndDefine 
Define Class st_edit as editbox
FontBold = .F.
FontName = "Courier"
Height = 56
Margin = 0
ScrollBars = 2
Width = 101



PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.ScrollBars=0
    this.SpecialEffect=1
    =sq3_camp('I',this.name)
  else
    this.ScrollBars=2
    this.SpecialEffect=0
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC
PROCEDURE Init
if _psql
  This.ReadOnly=.T.
  this.enabled=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.ScrollBars=0
  else
    this.ScrollBars=2
  endif
else
  this.alto=this.height
endif

ENDPROC


EndDefine 
Define Class f_aux as form
Height = 250
Width = 448
ShowTips = .T.
AutoCenter = .T.
Caption = "Form"
Closable = .T.
WindowType = 1
AlwaysOnTop = .F.
BackColor = Rgb(255,255,128)
literal_1 = ""
literal_2 = ""
literal_3 = ""

ADD OBJECT bot_salir as commandbutton WITH Top = 217, Left = 415, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", ToolTipText = "Salir"
ADD OBJECT l_negra as line WITH Height = 0, Left = 375, Top = 211, Width = 72
ADD OBJECT l_blanca as line WITH Height = 0, Left = 375, Top = 212, Width = 72, BorderColor = Rgb(255,255,255)
ADD OBJECT ayuda as image WITH Picture = "bmp\ayuda.bmp", Stretch = 0, BackStyle = 0, BorderStyle = 0, Enabled = .T., Height = 21, Left = 11, Top = 221, Visible = .F., Width = 12
ADD OBJECT bot_ok as commandbutton WITH Top = 217, Left = 383, Height = 29, Width = 29, Picture = "bmp\ok.bmp", Caption = "", ToolTipText = "Aceptar [CTRL+ENTER]"


PROCEDURE itr
Parameter _itr
*
return

ENDPROC
PROCEDURE Unload
on key
_xobjw='W'+trim(prog)
if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'CERRAR'
endif
lx1='G'+trim(sysprg.PRG_PROG)
if used(lx1)
  select alias (lx1)
  lx1=dbf()
  use
  delete file (lx1)
endif
select SYSPRG

ENDPROC
PROCEDURE Load
select SYSPRG
store .F. to _bot_ok,bot_ok
prog=thisform.name
do case
case PRG_LIN>0 .or. PRG_COL>0
  thisform.autocenter=.F.
  thisform.top=PRG_LIN
  thisform.left=PRG_COL
case thisform.autocenter=.T.
  thisform.autocenter=.T.
endcase
_linea=PRG_TIT
if _xidiom>1
  _linea=f3_t(_linea)
endif
thisform.caption=_linea
thisform.BorderStyle=2
thisform.l_negra.left=2
thisform.l_negra.width=thisform.width-4
thisform.l_blanca.left=2
thisform.l_blanca.width=thisform.width-4
_inicio=.T.
_xobjw='W'+trim(prog)
if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'ABRIR'
endif
select SYSPRG

ENDPROC
PROCEDURE Activate
if finalmenu=.T.
  thisform.release
  return
endif
store 0 to _xier,_nerrores
on key
on key label 'ESC'        do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok('O')
on key label 'F9' 		  do =f3_hlp()
on key label 'F2'		  do =f3_limpia()
on key label 'F3'		  do =f3_tecl(3)
on key label 'F4'		  do =f3_tecl(4)
on key label 'F5'		  do =f3_tecl(5)
on key label 'F6'		  do =f3_tecl(6)
on key label 'F7'		  do =f3_tecl(7)
on key label 'F8'		  do =f3_tecl(8)
on key label 'F10'		  do =f3_tecl(10)
on key label 'F11'		  do =f3_tecl(11)
on key label 'F12'		  do =f3_tecl(12)
if at(_cse,_usrmaxp)>0
  on key label 'SHIFT+F2' do =f3_posicion()
endif
if !empty(_progant)
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=_progant
      lx1='sysmemo.PRG_M'+ltrim(str(nd))
      save all except _* to memo &lx1
      exit
    endif
  endfor
endif
prog=upper(this.name)
_progant=''
do case
case _fn>0
  lx1='FN'+str(_fn,1)
  _fn=0
  thisform.itr(lx1)
case _enter=.T.
  _enter=.F.
  thisform.bot_ok.click
case _esc=.T.
  _esc=.F.
  thisform.bot_salir.click
endcase
select SYSPRG
seek prog
_lockh=0
if _inicio
  thisform.inicio
endif
store .F. to _inicio,_controlc
thisform.refresh

ENDPROC


PROCEDURE bot_salir.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_salir.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_salir.Click
bot_ok=.F.
thisform.release

ENDPROC
PROCEDURE bot_salir.Init
if _xidiom>1
  this.ToolTipText=f3_t(this.ToolTipText)
endif
ENDPROC


PROCEDURE bot_ok.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_ok.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_ok.Init
if _xidiom>1
  this.ToolTipText=f3_t(this.ToolTipText)
endif
ENDPROC
PROCEDURE bot_ok.Click
store .T. to bot_ok,_correcto_aux,_bot_ok
_lxerr=''
if !empty(sysprg.PRG_CI)
  =f3_ci()
endif
if empty(_lxerr)
  if !empty(thisform.literal_1)
    do form st3sn with 2,1,thisform.literal_1,thisform.literal_2,thisform.literal_3
    _correcto_aux=iif(conf='S',.T.,.F.)
  endif
  if _correcto_aux
    _xobjw='W'+trim(prog)
    if file(_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
      do (_xobjw) with 'PROCESO'
    endif
  endif
endif
do case
case !empty(_lxerr)
  do form st3inc
case _correcto_aux
  thisform.release
endcase
select SYSPRG
ENDPROC


EndDefine 
Define Class st_combo as combobox
FontBold = .F.
FontName = "Courier"
Height = 19
Margin = 2
Style = 2
Width = 112
DisabledItemBackColor = Rgb(192,192,192)
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)
DisabledItemForeColor = Rgb(0,0,128)



PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=19
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif

ENDPROC
PROCEDURE Init
if _psql
  this.RowSourceType=0
  this.RowSource=''
  this.enabled=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif

ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if nKeyCode=13
  keyboard chr(9)
endif
ENDPROC


EndDefine 
Define Class st_geti as textbox
FontBold = .F.
FontName = "Courier"
Enabled = .F.
Height = 18
Margin = 0
Width = 113
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)
rango_fec = 24
formato = ""
formato_bak = ""



PROCEDURE RangeLow
nd=this.rango_fec
_rangelow=this.value
if nd<>0 .and. _psql=.F. .and. type('_rangelow')='D' .and. dtoc(_rangelow)<>'  /  /    '
  _rangelow=gomonth(date(),-nd)
endif
return _rangelow

ENDPROC
PROCEDURE RangeHigh
nd=this.rango_fec
_rangehigh=this.value
if nd<>0 .and. _psql=.F. .and. type('_rangehigh')='D' .and. dtoc(_rangehigh)<>'  /  /    '
  _rangelow=gomonth(date(),nd)
endif
return _rangehigh
ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if this.backcolor=RGB(0,0,128)
  lx_tecla='/'+ltrim(str(nKeyCode))+'/'
  if at(lx_tecla,'/1/3/4/5/6/9/10/13/15/18/19/24/27/29/')=0
    lx_valor=this.value
    do case
    case type('lx_valor')='C'
      this.value=''
    case type('lx_valor')='N'
      this.value=0
    case type('lx_valor')='D'
      this.value={  /  /  }
    endcase
  endif
  this.backcolor=RGB(255,255,255)
  this.forecolor=RGB(0,0,0)
endif
ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
*
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE LostFocus
if !empty(this.formato_bak)
  this.InputMask=this.formato_bak
else
  this.InputMask=''
endif
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE GotFocus
this.formato_bak=this.InputMask
if !empty(this.formato)
  this.InputMask=this.formato
endif
_nvarh=upper(this.name)
_itroldval=&_nvarh
_nvarh=''
this.backcolor=RGB(0,0,128)
this.forecolor=RGB(255,255,255)
ENDPROC
PROCEDURE Init
************
Private _aaa, _ccc

on error a = 1
_ccc = 'This.ControlSource'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      This.AddProperty('_Control', .F.)

      _aaa = &_ccc
      Public &_aaa
      If Type('&_aaa') = 'L'
         Store '' To &_aaa
         This._Control = .T.
      EndIf
   EndIf
EndIf

on error
************

if _psql
  This.enabled=.T.
  This.ReadOnly=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif

ENDPROC
PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC
PROCEDURE Destroy
************
Private _aaa, _ccc

on error a = 1
_ccc = 'This.ControlSource'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      If This._Control
         _aaa = &_ccc
******   Release &_aaa
      EndIf
   EndIf
EndIf

on error
************

ENDPROC


EndDefine 
Define Class st_box as shape
Height = 68
Width = 68
BackStyle = 0
BorderWidth = 1
SpecialEffect = 0



PROCEDURE cuadro
Parameter lx1,lx5,nd1,nd_top,nd_left,nd_width,nd_height
*
lx_path=iif(upper(left(lx1,4))='PAGE','.pageframe1.'+lx1,'')
lx3='thisform'+lx_path+'.AddObject("L1_'+lx5+'","LINE")'
&lx3
lx4='thisform'+lx_path+'.AddObject("L2_'+lx5+'","LINE")'
&lx4
lx1='thisform'+lx_path+'.L1_'+lx5
lx2='thisform'+lx_path+'.L2_'+lx5
with &lx1
  .top=nd_top
  .left=nd_left
  if nd1=16
    .Bordercolor=RGB(128,128,128)
  else
    .Bordercolor=RGB(255,255,255)
  endif
  .Height=nd_Height
  .Width=0
  .visible=.T.
endwith
with &lx2
  .top=nd_top
  .left=nd_left
  if nd1=16
    .Bordercolor=RGB(128,128,128)
  else
    .Bordercolor=RGB(255,255,255)
  endif
  .Height=0
  .Width=nd_Width
  .visible=.T.
endwith
*
return
ENDPROC
PROCEDURE Init
nd1=this.ColorScheme
if nd1=16
  this.bordercolor=RGB(255,255,255)
else
  this.bordercolor=RGB(128,128,128)
endif
if nd1=16 .or. nd1=17
  this.SpecialEffect=1
*  thisform.cuadro(this.parent.name,this.name,nd1,this.top,this.left,this.width,this.height)
*  this.cuadro(this.parent.name,this.name,nd1,this.top,this.left,this.width,this.height)
endif
ENDPROC


EndDefine 
Define Class st_say3d as textbox
FontBold = .F.
FontName = "Courier"
Enabled = .F.
Height = 18
Margin = 0
Width = 113
BackColor = Rgb(255,255,255)
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)



PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC
PROCEDURE Init
if _psql
  This.enabled=.T.
  This.ReadOnly=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif

ENDPROC


EndDefine 
Define Class st_get as textbox
FontBold = .F.
FontName = "Courier"
Alignment = 3
Height = 18
Margin = 0
Width = 113
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)
rango_fec = 24
formato = ""
formato_bak = ""



PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC
PROCEDURE GotFocus

Private nStado, oGet

Store 0 To nStado

this.formato_bak=this.InputMask
if !empty(this.formato)
  this.InputMask=this.formato
endif

_nvarh=upper(this.name)
_itroldval=&_nvarh
_nvarh=''

_xobjw = 'X' + alltrim(upper(_progant))
oGet = this
if file (_xobjw + '.PRG') .Or.  file(_xobjw + '.FXP')
   do (_xobjw) with 'FOCUS', .T., oGet
endif

this.backcolor=RGB(0,0,128)
this.forecolor=RGB(255,255,255)

Return nStado

ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
*
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE LostFocus

Private nStado, oGet

Store 0 To nStado

if !empty(this.formato_bak)
  this.InputMask=this.formato_bak
else
  this.InputMask=''
endif

_xobjw = 'X' + alltrim(upper(_progant))
oGet = this
if file (_xobjw + '.PRG') .Or.  file(_xobjw + '.FXP')
   do (_xobjw) with 'FOCUS', .F., oGet
endif

this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

Return nStado

ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if this.backcolor=RGB(0,0,128)
  lx_tecla='/'+ltrim(str(nKeyCode))+'/'
  if at(lx_tecla,'/1/3/4/5/6/9/10/13/15/18/19/24/27/29/')=0
    lx_valor=this.value
    do case
    case type('lx_valor')='C'
      this.value=''
    case type('lx_valor')='N'
      this.value=0
    case type('lx_valor')='D'
      this.value={  /  /  }
    endcase
  endif
  this.backcolor=RGB(255,255,255)
  this.forecolor=RGB(0,0,0)
endif
ENDPROC
PROCEDURE Init
if _psql
  This.ReadOnly=.T.
  This.enabled=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif
ENDPROC


EndDefine 
Define Class st_frame as pageframe
ErasePage = .T.
PageCount = 1
Width = 405
Height = 155
release = .T.
Page1.FontName = "MS Sans Serif"
Page1.FontSize = 8
Page1.Caption = "Page1"
Page1.ForeColor = Rgb(0,0,128)



PROCEDURE Init
for n_pag=1 to this.PageCount
  lxpag='this.Page'+ltrim(str(n_pag))
  lx=lxpag+'.Caption'
  _linea=&lx
  if _xidiom>1
    _linea=f3_t(_linea)
  endif
  _linea='\<'+ltrim(str(n_pag))+' '+_linea

  lx=lxpag+'.Caption'
  &lx=_linea
  lx=lxpag+'.Fontname'
  &lx='Arial'
  lx=lxpag+'.Fontsize'
  &lx=8
  lx=lxpag+'.Forecolor'
  &lx=RGB(0,0,128)
  if _psql=.T.
    lx=lxpag+'.Backcolor'
    &lx=RGB(255,255,128)
  endif

   ************
   Private _inx, _aaa, _bbb, _ccc, _ddd1, _ddd2

   on error a = 1
   _bbb = lxpag + '.ControlCount'
   For _Inx = 1 To &_bbb
      _ccc = lxpag + '.Controls(_inx).ControlSource'
      If Type('&_ccc') # 'U'
         If !Empty(&_ccc)
            _ddd1 = lxpag + ".Controls(_inx).Name"
            _ddd1 = &ddd1
            _ddd1 = lxpag + ".Controls(_inx)." + _ddd1 + ".AddProperty('_Control', .F.)"
            &_ddd1

            _aaa = &_ccc
            Public &_aaa
            If Type('&_aaa') = 'L'
               Store '' To &_aaa
               _ddd2 = lxpag + ".Controls(_inx).Name._Control"
               &_ddd2 = .T.
            EndIf
         EndIf
      EndIf
   EndFor

   on error
   ************
endfor

ENDPROC
PROCEDURE Destroy
************
for n_pag=1 to this.PageCount
  lxpag='this.Page'+ltrim(str(n_pag))
  Private _inx, _aaa, _bbb, _ccc

  on error a = 1
  _bbb = lxpag + '.ControlCount'
  For _Inx = 1 To &_bbb
     _ccc = lxpag + '.Controls(_inx).ControlSource'
     If Type('&_ccc') # 'U'
        If !Empty(&_ccc)
           _aaa = &_ccc
           If This.Release
*              Release &_aaa
           EndIf
        EndIf
     EndIf
  EndFor

  on error

endfor
************

ENDPROC


EndDefine 
Define Class st_getf as textbox
FontBold = .F.
FontName = "Courier"
StrictDateEntry = 0
Height = 18
Margin = 0
Width = 113
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)
rango_fec = 1200



PROCEDURE Refresh
_nvar_date=upper(this.name)
_valor_date=&_nvar_date
if isnull(_valor_date)
  &_nvar_date=DToC({  /  /    })
endif
ENDPROC
PROCEDURE Init
if _psql
  This.ReadOnly=.T.
  This.enabled=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif

ENDPROC
PROCEDURE RangeLow
nd=this.rango_fec
_rangelow=this.value
if nd<>0 .and. _psql=.F. .and. !empty(_rangelow)
  _rangelow=dtot(gomonth(date(),-nd))
endif
return _rangelow
ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if this.backcolor=RGB(0,0,128)
  lx_tecla='/'+ltrim(str(nKeyCode))+'/'
  if at(lx_tecla,'/1/3/4/5/6/9/10/13/15/18/19/24/27/29/')=0
    lx_valor=this.value
    this.value=dtot({  /  /    })
  endif
  this.backcolor=RGB(255,255,255)
  this.forecolor=RGB(0,0,0)
endif

ENDPROC
PROCEDURE LostFocus
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
*
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE RangeHigh
nd=this.rango_fec
_rangehigh=this.value
if nd<>0 .and. _psql=.F. .and. !empty(_rangehigh)
  _rangehigh=dtot(gomonth(date(),nd))
endif
return _rangehigh

ENDPROC
PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif

ENDPROC
PROCEDURE GotFocus
_nvarh=upper(this.name)
_itroldval=&_nvarh
if isnull(_itroldval)
  &_nvarh=DToC({  /  /    })
endif
_nvarh=''
this.backcolor=RGB(0,0,128)
this.forecolor=RGB(255,255,255)

ENDPROC


EndDefine 
Define Class barra_iconos as toolbar
Caption = "Accesos directos"
Height = 62
Left = 0
Top = 0
Width = 173

ADD OBJECT Shape0 as shape WITH Top = 5, Left = 5, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_1 as st_bot WITH AutoSize = .F., Top = 5, Left = 11, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape1 as shape WITH Top = 5, Left = 35, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_2 as st_bot WITH AutoSize = .F., Top = 5, Left = 41, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape2 as shape WITH Top = 5, Left = 65, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_3 as st_bot WITH AutoSize = .F., Top = 5, Left = 71, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape3 as shape WITH Top = 5, Left = 95, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_4 as st_bot WITH AutoSize = .F., Top = 5, Left = 101, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape4 as shape WITH Top = 5, Left = 125, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_5 as st_bot WITH AutoSize = .F., Top = 5, Left = 131, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape5 as shape WITH Top = 5, Left = 155, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_6 as st_bot WITH AutoSize = .F., Top = 29, Left = 5, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape6 as shape WITH Top = 29, Left = 29, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_7 as st_bot WITH AutoSize = .F., Top = 29, Left = 35, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape7 as shape WITH Top = 29, Left = 59, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_8 as st_bot WITH AutoSize = .F., Top = 29, Left = 65, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape8 as shape WITH Top = 29, Left = 89, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_9 as st_bot WITH AutoSize = .F., Top = 29, Left = 95, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape9 as shape WITH Top = 29, Left = 119, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT prg_10 as st_bot WITH AutoSize = .F., Top = 29, Left = 125, Height = 25, Width = 25, Caption = "", Visible = .F.
ADD OBJECT Shape10 as shape WITH Top = 29, Left = 149, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT Shape11 as shape WITH Top = 29, Left = 155, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT Shape12 as shape WITH Top = 29, Left = 161, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT Shape13 as shape WITH Top = 53, Left = 5, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT Shape14 as shape WITH Top = 53, Left = 11, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .F.
ADD OBJECT Shape15 as shape WITH Top = 53, Left = 17, Height = 5, Width = 7, BackStyle = 0, BorderStyle = 0, Visible = .T.


PROCEDURE Show
LPARAMETERS nStyle
*
select SYSUSER
lx=USR_ICON+space(10)
select SYSPRG
store 0 to num,nd
do while !empty(lx)
  num=num+1
  lx1=left(lx,12)
  if left(lx1,1)='*'
    lx2='thisform.Shape'+ltrim(str(num-1))+'.visible=.T.'
    &lx2
  endif
  lx1=subs(lx1,3)
  lx=subs(lx,13)
  seek lx1
  if !eof()
    lx2='thisform.prg_'+ltrim(str(num))+'.ToolTipText="'+trim(sysprg.PRG_TIT)+' ['+trim(lx1)+']"'
    &lx2
    if empty(PRG_ICON)
      lx2='thisform.prg_'+ltrim(str(num))+'.Picture="BMP/LINEAS.BMP"'
    else
      lx2='thisform.prg_'+ltrim(str(num))+'.Picture="'+trim(PRG_ICON)+'"'
    endif
    &lx2
    lx2='thisform.prg_'+ltrim(str(num))+'.visible=.T.'
    &lx2
  endif
enddo

ENDPROC


PROCEDURE prg_1.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_2.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_3.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_4.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_5.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_6.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_7.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_8.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_9.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


PROCEDURE prg_10.Click
lx=this.ToolTipText
nd1=at('[',lx)
nd2=at(']',lx)
if nd1>0 .and. nd2>nd1
  lx=subs(lx,nd1+1,nd2-nd1-1)
  =f3_bot(lx,'ST3PRG')
endif
ENDPROC


EndDefine 
Define Class st_geth as textbox
FontBold = .F.
FontName = "Courier"
FontUnderline = .F.
Height = 18
Margin = 0
Width = 113
ForeColor = Rgb(0,0,0)
BackColor = Rgb(255,255,255)
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)
BorderColor = Rgb(0,0,0)
rango_fec = 24
formato = ""
formato_bak = ""



PROCEDURE Destroy
************
Private _aaa, _ccc

on error a = 1
_ccc = 'This.ControlSource'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      If This._Control
         _aaa = &_ccc
******   Release &_aaa
      EndIf
   EndIf
EndIf

on error
************

ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
*
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if this.backcolor=RGB(0,0,128)
  lx_tecla='/'+ltrim(str(nKeyCode))+'/'
  if at(lx_tecla,'/1/3/4/5/6/9/10/13/15/18/19/24/27/29/')=0
    lx_valor=this.value
    do case
    case type('lx_valor')='C'
      this.value=''
    case type('lx_valor')='N'
      this.value=0
    case type('lx_valor')='D'
      this.value={  /  /  }
    endcase
  endif
  this.backcolor=RGB(255,255,255)
  this.forecolor=RGB(0,0,0)
endif
ENDPROC
PROCEDURE GotFocus

Private nStado, oGet

Store 0 To nStado

this.formato_bak=this.InputMask
if !empty(this.formato)
  this.InputMask=this.formato
endif
_nvarh=upper(this.name)
_itroldval=&_nvarh

if _psql=.F. .And. This.Class != "Codpro"
   thisform.ayuda.visible=.T.
   This.StatusBarText = f3_t('Pulse F9 ó botón derecho para consulta')
endif

_xobjw = 'X' + alltrim(upper(_progant))
oGet = this
if file (_xobjw + '.PRG') .Or.  file(_xobjw + '.FXP')
   do (_xobjw) with 'FOCUS', .T., oGet
endif

this.backcolor=RGB(0,0,128)
this.forecolor=RGB(255,255,255)

Return nStado

ENDPROC
PROCEDURE RightClick
if _psql=.F.
  =f3_hlp()
endif
ENDPROC
PROCEDURE LostFocus

Private nStado, oGet

Store 0 To nStado

if !empty(this.formato_bak)
  this.InputMask=this.formato_bak
else
  this.InputMask=''
endif

_nvarh=''
thisform.ayuda.visible=.F.
This.StatusBarText = ' * '

_xobjw = 'X' + alltrim(upper(_progant))
oGet = this
if file (_xobjw + '.PRG') .Or.  file(_xobjw + '.FXP')
   do (_xobjw) with 'FOCUS', .F., oGet
endif

this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

Return nStado

ENDPROC
PROCEDURE Init
************
Private _aaa, _ccc

on error a = 1
_ccc = 'This.ControlSource'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      This.AddProperty('_Control', .F.)

      _aaa = &_ccc
      Public &_aaa
      If Type('&_aaa') = 'L'
         Store '' To &_aaa
         This._Control = .T.
      EndIf
   EndIf
EndIf

on error
************

if _psql
  This.enabled=.T.
  This.ReadOnly=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif

ENDPROC
PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC
PROCEDURE RangeHigh
nd=this.rango_fec
_rangehigh=this.value
if nd<>0 .and. _psql=.F. .and. type('_rangehigh')='D' .and. dtoc(_rangehigh)<>'  /  /    '
  _rangelow=gomonth(date(),nd)
endif
return _rangehigh
ENDPROC
PROCEDURE RangeLow
nd=this.rango_fec
_rangelow=this.value
if nd<>0 .and. _psql=.F. .and. type('_rangelow')='D' .and. dtoc(_rangelow)<>'  /  /    '
  _rangelow=gomonth(date(),-nd)
endif
return _rangelow

ENDPROC


EndDefine 
Define Class f_pant2 as form
Top = 0
Left = 0
Height = 425
Width = 633
ShowTips = .T.
Caption = "Form"
WindowState = 0
BackColor = Rgb(192,192,192)
bloqueo_opt = .T.
caja_h = 0
creg_fc = ""
creg_r1 = ""
exp_clave = ""
grid_h = 0
grid_personalizado = .F.
n_reg_alta = 0
n_registro = 0
rec_registro = .T.
valor_clave = ""
n_registro2 = 0
reordenar = .T.
bot_estado = ""

ADD OBJECT Shape1 as shape WITH Top = 377, Left = 24, Height = 29, Width = 91, BackStyle = 0, SpecialEffect = 0, BackColor = Rgb(255,0,0)
ADD OBJECT bot_top as commandbutton WITH Top = 382, Left = 30, Height = 21, Width = 21, Picture = "bmp\top.bmp", DisabledPicture = "fctmp\", Caption = "", TabIndex = 13, TabStop = .F., ToolTipText = "Inicio"
ADD OBJECT bot_ant as commandbutton WITH AutoSize = .F., Top = 382, Left = 50, Height = 21, Width = 21, Picture = "bmp\prior.bmp", Caption = "", TabIndex = 14, TabStop = .F., ToolTipText = "Anterior"
ADD OBJECT bot_sig as commandbutton WITH Top = 382, Left = 70, Height = 21, Width = 21, Picture = "bmp\next.bmp", Caption = "", TabIndex = 15, TabStop = .F., ToolTipText = "Siguiente"
ADD OBJECT bot_bottom as commandbutton WITH Top = 382, Left = 90, Height = 21, Width = 21, Picture = "bmp\bottom.bmp", DisabledPicture = "fctmp\", Caption = "", TabIndex = 16, TabStop = .F., ToolTipText = "Final"
ADD OBJECT bot_modi as commandbutton WITH Comment = "", Top = 378, Left = 135, Height = 29, Width = 29, Picture = "bmp\graba.bmp", Caption = "", TabIndex = 2, ToolTipText = "Grabar [CTRL+TAB]"
ADD OBJECT bot_baja as commandbutton WITH AutoSize = .F., Top = 378, Left = 165, Height = 29, Width = 29, Picture = "bmp\baja.bmp", Caption = "", TabIndex = 3, ToolTipText = "Baja"
ADD OBJECT Bot_salir as commandbutton WITH Top = 378, Left = 594, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 11, ToolTipText = "Salir"
ADD OBJECT bot_blanc as commandbutton WITH Top = 378, Left = 223, Height = 29, Width = 29, Picture = "bmp\altac.bmp", DisabledPicture = "fctmp\", Caption = "", TabIndex = 5, ToolTipText = "Altas "
ADD OBJECT C_lock as checkbox WITH Top = 378, Left = 194, Height = 29, Width = 29, Picture = "bmp\unlock.bmp", DownPicture = "bmp\lock.bmp", Caption = "", Value = 0, ControlSource = "m.c_lock", Style = 1, TabIndex = 4, TabStop = .F., ToolTipText = "Bloquear registro"
ADD OBJECT l_negra as line WITH Height = 0, Left = 552, Top = 373, Width = 72
ADD OBJECT l_blanca as line WITH Height = 0, Left = 552, Top = 374, Width = 72, BorderColor = Rgb(255,255,255)
ADD OBJECT bot_rec as commandbutton WITH Top = 378, Left = 252, Height = 29, Width = 29, Picture = "bmp\back.bmp", Caption = "", TabIndex = 6, ToolTipText = "Deshacer", Visible = .T.
ADD OBJECT Pageframe1 as pageframe WITH ErasePage = .T., PageCount = 1, Top = 24, Left = 0, Width = 631, Height = 312, TabIndex = 1, Page1.FontName = "MS Sans Serif", Page1.FontSize = 8, Page1.FontUnderline = .F., Page1.Caption = "Page1", Page1.ForeColor = Rgb(0,0,128)
ADD OBJECT "Pageframe1.Page1.Grid1" as grid WITH ColumnCount = 1, FontBold = .F., FontName = "Courier New", FontSize = 8, Enabled = .T., GridLines = 2, HeaderHeight = 15, Height = 164, Highlight = .T., Left = 8, Panel = 1, ReadOnly = .T., RecordMark = .F., RowHeight = 16, TabIndex = 1, TabStop = .F., Top = 1, Width = 612, ForeColor = Rgb(0,0,255), Column1.FontBold = .F., Column1.FontName = "Courier New", Column1.FontSize = 8, Column1.Alignment = 0, Column1.Enabled = .T., Column1.ReadOnly = .T., Column1.ForeColor = Rgb(0,0,255)
ADD OBJECT "Pageframe1.Page1.Grid1.Column1.Header1" as header WITH Caption = "Header1"
ADD OBJECT "Pageframe1.Page1.Grid1.Column1.Text1" as textbox WITH FontBold = .F., FontName = "Courier New", FontSize = 8, Alignment = 0, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,255), BackColor = Rgb(255,255,255), SelectedForeColor = Rgb(0,0,255), SelectedBackColor = Rgb(255,255,255)
ADD OBJECT "Pageframe1.Page1.caja_lin" as st_box WITH Top = 174, Left = 8, Height = 102, Width = 613, BackStyle = 0, FillStyle = 0, Visible = .T., BackColor = Rgb(255,255,255), FillColor = Rgb(255,255,128)
ADD OBJECT Listados as combobox WITH FontBold = .F., FontName = "Courier New", FontSize = 9, BoundColumn = 1, ControlSource = "_listado", Height = 19, Left = 315, Margin = 2, Sorted = .F., Style = 2, TabIndex = 8, TabStop = .F., ToolTipText = "Listados [ALT+F2]", Top = 383, Visible = .F., Width = 167
ADD OBJECT Ayuda as image WITH Picture = "bmp\ayuda.bmp", Stretch = 2, BackStyle = 0, BorderStyle = 0, Enabled = .T., Height = 21, Left = 118, Top = 383, Visible = .F., Width = 12, ToolTipText = "Consultar fichero"
ADD OBJECT bot_lint as commandbutton WITH AutoSize = .F., Top = 382, Left = 1, Height = 21, Width = 21, Picture = "bmp\linterna.bmp", Caption = "", TabIndex = 12, TabStop = .F., ToolTipText = "Ver fichero"
ADD OBJECT bot_ok as commandbutton WITH Top = 378, Left = 564, Height = 29, Width = 29, Picture = "bmp\ok.bmp", Caption = "", Enabled = .T., TabIndex = 10, ToolTipText = "Confirmar [CTRL+ENTER]", Visible = .T.
ADD OBJECT bot_persiana as commandbutton WITH Top = 378, Left = 535, Height = 29, Width = 29, Picture = "bmp\bajap.bmp", Caption = "", Enabled = .F., TabIndex = 9, ToolTipText = "Bajar ventana líneas", Visible = .T.
ADD OBJECT bot_reord as commandbutton WITH Top = 378, Left = 281, Height = 29, Width = 29, Picture = "bmp\reord.bmp", Caption = "", Enabled = .F., TabIndex = 6, ToolTipText = "Reordenar líneas", Visible = .T.


PROCEDURE itr
Parameter _itr
*
return

ENDPROC
PROCEDURE estado_normal
if thisform.Shape1.Backcolor=RGB(0,128,0) .and. thisform.pageframe1.page1.caja_lin.visible=.T.
  store .T. to _altacl
  _modi_lin=.F.
  select (_xfc2)
  scatter memvar blank memo
  select (_xfc)
  scatter memvar
  thisform._l_alta.Caption=f3_t('+ L í n e a')
  thisform.Shape1.Backcolor=RGB(0,0,128)
  thisform.bot_rec.enabled=.T.
  thisform.pageframe1.page1.grid1.DeleteMark=iif(thisform.no_bajal=.T..or.thisform.no_modi=.T.,.F.,.T.)
  thisform.bot_baja.enabled=.T.
  thisform.c_lock.enabled=.T.
  thisform.bot_persiana.enabled=.T.
  thisform.bot_reord.enabled=.T.
  thisform.bot_ok.enabled=.F.

  if thisform.no_altal=.T.
     thisform.bot_persiana.click
  endif

  thisform.iniciol
  thisform.refresh
  select SYSPRG
endif

ENDPROC
PROCEDURE bot_visible
lx=thisform.bot_estado
lx=trim(lx)
do while !empty(lx)
  nd=at(',',lx)
  if nd>0
    lx1='thisform.'+left(lx,nd-1)+'.visible'
    lx=subs(lx,nd+1)
  else
    lx1='thisform.'+lx+'.visible'
    lx=''
  endif
  &lx1=.T.
enddo
thisform.bot_estado=''

ENDPROC
PROCEDURE bot_invisible
Parameter botones_si,botones_no
*
lx=thisform.bot_estado
if empty(lx)
  lx='bot_modi,bot_baja,c_lock,bot_blanc,bot_rec,bot_reord,bot_persiana,bot_ok,bot_top,bot_ant,bot_sig,bot_bottom,bot_lint'
  if type('botones_si')='C' .and. !empty(botones_si)
    lx=lx+','+botones_si
  endif
  if type('botones_no')='C'
    botones_no=botones_no+','
    nd=at(',',botones_no)
    do while !empty(botones_no)
      nd=at(',',botones_no)
      if nd>0
        lx1=left(botones_no,nd-1)
        botones_no=subs(botones_no,nd+1)
      else
        lx1=botones_no
        botones_no=''
      endif
      nd=at(lx1,lx)
      if nd>0
        lx=stuff(lx,nd,len(lx1),'')
      endif
    enddo
  endif
  nd=at(',,',lx)
  do while nd>0
    lx=stuff(lx,nd,1,'')
    nd=at(',,',lx)
  enddo
  if right(lx,1)=','
    lx=left(lx,len(lx)-1)
  endif
  lx2=''
  do while !empty(lx)
    nd=at(',',lx)
    if nd>0
      lx3=left(lx,nd-1)
      lx1='thisform.'+lx3+'.visible'
      lx=subs(lx,nd+1)
    else
      lx3=lx
      lx1='thisform.'+lx3+'.visible'
      lx=''
    endif
    if &lx1=.T.
      lx2=lx2+lx3+','
      &lx1=.F.
    endif
  enddo
  if right(lx2,1)=','
    lx2=left(lx2,len(lx2)-1)
  endif
  thisform.bot_estado=lx2
endif

ENDPROC
PROCEDURE adoconn_access

*>
*> Conectar a la DB mediante ADO a partir de ODBC.

*> Conexión ADO ya creada.
If Type('This.ADoConn') # 'O'
   =ThisForm.InitAdo()
EndIf

Return ThisForm.AdoConn


ENDPROC
PROCEDURE adors_access
*>
*> Conectar a la DB mediante ADO a partir de ODBC.

*> Conexión ADO ya creada.
If Type('This.ADoRs') # 'O'
   =ThisForm.InitAdo()
EndIf

Return ThisForm.AdoRs

ENDPROC
PROCEDURE initado
*> initado
*> Conectar a la DB mediante ADO a partir de ODBC.

Local cDSN

cDSN = SqlGetProp(_ASql, 'ConnectString')			&& DSN de ODBC.

With This
   .AdoConn = CreateObject('ADODB.Connection')		&& Objeto conexión.
   .AdoRs   = CreateObject('ADODB.RecordSet')		&& Objeto RecordSet.

   .AdoConn.ConnectionString = cDSN
   .AdoConn.Open()
EndWith

=ThisForm.OpenRs()

ENDPROC
PROCEDURE ofoxado_access
*>
*> Crear objeto de la clase FOXADO.

If Type('This.oFOXADO') # 'O'
   This.oFOXADO = CreateObject('foxado')
EndIf

Return This.oFOXADO



ENDPROC
PROCEDURE openrs
*>openrs

*>
*> Crear el RS de consulta del fichero.

#Include "adovfp.h"
#Include "dbf2rs.h"

Private cSQL

If This.AdoRs.State > 0
   This.AdoRs.Close
EndIf

cSQL = "Select * From " + _xfc + _em

=This.AdoRs.Open(cSQL, This.AdoConn.ConnectionString, ADOPENSTATIC, ADLOCKREADONLY)

ENDPROC
PROCEDURE Activate
prog=upper(this.name)
do case
case finalmenu=.T.
  thisform.release
  return
case _psql=.T.
  return
endcase
_controlc=iif(Empty(_xier),_controlc,.T.)
store 0 to _xier,_nerrores
on key
on key label 'ESC'        do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok('O')
on key label 'CTRL+TAB'   do =f3_ok('G')
on key label 'CTRL+HOME'  do =f3_nav('I')
on key label 'CTRL+PGUP'  do =f3_nav('A')
on key label 'CTRL+PGDN'  do =f3_nav('S')
on key label 'CTRL+END'   do =f3_nav('F')
on key label 'F9' 		  do =f3_hlp()
on key label 'F2'		  do =f3_limpia()
on key label 'F3'		  do =f3_tecl(3)
on key label 'F4'		  do =f3_tecl(4)
on key label 'F5'		  do =f3_tecl(5)
on key label 'F6'		  do =f3_tecl(6)
on key label 'F7'		  do =f3_tecl(7)
on key label 'F8'		  do =f3_tecl(8)
on key label 'F10'		  do =f3_tecl(10)
on key label 'F11'		  do =f3_tecl(11)
on key label 'F12'		  do =f3_tecl(12)
if at(_cse,_usrmaxp)>0
  on key label 'SHIFT+F2' do =f3_posicion()
endif
if !empty(_progant)
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=_progant
      lx1='sysmemo.PRG_M'+ltrim(str(nd))
      save all except _* to memo &lx1
      exit
    endif
  endfor
endif
prog=upper(this.name)
_progant=prog
if thisform.listados.visible=.T.
  _listado=thisform.listados.list(1)
  on key label 'ALT+F2'  do =f3_lst()
endif
do case
case _fn>0
  _controlc=.F.
  lx1='FN'+str(_fn,1)
  thisform.itr(lx1)
case !empty(_botlst)
  _controlc=.F.
  _botlst=''
  thisform.listados.setfocus
  keyboard chr(32)
case !empty(_botnav)
  do case
  case _altac
    do form st3sn with 1,2,'No se puede utilizar las teclas de navegación en ALTAS'
  case thisform.c_lock.value=1
    do form st3sn with 1,2,'No se puede utilizar las teclas de navegación con la ficha bloqueada'
  case _botnav='I'
    thisform.bot_top.click
  case _botnav='A'
    thisform.bot_ant.click
  case _botnav='S'
    thisform.bot_sig.click
  case _botnav='F'
    thisform.bot_bottom.click
  endcase
  store '' to _botnav
  _controlc=.F.
case _esc=.T.
  store .F. to _esc,_controlc
  do case
  case thisform.no_alta=.T. .and. thisform.bot_top.visible=.T.
    thisform.release
    return
  case thisform.Shape1.Backcolor=RGB(0,128,0)
    _modi_lin=.F.
    thisform.pageframe1.page1.grid1.DeleteMark=iif(thisform.no_bajal=.T..or.thisform.no_modi=.T.,.F.,.T.)
    thisform.bot_baja.enabled=.T.
    thisform.c_lock.enabled=.T.
    thisform.bot_persiana.enabled=.T.
    thisform.bot_reord.enabled=.T.
    thisform.bot_ok.enabled=.T.
    select (_xfc2)
    scatter memvar blank memo
    select (_xfc)
    scatter memvar memo
    _altac=.F.
    do case
    case thisform.pageframe1.page1.caja_lin.visible=.F.
      thisform.bot_persiana.click
    case thisform.no_altal=.T. .and. thisform.pageframe1.page1.caja_lin.visible=.T.
      thisform.bot_persiana.click
    otherwise
      thisform._l_alta.Caption=f3_t('+ L í n e a')
      thisform.Shape1.Backcolor=RGB(0,0,128)
    endcase
    _altacl=.T.
    _inicio_lalta=.T.
    thisform.iniciol
    _inicio_lalta=.F.
    _lxfocus=trim(_1ernvarl)+'.setfocus'
    select SYSPRG
    thisform.refresh
    &_lxfocus
    return
  otherwise
    if eof(_xfc)
      select (_xfc)
      go top
      select SYSPRG
    endif
    if (atc('FALTAC.BMP',thisform.bot_blanc.picture)>0.or.thisform.c_lock.value=1).and.!eof(_xfc)
      if thisform.no_alta=.T.
        thisform.c_lock.value=0
        thisform.c_lock.valid
      else
        thisform.bot_blanc.click
      endif
    else
      thisform.bot_salir.click
    endif
  endcase
case _enter=.T.
  store .F. to _enter,_controlc
  if thisform.bot_ok.enabled=.T.
    thisform.bot_ok.click
  endif
case _graba=.T.
  store .F. to _graba,_controlc
  if thisform.bot_modi.enabled=.T.
    thisform.bot_modi.click
  endif
endcase

select SYSPRG
seek prog
_xfc=mline(sysprg.PRG_FICH,1)
_xfc=ltrim(chrtran(_xfc,'*',''))
_xfc2=mline(sysprg.PRG_FICH,2)
_xfc2=ltrim(chrtran(_xfc2,'*',''))
_xfctmp=thisform.pageframe1.page1.Grid1.RecordSource
m.c_lock=thisform.c_lock.value
_1ernvar=PRG_1er
if empty(_1ernvar)
  _1ernvar='thisform.bot_salir'
endif
_1ernvarl=PRG_1erl
if empty(_1ernvarl)
  _1ernvarl=_1ernvar
endif
_altac=iif(thisform.Shape1.Backstyle=1.and.thisform.Shape1.Backcolor=RGB(255,0,0),.T.,.F.)
do case
case _inicio=.T.
  select (_xfc)
  thisform.exp_clave=''
  =f3_bottom(_xfc)
  _controlc=.F.
  nd=iif(eof(),0,1)
  thisform.n_registro=nd
  if eof()
    select SYSPRG
  endif
  thisform.valor_clave=''

  do case
  case nd=0 .and. sysprg.PRG_AVIS=1
    =f3_sn(1,1,'Este fichero no contiene ninguna ficha')
    if thisform.no_alta=.T.
      thisform.release
      return
    endif
    thisform.bot_blanc.click
  case nd=0 .or. sysprg.PRG_ALTA=1 .and. thisform.no_alta=.F.
    thisform.bot_blanc.click
  otherwise
    thisform.bot_persiana.click
    thisform.inicio
  endcase
  select SYSPRG
case _controlc=.T.
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=prog
      lx1='sysmemo.PRG_M'+ltrim(str(nd))
      restore from memo &lx1 addi
      exit
    endif
  endfor

  _controlc=.F.
  lx1=thisform.creg_fc
  lx1=alltrim(lx1)
  lx2=thisform.creg_r1
  lx2=lx2+space(10)
  do while !empty(lx1)
    nd=at(']',lx1)
    lx3=subs(lx1,2,nd-2)
    lx1=iif(nd=len(lx1),'',subs(lx1,nd+1))
    r1ls=val(left(lx2,10))
    lx2=subs(lx2,11)
    select (lx3)
    gather memvar memo
  enddo

  select SYSPRG
  thisform.inicio
  thisform.iniciol
case _fn>0
  _fn=0
case _controlc=.F.
  select (_xfc)
  thisform.inicio
endcase

_controlc=.T.
_inicio=.F.
_inicio_alta=.F.
thisform.refresh
if _cambio_e=.T.
  thisform.deactivate
endif
select SYSPRG

ENDPROC
PROCEDURE Resize
if this.closable=.T.
  this.closable=.F.
else
  this.closable=.T.
endif

ENDPROC
PROCEDURE Unload
if _psql
  return
endif
on key
prog=upper(this.name)
select SYSPRG
seek prog
_xfc=mline(PRG_FICH,1)
_xfc=ltrim(chrtran(_xfc,'*',''))
select (_xfc)
if !eof()
  unlock
endif
_alig='G'+trim(sysprg.PRG_PROG)
select alias(_alig)
lx=dbf()
use
delete file (lx)
select SYSPRG
_progant=''
=f3_cerrar(upper(this.name))

ENDPROC
PROCEDURE Load
select SYSPRG
if _psql=.T.
  this.WindowType=1
  this.Backcolor=RGB(255,255,128)
else
  thisform.creg_fc=_c_reg
  do case
  case PRG_LIN>0 .or. PRG_COL>0
    thisform.autocenter=.F.
    thisform.top=PRG_LIN
    thisform.left=PRG_COL
  case thisform.autocenter=.T.
    thisform.autocenter=.T.
  endcase
  _logmem=_logmem+'Entrada : '+dtoc(date())+' '+time()+' '+PRG_PROG+' '+PRG_TIT+cr
  if atc(_cse,PRG_ASEGL)>0 .or. PRG_ASEGL='TODOS'
    thisform.no_altal=.T.
    thisform.bot_blanc.visible=.F.
    thisform.bot_persiana.visible=.F.
    thisform.no_alta=.T.
  endif
  if atc(_cse,PRG_ASEG)>0 .or. PRG_ASEG='TODOS'
    thisform.bot_blanc.visible=.F.
    thisform.no_alta=.T.
  endif
  if atc(_cse,PRG_MSEG)>0 .or. PRG_MSEG='TODOS'
    thisform.pageframe1.page1.grid1.DeleteMark=.F.
    thisform.bot_modi.visible=iif(thisform.no_alta=.T.,.F.,.T.)
    thisform.c_lock.visible=.F.
    thisform.bot_reord.visible=.F.
    thisform.bot_rec.visible=.F.
    thisform.no_modi=.T.
  endif
  if atc(_cse,PRG_MSEGL)>0 .or. PRG_MSEGL='TODOS'
    thisform.no_modil=.T.
  endif
  if atc(_cse,PRG_BSEG)>0 .or. PRG_BSEG='TODOS'
    thisform.bot_baja.visible=.F.
    thisform.no_baja=.T.
  endif
  if atc(_cse,PRG_BSEGL)>0 .or. PRG_BSEGL='TODOS'
    thisform.pageframe1.page1.grid1.DeleteMark=.F.
    thisform.no_bajal=.T.
  endif
endif
_linea=PRG_TIT
if _xidiom>1
  _linea=f3_t(_linea)
endif
thisform.icon=iif(Empty(_Icono), '', _Icono)
thisform.caption=_linea
thisform.BorderStyle=2
thisform.l_negra.left=2
thisform.l_negra.width=thisform.width-4
thisform.l_blanca.left=2
thisform.l_blanca.width=thisform.width-4
if thisform.bloqueo_opt=.F.
  thisform.bot_modi.enabled=.F.
  thisform.bot_baja.enabled=.F.
endif
nd1=thisform.pageframe1.page1.grid1.height
nd2=thisform.pageframe1.page1.caja_lin.height
thisform.grid_h=nd1
thisform.caja_h=nd2
if _psql=.F.
  r1ls=recno()
  lx=trim(PRG_PROG)
  lx2=PRG_PROG
  nd=len(lx)
  skip
  do while !eof() .and. left(PRG_PROG,nd)=lx .and. len(trim(PRG_PROG))=nd+1
    if PRG_TIPO='L' .and. PRG_OK=.T.
      if thisform.listados.visible=.F.
        thisform.listados.visible=.T.
        _listado=PRG_TIT+' '+PRG_PROG
      endif
      thisform.listados.additem(PRG_TIT+' '+PRG_PROG)
    endif
    skip
  enddo
  set order to IDX2
  seek lx2
  do while !eof() .and. LS_PRG=lx2
    if PRG_TIPO='L' .and. PRG_OK=.T.
      if thisform.listados.visible=.F.
        thisform.listados.visible=.T.
        _listado=PRG_TIT+' '+PRG_PROG
      endif
      thisform.listados.additem(PRG_TIT+' '+PRG_PROG)
    endif
    skip
  enddo
  set order to IDX1
  go r1ls
  _inicio=.T.
  store '' to _botcont,_botnav
  _xobjw='W'+trim(prog)
  if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
    do (_xobjw) with 'ABRIR',,_progant
  endif
endif
select SYSPRG

ENDPROC


PROCEDURE Shape1.Init
thisform.AddObject("_L_ALTA","LABEL")
nd_top=this.top
nd_left=this.left
with thisform._l_alta
  .Caption=f3_t(' A ñ a d i r')
  .BackStyle=0
  .Top=nd_top+7
  .left=nd_left+9
  .Autosize=.T.
  .ForeColor=RGB(255,255,255)
  .Fontname='MS Sans Serif'
  .Fontbold=.T.
  .Fontsize=10
endwith

ENDPROC


PROCEDURE bot_top.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_top.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_top.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_top.Click
*> Si no hay un tipo de acceso a datos definido, por defecto se usa ODBC
If Type('_ADO_SN') # 'C'
	_ADO_SN='O'
EndIf

Do Case
Case _ADO_SN=='A'
	ThisForm.AdoRs.MoveFirst
	*> Pasa el registro actual del RS al DBF.
	If !ThisForm.AdoRs.Bof()
	   Select (_xfc)
	   Zap
	   =ThisForm.oFOXADO.AddRecordFromRS(ThisForm.AdoRs, _xfc, .F.)
	   Scatter Memvar Memo
	   ThisForm.inicio
	   ThisForm.Refresh
	EndIf
*> Conexión con ODBC
Case _ADO_SN=='O'
	select(_xfc)
	=f3_top(_xfc)
	if !eof()
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  thisform.inicio
	  thisform.refresh
	else
	  wait window f3_t('Fichero vacío') timeout .8
	endif
	select SYSPRG
*Case _ADO_SN=='R'
	*> No implementada, opción para RDB
*> Por defecto conexión ODBC
OtherWise
	select(_xfc)
	=f3_top(_xfc)
	if !eof()
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  thisform.inicio
	  thisform.refresh
	else
	  wait window f3_t('Fichero vacío') timeout .8
	endif
	select SYSPRG
EndCase

ENDPROC


PROCEDURE bot_ant.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_ant.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_ant.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_ant.Click
*> Si no hay un tipo de acceso a datos definido, por defecto se usa ODBC
If Type('_ADO_SN') # 'C'
	_ADO_SN='O'
EndIf

Do Case
*> Conexión con ADO
Case _ADO_SN=='A'
	If ThisForm.AdoRs.Bof() .Or. ThisForm.AdoRs.Eof()
	   =WaitWindow('Principio de fichero')
	   Return
	EndIf
	ThisForm.AdoRs.MovePrevious
	If ThisForm.AdoRs.Bof()
	   =WaitWindow('Principio de fichero')
	   ThisForm.AdoRs.MoveFirst
	EndIf
	*> Pasa el registro actual del RS al DBF.
	If !ThisForm.AdoRs.Bof()
	   Select (_xfc)
	   Zap
	   =ThisForm.oFOXADO.AddRecordFromRS(ThisForm.AdoRs, _xfc, .F.)
	   Scatter Memvar Memo
	   ThisForm.inicio
	   ThisForm.Refresh
	EndIf
*> Conexión con ODBC
Case _ADO_SN=='O'
	select (_xfc)
	=f3_ant(_xfc)
	if eof()
	  thisform.bot_top.click
	else
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  thisform.inicio
	  thisform.refresh
	endif
	select SYSPRG
*Case _ADO_SN=='R'
	*> No implementada, opción para RDB
*> Por defecto conexión ODBC
OtherWise
	select (_xfc)
	=f3_ant(_xfc)
	if eof()
	  thisform.bot_top.click
	else
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  thisform.inicio
	  thisform.refresh
	endif
	select SYSPRG
EndCase


ENDPROC


PROCEDURE bot_sig.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_sig.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_sig.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_sig.Click
*> Si no hay un tipo de acceso a datos definido, por defecto se usa ODBC
If Type('_ADO_SN') # 'C'
	_ADO_SN='O'
EndIf

Do Case
*> Conexión con ADO
Case _ADO_SN=='A'
	If ThisForm.AdoRs.Bof() .Or. ThisForm.AdoRs.Eof()
	   =WaitWindow('Final de fichero')
	   Return
	EndIf
	ThisForm.AdoRs.MoveNext
	If ThisForm.AdoRs.Eof()
	   =WaitWindow('Final de fichero')
	   ThisForm.AdoRs.MoveLast
	EndIf
	*> Pasa el registro actual del RS al DBF.
	If !ThisForm.AdoRs.Eof()
	   Select (_xfc)
	   Zap
	   =ThisForm.oFOXADO.AddRecordFromRS(ThisForm.AdoRs, _xfc, .F.)
	   Scatter Memvar Memo
	   ThisForm.inicio
	   ThisForm.Refresh
	EndIf
*> Conexión con ODBC
Case _ADO_SN=='O'
	select (_xfc)
	=f3_pos(_xfc)
	if eof()
	  thisform.bot_bottom.click
	endif
	if !eof()
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  thisform.inicio
	  thisform.refresh
	endif
	select SYSPRG
*Case _ADO_SN=='R'
	*> No implementada, opción para RDB
*> Por defecto conexión ODBC
OtherWise
	select (_xfc)
	=f3_pos(_xfc)
	if eof()
	  thisform.bot_bottom.click
	endif
	if !eof()
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  thisform.inicio
	  thisform.refresh
	endif
	select SYSPRG
EndCase



ENDPROC


PROCEDURE bot_bottom.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_bottom.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_bottom.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_bottom.Click
*> Si no hay un tipo de acceso a datos definido, por defecto se usa ODBC
If Type('_ADO_SN') # 'C'
	_ADO_SN='O'
EndIf

Do Case
*> Conexión con ADO
Case _ADO_SN=='A'
	ThisForm.AdoRs.MoveLast
	*> Pasa el registro actual del RS al DBF.
	If !ThisForm.AdoRs.Eof()
	   Select (_xfc)
	   Zap
	   =ThisForm.oFOXADO.AddRecordFromRS(ThisForm.AdoRs, _xfc, .F.)
	   Scatter Memvar Memo
	   ThisForm.inicio
	   ThisForm.Refresh
	EndIf

*> Conexión con ODBC
Case _ADO_SN=='O'
	select(_xfc)
	=f3_bottom(_xfc)
	if !eof()
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  if _inicio=.F.
	    thisform.inicio
	    thisform.refresh
	  endif
	else
	  wait window f3_t('Fichero vacío') timeout .8
	endif
	select SYSPRG
*Case _ADO_SN=='R'
	*> No implementada, opción para RDB
*> Por defecto conexión ODBC
OtherWise
	select(_xfc)
	=f3_bottom(_xfc)
	if !eof()
	  select (_xfc2)
	  scatter memvar blank memo
	  select (_xfc)
	  scatter memvar memo
	  if _inicio=.F.
	    thisform.inicio
	    thisform.refresh
	  endif
	else
	  wait window f3_t('Fichero vacío') timeout .8
	endif
	select SYSPRG
EndCase



ENDPROC


PROCEDURE bot_modi.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_modi.Click
_lxfocus=trim(_1ernvarl)+'.setfocus'
store '' to _lxerr,_lxerr2
if !empty(sysprg.PRG_CI)
  =f3_ci()
  _lxfocus=iif(empty(_lxerr),_lxfocus,trim(_1ernvar)+'.setfocus')
endif
store .F. to _ya_alta,_ya_modi
store 0 to r1_alta,r2_alta,_xier
_xobjwc='X'+trim(_xfc)
_xobjw='X'+trim(_xfc2)
* ------------------- Alta de cabecera  --------------------------------
select (_xfc)
do case
case !empty(_lxerr)
  do form st3inc
case _altac
  if empty(_lxerr)
    =f3_ci('C2')
    _lxerr2=_lxerr
  endif
  if empty(_lxerr) .and. (file(_xobjwc+'.PRG') .or.  file(_xobjwc+'.FXP'))
    select alias (_xfc)
    do (_xobjwc) with 'ALTA',.T., _progant
  endif
  if !empty(_lxerr)
    do form st3inc
  else
    select (_xfc)
    r1_alta=iif(eof(),0,recno())
    if _ya_alta=.F.
      =f3_ins(_xfc)
      _lxfocus=trim(_1ernvarl)+'.setfocus'
    endif
    thisform.n_reg_alta=recno()
    for nd=1 to memlines(sysprg.PRG_DESC)
      lx=mline(sysprg.PRG_DESC,nd)
      if !empty(lx)
        lx=lx+'.enabled=.F.'
        &lx
      endif
    endfor
    if _xier=0 .and. (file(_xobjwc+'.PRG') .or.  file(_xobjwc+'.FXP'))
      select (_xfc)
      do (_xobjwc) with 'ALTA',.F.,_progant
    endif
  endif
case eof()
  =f3_sn(1,2,'Está intentando modificar un registro inexistente')
  _lxfocus=trim(_1ernvar)+'.setfocus'
  _xier=1
endcase
* ---------------- Alta o modificar línea ----------------------------
store .F. to _ya_alta,_ya_modi
if empty(_lxerr) .and. _xier=0 .and. thisform.pageframe1.page1.caja_lin.visible=.T.
  if !empty(sysprg.PRG_CIL)
    =f3_ci('C2')
  endif
  do case
  case !empty(_lxerr)
*    wait window f3_t('Línea no actualizada por falta de datos')
    _lxerr=''
  case _altacl
    if file(_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
      select alias (_xfc2)
      do (_xobjw) with 'ALTA',.T.,_progant
    endif
    if !empty(_lxerr)
      do form st3inc
    else
      select (_xfc2)
      r2_alta=iif(eof(),0,recno())
      if _ya_alta=.F.
        =f3_ins(_xfc2)
        _lxfocus=trim(_1ernvarl)+'.setfocus'
      endif
      exp_1=recno()
      if _xier=0 .and. _ya_alta=.F.
        insert into (_xfctmp) from memvar
      endif
      if file(_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
        do (_xobjw) with 'ALTA',.F.,_progant
        select alias (_xfc2)
      endif
      if empty(_lxerr) .and. (file (_xobjwc+'.PRG') .or.  file(_xobjwc+'.FXP'))
        select alias(_xfc)
        do (_xobjwc) with 'LINEA',.F.,_progant
      endif
      select (_xfc2)
    endif
  case _altacl=.F. .and. eof(_xfc2)
*    =f3_sn(1,2,'Está intentando modificar una línea inexistente')
  case _altacl=.F.
    if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
      select alias (_xfc2)
      do (_xobjw) with 'MODI',.T.,_progant
    endif
    if !empty(_lxerr)
      do form st3inc
    else
      select (_xfc2)
*     _r1l=recno()
      if _ya_modi=.F.
*       scatter memvar
        =f3_upd(_xfc2)
      endif
      if _xier=0
        if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
          do (_xobjw) with 'MODI',.F.,_progant
        endif
        if empty(_lxerr) .and. (file (_xobjwc+'.PRG') .or.  file(_xobjwc+'.FXP'))
          select (_xfc)
          do (_xobjwc) with 'LINEA',.F.,_progant
        endif
        select alias (_xfctmp)
*       Go Top
*       locate for EXP_1=_r1l
        gather memvar memo
      endif
    endif
  endcase
endif
* ---------------- Modificar Cabecera -------------------------------
store .F. to _ya_alta,_ya_modi
if empty(_lxerr) .and. _xier=0
  if file(_xobjwc+'.PRG') .or.  file(_xobjwc+'.FXP')
    select alias (_xfc)
    do (_xobjwc) with 'MODI',.T.,_progant
  endif
  if !empty(_lxerr)
    do form st3inc
  else
    select (_xfc)
    if eof()
      =f3_sn(1,2,'Está intentando modificar un registro inexistente')
      _lxfocus=trim(_1ernvar)+'.setfocus'
      _xier=1
    else
      if _ya_modi=.F.
        =f3_upd(_xfc)
      endif
    endif
    if _xier=0 .and. (file(_xobjwc+'.PRG') .or.  file(_xobjwc+'.FXP'))
      do (_xobjwc) with 'MODI',,_progant
      select alias (_xfc)
    endif
  endif
endif
* -------------- Final de grabación ---------------------------------
do case
case empty(_lxerr) .and. _xier=0
  do case
  *> Alta de cabecera.
  case thisform.Shape1.Backcolor=RGB(255,0,0)
    thisform.bot_persiana.enabled=.T.
    thisform.bot_reord.enabled=.T.
    thisform.Shape1.Backstyle=1
    thisform._l_alta.Caption=f3_t('+ L í n e a')
    thisform.Shape1.Backcolor=RGB(0,0,128)
    thisform.bot_ok.enabled=.T.
    thisform.bot_rec.enabled=.T.
    thisform.bot_baja.enabled=.T.
    thisform.bot_blanc.enabled=.T.

    thisform.c_lock.value=1
    thisform.c_lock.enabled=.T.
    thisform.bot_blanc.tooltiptext=f3_t('Ficha nueva')
    lx=thisform.bot_blanc.picture
    nd=atc('FALTAC.BMP',lx)
    if nd>0
      lx=stuff(lx,nd,10,'ALTAC.BMP')
      thisform.bot_blanc.picture=lx
    endif
  *> Modificación de línea.
  case thisform.Shape1.Backcolor=RGB(0,128,0)
    _modi_lin=.F.
    thisform.pageframe1.page1.grid1.DeleteMark=iif(thisform.no_bajal=.T..or.thisform.no_modi=.T.,.F.,.T.)
    thisform.bot_baja.enabled=.T.
    thisform.bot_blanc.enabled=.T.
    thisform.c_lock.enabled=.T.
    thisform.bot_persiana.enabled=.T.
    thisform.bot_reord.enabled=.T.
    thisform.bot_salir.enabled=.T.
    thisform.bot_lint.enabled=.T.

    thisform.Shape1.Backstyle=0
    thisform._l_alta.visible=.F.
    thisform.bot_top.visible=.T.
    thisform.bot_sig.visible=.T.
    thisform.bot_ant.visible=.T.
    thisform.bot_bottom.visible=.T.
    thisform.bot_persiana.click

*    if thisform.pageframe1.page1.caja_lin.visible=.T.
*      thisform._l_alta.Caption=f3_t('+ L í n e a')
*      thisform.Shape1.Backcolor=RGB(0,0,128)
*    endif
  endcase

  if thisform.no_altal=.T. .and. thisform.pageframe1.page1.caja_lin.visible=.T.
    thisform.bot_persiana.click
  endif

  select (_xfc2)
  scatter memvar blank memo
  select (_xfc)
  scatter memvar memo
  if _altac=.T.
    _altac=.F.
    thisform.inicio
  endif

  _altacl=.T.
  _inicio_lalta=.T.
  thisform.iniciol
  _inicio_lalta=.F.
  thisform.refresh
case r1_alta>0
  select (_xfc)
  go r1_alta
case r2_alta>0
  select (_xfc2)
  go r2_alta
endcase

_lxerr=''
_xier=0
select SYSPRG
&_lxfocus

ENDPROC
PROCEDURE bot_modi.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_modi.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC


PROCEDURE bot_baja.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_baja.Click
if eof(_xfc)
  do form st3sn with 1,2,'Está intentando eliminar un registro inexistente'
else
  _ya_baja=.F.
  prog=wontop()
  _lxerr=''
  _xobjw='X'+trim(_xfc)
  if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
    select alias (_xfc)
    do (_xobjw) with 'BAJA',.T.,_progant
  endif
  do case
  case _ya_baja

  case !empty(_lxerr)
    do form st3inc
  case lock()
    _ok=.F.
    do form st3bajac
    if _xier=0 .and. _ok=.T.
      _ya_baja=.T.
      if (file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP'))
        do (_xobjw) with 'OK_BAJA',.F.,_progant
        select alias (_xfc)
      endif
      thisform.bot_sig.click
    endif
    _ok=.T.
    _xier=0
  otherwise
    do form st3sn with 1,2,'La ficha está bloqueada por otro usuario','No puede eliminarla hasta que se desbloquee'
  endcase
  if _ya_baja
    _ya_baja=.F.
    select alias (_xfc)
    if !eof()
      scatter memvar memo
      thisform.c_lock.value=0
      thisform.c_lock.valid
      _lxfocus=trim(_1ernvar)+'.setfocus'
      thisform.inicio
      thisform.refresh
      &_lxfocus
    else
      =f3_sn(1,1,'Fichero vacío, Paso a modo de añadir fichas')
      thisform.bot_blanc.enabled=.T.
      thisform.bot_blanc.click
    endif
    _inicio_alta=.F.
    thisform.refresh
  endif
endif
select SYSPRG

ENDPROC
PROCEDURE bot_baja.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_baja.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC


PROCEDURE Bot_salir.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC
PROCEDURE Bot_salir.Click
if !ThisForm._Cerrar .Or. f3_sn(2, 3, 'Desea usted abandonar el proceso?')
   thisform.release
EndIf

ENDPROC
PROCEDURE Bot_salir.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE Bot_salir.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC


PROCEDURE bot_blanc.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_blanc.Click
thisform.bot_blanc.enabled=.T.
thisform.bot_persiana.enabled=.F.
thisform.bot_reord.enabled=.F.
thisform.pageframe1.page1.grid1.DeleteMark=iif(thisform.no_bajal=.T..or.thisform.no_modi=.T.,.F.,.T.)
_lxfocus=trim(_1ernvar)+'.setfocus'
if (atc('FALTAC.BM',this.picture)>0.and.thisform.Shape1.Backcolor=RGB(255,0,0)).or._cambio_e=.T.
  select (_xfc)
  if eof()
    =f3_top(_xfc)
  endif
  r1=thisform.n_reg_alta
  if r1=0 .and. !eof()
    r1=recno()
    thisform.n_reg_alta=r1
  endif
  if r1>0
    _altac=.F.
    this.tooltiptext=f3_t('Ficha nueva')
    thisform.Shape1.Backstyle=0
    thisform._l_alta.visible=.F.
    thisform.bot_top.visible=.T.
    thisform.bot_ant.visible=.T.
    thisform.bot_sig.visible=.T.
    thisform.bot_bottom.visible=.T.
    thisform.bot_ok.enabled=.T.
    thisform.bot_rec.enabled=.T.
    thisform.bot_lint.enabled=.T.
    if thisform.bloqueo_opt=.F.
      thisform.bot_modi.enabled=.F.
      thisform.bot_baja.enabled=.F.
    else
      thisform.bot_modi.enabled=.T.
      thisform.bot_baja.enabled=.T.
    endif
    thisform.bot_modi.visible=iif(thisform.no_modi=.T.,.F.,.T.)
    thisform.c_lock.enabled=.T.
    lx=this.picture
    nd1=atc('FALTAC.BMP',lx)
    nd2=atc('FALTAC.BMX',lx)
    do case
    case nd1>0
      lx=stuff(lx,nd1,10,'ALTAC.BMP')
      this.picture=lx
    case nd2>0
      lx=stuff(lx,nd2,10,'ALTAC.BMX')
      this.picture=lx
    endcase
    for nd=1 to memlines(sysprg.PRG_DESC)
      lx=mline(sysprg.PRG_DESC,nd)
      if !empty(lx)
        lx=lx+'.enabled=.F.'
        &lx
      endif
    endfor
    select alias (_xfc)
    r1=thisform.n_reg_alta
    if r1>0
      go r1
      scatter memvar memo
    endif
    if thisform.pageframe1.page1.caja_lin.visible=.T.
      thisform.bot_persiana.click
    endif
  else
    =f3_sn(1,1,'Fichero vacío, No puede entrar en modo de modificación')
    select SYSPRG
    &_lxfocus
    return
  endif
else
  store .T. to _altac,_altacl
  this.tooltiptext=f3_t('Fin de la alta')
  select alias (_xfc2)
  scatter memvar blank memo
  select alias (_xfc)
  r1=iif(eof(),0,recno())
  thisform.n_reg_alta=r1
  scatter memvar blank memo
  thisform.Shape1.Backstyle=1
  thisform._l_alta.Caption=f3_t(' A ñ a d i r')
  thisform.Shape1.Backcolor=RGB(255,0,0)
  thisform._l_alta.visible=.T.
  thisform.bot_top.visible=.F.
  thisform.bot_ant.visible=.F.
  thisform.bot_sig.visible=.F.
  thisform.bot_bottom.visible=.F.
  thisform.bot_rec.enabled=.F.
  thisform.bot_lint.enabled=.F.
  thisform.bot_ok.enabled=.F.
  thisform.bot_modi.enabled=.T.
  thisform.bot_modi.visible=.T.
  thisform.bot_baja.enabled=.F.
  thisform.c_lock.value=0
  thisform.c_lock.enabled=.F.
  lx=this.picture
  nd1=atc('\ALTAC.BMP',lx)
  nd2=atc('\ALTAC.BMX',lx)
  do case
  case nd1>0
    lx=stuff(lx,nd1,10,'\FALTAC.BMP')
    this.picture=lx
  case nd2>0
    lx=stuff(lx,nd2,10,'\FALTAC.BMX')
    this.picture=lx
  endcase
  for nd=1 to memlines(sysprg.PRG_DESC)
    lx=mline(sysprg.PRG_DESC,nd)
    if !empty(lx)
      lx=lx+'.enabled=.T.'
      &lx
    endif
  endfor
  if thisform.pageframe1.page1.caja_lin.visible=.F.
    thisform.bot_persiana.click
  endif
  _inicio_alta=.T.
endif
thisform.inicio
thisform.iniciol
* En VFP6 da error : &_lxfocus
_inicio_alta=.F.
thisform.refresh
select SYSPRG

ENDPROC
PROCEDURE bot_blanc.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_blanc.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC


PROCEDURE C_lock.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE C_lock.Valid
_lxfocus=''
select (_xfc)
do case
*> No bloquear registro.
case thisform.c_lock.value=0
  =f3_unlock(_xfc)                 && Desbloqueo BD.
  unlock                           && Desbloqueo DBF.

  thisform.Shape1.Backstyle=0
  thisform._l_alta.visible=.F.
  thisform.bot_top.visible=.T.
  thisform.bot_sig.visible=.T.
  thisform.bot_ant.visible=.T.
  thisform.bot_bottom.visible=.T.
  thisform.bot_persiana.enabled=.F.
  thisform.bot_reord.enabled=.F.
  thisform.bot_ok.enabled=.F.
  thisform.bot_salir.enabled=.T.
  thisform.bot_blanc.enabled=.T.
  this.ToolTipText = f3_t('Alta línea')

  if thisform.bloqueo_opt=.F.
    thisform.bot_modi.enabled=.F.
    thisform.bot_baja.enabled=.F.
  else
    thisform.bot_modi.enabled=.T.
    thisform.bot_baja.enabled=.T.
  endif

  if thisform.pageframe1.page1.caja_lin.visible=.T.
    thisform.bot_persiana.click
  endif
  lx=thisform.bot_blanc.picture
  nd=atc('FALTAC.BMP',lx)
  if nd>0
    lx=stuff(lx,nd,10,'ALTAC.BMP')
    thisform.bot_blanc.picture=lx
  endif
  thisform.bot_blanc.tooltiptext=f3_t('Altas')
*> Fichero vacío.
case eof()
  thisform.c_lock.value=0
  =f3_sn(1,2,'Está intentando bloquear un registro inexistente')
*> Registro bloqueado por otro usuario.
case !f3_rlock(_xfc)
  =f3_sn(1,2,'El registro está bloqueado por otro usuario','Espere a que se libere para bloquearlo')
  thisform.c_lock.value=0
*> Por defecto bloquea el registro actual.
otherwise
  thisform.bot_top.visible=.F.
  thisform.bot_sig.visible=.F.
  thisform.bot_ant.visible=.F.
  thisform.bot_bottom.visible=.F.
  thisform.Shape1.Backstyle=1
  thisform._l_alta.visible=.T.
  thisform.bot_persiana.enabled=.T.
  thisform.bot_reord.enabled=.T.
  thisform.bot_ok.enabled=.F.
  thisform.bot_salir.enabled=.F.
  thisform.bot_reord.enabled=.T.
  thisform.bot_blanc.enabled=.F.

  thisform._l_alta.Caption=f3_t('+ L í n e a')
  thisform.Shape1.Backcolor=RGB(0,0,128)
  this.ToolTipText = f3_t('Fin alta línea')

*  if thisform.bloqueo_opt=.F.
*    thisform.bot_modi.enabled=.T.
*    thisform.bot_baja.enabled=.T.
*  endif

  thisform.bot_modi.enabled=.T.
  thisform.bot_baja.enabled=.T.
  thisform.pageframe1.Activepage=1

  do case
  case thisform.no_altal=.T. .and. thisform.pageframe1.page1.caja_lin.visible=.T.
    thisform.bot_persiana.click
  case thisform.no_altal=.T.
    thisform._l_alta.Caption=f3_t('Mod.cabec')
    thisform.Shape1.Backcolor=RGB(0,128,0)
  case thisform.pageframe1.page1.caja_lin.visible=.F.
    thisform.bot_persiana.click
  endcase

  _altacl=.T.
  select (_xfc2)
  scatter memvar blank memo
  select (_xfc)
  scatter memvar memo
  select SYSPRG
  thisform.iniciol
  _lxfocus=trim(_1ernvarl)+'.setfocus'
endcase
_lockh=thisform.c_lock.value
thisform.refresh
select SYSPRG
if !empty(_lxfocus)
  &_lxfocus
endif

ENDPROC
PROCEDURE C_lock.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE C_lock.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC


PROCEDURE bot_rec.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC
PROCEDURE bot_rec.Click
_lxfocus=trim(_1ernvar)+'.setfocus'
select (_xfc2)
if !eof() .and. _altacl=.F. .and. thisform.pageframe1.page1.caja_lin.visible=.T.
  scatter memvar memo
  thisform.pageframe1.ActivePage=1
  thisform.iniciol
  _lxfocus=trim(_1ernvarl)+'.setfocus'
endif
select (_xfc)
if !eof()
  scatter memvar memo
  thisform.inicio
endif
select SYSPRG
&_lxfocus
thisform.refresh
&_lxfocus

ENDPROC
PROCEDURE bot_rec.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_rec.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC


PROCEDURE Pageframe1.Init
for n_pag=1 to this.PageCount
  lxpag='this.Page'+ltrim(str(n_pag))
  lx=lxpag+'.Caption'
  _linea=&lx
  if _xidiom>1
    _linea=f3_t(_linea)
  endif
  _linea='\<'+ltrim(str(n_pag))+' '+_linea
  lx=lxpag+'.Caption'
  &lx=_linea
  lx=lxpag+'.Fontname'
  &lx='Arial'
  lx=lxpag+'.Fontsize'
  &lx=8
  lx=lxpag+'.Forecolor'
  &lx=RGB(0,0,128)
  if _psql
    lx=lxpag+'.Backcolor'
    &lx=RGB(255,255,128)
  endif
endfor

ENDPROC


PROCEDURE Pageframe1.Page1.Grid1.Init
* Readaptar el Grid
*
if _psql
  this.ColumnCount=1
  this.enabled=.F.
  return
endif

systmp=_tmp+trim(sys(2015))
systmpf=systmp+'.FPT'
systmp=systmp+'.DBF'
_alig='G'+upper(trim(sysprg.PRG_PROG))
lx1='FCTMP\'+_alig+'.DBF'
copy file &lx1 to &systmp
lx1='FCTMP\'+_alig+'.FPT'
if file(lx1)
  copy file &lx1 to &systmpf
endif

use (systmp) in 0 alias (_alig) excl
lx1='this.RecordSource= "'+_alig+'"'
&lx1
ndlin=iif(thisform.reordenar=.T.,1,0)
totc=this.ColumnCount
tot=0

if thisform.grid_personalizado=.F.
  totc=iif(thisform.reordenar=.T.,totc+1,totc)
  for nd=1 to totc
    lx=mline(sysprg.PRG_GRID,nd)
    do case
    case nd=1 .and. thisform.reordenar=.T.
    case .not. empty(lx)
      tot=tot+1
      nd1=at(',',lx)
      m.FIELD_NAME=left(lx,nd1-1)
      lx=subs(lx,nd1+1)
      nd1=at(',',lx)
      lx1='this.column'+ltrim(str(tot))+'.header1.caption'
      &lx1=left(lx,nd1-1)
      lx=subs(lx,nd1+1)
      m.FIELD_TYPE=left(lx,1)
      lx=subs(lx,2)
      nd1=at(',',lx)
      nd2=at('.',lx)
      if nd1=0
        lx1=lx
        lx=''
      else
        lx1=left(lx,nd1-1)
        lx=subs(lx,nd1+1)
      endif
      do case
      case m.FIELD_TYPE='N' .and. nd2>0
        lng=val(left(lx1,nd2-1))
        lng=lng*7+7
      case m.FIELD_TYPE='D'
        lng=77
      case m.FIELD_TYPE='M'
        lng=32
      otherwise
        lng=val(lx1)
        lng=lng*7+7
      endcase
      m.FIELD_NAME=_alig+'.'+m.FIELD_NAME
      nd1=iif(!empty(lx),val(lx)*7+7,lng)
      lx1='this.column'+ltrim(str(tot))+'.width'
      &lx1=nd1
      lx1='this.column'+ltrim(str(tot))+'.Header1.Fontname'
      &lx1='Ms Sans Serif'
      lx1='this.column'+ltrim(str(tot))+'.Header1.FontSize'
      &lx1=8
      lx1='this.column'+ltrim(str(tot))+'.ControlSource'
      &lx1=m.FIELD_NAME
    endcase
  endfor
endif

if _xidiom>1
  _nlineas=this.ColumnCount
  for nd=1 to _nlineas
    lx='this.column'+ltrim(str(nd))+'.header1.caption'
    &lx=f3_t(&lx)
  endfor
endif
*this.SetAll("DynamicBackColor","iif(MOD(recno(),2)=0,RGB(255,255,255),RGB(0,255,255))", "Column")
select SYSPRG

ENDPROC
PROCEDURE Pageframe1.Page1.Grid1.Deleted
LPARAMETERS nRecNo
*
select (_xfctmp)
go nRecNo
scatter memvar
r1=EXP_1
if r1>0
  select (_xfc2)
  =f3_seek(_xfc2)

  select (_xfc2)
  if !eof()
    _xobjw='X'+trim(_xfc2)
    if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
      do (_xobjw) with 'BAJA',.T.,_progant
    endif
    select (_xfc2)
    if empty(_lxerr)
      =f3_baja(_xfc2)
      _xobjw='X'+trim(_xfc2)
      if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
        do (_xobjw) with 'BAJA',.F.,_progant
      endif
      _xobjw='X'+trim(_xfc)
      if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
        do (_xobjw) with 'BAJA_LIN',.F.,_progant
      endif
      =f3_upd(_xfc)
    endif
  endif

  select (_xfctmp)
* thisform.inicio
  activate window wnada
  deactivate window wnada
  select SYSPRG
else
  if !deleted()
    wait window f3_t('Esta línea no puede borrarse haciendo click con el mouse'+cr+'Probablemente sea un comentario o información histórica')
  endif
endif
select (_xfctmp)
thisform.refresh
_lxfocus=trim(_1ernvarl)+'.setfocus'
&_lxfocus

ENDPROC


PROCEDURE Pageframe1.Page1.Grid1.Column1.Text1.Click
*>
*> Click sobre la primera columna para modificar la línea actual
select (_xfctmp)
do case
*> No se permite modificar líneas.
case thisform.no_modil=.T.
  =f3_sn(1,1,'Atención, no tiene prioridad suficiente para modificar líneas')
*> Selecciona la línea actual.
case !eof() .and. exp_1>0
  if thisform.pageframe1.page1.caja_lin.visible=.F.
    thisform.bot_persiana.tooltiptext=f3_t('Bajar ventana líneas')
    lx=thisform.bot_persiana.picture
    nd=atc('SUBEP.BMP',lx)
    if nd>0
      lx=stuff(lx,nd,9,'BAJAP.BMP')
      thisform.bot_persiana.picture=lx
    endif
    thisform.pageframe1.page1.grid1.height=thisform.grid_h
    thisform.pageframe1.page1.caja_lin.visible=.T.

    *> Propiedad visible de los campos de la línea actual a visualizar.
    for nd=1 to memlines(sysprg.PRG_LVIS)
      lx=mline(sysprg.PRG_LVIS,nd)
      &lx
    endfor
  endif

  *> Bloquear documento.
  if !f3_rlock(_xfc)
     =f3_sn(1,2,'El registro está bloqueado por otro usuario','Espere a que se libere para bloquearlo')
     return
  endif

  *> Configurar botones.
  thisform.bot_baja.enabled=.F.
  thisform.c_lock.enabled=.F.
  thisform.bot_persiana.enabled=.F.
  thisform.bot_reord.enabled=.F.
  thisform.bot_ok.enabled=.F.
  thisform.bot_blanc.enabled=.F.
  thisform.bot_salir.enabled=.F.
  thisform.bot_lint.enabled=.F.

  thisform._l_alta.Caption=f3_t('Mod. línea')
  thisform.Shape1.Backcolor=RGB(0,128,0)

  if thisform.bot_top.visible=.T.
    thisform.Shape1.Backstyle=1
    thisform._l_alta.visible=.T.
    thisform.bot_top.visible=.F.
    thisform.bot_ant.visible=.F.
    thisform.bot_sig.visible=.F.
    thisform.bot_bottom.visible=.F.
  endif

  _modi_lin=.T.
  thisform.pageframe1.page1.grid1.DeleteMark=.F.
  select (_xfctmp)
  scatter memvar
  =f3_seek(_xfc2)

  select SYSPRG
  _altacl=.F.
  _lxfocus=trim(_1ernvarl)+'.setfocus'
  thisform.iniciol
  select SYSPRG
  thisform.refresh
  &_lxfocus
case !eof()

endcase

select SYSPRG

ENDPROC


PROCEDURE Listados.Valid
if lastkey()<>9
  select SYSPRG
  r1ls=recno()
  lx=right(this.value,10)
  seek alltrim(lx)
  if !eof()
    if wexist(lx)
      activate window (lx)
    else
      =f3_bot(PRG_PROG,'ST3PRG')
    endif
  else
    go r1ls
  endif
endif

ENDPROC


PROCEDURE bot_lint.Click
if _psql
  =sq3_lint()
  thisform.release
  return
endif
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx
=f3_hlpfc(_xfc)
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx
_lxfocus=trim(_1ernvar)+'.setfocus'
&_lxfocus

ENDPROC
PROCEDURE bot_lint.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC


PROCEDURE bot_ok.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_ok.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_ok.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC


PROCEDURE bot_persiana.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_persiana.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_persiana.Click
if thisform.pageframe1.page1.caja_lin.visible=.F.
  this.tooltiptext=f3_t('Bajar ventana líneas')
  lx=this.picture
  nd=atc('SUBEP.BMP',lx)
  if nd>0
    lx=stuff(lx,nd,9,'BAJAP.BMP')
    this.picture=lx
  endif
  thisform.pageframe1.page1.grid1.height=thisform.grid_h
  thisform.pageframe1.page1.caja_lin.visible=.T.
  for nd=1 to memlines(sysprg.PRG_LVIS)
    lx=mline(sysprg.PRG_LVIS,nd)
    &lx
  endfor
  if  thisform.Shape1.Backcolor=RGB(0,128,0)
    thisform._l_alta.Caption=f3_t('+ L í n e a')
    thisform.Shape1.Backcolor=RGB(0,0,128)
  endif
else
  this.tooltiptext=f3_t('Subir ventana líneas')
  lx=this.picture
  nd=atc('BAJAP.BMP',lx)
  if nd>0
    lx=stuff(lx,nd,9,'SUBEP.BMP')
    this.picture=lx
  endif
  for nd=1 to memlines(sysprg.PRG_LINV)
    lx=mline(sysprg.PRG_LINV,nd)
    &lx
  endfor
  thisform.pageframe1.page1.grid1.height=thisform.grid_h+thisform.caja_h+4
  thisform.pageframe1.page1.caja_lin.visible=.F.

  if  thisform.Shape1.Backcolor=RGB(0,0,128) .or. thisform.no_altal=.T.
    thisform._l_alta.Caption=f3_t('Mod.cabec')
    thisform.Shape1.Backcolor=RGB(0,128,0)
  endif
endif

ENDPROC
PROCEDURE bot_persiana.Init
do case
case _psql=.T.
  this.enabled=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC


PROCEDURE bot_reord.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_reord.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_reord.Click
* _tmpfic=_tmp+trim(sys(3))
_tmpfic=_tmp+trim(sys(2015))
_tmpdbf=_tmpfic+'.DBF'
_tmpcdx=_tmpfic+'.CDX'
copy file 'SYSLIN.DBF' to (_tmpdbf)
copy file 'SYSLIN.CDX' to (_tmpcdx)
use (_tmpfic) in 0 order tag 'CODIGO' alias 'SYSLIN'
_alig='G'+upper(trim(sysprg.PRG_PROG))
select (_alig)
ndmax=min(fcount(),6)
lx_lin=field(2)
lx_cmp=''
for num=2 to ndmax
  lx=mline(sysprg.PRG_GRID,num)
  if at('[',lx)<>0 .or. empty(lx)
    exit
  else
    nd1=at(',',lx)
    m.FIELD_NAME=left(lx,nd1-1)
    lx=subs(lx,nd1+1)
    nd1=at(',',lx)
    lx=subs(lx,nd1+1)
    m.FIELD_TYPE=left(lx,1)
    lx=subs(lx,2)
    nd1=at(',',lx)
    nd2=at('.',lx)
    if nd1=0
      lx1=lx
      lx=''
    else
      lx1=left(lx,nd1-1)
      lx=subs(lx,nd1+1)
    endif
    m.FIELD_DEC=0
    do case
    case m.FIELD_TYPE='N' .and. nd2>0
      m.FIELD_LEN=val(left(lx1,nd2-1))
      m.FIELD_DEC=val(subs(lx1,nd2+1))
      lx_cmp=lx_cmp+'+str('+trim(m.FIELD_NAME)+','+ltrim(str(m.FIELD_LEN))+','+ltrim(str(m.FIELD_DEC))+')'+'+"|"'
    case m.FIELD_TYPE='D'
      lx_cmp=lx_cmp+'+dtoc('+trim(m.FIELD_NAME)+')'+'+"|"'
    case m.FIELD_TYPE='C'
      lx_cmp=lx_cmp+'+'+trim(m.FIELD_NAME)+'+"|"'
    endcase
  endif
endfor
lx_cmp=subs(lx_cmp,2)
_algun_cero=.F.
m.ST3_LIN=0
go top
do while !eof()
  m.ST3_LIN=m.ST3_LIN+2
  m.ST3_TXT=&lx_cmp
  m.EXP_1=EXP_1
  insert into SYSLIN from memvar
  skip
enddo
_ok=.F.
if _algun_cero
  =f3_sn(1,2,'No pueden reordenarse las líneas','Probablemente existan comentarios o líneas históricas')
else
  select SYSLIN
  _valor_ant=0
  do form st3lin
endif
linea=0
if _ok
  select SYSLIN
  go top
  do while !eof()
    linea=linea+2
    =f3_upd(_xfc2,'CODIGO',lx_lin,'linea')
    select SYSLIN
    skip
  enddo
endif
select SYSLIN
use
delete file (_tmpdbf)
delete file (_tmpcdx)
select SYSPRG
if _ok
  thisform.inicio
  thisform.refresh
endif

ENDPROC
PROCEDURE bot_reord.Init
do case
case _psql=.T.
  this.enabled=.F.
case thisform.reordenar=.F.
  this.visible=.F.
case _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endcase
ENDPROC


EndDefine 
Define Class f_form as form
Height = 244
Width = 451
ShowTips = .T.
AutoCenter = .T.
Caption = "Form"
MaxButton = .F.
BackColor = Rgb(192,192,192)

ADD OBJECT bot_ok as commandbutton WITH Top = 210, Left = 378, Height = 29, Width = 29, Picture = "bmp\ok.bmp", Caption = "", TabIndex = 4, ToolTipText = "Aceptar [CTRL+ENTER]"
ADD OBJECT bot_salir as commandbutton WITH Top = 210, Left = 410, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 5, ToolTipText = "Salir"
ADD OBJECT l_negra as line WITH Height = 0, Left = 367, Top = 203, Width = 72
ADD OBJECT l_blanca as line WITH Height = 0, Left = 367, Top = 204, Width = 72, BorderColor = Rgb(255,255,255)
ADD OBJECT Listados as combobox WITH FontBold = .F., FontName = "Courier New", FontSize = 9, ControlSource = "_listado", Height = 19, Left = 163, Margin = 2, Sorted = .F., Style = 2, TabIndex = 2, Top = 212, Visible = .F., Width = 173
ADD OBJECT ayuda as image WITH Picture = "bmp\ayuda.bmp", Stretch = 2, BackStyle = 0, BorderStyle = 0, Enabled = .T., Height = 21, Left = 36, Top = 215, Visible = .F., Width = 12
ADD OBJECT bot_lint as commandbutton WITH AutoSize = .F., Top = 215, Left = 9, Height = 21, Width = 21, Picture = "bmp\linterna.bmp", Caption = "", TabIndex = 1, TabStop = .F., ToolTipText = "Ver fichero"


PROCEDURE sortg
*>
*> Clasifica los GRIDs por columnas.
Parameters _Grid, _Campo2, _Campo3, _Campo4, _Campo5, _Campo6
Local _lx1, _lx2, _lx3, _oldSele
Local _aa, _bb, _cc

If ThisForm._sortg
   _oldSele = Alias()

   _lx1 = Sys(2015)
   _lx2=_tmp + _lx1
   _lx3 = ''

   Select (_Grid)
   If !Eof()
      _bb = RecNo()
   EndIf

   For _Inx = 2 To PCount()
      _aa = Str(_Inx, 1, 0)
      _cc = '_Campo' + _aa
      If Type('&_cc')=='U' .Or. Type('&_cc')=='L'
         Exit
      EndIf

      _lx3 = Iif(Empty(_lx3), Field(_Campo&_aa), _lx3 + ',' + Field(_Campo&_aa))
   EndFor

   Sort To (_lx2) On &_lx3
   Zap
   Append From (_lx2)
   _lx2 = _lx2 + '.Dbf'
   Delete File (_lx2)

   *>
   Select (_oldSele)
   If Type('_bb')=='N'
      Go _bb
   EndIf

   Thisform.Refresh
EndIf

*>

ENDPROC
PROCEDURE Init
************
Private _inx, _aaa, _ccc, _ddd1, ddd2, _aa

_aa = On('Error')
on error a = 1

For _Inx = 1 To This.ControlCount
   _ccc = 'This.Controls(_inx).ControlSource'
   If Type('&_ccc') # 'U'
      If !Empty(&_ccc)
         _ddd1 = This.Controls(_inx).Name
         _ddd1 = "This." + _ddd1 + ".AddProperty('_Control', .F.)"
         &_ddd1

         _aaa = &_ccc
         Public &_aaa
         If Type('&_aaa') = 'L'
            Store '' To &_aaa
            _ddd2 = "This." + This.Controls(_inx).Name + "._Control"
            &_ddd2 = .T.
         EndIf
      EndIf
   EndIf
EndFor

on error &_aa
************

ENDPROC
PROCEDURE Resize
if this.closable=.T.
  this.closable=.F.
else
  this.closable=.T.
endif

ENDPROC
PROCEDURE Activate
if finalmenu=.T.
  thisform.release
  return
endif
store 0 to _xier,_nerrores
on key
on key label 'ESC'        do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok('O')
on key label 'F9' 		  do =f3_hlp()
on key label 'F2'		  do =f3_limpia()
on key label 'F3'		  do =f3_tecl(3)
on key label 'F4'		  do =f3_tecl(4)
on key label 'F5'		  do =f3_tecl(5)
on key label 'F6'		  do =f3_tecl(6)
on key label 'F7'		  do =f3_tecl(7)
on key label 'F8'		  do =f3_tecl(8)
on key label 'F10'		  do =f3_tecl(10)
on key label 'F11'		  do =f3_tecl(11)
on key label 'F12'		  do =f3_tecl(12)

if at(_cse,_usrmaxp)>0
  on key label 'SHIFT+F2' do =f3_posicion()
endif
if !empty(_progant)
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=_progant
      lx1='sysmemo.PRG_M'+ltrim(str(nd))
      save all except _* to memo &lx1
      exit
    endif
  endfor
endif
prog=upper(this.name)
_progant=prog
if thisform.listados.visible=.T.
  on key label 'CTRL+F1'    do =f3_lst()
  _listado=thisform.listados.list(1)
endif
do case
case _cambio_e
  _controlc=.F.
  _inicio=.T.
case !empty(_botlst)
  _controlc=.F.
  store '' to _botlst
  keyboard chr(32)
  thisform.listados.setfocus
case _fn>0
  lx1='FN'+str(_fn,1)
  _fn=0
  thisform.itr(lx1)
case _enter=.T.
  _enter=.F.
  thisform.bot_ok.click
case _esc=.T.
  _esc=.F.
  thisform.bot_salir.click
endcase
select SYSPRG
seek prog
_xfc=mline(sysprg.PRG_FICH,1)
_xfc=ltrim(chrtran(_xfc,'*',''))
_1ernvar=PRG_1er
_lockh=0
if _cambio_e=.T.
  thisform.deactivate
endif
do case
case empty(_1ernvar)
  thisform.inicio
case !empty(_lxerr)
  _lxerr=''
  lx=trim(_1ernvar)+'.setfocus'
  &lx
case _inicio
  thisform.inicio
  lx=trim(_1ernvar)+'.setfocus'
  &lx
case _controlc=.T.
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=prog
      lx1='sysmemo.PRG_M'+ltrim(str(nd))
      restore from memo &lx1 addi
      exit
    endif
  endfor
endcase
_controlc=.T.
_inicio=.F.
thisform.refresh

ENDPROC
PROCEDURE Load
select SYSPRG
do case
case PRG_LIN>0 .or. PRG_COL>0
  thisform.autocenter=.F.
  thisform.top=PRG_LIN
  thisform.left=PRG_COL
case thisform.autocenter=.T.
  thisform.autocenter=.T.
endcase
_logmem=_logmem+'Entrada : '+dtoc(date())+' '+time()+' '+PRG_PROG+' '+PRG_TIT+cr
_linea=PRG_TIT
if _xidiom>1
  _linea=f3_t(_linea)
endif
thisform.icon=iif(Empty(_Icono), '', _Icono)
thisform.caption=_linea
thisform.BorderStyle=2
thisform.l_negra.left=2
thisform.l_negra.width=thisform.width-4
thisform.l_blanca.left=2
thisform.l_blanca.width=thisform.width-4
r1ls=recno()
lx=trim(PRG_PROG)
lx2=PRG_PROG
nd=len(lx)
skip
do while !eof() .and. left(PRG_PROG,nd)=lx .and. len(trim(PRG_PROG))=nd+1
  if PRG_TIPO='L' .and. PRG_OK=.T.
    if thisform.listados.visible=.F.
      thisform.listados.visible=.T.
      _listado=PRG_TIT+' '+PRG_PROG
    endif
    thisform.listados.additem(f3_t(PRG_TIT+' '+PRG_PROG))
  endif
  skip
enddo
set order to IDX2
seek lx2
do while !eof() .and. LS_PRG=lx2
  if PRG_TIPO='L' .and. PRG_OK=.T.
    if thisform.listados.visible=.F.
      thisform.listados.visible=.T.
      _listado=PRG_TIT+' '+PRG_PROG
    endif
    thisform.listados.additem(f3_t(PRG_TIT+' '+PRG_PROG))
  endif
  skip
enddo
set order to IDX1
go r1ls
_inicio=.T.
_xobjw='W'+trim(prog)
if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'ABRIR', , prog
endif
select SYSPRG

ENDPROC
PROCEDURE Unload
on key
select SYSPRG
=f3_cerrar(upper(thisform.name))

ENDPROC
PROCEDURE Destroy
************
Private _inx, _aaa, _ccc, _ddd

_aa = On('Error')
on error a = 1
For _Inx = 1 To This.ControlCount
   _ccc = 'This.Controls(_inx).ControlSource'
   If Type('&_ccc') # 'U'
      If !Empty(&_ccc)
         _ddd = "This." + This.Controls(_inx).Name + "._Control"
         If &_ddd
            _aaa = &_ccc
******      Release &_aaa
         EndIf
      EndIf
   EndIf
EndFor

on error &_aa
************

ENDPROC


PROCEDURE bot_ok.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_ok.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_ok.Init
if _xidiom>1
  this.ToolTipText=f3_t(this.ToolTipText)
endif
ENDPROC
PROCEDURE bot_ok.Click
prog=wontop()
_lxerr=''
thisform.itr('COMPROBAR')
if !empty(_lxerr)
  do form st3inc
  _lxerr=''
  return
endif
if !empty(sysprg.PRG_CI)
  =f3_ci()
endif
if empty(_lxerr)
  select alias (_xfc)
  _xobjw='W'+trim(prog)
  if file(_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
    do (_xobjw) with 'PROCESO', , prog
    select alias (_xfc)
  endif
endif
if !empty(_lxerr)
  do form st3inc
else
  thisform.itr('OK')
  thisform.inicio
  thisform.refresh
endif
lx=trim(_1ernvar)+'.setfocus'
&lx
select SYSPRG

ENDPROC


PROCEDURE bot_salir.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_salir.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_salir.Init
if _xidiom>1
  this.ToolTipText=f3_t(this.ToolTipText)
endif
ENDPROC
PROCEDURE bot_salir.Click
if !ThisForm._Cerrar .Or. f3_sn(2, 3, 'Desea usted abandonar el proceso?')
   thisform.release
EndIf

ENDPROC


PROCEDURE Listados.Valid
if lastkey()<>9
  select SYSPRG
  r1ls=recno()
  lx=right(this.value,10)
  seek alltrim(lx)
  if !eof()
    if wexist(lx)
      activate window (lx)
    else
      =f3_bot(PRG_PROG,'ST3PRG')
    endif
  else
    go r1ls
  endif
endif

ENDPROC


PROCEDURE bot_lint.Click
if _psql
  =sq3_lint()
  thisform.release
  return
endif
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx
=f3_hlpfc(_xfc)
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx
_lxfocus=trim(_1ernvar)+'.setfocus'
&_lxfocus

ENDPROC
PROCEDURE bot_lint.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC


EndDefine 
Define Class f_list as form
Height = 186
Width = 412
ShowTips = .T.
AutoCenter = .T.
Caption = "Form"
MaxButton = .F.
BackColor = Rgb(192,192,192)
n_registro = 0

ADD OBJECT bot_pant as commandbutton WITH Top = 153, Left = 50, Height = 29, Width = 29, Picture = "bmp\pantalla.bmp", Caption = "", ToolTipText = "Pantalla [CTRL+ENTER]"
ADD OBJECT bot_impp as commandbutton WITH Top = 153, Left = 83, Height = 29, Width = 29, Picture = "bmp\printp.bmp", Caption = "", ToolTipText = "Impresora predeterminada (Botón derecho, cambiar)"
ADD OBJECT bot_imp as commandbutton WITH Top = 153, Left = 116, Height = 29, Width = 29, Picture = "bmp\print.bmp", Caption = "", ToolTipText = "Impresora"
ADD OBJECT bot_salir as commandbutton WITH Top = 153, Left = 369, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", ToolTipText = "Salir"
ADD OBJECT l_negra as line WITH Height = 0, Left = 338, Top = 148, Width = 72
ADD OBJECT l_blanca as line WITH Height = 0, Left = 338, Top = 149, Width = 72, BorderColor = Rgb(255,255,255)
ADD OBJECT Line1 as line WITH Height = 0, Left = 338, Top = 148, Width = 72
ADD OBJECT Line2 as line WITH Height = 0, Left = 338, Top = 149, Width = 72, BorderColor = Rgb(255,255,255)
ADD OBJECT bot_modr as commandbutton WITH Top = 153, Left = 150, Height = 29, Width = 29, Picture = "bmp\report.bmp", Caption = "", ToolTipText = "Modificar Formato", Visible = .F.
ADD OBJECT idioma as combobox WITH FontBold = .F., FontName = "Courier", RowSourceType = 5, RowSource = "_idiom", ControlSource = "_idioma", Height = 18, Left = 190, Margin = 2, NumberOfElements = 0, Style = 2, Top = 163, Visible = .F., Width = 100
ADD OBJECT Ayuda as image WITH Picture = "bmp\ayuda.bmp", Stretch = 2, BackStyle = 0, BorderStyle = 0, Enabled = .T., Height = 21, Left = 33, Top = 157, Visible = .F., Width = 12


PROCEDURE itr
Parameter _itr
*
return
ENDPROC
PROCEDURE Release
select (_xfc)
nd=iif(eof(),0,recno())
if nd<>0 .and. nd<>thisform.n_registro
  nd=thisform.n_registro
  go nd
endif
select SYSPRG

ENDPROC
PROCEDURE Resize
if this.closable=.T.
  this.closable=.F.
else
  this.closable=.T.
endif

ENDPROC
PROCEDURE Unload
select SYSPRG
on key
=f3_cerrar(upper(thisform.name))

ENDPROC
PROCEDURE Activate
if finalmenu=.T.
  thisform.release
  return
endif
store 0 to _xier,_nerrores
on key
on key label 'ESC'        do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok('O')
on key label 'F9' 		  do =f3_hlp()
on key label 'F2'		  do =f3_limpia()
on key label 'F3'		  do =f3_tecl(3)
on key label 'F4'		  do =f3_tecl(4)
on key label 'F5'		  do =f3_tecl(5)
on key label 'F6'		  do =f3_tecl(6)
on key label 'F7'		  do =f3_tecl(7)
on key label 'F8'		  do =f3_tecl(8)
on key label 'F10'		  do =f3_tecl(10)
on key label 'F11'		  do =f3_tecl(11)
on key label 'F12'		  do =f3_tecl(12)
if at(_cse,_usrmaxp)>0
  on key label 'SHIFT+F2' do =f3_posicion()
endif
if !empty(_progant)
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=_progant
      lx1='sysmemo.PRG_M'+ltrim(str(nd))
      save all except _* to memo &lx1
      exit
    endif
  endfor
endif
prog=upper(this.name)
_progant=''
do case
case _cambio_e
  _inicio=.T.
case _fn>0
  lx1='FN'+str(_fn,1)
  _fn=0
  thisform.itr(lx1)
case _enter=.T.
  _enter=.F.
  thisform.bot_pant.click
case _esc=.T.
  _esc=.F.
  thisform.bot_salir.click
endcase
select SYSPRG
seek prog
_lockh=0
_1ernvar=PRG_1er
if empty(_1ernvar)
  _1ernvar='thisform.bot_salir'
endif
_xfc=mline(sysprg.PRG_FICH,1)
_xfc=ltrim(chrtran(_xfc,'*',''))
select (_xfc)
nd=iif(eof(),0,recno())
thisform.n_registro=nd
select SYSPRG
if _inicio
  thisform.inicio
endif
_controlc=.T.
_inicio=.F.
if _cambio_e=.T.
  thisform.deactivate
endif
thisform.refresh

ENDPROC
PROCEDURE Load
select SYSPRG
lx='LSTW2\'+trim(PRG_PROG)+'.FRX'
if file(lx)
  thisform.idioma.visible=.T.
  _idioma=_idiom(_xidiom)
else
  _idioma=''
  thisform.idioma.visible=.F.
endif
do case
case PRG_LIN>0 .or. PRG_COL>0
  thisform.autocenter=.F.
  thisform.top=PRG_LIN
  thisform.left=PRG_COL
case thisform.autocenter=.T.
  thisform.autocenter=.T.
endcase
_logmem=_logmem+dtoc(date())+' '+time()+' '+PRG_PROG+' '+PRG_TIT+cr
_linea=PRG_TIT
if _xidiom>1
  _linea=f3_t(_linea)
endif
thisform.icon=iif(Empty(_Icono), '', _Icono)
thisform.caption=_linea
thisform.BorderStyle=2
thisform.l_negra.left=2
thisform.l_negra.width=thisform.width-4
thisform.l_blanca.left=2
thisform.l_blanca.width=thisform.width-4
_inicio=.T.
if at(_cse,'MP')=0
  thisform.bot_modr.visible=.F.
endif
progl=trim(sysprg.PRG_PROG)
_xobjw='W'+trim(progl)
if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'ABRIR'
endif
select SYSPRG

ENDPROC


PROCEDURE bot_pant.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_pant.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_pant.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC
PROCEDURE bot_pant.Click
=st3ls('PAN')
nd=thisform.n_registro
if nd<>0
  select (_xfc)
  if nd<=reccount()
    go nd
  else
    go top
  endif
  select SYSPRG
endif

ENDPROC


PROCEDURE bot_impp.RightClick
*>
*> Establecer la impresora predeterminada.

Set Printer To Name GetPrinter()

*>
*> Do St3Imp2.Mpr
*>

ENDPROC
PROCEDURE bot_impp.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_impp.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_impp.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC
PROCEDURE bot_impp.Click
=st3ls('IMPP')
nd=thisform.n_registro
if nd<>0
  select (_xfc)
  if nd<=reccount()
    go nd
  else
    go top
  endif
  select SYSPRG
endif

ENDPROC


PROCEDURE bot_imp.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_imp.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_imp.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC
PROCEDURE bot_imp.Click
=st3ls('IMP')
nd=thisform.n_registro
if nd<>0
  select (_xfc)
  if nd<=reccount()
    go nd
  else
    go top
  endif
  select SYSPRG
endif

ENDPROC


PROCEDURE bot_salir.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_salir.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_salir.Click
thisform.release
ENDPROC
PROCEDURE bot_salir.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC


PROCEDURE bot_modr.GotFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'X'
this.picture=lx

ENDPROC
PROCEDURE bot_modr.LostFocus
lx=this.picture
lx=left(lx,len(lx)-1)+'P'
this.picture=lx

ENDPROC
PROCEDURE bot_modr.Click
prog=wontop()
_xfc=mline(sysprg.PRG_FICH,1)
_xfc=ltrim(chrtran(_xfc,'*',''))
select (_xfc)
lst='LSTW\'+trim(prog)
if file (lst+'.FRX')
  push menu _MSYSMENU
  set sysmenu to default
  DEFINE PAD _msm_edit OF _MSYSMENU PROMPT "\<Edición" KEY ALT+E
  ON PAD _msm_edit OF _MSYSMENU ACTIVATE POPUP _medit
  DEFINE POPUP _medit MARGIN RELATIVE
  DEFINE BAR _med_undo  OF _medit PROMPT "\<Deshacer" KEY CTRL+Z
  DEFINE BAR _med_redo  OF _medit PROMPT "Re\<hacer" KEY CTRL+R
  DEFINE BAR _med_sp100 OF _medit PROMPT "\-"
  DEFINE BAR _med_cut   OF _medit PROMPT "Cor\<tar" KEY CTRL+X
  DEFINE BAR _med_copy  OF _medit PROMPT "\<Copiar" KEY CTRL+C
  DEFINE BAR _med_paste OF _medit PROMPT "\<Pegar" KEY CTRL+V
  DEFINE BAR _med_pstlk OF _medit PROMPT "P\<egado especial"
  DEFINE BAR _med_sp200 OF _medit PROMPT "\-"
  DEFINE BAR _med_slcta OF _medit PROMPT "Se\<leccionar todo" KEY CTRL+A
  DEFINE BAR _med_sp300 OF _medit PROMPT "\-"
  DEFINE BAR _med_find  OF _medit PROMPT "\<Buscar..." KEY CTRL+F
  DEFINE BAR _med_repl  OF _medit PROMPT "Ree\<mplazar..." KEY CTRL+L
  DEFINE BAR _med_goto  OF _medit PROMPT "I\<r a línea..."
  DEFINE BAR _med_sp400 OF _medit PROMPT "\-"
  DEFINE BAR _med_insob OF _medit PROMPT "\<Insertar objeto"
  DEFINE BAR _med_obj   OF _medit PROMPT "\<Objeto..."
  DEFINE BAR _med_link  OF _medit PROMPT "\<Vínculos..."
*
  DEFINE PAD _msm_view OF _MSYSMENU PROMPT "\<Ver" KEY ALT+V
  ON PAD _msm_view OF _MSYSMENU ACTIVATE POPUP _mview
  DEFINE POPUP _mview MARGIN RELATIVE
  DEFINE BAR _mvi_toolb OF _mview PROMPT "\<Barras de herramientas..."
  modi report (lst) noenv
  for nidiom=2 to sysprg.LS_IDIOM
    lx1='LSTW'+str(nidiom,1)+'\ST3L.FRX'
    if file(lx1)
      lst1='LSTW\'++trim(prog)
      lst2='LSTW'+str(nidiom,1)+'\'+trim(prog)
      =sy3lsi(lst1,lst2,nidiom)
      modi report (lst2)
    endif
  endfor
  release pad _msm_edit of _MSYSMENU
  release pad _msm_view of _MSYSMENU
  pop menu _MSYSMENU
endif
select SYSPRG

ENDPROC
PROCEDURE bot_modr.Init
if _xidiom>1
  this.tooltiptext=f3_t(this.tooltiptext)
endif
ENDPROC


EndDefine 
Define Class st_grid as grid
ColumnCount = 0
FontName = "Courier"
DeleteMark = .F.
HeaderHeight = 17
Height = 133
RowHeight = 16
Width = 249



PROCEDURE sortc
*>
*> Clasifica los GRIDs por columnas.
Parameters _Grid, _Campo
Private _lx1, _lx2, _lx3, _oldSele

_oldSele = Alias()

_lx1 = Sys(2015)
_lx2=_tmp + _lx1

Select (_Grid)
_lx3 = Field(_Campo)
Sort To (_lx2) On &_lx3

Zap
Append From (_lx2)
_lx2 = _lx2 + '.Dbf'
Delete File (_lx2)

*>
Select (_oldSele)
Thisform.Refresh

ENDPROC
PROCEDURE Init

Local nInx, nIny

=DoDefault()

if _psql
  this.ColumnCount=0
  this.enabled=.F.
  return
endif

_nlineas=this.ColumnCount

if atc('['+trim(this.name)+']',sysprg.PRG_GRID)>0
  set memowidth to 80
*   systmp=_tmp+trim(sys(3))
  systmp=_tmp+trim(sys(2015))
  sysfpt=systmp+'.FPT'
  systmp=systmp+'.DBF'
  _alig=upper(trim(this.name))
  lx1='FCTMP\'+_alig+'.DBF'
  lx2='FCTMP\'+_alig+'.FPT'
  copy file &lx1 to &systmp
  if file(lx2)
    copy file &lx2 to &sysfpt
  endif
  use (systmp) in 0 alias (_alig) excl
  lx1='this.RecordSource= "'+_alig+'"'
  &lx1
endif

if _xidiom>1
  for nd=1 to _nlineas
    lx='this.column'+ltrim(str(nd))+'.header1.caption'
    &lx=f3_t(&lx)
  endfor
endif

*> Cambiar, si cal, el texto de la columna de lote.
If This.ControlLote
	With This
		For nInx = 1 To .ColumnCount
			For nIny = 1 To .Columns(nInx).ControlCount
				If Upper(.Columns(nInx).Controls(nIny).Class)=='LOTEGT'
					.Columns(nInx).Header1.Caption = Iif(Type('_TCL')=='C', _TCL, .Columns(nInx).Header1.Caption)
					Exit
				EndIf
			EndFor
		EndFor
	EndWith
EndIf

ENDPROC
PROCEDURE Destroy
if _psql
  return
endif
_alig=trim(this.name)
if used(_alig)
  select alias(_alig)
  lx=dbf()
  use
  delete file (lx)
  lx=left(lx,len(lx)-3)+'FPT'
  if file (lx)
    delete file (lx)
  endif
endif
select SYSPRG

ENDPROC


EndDefine 
