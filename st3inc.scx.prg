Public ST3INC_45T0S4KQZ
SET CLASSLIB TO "st3class.vcx" ADDITIVE

ST3INC_45T0S4KQZ = CreateObject("ST3INC" )
If ST3INC_45T0S4KQZ.WindowType == 1
ST3INC_45T0S4KQZ.Show(1)
else
ST3INC_45T0S4KQZ.Show()
EndIf



Define Class ST3INC as form
Height = 274
Width = 613
ShowTips = .T.
AutoCenter = .T.
BorderStyle = 3
Caption = "INCIDENCIAS"
ControlBox = .T.
Closable = .T.
FontBold = .T.
FontName = "Courier"
FontSize = 10
MaxButton = .F.
MinButton = .F.
WindowType = 1
WindowState = 0
LockScreen = .F.
AlwaysOnTop = .F.
ColorSource = 5
BackColor = Rgb(192,192,192)
fn_boton1 = ""
fn_boton2 = ""
fn_boton3 = ""

ADD OBJECT Image1 as image WITH DragMode = 0, Picture = "bmp\caution.bmp", Stretch = 0, BackStyle = 0, Height = 40, Left = 9, Top = 0, Width = 40
ADD OBJECT St_edit1 as st_edit WITH FontName = "Courier New", FontSize = 9, Height = 179, Left = 10, ReadOnly = .T., TabIndex = 5, Top = 37, Width = 594, ControlSource = "_lxerr"
ADD OBJECT bot_pan as st_bot WITH AutoSize = .F., Top = 228, Left = 16, Height = 29, Width = 29, Picture = "bmp\pantalla.bmp", Caption = "", ToolTipText = "Pantalla"
ADD OBJECT bot_impp as st_bot WITH AutoSize = .F., Top = 228, Left = 52, Height = 29, Width = 29, Picture = "bmp\printp.bmp", Caption = "", ToolTipText = "Impresora Predeterminada"
ADD OBJECT bot_imp as st_bot WITH AutoSize = .F., Top = 228, Left = 88, Height = 29, Width = 29, Picture = "bmp\print.bmp", Caption = "", ToolTipText = "Impresora"
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 229, Left = 571, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 1, ToolTipText = "Salir"


PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
*
if _esc=.T.
  _esc=.F.
  thisform.release
endif

ENDPROC
PROCEDURE Init
*>
Parameters _Eliminar, _Mensaje

ThisForm._Eliminar = Iif(Type('_Eliminar') # 'L', .F., _Eliminar)

If Type('_Mensaje') == 'C'
   _LxErr = _Mensaje
EndIf

on ESCAPE thisform.release

This.Caption = f3_t(This.Caption)

_controlc=.F.
if _xidiom>1
  lx_inc=_lxerr+'   '
  _lxerr=''
  do while !empty(lx_inc)
    nd=at(cr,lx_inc)
    if nd=0
      lx=lx_inc
      lx_inc=''
    else
      lx=left(lx_inc,nd-1)
      lx_inc=subs(lx_inc,nd+2)
    endif
    lx=f3_t(lx)
    _lxerr=_lxerr+lx+cr
  enddo
endif
ENDPROC
PROCEDURE Release
*>
If ThisForm._Eliminar
   _LxErr = ''
EndIf

=DoDefault()
ENDPROC


PROCEDURE bot_pan.Click
select SYSPRG
r1prg=recno()
report form st3inc preview while recno()=r1prg noconsole
go r1prg
thisform.release
ENDPROC


PROCEDURE bot_impp.Click
select SYSPRG
r1prg=recno()
report form st3inc to printer while recno()=r1prg noconsole
go r1prg
thisform.release
ENDPROC


PROCEDURE bot_imp.Click
select SYSPRG
r1prg=recno()
report form st3inc to printer prompt while recno()=r1prg noconsole
go r1prg
thisform.release

ENDPROC


PROCEDURE bot_esc.Click
thisform.release

ENDPROC


EndDefine 
