Public st3hlpf_45T0S4WWN
SET CLASSLIB TO "st3class.vcx" ADDITIVE

st3hlpf_45T0S4WWN = CreateObject("st3hlpf" )
If st3hlpf_45T0S4WWN.WindowType == 1
st3hlpf_45T0S4WWN.Show(1)
else
st3hlpf_45T0S4WWN.Show()
EndIf



Define Class st3hlpf as form
Height = 105
Width = 525
AutoCenter = .T.
Caption = (f3_t('Establecer filtro'))
FontBold = .T.
FontSize = 10
MaxButton = .F.
MinButton = .F.
WindowType = 1
AlwaysOnTop = .T.
ColorSource = 5
BackColor = Rgb(192,192,192)

ADD OBJECT L_normal1 as l_normal WITH FontItalic = .T., FontSize = 10, FontUnderline = .T., Alignment = 0, Left = 10, Top = 5, TabIndex = 1, ForeColor = Rgb(0,0,128)
ADD OBJECT cnd1 as st_combo WITH RowSourceType = 5, RowSource = "_cnd1", DisplayValue = , ControlSource = "_cond1", Height = 19, Left = 27, Style = 2, TabIndex = 2, Top = 27, Width = 37
ADD OBJECT _txt1 as st_get WITH ControlSource = "_txt1", Left = 72, TabIndex = 3, Top = 27, Width = 168
ADD OBJECT cnd2 as st_combo WITH RowSourceType = 5, RowSource = "_cnd1", DisplayValue = , ControlSource = "_cond2", Height = 19, Left = 279, Style = 2, TabIndex = 4, Top = 27, Width = 37
ADD OBJECT cnd3 as st_combo WITH RowSourceType = 5, RowSource = "_cnd1", DisplayValue = , ControlSource = "_cond3", Height = 19, Left = 27, Style = 2, TabIndex = 6, Top = 45, Width = 37
ADD OBJECT cnd4 as st_combo WITH RowSourceType = 5, RowSource = "_cnd1", DisplayValue = , ControlSource = "_cond4", Height = 19, Left = 279, Style = 2, TabIndex = 8, Top = 45, Width = 37
ADD OBJECT _txt2 as st_get WITH ControlSource = "_txt2", Left = 333, TabIndex = 5, Top = 27, Width = 168
ADD OBJECT _txt3 as st_get WITH ControlSource = "_txt3", Left = 72, TabIndex = 7, Top = 45, Width = 168
ADD OBJECT _txt4 as st_get WITH ControlSource = "_txt4", Left = 333, TabIndex = 9, Top = 45, Width = 168
ADD OBJECT bot_filtro as commandbutton WITH Top = 72, Left = 27, Height = 28, Width = 94, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "Quitar filtros", TabIndex = 10, TabStop = .F., ColorSource = 0
ADD OBJECT bot_ok as commandbutton WITH Top = 72, Left = 432, Height = 29, Width = 29, FontBold = .T., FontSize = 10, Picture = "bmp\ok.bmp", Caption = "", Default = .T., TabIndex = 11, ToolTipText = "Aceptar", ColorSource = 0
ADD OBJECT bot_esc as commandbutton WITH Top = 72, Left = 468, Height = 29, Width = 29, FontBold = .T., FontSize = 10, Picture = "bmp\close.bmp", Caption = "", TabIndex = 12, ToolTipText = "Salir", ColorSource = 0
ADD OBJECT L_normal2 as l_normal WITH Caption = "Y", Left = 259, Top = 30
ADD OBJECT L_normal3 as l_normal WITH Caption = "Y", Height = 17, Left = 258, Top = 48, Width = 9
ADD OBJECT L_normal4 as l_normal WITH Caption = "O", Height = 17, Left = 9, Top = 48, Width = 11


PROCEDURE Activate
thisform.l_normal1.caption=trim(_txtvar)
_ok=.F.
ENDPROC


PROCEDURE cnd1.Valid
if empty(_cond1)
  store space(4)  to _cond2,_cond3,_cond4
  store space(20) to _txt1,_txt2,_txt3,_txt4
  thisform.refresh
endif

ENDPROC


PROCEDURE cnd2.Valid
if empty(_cond2)
  store space(20) to _txt2
  thisform.refresh
endif

ENDPROC


PROCEDURE cnd3.Valid
if empty(_cond3)
  store space(4)  to _cond4
  store space(20) to _txt3,_txt4
  thisform.refresh
endif

ENDPROC


PROCEDURE cnd4.Valid
if empty(_cond4)
  store space(20) to _txt4
  thisform.refresh
endif

ENDPROC


PROCEDURE bot_filtro.Click
store '' to _cond1,_txt1,_cond2,_txt2,_cond3,_txt3,_cond4,_txt4
for nd=1 to _xbnum
  store '' to _xbusca(nd,5),_xbusca(nd,6),_xbusca(nd,7),_xbusca(nd,8)
endfor
_ok=.T.
thisform.release
ENDPROC


PROCEDURE bot_ok.Click
_ok=.T.
thisform.release
ENDPROC


PROCEDURE bot_esc.Click
_ok=.F.
thisform.release
ENDPROC


EndDefine 
