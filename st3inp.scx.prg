Public ST3INP_45T0S4N0E

ST3INP_45T0S4N0E = CreateObject("ST3INP" )
If ST3INP_45T0S4N0E.WindowType == 1
ST3INP_45T0S4N0E.Show(1)
else
ST3INP_45T0S4N0E.Show()
EndIf



Define Class ST3INP as form
Height = 308
Width = 600
BufferMode = 0
AutoCenter = .T.
BorderStyle = 2
Caption = "st3inp"
FontBold = .T.
FontSize = 10
MaxButton = .F.
MinButton = .T.
WindowType = 1
ColorSource = 5
BackColor = Rgb(255,255,128)

ADD OBJECT Label1 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label1", Height = 15, Left = 6, Top = 8, Width = 41, TabIndex = 3, ColorSource = 0
ADD OBJECT Label2 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label2", Height = 15, Left = 6, Top = 25, Width = 41, TabIndex = 7, ColorSource = 0
ADD OBJECT Label3 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label3", Height = 15, Left = 6, Top = 43, Width = 41, TabIndex = 12, ColorSource = 0
ADD OBJECT Label4 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label4", Height = 15, Left = 6, Top = 60, Width = 41, TabIndex = 16, ColorSource = 0
ADD OBJECT Label5 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label5", Height = 15, Left = 6, Top = 77, Width = 41, TabIndex = 20, ColorSource = 0
ADD OBJECT Label6 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label6", Height = 15, Left = 6, Top = 94, Width = 41, TabIndex = 24, ColorSource = 0
ADD OBJECT Label7 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label7", Height = 15, Left = 6, Top = 111, Width = 41, TabIndex = 28, ColorSource = 0
ADD OBJECT Text1 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Enabled = .T., Height = 17, Left = 140, Margin = 0, ReadOnly = .F., TabIndex = 4, Top = 8, Width = 113, ColorSource = 0, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255), DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text2 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 8, Top = 25, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text3 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 11, Top = 42, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text4 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 15, Top = 59, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text5 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 19, Top = 76, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text6 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 23, Top = 93, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text7 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 27, Top = 110, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT bot_ok as commandbutton WITH Top = 279, Left = 163, Height = 25, Width = 84, FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Aceptar", Default = .T., TabIndex = 61, ColorSource = 0
ADD OBJECT bot_esc as commandbutton WITH Top = 279, Left = 283, Height = 25, Width = 86, FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Cancelar", TabIndex = 62, ColorSource = 0
ADD OBJECT Label8 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label8", Height = 15, Left = 6, Top = 128, Width = 41, TabIndex = 32, ColorSource = 0
ADD OBJECT Label9 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label9", Height = 15, Left = 6, Top = 145, Width = 41, TabIndex = 36, ColorSource = 0
ADD OBJECT Label10 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label10", Height = 15, Left = 6, Top = 163, Width = 48, TabIndex = 40, ColorSource = 0
ADD OBJECT Label11 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label11", Height = 15, Left = 6, Top = 180, Width = 48, TabIndex = 43, ColorSource = 0
ADD OBJECT Label12 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label12", Height = 15, Left = 6, Top = 197, Width = 48, TabIndex = 47, ColorSource = 0
ADD OBJECT Label13 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label13", Height = 15, Left = 6, Top = 214, Width = 48, TabIndex = 50, ColorSource = 0
ADD OBJECT Label14 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label14", Height = 15, Left = 6, Top = 231, Width = 48, TabIndex = 54, ColorSource = 0
ADD OBJECT Text8 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 31, Top = 127, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text9 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 34, Top = 144, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text10 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 37, Top = 161, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text11 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 41, Top = 178, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text12 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 45, Top = 195, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text13 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 49, Top = 212, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Text14 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 53, Top = 228, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Label15 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 0, Caption = "Label15", Height = 15, Left = 6, Top = 248, Width = 48, TabIndex = 58, ColorSource = 0
ADD OBJECT Text15 as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, Alignment = 3, BackStyle = 1, BorderStyle = 1, Height = 17, Left = 140, Margin = 0, TabIndex = 57, Top = 245, Width = 113, ColorSource = 0, DisabledBackColor = Rgb(192,192,192), DisabledForeColor = Rgb(0,0,128)
ADD OBJECT Line1 as line WITH Height = 0, Left = 0, Top = 269, Width = 600
ADD OBJECT Line2 as line WITH Height = 0, Left = -1, Top = 270, Width = 600, BorderColor = Rgb(255,255,255)
ADD OBJECT Check1 as checkbox WITH Top = 0, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check1", ControlSource = "", TabIndex = 2, Visible = .F., ColorSource = 0
ADD OBJECT Check2 as checkbox WITH Top = 18, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check2", ControlSource = "", TabIndex = 6, Visible = .F., ColorSource = 0
ADD OBJECT Check3 as checkbox WITH Top = 36, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check3", ControlSource = "", TabIndex = 10, Visible = .F., ColorSource = 0
ADD OBJECT Check4 as checkbox WITH Top = 54, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check4", ControlSource = "", TabIndex = 14, Visible = .F., ColorSource = 0
ADD OBJECT Check5 as checkbox WITH Top = 72, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check5", ControlSource = "", TabIndex = 18, Visible = .F., ColorSource = 0
ADD OBJECT Check6 as checkbox WITH Top = 90, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check6", ControlSource = "", TabIndex = 22, Visible = .F., ColorSource = 0
ADD OBJECT Combo1 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 1, Top = 0, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo2 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 5, Top = 17, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo3 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 9, Top = 35, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo4 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 13, Top = 53, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo5 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 17, Top = 71, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo6 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 21, Top = 89, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo7 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 25, Top = 107, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo8 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 29, Top = 125, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo9 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 33, Top = 143, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo10 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 38, Top = 161, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo11 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 42, Top = 179, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo12 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 48, Top = 197, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo13 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 51, Top = 216, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo14 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 55, Top = 234, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Combo15 as combobox WITH FontBold = .F., FontItalic = .F., FontName = "Courier", FontSize = 10, RowSourceType = 5, Height = 17, Left = 264, Margin = 2, Style = 2, TabIndex = 60, Top = 251, Visible = .F., Width = 184, ColorSource = 0
ADD OBJECT Check7 as checkbox WITH Top = 108, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check7", ControlSource = "", TabIndex = 26, Visible = .F., ColorSource = 0
ADD OBJECT Check8 as checkbox WITH Top = 126, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check8", ControlSource = "", TabIndex = 30, Visible = .F., ColorSource = 0
ADD OBJECT Check9 as checkbox WITH Top = 144, Left = 468, Height = 15, Width = 64, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check9", ControlSource = "", TabIndex = 35, Visible = .F., ColorSource = 0
ADD OBJECT Check10 as checkbox WITH Top = 162, Left = 468, Height = 15, Width = 71, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check10", ControlSource = "", TabIndex = 39, Visible = .F., ColorSource = 0
ADD OBJECT Check11 as checkbox WITH Top = 180, Left = 468, Height = 15, Width = 71, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check11", ControlSource = "", TabIndex = 44, Visible = .F., ColorSource = 0
ADD OBJECT Check12 as checkbox WITH Top = 196, Left = 468, Height = 15, Width = 71, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check12", ControlSource = "", TabIndex = 46, Visible = .F., ColorSource = 0
ADD OBJECT Check13 as checkbox WITH Top = 216, Left = 468, Height = 15, Width = 71, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check13", ControlSource = "", TabIndex = 52, Visible = .F., ColorSource = 0
ADD OBJECT Check14 as checkbox WITH Top = 234, Left = 468, Height = 15, Width = 71, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check14", ControlSource = "", TabIndex = 56, Visible = .F., ColorSource = 0
ADD OBJECT Check15 as checkbox WITH Top = 250, Left = 468, Height = 15, Width = 71, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, AutoSize = .T., BackStyle = 0, Caption = "Check15", ControlSource = "", TabIndex = 59, Visible = .F., ColorSource = 0


PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok()
*
do case
case _esc=.T.
  _esc=.F.
  thisform.bot_esc.click
case _enter=.T.
  _enter=.F.
  thisform.bot_ok.click
endcase
ENDPROC
PROCEDURE Init
* Parametros
*	tit_inp		Título de la pantalla
*	txt_inp		Textos a pasar
*	var_inp		Variables a cargar  (si precedido por $, es SAY)
*	fmt_inp		Máscaras a utilizar (si precedido por $, no. líneas)
*				(en este caso poner la siguiente linea en blanco)
*
* Si Cuadro compinado
*	fmt_inp  #Combo.<matriz>
*
* Si Check box
*	fmt_inp	#CheckBox
*
_controlc=.F.
bot_ok=1
store 0 to n_inp
if _xidiom>1
  this.caption=f3_t(tit_inp)
else
  this.caption=tit_inp
endif
do while len(txt_inp)<>0
  n_inp=n_inp+1
  ndi1=at('/',txt_inp)
  ndi2=at('/',var_inp)
  ndi3=at('/',fmt_inp)
  if ndi1>0
    if _xidiom>1
      lxi2=f3_t(left(txt_inp,ndi1-1))
    else
      lxi2=left(txt_inp,ndi1-1)
    endif
    txt_inp=subs(txt_inp,ndi1+1)
    lxi3=left(var_inp,ndi2-1)
    var_inp=subs(var_inp,ndi2+1)
    lxi4=iif(ndi3=1,'',left(fmt_inp,ndi3-1))
    fmt_inp=iif(ndi3=len(fmt_inp),'',subs(fmt_inp,ndi3+1))
  else
    lxi2=txt_inp
    lxi3=var_inp
    lxi4=fmt_inp
    store '' to txt_inp,var_inp,fmt_inp
  endif
  do case
  case left(lxi4,1)='#' .and. upper(left(lxi4,7))='#CO'
    nd=at('.',lxi4)
    if nd>0
      lxi1='this.label'+ltrim(str(n_inp))+'.caption'
      &lxi1=lxi2
      lxi1='this.text'+ltrim(str(n_inp))+'.visible'
      &lxi1=.F.
      lxi1='this.text'+ltrim(str(n_inp))+'.top'
      _top=&lxi1
      lxi1='this.combo'+ltrim(str(n_inp))+'.top'
      &lxi1=_top
      lxi1='this.combo'+ltrim(str(n_inp))+'.left'
      &lxi1=140
      lxi1='this.combo'+ltrim(str(n_inp))+'.ControlSource'
      &lxi1=lxi3
      lxi1='this.combo'+ltrim(str(n_inp))+'.RowSource'
      &lxi1=subs(lxi4,nd+1)
      lxi1='this.combo'+ltrim(str(n_inp))+'.visible'
      &lxi1=.T.
      lxi1='this.combo'+ltrim(str(n_inp))+'.width'
      &lxi1=400
    endif
  case left(lxi4,1)='#' .and. upper(left(lxi4,3))='#CH'
    lxi1='this.label'+ltrim(str(n_inp))+'.visible'
    &lxi1=.F.
    lxi1='this.text'+ltrim(str(n_inp))+'.visible'
    &lxi1=.F.
    lxi1='this.text'+ltrim(str(n_inp))+'.top'
    _top=&lxi1
    lxi1='this.check'+ltrim(str(n_inp))+'.top'
    &lxi1=_top
    lxi1='this.check'+ltrim(str(n_inp))+'.left'
    &lxi1=140
    lxi1='this.check'+ltrim(str(n_inp))+'.caption'
    &lxi1=lxi2
    lxi1='this.check'+ltrim(str(n_inp))+'.ControlSource'
    &lxi1=lxi3
    lxi1='this.check'+ltrim(str(n_inp))+'.visible'
    &lxi1=.T.
  case left(lxi3,6)='$NADA$'
    lxi1='this.label'+ltrim(str(n_inp))+'.caption'
    &lxi1=lxi2
    lxi2='this.text'+ltrim(str(n_inp))+'.visible=.F.'
    &lxi2
  otherwise
    lxi1='this.label'+ltrim(str(n_inp))+'.caption'
    &lxi1=lxi2
    if left(lxi3,1)='$'
      lxi3=subs(lxi3,2)
      lxi1='this.text'+ltrim(str(n_inp))+'.Enabled=.F.'
      &lxi1
    endif
    lxi1='this.text'+ltrim(str(n_inp))+'.ControlSource'
    &lxi1=lxi3
    do case
    case left(lxi4,1)='D'
      lxi4='99/99/9999'
    case empty(lxi4)
      lxi4=&lxi3
    case left(lxi4,1)='$'
      lxi1='this.text'+ltrim(str(n_inp))+'.Height='+str(val(subs(lxi4,2,1))*17,3)
      &lxi1
      lxi4=&lxi3
    otherwise
      lxi1='this.text'+ltrim(str(n_inp))+'.InputMask'
      &lxi1=lxi4
    endcase
    ndi4=min(len(lxi4)*8+8,448)
    lxi1='this.text'+ltrim(str(n_inp))+'.Width'
    &lxi1=ndi4
  endcase
enddo
ndi=n_inp*17+60
this.height=ndi
for ndi=n_inp+1 to 15
  lxi1='this.label'+ltrim(str(ndi))+'.visible=.F.'
  lxi2='this.text'+ltrim(str(ndi))+'.visible=.F.'
  &lxi1
  &lxi2
endfor
this.line1.top=n_inp*17+17
this.line2.top=n_inp*17+18
this.bot_ok.top=n_inp*17+25
this.bot_esc.top=n_inp*17+25
if _xidiom>1
  this.bot_ok.caption=f3_t(this.bot_ok.caption)
  this.bot_esc.caption=f3_t(this.bot_esc.caption)
endif
move window st3inp center

ENDPROC


PROCEDURE bot_ok.Click
bot_ok=1
thisform.release
ENDPROC


PROCEDURE bot_esc.Click
bot_ok=0
thisform.release
ENDPROC


EndDefine 
