Public SY3VAR_45T0S4YTG
SET CLASSLIB TO "st3class.vcx" ADDITIVE

SY3VAR_45T0S4YTG = CreateObject("SY3VAR" )
If SY3VAR_45T0S4YTG.WindowType == 1
SY3VAR_45T0S4YTG.Show(1)
else
SY3VAR_45T0S4YTG.Show()
EndIf



Define Class SY3VAR as form
Height = 350
Width = 635
ShowTips = .T.
AutoCenter = .T.
BorderStyle = 2
Caption = (f3_t('Definición de Variables'))
BackColor = Rgb(192,192,192)

ADD OBJECT L_normal1 as l_normal WITH Caption = "Variable", Left = 16, Top = 8, TabIndex = 3
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 312, Left = 584, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 38, ToolTipText = "Salir"
ADD OBJECT Pageframe1 as st_frame WITH ErasePage = .T., PageCount = 2, Top = 32, Left = 8, Width = 609, Height = 273, Page1.Caption = "Variables", Page2.Caption = "Definición"
ADD OBJECT "Pageframe1.Page1.Shape2" as shape WITH Top = 193, Left = 488, Height = 45, Width = 112, BackStyle = 0, SpecialEffect = 0
ADD OBJECT "Pageframe1.Page1.List3" as listbox WITH FontBold = .F., FontName = "Courier", ColumnCount = 2, ColumnWidths = "88,300", RowSourceType = 6, RowSource = "SYSVAR", ControlSource = "m.var_var", Height = 233, Left = 15, TabIndex = 1, Top = 8, Width = 457
ADD OBJECT "Pageframe1.Page1.bot_bajav" as st_bot WITH AutoSize = .F., Top = 200, Left = 527, Height = 29, Width = 29, Picture = "bmp\baja.bmp", Caption = "", TabIndex = 3, ToolTipText = "Eliminar"
ADD OBJECT "Pageframe1.Page1.bot_dupv" as st_bot WITH AutoSize = .F., Top = 200, Left = 559, Height = 29, Width = 29, Picture = "bmp\alta2.bmp", Caption = "", TabIndex = 4, ToolTipText = "Duplicar"
ADD OBJECT "Pageframe1.Page1.bot_altav" as st_bot WITH AutoSize = .F., Top = 200, Left = 495, Height = 29, Width = 29, Picture = "bmp\alta.bmp", Caption = "", TabIndex = 2, ToolTipText = "Crear"
ADD OBJECT "Pageframe1.Page2.var_ln" as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.var_ln", Height = 17, InputMask = "999", Left = 135, Margin = 0, TabIndex = 8, Top = 41, Width = 32
ADD OBJECT "Pageframe1.Page2.L_normal22" as l_normal WITH Caption = "Formato", Left = 15, Top = 57, TabIndex = 11
ADD OBJECT "Pageframe1.Page2.L_normal23" as l_normal WITH Caption = "Valor a buscar", Left = 111, Top = 74, TabIndex = 16
ADD OBJECT "Pageframe1.Page2.L_normal25" as l_normal WITH Caption = "Formato", Left = 314, Top = 22, TabIndex = 5
ADD OBJECT "Pageframe1.Page2.ITR_APROX1" as st_chek WITH Top = 91, Left = 556, Caption = "", ControlSource = "m.itr_aprox1", TabIndex = 22
ADD OBJECT "Pageframe1.Page2.HL_FC" as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.HL_FC", Format = "!!!!!!!!!!", Height = 17, Left = 402, Margin = 0, TabIndex = 14, Top = 57, Width = 91
ADD OBJECT "Pageframe1.Page2.L_normal28" as l_normal WITH FontItalic = .T., FontSize = 10, FontUnderline = .T., Caption = "Campos a inicializar", Left = 7, Top = 145, TabIndex = 31, ForeColor = Rgb(0,0,128)
ADD OBJECT "Pageframe1.Page2.L_normal3" as l_normal WITH Caption = "Indice", Left = 15, Top = 74, TabIndex = 15
ADD OBJECT "Pageframe1.Page2.ITR_APROX3" as st_chek WITH Top = 125, Left = 556, Caption = "", ControlSource = "m.itr_aprox3", TabIndex = 30
ADD OBJECT "Pageframe1.Page2.L_normal2" as l_normal WITH Caption = "Aprox", Left = 551, Top = 74, TabIndex = 18
ADD OBJECT "Pageframe1.Page2.var_dec" as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.var_dec", Height = 17, InputMask = "9", Left = 402, Margin = 0, TabIndex = 10, Top = 41, Width = 16
ADD OBJECT "Pageframe1.Page2.L_normal20" as l_normal WITH Caption = "Tipo de variable", Left = 15, Top = 21, TabIndex = 3
ADD OBJECT "Pageframe1.Page2.VAR_FMT" as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.VAR_FMT", Height = 17, InputMask = "XXXXXXXXXXXXXXXXXX", Left = 135, Margin = 0, TabIndex = 12, TabStop = .F., Top = 57, Width = 163
ADD OBJECT "Pageframe1.Page2.L_normal21" as l_normal WITH Caption = "No. de carácteres", Left = 15, Top = 41, TabIndex = 7
ADD OBJECT "Pageframe1.Page2.L_normal24" as l_normal WITH Caption = "Fichero", Left = 455, Top = 74, TabIndex = 17
ADD OBJECT "Pageframe1.Page2.L_normal26" as l_normal WITH Caption = "Decimales", Left = 314, Top = 41, TabIndex = 9
ADD OBJECT "Pageframe1.Page2.ITR_APROX2" as st_chek WITH Top = 108, Left = 556, Caption = "", ControlSource = "m.itr_aprox2", TabIndex = 26
ADD OBJECT "Pageframe1.Page2.L_normal7" as l_normal WITH Caption = "Fichero ayuda", Left = 314, Top = 57, TabIndex = 13
ADD OBJECT "Pageframe1.Page2.itr_blanc" as editbox WITH FontBold = .F., FontName = "Courier", Height = 80, Left = 7, TabIndex = 32, Top = 161, Width = 545, ControlSource = "m.itr_blanc"
ADD OBJECT "Pageframe1.Page2.VAR_TIPO" as st_combo WITH RowSourceType = 1, RowSource = "Carácter,Numérico,Fecha,Hora,Lógico,Memo,General", ControlSource = "m.VAR_TIPO", Left = 135, NumberOfElements = 7, TabIndex = 4, Top = 22
ADD OBJECT "Pageframe1.Page2.VAR_MAY" as st_combo WITH RowSourceType = 1, RowSource = "Mayúsculas,May./Min", ControlSource = "m.VAR_MAY", Left = 402, NumberOfElements = 2, TabIndex = 6, Top = 22
ADD OBJECT "Pageframe1.Page2.ITR_IDX1" as st_get WITH ControlSource = "m.ITR_IDX1", Height = 18, InputMask = "!!!!!!!!!!", Left = 15, TabIndex = 19, Top = 90, Width = 89
ADD OBJECT "Pageframe1.Page2.ITR_VALOR1" as st_get WITH ControlSource = "m.ITR_VALOR1", Height = 18, Left = 111, TabIndex = 20, Top = 90, Width = 337
ADD OBJECT "Pageframe1.Page2.ITR_FIC1" as st_get WITH ControlSource = "m.ITR_FIC1", Height = 18, InputMask = "!!!!!!!!!!", Left = 455, TabIndex = 21, Top = 90, Width = 89
ADD OBJECT "Pageframe1.Page2.ITR_FIC2" as st_get WITH ControlSource = "m.ITR_FIC2", Height = 18, InputMask = "!!!!!!!!!!", Left = 455, TabIndex = 25, Top = 107, Width = 89
ADD OBJECT "Pageframe1.Page2.ITR_FIC3" as st_get WITH ControlSource = "m.ITR_FIC3", Height = 18, InputMask = "!!!!!!!!!!", Left = 455, TabIndex = 29, Top = 124, Width = 89
ADD OBJECT "Pageframe1.Page2.ITR_IDX2" as st_get WITH ControlSource = "m.ITR_IDX2", Height = 18, InputMask = "!!!!!!!!!!", Left = 15, TabIndex = 23, Top = 107, Width = 89
ADD OBJECT "Pageframe1.Page2.ITR_IDX3" as st_get WITH ControlSource = "m.ITR_IDX3", Height = 18, InputMask = "!!!!!!!!!!", Left = 15, TabIndex = 27, Top = 124, Width = 89
ADD OBJECT "Pageframe1.Page2.ITR_VALOR2" as st_get WITH ControlSource = "m.ITR_VALOR2", Height = 18, Left = 111, TabIndex = 24, Top = 107, Width = 337
ADD OBJECT "Pageframe1.Page2.ITR_VALOR3" as st_get WITH ControlSource = "m.ITR_VALOR3", Height = 18, Left = 111, TabIndex = 28, Top = 124, Width = 337
ADD OBJECT "Pageframe1.Page2.L_normal1" as l_normal WITH Caption = "Descripción", Left = 15, Top = 4, TabIndex = 1
ADD OBJECT "Pageframe1.Page2.var_des" as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.VAR_DES", Height = 18, Left = 135, Margin = 0, TabIndex = 2, Top = 4, Width = 337
ADD OBJECT "Pageframe1.Page2.bot_modv" as st_bot WITH AutoSize = .F., Top = 210, Left = 567, Height = 29, Width = 29, Picture = "bmp\graba.bmp", Caption = "", TabIndex = 33, ToolTipText = "Grabar"
ADD OBJECT var_des as st_say3d WITH ControlSource = "m.VAR_DES", Height = 18, Left = 176, Top = 8, Width = 337
ADD OBJECT var_var as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.VAR_VAR", Format = "", Height = 18, InputMask = "!!!!!!!!!!", Left = 80, Margin = 0, TabIndex = 1, TabStop = .F., Top = 8, Width = 85


PROCEDURE formato_var
M.VAR_LN=max(1,M.VAR_LN)
M.VAR_DEC=iif(left(M.VAR_TIPO,1)='N',M.VAR_DEC,0)
do case
case left(M.VAR_TIPO,1)='N'
  if left(M.VAR_MAY,1)='C' .and. M.VAR_LN>3            && Número con formato
    M.VAR_LN=iif(M.VAR_DEC=0,M.VAR_LN,max(M.VAR_LN,M.VAR_DEC+2))
    nd=iif(M.VAR_DEC=0,M.VAR_LN,M.VAR_LN-M.VAR_DEC-1)
    do case
    case nd<=6
      M.VAR_FMT=repl('9',nd-3)+',999'
    case nd<=9
      M.VAR_FMT=repl('9',nd-6)+',999,999'
    otherwise
      M.VAR_FMT=repl('9',nd-9)+',999,999,999'
    endcase
    if M.VAR_DEC>0
      M.VAR_FMT=M.VAR_FMT+'.'+repl('9',M.VAR_DEC)
    endif
  else
    if M.VAR_DEC=0
      M.var_fmt=repl('9',M.VAR_LN)
    else
      M.VAR_LN=max(M.VAR_LN,M.VAR_DEC+2)
      M.VAR_FMT=repl('9',M.VAR_LN-M.VAR_DEC-1)+'.'+repl('9',M.VAR_DEC)
    endif
  endif
case left(M.VAR_TIPO,1)='C'
  lx=iif(at('/',M.VAR_MAY)>0,'X','!')
  M.VAR_FMT="repl('"+lx+"',"+ltrim(str(M.VAR_LN))+')'
case left(M.VAR_TIPO,1)='F'
  M.VAR_FMT='D'
  M.VAR_LN=10
case left(M.VAR_TIPO,1)='H'
  M.VAR_FMT='T'
  M.VAR_LN=18
endcase
***thisform.refresh
ENDPROC
PROCEDURE Init
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

select SYSVAR
scatter memvar memo
scatter name _ovar
*select SYSPRG
*thisform.refresh

ENDPROC
PROCEDURE Refresh
*>
with thisforM.Pageframe1.page2
do case
case left(M.var_tipo,1)='C'                        && Carácter
  M.var_dec=0
  .var_ln.enabled=.T.
  .var_dec.visible=.F.
  .var_may.visible=.T.
  .var_may.RowSource='Mayúsculas,May./Min'
case left(M.var_tipo,1)='N'                        && Numérico
  .var_ln.enabled=.T.
  .var_dec.visible=.T.
  .var_may.visible=.T.
  .var_may.RowSource='Con formato,Sin formato'
case left(M.var_tipo,1)='F'                        && Fecha
  M.var_ln=10
  M.var_dec=0
  M.var_may=space(len(M.var_may))
  .var_ln.enabled=.F.
  .var_dec.visible=.F.
  .var_may.visible=.F.
case left(M.var_tipo,1)='H'                        && Hora
  M.var_ln=18
  M.var_dec=0
  M.var_may=space(len(M.var_may))
  .var_ln.enabled=.F.
  .var_dec.visible=.F.
  .var_may.visible=.F.
case left(M.var_tipo,1)='L'                        && Lógico
  M.var_ln=1
  M.var_dec=0
  M.var_may=space(len(M.var_may))
  .var_ln.enabled=.F.
  .var_dec.visible=.F.
  .var_may.visible=.F.
case left(M.var_tipo,1)='M' .or. left(M.var_tipo,1)='G'  && Memo/General
  M.var_ln=4
  M.var_dec=0
  M.var_may=space(len(M.var_may))
  .var_ln.enabled=.F.
  .var_dec.visible=.F.
  .var_may.visible=.F.
endcase

*
if empty(M.ITR_IDX1)
  store '' to M.ITR_VALOR1,M.ITR_FIC1
  M.ITR_APROX1=0
  .ITR_VALOR1.enabled=.F.
  .ITR_FIC1.enabled=.F.
  .ITR_APROX1.enabled=.F.
else
  .ITR_VALOR1.enabled=.T.
  .ITR_FIC1.enabled=.T.
  .ITR_APROX1.enabled=iif(M.ITR_APROX2=1.or.M.ITR_APROX3=1,.F.,.T.)
endif

if empty(M.ITR_IDX2)
  store '' to M.ITR_VALOR2,M.ITR_FIC2
  M.ITR_APROX2=0
  .ITR_VALOR2.enabled=.F.
  .ITR_FIC2.enabled=.F.
  .ITR_APROX2.enabled=.F.
else
  .ITR_VALOR2.enabled=.T.
  .ITR_FIC2.enabled=.T.
  .ITR_APROX2.enabled=iif(M.ITR_APROX1=1.or.M.ITR_APROX3=1,.F.,.T.)
endif

if empty(M.ITR_IDX3)
  store '' to M.ITR_VALOR3,M.ITR_FIC3
  M.ITR_APROX3=0
  .ITR_VALOR3.enabled=.F.
  .ITR_FIC3.enabled=.F.
  .ITR_APROX3.enabled=.F.
else
  .ITR_VALOR3.enabled=.T.
  .ITR_FIC3.enabled=.T.
  .ITR_APROX3.enabled=iif(M.ITR_APROX1=1.or.M.ITR_APROX2=1,.F.,.T.)
endif
endwith

ENDPROC


PROCEDURE bot_esc.Click
thisform.release
ENDPROC


PROCEDURE Pageframe1.Page1.Activate
*>
thisform.var_var.enabled = .T.


ENDPROC
PROCEDURE Pageframe1.Page2.Activate
*>
thisform.var_var.valid
thisform.var_var.enabled = .F.
thisform.pageframe1.page2.var_des.setfocus

ENDPROC


PROCEDURE Pageframe1.Page1.List3.Click
m.var_var=sysvar.VAR_VAR
thisform.var_var.valid
*select SYSPRG
***thisform.refresh
ENDPROC
PROCEDURE Pageframe1.Page1.List3.DblClick
*>
ThisForm.PageFrame1.ActivePage = 2
ThisForm.Refresh

ENDPROC


PROCEDURE Pageframe1.Page1.bot_bajav.Click
_ok=f3_sn(2,1,'Desea eliminar la variable ['+sysvar.VAR_VAR+']',sysvar.VAR_DES)
if _ok
  select SYSVAR
  delete record recno()
  set near on
  seek m.VAR_VAR
  set near off
  if eof()
    go bottom
  endif
  scatter memvar memo
*  select SYSPRG
***  thisform.refresh
endif

ENDPROC


PROCEDURE Pageframe1.Page1.bot_dupv.Click
store space(10) to m.varvar
m.var_varo=m.VAR_VAR
_ok=f3_inp('Duplicar una variable','Variable origen,Variable destino','!var_varo,m.varvar',',!!!!!!!!!!')
if !empty(m.varvar).and. !empty(m.var_varo) .and. _ok
  select SYSVAR
  seek m.VAR_VARO
  if !eof()
    scatter memvar memo
    itrblanc=ITR_BLANC
    m.VAR_VAR=varvar
    seek m.VAR_VAR
    if eof()
      append blank
      gather memvar memo
      repl ITR_BLANC with itrblanc
***      thisform.refresh
      thisform.pageframe1.Page2.VAR_DES.setfocus
      thisform.pageframe1.ActivePage=2
    else
      =f3_sn(1,1,'Nombre de variable repetido')
    endif
  else
    =f3_sn(1,1,'Nombre de variable origen inexistente')
  endif
*  select SYSPRG
endif

ENDPROC


PROCEDURE Pageframe1.Page1.bot_altav.Click
varvar=m.VAR_VAR
m.var_var=space(10)
_ok=f3_inp('Definir una variable','Nombre de la variable','m.var_var','!!!!!!!!!!')
if !empty(m.var_var) .and. _ok
  select SYSVAR
  seek m.VAR_VAR
  if eof()
    append blank
    repl VAR_VAR with m.VAR_VAR
    scatter memvar memo
***    thisform.refresh
    thisform.pageframe1.Page2.VAR_DES.setfocus
    thisform.pageframe1.ActivePage=2
  else
    =f3_sn(1,1,'Nombre de variable repetido')
    m.VAR_VAR=varvar
  endif
  select SYSFC
else
  m.VAR_VAR=varvar
endif

ENDPROC


PROCEDURE Pageframe1.Page2.var_ln.Valid
thisform.formato_var

ENDPROC


PROCEDURE Pageframe1.Page2.ITR_APROX1.Click
if m.itr_aprox1=1
  store 0 to m.itr_aprox2,m.itr_aprox3
  thisform.pageframe1.page2.itr_aprox2.enabled=.T.
  thisform.pageframe1.page2.itr_aprox3.enabled=.T.
else
  thisform.pageframe1.page2.itr_aprox2.enabled=.F.
  thisform.pageframe1.page2.itr_aprox3.enabled=.F.
endif
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_APROX3.Click
if m.itr_aprox3=1
  store 0 to m.itr_aprox2,m.itr_aprox1
  thisform.pageframe1.page2.itr_aprox2.enabled=.T.
  thisform.pageframe1.page2.itr_aprox1.enabled=.T.
else
  thisform.pageframe1.page2.itr_aprox2.enabled=.F.
  thisform.pageframe1.page2.itr_aprox1.enabled=.F.
endif
ENDPROC


PROCEDURE Pageframe1.Page2.var_dec.Valid
thisform.formato_var

ENDPROC


PROCEDURE Pageframe1.Page2.ITR_APROX2.Click
if m.itr_aprox2=1
  store 0 to m.itr_aprox1,m.itr_aprox3
  thisform.pageframe1.page2.itr_aprox1.enabled=.T.
  thisform.pageframe1.page2.itr_aprox3.enabled=.T.
else
  thisform.pageframe1.page2.itr_aprox1.enabled=.F.
  thisform.pageframe1.page2.itr_aprox3.enabled=.F.
endif
ENDPROC


PROCEDURE Pageframe1.Page2.VAR_TIPO.Valid
thisform.formato_var
ENDPROC


PROCEDURE Pageframe1.Page2.VAR_MAY.Valid
thisform.formato_var

ENDPROC


PROCEDURE Pageframe1.Page2.ITR_IDX1.Valid
thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_VALOR1.Valid
***thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_FIC1.Valid
***thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_FIC2.Valid
***thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_FIC3.Valid
***thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_IDX2.Valid
thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_IDX3.Valid
thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_VALOR2.Valid
***thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.ITR_VALOR3.Valid
***thisform.refresh
ENDPROC


PROCEDURE Pageframe1.Page2.bot_modv.Click
select SYSVAR
if !empty(m.VAR_VAR) .and. sysvar.VAR_VAR=m.VAR_VAR
  gather memvar memo
endif
***thisform.refresh
*select SYSPRG
thisform.pageframe1.ActivePage=1

ENDPROC


PROCEDURE var_var.LostFocus
if thisform.pageframe1.Activepage=2
  this.valid
  thisform.pageframe1.Page2.VAR_DES.setfocus
  thisform.refresh
endif
ENDPROC
PROCEDURE var_var.Valid
if !empty(m.VAR_VAR)
  Set Near On
  select SYSVAR
  seek m.VAR_VAR
  Set Near Off
  if eof()
    select SYSPRG
    return .F.
  endif

  scatter memvar memo
  select SYSPRG
  thisform.refresh
* thisform.Pageframe1.ActivePage=2
endif
ENDPROC


EndDefine 
