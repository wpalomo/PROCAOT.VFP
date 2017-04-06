Public SY3VAR_45T0S4YC2
SET CLASSLIB TO "st3class.vcx" ADDITIVE

SY3VAR_45T0S4YC2 = CreateObject("SY3VAR" )
If SY3VAR_45T0S4YC2.WindowType == 1
SY3VAR_45T0S4YC2.Show(1)
else
SY3VAR_45T0S4YC2.Show()
EndIf



Define Class SY3VAR as form
Height = 407
Width = 624
ShowTips = .T.
AutoCenter = .T.
BorderStyle = 3
Caption = "Definición de ficheros"
WindowType = 1
BackColor = Rgb(192,192,192)

ADD OBJECT Shape1 as shape WITH Top = 368, Left = 7, Height = 33, Width = 213, BackStyle = 0, SpecialEffect = 0
ADD OBJECT Label2 as l_stit WITH Caption = "[FC_FC]", Left = 12, Top = 0, TabIndex = 1
ADD OBJECT L_stit4 as l_stit WITH Caption = "Modificar", Left = 18, Top = 378, TabIndex = 16
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 378, Left = 585, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 17, ToolTipText = "Salir"
ADD OBJECT bot_dbf as st_bot WITH AutoSize = .F., Top = 374, Left = 108, Height = 23, Width = 100, Caption = "Fichero", TabIndex = 14
ADD OBJECT Pageframe1 as st_frame WITH ErasePage = .T., PageCount = 3, Top = 22, Left = 9, Width = 613, Height = 294, TabIndex = 2, Page1.Caption = "Fichero", Page2.Caption = "Integridad referencial", Page3.Caption = "Datos remotos"
ADD OBJECT "Pageframe1.Page1.List1" as listbox WITH FontBold = .F., FontName = "Courier", RowSourceType = 5, RowSource = "cmp", ControlSource = "m.campo", Height = 171, Left = 7, TabIndex = 4, Top = 90, Width = 218
ADD OBJECT "Pageframe1.Page1.Shape2" as shape WITH Top = 3, Left = 7, Height = 67, Width = 596, BackStyle = 0, SpecialEffect = 0
ADD OBJECT "Pageframe1.Page1.fc_nom" as textbox WITH FontBold = .F., FontName = "Courier", Alignment = 3, ControlSource = "m.FC_NOM", Height = 17, Left = 142, Margin = 0, TabIndex = 1, Top = 9, Width = 164
ADD OBJECT "Pageframe1.Page1.fc_fcs" as textbox WITH FontBold = .F., FontName = "Courier", ControlSource = "m.FC_FCS", Height = 17, Left = 142, Margin = 0, TabIndex = 2, Top = 27, Width = 370
ADD OBJECT "Pageframe1.Page1.List2" as listbox WITH FontBold = .F., FontName = "Courier", RowSourceType = 5, RowSource = "cmpidx", ControlSource = "m.fc_idx", Height = 171, Left = 236, TabIndex = 5, Top = 90, Width = 362, SelectedItemBackColor = Rgb(128,128,128)
ADD OBJECT "Pageframe1.Page1.fc_dbc" as textbox WITH FontBold = .F., FontName = "Courier", Alignment = 3, ControlSource = "m.FC_DBC", Format = "!!!!!!!!!!", Height = 17, Left = 142, Margin = 0, TabIndex = 3, Top = 45, Width = 92
ADD OBJECT "Pageframe1.Page1.L_normal6" as l_normal WITH Caption = "Nombre lógico", Left = 17, Top = 8
ADD OBJECT "Pageframe1.Page1.L_normal7" as l_normal WITH Caption = "Base de datos", Left = 17, Top = 44
ADD OBJECT "Pageframe1.Page1.L_normal8" as l_normal WITH Caption = "Nombre sustitutivo", Left = 17, Top = 26
ADD OBJECT "Pageframe1.Page1.L_normal9" as l_normal WITH Caption = "Variables", Left = 8, Top = 71
ADD OBJECT "Pageframe1.Page1.L_normal10" as l_normal WITH Caption = "Indices", Left = 240, Top = 71
ADD OBJECT "Pageframe1.Page1.FC_REMOTO" as st_chek WITH Top = 8, Left = 359, Width = 101, Caption = "Fichero remoto", ControlSource = "FC_REMOTO", TabIndex = 4
ADD OBJECT "Pageframe1.Page2.grid_dbc" as st_grid WITH ColumnCount = 3, FontBold = .F., FontName = "Courier New", Height = 253, Left = 0, Panel = 1, RecordSource = "sysint", RecordSourceType = 1, RowHeight = 22, Top = 8, Width = 603, Column1.FontBold = .F., Column1.FontName = "Courier New", Column1.ControlSource = "sysint.INT_TIP", Column1.Width = 98, Column1.Sparse = .F., Column2.FontBold = .F., Column2.FontName = "Courier New", Column2.ControlSource = "sysint.INT_FIC", Column2.Width = 82, Column3.FontBold = .F., Column3.FontName = "Courier New", Column3.ControlSource = "sysint.INT_EXP", Column3.Width = 399
ADD OBJECT "Pageframe1.Page2.grid_dbc.Column1.Header1" as header WITH FontName = "MS Sans Serif", FontSize = 8, Caption = "Tipo"
ADD OBJECT "Pageframe1.Page2.grid_dbc.Column1.INT_TIP" as st_combo WITH FontSize = 8, RowSourceType = 5, RowSource = "ri_tipos", Style = 2
ADD OBJECT "Pageframe1.Page2.grid_dbc.Column2.Header1" as header WITH FontName = "MS Sans Serif", FontSize = 8, Caption = "Fichero"
ADD OBJECT "Pageframe1.Page2.grid_dbc.Column2.INT_FIC" as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, BorderStyle = 0, InputMask = "!!!!!!!!!!", Margin = 0, ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Pageframe1.Page2.grid_dbc.Column3.Header1" as header WITH FontName = "MS Sans Serif", FontSize = 8, Caption = "Expresión"
ADD OBJECT "Pageframe1.Page2.grid_dbc.Column3.INT_EXP" as textbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, BorderStyle = 0, Margin = 0, ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT bot_var as st_bot WITH AutoSize = .F., Top = 374, Left = 252, Height = 23, Width = 100, Caption = "Variables", TabIndex = 15
ADD OBJECT FC_CONT as st_chek WITH Top = 324, Left = 99, Width = 88, Caption = "Contabilidad", ControlSource = "FC_CONT", TabIndex = 4
ADD OBJECT FC_FAC as st_chek WITH Top = 342, Left = 99, Caption = "Comercial", ControlSource = "FC_FAC", TabIndex = 5
ADD OBJECT FC_PROD as st_chek WITH Top = 324, Left = 202, Caption = "Producción", ControlSource = "FC_PROD", TabIndex = 6
ADD OBJECT FC_CI as st_chek WITH Top = 342, Left = 297, Caption = "Control instrumental", ControlSource = "FC_CI", TabIndex = 8
ADD OBJECT FC_PL as st_chek WITH Top = 324, Left = 297, Caption = "Control calidad", ControlSource = "FC_CL", TabIndex = 7
ADD OBJECT FC_MOD1 as st_chek WITH Top = 324, Left = 452, Width = 68, Caption = "Módulo 1", ControlSource = "FC_MOD1", TabIndex = 9
ADD OBJECT FC_MOD2 as st_chek WITH Top = 342, Left = 452, Caption = "Módulo 2", ControlSource = "FC_MOD2", TabIndex = 10
ADD OBJECT FC_MOD3 as st_chek WITH Top = 324, Left = 542, Caption = "Módulo 3", ControlSource = "FC_MOD3", TabIndex = 11
ADD OBJECT L_stit3 as l_stit WITH Caption = "Módulos", Left = 10, Top = 324, TabIndex = 3


PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
*
do case
case _esc=.T.
  _esc=.F.
  thisform.bot_esc.click
endcase
ENDPROC
PROCEDURE Release
select SYSFC
gather memvar

ENDPROC
PROCEDURE Init
if empty(m.FC_DBC)
  this.pageframe1.page2.enabled=.F.
endif
thisform.label2.caption=trim(sysfc.FC_FC)
ENDPROC


PROCEDURE bot_esc.Click
bot_ok=0
thisform.release
ENDPROC


PROCEDURE bot_dbf.Click
select TMP
modi stru
numcmp=fcount()
lxerr=''
store space(28) to cmp
tmpsys=_tmp+sys(2015)
copy structure extended to (tmpsys)
use (tmpsys) in 0 alias 'TMPSYS'

select TMPSYS
nd=0
go top
do while !eof()
  nd=nd+1
  nvar=FIELD_NAME
  vtip=FIELD_TYPE
  vlen=FIELD_LEN
  vdec=FIELD_DEC
  nvar=left(nvar+space(10),10)
  select SYSVAR
  seek nvar
  if nd<=maxcmp
    cmp(nd)=iif(eof(),space(18)+nvar,VAR_DES+' '+nvar)
  else
    cmp(maxcmp)=f3_t('... Hay más')
  endif

  if eof()
    scatter memvar blank
    m.VAR_VAR=nvar
    m.VAR_LN=vlen
    m.VAR_DEC=vdec
    do case
    case vtip='N'
      m.VAR_TIPO='Numérico'
      if vdec=0
        m.VAR_FMT=repl('9',vlen)
      else
        m.VAR_FMT=repl('9',vlen-vdec-1)+'.'+repl('9',vdec)
      endif
    case vtip='C'
      m.VAR_TIPO='Carácter'
      m.VAR_FMT="repl('X',"+ltrim(str(vlen))+')'
      m.VAR_MAY='May./Min.'
    case vtip='D'
      m.VAR_TIPO='Fecha'
      m.VAR_FMT='D'
    case vtip='T'
      m.VAR_TIPO='Hora'
      m.VAR_FMT='T'
    case vtip='M'
      m.VAR_TIPO='Memo'
    case vtip='G'
      m.VAR_TIPO='General'
    case vtip='L'
      m.VAR_TIPO='Lógico'
    endcase
    insert into SYSVAR from memvar
    lxerr=lxerr+nvar+' '+f3_t('AVISO, No existía en SYSVAR')+cr
  else
    lx1=left(VAR_TIPO, 1)
    lx1=iif(lx1=='F', 'D', lx1)
    lx1=iif(lx1=='H', 'T', lx1)

    do case
    case at(vtip,'NCLDTMG')=0
      lxerr=lxerr+nvar+f3_t(' Tipo no contemplado por el Parametrizador')+cr
    case vtip<>lx1
      lxerr=lxerr+nvar+f3_t(' Habría de ser de tipo ['+VAR_TIPO+']')+cr
    case (vtip='C'.or.vtip='N') .and. vlen<>VAR_LN
      lxerr=lxerr+nvar+f3_t(' Habría de estar dimensionada a ['+str(VAR_LN,3)+'] carácteres')+cr
    case vtip='N' .and. vdec<>VAR_DEC
      lxerr=lxerr+nvar+f3_t(' Habría de tener ['+str(VAR_DEC,2)+'] decimales')+cr
    endcase
  endif
  select TMPSYS
  skip
enddo

use
delete file (tmpsys)
select TMP
store '' to cmpidx,lx1
store .F. to hay_codigo
for nd=1 to 15
  lx=key(nd)
  if !empty(lx)
    lx2=tag(nd)+'  '
    hay_codigo=iif(lx2='CODIGO  ',.T.,hay_codigo)
    cmpidx(nd)=tag(nd)+' '+key(nd)
    lx1=lx1+tag(nd)+' '+key(nd)+cr
  else
    exit
  endif
endfor

lxerr=iif(hay_codigo=.F.,lxerr+f3_t('Falta el índice CODIGO')+cr,lxerr)
if !empty(lxerr)
  define window wedit from 2,0 to 30,70 title f3_t('Incidencias') font 'Courier',10 color RGB(0,0,0,255,255,128)
  move window wedit center
  activate window wedit
  SW_OKE=0
  @ .5,1 edit lxerr size 22,68 scroll font 'Courier',10 tab noedit when mdown()
  @ 23,0 to 23,70
  @ 23.5,63 get SW_OKE pict '@*BHT BMP\CLOSE' size 2.5,4.5
  read modal
  deactivate window wedit
  release window wedit
endif

select SYSFC
repl FC_KEY with lx1
thisform.refresh

ENDPROC


PROCEDURE Pageframe1.Page1.fc_dbc.Valid
if empty(m.FC_DBC)
  thisform.pageframe1.page2.enabled=.F.
else
  thisform.pageframe1.page2.enabled=.T.
endif

ENDPROC


PROCEDURE Pageframe1.Page2.grid_dbc.Column1.INT_TIP.Valid
if empty(INT_TIP)
  repl INT_FIC with '',INT_EXP with ''
  thisform.refresh
endif
ENDPROC


PROCEDURE Pageframe1.Page2.grid_dbc.Column2.INT_FIC.When
_ok=iif(empty(sysint.INT_TIP),.F.,.T.)
return _ok

ENDPROC
PROCEDURE Pageframe1.Page2.grid_dbc.Column2.INT_FIC.Valid
select SYSFC
r1=iif(eof(),0,recno())
seek sysint.INT_FIC
if eof()
  _ok=.F.
else
  _ok=.T.
  lx=''
  for nd2=1 to memlines(sysfc.FC_KEY)
    lx1=mline(sysfc.FC_KEY,nd2)
    if left(lx1,7)='CODIGO '
      lx=trim(subs(lx1,8))
      nd2=999
    endif
  endfor
  repl sysint.INT_EXP with lx
endif
select SYSFC
if r1>0
  go r1
endif
return _ok
ENDPROC


PROCEDURE Pageframe1.Page2.grid_dbc.Column3.INT_EXP.When
_ok=iif(empty(INT_FIC),.F.,.T.)
return _ok

ENDPROC


PROCEDURE bot_var.Click
select SYSPRG
do form sy3var
select SYSFC

ENDPROC


EndDefine 
