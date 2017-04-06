Public st3hlp_45T0S4VK1
SET CLASSLIB TO "st3class.vcx" ADDITIVE

st3hlp_45T0S4VK1 = CreateObject("st3hlp" )
If st3hlp_45T0S4VK1.WindowType == 1
st3hlp_45T0S4VK1.Show(1)
else
st3hlp_45T0S4VK1.Show()
EndIf



Define Class st3hlp as form
Height = 268
Width = 577
AutoCenter = .T.
BorderStyle = 3
Caption = "Buscar"
Closable = .F.
FontBold = .F.
FontName = "Courier"
FontSize = 10
MaxButton = .F.
Visible = .F.
TabStop = .F.
WindowType = 1
WindowState = 0
LockScreen = .F.
AlwaysOnTop = .T.
WhatsThisHelpID = 888
ColorSource = 5
BackColor = Rgb(255,255,128)

ADD OBJECT Label4 as label WITH AutoSize = .T., FontBold = .T., FontSize = 8, BackStyle = 0, Caption = "", Height = 16, Left = 12, Top = 287, Width = 2, TabIndex = 8, ColorSource = 0
ADD OBJECT Grid1 as grid WITH ColumnCount = 20, FontBold = .F., FontSize = 8, DeleteMark = .F., GridLines = 2, Height = 225, Left = 9, Panel = 1, ReadOnly = .T., RecordMark = .F., RecordSource = "ayuda", RecordSourceType = 1, RowHeight = 14, TabIndex = 1, Top = 5, Visible = .T., Width = 560, Column1.FontBold = .F., Column1.FontSize = 8, Column1.ControlSource = "", Column1.Width = 200, Column1.ReadOnly = .T., Column1.Visible = .T., Column2.FontBold = .F., Column2.FontSize = 8, Column2.ControlSource = "", Column2.Width = 200, Column2.ReadOnly = .T., Column2.Visible = .T., Column3.FontBold = .F., Column3.FontSize = 8, Column3.ControlSource = "", Column3.Width = 200, Column3.ReadOnly = .T., Column3.Visible = .T., Column4.FontBold = .F., Column4.FontSize = 8, Column4.ControlSource = "", Column4.Width = 200, Column4.ReadOnly = .T., Column4.Visible = .T., Column5.FontBold = .F., Column5.FontSize = 8, Column5.ControlSource = "", Column5.Width = 200, Column5.ReadOnly = .T., Column5.Visible = .T., Column6.FontBold = .F., Column6.FontSize = 8, Column6.ControlSource = "", Column6.Width = 200, Column6.ReadOnly = .T., Column6.Visible = .T., Column7.FontBold = .F., Column7.FontSize = 8, Column7.ControlSource = "", Column7.Width = 200, Column7.ReadOnly = .T., Column7.Visible = .T., Column8.FontBold = .F., Column8.FontSize = 8, Column8.ControlSource = "", Column8.Width = 200, Column8.ReadOnly = .T., Column8.Visible = .T., Column9.FontBold = .F., Column9.FontSize = 8, Column9.ControlSource = "", Column9.Width = 200, Column9.ReadOnly = .T., Column9.Visible = .T., Column10.FontBold = .F., Column10.FontSize = 8, Column10.ControlSource = "", Column10.Width = 200, Column10.ReadOnly = .T., Column10.Visible = .T., Column11.FontBold = .F., Column11.FontSize = 8, Column11.ControlSource = "", Column11.Width = 200, Column11.ReadOnly = .T., Column11.Visible = .T., Column12.FontBold = .F., Column12.FontSize = 8, Column12.ControlSource = "", Column12.Width = 200, Column12.ReadOnly = .T., Column12.Visible = .T., Column13.FontBold = .F., Column13.FontSize = 8, Column13.ControlSource = "", Column13.Width = 200, Column13.ReadOnly = .T., Column13.Visible = .T., Column14.FontBold = .F., Column14.FontSize = 8, Column14.ControlSource = "", Column14.Width = 200, Column14.ReadOnly = .T., Column14.Visible = .T., Column15.FontBold = .F., Column15.FontSize = 8, Column15.ControlSource = "", Column15.Width = 200, Column15.ReadOnly = .T., Column15.Visible = .T., Column16.FontBold = .F., Column16.FontSize = 8, Column16.ControlSource = "", Column16.Width = 200, Column16.ReadOnly = .T., Column16.Visible = .T., Column17.FontBold = .F., Column17.FontSize = 8, Column17.ControlSource = "", Column17.Width = 200, Column17.ReadOnly = .T., Column17.Visible = .T., Column18.FontBold = .F., Column18.FontSize = 8, Column18.ControlSource = "", Column18.Width = 200, Column18.ReadOnly = .T., Column18.Visible = .T., Column19.FontBold = .F., Column19.FontSize = 8, Column19.ControlSource = "", Column19.Width = 200, Column19.ReadOnly = .T., Column19.Visible = .T., Column20.FontBold = .F., Column20.FontSize = 8, Column20.ControlSource = "", Column20.Width = 200, Column20.ReadOnly = .T., Column20.Visible = .T.
ADD OBJECT "Grid1.Column1.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column1.Text1" as textbox WITH FontBold = .F., FontSize = 8, BackStyle = 0, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column2.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column2.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column3.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column3.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column4.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column4.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column5.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column5.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column6.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column6.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column7.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column7.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column8.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column8.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column9.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column9.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column10.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column10.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column11.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column11.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column12.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column12.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column13.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column13.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column14.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column14.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column15.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column15.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column16.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column16.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column17.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column17.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column18.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column18.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column19.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column19.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT "Grid1.Column20.Header1" as header WITH FontBold = .F., FontName = "MS Sans Serif", FontSize = 8, Caption = "Header1"
ADD OBJECT "Grid1.Column20.Text1" as textbox WITH FontBold = .F., FontSize = 8, BorderStyle = 0, Margin = 0, ReadOnly = .T., ColorSource = 3, ForeColor = Rgb(0,0,0), BackColor = Rgb(255,255,255)
ADD OBJECT L_normal1 as l_normal WITH FontSize = 7, Caption = "Click sobre cabecera de una columna, ordena la ayuda por dicha columna", Left = 9, Top = 246, TabIndex = 6, ForeColor = Rgb(0,0,255)
ADD OBJECT L_normal2 as l_normal WITH FontSize = 7, Caption = "Click derecho sobre cabecera de una columna, establece filtro sobre dicha columna", Left = 9, Top = 256, TabIndex = 7, ForeColor = Rgb(0,0,255)
ADD OBJECT bot_ok as st_bot WITH AutoSize = .F., Top = 234, Left = 510, Height = 29, Width = 29, Picture = "bmp\ok.bmp", Caption = "", TabIndex = 3, ToolTipText = "Aceptar [CTRL+ENTER]"
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 234, Left = 540, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 4, ToolTipText = "Salir [ESC]"
ADD OBJECT Bot_prn as st_bot WITH AutoSize = .F., Top = 234, Left = 451, Height = 29, Width = 29, Picture = "bmp\printp.bmp", Caption = "", TabIndex = 2, ToolTipText = "Imprimir"
ADD OBJECT L_normal3 as l_normal WITH FontSize = 7, Caption = "Doble Click sobre una columna, ordena la ayuda por dicha columna en orden inverso", Left = 9, Top = 235, TabIndex = 5, ForeColor = Rgb(0,0,255)


PROCEDURE ordenar
Parameter _n_campo
*
lx1=sys(2015)
lx2=_tmp+lx1
select AYUDA
lx3=field(_n_campo)
sort to (lx2) on &lx3 /A
zap
append from (lx2)
lx2=lx2+'.DBF'
delete file (lx2)
thisform.refresh

ENDPROC
PROCEDURE filtro
Parameter _col
*
store space(2) to _cond1,_cond2,_cond3,_cond4
store space(20) to _txt1,_txt2,_txt3,_txt4
if !empty(_xbusca(_col,5))
  lx1=_xbusca(_col,5)
  _cond1=left(lx1,2)
  _txt1=subs(lx1,3)
  if !empty(_xbusca(_col,6))
    lx1=_xbusca(_col,6)
    _cond2=left(lx1,2)
    _txt2=subs(lx1,3)
  endif
  if !empty(_xbusca(_col,7))
    lx1=_xbusca(_col,7)
    _cond3=left(lx1,2)
    _txt3=subs(lx1,3)
    if !empty(_xbusca(_col,8))
      lx1=_xbusca(_col,8)
      _cond4=left(lx1,2)
      _txt4=subs(lx1,3)
    endif
  endif
endif
_ok=.T.
_txtvar=_xbusca(_col,3)
do form st3hlpf
if _ok=.T.
  _xbusca(_col,5)=_cond1+_txt1
  _xbusca(_col,6)=_cond2+_txt2
  _xbusca(_col,7)=_cond3+_txt3
  _xbusca(_col,8)=_cond4+_txt4
  _hlp_filtro=''
  for _num=1 to _xbnum
    lx1='thisform.grid1.Column'+ltrim(str(_num))+'.Header1.Backcolor'
    if empty(_xbusca(_num,5))
      &lx1=RGB(192,192,192)
    else
      _hlp_campo=trim(_xbusca(_num,1))
      _hlp_tipo=_xbusca(_num,4)
      store '' to lxx1,lxx2,lxx3,lxx4
      &lx1=RGB(0,255,255)
      lxx1=_hlp_campo+trim(left(_xbusca(_num,5),2))
      lx1=trim(subs(_xbusca(_num,5),3))
      do case
      case _hlp_tipo='N'
        lxx1=lxx1+lx1
      case _hlp_tipo='D'
        lxx1=lxx1+'{'+lx1+'}'
      otherwise
        lxx1=lxx1+'"'+lx1+'"'
      endcase
      if !empty(_xbusca(_num,6))
        lxx2='.and.'+_hlp_campo+trim(left(_xbusca(_num,6),2))
        lx1=trim(subs(_xbusca(_num,6),3))
        do case
        case _hlp_tipo='N'
          lxx2=lxx2+lx1
        case _hlp_tipo='D'
          lxx2=lxx2+'{'+lx1+'}'
        otherwise
          lxx2=lxx2+'"'+lx1+'"'
        endcase
      endif
      if !empty(_xbusca(_num,7))
        lxx3=_hlp_campo+trim(left(_xbusca(_num,7),2))
        lx1=trim(subs(_xbusca(_num,7),3))
        do case
        case _hlp_tipo='N'
          lxx3=lxx3+lx1
        case _hlp_tipo='D'
          lxx3=lxx3+'{'+lx1+'}'
        otherwise
          lxx3=lxx3+'"'+lx1+'"'
        endcase
        if !empty(_xbusca(_num,8))
          lxx4='.and.'+_hlp_campo+trim(left(_xbusca(_num,8),2))
          lx1=trim(subs(_xbusca(_num,8),3))
          do case
          case _hlp_tipo='N'
            lxx4=lxx4+lx1
          case _hlp_tipo='D'
            lxx4=lxx4+'{'+lx1+'}'
          otherwise
            lxx4=lxx4+'"'+lx1+'"'
          endcase
        endif
      endif
      ndh=at('**',lxx1)
      if ndh>1
        lxx1='atc('+subs(lxx1,ndh+2)+','+left(lxx1,ndh-1)+')>0'
      endif
      ndh=at('**',lxx2)
      if ndh>1
        lxx2='atc('+subs(lxx2,ndh+2)+','+left(lxx2,ndh-1)+')>0'
      endif
      if empty(lxx3)
        _hlp_filtro=_hlp_filtro+'.and.'+lxx1+lxx2
      else
        ndh=at('**',lxx3)
        if ndh>1
          lxx3='atc('+subs(lxx3,ndh+2)+','+left(lxx3,ndh-1)+')>0'
        endif
        ndh=at('**',lxx4)
        if ndh>1
          lxx4='atc('+subs(lxx4,ndh+2)+','+left(lxx4,ndh-1)+')>0'
        endif
        _hlp_filtro=_hlp_filtro+'.and.(('+lxx1+lxx2+').or.('+lxx3+lxx4+'))'
      endif
    endif
  endfor
  select AYUDA
  if empty(_hlp_filtro)
    set filter to
  else
    _hlp_filtro=subs(_hlp_filtro,6)
    store 0 to _xerrac,_xier
    select AYUDA
    set filter to &_hlp_filtro
    if _xier<>0
      =f3_sn(1,1,'No se ha podido establecer el filtro','Posiblemente las condiciones estén mal definidas')
    endif
    _xier=0
    _xerrac=1
  endif
endif

ENDPROC
PROCEDURE ordenardesc
Parameter _n_campo
*
lx1=sys(2015)
lx2=_tmp+lx1
select AYUDA
lx3=field(_n_campo)
sort to (lx2) on &lx3 /D
zap
append from (lx2)
lx2=lx2+'.DBF'
delete file (lx2)
thisform.refresh

ENDPROC
PROCEDURE Release
if _ayuda_especial=.T.
  store .F. to _hlpok,_ok
endif

ENDPROC
PROCEDURE Resize

*> Redimensionar la posición de los botones.
Local alto, ancho

ancho=thisform.width
alto=thisform.height

return

if alto>90
  with thisform
    .grid1.height=alto-40
    .grid1.width=ancho-7

    .bot_ok.top=alto-32
    .bot_esc.top=alto-32
    .bot_prn.top=alto-32

    .bot_ok.left=ancho-76
    .bot_esc.left=ancho-38
    .bot_prn.left=ancho-38

    .L_Normal1.top=alto-33
    .L_Normal2.top=alto-21
    .L_Normal2.top=alto-9
  endwith
endif

ENDPROC
PROCEDURE Init
on key
if at(_cse,_usrmaxp)>0
  on key label 'SHIFT+F2' do =f3_hlpos()
endif
store .F. to _hlpok,_ok,_controlc
if syshl.HL_LIN<>0
  thisform.top=syshl.HL_LIN
endif
if syshl.HL_COL<>0
  thisform.left=syshl.HL_COL
endif
if syshl.HL_ANCHO<>0
  thisform.Width=syshl.HL_ANCHO
endif
if syshl.HL_ALTO<>0
  thisform.Height=syshl.HL_ALTO
endif
if syshl.HL_ALTO<>0 .or. syshl.HL_ANCHO<>0
  thisform.resize
endif
_linea=iif(!empty(_lxbus),_lxbus,syshl.HL_TIT)
if _xidiom>1
  thisform.caption=f3_t(_linea)
else
  thisform.caption=_linea
endif
thisform.grid1.columnCount=_xbnum
for num=1 to _xbnum
  lx1='thisform.grid1.column'+ltrim(str(num))
  with &lx1
    lx='.Controlsource = "'+_xbusca(num,1)+'"'
    &lx
    .Width=_xbusca(num,2)
    .header1.caption=_xbusca(num,3)
  endwith
endfor

if _ayuda_especial=.T.
  thisform.bot_ok.visible=.F.
endif

select AYUDA
go top

*> EnVFP 6.0 no se permite.
***thisform.grid1.column1.setfocus

ENDPROC


PROCEDURE Grid1.AfterRowColChange
LPARAMETERS nColIndex
*
_hlp_car=''

ENDPROC


PROCEDURE Grid1.Column1.Header1.DblClick
thisform.ordenardesc(this.parent.ColumnOrder)
select AYUDA
go top

ENDPROC
PROCEDURE Grid1.Column1.Header1.Click
thisform.ordenar(this.parent.ColumnOrder)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column1.Header1.RightClick
thisform.filtro(this.parent.ColumnOrder)

ENDPROC


PROCEDURE Grid1.Column1.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(1)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC
PROCEDURE Grid1.Column1.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC


PROCEDURE Grid1.Column2.Header1.DblClick
thisform.ordenardesc(2)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column2.Header1.Click
thisform.ordenar(2)
select AYUDA
go top

ENDPROC
PROCEDURE Grid1.Column2.Header1.RightClick
thisform.filtro(2)
ENDPROC


PROCEDURE Grid1.Column2.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column2.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(2)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column3.Header1.DblClick
thisform.ordenardesc(3)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column3.Header1.Click
thisform.ordenar(3)
select AYUDA
go top


ENDPROC
PROCEDURE Grid1.Column3.Header1.RightClick
thisform.filtro(3)
ENDPROC


PROCEDURE Grid1.Column3.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column3.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(3)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column4.Header1.DblClick
thisform.ordenardesc(4)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column4.Header1.Click
thisform.ordenar(4)
select AYUDA
go top

ENDPROC
PROCEDURE Grid1.Column4.Header1.RightClick
thisform.filtro(4)
ENDPROC


PROCEDURE Grid1.Column4.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column4.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(4)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column5.Header1.Click
thisform.ordenar(5)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column5.Header1.RightClick
thisform.filtro(5)
ENDPROC
PROCEDURE Grid1.Column5.Header1.DblClick
thisform.ordenardesc(5)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column5.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column5.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(5)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column6.Header1.Click
thisform.ordenar(6)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column6.Header1.RightClick
thisform.filtro(6)
ENDPROC
PROCEDURE Grid1.Column6.Header1.DblClick
thisform.ordenardesc(6)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column6.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(6)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC
PROCEDURE Grid1.Column6.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC


PROCEDURE Grid1.Column7.Header1.Click
thisform.ordenar(7)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column7.Header1.RightClick
thisform.filtro(7)
ENDPROC
PROCEDURE Grid1.Column7.Header1.DblClick
thisform.ordenardesc(7)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column7.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column7.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(7)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column8.Header1.Click
thisform.ordenar(8)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column8.Header1.RightClick
thisform.filtro(8)
ENDPROC
PROCEDURE Grid1.Column8.Header1.DblClick
thisform.ordenardesc(8)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column8.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column8.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(8)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column9.Header1.Click
thisform.ordenar(9)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column9.Header1.RightClick
thisform.filtro(9)
ENDPROC
PROCEDURE Grid1.Column9.Header1.DblClick
thisform.ordenardesc(9)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column9.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column9.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(9)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column10.Header1.Click
thisform.ordenar(10)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column10.Header1.RightClick
thisform.filtro(10)
ENDPROC
PROCEDURE Grid1.Column10.Header1.DblClick
thisform.ordenardesc(10)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column10.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column10.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(10)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column11.Header1.Click
thisform.ordenar(11)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column11.Header1.RightClick
thisform.filtro(11)
ENDPROC
PROCEDURE Grid1.Column11.Header1.DblClick
thisform.ordenardesc(11)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column11.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column11.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(11)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column12.Header1.Click
thisform.ordenar(12)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column12.Header1.RightClick
thisform.filtro(12)
ENDPROC
PROCEDURE Grid1.Column12.Header1.DblClick
thisform.ordenardesc(12)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column12.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column12.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(12)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column13.Header1.Click
thisform.ordenar(13)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column13.Header1.RightClick
thisform.filtro(13)
ENDPROC
PROCEDURE Grid1.Column13.Header1.DblClick
thisform.ordenardesc(13)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column13.Text1.DblClick
thisform.ordenardesc(13)
select AYUDA
go top_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column13.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(13)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column14.Header1.Click
thisform.ordenar(14)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column14.Header1.RightClick
thisform.filtro(14)
ENDPROC
PROCEDURE Grid1.Column14.Header1.DblClick
thisform.ordenardesc(14)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column14.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column14.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(14)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column15.Header1.Click
thisform.ordenar(15)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column15.Header1.RightClick
thisform.filtro(15)
ENDPROC
PROCEDURE Grid1.Column15.Header1.DblClick
thisform.ordenardesc(15)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column15.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column15.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(15)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column16.Header1.Click
thisform.ordenar(16)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column16.Header1.RightClick
thisform.filtro(16)
ENDPROC
PROCEDURE Grid1.Column16.Header1.DblClick
thisform.ordenardesc(16)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column16.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column16.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(16)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column17.Header1.Click
thisform.ordenar(17)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column17.Header1.RightClick
thisform.filtro(17)
ENDPROC
PROCEDURE Grid1.Column17.Header1.DblClick
thisform.ordenardesc(17)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column17.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column17.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(17)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column18.Header1.Click
thisform.ordenar(18)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column18.Header1.RightClick
thisform.filtro(18)
ENDPROC
PROCEDURE Grid1.Column18.Header1.DblClick
thisform.ordenardesc(18)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column18.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column18.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(18)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column19.Header1.Click
thisform.ordenar(19)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column19.Header1.RightClick
thisform.filtro(19)
ENDPROC
PROCEDURE Grid1.Column19.Header1.DblClick
thisform.ordenardesc(19)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column19.Text1.DblClick
_hlpok=.T.
thisform.release
ENDPROC
PROCEDURE Grid1.Column19.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(19)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE Grid1.Column20.Header1.Click
thisform.ordenar(20)
select AYUDA
go top
ENDPROC
PROCEDURE Grid1.Column20.Header1.RightClick
thisform.filtro(20)
ENDPROC
PROCEDURE Grid1.Column20.Header1.DblClick
thisform.ordenardesc(20)
select AYUDA
go top
ENDPROC


PROCEDURE Grid1.Column20.Text1.DblClick
store .T. to _ok,_hlpok
thisform.release
ENDPROC
PROCEDURE Grid1.Column20.Text1.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _hlpok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
case betw(nKeyCode,32,255)
  lx1=field(20)
  if nKeyCode=127
    _hlp_car=iif(len(_hlp_car)>1,left(_hlp_car,len(_hlp_car)-1),'')
  else
    _hlp_car=_hlp_car+chr(nKeyCode)
  endif
  select ayuda
  r1=iif(eof(),0,recno())
  do case
  case type(lx1)='N'
    locate for &lx1>=val(_hlp_car)
  case type(lx1)='C'
    locate for &lx1=_hlp_car
  case type(lx1)='D'
    locate for left(dtoc(&lx1,len(_hlp_car))=_hlp_car
  endcase
  if !found() .and. r1>0
    go r1
  endif
endcase

ENDPROC


PROCEDURE bot_ok.Click
store .T. to _hlpok,_ok
thisform.release

ENDPROC


PROCEDURE bot_esc.Click
thisform.release

ENDPROC


PROCEDURE Bot_prn.Click
*>
List To Printer Prompt Off NoConsole

ENDPROC


EndDefine 
