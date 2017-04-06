Public st3trasp_45T0S4P6J
SET CLASSLIB TO "st3class.vcx" ADDITIVE

st3trasp_45T0S4P6J = CreateObject("st3trasp" )
If st3trasp_45T0S4P6J.WindowType == 1
st3trasp_45T0S4P6J.Show(1)
else
st3trasp_45T0S4P6J.Show()
EndIf



Define Class st3trasp as form
ScaleMode = 3
Height = 301
Width = 597
ShowTips = .T.
AutoCenter = .T.
BorderStyle = 2
Caption = ""
ControlBox = .F.
Closable = .T.
FontBold = .T.
FontSize = 10
MaxButton = .F.
MinButton = .F.
ClipControls = .F.
WindowType = 1
WindowState = 0
ColorSource = 5
BackColor = Rgb(192,192,192)

ADD OBJECT Label2 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 1, Caption = "Elementos seleccionados", Height = 15, Left = 336, Top = 4, Width = 146, TabIndex = 2, ColorSource = 0, BackColor = Rgb(192,192,192)
ADD OBJECT ndds as listbox WITH DragMode = 0, FontBold = .F., FontName = "Courier", FontSize = 10, ColumnCount = 1, RowSourceType = 5, RowSource = "m_destino", Value = 1, ControlSource = "ndds", Height = 228, Left = 325, MoverBars = .F., MultiSelect = .T., NumberOfElements = 0, TabIndex = 4, ToolTipText = "", Top = 25, Width = 264, ColorSource = 0
ADD OBJECT Label1 as label WITH AutoSize = .T., FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, BackStyle = 1, Caption = "Elementos posibles", Height = 15, Left = 24, Top = 4, Width = 111, TabIndex = 1, ColorSource = 0, BackColor = Rgb(192,192,192)
ADD OBJECT Line2 as line WITH Height = 0, Left = 0, Top = 264, Width = 624, BorderColor = Rgb(255,255,255)
ADD OBJECT boton1 as commandbutton WITH Top = 269, Left = 12, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "", TabIndex = 9, Visible = .F., ColorSource = 0
ADD OBJECT boton2 as commandbutton WITH Top = 269, Left = 96, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "", TabIndex = 10, Visible = .F., ColorSource = 0
ADD OBJECT boton3 as commandbutton WITH Top = 269, Left = 180, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "", TabIndex = 11, Visible = .F., ColorSource = 0
ADD OBJECT bot_rest as commandbutton WITH Top = 269, Left = 264, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Restaurar", TabIndex = 12, ColorSource = 0
ADD OBJECT bot_ok as commandbutton WITH Top = 269, Left = 432, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Aceptar", TabIndex = 14, TerminateRead = .T., ColorSource = 0
ADD OBJECT BOT_ESC as commandbutton WITH Top = 269, Left = 516, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "\<Cancelar", TabIndex = 15, TerminateRead = .T., ColorSource = 0
ADD OBJECT Line1 as line WITH Height = 0, Left = 0, Top = 263, Width = 600
ADD OBJECT bot_reor as commandbutton WITH Top = 269, Left = 348, Height = 25, Width = 73, FontBold = .T., FontName = "MS Sans Serif", FontSize = 8, Caption = "R\<eordenar", TabIndex = 13, ColorSource = 0
ADD OBJECT t_dcha as st_bot WITH AutoSize = .F., Top = 72, Left = 288, Height = 29, Width = 29, Picture = "bmp\derecha.bmp", Caption = "", TabIndex = 5, ToolTipText = "Todo"
ADD OBJECT dcha as st_bot WITH AutoSize = .F., Top = 108, Left = 288, Height = 29, Width = 29, Picture = "bmp\next.bmp", Caption = "", TabIndex = 6, ToolTipText = "Lo seleccionado"
ADD OBJECT t_izq as st_bot WITH AutoSize = .F., Top = 180, Left = 288, Height = 29, Width = 29, Picture = "bmp\izqda.bmp", Caption = "", TabIndex = 8, ToolTipText = "Todo"
ADD OBJECT izq as st_bot WITH AutoSize = .F., Top = 144, Left = 288, Height = 29, Width = 29, Picture = "bmp\prior.bmp", Caption = "", TabIndex = 7, ToolTipText = "Lo seleccionado"
ADD OBJECT ndor as listbox WITH FontBold = .F., FontName = "Courier", FontSize = 10, BoundColumn = 1, ColumnCount = 1, RowSourceType = 5, RowSource = "m_origen", Value = 1, ControlSource = "ndor", Height = 228, Left = 12, MultiSelect = .T., NumberOfElements = 10, TabIndex = 3, Top = 25, Width = 265, ColorSource = 0


PROCEDURE f3_mover
* --------------------------------
* Mover matriz de un lado a otro
*
* iz_dr	 	.T. si se mueve de izquierda -> Derecha (origen -> destino)
*        	.F. si se mueve de derecha -> Izquierda (origen -> destino)
* tr_todo	.T. si se traspasa todo, .F. sólo lo seleccionado
* nd1		primer elemento a mover
* nd2		último elemento a mover
* --------------------------------
Parameter iz_dr,tr_todo,nd1,nd2
*
for nd=nd1 to nd2
  do case
  case iz_dr .and. !empty(m_origen(nd)) .and. (st3trasp.ndor.selected(nd)=.T. .or. tr_todo=.T.)
    if !empty(m_destino(ndds)) .and. empty(m_destino(max_ds))
      =ains(m_destino,ndds)
      jjj = ndds
      st3trasp.ndds.selected(ndds)=.F.
      ndds = jjj
    endif
    if empty(m_destino(ndds))
      m_destino(ndds)=m_origen(nd)
      st3trasp.ndds.selected(ndds)=.F.
      ndds=min(ndds+1,max_ds)
      m_origen(nd)=space(30)
      st3trasp.ndor.selected(nd)=.F.
    endif
  case iz_dr
    st3trasp.ndor.selected(nd)=.F.
  case !empty(m_destino(nd)) .and. (st3trasp.ndds.selected(nd)=.T..or.tr_todo=.T.)
    if ndor<=max_or .and. !empty(m_origen(ndor)) .and. empty(m_origen(max_or))
      =ains(m_origen,ndor)
      jjj = ndor
      st3trasp.ndor.selected(ndor)=.F.
      ndor = jjj
    endif
    if ndor<=max_or .and. empty(m_origen(ndor))
      m_origen(ndor)=m_destino(nd)
      st3trasp.ndor.selected(ndor)=.F.
      ndor=min(ndor+1,max_or)
      m_destino(nd)=space(30)
      st3trasp.ndds.selected(nd)=.F.
    endif
  otherwise
    st3trasp.ndds.selected(nd)=.F.
  endcase
endfor

do case
case iz_dr .and. ndor<max_or
  ndor=ndor+1
case !iz_dr .and. ndds<max_ds
  ndds=ndds+1
endcase
thisform.refresh

ENDPROC
PROCEDURE Release
pop key
if bot_ok=1
  _xcancelado=.F.
  nd1=max(max_or,max_ds)
  for nd=1 to nd1
    if nd<=max_or
      &arr_or(nd)=m_origen(nd)
    endif
    if nd<=max_ds
      &arr_ds(nd)=m_destino(nd)
    endif
  endfor
endif
_ok=iif(bot_ok=1,.T.,.F.)
ENDPROC
PROCEDURE Init
push key

_controlc=.F.
if type('_elem_pos')='C'
  this.label1.caption=_elem_pos
endif
if type('_elem_el')='C'
  this.label2.caption=_elem_el
endif
store 1 to ndor,ndds
this.caption=f3_t(_titulo)
if _xidiom>1
  this.label1.caption=f3_t(this.label1.caption)
  this.label2.caption=f3_t(this.label2.caption)
  this.bot_rest.caption=f3_t(this.bot_rest.caption)
  this.bot_ok.caption=f3_t(this.bot_ok.caption)
  this.bot_esc.caption=f3_t(this.bot_esc.caption)
endif
this.ndor.numberOfElements=max_or
this.ndor.height=min(14,max_or)*16+4
this.ndds.numberOfElements=max_ds
this.ndds.height=min(14,max_ds)*16+4
if !empty(lxbot1)
  this.boton1.caption=f3_t(lxbot1)
  this.boton1.visible=.T.
endif
if !empty(lxbot2)
  this.boton2.caption=f3_t(lxbot2)
  this.boton2.visible=.T.
endif
if !empty(lxbot3)
  this.boton3.caption=f3_t(lxbot3)
  this.boton3.visible=.T.
endif
if _horizontal
  this.top=this.top-20
  this.height=this.height+40
  thisform.boton1.top=thisform.boton1.top+45
  thisform.boton2.top=thisform.boton2.top+45
  thisform.boton3.top=thisform.boton3.top+45
  thisform.line1.top=thisform.line1.top+45
  thisform.line2.top=thisform.line2.top+45
  thisform.bot_rest.top=thisform.bot_rest.top+45
  thisform.bot_ok.top=thisform.bot_ok.top+45
  thisform.bot_esc.top=thisform.bot_esc.top+45
  thisform.bot_reor.top=thisform.bot_reor.top+45
  thisform.label1.top=0
  thisform.label1.left=12
  thisform.ndor.top=15
  thisform.ndor.height=125
  thisform.ndds.height=125
  thisform.ndor.width=576
  thisform.ndds.width=576
  thisform.ndds.left=12
  thisform.ndds.top=180
  thisform.label2.left=12
  thisform.label2.top=165
  thisform.t_dcha.top=146
  thisform.t_dcha.left=255
  thisform.dcha.top=146
  thisform.dcha.left=290
  thisform.izq.top=146
  thisform.izq.left=325
  thisform.t_izq.top=146
  thisform.t_izq.left=360
  lx=thisform.t_dcha.picture
  nd=atc('derecha.bmp',lx)
  if nd>0
    lx=left(lx,nd-1)+'T_DOWN.BMP'
    thisform.t_dcha.picture=lx
  endif
  lx=thisform.dcha.picture
  nd=atc('next.bmp',lx)
  if nd>0
    lx=left(lx,nd-1)+'DOWN.BMP'
    thisform.dcha.picture=lx
  endif
  lx=thisform.t_izq.picture
  nd=atc('izqda.bmp',lx)
  if nd>0
    lx=left(lx,nd-1)+'T_UP.BMP'
    thisform.t_izq.picture=lx
  endif
  lx=thisform.izq.picture
  nd=atc('prior.bmp',lx)
  if nd>0
    lx=left(lx,nd-1)+'UP.BMP'
    thisform.izq.picture=lx
  endif
endif
_horizontal=.F.

ENDPROC
PROCEDURE Activate
on key
on key label 'ESC'        do =f3_esc()
on key label 'CTRL+ENTER' do =f3_ok()
*
do case
case _esc=.T.
  _esc=.F.
  thisform.BOT_ESC.click
case _enter=.T.
  _enter=.F.
  thisform.BOT_OK.click
endcase
ENDPROC


PROCEDURE ndds.DblClick
thisform.f3_mover(.F.,.F.,ndds,ndds)

ENDPROC


PROCEDURE boton1.Click
do (lxprg1)
this.refresh

ENDPROC


PROCEDURE boton2.Click
do (lxprg2)
this.refresh

ENDPROC


PROCEDURE boton3.Click
do (lxprg3)
this.refresh

ENDPROC


PROCEDURE bot_rest.Click
store 1 to ndor,ndds
nd1=max(max_or,max_ds)
for nd=1 to nd1
  if nd<=max_or
    m_origen(nd)=&arr_or(nd)
    st3trasp.ndor.selected(nd)=iif(nd=1,.T.,.F.)
  endif
  if nd<=max_ds
    m_destino(nd)=&arr_ds(nd)
    st3trasp.ndds.selected(nd)=iif(nd=1,.T.,.F.)
  endif
endfor

ENDPROC


PROCEDURE bot_ok.Click
bot_ok=1
thisform.release
ENDPROC


PROCEDURE BOT_ESC.Click
bot_ok=0
thisform.release
ENDPROC


PROCEDURE bot_reor.Click
nd1=max(max_or,max_ds)
store 0 to ndo1,nds1
for nd=1 to nd1
  if nd<=max_or
    do case
    case !empty(m_origen(nd)) .and. ndo1+1=nd
      ndo1=ndo1+1
    case !empty(m_origen(nd))
      ndo1=ndo1+1
      m_origen(ndo1)=m_origen(nd)
      m_origen(nd)=''
    endcase
    st3trasp.ndor.selected(nd)=iif(nd=1,.T.,.F.)
  endif
  if nd<=max_ds
    do case
    case !empty(m_destino(nd)) .and. nds1+1=nd
      nds1=nds1+1
    case !empty(m_destino(nd))
      nds1=nds1+1
      m_destino(nds1)=m_destino(nd)
      m_destino(nd)=''
    endcase
    st3trasp.ndds.selected(nd)=iif(nd=1,.T.,.F.)
  endif
endfor
*
store 1 to ndor,ndds
thisform.refresh
ENDPROC


PROCEDURE t_dcha.Click
thisform.f3_mover(.T.,.T.,1,max_or)

ENDPROC


PROCEDURE dcha.Click
thisform.f3_mover(.T.,.F.,1,max_or)

ENDPROC


PROCEDURE t_izq.Click
thisform.f3_mover(.F.,.T.,1,max_ds)

ENDPROC


PROCEDURE izq.Click
thisform.f3_mover(.F.,.F.,1,max_ds)

ENDPROC


PROCEDURE ndor.DblClick
thisform.f3_mover(.T.,.F.,ndor,ndor)

ENDPROC


EndDefine 
