Public SY3DUSR_45T0S4U6R
SET CLASSLIB TO "st3class.vcx" ADDITIVE

SY3DUSR_45T0S4U6R = CreateObject("SY3DUSR" )
If SY3DUSR_45T0S4U6R.WindowType == 1
SY3DUSR_45T0S4U6R.Show(1)
else
SY3DUSR_45T0S4U6R.Show()
EndIf



Define Class SY3DUSR as form
Height = 250
Width = 578
ShowTips = .T.
AutoCenter = .T.
BackColor = Rgb(192,192,192)
BorderStyle = 2
Caption = (f3_t('Definición de Usuarios'))
WindowType = 1
WindowState = 0
FontBold = .T.
FontSize = 10
ColorSource = 5

ADD OBJECT Shape2 as shape WITH BackStyle = 0, Height = 38, Left = 295, Top = 193, Width = 129, SpecialEffect = 0, ColorSource = 0
ADD OBJECT List1 as listbox WITH FontBold = .F., FontName = "Courier", BoundColumn = 1, ColumnCount = 3, RowSourceType = 6, RowSource = "USUARIO", ControlSource = "n_usr", Height = 166, Left = 6, NumberOfElements = 0, TabIndex = 3, Top = 24, Width = 283, FontSize = 10, ColorSource = 0
ADD OBJECT List2 as listbox WITH FontBold = .F., FontName = "Courier", ColumnCount = 2, RowSourceType = 6, RowSource = "GRUPO", ControlSource = "n_gr", Height = 166, Left = 295, TabIndex = 4, Top = 24, Width = 277, FontSize = 10, ColorSource = 0
ADD OBJECT L_normal3 as l_normal WITH Caption = "Usuarios", Left = 90, Top = 9
ADD OBJECT L_normal4 as l_normal WITH Caption = "Grupos de Usuarios", Left = 369, Top = 9
ADD OBJECT Bot_esc as st_bot WITH AutoSize = .F., Top = 198, Left = 540, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", ToolTipText = "Salir"
ADD OBJECT bot_pas as st_bot WITH AutoSize = .F., Top = 198, Left = 153, Height = 23, Width = 109, Caption = "Password"
ADD OBJECT bot_icon as st_bot WITH AutoSize = .F., Top = 225, Left = 153, Height = 23, Width = 109, Caption = "Barra de Iconos"
ADD OBJECT gru_add as st_bot WITH AutoSize = .F., Top = 198, Left = 306, Height = 29, Width = 29, Picture = "bmp\alta.bmp", Caption = "", ToolTipText = "Añadir"
ADD OBJECT gru_mod as st_bot WITH AutoSize = .F., Top = 198, Left = 342, Height = 29, Width = 29, Picture = "bmp\modi.bmp", Caption = "", ToolTipText = "Modificar"
ADD OBJECT gru_elim as st_bot WITH AutoSize = .F., Top = 198, Left = 378, Height = 29, Width = 29, Picture = "bmp\baja.bmp", Caption = "", ToolTipText = "Eliminar"
ADD OBJECT Shape1 as shape WITH BackStyle = 0, Height = 38, Left = 7, Top = 193, Width = 129, SpecialEffect = 0, ColorSource = 0
ADD OBJECT usr_add as st_bot WITH AutoSize = .F., Top = 198, Left = 18, Height = 29, Width = 29, Picture = "bmp\alta.bmp", Caption = "", ToolTipText = "Añadir"
ADD OBJECT usr_mod as st_bot WITH AutoSize = .F., Top = 198, Left = 54, Height = 29, Width = 29, Picture = "bmp\modi.bmp", Caption = "", ToolTipText = "Modificar"
ADD OBJECT usr_elim as st_bot WITH AutoSize = .F., Top = 198, Left = 90, Height = 29, Width = 29, Picture = "bmp\baja.bmp", Caption = "", ToolTipText = "Eliminar"


PROCEDURE Activate
on key
on key label 'ESC' do =f3_esc()
*
if _esc=.T.
  _esc=.F.
  thisform.release
endif
ENDPROC


PROCEDURE Bot_esc.Click
thisform.release

ENDPROC


PROCEDURE bot_pas.Click
select SYSUSER
seek ' '+usuario.USR_COD
do form st3cc

ENDPROC


PROCEDURE bot_icon.Click
select SYSUSER
seek ' '+usuario.USR_COD
usricon=USR_ICON
select SYSPRG
nd=reccount()
dimension _origen(nd),_destino(15)
store space(12) to _origen,_destino
set filter to PRG_TIPO<>'A'
go top
store 0 to ntot,numo,numd
do while .not. eof()
  ntot=ntot+1
  nd=at(PRG_PROG,usricon)
  if nd=0
    numo=numo+1
    _origen(numo)='  '+PRG_PROG+' '+PRG_TIT
  else
    numd=numd+1
    lx2=subs(usricon,nd-2,2)
    _destino(numd)=lx2+PRG_PROG+' '+PRG_TIT
  endif
  skip
enddo
set filter to
_ok=st3trasp('Barra de iconos','_origen',ntot,'_destino',15,'Separador','sy3u_lin')
select SYSUSER
if _ok
  usricon=''
  for nd=1 to 15
    usricon=usricon+left(_destino(nd),12)
  endfor
  repl USR_ICON with usricon
  if empty(usricon)
    repl USR_LN with 0,USR_COL with 0
  endif
endif

ENDPROC


PROCEDURE gru_add.Click
select SYSUSER
scatter memvar blank
_ok=f3_inp('Definir un Grupo de Usuarios','Grupo,Nombre','m.USR_GR,m.USR_NOM','!,')
if _ok .and. !empty(m.USR_GR)
  select SYSUSER
  seek m.USR_GR
  if eof()
    append blank
    gather memvar
    select GRUPO
    append blank
    gather memvar
  else
    =f3_sn(1,1,'Grupo de Usuarios ya existente --> No se ha dado la alta')
  endif
endif
sy3dusr.list2.setfocus

ENDPROC


PROCEDURE gru_mod.Click
select SYSUSER
seek grupo.USR_GR
scatter memvar
_ok=f3_inp('Modificar un Grupo de Usuarios','Grupo,Nombre','!m.USR_GR,m.USR_NOM',',')
if _ok
  gather memvar
  select GRUPO
  gather memvar
endif
sy3dusr.list2.setfocus

ENDPROC


PROCEDURE gru_elim.Click
if  grupo.USR_GR='P' .or. grupo.USR_GR='M'
  =f3_sn(1,1,'No puede eliminar este grupo de usuarios')
  return
endif
_ok=f3_sn(2,1,'Desea eliminar el Grupo de Usuarios ['+grupo.USR_GR+']','['+grupo.USR_NOM+']')
if _ok
  select SYSUSER
  seek grupo.USR_GR
  delete record recno()
  go top
  do while !eof()
    if USR_NIV=grupo.USR_GR
      repl USR_NIV with ' '
    endif
    skip
  enddo
  select USUARIO
  go top
  do while .not.eof()
   if USR_NIV=grupo.USR_GR
     repl USR_NIV with ' '
    endif
    skip
  enddo
  sy3dusr.list1.setfocus
  select GRUPO
  delete record recno()
endif
sy3dusr.list2.setfocus

ENDPROC


PROCEDURE usr_add.Click
select SYSUSER
scatter memvar blank
m.USR_IDIOM=1
tit_inp='Definir un Usuario'
txt_inp='Código/Nombre/Grupo/Idioma/Empresa/Anclaje iconos/Acceso a Empresas/Permitidas/Prohibidas'
var_inp='m.USR_COD/m.USR_NOM/m.USR_NIV/m.USR_IDIOM/m.USR_EMP/m.USR_ICONP/$NADA$/m.USR_EMPS/m.USR_EMPN'
fmt_inp='!!!!!!//!/#Combo._idiom/999/#Combo._anclaje///'
bot_ok=0
hay_error=.F.
do form st3inp
hay_error=iif(seek(m.USR_NIV,'SYSUSER').or.empty(m.USR_NIV),hay_error,.T.)
do case
case bot_ok=1 .and. hay_error
  do form st3sn with 1,1,'Grupo de usuarios inexistente'
case m.USR_NIV='P' .and. _cse<>'P'
  do form st3sn with 1,1,'No puede definir un usuario tipo Programador'
case bot_ok=1 .and. !empty(m.USR_COD)
  select SYSUSER
  seek ' '+m.USR_COD
  if eof()
    lxpas=space(6)
    lxpasd=chr(asc(subs(lxpas,1,1))+asc(subs(m.USR_COD,5,1))-int(asc(subs(m.USR_COD,3,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,2,1))+asc(subs(m.USR_COD,2,1))-int(asc(subs(m.USR_COD,1,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,3,1))+asc(subs(m.USR_COD,1,1))-int(asc(subs(m.USR_COD,4,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,4,1))+asc(subs(m.USR_COD,6,1))-int(asc(subs(m.USR_COD,3,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,5,1))+asc(subs(m.USR_COD,4,1))-int(asc(subs(m.USR_COD,2,1))/3))
    m.USR_PAS=lxpasd+chr(asc(subs(lxpas,6,1))+asc(subs(m.USR_COD,3,1))-int(asc(subs(m.USR_COD,3,1))/3))
    append blank
    gather memvar
    repl USR_GR with ' '
    select USUARIO
    append blank
    gather memvar
  else
    do form st3sn with 1,1,'Usuario ya existente --> No se ha dado la alta'
  endif
endcase
sy3dusr.list1.setfocus

ENDPROC


PROCEDURE usr_mod.Click
select SYSUSER
seek ' '+usuario.USR_COD
scatter memvar
m.USR_ICONP=USR_ICONP
m.ACTIVA=1
_ok=f3_inp('Modificar un Usuario',;
           'Código,Nombre,Grupo,Idioma,Empresa,Anclaje iconos,Acceso a Empresas,Permitidas,Prohibidas',;
           '!m.USR_COD,m.USR_NOM,m.USR_NIV,m.USR_IDIOM,m.USR_EMP,m.USR_ICONP,,m.USR_EMPS,m.USR_EMPN',;
           '!!!!!!,,!,CMB_idiom,999,CMB_anclaje')
hay_error=iif(seek(m.USR_NIV,'SYSUSER').or.empty(m.USR_NIV),.F.,.T.)
bot_ok=iif(_ok,1,0)
do case
case bot_ok=1 .and. hay_error
  do form st3sn with 1,1,'Grupo de usuarios inexistente'
case m.USR_NIV='P' .and. _cse<>'P'
  do form st3sn with 1,1,'No puede modificar un usuario de tipo Programador'
case bot_ok=1
  seek ' '+usuario.USR_COD
  gather memvar
  select USUARIO
  gather memvar
endcase
select SYSUSER
sy3dusr.list1.setfocus

ENDPROC


PROCEDURE usr_elim.Click
_ok=f3_sn(2,1,'Desea eliminar el Usuario ['+usuario.USR_COD+']','['+usuario.USR_NOM+']')
if _ok
  select SYSUSER
  seek ' '+usuario.USR_COD
  delete record recno()
  select USUARIO
  delete record recno()
endif
sy3dusr.list1.setfocus

ENDPROC


EndDefine 
