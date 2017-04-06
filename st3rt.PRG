* Biblioteca de funciones programadas
*            st3rt
*
* Version 3.0   Carlos Casals Fradera  Septiembre 1.996
*

* -------------------------------------
* Conexión con formulario st3inp 
*	_tit_inp		Título pantalla
*	_txt_inp		Textos a pasar (labels)
*	_var_inp		Variables a cargar		si empieza por ! (es un say)
*	_fmt_inp		Formatos		CHK		Check
*									CMB<nombre array>		Combo
*
* -------------------------------------
Function f3_inp
Parameters _tit_inp,_txt_inp,_var_inp,_fmt_inp
*
tit_inp=_tit_inp
_txt_inp=_txt_inp+',  '
_var_inp=_var_inp+',  '
_fmt_inp=_fmt_inp+',  '
store '' to txt_inp,var_inp,fmt_inp
nd1=at(',',_txt_inp)
do while nd1>0
  txt_inp=txt_inp+left(_txt_inp,nd1-1)+'/'
  _txt_inp=subs(_txt_inp,nd1+1)
  nd2=at(',',_var_inp)                        && Variables
  lx1=''
  if nd2>0
    lx1=left(_var_inp,nd2-1)
    _var_inp=subs(_var_inp,nd2+1)
  endif
  do case
  case empty(lx1)
    var_inp=var_inp+'$NADA$/'
  case left(lx1,1)='!'
    var_inp=var_inp+'$'+subs(lx1,2)+'/'
  otherwise
    var_inp=var_inp+lx1+'/'
  endcase
  nd2=at(',',_fmt_inp)                        && Formato
  lx1=''
  if nd2>0
    lx1=left(_fmt_inp,nd2-1)
    _fmt_inp=subs(_fmt_inp,nd2+1)
  endif
  do case
  case empty(lx1)
    fmt_inp=fmt_inp+'/'
  case len(lx1)=3 .and. upper(left(lx1,3))='CHK'
 	fmt_inp=fmt_inp+'#CheckBox/'
  case len(lx1)>3 .and. upper(left(lx1,3))='CMB'
    fmt_inp=fmt_inp+'#Combo.'+subs(lx1,4)+'/'
  otherwise
    fmt_inp=fmt_inp+lx1+'/'
  endcase
  nd1=at(',',_txt_inp)
enddo
bot_ok=0
if len(_txt_inp)>0
  txt_inp=left(txt_inp,len(txt_inp)-1)
  var_inp=left(var_inp,len(var_inp)-1)
  fmt_inp=left(fmt_inp,len(fmt_inp)-1)
  do form st3inp
endif
_ok=iif(bot_ok=1,.T.,.F.)
*
return _ok


* ***************************************************
* Funcion que traspasa información de un array a otro
*
* !! ATENCION !! Los Arrays HAN DE SER de 1 COLUMNA
*   titulo --> Título de la ventana
*
*   Arr_or --> Nombre Array Origen
*   max_or --> No. máximo elementos Origen
*
*   Arr_ds --> Nombre Array Destino
*   max_ds --> No. máximo elementos Destino
*
*   boton1,prog1  	--> Boton 1 y programa 1
*   boton2,prog2
*   boton3,prog3
*
*	_elem_pos,_elem_el	-->título elementos posibles/elegidos
*
* Variables	ndor,ndds  (no. de opción elegida)
*			_xcancelado
* arrays	m_origen,m_destino
*
* ***************************************************
Function st3trasp
Parameters titulo,arr_or,max_or,arr_ds,max_ds,lxbot1,lxprg1,lxbot2,lxprg2,lxbot3,lxprg3,_elem_pos,_elem_el
*
_titulo=titulo
bot_ok=0
if max_or<=0 .or. max_ds<=0
  return .F.
endif
dimension m_origen(max_or),m_destino(max_ds)
store '' to m_origen,m_destino
nd1=max(max_or,max_ds)
for nd=1 to nd1
  if nd<=max_or
    m_origen(nd)=&arr_or(nd)
  endif
  if nd<=max_ds
    m_destino(nd)=&arr_ds(nd)
  endif
endfor
do form st3trasp
*
return iif(bot_ok=1,.T.,.F.)

* -------------------------------------
* Conexión con formulario st3sn
* Message box
*
*  _nbotones	No. de botones a enseñar (Max, 3)
*  _dibujo		Bitmap de la izquierda    	1	Atención
*											2	Aviso
*											3	Pregunta
*											4	Prohibido
*  _texto1.		Literales del mensaje
*  _titulo		Título de la ventana
*  _lx1...		Literales de los botones
* -------------------------------------
Function f3_sn
Parameters _nbotones,_dibujo,_texto1,_texto2,_texto3,_titulo,_lx1,_lx2,_lx3
*
conf='N'
do form st3sn with _nbotones,_dibujo,_texto1,_texto2,_texto3,_titulo,_lx1,_lx2,_lx3
conf=iif(_nbotones=1,'S',conf)
_ok=iif(conf='S',.T.,.F.)
return _ok

* -------------------------------------
* Conexión con formulario st3help
* -------------------------------------
Function f3_help
Parameters _nvarh,_valorh,_mensaje_hlp
*
store .T. to _hayhlp,_hlpok
do st3hlp
*
return _hlpok

* ---------------------------------
* saltar 'n' inputs (Salida normal)
* ---------------------------------
Function f3_salto
Parameters _salto
*
lx_tecla='/'+ltrim(str(lastkey()))+'/'
if mdown()=.T. .or. at(lx_tecla,'/3/5/15/18/19/27/29/')<>0
  _salto=iif(type('_salto')='N',0,'')
endif
return _salto

* ---------------------------------
* saltar 'n' inputs (salida hacia atras)
* ---------------------------------
Function f3_rsalto
Parameters _salto
*
lx_tecla='/'+ltrim(str(lastkey()))+'/'
if mdown()=.T. .or. at(lx_tecla,'/5/19/')=0
  _salto=iif(type('_salto')='N',0,'')
endif
return _salto

* ---------------------------------
* Activar popup listado
* ---------------------------------
Function f3_lst
*
_botlst='LISTAR'
activate window wnada
deactivate window wnada
*
return


* ---------------------------------
* Ficha anterior
* ---------------------------------
Function f3_nav
Parameters _forma
*
_botnav=_forma
activate window wnada
deactivate window wnada
*
return

* ---------------------------------
* Contar fichas
* ---------------------------------
Function f3_cont
Parameters _forma
*
_botcont=_forma
activate window wnada
deactivate window wnada
*
return

* ------------------------------------------------
* Control teclas F3 a F12
* ------------------------------------------------
Function f3_tecl
Parameters _forma
*
_fn=_forma
if _fn=12 .and. left(_usrcod,5)='PENTA'
  _fn=0
  susp
endif
activate window wnada
deactivate window wnada
*
return

* ------------------------------------------------
* Control modo debug
* ------------------------------------------------
Function f3_debug

if left(_usrcod,5)='PENTA'
   IF TYPE("goDebug")="U"
       PUBLIC goDebug
   ENDIF

   GoDebug = SYS(1270)

   *-- Call the debugger
   DEBUG
   *-- leave a warning to release the goDEBUG reference
   WAIT WINDOW "DON'T FORGET TO RELEASE GODEBUG!!"
endif

activate window wnada
deactivate window wnada

return

* ----------------------------------
* Posiciona una pantalla
* ----------------------------------
Function f3_posicion
*
private nd1,nd2
nd1=wlcol()*8
nd2=wlrow()*13
select SYSPRG
repl PRG_COL with nd1,PRG_LIN with nd2
*
return

* ----------------------------------
* Posiciona una ayuda
* ----------------------------------
Function f3_hlpos
*
Private nd1,nd2,nd3,nd4
nd1=wlcol()*8
nd2=wlrow()*13
nd3=st3hlp.HEIGHT
nd4=st3hlp.WIDTH
select SYSHL
repl HL_COL with nd1,HL_LIN with nd2,HL_ALTO with nd3,HL_ANCHO with nd4
select AYUDA
*
return

* ------------------------------------------------------------------------
* Busca incondicional de un registro
*	_fics		Fichero
*	_keys		Nombres de los campos a buscar (Separados entre comas)
* 				[campo=campo memoria,  ... ]  (sin uso en DBF)
*	_blancs		Blancos a reemplazar
*	_replaces	Reemplazo
*	_var_priv	Variables privadas
*	_n_indice	Nombre del índice
*
* Abrir fichero si no lo está. AVC - 11.10.2002
* ------------------------------------------------------------------------
Function f3_seek
Parameters _fics,_keys,_blancs,_replaces,_var_priv,_n_indice
Local oPRC, lx, lx1, lx2, lx22, nd, _encontrado, _alis
Private fcomp

set memowidth to 2000
do case
*case wontop()<>'ST3TERM' .and. wontop()<>trim(sysprg.PRG_PROG) .and. !empty(wontop())
*  return .T.
case _esc=.T.
  return .T.
endcase

_alis=alias()
If Empty(_alis)
   Select SYSPRG
   _alis=alias()
EndIf

if type('_blancs')<>'C'
  _blancs=''
endif
_blancs=upper(_blancs)

  select SYSFC
  seek upper(_fics)
  if len(_fics) = 4
    sql_fc=_fics + _em
  else
    sql_fc=_fics
  endif
  fcomp=FC_CLAVE

  if type('_n_indice')='C'
    fcomp2=''
    for num=1 to memlines(sysfc.FC_CLAVE2)
      fcomp2=mline(sysfc.FC_CLAVE2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcomp=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif

  if type('_keys')='C' .and. at('[',_keys)>0           && Proceso de sustituciones
    nd=at('[',_keys)
    _keys=trim(subs(_keys,nd+1))
    _keys=left(_keys,len(_keys)-1)
    do while len(_keys)>0
      nd=at(',',_keys)
      if nd=0
        lx1=_keys
        _keys=''
      else
        lx1=left(_keys,nd-1)
        _keys=subs(_keys,nd+1)
      endif
      nd=at('=',lx1)
      if nd>0
        lx2=subs(lx1,nd+1)
        lx22=iif(upper(left(lx2,2))<>'M.','m.'+lx2,lx2)   && de memoria
        lx2 = iif(type(lx22)=='U', lx2, lx22)
        lx1=left(lx1,nd-1)
        lx1=iif(upper(left(lx1,2))<>'M.','m.'+lx1,lx1)   && clave
        nd=atc(lx1,fcomp)
        if nd>0
          fcomp=stuff(fcomp,nd,len(lx1),lx2)
        endif
      endif    
    enddo    
  endif

  lxcomp=&fcomp
  lxselect='SELECT * FROM '+sql_fc+' where '+lxcomp
  _xier=f3_SqlExec(_asql,lxselect,'consulta')
  if _xier <= 0
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
     Return .F.
  endif

  select CONSULTA

_encontrado=.F.
if type('_var_priv')='C' .and. !empty(_var_priv)
  _var_priv=_var_priv+',  '
  nd=at(',',_var_priv)
  do while nd>0
    lx=left(_var_priv,nd-1)
    _var_priv=subs(_var_priv,nd+1)
    nd=at('.',lx)
    if nd>0
      lx=subs(lx,nd+1)
    endif
    nd=at('*',lx)
    if nd=0
      private &lx
    else
      private all like &lx
    endif
    nd=at(',',_var_priv)
  enddo
endif

do case
case !eof()
  _encontrado=.T.
  scatter memvar memo
  if type('_replaces')='C'
    do while !empty(_replaces)
      nd=at(',',_replaces)
      if nd=0
        lx=_replaces
        _replaces=''
      else
        lx=left(_replaces,nd-1)
        _replaces=subs(_replaces,nd+1)
      endif
      &lx
    enddo
  endif
case empty(_blancs)

case left(_blancs,3)='ALL'
  scatter memvar blank memo
case left(_blancs,1)='!'
  _blancs=subs(_blancs,2)
  private (_blancs)
  scatter memvar blank memo
otherwise
  do while !empty(_blancs)
    nd1=at(',',_blancs)
    if nd1=0
      lx1=_blancs
      _blancs=''
    else
      lx1=left(_blancs,nd1-1)
      _blancs=iif(len(_blancs)=nd1,'',subs(_blancs,nd1+1))
    endif
    do case
    case empty(lx1)
      
    case type(lx1)='C'
      &lx1=space(len(&lx1))
    case type(lx1)='N'
      &lx1=0
    case type(lx1)='D'
      &lx1={  /  /  }
    endcase
  enddo
endcase

if _encontrado
   If !Used(_Fics)
      *> Abrir el fichero.
      oPRC = CreateObject('procaot')
      =oPRC.OpenTabla(_Fics)
      Release oPRC
   EndIf

   Select (_Fics)
   If Eof()
      Append Blank
   EndIf
   Gather MemVar Memo
else
   If Used(_fics)
      Select (_fics)
      Delete All
   EndIf
endif

If Used('Consulta')
   Use In Consulta
EndIf

if !empty(_alis)
   select alias (_alis)
endif
*
return _encontrado

* --------------------------------------------
* Busca incondicional del rowid de un registro
*	_fics		Fichero
*	_keys		Nombres de los campos a buscar (Separados entre comas)
* 				[campo=campo memoria,  ... ]  (sin uso en DBF)
*	_blancs		Blancos a reemplazar
*	_replaces	Reemplazo
*	_var_priv	Variables privadas
*	_n_indice	Nombre del índice
*
* Devuelve el RowId, si lo encuentra, como _fics + 'RowId'
* ---------------------------------------------
Function f3_seekRowId
Parameters _fics,_keys,_blancs,_replaces,_var_priv,_n_indice
*
set memowidth to 2000
do case
case wontop()<>'ST3TERM' .and. wontop()<>trim(sysprg.PRG_PROG) .and. !empty(wontop())
  return .T.
case _esc=.T.
  return .T.
endcase

_alis=alias()
If Empty(_alis)
   Select SYSPRG
   _alis=alias()
EndIf

if type('_blancs')<>'C'
  _blancs=''
endif
_blancs=upper(_blancs)

  select SYSFC
  seek upper(_fics)
  if len(_fics) = 4
     sql_fc = _fics + _em
  else
     sql_fc=_fics
  endif

  fcomp=FC_CLAVE
  if type('_n_indice')='C'
    fcomp2=''
    for num=1 to memlines(sysfc.FC_CLAVE2)
      fcomp2=mline(sysfc.FC_CLAVE2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcomp=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif
  if type('_keys')='C' .and. at('[',_keys)>0           && Proceso de sustituciones
    nd=at('[',_keys)
    _keys=trim(subs(_keys,nd+1))
    _keys=left(_keys,len(_keys)-1)
    do while len(_keys)>0
      nd=at(',',_keys)
      if nd=0
        lx1=_keys
        _keys=''
      else
        lx1=left(_keys,nd-1)
        _keys=subs(_keys,nd+1)
      endif
      nd=at('=',lx1)
      if nd>0
        lx2=subs(lx1,nd+1)
        lx2=iif(upper(left(lx2,2))<>'M.','m.'+lx2,lx2)   && de memoria
        lx1=left(lx1,nd-1)
        lx1=iif(upper(left(lx1,2))<>'M.','m.'+lx1,lx1)   && clave
        nd=atc(lx1,fcomp)
        if nd>0
          fcomp=stuff(fcomp,nd,len(lx1),lx2)
        endif
      endif    
    enddo    
  endif

  lxcomp=&fcomp
  lxselect = 'Select ' + sql_fc + '.*, ' + ;
             'RowIdToChar(RowId) As ' + sql_fc + 'RowId' + ;
             ' From ' + sql_fc + ' Where ' + lxcomp

  _xier=f3_SqlExec(_asql,lxselect,'consulta')
  if _xier <= 0
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
     Return .F.
  endif

  select CONSULTA

_encontrado=.F.
if type('_var_priv')='C' .and. !empty(_var_priv)
  _var_priv=_var_priv+',  '
  nd=at(',',_var_priv)
  do while nd>0
    lx=left(_var_priv,nd-1)
    _var_priv=subs(_var_priv,nd+1)
    nd=at('.',lx)
    if nd>0
      lx=subs(lx,nd+1)
    endif
    nd=at('*',lx)
    if nd=0
      private &lx
    else
      private all like &lx
    endif
    nd=at(',',_var_priv)
  enddo
endif

do case
case !eof()
  _encontrado=.T.
  scatter memvar memo
  if type('_replaces')='C'
    do while !empty(_replaces)
      nd=at(',',_replaces)
      if nd=0
        lx=_replaces
        _replaces=''
      else
        lx=left(_replaces,nd-1)
        _replaces=subs(_replaces,nd+1)
      endif
      &lx
    enddo
  endif
case empty(_blancs)

case left(_blancs,3)='ALL'
  scatter memvar blank memo
case left(_blancs,1)='!'
  _blancs=subs(_blancs,2)
  private (_blancs)
  scatter memvar blank memo
otherwise
  do while !empty(_blancs)
    nd1=at(',',_blancs)
    if nd1=0
      lx1=_blancs
      _blancs=''
    else
      lx1=left(_blancs,nd1-1)
      _blancs=iif(len(_blancs)=nd1,'',subs(_blancs,nd1+1))
    endif
    do case
    case empty(lx1)
      
    case type(lx1)='C'
      &lx1=space(len(&lx1))
    case type(lx1)='N'
      &lx1=0
    case type(lx1)='D'
      &lx1={  /  /  }
    endcase
  enddo
endcase

if _encontrado
   If Used(_Fics)
      Select (_Fics)
      If Eof()
         Append Blank
      EndIf
      Gather MemVar Memo
   Else
      Wait Window "Fichero " + _Fics + " no está abierto" NoWait
   EndIf
endif

If Used('Consulta')
   Use In Consulta
EndIf

if !empty(_alis)
   select alias (_alis)
endif
*
return _encontrado

* ----------------------------------
* Bloquear registro actual
*	_fics		Fichero
*	_n_indice	Nombre del índice
* ----------------------------------
Function f3_rlock
Parameters _fics, _n_indice
*
set memowidth to 2000
do case
case wontop()<>'ST3TERM' .and. wontop()<>trim(sysprg.PRG_PROG) .and. !empty(wontop())
  return .T.
case _esc=.T.
  return .T.
endcase
_alis=alias()

  select SYSFC
  seek upper(_fics)
  if len(_fics) = 4
     sql_fc = _fics + _em
  else
     sql_fc =_fics
  endif

  fcomp=FC_CLAVE
  if type('_n_indice')='C'
    fcomp2=''
    for num=1 to memlines(sysfc.FC_CLAVE2)
      fcomp2=mline(sysfc.FC_CLAVE2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcomp=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif

  lxcomp=&fcomp
  lxselect='SELECT * FROM '+sql_fc+' where '+lxcomp
  _xier=f3_SqlExec(_asql, lxselect)

return iif(_xier <= 0, .F., .T.)

* ----------------------------------
* Desloquear registro actual
*	_fics		Fichero
*	_n_indice	Nombre del índice
* ----------------------------------
Function f3_unlock
Parameters _fics, _n_indice
*
set memowidth to 2000
do case
case wontop()<>'ST3TERM' .and. wontop()<>trim(sysprg.PRG_PROG) .and. !empty(wontop())
  return .T.
case _esc=.T.
  return .T.
endcase
_alis=alias()

=SqlCommit(_Asql)
return

* ----------------------------------
* Ayuda completa (Tecla F9)
* ----------------------------------
Function f3_hlp

if empty(_nvarh)
  return
endif

_valorh=&_nvarh
tipo=type('_valorh')
do case
case tipo='C'
  _valorh=''
case tipo='D'
  _valorh={  /  /  }
case tipo='N'
  _valorh=0
endcase
_hlpok=.F.
_hayhlp=.T.

Do Case
	*> Conexión con ADO
	Case _ADO_SN=='A' 
		do st3hlpADO
	*> Conexión con ODBC
	Case _ADO_SN=='O' 
		do st3hlp
	*Case _ADO_SN=='R'
		*> No implementada, opción para RDB
	*> Por defecto conexión ODBC
	OtherWise                                              
		do st3hlp
EndCase

_hayhlp=.F.
if _hlpok=.T.
  keyboard '{ENTER}'
endif
*
return

* ----------------------------------
* Ayuda completa de fichero 
* ----------------------------------
Function f3_hlpfc
*
Parameters _fic
*
if empty(_fic)
  return
endif
_fic=trim(_fic)
select (_fic)
_nvarh=field(1)
select SYSPRG
_hlpok=.F.
_hayhlp=.T.

Do Case
	*> Conexión con ADO
	Case _ADO_SN=='A' 
		do st3hlpFcADO with _fic
	*> Conexión con ODBC
	Case _ADO_SN=='O' 
		do st3hlpfc with _fic
	*Case _ADO_SN=='R'
		*> No implementada, opción para RDB
	*> Por defecto conexión ODBC
	OtherWise                                              
		do st3hlpfc with _fic
EndCase

_hayhlp=.F.
*
return

* ----------------------------------
* Tecla escape
* ----------------------------------
Function f3_esc
*
_esc=.T.
activate window wnada
deactivate window wnada
*
return .T.

* ----------------------------------
* Tecla Ctrl+Enter  o Alt+Enter
* ----------------------------------
Function f3_ok
Parameters _forma
*
store .F. to _enter,_graba
if _forma='O'
  _enter=.T.
else
  _graba=.T.
endif
activate window wnada
deactivate window wnada
*
return .T.

* ----------------------------------
* Cierra ficheros form
* ----------------------------------
Function f3_cerrar
Parameters _prog
*
_xobjw='W'+trim(_prog)
if file (_xobjw+'.PRG') .or.  file(_xobjw+'.FXP')
  do (_xobjw) with 'CERRAR', , _prog
endif


_logmem=_logmem+'Salida  : '+dtoc(date())+' '+time()+' '+PRG_PROG+' '+PRG_TIT+cr
nd=at('['+_prog+'/',_xopenfc)
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
    if lx2=_prog .or. left(lx2,2)='XL'           && Borrar transitorios
      select alias (lx2)
      lx3=dbf()
      use
      if file(lx3)
        delete file (lx3)
      endif
    else
      select alias (lx2)
      use
    endif
  endif
  nd=at('['+_prog+'/',_xopenfc)
enddo

***   ANTES  lx1='G'+trim(sysprg.PRG_PROG)
lx1='G'+trim(_prog)

if used(lx1)
  select alias (lx1)
  lx1=dbf()
  use
  delete file (lx1)
endif
select SYSPRG
if finalmenu=.F.
  for nd=1 to 12
    lx1='sysmemo.PRG_'+ltrim(str(nd))
    if &lx1=sysprg.PRG_PROG
      repl &lx1 with ''
      exit
    endif
  endfor
endif
*
return 

* ***********************************
* Funciones sobre fichero relacionado
* ***********************************


* ----------------------------------
* Inicializa un campo a blancos
* ----------------------------------
Function f3_limpia
*
keyboard '{CTRL+Y}'
*
return 

* ---------------------------------------------
* Interacción estandard
*	_itrvar		Variable que se busca
*	_replaces	Campos a reemplazar si se encuentra la ficha
*	_itr_altac	Dar ayuda igualmente en alta (Desconectado)
*   _var_priv	Variables privadas (No actualizar)
*   _blancs     Blancos a reemplazar. AVC - 08.07.1999
* ---------------------------------------------
Function f3_itr
Parameters _itrvar,_replaces,_var_priv,_blancs
*
set memowidth to 2000
_itrval=&_itrvar
do case
*case wontop()<>trim(sysprg.PRG_PROG)
*  return .T.
case empty(_itrvar)
  return .T.
case _esc=.T.
  return .T.
*case type('_itrval')=type('_itroldval') .and. _itrval==_itroldval
*  return .T.
endcase

_itrvar=upper(left(_itrvar+space(10),10))
if type('_var_priv')='C' .and. !empty(_var_priv)
  _var_priv=_var_priv+',  '
  nd=at(',',_var_priv)
  do while nd>0
    lx=left(_var_priv,nd-1)
    _var_priv=subs(_var_priv,nd+1)
    nd=at('*',lx)
    if nd=0
      private all like &lx
    else
      private all like &lx
    endif
    nd=at(',',_var_priv)
  enddo
endif

if type('_blancs')<>'C'
  _blancs=''
endif
_blancs = upper(_blancs)

select SYSVAR
seek _itrvar
lx1=left(_xfc+space(10),10)
do case
case eof()
  select SYSPRG
  return .T.
case (_altac.or._lockh=1) .and. (lx1=ITR_FIC1.or.lx1=ITR_FIC2.or.lx1=ITR_FIC3)
  select SYSPRG
  return .T.
case empty(&_itrvar)       && Inicializa si se entra blancos
  lx5=''
  lx6='['+wontop()+']'
  if !eof()
    set memowidth to 256
    for nd=1 to memlines(itr_blanc)
      lx4=mline(itr_blanc,nd)
      do case
      case empty(lx4)
      
      case left(lx4,len(lx6))=lx6
        lx5=subs(lx4,len(lx6)+1)
        nd=9999
      case at('[',lx4)=0
        lx5=lx4
      endcase
    endfor

    do while !empty(lx5)
      nd=at(',',lx5)
      if nd=0
        lx4=lx5
        lx5=''
      else
        lx4=left(lx5,nd-1)
        lx5=subs(lx5,nd+1)
      endif
      do case
      case type(lx4)='C'
        &lx4=space(len(&lx4))
      case type(lx4)='N'
        &lx4=0
      case type(lx4)='D'
        &lx4={  /  /  }
      endcase
    enddo
  endif
  select SYSPRG
  return .T.
endcase
_encontrado=.F.

if !empty(ITR_IDX1)
  lx1=ITR_VALOR1
  If At('=', lx1) > 0
     &lx1
  EndIf

  _itrval=&lx1
  if !empty(_itrval) .Or. _itrval
    =f3_bus(ITR_FIC1,ITR_IDX1,_itrval,ITR_APROX1,ITR_VALOR1)
  endif
  if !empty(sysvar.ITR_IDX2) .and. _encontrado=.F.
    select SYSVAR
    lx1=ITR_VALOR2
    _itrval=&lx1
    if !empty(_itrval)
      =f3_bus(ITR_FIC2,ITR_IDX2,_itrval,ITR_APROX2,ITR_VALOR2)
    endif

    if !empty(sysvar.ITR_IDX3) .and. _encontrado=.F.
      select SYSVAR
      lx1=ITR_VALOR3
      _itrval=&lx1
      if !empty(_itrval)
        =f3_bus(ITR_FIC3,ITR_IDX3,_itrval,ITR_APROX3,ITR_VALOR3)
      endif
    endif
  endif
endif

if _encontrado
  if type('_replaces')='C'
    do while !empty(_replaces)
      nd=at(',',_replaces)
      if nd=0
        lx=_replaces
        _replaces=''
      else
        lx=left(_replaces,nd-1)
        _replaces=subs(_replaces,nd+1)
      endif
      &lx
    enddo
  endif
else
  do case
    case empty(_blancs)

    case left(_blancs,3)='ALL'
      scatter memvar blank memo

    case left(_blancs,1)='!'
      _blancs=subs(_blancs,2)
      private (_blancs)
      scatter memvar blank memo

    otherwise
      do while !empty(_blancs)
        nd1=at(',',_blancs)
        if nd1=0
          lx1=_blancs
          _blancs=''
        else
          lx1=left(_blancs,nd1-1)
          _blancs=iif(len(_blancs)=nd1,'',subs(_blancs,nd1+1))
        endif
        do case
          case empty(lx1)
      
          case type(lx1)='C'
            &lx1=space(len(&lx1))
          case type(lx1)='N'
            &lx1=0
          case type(lx1)='D'
            &lx1={  /  /  }
        endcase
      enddo
  endcase
endif           && if encontrado

*>
select SYSPRG
return _encontrado

* ---------------------------------------------
* Búsqueda estandard  (Acción de búsqueda)
*   _itrfic		Fichero  							DELE
*	_itridx		Indice								CODIGO
*	_itrval		Valor								248
*	_itraprox	Búsqueda aproximada					0/1
*	_itrvalor	Expresión que contiene el valor		left(DEL_NOM,3)
* ---------------------------------------------
Function f3_bus
Parameters _itrfic,_itridx,_itrval,_itraprox,_itrvalor
*
do case
case atc('val(',_itrvalor)>0
  nd4=atc('val(',_itrvalor)
  _itrvalor=subs(_itrvalor,nd4+4)
  nd4=1
  nd5=0
  do while nd4>0
    nd5=nd5+1
    do case
    case subs(_itrvalor,nd5,1)='('
      nd4=nd4+1
    case subs(_itrvalor,nd5,1)=')'
      nd4=nd4-1
    endcase
  enddo
  _itrvalor=left(_itrvalor,nd5-1)
  if alltrim(&_itrvalor)<>ltrim(str(val(&_itrvalor)))
    _encontrado=.F.
    return
  endif
case atc('left(',_itrvalor)>0
  nd4=atc('left(',_itrvalor)
  _itrvalor=subs(_itrvalor,nd4+5)
  nd4=at(',',_itrvalor)
  if nd4>0
    _itrvalor=left(_itrvalor,nd4-1)
    _itrvalor=trim(&_itrvalor)
*    if len(_itrval)<len(_itrvalor)
*	   _encontrado=.F.
*       return
*    endif
  endif
endcase
_itrfic=trim(_itrfic)
select (_itrfic)
if _itraprox=1 .and. type('_itrval')='C' .and. (left(_itrval,1)='*' .or. right(trim(_itrval),1)='*')
  _valorh=trim(_itrval)
  _hlpok=.F.
  do st3hlp
  _encontrado=_hlpok
else
  if type('_itrval')='C'
    _itrval=upper(_itrval)
  endif
  if type('_itroldval')='C'
    _itroldval=upper(_itroldval)
  endif

  do case
  case _itraprox=1
    _encontrado=.F.
  otherwise
    select SYSFC
    seek _itrfic
    if len(_itrfic) = 4
      sql_fc = _itrfic + _em
    else
      sql_fc=_itrfic
    endif

    Do Case
       case _itridx='CODIGO'
          fcomp=FC_CLAVE
       case _itridx='DESCRI' .Or. _itridx='NOMBRE'
          fcomp=FC_CLAVE2
 	   OtherWise
          fcomp=FC_CLAVE
    EndCase

    lxcomp=&fcomp
    ad=at('=',Lxcomp,2) 
    nd=at('=',lxcomp)
    if nd>0 .And. ad = 0
       if type('_itrval')='C'
          lxcomp=left(lxcomp,nd)+_cm+_itrval+_cm
       endif
    endif

    lxselect='SELECT * FROM '+sql_fc+' where '+lxcomp
    _xier=f3_SqlExec(_asql,lxselect,'consulta')
    if _xier < 0
       _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
       =Anomalias()
    endif

    select CONSULTA
    _encontrado=iif(eof(),.F.,.T.)
  endcase

  if _encontrado=.T.
    scatter memvar memo
  endif
endif
*
return

* --------------------------------------------
* Activar una pantalla
* Permitir pasar parámetros. AVC - 04.08.1999
* --------------------------------------------
Function f3_bot
Parameters _datos,_programa, prm1, prm2, prm3, prm4, prm5, prm6, prm7, prm8, prm9
Private _inx, _lx, lx1, nd
*
do  case
case upper(_programa)='AUX'
  prog=left(upper(_datos)+space(10),10)
  select SYSPRG
  r1_prg=recno()
  seek prog
  prog=trim(prog)
  _datos='pntw\'+prog
  private _xobjw

  for nd=1 to memlines(sysprg.PRG_VAR)
     lx1=mline(sysprg.PRG_VAR,nd)
     if !empty(lx1)
        &lx1
     endif
  endfor

  *> Formar la lista de parámetros.
  _lx = ''
  For _inx = 1 To PCount() - 2
     _lx = _lx + Iif(Empty(_lx), "Do Form (_datos) With prm" + Str(_inx, 1, 0), ", prm" + Str(_inx, 1, 0))
  EndFor

  If Empty(_lx)
     _lx = "Do Form (_datos)"
  EndIf
  &_lx
  *do form (_datos)

  select SYSPRG
  go r1_prg
  prog=trim(PRG_PROG)
  return

case upper(_programa)='ST3PRG'
  prog=upper(_datos)
  if wexist(_datos)
    activate window (_datos)
    lx=_datos+'.WindowState'
    if &lx<>0
      &lx=0
    endif
    return
  else
    prg='ST3PRG'
  endif
otherwise
  prog=''
  prg=_programa
endcase
swprg=.F.
clear events
_xier=0
*
return

* ----------------------------------
* Traduce una línea de texto
* ----------------------------------
Function f3_t
LParameters f3t_linea,_idioma
*

if type('_idioma')<>'N'
  _idioma=_xidiom
endif
if _idioma>1 .and. .not. empty(f3t_linea)
  f3t_ali=alias()
  f3t_nd1=at('[',f3t_linea)
  f3t_nd2=at(']',f3t_linea)
  if f3t_nd1>0 .and. f3t_nd2>f3t_nd1
    f3t_lx1=subs(f3t_linea,f3t_nd1+1,f3t_nd2-f3t_nd1-1)
    f3t_linea=stuff(f3t_linea,f3t_nd1+1,f3t_nd2-f3t_nd1-1,'')
  else
    f3t_lx1=''
  endif
  if empty(f3t_linea) .or. f3t_linea='[]'
   
  else
    if len(f3t_linea)<=30
      select SYSTXT30
      seek left(f3t_linea+space(30),30)
    else
      select SYSTXT
      seek left(f3t_linea+space(80),80)
    endif
    do case
    case eof()
      append blank
      repl TXT_I1 with f3t_linea
    case memlines(TXT_I2)>=_idioma-1
      f3t_lx=mline(TXT_I2,_idioma-1)
      f3t_linea=iif(!empty(f3t_lx),f3t_lx,f3t_linea)
    endcase
  endif
  f3t_nd1=at('[',f3t_linea)
  if f3t_nd1>0
    f3t_linea=stuff(f3t_linea,f3t_nd1,1,'['+f3t_lx1)
  endif
  if !empty(f3t_ali)
    select alias (f3t_ali)
  else
    select SYSPRG
  endif
endif
*
return trim(f3t_linea)

* ----------------------------------
* Funcion encriptación del Password
*
*	LXP		Password
*	LXU		Usercod
* ----------------------------------
function f3_pas
Parameters lxp,lxu
*
lxp=chrtran(lxp,chr(13),'')
lxp=left(lxp+'......',6)
lxu=left(lxu+'......',6)
lxpas='ABCDEFGHIJKLMNÑOPQRSTUVWXYZ0123456789.'
lxpas=lxpas+lxpas
lxsal='52349983487736554328837666123438773651'
lx1=''
for nd=1 to 6
  nd1=max(1,at(subs(lxu,nd,1),lxpas))
  nd2=at(subs(lxp,nd,1),lxpas)
  nd3=nd2+val(subs(lxsal,nd1,1))
  lx1=lx1+subs(lxpas,nd3,1)
endfor
*
return lx1

* ---------------------------------
* Comprobacion Base de Datos
*	_tipo_p		C2	Pantalla de líneas
* ---------------------------------
Function  f3_ci
Parameters _tipo_p
*
if type('_tipo_p')='C' .and. _tipo_p='C2'
  xnd2=memlines(sysprg.PRG_CIL)
else
  _tipo_p='F'
  xnd2=memlines(sysprg.PRG_CI)
endif
lxtit=f3_t('CONDICIONES NO CUMPLIDAS AL INTRODUCIR LOS DATOS')
ali=alias()
select SYSVAR
_lxerr=''
lxerr=''
set memowidth to 255
for xnd1=1 to xnd2
  if _tipo_p='C2'
    _lxe=mline(sysprg.PRG_CIL,xnd1)
  else
    _lxe=mline(sysprg.PRG_CI,xnd1)
  endif
  do case
  case !empty(_lxe) .and. left(_lxe,1)='['
    lxerr=f3_t(chrtran(_lxe,'[]',''))
  case .not. empty(_lxe)
    correcto=&_lxe
    do case
    case !correcto .and. at('()',_lxe)>0

    case !correcto .and. !empty(lxerr)
      _lxerr=_lxerr+lxerr+cr
    case !correcto
      lx1=lower(chrtran(_lxe,' ',''))
      lx2='.and./.or./!empty/.not.empty/empty/seek/upper/left/right/'
      lx3=f3_t(' y / o / falta entrar/ falta entrar/ se ha entrado/ no se ha encontrado/ Mayúsculas/ Izquierda/ Derecha/')
      do while .not. empty(lx2)
        nd1=at('/',lx2)
        nd2=at('/',lx3)
        lx4=left(lx2,nd1-1)
        lx5=left(lx3,nd2-1)
        lx2=subs(lx2,nd1+1)
        lx3=subs(lx3,nd2+1)
        nd=at(lx4,lx1)
        do while nd>0
          lx1=stuff(lx1,nd,len(lx4),lx5)
          nd=at(lx4,lx1)
        enddo
      enddo
      nd=at('_',lx1)
      select SYSVAR
      do while nd<>0
        store nd to nd1,nd2
        do while nd1-1>=1 .and. subs(lx1,nd1-1,1)>='a' .and. subs(lx1,nd1-1,1)<='z'
          nd1=nd1-1
        enddo
        do while nd2+1<=len(lx1) .and. subs(lx1,nd2+1,1)>='a' .and. subs(lx1,nd2+1,1)<='z'
          nd2=nd2+1
        enddo
        lx2=upper(subs(lx1,nd1,nd2-nd1+1))
        seek lx2
        if eof()
          lx1=stuff(lx1,nd,1,'|')
        else
          lx1=stuff(lx1,nd1,nd2-nd1+1,' '+trim(f3_t(VAR_DES)))
        endif
        nd=at('_',lx1)
      enddo
      do case
      case subs(lx1,nd,1)='(' .and. subs(lx1,nd+1,1)=')'
        nd=nd+1
      case subs(lx1,nd,1)='('
        lx1=stuff(lx1,nd,1,' (')
        nd=nd+1
      case subs(lx1,nd,1)=')'
        lx1=stuff(lx1,nd,1,') ')
        nd=nd+1
      case subs(lx1,nd,1)=')'
        lx1=stuff(lx1,nd,1,'_')
      endcase
      _lxerr=_lxerr+lx1+cr
    endcase
    lxerr=''
  endcase
endfor

if !empty(ali)
   select alias (ali)
 endif
*
return

* -------------------------------------
* salvar/Restaurar variables de memoria
* -------------------------------------
Function f3_mem
Parameters _forma,_pantalla
*
_pantalla=left(_pantalla+space(10),10)
_ali=alias()
for nd=1 to 12
  lx1='sysmemo.PRG_'+ltrim(str(nd))
  if &lx1=_pantalla
    lx1='sysmemo.PRG_M'+ltrim(str(nd))
    if _forma='S'
      save all except _* to memo &lx1 
    else
      restore from memo &lx1 addi
    endif
    exit
  endif
endfor
select (_ali)
*
return

* ------------------------------------------
* Presentación por pantalla de un termómetro
*
*	_acciont	TEXTO	enseñar texto 'Espere..'
*				LINEA	Con valor actual y total
*				FIN		Cierra el formulario
*				TITULO	Enseña un título (2o. parámetro el título)
* ------------------------------------------
Function f3_term
Parameters _acciont,_valort,_valort_tot
*
_acciont=upper(_acciont)
do case
case _acciont='FIN'
  ST3TERM.release
case _acciont='TEXTO'
  st3term.L_normal1.visible=.T.
  st3term.refresh
case _acciont='LINEA'
  st3term.L_normal1.visible=.F.
  _nd=round(min(_valort,_valort_tot)*360/_valort_tot,0)
  if _nd>1
    st3term.LINEA_OK.width=_nd
  endif
  st3term.refresh
case _acciont='LIMPIA'
  st3term.L_normal1.visible=.F.
  st3term.Label2.visible=.F.
  st3term.Label3.visible=.F.
  st3term.Label4.visible=.F.
  st3term.Label5.visible=.F.
  st3term.Label6.visible=.F.
  st3term.Label7.visible=.F.
  st3term.Label8.visible=.F.
  st3term.Label9.visible=.F.
  st3term.Line1.visible=.F.
  st3term.Line2.visible=.F.
  st3term.Line3.visible=.F.
  st3term.Line4.visible=.F.
  st3term.Line5.visible=.F.
  st3term.Line6.visible=.F.
  st3term.Line7.visible=.F.
  st3term.Line8.visible=.F.
  st3term.Line9.visible=.F.
  st3term.Line10.visible=.F.
  st3term.Line11.visible=.F.
  st3term.st_box1.visible=.F.
  st3term.LINEA_OK.visible=.F.
  st3term.refresh
case _acciont='RESTAURA'
  st3term.L_normal1.visible=.F.
  st3term.Label2.visible=.T.
  st3term.Label3.visible=.T.
  st3term.Label4.visible=.T.
  st3term.Label5.visible=.T.
  st3term.Label6.visible=.T.
  st3term.Label7.visible=.T.
  st3term.Label8.visible=.T.
  st3term.Label9.visible=.T.
  st3term.Line1.visible=.T.
  st3term.Line2.visible=.T.
  st3term.Line3.visible=.T.
  st3term.Line4.visible=.T.
  st3term.Line5.visible=.T.
  st3term.Line6.visible=.T.
  st3term.Line7.visible=.T.
  st3term.Line8.visible=.T.
  st3term.Line9.visible=.T.
  st3term.Line10.visible=.T.
  st3term.Line11.visible=.T.
  st3term.st_box1.visible=.T.
  st3term.LINEA_OK.visible=.T.
  st3term.refresh
case _acciont='TITULO'
  st3term.caption=f3_t(_valort)
  _valort=0
endcase
*
return

* ------------------ FUNCIONES ODBC


* ------------------------------------------
* Insertar un registro
*
*	_fichero 	Tabla en la que deseo insertar
* ------------------------------------------
Function f3_ins
Parameters _fichero
*
Private StsCentury
set memowidth to 2000
_ali=alias()
_insertado=.T.

  select SYSFC
  seek upper(_fichero)
  if len(_fichero) = 4
     sql_fc =_fichero + _em
  else
     sql_fc = _fichero
  endif

  fcinsert=FC_INSERT
  set point to '.'
  fcinsert=&fcinsert

  ***> Control fecha en blanco. Poner fecha mínima en lugar de nulo.----
  StsCentury = Sys(2001, 'Century')
  Set Century On
  nd=at("to_date('  /  /    ','dd-mm-yyyy')",fcinsert)
  do while nd>0
     fcinsert=stuff(fcinsert,nd,34, "to_date('" + DToC(_FecMin) + "','dd-mm-yyyy')")
     nd=at("to_date('  /  /    ','dd-mm-yyyy')",fcinsert)
  enddo
  Set Century &StsCentury

  lxinsert='INSERT INTO '+sql_fc+' VALUES ('+fcinsert+')'
  _xier=f3_SqlExec(_asql,lxinsert)
  if _xier>0
    select (_fichero)
    if eof()
      insert into (_fichero) from memvar
    else 
      gather memvar memo
    endif
  else
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
    _insertado=.F.
  endif

_xier=0

if !empty(_ali)
   select alias(_ali)
endif
*
return _insertado


* ------------------------------------------
* Modificar un registro
*
*	_fichero 	Tabla en la que deseo modificar
*	_n_indice	Código de índice
*	_campos		Campos que quiero actualizar
* ------------------------------------------
Function f3_upd
Parameters _fichero,_n_indice,_campos,_valores
*
private num
set memowidth to 2000
_ali=alias()
_actualizado=.T.
if type('_valores')<>'C'
  _valores=''
endif

  select SYSFC
  seek upper(_fichero)
  if len(_fichero)=4
    sql_fc = _fichero+_em
  else
    sql_fc = _fichero
  endif

  set point to '.'
  fcupdate=FC_UPDATE
  if type('_campos')='C'               && Se definen manualmente los campos a actualizar
    fcupdate=''
    do while !empty(_campos)
      nd=at(',',_campos)
      if nd=0
        varvar=_campos
        _campos=''
      else
        varvar=left(_campos,nd-1)
        _campos=subs(_campos,nd+1)
      endif
      if empty(_valores)
        v_varvar=varvar
      else
        nd=at(',',_valores)
        if nd=0
          v_varvar=valores
          _valores=''
        else
          v_varvar=left(_valores,nd-1)
          _campos=subs(_valores,nd+1)
        endif
      endif
      fcupdate=fcupdate+'+","+'
      varvar=left(varvar+space(10),10)
      select SYSVAR
      seek varvar
      varvar=trim(varvar)
      do case
      case type('v_varvar')='C'
        fcupdate = fcupdate + '"' + varvar + '=" + _cm + m.' + v_varvar + ' + _cm'
      case type('v_varvar')='D'
        *fcupdate=fcupdate+'"'+varvar+'=to_date("+_cm+dtoc(m.'+v_varvar+')+_cm+","+_cm+"dd-mm-yyyy"+_cm+")"'
        fcupdate = fcupdate + '"' + varvar + "=' + _GCD(v_varvar)"
      case type('v_varvar')='N'
        if sysvar.VAR_DEC=0
          fcupdate=fcupdate+'"'+varvar+'="+str(m.'+v_varvar+','+ltrim(str(sysvar.VAR_LN))+')'
        else
          fcupdate=fcupdate+'"'+varvar+'="+str(m.'+v_varvar+','+ltrim(str(sysvar.VAR_LN))+','+str(sysvar.VAR_DEC,1)+')'
        endif
      endcase
    enddo
    if !empty(fcupdate)
      fcupdate=subs(fcupdate,6)
      nd=at('"+"',fcupdate)
      do while nd>0
        fcupdate=stuff(fcupdate,nd,3,'')
        nd=at('"+"',fcupdate)
      enddo  
    endif
  endif
  fcupdate=&fcupdate

  ***> Control fecha en blanco. Poner fecha mínima en lugar de nulo.----
  StsCentury = Sys(2001, 'Century')
  Set Century On
  nd=at("to_date('  /  /    ','dd-mm-yyyy')",fcupdate)
  do while nd>0
     fcupdate = stuff(fcupdate,nd,34, "to_date('" + DToC(_FecMin) + "','dd-mm-yyyy')")
     nd=at("to_date('  /  /    ','dd-mm-yyyy')",fcupdate)
  enddo
  Set Century &StsCentury

  select SYSFC
  fcclave=FC_CLAVE
  if type('_n_indice')='C'					&& Se define una condición especial
    fcomp2=''
    for num=1 to memlines(sysfc.FC_CLAVE2)
      fcomp2=mline(sysfc.FC_CLAVE2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcclave=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif

  set point to ","
  lxclave=&fcclave
  if _teclado_esp=.F.
    set point to "."
  endif
  lxupdate='UPDATE '+sql_fc+' SET '+fcupdate+' where '+lxclave
  _xier=f3_SqlExec(_asql,lxupdate)

  if _xier>0
    select (_fichero)
    if eof()
      insert into (_fichero) from memvar
    else
      gather memvar memo
    endif
  else
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
    _actualizado=.F.
  endif

_xier=0

if !empty(_ali)
   select alias(_ali)
endif
*
return _actualizado

* ------------------------------------------
* Eliminar un registro
*
*	_fichero 	Tabla en la que deseo eliminar la/s ficha/s
*	_n_indice	Indice de baja
* ------------------------------------------
Function f3_baja
Parameters _fichero,_n_indice
*
private num
set memowidth to 2000
_ali=alias()
_eliminado=.T.

  select SYSFC
  seek upper(_fichero)
  if len(_fichero)=4
    sql_fc=_fichero+_em
  else
    sql_fc=_fichero
  endif
  fcclave=FC_CLAVE
  if type('_n_indice')='C'					&& Se usa un índice diferente al principal
    fcomp2=''
    for num=1 to memlines(sysfc.FC_CLAVE2)
      fcomp2=mline(sysfc.FC_CLAVE2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcclave=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif

  lxclave=&fcclave
  lxdelete='DELETE FROM '+sql_fc+' where '+lxclave
  _xier=f3_SqlExec(_asql,lxdelete)
  if _xier <= 0
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
    _eliminado=.F.
  endif

_xier=0

if !empty(_ali)
   select alias(_ali)
endif
*
return _eliminado


* ------------------------------------------
* Ir al inicio de una tabla
*
*	_fichero 	Tabla en la que deseo consultar la/s ficha/s
*	_n_indice	Indice de busqueda
* ------------------------------------------
Function f3_top
Parameters _fichero,_n_indice
*
set memowidth to 2000
_ali=alias()
_gotop=.T.

  select SYSFC
  seek upper(_fichero)
  if len(_fichero)=4
    sql_fc=_fichero+_em
  else
    sql_fc=_fichero
  endif
  fcwhere=FC_INIT
  if type('_n_indice')='C'					&& Se usa un índice diferente al principal
    fcomp2=''
    for num=1 to memlines(sysfc.FC_INIT2)
      fcomp2=mline(sysfc.FC_INIT2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcwhere=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif
  select (_fichero)
  zap
  lxwhere=&fcwhere
  lxselect='SELECT * FROM '+sql_fc+' where '+lxwhere
  _xier=f3_SqlExec(_asql,lxselect,_fichero)
  if _xier <= 0
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
     _gotop=.F.
  endif

  _xier=0
  select (_fichero)
  go top

if !empty(_ali)
   select alias(_ali)
endif
*
return _gotop

* ------------------------------------------
* Ir al final de una tabla
*
*	_fichero 	Tabla en la que deseo consultar la/s ficha/s
*	_n_indice	Indice de busqueda
* ------------------------------------------
Function f3_bottom
Parameters _fichero,_n_indice
*
set memowidth to 2000
_ali=alias()
_gotop=.T.

  select SYSFC
  seek upper(_fichero)
  if len(_fichero)=4
    sql_fc=_fichero+_em
  else
    sql_fc=_fichero
  endif
  fcwhere=FC_BOTTOM
  if type('_n_indice')='C'					&& Se usa un índice diferente al principal
    fcomp2=''
    for num=1 to memlines(sysfc.FC_BOTTOM2)
      fcomp2=mline(sysfc.FC_BOTTOM2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcwhere=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif
  select (_fichero)
  zap
  lxwhere=&fcwhere
  lxselect='SELECT * FROM '+sql_fc+' where '+lxwhere
  _xier=f3_SqlExec(_asql,lxselect,_fichero)

  if _xier>0
    _gotop=.T.
  else
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     _gotop=.F.
  endif
  _xier=0

if !empty(_ali)
   select alias(_ali)
endif
*
return _gotop


* ------------------------------------------
* Ficha anterior
*
*	_fichero 	Tabla en la que deseo consultar la/s ficha/s
*	_n_indice	Indice de busqueda
* ------------------------------------------
Function f3_ant
Parameters _fichero,_n_indice
*
set memowidth to 2000
_ali=alias()
_gotop=.T.

  select SYSFC
  seek upper(_fichero)
  if len(_fichero)=4
    sql_fc=_fichero+_em
  else
    sql_fc=_fichero
  endif
  fcwhere=FC_ANT
  if type('_n_indice')='C'					&& Se usa un índice diferente al principal
    fcomp2=''
    for num=1 to memlines(sysfc.FC_ANT2)
      fcomp2=mline(sysfc.FC_ANT2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcwhere=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif
  select (_fichero)
  zap
  lxwhere=&fcwhere
  lxselect='SELECT * FROM '+sql_fc+' where '+lxwhere
  _xier=f3_SqlExec(_asql,lxselect,_fichero)

  if _xier>0
    _gotop=.T.
  else
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
     _gotop=.F.
  endif
  _xier=0

if !empty(_ali)
   select alias(_ali)
endif
*
return _gotop

* ------------------------------------------
* Ficha posterior
*
*	_fichero 	Tabla en la que deseo consultar la/s ficha/s
*	_n_indice	Indice de busqueda
* ------------------------------------------
Function f3_pos
Parameters _fichero,_n_indice
*
set memowidth to 2000
_ali=alias()
_gotop=.T.

  select SYSFC
  seek upper(_fichero)
  if len(_fichero)=4
    sql_fc=_fichero+_em
  else
    sql_fc=_fichero
  endif
  fcwhere=FC_POS
  if type('_n_indice')='C'					&& Se usa un índice diferente al principal
    fcomp2=''
    for num=1 to memlines(sysfc.FC_POS2)
      fcomp2=mline(sysfc.FC_POS2,num)
      if atc(_n_indice+' ',fcomp2)>0
        fcwhere=subs(fcomp2,len(_n_indice)+1)
        num=9999
      endif
    endfor
  endif
  select (_fichero)
  zap
  lxwhere=&fcwhere
  lxselect='SELECT * FROM '+sql_fc+' where '+lxwhere
  _xier=f3_SqlExec(_asql,lxselect,_fichero)

  if _xier>0
    _gotop=.T.
  else
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
     _gotop=.F.
  endif
  _xier=0

if !empty(_ali)
   select alias(_ali)
endif
*
return _gotop

* -------------------------------------------------------
* Pregunta SQL
*
*	lxsele		Selecciona
*	lxfrom		ficheros
*	lxwher		Condiciones
*	lxord		Orden
*   lxgrup  	Agrupación
*	lxcom		Nombre del fichero donde se guardan los datos
*	lxcomfld	Campos donde se guardan los datos
*	lxBloq		Tipo de bloqueo ('' o 'For Update [NoWait]')
* -----------------------------------------------------------------
Function f3_sql
Parameters lxsele,lxfrom,lxwher,lxord,lxgrup,lxcom,lxcomfld, lxBloq
Private Bbb, lx, lx1, lx2, lx3, nd, nd1, _hay_recno, lx_sql, _sql_tmp, _sql_zap

set memowidth to 2000
_sql_zap=.T.
if type('lxgrup')<>'C'
  lxgrup=''
endif
if type('lxcom')<>'C'
  lxcom=trim(sysprg.PRG_PROG)
  if !used(lxcom)
    lxcom='G'+lxcom
  endif
else
  if left(lxcom,1)='+'
    lxcom=subs(lxcom,2)
    _sql_zap=.F.
  endif
endif
if type('lxComFld')<>'C'
  lxComFld=''
endif
if type('lxBloq')<>'C'
  lxBloq=''
endif
_lxcom='_'+lxcom

  lx1=lxfrom+',    '
  lxfrom=''
  nd=at(',',lx1)
  do while nd>0
    lx2=left(lx1,nd-1)
    lx1=subs(lx1,nd+1)
    lxfrom=lxfrom+','+lx2+iif(len(lx2)=4,_em,'')
    nd=at(',',lx1)
  enddo
  lxfrom=subs(lxfrom,2)
  if !empty(lxwher)
    nd=atc('betw(',lxwher)
    do while nd>0
      lx1=subs(lxwher,nd)
      nd1=at(')',lx1)
      lx1=left(lx1,nd1)               && expresion original
      lx2=subs(lx1,6,len(lx1)-6)      && expresion a traducir   
      nd1=at(',',lx2)
      lx3=left(lx2,nd1-1)+' BETWEEN '
      lx2=subs(lx2,nd1+1)
      nd1=at(',',lx2)
      lx3=lx3+left(lx2,nd1-1)+' AND '+subs(lx2,nd1+1)
      lxwher=stuff(lxwher,nd,len(lx1),lx3)
      nd=atc('betw(',lxwher)
    enddo
    nd=atc('.and.',lxwher)
    do while nd>0
      lxwher=stuff(lxwher,nd,5,' AND ')
      nd=atc('.and.',lxwher)
    enddo
    nd=atc('.or.',lxwher)
    do while nd>0
      lxwher=stuff(lxwher,nd,4,' OR ')
      nd=atc('.or.',lxwher)
    enddo
    lxwher=lxwher+' '
    nd=atc('m.',lxwher)
    do while nd>0
      lx1=subs(lxwher,nd)
      nd1=at(' ',lx1)
      if nd1>0
        lx1=left(lx1,nd1-1)
        nd1=len(lx1)
        lx2=&lx1
        do case
        case type('lx2')='C'
          lx2=_cm+lx2+_cm
        case type('lx2')='N'

        endcase
        lxwher=stuff(lxwher,nd,nd1,lx2)
      else
        lxwher=''
      endif
      nd=atc('m.',lxwher)
    enddo
  endif
  nd=atc('recno(),',lxsele)
  _hay_recno=.F.
  if nd>0
    _hay_recno=.T.
    lxsele=stuff(lxsele,nd,8,'')
  endif

lx='SELECT '+lxsele+' FROM '+lxfrom
if !empty(lxwher)
  lx=lx+' WHERE '+lxwher
endif
if !empty(lxgrup)
  lx=lx+' GROUP BY '+lxgrup
endif
if !empty(lxord)
  lx=lx+' ORDER BY '+lxord
endif

  lx_sql=lx + ' ' + lxBloq
  _xier=f3_SqlExec(_asql,lx_sql,'SQL_CONS')

  if _xier <= 0
     _LxErr = 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr + _LxErr
     =Anomalias()
     Return .F.
  endif

  *> Crear cursor, si no existe.
  If !Used(lxcom)
     Select SQL_CONS
     _sql_tmp = Alltrim(GetEnv('TMP')) + '\' + lxcom + ".DBF"

     && Antes.
     && Copy Structure To (_sql_tmp)
     && Use &_sql_tmp Alias &lxcom In 0 Exclusive
     && End Antes.

     =AFields(Bbb)
     Create Cursor (lxcom) From Array Bbb
     Select (lxcom)
  EndIf

  select SQL_CONS
  lx=dbf()
  select (lxcom)
  if _sql_zap
    zap
  endif
  If !Empty(lxComFld)
*     Append From &lx Fields &lxComFld
     Append From Dbf('SQL_CONS') Fields &lxComFld
  Else
*     Append From &lx
     Append From Dbf('SQL_CONS')
  EndIf
  go top
  if _hay_recno
    repl all EXP_1 with 1
    go top
  endif

  select SQL_CONS
  _oksql=iif(eof(),.F.,.T.)
  use

return _oksql

****************************************************************************
* Overload de f3_SqlExec() de VFP, para permitir uso de ADO.
*
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarvar -----> Nombre de la variable.
*
* Devuelve: nStatus -----> Resultado: < 0, Error.
*                                     > 0, OK.
*                                     = 0, Revisar configuración.
*
*> Añadir control de caracteres ilegales. AVC - 15.09.2006
****************************************************************************
Function f3_SqlExec
Parameters nASql, cSQL, cCursor

Private nStatus, cOldSele, oDBG, cSentencia

*> Conversión de caracteres ilegales (apóstrofos, ... ).
cSentencia = cSQL			&& _GRCI(cSQL)

If Type('cCursor')=='C'
   nStatus = SqlPrepare(nASql, cSentencia, cCursor)
Else
   nStatus = SqlPrepare(nASql, cSentencia)
   cCursor = ""
EndIf

If nStatus > 0
   nStatus = SqlExec(nASql)
   If nStatus <= 0
      _LxErr = Message() + cr + '(PREPARE) ' + cSentencia + cr
   EndIf

   =SqlMoreResults(nASql)
Else
   *> Tratamiento de errores.
   _LxErr = Message() + cr + '(EXEC) ' + cSentencia + cr
EndIf

If _DEBUGSQL
	oDBG = CreateObject('msgdbg', [SQL=cSentencia, ERR=_LxErr])

	*> IMPORTANTE!!, La cadena total no debe exceder de 254 caracteres.
	=oDBG.exec([USR=&_UsrCod, CUR=&cCursor])
EndIf

Return nStatus

*>-------------------------------------------------------------------
*> Salvar los datos de un fichero para recuperarlos posteriormente
*> _fichero_s 	Fichero a salvar
*>-------------------------------------------------------------------
Function f3_salva
Parameters _fichero_s
*
lx1='X_'+trim(_fichero_s)
if used(lx1)       && Ya está salvado un fichero
  return
endif
_ali=alias()
select (_fichero_s)

_tmp_s1=Sys(2023) + "\" + Sys(2015)
_tmp_s2=_tmp+_tmp_s1
copy structure to (_tmp_s2)
use (_tmp_s2) in 0 alias &lx1
insert into &lx1 from memvar

if !empty(_ali)
   select alias(_ali)
endif
*
return

* ------------------------------------------------------
* Recupera los datos de un fichero salvado anteriormente
* _fichero_s 	Fichero a recuperar
* ------------------------------------------------------
Function f3_recupera
Parameters _fichero_s
*
lx1='X_'+trim(_fichero_s)
if !used(lx1)                && No hay datos salvados para recuperar
  return
endif
_ali=alias()
select &lx1
scatter memvar memo
use
delete file (_tmp_s2+'.DBF')
if file (_tmp_s2+'.FPT')
  delete file (_tmp_s2+'.FPT')
endif  

if !empty(_ali)
   select alias(_ali)
endif
*
return

****************************************************************************
* Obtener una sentencia SQL parcial cuando está involucrado un campo fecha.
* Preparado, por el momento para ORACLE, AS/400 y SQLSERVER.
****************************************************************************
* Añadir entorno ACCESS. AVC - 03.02.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           dVarVar -----> Expresión de fecha a utilizar.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtDate
Parameters cEntorn, cVersion, dVarVar
Private cEntornL, cSelec
Local Inx, lEstado, dMyDate

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To cSelec, _LxErr
dMyDate = Iif(Type('dVarVar')=='D' .Or. Type('dVarVar')=='T', dVarVar, CToD(dVarVar))
dMyDate = Iif(Empty(dMyDate), Date(), dMyDate)

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "TO_DATE('" + DToC(dMyDate) + "', 'DD-MM-YYYY')"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "'" + Transform(Val(DToC(dMyDate, 1)), '9999-99-99') + "'"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "CONVERT(DateTime, '" + DToC(dMyDate) + "', 103)"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = dMyDate

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cSelec = "#" + DToC(dMyDate) + "#"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener una sentencia SQL parcial cuando está involucrado un campo hora.
* Preparado, por el momento para ORACLE y SQLSERVER.
****************************************************************************
* Añadir entorno ACCESS. AVC - 03.02.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           dVarVar -----> Expresión de hora a utilizar.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtTime
Parameters cEntorn, cVersion, dVarVar
Private cEntornL, cSelec
Local Inx, lEstado, dMyDate

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To cSelec, _LxErr
dMyDate = Iif(Type('dVarVar')=='D' .Or. Type('dVarVar')=='T', dVarVar, CToT(dVarVar))
dMyDate = Iif(Empty(dMyDate), Date(), dMyDate)

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "TO_DATE('" + DToC(dMyDate) + "', 'YYYYMMDDHH24MMSS')"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "'" + Transform(Val(DToC(dMyDate, 1)), '9999-99-99') + "'"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "CONVERT(DateTime, '" + DToC(dMyDate) + "', 103)"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = dMyDate

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cSelec = "#" + DToC(dMyDate) + "#"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener una sentencia SQL para recuperar un campo fecha como char.
* Preparado, por el momento para ORACLE y AS/400.
****************************************************************************
* Añadir entorno ACCESS. AVC - 13.02.2003
* Añadir formato fechas variable. AVC - 21.03.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarvar -----> Expresión de fecha a convertir.
*           cDFormat ----> Formato fecha.
*                      0 : 'DD-MM-YYYY' (Por defecto).
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtToChar
Parameters cEntorn, cVersion, dVarVar, cDFormat
Private cEntornL, cSelec
Local Inx, lEstado

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

If Type('cDFormat') # 'C'
   cDFormat = '0'
EndIf

Store .T. To lEstado
Store Space(1) To cSelec, _LxErr

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "TO_CHAR('" + cVarVar + "', " + cDFormat + ")"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "CAST(" + cVarvar + " AS CHAR)"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "CONVERT(CHAR, " + cVarvar + ", 103)"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = cVarVar

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cSelec = "FORMAT(" + cVarvar + ", " + cDFormat + ")"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener una sentencia SQL para recuperar un campo char como number.
* Preparado, por el momento para ORACLE y AS/400.
*
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarvar -----> Nombre de la variable.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtToNumber
Parameters cEntorn, cVersion, cVarVar
Private cEntornL, cSelec
Local Inx, lEstado

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To cSelec, _LxErr

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "TO_NUMBER(" + cVarVar + ")"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "CAST(" + cVarvar + " AS NUMBER)"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "CONVERT(DECIMAL, " + cVarvar + ")"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = "VAL(" + cVarVar + ")"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener una sentencia SQL parcial cuando está involucrado un campo NULL.
* Agregar parámetro 'cValor'. AVC - 12.09.21002
****************************************************************************
* Añadir entorno ACCESS. AVC - 03.02.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarVar -----> Expresión de NULL a validar.
*           cValor ------> Valor a devolver si NULL (opcional).
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtNvl
Parameters cEntorn, cVersion, cVarVar, cValor
Private cEntornL, cSelec
Local Inx, lStado, nLen, cValorL

If Type('cValor')=='U' .Or. Type('cValor')=='L'
   cValor = 0
EndIf

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store Space(1) To cSelec, _LxErr

Do Case
   Case Type('cValor')=='C'
      nLen = Len(cValor)
      cValorL = "'" + cValor + "'"
   Case Type('cValor')=='N'
      nLen = Len(AllTrim(Str(cValor)))
      cValorL = Str(cValor, nLen)
   Case Type('cValor')=='D'
      cValorL = CToD(cValor)
EndCase

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "NVL(" + cVarVar + ", " + cValorL + ")"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = cVarVar

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "ISNULL(" + cVarVar + ", " + cValorL + ")"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = cVarVar

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cSelec = "IIF(ISNULL(" + cVarVar + "), " + cValorL + ", " + cVarVar + ")"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener el carácter de concatenación de sentencias SQL.
****************************************************************************
* Añadir entorno SQLSERVER. AVC - 11.09.2002
* Añadir entorno VFP. AVC - 14.09.2002
* Añadir entorno ACCESS. AVC - 03.02.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtCat
Parameters cEntorn, cVersion
Private cEntornL, cSelec
Local Inx, lStado

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store Space(1) To cSelec, _LxErr

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "||"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "||"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "+ "

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = "+ "

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cSelec = "+ "

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener expresión de claves numéricas de sentencias SQL.
****************************************************************************
* Añadir entorno ACCESS. AVC - 03.02.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarvar -----> Nombre de la variable.
*           nLength -----> Posiciones enteras.
*           nDecimals ---> Posiciones decimales.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtStr
Parameters cEntorn, cVersion, cVarvar, nLength, nDecimales
Private cEntornL, cSelec
Local cMask, Inx, lStado

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store Space(1) To cSelec, _LxErr

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cMask = Replicate('9', nLength) + Iif(nDecimales > 0, '.' + Replicate('9', nDecimales), '')
      cSelec = "TO_CHAR(" + cVarvar + ", '" + cMask + "')"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "CAST(" + Varvar + " AS CHAR)"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "CONVERT(CHAR(" + AllTrim(Str(nLength)) + "), " + Varvar + ")"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = "STR(" + cVarvar + ", " + Str(nLength) + "), " + Str(nDecimales) + ")"

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cMask = Replicate('#', nLength) + Iif(nDecimales > 0, '.' + Replicate('9', nDecimales), '')
      cSelec = "FORMAT(" + cVarvar + ", '" + cMask + "')"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Obtener una sentencia SQL parcial para convertir SubStrings.
* Preparado, por el momento para ORACLE y AS/400.
****************************************************************************
* Añadir entorno ACCESS. AVC - 03.02.2003
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarVar -----> Expresión de substring a utilizar.
*           nStart ------> Posición inicial.
*           nLength -----> Longitud ó posición final, según el caso.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetCvtSubStr
Parameters cEntorn, cVersion, cVarVar, nStart, nLength
Private cEntornL, cSelec
Local Inx, lEstado

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To cSelec, _LxErr

cEntornL = GetEntornID(cEntorn, cVersion)

Do Case
   *> Sentencia para entorno ORACLE.
   Case cEntornL=='O'
      cSelec = "SUBSTR(" + cVarVar + ", " + Str(nStart) + ", " + Str(nLength) + ")"

   *> Sentencia para entorno AS/400.
   Case cEntornL=='A'
      cSelec = "SUBSTR(" + cVarVar + ", " + Str(nStart) + ", " + Str(nLength) + ")"

   *> Sentencia para entorno SQLSERVER.
   Case cEntornL=='S'
      cSelec = "SUBSTRING(" + cVarVar + ", " + Str(nStart) + ", " + Str(nLength) + ")"

   *> Sentencia para entorno VFP.
   Case cEntornL=='V'
      cSelec = "SUBSTR(" + cVarVar + ", " + Str(nStart) + ", " + Str(nLength) + ")"

   *> Sentencia para entorno ACCESS.
   Case cEntornL=='M'
      cSelec = "MID(" + cVarVar + ", " + Str(nStart) + ", " + Str(nLength) + ")"

   *> Resto de casos: Error.
   Otherwise
      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
EndCase

Return cSelec

****************************************************************************
* Convertir / substituir caracteres ilegales en sentencias SQL.
* Trabaja de forma general para todas las BBDD.
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarVar -----> Sentencia SQL a convertir.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetReplaceIllegalChar
Parameters cEntorn, cVersion, cVarVar
Private cEntornL
Local lEstado, cSelec, nInx, cCar, cCar1, cCar2

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To _LxErr
Store "" To cSelec

cEntornL = GetEntornID(cEntorn, cVersion)

For nInx = 1 To Len(cVarVar)
	cCar = SubStr(cVarVar, nInx, 1)
	cCar1= SubStr(cVarVar, nInx + 1, 1)
	cCar2= SubStr(cVarVar, nInx - 1, 1)

	If cCar==_cm
		Do Case
			Case cCar2=='=' .Or. ;
				 cCar2=='<' .Or. ;
				 cCar2=='>'
				cSelec = cSelec + cCar

			Case cCar1==','
				cSelec = cSelec + cCar

			Case Between(cCar1, 'A', 'Z') .Or. ;
	 			 Between(cCar1, 'a', 'z') .Or. ;
	 			 Between(cCar1, '0', '9')
				cSelec = cSelec + cCar + _cm

			*> Es un carácter. Se pasa tal cual.
			Otherwise
				cSelec = cSelec + cCar
		EndCase
	Else
		cSelec = cSelec + cCar
	EndIf
EndFor

*!*	Do Case
*!*	   *> Sentencia para entorno ORACLE.
*!*	   Case cEntornL=='O'

*!*	   *> Sentencia para entorno AS/400.
*!*	   Case cEntornL=='A'

*!*	   *> Sentencia para entorno SQLSERVER.
*!*	   Case cEntornL=='S'

*!*	   *> Sentencia para entorno VFP.
*!*	   Case cEntornL=='V'

*!*	   *> Sentencia para entorno ACCESS.
*!*	   Case cEntornL=='M'

*!*	   *> Resto de casos: Error.
*!*	   Otherwise
*!*	      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
*!*	EndCase

Return cSelec

****************************************************************************
* Convertir / substituir caracteres ilegales en cadenas.
* Trabaja de forma general para todas las BBDD.
****************************************************************************
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cVarVar -----> Cadena a convertir.
*
* Devuelve: cValor ------> Cadena convertida, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
****************************************************************************
Function GetReplaceChar
Parameters cEntorn, cVersion, cVarVar
Private cEntornL
Local lEstado

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To _LxErr

cEntornL = GetEntornID(cEntorn, cVersion)

*!*	Do Case
*!*	   *> Sentencia para entorno ORACLE.
*!*	   Case cEntornL=='O'

*!*	   *> Sentencia para entorno AS/400.
*!*	   Case cEntornL=='A'

*!*	   *> Sentencia para entorno SQLSERVER.
*!*	   Case cEntornL=='S'

*!*	   *> Sentencia para entorno VFP.
*!*	   Case cEntornL=='V'

*!*	   *> Sentencia para entorno ACCESS.
*!*	   Case cEntornL=='M'

*!*	   *> Resto de casos: Error.
*!*	   Otherwise
*!*	      _LxErr = 'Entorno de base de datos no soportado por el programa' + cr
*!*	EndCase

Return StrTran(cVarVar, "'", "''")

**********************************************************************
* Versión MACRO de GetCvtDate.
**********************************************************************
Function _GCD
Parameters dVarVar

Return GetCvtDate(_ENTORNO, _VERSION, dVarVar)

**********************************************************************
* Versión MACRO de GetCvtTime.
**********************************************************************
Function _GCT
Parameters dVarVar

Return GetCvtTime(_ENTORNO, _VERSION, dVarVar)

**********************************************************************
* Versión MACRO de GetCvtToChar.
**********************************************************************
Function _GCTC
Parameters cVarVar

Return GetCvtToChar(_ENTORNO, _VERSION, cVarVar)

**********************************************************************
* Versión MACRO de GetCvtToNumber.
**********************************************************************
Function _GCTN
Parameters cVarVar

Return GetCvtToNumber(_ENTORNO, _VERSION, cVarVar)

**********************************************************************
* Versión MACRO de GetCvtNvl.
**********************************************************************
Function _GCN
Parameters cVarVar, cValor

Return GetCvtNvl(_ENTORNO, _VERSION, cVarVar, cValor)

**********************************************************************
* Versión MACRO de GetCvtCat.
**********************************************************************
Function _GCC

Return GetCvtCat(_ENTORNO, _VERSION)

**********************************************************************
* Versión MACRO de GetCvtStr.
**********************************************************************
Function _GCS
Parameters cVarvar, nLength, nDecimales

Return GetCvtStr(_ENTORNO, _VERSION, cVarvar, nLength, nDecimales)

**********************************************************************
* Versión MACRO de GetCvtSubStr.
**********************************************************************
Function _GCSS
Parameters cVarvar, nStart, nLength

Return GetCvtSubStr(_ENTORNO, _VERSION, cVarvar, nStart, nLength)

**********************************************************************
* Versión MACRO de GetReplaceIllegalChar.
**********************************************************************
Function _GRCI
Parameters cVarvar

Return GetReplaceIllegalChar(_ENTORNO, _VERSION, cVarvar)

**********************************************************************
* Versión MACRO de GetReplaceChar.
**********************************************************************
Function _GRC
Parameters cVarvar

Return GetReplaceChar(_ENTORNO, _VERSION, cVarvar)

**********************************************************************
* Obtener una sentencia SQL a partir de la tabla SYSQUERY.
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cProgram ----> Identificador de programa.
*           cModul ------> Identificador de módulo.
*
* Devuelve: cSelec ------> Sentencia SQL, si OK, '' si no OK.
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
**********************************************************************
Function GetFullStringQry
Parameters cEntorn, cVersion, cProgram, cModul, cSelec
Local Inx, lStado, nOldMemo

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(1) To cSelec, _LxErr

If !Used('SYSQUERY')
   Select 108
   Use SYSQUERY Order 1
EndIf

Select SYSQUERY
Set Order To 1
Seek cEntornL + PadR(cProgram, 12) + PadR(cModul, 12)
If !Found()
   _LxErr = 'No existe la sentencia solicitada en SYSQUERY' + cr
   Store .F. To lEstado
   Return lEstado
Endif

*> Concatenar las líneas que componen la sentencia SQL.
nOldMemo = Set('MEMOWIDTH')
Set MemoWidth To 256
For Inx = 1 To MemLines(cSelect)
   cSelec = cSelec + AllTrim(MLine(cSelect, Inx))
EndFor

Set MemoWidth To nOldMemo

Return lStado

*****************************************************************************
* Obtener los parámetros de una sentencia SQL a partir de la tabla SYSQUERY.
* Devuelve los parámetros para la llamada a f3_sql().
*
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*           cProgram ----> Identificador de programa.
*           cModul ------> Identificador de módulo.
*
* Devuelve: cSelec ------> Sección SELECT de la sentencia SQL.
*           cFrom -------> Sección FROM de la sentencia SQL.
*           cWhere ------> Sección WHERE de la sentencia SQL.
*           cOrder ------> Sección ORDER de la sentencia SQL.
*           cGroup ------> Sección GROUP de la sentencia SQL.
*
*           _LxErr ------> Texto incidencia, si no OK.
*           lEstado -----> Resultado .T./.F.
*****************************************************************************
Function GetStringQry
Parameters cEntorn, cVersion, cProgram, cModul, cSelec, cFrom, cWhere, cOrder, cGroup
Local cEntornL
Local Inx, lStado, nOldMemo, cCond

If Type('_LxErr') # 'C'
   Public _LxErr
EndIf

Store .T. To lEstado
Store Space(0) To cSelec, cFrom, cWhere, cOrder, cGroup, cAuxCond, _LxErr

If !Used('SYSQUERY')
   Select 108
   Use SYSQUERY Order 1
EndIf

cEntornL = GetEntornID(cEntorn, cVersion)

Select SYSQUERY
Set Order To 1
Seek cEntornL + PadR(cProgram, 12) + PadR(cModul, 12)
If !Found()
   _LxErr = 'No existe la sentencia solicitada en SYSQUERY' + cr + ;
            'PROGRAMA: ' + cProgram + ' MODULO: ' + cModul + cr
   Store .F. To lEstado
   Return lEstado
Endif

nOldMemo = Set('MEMOWIDTH')
Set MemoWidth To 512

*> Generar sección SELECT de la cláusula SQL.
For Inx = 1 To MemLines(cSelecT)
   cSelec = cSelec + AllTrim(MLine(cSelecT, Inx))
EndFor

*> Generar sección FROM de la cláusula SQL.
For Inx = 1 To MemLines(cFromT)
   cFrom = cFrom + AllTrim(MLine(cFromT, Inx))
EndFor

*> Generar sección WHERE de la cláusula SQL.
For Inx = 1 To MemLines(cWhereT)
   cWhere = cWhere + AllTrim(MLine(cWhereT, Inx))
EndFor

*> Generar sección ORDER de la cláusula SQL.
For Inx = 1 To MemLines(cOrderT)
   cOrder = cOrder + AllTrim(MLine(cOrderT, Inx))
EndFor
	
*> Generar sección GROUP de la cláusula SQL.
For Inx = 1 To MemLines(cGroupT)
   cGroup = cGroup + AllTrim(MLine(cGroupT, Inx))
EndFor

*> Condiciones especiales de la cláusula.
For Inx = 1 To MemLines(AuxCondT)
   cAuxCond = cAuxCond + AllTrim(MLine(AuxCondT, Inx))
EndFor
	
*> Cláusulas where condicionales, que sustituyen a las generales.
For Inx = 1 to MemLines(AuxCondT)
   cCond = MLine(AuxCondT, Inx)
   If !Empty(cCond)
      cWhere = &cCond
   EndIf
EndFor

*> Substitución de macros.
cSelec = ReplaceMacrosQry(@cSelec)
cFrom  = ReplaceMacrosQry(@cFrom)
cWhere = ReplaceMacrosQry(@cWhere)
cOrder = ReplaceMacrosQry(@cOrder)
cGroup = ReplaceMacrosQry(@cGroup)

Set MemoWidth To nOldMemo
Return lEstado

***************************************************************************************
* Realización de macros sobre sentencias SQL generadas por GetStringQuery().
* Estas macros deben de estar dentro del par de caracteres '[' y ']'.
*
* Utilizado también por el tratamiento de ayudas. AVC - 25.10.2002
*   Recibe: cStringIn -----> Sentencia que contiene las macros.
* 
* Devuelve: cStringOut ----> Sentencia SQL con las macros sustituídas por sus valores.
***************************************************************************************
Function ReplaceMacrosQry
Parameters cStringIn
Private cStringOut
Local nd1, nd2, cString1, cString2, cString3

cStringOut = cStringIn

nd1 = At("[", cStringOut)
Do While nd1 > 0
   nd2 = At("]", cStringOut)
   If nd2 = 0
      Return cStringOut
   EndIf

  cString1 = SubStr(cStringOut, nd1 + 1,(nd2 - 1) - nd1)
  cString2 = SubStr(cStringOut, nd1, (nd2 - nd1) + 1)
  cStringOut = StrTran(cStringOut, cString2, &cString1)
  nd1 = At("[", cStringOut)
EndDo

*> Ejecutar la macro. Tener en cuenta que VFP no permite variables de mas de 254c.
*> Procesar los substrings separados por '@@'.
*> Tener en cuenta que, una vez expandida la macro, el tamano del substring
*> puede llegar a ser mayor de 254 caracteres.

cString2 = cStringOut
cStringOut = ""

Do While Len(cString2) > 0
   nd1 = At("@@", cString2)
   nd1 = Iif(nd1==0, Len(cString2) + 1, nd1)
   cString1 = Left(cString2, nd1 - 1)
   cString2 = SubStr(cString2, nd1 + 2)
   cStringOut = cStringOut + &cString1

   *> Procesar macros largas, que no pueden ser ejecutadas.
   *nd1 = Ceiling(Len(cString1) / 200)
   *For nd2 = 0 To nd1 - 1
   *   cString3 = SubStr(cString1, (nd2 * 200) + 1, 200)
   *   cStringOut = cStringOut + &cString3
   *EndFor
EndDo

Return cStringOut

****************************************************************************
* Obtener un identificador, según el entorno de base de datos.
****************************************************************************
* Agregar MS Access. AVC - 03.02.2003
****************************************************************************
* Utilizado por:
*      - St3Rt.GetCvtNvl().
*      - St3Rt.GetCvtDate().
*      - Ora_Proc.Ora_NewNum().
*
*   Recibe: cEntorn -----> Entorno DB (ORACLE, DB2, SQLSERVER, ...).
*           cVersion ----> No utilizado. Para futuras ampliaciones.
*
* Devuelve: cEntornID ---> Identificador de entorno.
*           _LxErr ------> Texto incidencia, si no OK.
****************************************************************************
Function GetEntornID
Parameters cEntorn, cVersion
Private cEntornID

*> Agrupar por entorno de base de datos.
Do Case
   Case cEntorn = 'ORACLE'
      cEntornID = 'O'
   Case cEntorn = 'DB2'
      cEntornID = 'A'
   Case cEntorn = 'SQLSERVER'
      cEntornID = 'S'
   Case cEntorn = 'VFP'
      cEntornID = 'V'
   Case cEntorn = 'INFORMIX'
      cEntornID = 'O'
   Case cEntorn = 'MYSQL'
      cEntornID = 'O'
   Case cEntorn = 'DBF'
      cEntornID = 'O'
   Case cEntorn = 'ACCESS'
      cEntornID = 'M'

   *> Por defecto, tratamiento para ORACLE.
   Otherwise
      cEntornID = 'O'
EndCase

Return cEntornID
