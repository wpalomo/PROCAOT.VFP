* Programa de ayudas 
*     ST3HLP
*
*	Array _xbusca	1		Nombre del campo con alias (ayuda.<campo>)
*					2		Longitud
*					3		Descripci�n campo
*					4		Tipo
*					5,6,7,8	Filtros establecidos
*

Private _hlpok, _xbusca
store .F. to _controlc,_hlpok
if type('_ayuda_especial')<>'L'
  _ayuda_especial=.F.
endif

_nvarh=upper(_nvarh)
if empty(_nvarh)
  _mensaje_hlp=.T.
  _ayuda_especial=.F.
  return
endif

_alihlp=alias()
If Empty(_alihlp)
   Select SYSPRG
   _alihlp=alias()
EndIf
if !used('SYSHL')
  use SYSHL order tag SYSHL in 0
endif

select SYSHL
_winhlp=left(wontop()+space(10),10)
_nvarh=left(_nvarh+space(10),10)
seek _winhlp+_nvarh
if eof()
  seek space(10)+_nvarh
endif
_nvarh=trim(_nvarh)
do case
case eof()
  select SYSVAR
  seek _nvarh
  if eof() .or. empty(HL_FC) .or. atc('/'+trim(HL_FC)+']',_xopenfc)=0
    select (_alihlp)
    _controlc=.F.
    _mensaje_hlp=.T.
    _ayuda_especial=.F.
    return
  else
    _xfxa=HL_FC
    select SYSHL
    seek space(10)+_xfxa
     _xfxa=trim(_xfxa)
  endif
case atc('/'+trim(FC_FC)+']',_xopenfc)=0
  select (_alihlp)
  _controlc=.F.
  _mensaje_hlp=.T.
  _ayuda_especial=.F.
  return
otherwise
  _xfxa=trim(FC_FC)
endcase
if _xfc=_xfxa .and. _lockh=1
  do form st3sn with 1,2,'No se puede dar ayuda si el registro est� bloqueado'
  _mensaje_hlp=.T.
  _ayuda_especial=.F.
  return
endif
_prghlp='V'+trim(_nvarh)
_prghlp=chrtran(_prghlp,'_','')

if file(_prghlp+'.PRG') .or. file(_prghlp+'.FXP')
  _prghlpx='='+_prghlp+'("'+_winhlp+'","INICIO")'
  &_prghlpx
endif

store '' to _lxsele,_lxfrom,_lxwhere,_lxbus,_lxord,_lxgrup,_replaces,_hlpriv,_lxequi
if eof()                                 && Definici�n directa
  _lxfrom=trim(_xfxa)
  select (_xfxa)
  num=fcount()
  for nd=1 to num
    if type(field(nd))='N' .or. type(field(nd))='C' .or. type(field(nd))='D'
      _lxsele=_lxsele+','+field(nd)
    endif
  endfor
  _lxsele=subs(_lxsele,2)
else                                         && Definici�n personalizada
  for nd=1 to memlines(syshl.HL_EXPR)
    lx=mline(syshl.HL_EXPR,nd)
    lx=chrtran(lx,"'",'"')
    do case
    case empty(lx)
    case left(lx,1)='['
      lx1=lx
    case lx1='[Selecciona]'
      _lxsele=_lxsele+lx
    case lx1='[Fichero]'
      _lxfrom=_lxfrom+lx
    case lx1='[Cuando]'
      _lxwhere=_lxwhere+lx
    case lx1='[Agrupado]'
      _lxgrup=_lxgrup+lx
    case lx1='[Ordenado]'
      _lxord=_lxord+lx
    case lx1='[Reemplazando]'
      _replaces=_replaces+lx
    case lx1='[Privadas]'
      _hlpriv=_hlpriv+lx
    case lx1='[Equivalencias]'
      _lxequi=_lxequi+lx
    endcase
  endfor
endif

if !empty(_hlpriv)
  _hlpriv=_hlpriv+',  '
  nd=at(',',_hlpriv)
  do while nd>0
    lx=left(_hlpriv,nd-1)
    _hlpriv=subs(_hlpriv,nd+1)
    nd=at('*',lx)
    if nd=0
      private all like &lx
    else
      private all like &lx
    endif
    nd=at(',',_hlpriv)
  enddo
endif

select alias(_xfxa)

_lx_from = ''
nd=at(',',_lxfrom)
do while nd>0
  lx=alltrim(left(_lxfrom,nd-1))
  if len(lx) = 4
     lx = lx + _em
  endif
  _lx_from = Iif(empty(_lx_from), lx, _lx_from + ',' + lx)
  _lxfrom=subs(_lxfrom,nd+1)
  nd=at(',',_lxfrom)
enddo

if !Empty(_lxfrom)
  lx=alltrim(_lxfrom)
  if len(lx) = 4
     lx = lx + _em
  endif
  _lx_from = Iif(empty(_lx_from), lx, _lx_from + ',' + lx)
endif
_lxfrom = _lx_from
_lxfrom = AllTrim(_lxfrom)

tipo=type('_valorh')
do case
case tipo='C' .and. !empty(_valorh)
  _valorhc=_valorh
  _valorh=trim(_valorh)
  do case
  case left(_valorh,1)='*'
    lx='upper('+_nvarh+') like upper('+_cm+'%'+subs(_valorh,2)+'%'+_cm+')'
    _lxbus='Contenga ['+subs(_valorh,2)+']'
  case right(_valorh,1)='*'
    lx='upper('+_nvarh+') like upper('+_cm+left(_valorh,len(_valorh)-1)+'%'+_cm+')'
    _lxbus='Empiece por ['+_valorh+']'
  otherwise
    lx=_nvarh+' = '+_cm+subs(_valorh,2)+_cm
    _lxbus='Sea igual a ['+_valorhc+']'
  endcase
  _lxwhere=iif(empty(_lxwhere),lx,'('+_lxwhere+') and '+lx)
case tipo='D' .and. !empty(_valorh)
  _lxbus='A partir de ['+dtoc(_valorh)+']'
  if empty(_lxwhere)
    _lxwhere=trim(_nvarh)+'>="'+dtoc(_valorh)+'"'
  else
    _lxwhere='('+_lxwhere+').and.'+trim(_nvarh)+'>="'+dtoc(_valorh)+'"'
  endif
case tipo='N' .and. _valorh<>0
  _lxbus='A partir de ['+str(_valorh)+']'
  if empty(_lxwhere)
    _lxwhere=trim(_nvarh)+'>="'+str(_valorh)+'"'
  else
    _lxwhere='('+_lxwhere+').and.'+trim(_nvarh)+'>="'+str(_valorh)+'"'
  endif
endcase

if empty(_lxord)
  nd=at(',',_lxsele)
  _lxord=iif(nd>0,left(_lxsele,nd-1),_lxsele)
endif

do while !empty(_lxequi)
  ndeq=at(',',_lxequi)
  if ndeq=0
    lx=_lxequi
    _lxequi=''
  else
    lx=left(_lxequi,ndeq-1)
    _lxequi=iif(ndeq=len(_lxequi),'',subs(_lxequi,ndeq+1))
  endif
  ndeq=at('=',lx)
  if ndeq>0 .and. len(lx)>ndeq
    lx1=alltrim(left(lx,ndeq-1))
    lx=alltrim(subs(lx,ndeq+1))
    ndeq=atc(lx1,_lxwhere)
    if ndeq>0
      _lxwhere=stuff(_lxwhere,ndeq,len(lx1),lx)
    endif
  endif
enddo
* ---------------- Efectuo la consulta
_lxsql='select &_lxsele from &_lxfrom '

nd1 = At("[", _lxwhere)
Do While nd1 > 0
   nd2 = At("]", _lxwhere)
   If nd2 > 0
      cString1 = SubStr(_lxwhere, nd1 + 1,(nd2 - 1) - nd1)
      cString2 = SubStr(_lxwhere, nd1, (nd2 - nd1) + 1)
      _lxwhere = StrTran(_lxwhere, cString2, &cString1)
      nd1 = At("[", _lxwhere)
   EndIf
EndDo

if !empty(_lxwhere)
   nd=at('=',_lxwhere)
   if nd>0
      nd1 = at('.',_lxwhere)
      if nd1 > 0
         na=at(')',Substr(_lxwhere, nd))
         na=Iif(na==0, at(' ',Substr(_lxwhere, nd)), na)
         if na=0
            na=len(_lxwhere) - nd + 2
         endif

         nn=substr(_lxwhere, nd+1, na-2)
         _lxwhere = strtran(_lxwhere, nn, "'" + &nn + "'")
      endif
   endif

  _lxsql=_lxsql+'where '+_lxwhere+' '
endif

if !empty(_lxgrup)
  _lxsql=_lxsql+'group by &_lxgrup '
endif
if !empty(_lxord)
   _lxsql=_lxsql+'order by &_lxord '
endif

_lxsql = StrTran(_lxsql, '"', "'")

nd=sqlexec(_asql,_lxsql,'AYUDA')
if nd<0
  _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
  _LxErr = _LxErr + 'SENTENCIA: ' + _lxsql + cr
  =Anomalias()

  select (_alihlp)
  _mensaje_hlp=.T.
  _ayuda_especial=.F.
  return
else
  =sqlmoreresults(_asql)
endIf

if file(_prghlp+'.PRG') .or. file(_prghlp+'.FXP')
  _prghlpx='='+_prghlp+'("'+_winhlp+'","LECTURA")'
  &_prghlpx
  select AYUDA
endif

do case
case reccount()=0
  _ndh=2
  do while !empty(prog(_ndh))
    lx1=prog(_ndh-1)
    _ndh=_ndh+1
  enddo
  if _mensaje_hlp=.T.
    _LxErr = 'No se ha encontrado ning�n registro que cumpla las condiciones establecidas' + cr
    Do Form St3Inc
  endif
*case reccount()==1 .and. _ayuda_especial=.F.
*  go top
*  scatter memvar memo
*  activate window wnada
*  deactivate window wnada
*  _hlpok=.T.
OtherWise                         &&  case reccount() >= 1
  _xbnum=fcount()
  dimension _xbusca(_xbnum,8),_cnd1(8)
  _cnd1(1)='  '
  _cnd1(2)='>='
  _cnd1(3)='<='
  _cnd1(4)='= '
  _cnd1(5)='<>'
  _cnd1(6)='**'
  _cnd1(7)='> '
  _cnd1(8)='< '
  _cerrarvar=.F.
  if !used('SYSVAR')
    use SYSVAR order tag idx1 in 0
  endif
  select AYUDA
  for nd=1 to _xbnum
    _xbusca(nd,4)='C'
    nvar=field(nd)
    select SYSVAR
    seek nvar
    if !eof()
      m.VAR_DES=VAR_DES
      do case
      case left(VAR_FMT,1)='9'
        lx2=trim(VAR_FMT)
        nd2=len(lx2)*6.5+7
        _xbusca(nd,4)='N'
      case left(VAR_FMT,1)='r'
        lx2=VAR_FMT
        lx2=&lx2
        nd2=len(lx2)*6.5+7
      case left(VAR_FMT,1)='D'
        nd2=65
        _xbusca(nd,4)='D'
      endcase
    else
      nd2=140
      m.VAR_DES=nvar 
    endif
    select AYUDA
    _xbusca(nd,1)='ayuda.'+trim(nvar)
    _xbusca(nd,2)=nd2
    _xbusca(nd,3)=VAR_DES
    if _xidiom>1
      _xbusca(nd,3)=f3_t(VAR_DES)
    endif
    store '' to _xbusca(nd,5),_xbusca(nd,6),_xbusca(nd,7),_xbusca(nd,8)
  endfor

  store '' to _hlp_filtro,_hlp_car
  do form st3hlp
  if _hlpok
    select AYUDA
    scatter memvar memo
  endif 
endcase

if _hlpok=.T. .and. !empty(_replaces)
  _replaces = "&_replaces"
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

if file(_prghlp+'.PRG') .or. file(_prghlp+'.FXP')
  _prghlpx='='+_prghlp+'("'+_winhlp+'","FINAL")'
  &_prghlpx
endif

select AYUDA
use
_controlc=.F.
select (_alihlp)
_mensaje_hlp=.T.
_ayuda_especial=.F.

*
return _hlpok
