* Programa de ayudas directo sobre un fichero
*     ST3HLPFC
*
*	Array _xbusca	1		Nombre del campo con alias (ayuda.<campo>)
*					2		Longitud
*					3		Descripción campo
*					4		Tipo
*					5,6,7,8	Filtros establecidos
*
Parameter _fic
Private _hlpok

store .F. to _controlc,_hlpok
_fic=Upper(AllTrim(_fic))
if empty(_nvarh)
  _mensaje_hlp=.T.
  return
endif

_alihlp=alias()
if !used('SYSHL')
  use SYSHL order tag SYSHL in 0
endif

select SYSHL
_winhlp=left(wontop()+space(10),10)
_fic=left(_fic+space(10),10)
seek _winhlp+_fic
if eof()
  seek space(10)+_fic
endif

store '' to _lxsele,_lxfrom,_lxwhere,_lxbus,_lxord,_lxgrup,_replaces,_hlpriv
do case
case !eof() .and. atc('/'+trim(FC_FC)+']',_xopenfc)=0
  select (_alihlp)
  _controlc=.F.
  _mensaje_hlp=.T.
  return
case !eof()                                           && Hay ayuda definida
  _xfxa=trim(FC_FC)
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
    endcase
  endfor
otherwise                                             && Definición directa
  _xfxa=AllTrim(_fic)
  _lxfrom=AllTrim(_fic)
  select (_xfxa)
  num=fcount()
  for nd=1 to num
    if type(field(nd))='N' .or. type(field(nd))='C' .or. type(field(nd))='D'
      _lxsele=_lxsele+','+field(nd)
    endif
  endfor
  _lxsele=subs(_lxsele,2)
  _lxsele = '*'
endcase

if _xfc=_xfxa .and. _lockh=1
  do form st3sn with 1,2,'No se puede dar ayuda si el registro está bloqueado'
  _mensaje_hlp=.T.
  return
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

_lxfrom = AllTrim(_lxfrom)
if Len(_lxfrom) = 4
  _lxfrom=_lxfrom+_em
endif

*> Código asociado al fichero sobre el que se realiza la ayuda.
_prghlp='V'+trim(_fic)
if empty(_lxord)
  nd=at(',',_lxsele)
  _lxord=iif(nd>0,left(_lxsele,nd-1),'1')
endif

* ---------------- Efectuo la consulta
_lxsql='select &_lxsele from &_lxfrom '
if !empty(_lxwhere)
  _lxsql=_lxsql+'where '+_lxwhere+' '
endif
_lxsql=_lxsql+'order by &_lxord '
if !empty(_lxgrup)
  _lxsql=_lxsql+'group by &_lxgrup '
endif

*> Código a ejecutar ANTES de cargar la ayuda.
if file(_prghlp+'.PRG') .or. file(_prghlp+'.FXP')
  _prghlpx = "=" + _prghlp + "('" + _winhlp + "', 'INICIO')"
  &_prghlpx
endif

If !Used('AYUDA')
   nd=f3_SqlExec(_asql,_lxsql,'AYUDA')
   if nd=<0
     * _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
     Do Form St3Inc With .T.
     if left(_usrcod,5)='PENTA'
        If f3_sn(2, 1, 'Error en generación de ayuda')
           Suspend
        EndIf
     endif

     select (_alihlp)
     _mensaje_hlp=.T.
     _ayuda_especial=.F.
     return
   endif
EndIf

*> Código a ejecutar ANTES de visualizar la ayuda.
if file(_prghlp+'.PRG') .or. file(_prghlp+'.FXP')
  _prghlpx = "=" + _prghlp + "('" + _winhlp + "', 'LECTURA')"
  &_prghlpx
endif

select AYUDA

do case
case reccount()=0
  _ndh=2
  do while !empty(prog(_ndh))
    lx1=prog(_ndh-1)
    _ndh=_ndh+1
  enddo
  if _mensaje_hlp=.T.
    do form st3sn with 1,2,'No se ha encontrado ningún registro que cumpla','las condiciones establecidas'
  endif
*case reccount()=1
*  go top
*  scatter memvar memo
*  activate window wnada
*  deactivate window wnada
*  _hlpok=.T.
Otherwise
  _xbnum=fcount()
  dimension _xbusca(_xbnum,8),_cnd1(7)
  _cnd1(1)='  '
  _cnd1(2)='>='
  _cnd1(3)='<='
  _cnd1(4)='> '
  _cnd1(5)='< '
  _cnd1(6)='= '
  _cnd1(7)='<>'
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

  *> Visualizar los registros.
  do form st3hlp
  if _hlpok
    select AYUDA
    scatter memvar memo
  endif 
endcase

if _hlpok=.T. .and. !empty(_replaces)
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

*> Código a ejecutar DESPUES de visualizar la ayuda.
if file(_prghlp+'.PRG') .or. file(_prghlp+'.FXP')
  _prghlpx = "=" + _prghlp + "('" + _winhlp + "', 'FINAL')"
  &_prghlpx
endif

select AYUDA
use

_controlc=.F.
select (_alihlp)
_mensaje_hlp=.T.
*
return
