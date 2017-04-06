
*> Programa de cambio de Usuario
*>        SY3USR
*>
*> Liberar posibles recursos bloqueados por el usuario. AVC - 21.03.2003

Private _r1
Private oPRC, lIsUsed, cWhere, lStado, cCampos, cValores

if !used('SYSUSER')
  use SYSUSER order tag idx1 in 0
endif

Select SYSLOG
If _r1log > 0
  Go _r1log
EndIf

_logmem = _logmem + 'SALIDA  : ' + DToC(Date())+' ' + Time() + cr
Replace All LOG_FECF With DateTime(), ;
            LOG_MEM With _logmem ;
   For LOG_USR = _usrcod .And. ;
       LOG_USRM = Sys(0) .And. ;
       Empty(LOG_FECF)

select SYSUSER
total=0
_correcto=.T.
do form sy3usr
on key  

if _correcto=.T.
  *> Liberar recursos, si cal.
  lIsUsed = Used('F00a')

  If !lIsUsed
     oPRC = CreateObject('procaot')
     =oPRC.OpenTabla('F00a')
     Release oPRC
  EndIf

  *> Libera numeraciones bloqueadas por el usuario, por terminación anormal de sesión, p.ej.
  cWhere = "F00aUsrLck='" + _UsrCod + "'"
  cCampos = 'F00aUsrLck'
  cValores = Space(6)
  =f3_UpdTun('F00a', , cCampos, cValores, , cWhere, 'S')
  If !lIsUsed
     Use In F00a
  EndIf

  *> Guardar registro de LOG.
  Select SYSLOG
  Append Blank
  _r1log=recno()
  _logmem='ENTRADA : '+dtoc(date())+' '+time()+cr
  replace LOG_FECI with datetime(), ;
          LOG_FECF with CToT(''), ;
          LOG_USR  with _usrcod, ;
          LOG_MEM  with _logmem, ;
          LOG_USRM with sys(0)

  locate for empty(LOG_FECF) .and. LOG_USR = _usrcod .and. LOG_USRM # sys(0)
  if found()
     =f3_sn(1,1,'El usuario [' + alltrim(LOG_USRM) + '] está utilizando este código')
     select SYSLOG
     go _r1log
     _logmem=_logmem+'SALIDA  : '+dtoc(date())+' '+time()+cr
     replace LOG_FECF with datetime(),LOG_MEM with _logmem
     finalmenu = .T.
     _ok = .F.
     return
  endif

  select SYSUSER
  if !empty(USR_ICON)
    nd=val(left(USR_ICONP,1))-1
    nd=iif(betw(nd,0,3),nd,0)
    _barra_icono.dock(nd)
    _barra_icono.show
  else
    _barra_icono.hide
  endif

  _ok = .T.
  do st3opm
  if _xier<>0 .Or. !_ok
    close all
    quit
  endif
else
  select SYSMEMO
  use
  delete file (_dbfmemo)
  delete file (_fptmemo)
  set SYSMENU to default
  on error
  clear all
  close all
  quit
endif

select SYSUSER
use
select SYSPRG

_ok = .T.
return
