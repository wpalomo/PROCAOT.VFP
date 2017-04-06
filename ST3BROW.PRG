* Programa Edición Standard (BROWSE)
*         ST3BROW
*
*
* Arrays 	lsql	Array donde se guardan las consultas o ediciones
*
if at(_cse,_usrmaxp)=0
  do form st3sn with 1,1,'Lo siento no tiene prioridad para ejecutar esta opción'
  return
endif
select SYSFC
set rela to FC_FC into SYSPRG
count all for !deleted() .and. left(FC_FC,2)<>'XL' .and. eof('SYSPRG') to _maxfc
dimension _lsql(_maxfc),cmpidx(15)
store '' to _lsql,fc_br
go top
store 0 to fc_idx,m.BOT_MODI,nd
do while !eof()
  if left(FC_FC,2)<>'XL' .and. eof('SYSPRG')
    lx=FC_KEY
    if at('CODIGO ',lx)>0
      fcfc=iif(empty(FC_FCS),trim(FC_FC),trim(FC_FCS))
      fcfc=iif(len(fcfc)>4,fcfc,fcfc+_em)
      fcfc1=iif(empty(_empfich),'',_empfich+'\'+fcfc)
      fcfc=_fich+'\'+fcfc
      if !empty(fcfc1) .and. file(fcfc1+'.DBF')
        fcfc=fcfc1
      endif
      if file(fcfc+'.DBF')
        nd=nd+1
        if _xidiom>1
          _lsql(nd)=FC_FC+' '+f3_t(FC_NOM)
        else
          _lsql(nd)=FC_FC+' '+FC_NOM
        endif
      endif
    endif
  endif
  skip
enddo
set rela to
on key
m.ls_graf=1
do form st3brow
select 1
use
select SYSPRG
*
return
