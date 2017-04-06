* Control del número de usuarios conectados a un programa
*                 ST3NUSR
*
dimension usr_or(100),usr_ds(100)
store '' to usr_or,usr_ds
select SYSLOG
ntot=0
go top
do while !eof() .and. empty(LOG_FECF) .and. ntot<99
  ntot=ntot+1
  lx1=str(day(LOG_FECI),2)+'/'+str(month(LOG_FECI),2)+'/'+str(year(LOG_FECI),4)
  lx2=str(hour(LOG_FECI),2)+':'+str(minute(LOG_FECI),2)
  usr_or(ntot)=LOG_USR+'  >> ['+lx1+']['+lx2+'] <<'+str(recno())
  skip
enddo
=st3trasp('Control de Usuarios','usr_or',ntot,'usr_ds',ntot,'','','','','','','Usuarios conectados','Usuarios a desconectar')
total=ntot
for nd=1 to ntot
  if !empty(usr_ds(nd))
    nd1=val(right(usr_ds(nd),10))
    go nd1
    repl LOG_FECF with datetime()
  endif  
endfor
select SYSLOG
count all for empty(LOG_FECF) to total
if total>=_n_usr
  do form st3sn with 1,1,'Hay demasiados usuarios conectados a la red','El programa se cancelará'
  close all
  quit
endif
if _r1log>0
  select SYSLOG
  go _r1log
endif
select SYSPRG
*
return