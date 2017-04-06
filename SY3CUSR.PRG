* Programa control de Usuarios
*               SY3CUSR
*
set memowidth to 80
use SYSUSER in 0 order tag idx1
select SYSUSER
use SYSLOG/SYSHIST in 0 excl
select SYSHIST
zap
select SYSUSER
count all for !deleted().and. !empty(USR_COD) to _numusr
dimension _usuarios(_numusr,2)
go top
_numusr=0
do while !eof()
  if !empty(USR_COD)
    _numusr=_numusr+1
    _usuarios(_numusr,1)=USR_COD
    _usuarios(_numusr,2)=USR_NOM
  endif
  skip
enddo
if _numusr>0
  fec_cusr=date()
  codigo_usr=1
  do form sy3cusr
endif
select SYSUSER
use 
select SYSHIST
zap
use
select 107
use (_logfc) order tag idx1 alias 'SYSLOG'
select SYSPRG
*
return 