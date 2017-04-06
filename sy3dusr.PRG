* Programa para la definición de Usuarios
*         SY3DUSR
*
use SYSUSER order tag idx1 in 0
select SYSUSER
tmp_gr=_tmp+sys(2015)
select USR_GR,USR_NOM from SYSUSER where !empty(USR_GR) into dbf (tmp_gr)
use
use (tmp_gr) in 0 excl alias 'GRUPO'
select GRUPO
index on usr_gr tag idx1
tmp_usr=_tmp+sys(2015)
select USR_COD,USR_NIV,USR_NOM from SYSUSER where empty(USR_GR) into dbf (tmp_usr)
use
use (tmp_usr) in 0 excl alias 'USUARIO' 
select USUARIO
index on USR_COD tag idx1
store 1 to n_usr,n_gr
dimension _anclaje(4)
_anclaje(1)=f3_t('1 Arriba')
_anclaje(2)=f3_t('2 Izquierda')
_anclaje(3)=f3_t('3 Derecha')
_anclaje(4)=f3_t('4 Abajo')
select SYSUSER
do form sy3dusr
select GRUPO
use
tmp1=tmp_gr+'.DBF'
tmp2=tmp_gr+'.CDX'
delete file (tmp1)
delete file (tmp2)
select USUARIO
use
tmp1=tmp_usr+'.DBF'
tmp2=tmp_usr+'.CDX'
delete file (tmp1)
delete file (tmp2)
select SYSUSER
seek ' '+_usrcod
if eof()
  cancel
endif
use
select SYSPRG
*
return
