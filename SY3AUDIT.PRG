* Función para conectar con fichero de auditoría
*
Function SY3AUDIT
Parameter audi_accion
*
_ali=alias()
select SYSPRG
r1prg=recno()
seek 'SY3AUDI'
m.USR_COD=_usrcod
m.USR_FEC=date()
m.USR_HOR=time()
m.USR_ACC=audi_accion
do form pntw\sy3audi
lx=_fich_aud+'\AUDITOR'
use (lx) order tag codigo in 0
insert into AUDITOR from memvar
select AUDITOR
use
select SYSPRG
go r1prg
select alias(_ali)
*
return