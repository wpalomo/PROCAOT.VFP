* Programa de cambio de Password
*        ST3CC
*
if !used('SYSUSER')
  use SYSUSER order tag idx1 in 0
endif
select SYSUSER
seek ' '+_usrcod
do form st3cc
select SYSUSER
use
select SYSPRG
*
return .T.
