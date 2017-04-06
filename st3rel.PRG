* FUNCION ENCONTRAR UN LITERAL PARA LISTADO (UN FICHERO)
*
FUNCTION ST3REL
Parameter _fichero,_clave,_retorno,_indice
Local _ok, _ali, lx1

_ali=Alias()
scatter memvar
if type('_Indice')='C'
  _ok=f3_seek (_fichero,_clave,,,,_indice)
else
  _ok=f3_seek (_fichero,_clave)
endif
if _ok .and. type('_retorno')='C'
  select(_fichero)
  lx1=&_retorno
else
  lx1=''
endif

if !empty(_ali)
   select alias (_ali)
endif

*
Return lx1

* FUNCION ENCONTRAR UN LITERAL PARA LISTADO (SQL)
*
FUNCTION ST3RELSQL
Parameter _fichero, _clave, _retorno
Private __lx
Local _ok, _ali, lx1

_ali = Alias()
__lx = Sys(2015)
_ok = f3_sql ('*', _fichero, _clave, , , __lx)

if _ok .and. type('_retorno')=='C'
  select(__lx)
  lx1 = &_retorno
else
  lx1=''
endif

if !empty(_ali)
   select alias (_ali)
endif

Return lx1
