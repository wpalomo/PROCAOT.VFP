* FUNCION ENCONTRAR UN LITERAL PARA LISTADO
*
FUNCTION LS3LS
Parameter _fichero,_clave,_retorno,_indice
*
_ali=Alias()
if type('_Indice')='C'
  _ok=f3_seek (_fichero,_clave,,,,_indice)
else
  _ok=f3_seek (_fichero,_clave)
endif
if _ok .and. type('_retorno')='C'
  lx1=&_retorno
else
  lx1=''
endif
select alias (_ali)
*
Return lx1
