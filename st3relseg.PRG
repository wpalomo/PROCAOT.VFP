* FUNCION ENCONTRAR UN LITERAL PARA LISTADO
*

use xlrecl in 0 alias xlrecl
select xlrecl

go top
do while !Eof()
   a = st3rel('F08C','[F08cCodPro=XLRecL.F50lCodPrt,F08cCodArt=XLRecL.F50lCodArt]','F08cDescri')

set step on
   
enddo

FUNCTION ST3REL
Parameter _fichero,_clave,_retorno,_indice
*
set step on

_ali=Alias()
scatter memvar
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

if !empty(_ali)
   select alias (_ali)
endif

*
Return lx1
