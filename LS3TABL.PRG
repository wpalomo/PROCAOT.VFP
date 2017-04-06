* Función de listado texto tabla
*
Function ls3tabl
Parameter lx1,lx2
*
_ali=alias()
lx1=left(lx1+space(10),10)
select TABL
seek lx1+lx2
lx2=iif(eof(),'',TAB_DES)
select alias (_ali)
*
return lx2
