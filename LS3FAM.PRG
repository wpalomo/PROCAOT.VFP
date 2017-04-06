* Función de devolver el nombre de una familia
*
Function ls3fam
Parameter famfam
*
_alil=alias()
select FAMI
seek famfam
lx=iif(eof(),'',FAM_DES)
select (_alil)
*
return lx