* Buscar una descripci�n de art�culo/Familia
*
Function ls3art
Parameter artart,campo
*
ali=alias()
select ARTI
seek artart
lx=iif(type('campo')='C',&campo,'')
select alias(ali)
*
return lx