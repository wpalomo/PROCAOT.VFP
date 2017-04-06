* Buscar una descripción de artículo/Familia
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