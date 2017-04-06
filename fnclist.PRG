****************************************************************
*              Funciones de búsqueda para listados             *
****************************************************************

* Fichero F02c (Almacenes), Descripción

Function Ls3F02cDes
Parameters Clave

_ali = Alias()

m.F02cCodAlm = Clave
encont=f3_seek("F02C", Clave,"lx", "lx=F02cDescri")
*lx="MANUAL - " + lx + " - " + Iif(encont,"True","False")

Select Alias (_ali)
Return lx


function tonta
 
return 'mierda'




*Booleano=f3_seek ("F03C","_alma,zona", "c_recept" , "C_recept=F03CDESCRI",,)

*_FICS: Alias del fichero en el que hay que encontrar la ficha.
*_KEYS: Valor de la clave de acceso.
*_BLANCS: Campos que hay que inicializar a blancos si no se encuentra la ficha.
*_REPLACES: Sustituciones a realizar en caso de encontrar la ficha.
*_VAR_PRIV: Variables que hay que declarar privadas para que no se modifiquen.
*_N_INDICE: Nombre del índice por el que hay que buscar (Por defecto CODIGO).




Function Ls3F03cDes
Parameters Clave
_ali=Alias()
encont=f3_seek ("F03C", Clave, , ,,'CODIGO')
*Return C_retorno
*Return clave
Return f03c.F03cDescri

* Fichero F03c (Zonas), Descripción

Function Ls3F02cDes
Parameters ClavAlm, ClavZon
_ali = Alias()

m.F03cCodAlm = ClavAlm
m.F03cCodZon = ClavZon
encont=f3_seek("F03C", ClavAlm+","+ClavZon,"lx", "lx=F03cDescri")
lx="MANUAL - " + lx + " - " + Iif(encont,"True","False")

Select Alias (_ali)
Return lx



Function Ls3F02cDes2
Parameters Clave, Abreviado
Private ValRtn

_ali = Alias()
ValRtn = ""

m.F02cCodAlm = Clave
If Abreviado
   =f3_seek("F02C", Clave,, "ValRtn=F02cDescri")
Else
   =f3_seek("F02C", Clave,, "ValRtn=F02cCodAlm")
EndIf

lx=iif(eof(),'',iif(abreviado,F00cAbrevi,F00cDescri))
Select Alias (_ali)
*
Return lx

* Función busqueda fichero F04C, tamaños
*
Function LS3F04
Parameters clave,abreviado
*
_ali=alias()
select F04C
seek clave
lx=iif(eof(),'',iif(abreviado,F04cAbrevi,F04ctextos))
select alias (_ali)
*
return lx



* Función busqueda fichero F01C, entidad
*
Function LS3F01
Parameters clave
*
_ali=alias()
select F01C
seek clave
lx=iif(eof(),'',F01cDescri)
select alias (_ali)
*
return lx

