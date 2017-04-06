* Función que actualiza valores de un pedido a proveedor
* 
Function ls3alp
*
_ali=alias()
teftef=TEF_TEF
procta=PRO_CTA
select TEFTO
seek teftef
select CUEN
seek procta
m.NOM_NOM=CUE_DES
m.NOM_DIR=CUE_DIR
m.NOM_DIR2=''
m.NOM_CP=CUE_CP
m.NOM_POB=CUE_POB
m.NOM_PRO=CUE_PRO
*
select (_ali)
*
return m.NOM_NOM