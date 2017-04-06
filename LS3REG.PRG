* Función que actualiza valores de un pedido a proveedor
* 
Function ls3reg
*
ali=alias()
deldele=DEL_DELE
deldels=DEL_DELS
clscls=CLS_CLS
select CLVST
seek clscls
select DELE
seek deldels
m.DEL_NOMS=DEL_NOM
seek deldele
m.DEL_NOME=DEL_NOM
select alias(ali)
*
return ''
