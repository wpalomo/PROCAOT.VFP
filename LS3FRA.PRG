* Función que actualiza valores de un pedido de cliente
* 
Function ls3fra
*
ali=alias()
teftef=TEF_TEF
nomdni=NOM_DNI
clicta=CTA_CTA
reprep=REP_REP
select TEFTO
seek teftef
select REPR
seek reprep
select CUEN
seek clicta
scatter memvar
select NFISCAL
seek nomdni
if !eof()
  scatter memvar
else
  select CUEN
  m.NOM_NOM=CUE_DES
  m.NOM_DIR=CUE_DIR
  m.NOM_DIR2=CUE_DIR2
  m.NOM_CP=CUE_CP
  m.NOM_POB=CUE_POB
  m.NOM_PRO=CUE_PRO
endif
cambio=1
select alias(ali)
*
return m.NOM_NOM
