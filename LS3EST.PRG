* Función que encuentra la familia en estadísticas
* 
Function ls3est
*
do case
* -----------------------------------
* Estadística de ventas
* -----------------------------------
case prog='FT3LV' .or. prog='FT3LVA'
  do case
  case LST_ORD=1 .or. LST_ORD=8
    select CUEN
    seek left(xlestad.ART_ART,14)
    lx1=CUE_DES
  case LST_ORD=2
    select ARTI
    seek xlestad.ART_ART
    lx1=ART_DES
  case LST_ORD=3
    select FAMI
    seek left(xlestad.ART_ART,_digfam)
    lx1=FAM_DES
  case LST_ORD=4
    select ZONA
    seek xlestad.ART_ART
    lx1=ZON_DES
  case LST_ORD=5
    select REPR
    seek xlestad.ART_ART
    lx1=REP_NOM
  case LST_ORD=6
    select ACTI
    seek xlestad.ART_ART
    lx1=ACT_DES
  case LST_ORD=7
    select TCLI
    seek xlestad.ART_ART
    lx1=TCL_DES
  endcase
* -----------------------------------
* Stock inmovilizado
* -----------------------------------
case prog='FT3LSI'
  if LST_ORD=1
    select CUEN
    seek left(xlestad.ART_ART,14)
    lx1=left(xlestad.ART_ART,4)+' '+subs(xlestad.ART_ART,9,6)+space(5)+' '+CUE_DES
  else
    select FAMI
    seek left(xlestad.ART_ART,_digfam)
    lx1=left(xlestad.ART_ART,_digfam)+space(16-_digfam)+' '+FAM_DES
  endif
* -----------------------------------
* Estadística de Compras
* -----------------------------------
case prog='SA3LC' .or. prog='SA3LCA'
  do case
  case LST_ORD=1
    select CUEN
    seek left(xlestad.ART_ART,14)
    lx1=left(xlestad.ART_ART,4)+' '+subs(xlestad.ART_ART,9,6)+space(5)+' '+CUE_DES
  case LST_ORD=2
    select ARTI
    seek xlestad.ART_ART
    lx1=xlestad.ART_ART+' '+ART_DES
  case LST_ORD=3
    select FAMI
    seek left(xlestad.ART_ART,_digfam)
    lx1=left(xlestad.ART_ART,_digfam)+space(16-_digfam)+' '+FAM_DES
  endcase    
endcase
select XLESTAD
*
return lx1