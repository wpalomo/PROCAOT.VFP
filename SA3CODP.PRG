Function SA3CODP
Parameters clave
*
cad=''
ca=alias()
select FCCD
seek clave
if !eof()
  cad=ART_ART2
endif    
select (ca)
return cad
