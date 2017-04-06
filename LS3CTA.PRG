* Función de devolver el nombre de una cuenta
*
Function ls3cta
Parameter ctacta
*
_alil=alias()
ctacta=left(ctacta+space(4),4)
lx=''
select PCON
seek ctacta
lx=iif(eof(),space(30),CTA_DES)
select (_alil)
*
return lx