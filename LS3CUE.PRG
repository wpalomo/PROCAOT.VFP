* Funcion de Listado para encontrar descripción cuenta
*
* 	ctacta		Cta a buscar  		4300     1093
*	ctatxt		Texto a insertar 	Telf.
*	ctatxt_c	Campo a insertar	CUE_TEL
*   ctanf		buscar nombre fiscal .T./.F.
*   lxriesgo	Variable donde guardar el riesgo
*
Function ls3cue
Parameter ctacta,ctatxt,ctatxt_c,ctanf,lxriesgo
*
ls_ali=alias()
select CUEN
seek ctacta
lx=iif(eof(),' ',CUE_DES)
if type('ctatxt')='C'
  lx1=&ctatxt_c
  if !empty(lx1)
    lx=trim(lx)+' '+ctatxt+' '+lx1
  endif
endif
if type('ctanf')='L'
  if ctanf=.T.
    select NFISCAL
    seek cuen.NOM_DNI
  endif
endif
if type('lxriesgo')='C'
  &lxriesgo=DOM_RIE
endif
select alias (ls_ali)
*
return lx