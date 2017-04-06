* Compone literal de condiciones de pago
*
Functio LS3CP
Parameter idioma
*
idioma=iif(type('idioma')='N',idioma,1)
lx=''
nef=max(1,TEF_NEF)
if idioma>1
  lx=iif(TEF_APL=0,'CAD',ltrim(str(TEF_APL)))
  nd1=TEF_APL
  for nd=2 to nef
    nd1=nd1+TEF_PER
    lx=lx+','+ltrim(str(nd1))
  endfor
  lx=lx+' DTE.INV.'
else
  lx=iif(TEF_APL=0,'Contado',ltrim(str(TEF_APL)))
  nd1=TEF_APL
  for nd=2 to nef
    nd1=nd1+TEF_PER
    lx=lx+','+ltrim(str(nd1))
  endfor
  lx=lx+' días F.F.'
endif
return lx