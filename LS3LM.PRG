* Función de listado de cuentas Corrientes
*
Function ls3lm
*
ctacta=CTA_CTA
fec={  /  /    }
store 0 to td,th
r1=recno()
do while !eof() .and. DIA_FA<FECI .and. CTA_CTA=ctacta
  r1=recno()
  if MVC_OK='S'
    fec=DIA_FA
    if DIA_AS>=999998          && Asientos de cierre
      td=td-DIA_IMPH
      th=th-DIA_IMPD
    else
      td=td+DIA_IMPD
      th=th+DIA_IMPH
    endif
  endif
  skip
enddo
if eof() .or. DIA_FA>FECI .or. CTA_CTA<>ctacta
  go r1
endif
lx=iif(empty(fec).or.(td=0.and.th=0),'',f3_t('Movimientos anteriores a')+'  '+dtoc(fec+1))
*
return lx
