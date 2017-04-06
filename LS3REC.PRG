* Función de devolver el nombre de una cuenta
*
Function ls3rec
Parameter fec1,fec2,valor
*
store '' to lx3
lx3=ynumlit(valor,1,iif(_empeuro=1,.T.,.F.))
lx3=left(trim(lx3)+iif(_empeuro=1,' euros ',' pesetas ')+repl('-',200),121)
store '' to lx1,lx2
lxmes='Enero     Febrero   Marzo     Abril     Mayo      Junio     Julio     Agosto    SeptiembreOctubre   Noviembre Diciembre '
nd1=day(fec1)
nd2=month(fec1)*10-9
nd3=year(fec1)
lx1=str(nd1,2)+' de '+trim(subs(lxmes,nd2,10))+' de '+str(nd3,4)
nd1=day(fec2)
nd2=month(fec2)*10-9
nd3=year(fec2)
lx2=str(nd1,2)+' de '+trim(subs(lxmes,nd2,10))+' de '+str(nd3,4)
*
return lx1