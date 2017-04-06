* Programa para introducir Parámetros del Listado
*                      SY3LS
*
* (Carlos Casals Fradera  Abril, 1996)
*
*	fic		Nombre del fichero principal
*	maxfc	Número máximo de ficheros
*	fcr		Resto de ficheros		FC_FC+' '+FC_NOM
*
*	maxcmp	Número máximo de campos del fichero pral
*	cmp  	CAMPOS FICHERO			VAR_VAR+' '+VAR_DES
*	cmpo	CAMPOS ORDEN			VAR_VAR+' '+VAR_DES
*	cmpx	CAMPOS POSIBLES ORD.
*
select SYSPRG
scatter memvar
prog=trim(PRG_PROG)
nd1=memlines(sysprg.PRG_FICH)
maxfc=0
dimension fcr(nd1-1)
store '' to fcr,fic
select SYSFC
for nd=1 to nd1
  lx1=mline(sysprg.PRG_FICH,nd)
  lx1=ltrim(chrtran(lx1,'*',''))
  seek lx1
  do case
  case eof() .or. empty(lx1)
 
  case maxfc=0
    maxfc=maxfc+1
    fic=lx1
    scatter memvar
    fic=trim(fic)
    idx1=mline(FC_KEY,1)
    idx1=trim(left(idx1,10))
    lx=iif(empty(FC_FCS),fic,trim(FC_FCS))
    if len(lx)>4
      lx=_fich+'\'+lx
    else
      lx=lx+_em
      lx=iif(.not.empty(FC_FCS).or.empty(_empfich),_fich+'\'+lx,_empfich+'\'+lx)
    endif
    use (lx) alias (fic) in 0
    select SYSFC
  otherwise
    maxfc=maxfc+1
    fcr(maxfc-1)=FC_FC+' '+FC_NOM
  endcase
endfor
if empty(fic)
  do form st3sn with 1,1,'Falta definir los ficheros del listado'
  return
endif
select SYSPRG
lsnind=m.LS_NIND
store '' to lxwhen,lxordn,lxrel,lxinic,lxcondl,lxord
for nd=1 to memlines(sysprg.PRG_LST)
  lx=alltrim(upper(mline(sysprg.PRG_LST,nd)))
  do case
  case empty(lx)
  case at('[ORDENADO]',lx)>0
    lx1='lxord'
  case at('[CUANDO]',lx)>0
    lx1='lxwhen'
  case at('[RELACIONANDO]',lx)>0 
    lx1='lxrel'
  case at('[INICIO]',lx)>0
    lx1='lxinic'
  case at('[COND.LINEA]',lx)>0
    lx1='lxcondl'
  case at('[COND.FINAL]',lx)>0
    lx1='lxwhen'
  case lx1='lxord'
    lxordn=lxordn+lx
    lx=iif(right(lx,2)=cr,left(lx,len(lx)-2),lx)
    lxord=lxord+lx
  case lx1='lxrel'
    if maxfc>1
      lxrel=lxrel+lx+cr
    endif
  otherwise
    &lx1=&lx1+lx
  endcase
endfor
select alias(fic)
nd=fcount()
dimension cmp(nd),cmpo(10),cmpx(nd)
store space(40) to cmp,cmpo,cmpx
store 0 to nd1,nd2,maxcmp
lx1='#'+chrtran(lxord,',(+)','####')+'#'
for i=1 to nd                                 && Cargar input fichero
  varvar=field(i)
  select SYSVAR
  seek varvar
  if !eof() .and. VAR_FMT<>'M' .and. VAR_FMT<>'G' .and. VAR_FMT<>'L'
    maxcmp=maxcmp+1
    cmp(maxcmp)=VAR_VAR+' '+VAR_DES
    varvar='#'+trim(VAR_VAR)+'#'
    if at(varvar,lx1)>0 .and. nd1<10
      nd1=nd1+1
      cmpo(nd1)=VAR_VAR+' '+VAR_DES
    else
      nd2=nd2+1
      cmpx(nd2)=VAR_VAR+' '+VAR_DES
    endif
  endif
  select alias(fic)
endfor
do form sy3ls
select (fic)
use
select SYSPRG
*
return .T.
