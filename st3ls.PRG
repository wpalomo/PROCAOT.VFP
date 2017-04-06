* Programa de Impresión
*        ST3LS
*
Function st3ls
Parameter _xsalida
*
_finlst=.F.
prog=wontop()
_lxerr=''
if !empty(sysprg.PRG_CI)
  =f3_ci()
endif
_xobjw='W'+trim(prog)
if !file (_xobjw+'.PRG') .and. !file(_xobjw+'.FXP')
  _xobjw=''
endif
select SYSPRG
if !empty(_lxerr)
  define window wedit from 2,0 to 30,70 title f3_t('Incidencias') font 'Courier',10 color RGB(0,0,0,255,255,128)
  move window wedit center
  activate window wedit
  SW_OKE=0
  @ .5,1 edit lxerr size 22,68 scroll font 'Courier',10 tab noedit when mdown()
  @ 23,0 to 23,70
  @ 23.5,63 get SW_OKE pict '@*BHT BMP\CLOSE' size 2.5,4.5
  read modal
  deactivate window wedit
  release window wedit
  return
endif
_listar=.T.
_xfc=mline(sysprg.PRG_FICH,1)
_xfc=ltrim(chrtran(_xfc,'*',''))
lxlist='LSTW\'+trim(sysprg.PRG_PROG)

*> Anulado AVC - 06.07.1999
*if _xnidiom>1
*  for _xidioml=1 to _xnidiom
*    if _idiom(_xidioml)=_idioma
*      lx1='LSTW'+str(_xidioml,1)+'\'+trim(sysprg.PRG_PROG)
*      lxlist=iif(file(lx1+'.FRX'),lx1,lxlist)
*      _xidioml=99
*    endif
*  endfor
*endif

store '' to codigol,expr,expseek,expfor,tmp,_lxg,_lxfcg,listfrx,listfrt
gcambio=.F.
select (_xfc)
go top
_r1ls=iif(eof(),0,recno())
if _xier>=0 .and. !empty(_xobjw)
  do (_xobjw) with 'INICIO'
  select alias (_xfc)
endif
if _xier>=0
  for nd=1 to memlines(sysprg.prg_lst)
    lx=alltrim(upper(mline(sysprg.prg_lst,nd)))
    do case
    case empty(lx)
    case at('[ORDENADO]',lx)>0
      lx1='codigol'
    case at('[CUANDO]',lx)>0
      lx1='expr'
    case at('[RELACIONANDO]',lx)>0 
      lx1='lxrel'
    case at('[INICIO]',lx)>0
      lx1='expseek'
    case at('[COND.LINEA]',lx)>0
      lx1='expfor'
    case at('[COND.FINAL]',lx)>0
      lx1='expr'
    case lx1='lxrel'
      &lx
    otherwise
      &lx1=&lx1+lx
    endcase
  endfor
  if !empty(expr)           && Condiciones a la condicion expr 
    nd1=atc('[',expr)
    nd2=atc(']',expr)
    do while nd1>0 .and. nd2>nd1
      lx1=subs(expr,nd1,nd2-nd1+1)
      nd2=at(',',lx1)
      lx2=subs(lx1,2,nd2-2)
      if &lx2=.T.
        lx2=subs(lx1,nd2+1)
        lx2=left(lx2,len(lx2)-1)
      else
        lx2=''
      endif
      expr=stuff(expr,nd1,len(lx1),lx2)
      nd1=atc('[',expr)
      nd2=atc(']',expr)
    enddo
  endif
  select (_xfc)
  if left(_xfc,2)<>'XL'
    store 0 to _xier, ultimo
    if len(_xfc)=4
      sql_fc=_xfc+_em
    else
      sql_fc=_fichero
    endif
    lx_sql='SELECT * FROM '+sql_fc
    if !empty(expr)
      nd=atc('betw(',expr)
      do while nd>0
        lx1=subs(expr,nd)
        nd1=at(')',lx1)
        lx1=left(lx1,nd1)               && Expresion original
        lx2=subs(lx1,6,len(lx1)-6)      && Expresion a traducir   
        nd1=at(',',lx2)
        lx3=left(lx2,nd1-1)+' BETWEEN '
        lx2=subs(lx2,nd1+1)
        nd1=at(',',lx2)
        lx3=lx3+left(lx2,nd1-1)+' AND '+subs(lx2,nd1+1)
        expr=stuff(expr,nd,len(lx1),lx3)
        nd=atc('betw(',expr)
      enddo
      nd=atc('.and.',expr)
      do while nd>0
        expr=stuff(expr,nd,5,' AND ')
        nd=atc('.and.',expr)
      enddo
      nd=atc('.or.',expr)
      do while nd>0
        expr=stuff(expr,nd,4,' OR ')
        nd=atc('.or.',expr)
      enddo
      expr=expr+' '
      nd=atc('m.',expr)
      do while nd>0
        lx1=subs(expr,nd)
        nd1=at(' ',lx1)
        if nd1>0
          lx1=left(lx1,nd1-1)
          nd1=len(lx1)
          lx2=&lx1
          do case
          case type('lx2')='C'
            lx2=_cm+lx2+_cm
          case type('lx2')='N'

          endcase
          expr=stuff(expr,nd,nd1,lx2)
        else
          expr=''
        endif
        nd=atc('m.',expr)
      enddo
      lx_sql=lx_sql+' WHERE '+expr
    endif
    if !empty(codigol)
      codigol=&codigol
      lx_sql=lx_sql+' ORDER BY '+codigol
    endif
    _xier=SqlExec(_asql,lx_sql,'SQL_LIST')
    =sqlmoreresults(_asql)
    if _xier>0
      _xier=0
      select SQL_LIST
      go bottom
      r1_ultimo=iif(eof(),0,recno())
      go top
    endif
  else
    tmp=''
    if empty(sysprg.LS_NIND)
      expr=iif(empty(expr),'.not.deleted()','('+expr+').and..not.deleted()')
    else
      expr=iif(empty(expr),'.not.eof()',expr)
    endif
    tmp=sys(2015)
    tmp=_fich+'\'+tmp
    select (_xfc)
    store 0 to _xier,ultimo
    if _xier>=0 .and. empty(expseek)
*      codigol=&codigol           Para que sirve ? Asteriscado el 17 de septiembre de 1998.     
      set talk on
      index on &codigol to (tmp) for &expr
      set talk off
      set index to (tmp)
      go bottom
      ultimo=recno()
      go top
    endif
    select (_xfc)
    go top
    if empty(expseek)
      r1_primero=iif(eof(),0,recno())
      expfor='.not.deleted()'
      expr='.not.eof()'
    else
r1_primero=0
      set near on
      lx=sysprg.LS_NIND
*      set order to &lx
*      seek eval(expseek)
*      r1_primero=iif(eof(),0,recno())
*      set near off
      expfor=iif(empty(expfor),'.not.deleted()',expfor)
    endif
    select (_xfc)
    if r1_primero>0
      go r1_primero
    endif
  endif
  if _xier>=0  
    if _listado_esp
      set point to ','
      set separator to '.'
    else
      set point to "."
      set separator to ","
    endif
      do case
      case eof()

      case _xsalida='PAN' .and. _xdetall=2
        report form (lxlist) preview summary noconsole
      case _xsalida='PAN'
        report form (lxlist) preview noconsole
      case _xsalida='IMPP' .and. _xdetall=2
        report form (lxlist) to print summary noconsole
      case _xsalida='IMPP'
        report form (lxlist) to print noconsole
      case _xsalida='IMP' .and. _xdetall=2
        report form (lxlist) to print prompt summary noconsole
      case _xsalida='IMP'
        report form (lxlist) to print prompt noconsole
      endcase

    if _teclado_esp
      set point to ','
      set separator to '.'
    else
      set point to "."
      set separator to ","
    endif

    select (_xfc)
    if !empty(_xobjw)
      do (_xobjw) with 'FINAL'
      select (_xfc)
    endif
    if left(_xfc,2)='XL'
      select (_xfc)
      zap
    endif
  else
    _xier=0
  endif
endif

_finlst=.F.
store 0 to _xdetall, _xier

_listar=.F.
return
