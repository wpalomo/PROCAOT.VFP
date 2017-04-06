* Programa definido de entradas standard
*    'SY3PRG'
*
if _cse<>'P' .or. !file('SY3FICH.FXP')
  =f3_sn(1,1,'Lo siento, No tiene prioridad suficiente para utilizar','el parametrizador de pantallas')
  return
endif

select SYSPRG
on key
push menu _MSYSMENU
set sysmenu to default
go top
scatter memvar
define popup list1 scroll
define popup list2 scroll font 'Courier',10
if !used('SYSUSER')
  use sysuser in 0 order tag idx1
endif
select SYSUSER
seek ' '+_usrcod
prog=PRG_PROG
select SYSPRG
seek prog
if eof()
  go top
  prog=PRG_PROG
endif
prggrid=''
do form sy3prg
select SYSUSER
repl PRG_PROG with sysprg.PRG_PROG
use
select SYSPRG
pop menu _msysmenu
*
return 

* ----------------------------
* Incluir ficheros
*
*	Marca de exclusividad       * en byte 1
*	Marca de control registros  * en byte 2
* ----------------------------
Function y3_fincl
*
select SYSFC
dimension fcd(50)
dimension fco(reccount())
nd1=0
store '' to lxfic,fcd,fco
for nd=1 to memlines(sysprg.PRG_FICH)
  lx1=mline(sysprg.PRG_FICH,nd)
  if !empty(lx1)
    lx2=iif(left(lx1,1)=' '.or.left(lx1,1)='*',left(lx1,2),'  ')
    lx1=ltrim(chrtran(lx1,'*',''))
    select SYSFC
    seek lx1
    if !eof()
      nd1=nd1+1
      fcd(nd1)=lx2+FC_FC+' '+FC_NOM
      lxfic=lxfic+FC_FC+'/'
    endif
  endif
endfor
store 0 to nd,maxfc
go top
do while .not. eof()
  maxfc=maxfc+1
  if at(FC_FC+'/',lxfic)=0
    nd=nd+1
    fco(nd)='  '+FC_FC+' '+FC_NOM
  endif
  skip
enddo
if maxfc>0
  modificado=st3trasp('FICHEROS DE LA PANTALLA','fco',maxfc,'fcd',50,'Exclusivo','sy3f_ex','Cont.Registros','sy3f_cr')
  if modificado
    lx=''
    for nd=1 to 50
      if .not. empty(fcd(nd))
        lx=lx+left(fcd(nd),12)+cr
      endif
    endfor
    select SYSPRG
    repl PRG_FICH with lx
  endif
endif
select SYSPRG
*
return .T.


* *******************************************
* Funciones sobre un listado
* *******************************************


* ----------------------------
* Formato listado
* ----------------------------
Function y3_lfor
*
progl=alltrim(m.PRG_PROG)
if .not. file ('LSTW\'+progl+'.FRX')
  lx3='LSTW\'+progl+'.FRX'
  copy file LSTW\ST3L.FRX to (lx3)
  lx3='LSTW\'+progl+'.FRT'
  copy file LSTW\ST3L.FRT to (lx3)
endif
lxlist='LSTW\'+progl
=rty3open('A')
select 1
push menu _MSYSMENU
set sysmenu to default
DEFINE PAD _msm_edit OF _MSYSMENU PROMPT "\<Edición" KEY ALT+E
ON PAD _msm_edit OF _MSYSMENU ACTIVATE POPUP _medit
DEFINE POPUP _medit MARGIN RELATIVE
DEFINE BAR _med_undo  OF _medit PROMPT "\<Deshacer" KEY CTRL+Z
DEFINE BAR _med_redo  OF _medit PROMPT "Re\<hacer" KEY CTRL+R
DEFINE BAR _med_sp100 OF _medit PROMPT "\-"
DEFINE BAR _med_cut   OF _medit PROMPT "Cor\<tar" KEY CTRL+X
DEFINE BAR _med_copy  OF _medit PROMPT "\<Copiar" KEY CTRL+C
DEFINE BAR _med_paste OF _medit PROMPT "\<Pegar" KEY CTRL+V
DEFINE BAR _med_pstlk OF _medit PROMPT "P\<egado especial"
DEFINE BAR _med_sp200 OF _medit PROMPT "\-"
DEFINE BAR _med_slcta OF _medit PROMPT "Se\<leccionar todo" KEY CTRL+A
DEFINE BAR _med_sp300 OF _medit PROMPT "\-"
DEFINE BAR _med_find  OF _medit PROMPT "\<Buscar..." KEY CTRL+F
DEFINE BAR _med_repl  OF _medit PROMPT "Ree\<mplazar..." KEY CTRL+L
DEFINE BAR _med_goto  OF _medit PROMPT "I\<r a línea..."
DEFINE BAR _med_sp400 OF _medit PROMPT "\-"
DEFINE BAR _med_insob OF _medit PROMPT "\<Insertar objeto"
DEFINE BAR _med_obj   OF _medit PROMPT "\<Objeto..."
DEFINE BAR _med_link  OF _medit PROMPT "\<Vínculos..."
*
DEFINE PAD _msm_view OF _MSYSMENU PROMPT "\<Ver" KEY ALT+V
ON PAD _msm_view OF _MSYSMENU ACTIVATE POPUP _mview
DEFINE POPUP _mview MARGIN RELATIVE
DEFINE BAR _mvi_toolb OF _mview PROMPT "\<Barras de herramientas..." 
modi report &lxlist noenvironment
release pad _msm_edit of _MSYSMENU
release pad _msm_view of _MSYSMENU
pop menu _MSYSMENU
if sysprg.LS_ADAPTAR<>1
  lxlist=trim(lxlist)+'.FRX'
  use &lxlist alias 'LST' in 0
  select LST
  go top
  do while !eof()
    if OBJTYPE=8 .and. STRETCH=.F. .and. HEIGHT<=1875
      nvar=mline(EXPR,1)
      nvar=upper(trim(nvar))
      nd=at('->',nvar)
      if nd>0
        nvar=stuff(nvar,nd,2,'.')
        repl EXPR with lower(nvar)
      endif
      lxfmt=mline(PICTURE,1)
      lxfont=upper(mline(FONTFACE,1))
      ndsize=FONTSIZE
      control=iif(nvar='EM' .or. nvar='NEM' .or. nvar='CIF' .or. nvar='DATE()'.or.nvar='_PAGENO',.F.,.T.)
      if control=.T. .and. (at('COURIER NEW',lxfont)>0 .or. at('COURIER',lxfont)>0)
        if .not. empty(lxfmt)
          lxfmt=trim(subs(lxfmt,2))
          lxfmt=left(lxfmt,len(lxfmt)-1)
          do case
          case at('9',lxfmt)>0
            nd=at('9',lxfmt)
            lxfmt=subs(lxfmt,nd)
          case at('X',lxfmt)>0
            nd=at('X',lxfmt)
            lxfmt=subs(lxfmt,nd)
          case at('!',lxfmt)>0
            nd=at('!',lxfmt)
            lxfmt=subs(lxfmt,nd)
          case at('D',lxfmt)>0
            lxfmt='D'
          otherwise
            lxfmt=''
          endcase
        endif
        if empty(lxfmt)
          select SYSVAR
          nd=at('.',nvar)
          nvar2=subs(nvar,nd+1)
          seek nvar2
          lxfmt=trim(VAR_FMT)
          do case
          case eof()
            nd=0
          case upper(left(lxfmt,1))='R'
            lxfmt=&lxfmt
            nd=len(lxfmt)
          case upper(left(lxfmt,1))='D'
            nd=10
          otherwise
            nd=len(lxfmt)
          endcase
          select LST
        else
          nd=len(lxfmt)
        endif
        if nd>0
          do case
          case at('COURIER NEW',lxfont)>0 .and. FONTSIZE=8
            repl WIDTH  with nd*650+360
          case at('COURIER NEW',lxfont)>0 .and. FONTSIZE=9
            repl WIDTH  with nd*750+410
          case at('COURIER NEW',lxfont)>0 .and. FONTSIZE=10
            repl WIDTH  with nd*850+460
          case at('COURIER',lxfont)>0 .and. FONTSIZE=10
            repl WIDTH  with round(nd*833.333,2)
          endcase
        endif
      endif
    endif
    skip
  enddo
  use
endif
=rty3open('C')
select SYSPRG
for nidiom=2 to LS_IDIOM
  lx1='LSTW'+str(nidiom,1)+'\ST3L.FRX'
  if file(lx1)
    lst1='LSTW\'+progl
    lst2='LSTW'+str(nidiom,1)+'\'+progl
    =sy3lsi(lst1,lst2,nidiom)
    modi report (lst2)
  endif
endfor
*
return .T.


* ************************************
* F U N C I O N  E S 
* ************************************
* ------------------------------------
* Abrir y Cerrar Ficheros
*
* Forma   A --> Abrir
*         C --> Cerrar
* ------------------------------------
Function rty3open
Parameter forma
*
select SYSPRG
for nd=1 to memlines(sysprg.PRG_FICH)
  lx1=mline(sysprg.PRG_FICH,nd)
  lx1=ltrim(chrtran(lx1,'*',''))
  if !empty(lx1)
    select SYSFC
    seek lx1
    do case
    case eof()
      do form st3sn with 1,1,'Fichero ['+lx1+'] no creado en SYSFC'
    case forma='C'
      select (lx1)
      use
    otherwise
      lx=iif(len(lx1)>4,lx1,lx1+_em)
      lx=iif(!empty(_empfich).and.file(_empfich+'\'+lx+'.DBF'),_empfich,_fich)+'\'+lx
      if !empty(FC_FCS) .and. !file(lx+'.DBF')
        lx=iif(at('\',FC_FCS)=0,_fich+'\'+trim(FC_FCS),trim(FC_FCS))
      endif
      use (lx) alias (lx1) in 0
      select alias (lx1)
      if !empty(key(1))
        set order to 1
      endif
    endcase
  endif
  select SYSPRG
endfor
*
return .T.
