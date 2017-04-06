* Programa consultas/Edición Standard
*         ST3SQLE
*
*
* Arrays 	_sql_t	Textos condiciones
*			_sql_c	Condiciones
*			_lsql	Array donde se guardan las consultas
*
select SYSPRG
count all for (PRG_TIPO='F'.or.PRG_TIPO='C').and.!deleted() to npant
dimension _lprog(npant,2),_lsele(_nsql,3)
store '' to _lsele,_lxsele,SQL_C1,SQL_C2,SQL_C3,SQL_C4,SQL_C5,SQL_C6,SQL_C7,SQL_C8,SQL_C9,SQL_C10
store 1 to prgprog
store '' to APAR1,APAR2,APAR3,APAR4,APAR5,APAR6,APAR7,APAR8,APAR9,APAR10
store '' to CPAR1,CPAR2,CPAR3,CPAR4,CPAR5,CPAR6,CPAR7,CPAR8,CPAR9,CPAR10
store '' to YO2,YO3,YO4,YO5,YO6,YO7,YO8,YO9,YO10
nd=0
go top
do while !eof()
  if PRG_TIPO='F'.or.PRG_TIPO='C'
    nd=nd+1
    _lprog(nd,1)=PRG_PROG
    _lprog(nd,2)=PRG_TIT
  endif
  skip
enddo
store '' to _cmp1,_cmp2,_cmp3,_cmp4,_cmp5,_cmp6,_cmp7,_cmp8,_cmp9,_cmp10,_ali
use SYSSQL order tag idx1 in 0 alias SYSSQL
select SYSSQL
count all for !deleted() to _maxsql
_maxsql=max(1,_maxsql)
dimension _lsql(_maxsql+10),_sql_t(10),_sql_c(10),_sql_cond(10,3),_sql_ord(9)
for nd=1 to 10
  _sql_cond(nd,1)=space(20)
  _sql_cond(nd,2)='  '
  _sql_cond(nd,3)=''
endfor
dimension combo1(_nsql)
dimension combo2(7)
combo2(1)=f3_t('=  igual        ')
combo2(2)=f3_t('<> Diferente    ')
combo2(3)=f3_t('>= Mayor o igual')
combo2(4)=f3_t('>  Mayor que    ')
combo2(5)=f3_t('<= Menor o igual')
combo2(6)=f3_t('<  Menor que    ')
combo2(7)=f3_t('=* Contenga     ')
store '' to _sql,_lsql,fc_br,combo1
store space(20) to _sql_t
store space(50) to _sql_c
sql_cond=1
go top
nd=0
do while !eof()
  nd=nd+1
  _lsql(nd)=SQL_NOM
  skip
enddo
on key
_fin_sql=.F.
do while _fin_sql=.F.
  _fin_sql=.T.
  m.ls_graf=1
  _salida=''
  do form st3sqle
  if !empty(_salida)
    _fin_sql=.F.
    fc_br=''
    for nd=1 to memlines(syssql.SQL_FROM)
      lx1=mline(syssql.SQL_FROM,nd)
      if !empty(lx1)
        fc_br=fc_br+trim(lx1)+','
      endif
    endfor
    fc_br=left(fc_br,len(fc_br)-1)
    =sq3_open('A',fc_br)
    lx_sql=syssql.SQL_MEM
    nd=atc('#',lx_sql)                        && --------- Campos invisibles
    nd1=atc(' FROM ',lx_sql)
    if nd>1 .and. nd<nd1
      lx3=subs(lx_sql,nd1)                    && --------- Instrucción SQL a partir del FROM
      lx_sql=left(lx_sql,nd1-1)+'  '          && --------- parte del SQL del SELECT
      nd=atc('#',lx_sql)
      do while nd>1
        lx1=left(lx_sql,nd-1)
        lx2=subs(lx_sql,nd+1)
        nd=at(',',lx2)
        lx2=iif(nd=0,'',subs(lx2,nd+1))
        lx_sql=lx1+lx2
        nd=atc('#',lx_sql)
      enddo
      lx_sql=trim(lx_sql)
      if right(lx_sql,1)=','
        lx_sql=left(lx_sql,len(lx_sql)-1)
      endif
      lx_sql=lx_sql+lx3
    endif
    _tmp_sq=sys(2015)
    _tmp_sql=_tmp+_tmp_sq
    lx_sql=lx_sql+' into DBF '+_tmp_sql
    on error do st3sqler
    _xier=0
    store '' to lx_pot,lx_pos
    nd=at('POT(',lx_sql)
    do while nd>0
      lx_pot=lx_pot+str(nd+4)
      lx_sql=stuff(lx_sql,nd,3,'SUM')
      nd=at('POT(',lx_sql)
    enddo
    nd=at('POS(',lx_sql)
    do while nd>0
      lx_pos=lx_pos+str(nd+4)
      lx_sql=stuff(lx_sql,nd,3,'SUM')
      nd=at('POS(',lx_sql)
    enddo
    &lx_sql
    on error do st3err
    _tmp_dbf=alias()
    if _xier=0
      select (_tmp_dbf)
      do while !empty(lx_pos)
        nd=val(left(lx_pos,10))
        lx_pos=iif(len(lx_pos)=10,'',subs(lx_pos,10))
        n_campo=sq3_campo(nd)
        lx1=field(n_campo)
        sum all &lx1 to total
        total=total/100
        if total<>0
          repl all (lx1) with &lx1/total
        endif
      enddo
      do while !empty(lx_pot)
        nd=val(left(lx_pot,10))
        lx_pot=iif(len(lx_pot)=10,'',subs(lx_pot,10))
        lx2=subs(lx_sql,nd)
        nd1=at(')',lx2)
        lx2=left(lx2,nd1-1)
        nd1=at('.',lx2)
        f_campo=left(lx2,nd1-1)
        v_campo=subs(lx2,nd1+1)
        n_campo=sq3_campo(nd)
        select (f_campo)
        sum all &v_campo to total
        select (_tmp_dbf)
        lx1=field(n_campo)
        total=total/100
        if total<>0
          repl all (lx1) with &lx1/total
        endif
      enddo
      go top
      do case
      case _salida='BROWSE'
        nd=fcount()
        dimension tit_gr(nd)
        do form st3sqlp with _tmp_dbf
      case _salida='PANTALLA'
        _formato='QLSTW\'+trim(syssql.SQL_LST)
        if empty(syssql.SQL_LST) .or. !file(_formato+'.FRX')
          =sq3_lst(_tmp_sq)
        endif
        select (_tmp_dbf)
        go top
        report form (_formato) preview
      case _salida='IMPRESORA PREDETERMINADA'
        _formato='QLSTW\'+trim(syssql.SQL_LST)
        if empty(syssql.SQL_LST) .or. !file(_formato+'.FRX')
          =sq3_lst(_tmp_sq)
        endif
        select (_tmp_dbf)
        go top
        report form (_formato) to print noconsole
      case _salida='IMPRESORA'
        _formato='QLSTW\'+trim(syssql.SQL_LST)
        if empty(syssql.SQL_LST) .or. !file(_formato+'.FRX')
          =sq3_lst(_tmp_sq)
        endif
        select (_tmp_dbf)
        go top
        report form (_formato) to print prompt noconsole 
      endcase
    else
      _xier=0
    endif
    if file (_tmp_sql+'.DBF')
      select (_tmp_dbf)
      use
      delete file (_tmp_sql+'.DBF')
    endif
    =sq3_open('C',fc_br)
  endif
  select SYSSQL
enddo
select SYSSQL
use
select SYSPRG
*
return

* ------------------------------------------------
* Insertar y eliminar campos
*
* Forma   I --> Insertar
*         E --> Eliminar
* (sólo 1 fichero el principal)
* -----------------------------------------------
Function sq3_camp
Parameter forma,lxcamp
*
if forma='E'
  nd1=at('.'+lxcamp+',',_lxsele)
  do while nd1>0
    lx1=left(_lxsele,nd1)
    lx2=subs(_lxsele,nd1+1)
    do while len(lx1)>0
      if subs(lx1,len(lx1),1)=','
        exit
      else
        lx1=iif(len(lx1)=1,'',left(lx1,len(lx1)-1))
      endif
    enddo
    nd1=at(',',lx2)
    lx2=iif(nd1=len(lx2),'',subs(lx2,nd1+1))
    _lxsele=lx1+lx2
    nd1=at('.'+lxcamp+',',_lxsele)
  enddo
else
  lx1=_sqlopen
  nd=at(',',lx1)
  lx3=iif(nd=0,lx1,left(lx1,nd-1))
  select (lx3)
  lx4=lx3+'.'+lxcamp
  m.tot=iif(type(lx4)<>'U',1,0)
  select SYSPRG
  if tot=0
    wait window f3_t('Esta variable no está en el fichero ['+lx3+']')
  else
    _lxsele=_lxsele+'         '+lower(lx3)+'.'+lxcamp+','
  endif
endif
*
return

* ------------------------------------------------
* Abrir y Cerrar Ficheros
*
* Forma   A --> Abrir
*         C --> Cerrar
* -----------------------------------------------
Function sq3_open
Parameter forma,lxopen
*
do while !empty(lxopen)
  nd=at(',',lxopen)
  if nd=0
    lx2=lxopen
    lxopen=''
  else
    lx2=left(lxopen,nd-1)
    lxopen=subs(lxopen,nd+1)
  endif
  lx1=left(lx2+space(10),10) 
  _sql_open=iif(!empty(lx2).and.atc('/'+lx2+']',_xopenfc)=0,.T.,.F.)
  do case
  case _sql_open .and. forma='A'
    select SYSFC
    seek lx1
    fcfc=iif(len(trim(lx1))>4,trim(lx1),trim(lx1)+_em)
    fcfc=iif(!empty(_empfich).and.file(_empfich+'\'+fcfc+'.DBF'),_empfich,_fich)+'\'+fcfc
    if !empty(FC_FCS) .and. !file(fcfc+'.DBF')
      fcfc=iif(at('\',FC_FCS)=0,_fich+'\'+trim(FC_FCS),trim(FC_FCS))
    endif
    if file(fcfc+'.DBF')
       use (fcfc) alias (lx2) in 0
       select (lx2)
       if file(fcfc+'.CDX')
         set order to CODIGO
       endif
       go top
       scatter memvar blank memo
    endif
  case _sql_open .and. forma='C'
    select (lx2)
    use
  endcase
enddo
*
return


* ------------------------------------------------
* Crear relaciones con ficheros
* -----------------------------------------------
Function sq3_rela
*
sqltot=0
if memlines(SQL_FROM)<=0
  _lxrel=''
  repl SQL_RELA with _lxrel
  return
endif
dimension _sqlfic(memlines(SQL_FROM))
store '' to _sqlfic,_sqlopen
select SYSFC
for nd=1 to memlines(syssql.SQL_FROM)
  lx=mline(syssql.SQL_FROM,nd)
  if !empty(lx)
    lx=left(lx+space(10),10)
    seek lx
    if !eof()
      sqltot=sqltot+1
      _sqlfic(sqltot)=lx+FC_NOM
      _sqlopen=','+trim(lx)+_sqlopen
    endif
  endif
endfor
select SYSPRG
if sqltot>1
  _ok=f3_sn(2,1,'Desea establecer relaciones entre ficheros')
  if _ok=.F.
    return
  endif
else
  _lxrel=''
  repl syssql.SQL_RELA with _lxrel
  return
endif
select SYSSQL
dimension fco(sqltot-1),fcd(1)
_sqlopen=subs(_sqlopen,2)
=sq3_open('A',_sqlopen)
_lxrel=''
fc_pral=trim(left(_sqlfic(1),10))
lxini=lower(fc_pral)+'.'
for sqlnd=2 to sqltot
  fc_sec=trim(left(_sqlfic(sqlnd),10))
  lxf=lower(fc_sec)+'.'
  select (fc_sec)
  set order to CODIGO
  lxkey=upper(key())
  store '' to sqllx2,sqllx3                           && Cargo expresión relación
  do while !empty(lxkey)
    nd3=at('+',lxkey)
    if nd3=0
      lx4=lxkey
      lxkey=''
    else
      lx4=left(lxkey,nd3)
      lxkey=subs(lxkey,nd3+1)
    endif
    do case
    case left(lx4,4)='STR('
      sqllx2=sqllx2+'str('+lxini+subs(lx4,5)
      sqllx3=sqllx3+'str('+lxf+subs(lx4,5)
    case left(lx4,5)='DTOS('
      sqllx2=sqllx2+'dtos('+lxini+subs(lx4,6)
      sqllx3=sqllx3+'dtos('+lxf+subs(lx4,6)
    otherwise
      sqllx2=sqllx2+lxini+lx4
      sqllx3=sqllx3+lxf+lx4
    endcase
  enddo
  if type(sqllx2)='U'
    fcd(1)=_sqlfic(1)
    if sqltot>2
      store '' to fcd,fco
      nd1=0
      for nd=1 to sqltot
        if fc_sec<>trim(left(_sqlfic(nd),10))
          nd1=nd1+1
          fco(nd1)=_sqlfic(nd)
        endif
      endfor
      _ok=st3trasp('Relacion Fichero ['+_sqlfic(sqlnd)+']','fco',sqltot-1,'fcd',1,,,,,,,'Ficheros posibles','Fichero a relacionar')
      if _ok .and. trim(left(fcd(1),10))<>fc_pral
        lx1=lower(trim(left(fcd(1),10)))+'.'
        nd=at(lxini,sqllx2)
        do while nd>0
          sqllx2=stuff(sqllx2,nd,len(lxini),lx1)
          nd=at(lxini,sqllx2)
        enddo
      endif
    endif
* -------------- Casos especiales ------------------------------------------
    if type(sqllx2)='U' .and. upper(sqllx3)='CUEN.CUE_CTA+str(CUEN.CUE_SCTA)'
      nd1=at('.',sqllx2)
      if nd1>0
        lx1=left(sqllx2,nd1)+'CTA_CTA'
        if type(lx1)='U'
          lx1=left(sqllx2,nd1)+'PRO_CTA'
          if type(lx1)='U'
            lx1=left(sqllx2,nd1)+'CLI_CTA'
            if type(lx1)<>'U'
              sqllx2=lx1
            endif
          else
            sqllx2=lx1
          endif
        else
          sqllx2=lx1
        endif
      endif
    endif       
* ---------------------------------
    if type(sqllx2)='U'
      if !empty(fcd(1))
         lxrela=sqllx2+'='+sqllx3+space(20)
        _ok=f3_inp('Entre la relación',trim(subs(fcd(1),11)),'lxrela','')
        if _ok
          _lxrel=_lxrel+lxrela+cr  
        endif
      endif
    else
      _lxrel=_lxrel+sqllx2+'='+sqllx3+cr  
    endif
  else
    _lxrel=_lxrel+sqllx2+'='+sqllx3+cr  
  endif
endfor
=sq3_open('C',_sqlopen)
select SYSSQL
repl SQL_RELA with _lxrel
*
return

* ------------------------------------------------
* Orden campos
* -----------------------------------------------
Function sq3_ord
*
tot=0
_borrar=iif(ndds=1 .and. left(m_destino(1),1)='1',.T.,.F.)
for nd=1 to _nsql
  if !empty(m_destino(nd))
    do case
    case _borrar=.T.
      m_destino(nd)='  '+subs(m_destino(nd),3)
    case nd<=ndds .and. tot<9
      tot=tot+1
      m_destino(nd)=str(tot,1)+' '+subs(m_destino(nd),3)
    otherwise
      m_destino(nd)='  '+subs(m_destino(nd),3)
    endcase
  endif
endfor
st3trasp.ndds.setfocus
*
return

* ------------------------------------------------
* Agrupación
* -----------------------------------------------
Function sq3_agr
*
if !empty(m_destino(ndds))
  if left(m_destino(ndds),1)=' '
    wait window  f3_t('Sólo puede agrupar campos ordenados')
  else
    _borrar=iif(ndds=1 .and. left(m_destino(1),2)='1*',.T.,.F.)
    for nd=1 to _nsql
      do case
      case _borrar=.T.
        m_destino(nd)=left(m_destino(nd),1)+' '+subs(m_destino(nd),3)
      case !empty(m_destino(nd))
        m_destino(nd)=left(m_destino(nd),1)+iif(nd<=ndds,'*',' ')+subs(m_destino(nd),3)
      endcase
    endfor
    st3trasp.ndds.setfocus
  endif
endif
*
return

* ------------------------------------------------
* Operaciones sobre campos
* 	SUM		+	Suma
*	MAX 	X	Máximo
*	MIN		M	Mínimo
*	AVG		P	Promedio
*	CNT		C	Contador
*   POT		T	Porcentaje sobre total
* 	POS		S	Porcentaje sobre seleccionado
*	INV		I	Invisible   (sin operaciones)
* -----------------------------------------------
Function sq3_oper
*
if !empty(m_destino(ndds))
  if empty(left(m_destino(ndds),2))
    nvar=subs(m_destino(ndds),10)
    lx1=subs(m_destino(ndds),3,7)
    nd=at(' ',nvar)
    if nd>0
      nvar=subs(nvar,nd+1)
    endif
    OSUM=iif(at('+',lx1)>0,1,0)
    OMAX=iif(at('X',lx1)>0,1,0)
    OMIN=iif(at('M',lx1)>0,1,0)
    OPROM=iif(at('P',lx1)>0,1,0)
    OCONT=iif(at('C',lx1)>0,1,0)
    OPORT=iif(at('T',lx1)>0,1,0)
    OPORS=iif(at('S',lx1)>0,1,0)
    OINV=iif(at('I',lx1)>0,1,0)
    _ok=f3_inp('Calculos','Variable,Suma,Máximo,Mínimo,Promedio,Contador,% Sobre total,% sobre seleccionado,Invisible','$nvar,OSUM,OMAX,OMIN,OPROM,OCONT,OPORT,OPORS,OINV',',CHK,CHK,CHK,CHK,CHK,CHK,CHK,CHK')
    if _ok
      lx1=iif(OSUM=1,'+','')+iif(OMAX=1,'X','')+iif(OMIN=1,'M','')+iif(OPROM=1,'P','')+iif(OCONT=1,'C','')+iif(OPORT=1,'T','')+iif(OPORS=1,'S','')
      lx1=iif(OINV=1,'I      ',left(lx1+space(7),7))
      m_destino(ndds)=stuff(m_destino(ndds),3,7,lx1)
    endif
    st3trasp.ndds.setfocus
  else
    =f3_sn(1,1,'No se pueden hacer operaciones sobre un campo de ordenación')
  endif
endif
*
return

* ------------------------------------------------
* Componer la expresión
* -----------------------------------------------
Function sq3_comp
*
_sql_ord=''
lx='SELECT '
for nd=1 to memlines(syssql.SQL_SELE)
  lx1=mline(syssql.SQL_SELE,nd)
  if !empty(lx1)
    lx2=subs(lx1,3,7)
    lx3=subs(lx1,10)
    nd2=at(' ',lx3)
    lxcampo=''
    if !empty(lx3) .and. nd2>1
      if left(lx3,1)='#'                         && Expresión manual
        lxcampo=subs(lx3,nd2+1)
      else
        lxcampo=left(lx3,nd2-1)
      endif
      if empty(lx2)
        lx=lx+lxcampo+', '
      else                                       && Ciclo de operaciones
        lx2=trim(lx2)
        do while !empty(lx2)
          lxo=left(lx2,1)
          if !empty(lxo)
            if lxo='I'
              lx=lx+'#'+lxcampo+', '
            else
              do case
              case lxo='+'
                lxo='SUM'
              case lxo='X'
                lxo='MAX'
              case lxo='M'
                lxo='MIN'
              case lxo='P'
                lxo='AVG'
              case lxo='T'
                lxo='POT'
              case lxo='S'
                lxo='POS'
              otherwise
                lxo='CNT'
              endcase
              lx=lx+lxo+'('+lxcampo+'), '
            endif
          endif
          lx2=iif(len(lx2)=1,'',subs(lx2,2))
        enddo
      endif
      if left(lx1,1)<>' '
        num=val(left(lx1,1))
        lxg=subs(lx1,2,1)
        _sql_ord(num)=lxg+lxcampo
      endif
    endif
  endif
endfor
lx=left(lx,len(lx)-2)+' FROM '
for nd=1 to memlines(syssql.SQL_FROM)
  lx1=mline(syssql.SQL_FROM,nd)
  if !empty(lx1)
    lx=lx+trim(lx1)+', '
  endif
endfor
lx=left(lx,len(lx)-2)
lx2=''
for nd=1 to memlines(syssql.SQL_WHER)
  lx1=mline(syssql.SQL_WHER,nd)
  if !empty(lx1)
    nd1=at('[',lx1)
    nd2=at('=*',lx1)
    _apar='syssql.APAR'+ltrim(str(nd))
    _cpar='syssql.CPAR'+ltrim(str(nd))
    _yo='syssql.YO'+ltrim(str(nd))
    store '' to v_apar,v_cpar,v_yo
    if nd>=2 .and. nd<=10
      v_yo=&_yo
      v_yo=iif(v_yo='O','.or.','.and.')
    endif
    if nd>=1 .and. nd<=10
      v_apar=&_apar
      v_cpar=&_cpar
      v_apar=chrtran(v_apar,'[{ ','((')
      v_cpar=chrtran(v_cpar,']} ','))')
    endif
    do case
    case nd1=0 .and. nd2=0
      lx2=lx2+v_yo+v_apar+trim(lx1)+v_cpar
    case nd1>0 .and. nd2=0
      lx2=lx2+v_yo+v_apar+left(lx1,nd1-1)+'_cmp'+ltrim(str(nd))+v_cpar
    case nd1=0 .and. nd2>0
      lx2=lx2+v_yo+v_apar+'atc('+alltrim(subs(lx1,nd2+2))+','+alltrim(left(lx1,nd2-1))+')>0'+v_cpar
    otherwise
      lx2=lx2+v_yo+v_apar+'atc(trim(_cmp'+ltrim(str(nd))+'),'+alltrim(left(lx1,nd2-1))+')>0'+v_cpar
    endcase
  endif
endfor
*x2=iif(empty(lx2),lx2,left(lx2,len(lx2)-7))
lx3=''
for nd=1 to memlines(syssql.SQL_RELA)
  lx1=mline(syssql.SQL_RELA,nd)
  if !empty(lx1)
    lx3=lx3+trim(lx1)+' .and. '
  endif
endfor
lx3=iif(empty(lx3),lx3,left(lx3,len(lx3)-7))
do case
case !empty(lx2) .and. !empty(lx3)
  lx=lx+' WHERE ('+lx2+') .and. ('+lx3+')'
case !empty(lx2)
  lx=lx+' WHERE '+lx2
case !empty(lx3)
  lx=lx+' WHERE '+lx3
endcase
store '' to lxord,lxgroup
for nd=1 to 9
  if !empty(_sql_ord(nd))
    lx1=trim(subs(_sql_ord(nd),2))
    lxord=lxord+', '+lx1
    if left(_sql_ord(nd),1)='*'
      lxgroup=lxgroup+', '+lx1
    endif
  endif
endfor
if !empty(lxord)
  if syssql.SQL_DESC=1
    lx=lx+' ORDER BY '+subs(lxord,3)+' DESC'
  else
    lx=lx+' ORDER BY '+subs(lxord,3)
  endif
endif
if !empty(lxgroup)
  lx=lx+' GROUP BY '+subs(lxgroup,3)
endif
lx=chrtran(lx,"'",'"')
select SYSSQL
repl SQL_MEM with lx
select SYSPRG
*
return

* ------------------------------------------------
* Crear formato de Listado
* -----------------------------------------------
Function sq3_lst
Parameter formatol
*
select (_tmp_dbf)
tmpl=formatol
create report &tmpl from (_tmp_dbf)
lx1='QLSTW/'+formatol+'.FRX'
lx2='QLSTW/'+formatol+'.FRT'
copy file QLSTW/ST3LQ.FRX to (lx1)
copy file QLSTW/ST3LQ.FRT to (lx2)
use &lx1 in 0 alias 'DESTINO' excl
lx1=formatol+'.FRX'
use &lx1 in 0 alias 'ORIGEN' excl
select DESTINO
go 16
repl EXPR with '"'+trim(syssql.SQL_NOM)+'"'
select ORIGEN
go top
col=0
pos_1=16875
pos_2=12708
do while .not. eof()
  scatter memvar
  if OBJTYPE=8
    lxexp=EXPR
    lxpic=PICTURE
    if lxexp<>'DATE()' .and. lxexp<>'_PAGENO'
      select SYSVAR
      seek upper(lxexp)
      vardes=iif(.not.eof(),left(VAR_DES,VAR_LN),lxexp)
      vardes='"'+trim(vardes)+'"'
      m.WIDTH=iif(.not.eof(),round(VAR_LN*750+410,2),m.WIDTH)
      _sqllen=m.WIDTH
      m.HPOS=col
      if col+m.WIDTH+250>78650
        col=0
        pos_1=pos_1+1770.333
        pos_2=pos_2-1666.667
      endif
      select DESTINO
      append blank
      gather memvar
      repl VPOS with pos_1,FONTFACE with 'Courier New',EXPR with lxexp,FONTSIZE with 8,HEIGHT with 1666.667,HPOS with col,STRETCH with .F.
      m.WIDTH=round((len(vardes)-2)*750+410,2)
      append blank
      gather memvar
      repl VPOS with pos_2,EXPR with vardes,FONTFACE with 'Times New Roman',OBJTYPE with 5,FONTSTYLE with 1,HEIGHT with 1666.667,HPOS with col
      select ORIGEN
      col=col+_sqllen+250
    endif
  endif
  skip
enddo
select ORIGEN
use
select DESTINO
for nd=19 to 23
  go nd
  scatter memvar
  delete record recno()
  append blank
  gather memvar
endfor
pack  
use
lx1=tmpl+'.FRX'
lx2=tmpl+'.FRT'
delete file (lx1)
delete file (lx2)
select SYSSQL
repl SQL_LST with formatol
select (_tmp_dbf)
_formato='QLSTW\'+formatol
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
modi report &_formato noenv
release pad _msm_edit of _MSYSMENU
release pad _msm_view of _MSYSMENU
pop menu _MSYSMENU
*
return .T.

* ------------------------------------------------
* Pulsar botón linterna
* -----------------------------------------------
Function sq3_lint
*
sqltot=0
ntot=memlines(sysprg.PRG_FICH)
dimension fco(ntot),fcd(1)
store '' to fco,fcd
select SYSFC
for nd=1 to memlines(sysprg.PRG_FICH)
  lx=mline(sysprg.PRG_FICH,nd)
  if !empty(lx)
    lx=left(subs(lx,3)+space(10),10)
    seek lx
    if !eof()
      sqltot=sqltot+1
      fco(sqltot)=lx+FC_NOM
    endif
  endif
endfor
select SYSSQL
if _sqltot>1
  bot_ok=0
  _psql=.F.
  _ok=st3trasp('Elegir fichero','fco',sqltot,'fcd',1)
  _psql=.T.
else
  _ok=.T.
  fcd(1)=fco(1)
endif
if _ok .and. !empty(fcd(1))
  lint_fc=trim(left(fcd(1),10))
  select (lint_fc)
  ntot=fcount()
  dimension fco(ntot+1),fcd(ntot+1)
  store '' to fco,fcd
  for nd=1 to ntot
    fco(nd)=field(nd)
  endfor
  select SYSVAR
  for nd=1 to ntot
    lx=left(fco(nd)+space(10),10)
    seek lx
    if !eof()
      fco(nd)=trim(lx)+' '+VAR_DES
    else
      fco(nd)=lx
    endif
  endfor
  select SYSSQL
  _psql=.F.
  bot_ok=0
  _ok=st3trasp('Elegir campos','fco',ntot+1,'fcd',ntot+1,'Exp.Manual','sq3_exman')
  _psql=.T.
  if _ok
    for nd=1 to ntot+1
      if !empty(fcd(nd))
        if left(fcd(nd),1)='#'
          _lxsele=_lxsele+'         '+fcd(nd)+','
        else
          _lxsele=_lxsele+'         '+lower(trim(lint_fc))+'.'+fcd(nd)+','
        endif
      endif
    endfor
  endif
endif
*
return

* ------------------------------------------------
* Expresión manual
* -----------------------------------------------
Function sq3_exman
*
lxvar=left(m_destino(ndds)+space(60),60)
if !empty(lxvar) .and. left(lxvar,1)='#'
  lxvar=subs(lxvar,3)
endif
_ok=f3_inp('Expresión manual','Expresión','lxvar',)
if _ok
  m_destino(ndds)='# '+lxvar
endif
st3trasp.ndds.setfocus
*
return

* ------------------------------------------------
* Buscar un campo para porcentajes
* -----------------------------------------------
Function sq3_campo
Parameter nd1
*
lx1=left(lx_sql,nd1)
n_campo=1
nd1=at(',',lx1)
do while nd1>0
  n_campo=n_campo+1
  lx1=subs(lx1,nd1+1)
  nd1=at(',',lx1)
enddo
*
return n_campo