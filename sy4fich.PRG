* Programa Modificar estructura ficheros
*
*   Array cmp     Campos del Fichero   (Descripción + Nombre)
*         cmpidx  Campos del Indice
*         itrfc   Ficheros Disponibles para interacciones
*
*                SY3FICH
*
select SYSFC
scatter memvar
fic=trim(FC_FC)
r1fic=recno()
lxfic='fich\'+iif(len(trim(fic))<=4,fic+_em,fic)
dimension cmpidx(15),ri_tipos(6)
ri_tipos(1)='A Restr. '
ri_tipos(2)='B Cascada'
ri_tipos(3)='B Restr. '
ri_tipos(4)='M Cascada'
ri_tipos(5)='M Restr. '
ri_tipos(6)=''
tmp_int=_tmp+sys(2015)
lx1=tmp_int+'.DBF'
copy file sysint.dbf to (lx1)
use (tmp_int) alias 'SYSINT' in 0
store space(30) to cmpidx
for nd=1 to memlines(FC_KEY)
  cmpidx(nd)=mline(FC_KEY,nd)
endfor
select SYSINT
go top
for nd=1 to memlines(sysfc.fc_ins) step 2
  lx1=mline(sysfc.fc_ins,nd)
  lx2=mline(sysfc.fc_ins,nd+1)
  repl INT_TIP with left(lx1,10),INT_FIC with subs(lx1,11,10),INT_EXP with lx2
  skip
endfor
for nd=1 to memlines(sysfc.fc_mod) step 2
  lx1=mline(sysfc.fc_mod,nd)
  lx2=mline(sysfc.fc_mod,nd+1)
  repl INT_TIP with left(lx1,10),INT_FIC with subs(lx1,11,10),INT_EXP with lx2
  skip
endfor
for nd=1 to memlines(sysfc.fc_del) step 2
  lx1=mline(sysfc.fc_del,nd)
  lx2=mline(sysfc.fc_del,nd+1)
  repl INT_TIP with left(lx1,10),INT_FIC with subs(lx1,11,10),INT_EXP with lx2
  skip
endfor
go top
* --------------------------------------
use (lxfic) alias 'TMP' in 0 excl
select TMP
numcmp=fcount()
maxcmp=numcmp+50
dimension cmp(maxcmp)
store space(28) to cmp
for nd=1 to numcmp
  nvar=field(nd)
  nvar=left(nvar+space(10),10)
  select SYSVAR
  seek nvar
  cmp(nd)=iif(eof(),space(18)+nvar,VAR_DES+' '+nvar)
  select TMP
endfor
store 0 to bot_ok
store .F. to mod_dbf,mod_cdx
do form sy3fich
store '' to lxdel,lxmod,lxins
select SYSINT
go top
do while .not. eof()
  if !empty(INT_TIP) .and. !empty(INT_FIC) .and. !empty(INT_EXP)
    do case
    case left(INT_TIP,1)='A'
      lxins=lxins+cr+INT_TIP+INT_FIC+cr+INT_EXP
    case left(INT_TIP,1)='B'
      lxdel=lxdel+cr+INT_TIP+INT_FIC+cr+INT_EXP
    case left(INT_TIP,1)='M'
      lxmod=lxmod+cr+INT_TIP+INT_FIC+cr+INT_EXP
    endcase
  endif
  skip
enddo
lxdel=iif(len(lxdel)=0,'',subs(lxdel,3))
lxins=iif(len(lxins)=0,'',subs(lxins,3))
lxmod=iif(len(lxmod)=0,'',subs(lxmod,3))
select SYSINT
use
tmp_int=tmp_int+'.DBF'
delete file (tmp_int)
select SYSFC
repl fc_ins with lxins,fc_del with lxdel,fc_mod with lxmod
* ---------------- CONDICIONES ORACLE
if FC_REMOTO=1
  lx1=''
  for nd=1 to memlines(FC_KEY)
    lx=mline(FC_KEY,nd)
    if left(lx,7)='CODIGO '
      lx1=subs(lx,8)
    endif
  endfor
  if !empty(lx1)
    store '' to fcclave,fcbottom,fcmover
    do while !empty(lx1)
      nd=at('+',lx1)
      if nd>0
        lx2=left(lx1,nd-1)
        lx1=subs(lx1,nd+1)
      else
        lx2=lx1
        lx1=''
      endif
      fcclave=fcclave+'+" AND "+'
      do case
      case left(lx2,4)='STR('
        varvar=left(subs(lx2,5,len(lx2)-5)+space(10),10)
        select SYSVAR
        seek varvar
        select SYSFC        
        fcclave=fcclave+'"'+subs(lx2,5,len(lx2)-5)+'="'
        fcclave=fcclave+'+str(m.'+subs(lx2,5,len(lx2)-5)+','+ltrim(str(sysvar.VAR_LN))+')'
        fcbottom=fcbottom+'||lpad(to_char('+subs(lx2,5,len(lx2)-5)+'),'+ltrim(str(sysvar.VAR_LN))+',"+_cm+" "+_cm+")'
        fcmover=fcmover+'+"||"+_cm+str(m.'+subs(lx2,5,len(lx2)-5)+','+ltrim(str(sysvar.VAR_LN))+')+_cm'
      case left(lx2,5)='DTOS('

* FALTA INDICE FECHA

      otherwise
        fcclave=fcclave+'"'+lx2+'="'
        fcclave=fcclave+'+_cm+m.'+lx2+'+_cm'
        fcbottom=fcbottom+'||'+lx2
        fcmover=fcmover+'+"||"+_cm+m.'+lx2+'+_cm'
      endcase
    enddo
    if len(fcclave)>9
      fcbottom=subs(fcbottom,3)
      fcmover=subs(fcmover,6)
      fcinit='"('+fcbottom+')=(SELECT MIN('+fcbottom+') FROM "+sql_fc+")"'
      fcant='"('+fcbottom+')=(SELECT MAX(('+fcbottom+')) FROM "+sql_fc+" WHERE ('+fcbottom+')<"'+fcmover+'+")"'
      fcpos='"('+fcbottom+')=(SELECT MIN(('+fcbottom+')) FROM "+sql_fc+" WHERE ('+fcbottom+')>"'+fcmover+'+")"'
      fcbottom='"('+fcbottom+')=(SELECT MAX('+fcbottom+') FROM "+sql_fc+")"'
      repl FC_CLAVE with subs(fcclave,10),FC_BOTTOM with fcbottom,FC_INIT with fcinit,FC_ANT with fcant,FC_POS with fcpos
    endif
  endif    
  sql_fc=trim(fc_fc)
  sql_fc=iif(len(sql_fc)=4,sql_fc+_em,sql_fc)
  nd=SQLCOLUMNS(_asql,sql_fc,'FOXPRO','SQL_RESULT')

  if nd>0
    store '' to fcinsert,fcupdate
    select SQL_RESULT
    go top
    do while !eof()
      fcupdate=fcupdate+'+","+'
      fcinsert=fcinsert+'+","+'
      varvar=left(trim(FIELD_NAME)+space(10),10)
      select SYSVAR
      seek varvar
      select SQL_RESULT
      varvar=trim(varvar)
      do case
      case FIELD_TYPE='C'
        fcupdate=fcupdate+'"'+varvar+'="+_cm+m.'+varvar+'+_cm'
        fcinsert=fcinsert+'_cm+m.'+varvar+'+_cm'
      case FIELD_TYPE='T'
        fcupdate=fcupdate+'"'+varvar+'=to_date("+_cm+dtoc(m.'+varvar+')+_cm+","+_cm+"dd-mm-yyyy"+_cm+")"'
        fcinsert=fcinsert+'"to_date("+_cm+dtoc(m.'+varvar+')+_cm+","+_cm+"dd-mm-yyyy"+_cm+")"'
      case FIELD_TYPE='N'
        if FIELD_DEC=0
          fcupdate=fcupdate+'"'+varvar+'="+str(m.'+varvar+','+ltrim(str(sysvar.VAR_LN))+')'
          fcinsert=fcinsert+'str(m.'+varvar+','+ltrim(str(sysvar.VAR_LN))+')'
        else
          fcupdate=fcupdate+'"'+varvar+'="+str(m.'+varvar+','+ltrim(str(sysvar.VAR_LN))+','+str(FIELD_DEC,1)+')'
          fcinsert=fcinsert+'str(m.'+varvar+','+ltrim(str(sysvar.VAR_LN))+','+str(FIELD_DEC,1)+')'
        endif
      endcase
      skip
    enddo
    use
    select SYSFC
    if !empty(fcupdate)
      fcupdate=subs(fcupdate,6)
      nd=at('"+"',fcupdate)
      do while nd>0
        fcupdate=stuff(fcupdate,nd,3,'')
        nd=at('"+"',fcupdate)
      enddo  
      fcinsert=subs(fcinsert,6)
      nd=at('"+"',fcinsert)
      do while nd>0
        fcinsert=stuff(fcinsert,nd,3,'')
        nd=at('"+"',fcinsert)
      enddo  
      repl FC_UPDATE with fcupdate,FC_INSERT with fcinsert
    endif
  endif
endif
*
return
