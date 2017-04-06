* Reglas de Integridad sobre una Base de Datos
*               ST3RI
*
* Autor Carlos Casals Fradera Vers 3.0 Mayo 1.997
*
Parameter _dbforma
*
_dbforma=upper(_dbforma)
if type('_integridad')='U'
  _integridad=.T.
  begin transaction
else
  return
endif
if wexist('ST3BROW')
  scatter memvar
endif
store 0 to _xier,_xerrac
_dbali=alias()
if type('_em')='U'
  scatter memvar
  private _em,_fich,_empfich
  _empfich=''
  _fich=dbf()
  nd=len(_fich)
  do while subs(_fich,nd,1)<>'\'
    nd=nd-1
  enddo
  lx=subs(_fich,nd+1)
  _fich=iif(nd>1,left(_fich,nd-1),'_fich')
  if len(lx)>7
    lx=left(right(lx,7),3)
    _em=iif(val(lx)>0,lx,'001')
  else
    _em='001'
  endif
endif
_db_estado=.T.
if !used('SYSFC')
  use sysfc order tag idx1 in 0
endif
* ---------------------------------------
* Encontrar lista de reglas de integridad
* Array _st3ri  	1	Fichero
*					2	Expresión de la clave
*					3	Valor de la clave
*					4	.T./.F. (Se ha abierto el fichero)
*					5	Clave antigua
* ---------------------------------------
_lx1=iif(len(_dbali)>4.and.val(subs(_dbali,5))>0,left(_dbali,4),_dbali)
_lx1=trim(_lx1)+','
_lx3='sysfc.FC_'+iif(_dbforma='E','DEL',iif(_dbforma='I','INS','MOD'))
select SYSFC
dimension _st3ri(20,5),_keycamp(20)
store '' to _st3ri,_keycamp
_num_ri=0
do while !empty(_lx1) .and. _db_estado=.T.
  nd1=at(',',_lx1)
  _lx2=left(_lx1,nd1-1)
  _lx1=iif(len(_lx1)>nd1,subs(_lx1,nd1+1),'')
  seek _lx2
  if !eof() .and. !empty(FC_DBC)
    for nd=1 to memlines(&_lx3) step 2
      _num_ri=_num_ri+1
      lx4=mline(&_lx3,nd)
      _forma=subs(lx4,3,1)
      _st3ri(_num_ri,1)=subs(lx4,11)           && fichero
      store '' to _st3ri(_num_ri,2),_st3ri(_num_ri,3)
      _key=mline(&_lx3,nd+1)
      _st3ri(_num_ri,4)=.F.
      _fic=_st3ri(_num_ri,1)
      if !used(_fic)
        fcfc=iif(empty(FC_FCS),_fic,trim(FC_FCS))
        fcfc=iif(len(fcfc)>4,fcfc,fcfc+_em)
        fcfc1=iif(empty(_empfich),'',_empfich+'\'+fcfc)
        fcfc=_fich+'\'+fcfc
        if !empty(fcfc1) .and. file(fcfc1+'.DBF')
          fcfc=fcfc1
        endif
        if file(fcfc+'.DBF')
          use (fcfc) alias (_fic) order tag CODIGO in 0
          _st3ri(_num_ri,4)=.T.
        else
          store '' to lx,lx1
          _dbestado=.F.
        endif
      endif
      nd_ri=at('[',_key)
      if nd_ri>0
        lx1=left(_key,nd_ri-1)
        _vkey=&lx1
        _oldkey=left(_key,nd_ri-1)
        _oldkey=oldval(_oldkey,_dbali)
        _key=subs(_key,nd_ri+1)
        nd_ri=at(']',_key)
        if nd_ri>0
          _key=left(_key,nd_ri-1)
        endif
      else
        _vkey=&_key
        _oldkey=oldval(_key,_dbali)
      endif
      _st3ri(_num_ri,2)=_key
      _st3ri(_num_ri,3)=_vkey
      _st3ri(_num_ri,5)=_oldkey
      if _forma='R'
        _st3ri(_num_ri,2)='#_R_#'
        select alias (_fic)
        if _dbforma='A'
          _db_estado=iif(_oldkey<>_vkey,.F.,.T.)
        else
          locate for &_key=_vkey
          if _dbforma='I'
            _db_estado=iif(eof(),.F.,.T.)
          else 
            _db_estado=iif(eof(),.T.,.F.)
          endif
        endif
        select SYSFC
      endif
    endfor 
  endif
enddo
if _db_estado=.T.                               && No hay restricciones
  for nd=1 to _num_ri
    if _st3ri(nd,2)<>'#_R_#'
      _fic=_st3ri(nd,1)
      _key=_st3ri(nd,2)
      _vkey=_st3ri(nd,3)
      _oldkey=_st3ri(nd,5)
      select (_fic)
      do case
      case _dbforma='E'
        seek _vkey
        do while !eof() .and. &_key=_vkey
          if rlock()
            delete record recno()
            seek _vkey
          else
            _db_estado=.F.
            exit
          endif
        enddo
      case _dbforma='A' .and. _vkey<>_oldkey
        lx1=_key
        _numkey=0
        do while !empty(lx1)
          _numkey=_numkey+1
          nd=at('+',lx1)
          if nd=0
            lx2=lx1
            lx1=''
          else
            lx2=left(lx1,nd-1)
            lx1=subs(lx1,nd+1)
          endif
          nd=at('(',lx2)
          lx2=iif(nd>0,subs(lx2,nd+1),lx2)
          nd=at(')',lx2)
          lx2=iif(nd>0,left(lx2,nd-1),lx2)
          _keycamp(_numkey)=lx2        
        enddo
        select alias(_fic)
        seek _oldkey
        do while !eof() .and. &_key=_oldkey
          if rlock()
            for nd=1 to _numkey
              lx1=_keycamp(nd)
              lx2=_dbali+'.'+lx1
              repl &lx1 with &lx2
            endfor
            unlock
            seek _oldkey
          else
            _db_estado=.F.
            exit
          endif
        enddo
      endcase  
    endif
  endfor
endif
if _db_estado=.F.
  rollback
else
  end transaction 
endif
_xerrac=1
for nd=1 to _num_ri
  if _st3ri(nd,4)=.T.
    _fic=_st3ri(nd,1)
    select (_fic)
    use
  endif
endfor
select (_dbali)
*
return _db_estado
