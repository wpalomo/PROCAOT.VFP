* Programa que prepara un formato de listado para varios idiomas
*                       SY3LSI
*
Function sy3lsi
Parameter lsto,lst,_lst_idiom
*
_xidiomx=_xidiom
_xidiom=_lst_idiom
_ok=.T.
if file(lst+'.FRX')
  _ok=f3_sn(2,1,'Este formato ya existe','Desea recrearlo')
endif
if _ok=.T.
  copy file lsto+'.FRX' to lst+'.FRX'
  copy file lsto+'.FRT' to lst+'.FRT'
  lx=lst+'.FRX'
  use (lx) in 0 alias 'TMP_LST'
  select TMP_LST
  go top
  do while .not. eof()
    if OBJTYPE=5
      _linea=EXPR
      _comillas=left(_linea,1)
      _linea=f3_t(subs(_linea,2,len(_linea)-2))
      repl EXPR with _comillas+_linea+_comillas
    endif
    skip
  enddo
  use
endif
_xidiom=_xidiomx
*
return