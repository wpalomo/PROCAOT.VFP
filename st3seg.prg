* Programa de control de códigos de seguridad
*               ST3SEG
*
lx=_fich+'\EMPRESA'
_cerrar_emp=.F.
if !used('EMPRESA')
  _cerrar_emp=.T.
  use (lx) order tag CODIGO in 0
endif
select EMPRESA
do case
case eof()
  append blank
  repl EMP_COD with 1,EMP_NOM with 'DEMOSTRACION'
  _em='001'
  _nem='DEMOSTRACION'
case empty(_nem)
  go top
  _nem=EMP_NOM
  _em=right('000'+ltrim(left(str(EMP_COD),3)),3)
endcase
store '' to _titulo
store 0 to _ncam,_nusr,_npan,_nlst
nd=val(_em)
seek nd
select SYSPRG
do form st3seg
select EMPRESA
if _cerrar_emp=.T.
  use
endif
select SYSPRG
DO SY3USR
*
return
