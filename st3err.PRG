* Programa de control de errores de programa
*            'st3err'
*

if _xier<>0
  _nerrores=_nerrores+1
  if _nerrores>20
    if left(_usrcod,5)='PENTA'
      susp
    else
      cancel
    endif
  endif
  return
endif
_xier=error()
if type('_xerrac')<>'N'
  _xerrac=1
endif
if _xerrac=0
  return
endif
_alierr=alias()
do case
case _xier=155 .and. _nerrores<_intentos
  store 0 to _xier
  _nerrores=_nerrores+1
  retry
case _xier=155
  =f3_sn(1,4,'La impresora no responde','Existe algún problema con su impresora que impide su funcionamiento','Verifique su estado o su configuración de Windows')
*  lx=trim(sysprg.PRG_PROG)+'.release'
*  &lx
case _xier=1539
  =f3_sn(1,4,'Falló el desencadenante','Está intentando realizar una operación NO PERMITIDA por la','BASE DE DATOS')
*  lx=trim(sysprg.PRG_PROG)+'.release'
*  &lx
case _xier=1705
  _LxErr = Message() + cr + "PROGRAMA: " + Prog(1) + cr + "LINEA: " + Str(LineNo())
  Do Form St3Inc
  *** =f3_sn(1,4,'Alguien está utilizando en exclusiva el fichero ['+sysfc.FC_NOM+']')
case _xier=1707
  =f3_sn(1,4,'Estructura del índice errónea','Utilice la opción de Optimizar ficheros del menú de sistema')
case _xier=12 .and. atc('set order to ',message(1))>0
  =f3_sn(1,4,'Falta índice CODIGO','Utilice la opción de Optimizar ficheros del menú de sistema','y si persiste el problema llame a su instalador')
case _xier=1884
  =f3_sn(1,4,'Está intentando dar de alta 2 veces el mismo registro','en el fichero ['+_alierr+']')
*  lx=trim(sysprg.PRG_PROG)+'.release'
*  &lx
otherwise
  if left(_usrcod,5)='PENTA'
    _LxErr = Message() + cr + "PROGRAMA: " + Prog(1) + cr + "LINEA: " + Str(LineNo())
    Do Form St3Inc
    suspend
  else
    lx=trim(sysprg.PRG_PROG)+'.release'
    &lx
  endif
endcase
_nerrores=0
select alias(_alierr)
*
return
