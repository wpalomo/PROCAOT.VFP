* Funcion Opciones Manuales
* Se utiliza en SY3MENU
*
*
if !empty(m_destino(ndds))
  wait window f3_t('Atenci�n -> El elemento de destino est� ocupado')
  return .T.
endif
m.PRG_PRG=space(10)
m.PRG_DES=space(30)
tit_inp='Opciones Extras de Men�'
txt_inp='Programa/Descripci�n'
var_inp='m.PRG_PRG/m.PRG_DES'
fmt_inp='!!!!!!!!!!/'
do form st3inp
if bot_ok=1 .and. !empty(m.PRG_PRG) .and. !empty(m.PRG_DES)
  m_destino(ndds)='O '+m.PRG_PRG+' '+m.PRG_DES
  st3trasp.ndds.setfocus
endif
*
return
