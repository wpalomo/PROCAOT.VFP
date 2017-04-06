* Funcion Añadir linea a la barra de iconos (Se utiliza en SY3DUSR)
*         SY3U_LIN
*
if !empty(m_destino(ndds))
  lx2=iif(left(m_destino(ndds),1)='*',' ','*')
  m_destino(ndds)=lx2+subs(m_destino(ndds),2)
  st3trasp.ndds.setfocus
endif
*
return
