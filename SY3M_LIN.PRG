* Funcion Añadir linea (Se utiliza en SY3MENU)
*         SY3M_LIN
*
if !empty(m_destino(ndds))
  lx2=iif(subs(m_destino(ndds),2,1)='/',' ','/')
  m_destino(ndds)=stuff(m_destino(ndds),2,1,lx2)
  st3trasp.ndds.setfocus
endif
*
return
