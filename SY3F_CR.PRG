* Funcion Control de registros de un fichero (Se utiliza en SY3PRG)
*         SY3F_CR
*
if !empty(m_destino(ndds))
  lx2=iif(subs(m_destino(ndds),2,1)=' ','*',' ')
  m_destino(ndds)=left(m_destino(ndds),1)+lx2+subs(m_destino(ndds),3)
  st3trasp.ndds.setfocus
endif
*
return
