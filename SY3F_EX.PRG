* Funcion Hacer excluiivo un fichero (Se utiliza en SY3PRG)
*         SY3F_EX
*
if !empty(m_destino(ndds))
  lx2=iif(left(m_destino(ndds),1)=' ','*',' ')
  m_destino(ndds)=lx2+subs(m_destino(ndds),2)
  st3trasp.ndds.setfocus
endif
*
return
