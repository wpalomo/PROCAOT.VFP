* EVI 24/08/2006

PROCEDURE Conexion 

Set Exclusive Off

*>Leeremos un .INI común para todos los .EXE
cPath = 'CONEXION.INI'

_area=fopen(cPath, 0)

do while _area>0 .and. !feof(_area)
  lx=fget(_area)
  lx=alltrim(lx)
  if .not. empty(lx) .and. left(lx,1)<>'[' .and. left(lx,1)<>';'
    &lx
  endif
enddo

=FClose(_area)

*> Obtener conexión con BD  para el caso de llamada externa.
Open DataBase &_Conectar Shared
=Sys(3053)

_ASql = SqlConnect(_SqlCon)

Close Database

If _ASql <= 0
	=MessageBox('Conexión a Procaot no realizada ' ,64,'ALISFOUR')
	Return .F.
EndIf

*> Commit de posibles transacciones anteriores.
=SqlCommit(_ASql)

Return .T.

ENDPROC

PROCEDURE Desconexion

ret = SqlDisConnect(_ASql)

return ret

ENDPROC