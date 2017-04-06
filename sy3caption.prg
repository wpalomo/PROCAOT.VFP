*>
*> Revisualizar el 'CAPTION' de la ventana principal.
Private _Titulo
_Titulo = Trim(_titprg) + ' [' + ;
          _em + Space(1) + ;
          trim(_nem) + '] ' + ;
          _usrcod + ' [' + _alma + Space(1) + ;
          Trim(_almades) + ']'

if !Empty(_Procaot)
   _Titulo = _Titulo + ' [' + _Procaot + Space(1) + Trim(_ProDesc) + ']'
endif

modify windows screen title _Titulo font 'Courier',10 noclose
