* Programa de ayudas directo sobre un fichero
*     ST3HLPFCADO
*
*	Array _xbusca	1		Nombre del campo con alias (ayuda.<campo>)
*					2		Longitud
*					3		Descripción campo
*					4		Tipo
*					5,6,7,8	Filtros establecidos
*
Parameters _fic

Private cFic, objHelp

cFic = AllTrim(_fic)
objHelp = CreateObject('st3hlp', cFic, _ASql, _em)
=objHelp.doHelp()

*Select (objHelp.Source)
Scatter MemVar Memo

=objHelp.Release()
Release objHelp

Return
