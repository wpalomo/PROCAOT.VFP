*
* Funciones generales asociadas a F18L-Detalle documentos entrada.
*            XF18L
*
* Parámetros:
*   - _XEstado ----> Acción: 'MODI', 'ALTA', 'BAJA'
*   - _Antes ------> .T.: NO se ha realizado _XEstado,
*                    .F.: SI se ha realizado.
*   - _Programa ---> Programa que llama a la función.

Parameters _xestado, _antes, _programa

_xestado=Upper(_xestado)
_programa = AllTrim(Upper(_programa))

Do Case
   *> Edición de Documentos de Entrada.
   Case _programa=='ENTRDOCE' 
      Do Case
         Case _antes
            Select (_xfc2)
            Append Blank
            Gather MemVar
            Replace F18lEstado With Iif(Empty(F18lEstado), '0', F18lEstado)
            Scatter MemVar

            Do Case
               *> En altas, calcular el Nº de línea.
               Case _xestado='ALTA'
                  Select GENTRDOCE
                  Go Bottom
                  valor=Iif(Eof(), 10, Val(F18lLinDoc) + 10)
                  m.F18lLinDoc = Right('0000' + AllTrim(Str(Valor)), 4)
                  m.F18lEstado = '0'
               Case _xestado='BAJA'
               Case _xestado='MODI'
            EndCase
  
         Case !_antes
            Do Case
               Case _xestado='ALTA'
               Case _xestado='BAJA'
               Case _xestado='MODI'
            EndCase
      EndCase

   *> Resto de programas.
   OtherWise
EndCase

*>
Return
