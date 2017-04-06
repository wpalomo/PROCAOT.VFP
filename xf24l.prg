    
* Funciones generales asociadas a F24L-Detalle documentos salida.
*            XF24L
*
* Parámetros:
*   - _XEstado ----> Acción: 'MODI', 'ALTA', 'BAJA'
*   - _Antes ------> .T.: NO se ha realizado _XEstado,
*                    .F.: SI se ha realizado.
*   - _Programa ---> Programa que llama a la función.

*>
*> Modificaciones:
*>    - Números de línea, de diez en diez. AVC - 02.03.2000
*>

Parameter _xestado,_antes, _Programa

_Programa = Iif(empty(_Programa), _Progant, _Programa)
_xestado = Upper(_xestado)
_Programa = AllTrim(Upper(_Programa))

Do Case
   *> Edici¢n de Documentos de Salida.
   Case _Programa=='SALIDOCS'
      Do Case
         Case _Antes
            Select (_xfc2)
            If Eof()
               Append Blank
            EndIf
            Gather MemVar
            Replace All F24lFlgEst With '0' For Empty(F24lFlgEst)
            Go Top
            Scatter MemVar

            Select GSALIDOCS
            _r1 = Iif(Eof(), 0, RecNo())
            Replace All F24lFlgEst With '0' For Empty(F24lFlgEst)
            If Empty(_r1)
               Go Top
            Else
               Go _r1
            EndIf

            Do Case
               *> En altas, calcular el nº de línea.
               Case _XEstado='ALTA'
                   Select GSALIDOCS
                   Go Bottom
                   Valor = Iif(Eof(), 10, Val(F24lLinDoc) + 10)
                   m.F24lLinDoc=Right('0000' + AllTrim(Str(Valor)), 4)
               Case _XEstado='BAJA'
               Case _XEstado='MODI'
            EndCase
         Case !_Antes
            Do Case
               *> Grabar, si la hay, la línea de observaciones.
               Case _XEstado = 'ALTA' .Or. _XEstado = 'MODI'
                  Select GSALIDOCS
                  m.F24oCodPro=m.F24cCodPro
                  m.F24oTipDoc=m.F24cTipDoc
                  m.F24oNumDoc=m.F24cNumDoc
                  m.F24oLinObs=F24lLinDoc

                  DesObsD = F24lDesObs
                  ImpObsD = F24lImpObs

                  If Empty(DesObsD)
                     =f3_baja('F24O')
                  Else
                     _ok = f3_seek('F24O')
                     m.F24oDesObs = DesObsD
                     m.F24oImpObs = ImpObsD
                     m.F24o1erFlg = '0'
                     m.F24o2ndFlg = '0'
                     If _ok
                        =f3_upd('F24O')
                     Else
                        =f3_ins('F24O')
                     EndIf
                  EndIf
               *> Las bajas se tratan con On Delete Cascade.
               Case _XEstado='BAJA'
            EndCase
      EndCase

      *> Resto de programas.
      OtherWise
EndCase

*>
Return
