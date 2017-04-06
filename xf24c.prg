*
* Funciones generales asociadas a F24C-Cabecera documentos salida.
*            XF24C
*
* Parámetros:
*   - _XEstado --> Acción: 'MODI', 'ALTA', 'BAJA'
*   - _Antes ----> .T.: NO se ha realizado _XEstado,
*                  .F.: SI se ha realizado.
*   - _Programa ---> Programa que llama a la función.

Parameter _xestado,_antes, _Programa

_Programa = Iif(empty(_Programa), _Progant, _Programa)
_xestado = Upper(_xestado)
_Programa = AllTrim(Upper(_Programa))

**********************************************************************************
*> Grabar los registros de Direcciones y Observaciones (Altas/Modificaciones).   *
*> Las bajas no sería necesario tratarlas aquí, pues las procesa ORACLE de forma *
*> automática con ON DELETE CASCADE, pero se tratan por compatibilidad con       *
*> entorno DBF.                                                                  *
**********************************************************************************

Do Case
   *> Edición de Documentos de Salida.
   Case _Programa=='SALIDOCS'
      Do Case
         Case _Antes
         Case !_Antes
            Do Case
               *> Bajas: Tratado con On Delete Cascade.
               Case _XEstado = 'BAJA'

               *> Guardar dirección asociada.
               Case _XEstado = 'ALTA' .Or. _XEstado = 'MODI'
                  Select F24t
                  Scatter MemVar Memo To DirC

                  m.F24tCodPro = m.F24cCodPro
                  m.F24tTipDoc = m.F24cTipDoc
                  m.F24tNumDoc = m.F24cNumDoc
                  _ok=f3_seek('F24T')

                  If _ok
                     Select F24t
                     Gather MemVar From DirC
                     Replace F24tCodPro With m.F24cCodPro, ;
                             F24tTipDoc With m.F24cTipDoc, ;
                             F24tNumDoc With m.F24cNumDoc
                     Scatter MemVar
                     =f3_upd('F24T')
                  Else
                     Select F24t
                     Delete All
                     Append From Array DirC
                     Replace F24tCodPro With m.F24cCodPro, ;
                             F24tTipDoc With m.F24cTipDoc, ;
                             F24tNumDoc With m.F24cNumDoc
                     Scatter MemVar
                     =f3_ins('F24T')
                  EndIf

                  *> Observaciones de cabecera---------------------------------------
                  *> Si no hay observaciones, borrar registro.
                  m.F24oCodPro=m.F24cCodPro
                  m.F24oTipDoc=m.F24cTipDoc
                  m.F24oNumDoc=m.F24cNumDoc
                  m.F24oLinObs='0000'

                  If Empty(DesObsC)
                     =f3_baja('F24O')
                  Else
                     _ok=f3_seek('F24O')
                     =DelNulls('F24o')

                     m.F24oDesObs = DesObsC
                     m.F24o1erFlg = '0'
                     m.F24o2ndFlg = '0'
                     m.F24oImpObs = ImpObsC
                     If _ok
                        =f3_upd('F24O')
                     Else
                        =f3_ins('F24O')
                     EndIf
                  EndIf

                  *> Observaciones de pie--------------------------------------------
                  *> Si no hay observaciones, borrar registro.
                  m.F24oCodPro=m.F24cCodPro
                  m.F24oTipDoc=m.F24cTipDoc
                  m.F24oNumDoc=m.F24cNumDoc
                  m.F24oLinObs='9999'

                  If Empty(DesObsP)
                     =f3_baja('F24O')
                  Else
                     _ok=f3_seek('F24O')
                     =DelNulls('F24o')

                     m.F24oDesObs = DesObsP
                     m.F24o1erFlg = '0'
                     m.F24o2ndFlg = '0'
                     m.F24oImpObs = ImpObsP
                     If _ok
                        =f3_upd('F24O')
                     Else
                        =f3_ins('F24O')
                     EndIf
                  EndIf
            EndCase
      EndCase                 && _Antes
EndCase                       && _Programa

*>
_xier = 0
Return

