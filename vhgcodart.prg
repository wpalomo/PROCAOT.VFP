*>
*> Control de ayudas sobre código de artículo en grids.
Function vHGCodArt
Parameters _programa, _forma

Do Case
   *> Inventario de carga.
   Case _programa='INVCARG'
     Do Case
        Case _forma='LECTURA'
           Select AYUDA
           If !Empty(_Procaot)
              Delete For F08cCodPro <> _Procaot
           EndIf
           Go Top
     EndCase

   *> Edición de composición de artículos.
   Case _programa='FTCOMCOM'
     Do Case
        Case _forma='LECTURA'
           Select AYUDA
           If !Empty(_Procaot)
              Delete For F08cCodPro <> _Procaot
           EndIf
           Go Top
     EndCase
EndCase

*>
Return
