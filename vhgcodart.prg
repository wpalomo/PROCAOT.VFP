*>
*> Control de ayudas sobre c�digo de art�culo en grids.
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

   *> Edici�n de composici�n de art�culos.
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
