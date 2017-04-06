*>
*> Control de ayudas sobre cantidad física en grids.
Function vHGCanFis
Parameters _programa, _forma

Do Case
   Case _programa='SALIPETM'
      Do Case
         Case _forma='LECTURA'
            Select AYUDA
            Delete For F16cCodArt # SALIPETM.CodArt
      EndCase
EndCase

*>
Return
