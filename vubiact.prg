* Programa de filtrado especial origen 
*
Function vubiact
Parameters _programa,_forma

*
Do Case
   *> Cambio de ubicación en listas de trabajo.
   Case _programa ='PROCMANU'
      Do Case
         Case _forma = 'LECTURA'
            Select AYUDA
            Delete All For F08cCodPro <> m.CodPro
      EndCase
EndCase

*
Return
