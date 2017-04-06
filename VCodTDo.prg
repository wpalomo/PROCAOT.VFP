
*> Control de ayudas en tipo documento.
*>
Function vCodTDo
Parameters _programa, _forma

Do Case
   *> Entrada directa de material.
   Case _programa ='ENTREMAT'
      Do Case
		 *> Eliminar los TDOCs que NO sean de entrada.
         Case _forma = 'LECTURA'
            Select AYUDA
            Delete All For SubStr(F00kCodDoc, 1, 1) # '1'
			Go Top
      EndCase
EndCase

Return
