* Programa de filtrado especial origen 
*
Function vubinueva
Parameters _programa,_forma

*
Do Case
   *> Cambio de ubicación en listas de trabajo.
   Case AllTrim(_programa)=='CAMUBI'
      Do Case
         Case _forma = 'LECTURA'
            Select AYUDA

            Delete All For F16cCodPro <> TRALIS.F26lCodPro .Or. ;
                           F16cCodArt <> TRALIS.F26lCodArt .Or. ;
                           F16cSitStk <> TRALIS.F26lSitStk .Or. ;
                           F16cCodUbi == TRALIS.F26lUbiOri .Or. ;
                           (F16cCanFis-F16cCanRes) < TRALIS.F26lCanFis
            Go Top
      EndCase

   *> Cambio de ubicación en cancelación de entradas.
   Case AllTrim(_programa)=='CAMUBIPAL'
      Do Case
         Case _forma = 'LECTURA'
            Select AYUDA
*            Delete All For F16cCodPro <> CANMOV.CodPro .Or. ;
*                           F16cCodArt <> CANMOV.CodArt .Or. ;
*                           F16cCodUbi =  CANMOV.CodUbi .Or. ;
*                           F16cSitStk <> CANMOV.SitStk
      EndCase
EndCase

*
Return
