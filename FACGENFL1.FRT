   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    8    WINSPOOL HP LaserJet 4 LPT1:                            �HP LaserJet 4                    � @ g  	        	X  X                                                                                 @ MSUDOHP LaserJet 4                   �      <     d 
                    �DRIVER=WINSPOOL
DEVICE=HP LaserJet 4
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=9
COPIES=1
DEFAULTSOURCE=265
PRINTQUALITY=600
YRESOLUTION=600
TTOPTION=1
      
F70lNumFac                     
F70lCodCon                     
F70lCodSrv                     _pageno                                                       Courier New                    "999"                          Courier New                    "P�gina"                       Times New Roman                "Fecha"                        Times New Roman                "N�m.Factura"                  Times New Roman                "Propietario"                  Times New Roman                "NIF"                          Times New Roman                "Direcci�n"                    Times New Roman                "Poblaci�n"                    Times New Roman                "CP"                           Times New Roman                "Provincia"                    
F70cFecFac                                                    Courier New                    
F70cNumFac                                                    Courier New                    
F01pDescri                                                    Courier New                    
F01pNumNIF                                                    Courier New                    
F01pDirecc                                                    Courier New                    
F01pPoblac                                                    Courier New                    
F01pCodPos                                                    Courier New                    
F00jDescri                                                    Courier New                    Times New Roman                
"Unidades"                     
F70lTotLin                                                    Courier New                    5st3rel('F34N','[F34nCodCon=F70lCodCon]','F34NDescri')                                          Times New Roman                5st3rel('F34S','[F34sCodSrv=F70lCodSrv]','F34SDescri')                                          Times New Roman                5st3rel('F34S','[F34sCodSrv=F70lCodSrv]','F34SDescri')                                          Times New Roman                5st3rel('F34N','[F34nCodCon=F70lCodCon]','F34NDescri')                                          Times New Roman                Times New Roman                "Precio"                       Times New Roman                "Base Imponible"               Times New Roman                "%IVA"                         Times New Roman                
"Imp. IVA"                     Times New Roman                
"%Recargo"                     Times New Roman                "Imp. Recargo"                Times New Roman                "Total L�nea"                                                 "@J"                           Courier New                    V"Almacenes: T2 Opelog, S.A. C/Industria, 5308740 San Andr�s de la Barca (Barcelona) "           ls3img(f01plogo)                                              !Empty(F01pLogo)                                                                             
F70lTotEqv                                                    Courier New                    
F70lValEqv                                                    Courier New                    
F70lTotIva                                                    Courier New                    
F70lValIva                                                    Courier New                    
F70lBasImp                                                    Courier New                    
F70lPreUni                                                    Courier New                    
F70lUniCal                                                    Courier New                    Times New Roman                "Total"                        Times New Roman                "Total"                        sum(F70lTotLin)                                               Courier New                    sum(F70lTotEqv)                                               Courier New                    sum(F70lTotIva)                                               Courier New                    sum(F70lBasImp)                                               Courier New                    sum(F70lTotLin)                                               Courier New                    sum(F70lTotEqv)                                               Courier New                    sum(F70lTotIva)                                               Courier New                    sum(F70lBasImp)                                               Courier New                    Times New Roman                "Total Factura "               
F70cNumFac                                                    Times New Roman                sum(F70lTotLin)                                               Courier New                    sum(F70lTotEqv)                                               Courier New                    sum(F70lTotIva)                                               Courier New                    sum(F70lBasImp)                                               Courier New                    Times New Roman                "Base Imponible"               Times New Roman                "IVA"                          Times New Roman                
"Recargo"                     Times New Roman                "Total"                        Courier New                    Courier New                    Courier New                    Times New Roman                Courier New                    Times New Roman                dataenvironment                KLeft = 10
Top = 258
Width = 400
Height = 127
Name = "Dataenvironment"
                       