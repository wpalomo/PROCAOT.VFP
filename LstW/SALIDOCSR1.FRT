   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    ;  %  winspool \\s01\HP LaserJet 4 LPT1:                                                      �\\s01\HP LaserJet 4              � 0C� 	 �
od   X  X  Letter                                                        ����                DINU"   0  ����                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      �DRIVER=winspool
DEVICE=\\s01\HP LaserJet 4
OUTPUT=LPT1:
ORIENTATION=1
PAPERSIZE=9
ASCII=9
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
      
F24lCodPro                     F24lCodPro+F24lCodArt          "@I"                           Courier New                    "Fecha"                        date()                         Courier New                    _pageno                                                       Courier New                    "999"                          sysprg.PRG_PROG                                               Courier New                    "@I"                           Courier New                    "P�gina"                       "@I"                           Courier New                    "Hora"                         time()                                                        Courier New                    _em                            Times New Roman                _nem                           Times New Roman                "@I"                           Times New Roman                ."Listado de Documentos de Salida por Art�culo"                  Courier New                    "Tipo"                         Courier New                    "N�mero"                       Courier New                    "Documento"                    Courier New                    "Pedida"                       Courier New                    	"Servida"                      Courier New                    "-------- Cantidades --------"                                  Courier New                    "Propietario"                  
F24lCodPro                                                    Courier New                    
F01pDescri                                                    Courier New                    Courier New                    !"----------  Cliente  ----------"                               Courier New                    "C�digo"                       Courier New                    "Descripci�n"                  
F24lTipDoc                                                    Courier New                    
F24lNumDoc                                                    Courier New                    $IIf(IsNull(F24lCanEnv),0,F24lCanEnv)                                                           Courier New                    $IIf(IsNull(F24lCanDoc),0,F24lCanDoc)                                                           Courier New                    "@I"                           Courier New                    	"TOTALES"                      
F24lCanDoc                                                    Courier New                    
F24lCanEnv                                                    Courier New                    
F24cDirAso                                                    Courier New                    
F24tNomAso                                                    Courier New                    "XXXXXXXXXXXXXXXXXXXX"         %IIf(IsNull(F24cFecDoc),'',F24cFecDoc)                                                          Courier New                    %IIf(IsNull(F24cFecPre),'',F24cFecPre)                                                          Courier New                    Courier New                    "Fecha"                        Courier New                    "Documento"                    Courier New                    "Fecha"                        Courier New                    
"Prevista"                     Courier New                    "Reservada"                    $IIf(IsNull(F24lCanRes),0,F24lCanRes)                                                           Courier New                    
F24lCanRes                                                    Courier New                    
F24lCodArt                                                    Courier New                    
F08cDescri                                                    Courier New                    "XXXXXXXXXXXXXXXXXXXXXXXXX"                                     Courier New                    "Art�culo"                    Courier New                                                   "Descripci�n"                 "@I"                           Courier New                                                   	"PEDIDOS"                      
F24lCodArt                                                    Courier New                    Courier New                    Courier New                    Courier New                    Times New Roman                Times New Roman                Courier New                    dataenvironment                KLeft = 10
Top = 240
Width = 400
Height = 134
Name = "Dataenvironment"
                       