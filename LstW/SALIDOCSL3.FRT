   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    ;  %  winspool \\S01\HP LaserJet 4 LPT1:                                                      �\\S01\HP LaserJet 4              � 4C� 	 �4d  X  X  A4                                                            ����                DINU"   4  ����                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       �DRIVER=winspool
DEVICE=\\S01\HP LaserJet 4
OUTPUT=LPT1:
ORIENTATION=1
PAPERSIZE=9
ASCII=9
COPIES=1
DEFAULTSOURCE=257
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
                                     
F24lCodPro                     F24lCodPro+DToC(F24cFecPre)                                     1F24lCodPro+DToC(F24cFecPre)+F24lTipDoc+F24lNumDoc               "@I"                           Courier New                    "Fecha"                        date()                         Courier New                    _pageno                                                       Courier New                    "999"                          sysprg.PRG_PROG                                               Courier New                    "@I"                           Courier New                    "P�gina"                       "@I"                           Courier New                    "Hora"                         time()                                                        Courier New                    _em                            Times New Roman                _nem                           Times New Roman                
F24lCodArt                                                    Courier New                    
F08cDescri                                                    Courier New                    "XXXXXXXXXXXXXXXXXXXXXXXXX"                                     
F24lTipDoc                                                    Courier New                    
F24lNumDoc                                                    Courier New                    
F24lCanEnv                                                    Courier New                    
F24lCanDoc                                                    Courier New                    
F24lNumLot                                                    Courier New                    
F24cDirAso                                                    Courier New                    
F24lCanRes                                                    Courier New                    "@I"                           Times New Roman                4"Listado de Documentos de Salida por Fecha Prevista"            
F24tNomAso                                                    Courier New                    "XXXXXXXXXXXXXXXXXXXXXXXXX"                                     Courier New                    "CTC"                          Courier New                    "Tipo"                         Courier New                    "Fecha"                        Courier New                    "N�mero"                       Courier New                    "Documento"                    Courier New                    
"Art�culo"                     Courier New                    	"Cliente"                      Courier New                    "Descripci�n del art�culo"                                      Courier New                    "Documento"                    "@I"                           Courier New                    "Propietario"                  
F24lCodPro                                                    Courier New                    
F01pDescri                                                    Courier New                    Courier New                    "Nombre"                       "@I"                           Courier New                    "Fecha Prevista"               %IIf(IsNull(F24cFecPre),'',F24cFecPre)                                                          Courier New                    
F24cFecDoc                                                    Courier New                    Courier New                    "Pedida"                       Courier New                    	"Servida"                      Courier New                     "--------- Cantidades ---------"                                Courier New                    "Reservada"                    
F24lCanEnv                                                    Courier New                    
F24lCanDoc                                                    Courier New                    
F24lCanRes                                                    Courier New                    "@I"                           Courier New                    	"TOTALES"                      
F24lCanEnv                                                    Courier New                    
F24lCanDoc                                                    Courier New                    
F24lCanRes                                                    Courier New                    "@I"                           Courier New                    	"TOTALES"                      Courier New                    Courier New                    Courier New                    Times New Roman                Times New Roman                Courier New                    dataenvironment                KLeft = 10
Top = 240
Width = 400
Height = 134
Name = "Dataenvironment"
                       