
*> Enseñar ventana "Acerca de " PRODIS
Do Form frmAbout With _TITPRG, ;
                      _PROGRAM_VERSION, ;
                      _N_SERIE, ;
                      IIf(Type('_SERVICE_PACK') # "U", _SERVICE_PACK, ""), ;
                      _COPYRIGHT, ;
                      _TRADEMARK, ;
                      _LEGAL_NOTICE1 + Space(1) + ;
                      _LEGAL_NOTICE2 + Space(1) + ;
                      _LEGAL_NOTICE3, ;
                      _LOGO
