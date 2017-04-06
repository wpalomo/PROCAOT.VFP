*>
*> Funciones anexas a listado de inventario por ubicación.
*>
*> Recibe:
*>   cAccion   -----> Módulo que realiza la llamada.
*>                    'INICIO', al inicializar el formulario.
*>                    'ABRIR', al cargar el formulario.
*>                    'CERRAR', al descargar el formulario.
*>   cModo     -----> Parámetro no utilizado.
*>   cPantalla -----> Nombre del programa que realiza la llamada.
*>                    No utilizado en mantenimiento de artículos.

Parameters cAccion, cModo, cPantalla
Private cField, cFromF, cWhere, cOrder, cGroup
Local lStado, cSortFile, oUBI, oOCU

Do Case
   *> Cargar las ubicaciones del rango solicitado y sus ocupaciones.
   Case cAccion='INICIO'
      cField = "F10cCodUbi"
      cFromF = "F10c"
      cWhere = _GCSS("F10cCodUbi", 1, 4) + " ='" + _Alma + _cm + ;
               " And (" + _GCSS("F10cCodUbi", 5, 2) + " Between '" + ZonaD + "' And '" + ZonaH + "')" + ;
               " And (" + _GCSS("F10cCodUbi", 7, 2) + " Between '" + CalleD+ "' And '" + CalleH+ "')" + ;
               " And (" + _GCSS("F10cCodUbi", 9, 3) + " Between '" + FilaD + "' And '" + FilaH + "')" + ;
               " And (" + _GCSS("F10cCodUbi",12, 2) + " Between '" + PisoD + "' And '" + PisoH + "')" + ;
               " And (" + _GCSS("F10cCodUbi",14, 1) + " Between '" + ProfD + "' And '" + ProfH + "')"

      cOrder = "F10cCodUbi"
      cGroup = ""

      *> Cargar las ocupaciones.
      lStado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "XLUBICR")

      *> Cargar las ocupaciones.
      cField = "F16cCodPro, F16cCodArt, F08cDescri, " + ;
               "F16cTamPal, F16cNumPal, F16cNumLot, F16cSitStk, " + ;
               "F16cFecCad, F16cFecEnt, F16cTipDoc, F16cNumDoc, F16cCanFis, F16cCanRes"

      cFromF = "F16c,F08c"
      cOrder = ""

      Select XLUBICR
      Go Top
      Do While !Eof()
		 Scatter Name oUBI

         cWhere = "F16cCodPro Between '" + PropD + "' And '" + PropH + "' And " + ;
                  "F16cCodUbi='" + XLUBICR.F10cCodUbi + "' And " + ;
                  "F08cCodPro=F16cCodPro And F08cCodArt=F16cCodArt" 

         lStado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "XLUBICCUR")
         If lStado
            Select XLUBICCUR
            Go Top
            Do While !Eof()
	            Scatter Name oOCU
    	        Select XLUBIC
				Append Blank
				Gather Name oUBI
        	    Gather Name oOCU

	            Select XLUBICCUR
	            Skip
			EndDo
         EndIf

         Select XLUBICR
         Skip
      EndDo

      *> Clasificar el cursor de impresión.
      cSortFile = FnTemp(3)						&& Devuelve un DBF.
      Select XLUBIC
      Sort To (cSortFile) On F10cCodUbi /A, F16cCodPro /A, F16cCodArt /A, F16cNumPal /A, F16cNumLot /A, F16cSitStk /A, F16cFecCad /A
      Select XLUBIC
      Delete All
      Append From (cSortFile)
      Go Top

      Use In (Select(cSortFile))
      Use In (Select('XLUBICR'))
      Use In (Select('XLUBICCUR'))
      Delete File (cSortFile)
EndCase

Return
