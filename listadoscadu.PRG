************************************************************************
*> Listado de Preaviso de Caducidad de Artículos.                      *
*> Primer paso: Generar datos a imprimir.                              *
*>                                                                     *
*> Recibe: CodProD ---> Propietario Inicial.                           *
*>         CodProH ---> Propietario Final.                             *
*>         CodArtD ---> Artículo Inicial.                              *
*>         CodArtH ---> Artículo Final.                                *
*>         lSoloC  ---> Todos o solo artículos fuera de caducidad.     *
*>         dDate   ---> Fecha de inicio cálculo.                       *
*>                                                                     *
*> Genera: XLCADA  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - CCPANCAD    ---> Listado preaviso caducidad.                    *
************************************************************************
Procedure GenCadA
Parameters CodProD, CodProH, CodArtD, CodArtH, lSoloC, dDate
Local cWhere, cFormF, cOrder, cGroup, cField
Local lEstado

*> Por defecto, solo lista artículos fuera de caducidad.
If Type('lSoloC')=='L'
   lSoloC = .T.
EndIf

If Type('dDate')=='D'
   dDate = Date()
EndIf

cField = "*"
cGroup = ""
cFromF = "F08c,F01p,F16c"
cOrder = "F08cCodPro, F08cCodArt, F16cCodUbi, F16cFecCad"
cWhere = "F08cCodPro Between '" + CodProD + "' And '" + CodProH + "' And " + ;
         "F08cCodArt Between '" + CodArtD + "' And '" + CodArtH + "' And " + ;
         "F08cCaduca='S' And " + ;
         "F01pCodigo='" + F08cCodPro + "' And " + ;
         "F16cCodPro=F08cCodPro And F16cCodArt=F08cCodArt"
If lSoloC
   cOrder = "F08cCodPro, F08cCodArt, F16cFecCad"
EndIf

lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "XLCADACUR")
If !lEstado
   Return .F.
EndIf

*> Eliminar, si cal, los artículos con caducidad correcta.
Select XLCADACUR
If lSoloC
   Delete For F16cFecCad > dDate + F08cNumDia
   Go Top
EndIf

*> Copiar datos a cursor de trabajo. Si no existe, se crea.
If !Used('XLCADA')
   =CrtCursor('XLCADACUR', 'XLCADA', 'C')
EndIf

Select XLCADA
Append From Dbf('XLCADACUR')
Use In XLCADACUR

Return

************************************************************************
*> Listado de Preaviso de Caducidad de Artículos.                      *
*> Segundo paso: Impresión de datos.                                   *
*>                                                                     *
*> Recibe: XLCADA  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - CCPANCAD    ---> Listado preaviso caducidad.                    *
************************************************************************
Procedure PrtCadA

   If Used('XLCADA')
      Select XLCADA
      Go Top
      _Label1 = "PREAVISO DE CADUCIDAD DE ARTICULOS"
      Do Form St3Imp2 With _Label1, 'CCPANCADL1', 'XLCADA'
   Else
      _LxErr = "No hay artículos para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLCADA')
      Use in XLCADA
   EndIf

Return

