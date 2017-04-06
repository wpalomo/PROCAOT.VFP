*> Capçalera

   *> Descripció ................. Listados desde procesos
   *> Modul ...................... Listados.prg
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows'95
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 
   *> Data de fi ................. 

*> Notes
*>   - Traspasar procesos de impresión de albaranes a módulo SALIALBC. AVC - 07.11.2000
*>   - Añadir listado tarifas facturacion. AVC - 07.01.2002
*>   - Unificación sentencias SQL. AVC - 16.09.2002
*>

*> Módulo......................... EntrDocE1  (List. Documentos Entrada, por Artículo)
*>                                 EntrDocE2  (List. Documentos Entrada, por Documento)
*>                                 EntrDocE3  (List. Documentos Entrada, por Fecha Entrada)
*>                                 EntrDocE4  (List. Documentos Entrada, por Entidad)
*>                                 EntrMPNA1  (List. MP pendientes de asignar)
*>                                 EntrMPNU1  (List. MP pendientes de ubicar)
*>                                 EntrRECM1  (List. recuento mercancía)
*>                                 EntrCNFE1  (List. confirmación entradas)

*>                                 SaliDocS1  (List. Documentos Salida, por Artículo)
*>                                 SaliDocS2  (List. Documentos Salida, por Documento)
*>                                 SaliDocS3  (List. Documentos Salida, por Fecha)
*>                                 SaliDocS4  (List. Documentos Salida, por Nº Serie)
*>                                 SaliDocS5  (List. Documentos Salida, por Cliente)

*>                                 SaliMPNA1  (List. MP pendientes de asignar a lista)
*>                                 SaliRPNA1  (List. Reposiciones pendientes de asignar a lista)

*>                                 DevCEditL1 (Devoluciones de clientes)

*>                                 LDiaA      (List. Diario Salida, por Artículo)
*>                                 LDiaE      (List. Diario Salida, por Entidad)
*>                                 LDiaF      (List. Diario Salida, por Fecha)
*>                                 LDiaD      (List. Diario Salida, por Tipo y Número)

*>                                 GenPanLpr          (Listas de trabajo por Nº lista - Generar)
*>                                 PrtPanLpr          (Listas de trabajo por Nº lista - Imprimir)

*>                                 GenCadA            (Preaviso caducidad productos - Generar)
*>                                 PrtCadA            (Preaviso caducidad productos - Imprimir)

*>                                 LInvLRec           (Lista Inventario, recuento - generar)
*>                                 LInvLCtr           (Lista Inventario, control - generar)
*>                                 PrtLInvLRec        (Lista Inventario, recuento - imprimir)
*>                                 PrtLInvLCtr        (Lista Inventario, control - imprimir)

*>                                 GenAlrG            (Albaranes de transporte global - Generar)
*>                                 PrtAlrG            (Albaranes de transporte global - Imprimir)
*>                                 GenAlrD            (Albaranes de transporte detalle - Generar)
*>                                 PrtAlrD            (Albaranes de transporte detalle - Imprimir)
*>                                 GenAlrR            (Resumen de expediciones - Generar)
*>                                 PrtAlrR            (Resumen de expediciones - Imprimir)

*>                                 GenCBul            (Contenido bultos - Generar)
*>                                 CrtCBul            (Contenido bultos - Crear)
*>                                 PrtCBul            (Contenido bultos - Imprimir)
*>                                 PrtDBul            (Packing List     - Imprimir)

*>                                 GenParte           (Partes de montaje - Generar)
*>                                 PrtParte           (Partes de montaje - Imprimir)

*>								   GenParteCabeceras  (Cabeceras de Partes de montaje - Generar)
*> 								   PrtParteCabeceras  (Cabeceras de Partes de montaje - Imprimir)	

*> 								   GenCompArtPorComponente (Artículos que usan un componente - Generar)
*> 								   PrtArtPorComponente (Artículos que usan un componente - Imprimir)

*>                                 GenCompArt         (Ficha de composición de Articulos - Generar)
*>                                 PrtCompArt         (Ficha de composición de Articulos - Imprimir)

*>                                 GenPacking         (Packing List - Generar)
*>                                 PrtPacking         (Packing List - Imprimir)

*>           (C)Copyright ALISFOUR 2002 BARCELONA

#Define COPIA_CLIENTE 'COPIA PARA EL CLIENTE'
#Define COPIA_DISTRIBUCION 'ALMACEN DISTRIBUCION (ORDEN NUMERICO)'
#Define COPIA_SERVICIO 'SERVICIO CLIENTES (ORDEN NUMERICO)'

********************  DOCUMENTOS DE ENTRADA  *********************

******************************************************************
*  Listado Documentos de Entrada, por artículo                   *
*  Añadir selección proveedor. AVC - 24.10.2002                  *
*  Añadir selección detalle/cabecera. AVC - 24.10.2002           *
******************************************************************
Procedure EntrDocE1
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Private cFor, cRpt, cLbl
Local lStado, loImp2, cDesProveedor

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Crear cursor de trabajo.
=CrtMCursor([F18c,F18l,F08c,F01p,F01c], 'LSTDENT', 'C')

*> Campos y ficheros a recuperar.
lxSelect = "*"
lxFrom = "F18c,F18l,F08c,F01p"

*> Cláusula Where (enlaces de ficheros).
lxWhere = "F18lCodPro=F18cCodPro And F18lTipDoc=F18cTipDoc And F18lNumDoc=F18cNumDoc And " + ;
          "F18lCodPro=F08cCodPro And F18lCodArt=F08cCodArt And " + ;
          "F01pCodigo=F18lCodPro"

*> Cláusula Where (filtro de pantalla).
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf

If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

*> Cláusula where (rango de selección).
lxWhere = lxWhere + ;
          " And F18lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F18lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F18cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F18cFecPed Between " + FDoc_Ini + " And " + FDoc_Fin

*> Agregar rango entidad (proveedor).
If !Empty(m.Ini_CEnt) .Or. !Empty(m.Fin_CEnt)
   lxWhere = lxWhere + ;
             " And F18cTipEnt Between '" + m.Ini_TEnt + "' And '" + m.Fin_TEnt + "'" + ;
             " And F18cCodEnt Between '" + m.Ini_CEnt + "' And '" + m.Fin_CEnt + "'" + ;
             " And F01CTIPENT=F18cTipEnt And F01CCODIGO=F18cCodEnt "
   lxFrom = lxFrom + ",F01c"
EndIf

*> Cláusula Order.
lxOrder = "F18lCodPro, F18lCodArt, F18cTipDoc, F18cNumDoc"

lStado = f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LstDEnt')
If !lStado
   _LxErr = 'No se han podido cargar los documentos de entrada solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

*> Para obtener datos del proveedor.
Select LstDEnt
Go Top
Do While !Eof()
   m.F01cTipEnt = F18cTipEnt
   m.F01cCodigo = F18cCodEnt

   cDesProveedor = ''
   If f3_seek('F01c')
      cDesProveedor = F01c.F01cDescri
   EndIf

   Select LstDEnt
   Replace F01cDescri With cDesProveedor
   Skip
EndDo

Go Top

If cTipoListado=='1'
   cRpt = 'EntrDocEL1'
   cLbl = 'DOCUMENTOS ENTRADA POR ARTICULO - DETALLE'
Else
   cRpt = 'EntrDocER1'
   cLbl = 'DOCUMENTOS ENTRADA POR ARTICULO - RESUMEN'
EndIf

*> Selección por estado pedido de compra.
cFor = Iif(cEstadoDocumentos=='2', "F18cEstado='0'", ;
          Iif(cEstadoDocumentos=='3', "F18cEstado>'0' And F18cEstado<'3'", ;
             Iif(cEstadoDocumentos=='4', "F18cEstado='4'", ;
                Iif(cEstadoDocumentos=='5', "F18cEstado>'2'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDENT, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL.
If Used('LstDEnt')
   Use in LstDEnt
EndIf

Return

******************************************************************
*  Listado Documentos de Entrada, por documento                  *
*  Añadir selección proveedor. AVC - 24.10.2002                  *
*  Añadir selección detalle/cabecera. AVC - 24.10.2002           *
******************************************************************
Procedure EntrDocE2
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Private cFor, cRpt, cLbl
Local lStado, loImp2, cOldDoc, cCurDoc, cDesProveedor

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Crear cursor de trabajo.
=CrtMCursor([F18c,F18l,F08c,F01p,F01c], 'LSTDENT', 'C')

*> Campos y ficheros a recuperar.
lxSelect = "*"
lxFrom = "F18c,F18l,F08c,F01p"

*> Cláusula Where (enlaces de ficheros)
lxWhere = "F18lCodPro=F18cCodPro And F18lTipDoc=F18cTipDoc And F18lNumDoc=F18cNumDoc And " + ;
          "F18lCodPro=F08cCodPro And F18lCodArt=F08cCodArt And " + ;
          "F01pCodigo=F18lCodPro"

*> Cláusula Where (filtro de pantalla)
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

*> Cláusula where (rango de selección).
lxWhere = lxWhere + ;
          " And F18lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F18lTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F18lNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F18lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F18cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F18cFecPed Between " + FDoc_Ini + " And " + FDoc_Fin

*> Agregar rango entidad (proveedor).
If !Empty(m.Ini_CEnt) .Or. !Empty(m.Fin_CEnt)
   lxWhere = lxWhere + ;
             " And F18cTipEnt Between '" + m.Ini_TEnt + "' And '" + m.Fin_TEnt + "'" + ;
             " And F18cCodEnt Between '" + m.Ini_CEnt + "' And '" + m.Fin_CEnt + "'" + ;
             " And F01CTIPENT=F18cTipEnt And F01CCODIGO=F18cCodEnt "

   lxFrom = lxFrom + ",F01c"
EndIf

*> Cláusula Order.
lxOrder = "F18cCodPro, F18cTipDoc, F18cNumDoc"

lStado = f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LstDEnt')
If !lStado
   _LxErr = 'No se han podido cargar los documentos de entrada solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'EntrDocEL2'
   cLbl = 'DOCUMENTOS ENTRADA POR DOCUMENTO - DETALLE'
Else
   *> Eliminar registros duplicados.
   Store '' To cOldDoc, cCurDoc

   Select LstDEnt
   Go Top
   Do While !Eof()
      cCurDoc = F18cCodPro + F18cTipDoc + F18cNumDoc
      If cCurDoc==cOldDoc
         Delete Next 1
      EndIf

      cOldDoc = cCurDoc
      Skip
   EndDo
   Go Top

   cRpt = 'EntrDocER2'
   cLbl = 'DOCUMENTOS ENTRADA POR DOCUMENTO - RESUMEN'
EndIf

*> Para obtener datos del proveedor.
Select LstDEnt
Go Top
Do While !Eof()
   m.F01cTipEnt = F18cTipEnt
   m.F01cCodigo = F18cCodEnt

   cDesProveedor = ''
   If f3_seek('F01c')
      cDesProveedor = F01c.F01cDescri
   EndIf

   Select LstDEnt
   Replace F01cDescri With cDesProveedor
   Skip
EndDo

Go Top

*> Selección por estado pedido de compra.
cFor = Iif(cEstadoDocumentos=='2', "F18cEstado='0'", ;
          Iif(cEstadoDocumentos=='3', "F18cEstado>'0' And F18cEstado<'3'", ;
             Iif(cEstadoDocumentos=='4', "F18cEstado='4'", ;
                Iif(cEstadoDocumentos=='5', "F18cEstado>'2'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDENT, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL.
If Used('LstDEnt')
   Use in LstDEnt
EndIf
Return

******************************************************************
*  Listado Documentos de Entrada, por fecha entrada              *
*  Añadir selección proveedor. AVC - 24.10.2002                  *
*  Añadir selección detalle/cabecera. AVC - 24.10.2002           *
******************************************************************
Procedure EntrDocE3
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Private cFor, cRpt, cLbl
Local lStado, loImp2, cOldDoc, cCurDoc, cDesProveedor

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Crear cursor de trabajo.
=CrtMCursor([F18c,F18l,F08c,F01p,F01c], 'LSTDENT', 'C')

*> Campos y ficheros a recuperar.
lxSelect = "*"
lxFrom = "F18c,F18l,F08c,F01p"

*> Cláusula Where (enlaces de ficheros)
lxWhere = "F18lCodPro=F18cCodPro And F18lTipDoc=F18cTipDoc And F18lNumDoc=F18cNumDoc And " + ;
          "F18lCodPro=F08cCodPro And F18lCodArt=F08cCodArt And " + ;
          "F01pCodigo=F18lCodPro"

*> Cláusula Where (filtro de pantalla)
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

*> Cláusula where (rango de selección).
lxWhere = lxWhere + ;
          " And F18lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F18lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F18cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F18cFecPed Between " + FDoc_Ini + " And " + FDoc_Fin

*> Agregar rango entidad (proveedor).
If !Empty(m.Ini_CEnt) .Or. !Empty(m.Fin_CEnt)
   lxWhere = lxWhere + ;
             " And F18cTipEnt Between '" + m.Ini_TEnt + "' And '" + m.Fin_TEnt + "'" + ;
             " And F18cCodEnt Between '" + m.Ini_CEnt + "' And '" + m.Fin_CEnt + "'" + ;
             " And F01CTIPENT=F18cTipEnt And F01CCODIGO=F18cCodEnt "

   lxFrom = lxFrom + ",F01c"
EndIf

*> Cláusula Order.
lxOrder = "F18cCodPro, F18cFecPre, F18lCodPro, F18lTipDoc, F18lNumDoc, F18lCodArt"

lStado = f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LstDEnt')
If !lStado
   _LxErr = 'No se han podido cargar los documentos de entrada solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'EntrDocEL3'
   cLbl = 'DOCUMENTOS ENTRADA POR FECHA ENTREGA - DETALLE'
Else
   Store '' To cOldDoc, cCurDoc

   Select LstDEnt
   Go Top
   Do While !Eof()
      cCurDoc = F18cCodPro + DToC(F18cFecPed) + F18cTipDoc + F18cNumDoc
      If cCurDoc==cOldDoc
         Delete Next 1
      EndIf

      cOldDoc = cCurDoc
      Skip
   EndDo
   Go Top

   cRpt = 'EntrDocER3'
   cLbl = 'DOCUMENTOS ENTRADA POR FECHA ENTREGA - RESUMEN'
EndIf

*> Para obtener datos del proveedor.
Select LstDEnt
Go Top
Do While !Eof()
   m.F01cTipEnt = F18cTipEnt
   m.F01cCodigo = F18cCodEnt

   cDesProveedor = ''
   If f3_seek('F01c')
      cDesProveedor = F01c.F01cDescri
   EndIf

   Select LstDEnt
   Replace F01cDescri With cDesProveedor
   Skip
EndDo

Go Top

*> Selección por estado pedido de compra.
cFor = Iif(cEstadoDocumentos=='2', "F18cEstado='0'", ;
          Iif(cEstadoDocumentos=='3', "F18cEstado>'0' And F18cEstado<'3'", ;
             Iif(cEstadoDocumentos=='4', "F18cEstado='4'", ;
                Iif(cEstadoDocumentos=='5', "F18cEstado>'2'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDENT, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL.
If Used('LstDEnt')
   Use in LstDEnt
EndIf
Return

******************************************************************
*  Listado Documentos de Entrada, por Entidad                    *
******************************************************************
Procedure EntrDocE4
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Private cFor, cRpt, cLbl
Local lStado, loImp2, cOldDoc, cCurDoc, cDesProveedor

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Crear cursor de trabajo.
=CrtMCursor([F18c,F18l,F08c,F01p,F01c], 'LSTDENT', 'C')

*> Campos y ficheros a recuperar.
lxSelect = "*"
lxFrom = "F18c,F18l,F08c,F01p"

*> Cláusula Where (enlaces de ficheros)
lxWhere = "F18lCodPro=F18cCodPro And F18lTipDoc=F18cTipDoc And F18lNumDoc=F18cNumDoc And " + ;
          "F18lCodPro=F08cCodPro And F18lCodArt=F08cCodArt And " + ;
          "F01pCodigo=F18lCodPro And"

*> Cláusula Where (filtro de pantalla)
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

*> Cláusula where (rango de selección).
lxWhere = lxWhere + ;
          " F18cTipEnt Between '" + m.Ini_TEnt + "' And '" + m.Fin_TEnt + "'" + ;
          " And F18cCodEnt Between '" + m.Ini_CEnt + "' And '" + m.Fin_CEnt + "'" + ;
          " And F18lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F18lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F18cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F18cFecPed Between " + FDoc_Ini + " And " + FDoc_Fin

*> Agregar rango entidad.
If !Empty(m.Ini_CEnt) .Or. !Empty(m.Fin_CEnt)
   lxWhere = lxWhere + ;
             " And F18cTipEnt Between '" + m.Ini_TEnt + "' And '" + m.Fin_TEnt + "'" + ;
             " And F18cCodEnt Between '" + m.Ini_CEnt + "' And '" + m.Fin_CEnt + "'" + ;
             " And F01CTIPENT=F18cTipEnt And F01CCODIGO=F18cCodEnt "

   lxFrom = lxFrom + ",F01c"
EndIf

*> Cláusula Order.
lxOrder = "F18cCodPro, F18cTipEnt, F18cCodEnt, F18cTipDoc, F18cNumDoc, F18lCodArt"

lStado = f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LstDEnt')
If !lStado
   _LxErr = 'No se han podido cargar los documentos de entrada solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'EntrDocEL4'
   cLbl = 'DOCUMENTOS ENTRADA POR ENTIDAD - DETALLE'
Else
   Store '' To cOldDoc, cCurDoc

   Select LstDEnt
   Go Top
   Do While !Eof()
      cCurDoc = F18cCodPro + F18cTipEnt + F18cCodEnt + F18cTipDoc + F18cNumDoc
      If cCurDoc==cOldDoc
         Delete Next 1
      EndIf

      cOldDoc = cCurDoc
      Skip
   EndDo
   Go Top

   cRpt = 'EntrDocER4'
   cLbl = 'DOCUMENTOS ENTRADA POR ENTIDAD - RESUMEN'
EndIf

*> Para obtener datos de la entidad.
Select LstDEnt
Go Top
Do While !Eof()
   m.F01cTipEnt = F18cTipEnt
   m.F01cCodigo = F18cCodEnt

   cDesProveedor = ''
   If f3_seek('F01c')
      cDesProveedor = F01c.F01cDescri
   EndIf

   Select LstDEnt
   Replace F01cDescri With cDesProveedor
   Skip
EndDo

Go Top

*> Selección por estado pedido de compra.
cFor = Iif(cEstadoDocumentos=='2', "F18cEstado='0'", ;
          Iif(cEstadoDocumentos=='3', "F18cEstado>'0' And F18cEstado<'3'", ;
             Iif(cEstadoDocumentos=='4', "F18cEstado='4'", ;
                Iif(cEstadoDocumentos=='5', "F18cEstado>'2'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDENT, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL.
If Used('LstDEnt')
   Use in LstDEnt
EndIf
Return

******************************************************************
*          Listado de movimientos pendientes de asignar          *
******************************************************************
PROCEDURE EntrMPNA

*> Campos y ficheros a recuperar
lxSelect = "Select F14c" + _em + ".*," + ;
           "       F08c" + _em + ".F08cDescri " + ;
           "From F14c" + _em + ", F08c" + _em

*> Cláusula Where (enlaces de ficheros)
lxSelect = lxSelect + " Where F08cCodPro=F14cCodPro And F08cCodArt=F14cCodArt"

*> Cláusula Where (filtro de pantalla)
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

_Filtro = " And F14cCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F14cNumEnt Between '" + m.Ini_NEn + "' And '" + m.Fin_NEn + "'" + ;
          " And F14cTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F14cNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F14cCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And " + _GCD(F14cFecDoc) + " Between " + FDoc_Ini + " And " + FDoc_Fin + ;
          " And " + _GCSS("F14cTipMov", 1, 1) + " = '1' And F14cUbiOri='" + Space(14)+ "'"

lxSelect = lxSelect + _Filtro

*> Cláusula Order By
lxSelect = lxSelect + " Order By F14cNumPal"
_xier = SqlExec(_aSql, lxSelect,'LstMPNA')
=SqlMoreResults(_aSql)

Do Case
   Case _xier <=0
      _LxErr = 'Error cargando datos para generar listado' + cr
      Do Form St3Inc
      _LxErr = ''
      Return
   OtherWise
      Select LstMPNA
      Go Top
      If Eof()
         _LxErr = 'No hay Movimientos Pendientes de asignar en el rango seleccionado' + cr
         Do Form St3Inc
         _LxErr = ''
         Return
      EndIf
EndCase

_Label1 = "MOVIMIENTOS PENDIENTES DE ASIGNAR"
Do Form St3Imp2 With _Label1, 'EntrMPNAL1', 'LstMPNA'

*> Cerrar cursor generado por SQL
If Used('LstMPNA')
   Use in LstMPNA
EndIf
Return

******************************************************************
*          Listado de movimientos pendientes de ubicar           *
******************************************************************
PROCEDURE EntrMPNU

*> Campos y ficheros a recuperar
lxSelect = "Select F14c" + _em + ".*," + ;
           "       F08c" + _em + ".F08cDescri, F01p" + _em + ".* " + ;
           "From F14c" + _em + ",F01p" + _em + ", F08c" + _em  
*> Cláusula Where (enlaces de ficheros)
lxSelect = lxSelect + " Where F14cCodPro=F08cCodPro And F14cCodArt=F08cCodArt And F01pCodigo=F14cCodPro And F01pCodigo=F08cCodPro"

*> Cláusula Where (filtro de pantalla)
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = GetCvtDate(_ENTORNO, _VERSION, _FecMin)
EndIf

_Filtro = " And F14cCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F14cNumEnt Between '" + m.Ini_NEn + "' And '" + m.Fin_NEn + "'" + ;
          " And F14cTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F14cNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F14cCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And " + _GCD(F14cFecDoc) + " Between " + FDoc_Ini + " And " + FDoc_Fin + ;
          " And " + _GCSS("F14cTipMov", 1, 1) + " = '1' And F14cUbiOri > '0'"

lxSelect = lxSelect + _Filtro

*> Cláusula Order By.
lxSelect = lxSelect + " Order By F14cNumPal"
_xier = SqlExec(_aSql, lxSelect,'LstMPNU')
=SqlMoreResults(_aSql)

Do Case
   Case _xier <=0
      _LxErr = 'Error cargando datos para generar listado' + cr
      Do Form St3Inc With .T.
      Return

   OtherWise
      Select LstMPNU
      Go Top
      If Eof()
         _LxErr = 'No hay Movimientos Pendientes de ubicar en el rango seleccionado' + cr
         Do Form St3Inc With .T.
         Return
      EndIf
EndCase

_Label1 = "MOVIMIENTOS PENDIENTES DE UBICAR"
Do Form St3Imp2 With _Label1, 'EntrMPNUL1', 'LstMPNU'

*> Cerrar cursor generado por SQL
If Used('LstMPNU')
   Use in LstMPNU
EndIf
Return

************************************************************************
*> Impresión de Movimientos Pendientes de ubicar.                      *
*> Primer paso: Generar datos del MP actual.                           *
*>                                                                     *
*> Recibe: _NumMvt ---> Nº de movimiento del MP.                       *
*>                                                                     *
*> Genera: XLMPNU ----> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - EntrMpNu ----> Listado de MPs no ubicados.                      *
************************************************************************
Procedure GenEntrMpNu
Parameters _NumMvt
Local _Selec, _From, _Where, _Order

   _Selec = "F14c" + _em + ".*, F26l" + _em + ".*, F08c" + _em + ".*, F05c" + _em + ".*"
   _From  = "F26c,F26l,F08c,F05c"
   _Order = "F26cNumLst, F26lNumLst, F26lUbiOri"
   _Where = "F26cNumLst='" + _NumLst + "' And " + ;
            "F26lNumLst=F26cNumLst And " + ;
            "F08cCodPro=F26lCodPro And F08cCodArt=F26lCodArt"

   If !f3_sql(_Selec, _From, _Where, _Order, '', 'PANLPRCUR')
      _LxErr = 'No se han podido cargar las listas de trabajo a imprimir' + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLPANLPR')
      =CrtCursor('PANLPRCUR', 'XLPANLPR', 'C')
   EndIf

   Select XLPANLPR
   Append From Dbf('PANLPRCUR')

Return .T.

************************************************************************
*> Procedimiento de impresión de MPs pendientes de ubicar.             *
*> Segundo paso: Imprimir los datos generados por GenEntrMpNu.         *
*>                                                                     *
*> Recibe: XLMPNU ----> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - EntrMpNu ----> Listado de MPs no ubicados.                      *
************************************************************************
Procedure PrtEntrMpNu

   If Used('XLPANLPR')
      Select XLPANLPR
      Go Top
      _Label1 = "IMPRESION DE DE LISTAS DE TRABAJO (POR Nº LISTA)"
      Do Form St3Imp2 With _Label1, 'CcPanLprL1', 'XLPANLPR'
   Else
      _LxErr = "No hay listas de trabajo para imprimir" + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLPANLPR')
      Use in XLPANLPR
   EndIf
   
*>
Return

**********************************************************


******************************************************************
* Listado Documentos de Salida, por artículo.                    *
* Tomar dirección de F24t, no de F22p. AVC - 03.08.1999          *
* Construcción dinámica de SQL. AVC - 24.09.2002                 *
* Agrupar expedidos y exportados como expedidos. AVC - 29.06.2010*
******************************************************************
PROCEDURE SaliDocS1
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Private cFor, cRpt, cLbl
Local loImp2, lStado

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Campos y ficheros a recuperar
lxSelect = " F24l" + _em + ".*," + ;
           " F24c" + _em + ".*," + ;
           " F08cDescri, F01pDescri, F24tNomAso"

*> Cláusula From (enlaces de ficheros)
lxFrom   = "F24l,F24c,F08c,F01p,F24t"

*> Cláusula Where (enlaces de ficheros)
lxWhere  = "       F24lCodPro=F24cCodPro" + ;
           " And   F24lTipDoc=F24cTipDoc" + ;
           " And   F24lNumDoc=F24cNumDoc" + ;
           " And   F24lCodPro=F08cCodPro" + ;
           " And   F24lCodArt=F08cCodArt" + ;
           " And   F24lCodPro=F01pCodigo" + ;
           " And   F24tCodPro=F24lCodPro" + ;
           " And   F24tTipDoc=F24lTipDoc" + ;
           " And   F24tNumDoc=F24lNumDoc"

*> Cláusula Order (enlaces de ficheros)
lxOrder = "F24lCodPro, F24lCodArt, F24lTipDoc, F24lNumDoc, F24lNumLot, F24cDirAso, F24cFecPre"

*> Filtro por fecha prevista inicial.
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha prevista final.
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento inicial.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento final.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

lxWhere = lxWhere + ;
          " And F24lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F24lTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F24lNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F24cDirAso Between '" + m.Ini_DAs + "' And '" + m.Fin_DAs + "'" + ;
          " And F24lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F24cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin

*-->
Store '' To lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
lStado = GetStringQry(_ENTORNO, _VERSION, 'LISTADOS', 'SALIDOCS1', @lxSelect, @lxFrom, @lxWhere, @lxOrder, @lxGroup)
If !lStado
   Do Form St3Inc With .T.
   Return
EndIf
*<--

If !f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LSTDSAL')
   Return
EndIf

Select LSTDSAL
Go Top
If Eof()
   _LxErr = 'No hay Documentos de Salida entre los límites solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'SaliDocSL1'
   cLbl = 'DOCUMENTOS SALIDA POR ARTICULO - DETALLE'
Else
   cRpt = 'SaliDocSR1'
   cLbl = 'DOCUMENTOS SALIDA POR ARTICULO - RESUMEN'
EndIf

*> Selección por estado pedido.
cFor = Iif(cEstadoDocumentos=='2', "F24cFlgEst='0'", ;
          Iif(cEstadoDocumentos=='3', "F24cFlgEst>'0' And F24cFlgEst<'3'", ;
             Iif(cEstadoDocumentos=='4', "F24cFlgEst>='6'", ;
                Iif(cEstadoDocumentos=='5', "F24cFlgEst='4'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDSAL, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL.
If Used('LSTDSAL')
   Use in LSTDSAL
EndIf

Return

******************************************************************
* Listado Documentos de Salida, por documento.                   *
* Tomar dirección de F24t, no de F22p. AVC - 03.08.1999          *
* Construcción dinámica de SQL. AVC - 24.09.2002                 *
* Agrupar expedidos y exportados como expedidos. AVC - 29.06.2010*
******************************************************************
PROCEDURE SaliDocS2
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Private cFor, cRpt, cLbl
Local loImp2, lStado, cOldDoc, cCurDoc

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Campos y ficheros a recuperar
lxSelect = " F24l" + _em + ".*," + ;
           " F24c" + _em + ".*," + ;
           " F08cDescri, F01pDescri, F24tNomAso"

lxFrom   = "F24l,F24c,F08c,F01p,F24t"

*> Cláusula Where (enlaces de ficheros)
lxWhere  = "       F24lCodPro=F24cCodPro" + ;
           " And   F24lTipDoc=F24cTipDoc" + ;
           " And   F24lNumDoc=F24cNumDoc" + ;
           " And   F24lCodPro=F08cCodPro" + ;
           " And   F24lCodArt=F08cCodArt" + ;
           " And   F24lCodPro=F01pCodigo" + ;
           " And   F24tCodPro=F24lCodPro" + ;
           " And   F24tTipDoc=F24lTipDoc" + ;
           " And   F24tNumDoc=F24lNumDoc"

lxOrder = "F24lCodPro, F24lTipDoc, F24lNumDoc, F24lCodArt, F24lNumLot"

*> Filtro por fecha prevista inicial.
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha prevista final.
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento inicial.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento final.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

LxWhere = lxWhere + ;
          " And F24lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F24lTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F24lNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F24cDirAso Between '" + m.Ini_DAs + "' And '" + m.Fin_DAs + "'" + ;
          " And F24lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F24cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin

*-->
Store '' To lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
lStado = GetStringQry(_ENTORNO, _VERSION, 'LISTADOS', 'SALIDOCS2', @lxSelect, @lxFrom, @lxWhere, @lxOrder, @lxGroup)
If !lStado
   Do Form St3Inc With .T.
   Return
EndIf
*<--

If !f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LSTDSAL')
   Return
EndIf

Select LSTDSAL
Go Top
If Eof()
   _LxErr = 'No hay Documentos de Salida entre los límites solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'SaliDocSL2'
   cLbl = 'DOCUMENTOS SALIDA POR DOCUMENTO - DETALLE'
Else
   *> Eliminar registros duplicados.
   Store '' To cOldDoc, cCurDoc

   Select LSTDSAL
   Go Top
   Do While !Eof()
      cCurDoc = F24cCodPro + F24cTipDoc + F24cNumDoc
      If cCurDoc==cOldDoc
         Delete Next 1
      EndIf

      cOldDoc = cCurDoc
      Skip
   EndDo
   Go Top

   cRpt = 'SaliDocSR2'
   cLbl = 'DOCUMENTOS SALIDA POR DOCUMENTO - RESUMEN'
EndIf

*> Selección por estado pedido.
cFor = Iif(cEstadoDocumentos=='2', "F24cFlgEst='0'", ;
          Iif(cEstadoDocumentos=='3', "F24cFlgEst>'0' And F24cFlgEst<'3'", ;
             Iif(cEstadoDocumentos=='4', "F24cFlgEst>='6'", ;
                Iif(cEstadoDocumentos=='5', "F24cFlgEst='4'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDSAL, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL
If Used('LSTDSAL')
   Use in LSTDSAL
EndIf

Return

******************************************************************
* Listado Documentos de Salida, por fecha prevista.              *
* Tomar dirección de F24t, no de F22p. AVC - 03.08.1999          *
* Construcción dinámica de SQL. AVC - 24.09.2002                 *
* Agrupar expedidos y exportados como expedidos. AVC - 29.06.2010*
******************************************************************
PROCEDURE SaliDocS3
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Local loImp2, lStado, cOldDoc, cCurDoc

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Campos y ficheros a recuperar
lxSelect = " F24l" + _em + ".*," + ;
           " F24c" + _em + ".*," + ;
           " F08cDescri, F01pDescri, F24tNomAso"

lxFrom   = "F24l,F24c,F08c,F01p,F24t"

*> Cláusula Where (enlaces de ficheros)
lxWhere  = "       F24lCodPro=F24cCodPro" + ;
           " And   F24lTipDoc=F24cTipDoc" + ;
           " And   F24lNumDoc=F24cNumDoc" + ;
           " And   F24lCodPro=F08cCodPro" + ;
           " And   F24lCodArt=F08cCodArt" + ;
           " And   F24lCodPro=F01pCodigo" + ;
           " And   F24tCodPro=F24lCodPro" + ;
           " And   F24tTipDoc=F24lTipDoc" + ;
           " And   F24tNumDoc=F24lNumDoc"

lxOrder = "F24cFecPre, F24cCodPro, F24cTipDoc, F24cNumDoc, F24lCodArt, F24lNumLot"

*> Filtro por fecha prevista inicial.
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha prevista final.
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento inicial.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento final.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

lxWhere = lxWhere + ;
          " And F24lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F24lTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F24lNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F24cDirAso Between '" + m.Ini_DAs + "' And '" + m.Fin_DAs + "'" + ;
          " And F24lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F24cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin

*-->
Store '' To lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
lStado = GetStringQry(_ENTORNO, _VERSION, 'LISTADOS', 'SALIDOCS3', @lxSelect, @lxFrom, @lxWhere, @lxOrder, @lxGroup)
If !lStado
   Do Form St3Inc With .T.
   Return
EndIf
*<--

If !f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LSTDSAL')
   Return
EndIf

Select LSTDSAL
Go Top
If Eof()
   _LxErr = 'No hay Documentos de Salida entre los límites solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'SaliDocSL3'
   cLbl = 'DOCUMENTOS SALIDA POR FECHA PREVISTA - DETALLE'
Else
   *> Eliminar registros duplicados.
   Store '' To cOldDoc, cCurDoc

   Select LSTDSAL
   Go Top
   Do While !Eof()
      cCurDoc = F24cCodPro + F24cTipDoc + F24cNumDoc
      If cCurDoc==cOldDoc
         Delete Next 1
      EndIf

      cOldDoc = cCurDoc
      Skip
   EndDo
   Go Top

   cRpt = 'SaliDocSR3'
   cLbl = 'DOCUMENTOS SALIDA POR FECHA PREVISTA - RESUMEN'
EndIf

*> Selección por estado pedido.
cFor = Iif(cEstadoDocumentos=='2', "F24cFlgEst='0'", ;
          Iif(cEstadoDocumentos=='3', "F24cFlgEst>'0' And F24cFlgEst<'3'", ;
             Iif(cEstadoDocumentos=='4', "F24cFlgEst>='6'", ;
                Iif(cEstadoDocumentos=='5', "F24cFlgEst='4'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDSAL, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release
  
*> Cerrar cursor generado por SQL
If Used('LstDSal')
   Use in LstDSal
EndIf

Return
******************************************************************
* Listado Documentos de Salida, por nº de serie.                 *
******************************************************************
PROCEDURE SaliDocS4
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Local loImp2, lStado

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Campos y ficheros a recuperar
lxSelect = " F24l" + _em + ".*," + ;
           " F24c" + _em + ".*," + ;
           " F24s" + _em + ".*," + ;
           " F08cDescri, F01pDescri, F24tNomAso"

lxFrom   = "F24l,F24c,F24s,F08c,F01p,F24t"

*> Cláusula Order By
lxOrder = "F24lCodPro, F24lCodArt,F24lTipDoc, F24lNumDoc, F24lLinDoc,F24lNumLot, F24cDirAso, F24cFecPre"

*> Cláusula Where (enlaces de ficheros)
lxWhere =  "       F24lCodPro=F24cCodPro" + ;
           " And   F24lTipDoc=F24cTipDoc" + ;
           " And   F24lNumDoc=F24cNumDoc" + ;
           " And   F24lCodPro=F08cCodPro" + ;
           " And   F24lCodArt=F08cCodArt" + ;
           " And   F24lCodPro=F01pCodigo" + ;
		   " And   F24lCodPro=F24sCodPro" + ;
		   " And   F24lTipDoc=F24sTipDoc" + ;
		   " And   F24lNumDoc=F24sNumDoc" + ;
		   " And   F24lLinDoc=F24sLinDoc" + ;
           " And   F24tCodPro=F24lCodPro" + ;
           " And   F24tTipDoc=F24lTipDoc" + ;
           " And   F24tNumDoc=F24lNumDoc"

*> Filtro por fecha prevista inicial.
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha prevista final.
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento inicial.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento final.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

lxWhere = lxWhere + ;
          " And F24lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F24lTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F24lNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F24cDirAso Between '" + m.Ini_DAs + "' And '" + m.Fin_DAs + "'" + ;
          " And F24lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F24sCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F24sTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F24sNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F24cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin

*-->
Store '' To lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
lStado = GetStringQry(_ENTORNO, _VERSION, 'LISTADOS', 'SALIDOCS4', @lxSelect, @lxFrom, @lxWhere, @lxOrder, @lxGroup)
If !lStado
   Do Form St3Inc With .T.
   Return
EndIf
*<--

If !f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LSTDSAL')
   Return
EndIf

Select LSTDSAL
Go Top
If Eof()
   _LxErr = 'No hay Documentos de Salida entre los límites solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CURSOR=LSTDSAL, RPT=SaliDocSL4])
=loImp2.Exec('?', [LBL=DOCUMENTOS SALIDA POR Nº DE SERIE])

loImp2.Release
  
*> Cerrar cursor generado por SQL
If Used('LstDSal')
   Use in LstDSal
EndIf
Return

******************************************************************
* Listado Documentos de Salida, por cliente. AVC - 08.11.2002    *
* Aquí NO se utilizará SYSQUERY.                                 *
* Agrupar expedidos y exportados como expedidos. AVC - 29.06.2010*
******************************************************************
PROCEDURE SaliDocS5
Parameters cEstadoDocumentos, cTipoListado

Private lxSelect, lxFrom, lxWhere, lxOrder, lxGroup
Local loImp2, lStado, cOldDoc, cCurDoc

If Type('cEstadoDocumentos') # 'C'
   cEstadoDocumentos = '1'
EndIf
If Type('cTipoListado') # 'C'
   cTipoListado = '1'
EndIf

*> Campos y ficheros a recuperar
lxSelect = " F24l" + _em + ".*," + ;
           " F24c" + _em + ".*," + ;
           " F08cDescri, F01pDescri, F24tNomAso"

lxFrom   = "F24l,F24c,F08c,F01p,F24t"

*> Cláusula Where (enlaces de ficheros)
lxWhere  = "       F24lCodPro=F24cCodPro" + ;
           " And   F24lTipDoc=F24cTipDoc" + ;
           " And   F24lNumDoc=F24cNumDoc" + ;
           " And   F24lCodPro=F08cCodPro" + ;
           " And   F24lCodArt=F08cCodArt" + ;
           " And   F24lCodPro=F01pCodigo" + ;
           " And   F24tCodPro=F24lCodPro" + ;
           " And   F24tTipDoc=F24lTipDoc" + ;
           " And   F24tNumDoc=F24lNumDoc"

lxOrder = "F24cCodPro, F24cDirAso, F24lTipDoc, F24lNumDoc, F24lCodArt, F24lNumLot"

*> Filtro por fecha prevista inicial.
If !Empty(m.Ini_FPr) .And. !IsNull(m.Ini_FPr)
   FPre_Ini = _GCD(m.Ini_FPr)
Else
   FPre_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha prevista final.
If !Empty(m.Fin_FPr) .And. !IsNull(m.Fin_FPr)
   FPre_Fin = _GCD(m.Fin_FPr)
Else
   FPre_Fin = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento inicial.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento final.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

LxWhere = lxWhere + ;
          " And F24lCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
          " And F24lTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
          " And F24lNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
          " And F24cDirAso Between '" + m.Ini_DAs + "' And '" + m.Fin_DAs + "'" + ;
          " And F24lCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
          " And F24cFecPre Between " + FPre_Ini + " And " + FPre_Fin + ;
          " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin

If !f3_sql(lxSelect, lxFrom, lxWhere, lxOrder, , 'LSTDSAL')
   Return
EndIf

Select LSTDSAL
Go Top
If Eof()
   _LxErr = 'No hay Documentos de Salida entre los límites solicitados' + cr
   Do Form St3Inc With .T.
   Return
EndIf

If cTipoListado=='1'
   cRpt = 'SaliDocSL5'
   cLbl = 'DOCUMENTOS SALIDA POR CLIENTE - DETALLE'
Else
   *> Eliminar registros duplicados.
   Store '' To cOldDoc, cCurDoc

   Select LSTDSAL
   Go Top
   Do While !Eof()
      cCurDoc = F24cCodPro + F24cDirAso + F24cTipDoc + F24cNumDoc
      If cCurDoc==cOldDoc
         Delete Next 1
      EndIf

      cOldDoc = cCurDoc
      Skip
   EndDo
   Go Top

   cRpt = 'SaliDocSR5'
   cLbl = 'DOCUMENTOS SALIDA POR CLIENTE - RESUMEN'
EndIf

*> Selección por estado pedido.
cFor = Iif(cEstadoDocumentos=='2', "F24cFlgEst='0'", ;
          Iif(cEstadoDocumentos=='3', "F24cFlgEst>'0' And F24cFlgEst<'3'", ;
             Iif(cEstadoDocumentos=='4', "F24cFlgEst>='6'", ;
                Iif(cEstadoDocumentos=='5', "F24cFlgEst='4'", "!Eof()"))))

*> Objeto impresión.
loImp2 = CreateObject('st3imp2', , [CR=LSTDSAL, FOR=&cFor, RPT=&cRpt])
=loImp2.Exec('?', [LBL=&cLbl])
loImp2.Release

*> Cerrar cursor generado por SQL
If Used('LSTDSAL')
   Use in LSTDSAL
EndIf

Return

******************************************************************
*> Listado de movimientos pendientes de asignar a lista          *
*> Permitir valores NULL en F24c. AVC - 13.12.2000               *
*> Cambiar Sqlexec por F3_sql(). AVC - 18.09.2002                *
******************************************************************
PROCEDURE SaliMPNA
Private cField, cFromF, cWhere, cOrder, cGroup
Local lEstado

If Used('SaliMPNA')
   Use In SaliMPNA
EndIf

*> Crear estructura del cursor de trabajo.
=CrtCursor('F14c', 'SALIMPNA', 'C')
=AddFldToCursor('SALIMPNA', [NAME=F14cUbiOr1,TYPE=C,LENGTH=14])
=AddFldToCursor('SALIMPNA', [NAME=F24cRutHab,TYPE=C,LENGTH=4])
=AddFldToCursor('SALIMPNA', [NAME=F24cDirAso,TYPE=C,LENGTH=13])
=AddFldToCursor('SALIMPNA', [NAME=F24cCodTra,TYPE=C,LENGTH=6])
=AddFldToCursor('SALIMPNA', [NAME=F08cDescri,TYPE=C,LENGTH=30])

*> Campos y ficheros a recuperar.
cField = "*"

cFromF = "F14c,F24c,F08c"

*> Cláusula Where (enlaces de ficheros)
cWhere =          "F08cCodPro=F14cCodPro And F08cCodArt=F14cCodArt And "
cWhere = cWhere + "F24cCodPro=F14cCodPro And F24cTipDoc=F14cTipDoc And F24cNumDoc=F14cNumDoc"

*> Filtro por fecha documento desde.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento hasta.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

cWhere = cWhere + " And F14cCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
                  " And F14cTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
                  " And F14cNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
                  " And F14cCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
                  " And F14cNumLst='" + Space(6) + "'" + ;
                  " And " + _GCSS("F14cTipMov", 1, 1) + "='2'" + ;
                  " And F24cRutHab Between '" + m.Ini_Rut + "' And '" + m.Fin_Rut + "'" + ;
                  " And F24cCodTra Between '" + m.Ini_Trp + "' And '" + m.Fin_Trp + "'" + ;
                  " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin

lEstado = f3_sql(cField, cFromF, cWhere, , , 'SaliMPNA')
If !lEstado
   _LxErr = 'No hay movimientos pendientes de asignar a lista en el rango seleccionado' + cr
   Do Form St3Inc With .T.
   Return
EndIf

Select SaliMPNA
Go Top

_Label1 = "RESERVAS PENDIENTES DE ASIGNAR A LISTA"
Do Form St3Imp2 With _Label1, 'SaliRESL1', 'SaliMPNA'

*> Cerrar cursor generado por SQL
If Used('SaliMPNA')
   Use in SaliMPNA
EndIf

Return

******************************************************************
*> Listado de reposiciones pendientes de asignar a lista         *
*> Cambiar Sqlexec por F3_sql(). AVC - 18.09.2002                *
******************************************************************
PROCEDURE SaliRPNA
Private cField, cFromF, cWhere, cOrder
Local lEstado

If Used('SaliRPNA')
   Use In SaliRPNA
EndIf

*> Cláusulas de la sentencia SQL.
cOrder = "F14A.F14cNumMov"
cField = "F14A.*, F14B.*, F24cRutHab, F24cDirAso, F24cCodTra, F08cDescri"
cFromF = "F14c" + _em + " F14A, F14c" + _em + " F14B, F24c" + _em + ", F08c" + _em
cWhere = "F08cCodPro=F14A.F14cCodPro And F08cCodArt=F14A.F14cCodArt And " + ;
         "F24cCodPro=F14A.F14cCodPro And F24cTipDoc=F14A.F14cTipDoc And F24cNumDoc=F14A.F14cNumDoc"

*> Filtro por fecha documento desde.
If !Empty(m.Ini_FDo) .And. !IsNull(m.Ini_FDo)
   FDoc_Ini = _GCD(m.Ini_FDo)
Else
   FDoc_Ini = _GCD(_FecMin)
EndIf

*> Filtro por fecha documento hasta.
If !Empty(m.Fin_FDo) .And. !IsNull(m.Fin_FDo)
   FDoc_Fin = _GCD(m.Fin_FDo)
Else
   FDoc_Fin = _GCD(_FecMin)
EndIf

cWhere = cWhere + " And F14A.F14cCodPro Between '" + m.Ini_Pro + "' And '" + m.Fin_Pro + "'" + ;
                  " And F14A.F14cTipDoc Between '" + m.Ini_TDo + "' And '" + m.Fin_TDo + "'" + ;
                  " And F14A.F14cNumDoc Between '" + m.Ini_NDo + "' And '" + m.Fin_NDo + "'" + ;
                  " And F14A.F14cCodArt Between '" + m.Ini_Art + "' And '" + m.Fin_Art + "'" + ;
                  " And F24cRutHab Between '" + m.Ini_Rut + "' And '" + m.Fin_Rut + "'" + ;
                  " And F24cCodTra Between '" + m.Ini_Trp + "' And '" + m.Fin_Trp + "'" + ;
                  " And F24cFecDoc Between " + FDoc_Ini + " And " + FDoc_Fin + ;
                  " And " + _GCSS("F14B.F14cTipMov", 1, 2) + "='30'" + ;
                  " And F14A.F14cNumMov=F14B.F14cNumMov-1" + ;
                  " And F14A.F14cNumLst='" + Space(6) + "'" + ;
                  " And F14A.F14cCodPro=F14B.F14cCodPro" + ;
                  " And F14A.F14cTipDoc=F14B.F14cTipDoc" + ;
                  " And F14A.F14cNumDoc=F14B.F14cNumDoc" + ;
                  " And F14A.F14cCodArt=F14B.F14cCodArt" + ;
                  " And F14A.F14cNumPal=F14B.F14cNumPal" + ;
                  " And F14A.F14cNumLot=F14B.F14cNumLot"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'SaliRPNA')
If !lStado
   _LxErr = 'No hay reposiciones pendientes de asignar a lista en el rango seleccionado' + cr
   Do Form St3Inc With .T.
   Return
EndIf

Select SaliRPNA
Go Top
_Label1 = "REPOSICIONES PENDIENTES DE ASIGNAR A LISTA"
Do Form St3Imp2 With _Label1, 'SaliREPL1', 'SaliRPNA'

*> Cerrar cursor generado por SQL.
If Used('SaliRPNA')
   Use in SaliRPNA
EndIf
Return

********************  LISTAS DE PREPARACION  *********************

******************************************************************
*                Listas de Preparación, por Nº de lista          *
******************************************************************
PROCEDURE SaLTILst1
Parameters LstD, LstH, Modo
Private cField, cFromF, cWhere, cOrder
Private loImp, cLbl
Local lEstado, cNomAso, cDesTra

If Used('ListLst')
   Use In ListLst
EndIf

If Empty(LstD) .Or. IsNull(LstD)
   LstD=space(6)
EndIf 

If Empty(LstH) .or. IsNull(LstH)
   LstH='ZZZZZZ'
EndIf 

*> Campos y ficheros a recuperar
cField = "F26lUbiOri,F26lUbiDes,F26lTipMov,F26lCodPro,F26lCodArt," + ;
         "F26lSitStk,F08cDescri,F08cTipUni," + ;
         "F26lOriRes,F26lNumLot,F26lNumPal,F26lNumMac,F26lFecCad,F26lCanFis," + ;
         "F05cNombre,F08cUniPac,F08cPacCaj," + ;
         "0 As Cajas, 0 As Unid," + ;
         "F26lTipDoc,F26lNumDoc,F26lFlag1,F26lTipLst,F26lNumLst,F26lCodOpe,F26lEstMov," + ;
         "'" + Space(35) + "' As F01tDescri, " + ;
         "'" + Space(35) + "' As F24tNomAso, " + ;
         "'" + Space(14) + "' As UbiPic, " + ;
         "'" + Space(60) + "' As Observ, " + ;
         "'" + Space(60) + "' As CapObs"

cFromF = "F26l,F05c,F08c"

cWhere = "F26lNumLst Between '" + LstD + "' And '" + LstH + "' And " + ;
         "F08cCodPro=F26lCodPro And F08cCodArt=F26lCodArt And " + ; 
         "F05cCodOpe=F26lCodOpe"

cOrder = "F26lNumLst, F26lCodOpe, F26lUbiOri, F26lCodArt, F26lNumLot, F26lTipDoc, F26lNumDoc"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'ListLst')

*> Cargar la ubicación de picking, para los MPs de preparación pendientes de reposición.
Select ListLst
Replace All UbiPic With Space(14)

Scan For SubStr(F26lTipMov, 1, 1)=='2'
   *> Calcular cajas y unidades sueltas.
   Replace Cajas With Int(F26lCanFis / (F08cUniPac * F08cPacCaj)), ;
           Unid  With Mod(F26lCanFis, (F08cUniPac * F08cPacCaj))

   *> Leer la dirección asociada.
   Store Space(1) To cNomAso
   m.F24tCodPro = F26lCodPro
   m.F24tTipDoc = F26lTipDoc
   m.F24tNumDoc = F26lNumDoc
   If f3_seek('F24t')
      cNomAso = F24t.F24tNomAso
   EndIf

   *> Leer el transportista.
   Store Space(1) To cDesTra
   m.F24cCodPro = F26lCodPro
   m.F24cTipDoc = F26lTipDoc
   m.F24cNumDoc = F26lNumDoc
   If f3_seek('F24c')
      m.F01tCodigo = F24c.F24cCodTra
      If f3_seek('F01t')
         cDesTra = F01t.F01tDescri
      EndIf
   EndIf

   *> Buscar reposiciones.
   _Selec =          "F14cCodPro='" + ListLst.F26lCodPro + "' And "
   _Selec = _Selec + "F14cTipDoc='" + ListLst.F26lTipDoc + "' And "
   _Selec = _Selec + "F14cNumDoc='" + ListLst.F26lNumDoc + "' And "
   _Selec = _Selec + "F14cCodArt='" + ListLst.F26lCodArt + "' And "
   _Selec = _Selec + "F14cNumLot='" + ListLst.F26lNumLot + "' And "
   _Selec = _Selec + "F14cSitStk='" + ListLst.F26lSitStk + "' And "
   _Selec = _Selec + "F14cUbiOri='" + ListLst.F26lUbiOri + "' And "
   _Selec = _Selec + "F14cFecCad=" + _GCD(ListLst.F26lFecCad) + " And "
   _Selec = _Selec + "F14cTipMov Like '35%' And "
   _Selec = _Selec + "F10cCodUbi=F14cUbiOri And F10cPickSn<>F14cOriRes"

   _From = "F14c,F10c"
   _Orden = ''

   If f3_sql("F14cNumMov As MvtOri ", _From, _Selec, _Orden, , 'ListLstDet')
      *> Con el MP origen, leer el destino.
      m.F14cNumMov = PadL(AllTrim(Str(Val(ListLstDet.MvtOri) + 1, 10, 0)), 10, '0')
      If f3_seek('F14c')
         Replace ListLst.UbiPic With F14c.F14cUbiOri
      EndIf
   EndIf

   Select ListLst
   Replace F24tNomAso With cNomAso
EndScan

*> Añadir línea de observaciones.
Select ListLst
Go Top
Do While !Eof()
   _Campos = '*'
   _Selec = "F24oCodPro='" + F26lCodPro + "' And " + ;
            "F24oTipDoc='" + F26lTipDoc + "' And " + ;
            "F24oNumDoc='" + F26lNumDoc + "' And " + ;
            "F24oLinObs='9999'"
   =f3_sql(_Campos, 'F24O', _Selec, , , 'F24oCur')

   Select ListLst
   Replace Observ With Iif(SubStr(F24oCur.F24oImpObs, 3, 1)=='S', ;
                           AllTrim(MLine(F24oCur.F24oDesObs, 1)), Space(60))

   _Campos = '*'
   _Selec = "F24oCodPro='" + F26lCodPro + "' And " + ;
            "F24oTipDoc='" + F26lTipDoc + "' And " + ;
            "F24oNumDoc='" + F26lNumDoc + "' And " + ;
            "F24oLinObs='0000'"
   =f3_sql(_Campos, 'F24O', _Selec, , , 'F24oCur')

   Select ListLst
   Replace CapObs With Iif(SubStr(F24oCur.F24oImpObs, 3, 1)=='S', ;
                           AllTrim(MLine(F24oCur.F24oDesObs, 1)), Space(60))
   Skip
EndDo

Select ListLst
Go Top
If Eof()
   _LxErr = 'No hay listas de trabajo para imprimir' + cr
   Do Form St3Inc With .T.
   Return
EndIf

Do Case
   *> Imprimir listas, agrupando MPs.
   Case Modo = '1'
      _Label1 = "LISTAS DE TRABAJO POR Nº DE LISTA (AGRUPADAS)"

   *> Imprimir listas, sin agrupar MPs.
   Otherwise
      _Label1 = "LISTAS DE TRABAJO POR Nº DE LISTA (DETALLE)"
EndCase

loImp2 = CreateObject('st3imp2', , [CR=ListLst, RPT=SaLTILstL1])
=loImp2.Exec('?', [LBL=&_Label1])
loImp2.Release

*> Cerrar cursores generados por SQL.
If Used('ListLst')
   Use in ListLst
EndIf

If Used('ListLstDet')
   Use in ListLstDet
EndIf

If Used('F24oCur')
   Use In F24oCur
EndIf

Return

******************************************************************
*                Listas de Preparación, por Documento            *
******************************************************************
PROCEDURE SaLTILst2
Parameters CodProI, CodProF, TIpDocI, TipDocF, NumDocI, NumDocF
Private lxSelect, lxFiltro, _Label1

*> Campos y ficheros a recuperar
lxSelect = "Select F26lUbiOri,f26lTipMov,f26lCodPro,f26lCodArt,f08cdescri,f26lNumLot," + ;
                   "F26lfecCad,f26lCanfis,f26lTipDoc,f26lNumDoc,f26lFecDoc,f26lTipLst," + ;
                   "F26lNumLst,F26lCodOpe,F01pDescri" + ;
                   " From F26l" + _em + ", F08c" + _em + ", F01p" + _em

*> Cláusula Where (enlaces de ficheros)
 lxSelect = lxSelect + " Where F26lCodPro=F08cCodPro And F26lCodArt=F08cCodArt" + ;
                       "   And F26lCodPro=F01pCodigo"
                       
*> Cláusula Where (filtro de pantalla)
lxFiltro = " And F26lCodPro Between '" + CodProI + "' And '" + CodProF + "'" 
lxFiltro = lxFiltro + " And F26lTipDoc Between '" + TipDocI + "' And '" + TipDocF + "'" 
lxFiltro = lxFiltro + " And F26lNumDoc Between '" + NumDocI + "' And '" + NumDocF + "'" 

lxSelect = lxSelect + lxFiltro

*> Cláusula Order By
lxSelect = lxSelect + " Order By F26lCodPro,F26lTipDoc,F26lNumDoc,F26lFecDoc,F26lNumLst,F26lCodOpe,"+;
                      "F26lUbiOri,F26lCodArt,F26lNumLot"

=f3_SqlExec(_aSql, lxSelect,'ListLst')
  
_Label1 = "IMPRESION DE LISTAS POR Nº DOCUMENTO"
Do Form St3Imp2 With _Label1, 'SaLTILstL2', 'ListLst'

*> Cerrar cursor generado por SQL
If Used('ListLst')
   Use In ListLst
EndIf
Return

************************************************************************
*> Impresión de Listas de Trabajo.                                     *
*> Primer paso: Generar datos de la lista actual.                      *
*>                                                                     *
*> Recibe: _NumLst ---> Nº de Lista de Trabajo.                        *
*>                                                                     *
*> Genera: XLPANLPR --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - CcPanLpr ----> Consulta de listas de trabajo.                   *
************************************************************************
Procedure GenPanLpr
Parameters _NumLst
Local _Selec, _From, _Where, _Order

   _Selec = "F26c" + _em + ".*, F26l" + _em + ".*, F08c" + _em + ".*, F05c" + _em + ".*," + ;
            "LPad(' ', 14) As UbiPic, LPad(' ', 60) As Observ"

   _Selec = '*'
   _From  = "F26c,F26l,F08c,F05c" + _em
   _Order = "F26cNumLst, F26lNumMac, F26lNumLst, F26lUbiOri"
   _Where = "F26cNumLst='" + _NumLst + "' And " + ;
            "F26lNumLst=F26cNumLst And " + ;
            "F05cCodOpe=F26cCodOpe And " + ;
            "F08cCodPro=F26lCodPro And F08cCodArt=F26lCodArt"

   If !f3_sql(_Selec, _From, _Where, _Order, '', 'PANLPRCUR')
      _LxErr = 'No se han podido cargar las listas de trabajo a imprimir' + cr
      Do Form St3Inc With .T.
      Return .F.
   EndIf

   *> Agregar campos a rellenar con posterioridad.
   =AddFldToCursor('PANLPRCUR', [NAME=UbiPic,TYPE=C,LENGTH=14])
   =AddFldToCursor('PANLPRCUR', [NAME=Observ,TYPE=C,LENGTH=60])

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLPANLPR')
      =CrtCursor('PANLPRCUR', 'XLPANLPR', 'C')
   EndIf

   Select XLPANLPR
   Append From Dbf('PANLPRCUR')

   Use In PANLPRCUR

Return .T.

************************************************************************
*> Procedimiento de impresión de Listas de Trabajo por nº de lista.    *
*> Segundo paso: Imprimir los datos generados por GenPanLpr.           *
*>                                                                     *
*> Recibe: XLPANLPR --> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - CcPanLpr ----> Consulta de Listas de Trabajo.                   *
************************************************************************
Procedure PrtPanLpr

Private loImp

   If Used('XLPANLPR')
      Select XLPANLPR

      *> Obtener la ubicación de picking para aquellos MPs de preparación con reposiciones.
      Select XLPANLPR
      Replace All UbiPic With Space(14)

      Scan For SubStr(F26lTipMov, 1, 1)=='2'
         _Selec =          "F14cCodPro='" + XLPANLPR.F26lCodPro + "' And "
         _Selec = _Selec + "F14cTipDoc='" + XLPANLPR.F26lTipDoc + "' And "
         _Selec = _Selec + "F14cNumDoc='" + XLPANLPR.F26lNumDoc + "' And "
         _Selec = _Selec + "F14cCodArt='" + XLPANLPR.F26lCodArt + "' And "
         _Selec = _Selec + "F14cNumLot='" + XLPANLPR.F26lNumLot + "' And "
         _Selec = _Selec + "F14cSitStk='" + XLPANLPR.F26lSitStk + "' And "
         _Selec = _Selec + "F14cUbiOri='" + XLPANLPR.F26lUbiOri + "' And "
         _Selec = _Selec + "F14cFecCad=" + _GCD(XLPANLPR.F26lFecCad) + " And "
         _Selec = _Selec + "F14cTipMov Like '35%' And "
         _Selec = _Selec + "F10cCodUbi=F14cUbiOri and F10cPickSn<>F14cOriRes"

         _From = "F14c,F10c"
         _Orden = ''

         *> Leer el MP de salida.
         If f3_sql("F14cNumMov As MvtOri", _From, _Selec, _Orden, , 'XLPANLPRDet')
            *> Con el MP origen, leer el destino.
            m.F14cNumMov = PadL(AllTrim(Str(Val(XLPANLPRDet.MvtOri) + 1, 10, 0)), 10, '0')
            If f3_seek('F14C')
               Replace XLPANLPR.UbiPic With F14c.F14cUbiOri
            EndIf
         EndIf

         *>
         Select XLPANLPR
      EndScan

      *> Añadir línea de observaciones.
      Select XLPANLPR
      Go Top
      Do While !Eof()
         _Campos = '*'
         _Selec = "F24oCodPro='" + F26lCodPro + "' And " + ;
                  "F24oTipDoc='" + F26lTipDoc + "' And " + ;
                  "F24oNumDoc='" + F26lNumDoc + "' And " + ;
                  "F24oLinObs='9999'"

         =!f3_sql(_Campos, 'F24O', _Selec, , , 'F24oCur')
         Select XLPANLPR
         Replace Observ With Iif(SubStr(F24oCur.F24oImpObs, 1, 1)=='S', ;
                                 AllTrim(MLine(F24oCur.F24oDesObs, 1)), Space(60))
         Skip
      EndDo

      Go Top
      _Label1 = "IMPRESION DE DE LISTAS DE TRABAJO (POR Nº LISTA)"

      loImp2 = CreateObject('st3imp2', , [CR=XLPANLPR, RPT=CcPanLprL1])
      =loImp2.Exec('?', [LBL=&_Label1])
      loImp2.Release
   Else
      _LxErr = "No hay listas de trabajo para imprimir" + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('XLPANLPR')
      Use in XLPANLPR
   EndIf
   If Used('XLPANLPRDET')
      Use in XLPANLPRDET
   EndIf
   
   If Used('F24oCur')
      Use In F24oCur
   EndIf

Return

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
Local cOldProd

*> Por defecto, solo lista artículos fuera de caducidad.
If Type('lSoloC')=='L'
   lSoloC = .T.
EndIf

If Type('dDate')<>'D'
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

*> Poner a cero dias caducidad de las lineas duplicadas. Para el report.
cOldProd = Space(19)
Select XLCADACUR
Go Top
Do While !Eof()
   If F08cCodPro + F08cCodArt == cOldProd
      Replace F08cNumDia With 0
   EndIf
   cOldProd = F08cCodPro + F08cCodArt

   Skip
EndDo

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
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLCADA')
      Use in XLCADA
   EndIf

Return

************************************************************************
*> Impresión de Albaranes de Transporte - Global.                      *
*> Primer paso: Generar datos de la hoja de ruta actual.               *
*>                                                                     *
*> Recibe: _HojRut ---> Nº de Hoja de Ruta.                            *
*>                                                                     *
*> Genera: XLALRG  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaExAlr     ---> Impresión de Albaranes de Transporte.          *
*>                                                                     *
*> Añadir observaciones. AVC -10.07.2000                               *
*> Adaptar a multiplataforma. AVC - 07.02.2003                         *
************************************************************************
Procedure GenAlrG
Parameters _HojRut
Local lStado
Private _Selec, _From, _Where, _Order, _Group

*> Crear un cursor con la estructura de trabajo.
=CrtMCursor([F01p,F01t,F24c,F24t,F27c,F30c,F31c], 'ALRGCUR', 'C')
=AddFldToCursor('ALRGCUR', [NAME=Observ,TYPE=C,LENGTH=60])
=AddFldToCursor('ALRGCUR', [NAME=Detserv,TYPE=C,LENGTH=60])

_Selec = "Distinct F01pDescri, F01pNumNif, F01pDirecc, F01pPoblac, F01pNumTel, F01pNumFax, " + ;
         "F01tDescri, F01tNumNif, " + ;
         "F24tNomAso, F24t1ERDir, F24t2NDDir, F24tDPobla, F24tDProvi, F24tCodPos, " + ;
         "F24cDirAso, " + ;
         "F30cAlbRep, F30cCodEnt, F27cBultos, F27cPesoKg, F27cVolume, " + ;
         "F30cTipEnt, F30cCodEnt, F30cTipDoc, F30cNumDoc, " + ;
         "F27cDirAso, " + ;
         "F31cHojRut, F31cFecRut, F31cCodVeh"
 		   
_From  = "F01p,F01t,F24c,F24t,F27c,F30c,F31c"

_Where = "F31cHojRut='" + _HojRut + "' And " + ;
         "F30cHojRut=F31cHojRut And " + ;
         "F27cNumAlb=F30cAlbRep And " + ;
         "F24tCodPro=F27cCodPro And " + ;
         "F24tTipDoc=F27cTipDoc And " + ;
         "F24tNumDoc=F27cNumDoc And " + ;          
         "F24cCodPro=F27cCodPro And " + ;
         "F24cTipDoc=F27cTipDoc And " + ;
         "F24cNumDoc=F27cNumDoc And " + ;          
         "F01tCodigo=F30cCodTra And " + ;
         "F01pCodigo=F27cCodPro"

_Order = "F31cHojRut, F30cAlbRep, F30cNumDoc"
_Group = ""

lStado = f3_sql(_Selec, _From, _Where, _Order, _Group, 'ALRGCUR')

*> Copiar datos a cursor de trabajo. Si no existe, se crea.
If !Used('XLALRG')
   =CrtCursor('ALRGCUR', 'XLALRG', 'C')
EndIf

Select XLALRG
_r1 = RecCount()
Append From Dbf('ALRGCUR')

*> Añadir la primera línea de observaciones de la etiqueta de expedición.
Select XLALRG
Scan For RecNo() > _r1
   _Campos = '*'
   _Selec = "F24oCodPro='" + F30cCodEnt + "' And " + ;
            "F24oTipDoc='" + F30cTipDoc + "' And " + ;
            "F24oNumDoc='" + F30cNumDoc + "' And " + ;
            "F24oLinObs='0000'"

   =!f3_sql(_Campos, 'F24o', _Selec, , , 'F24oCur')
   Select XLALRG
   Replace Observ With Iif(SubStr(F24oCur.F24oImpObs, 6, 1)=='S', ;
                           AllTrim(MLine(F24oCur.F24oDesObs, 1)), Space(60))

   _Campos = '*'
   _Selec = "F24oCodPro='" + F30cCodEnt + "' And " + ;
            "F24oTipDoc='" + F30cTipDoc + "' And " + ;
            "F24oNumDoc='" + F30cNumDoc + "' And " + ;
            "F24oLinObs='9999'"

   =!f3_sql(_Campos, 'F24o', _Selec, , , 'F24oCur')

   Select XLALRG
   Replace Detserv With Iif(SubStr(F24oCur.F24oImpObs, 6, 1)=='S', ;
                           AllTrim(MLine(F24oCur.F24oDesObs, 1)), Space(60))
EndScan

If Used('F24oCur')
   Use In F24oCur
EndIf
If Used('ALRGCUR')
   Use In ALRGCUR
EndIf

Return

************************************************************************
*> Impresión de Albaranes de Transporte - Global.                      *
*> Segundo paso: Imprimir los datos generados por GenAlrG.             *
*>                                                                     *
*> Recibe: XLALRG ----> Cursor con los datos a imprimir.               *
*>         _Report ---> Nombre del report de salida.                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaExAlr     ---> Impresión de Albaranes de Transporte.          *
************************************************************************
Procedure PrtAlrG
Parameters _Report
Local loImp

   Set NullDisplay To Space(1)

   If Used('XLALRG')
      Select XLALRG
      Go Top
      _Label1 = "IMPRESION DE ALBARANES DE TRANSPORTE (GLOBAL)"

      loImp2 = CreateObject('st3imp2', , [CR=XLALRG, RPT=&_Report])
      =loImp2.Exec('?', [LBL=&_Label1])
      loImp2.Release
   Else
      _LxErr = "No hay albaranes de transporte para imprimir" + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLALRG')
      Use in XLALRG
   EndIf

   Set NullDisplay To
   
Return

************************************************************************
*> Impresión de Albaranes de Transporte - Detalle.                     *
*> Primer paso: Generar datos de la hoja de ruta actual.               *
*>                                                                     *
*> Recibe: _HojRut ---> Nº de Hoja de Ruta.                            *
*>                                                                     *
*> Genera: XLALRD  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaExAlr     ---> Impresión de Albaranes de Transporte.          *
************************************************************************
Procedure GenAlrD
Parameters _HojRut
Private _Selec, _Campos

*> Cargar el detalle de la Hoja de ruta.
_Campos = '*'
_Selec = "F31lHojRut='" + _HojRut + "'"
If !f3_sql(_Campos, ;
           'F31L', ;
           _Selec, ;
           'F31lHojRut,F31lAlbRep', ;
           '', ;
           'HOJDET')
   _LxErr = 'No se han podido cargar los albaranes de esta Hoja de Ruta' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select HOJDET
Go Top
If Eof()
   _LxErr = 'Esta Hoja de Ruta no contiene albaranes' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Cargar el detalle de cada albarán.
Select HOJDET
Go Top

Do While !Eof()
   _Campos = 'F27l' +_em + '.*, F08cDescri'
   _Selec = "F27lNumAlb='" + HOJDET.F31lAlbRep + "' And " + ;
            "F08cCodPro=F27lCodPro And F08cCodArt=F27lCodArt"

   =!f3_sql(_Campos, ;
            'F27L,F08C', ;
            _Selec, ;
            'F27lCodPro,F27lCodArt,F27lNumLot', ;
            '', ;
            'F27lCur')

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLALRD')
      =CrtCursor('F27lCur', 'XLALRD', 'C')
   EndIf

   Select XLALRD
   Append From Dbf('F27lCur')

   Select HOJDET
   Skip
EndDo

Return

************************************************************************
*> Impresión de Albaranes de Transporte - Detalle.                     *
*> Segundo paso: Imprimir los datos generados por GenAlrD.             *
*>                                                                     *
*> Recibe: XLALRD ----> Cursor con los datos a imprimir.               *
*>         _Report ---> Nombre del report de salida.                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaExAlr     ---> Impresión de Albaranes de Transporte.          *
************************************************************************
Procedure PrtAlrD
Parameters _Report
Local loImp

   If Used('XLALRD')
      Select XLALRD
      Go Top
      _Label1 = "IMPRESION DE ALBARANES DE TRANSPORTE (DETALLE)"

      loImp2 = CreateObject('st3imp2', , [CR=XLALRD, RPT=&_Report])
      =loImp2.Exec('?', [LBL=&_Label1])
      loImp2.Release
   Else
      _LxErr = "No hay albaranes de transporte para imprimir" + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLALRD')
      Use in XLALRD
   EndIf
   
Return

************************************************************************
*> Impresión de Albaranes de Transporte - Resumen Expedición.          *
*> Primer paso: Generar datos de expedición.                           *
*>                                                                     *
*> Recibe: _HojRut ---> Nº de Hoja de Ruta.                            *
*>                                                                     *
*> Genera: XLALRR  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaExAlr     ---> Impresión de Albaranes de Transporte.          *
************************************************************************
Procedure GenAlrR
Parameters _HojRut
Private _Selec, _Campos

*> Cargar el detalle de la Hoja de ruta.
_Campos = '*'
_Selec = "F31lHojRut='" + _HojRut + "'"
If !f3_sql(_Campos, ;
           'F31L', ;
           _Selec, ;
           'F31lHojRut,F31lAlbRep', ;
           '', ;
           'HOJDET')
   _LxErr = 'No se han podido cargar los albaranes de esta Hoja de Ruta' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select HOJDET
Go Top
If Eof()
   _LxErr = 'Esta Hoja de Ruta no contiene albaranes' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Cargar el detalle de cada albarán.
Select HOJDET
Go Top

Do While !Eof()
   _Campos = 'F27l' +_em + '.*, F08cDescri'
   _Selec = "F27lNumAlb='" + HOJDET.F31lAlbRep + "' And " + ;
            "F08cCodPro=F27lCodPro And F08cCodArt=F27lCodArt"

   =!f3_sql(_Campos, ;
            'F27L,F08C', ;
            _Selec, ;
            'F27lCodPro,F27lCodArt,F27lNumLot,F27lFecCad', ;
            '', ;
            'F27lCur')

   *> Copiar datos a cursor de trabajo. Si no existe, se crea.
   If !Used('XLALRR')
      =CrtCursor('F27lCur', 'XLALRR', 'C')
   EndIf

   Select XLALRR
   Append From Dbf('F27lCur')

   Select HOJDET
   Skip
EndDo

Return

************************************************************************
*> Impresión de Albaranes de Transporte - Resumen Expedición.          *
*> Segundo paso: Imprimir los datos generados por GenAlrR.             *
*>                                                                     *
*> Recibe: XLALRR ----> Cursor con los datos a imprimir.               *
*>         _Report ---> Nombre del report de salida.                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - SaExAlr     ---> Impresión de Albaranes de Transporte.          *
************************************************************************
Procedure PrtAlrR
Parameters _Report
Private _sort, loImp

   If Used('XLALRR')
      *> Ordenar cursor por artículo.
      _Sort = AllTrim(Sys(2015))
      Select XLALRR
      Sort To (_Sort) On F27lCodPro, F27lCodArt, F08cDescri, F27lNumLot, F27lFecCad
      Zap
      Append From (_Sort)
      Delete File(_Sort)

      *> Acumulados Peso y volumen.
      Select XLALRR
      Sum(F27lPesBru) To _F27lPesBru
      Sum(F27lVolume) To _F27lVolume
      
      Go Top
      _Label1 = "IMPRESION DE RESUMEN DE EXPEDICIONES"

      loImp2 = CreateObject('st3imp2', , [CR=XLALRR, RPT=&_Report])
      =loImp2.Exec('?', [LBL=&_Label1])
      loImp2.Release
   Else
      _LxErr = "No hay albaranes de transporte para imprimir" + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLALRR')
      Use in XLALRR
   EndIf
   
Return

************************************************************************
*> Impresión de contenido de bultos de picking.                        *
*> Generar datos del bulto actual.                                     *
*>                                                                     *
*> Recibe:                                                             *
*>	- _NumMac ---> Nº de Bulto.                                        *
*>	- _Origen ---> Origen de datos (LS: Listas, MC: MACs)              *
*>                                                                     *
*> Genera: XLCBUL  ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FEtiExpe ------> Impresión de etiquetas de expedición.          *
************************************************************************
Procedure GenCBul

Parameters _NumMac, _Origen

Local lStado, oMAC, oF08c, oF01t, oF24t, oF27c
Private cWhere

lStado = CrtCBul(_NumMac, _Origen)
If !lStado
	_LxErr = _LxErr + 'No se ha podido cargar el bulto ' + _NumMac + cr
	Use In (Select ("TMacCur"))
	Return lStado
EndIf

*> Cargar datos del artículo, y descripciones.
Select TMacCur
Go Top
Do While !Eof()
	Scatter Name oMAC

	*> Leer datos del artículo.
	m.F08cCodPro = oMAC.CodPro
	m.F08cCodArt = oMAC.CodArt

	=f3_seek('F08c')
	Select F08c
	Scatter Name oF08c

	*> Leer datos del transportista.
	m.F01tCodigo = oMAC.CodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oF01t

	*> Leer datos del cliente.
	m.F24tCodPro = oMAC.F24cCodPro
	m.F24tTipDoc = oMAC.F24cTipDoc
	m.F24tNumDoc = oMAC.F24cNumDoc

	If f3_seek("F24t")
		Select F24t
		Scatter Name oF24t
	Else
		Select F24t
		Scatter Name oF24t Blank
	EndIf

	cWhere = "F27cCodPro='" + oMAC.CodPro + "' And F27cTipDoc='" + oMAC.TipDoc + "' And F27cNumDoc='" + oMAC.NumDoc + "'"
	lStado = f3_sql("*", "F27c", cWhere, , , "F27cCur")
	Select F27cCur
	Scatter Name oF27c	

	Select TMacCur
	Replace DesCli With oF24t.F24tNomAso, ;
			DesTra With oF01t.F01tDescri, ;
			DesArt With oF08c.F08cDescri, ;
			TipPro With oF08c.F08cTipPro, ;
			NumAlb With oF27c.F27cNumAlb

   *> Cálculo del peso y volumen de la línea.
   Replace PesLin With CanFis * oF08c.F08cPesUni, ;
           VolLin With CanFis * oF08c.F08cVolUni / 1000.0

   Select TMacCur
   Skip
EndDo

Go Top

*> Copiar datos a cursor de trabajo. Si no existe, se crea.
If !Used('XLCBUL')
   =CrtCursor('TMacCur', 'XLCBUL', 'C')
EndIf

Select XLCBUL
Append From Dbf('TMacCur')
Use In (Select("TMacCur"))
Use In (Select("F27cCur"))

Return

************************************************************************
*> Impresión de contenido de bultos de fracciones.                     *
*> Generar datos del bulto actual.                                     *
*>                                                                     *
*>	- _NumMac ---> Nº de Bulto.                                        *
*>	- _Origen ---> Origen de datos (LS: Listas, MC: MACs)              *
*>                                                                     *
*> Genera: XLCBUL  ---> Cursor con los datos a imprimir.               *
************************************************************************
Procedure CrtCBul

Parameters cBulto, cOrigen

Private cWhere, cFromF
Local lStado, oMAC

Use In (Select ("TMacCur"))

*> Crear un cursor con la estructura de trabajo.
=CrtMCursor([F26l,F26v], 'TMacCur', 'C')
=CrtFCursor('TMacCur', [TBL=F24c,FLD=F24cCodPro,FLD=F24cTipDoc,FLD=F24cNumDoc,FLD=F24cFecDoc,FLD=F24cDirAso,FLD=F24cCodTra])
=AddFldToCursor('TMacCur', [NAME=NumMac,TYPE=C,LENGTH=9])
=AddFldToCursor('TMacCur', [NAME=PesLin,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor('TMacCur', [NAME=VolLin,TYPE=N,LENGTH=12,DECIMALS=3])
=AddFldToCursor('TMacCur', [NAME=TipDoc,TYPE=C,LENGTH=4])
=AddFldToCursor('TMacCur', [NAME=NumDoc,TYPE=C,LENGTH=13])
=AddFldToCursor('TMacCur', [NAME=NumAlb,TYPE=C,LENGTH=13])
=AddFldToCursor('TMacCur', [NAME=FecDoc,TYPE=D])
=AddFldToCursor('TMacCur', [NAME=CodTra,TYPE=C,LENGTH=6])
=AddFldToCursor('TMacCur', [NAME=DesTra,TYPE=C,LENGTH=30])
=AddFldToCursor('TMacCur', [NAME=CodCli,TYPE=C,LENGTH=15])
=AddFldToCursor('TMacCur', [NAME=DesCli,TYPE=C,LENGTH=40])
=AddFldToCursor('TMacCur', [NAME=CodPro,TYPE=C,LENGTH=6])
=AddFldToCursor('TMacCur', [NAME=CodArt,TYPE=C,LENGTH=13])
=AddFldToCursor('TMacCur', [NAME=DesArt,TYPE=C,LENGTH=40])
=AddFldToCursor('TMacCur', [NAME=TipPro,TYPE=C,LENGTH=4])
=AddFldToCursor('TMacCur', [NAME=NumLot,TYPE=C,LENGTH=15])
=AddFldToCursor('TMacCur', [NAME=FecCad,TYPE=D])
=AddFldToCursor('TMacCur', [NAME=CanFis,TYPE=N,LENGTH=12,DECIMALS=3])

*> Cargar los datos del bulto, en función del origen de datos.
Do Case
	*> Origen: Listas.
	Case cOrigen=='LS'
		cFromF = "F26l,F24c"
		cWhere = "F26lNumMac='" + cBulto + "' And "
		cWhere = cWhere + "F24cCodPro=F26lCodPro And F24cTipDoc=F26lTipDoc And F24cNumDoc=F26lNumDoc"

		lStado = f3_sql("*", cFromF, cWhere, , , "TMacCur")
		If !lStado
			If _xier <= 0
				_LxErr = "Error en carga de datos"
				Return .F.
			EndIf

			_LxErr = "No hay datos a tratar"
			Use In (Select ("TMacCur"))
			Return .F.
		EndIf

	*> Origen: MACs.
	Case cOrigen=='MC'
		cFromF = "F26v,F24c,F26l"
		cWhere = "F26vNumMac='" + cBulto + "' And "
		cWhere = cWhere + "F24cCodPro=F26vCodPro And F24cTipDoc=F26vTipDoc And F24cNumDoc=F26vNumDoc And "
		cWhere = cWhere + "F26lCodPro=F24cCodPro And F26lTipDoc=F24cTipDoc And F26lNumDoc=F24cNumDoc"

		lStado = f3_sql("*", cFromF, cWhere, , , "TMacCur")
		If !lStado
			If _xier <= 0
				_LxErr = "Error en carga de datos"
				Return .F.
			EndIf

			_LxErr = "No hay datos a tratar"
			Use In (Select ("TMacCur"))
			Return .F.
		EndIf

	*> Resto de casos: Error.
	Otherwise
		_LxErr = "Origen de datos no válido"
		Return .F.
EndCase

*> Pasar los campos de fichero a campos de trabajo.
Select TMacCur
Go Top
Do While !Eof()
	Scatter Name oMAC

	Replace NumMac With cBulto, ;
			CodPro With oMAC.F24cCodPro, ;
			TipDoc With oMAC.F24cTipDoc, ;
			NumDoc With oMAC.F24cNumDoc, ;
			FecDoc With oMAC.F24cFecDoc, ;
			CodCli With oMAC.F24cDirAso, ;
			CodTra With oMAC.F24cCodTra, ;
			CodArt With oMAC.F26lCodArt, ;
			CanFis With oMAC.F26lCanFis, ;
			NumLot With oMAC.F26lNumLot, ;
			FecCad With oMAC.F26lFecCad

	Select TMacCur
	Skip
EndDo

Return

************************************************************************
*> Impresión de contenido de bultos.                                   *
*> Segundo paso: Imprimir los datos generados por GenCBul.             *
*>                                                                     *
*> Recibe: XLCBUL ----> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FEtiExpe  -----> Impresión de etiquetas de expedición.          *
************************************************************************
Procedure PrtCBul

Private loImp2, cLbl

If Used('XLCBUL')
	Select XLCBUL
	Go Top

	*> Objeto impresión.
	cLbl = "IMPRESION DE CONTENIDO DE BULTOS"
	loImp2 = CreateObject('st3imp2', , [CR=XLCBUL, RPT=SaVovPrCL1])
	=loImp2.Exec('?', [LBL=&cLbl])
	loImp2.Release
Else
	_LxErr = "No hay datos de contenido bultos para imprimir"
	Do Form St3Inc With .T.
EndIf

*> Cerrar cursor generado por SQL.
Use In (Select ("XLCBUL"))
   
Return

************************************************************************
*> Impresión de packing list.                                          *
*> Segundo paso: Imprimir los datos generados por GenCBul.             *
*>                                                                     *
*> Recibe: XLCBUL ----> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FEtiExpe  -----> Impresión de etiquetas de expedición.          *
************************************************************************
Procedure PrtDBul

Private loImp2, cLbl
Local _Sort

If Used('XLCBUL')
	*> Clasificar el fichero por Documento / Bulto.
    _Sort = AllTrim(Sys(2015))
    Select XLCBUL
    Sort To (_Sort) On CodPro, TipDoc, NumDoc, NumMac, CodArt
    Zap
    Append From (_Sort)
    Delete File(_Sort)
	
	Select XLCBUL
	Go Top

	*> Objeto impresión.
	cLbl = "IMPRESION DE PACKING LIST"
	loImp2 = CreateObject('st3imp2', , [CR=XLCBUL, RPT=SaVovPrCL2])
	=loImp2.Exec('?', [LBL=&cLbl])
	loImp2.Release
Else
	_LxErr = "No hay datos de packing list para imprimir"
	Do Form St3Inc With .T.
EndIf

*> Cerrar cursor generado por SQL.
Use In (Select ("XLCBUL"))
   
Return

******************************************************************
*               Listados de Artículos/ubicación                  *
******************************************************************
PROCEDURE LInvAU1
Private lx_sql, lx_select, f_from, f_where, f_orden
Private Err
Private cField, cFromF, cWhere, cOrder
Local lStado

cField = "F16cCodUbi, F16cCodPro, F16cCodArt, F08cDescri, " + ;
         "F16cTamPal, F16cNumPal, F16cNumLot, " + ;
         "F16cSitStk, F16cFecCad, " + ;
         "F16cFecEnt, F16cCanFis, F16cCanRes, F16cTipDoc, F16cNumDoc"
            
cFromF = "F16c,F08c"

cWhere = _GCSS("F16cCodUbi", 1, 4) + "='" + _Alma + "' And " + ;
         "(F16cCodPro Between '" + PropD + "' And '" + PropH + "') And " + ;
         "(F16cCodArt Between '" + ArtD + "' And '" + ArtH + "') And " + ;
         "(F16cSitStk Between '" + SStkD + "' And '" + SStkH + "') And " + ;
         "F08cCodPro=F16cCodPro And F08cCodArt=F16cCodArt"

cOrder = "F16cCodPro,F16cCodArt,F16cCodUbi,F16cNumPal,F16cNumLot,F16cSitStk,F16cFecCad"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'ListLst')

_Label1 = "INVENTARIO ARTICULOS/UBICACION"
Do Form St3Imp2 With _Label1, 'LInvAUL1', 'ListLst'

*> Cerrar cursor generado por SQL.
If Used('ListLst')
   Use in ListLst
EndIf

Return

******************************************************************
*               Listados de Artículos/Lote                       *
******************************************************************

PROCEDURE LInvAL1
Private lx_sql, lx_select, f_from, f_where, f_orden
Private Err
Private cField, cFromF, cWhere, cOrder
Local lStado

cField = "F16cCodUbi, F16cCodPro, F16cCodArt, F08cDescri, " + ;
         "F16cTamPal, F16cNumPal, F16cNumLot, " + ;
         "F16cSitStk, F16cFecCad, " + ;
         "F16cFecEnt, F16cCanFis, F16cCanRes, F16cTipDoc, F16cNumDoc"
            
cFromF = "F16c,F08c"

cWhere = _GCSS("F16cCodUbi", 1, 4) + "='" + _alma + "' And " + ;
         "(F16cCodPro Between '" + PropD + "' And '" + PropH + "') And " + ;
         "(F16cCodArt Between '" + ArtD + "' And '" + ArtH + "') And " + ;
         "(F16cNumLot Between '" + LotD + "' And '" + LotH + "') And " + ;
         "F08cCodPro=F16cCodPro And F08cCodArt=F16cCodArt"

cOrder = "F16cCodPro,F16cCodArt,F16cCodUbi,F16cNumPal,F16cNumLot,F16cSitStk,F16cFecCad"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'ListLst')

_Label1 = "INVENTARIO ARTICULO/LOTE"
Do Form St3Imp2 With _Label1,'LInvALL1', 'ListLst'

*> Cerrar cursor generado por SQL
If Used('ListLst')
   Use in ListLst
EndIf

Return

******************************************************************
*               Listados de Família/Artículos                    *
******************************************************************

PROCEDURE LInvFM1
Private lx_sql, lx_select, f_from, f_where, f_orden
Private Err
Private cField, cFromF, cWhere, cOrder
Local lStado

cField = "F16cCodUbi, F16cCodPro, F16cCodArt, F08cTipFam, " + ;
         _GCN("F00gDescri", Space(30)) + " As F00gDescri, " + ;
         _GCSS("F08cDescri", 1, 30) + " As F08cDescri, F16cTamPal, F16cNumPal, " + ;
         "F16cNumLot, F16cSitStk, F16cFecCad, F16cFecEnt, " + ;
         "F16cCanFis, F16cCanRes, F16cTipDoc, F16cNumDoc"
            
cFromF = "F16c,F08c,F00g"

cWhere = _GCSS("F16cCodUbi", 1, 4) + "='" + _Alma + "'" + ;
         " And (F16cCodPro Between '" + PropD + "' And '" + PropH + "')" + ;
         " And (F16cCodArt Between '" + ArtD + "' And '" + ArtH + "')" + ;
         " And (F08cTipFam Between '" + FamD + "' And '" + FamH + "')" + ;
         " And F08cCodPro=F16cCodPro" + ;
         " And F08cCodArt=F16cCodArt" + ;
         " And F00gCodFam=F08cTipFam" 
           
cOrder = "F08cTipFam,F16cCodPro,F16cCodArt,F16cCodUbi,F16cNumLot,F16cNumPal,F16cSitStk,F16cFecCad"

lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'ListLst')
_Label1 = "INVENTARIO FAMILIA/ARTICULO"
Do Form St3Imp2 With _Label1,'LInvFML1', 'ListLst'

*> Cerrar cursor generado por SQL.
If Used('ListLst')
   Use in ListLst
EndIf

Return

******************************************************************
*               Listados Boleta de Entrada                       *
* Llamado desde: ENTRBOLE --> Impresión de Boleta de Entrada     *
* Corregir datos de propietario. AVC - 19.06.2006                *
******************************************************************
PROCEDURE EntrBolE1
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio   
Local oF01c, oF01p


   Set NullDisplay To ''
   _LxErr = ''

   =CrtMCursor([F08c,F18c,F18m,F18n,F18l,F01c,F01p,F00j], '_BdeCaa', 'C')

   f_from    = "F08c,F18c,F18m,F18n,F18l"
   f_group = ''
   f_orden = "F08cCodArt,F18nNumEnt, F18nNumDoc, F18cFecPed, F18mFecEnt" 

   *********************************************
   * Por cada Albarán obtener sus documentos
   *********************************************              
   Select CAABDE
   Locate For Marca==1
   Do While !Eof()
      *> Condición.
      f_where = "     F18mCodPro = '" + CAABDE.F18mCodPro + _cm + ;
                " And F18mNumEnt = '" + CAABDE.F18mNumEnt + _cm + ;
  		        " And F18lCodPro = F18cCodPro" + ;
  		        " And F18lTipDoc = F18cTipDoc" + ;
                " And F18lNumDoc = F18cNumDoc" + ;
                " And F18nNumEnt = F18mNumEnt" + ;
                " And F18nCodPro = F18lCodPro" + ;
                " And F18nTipDoc = F18lTipDoc" + ;
                " And F18nNumDoc = F18lNumDoc" + ;
                " And F18nLinDoc = F18lLinDoc" + ;
                " And F08cCodPro = F18nCodPro" + ;
                " And F08cCodArt = F18nCodArt"
	   
      If !f3_sql('*', f_from, f_where, f_orden, f_group, '_BdeCaa')
         _LxErr = _LxErr + _LxErr + 'No se han podido cargar los datos a procesar' + cr + ;
                  'Nº DE ALBARAN: ' + CAABDE.F18mNumEnt + cr

         Select CAABDE
         Skip
         Loop
      EndIf

      *> Leer datos de la ficha del propietario.
      m.F01pCodigo = CAABDE.F18mCodPro
      If !f3_seek('F01P')
         Select F01p
         Append Blank
      Else
         Select F01p
      EndIf
      Scatter Name oF01p

      *> Leer datos de la provincia.
      m.F00jCodPrv = F01p.F01pProvin
      If !f3_seek('F00J')
         Select F00j
         Append Blank
      EndIf

      *> Leer datos de la entidad.
      *If !f3_seek('F01C', [m.F01cTipEnt='PROP',m.F01cCodigo=CAABDE.F18mCodPro])
      m.F01cTipEnt='PROV'
      m.F01cCodigo=_BdeCaa.F18cCodEnt
      If !f3_seek('F01C')
         Select F01c
         Append Blank
      EndIf
      Scatter Name oF01c

      Select _BdeCaa
      Replace F00jDescri With F00j.F00jDescri
      Replace F01cDescri With F01c.F01cDescri
      Gather Name oF01c
      Gather Name oF01p

      If !Used('XLEntrBolEL1')
         =CrtCursor('_BdeCaa', 'XLEntrBolEL1', 'C')
      EndIf 

      *> Añadir nuevo documento de albarán para imprimir.
      Select XLEntrBolEL1
      Append From Dbf('_BdeCaa')

      Select CAABDE
      Continue
   EndDo 

   If !Empty(_LxErr)
      Do Form St3Inc With .T.
   EndIf

   _Label1 = "BOLETA DE ENTRADA"
   Select XLEntrBolEL1
   Go Top 
   If !Eof()
      Do Form St3Imp2 With _Label1,'EntrBolEL1', 'XLEntrBolEL1'
   Else
      =f3_sn(1,1,"No hay Albaranes para imprimir")
   EndIf   

   *> Cerrar cursores generado por SQL.
   If Used('_BdeCaa')
      Use in _BdeCaa
   EndIf

   If Used('XLEntrBolEL1')
      Use in XLEntrBolEL1
   EndIf

Return

******************************************************************
*               Listados Albarán de Entrada                      *
* Recibe : Destinatarios de las copias.                          *
* Llamado desde: ENTRALBI --> Impresión de Albarán de Entrada    *
******************************************************************
Procedure EntrAlbI1
Parameters nCCli, nCAlm, nCSer

Private _Inx, loImp2, oAlb

   _LxErr = ''

   *> Primera parte: Generar datos del albarán actual.
   Select CAALBE
   Locate For Marca==1
   Do While Found()
      Scatter Name oAlb
      Do GenAlbI1 With oAlb.F18mCodPro, oAlb.F18mNumEnt

      *> Leer el siguiente documento.
      Select CAALBE
      Continue
   EndDo      

   Wait Clear

   If !Empty(_LxErr)
      Do Form St3Inc With .T.
      Return
   EndIf

   *> Objeto impresión.
   loImp2 = CreateObject('st3imp2', , [CR=XLEntrAlbIL1, RPT=EntrAlbIL1])

   *> Imprimir albaranes, copia proveedores.
   If nCCli==1
      =loImp2.Exec('?', [[LB=ALBARAN DE ENTRADA (COPIA PROVEEDOR)])
   EndIf

   *> Imprimir albaranes, copia almacén.
   If nCAlm==1
      =loImp2.Exec('?', [[LB=ALBARAN DE ENTRADA (COPIA ALMACEN)])
   EndIf

   *> Imprimir albaranes, copia servicio contabilidad.
   If nCSer==1
      =loImp2.Exec('?', [[LB=ALBARAN DE ENTRADA (COPIA CONTABILIDAD)])
   EndIf

   If Used('XLEntrAlbIL1')
      Use In XLEntrAlbIL1
   EndIf

   loImp2.Release

Return

******************************************************************
*> Generar datos para impresión del albarán de entrada.
*> Corregida duplicación de líneas. AVC - 30.04.2008
******************************************************************
PROCEDURE GenAlbI1
Parameters cCodPro, cNumEnt

Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio
Local lFirstTime, oF01c, oF01p, lStado, nCurReg

  Set NullDisplay To ''
  lFirstTime = .T.

  =CrtMCursor([F08c,F18c,F18m,F18n,F18l,F01c,F01p,F00j,F18o], '_AdeCaa', 'C')

  f_from    = "F18m,F18n,F08c,F18c,F18l"

  *> Condición.
  f_where = "     F18mNumEnt = '" + cNumEnt + _cm + ;
  		    " And F18lCodPro = F18cCodPro" + ;
  		    " And F18lTipDoc = F18cTipDoc" + ;
  		    " And F18lNumDoc = F18cNumDoc" + ;
            " And F18nNumEnt = F18mNumEnt" + ;
            " And F18nCodPro = F18lCodPro" + ;
            " And F18nTipDoc = F18lTipDoc" + ;
            " And F18nNumDoc = F18lNumDoc" + ;
            " And F18nLinDoc = F18lLinDoc" + ;
            " And F08cCodPro = F18nCodPro" + ;
            " And F08cCodArt = F18nCodArt"
	   
  f_group = ''
  f_orden = "F08cCodArt,F18nNumEnt, F18nNumDoc, F18cFecPed, F18mFecEnt"

  If !f3_sql('*', f_from, f_where, f_orden, f_group, '_AdeCaa')
     _LxErr = _LxErr + 'No se han podido cargar los datos a procesar' + cr + ;
              'Nº DE ALBARAN: ' + cNumEnt + cr
     Return
  EndIf
	 	
  *> Leer datos adicionales.
  Select _AdeCaa
  Go Top
  Do While !Eof()
  	 nCurReg = RecNo()

     *> Leer datos de la ficha del propietario.
     m.F01pCodigo = cCodPro
      If !f3_seek('F01P')
         Select F01p
         Append Blank
      Else
         Select F01p
      EndIf
      Scatter Name oF01p

     *> Leer datos de la provincia.
     m.F00jCodPrv = F01p.F01pProvin
     If !f3_seek('F00J')
        Select F00j
        Append Blank
     EndIf

     *> Leer datos de la entidad (Pa qué?).
     If !f3_seek('F01C', [m.F01cTipEnt='PROP',m.F01cCodigo=cCodPro])
        Select F01c
        Append Blank
     EndIf
     Scatter Name oF01c

     *> Leer observaciones remite.
     lx_select = "F18oObserv As Remite"
	 f_where   = "F18oNumEnt='" + _AdeCaa.F18mNumEnt + "' And F18oNumObs=1"
     lStado = f3_sql(lx_select, 'F18o', f_where, , , 'F18oRemite')
     If _xier <= 0
        _LxErr = "Error al cargar observaciones remite" + cr
        Do Form St3Inc With .T.
        Return .F.
     EndIf

     *> Leer observaciones asociadas a la entrada.
     lx_select = "F18oObserv As Observ"
	 f_where   = "F18oNumEnt='" + _AdeCaa.F18mNumEnt + "' " && And F18oNumObs Is Null"
     lStado = f3_sql(lx_select, 'F18o', f_where, , , 'F18oObserv')
     If _xier <= 0
        _LxErr = "Error al cargar observaciones entrada" + cr
        Do Form St3Inc With .T.
        Return .F.
     EndIf

     *> Agregar columnas de observaciones al cursor de trabajo.
     If lFirstTime
        =UpdCursor('F18oRemite', '_AdeCaa', 'C')
        =UpdCursor('F18oObserv', '_AdeCaa', 'C')
        Store .F. To lFirstTime
     EndIf

     Select _AdeCaa
     Go nCurReg
     Replace Observ With F18oObserv.Observ, ;
             Remite With F18oRemite.Remite, ;
             F00jDescri With F00j.F00jDescri
     Gather Name oF01c
     Gather Name oF01p

     *> Agregar datos a cursor de impresión.
     If !Used('XLEntrAlbIL1')
        =CrtCursor('_AdeCaa', 'XLEntrAlbIL1', 'C')
     EndIf 

     Select _AdeCaa
     Skip
  EndDo

  Select XLEntrAlbIL1
  Append From Dbf('_AdeCaa')
  Select _AdeCaa
  Go Top

  *> Metros al cubo y kilos pesados.---------------------
  f_where = "F20cCodPro='" + cCodPro + "' And " + ;
            "F20cNumEnt='" + cNumEnt + "' And " + ;
            "F08cCodPro=F20cCodPro And F08cCodArt=F20cCodArt"
  f_where = f_where + " And F20cTipMov like '1%'"
	   
  f_group = ''
  f_orden = "F20cCodPro,F20cCodArt,F20cNumLot" 

  lStado = f3_sql('*', 'F20c,F08c', f_where, f_orden, f_group, 'PesVol')
  If _xier <= 0
     _LxErr = "Error al cargar peso / volumen"
     Do Form St3Inc With .T.
     Return .F.
  EndIf

  *> Utiliza campos de la ficha de artículo para acumulados.
  Select PesVol
  Go Top
  Do While !Eof()	 
     Replace F08cVolUni With (F08cVolUni * F20cCanFis) / 1000

     lx_select = GetCvtNvl(_ENTORNO, _VERSION, "F16tPesEnt") + " As F16tPesEnt"
     f_where = "F16tNumPal='" + F20cNumPal + _cm
     lStado = f3_sql(lx_select, 'F16t', f_where, , , 'PesoPes')
     If _xier <= 0
        _LxErr = "Error al cargar peso entrado"
        Do Form St3Inc With .T.
        Return .F.
     EndIf

	 Select PesoPes
	 Go Top
	 If Eof()
        Select PesVol
		Replace F08cPesUni With (PesVol.F08cPesUni * F20cCanFis)
	 Else
        Select PesVol
		Replace F08cPesUni With PesoPes.F16tPesEnt
	 EndIf   

     Select PesVol
	 Skip
  EndDo

  Select PesVol
  Go Top
  Sum All F08cPesUni to TotPes
  
  Select PesVol
  Go Top
  Sum All F08cVolUni To TotVol

  Select XLEntrAlbIL1
  Replace All F08cPesUni With TotPes For F18mCodpro==cCodPro And F18mNumEnt==cNumEnt
  Replace All F08cVolUni With TotVol For F18mCodpro==cCodPro And F18mNumEnt==cNumEnt

  *> Cerrar cursores generados por SQL.
  If Used('_AdeCaa')
     Use In _AdeCaa
  EndIf
  If Used('PesVol')
     Use In PesVol
  EndIf
  If Used('PesoPes')
     Use In PesoPes
  EndIf
  If Used('F18oRemite')
     Use In F18oRemite
  EndIf
  If Used('F18oObserv')
     Use In F18oObserv
  EndIf

Return

************************************************************************
*> Anexo al albaran de entrada.                                        *
*> Primer paso: Selección de datos a imprimir.                         *
************************************************************************
Procedure AnexoAlb
Private _Inx

   Select CAALBE
   Locate For Marca==1
   Do While Found()
      Scatter MemVar
 
      Do GenAnx1 With F18mCodPro, F18mNumEnt

      *> Leer el siguiente documento.
      Select CAALBE
      Continue
   EndDo      

   Wait Clear

   If !Empty(_LxErr)
      Do Form St3Inc With .T.
      Return
   EndIf

   *> Imprimir los datos anexos del albarán.
   Do PrtAnx1

   If Used('XLEntrAlbIL1')
      Use In XLEntrAlbIL1
   EndIf

Return

************************************************************************
*> Anexo al albaran de entrada.                                        *
*> Segundo paso: Generación de datos.                                  *
************************************************************************
PROCEDURE GenAnx1
Parameters cCodPro, cNumEnt

Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err

Set NullDisplay To ''
lx_select = " Select F20cCodPro,F20cCodArt,F20cNumLot,F20cNumPal,F20cNument,F20cCanFis,F08cDescri,F08cPesUni,F08cVolUni, F08cPesUni As PesEnt,F08cVolUni As VolEnt, " + ;
			" F01p" + _em + ".* "

f_from    = " From F20c" + _em + "," + ;
            "      F08c" + _em + "," + ;           
            "      F01p" + _em 
              
   *********************************************
   * Por cada Albarán obtener sus documentos
   *********************************************              
   f_where = " Where F20cCodPro = '" + cCodPro + _cm + ;
             " And   F20cNumEnt = '" + cNumEnt + _cm + ;
             " And   F08cCodPro = F20cCodPro" + ;
             " And   F08cCodArt = F20cCodArt" + ;
             " And   F01pCodigo = F20cCodPro " 
	   
   f_group = ''
   f_orden = " ORDER BY F20cCodPro,F20cCodArt,F20cNumLot" 

   lx_sql = lx_select + f_from + f_where + f_group + f_orden

   Err=SqlExec(_aSql, lx_sql,'_AdeCaa')

   If Err <= 0
     _LxErr = _LxErr + 'Error al generar datos para impresión Anexo de Entrada' + cr + ;
                       'MENSAJE: ' + Message() + cr
     Return
   EndIf
	 	
   =SqlMoreResults(_aSql)
   Select _AdeCaa

   *> PESO Y VOLUMEN.--------------------------------------------------------
   Select _AdeCaa
   Replace All PesEnt With 0
   Replace All VolEnt With 0

   Go Top
   Do While !Eof()	 
     _Sentencia = " Select " + GetCvtNvl(_ENTORNO, _VERSION, "F16tPesEnt") + " As F16tPesEnt From F16t" + _em + ;
                  " Where F16tNumPal = '" + _AdeCaa.F20cNumPal + "'" 
		 			  
	 _Ok = SqlExec(_Asql,_Sentencia,'PesoPes')
	 If _Ok <= 0
	    _LxErr = _LxErr + 'Error generando Anexo' + cr + ;
	                      'MENSAJE: ' + Message() + cr
		Return
	 EndIf
		 
	 =SqlMoreResults(_Asql)

	 Select _AdeCaa
     Replace VolEnt With (_AdeCaa.F08cVolUni*F20cCanFis)

     Select PesoPes 
	 If Eof()
        Select _AdeCaa
        Replace PesEnt With (_AdeCaa.F08cPesUni*F20cCanFis)
	 Else
        Select _AdeCaa
		Replace PesEnt With PesoPes.F16tPesEnt
	 EndIf   

	 Select _AdeCaa
	 Skip
   EndDo	

   *> Nombre fichero donde están los documentos del albarán que irán a fichero de impresión.
   a = Dbf()

   If !Used('XLAnexAlb1')
      =CrtCursor('_AdeCaa','XLAnexAlb1','C')
   EndIf 

   *> Añadir nuevo documento de albarán para imprimir
   Select XLAnexAlb1
   Append From &a

   *> Cerrar cursores generados por SQL.
   If Used('_AdeCaa')
      Use in _AdeCaa
   EndIf
   If Used('PesVol')
      Use In PesVol
   EndIf
   If Used('PesoPes')
      Use In PesoPes
   EndIf

Return

************************************************************************
*> Anexo al albaran de entrada.                                        *
*> Tercer paso: Impresión de datos.                                    *
************************************************************************
PROCEDURE PrtAnx1
Private _Label

   _Label1 = "ANEXO AL ALBARAN DE ENTRADA"
   Select XLAnexAlb1
   Go Top 
   Do Form St3Imp2 With _Label1,'EntrAAnex1', 'XLAnexAlb1'

Return

*********************************************************************
*               Listados Boleta de Descarga                         *
* Llamado desde: ENTRBOLD --> Impresión de Boleta de Descarga.      *
* Lectura separada de provincia. AVC - 07.05.2003                   *
*********************************************************************
PROCEDURE EntrBolD1
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden, Err
Local lStado, cDesProv

lx_select = "F18nNumEnt, F18nNumDoc, F18cFecPed, F18mFecEnt, F01pDescri, F01pCodPos, " + ;
            "F01pDirecc, F01pNumNif, F18mAlbPrv, F18mMuelle, " + ;
            "Count(F18nLinPed) As NLineas"

f_from    = "F18c,F18m,F18n,F01p"

*********************************************
* Por cada Albarán obtener sus documentos
*********************************************              

   Select RNGALB
   Locate For Marca==1
   Do While !Eof()
     *> Condición
     f_where = "F18mNumEnt = '" + RNGALB.F18mNumEnt + _cm + ;
               " And   F18nNumEnt = F18mNumEnt" + ;
               " And   F18cCodPro = F18nCodPro" + ;
               " And   F18cTipDoc = F18nTipDoc" + ;
               " And   F18cNumDoc = F18nNumDoc" + ;
               " And   F01pCodigo = F18mCodPro"
               
     f_group = "F18nNumEnt, F18nNumDoc, F18cFecPed, F18mFecEnt, F01pDescri, " + ;
               "F01pCodPos, F01pDirecc, F01pNumNif, F18mAlbPrv, F18mMuelle"

     f_orden = "F18nNumEnt, F18nNumDoc, F18cFecPed, F18mFecEnt" 

     lStado = f3_sql(lx_select, f_from, f_where, f_orden, f_group, '_DocAlb')
     If !lStado
        If _xier <=0
           _LxErr = "Error al cargar datos para listado" + cr
           Do Form St3Inc With .T.
           Return .F.
        EndIf
     EndIf
     
     If !Used('XLEntrBolDL1')
        =CrtCursor('_DocAlb', 'XLEntrBolDL1', 'C')
     EndIf 
     
     *> Añadir nuevo documento de albarán para imprimir
     Select XLEntrBolDL1
     Append From Dbf('_DocAlb')
     
     Select RNGALB
     Continue
   EndDo 
   
   _Label1 = "BOLETA DE DESCARGA"
   If Used('XLEntrBolDL1')
      *> Agregar campo provincia al cursor.
      =AddFldToCursor('XLEntrBolDL1', [NAME=F00jDescri,TYPE=C,LENGTH=40])

      *> Descripción de la provincia.
      Select XLEntrBolDL1
      Go Top
      Do While !Eof()
         m.F00jCodPrv = F01pProvin
         If f3_seek('F00j')
            cDesProv = F00j.F00jDescri
         Else
            cDesProv = Space(40)
         EndIf

         Select XLEntrBolDL1
         Replace F00jDescri With cDesProv
         Skip
      EndDo

      Go Top 
      Do Form St3Imp2 With _Label1,'EntrBolDL1', 'XLEntrBolDL1'
   Else
      _LxErr = "No hay albaranes para imprimir" + cr
      Do Form St3Inc With .T.
   EndIf   

   *> Cerrar cursores generados por SQL.
   Use In (Select('_DocAlb'))
   Use In (Select('XLEntrBolDL1'))

Return

******************************************************************
*               Listado Albarán Comparativo                      *
* Llamado desde: ENTRALBC -- > Albarán Comparativo.              *
******************************************************************

PROCEDURE EntrAlbC1
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio   

lx_select = "F18m" + _em + ".*, F18n" + _em + ".*, F08c" + _em + ".*, F01p" + _em + ".*, ' ' As St"
f_from    = "F18m,F18n,F08c,F01p"

   *********************************************
   *  Por cada albarán obtener sus documentos.
   *********************************************              
   Select RNGCMP
   Locate For Marca==1
   Do While Found()
     *> Condición.
     f_where = "F18mCodPro='" + RNGCMP.F18mCodPro + _cm + ;
               " And   F18mNumEnt ='" + RNGCMP.F18mNumEnt + _cm + ;
     		   " And   F18nNumEnt ='" + RNGCMP.F18mNumEnt + _cm + ;
               " And   F01pCodigo = F18nCodPro" + ;
               " And   F08cCodPro = F18nCodPro" + ;
               " And   F08cCodArt = F18nCodArt"  
               
     f_orden = "F18nNumEnt, F18nCodPro, F18nCodArt" 

     =f3_sql(lx_select, f_from, f_where, f_orden, , '_LinAlb')

     Select _LinAlb
   
     *> Crear cursor de impresión.
     If !Used('XLEntrALBCL1')
        =CrtCursor('_LinAlb', 'XLEntrALBCL1', 'C')
     EndIf 

     Select XLEntrALbCL1
     Append From Dbf('_LinAlb')
     
     Select RNGCMP
     Continue
   EndDo

   _Label1 = "ALBARÁN COMPARATIVO"
   Select XLEntrAlbCL1
   Replace All St With Iif(F18nCanRec < F18nCanAlb, '*', Space(1))

   Go Top
   If !Eof()
      Do Form St3Imp2 With _Label1,'EntrAlbCL1', 'XLEntrAlbCL1'
   Else
      =f3_sn(1,1,"No hay Albaranes para imprimir")
   EndIf

   *> Cerrar cursores generado por SQL.
   If Used('_LinAlb')
      Use in _LinAlb
   EndIf

   If Used('XLEntrAlbCL1')
      Use in XLEntrAlbCL1
   EndIf

Return

******************************************************************
*     Listado Estado Documentos de Salida (Cabecera)
******************************************************************
PROCEDURE ESTDOC1
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio   
Local lStado

Inicio=.T.

*> Crear cursor de trabajo.
=CrtMCursor([F24c,F01p,F22p,F01t,F24t], 'XLCABEST', 'C')

lx_select = "*"

f_from    = "F24c,F01p"

*********************************************
*    Tratar Documentos
*********************************************

   Select CABEST
   Locate For Marca = 1
   Do While Found()
     *> Condición     
     f_where = "F24cCodPro='" + CABEST.F24cCodPro + _cm + ;
               " And   F24cTipDoc='" + CABEST.F24cTipDoc + _cm + ;
               " And   F24cNumDoc='" + CABEST.F24cNumDoc + _cm + ;
               " And   F01pCodigo='" + CABEST.F24cCodPro + _cm

     f_orden = "F24cCodPro, F24cTipDoc, F24cNumDoc"

     lStado = f3_sql(lx_select, f_from, f_where, f_orden, , 'XLCABEST')
     If !lStado
        *> Si no hay datos a añadir al fichero de impresión, mirar siguiente documento.
        Select CABEST
        Skip
        Loop 
     EndIf

     Select XLCABEST
     Go Top

     *> Agregar nombre transportista al cursor.     
     m.F01tCodigo = F24cCodTra
     If f3_seek('F01t')
        Select XLCABEST
        Replace F01tDescri With F01t.F01tDescri
     EndIf

     *> Agregar datos dirección del documento.
     Select XLCABEST
     m.F24tCodPro = F24cCodPro
     m.F24tTipDoc = F24cTipDoc
     m.F24tNumDoc = F24cNumDoc
     If f3_seek('F24t')
        Scatter Name oF24t
        Select XLCABEST
        Gather Name oF24t
     EndIf

     *> Crear cursor para impresión, si no existe.
     If Inicio=.T.
        =CrtCursor('XLCABEST','XLCABESTL1','C')
        Inicio=.F.
     EndIf 

     *> Añadir nuevas líneas al cursor de documentos
     Select XLCABESTL1
     Append From Dbf('XLCABEST')
     
     Select CABEST
     Continue
   EndDo 
   
   _Label1 = "ESTADO DE DOCUMENTOS DE SALIDA - CABECERA"
   If Inicio=.F.
      Select XLCABESTL1
      Go Top 
      Do Form St3Imp2 With _Label1,'CCDOCSSL1', 'XLCABESTL1'
   Else
      =f3_sn(1,1,"No hay documentos de salida para imprimir")
   EndIf   

   *> Cerrar cursores generados por SQL.
   If Used('XLCABEST')
      Use in XLCABEST
   EndIf

   If Used('XLCABESTL1')
      Use in XLCABESTL1
   EndIf

Return

******************************************************************
*     Listado Estado Documentos de Salida (Detalle)
******************************************************************
PROCEDURE EstDoc2
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio   

Inicio=.T.

lx_select = "*"

f_from    = "F24l,F08c"

***************************************************
* Por cada Documento de salida obtener el detalle.
***************************************************
   Select LINEST
   Go Top
   Do While !Eof()
     *> Condición
     f_where = "F24lCodPro ='" + LINEST.F24LCodPro + _cm + ;
               " And   F24lTipDoc ='" + LINEST.F24LTipDoc + _cm + ;
               " And   F24lNumDoc ='" + LINEST.F24LNumDoc + _cm + ;
               " And   F24lLinDoc ='" + LINEST.F24LLinDoc + _cm + ;
               " And   F08cCodPro = F24lCodPro" + ;
               " And   F08cCodArt = F24lCodArt"
               
     f_orden = "F24lLinDoc" 

     lStado = f3_sql(lx_select, f_from, f_where, f_orden, , 'LINEST1')
     If !lStado
        *> Si no hay datos a añadir al fichero de impresión, mirar siguiente línea documento.
        Select LINEST
        Skip 
        Loop 
     EndIf

     *> Crear cursor para impresión, si no existe.
     If Inicio=.T.
        =CrtCursor('LINEST1','XLLINEST1','C')
        Inicio=.F.
     EndIf 

     *> Añadir nuevas líneas de documentos para imprimir.
     Select XLLINEST1
     Append From Dbf('LINEST1')
     
     Select LINEST
     Skip
   EndDo 
   
   _Label1 = "ESTADO DOCUMENTOS DE SALIDA - DETALLE"
   If Inicio=.F.
      Select XLLINEST1
      Go Top 
      Do Form St3Imp2 With _Label1,'CCDOCSSL2', 'XLLINEST1'
   Else
      =f3_sn(1,1,"No hay documentos de salida para imprimir")
   EndIf

   *> Cerrar cursores generados por SQL.
   If Used('LINEST1')
      Use in LINEST1
   EndIf

   If Used('XLLINEST1')
      Use in XLLINEST1
   EndIf   

Return
  
******************************************************************
*               Listado de Recuento de Material
******************************************************************
PROCEDURE ENTRRECM1
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio   

Inicio=.T.

=CrtMCursor([F08c,F18m,F18n,F01p,F01t], 'LINALB1', 'C')

f_from    = "F08c,F18m,F18n,F01p"

*********************************************
* Obtener líneas
*********************************************              

   Select LINALB
   Go Top
   Do While !Eof()

     *> Selección de registros.
     f_where = "       F18nCodPro='" + LINALB.F18nCodPro + _cm + ;
               " And   F18nTipDoc='" + LINALB.F18nTipDoc + _cm + ;
               " And   F18nNumDoc='" + LINALB.F18nNumDoc + _cm + ;
               " And   F18nLinDoc='" + LINALB.F18nLinDoc + _cm + ;
               " And   F18nSitStk='" + LINALB.F18nSitStk + _cm + ;
               " And   F18nNumLot='" + LINALB.F18nNumLot + _cm + ;
               " And   F18nCodArt='" + LINALB.F18nCodArt + _cm + ;
               " And   F08cCodPro=F18nCodPro" + ;
               " And   F08cCodArt=F18nCodArt" + ;
               " And   F18mNumEnt='" + m.NumEnt + _cm + ;
               " And   F18nNumEnt=F18mNumEnt" + ;
		       " And   F01pCodigo=F18nCodPro"
     
     f_orden = "F18nCodPro, F18nTipDoc, F18nNumDoc, F18nLinDoc" 

     f_group = ''

     lStado = f3_sql('*', f_from, f_where, f_orden, f_group, 'LINALB1')
     If lStado
       *> Agregar el transportista. Como se trata de una única entrada, solo
        *> hay que realizar una lectura en la tabla de transportistas.
        Select LINALB1
        Go Top

        m.F01tCodigo = F18mCodTra
        If f3_seek('F01t')
           Select F01t
           Go Top
           Select LINALB1
           Replace All F01tDescri With F01t.F01tDescri
        Else
           Select LINALB1
           Replace All F01tDescri With Space(30)
        EndIf
     EndIf

     *> Si no hay datos a añadir al fichero de impresión, mirar siguiente documento.
     Select LINALB1
     Go Top
     If Eof()
        Select LINALB
        Skip 
        Loop 
     EndIf
     
     If Inicio=.T.
        =CrtCursor('LINALB1', 'XLLINALB1', 'C')
        Inicio=.F.
     EndIf 

     *> Añadir nuevas líneas de documentos para imprimir.
     Select XLLINALB1
     Append From Dbf('LINALB1')
     
     Select LINALB
     Skip
   EndDo 
   
   _Label1 = "RECUENTO DE MERCANCIA"
   If Inicio=.F.
      Select XLLINALB1
      Go Top 
      Do Form St3Imp2 With _Label1,'ENTRRECML1', 'XLLINALB1'
   Else
      =f3_sn(1,1,"No hay lineas para imprimir")    
   EndIf   

   *> Cerrar cursor generado por SQL

   If Used('LINALB1')
      Use in LINALB1
   EndIf

   If Used('XLLINALB1')
      Use in XLLINALB1
   EndIf   

Return

******************************************************************
*               Listado de Confirmación de Entradas
******************************************************************
PROCEDURE ENTRCNFE1
Private lx_sql, lx_select, f_from, f_where, f_group, f_orden
Private Err, Inicio   

Inicio=.T.

=CrtMCursor([F08c,F14c,F18m,F18n,F01p,F01t], 'CNFENT1', 'C')

f_from    = "F08c,F14c,F18m,F18n,F01p"

*********************************************
* Obtener Líneas
*********************************************
   Select CNFENT
   Locate For !Empty(Sel)
   Do While Found()
     
     *> Generar cursor de trabajo.
     *> Como se pueden confirmar MPs sin documento, en este caso se toman
     *> los datos del cursor CNFENT.
     f_where = "       F18nCodPro='" + CNFENT.CodPro + _cm + ;
               " And   F18nTipDoc='" + CNFENT.TipDoc + _cm + ;
               " And   F18nNumDoc='" + CNFENT.NumDoc + _cm + ;
               " And   F18nLinDoc='" + PadL(CNFENT.LinDoc, 4, '0') + _cm + ;
               " And   F14cNumMov='" + CNFENT.NumMov + _cm + ;
               " And   F18mNumEnt='" + CNFENT.NumEnt + _cm + ;
               " And   F18nNumEnt=F18mNumEnt" + ;
               " And   F08cCodPro=F18nCodPro" + ;
               " And   F08cCodArt=F18nCodArt" + ;
		       " And   F01pCodigo=F18nCodPro"

     f_orden = "F18nCodPro, F18nTipDoc, F18nNumDoc, F18nLinDoc" 
     f_group = ''

     lStado = f3_sql('*', f_from, f_where, f_orden, f_group, 'CNFENT1')
     If !lStado
        Select CNFENT1
        Append Blank
        Replace F18nCodPro With CNFENT.CodPro, ;
                F18nTipDoc With CNFENT.TipDoc, ;
                F18nNumDoc With CNFENT.NumDoc, ;
                F18nNumEnt With CNFENT.NumEnt, ;
                F18nCodArt With CNFENT.CodArt, ;
                F18nNumLot With CNFENT.NumLot, ;
                F14cUbiOri With CNFENT.CodUbi, ;
                F18nCanRec With CNFENT.CanFis
     EndIf
     
     *> Obtener datos del MP.----------------------------------------------------------
     f_where = "F14cNumMov='" + CNFENT.NumMov + "'"
     lStado = f3_sql("*", "F14c", f_where, , , 'F14cCur')
     If !lStado
       _LxErr = 'Error leyendo datos de Movimientos Pendientes' + cr + ;
                'MENSAJE: ' + Message() + cr
       Do Form St3Inc With .T.
       Return .T.
     EndIf

     =SqlMoreResults(_ASql)
     Select CNFENT1
     Replace F14cUbiOri With F14cCur.F14cUbiOri

     *> Descripción propietario.
     m.F01pCodigo = CNFENT1.F18nCodPro
     =f3_seek('F01P')

     *> Descripción artículo.
     m.F08cCodPro = CNFENT1.F18nCodPro
     m.F08cCodArt = CNFENT1.F18nCodArt
     =f3_seek('F08C')

     *> Obtener el transportista.
     m.F01tCodigo = CNFENT1.F18mCodTra
     If !f3_seek('F01t')
        Select F01t
        Append Blank
        Go Top
     EndIf

     Select CNFENT1
     Replace F01pDescri With F01p.F01pDescri, ;
             F08cDescri With F08c.F08cDescri, ;
             F01tDescri With F01t.F01tDescri

     If Inicio=.T.
        =CrtCursor('CNFENT1', 'XLCNFENT1', 'C')            
        Inicio=.F.
     EndIf 
     
     *>añadir nuevas Lineas de documentos para imprimir
     Select XLCNFENT1
     Append From Dbf('CNFENT1')

     *>
     Select CNFENT
     Continue
   EndDo 
   
   _Label1 = "CONFIRMACION DE ENTRADAS"
   If Inicio=.F.
      Select XLCNFENT1
      Go Top 
      Do Form St3Imp2 With _Label1,'ENTRCNFEL1', 'XLCNFENT1'
   Else
      =f3_sn(1,1,"No hay lineas para imprimir")    
   EndIf   

   *> Cerrar cursor generado por SQL
   If Used('CNFENT1')
      Use in CNFENT1
   EndIf

   If Used('XLCNFENT1')
      Use in XLCNFENT1
   EndIf

Return

******************************************************************
*> Listado de Devoluciones de Material.
*> Llamado desde DEVCEDIT, edición de devoluciones de clientes.
******************************************************************
Procedure DevCEditL1

   Select DEVCEDIT
   Go Top
   If !Eof()
      *> Generar datos de ESTA devolución.
      Do GenDevCli With F19lNumDev

      *> Imprimir devolución de cliente.
      Do PrtDevCli

      If Used('XLDevCli')
         Use In XLDevCli
      EndIf
   EndIf

Return

************************************************************************
*> Procedimiento de impresión de devoluciones de clientes.             *
*> Genera XLDevCli, que después se envía al report.                    *
*> Primer paso: Generar datos del Nº de devolución actual.             *
*>                                                                     *
*> Recibe: _NumDev ---> Nº de devolución de cliente.                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - DevCEdit ----> Edición de devoluciones de clientes.             *
************************************************************************
Procedure GenDevCli

   Parameters _NumDev

   Private lx_sql, lx_select, f_from, f_where, f_orden
   Private Err

   *> Formar strings de conexión ORACLE.
   lx_select= "Select * "

   f_from = "From F19l" + _em + Space(1)

   f_where= "Where F19lNumDev='" + _NumDev + "' "

   f_Orden = " Order By F19lNumDev, F19lCodArt, F19lNumLot"

   lx_sql = lx_select + f_from + f_where + f_orden
   Err = SqlExec(_aSql, lx_sql, 'NCursor')
   =SqlMoreResults(_aSql)

   If Err <= 0
      _LxErr = "Error al generar datos impresión de devoluciones de clientes" + cr + ;
               "MENSAJE: " + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return
   EndIf

   Select NCursor
   _a = Dbf()

   *> Añadir datos a XLDevCli, utilizado por el Report.
   If !Used('XLDevCli')
      =CrtCursor('NCursor', 'XLDevCli', 'C')            
   EndIf 

   *> Si no hay datos a tratar, abandonar el proceso.
   Select XLDevCli
   Append From &_a
   Use In NCursor

Return

************************************************************************
*> Procedimiento de impresión de devoluciones de clientes.             *
*> Segundo paso: Imprimir los datos generados por GenDevCli.           *
*> Recibe: XLDevCli --> Cursor que se envía al Report.                 *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - DevCEdit ---> Edición de devoluciones.                          *
*>   - DevCList ---> Impresión de devoluciones.                        *
************************************************************************
Procedure PrtDevCli

   _Label1 = "IMPRESION DE DEVOLUCIONES DE CLIENTE"

   If Used('XLDevCli')
      Select XLDevCli
      Go Top
      Do Form St3Imp2 With _Label1, 'DevCListL1', 'XLDevCli'
   Else
      _LxErr = 'No hay Devoluciones de Clientes para imprimir' + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLDevCli')
      Use in XLDevCli
   EndIf
   
*>
Return

**************************************
*>  Albarán de Transportista Global <*
**************************************
Procedure SaExAlbTL1
Private f_select, f_from, f_where, lxselect

f_select = " Select distinct F01pDescri, F01pNumNif, F01pDirecc, F01pPoblac, F01pNumTel, F01pNumFax," + ;
           "                 F00jDescri," + ;
           "				 F01tDescri, F01tNumNif," + ;
           "                 F24tNomAso, F24t1ERDir, F24tDpobla, F24tDprovi, F24tCodpos," + ; 	
           "				 F30cAlbRep, F30cCodEnt, F30cBultos, F30cPesoKg, F30cVolume," + ;
 		   "                 F31cHojRut, F31cFecRut, F31cCodVeh"
 		   
f_from   = " From F31c" + _em + "," + ;
           "      F30c" + _em + "," + ;
           "      F24t" + _em + "," + ;
           "      F01t" + _em + "," + ;
           "      F01p" + _em + "," + ;
           "      F00j" + _em  

f_where =  " Where F31cHojRut ='" + m.HojRut + _cm + ;
           " And   F30cHojRut = F31cHojRut" + ;
           " And   F24tCodPro(+) = F30cCodEnt" + ;
           " And   F24tTipDoc(+) = F30cTipDoc" + ;
           " And   F24tNumDoc(+) = F30cNumDoc" + ;          
           " And   F01tCodigo(+) = F30cCodTra" + ;
           " And   F01pCodigo(+) = F30cCodEnt" + ;
           " And   F00jCodPrv(+) = F01pProvin"

lxselect= f_select + f_from + f_where 
Err=SqlExec(_aSql, lxselect,'XLALBTRA')
If Err < 0
   _LxErr = "Error al cargar los datos para el listado" + cr
   Do Form St3Inc
   _LxErr = ''
   Return
EndIf

=SqlMoreResults(_aSql)
Select XLALBTRA
Go Top
If Eof()
   _LxErr = "No hay datos entre los límites seleccionados" + cr
   Do Form St3Inc
   _LxErr = ''
   Return
EndIf

_Label1="ALBARAN DE TRANSPORTE GLOBAL"
Do Form St3Imp2 With _Label1, 'SAEXALBTL1', 'XLALBTRA'
Return 

***************************************
*>  Albarán de Transportista Detalle <*
***************************************
Procedure SaExAlbDL1

_Label1="ALBARAN DE TRANSPORTE DETALLE"
Do Form St3Imp2 With _Label1, 'SAEXALBDL1', 'DETALB'
Return 

************************************************************************
*> Procedimiento de impresión de listas de inventario, recuento.       *
*> Genera XLRecL, que después se envía al report.                      *
*>                                                                     *
*> Adaptar a compatibilidad SQL. AVC - 18.09.2002                      *
*>                                                                     *
*> Primer paso: Generar datos del Nº de lista actual.                  *
*>                                                                     *
*> Recibe: _NumLst ---> Nº de lista de inventario.                     *
*>         _OrdRec ---> Recorrido: Vertical/Horizontal.                *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - LInvLRecL1 ----> Impresión listas de inventario, recuento.      *
************************************************************************
Procedure LInvLRec
Parameters _NumLst, _OrdRec
Private lx_sql, lx_select, f_from, f_where, f_orden
Private Err
Private cField, cFromF, cWhere, cOrder
Local lStado

   *> Formar strings de conexión con la BD.
   cField = "F50c" + _em + ".*, F50l" + _em + ".*, '" + Space(40) + "' As F08cDescri"
   cFromF = "F50c,F50l"
   cWhere = "F50cNumInv='" + _NumLst + "' And F50lNumInv=F50cNumInv"

   *> Establecer el orden según el recorrido.
   If _OrdRec=='H'
      cOrder = "F50lNumInv, " + ;
               _GCSS("F50lCodUbi", 1, 8) + ", " + ;
               _GCSS("F50lCodUbi",12, 2) + ", " + ;
               _GCSS("F50lCodUbi", 9, 3) + ", " + ;
               "F50lCodArt, F50lNumLot"
   Else
      cOrder = "F50lNumInv, F50lCodUbi, F50lCodArt, F50lNumLot"
   EndIf

   lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'LRecCur')

   *> Añadir la descripción del artículo.
   Select LRecCur
   Go Top
   Do While !Eof()
      m.F08cCodPro = F50lCodPrt
      m.F08cCodArt = F50lCodArt
      If f3_seek('F08c')
         Replace LRecCur.F08cDescri With F08c.F08cDescri
      EndIf

      Select LRecCur
      Skip
   EndDo

   *> Añadir datos a XLRecL, utilizado por el Report.
   If !Used('XLRecL')
      =CrtCursor('LRecCur', 'XLRecL', 'C')            
   EndIf 

   *> Si no hay datos a tratar, abandonar el proceso.
   Select XLRecL
   Append From Dbf('LRecCur')
   Use In LRecCur

Return

************************************************************************
*> Procedimiento de impresión de listas de inventario, recuento.       *
*> Segundo paso: Imprimir los datos generados por LInvLRec.            *
*> Recibe: XLRecL --> Cursor que se envía al Report.                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
************************************************************************
Procedure PrtLInvLRec

   _Label1 = "IMPRESION DE LISTAS DE INVENTARIO - RECUENTO"

   If Used('XLRecL')
      Select XLRecL
      Replace All F50lCodPrt With Iif(Type('_Procaot')=='C', _Procaot, F50lCodPrt)
      Go Top
      Do Form St3Imp2 With _Label1, 'LinvLRecL1', 'XLRecL'
   Else
      _LxErr = 'No hay datos para imprimir' + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLRecL')
      Use in XLRecL
   EndIf
   
Return

************************************************************************
*> Procedimiento de impresión de listas de inventario, control.        *
*> Genera XLCtrL, que después se envía al report.                      *
*> Primer paso: Generar datos del Nº de lista actual.                  *
*>                                                                     *
*> Recibe: _NumLst ---> Nº de lista de inventario.                     *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - LInvLCtrL1 ----> Impresión listas de inventario, control.       *
************************************************************************
Procedure LInvLCtr
Parameters _NumLst, _OrdRec
Private lx_sql, lx_select, f_from, f_where, f_orden
Private Err
Private cField, cFromF, cWhere, cOrder
Local lStado

   *> Formar strings de conexión con la BD.
   cField = "F50c" + _em + ".*, F50l" + _em + ".*, '" + Space(40) + "' As F08cDescri"
   cFromF = "F50c,F50l"
   cWhere = "F50cNumInv='" + _NumLst + "' And F50lNumInv=F50cNumInv"

   *> Establecer el orden según el recorrido.
   If _OrdRec=='H'
      cOrder = "F50lNumInv, " + ;
               _GCSS("F50lCodUbi", 1, 8) + ", " + ;
               _GCSS("F50lCodUbi",12, 2) + ", " + ;
               _GCSS("F50lCodUbi", 9, 3) + ", " + ;
               "F50lCodArt, F50lNumLot"
   Else
      cOrder = "F50lNumInv, F50lCodUbi, F50lCodArt, F50lNumLot"
   EndIf

   lStado = f3_sql(cField, cFromF, cWhere, cOrder, , 'LRecCtr')

   *> Añadir la descripción del artículo.
   Select LRecCtr
   Go Top
   Do While !Eof()
      m.F08cCodPro = F50lCodPrt
      m.F08cCodArt = F50lCodArt
      If f3_seek('F08c')
         Replace LRecCtr.F08cDescri With F08c.F08cDescri
      EndIf

      Select LRecCtr
      Skip
   EndDo

   *> Añadir datos a XLCtrL, utilizado por el Report.
   If !Used('XLCtrL')
      =CrtCursor('LRecCtr', 'XLCtrL', 'C')            
   EndIf 

   *> Si no hay datos a tratar, abandonar el proceso.
   Select XLCtrL
   Append From Dbf('LRecCtr')
   Use In LRecCtr

Return

************************************************************************
*> Procedimiento de impresión de listas de inventario, control.        *
*> Segundo paso: Imprimir los datos generados por LInvLCtr.            *
*> Recibe: XLCtrL --> Cursor que se envía al Report.                   *
*>                                                                     *
*> Módulo llamado desde:                                               *
************************************************************************
Procedure PrtLInvLCtr

   _Label1 = "IMPRESION DE LISTAS DE INVENTARIO - CONTROL"

   If Used('XLCtrL')
      Select XLCtrL
      Go Top
      Do Form St3Imp2 With _Label1, 'LinvLCtrL1', 'XLCtrL'
   Else
      _LxErr = 'No hay datos para imprimir' + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLCtrL')
      Use in XLCtrL
   EndIf
   
*>
Return

************************************************************************
*> Procedimiento de impresión de los numeros de serie por documento.   *
*>                            										   *
*> Recibe: NumDoc --> El numero del documento.                         *
*>                                                                     *
*> Módulo llamado desde:SAliAlbc                                       *
************************************************************************
Procedure SaliAlbcS

 Parameters _NumDoc

   Private lx_sql, lx_select, f_from, f_where, f_orden
   Private Err

   *> Formar strings de conexión ORACLE.
   lx_select= "Select F24s" + _em + ".*" + Space(1)

   f_from = "From F24S" + _em  + Space(1)

   f_where= "Where F24SNumDoc='" + _NumDoc + "'"

   f_Orden = " Order By F24sLinDoc"

   lx_sql = lx_select + f_from + f_where + f_orden
   Err = SqlExec(_aSql, lx_sql, 'SerialCur')
   =SqlMoreResults(_aSql)

   If Err <= 0
      _LxErr = "Error al buscar los numeros de serie" + cr + ;
               "MENSAJE: " + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return
   EndIf
   
   
   _Label1 = "IMPRESION DE NUMEROS DE SERIE DE ARTÍCULOS"

   If Used('SerialCur')
      Select SerialCur
      Go Top
      Do Form St3Imp2 With _Label1, 'LISTnUMsER', 'SerialCur'
   Else
      _LxErr = 'No hay datos para imprimir' + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('SerialCur')
      Use in SerialCur
   EndIf
   
************************************************************************
*> Procedimiento de listado de articulos por cliente.                  *
*>                            										   *
*> Recibe: F01pCodigo --> ElPropietario.                               *
*>                                                                     *
*> Módulo llamado desde:FartxCli                                       *
************************************************************************
Procedure FartxCli
Parameters _CodIni,_CodFin

Private lx_sql, lx_select, f_from, f_where, f_orden
Private Err

Create Cursor Listado(CodPro Char(6), ; 
                      Despro Char(40), ;
                      CodArt Char(13), ;
                      DEscri Char(40), ;
                      UniPAc Number(6), ;
                      PacCaj Number(6), ;
                      CajPal Number(6), ;
                      CajMan Number(5), ;
                      TipUni Char(4), ;
                      TamAbi Char(4), ;
                      Voluni Number(12,4), ;
                      PesUni Number(12,4), ;
                      seguro Number(6,0), ;  
                      Cantidad number(10,2))


   *> Formar strings de conexión ORACLE.
   f_select= "*"
   f_from = "F08c,F01p"
   f_where= "F08cCodPro Between '" + _CodIni + "' And '" + _CodFin + "'"
   f_where= f_where + " And F01pCodigo=F08cCodPro"
   f_Orden= "F08cCodPro,F08cCodArt"

   =f3_sql(f_select, f_from, f_where, f_orden, , "Articulos")

   Select Articulos
   Go Top
   Do While !Eof()
      *> Obtener el stock de cada producto.
      f_select = GetCvtNvl(_ENTORNO, _VERSION, "Sum(F16cCanFis)") + " As CANTIDAD"
      f_from  = "F16c"
      f_where = "F16cCodPro='" + F08cCodPro + "' And F16cCodArt='" + F08cCodArt + "'"

      =f3_sql(f_select, f_from, f_where, , , 'F16cCantid')

      *> Variables de cantidades, primero inicializarlas (Es un bucle).
      _Volumen = 0 
      _Peso = 0 
      _Coste = 0
      _Cuantos = 0

      _Volumen = Articulos.F08cVoluni
      _Peso = Articulos.F08cPesuni
      _Coste = Articulos.F08cPcoste
   
      _Cuantos = Articulos.F08cUniPAc * Articulos.F08cPacCaj * Articulos.F08cCajPal * Articulos.F08cCajMan * f16CCANTID.cANTIDAD
      _Volumen = IIF(_Volumen=0,0,Round(_Volumen ,4))
      _Peso = IIF(_peso = 0,0,Round(_Peso,4))
     
      Select Listado
      Go Top
      Append Blank

      Replace Codpro With Articulos.F08cCodPro, ;
              Despro With Propietario.F01pdescri, ;
              CodArt With Articulos.F08cCodArt, ;
              DEsCri With Articulos.F08cdescri, ;
              TamAbi With Articulos.F08ctamAbi, ;
              UniPac With Articulos.F08cUniPAc, ;
              PacCaj With Articulos.F08cPacCaj, ;
              CajPal With Articulos.F08cCajPal, ;
              CajMan With Articulos.F08cCajMan, ;
              TipUni With Articulos.F08cTipUni, ;
              VolUni With _Volumen, ;
              Pesuni With _Peso, ;
              Seguro With _Coste, ;
              Cantidad With _Cuantos

      Select Articulos  
      Skip
   Enddo
     
   _Label1 = "LISTADO DE ARTÍCULOS POR CLIENTE"

   Select Listado
   Go Top
   If !Eof()
      Do Form St3Imp2 With _Label1,'Fartxcli','Listado'
   Else
      _LxErr = 'No hay datos para imprimir' + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('Listado')
      Use in Listado
   EndIf

Return

************************************************************************
*> Impresión de auditoría de artículos.                                *
*> Primer paso: Generar datos para impresión.                          *
*>                                                                     *
*> Recibe: cCodProI --> Propietario inicial.                           *
*>         cCodProH --> Propietario final.                             *
*>         cCodArtI --> Artículo inicial.                              *
*>         cCodArtH --> Artículo final.                                *
*>         lTodos   --> .T., Todos los artículos.                      *
*>                  --> .F., Solo artículos con incidencias.           *
*>                                                                     *
*> Genera: XLAUDIT ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FtGenAdt    ---> Auditoría de artículos.                        *
************************************************************************
Procedure GenAudit
Parameters cCodProI, cCodProH, cCodArtI, cCodArtH, cAuditParm, lTodos
Private lEstado
Local cWhere, cField, cFormF, cOrder, cGroup
Local oAUDIR, oAUDIT1, cUbiPic

If Used('XLAUDITCUR')
   Use In XLAUDITCUR
EndIf
If Used('XLAUDITCUR1')
   Use In XLAUDITCUR1
EndIf

*> Cargar los artículos a procesar.
cField =          "F08c" + _em + ".*, '" + Space(14) + "' As F08cUbiPic, "
cField = cField + "0 As nCodBar, 0 As nTipPro, 0 As nTipArt, 0 As nFacPal, "
cField = cField + "0 As nPesUni, 0 As nVolUni, 0 As nUbiPic, 0 As nUniMed"

cFromF = 'F08c'
cOrder = 'F08cCodPro, F08cCodArt'
cGroup = ''

cWhere = "F08cCodPro Between '" + cCodProI + "' And '" + cCodProH + "' And " + ;
         "F08cCodArt Between '" + cCodArtI + "' And '" + cCodArtH + "'"

=f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'XLAUDITCUR')
Select XLAUDITCUR
Go Top

*> Copiar registro a cursor de trabajo. Si no existe, se crea.
If !Used('XLAUDIT')
   Select XLAUDITCUR
   =AFields(vFields)
   Create Cursor XLAUDIT From Array vFields
EndIf

*> Para cada producto, cargar su ubicación de picking unidades (Solo PAs y MAs).
Select XLAUDITCUR
Go Top
Do While !Eof()
   Scatter Name oAUDIT

   cField = 'F12cCodUbi'
   cFromF = 'F12c,F10c'
   cOrder = ''
   cGroup = ''
   cWhere = "F12cCodPro='" + XLAUDITCUR.F08cCodPro + "' And " + ;
            "F12cCodArt='" + XLAUDITCUR.F08cCodPro + "' And " + ;
            "F10cCodUbi=F12cCodUbi And (F10cPickSN='U' Or F10cPickSN='U')"

   =f3_sql(cField, cFromF, cWhere, cOrder, cGroup, 'XLAUDITCUR1')
   Select XLAUDITCUR1
   Go Top
   cUbiPic = Iif(Eof(), Space(14), XLAUDITCUR1.F12cCodUbi)

   Select XLAUDIT
   Append Blank
   Gather Name oAUDIT
   Replace F08cUbiPic With cUbiPic

   *>
   Select XLAUDITCUR
   Skip
EndDo

*> Validar registro, si parámetro 'solo con incidencias' activo.
If !lTodos
   Select XLAUDIT
   Go Top
   Do While !Eof()
      Do Case
         *> Código de barras.
         Case SubStr(cAuditParm, 1, 1)=='S' .And. Empty(F08cCodEan)
            Replace XLAUDIT.nCodBar With 1
            Skip
            Loop

         *> Clase artículo (sección).
         Case SubStr(cAuditParm, 2, 1)=='S' .And. !VrfAudit(2, F08cSeccio)
            Replace XLAUDIT.nTipPro With 1
            Skip
            Loop

         *> Tipo artículo.
         Case SubStr(cAuditParm, 3, 1)=='S' .And. Empty(F08cTipPro)
            Replace XLAUDIT.nTipArt With 1
            Skip
            Loop

         *> Factores.
         Case SubStr(cAuditParm, 4, 1)=='S' .And. ;
            (F08cUniPac==0 Or F08cPacCaj==0 Or F08cCajPal==0)
            Replace XLAUDIT.nFacPal With 1
            Skip
            Loop

         *> Volumen unitario.
         Case SubStr(cAuditParm, 5, 1)=='S' .And. Empty(F08cVolUni)
            Replace XLAUDIT.nVolUni With 1
            Skip
            Loop

         *> Peso unitario.
         Case SubStr(cAuditParm, 6, 1)=='S' .And. Empty(F08cPesUni)
            Replace XLAUDIT.nPesUni With 1
            Skip
            Loop

         *> Ubicación de picking.
         Case SubStr(cAuditParm, 7, 1)=='S' .And. Empty(F08cUbiPic) .And. ;
            (F08cSeccio=='PA' Or F08cSeccio=='MA')
            Replace XLAUDIT.nUbiPic With 1
            Skip
            Loop

         *> Medidas del producto.
         Case SubStr(cAuditParm, 7, 1)=='S' .And. ;
            (F08cMatLar==0 Or F08cMatAlt==0 Or F08cMatAnc==0)
            Replace XLAUDIT.nUniMed With 1
            Skip
            Loop
      EndCase

      *> Artículo sin incidencias: Borrar de cursor.
      Replace F08cCodArt With Space(13)
      Skip
   EndDo
EndIf


If Used('XLAUDITCUR')
   Use In XLAUDITCUR
EndIf
If Used('XLAUDITCUR1')
   Use In XLAUDITCUR1
EndIf

Return

************************************************************************
*> Impresión de auditoría de artículos.                                *
*> Segundo paso: Realizar la impresión.                                *
*>                                                                     *
*> Recibe: XLAUDIT ---> Cursor con los datos a imprimir.               *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FtGenAdt    ---> Auditoría de artículos.                        *
************************************************************************
Procedure PrtAudit

   If Used('XLAUDIT')
      Select XLAUDIT
      Go Top
      _Label1 = "AUDITORIA DE ARTICULOS"
      Do Form St3Imp2 With _Label1, 'FTGENADTL1', 'XLAUDIT', , "!Empty(F08cCodArt)"
   Else
      _LxErr = "No hay datos para imprimir la auditoría" + cr
      Do Form St3Inc With .T.
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLAUDIT')
      Use in XLAUDIT
   EndIf
   
Return

************************************************************************
*> Impresión de auditoría de artículos.                                *
*> Procesos asociados de validación.                                   *
*>                                                                     *
*> Recibe: nWhat   ---> Tipo de parámetro a validar.                   *
*>         cParam1 ---> Parámetro adicional 1.                         *
*>         cParam2 ---> Parámetro adicional 2.                         *
*>         cParam3 ---> Parámetro adicional 3.                         *
*>                                                                     *
*> Módulo llamado desde:                                               *
*>   - FtGenAdt    ---> Auditoría de artículos.                        *
************************************************************************
Procedure VrfAudit
Parameters nWhat, cParam1, cParam2, cParam3
Local lEstado

Store .T. To lEstado

Do Case
   *> Validación adicional a tipo artículo (sección).
   Case nWhat==2
      lEstado = f3_seek('F00N', [m.F00nCodSec=cParam1])

   *> Resto validaciones: Ignorar.
EndCase

Return lEstado

************************************************************************
*> Impresión de Ficha de composición de Articulos.                     *
*> Primer paso: Generar datos del  Cursor							   *
*>                                                                     *
*> Recibe: . 						                                   *
*>                                                                     *
*> Genera: XLCOMART  ---> Cursor con los datos a imprimir.             *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - FTCOMCOM2 ------> Impresión de Ficha de composición de Articulos. *
*>                                                                     *
************************************************************************
Procedure GenCompArt
Parameters _Prop,_Art
Local lStado
Private _Fields, _Where, _From, _Selec
Local lStado


_Where = "F08lCodPro='"  + _Prop  + "' And " + ;
         "F08lCODART='"  + _Art   + "'" 

_okl=f3_sql('*', 'F08L', _Where, , , 'F08L')

*> Copiar datos a cursor de trabajo. Si no existe, se crea.
If !Used('XLCOMART')
   =CrtCursor('F08L', 'XLCOMART', 'C')
EndIf

Select XLCOMART
Append From Dbf('F08L')

Return


************************************************************************
*> Impresión de Ficha de composición de Articulos.                     *
*> Segundo paso: Imprimir los datos generados por GenCBul.             *
*>                                                                     *
*> Recibe: XLCOMART ----> Cursor con los datos a imprimir.             *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - FTCOMCOM2 ------> Impresión de Ficha de composición de Articulos. *
************************************************************************
Procedure PrtCompArt  
Private loImp2

   If Used('XLCOMART')
      Select XLCOMART
      Go Top
      _Label1 = "IMPRESION DE FICHA DE COMPOSICIÓN DE ARTICULOS"
      Do Form St3Imp2 With _Label1,'FTCOMCOM2','XLCOMART'

      *loImp2.Release
   Else
      _LxErr = "No hay datos de composición de Articulos." + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLCOMART')
      Use in XLCOMART
   EndIf
   
Return


************************************************************************
*> Impresión de Partes de montaje					                   *
*> Primer paso: Generar datos del  Cursor							   *
*>                                                                     *
*> Recibe: . 						                                   *
*>                                                                     *
*> Genera: XLSALIPMON  ---> Cursor con los datos a imprimir.           *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - SALIPMON ------> Impresión de Ficha de Parte de montaje. 		   *
*> - SALIRELP ------> Impresión listado de Fichas de Parte de montaje, *
*>                    entre límites                                    *
************************************************************************

*> 03.12.2007 (AVC) Modificado el tratamiento de observaciones, que pueden no existir.

Procedure GenParte
Parameters _Prop, _TDoc, _nParte
Private cCampos, f_where, cUbiPick
Local lStado, _ok, _TDocEntrada, oObs

_TDocEntrada = Val(_TDoc) - 1000
_TDocEntrada = AllTrim(Str(_TDocEntrada))

cCampos = " F24CTIPENT As TipEnt, F18LCANPED As Cantidad, F24LNUMDOC As nPedido," + ;
 		  "	F24CCODENT As CodEnt, F08CDESCRI As Descri, F24lCodPro As CodPro," + ;
 		  " F24LCODART As CodCom, F24LCANDOC As Cantid, F24LPRECIO As Precio," + ;
 		  " F18LCODART As Articulo, F24LTIPDOC As TipoSalida, '0000' As F24OLINOBS," + ;
 		  "'" + Space(14) + "' As UbiPck"

f_where = " F24LCODPRO='" + _Prop + "' And F24LNUMDOC='" + _nParte + "' And " + ;
 		  " F24LTIPDOC='" + _TDoc  + "' And " + ;
 		  " F24CCODPRO=F24LCODPRO And F24CNUMDOC=F24LNUMDOC And " + ;
		  " F08CCODPRO=F24LCODPRO And F08CCODART=F24LCODART And " + ;
		  " F18LCODPRO= F24CCODPRO And F18LTIPDOC='" + _TDocEntrada + "' And " + ;
		  " F18LNUMDOC=F24CNUMDOC"

_ok = f3_sql(cCampos, 'F24C,F24L,F08C,F18L', f_Where, , , '+XLSALIPMON')

Select XLSALIPMON
Go Top
Do While !Eof()
	*> Guardar la ubicación de picking, a efectos de listado.
	cCampos = "F12cCodUbi"
	f_Where = "F12cCodPro='" + CodPro + "' And F12cCodArt='" + CodCom + "'"
	_ok = f3_sql(cCampos, "F12c", f_where, , , "F12cUbi")
	If _ok
		Select F12cUbi
		Go Top
		cUbiPick = F12cCodUbi
	Else
		cUbiPick = ''
	EndIf
	Use In F12cUbi
			
	Select XLSALIPMON
	Replace UbiPck With cUbiPick
	Skip
EndDo

Go Top
Return

************************************************************************
*> Impresión de Ficha de Partes de montaje. 	                       *
*> Segundo paso: Imprimir los datos generados por GenPArte.            *
*>                                                                     *
*> Recibe: XLSALIPMON ----> Cursor con los datos a imprimir.           *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - SALIPMON ------> Impresión de Ficha de Parte de montaje.		   *
************************************************************************
Procedure PrtParte  
Private loImp2
Local _Label1, frmImpresion, cArticulo

   If Used('XLSALIPMON')
      Select XLSALIPMON
      Go Top
	  If !Eof()
		  cArticulo = Articulo

		  *> Listado de composición.
		  _Label1 = 'LISTADO DE PARTES DE MONTAJE-COMPOSICION'
	      frmImpresion = CreateObject('st3imp2', 'W', [LABEL=&_Label1, REPORT=SALIPMON,CURSOR=XLSALIPMON])

		  *> Imprimir listado con foto. Solo si existe la foto asociada al producto acabado.
	      If File(_FotoDir + Alltrim(cArticulo) + _FotoExt)
		     Select XLSALIPMON
      		 Go Top
		     _Label1 = 'LISTADO DE PARTES DE MONTAJE-FOTO'
			 frmImpresion.exec('?', [REPORT=SALIPMONF])
	      EndIf

		  frmImpresion.Release
	  EndIf
   Else
      _LxErr = "No hay datos de Parte de montaje"
      Do Form St3Inc With .T.
   EndIf

*> Cerrar cursor generado por SQL.
If Used('XLSALIPMON')
 Use in XLSALIPMON
EndIf
   
Return

************************************************************************
*> Impresión de Ficha de Partes de montaje. 	                       *
*> Segundo paso: Imprimir los datos generados por GenParte.            *
*>                                                                     *
*> Recibe: XLSALIPMON ----> Cursor con los datos a imprimir.           *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - SALIRELP ------> Impresión listado de Fichas de Parte de montaje, *
*>                    entre límites                                    *
************************************************************************
Procedure PrtSaliRelPParte
Private loImp2
Local _Label1, frmImpresion, cArticulo

   If Used('XLSALIPMON')
      Select XLSALIPMON
      Go Top
      If !Eof()
		  *> Listado de composición
		  _Label1 = 'LISTADO DE PARTES DE MONTAJE-COMPOSICION'
	      frmImpresion = CreateObject('st3imp2', 'W', [LABEL=&_Label1, REPORT=SALIRELPL1,CURSOR=XLSALIPMON])

	      *> Imprimir listado con foto. Solo si existe la foto asociada al producto acabado.
	      Select XLSALIPMON
	      Go Top
	      Do While !Eof()
	         cArticulo = Articulo
		  	 If !File(_FotoDir + Alltrim(cArticulo) + _FotoExt)
		  	    Delete Next 1
		  	 EndIf
		  	 Skip
		  EndDo

          Select XLSALIPMON
	      Go Top
		  If !Eof()
		      _Label1 = 'LISTADO DE PARTES DE MONTAJE-FOTO'
			  frmImpresion.exec('?', [REPORT=SALIRELPLF])
		  EndIf

		  frmImpresion.Release
	  EndIf
   Else
      _LxErr = "No hay datos de Parte de montaje" + cr
      Do Form St3Inc With .T.
   EndIf

*> Cerrar cursor generado por SQL.
If Used('XLSALIPMON')
 Use in XLSALIPMON
EndIf

Return

************************************************************************
*> Impresión de Cabeceras de Partes de montaje					       *
*> Primer paso: Generar datos del  Cursor							   *
*>                                                                     *
*> Recibe: . 						                                   *
*>                                                                     *
*> Genera: XLSALIRELPR1  ---> Cursor con los datos a imprimir.         *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - SALIRELPR1 ------> Impresión de CABECERAS de Parte de montaje.	   *
*>                                                                     *
************************************************************************
Procedure GenParteCabeceras
Parameters _Prop, _TDoc, nParte

Private cCampos, f_where
Local lStado, _ok

cCampos = " F18LCANPED as Cantid, F18LNUMDOC as nPedido,F18CFECPED,F18CFECPRE, " + ;
 		  "	F18CTIPENT as TipEnt, F18CCODENT as CodEnt,F08CDESCRI as Descri,F18LCODPRO As CodPro," + ;
 		  " F18LCODART As Articulo,F01CDESCRI as DescTaller"

f_where = " F18LCODPRO='" + _Prop + "' And F18LNUMDOC='" + nParte + "' And " + ;
 		  " F18LTIPDOC='" + TDocEntrada  + "' And " + ;
		  " F08CCODPRO=F18LCODPRO And F08CCODART=F18LCODART And F18CCODPRO=F18LCODPRO And " + ;
		  " F18CTIPDOC= F18LTIPDOC And F18CNUMDOC=F18LNUMDOC And F01CTIPENT=F18CTIPENT And " + ;
		  " F18CCODENT=F01CCODIGO" 

_ok = f3_sql(cCampos, 'F08C,F18L,F18C,F01C', f_Where, , , '+SALIRELPR1')

Select SALIRELPR1 
Return

************************************************************************
*> Impresión de Ficha de Partes de montaje. 	                       *
*> Segundo paso: Imprimir los datos generados por GenPArte.            *
*>                                                                     *
*> Recibe: XLSALIPMON ----> Cursor con los datos a imprimir.           *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - SALIRELPR1 ------> Impresión de Cabeceras de Parte de montaje.	   *
************************************************************************
Procedure PrtParteCabeceras  
Private loImp2
Local _Label1
Local frmImpresion

   If Used('SALIRELPR1')
      Select SALIRELPR1
      Go Top
	  _Label1 = 'LISTADO DE PARTES DE MONTAJE'
      frmImpresion = CreateObject('st3imp2', 'W', [LABEL=&_Label1, REPORT=SALIRELPR1,CURSOR=SALIRELPR1])
	  frmImpresion.Release
   Else
      _LxErr = "No hay datos de Cabeceras de Partes de montaje." + cr
      Do Form St3Inc With .T.
   EndIf

*> Cerrar cursor generado por SQL.
If Used('SALIRELPR1')
 Use in SALIRELPR1
EndIf
   
Return

************************************************************************
*> Impresión de Artículos formados por un componente.                  *
*> Primer paso: Generar datos del  Cursor							   *
*>                                                                     *
*> Recibe: . 						                                   *
*>                                                                     *
*> Genera: XLARTPORCOM  ---> Cursor con los datos a imprimir.          *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - FTCOMCOM3 ------> Impresión Componente / Articulos.			   *
************************************************************************
Procedure GenCompArtPorComponente
Parameters _Prop,_Com
Private _Fields, _Where, _From, _Selec
Local lStado

_Where = "F08LCodPro='"  + _Prop  + "' And " + ;
         "F08LCODCOM='"  + _Com   + "' ORDER BY F08LCODART" 

_okl=f3_sql('*', 'F08L', _Where, , , '+XLARTPORCOM')

Select XLARTPORCOM
Return

************************************************************************
*> Impresión de Articulos formados por un componente.                  *
*> Segundo paso: Imprimir los datos generados por GenCompArt.          *
*>                                                                     *
*> Recibe: XLARTPORCOM ----> Cursor con los datos a imprimir.          *
*>                                                                     *
*> Módulo llamado desde:                                               *
*> - FTCOMCOM3 ------> Impresión Componente / Articulos.			   *
************************************************************************
Procedure PrtArtPorComponente  
Private loImp2

   If Used('XLARTPORCOM')
      Select XLARTPORCOM
      Go Top
      _Label1 = "IMPRESION ARTICULOS ASOCIADOS A COMPONENTE"
      loImp2 = CreateObject('st3imp2', '?', [LABEL=&_Label1, REPORT=FTCOMCOM3,CURSOR=XLARTPORCOM])
	  loImp2.Release
   Else
      _LxErr = "No hay datos de composición de Articulos." + cr
      Do Form St3Inc
      _LxErr = ''
   EndIf

   *> Cerrar cursor generado por SQL.
   If Used('XLARTPORCOM')
      Use in XLARTPORCOM
   EndIf
   
Return

************************************************************************
*> Impresión de Packing List.
*> Primer paso: Generar datos para el documento actual.

*> Recibe: CodPro, TipDoc, NumDoc ---> Documento a calcular.

*> Genera: XLALRG ---> Cursor con los datos a imprimir.

*> Módulo llamado desde:
*>   - SalPacking ---> Impresión de Packing List.
************************************************************************
Procedure GenPacking
Parameters cCodPro, cTipDoc, cNumDoc

Local lStado
Private cSel, cWhere, cGroup, cOrder, cFromF

	cSel = "F26lCodPro,F26lTipDoc,F26lNumDoc,F26lCodArt,F26lNumLot,F26lNumMas,F26lNumMac,F08cDesCri, " + ;
	   	   "F24tNomAso,F24t1ErDir,F24t2NdDir,F24t3RdDir,F24tDPobla,F24tDProvi, " + ;
	       "F24tCodPos,F01pCodigo,F01pDescri,'" + Space(40) + "' As F01tDesCri, Sum(F26lCanFis) As F26lCanFis"

	*> Cargar los MPs de expedición del documento actual.
	cWhere = "F26lCodPro = '" + cCodPro + "' And " + ;
			 "F26lTipDoc = '" + cTipDoc + "' And " + ;
			 "F26lNumDoc = '" + cNumDoc + "' And " + ;
			 "F26lCodPro = F01pCodigo		 And " + ;
			 "F26lCodPro = F24tCodPro		 And " + ;
			 "F26lTipDoc = F24tTipDoc		 And " + ;           
			 "F26lNumDoc = F24tNumDoc		 And " + ;
			 "F26lCodPro = F08cCodPro		 And " + ;
			 "F26lCodArt = F08cCodArt		 And " + ;
			 "F26lEstMov<>'4' "

	cGroup = "F26lCodPro,F26lTipDoc,F26lNumDoc,F26lCodArt,F26lNumLot,F26lNumMas,F26lNumMac,F08cDesCri, " + ;
			 "F24tNomAso,F24t1ErDir,F24t2NdDir,F24t3RdDir,F24tDPobla,F24tDProvi, " + ;
			 "F24tCodPos,F01pCodigo,F01pDesCri"

	cOrder = "F26lCodPro,F26lTipDoc,F26lNumDoc,F26lNumMac,F26lCodArt,F26lNumLot"
	cFromF = "F26l,F24t,F01p,F08c"

	=f3_sql(cSel, cFromF, cWhere, cOrder, cGroup, '+XLSALPACKING')

Return

************************************************************************
*> Impresión de Packing List.
*> Segundo paso: Impresión de datos

*> Recibe: XLSALPACKING ---> Cursor con los datos aimprimir.

*> Módulo llamado desde:
*>   - SalPacking ---> Impresión de Packing List.
************************************************************************
Procedure PrtPacking

Private loImp, _Label1, _Docs, cLocalDoc, oDoc

	If Used('XLSALPACKING')
		*> Actualizar datos de transportista.
		_Docs = ' '

		Select XLSALPACKING
		Go Top
		Do While !Eof()
			Scatter Name oDoc
			cLocalDoc = oDoc.F26lCodPro + oDoc.F26lTipDoc + oDoc.F26lNumDoc

			If _Docs<>cLocalDoc
				_Docs = cLocalDoc
				m.F24cCodPro = oDoc.F26lCodPro
				m.F24cTipDoc = oDoc.F26lTipDoc
				m.F24cNumDoc = oDoc.F26lNumDoc

				If f3_seek('F24c')
					m.F01tCodigo = F24c.F24cCodTra
					If f3_seek('F01t')
						Select XLSALPACKING
						Replace F01tDescri With F01t.F01tDesCri
					EndIf
				EndIf
			EndIf

			Select XLSALPACKING
			Skip	
		EndDo

		Select XLSALPACKING
		Go Top
		_Label1 = "IMPRESION DE PACKING LIST"

		loImp2 = CreateObject('st3imp2', , [CR=XLSALPACKING, RPT=SaliPalSt1])
		=loImp2.Exec('?', [LBL=&_Label1])
		loImp2.Release
	Else
		_LxErr = "No hay datos para imprimir packing list"
		Do Form St3Inc With .T.
	EndIf

	*> Cerrar cursor generado por SQL.
	Use In (Select("XLSALPACKING"))

Return

************************************************************************
*> Impresión de listas de carga, resumen.
*> Primer paso: Generar datos de la lista actual.

*> Recibe:
*>	- cNumLst, Nº lista actual.

*> Devuelve:
*>	- XLGENLSCR, Cursor con los datos a imprimir.
*>	- Resultado (.T. / .F.)

*> Módulo llamado desde:
*>   - SaExPLst ----> Impresión de listas de carga.
************************************************************************

Procedure GenLsCR

	Parameters cNumLst
	Local oLst

	m.F80cNumLst = cNumLst
	If f3_seek("F80c")
		Select F80c
		Scatter Name oLst

		*> Copiar datos a cursor de trabajo. Si no existe, se crea.
		If !Used('XLGENLSCR')
			=CrtCursor('F80c', 'XLGENLSCR', 'C')
		EndIf

		Select XLGENLSCR
		Append Blank
		Gather Name oLst
	EndIf

Return

************************************************************************
*> Impresión de listas de carga, resumen.
*> Segundo paso: Imprimir los datos generados por GenLsCR

*> Recibe:
*>	- XLGENLSCR, Cursor con los datos a imprimir.

*> Módulo llamado desde:
*>   - SaExPLst ----> Impresión de listas de carga.
************************************************************************

Procedure PrtLsCR

	Private loImp, _Label1, _Report

	If Used('XLGENLSCR')
		Select XLGENLSCR
		Calculate Min(F80cFecCre) To FechaDesde
		Calculate Max(F80cFecCre) To FechaHasta

		Go Top
		_Label1 = "IMPRESION DE LISTAS DE CARGA (RESUMEN)"
		_Report = "SAEXPLSTL1"

		loImp2 = CreateObject('st3imp2', , [CR=XLGENLSCR, RPT=&_Report])
		=loImp2.Exec('?', [LBL=&_Label1])
		loImp2.Release
	EndIf

	Use In (Select("XLGENLSCR"))

Return

************************************************************************
*> Impresión de listas de carga, detalle.
*> Primer paso: Generar datos de la lista actual.

*> Recibe:
*>	- cNumLst, Nº lista actual.

*> Devuelve:
*>	- XLGENLSCD, Cursor con los datos a imprimir.
*>	- Resultado (.T. / .F.)

*> Módulo llamado desde:
*>   - SaExPLst ----> Impresión de listas de carga.
************************************************************************

Procedure GenLsCD

	Parameters cNumLst

	Private cWhere
	Local oLst, oF24c, oF24t, oF01t, lStado

	cWhere = "F80cNumLst='" + cNumLst + "' And F80lNumLst=F80cNumLst"
	If f3_sql("*", "F80c,F80l", cWhere, , , "F80CLLSTD")
		*> Copiar datos a cursor de trabajo. Si no existe, se crea.
		If !Used('XLGENLSCD')
			=CrtMCursor('F80c,F80l', 'XLGENLSCD', 'C')

			*> Agregar campos adicionales asociados al documento.
			=CrtFCursor('XLGENLSCD', [TBL=F24c,FLD=F24cCodTra,FLD=F24cDirAso,FLD=F24cUbiExp])
			=CrtFCursor('XLGENLSCD', [TBL=F24t,FLD=F24tPunOpe,FLD=F24tNomAso])
			=CrtFCursor('XLGENLSCD', [TBL=F01p,FLD=F01pDescri])
		EndIf

		Select F80CLLSTD
		Go Top
		Do While !Eof()
			Scatter Name oLst

			*> Datos del documento de salida.
			m.F24cCodPro = oLst.F80lCodPro
			m.F24cTipDoc = oLst.F80lTipDoc
			m.F24cNumDoc = oLst.F80lNumDoc
			lStado = f3_seek("F24c")
			Select F24c
			If lStado
				Scatter Name oF24c
			Else
				Scatter Name oF24c Blank
			EndIf

			*> Datos del cliente.
			m.F24tCodPro = oLst.F80lCodPro
			m.F24tTipDoc = oLst.F80lTipDoc
			m.F24tNumDoc = oLst.F80lNumDoc
			lStado = f3_seek("F24t")
			Select F24t
			If lStado
				Scatter Name oF24t
			Else
				Scatter Name oF24t Blank
			EndIf

			*> Datos del transportista.
			m.F01tCodigo = oF24c.F24cCodTra
			lStado = f3_seek("F01t")
			Select F01t
			If lStado
				Scatter Name oF01t
			Else
				Scatter Name oF01t Blank
			EndIf
			
			Select XLGENLSCD
			Append Blank
			Gather Name oLst
			Gather Name oF24c
			Gather Name oF24t
			Gather Name oF01t

			Select F80CLLSTD
			Skip
		EndDo
	EndIf

	Use In (Select ("F80CLLSTD"))

Return

************************************************************************
*> Impresión de listas de carga, detalle.
*> Segundo paso: Imprimir los datos generados por GenLsCD

*> Recibe:
*>	- XLGENLSCD, Cursor con los datos a imprimir.

*> Módulo llamado desde:
*>   - SaExPLst ----> Impresión de listas de carga.
************************************************************************

Procedure PrtLsCD

	Private loImp, _Label1, _Report

	If Used('XLGENLSCD')
		Select XLGENLSCD
		Go Top
		_Label1 = "IMPRESION DE LISTAS DE CARGA (DETALLE)"
		_Report = "SAEXPLSTL2"

		loImp2 = CreateObject('st3imp2', , [CR=XLGENLSCD, RPT=&_Report])
		=loImp2.Exec('?', [LBL=&_Label1])
		loImp2.Release
	EndIf

	Use In (Select("XLGENLSCD"))

Return
