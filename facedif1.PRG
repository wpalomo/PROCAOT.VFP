*>
*>                          MODULO DE APOYO A EDICION DE FACTURAS.
*>
*> Procedures: CargarCabeceraFactura .......................... Carga cabecera de factura.
*>             CargarDatosPagoFactura ......................... Carga condiciones de pago.
*>             CargarAcumuladosFactura ........................ Carga importes factura.
*>             CargarDetalleFactura ........................... Carga detalle factura.
*>
*>             CalcularAcumuladosFactura ...................... Calcular importes.
*>             EliminarFactura ................................ Borrar factura.
*>             EscogerFactura ................................. Seleccionar factura.
*>
*>             GrabarFactura .................................. Grabar cambios factura.
*>             GrabarCabeceraFactura .......................... Grabar cabecera de factura.
*>             GrabarDatosPagoFactura ......................... Grabar condiciones de pago.
*>             GrabarAcumuladosFactura ........................ Grabar importes factura.
*>             GrabarDetalleFactura ........................... Grabar detalle factura.

*>
*> Cargar los datos de la cabecera de la factura.
*>
*>   Recibe : cNumFac -----> Nº de factura.
*>
*> Devuelve : .T. /.F. (Resultado).
*>            F70C (Datos cabecera).
*>
Function CargarCabeceraFactura
Parameters cNumFac
Private lEstado, cWhere

   cWhere = "F70cNumFac='" + cNumFac + "'"
   lEstado = f3_sql('*', 'F70C', cWhere, , , 'F70C')

Return lEstado

*>
*> Cargar las condiciones de pago de la factura.
*>
*>   Recibe : cNumFac -----> Nº de factura.
*>
*> Devuelve : .T. /.F. (Resultado).
*>            F70P (Condiciones de pago).
*>
Function CargarDatosPagoFactura
Parameters cNumFac
Private lEstado, cWhere

   cWhere = "F70pNumFac='" + cNumFac + "'"
   lEstado = f3_sql('*', 'F70P', cWhere, , , 'F70P')

Return lEstado

*>
*> Cargar los acumulados de la factura.
*>
*>   Recibe : cNumFac -----> Nº de factura.
*>
*> Devuelve : .T. /.F. (Resultado).
*>            F70I (Totales factura).
*>
Function CargarAcumuladosFactura
Parameters cNumFac
Private lEstado, cWhere

   cWhere = "F70iNumFac='" + cNumFac + "'"
   lEstado = f3_sql('*', 'F70I', cWhere, , , 'F70I')

Return lEstado

*>
*> Cargar las líneas de detalle de la factura.
*>
*>   Recibe : cNumFac -----> Nº de factura.
*>
*> Devuelve : .T. /.F. (Resultado).
*>            F70L (Detalle factura).
*>
Function CargarDetalleFactura
Parameters cNumFac
Private lEstado, cWhere

   cWhere = "F70lNumFac='" + cNumFac + "'"
   lEstado = f3_sql('*', 'F70L', cWhere, , , 'F70L')

Return lEstado

*>
*> Eliminar una factura. Borra solo la cabecera. El resto de datos asociados a la factura
*> (Detalle, acumulados, condiciones de pago...) se realiza por trigger en cascada.
*>
*>   Recibe : cFactura -----> Nº factura.
*>
*> Devuelve : .T. /.F. (Resultado).

Function EliminarFactura
Parameters cFactura
Private lEstado, cWhere

   *> Condición de borrado de la factura.
   cWhere = "F70cNumFac='" + cFactura + "'"
   lEstado = f3_DelTun('F70C', , cWhere, 'N')

Return lEstado

*>
*> Calcular los acumulados de la factura.
*>
*>    Recibe: F70L (Detalle Factura)
*>
*> Devuelve : .T. /.F. (Resultado).
*>            F70I (Totales factura).
*>
Function CalcularAcumuladosFactura
Private lEstado

Local Array vTotales(5, 6)
Local nInx, nTipos, lHayTipo, cIndex

   Store 0 To nTipos
   Store .T. To lEstado

   *> Inicializar el array.
   For nInx = 1 To 5
      vTotales(nInx, 1) = 0                           && Base imponible.
      vTotales(nInx, 2) = Space(4)                    && Código de impuesto.
      vTotales(nInx, 3) = 0                           && % de IVA.
      vTotales(nInx, 4) = 0                           && Importe IVA.
      vTotales(nInx, 5) = 0                           && % de equivalencia.
      vTotales(nInx, 6) = 0                           && Importe equivalencia.
   EndFor

   Select F70l
   Go Top
   Do While !Eof()
      Store .F. To lHayTipo

      *> Buscar el tipo de impuesto en el array.
      For nInx = 1 To nTipos
         If vTotales(nInx, 2)==F70l.F70lCodImp
            Store .T. To lHayTipo
            Exit
         EndIf
      EndFor

      *> Añadir un un nuevo tipo al array.
      If !lHayTipo
         nTipos = Iif(nTipos < 5, nTipos + 1, nTipos)
         nInx = nTipos

         vTotales(nInx, 2) = F70l.F70lCodImp
         vTotales(nInx, 3) = F70l.F70lValIva
         vTotales(nInx, 5) = F70l.F70lValEqv
      EndIf

      *> Acumular importes.
      vTotales(nInx, 1) = vTotales(nInx, 1) + F70l.F70lBasImp
      vTotales(nInx, 4) = vTotales(nInx, 4) + F70l.F70lTotIva
      vTotales(nInx, 6) = vTotales(nInx, 6) + F70l.F70lTotEqv

      *>
      Select F70l
      Skip
   EndDo

   *> Pasar los acumulados del array al cursor F70I.
   Select F70I
   Go Top
   
   For nInx = 1 To 4
      cIndex = Str(nInx, 1, 0)

      Replace F70iBasIm&cIndex With vTotales(nInx, 1), ;
              F70iCodIm&cIndex With vTotales(nInx, 2), ;
              F70iValIv&cIndex With vTotales(nInx, 3), ;
              F70iTotIv&cIndex With vTotales(nInx, 4), ;
              F70iValEq&cIndex With vTotales(nInx, 5), ;
              F70iTotEq&cIndex With vTotales(nInx, 6)
   EndFor

   Replace F70iBasImR With vTotales(nInx, 1), ;
           F70iTotIvR With vTotales(nInx, 4), ;
           F70iTotEqR With vTotales(nInx, 6)
Return lEstado

*>
*> Grabar los cambios en la factura actual.
*> El COMMIT/ROLLBACK se realiza en el programa principal.
*>
*>   Recibe : cFactura -----> Nº factura (Opcional. Solo para dar de alta una nueva factura).
*>
*> Devuelve : .T. /.F. (Resultado).

Function GrabarFactura
Parameters cFactura
Private lEstado

   *> Grabar cabecera (F70C).
   lEstado = GrabarCabeceraFactura(cFactura)
   If !lEstado
      Return lEstado
   EndIf

   *> Grabar condiciones de pago (F70P).
   lEstado = GrabarDatosPagoFactura(cFactura)
   If !lEstado
      Return lEstado
   EndIf

   *> Grabar acumulados (F70I).
   lEstado = GrabarAcumuladosFactura(cFactura)
   If !lEstado
      Return lEstado
   EndIf

   *> Grabar detalle factura (F70L).
   lEstado = GrabarDetalleFactura(cFactura)
   If !lEstado
      Return lEstado
   EndIf

Return lEstado

*>
*> Grabar acumulados de la factura.
*>
*>   Recibe : cFactura -----> Nº factura (Opcional. Solo para dar de alta una nueva factura).
*>
*> Devuelve : .T. /.F. (Resultado).

Function GrabarCabeceraFactura
Parameters cFactura
Private lEstado, cWhere

   *> Pasar campos de memoria a cursor.
   Select F70c
   Go Top
   Gather MemVar

   If Type('cFactura')=='C'
      *> Dar de alta la factura.
      Replace F70cNumFac With cFactura
      lEstado = f3_InsTun('F70C', 'F70C', 'N')
   Else
      *> Actualizar la factura.
      cWhere = "F70cNumFac='" + F70c.F70cNumFac + "'"
      lEstado = f3_UpdTun('F70C', , , , , cWhere, 'N')
   EndIf

Return lEstado

*>
*> Grabar condiciones de pago de la factura.
*>
*>   Recibe : cFactura -----> Nº factura (Opcional. Solo para dar de alta una nueva factura).
*>
*> Devuelve : .T. /.F. (Resultado).

Function GrabarDatosPagoFactura
Parameters cFactura
Private lEstado, cWhere

   *> Pasar campos de memoria a cursor.
   Select F70p
   Go Top
   Gather MemVar

   If Type('cFactura')=='C'

      *> Dar de alta la factura.
      Replace F70pNumFac With cFactura
      lEstado = f3_InsTun('F70P', 'F70P', 'N')
   Else

      *> Actualizar la factura.
      cWhere = "F70pNumFac='" + F70p.F70pNumFac + "'"
      lEstado = f3_UpdTun('F70P', , , , , cWhere, 'N')
   EndIf

Return lEstado

*>
*> Grabar acumulados de la factura.
*>
*>   Recibe : cFactura -----> Nº factura (Opcional. Solo para dar de alta una nueva factura).
*>
*> Devuelve : .T. /.F. (Resultado).

Function GrabarAcumuladosFactura
Parameters cFactura
Private lEstado, cWhere

   *> Pasar campos de memoria a cursor.
   Select F70i
   Go Top

   If Type('cFactura')=='C'

      *> Dar de alta la factura.
      Replace F70iNumFac With cFactura
      lEstado = f3_InsTun('F70I', 'F70I', 'N')
   Else
      *> Actualizar la factura.
      cWhere = "F70iNumFac='" + F70i.F70iNumFac + "'"
      lEstado = f3_UpdTun('F70I', , , , , cWhere, 'N')
   EndIf

Return lEstado

*>
*> Grabar detalle de la factura.
*>
*>   Recibe : cFactura -----> Nº factura (Opcional. Solo para dar de alta una nueva factura).
*>
*> Devuelve : .T. /.F. (Resultado).

Function GrabarDetalleFactura
Parameters cFactura
Private lEstado, cWhere

   Select F70l
   If Type('cFactura')=='C'
      Replace All F70lNumFac With cFactura
   EndIf
   Go Top

   *> Borrar el detalle de la factura existente.
   cWhere = "F70lNumFac='" + F70L.F70lNumFac + "'"
   =f3_DelTun('F70L', , cWhere, 'N')

   *> Dar de alta las líneas de detalle.
   Select F70L
   Locate For !Deleted()
   Do While Found()
      lEstado = f3_InsTun('F70L', 'F70L', 'N')
      If !lEstado
         Return lEstado
      EndIf

      *>
      Select F70L
      Continue
   EndDo

Return lEstado

*>
*> Seleccionar nº de factura.
*>
*> Devuelve: cFactura ----> Nº de factura.

Function EscogerFactura

   Private cFactura, nTotal

   *> Valor por defecto que devolverá la función.
   Store Space(10) To cFactura

   Select F70c
   Go Top

   *> No hay ninguna factura.
   If Eof()
      Return cFactura
   EndIf

   nTotal = RecCount()                  && Total facturas candidatas.

   *> Solo hay una factura: Devolver directamente.
   If nTotal==1
      cFactura = F70cNumFac
      Return cFactura
   EndIf

   Dimension cFacOri(nTotal)            && Facturas candidatas.
   Dimension cFacDes(1)                 && Factura seleccionada.

   n = 0
   cFacDes(1) = Space(10)

   Do While !Eof()
      n = n + 1
      cFacOri(n) = F70cNumFac
      Skip
   EndDo

   *> Selección de factura
   =st3trasp('Selección de factura', ;
            'cFacOri', ;
            nTotal, ;
            'cFacDes', ;
            1,,,,,,, ;
            'Facturas posibles', ;
            'Factura por defecto')

   cFactura = cFacDes(1)

Return cFactura
