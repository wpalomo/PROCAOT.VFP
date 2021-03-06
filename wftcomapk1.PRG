*>
*> Listado de Art�culos / Picking.
*>
*> Recibe:
*>   _forma    -----> M�dulo que realiza la llamada.
*>                    'INICIO', al inicializar el formulario.
*>                    'ABRIR', al cargar el formulario.
*>                    'CERRAR', al descargar el formulario.
*>   _modo     -----> Par�metro no utilizado.
*>   _pantalla -----> Nombre del programa que realiza la llamada.
*>                    No utilizado en mantenimiento de art�culos.

Parameters _forma, _modo, _pantalla
Private cSelec, cFrom, cWhere, cOrder, cGroup

Do Case
   *> Generar el cursor con los datos para el listado.
   Case _forma = 'INICIO'
      cSelec = "*"

      cFrom = "F08c,F12c"

      cWhere = "F08cCodPro Between '" + Ini_Pro + "' And '" + Fin_Pro + "' And " + ;
               "F08cCodArt Between '" + Ini_Art + "' And '" + Fin_Art + "' And " + ;
               "F12cCodPro = F08cCodPro And F12cCodArt = F08cCodArt"

      cOrder = ""
      cGroup = ""
      =f3_sql(cSelec, cFrom, cWhere, cOrder, cGroup, 'XLTEMP')

      *> Eliminar los art�culos sin ubicaciones de picking, seg�n par�metros.
      Select XLTEMP
      If Lst_ord = 2
         Delete For Empty(F12cCodUbi)
      EndIf

      Go Top
      _xier = 0

   *> Llamada al final del listado.
   Case _forma = 'FINAL'
EndCase

*>
Return
