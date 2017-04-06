*>
*> Listado de Artículos / Picking.
*>
*> Recibe:
*>   _forma    -----> Módulo que realiza la llamada.
*>                    'INICIO', al inicializar el formulario.
*>                    'ABRIR', al cargar el formulario.
*>                    'CERRAR', al descargar el formulario.
*>   _modo     -----> Parámetro no utilizado.
*>   _pantalla -----> Nombre del programa que realiza la llamada.
*>                    No utilizado en mantenimiento de artículos.

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

      *> Eliminar los artículos sin ubicaciones de picking, según parámetros.
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
