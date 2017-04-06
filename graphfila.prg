*> Declarar la clase texto para poder añadir eventos a los botones.
*> Recibe: _Almacen ---> Almacén.
*>         _Zona ------> Zona.
Parameters _Almacen, _Zona

Do Form Pntw\GraphFila With _Almacen, _Zona

*> Clase cabecera de la calle.
Define Class ClsTextBoxFila As TextBox

   almacen = .F.
   zona = .F.
   calle = .F.

   Procedure Click
      ThisForm.Click(This.almacen, This.zona, This.calle)
   EndProc

   Procedure DblClick
      ThisForm.DblClick(This.almacen, This.zona, This.calle)
   EndProc

   Procedure RightClick
      ThisForm.RightClick(This.almacen, This.zona, This.calle)
   EndProc

EndDefine

*> Clase detalle de la calle.
Define Class ClsTextBoxFilaD As TextBox

   almacen = .F.
   zona = .F.
   calle = .F.

   BackStyle = 1
   BackColor = Rgb(64, 0, 0)

EndDefine
