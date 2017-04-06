*> Declarar la clase texto para poder añadir eventos a los botones.
*> Recibe: _Almacen ---> Almacén.
*>         _Zona ------> Zona.
*>         _Calle -----> Calle.
*>         _Tipo ------> Caption de la ubicación. (Estado/Control).

Parameters _Almacen, _Zona, _Calle, _Tipo


_Tipo = Iif(Type('_Tipo') # 'C', 'S', _Tipo)
Do Form Pntw\GraphMapa With _Almacen, _Zona, _Calle, _Tipo

Define Class ClsTextBoxMapa As TextBox

   BackColor = 0

   Procedure Click
      ThisForm.Click(This.ToolTipText)
   EndProc

   Procedure DblClick
      ThisForm.DblClick(This.ToolTipText)
   EndProc

   Procedure RightClick
      ThisForm.RightClick(This.ToolTipText)
   EndProc

EndDefine

*> Clase detalle de la calle.
Define Class ClsTextBoxMapaD As TextBox

   almacen = .F.
   zona = .F.
   calle = .F.

   BackStyle = 1
   BackColor = Rgb(64, 0, 0)

EndDefine
