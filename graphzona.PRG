*>
*> Declarar la clase texto para poder añadir eventos a los botones.
*> Recibe: 

Do Form Pntw\GraphZona

Define Class ClsTextBoxZona As TextBox
   almacen = .F.
   zona = .F.
   calle = .F.

   Procedure Click
      ThisForm.Click(This.almacen, This.zona)
   EndProc

   Procedure DblClick
      ThisForm.DblClick(This.almacen, This.zona)
   EndProc

   Procedure RightClick
      ThisForm.RightClick(This.almacen, This.zona)
   EndProc

EndDefine
