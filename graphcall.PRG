*> Declarar la clase texto para poder a�adir eventos a los botones.
*> Recibe: _Conexion --> Conexi�n con la base de datos.
*>         _Almacen ---> Almac�n.
*>         _Zona ------> Zona.
Parameters _Almacen, _Zona

Do Form Pntw\GraphCall With _Almacen, _Zona

Define Class ClsTextBoxCalle As TextBox

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
