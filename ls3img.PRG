* FUNCION CARGAR UNA IMAGEN PARA UN LISTADO
*

Function LS3IMG
Parameters FichImg

If Type('FichImg')<>'C'
   FichImg = ""
EndIf
?fichimg
Return FichImg
