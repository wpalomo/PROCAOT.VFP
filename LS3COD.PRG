* Función de devolver el código de una cuenta reducido
*
Function ls3cod
Parameter ctacta
*
lx=left(ctacta,4)+subs(ctacta,9,6)
*
return lx