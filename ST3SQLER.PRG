* ------------------------------------------------------------
* Programa de control de errores al efectuar una preguntas SQL 
*                  st3sqler
* ------------------------------------------------------------
_xier=1
do form st3sn with 1,1,'ERROR en la ejecución de la consulta SQL','['+message()+']'
*
return 