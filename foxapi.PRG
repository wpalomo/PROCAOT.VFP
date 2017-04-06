*>
*> Programa: FOXAPI.PRG
*> Descripción  .............. Funciones de acceso a ficheros INI utilizando la API.

*> Módulos ................... GetKeyIni (Consultar una clave de un fichero INI)

*>-----------------------------------------------------------------------------------
*> Definición de constantes de los procesos.
*>-----------------------------------------------------------------------------------
Function InitIni

   *> Leer el valor de una clave.
   Declare Integer GetPrivateProfileString ;
           In Win32Api ;
           String cSeccion, ;
           String cClave, ;
           String cDefault, ;
           String @Valor, ;
           Integer nTama, ;
           String cFichero

   *> Leer el valor de una sección.
   Declare Integer GetPrivateProfileSection ;
           In Win32Api ;
           String cSeccion, ;
           String @Valor, ;
           Integer nTama, ;
           String cFichero

Return .T.

*>-----------------------------------------------------------------------------------
*> Obtener el valor de una clave en un fichero INI.
*>   Recibe: _cKey -------> Clave a consultar.
*>           _cSeccion ---> Sección del fichero.
*>           _cFile ------> Nombre del fichero INI.
*>
*> Devuelve: _cValor -----> Valor de la clave. NULL si no existe.
*>           _cRetorno ---> Código de status.
*>-----------------------------------------------------------------------------------
Procedure GetKeyIni
Parameters _cKey, _cSeccion, _cFile
Local _cValor
Local _nRetorno
Local _nTama, _cDefault

_nTama = 255                          && Tamaño valor a devolver
_cDefault = ""                        && Valor por defecto

=InitIni()

*> Rellenar la variable a enviar a la API.
_cValor = Replicate(Chr(0), _nTama)
_nRetorno = GetPrivateProfileString ( ;
               _cSeccion, ;
               _cKey, ;
               _cDefault, ;
               @_cValor, ;
               _nTama, ;
               _cFile)

Return _cValor
