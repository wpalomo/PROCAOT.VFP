***> Programa de cambio de almacén

Set Point To "."
Set Date To British
Set Delete On
Set Status Bar On
Set Century On

_Connection = 'C:\CONNARCO.INI'
Do Case
   Case File(_Connection)
      _Area  = FOpen(_Connection,0)
   Otherwise
      _lxErr = IIf(_xier<>0, _lxerr + 'No existe fichero de conexión [ *.INI ]' + cr, _lxerr)
EndCase

Do While _Area > 0  .And.  !FEOF(_Area)
   lx = FGet(_Area)
   lx = AllTrim(lx)
   If .Not. Empty(lx) .And. Left(lx, 1) <> '[' .And. Left(lx,1) <> ';'
      &lx
   EndIf
EndDo
=FClose(_Area)

Open Database &_Database               && OPEN DATABASE CONEXION
_aSql = SqlConnect(_Connection)        && _ASQL=SQLCONNECT(TUNDEN')

Close Database
On Error _xier=0
For nd=1 To _aSql-1
   =SqlDisconnect(nd)
EndFor


*> Borrar registros.--------------------------------------------------
FichDos = "F:\Win95\Arco\Temp\F00a001"
Do BorrarF00a
Do ImportF00a
FichDos = "F:\Win95\Arco\Temp\F00b001"
Do BorrarF00b
Do ImportF00b
FichDos = "F:\Win95\Arco\Temp\F00c001"
Do BorrarF00c
Do ImportF00c
FichDos = "F:\Win95\Arco\Temp\F00d001"
Do BorrarF00d
Do ImportF00d
FichDos = "F:\Win95\Arco\Temp\F00e001"
Do BorrarF00e
Do ImportF00e
FichDos = "F:\Win95\Arco\Temp\F00f001"
Do BorrarF00f
Do ImportF00f
FichDos = "F:\Win95\Arco\Temp\F00g001"
Do BorrarF00g
Do ImportF00g
FichDos = "F:\Win95\Arco\Temp\F00h001"
Do BorrarF00h
Do ImportF00h
Return


Procedure BorrarF00a
Wait Window "Borrando F00a" NoWait
= SqlExec(_aSql, "Delete From F00a001")
= SqlCommit(_aSql)
Return



Procedure ImportF00a
Private Sw
Wait Window "Importando F00A" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00a001 Values('" + F00aCodNum +"', '"+ F00aDescri +"', '"+ ;
       F00aAbrevi +"', " + Str(F00aNumero)+ ")"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  


Procedure BorrarF00b
Wait Window "Borrando F00b" NoWait
= SqlExec(_aSql, "Delete From F00b001")
= SqlCommit(_aSql)
Return



Procedure ImportF00b
Private Sw
Wait Window "Importando F00b" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00b001 Values('" + F00bCodMov +"', '"+ F00bDescri +"', '"+ ;
       F00bAbrevi +"', '" + F00bEntSal + "')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  


Procedure BorrarF00c
Wait Window "Borrando F00c" NoWait
= SqlExec(_aSql, "Delete From F00c001")
= SqlCommit(_aSql)
Return



Procedure ImportF00c
Private Sw
Wait Window "Importando F00c" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00c001 Values('" + F00cCodStk +"', '"+ F00cDescri +"', '"+ ;
       F00cAbrevi +"', '" + F00cEntSal + "')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  


Procedure BorrarF00d
Wait Window "Borrando F00d" NoWait
= SqlExec(_aSql, "Delete From F00d001")
= SqlCommit(_aSql)
Return



Procedure ImportF00d
Private Sw
Wait Window "Importando F00d" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00d001 Values('" + F00dCodMag +"', '"+ F00dDescri +"', '"+ ;
             F00dAbrevi +"')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  


Procedure BorrarF00e
Wait Window "Borrando F00e" NoWait
= SqlExec(_aSql, "Delete From F00e001")
= SqlCommit(_aSql)
Return



Procedure ImportF00e
Private Sw
Wait Window "Importando F00e" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00e001 Values('" + F00eTipPro +"', '"+ F00eDescri +"', '"+ ;
       F00eAbrevi +"')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  


Procedure BorrarF00f
Wait Window "Borrando F00f" NoWait
= SqlExec(_aSql, "Delete From F00f001")
= SqlCommit(_aSql)
Return



Procedure ImportF00f
Private Sw
Wait Window "Importando F00f" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00f001 Values('" + F00fTamPal +"', '"+ F00fDescri +"', '"+ ;
       F00fAbrevi +"')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  


Procedure BorrarF00g
Wait Window "Borrando F00g" NoWait
= SqlExec(_aSql, "Delete From F00g001")
= SqlCommit(_aSql)
Return



Procedure ImportF00g
Private Sw
Wait Window "Importando F00g" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00g001 Values('" + F00gCodFam +"', '"+ F00gDescri +"', '"+ ;
       F00gAbrevi +"')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  

Procedure BorrarF00h
Wait Window "Borrando F00h" NoWait
= SqlExec(_aSql, "Delete From F00h001")
= SqlCommit(_aSql)
Return



Procedure ImportF00h
Private Sw
Wait Window "Importando F00h" NoWait
Use (FichDos) Alias Std

Go Top
Do While !EOF()
   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F00h001 Values('" + F00hCodUni +"', '"+ F00hDescri +"', '"+ ;
       F00hAbrevi +"')"

   Sw = SqlExec(_aSql, strSql)
   If Sw < 0
      = AError(OraErr)
      ? OraErr(2)
   EndIf

   Select Std
   Skip
EndDo
= SqlCommit(_aSql)
Use In Std
Return  
