***> Programa de cambio de almacén

Set Point To "."
Set Date To British
Set Delete On
Set Status Bar On
Set Century On

_Connection = 'C:\CONNDB.INI'
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

DirDos = "D:\Almacen\Dbf\"
*DirDos = "F:\Caoticos\Prodis\T2\Dbf\"
*DirDos = "F:\Caoticos\Prodis\Cana26\Ca08\Dbf\"

FichDos= "F24c"
Select 0
Use (DirDos + FichDos) Alias F24c Order 1

FichDos= "F24l"
Select 0
Use (DirDos + FichDos) Alias F24l Order 1

FichDos= "F24d"
Select 0
Use (DirDos + FichDos) Alias F24d Order 1

Do ImportDocs

Use In F24c
Use In F24l
Use In F24d
Return



Procedure ImportDocs
Select F24c
Go Top
Do While !EOF()
   If F24cEstado <> '0'
      Skip
      Loop
   EndIf

set step on

   c_Clave = F24cCodAmo + F24cTipDoc + F24cNumDoc

   strSql = "Select * From F24c001 Where F24cCodPro='" + F24cCodAmo + "' And " + ;
        "F24cTipDoc = '" + F24cTipDoc + "' And F24cNumDoc = '" + F24cNumDoc + "'"
   _Err = SqlExec(_aSql, strSql)
   = SqlMoreResults(_aSql)
   Select SqlResult
   Go Top
   If !EOF()
      Select F24c
      Skip
      Loop
   EndIf
   
   *> Importar cabecera.----------------------------------------------
   Select F24c
   Wait Window "Importando documento " + F24cCodAmo + " - " + F24cNumDoc NoWait

   *> Añadir registro en DB Oracle.-----------------------------------
   strSql = "Insert Into F24c001 Values('"+ F24cCodAmo +"', '"+ F24cTipDoc +"', '"+ ;
       PadR(F24cNumDoc,13) +"', To_Date('"+ DToC(F24cFecPed) + "','dd-mm-yyyy'), '" + ;
       Left(F24cCodCli,13) + "', LPad(' ',15), To_Date('" + DToC(F24cFecEnt) + "','dd-mm-yyyy'), '" + ;
       F24cRutTra +"', 1, LPad(' ',4), LPad(' ',6), LPad(' ',4), LPad(' ',6), LPad(' ',6), " + ;
       "LPad(' ',2), 'T2    ', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, " + ;
       "To_Date('"+ DToC(Date()) + "','dd-mm-yyyy'), 0, '0001', '0001', '0', 0, LPad(' ',13), "+;
       "To_Date('01-01-1992','dd-mm-yyyy'), 'N', 'N', 'N', 1, 'N', 'N', LPad(' ',5), 0, " + ;
       "LPad(' ',4), LPad(' ',15), LPad(' ',4), LPad(' ',15), ' ', 'N', 'P', LPad(' ',14), " + ;
       "'0', ' ', ' ')"

   _Err = SqlExec(_aSql, strSql)
*   = SqlMoreResults(_aSql)
   If _Err < 0
set step on

      = AError(OraErr)
      ? OraErr(2)
      Select F24c
      Skip
      Loop
   EndIf


   *> Dar de alta direccion.------------------------------------------
   Select F24d
   Seek c_Clave

   If !EOF()
      strSql = "Insert Into F24t001 Values('"+ F24dCodAmo +"', '"+ F24dTipDoc +"', '"+ ;
          PadR(F24dNumDoc,13) +"', LPad(' ',13), 'M', '" + PadR(F24dNombre, 50) + "', '" + ;
          PadR(F24dDirecc, 40) + "', LPad(' ',40), LPad(' ',40), '" + F24dPoblac + "', " + ;
          "LPad('Provincia',35), '" + Right(F24dProvin,2) + F24dCodPos + "', '" + ;
          F24dProvin + "', '   ', '" + PadR(F24dNumTel,15) + "', LPad(' ',15), '" + ;
          PadR(F24dNumNif,15) + "', '" + F24c.F24cFlete + "', '" + F24c.F24cExtrar + "')"

      _Err = SqlExec(_aSql, strSql)
      If _Err < 0
set step on
         = AError(OraErr)
         Select F24c
         Skip
         Loop
      EndIf
   EndIf

   *> Dar de alta las lineas.-----------------------------------------
   Select F24l
   Seek c_Clave

set step on

   Do While !EOF()  .And.  F24lCodPro + F24lTipDoc + F24lNumDoc == c_Clave
      strSql = "Insert Into F24l001 Values('"+ F24lCodAmo +"', '"+ F24lTipDoc +"', '"+ ;
          PadR(F24lNumDoc,13) +"', " + PadL(F24lLinDoc,4,'0') + "', " + ;
          "To_Date('" + DToC(F24c.F24cFecPed) + "','dd-mm-yyyy'), " + ;
          "To_Date('" + DToC(F24c.F24cFecEnt) + "','dd-mm-yyyy'), '" + F24c.F24cRutTra + "', "+;
          "LPad(' ',4), LPad(' ',13), " + PadR(F24lCodArt, 13) + ", " + ;
          PadR(F24lNumLot, 15) + ", '" + F24lSitStk + "', " + ;
          "To_Date('" + DToC(F24lFecCad) + "', 'dd-mm-yyyy'), " + Str(F24lCanPed) + ", " + ;
          Str(F24lCanSer) + ", " + Str(F24lCanRes) + ", '0001', " + F24lPreUni + ", '0')"

      _Err = SqlExec(_aSql, strSql)
      If _Err < 0
set step on
         = AError(OraErr)
         Select F24c
         Skip
         Loop
      EndIf

      Select F24l
      Skip
   EndDo

   Select F24c
   Skip
EndDo
* = SqlCommit(_aSql)
Return
