
*> Programa de conexión con la base de datos.

  Private _ali
  Private cActivePC, cInitFile
  Private cDataBase, cConexion, cEntorno

  Private oProcaot
 
  _ali = Alias()
  oProcaot = CreateObject("FOXAPI")
  
  *> Buscar la conexión con la base de datos.
  cInitFile = AllTrim(Sys(5)) + CurDir() + 'CONNDB.INI'
  cActivePC = oProcaot.GetPC()

*!*	 If cActivePC='PCJUANBOC'
*!*		set step on
*!*	 EndIf

  If !File(cInitFile)
    _LxErr = "No existe fichero de conexión INI [ " +  cInitFile + " ]" + cr
    Do Form St3Inc With .T.
    Release oProcaot
    Return .Null.
  EndIf

  _LxErr = ''

  cDataBase = oProcaot.GetKeyIni("DataBase", cActivePC, cInitFile)
  cConexion = oProcaot.GetKeyIni("Conexion", cActivePC, cInitFile)
  _Entorno  = oProcaot.GetKeyIni("_Entorno", cActivePC, cInitFile)
  _Version  = oProcaot.GetKeyIni("_Version", cActivePC, cInitFile)

  If Empty(cDataBase) .Or. IsNull(cDataBase)
     cDataBase = oProcaot.GetKeyIni("DataBase", '.DEFAULT', cInitFile)
     cConexion = oProcaot.GetKeyIni("Conexion", '.DEFAULT', cInitFile)
     _Entorno  = oProcaot.GetKeyIni("_Entorno", '.DEFAULT', cInitFile)
     _Version  = oProcaot.GetKeyIni("_Version", '.DEFAULT', cInitFile)
  EndIf

  Release oProcaot

  If Empty(cDataBase) .Or. IsNull(cDataBase)
     _LxErr = _LxErr + ;
              'No se ha definido conexión para ' + cActivePC + cr + ;
              'No se ha definido la conexión por defecto [.DEFAULT] ' + cr
  EndIf
  
  *> Abrir conexión con PROCAOT.
  Open DataBase &cDataBase
  _ASql = SqlConnect(cConexion)
  If _ASql <= 0
     _LxErr = _LxErr + ;
              ' MENSAJE: ' + Message() + cr + ;
              'PROGRAMA: ' + Prog() + cr + ;
              'DATABASE: ' + cDataBase + cr + ;
              'CONEXION: ' + cConexion + cr + ;
              '  ESTADO: ' + Str(_ASql)
  EndIf
  Close DataBase

  If !Empty(_LxErr)
    Do Form St3Inc With .T.
    Return .F.
  EndIf

*>
*> Seleccionar el almacén de trabajo.
=SqlExec(_ASql, 'Select * From F02c001 Order By F02cCodAlm', 'Consulta')
=SqlMoreResults(_aSql)
Select Consulta

ntot=reccount()
ntot = iif(ntot==0, 1, ntot)
dimension alm_or(ntot),alm_ds(1)
store '' to alm_or,alm_ds
ntot=0
go top
do while .not. eof()
  ntot=ntot+1
  alm_or(ntot) = f02ccodalm + Space(1) + f02cdescri
  skip
enddo

if ntot=0
  =f3_sn(1,2,'No existe ningún almacén','Creado almacén 01')
  Insert Into F02c001 (F02cCodAlm, F02cDescri) Values('01', 'Almacén 01')
  ntot=1
endif

if ntot=1
  go top
  _alma=f02ccodalm
  _almades=f02cdescri
else
  _ok=.F.
  =st3trasp('Cambio de almacén', ;
            'alm_or', ;
            ntot, ;
            'alm_ds', ;
            1,,,,,,, ;
            'Almacenes posibles', ;
            'Almacén de trabajo')
  do case
  case _ok .and. !empty(alm_ds(1))
    _alma=left(alm_ds,4)
    _almades=subs(alm_ds,6)
  case empty(_alma)
    cancel
  endcase
endif

*>
*> Seleccionar el propietario por defecto.
If !Empty(_Procaot)
=SqlExec(_aSql,'Select * From F01p001 Order By F01pCodigo','Consulta')
=SqlMoreResults(_aSql)
Select Consulta

ntot=reccount()
ntot = iif(ntot==0, 1, ntot)
dimension prp_or(ntot), prp_ds(1)
store '' to prp_or, prp_ds
ntot=0

go top
do while .not. eof()
  ntot=ntot+1
  prp_or(ntot) = F01pCodigo + ' ' + F01pDescri
  skip
enddo

if ntot=0
  =f3_sn(1, 2, 'No existe ningún Propietario', 'Creado propietario')
  ntot=1
else
   Locate For F01pCodigo==_Procaot
   If !Found()
      Go Top
   EndIf

   _Procaot = F01pCodigo
   _ProDesc = F01pDescri
endif

if ntot > 1 .And. Empty(_Procaot)
  _ok=.F.
  =st3trasp('Cambio de propietario', ;
            'prp_or', ;
            ntot, ;
            'prp_ds', ;
            1,,,,,,, ;
            'Propietarios posibles', ;
            'Propietario por defecto')
  do case
  case _ok .and. !empty(prp_ds(1))
    _Procaot = left(prp_ds, 6)
    _ProDesc = Subs(prp_ds, 8)
  case empty(_alma)
*   cancel
  endcase
endif
EndIf
*>
Do Sy3Caption

If !Empty(_ali)
   Select alias(_ali)
EndIf
Return  
