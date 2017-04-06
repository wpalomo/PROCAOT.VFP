*>Conexion con FSV y PROCAOT.
*>JLLF 23-12-2004 17:07:00 PM

Public Array _idiom(2)

Public _pantalla, _usrmaxp
Public _lxerr
Public _nerrores
Public _esc,_TECLADO_ESP
Public _xidiom, _xnidiom, _Propietario
Public _xier

Public _BACKGROUND

Public _FASIV, cAlmacenFASIV, _COMVFP, _COMVFPFASIV, _COMUBICDEFAULT
Public _ASqlFASIV, _Alma, _Procaot, _FecMin, _DEBUGSQL

Public _UsrCod
Public _JobDir
Public _juliano
Public _tmp
Public _entorno, _PSql
Public _Procaot	, _Version					&& Propietario con el que se trabaja (En conectausuario)
Public _Password					&& Password del operario
Store '' To _pantalla,_usrmaxp
Store 1 To _xidiom,_xnidiom
Store '' To _lxerr
Store 0 To _nerrores
Store .F. To _esc
Store .Null. To _ASqlFASIV

Store Space(6) To _UsrCod


_Juliano = 2415020

Store 'VB' To _Lenguaje

set century on
set date to british

Set Default To &_PathProcaot

Select 100
Use SYSPRG Order 1 Shared
Select 102
Use SYSFC Order 1 Shared
Select 103
Use SYSVAR Order 1 Shared

set classlib  to procaot  additive
set classlib  to alisfour additive

*set classlib  to customer additive

*> No actualizado a la nueva nomenclatura de clases.
set classlib  to preparacion additive
set classlib  to reserva additive
set classlib  to ubicar additive
set classlib  to actualizar additive
set classlib  to stocks additive
set classlib to reposicion addi
Set Procedure To Ordenar_Ubicacion    Additive

set procedure to wprocora additive
set procedure to wproc001 additive
set procedure to st3rt    additive
set procedure to ora_proc additive
set procedure to ora_cu00 additive
set procedure to ora_ca00 additive

CD FICH
x=createobject('procaot')
x.opentabla('f08c')
x.opentabla('f26c')
x.opentabla('f26l')

x.opentabla('f10c')
x.opentabla('f16c')
x.opentabla('f14c')
x.opentabla('f24c')
x.opentabla('f24l')
x.opentabla('f13c')

CD..

Return .T.
