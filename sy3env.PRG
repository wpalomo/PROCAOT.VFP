* Programa de cambio de entorno de base de datos.
*        SY3USR
*************************************************
* Añadir entorno ACCESS. AVC - 03.02.2003
*************************************************

Private ntot, ntott, env_or, env_ds
Private _xidiom, _psql
Local nInx
Declare _ENTORNOARR(6), _VERSIONARR(6)

If Type('_ENTORNOARR')=='U' .Or. Type('_VERSIONARR')=='U'
   Return
EndIf

_xidiom = 0
_psql = .F.

_ENTORNOARR(1) = 'MYSQL'
_ENTORNOARR(2) = 'VFP'
_ENTORNOARR(3) = 'DB2'
_ENTORNOARR(4) = 'ORACLE'
_ENTORNOARR(5) = 'SQLSERVER'
_ENTORNOARR(6) = 'ACCESS'

_VERSIONARR(1) = '2.0'
_VERSIONARR(2) = '6.0'
_VERSIONARR(3) = '4.5'
_VERSIONARR(4) = '8.x'
_VERSIONARR(5) = '7.0'
_VERSIONARR(6) = '200x'

*> Seleccionar el entorno de trabajo por defecto.
ntot = ALen(_ENTORNOARR)
ntot = Iif(ntot==0, 1, ntot)

ntott= ALen(_VERSIONARR)
If ntot # ntott
   Return
EndIf

Dimension env_or(ntot), env_ds(1)
Store '' To env_or, env_ds
ntott = ntot
ntot = 0

For nInx = 1 To ntott
  ntot = ntot + 1
  env_or(ntot) = Left(_ENTORNOARR(nInx)+ Space(12), 12) + Left(_VERSIONARR(nInx) + Space(12), 12)
EndFor

Do st3trasp In St3Rt With 'Cambio de entorno', ;
                          'env_or', ;
                          ntot, ;
                          'env_ds', ;
                          1,,,,,,, ;
                          'Entornos posibles', ;
                          'Entorno por defecto'
Do Case
   Case !Empty(env_ds(1))
    _ENTORNO = AllTrim(Left(env_ds, 12))
    _VERSION = AllTrim(SubStr(env_ds, 13))

   Otherwise
EndCase

Return
