*>
*> Programa: PROCREPOS.PRG
*> Descripción  .............. Procesos varios de reposiciones de material.

*> Módulos ................... RepCajToUni   (Módulo principal de reposiciones de Cajas
*>                                            a Unidades por MINIMOS)
*>                             RepCajToGrp   (Módulo principal de reposiciones de Cajas
*>                                            a Grupos por MINIMOS)
*>                             RepPicCajToUni(Módulo principal de reposiciones de picos de
*>                                            Cajas a Unidades)
*>                             RepPalToCaj   (Módulo principal de reposiciones de Palets
*>                                            a Cajas por MINIMOS)
*>                             RepCajUni     (Reposiciones a unidades de UN Artículo)
*>                             RepCajGrp     (Reposiciones a grupos de UN Artículo)
*>                             RepPicCajUni  (Módulo principal de reposiciones de picos menores
*>                                            que un grupo de UN Artículo a Unidades)
*>                             RepPalCaj     (Reposiciones a cajas de UN Artículo)
*>                             ActCajToUni   (Actualizar reposiciones de Cajas a Unidades
*>                                            por MINIMOS)
*>                             ActCajToGrp   (Actualizar reposiciones de Cajas a Grupos
*>                                            por MINIMOS)
*>                             ActPicCajToUni(Reposiciones de picos menores de un grupo a
*>                                            unidades de UNA ocupación de UN Artículo)
*>                             ActPalToCaj   (Actualizar reposiciones de Palet a Cajas
*>                                            por MINIMOS)

*>                             CancRepos     (Cancelación de reposiciones)
*>                             ReposAuto     (Reposiciones automáticas)
*>                             ReposAutoMPK  (Reposiciones automáticas de UN MP - Picking)
*>                             ReposAutoMPU  (Reposiciones automáticas de UN MP - Unidades)
*>                             ReposAutoMPG  (Reposiciones automáticas de UN MP - Grupos)

*>                             ERepAnt       (Existen reposiciones anteriores de UN artículo)
*>                             ERepAntUbi    (Existen reposiciones anteriores de UNA ubicación)
*>                             CRepAnt       (Cantidad en reposiciones anteriores de UN artículo)
*>                             EMpsAnt       (Existen MPs de una ocupación)
*>                             COcuUbic      (Cantidad actual en ubicación a reponer)
*>                             GOcupacion    (Leer una ocupación)
*>                             CRepUbicMps   (Cantidad de una ocupación en MPs de reposiciones)
*>                             GetOcuRep     (Ubicaciones candidatas para reponer)
*>                             VerificaMLote (Control productos multilote)

*>                             LlenarAcFnActz(Parámetros función actualización)
*>                             DelMvtF14c    (Borrar un MP)
*>                             ExistF14c     (Comprueba si existe un MP)
*>                             GrabMpF14c    (Generar MPs de reposición)
*>                             BloqMpF14c    (Bloquear MPs que dependen de otros)
*>                             BloqListaF26l (Bloquear Listas que dependen de otras)

*>-----------------------------------------------------------------------------------
*> Generación de reposiciones de CAJAS a UNIDADES (Fracciones).
*> Genera cursor REPPICK y llama a proceso de generación de reposiciones.
*>
*> Llamado desde: SALIREPU ---> Generación de reposiciones a UNIDADES.
*>-----------------------------------------------------------------------------------
Procedure RepCajToUni

Private f_select, f_from, f_where, f_orden, lx_sql
Private Err

=f3_sn(1,1,"Este proceso bloqueará las ubicaciones de picking bajo mínimos,",;
           "y las ocupaciones candidatas a reponer a dichas ubicaciones")

If !f3_sn(2, 1, 'Generar reposiciones de UNIDADES')
   Return
EndIf

*> Cláusula de selección de artículos.
f_select = " Select F08cCodPro, F08cCodArt"
 
f_from   = " From F08c" + _em
 
f_where  = " Where F08cCodPro Between '" + m.CodProI + "' And '" + m.CodProF + _cm + ;
           " And   F08cCodArt Between '" + m.CodArtI + "' And '" + m.CodArtF + _cm

f_orden  = " Order By F08cCodPro, F08cCodArt"

lx_sql = f_select + f_from + f_where + f_orden

Err = f3_SqlExec(_ASql, lx_sql, '_F08cCajToUni')
If Err <= 0
   _LxErr = 'Error cargando artículos para generar reposiciones a unidades' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select _F08cCajToUni
Go Top
Do While !Eof()
   Wait Window "Generando reposición para artículo: " + F08cCodArt NoWait
   Do RepCajUni With F08cCodPro, F08cCodArt

   Select _F08cCajToUni
   Skip
EndDo

If Used('_F08cCajToUni')
   Use In _F08cCajToUni
EndIf

If Used('_F16cOcuRep')
   Use In _F16cOcuRep
EndIf

*>----------------------------------------------------------------
*> Reposición a UNIDADES de un Propietario/Artículo.
*> Opcionalmente recibe una ubicación concreta donde realizar
*> la reposición, p.ej., si se llama desde reservas. (no impl).
*> Recibe: Propietario/Artículo a generar reposiciones.
*>
*> Orden de búsqueda de ocupaciones candidatas: (AVC - 20.06.2000)
*>   - MPs '3070' del producto.
*>   - Ocupaciones de cajas del producto.
*>----------------------------------------------------------------
Procedure RepCajUni
Parameters CodPro, CodArt, UbiUni

Public TipMovS, TipMovE
Private FncA, PrmA
Private _okk
Private f_select, f_from, f_where, f_orden, lx_sql
Private CanRepAct

TipMovS = '3560'                      && Salida origen.
TipMovE = '3060'                      && Entrada destino.

*> Comprobar que existe el tipo de movimiento ORIGEN.
m.F00bCodMov = TipMovS
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento ORIGEN (3560)' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Comprobar que existe el tipo de movimiento DESTINO.
m.F00bCodMov = TipMovE
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento DESTINO (3060)' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Selecciona todas las ocupaciones de picking UNIDADES de este artículo que no lleguen
*> al stock mínimo. Selección: ubicaciones que tengan Picking = 'U' (F10cPickSn), y con
*> stock es esas ubicaciones F16cCanFis), menor que el stock mínimo establecido (F12cCanMin).

f_select = " Select F12cCodPro, F12cCodArt, F12cCodAlm, F12cCodUbi, F12cPriori," + ;
           " F12cCanMin, F12cCanMax, 0 As CanRep"
 
f_from   = " From F12c" + _em + "," + ;
           "      F10c" + _em    

f_where  = " Where F12cCodPro='" + CodPro + "'" + ;
           " And   F12cCodArt='" + CodArt + "'" + ;
           " And   F10cCodUbi=F12cCodUbi" + ;
           " And   F10cPickSN='U'" + ; 
           " And   F10cEstEnt='N'" + ; 
           " And   " + _GCN("F12cCanMin") + ;
           " > (Select " + _GCN("Sum(F16cCanFis-F16cCanRes)") + ;
           " From F16c" + _em + ;
           "                   Where F16cCodUbi=F12cCodUbi" + ;
           "                   And   F16cCodPro=F12cCodPro" + ;
           "                   And   F16cCodArt=F12cCodArt" + ;
           "                   And   F16cSitStk='1000')"

f_orden  = " Order By F12cPriori, F12cCodPro, F12cCodArt, F12cCodUbi"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F12cCajToUni')
If Err <= 0
   _LxErr = 'Error cargando datos para generar reposiciones de unidades' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Crear objetos actualización.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
FncA.ObjParm = PrmA

Select _F12cCajToUni
Go Top

Do While !Eof()
   *> Si ya existe reposición para esta ubicación destino, no hacerla esta vez.
   Scatter MemVar
   If ERepAnt(F12cCodPro, F12cCodArt, F12cCodUbi, TipMovE)
      Select _F12cCajToUni
      Skip
      Loop
   EndIf

   *> Cantidad actual en ocupaciones de este artículo/ubicación.
   CanRepAct = COcuUbic(F12cCodPro, F12cCodArt, F12cCodUbi)
   Select _F12cCajToUni
   If CanRepAct >= F12cCanMin
      Skip
      Loop
   EndIf

   *> Buscar ocupaciones candidatas para reposición.
   *> Toma de picking cajas y palet, por este orden.
   *> Añade registros a cursor _F16cOcuRep, con las ocupaciones encontradas.
   Select _F12cCajToUni
   If !GetOcuRep(F12cCodPro, F12cCodArt, 'S')   && Antes 'SN'.
      Select _F12cCajToUni
      Skip
      Loop
   EndIf

   *> Si el artículo tiene MPs de preparación, NO se genera la reposición por mínimos,
   *> pues debe hacerse la reposición automática, la cual bajará toda la ocupación.
   Select _F16cOcuRep
   Locate For !Empty(F14cUbiOri) .And. SubStr(F14cTipMov, 1, 1) == '2'
   If Found()
      Select _F12cCajToUni
      Skip
      Loop
   EndIf

   *> Proceso que genera las reposiciones de cajas a unidades.
   *> Repone por cantidad máxima.
   Select _F12cCajToUni
   Replace CanRep With F12cCanMax - CanRepAct

   *> Actualizar las reposiciones a unidades del Artículo/Ubicación actual.
   If ActCajToUni()
      =SqlCommit(_ASql)
   Else
      =SqlRollBack(_ASql)
   EndIf

   *>
   Select _F12cCajToUni
   Skip
EndDo

=SqlCommit (_ASql)

Return 

*>------------------------------------------------------------------
*> Actualizar reposiciones de cajas a unidades.
*> Trabaja con los datos del cursor _F16cOcuRep (ocupaciones candidatas),
*> y el cursor _F12cCajToUni(Artículo/ubicación/cantidad actual).
*> Redondear al múltiplo inferior de la caja. AVC - 18.10.2000
*>------------------------------------------------------------------
Function ActCajToUni

Private _ok
Private SOcuCan                         && Sumatorio cantidad ya repuesta.
Private CFalta                          && Cantidad a actualizar cada vez.
Private _CPico                          && Pico de la ocupación candidata.
Private _Selec

Store 0 To SOcuCan, CFalta
Store .F. To _ok

Select _F16cOcuRep
Go Top
If Eof()
  Return
EndIf

*> Leer datos de la ficha del artículo, para control de multilote.
m.F08cCodPro = F12cCodPro
m.F08cCodArt = F12cCodArt
If !f3_seek('F08C')
   _LxErr = 'Error leyendo artículo: ' + F12cCodArt + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select _F16cOcuRep
Do While !Eof() .And. _F12cCajToUni.CanRep > SOcuCan
*** Do While !Eof() .And. SOcuCan <= _F12cCajToUni.F12cCanMin  && .And. _F12cCajToUni.CanRep > SOcuCan

   *> Si el producto NO admite multilote, comprobar ocupación candidata.
   If !VerificaMLote(_F12cCajToUni.F12cCodUbi)
      Select _F16cOcuRep
      Skip
      Loop
   EndIf
      
   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Ver si la cantidad necesaria es mayor de una caja.
   *> Intentar reponer siempre múltiplos de caja completa.

   If (_F12cCajToUni.CanRep - SOcuCan) < _F16cOcuRep.CanDis
      *> Cantidad necesaria menor que disponible: Calcular.
      _FCaj = F08c.F08cUniPac * F08c.F08cPacCaj    &&_F16cOcuRep.F16cUniPac * _F16cOcuRep.F16cPacCaj
      CFalta = _F12cCajToUni.CanRep - SOcuCan

      *> Calcular picos (cajas incompletas).
      _CPico = _F16cOcuRep.CanDis - (Floor(_F16cOcuRep.CanDis / _FCaj) * _FCaj)

      If CFalta <= _CPico
         CFalta = _CPico
      Else
         *> Redocear a múltiplo de caja.
         CFalta = CFalta - _CPico
         CFalta = Iif(CFalta > _FCaj, (Floor(CFalta  / _FCaj) * _FCaj), _FCaj)
         CFalta = Iif(CFalta > _F16cOcuRep.CanDis, _F12cCajToUni.CanRep - SOcuCan, CFalta)
         CFalta = Floor(CFalta / _FCaj) * _FCaj

         *> Enviar siempre el posible pico.
         CFalta = CFalta + _CPico
      EndIf
   Else
      *> Cantidad necesaria mayor que disponible: Mover todo.
      CFalta = _F16cOcuRep.CanDis
   EndIf

   *>
   If CFalta + SOcuCan > _F12cCajToUni.CanRep     && _F12cCajToUni.F12cCanMax
      CFalta = CFalta - _FCaj
   EndIf

   If CFalta <= 0
      Select _F16cOcuRep
      Skip
      Loop
   EndIf

   *> Actualizar la reposición a unidades.
   *>       "     F16cCanRes=F16cCanRes+" + Str(CFalta) 

   Select _F16cOcuRep
   _Selec = "Update F16c" + _em + ;
            " Set F16cFlag1='R'" + ;
            " Where " + GenerarRowId()

*           " Where RowId=CharToRowId('" + _F16cOcuRep.F16cRowId + "')"

   _err = f3_SqlExec(_ASql, _Selec)
   If _err <= 0
      _LxErr = 'Error actualizando cantidad reservada en ocupación' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

   *> Actualizar parámetros para la función de actualización.
   *> Tomará los datos del cursor de ocupaciones activo.
   Select _F16cOcuRep
   Scatter MemVar

   *> Leer la ocupación.
   If !GOcupacion(m.F16cCodPro, m.F16cCodArt, m.F16cCodUbi, m.F16cNumPal, m.F16cFecCad, m.F16cNumLot, m.F16cSitStk)
      _LxErr = _LxErr + 'Error cargando ocupación de cajas a unidades' + cr + ;
                        'MENSAJE: ' + Message() + cr
     Return .F.
   EndIf

   Select F16cCursor
   Scatter MemVar
   Do LlenarAcFnActz

   *> Generar movimientos pendientes.
   *> TipMovE y TipMovS son variables públicas definidas en procedimiento principal.
   FncA.ObjParm.POCFis = CFalta
   FncA.ObjParm.PMORes = 'S'

   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMpF14c('S', TipMovS)
   If _ok
      =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
      =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

      FncA.ObjParm.PUbOld = _F12cCajToUni.F12cCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMpF14c('E', TipMovE)
      If _ok
         SOcuCan = SOcuCan + CFalta
      EndIf
   EndIf

   *>
   Select _F16cOcuRep
   Skip
EndDo

Return _ok

*>-----------------------------------------------------------------------------------
*> Generación de reposiciones de picos de CAJAS a UNIDADES (Fracciones).
*> Genera cursor REPPICK y llama a proceso de generación de reposiciones.
*>
*> Llamado desde:
*>-----------------------------------------------------------------------------------
Procedure RepPicCajToUni

Private f_select, f_from, f_where, f_orden, lx_sql
Private Err

=f3_sn(1,1,"Este proceso bloqueará las ubicaciones de picking bajo mínimos,",;
           "y las ocupaciones candidatas a reponer a dichas ubicaciones")

If !f3_sn(2, 1, 'Generar reposiciones de Picos de cajas a UNIDADES')
   Return
EndIf

*> Cláusula de selección de artículos.
f_select = " Select F08cCodPro, F08cCodArt"
 
f_from   = " From F08c" + _em
 
f_where  = " Where F08cCodPro Between '" + m.CodProI + "' And '" + m.CodProF + _cm + ;
           " And   F08cCodArt Between '" + m.CodArtI + "' And '" + m.CodArtF + _cm

f_orden  = " Order By F08cCodPro, F08cCodArt"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F08cPicCajToUni')
If Err <= 0
   _LxErr = 'Error cargando artículos para generar reposiciones de picos a unidades' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select _F08cPicCajToUni
Go Top
Do While !Eof()
   Wait Window "Generando reposición para artículo: " + F08cCodArt NoWait
   Do RepPicCajUni With F08cCodPro, F08cCodArt

   *>
   Select _F08cPicCajToUni
   Skip
EndDo

If Used('_F08cPicCajToUni')
   Use In _F08cPicCajToUni
EndIf

If Used('_F16cOcuRep')
   Use In _F16cOcuRep
EndIf

*>--------------------------------------------------------------------------------
*> Generación de reposición de picos de cajas a unidades de UN artículo.
*> Genera cursor de trabajo y llama a proceso de generación de reposiciones.
*>
*> Llamado desde:
*>--------------------------------------------------------------------------------
Function RepPicCajUni
Parameters CodPro, CodArt

Private f_select, f_from, f_where, f_orden, lx_sql
Private Err

Wait Window "Generando reposición de picos para artículo: " + CodArt NoWait

*> Cláusula de selección de ocupaciones de cajas con picos.
f_select = " Select *"
 
f_from   = " From F16c" + _em + ", F10c" + _em
 
f_where  = " Where F16cCodPro='" + CodPro + "'" + ;
           " And   F16cCodArt='" + CodArt + "'" + ;
           " And   F10cCodUbi=F16cCodUbi" + ;
           " And   F10cPickSN='S'" + ;
           " And   F10cEstSal='N'"

f_orden  = " Order By F16cCodUbi, F16cFecCad, F16cNumLot"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F16cPicCajToUni')
If Err <= 0
   _LxErr = 'Error cargando artículos para generar reposiciones picos a unidades' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select _F16cPicCajToUni
Go Top
Do While !Eof()
   *> Enviar picos MENORES DE UN GRUPO a unidades.
   Do ActPicCajToUni With F16cCodPro, F16cCodArt, F16cCodUbi, F16cNumPal, F16cFecCad, F16cNumLot, F16cSitStk

   *>
   Select _F16cPicCajToUni
   Skip
EndDo

If Used('_F16cPicCajToUni')
   Use In _F16cPicCajToUni
EndIf

Return

*>-------------------------------------------------------------------------------
*> Pasos:
*>    - Comprobar que existe la ocupación.
*>    - Comprobar que cantidad es menor que el grupo mas pequeño.
*>    - Cargar ubicaciones de picking.
*>    - Tomar la que tenga menor cantidad de producto.
*>    - Generar reposición (3560/3060).
*>-------------------------------------------------------------------------------
Function ActPicCajToUni
Parameters CodPro, CodArt, CodUbi, NumPal, FecCad, NumLot, SitStk
Private _FPal, _FCaj, _FGrp
Private TipMovE, TipMovS

TipMovS = '3560'                      && Salida origen.
TipMovE = '3060'                      && Entrada destino.

*> Comprobar que existe el tipo de movimiento ORIGEN.
m.F00bCodMov = TipMovS
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento ORIGEN (3560)' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Comprobar que existe el tipo de movimiento DESTINO.
m.F00bCodMov = TipMovE
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento DESTINO (3060)' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Comprobar que existe la ocupación de cajas (origen).
*> Devuelve cursor F16cCursor.
If !GOcupacion(CodPro, CodArt, CodUbi, NumPal, FecCad, NumLot, SitStk)
   _LxErr = _LxErr + 'Error cargando ocupación de picos cajas' + cr + ;
                     'MENSAJE: ' + Message() + cr
   Return .F.
EndIf

*> Comprobar si ya hay reposiciones anteriores para esta ocupación origen de cajas.
Scatter MemVar
If ERepAnt(CodPro, CodArt, CodUbi, TipMovS)
   Return .F.
EndIf

*> Si hay reservas pendientes, volver.
If EMpsAnt(CodPro, CodArt, CodUbi, NumPal, FecCad, NumLot, '2%')
   Return .F.
EndIf

*> Seleccionar las ubicaciones de picking UNIDADES del artículo.
If !GetUbiPic(CodPro, CodArt, 'U', '_FRepPicos')
   Return .F.
EndIf

*> Si no hay ubicaciones de unidades, volver.
Select _FRepPicos
Go Top
If Eof()
   Return .F.
EndIf

*> Seleccionar las ubicaciones de GRUPOS del artículo.
If !GetUbiGrp(CodPro, CodArt, '_FRepGrupos')
   Return .F.
EndIf

*> Obtener los datos de la ficha de producto, para factores paletización.
m.F08cCodPro = CodPro
m.F08cCodArt = CodArt
If !f3_seek('F08c')
   Return .F.
EndIf

Select F08c
Go Top
_FCaj = F08cUniPac * F08cPacCaj
_FPal = _FCaj * F08cCajPal

*> Determinar la cantidad a reponer.
_Cantid = F16cCursor.F16cCanFis - ;
          F16cCursor.F16cCanRes - ;
          CRepUbicMps(F16cCursor.F16cCodPro, ;
                      F16cCursor.F16cCodArt, ;
                      F16cCursor.F16cCodUbi, ;
                      F16cCursor.F16cNumPal, ;
                      F16cCursor.F16cFecCad, ;
                      F16cCursor.F16cNumLot)

If _Cantid >= _FPal .Or. (_Cantid >= _FCaj .And. _FCaj > 1)
   Return .F.
EndIf

*> Cantidad mayor que el grupo mas pequeño: Volver.
Select _FRepGrupos
Locate For F08gUniAgr <= _Cantid
If Found()
   Return .F.
EndIf

*> No hay picos: Volver.
If _Cantid <= 0
   Return .F.
EndIf

*> Actualizar la cantidad ocupada y las reposiciones pendientes para cada
*> una de las ubicaciones de picking del artículo de esta ocupación, para
*> reponer sobre la ubicación de picking con menos cantidad.
Select _FRepPicos
Go Top
Do While !Eof()
   Replace CanOcuTot With COcuUbic(_FRepPicos.F12cCodPro, _FRepPicos.F12cCodArt, _FRepPicos.F12cCodUbi) + ;
                          CRepAnt(_FRepPicos.F12cCodPro, _FRepPicos.F12cCodArt, _FRepPicos.F12cCodUbi, TipMovE)
   *>
   Select _FRepPicos
   Skip
EndDo

*> Generar el movimiento de reposición sobre la ubicación de picking de menor cantidad,
*> acumulando la cantidad actual con la de las reposiciones pendientes.
_Minimo = 9999999
_r1 = 0
Select _FRepPicos
Scan
   If CanOcuTot < _Minimo
      _Minimo = CanOcuTot
      _r1 = RecNo()
   EndIf
EndScan

*> Se posiciona en el registro que contiene la ubicación de picking con menor cantidad.
Go _r1

*> Crear objetos actualización.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
FncA.ObjParm = PrmA

*> Inicializar objeto de actualización.
FncA.ObjParm.Inicializar

*> Actualizar parámetros para la función de actualización.
*> Toma los datos del F16cCursor, que contiene la ocupación.
Select F16cCursor
Scatter MemVar
Do LlenarAcFnActz

*> Generar movimientos pendientes.
FncA.ObjParm.POCFis = _Cantid
FncA.ObjParm.PMORes = 'S'

_OldMP = Ora_NewMP()                     && Nº movimiento salida.
_NewMP = Ora_NewMP()                     && Nº movimiento entrada.

FncA.ObjParm.PMNMov = _OldMP
_ok = GrabMpF14c('S', TipMovS)
If _ok
   =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
   =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

   FncA.ObjParm.PUbOld = _FRepPicos.F12cCodUbi
   FncA.ObjParm.PMNMov = _NewMP
   _ok = GrabMpF14c('E', TipMovE)
EndIf

*> Actualizar cambios.
=SqlCommit(_ASql)   

*> Eliminar el objeto de actualización.
Release FncA
Release PrmA

If Used('_FRepPicos')
   Use In _FRepPicos
EndIf

If Used('_FRepGrupos')
   Use In _FRepGrupos
EndIf

Return

*>-----------------------------------------------------------------------------------
*> Generación de reposiciones de CAJAS a GRUPOS.
*> Genera cursor REPPICK y llama a proceso de generación de reposiciones.
*>
*> Llamado desde: SALIREPG ---> Generación de reposiciones a GRUPOS.
*>-----------------------------------------------------------------------------------
Procedure RepCajToGrp

Private f_select, f_from, f_where, f_orden, lx_sql
Private Err

=f3_sn(1,1,"Este proceso bloqueará las ubicaciones de picking bajo mínimos,",;
           "y las ocupaciones candidatas a reponer a dichas ubicaciones")

If !f3_sn(2, 1, 'Generar reposiciones de GRUPOS')
   Return
EndIf

*> Cláusula de selección de artículos.
f_select = " Select F08cCodPro, F08cCodArt"
 
f_from   = " From F08c" + _em
 
f_where  = " Where F08cCodPro Between '" + m.CodProI + "' And '" + m.CodProF + _cm + ;
           " And   F08cCodArt Between '" + m.CodArtI + "' And '" + m.CodArtF + _cm

f_orden  = " Order By F08cCodPro, F08cCodArt"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F08cCajToGrp')
If Err <= 0
   _LxErr = 'Error cargando artículos para generar reposiciones a grupos' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

Select _F08cCajToGrp
Go Top
Do While !Eof()
   Wait Window "Generando reposición para artículo: " + F08cCodArt NoWait

   *> Reponer cajas a grupos, por mínimos.
   Do RepCajGrp With F08cCodPro, F08cCodArt

   *>
   Select _F08cCajToGrp
   Skip
EndDo

If Used('_F08cCajToGrp')
   Use In _F08cCajToGrp
EndIf

If Used('_F16cOcuRep')
   Use In _F16cOcuRep
EndIf

*>------------------------------------------------------------
*> Reposición a GRUPOS de un Propietario/Artículo.
*> Opcionalmente recibe una ubicación concreta donde realizar
*> la reposición, p.ej., si se llama desde reservas. (no impl).
*> Recibe: Propietario/Artículo a generar reposiciones.
*>------------------------------------------------------------
Procedure RepCajGrp
Parameters CodPro, CodArt, UbiGrp

Public TipMovS, TipMovE
Private _okk
Private f_select, f_from, f_where, f_orden, lx_sql
Private CanRepAct

TipMovS = '3580'                      && Salida origen.
TipMovE = '3080'                      && Entrada destino.

*> Comprobar que existe el tipo de movimiento ORIGEN.
m.F00bCodMov = TipMovS
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento ORIGEN (3580)' + cr
   Do Form St3Inc With .T.
   =SqlRollBack(_ASql)
   Return .F.
EndIf

*> Comprobar que existe el tipo de movimiento DESTINO.
m.F00bCodMov = TipMovE
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento DESTINO (3080)' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Selecciona todas las ocupaciones de picking GRUPOS de este artículo que no lleguen
*> al stock mínimo. Selección: ubicaciones que tengan Picking = 'G' (F10cPickSn), y con
*> stock es esas ubicaciones F16cCanFis), menor que el stock mínimo establecido (F12cCanMin).

f_select = " Select F08g" + _em + ".*, 0 CanRep"
 
f_from   = " From F08g" + _em + "," + ;
           "      F10c" + _em    
 
f_where  = " Where F08gCodPro='" + CodPro + "'" + ;
           " And   F08gCodArt='" + CodArt + "'" + ;
           " And   F10cCodUbi=F08gCodUbi" + ;
           " And   F10cPickSN='G'" + ; 
           " And   F10cEstEnt='N'" + ; 
           " And   " + _GCN("F08gCanMin") + ;
           " >=(Select " + _GCN("Sum(F16cCanFis-F16cCanRes") + ;
           " From F16c" + _em + ; 
           "                   Where F16cCodUbi=F08gCodUbi" + ;
           "                   And   F16cCodPro=F08gCodPro" + ;
           "                   And   F16cCodArt=F08gCodArt" + ;
           "                   And   F16cSitStk='1000')"

f_orden  = " Order By F08gLvlAgr, F08gCodPro, F08gCodArt, F08gCodUbi"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F08gCajToGrp')
If Err <= 0
   _LxErr = 'Error cargando datos para generar reposiciones de grupos' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select _F08gCajToGrp
Go Top

Do While !Eof()
   *> Si ya existe reposición para esta ubicación destino, no hacerla esta vez.
   Scatter MemVar
   If ERepAnt(F08gCodPro, F08gCodArt, F08gCodUbi, TipMovE)
      Select _F08gCajToGrp
      Skip
      Loop
   EndIf
  
   *> Cantidad actual en ocupaciones de este artículo/ubicación.
   CanRepAct = COcuUbic(F08gCodPro, F08gCodArt, F08gCodUbi)
   Select _F08gCajToGrp
   If CanRepAct > F08gCanMin
      Skip
      Loop
   EndIf

   *> Buscar ocupaciones candidatas para reposición.
   *> Toma de picking cajas.
   *> Recupera cursor _F16cOcuRep, con las ocupaciones encontradas.
   Select _F08gCajToGrp
   If !GetOcuRep(F08gCodPro, F08gCodArt, 'S')     && Antes 'SN'.
      Select _F08gCajToGrp
      Skip
      Loop
   EndIf

   *> Si el artículo tiene MPs de preparación, NO se genera la reposición por mínimos,
   *> pues debe hacerse la reposición automática, la cual bajará toda la ocupación.
   *>
   *> Comprobar MPs de preparación.
   *> Si el artículo tiene MPs de preparación, NO se genera la reposición por mínimos,
   *> pues debe hacerse la reposición automática, la cual bajará toda la ocupación.
   *>
   *> En reposiciones de cajas a grupos NO HAY REPOSICIONES AUTOMATICAS, por lo que
   *> no se comprueba si hay MPs de preparación sobre las ubicaciones candidatas.
   *>
   *> Comprobar MPs de preparación.

   Select _F16cOcuRep
   Locate For !Empty(F14cUbiOri) .And. SubStr(F14cTipMov, 1, 1) == '2'
   If Found()
      Select _F08gCajToGrp
      Skip
      Loop
   EndIf

   *> Proceso que genera las reposiciones de cajas a grupos.
   *> Repone por cantidad máxima.
   *> Repone por cantidad mínima, por problemas de espacio físico de ubicación. AVC - 12.11.2000
   *> Repone por cantidad máxima, again. AVC - 13.02.2001
   
   Select _F08gCajToGrp
*********************
     Replace CanRep With F08gCanMax - CanRepAct             && Por máximo.
*****Replace CanRep With F08gCanMin - CanRepAct             && Por mínimo.
*********************

   *> Actualizar las reposiciones a grupos del Artículo/Ubicación actual.
   If ActCajToGrp()
      =SqlCommit(_ASql)
   Else
      =SqlRollBack(_ASql)
   EndIf

   *>
   Select _F08gCajToGrp
   Skip
EndDo

=SqlCommit (_ASql)

Return 

*>------------------------------------------------------------------
*> Actualizar reposiciones de cajas a grupos.
*> Trabaja con los datos del cursor _F16cOcuRep (ocupaciones candidatas),
*> y el cursor _F08gCajToGrp(Artículo/ubicación/cantidad actual).
*> Redondear a múltiplo de CAJA, no de GRUPO. AVC - 28.06.2000
*> Redondear a múltiplo de GRUPO. AVC - 19.07.2000
*>------------------------------------------------------------------
Function ActCajToGrp

Private FncA, PrmA
Private _ok
Private SOcuCan                         && Sumatorio cantidad ya repuesta.
Private CFalta                          && Cantidad a actualizar cada vez.
Private _Selec
                                 
Store 0 To SOcuCan, CFalta
Store .T. To _ok

Select _F16cOcuRep
Go Top

Do While !Eof() .And. _F08gCajToGrp.CanRep > SOcuCan

   *> Crear objetos actualización.---------------------------------------
   FncA = CreateObject('OraFncActz')
   PrmA = CreateObject('OraPrmActz')
   FncA.ObjParm = PrmA

   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Calcular la cantidad a reponer.
   CFalta = _F16cOcuRep.CanDis

   *> Ver si la cantidad necesaria es mayor de un grupo.
   *> Intentar reponer siempre múltiplos de grupo.
   *** Múltiplos de CAJA >>>> _FCaj = _F16cOcuRep.F16cUniPac * _F16cOcuRep.F16cPacCaj
   _FCaj = _F08gCajToGrp.F08gUniAgr

   If (_F08gCajToGrp.CanRep - SOcuCan) < _F16cOcuRep.CanDis
      _CNec = _F08gCajToGrp.CanRep - SOcuCan
      CFalta = Ceiling(_CNec  / _FCaj) * _FCaj
      CFalta = IIf(CFalta > _F16cOcuRep.CanDis, _F08gCajToGrp.CanRep - SOcuCan, CFalta)
   EndIf              

   CFalta = Floor(CFalta  / _FCaj) * _FCaj
   If CFalta < _FCaj
      Select _F16cOcuRep
      Skip
      Loop
   EndIf

   *> Actualizar la reposición a grupos.
   *>       "     F16cCanRes=F16cCanRes+" + Str(CFalta)

   _Selec = "Update F16c" + _em + ;
            " Set F16cFlag1='R'" + ;
            " Where " + GenerarRowId()

*           " Where RowId=CharToRowId('" + _F16cOcuRep.F16cRowId + "')"

   _err = f3_SqlExec(_ASql, _Selec)
   If _err <= 0
      _LxErr = 'Error actualizando cantidad reservada en ocupación' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc With .T.
      Return .F.
   EndIf

   *> Actualizar parámetros para la función de actualización.
   *> Tomará los datos del cursor de ocupaciones activo.
   Select _F16cOcuRep
   Scatter MemVar

   *> Leer la ocupación.
   If !GOcupacion(m.F16cCodPro, m.F16cCodArt, m.F16cCodUbi, m.F16cNumPal, m.F16cFecCad, m.F16cNumLot, m.F16cSitStk)
      _LxErr = _LxErr + 'Error cargando ocupación de cajas a grupos' + cr + ;
                        'MENSAJE: ' + Message() + cr
     Return .F.
   EndIf

   Select F16cCursor
   Scatter MemVar
   Do LlenarAcFnActz

   *> Generar Movimientos Pendientes.
   *> TipMovE y TipMovS son variables públicas definidas en procedimiento principal.
   FncA.ObjParm.POCFis = CFalta
   FncA.ObjParm.PMORes = 'S'

   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMpF14c('S', TipMovS)
   If _ok
      =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
      =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

      FncA.ObjParm.PUbOld = _F08gCajToGrp.F08gCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMpF14c('E', TipMovE)
      If _ok
         SOcuCan = SOcuCan + CFalta
      EndIf
   EndIf

   *> Reponer picos menores de un grupo, si hay, de esta ocupación.
   Select _F16cOcuRep
   Do ActPicCajToUni With F16cCodPro, F16cCodArt, F16cCodUbi, F16cNumPal, F16cFecCad, F16cNumLot, F16cSitStk

   *>
   Select _F16cOcuRep
   Skip
EndDo

Return _ok

*>-----------------------------------------------------------------------------------
*> Generación de reposiciones de PALETS a CAJAS.
*> Genera cursor REPPICK y llama a proceso de generación de reposiciones.
*>
*> Llamado desde: SALIREPC ---> Generación de reposiciones a CAJAS.
*>-----------------------------------------------------------------------------------
Procedure RepPalToCaj

Private f_select, f_from, f_where, f_orden, lx_sql
Private Err

=f3_sn(1,1,"Este proceso bloqueará las ubicaciones de picking bajo mínimos,",;
           "y las ocupaciones candidatas a reponer a dichas ubicaciones")

If !f3_sn(2, 1, 'Generar reposiciones de CAJAS')
   Return
EndIf

*> Cláusula de selección de artículos.
f_select = " Select F08cCodPro, F08cCodArt"
 
f_from   = " From F08c" + _em
 
f_where  = " Where F08cCodPro Between '" + m.CodProI + "' And '" + m.CodProF + _cm + ;
           " And   F08cCodArt Between '" + m.CodArtI + "' And '" + m.CodArtF + _cm

f_orden  = " Order By F08cCodPro, F08cCodArt"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F08cPalToCaj')
If Err <= 0
   _LxErr = 'Error cargando artículos para generar reposiciones a cajas' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

Select _F08cPalToCaj
Go Top
Do While !Eof()
   Wait Window "Generando reposición para artículo: " + F08cCodArt NoWait
   Do RepPalCaj With F08cCodPro, F08cCodArt

   Select _F08cPalToCaj
   Skip
EndDo

If Used('_F08cPalToCaj')
   Use In _F08cPalToCaj
EndIf

If Used('_F16cOcuRep')
   Use In _F16cOcuRep
EndIf

*>------------------------------------------------------------
*> Reposición a CAJAS de UN propietario/artículo.
*> Opcionalmente recibe una ubicación concreta donde realizar
*> la reposición, p.ej., si se llama desde reservas. (no impl).
*> Recibe: Propietario/Artículo a generar reposiciones.
*>------------------------------------------------------------
Procedure RepPalCaj
Parameters CodPro, CodArt, UbiCaj

Public TipMovS, TipMovE
Private FncA, PrmA
Private _okk
Private f_select, f_from, f_where, f_orden, lx_sql
Private CanRepAct

TipMovS = '3570'                      && Salida origen.
TipMovE = '3070'                      && Entrada destino.

*> Comprobar que existe el tipo de movimiento ORIGEN.
m.F00bCodMov = TipMovS
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento ORIGEN (3570)' + cr
   Do Form St3Inc
   _LxErr = ''
   Return .F.
EndIf

*> Comprobar que existe el tipo de movimiento DESTINO.
m.F00bCodMov = TipMovE
If !f3_seek('F00B')
   _LxErr = 'No se ha podido encontrar el tipo de movimiento DESTINO (3070)' + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Selecciona todas las ocupaciones de picking CAJAS de este artículo que no lleguen
*> al stock mínimo. Selección: ubicaciones que tengan Picking = 'S' (F10cPickSn), y con
*> stock en esas ubicaciones (F16cCanFis), menor que el stock mínimo establecido (F12cCanMin).
f_select = " Select F12cCodPro, F12cCodArt, F12cCodAlm, F12cCodUbi, F12cPriori," + ;
           " F12cCanMin, F12cCanMax, 0 CanRep"
 
f_from   = " From F12c" + _em + "," + ;
           "      F10c" + _em    
 
f_where  = " Where F12cCodPro='" + CodPro + "'" + ;
           " And   F12cCodArt='" + CodArt + "'" + ;
           " And   F10cCodUbi=F12cCodUbi" + ;
           " And   F10cPickSN='S'" + ; 
           " And   F10cEstEnt='N'" + ; 
           " And   " + _GCN("F12cCanMin") + ;
           " >=(Select " + _GCN("Sum(F16cCanFis-F16cCanRes)") + ;
           " From F16c" + _em + ;
           "                   Where F16cCodUbi=F12cCodUbi" + ;
           "                   And   F16cCodPro=F12cCodPro" + ;
           "                   And   F16cCodArt=F12cCodArt" + ;
           "                   And   F16cSitStk='1000')"

f_orden  = " Order By F12cPriori, F12cCodPro, F12cCodArt, F12cCodUbi"

lx_sql = f_select + f_from + f_where + f_orden 

Err = f3_SqlExec(_ASql, lx_sql, '_F12cPalToCaj')
If Err <= 0
   _LxErr = 'Error cargando datos para generar reposiciones de cajas' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return .F.
EndIf

*> Crear objetos actualización.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
FncA.ObjParm = PrmA

Select _F12cPalToCaj
Go Top

Do While !Eof()
   *> Si ya existe reposición para esta ubicación destino, no hacerla esta vez.
   Scatter MemVar
   If ERepAnt(F12cCodPro, F12cCodArt, F12cCodUbi, TipMovE)
      Select _F12cPalToCaj
      Skip
      Loop
   EndIf
  
   *> Cantidad actual en ocupaciones de este artículo/ubicación.
   CanRepAct = COcuUbic(F12cCodPro, F12cCodArt, F12cCodUbi)
   Select _F12cPalToCaj
   If CanRepAct > F12cCanMin
      Skip
      Loop
   EndIf

   *> Buscar ocupaciones candidatas para reposición.
   *> Recupera cursor _F16cOcuRep, con las ocupaciones encontradas.
   Select _F12cPalToCaj
   If !GetOcuRep(F12cCodPro, F12cCodArt, 'N')
      Select _F12cPalToCaj
      Skip
      Loop
   EndIf

   *> Si el artículo tiene MPs de preparación, NO se genera la reposición por mínimos,
   *> pues debe hacerse la reposición automática, la cual bajará toda la ocupación.
   Select _F16cOcuRep
   Locate For !Empty(F14cUbiOri) .And. SubStr(F14cTipMov, 1, 1) == '2'
   If Found()
      Select _F12cPalToCaj
      Skip
      Loop
   EndIf

   *> Proceso que genera las reposiciones de palets a cajas.
   *> Repone por cantidad máxima.
   *> _FCaj = _F16cOcuRep.F16cUniPac * _F16cOcuRep.F16cPacCaj
   *> CanRepAct = Floor(CanRepAct / _FCaj) * _FCaj
   Select _F12cPalToCaj
   Replace CanRep With F12cCanMax - CanRepAct

   *> Actualizar las reposiciones a cajas del Artículo/Ubicación actual.
   If ActPalToCaj()
      =SqlCommit(_ASql)
   Else
      =SqlRollBack(_ASql)
   EndIf

   *>
   Select _F12cPalToCaj
   Skip
EndDo

=SqlCommit (_ASql)

Return 

*>------------------------------------------------------------------------
*> Actualizar reposiciones de palets a cajas.
*> Trabaja con los datos del cursor _F16cOcuRep (ocupaciones candidatas),
*> y el cursor _F12cCajToUni(Artículo/ubicación/cantidad actual).
*>
*> Evitar partir un palet. En el momento que la cantidad repuesta sea
*> mayor que CanMin, terminar la reposición. AVC -19.07.2000
*>------------------------------------------------------------------------
Function ActPalToCaj

Private _ok
Private SOcuCan                         && Sumatorio cantidad ya repuesta.
Private CFalta                          && Cantidad a actualizar cada vez.
Private _Selec
                                 
Store 0 To SOcuCan, CFalta

Select _F16cOcuRep
Go Top

Do While !Eof() .And. SOcuCan <= _F12cPalToCaj.F12cCanMin  && .And. _F12cPalToCaj.CanRep > SOcuCan
   
   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Calcular la cantidad a reponer.
   CFalta = _F16cOcuRep.CanDis

   *> Ver si la cantidad necesaria es mayor de una caja.
   *> Intentar reponer siempre múltiplos de caja completa.
   If (_F12cPalToCaj.CanRep - SOcuCan) < _F16cOcuRep.CanDis
      _FCaj = _F16cOcuRep.F16cUniPac * _F16cOcuRep.F16cPacCaj
      _CNec = _F12cPalToCaj.CanRep - SOcuCan
      CFalta = Ceiling(_CNec  / _FCaj) * _FCaj
      CFalta = IIf(CFalta > _F16cOcuRep.CanDis, _F12cPalToCaj.CanRep - SOcuCan, CFalta)
   EndIf              

   *> Miro si tengo cantida disponible en la ocupación
   if CFalta == 0
	   Select _F16cOcuRep
	   Skip
	   Loop
   endIf

   *> Actualizar la reposición a cajas.
   *        "     F16cCanRes=F16cCanRes+" + Str(CFalta)

   _Selec = "Update F16c" + _em + ;
            " Set F16cFlag1='R'" + ;
            " Where " + GenerarRowId()

*           " Where RowId=CharToRowId('" + _F16cOcuRep.F16cRowId + "')"

   _err = f3_SqlExec(_ASql, _Selec)
   If _err <= 0
      _LxErr = 'Error actualizando cantidad reservada en ocupación' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc With .T.
      Return .F.
   EndIf

   *> Actualizar parámetros para la función de actualización.
   *> Tomará los datos del cursor de ocupaciones activo.
   Select _F16cOcuRep
   Scatter MemVar

   *> Leer la ocupación.
   If !GOcupacion(m.F16cCodPro, m.F16cCodArt, m.F16cCodUbi, m.F16cNumPal, m.F16cFecCad, m.F16cNumLot, m.F16cSitStk)
      _LxErr = _LxErr + 'Error cargando ocupación de palet a cajas' + cr + ;
                        'MENSAJE: ' + Message() + cr
     Return .F.
   EndIf

   Select F16cCursor
   Scatter MemVar
   Do LlenarAcFnActz

   *> Generar Movimientos Pendientes.
   *> TipMovE y TipMovS son variables públicas definidas en procedimiento principal.
   FncA.ObjParm.POCFis = CFalta
   FncA.ObjParm.PMORes = 'N'

   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMpF14c('S', TipMovS)
   If _ok
      =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
      =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

      FncA.ObjParm.PUbOld = _F12cPalToCaj.F12cCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMpF14c('E', TipMovE)
      If _ok
         SOcuCan = SOcuCan + CFalta
      EndIf
   EndIf

   Select _F16cOcuRep
   Skip
EndDo

Return _ok

*>------------------------------------------------------------------
*> Comprueba si ya existe una reposición a una ubicación destino.
*> Recibe Propietario, Artículo, Ubicación, Tipo movimiento.
*> Tipo movimiento puede contener comodín '%'
*>------------------------------------------------------------------
Function ERepAnt
Parameters CodPro, CodArt, CodUbi, TipMov

Private f_select, f_where, _ok 

f_select = " Select * From F14c" + _em
f_where  = " Where F14cUbiOri='" + CodUbi + _cm + ;
           " And   F14cCodPro='" + CodPro + _cm + ;
           " And   F14cCodArt='" + CodArt + _cm + ;
           Iif(At('%', TipMov)>0, " And   F14cTipMov Like '" + TipMov + "'", ;
           " And   F14cTipMov='" + TipMov + "'")

Err = f3_SqlExec(_ASql, f_select + f_where)
If Err > 0
   Go Top
   Err = Iif(Eof(), 1, -1)
EndIf
   
Return Iif(Err > 0, .F., .T.)

*>------------------------------------------------------------------
*> Comprueba si ya existe una reposición sobre una ubicación.
*> Recibe Ubicación, Tipo movimiento.
*> Tipo movimiento puede contener comodín '%'
*>------------------------------------------------------------------
Function ERepAntUbi
Parameters CodUbi, TipMov

Private f_select, f_where, _ok 

f_select = " Select * From F14c" + _em
f_where  = " Where F14cUbiOri='" + CodUbi + _cm + ;
           Iif(At('%', TipMov)>0, " And   F14cTipMov Like '" + TipMov + "'", ;
           " And   F14cTipMov='" + TipMov + "'")

Err = f3_SqlExec(_ASql, f_select + f_where)
If Err > 0
   Go Top
   Err = Iif(Eof(), 1, -1)
EndIf
   
Return Iif(Err > 0, .F., .T.)

*>-------------------------------------------------------------
*> Calcular la cantidad de reposiciones ya existentes para un
*> artículo/ubicación.
*> Recibe Propietario, Artículo, Ubicación, Tipo movimiento.
*> Tipo movimiento puede contener comodín '%'
*>-------------------------------------------------------------
Function CRepAnt
Parameters CodPro, CodArt, CodUbi, TipMov

Private f_select, f_where, _ok 

f_select = " Select " + _GCN("F14cCanFis") + " As CanRepAnt From F14c" + _em
f_where  = " Where F14cUbiOri='" + CodUbi + _cm + ;
           " And   F14cCodPro='" + CodPro + _cm + ;
           " And   F14cCodArt='" + CodArt + _cm + ;
           Iif(At('%', TipMov)>0, ;
               " And   F14cTipMov Like '" + TipMov + "'", ;
               " And   F14cTipMov='" + TipMov + "'")

Err = f3_SqlExec(_ASql, f_select + f_where, 'F14cRepAnt')
If Err <= 0
   _LxErr = _LxErr + 'Error acumulando cantidad en reposiciones artículo/ubicación' + cr + ;
                     'MENSAJE: ' + Message() + cr
   Return 0
EndIf

Select F14cRepAnt
Go Top
Return F14cRepAnt.CanRepAnt

*>------------------------------------------------------------------
*> Comprueba si ya existen MPs de una ocupación.
*> Recibe Ocupación, Tipo movimiento.
*> Tipo movimiento puede contener comodín '%'
*>------------------------------------------------------------------
Function EMpsAnt
Parameters CodPro, CodArt, CodUbi, NumPal, FecCad, NumLot, TipMov

Private f_select, f_where, _ok 

f_select = " Select * From F14c" + _em
f_where  = " Where F14cUbiOri='" + CodUbi + _cm + ;
           " And   F14cCodPro='" + CodPro + _cm + ;
           " And   F14cCodArt='" + CodArt + _cm + ;
           " And   F14cNumPal='" + NumPal + _cm + ;
           " And   F14cFecCad=" + _GCD(FecCad) + ;
           " And   F14cNumLot='" + NumLot + _cm + ;
           Iif(At('%', TipMov)>0, " And   F14cTipMov Like '" + TipMov + "'", ;
           " And   F14cTipMov='" + TipMov + "'")

Err = f3_SqlExec(_ASql, f_select + f_where)
If Err > 0
   Go Top
   Err = Iif(Eof(), 1, -1)
EndIf
   
Return Iif(Err > 0, .F., .T.)

*>-----------------------------------------------------------
*> Cantidad actual en ubicación a reponer.
*> Recibe Propietario, Artículo, Ubicación.
*> Devuelve cantidad ya existente en la ubicación a reponer.
*>-----------------------------------------------------------
Function COcuUbic
Parameters CodPro, CodArt, CodUbi

Private f_select, _where, _from, _ok

f_select = " Select " + _GCN("Sum(F16cCanFis-F16cCanRes)") + " As CanUbic"
_from =    " From F16c" + _em 

f_where  = " Where F16cCodUbi='" + CodUbi + "'" + ;
           " And   F16cCodPro='" + CodPro + "'" + ;
           " And   F16cCodArt='" + CodArt + "'" + ;
           " And   F16cSitStk='1000'"

Err = f3_SqlExec(_ASql, f_select + _from + f_where)
If Err <= 0
   _LxErr = _LxErr + 'Error cargando stock artículo en ocupaciones' + cr + ;
                     'MENSAJE: ' + Message() + cr
   Return 0
EndIf

Return CanUbic

*>-----------------------------------------------------------
*> Leer una ocupación.
*>-----------------------------------------------------------
Function GOcupacion
Parameters CodPro, CodArt, CodUbi, NumPal, FecCad, NumLot, SitStk

Private f_select, _where, _from

f_select = " Select *"
_from =    " From F16c" + _em 

f_where  = " Where F16cCodUbi='" + CodUbi + "'" + ;
           " And   F16cCodPro='" + CodPro + "'" + ;
           " And   F16cCodArt='" + CodArt + "'" + ;
           " And   F16cNumPal='" + NumPal + "'" + ;
           " And   F16cFecCad=" + _GCD(FecCad) + ;
           " And   F16cNumLot='" + NumLot + "'" + ;
           " And   F16cSitStk='" + SitStk+ "'"

Err = f3_SqlExec(_ASql, f_select + _from + f_where, 'F16cCursor')
If Err <= 0
   _LxErr = _LxErr + "Error buscando ocupación " + cr + ;
            "MENSAJE: " + Message() + cr
   Return .F.
EndIf

Select F16cCursor
Go Top
Return Iif(Eof(), .F., .T.)

*>-----------------------------------------------------------
*> Obtener cantidad ya reservada en reposiciones anteriores.
*> Recibe Propietario, Artículo, Ubicación, Palet, Cad, Lote.
*> Devuelve cantidad acumulada en MPs de reposición.
*>-----------------------------------------------------------
Function CRepUbicMps
Parameters CodPro, CodArt, CodUbi, NumPal, FecCad, NumLot

Private f_select, _where, _from, _ok

f_select = " Select " + _GCN("Sum(F14cCanFis)") + " As CanRepos"
_from =    " From F14c" + _em 

f_where  = " Where F14cUbiOri='" + CodUbi + _cm + ;
           " And   F14cCodPro='" + CodPro + _cm + ;
           " And   F14cCodArt='" + CodArt + _cm + ;
           " And   F14cNumPal='" + NumPal + _cm + ;
           " And   F14cNumLot='" + NumLot + _cm + ;
           " And   F14cTipMov Like '35%'"

Err = f3_SqlExec(_ASql, f_select + _from + f_where)
If Err <= 0
   _LxErr = 'Error cargando reposiciones anteriores en ubicación origen' + cr + ;
            'MENSAJE: ' + Message() + cr
   Do Form St3Inc With .T.
   Return 0
EndIf

Return CanRepos

*>------------------------------------------------------------
*> Cargar ocupaciones candidatas para reponer.
*> Estarán ordenadas por Lote, caducidad y fecha entrada.
*> Recibe: Propietario, Artículo y tipo ubicación.
*> Genera: Cursor _F16cOcuRep con ocupaciones candidatas.
*>
*> Quitar referencia a F14c. De momento se mantienen los
*> campos para futuras ampliaciones.
*>------------------------------------------------------------
Function GetOcuRep
Parameters CodPro, CodArt, TipUbi

Private f_select, f_from, f_where, f_orden, lx_sql, _ok
Private _Inx, _TipUbi, _Cursor
Private _F16cRowId, _RecNo

For _Inx = 1 To Len(TipUbi)
   _TipUbi = SubStr(TipUbi, _Inx, 1)
   _Cursor = Iif(_Inx==1, '_F16cOcuRep', '_OcuRepCur')

   f_select = "Select F16cCodPro, F16cCodArt, F16cNumLot, F16cFecCad, F16cFecEnt, " + ; 
              "F16cCodUbi, F16cSitStk, F16cNumPal, " + ;
              _GCN("F16cCanFis") + " As CanDis, " + ;
              _GCN("F16cUniPac") + " As F16cUniPac, " + ;
              _GCN("F16cPacCaj") + " As F16cPacCaj, " + ;
              "F16cCanFis-F16cCanRes As nDisponible, " + ;
              "F16cUniPac*F16cPacCaj As nFactorCaja, " + ;
              "0 As Sueltas, " + ;
              "F16cFlag1, F16cTamPal, " + ;
              "'" + Space(ObtenerTamanyRowId()) + "' As F16cRowId, " + ;
              "'" + Space(14) + "' As F14cUbiOri, " + ;
              "'" + Space(4) + "' As F14cTipMov"

   f_from   = " From F16c" + _em + ", F10c" + _em

   f_where  = " Where F16cCodPro='" + CodPro + _cm + ;
              " And   F16cCodArt='" + CodArt + _cm + ;
              " And   " + _GCSS("F16cSitStk", 1, 1) + "='1'" + ;
              " And   F10cCodUbi=F16cCodUbi" + ;
              " And   F10cPickSN='" + _TipUbi + _cm + ; 
              " And   F16cCanFis > 0"

   f_orden  = " Order by F16cCodPro, F16cCodArt," + ;
              "          F16cFecCad, F16cNumLot, F16cFecEnt," + ;
              "          F14cUbiOri Desc," + ;
              "          nDisponible"

   lx_sql = f_select + f_from + f_where + f_orden 

   Err = f3_SqlExec(_ASql, lx_sql, _Cursor)
   If Err <= 0
      _LxErr = 'Error cargando ocupaciones candidatas a reposición' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc With .T.
      Return .F.
   EndIf

   If _Inx > 1
      Select _F16cOcuRep
      Append From Dbf(_Cursor)
   EndIf
EndFor

*> Eliminar registro duplicados del cursor de ocupaciones candidatas.
Select _F16cOcuRep
Replace All Sueltas With Mod(nDisponible, nFactorCaja)

Go Top
Do While !Eof()
   _RecNo = RecNo()
   _F16cRowId = ConcatenarRowId()       && F16cRowId
   Delete For CtrlConcatenarRowId()==_F16cRowId .And. RecNo() # _RecNo

*  Delete For F16cRowId = _F16cRowId .And. RecNo() # _RecNo

   Go _RecNo
   Skip
EndDo

*> Restar a cantidad disponible la cantidad ya reservada de otras reposiciones.
Go Top
Do While !Eof()
   Replace CanDis With CanDis - CRepUbicMps(F16cCodPro, ;
                                            F16cCodArt, ;
                                            F16cCodUbi, ;
                                            F16cNumPal, ;
                                            F16cFecCad, ;
                                            F16cNumLot)
   Select _F16cOcuRep
   Skip
EndDo

*> Dejar solo los registros con cantidad disponible.
Select _F16cOcuRep
Delete For CanDis <= 0

Go Top
Return Iif(Eof(), .F., .T.)

*>--------------------------------------------------------
*> Proceso de cancelación de una reposición.
*> Recibe : NMov  (Nº movimiento origen)
*>          NMovR (Nº movimiento destino)
*>--------------------------------------------------------
Function CancRepos
Parameters NMov, NMovR

Private Nro, _okk
Private _CodPro, _CodArt, _CodUbi, _NumPal, _NumLot, _SitStk
Private _CanRepos

     *> Comprobar si existe F14c (origen)
     _okk=ExistF14c(NMov)
     IF !_okk
        _LxErr=_LxErr + "No se encontró movimiento ORIGEN [" + NMov + "] en F14c." + Cr
        return .F.
     EndIf  

     *> Guardar datos para actualizar los MPs y listas afectados por la reposición.
     Scatter MemVar
     f_where14 = "F14cCodPro='" + m.F14cCodPro + "' And " + ;
                 "F14cCodArt='" + m.F14cCodArt + "' And " + ;
                 "F14cUbiOri='" + m.F14cUbiOri + "' And " + ;
                 "F14cNumPal='" + m.F14cNumPal + "' And " + ;
                 "F14cFecCad=" + _GCD(m.F14cFecCad) + " And " + ;
                 "F14cNumLot='" + m.F14cNumLot + "' And " + ;
                 "F14cSitStk='" + m.F14cSitStk + "' And " + ;
                 "F14cTipMov Between '2000' And '2998'"

     f_where26 = "F26lCodPro='" + m.F14cCodPro + "' And " + ;
                 "F26lCodArt='" + m.F14cCodArt + "' And " + ;
                 "F26lUbiOri='" + m.F14cUbiOri + "' And " + ;
                 "F26lNumPal='" + m.F14cNumPal + "' And " + ;
                 "F26lFecCad=" + _GCD(m.F14cFecCad) + " And " + ;
                 "F26lNumLot='" + m.F14cNumLot + "' And " + ;
                 "F26lSitStk='" + m.F14cSitStk + "' And " + ;
                 "F26lTipMov Between '2000' And '2998'"

     f_where16 = "F16cCodPro='" + m.F14cCodPro + "' And " + ;
                 "F16cCodArt='" + m.F14cCodArt + "' And " + ;
                 "F16cCodUbi='" + m.F14cUbiOri + "' And " + ;
                 "F16cNumPal='" + m.F14cNumPal + "' And " + ;
                 "F16cFecCad=" + _GCD(m.F14cFecCad) + " And " + ;
                 "F16cNumLot='" + m.F14cNumLot + "' And " + ;
                 "F16cSitStk='" + m.F14cSitStk + "'"

     *> Guardar la ubicación origen para calcular los MPs de preparación.
     _CodPro = m.F14cCodPro
     _CodArt = m.F14cCodArt
     _CodUbi = m.F14cUbiOri
     _NumPal = m.F14cNumPal
     _FecCad = m.F14cFecCad
     _NumLot = m.F14cNumLot
     _SitStk = m.F14cSitStk

     *> Comprobar si existe F14c (destino)
     _okk=ExistF14c(NMovR)
     If !_okk
        _LxErr=_LxErr + "No se encontró movimiento DESTINO [" + NMovR + "] en F14c." + Cr
        Return .F.
     EndIf  

     *>  Borrar MP origen.
     _okk = DelMvtF14c(NMov)
     If !_okk
        _LxErr=_LxErr + "No se ha podido borrar movimiento DESTINO [" + NMov + "] en F14c." + cr
        Return .F. 
     EndIf

     *> Borrar MP destino.
     _okk = DelMvtF14c(NMovR)
     If !_okk
        _LxErr = _LxErr + "No se ha podido borrar movimiento DESTINO[" + NMovR + "] en F14c." + cr
        Return .F. 
     EndIf  

     *> Comprobar si hay mas reposiciones asociadas a esta ocupación.
     _CanRepos = CRepUbicMps(_CodPro, _CodArt, _CodUbi, _NumPal, _FecCad, _NumLot)

     *> Actualizar MPs de preparación asociados a esta reposición.
     FlgBlq = Iif(_CanRepos > 0, 'B', Space(1))

     Sw = F3_UpdTun('F14c', , 'F14cFlag1', 'FlgBlq', , f_where14, 'N')
     If Sw = .F.
        _LxErr=_LxErr + "No se han podido actualizar MPs de preparación[" + NMovR + "] en F14c." + cr
        Return .F. 
     EndIf

     *> Actualizar listas, si estos MPs están asignados.
     Sw = F3_UpdTun('F26l', , 'F26lFlag1', 'FlgBlq', , f_where26, 'N')
     If Sw = .F.
        _LxErr=_LxErr + "No se han podido actualizar listas de preparación[" + NMovR + "] en F14c." + cr
        Return .F. 
     EndIf
     
     *> Actualizar estado ocupación de reposición.
*     _Fields = "RowIdToChar(F16c" + _em + ".RowId) As F16cRowId, F16cCanRes"

     _Fields = "*"
     err = f3_sql(_Fields, 'F16c', f_where16, , , 'F16cCur')
     If !err
        _LxErr = _LxErr + 'Error leyendo ocupación a actualizar' + cr + ;
                          'MENSAJE: ' + Message() + cr
        Return .F.
     EndIf

     FlgBlq = Iif(_CanRepos > 0, 'R', Space(1))

     Select F16cCur
     Go Top
     If !Eof()
        *> Acumular la cantidad reservada resultante de los MPs de preparación
        *> que ya hubiera sobre esta ubicación.
        *> CResF14c está en Ora_Proc.

        _CRes = CResF14c(_CodPro, _CodArt, _CodUbi, _NumPal, _NumLot, _SitStk)

        Select F16cCur
        Replace F16cCanRes With _CRes
        If F16cCanRes < 0
           Replace F16cCanRes With 0
        EndIf

        _Selec = "Update F16c" + _em + ;
                 " Set F16cFlag1='" + FlgBlq + "'," + ;
                 "     F16cCanRes=" + Str(F16cCur.F16cCanRes) + ;
                 " Where " + GenerarRowId()

*                " Where RowId=CharToRowId('" + F16cCur.F16cRowId + "')"

        err = f3_SqlExec(_ASql, _Selec)
        If err <= 0
           _LxErr = _LxErr + "No se ha podido actualizar ocupación" + cr + ;
                             "MENSAJE: " + Message() + cr
           Return .F.
        EndIf
     EndIf

Return .T.

*>--------------------------------------------------------
*> Proceso de generación de reposiciones automáticas.
*> Pasos:
*>    - Seleccionar MPs '2000' a '2998' con F14cOrires
*>      diferente a F10cPickSn (F10cCodUbi=F14cUbiOri),
*>      que no estén bloqueados.
*>    - Generar MPs 3560/3060 (Unidades),
*>                  3570/3070 (Cajas),
*>                  3580/3080 (Grupos)
*>    - Bloquear MPs '2000' a '2998'.
*>    - Bloquear lista, si el MP ya se ha asignado.
*>
*> NO se repone de cajas a grupos. Esto solo se hace con
*> reposiciones por mínimos. AVC - 26.06.2000
*>
*> Reposiciones automáticas, solo de palet a cajas. No se
*> repone a grupos ni a unidades. AVC 26.09.2000
*>--------------------------------------------------------
Function ReposAuto

Private _Selec
Private TipMovE, TipMovS
Local _err

   _Selec = "Select * From F14c" + _em + ", F10c" + _em + ;
            " Where F14cTipMov Between '2000' And '2998'" + ;
            " And   F10cCodUbi=F14cUbiOri" + ;
            " And   F14cOriRes<>F10cPickSn"

   _err = f3_SqlExec(_ASql, _Selec, 'F14cCur')
   If _err <= 0
      _LxErr = 'Error cargando movimientos pendientes para reposiciones automáticas' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc With .T.
      Return .F.
   EndIf

   *> Para cada MP seleccionado, generar una reposición, si se puede.
   Select F14cCur
   Locate For !Empty(F14cOriRes)
   Do While Found()
      Wait Window 'Procesando movimiento: ' + ;
                  F14cCodArt + '-' + ;
                  F14cNumLot + '-' + ;
                  F14cUbiOri NoWait
      Do Case
         *> MP de cajas: Reponer de palet a cajas.
         Case F14cOriRes=='C'
            Do Case
               *> Es de palet: Reponer a cajas.
               Case F10cPickSn=='N'
                  TipMovS = '3570'
                  TipMovE = '3070'
                  Do ReposAutoMPK With F14cNumMov, TipMovS, TipMovE, 'S'

               *> Error: No se repone.
               OtherWise
            EndCase

            Select F14cCur
            Replace F14cOriRes With Space(1)

         *> MP de unidades: Reponer de palet a cajas y de cajas a unidades.
         Case F14cOriRes=='U'
            Do Case
               *> Es de palet: Reponer a cajas.
               Case F10cPickSn=='N'
                  TipMovS = '3570'
                  TipMovE = '3070'
                  Do ReposAutoMPK With F14cNumMov, TipMovS, TipMovE, 'S'

               *> Es de cajas: Reponer a unidades.
               Case F10cPickSn=='S'
*                  TipMovS = '3560'
*                  TipMovE = '3060'
*                  Do ReposAutoMPU With F14cNumMov, TipMovS, TipMovE, 'U'

               *> Error: No se repone.
               OtherWise
            EndCase

            Select F14cCur
            Replace F14cOriRes With Space(1)

         *> MP de grupos: Reponer de palet a cajas.
         Case F14cOriRes=='G'
            Do Case
               *> Es de palet: Reponer a cajas.
               Case F10cPickSn=='N'
                  TipMovS = '3570'
                  TipMovE = '3070'
                  Do ReposAutoMPK With F14cNumMov, TipMovS, TipMovE, 'S'

               *> Es de cajas: Reponer a grupos.
               Case F10cPickSn=='S'
*                  TipMovS = '3580'
*                  TipMovE = '3080'
*                  Do ReposAutoMPG With F14cNumMov, TipMovS, TipMovE

               *> Error: No se repone.
               OtherWise
            EndCase

            Select F14cCur
            Replace F14cOriRes With Space(1)

         *> Resto de casos: Error, no se toman en consideración.
         Otherwise
            Select F14cCur
            Replace F14cOriRes With Space(1)
      EndCase

      *> Tomar el siguiente MP pendiente de reposición.
      Select F14cCur
      Locate For !Empty(F14cOriRes)
   EndDo

   *> Actualizar los cambios.
   =SqlCommit(_ASql)

Return .T.

*>---------------------------------------------------------------------
*> Generación del movimiento de reposición automática de UN MP-Picking.
*> Pasos:
*>    - Seleccionar las ubicaciones destino de artículo (F12c).
*>    - Generar MPs 3560/3060 (Unidades),
*>                  3570/3070 (Cajas),
*>                  3580/3080 (Grupos)
*>    - Bloquear MPs '2000' a '2998'.
*>    - Bloquear lista, si el MP ya se ha asignado.
*> NO se repone de cajas a grupos. Esto solo se hace con
*> reposiciones por mínimos. AVC - 26.06.2000
*>---------------------------------------------------------------------
Function ReposAutoMPK
Parameters NMovMP, TipMovS, TipMovE, OriRes

Private _Selec, _OldMP, _NewMP, PrmA, FncA
Private _r1, _Minimo
Local _ok

   *> Comprobar que existe el MP.
   If !ExistF14c(NMovMP)
      _LxErr = _LxErr + "No se encontró MP de preparación picking [" + NMovMP + "]" + cr
      Return .F.
   EndIf  

   Select F14c
   Scatter MemVar

   *> Comprobar que la ubicación de este MP NO tenga ya una reposición.
   If ERepAnt(F14cCodPro, F14cCodArt, F14cUbiOri, TipMovS)
      Return
   EndIf

   *> Buscar la ocupación correpondiente a este MP.
   *> Devuelve cursor F16cCursor.
   If !GOcupacion(m.F14cCodPro, m.F14cCodArt, m.F14cUbiOri, m.F14cNumPal, m.F14cFecCad, m.F14cNumLot, m.F14cSitStk)
      _LxErr = _LxErr + 'Error cargando ocupación actual' + cr + ;
                        'MENSAJE: ' + Message() + cr
      Return .F.
   EndIf

   Select F16cCursor
   Go Top
   Scatter MemVar

   *> Seleccionar las ubicaciones de picking del artículo.
   If !GetUbiPic(m.F14cCodPro, m.F14cCodArt, OriRes, '_FRepAuto')
      Return .F.
   EndIf

   *> Si el artículo no tiene ubicaciones de picking, volver.
   Select _FRepAuto
   Go Top
   If Eof()
      Return
   EndIf

   *> Actualizar la cantidad ocupada y las reposiciones pendientes para cada
   *> una de las ubicaciones de picking del artículo de este MP.
   Do While !Eof()
      Replace CanOcuTot With COcuUbic(_FRepAuto.F12cCodPro, _FRepAuto.F12cCodArt, _FRepAuto.F12cCodUbi) + ;
                             CRepAnt(_FRepAuto.F12cCodPro, _FRepAuto.F12cCodArt, _FRepAuto.F12cCodUbi, TipMovE)
      *>
      Select _FRepAuto
      Skip
   EndDo

   *> Generar el movimiento de reposición sobre la ubicación de picking de menor cantidad,
   *> acumulando la cantidad actual con la de las reposiciones pendientes.
   _Minimo = 9999999
   _r1 = 0
   Select _FRepAuto
   Scan
      If CanOcuTot < _Minimo
         _Minimo = CanOcuTot
         _r1 = RecNo()
      EndIf
   EndScan

   *> Se posiciona en el registro que contiene la ubicación de picking con menor cantidad.
   Go _r1

   *> Crear objetos actualización.---------------------------------------
   FncA = CreateObject('OraFncActz')
   PrmA = CreateObject('OraPrmActz')
   FncA.ObjParm = PrmA

   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Actualizar parámetros para la función de actualización.
   *> Toma los datos del F16cCursor, que contiene la ocupación del MP.
   Select F16cCursor
   Scatter MemVar
   Do LlenarAcFnActz

   *> Generar movimientos pendientes.
   FncA.ObjParm.POCFis = F16cCanFis
   FncA.ObjParm.PMORes = 'N'

   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMpF14c('S', TipMovS)
   If _ok
      =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
      =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

      FncA.ObjParm.PUbOld = _FRepAuto.F12cCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMpF14c('E', TipMovE)
   EndIf

   *> Actualizar cambios.
   =SqlCommit(_ASql)   

   *> Eliminar el objeto de actualización.
   Release FncA
   Release PrmA

   If Used('_FRepAuto')
      Use In _FRepAuto
   EndIf

Return .T.

*>---------------------------------------------------------------------
*> Generación del movimiento de reposición automática de UN MP-Unidades.
*> Pasos:
*>    - Seleccionar las ubicaciones destino de artículo (F12c).
*>    - Generar MPs 3560/3060 (Unidades),
*>    - Bloquear MPs '2000' a '2998'.
*>    - Bloquear lista, si el MP ya se ha asignado.
*>---------------------------------------------------------------------
Function ReposAutoMPU
Parameters NMovMP, TipMovS, TipMovE, OriRes

Private _Selec, CanRepos, _FCaj
Private _r1, _Minimo, _OldMP, _NewMP, PrmA, FncA
Local _ok

   *> Comprobar que existe el MP.
   If !ExistF14c(NMovMP)
      _LxErr = _LxErr + "No se encontró MP de preparación unidades [" + NMovMP + "]" + cr
      Return .F.
   EndIf  

   Select F14c
   Scatter MemVar

   *> Comprobar que la ubicación de este MP NO tenga ya una reposición.
   If ERepAnt(F14cCodPro, F14cCodArt, F14cUbiOri, TipMovS)
      Return
   EndIf

   *> Buscar la ocupación correpondiente a este MP.
   *> Devuelve cursor F16cCursor.
   If !GOcupacion(m.F14cCodPro, m.F14cCodArt, m.F14cUbiOri, m.F14cNumPal, m.F14cFecCad, m.F14cNumLot, m.F14cSitStk)
      _LxErr = _LxErr + 'Error cargando ocupación actual' + cr + ;
                        'MENSAJE: ' + Message() + cr
      Return .F.
   EndIf

   Select F16cCursor
   Go Top
   Scatter MemVar

   *> Seleccionar las ubicaciones de unidades del artículo.
   If !GetUbiPic(m.F14cCodPro, m.F14cCodArt, OriRes, '_FRepAuto')
      Return .F.
   EndIf

   *> Si el artículo no tiene ubicaciones de unidades, volver.
   Select _FRepAuto
   Go Top
   If Eof()
      Return
   EndIf

   *> Actualizar la cantidad ocupada y las reposiciones pendientes para cada
   *> una de las ubicaciones de unidades del artículo de este MP.
   Do While !Eof()
      Replace CanOcuTot With COcuUbic(_FRepAuto.F12cCodPro, _FRepAuto.F12cCodArt, _FRepAuto.F12cCodUbi) + ;
                             CRepAnt(_FRepAuto.F12cCodPro, _FRepAuto.F12cCodArt, _FRepAuto.F12cCodUbi, TipMovE)
      *>
      Select _FRepAuto
      Skip
   EndDo

   *> Generar el movimiento de reposición sobre la ubicación de unidades de menor cantidad,
   *> acumulando la cantidad actual con la de las reposiciones pendientes.
   _Minimo = 9999999
   _r1 = 0
   Select _FRepAuto
   Scan
      If CanOcuTot < _Minimo
         _Minimo = CanOcuTot
         _r1 = RecNo()
      EndIf
   EndScan

   *> Se posiciona en el registro que contiene la ubicación de unidades con menor cantidad.
   Go _r1

   *> Calcular la cantidad a reponer. Si menor que cantidad grupo, volver.
   _FCaj = F16cUniPac * F16cPacCaj
   CanRepos = Floor((F16cCanFis - F16cCanRes) / _FCaj) * _FCaj
   If CanRepos < _FCaj
      CanRepos = F14cCanFis
   EndIf

   *> Crear objetos actualización.---------------------------------------
   FncA = CreateObject('OraFncActz')
   PrmA = CreateObject('OraPrmActz')
   FncA.ObjParm = PrmA

   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Actualizar parámetros para la función de actualización.
   *> Toma los datos del F16cCursor, que contiene la ocupación del MP.
   Select F16cCursor
   Scatter MemVar
   Do LlenarAcFnActz

   *> Generar movimientos pendientes.
   FncA.ObjParm.POCFis = F14cCanFis
   FncA.ObjParm.PMORes = 'S'

   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMpF14c('S', TipMovS)
   If _ok
      =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
      =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

      FncA.ObjParm.PUbOld = _FRepAuto.F12cCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMpF14c('E', TipMovE)
   EndIf

   *> Actualizar cambios.
   =SqlCommit(_ASql)   

   *> Eliminar el objeto de actualización.
   Release FncA
   Release PrmA

   If Used('_FRepAuto')
      Use In _FRepAuto
   EndIf

Return .T.

*>---------------------------------------------------------------------
*> Generación del movimiento de reposición automática de UN MP-Grupos.
*> Pasos:
*>    - Seleccionar las ubicaciones destino de artículo (F12c).
*>    - Generar MPs 3580/3080 (Grupos)
*>    - Bloquear MPs '2000' a '2998'.
*>    - Bloquear lista, si el MP ya se ha asignado.
*>---------------------------------------------------------------------
Function ReposAutoMPG
Parameters NMovMP, TipMovS, TipMovE

Private _Selec, f_select, f_from, f_where, f_orden, CanRepos
Private _r1, _Minimo, _OldMP, _NewMP, PrmA, FncA
Local _ok

   *> Comprobar que existe el MP.
   If !ExistF14c(NMovMP)
      _LxErr = _LxErr + "No se encontró MP de preparación grupos [" + NMovMP + "]" + cr
      Return .F.
   EndIf  

   Select F14c
   Scatter MemVar

   *> Comprobar que la ubicación de este MP NO tenga ya una reposición.
   If ERepAnt(F14cCodPro, F14cCodArt, F14cUbiOri, TipMovS)
      Return
   EndIf

   *> Buscar la ocupación correpondiente a este MP.
   *> Devuelve cursor F16cCursor.
   If !GOcupacion(m.F14cCodPro, m.F14cCodArt, m.F14cUbiOri, m.F14cNumPal, m.F14cFecCad, m.F14cNumLot, m.F14cSitStk)
      _LxErr = _LxErr + 'Error cargando ocupación actual' + cr + ;
                        'MENSAJE: ' + Message() + cr
      Return .F.
   EndIf

   Select F16cCursor
   Go Top
   Scatter MemVar

   *> Seleccionar las ubicaciones de grupos del artículo.
   If !GetUbiGrp(m.F14cCodPro, m.F14cCodArt, '_FRepAuto')
      Return .F.
   EndIf

   *> Si el artículo no tiene ubicaciones de grupos, volver.
   Select _FRepAuto
   Go Top
   If Eof()
      Return
   EndIf

   *> Actualizar la cantidad ocupada y las reposiciones pendientes para cada
   *> una de las ubicaciones de grupos del artículo de este MP.
   Do While !Eof()
      Replace CanOcuTot With COcuUbic(_FRepAuto.F08gCodPro, _FRepAuto.F08gCodArt, _FRepAuto.F08gCodUbi) + ;
                             CRepAnt(_FRepAuto.F08gCodPro, _FRepAuto.F08gCodArt, _FRepAuto.F08gCodUbi, TipMovE)
      *>
      Select _FRepAuto
      Skip
   EndDo

   *> Generar el movimiento de reposición sobre la ubicación de grupos de menor cantidad,
   *> acumulando la cantidad actual con la de las reposiciones pendientes.
   _Minimo = 9999999
   _r1 = 0
   Select _FRepAuto
   Scan
      If CanOcuTot < _Minimo
         _Minimo = CanOcuTot
         _r1 = RecNo()
      EndIf
   EndScan

   *> Se posiciona en el registro que contiene la ubicación de grupos con menor cantidad.
   Go _r1

   *> Calcular la cantidad a reponer. Si menor que cantidad grupo, volver.
   CanRepos = Floor(F14cCanFis / _FrepAuto.F08gUniAgr) * _FrepAuto.F08gUniAgr
   If CanRepos < _FrepAuto.F08gUniAgr
      Return
   EndIf

   *> Crear objetos actualización.---------------------------------------
   FncA = CreateObject('OraFncActz')
   PrmA = CreateObject('OraPrmActz')
   FncA.ObjParm = PrmA

   *> Inicializar objeto de actualización.
   FncA.ObjParm.Inicializar

   *> Actualizar parámetros para la función de actualización.
   *> Toma los datos del F16cCursor, que contiene la ocupación del MP.
   Select F16cCursor
   Scatter MemVar
   Do LlenarAcFnActz

   *> Generar movimientos pendientes.
   FncA.ObjParm.POCFis = CanRepos
   FncA.ObjParm.PMORes = 'S'

   _OldMP = Ora_NewMP()                     && Nº movimiento salida.
   _NewMP = Ora_NewMP()                     && Nº movimiento entrada.

   FncA.ObjParm.PMNMov = _OldMP
   _ok = GrabMpF14c('S', TipMovS)
   If _ok
      =BloqMpF14c(FncA.ObjParm.PUbOld)      && Bloquea MPs '2xxx'.
      =BloqListaF26l(FncA.ObjParm.PUbOld)   && Bloquea Listas '2xxx'.

      FncA.ObjParm.PUbOld = _FRepAuto.F08gCodUbi
      FncA.ObjParm.PMNMov = _NewMP
      _ok = GrabMpF14c('E', TipMovE)
   EndIf
   
   *> Actualizar cambios.
   =SqlCommit(_ASql)   

   *> Eliminar el objeto de actualización.
   Release FncA
   Release PrmA

   If Used('_FRepAuto')
      Use In _FRepAuto
   EndIf

Return .T.

*>----------------------------------------------------------
*> Obtener las ubicaciones de grupos de un artículo.
*> Recibe: CodPro, CodArt ---> Artículo.
*>         OriRes ----> Tipo de ubicación.
*>         PicCur ----> Nombre del cursor a devolver.
*>----------------------------------------------------------
Function GetUbiPic
Parameters CodPro, CodArt, OriRes, PicCur
Private f_select, f_from, f_where, f_orden, lx_sql
Local Err

   *> Seleccionar las ubicaciones de picking del artículo.
   f_select = " Select F12cCodPro, F12cCodArt, F12cCodAlm, F12cCodUbi, F12cPriori," + ;
              " F12cCanMin, F12cCanMax, 0 As CanOcuTot"
   f_from   = " From F12c" + _em + ",F10c" + _em

   f_where  = " Where F12cCodPro='" + CodPro + "'" + ;
              " And   F12cCodArt='" + CodArt + "'" + ;
              " And   F10cCodUbi=F12cCodUbi" + ;
              " And   F10cEstEnt='N'" + ;
              " And   F10cPickSN='" + OriRes+ "'"

   f_orden  = " Order By F12cPriori, F12cCodPro, F12cCodArt, F12cCodUbi"

   lx_sql = f_select + f_from + f_where + f_orden 
   Err = f3_SqlExec(_ASql, lx_sql, PicCur)
   If Err <= 0
      _LxErr = _LxErr + 'Error cargando ubicaciones de picking artículo' + cr + ;
                        'MENSAJE: ' + Message() + cr
      Return .F.
   EndIf

Return .T.

*>----------------------------------------------------------
*> Obtener las ubicaciones de grupos de un artículo.
*> Recibe: CodPro, CodArt ---> Artículo.
*>         PicCur ----> Nombre del cursor a devolver.
*>----------------------------------------------------------
Function GetUbiGrp
Parameters CodPro, CodArt, PicCur
Private f_select, f_from, f_where, f_orden, lx_sql
Local Err

   *> Seleccionar las ubicaciones de grupos del artículo.
   f_select = " Select F08g" + _em + ".*, 0 As CanOcuTot"
   f_from   = " From F08g" + _em + "," + ;
              "      F10c" + _em
   f_where  = " Where F08gCodPro='" + CodPro + "'" + ;
              " And   F08gCodArt='" + CodArt + "'" + ;
              " And   F10cCodUbi=F08gCodUbi" + ;
              " And   F10cEstEnt='N'" + ; 
              " And   F10cPickSN='G'"
   f_orden  = " Order By F08gLvlAgr, F08gCodPro, F08gCodArt, F08gCodUbi"

   lx_sql = f_select + f_from + f_where + f_orden 
   Err = f3_SqlExec(_ASql, lx_sql, PicCur)
   If Err <= 0
      _LxErr = _LxErr + 'Error cargando ubicaciones de grupos artículo' + cr + ;
                        'MENSAJE: ' + Message() + cr
      Return .F.
   EndIf

Return .T.

*>----------------------------------------------------------
*> Comprobar artículos multilote.
*> Recibe:      CodUbi - Ubicación destino a comprobar.
*>                F08C - Datos artículo a comprobar.
*>         _F16cOcuRep - Ocupaciones candidatas.
*>       _F12cCajToUni - Ubicaciones destino.
*>----------------------------------------------------------
Function VerificaMLote
Parameters CodUbi
Private _Where, _ok

   Select F08c
   If Eof() .Or. F08cMulLot # 'N'
      Return .T.
   EndIf

   *> Cargar las ocupaciones de la ubicación destino.
   _Where = "F16cCodUbi='" + CodUbi + "'"
   =f3_sql('*', ;
           'F16C', ;
           _Where, ;
           , , ;
           '_F16cOcuDes')

   Select _F16cOcuDes
   Locate For F16cCodPro==_F16cOcuRep.F16cCodPro .And. ;
              F16cCodArt==_F16cOcuRep.F16cCodArt .And. ;
              F16cNumLot#_F16cOcuRep.F16cNumLot

   _ok = !Found()
   Use In _F16cOcuDes

Return _ok

*>----------------------------------------------------------
*> Comprobar si existe el MP.
*>----------------------------------------------------------
Function ExistF14c
Parameter NMov
Private _ok

m.F14cNumMov = NMov
_ok = f3_seek('F14C')

Return _ok 

*>----------------------------------------------------------
*> Borra el MP.
*>----------------------------------------------------------
Function DelMvtF14c
Parameter NMov

Private f_delete, Err, _ok

f_delete = "Delete From F14c" + _em + " Where F14cNumMov='" + NMov + _cm

Err = f3_SqlExec(_ASql, f_delete) 

_ok=IIF(Err>0,.T.,.F.)

Return _ok

*>----------------------------------------------------------
*> Grabar Movimientos pendientes de reposiciones.
*> Recibe: Tipo movimiento(E/S), Tipo movimiento en MP.
*>----------------------------------------------------------
Function GrabMpF14c
Parameters EntSal, TipMov

Private _ok

FncA.ObjParm.PMEnSa = EntSal    
FncA.ObjParm.PMFgMP = 'S'
FncA.ObjParm.PMTMov = TipMov
*   FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMTMac = 'MSTD'
FncA.ObjParm.PMFMov = Date()
FncA.ActMP

_ok = Iif(FncA.ObjParm.PWCRtn >= '50', .F., .T.)

Return _ok

*>----------------------------------------------------------
*> Bloquear MPs '2xxx' que dependen de una reposición.
*> Recibe: Ubicación de la reposición.
*>----------------------------------------------------------
Function BloqMPF14c
Parameters CodUbi

Private _Selec

   _Selec = "Update F14c" + _em + ;
            " Set F14cFlag1='B'" + ;
            " Where F14cUbiOri='" + CodUbi + _cm + ;
            " And   F14cTipMov Between '2000' And '2998'"

   err = f3_SqlExec(_ASql, _Selec)
   If err <= 0
      _LxErr = 'Error actualizando flag bloqueo de MPs' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

Return .T.

*>----------------------------------------------------------
*> Bloquear Listas '2xxx' que dependen de una reposición.
*> Recibe: Ubicación de la reposición.
*>----------------------------------------------------------
Function BloqListaF26l
Parameters CodUbi

Private _Selec
Local err

   _Selec = "Update F26l" + _em + ;
            " Set F26lFlag1='B'" + ;
            " Where F26lUbiOri='" + CodUbi + _cm + ;
            " And   F26lTipMov Between '2000' And '2998'"

   err = f3_SqlExec(_ASql, _Selec)
   If err <= 0
      _LxErr = 'Error actualizando flag bloqueo de Listas' + cr + ;
               'MENSAJE: ' + Message() + cr
      Do Form St3Inc
      _LxErr = ''
      Return .F.
   EndIf

Return .T.

*>------------------------------------------------------------
*> Rellenar las propiedades para la función de actualización.
*>------------------------------------------------------------
Procedure LlenarAcFnActz

*****>FncA.ObjParm.PONEnt=
*****>FncA.ObjParm.PMTMov=
*FncA.ObjParm.POTDoc=F16cTipDoc   
*FncA.ObjParm.PONdoc=F16cNumDoc   
*FncA.ObjParm.POLDoc=F16cLinDoc   
*****>FncA.ObjParm.PMFDoc=
*****>FncA.ObjParm.PMDAso=
*****>FncA.ObjParm.PONPed=
*****>FncA.ObjParm.POLPed=
FncA.ObjParm.POCArt=F16CCODART   
FncA.ObjParm.PONLot=F16CNUMLOT   
FncA.ObjParm.POSStk=F16CSITSTK   
FncA.ObjParm.POFCad=F16CFECCAD   
FncA.ObjParm.POCFis=F16CCANFIS   
*****>FncA.ObjParm.PMRHab=
FncA.ObjParm.PUbOld=F16CCODUBI   
*****>FncA.ObjParm.PUBNew=
FncA.ObjParm.PONPal=F16CNUMPAL   
FncA.ObjParm.POTPal=F16CTAMPAL   
FncA.ObjParm.POFUni=F16CUNIVEN   
FncA.ObjParm.POFSer=F16CUNIPAC   
FncA.ObjParm.POFEnv=F16CPACCAJ   
FncA.ObjParm.POFPAL=F16CCAJPAL   
FncA.ObjParm.POFFab=F16CFECFAB   
****>FncA.ObjParm.POFCal=
FncA.ObjParm.PONAna=F16CNUMANA   
FncA.ObjParm.POCPro=F16CCODPRO   
*****>FncA.ObjParm.PMCOpe=
*****>FncA.ObjParm.PMNLst=
*****>FncA.ObjParm.PMNExp= 

*****>FncA.ObjParm.PMORes=
*****>FncA.ObjParm.PMTUbi= 
*****>FncA.ObjParm.PMTMas=
*****>FncA.ObjParm.PMNMas=
*****>FncA.ObjParm.PMMUni=
*****>FncA.ObjParm.PMORut=
*****>FncA.ObjParm.PMTERe=
*****>FncA.ObjParm.PMCERe=
*****>FncA.ObjParm.PMVHab=
*****>FncA.ObjParm.PMTMac=
*****>FncA.ObjParm.PMNMac=
*****>FncA.ObjParm.PMSecc=
FncA.ObjParm.POPico=F16cEsPico
FncA.ObjParm.POCAlm=_Alma

FncA.ObjParm.PMFlg1=F16cFlag1
FncA.ObjParm.PMFlg2=F16cFlag2

Return

*>----------------------------------------------------------------------------
*> Rellenar las propiedades para la función de actualización, a partir de MP.
*>----------------------------------------------------------------------------
Procedure LlenarAcFnActzMP

FncA.ObjParm.POCArt=F14CCODART
FncA.ObjParm.PONLot=F14CNUMLOT
FncA.ObjParm.POSStk=F14CSITSTK
FncA.ObjParm.POFCad=F14CFECCAD
FncA.ObjParm.POCFis=F14CCANFIS

FncA.ObjParm.PUbOld=F14CUbiOri
FncA.ObjParm.PONPal=F14CNUMPAL
FncA.ObjParm.POTPal=F14CTAMPAL
FncA.ObjParm.POFUni=F14CUNIVEN
FncA.ObjParm.POFSer=F14CUNIPAC
FncA.ObjParm.POFEnv=F14CPACCAJ
FncA.ObjParm.POFPAL=F14CCAJPAL
FncA.ObjParm.POFFab=F14CFECFAB
FncA.ObjParm.PONAna=F14CNUMANA
FncA.ObjParm.POCPro=F14CCODPRO

*FncA.ObjParm.POPico=F14cEsPico
FncA.ObjParm.POCAlm=_Alma

FncA.ObjParm.PMFlg1=F14cFlag1
FncA.ObjParm.PMFlg2=F14cFlag2

Return

*>-----------------------------------------------------------------------
*> Obtiene el string equivalente al antiguo RowId de ORACLE.
*> Debe estar activo el cursor imagen de F16c. (Generalmente _F16cOcuRep)
*> Devuelve: Cadena con sentencia WHERE equivalente a RowID.
*>-----------------------------------------------------------------------
Function GenerarRowId
Local cRowId, oF16c

Scatter Name oF16c

cRowId = ""
cRowId = cRowId + "F16cCodPro='" + oF16c.F16cCodPro + "' And "
cRowId = cRowId + "F16cCodArt='" + oF16c.F16cCodArt + "' And "
cRowId = cRowId + "F16cCodUbi='" + oF16c.F16cCodUbi + "' And "
cRowId = cRowId + "F16cNumLot='" + oF16c.F16cNumLot + "' And "
cRowId = cRowId + "F16cNumPal='" + oF16c.F16cNumPal + "' And "
cRowId = cRowId + "F16cSitStk='" + oF16c.F16cSitStk + "' And "
cRowId = cRowId + "F16cFecCad=" + _GCD(oF16c.F16cFecCad)

Return cRowId

*>-----------------------------------------------------------------------
*> Concatenar las columnas que equivalen al RowId de ORACLE.
*> Debe estar activo el cursor imagen de F16c. (Generalmente _F16cOcuRep)
*> Devuelve: Cadena con columnas del RowId.
*>-----------------------------------------------------------------------
Function ConcatenarRowId
Local cRowId, oF16c

Scatter Name oF16c

cRowId = ""
cRowId = cRowId + oF16c.F16cCodPro
cRowId = cRowId + oF16c.F16cCodArt
cRowId = cRowId + oF16c.F16cCodUbi
cRowId = cRowId + oF16c.F16cNumLot
cRowId = cRowId + oF16c.F16cNumPal
cRowId = cRowId + oF16c.F16cSitStk
cRowId = cRowId + DToS(oF16c.F16cFecCad)

Return cRowId

*>--------------------------------------------------------------
*> Concatenar los nombresd de las columnas del RowId de ORACLE.
*> Devuelve: Cadena con las columnas que formarían el RowId
*>--------------------------------------------------------------
Function CtrlConcatenarRowId
Local cCatRowId

cCatRowId = "F16cCodPro + F16cCodArt + F16cCodUbi + F16cNumLot + F16cNumPal + F16cSitStk + DToS(oF16c.F16cFecCad)"

Return cCatRowId

*>----------------------------------------------------------
*> Obtener el tamaño del equivalente al RowId de ORACLE.
*> Debe estar activo el cursor F16c.
*> Devuelve: Tamaño de las columnas del RowId.
*>----------------------------------------------------------
Function ObtenerTamanyRowId
Local nRowId
Local lIsOpen, oPRC, cOldSelec

cOldSelec = Select ()

lIsOpen = !Used('F16c')
If !lIsOpen
   oPRC = CreateObject('procaot')
   =oPRC.OpenTabla('F16c')
EndIf

Select F16c
Scatter Name oF16c

nRowId = 0
nRowId = nRowId + Len(F16cCodPro)
nRowId = nRowId + Len(oF16c.F16cCodArt)
nRowId = nRowId + Len(oF16c.F16cCodUbi)
nRowId = nRowId + Len(oF16c.F16cNumLot)
nRowId = nRowId + Len(oF16c.F16cNumPal)
nRowId = nRowId + Len(oF16c.F16cSitStk)
nRowId = nRowId + Len(DToS(oF16c.F16cFecCad))

If !Empty(cOldSelec)
   Select (cOldSelec)
EndIf

Return nRowId
