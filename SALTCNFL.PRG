*>
*> Procedimientos llamados desde el FORM SaLTCnfL para confirmar listas.----

*>======================================================================
*> CnfL_PrcConf ............. Proceso de confirmación de listas
*>======================================================================
Procedure CnfL_PrcConf
Private FncA, PrmA, n_KilMov, n_VolMov, b_HayErr, b_ErrMov
Store 0 to n_PesMov, n_VolMov

*> Crear objetos actualización.---------------------------------------
FncA = CreateObject('OraFncActz')
PrmA = CreateObject('OraPrmActz')
PrmA.Inicializar
FncA.ObjParm = PrmA

b_HayErr = .F.
Select LSTCNF
Scan For Sel==1
   If LSTCNF.F26lTipMov = _MovExp         && No trata mov.expedición
      Skip
      Loop
   EndIf
      
   b_ErrMov = .F.
   Wait Window 'Confirmando movimiento: ' + LSTCNF.F26lCodArt + Space(1) + ;
                                            LSTCNF.F26lNumLot + Space(1) + ;
                                            LSTCNF.F26lUbiOri NoWait

   Do Case
      ***> Confirmación de movimientos de preparación.----------------
      Case Between(LSTCNF.F26lTipMov, '2000', '2998')
         *> Buscar movimientos en MP.---------------------------------
         m.F14cNumMov = LSTCNF.F26lNumMov
         Sw = F3_Seek('F14c')
         If Sw = .F.
            _LxErr = 'Movimiento Nº: ' + LSTCNF.F26lNumMov + ;
                     ' Lista Nº: ' + LSTCNF.F26lNumLst + cr + ;
                     'no se encuentra en Movimientos Pendientes' + cr
            Do Form St3Inc
            _LxErr = ''
         Else
            Do CnfL_M2000
         EndIf
         
      ***> Confirmación de movimientos de reposición automática.-------
      Case F26lTipMov = '3510'
         m.F14cNumMov = LSTCNF.F26lNumMov
         c_MovSal = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiSal = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiSal = m.F14cUbiOri')
         b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3510', .F., .T.)
         
         m.F14cNumMov = Right(Replicate("0",10) + AllTrim(Str(Val(LSTCNF.F26lNumMov) + 1)), 10)
         c_MovEnt = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiEnt = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiEnt = m.F14cUbiOri')
         If b_Ok = .T.
            b_Ok = IIf( Sw = .F.  .Or.  c_TipMov <> '3010', .F., .T.)
         EndIf
         
         If b_Ok = .F.
            =F3_sn(1,1,"Movimiento de la lista no se encuentra en movimientos pendientes") 
         Else
            Do CnfL_M3510
         EndIf
         
         
      ***> Confirmación reposic.a unidades (Ubic.Convencionales).-----
      Case F26lTipMov = '3560'
         m.F14cNumMov = LSTCNF.F26lNumMov
         c_MovSal = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiSal = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiSal = m.F14cUbiOri')
         b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3560', .F., .T.)
         
         m.F14cNumMov = Right(Replicate("0",10) + AllTrim(Str(Val(LSTCNF.F26lNumMov) + 1)), 10)
         c_MovEnt = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiEnt = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiEnt = m.F14cUbiOri')
         If b_Ok = .T.
            b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3060', .F., .T.)
         EndIf
         
         If b_Ok = .F.
            =F3_sn(1,1,"Movimiento de la lista no se encuentra en movimientos pendientes") 
         Else
            Do CnfL_M3560
         EndIf
         
      ***> Confirmación de reposiciones a cajas.----------------------
      Case F26lTipMov = '3570'
         m.F14cNumMov = LSTCNF.F26lNumMov
         c_MovSal = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiSal = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiSal = m.F14cUbiOri')
         b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3570', .F., .T.)
         
         m.F14cNumMov = Right(Replicate("0",10) + AllTrim(Str(Val(LSTCNF.F26lNumMov) + 1)), 10)
         c_MovEnt = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiEnt = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiEnt = m.F14cUbiOri')
         If b_Ok = .T.
            b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3070', .F., .T.)
         EndIf
         
         If b_Ok = .F.
            =F3_sn(1,1,"Movimiento de la lista no se encuentra en movimientos pendientes") 
         Else
            Do CnfL_M3570
         EndIf

      ***> Confirmación de reposiciones a grupos.----------------------
      Case F26lTipMov = '3580'
         m.F14cNumMov = LSTCNF.F26lNumMov
         c_MovSal = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiSal = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiSal = m.F14cUbiOri')
         b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3580', .F., .T.)
         
         m.F14cNumMov = Right(Replicate("0",10) + AllTrim(Str(Val(LSTCNF.F26lNumMov) + 1)), 10)
         c_MovEnt = m.F14cNumMov
         c_TipMov = Space(4)
         c_UbiEnt = Space(14)
         Sw = F3_Seek('F14c', , , 'c_TipMov = m.F14cTipMov, c_UbiEnt = m.F14cUbiOri')
         If b_Ok = .T.
            b_Ok = IIf(Sw = .F.  .Or.  c_TipMov <> '3080', .F., .T.)
         EndIf
         
         If b_Ok = .F.
            =F3_sn(1,1,"Movimiento de la lista no se encuentra en movimientos pendientes") 
         Else
            Do CnfL_M3580
         EndIf
   EndCase

   =Ora_AcCbLs(LSTCNF.F26lNumLst)

*   If b_ErrMov = .F.
*      *> Recalcular cabecera.-----------------------------------------
*      b_Ok = Ora_AcCbLs(LSTCNF.F26lNumLst)
*      If b_Ok = .F.
*         b_ErrMov = .T.
*      EndIf
*   EndIf

   If b_ErrMov = .F.
      =SqlCommit(_ASql)   
   Else   
      =SqlRollback(_ASql)
   EndIf

   *>
   Select LSTCNF   
EndScan

*> Liberar objetos actualización.-------------------------------------
Release FncA
Release PrmA
Return 

*>====================================================================
*> CnfL_M2000 ............... Actualización movimiento 2000-2998
*>====================================================================
Procedure CnfL_M2000
Private n_PesMov, n_VolMov, f_Where

*> Calcular volumen y peso del movimiento.----------------------------
Select LSTCNF
Store 0 to n_PesMov, n_VolMov
Do PesVolAr In Ora_Ca00 With F26lCodPro, F26lCodArt, F26lCanFis, n_PesMov, n_VolMov

FncA.ObjParm.Inicializar

Do CnfL_CrgParm
FncA.ObjParm.PUbOld = LSTCNF.F26lUbiOri
FncA.ObjParm.POCUbi = LSTCNF.F26lUbiOri
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = LSTCNF.F26lCanFis
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'S'	
FncA.ObjParm.PTAcc  = '06'

FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
FncA.ObjParm.PMTMov = _MovOri
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMMvMP = LSTCNF.F26lNumMov
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
Select LSTCNF
If Empty(LSTCNF.F26lUbiDes)
   *> Ir a buscar la primera ubicación de expedición.-----------------
   f_Where = "F10cPickSN = 'E' And F10cEstEnt <> 'B' And F10cEstSal <> 'B'"
   c_UbiExp = Space(14)
   b_Ok = F3_SeekTun('F10c', f_Where, , 'c_UbiExp = m.F10cCodUbi')
   If b_Ok = .F.
      b_ErrMov = .T.
   Else
      Replace LSTCNF.F26lUbiDes With c_UbiExp
   EndIf
Else
   c_UbiExp = LSTCNF.F26lUbiDes
EndIf

Do CnfL_CrgParm
FncA.ObjParm.POCUbi = LSTCNF.F26lUbiDes
FncA.ObjParm.PUbOld = LSTCNF.F26lUbiDes
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = LSTCNF.F26lCanFis
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'E'	
FncA.ObjParm.PTAcc  = '07'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
FncA.ObjParm.PMTMov = _MovDes
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMMvMP = LSTCNF.F26lNumMov
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
Do CnfL_CrgParm
FncA.ObjParm.POCFis = 0
FncA.ObjParm.PMTMov = _MovExp
FncA.ObjParm.PMNMov = LSTCNF.F26lNumMov
FncA.ObjParm.PMFlg1 = ''
FncA.ObjParm.PUbOld = c_UbiExp
FncA.ObjParm.PUbNew = Space(14)	
FncA.ObjParm.PMFgMP = 'S'
FncA.ActMP
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Actualizar fecha y estado del movimiento en lista.-----------------
Sw = CnfL_ActLst(LSTCNF.F26lNumMov, LSTCNF.F26lNumLst)
If Sw = .F.
   b_ErrMov = .T.
EndIf
                
*> Si tiene Mac, actualizarlo.----------------------------------------
If !Empty(LSTCNF.F26lNumMac)
   *> Actualizar detalle de Mac.--------------------------------------
   Sw = CnfL_ActLMac(LSTCNF.F26lTipMac, LSTCNF.F26lNumMac, LSTCNF.F26lNumMov)
   If Sw = .F.
      b_ErrMov = .T.
   EndIf

   *> Actualizar cabecera de Mac.-------------------------------------
   Sw = CnfL_ActMac(LSTCNF.F26lTipMac, LSTCNF.F26lNumMac)
   If Sw = .F.
      b_ErrMov = .T.
   EndIf
EndIf

Return

*>====================================================================
*> CnfL_M3510 ............... Confirmar reposiciones AUTOMATICAS.
*>====================================================================
Procedure CnfL_M3510
Private n_PesMov, n_VolMov, f_Campos, f_Valores, f_Where
Store 0 To n_PesMov, n_VolMov

*> Calcular peso y volumen del movimiento.----------------------------
Select LSTCNF
Do PesVolAr In Ora_Ca00 With F26lCodPro, F26lCodArt, F26lCanFis, n_PesMov, n_VolMov

*> Buscar ocupación y poner CanRes = CanFis.--------------------------
Select LSTCNF
n_CanRes = F26lCanFis
f_Campos  = "F16cCanRes"
f_Valores = "n_CanRes"
f_Where = "F16cCodUbi='" + c_UbiSal + "' And F16cNumPal='" + F26lNumPal + ;
    "' And F16cCodPro='" + F26lCodPro + "' And F16cCodArt='" + F26lCodArt + ;
    "' And F16cNumLot='" + F26lNumLot + "' And F16cSitStk='" + F26lSitStk + ;
    "' And F16cFecCad=" + _GCD(F26lFecCad)

Sw = F3_UpdTun('F16c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

***> Tratamiento del movimiento 3510.---------------------------------
*> Actualización por movimiento 3510.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.PUbOld = c_UbiSal
FncA.ObjParm.POCUbi = c_UbiSal
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = 0
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'S'	
FncA.ObjParm.PTAcc  = '06'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3510'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3510.----------------------------------
f_Where = "F14cNumMov = '" + c_MovSal + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación origen.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiOri, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

***> Tratamiento del movimiento 3010.---------------------------------
*> Actualización por movimiento 3010.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.POCUbi = c_UbiEnt
FncA.ObjParm.PUbOld = c_UbiEnt
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = 0
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'E'	
FncA.ObjParm.PTAcc  = '07'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3010'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3010.----------------------------------
f_Where = "F14cNumMov = '" + c_MovEnt + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en LS y cambiar ubicación origen por
*> ubicación nueva.---------------------------------------------------
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F26lUbiOri, F26lFlag1"
f_Valores = "c_UbiEnt, c_Flag1"
f_Where = "F26lUbiOri='" + c_UbiSal   + "' And " + ;
          "F26lNumPal='" + F26lNumPal + "' And " + ;
          "F26lCodPro='" + F26lCodPro + "' And " + ;
          "F26lCodArt='" + F26lCodArt + "' And " + ;
          "F26lNumLot='" + F26lNumLot + "' And " + ;
          "F26lSitStk='" + F26lSitStk + "' And " + ;
          "F26lFecCad=" + _GCD(F26lFecCad) + " And F26lTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F26l', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en MP y cambiar ubicación origen por
*> ubicación nueva, y recalcular CanRes ocupación.--------------------
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F14cUbiOri, F14cFlag1"
f_Valores = "c_UbiEnt, c_Flag1"
f_Where = "F14cUbiOri='" + c_UbiSal   + "' And " + ;
          "F14cNumPal='" + F26lNumPal + "' And " + ;
          "F14cCodPro='" + F26lCodPro + "' And " + ;
          "F14cCodArt='" + F26lCodArt + "' And " + ;
          "F14cNumLot='" + F26lNumLot + "' And " + ;
          "F14cSitStk='" + F26lSitStk + "' And " + ;
          "F14cFecCad=" + _GCD(F26lFecCad) + " And F14cTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F14c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación destino.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiDes, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

*> Actualizar fecha y estado del movimiento en lista.-----------------
Sw = CnfL_ActLst(LSTCNF.F26lNumMov, LSTCNF.F26lNumLst)
If Sw = .F.
   b_ErrMov = .T.
EndIf
Return

*>====================================================================
*> CnfL_M3560 ............... Confirmar reposiciones de UNIDADES.
*>====================================================================
Procedure CnfL_M3560
Private n_PesMov, n_VolMov, f_Campos, f_Valores, f_Where
Store 0 To n_PesMov, n_VolMov

*> Calcular peso y volumen del movimiento.----------------------------
Select LSTCNF
Do PesVolAr In Ora_Ca00 With F26lCodPro, F26lCodArt, F26lCanFis, n_PesMov, n_VolMov

***> Tratamiento del movimiento 3560.---------------------------------
*> Actualización por movimiento 3560.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.PUbOld = c_UbiSal
FncA.ObjParm.POCUbi = c_UbiSal
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = LSTCNF.F26lCanFis
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'S'	
FncA.ObjParm.PTAcc  = '06'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3560'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3560.----------------------------------
f_Where = "F14cNumMov = '" + c_MovSal + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación origen.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiOri, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

***> Tratamiento del movimiento 3060.---------------------------------
*> Actualización por movimiento 3060.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.POCUbi = c_UbiEnt
FncA.ObjParm.PUbOld = c_UbiEnt
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = 0
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'E'	
FncA.ObjParm.PTAcc  = '07'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3060'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3060.----------------------------------
f_Where = "F14cNumMov = '" + c_MovEnt + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en LS y cambiar ubicación origen por
*> ubicación nueva.---------------------------------------------------
*>
*> NO se cambia la ubicación, porque no se permite reservar de
*> ocupaciones con reposiciones pendientes. AVC - 15.11.2000
*>
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F26lUbiOri, F26lFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Campos  = "F26lFlag1"
f_Valores = "c_Flag1"

f_Where = "F26lUbiOri='" + c_UbiSal          + "' And " + ;
          "F26lNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F26lCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F26lCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F26lNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F26lSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F26lFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And F26lTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F26l', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en MP y cambiar ubicación origen por
*> ubicación nueva, y recalcular CanRes ocupación.--------------------
*>
*> NO se cambia la ubicación, porque no se permite reservar de
*> ocupaciones con reposiciones pendientes. AVC - 15.11.2000
*>
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F14cUbiOri, F14cFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Campos  = "F14cFlag1"
f_Valores = "c_Flag1"

f_Where = "F14cUbiOri='" + c_UbiSal          + "' And " + ;
          "F14cNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F14cCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F14cCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F14cNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F14cSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F14cFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F14cTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F14c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación destino.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiDes, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

*> Actualizar fecha y estado del movimiento en lista.-----------------
Sw = CnfL_ActLst(LSTCNF.F26lNumMov, LSTCNF.F26lNumLst)
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar flag reposición de la ocupación.
Sw = CnfL_ActOcu(LSTCNF.F26lUbiOri, ;
                 LSTCNF.F26lCodPro, ;
                 LSTCNF.F26lCodArt, ;
                 LSTCNF.F26lNumPal, ;
                 LSTCNF.F26lNumLot, ;
                 LSTCNF.F26lSitStk)
Return

*>====================================================================
*> CnfL_M3570 ............... Confirmar reposiciones de CAJAS.
*>====================================================================
Procedure CnfL_M3570
Private n_PesMov, n_VolMov, f_Campos, f_Valores, f_Where
Store 0 To n_PesMov, n_VolMov

*> Calcular peso y volumen del movimiento.----------------------------
Select LSTCNF
Do PesVolAr In Ora_Ca00 With F26lCodPro, F26lCodArt, F26lCanFis, n_PesMov, n_VolMov

***> Tratamiento del movimiento 3570.---------------------------------
*> Actualización por movimiento 3570.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.PUbOld = c_UbiSal
FncA.ObjParm.POCUbi = c_UbiSal
FncA.ObjParm.POCFis = LSTCNF.f26lCanFis
FncA.ObjParm.POCRes = LSTCNF.f26lCanFis
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'S'	
FncA.ObjParm.PTAcc  = '06'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3570'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3570.----------------------------------
f_Where = "F14cNumMov = '" + c_MovSal + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación origen.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiOri, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

***> Tratamiento del movimiento 3070.---------------------------------
*> Actualización por movimiento 3070.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.POCUbi = c_UbiEnt
FncA.ObjParm.PUbOld = c_UbiEnt
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = 0
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'E'	
FncA.ObjParm.PTAcc  = '07'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3070'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3070.----------------------------------
f_Where = "F14cNumMov = '" + c_MovEnt + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en LS y cambiar ubicación origen por
*> ubicación nueva.---------------------------------------------------
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F26lUbiOri, F26lFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Where = "F26lUbiOri='" + c_UbiSal          + "' And " + ;
          "F26lNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F26lCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F26lCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F26lNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F26lSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F26lFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F26lTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F26l', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en MP y cambiar ubicación origen por
*> ubicación nueva, y recalcular CanRes ocupación.--------------------
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F14cUbiOri, F14cFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Where = "F14cUbiOri='" + c_UbiSal          + "' And " + ;
          "F14cNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F14cCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F14cCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F14cNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F14cSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F14cFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F14cTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F14c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación destino.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiDes, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

*> Actualizar fecha y estado del movimiento en lista.-----------------
Sw = CnfL_ActLst(LSTCNF.F26lNumMov, LSTCNF.F26lNumLst)
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar flag reposición de la ocupación.
Sw = CnfL_ActOcu(LSTCNF.F26lUbiOri, ;
                 LSTCNF.F26lCodPro, ;
                 LSTCNF.F26lCodArt, ;
                 LSTCNF.F26lNumPal, ;
                 LSTCNF.F26lNumLot, ;
                 LSTCNF.F26lSitStk)
Return

*>====================================================================
*> CnfL_M3580 ............... Confirmar reposiciones de GRUPOS.
*>====================================================================
Procedure CnfL_M3580
Private n_PesMov, n_VolMov, f_Campos, f_Valores, f_Where
Store 0 To n_PesMov, n_VolMov

*> Calcular peso y volumen del movimiento.----------------------------
Select LSTCNF
Do PesVolAr In Ora_Ca00 With F26lCodPro, F26lCodArt, F26lCanFis, n_PesMov, n_VolMov

***> Tratamiento del movimiento 3580.---------------------------------
*> Actualización por movimiento 3580.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm

FncA.ObjParm.PUbOld = c_UbiSal
FncA.ObjParm.POCUbi = c_UbiSal
FncA.ObjParm.POCFis = LSTCNF.f26lCanFis
FncA.ObjParm.POCRes = LSTCNF.F26lCanFis
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'S'	
FncA.ObjParm.PTAcc  = '06'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3580'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3580.----------------------------------
f_Where = "F14cNumMov = '" + c_MovSal + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación origen.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiOri, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

***> Tratamiento del movimiento 3080.---------------------------------
*> Actualización por movimiento 3080.---------------------------------
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.POCUbi = c_UbiEnt
FncA.ObjParm.PUbOld = c_UbiEnt
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = 0
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'E'	
FncA.ObjParm.PTAcc  = '07'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = '3080'
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente 3080.----------------------------------
f_Where = "F14cNumMov = '" + c_MovEnt + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en LS y cambiar ubicación origen por
*> ubicación nueva.---------------------------------------------------
*>
*> NO se cambia la ubicación, porque no se permite reservar de
*> ocupaciones con reposiciones pendientes. AVC - 15.11.2000
*>
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F26lUbiOri, F26lFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Campos  = "F26lFlag1"
f_Valores = "c_Flag1"

f_Where = "F26lUbiOri='" + c_UbiSal          + "' And " + ;
          "F26lNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F26lCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F26lCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F26lNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F26lSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F26lFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F26lTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F26l', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en MP y cambiar ubicación origen por
*> ubicación nueva, y recalcular CanRes ocupación.--------------------
*>
*> NO se cambia la ubicación, porque no se permite reservar de
*> ocupaciones con reposiciones pendientes. AVC - 15.11.2000
*>
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F14cUbiOri, F14cFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Campos  = "F14cFlag1"
f_Valores = "c_Flag1"

f_Where = "F14cUbiOri='" + c_UbiSal          + "' And " + ;
          "F14cNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F14cCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F14cCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F14cNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F14cSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F14cFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F14cTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F14c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación destino.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiDes, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

*> Actualizar fecha y estado del movimiento en lista.-----------------
Sw = CnfL_ActLst(LSTCNF.F26lNumMov, LSTCNF.F26lNumLst)
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar flag reposición de la ocupación.
Sw = CnfL_ActOcu(LSTCNF.F26lUbiOri, ;
                 LSTCNF.F26lCodPro, ;
                 LSTCNF.F26lCodArt, ;
                 LSTCNF.F26lNumPal, ;
                 LSTCNF.F26lNumLot, ;
                 LSTCNF.F26lSitStk)
*>
Return

*>====================================================================
*> CnfL_MRec ............... Confirmar entrada en destino.
*> Reposiciones en RF, con la salida origen confirmada.
*>====================================================================
Procedure CnfL_MRec
Parameters cTipMov

Private n_PesMov, n_VolMov, f_Campos, f_Valores, f_Where
Store 0 To n_PesMov, n_VolMov

*> Tipo movimiento incorrecto.
If Type('cTipMov') # 'C'
   Return .F.
EndIf

*> NO es movimiento de entrada por reposición.
If SubStr(cTipmov, 1, 2) # '30'
   Return .F.
EndIf

*> Calcular peso y volumen del movimiento.----------------------------
Select LSTCNF
Do PesVolAr In Ora_Ca00 With F26lCodPro, F26lCodArt, F26lCanFis, n_PesMov, n_VolMov

*> Actualizar el movimiento de entrada en ubicación destino.
FncA.ObjParm.Inicializar
Do CnfL_CrgParm
FncA.ObjParm.POCUbi = c_UbiEnt
FncA.ObjParm.PUbOld = c_UbiEnt
FncA.ObjParm.POCFis = LSTCNF.F26lCanFis
FncA.ObjParm.POCRes = 0
FncA.ObjParm.POVocu = n_VolMov
FncA.ObjParm.PMEnSa = 'E'	
FncA.ObjParm.PTAcc  = '07'
FncA.Ejecutar
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf
         
*> Creación del movimiento en Histórico.------------------------------
FncA.ObjParm.PMTMov = cTipMov
FncA.ObjParm.PMNMov = ''
FncA.ObjParm.PMFMov = Date()
FncA.ActHM
If FncA.ObjParm.PWCRtn >= '50'
   b_ErrMov = .T.
EndIf

*> Borrar movimiento pendiente cTipMov.----------------------------------
f_Where = "F14cNumMov = '" + c_MovEnt + "'"
Sw = F3_DelTun('F14c', , f_Where, 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en LS y cambiar ubicación origen por
*> ubicación nueva.---------------------------------------------------
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F26lUbiOri, F26lFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Where = "F26lUbiOri='" + c_UbiSal          + "' And " + ;
          "F26lNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F26lCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F26lCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F26lNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F26lSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F26lFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F26lTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F26l', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Desbloquear movimientos en MP y cambiar ubicación origen por
*> ubicación nueva, y recalcular CanRes ocupación.--------------------
Select LSTCNF
c_Flag1  = ' '
f_Campos  = "F14cUbiOri, F14cFlag1"
f_Valores = "c_UbiEnt, c_Flag1"

f_Where = "F14cUbiOri='" + c_UbiSal          + "' And " + ;
          "F14cNumPal='" + LSTCNF.F26lNumPal + "' And " + ;
          "F14cCodPro='" + LSTCNF.F26lCodPro + "' And " + ;
          "F14cCodArt='" + LSTCNF.F26lCodArt + "' And " + ;
          "F14cNumLot='" + LSTCNF.F26lNumLot + "' And " + ;
          "F14cSitStk='" + LSTCNF.F26lSitStk + "' And " + ;
          "F14cFecCad=" + _GCD(LSTCNF.F26lFecCad) + " And " + ;
          "F14cTipMov Between '2000' And '2998'"

Sw = F3_UpdTun('F14c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar cantidad reservada en ocupación destino.
If !UpdCResUbic(LSTCNF.F26lCodPro, ;
                LSTCNF.F26lCodArt, ;
                LSTCNF.F26lUbiDes, ;
                LSTCNF.F26lNumPal, ;
                LSTCNF.F26lNumLot, ;
                LSTCNF.F26lSitStk)
   b_ErrMov = .T.
EndIf

*> Actualizar fecha y estado del movimiento en lista.-----------------
Sw = CnfL_ActLst(LSTCNF.F26lNumMov, LSTCNF.F26lNumLst)
If Sw = .F.
   b_ErrMov = .T.
EndIf

*> Actualizar flag reposición de la ocupación.
Sw = CnfL_ActOcu(LSTCNF.F26lUbiOri, ;
                 LSTCNF.F26lCodPro, ;
                 LSTCNF.F26lCodArt, ;
                 LSTCNF.F26lNumPal, ;
                 LSTCNF.F26lNumLot, ;
                 LSTCNF.F26lSitStk)
Return

*>======================================================================
*> CnfL_ActLst .............. Actualizar registro de listas
*>======================================================================
Function CnfL_ActLst
Parameters p_NumMov, p_NumLst
Private f_Campos, f_Valores, f_Where, b_Ok

v_FecMov = Date()
v_EstMov = "3"

f_Campos  = "F26lFecMov, F26lEstMov"
f_Valores = "v_FecMov, v_EstMov"
f_Where   = "F26lNumMov = '" + p_NumMov + "'"

b_Ok = F3_UpdTun('F26l', , f_Campos, f_Valores, , f_Where, 'N', 'N')
Return b_Ok

*>======================================================================
*> CnfL_ActMac .............. Actualizar cabecera de MAC
*>======================================================================
Function CnfL_ActMac
Parameters p_TipMac, p_NumMac
Private f_Campos, f_From, f_Where, lx_Sql, f_Valores, b_Ok, v_Estado

f_Campos = "Min(F26lEstMov) EstMov, Count(Distinct F26lEstMov) Cont"
f_From   = "F26l" + _em
f_Where  = "F26lNumMac='" + p_NumMac + "'"
lx_Sql   = "Select " + f_Campos + " From " + f_From + " Where " + f_Where

Err = SqlExec(_aSql, lx_Sql)
=SqlMoreResults(_asql)
If Err > 0
   b_Ok = .T.
Else
   b_Ok = .F.
EndIf

If b_Ok = .T.
   Select SqlResult
   Go Top
   If SqlResult.Cont <= 1 .And. SqlResult.EstMov='3' 
      v_Estado='3'
   Else
      v_Estado='2'
   EndIf

   f_Campos  = "F26vEstBul"
   f_Valores = "v_Estado"
   f_Where   = "F26vTipMac='" + p_TipMac + "' And F26vNumMac='" + p_NumMac + "'"
   b_Ok = F3_UpdTun('F26v', , f_Campos, f_Valores, , f_Where, 'N', 'N')
EndIf   
If Used('SqlResult')
   Use In SqlResult
EndIf
Return b_Ok

*>======================================================================
*> CnfL_ActLMac ............. Actualizar detalle de MAC
*>======================================================================
Function CnfL_ActLMac
Parameters p_TipMac, p_NumMac, p_NumMov
Private f_Campos, f_From, f_Where, lx_Sql, f_Valores, b_Ok, v_Estado

v_Estado = '3'
f_Campos  = "F26wEstLin"
f_Valores = "v_Estado"
f_Where   = "F26wNMovMP='" + p_NumMov + "'"
b_Ok = F3_UpdTun('F26w', , f_Campos, f_Valores, , f_Where, 'N', 'N')
Return b_Ok

*======================================================================
*> CnfL_ActOcu .............. Actualizar flag ocupaciones.
*>======================================================================
Function CnfL_ActOcu
Parameters CodUbi, CodPro, CodArt, NumPal, NumLot, SitStk
Private f_Campos, f_Valores, f_Where, b_Ok
Private cSelec, cWhere, lEstado, nReposiciones

Store 0 To nReposiciones

cSelec = GetCvtNvl(_ENTORNO, _VERSION, "Count(*)") + " As NumRepos"
cWhere = "F14cUbiOri='" + CodUbi + "' And " + _GCSS("F14cTipMov", 1, 1) + "='3'"
lEstado = f3_sql(cSelec, "F14c", cWhere, , , "NumReposCur")
If lEstado
   Select NumReposCur
   Go Top
   nReposiciones = NumRepos
EndIf

*> Marcar/desmarcar flag ocupación, según queden reposiciones o no.
f_Campos  = "F16cFlag1"
f_Valores = Iif(nReposiciones > 0, 'R', Space(1))

f_Where   = "F16cCodUbi='" + CodUbi + "' And " + ;
            "F16cCodPro='" + CodPro + "' And " + ;
            "F16cCodArt='" + CodArt + "' And " + ;
            "F16cNumPal='" + NumPal + "' And " + ;
            "F16cNumLot='" + NumLot + "' And " + ;
            "F16cSitStk='" + SitStk + "'"

b_Ok = F3_UpdTun('F16c', , f_Campos, 'f_Valores', , f_Where, 'N', 'N')
Return b_Ok

*>======================================================================
*> CnfL_CrgParm ............. Cargar parámetros de actualización
*>======================================================================
Procedure CnfL_CrgParm
Select LSTCNF
With FncA.ObjParm
   *> Datos Ubicaciones
   .PUbNew = Space(14)          && Ubicacion nueva
   .PUFlag = 'S'				&& Flag actualizar ubicación ('S' o 'N')

   *> Datos Ocupaciones
   .POTDoc = F26lTipDoc  		&& Tipo documento
   .PONDoc = F26lNumDoc  		&& Número documento

   .POLDoc = F26lLinDoc   		&& Línea documento
   .POTEnt = Space(4)           && Tipo entidad
   .POCEnt = Space(6)           && Código entidad
   .PONPed = F26lNumPed   		&& Número pedido
   .POLPed = F26lLinPed   		&& Línea pedido
   .POFMov = F26lFecMov 		&& Fecha movimiento

   .POCPro = F26lCodPro   		&& Código propietario
   .POCArt = F26lCodArt   		&& Código artículo
   .PONLot = F26lNumLot   		&& Número lote
   .POSStk = F26lSitStk   		&& Situación stock
   .POFCad = F26lFecCad  		&& Fecha caducidad

   .POCFis = F26lCanFis   		&& Cantidad física
   .POCAlm = _Alma       		&& Código almacén
   .PONPal = F26lNumPal   		&& Número palet
   .POTPal = F26lTipPal   		&& Tipo palet

   .POFUni = F26lUniVen        	&& Factor unidad (unidades venta)
   .POFSer = F26lUniPac        	&& Factor servicio (unidades / pack)
   .POFEnv = F26lPacCaj        	&& Factor envase (packs / cajas)
   .POFPal = F26lCajPal        	&& Factor palet (cajas / palet)
   .POFFab = F26lFecFab         && Fecha fabricación
   .PONEnt = F26lNumEnt        	&& Número entrada
   .POFEnt = Date()             && Fecha entrada
   .POFCal = F26lFecCal      	&& Fecha calidad
   .PONAna = F26lNumAna        	&& Número análisis

   .POPico = 'N'
   If .POFUni * .POFSer * .POFEnv * .POFPal > F26lCanFis  
      .POPico='S'				&&Pico S/N
   EndIf

   .POCrd1 = F26lFlag1			&& Flag 1 ocupación
   .POCrd2 = F26lFlag2			&& Flag 2 ocupación
   .POFlag = 'S'                && Flag actualizar ocupación ('S' o 'N')

   *> Datos Situaciones Stock
   .PSFlag = 'N'                &&Flag actualizar situación stock ('S' o 'N')

   *> Datos Movimientos
   .PMFDoc = F26lFecDoc  		&&Fecha documento
   .PMRHab = F26lRutHab  		&&Ruta habitual
   .PMCOpe = F26lCodOpe  		&&Código operario
   .PMNLst = F26lNumLst  		&&Número lista
   .PMNExp = F26lNumExp  		&&Número expedición
   .PMORec = F26lOrdRec  		&&Orden recorrido
   .PMStat = F26lEstMov  		&&Estado movimiento
   .PMORes = F26lOriRes  		&&Origen reserva
   .PMTUbi = F26lTipUbi			&&Tipo ubicación
   .PMTMAs = F26lTipMAs			&&Tipo MAC asociado
   .PMNMAs = F26lNumMAs			&&Número MAC asociado
   .PMMUni = F26lMacUni  		&&MAC unido
   .PMORut = F26lOrdRut  		&&Orden ruta
   .PMTERe = F26lTEntRe  		&&Tipo entidad reexpedición
   .PMCERe = F26lCEntRe  		&&Código entidad reexpedición
   .PMVHab = F26lVenHab  		&&Vendedor habitual
   .PMTMac = F26lTipMac  		&&Tipo MAC
   .PMNMac = F26lNumMac  		&&Número MAC
   .PMSecc = F26lSeccio  		&&Sección

   .PMFgMP='N'                  && Flag actualizar movimiento pendiente ('S' o 'N')

   *> Más campos (para histórico de movimientos)
   .PMDAso = F26lNumMov   		&& Dirección asociada
   .PMDAso = F26lDirAso   		&& Dirección asociada
   .PMFEnv = _FecMin            && Fecha envío
   .PMFOrd = _FecMin     		&& Fecha orden
   .PMFgHM = 'S' && Flag actualizar histórico de movimientos ('S' o 'N')
EndWith
Return

*>---------------------------------------------------
*> Actualizar cantidad reservada en la ocupación.
*>---------------------------------------------------
Function UpdCResUbic
Parameters _CodPro, _CodArt, _CodUbi, _NumPal, _NumLot, _SitStk
Private _CanRes

*> Acumula la cantidad reservada en la ocupación, según MPs.
_CanRes = CResF14c(_CodPro, _CodArt, _CodUbi, _NumPal, _NumLot, _SitStk)

f_Campos  = "F16cCanRes"
f_Valores = "_CanRes"
f_Where   = "F16cCodPro='" + _CodPro + "' And " + ;
            "F16cCodArt='" + _CodArt + "' And " + ;
            "F16cCodUbi='" + _CodUbi + "' And " + ;
            "F16cNumLot='" + _NumLot + "' And " + ;
            "F16cSitStk='" + _SitStk + "'"

*            "F16cNumPal='" + _NumPal + "' And " + ;

Return F3_UpdTun('F16c', , f_Campos, f_Valores, , f_Where, 'N', 'N')
