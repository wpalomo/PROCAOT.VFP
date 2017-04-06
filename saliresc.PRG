*>
***> Función de cancelación de reserva a partir de un movimiento.-----
*> Actualizar flag enlace estado pedidos de SAP. AVC - 19.10.2000
Function DesReserva
Parameters c_NumMov

Private f_Select, f_Where, b_Err, n_Reg, _Ok, FncA, PrmA
Private c_FlgEst, c_Flag1, f_Where, f_Asign
Private c_CodPro, c_CodArt, c_UbiOri, c_NumPal, c_NumLot, c_SitStk, c_NumLst
Private c_TipDoc, c_NumDoc, c_CodAlm, c_LinDoc
Private n_LinDoc, n_CanMov, d_FecCad, n_CanFis, n_CanRes

   ****> Borrar movimiento del F14c.----------------------------------
   *> Comprobar si existe F14c.---------------------------------------
   Store " " To c_CodPro, c_CodArt, c_UbiOri, c_NumPal, c_NumLot, c_SitStk, c_NumLst, ;
                c_TipDoc, c_NumDoc, c_CodAlm, c_LinDoc
   Store 0   To n_LinDoc, n_CanMov
   Store _FecMin To d_FecCad

   f_Where = "F14cNumMov='" + c_NumMov + "'"
   f_Asign = "c_CodPro=F14cCodPro, c_CodArt=F14cCodArt, c_UbiOri=F14cUbiOri, " + ;
      "c_NumPal=F14cNumPal, c_NumLot=F14cNumLot, d_FecCad=F14cFecCad, c_SitStk=F14cSitStk, " + ;
      "c_NumLst=F14cNumLst, c_TipDoc=F14cTipDoc, c_NumDoc=F14cNumDoc, c_CodAlm=F14cCodAlm, " + ;
      "n_LinDoc=F14cLinDoc, n_CanMov=F14cCanFis"

   _Ok = F3_SeekTun('F14c', f_Where, , f_Asign)
   If !_Ok
      _lxerr = _lxerr + "No se encontró Movimiento [" + c_NumMov + "] en F14c" + Cr + ;
                        "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf  

   c_LinDoc = Right(AllTrim(Str(10000 + n_LinDoc)), 4)

   *> Borrar el Movimiento en el F14c.--------------------------------
   f_Where = "F14cNumMov='" + c_NumMov + "'"
   _Ok = F3_DelTun('F14c', , f_Where, 'N')
   If !_Ok
      _lxerr=_lxerr + "No se pudo borrar Movimiento [" + c_NumMov + "] en F14c" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F. 
   EndIf  

   ****> Desreservar cantidad movimiento en F16c.---------------------
   *> Comprobar si existe ocupación.----------------------------------
   Store 0 To n_CanFis, n_CanRes
   f_Where  = "F16cCodPro='" + c_CodPro + "' And F16cCodArt='" + c_CodArt + "'" + ;
         " And F16cCodUbi='" + c_UbiOri + "' And F16cNumPal='" + c_NumPal + "'" + ;
         " And F16cNumLot='" + c_NumLot + "' And F16cSitStk='" + c_SitStk + "'" + ;
         " And F16cFecCad=" + _GCD(d_FecCad)
   _Ok = F3_SeekTun('F16c', f_Where, , 'n_CanFis=F16cCanFis, n_CanRes=F16cCanRes')
   If !_Ok
      _lxerr=_lxerr + "No se encontró ocupación del movimiento [" + c_NumMov + "] en F16c" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf

   *> Restar cantidad reservada de F16c.------------------------------
   n_CanRes = IIf(n_CanRes - n_CanMov >= 0, n_CanRes - n_CanMov, 0)
   f_Where  = "F16cCodPro='" + c_CodPro + "' And F16cCodArt='" + c_CodArt + "'" + ;
         " And F16cCodUbi='" + c_UbiOri + "' And F16cNumPal='" + c_NumPal + "'" + ;
         " And F16cNumLot='" + c_NumLot + "' And F16cSitStk='" + c_SitStk + "'" + ;
         " And F16cFecCad=" + _GCD(d_FecCad)
   _Ok = F3_UpdTun('F16c', , 'F16cCanRes', 'n_CanRes', , f_Where, 'N', 'N')
   If !_Ok
      _lxerr=_lxerr + "No se pudo desreservar ocupación" + Cr + ;
                      "del movimiento ["+ c_NumMov +"] en F16c" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf  

   ****> Si existe, eliminar movimiento del F26l.---------------------
   *> Calcula número de movtos. de la Lista del mov.tratado.----------
   f_Select = "Select Count(*) Cont  From F26l" + _em
   f_Where  = " Where F26lNumLst='" + c_NumLst + "'" 
   Cont = SqlExec(_aSql, f_Select + f_Where) 
   If Cont >= 0
      =SqlMoreResults(_aSql)
      Cont = SqlResult.Cont
      Use In SqlResult
   Else
      _lxerr=_lxerr + "Error en cálculo de movimientos de la lista" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf

   *> Borra movimiento del F26l.--------------------------------------
   f_Where  = "F26lNumLst='" + c_NumLst + "'" 
   _Ok = F3_DelTun('F26l', , f_Where, 'N')
   If !_Ok  .And.  !Empty(c_NumLst)
      _lxerr=_lxerr + "No se pudo borrar movimiento [" + c_NumMov + "] en F26l" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf  

   If !Empty(c_NumLst)
      *> Actualizar cabecera de lista en F26c.---------------------
      _Ok = Ora_AcCbLs(c_NumLst, 'N')
      If !_Ok
         _lxerr=_lxerr + "No se pudo actualizar la cabecera de lista" + Cr + ;
               "para el Movimiento [" + c_NumMov + "] en F26c" + Cr + ;
               "MENSAJE: " + Message() + cr + cr
         Return .F.
      EndIf
   EndIf
     
   ****> Actualiza Estado y Cant.Reservada en línea documento (F24l).----
   *> Comprobar si existe línea de documento.-------------------------
   n_CanRes = 0
   c_FlgEst = Space(1)
   f_Where  = "F24lCodPro='" + c_CodPro + "' And F24lTipDoc='" + c_TipDoc + "'" + ;
         " And F24lNumDoc='" + c_NumDoc + "' And F24lLinDoc='" + c_LinDoc + "'"
   _Ok = F3_SeekTun('F24l', f_Where, , 'n_CanRes=F24lCanRes, c_FlgEst=F24lFlgEst')
   If !_Ok
      _lxerr=_lxerr + "No existe la línea del documento" + Cr + ;
                      "para el movimiento [" + c_NumMov + "] en F24l" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf       
           
   *> Restar cant.reservada de línea documento.-----------------------
   n_CanRes = IIf(n_CanRes - n_CanMov >= 0, n_CanRes - n_CanMov, 0)
   c_FlgEst = IIf(n_CanRes = 0, '0', c_FlgEst)
   f_Where  = "F24lCodPro='" + c_CodPro + "' And F24lTipdoc='" + c_TipDoc + "'" + ;
         " And F24lNumDoc='" + c_NumDoc + "' And F24lLinDoc='" + c_LinDoc + "'"
   _Ok = F3_UpdTun('F24l', , 'F24lCanRes, F24lFlgEst', 'n_CanRes, c_FlgEst', , f_Where, 'N', 'N')
   If !_Ok
      _lxerr=_lxerr + "No se pudo actualizar la línea del documento" + Cr + ;
                      "para el movimiento [" + c_NumMov + "] en F24l" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf  

   ****> Actualiza Estado de cabecera de documento (F24c).------------
   *> Calcular cantidades totales del documento.----------------------
   f_Select = "F24cPolSer, F24cFlag1, Sum(F24lCanDoc) As CanDoc, Sum(F24lCanRes + F24lCanEnv) As CanSer"
   f_Where  = "F24lCodPro='" + c_CodPro + "' And F24lTipDoc='" + c_TipDoc + "'" + ;
         " And F24lNumDoc='" + c_NumDoc + "' And F24cCodPro=F24lCodPro" + ;
         " And F24cTipDoc=F24lTipDoc And F24cNumDoc=F24lNumDoc"
   f_Group  = "F24lCodPro, F24lTipDoc, F24lNumDoc, F24cPolSer, F24cFlag1"
  
   _Ok = f3_sql(f_Select, 'F24l,F24c', f_Where, , f_Group, 'RESDOC')
   If !_Ok
      _lxerr=_lxerr + "No existen líneas para del documento" + Cr + ;
                      "para el movimiento [" + c_NumMov + "] en F24l" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf       
           
   *> Actualizar estado cabecera según cantidades y política.---------
   Select ResDoc
   If ResDoc.CanDoc <= ResDoc.CanSer
      c_FlgEst = '3'
      c_Flag1  = ResDoc.F24cFlag1
   Else
      If ResDoc.CanSer > 0
         c_FlgEst = '2'
         c_Flag1  = ResDoc.F24cFlag1
      Else
         If ResDoc.F24cPolSer = '2'
            c_FlgEst = '0'
            c_Flag1  = '0'
         Else
            c_FlgEst = '0'
            c_Flag1  = '0'
         EndIf
      EndIf
   EndIf
   
   f_Where  = "F24cCodPro='" + c_CodPro + "' And F24cTipDoc='" + c_TipDoc + "'" + ;
         " And F24cNumDoc='" + c_NumDoc + "'"
   _Ok = F3_UpdTun('F24c', , 'F24cFlgEst,F24cFlag1', 'c_FlgEst,c_Flag1', , f_Where, 'N', 'N')
   If !_Ok
      _lxerr=_lxerr + "No se pudo actualizar la línea del documento" + Cr + ;
                      "para el movimiento [" + c_NumMov + "] en F24l" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F.
   EndIf  

   ****> Actualizar la situación de stock.----------------------------
   FncA = CreateObject("OraFncActz")
   PrmA = CreateObject("OraPrmActz")
   PrmA.Inicializar
   FncA.ObjParm = PrmA
   
   *> Resta de Reservada por Mvto. actual.----------------------------
   FncA.ObjParm.PTAcc  = "08"
   FncA.ObjParm.PSCPro = c_CodPro      && CanRes.F14cCodPro
   FncA.ObjParm.PSCArt = c_CodArt      && CanRes.F14cCodArt
   FncA.ObjParm.PSCAlm = _Alma
   FncA.ObjParm.PStOld = "2000"
   FncA.ObjParm.PStNew = Space(4)
   FncA.ObjParm.PSCFis = n_CanMov      && CanRes.F14cCanFis
   FncA.ObjParm.PUFlag = "N"
   FncA.ObjParm.POFlag = "N"
   FncA.ObjParm.PSFlag = "S"
   FncA.Ejecutar
   _Ok = IIf(FncA.ObjParm.PWCRtn < "50", .T., .F.)
   If !_Ok
      _lxerr=_lxerr + "No se pudo actualizar el stock reservado" + Cr + ;
                      "para el movimiento [" + c_NumMov + "] en F13c" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F. 
   EndIf
   
   *> Suma a Stock MP la cantidad del movimiento.---------------------
   FncA.ObjParm.PTAcc  = "07"
   FncA.ObjParm.PSCPro = c_CodPro           && CanRes.F14cCodPro
   FncA.ObjParm.PSCArt = c_CodArt           && CanRes.F14cCodArt
   FncA.ObjParm.PSCAlm = _Alma
   FncA.ObjParm.PStOld = c_SitStk           && CanRes.F14cSitStk
   FncA.ObjParm.PStNew = Space(4)
   FncA.ObjParm.PSCFis = n_CanMov           && CanRes.F14cCanFis
   FncA.ObjParm.PUFlag = "N"
   FncA.ObjParm.POFlag = "N"
   FncA.ObjParm.PSFlag = "S"
   FncA.Ejecutar
   _Ok = IIf(FncA.ObjParm.PWCRtn < "50", .T., .F.)
   If !_Ok
      _lxerr=_lxerr + "No se pudo actualizar el stock físico" + Cr + ;
                      "para el movimiento [" + c_NumMov + "] en F13c" + Cr + ;
                      "MENSAJE: " + Message() + cr + cr
      Return .F. 
   EndIf
   Release FncA
   Release PrmA
Return .T.

*>
*> Cancelar los MPs de preparación de un documento completo.
*> Recibe: CodPro ----> Propietario.
*>         TipDoc ----> Tipo de documento.
*>         NumDoc ----> Nº de documento.
*>         LinDoc ----> Línea documento. Si .F., cancelar TODO el documento.
*>
Function CancelarReserva
Parameters CodPro, TipDoc, NumDoc, LinDoc
Local _Where

_Where = "F14cCodPro='" + CodPro + "' And F14cTipDoc='" + TipDoc + "' And F14cNumDoc='" + NumDoc + "' "
If Type('LinDoc')=='C'
   _Where = _Where + " And F14cLinDoc=" + LinDoc 
EndIf

=f3_sql('*', 'F14C', _Where, '', '', 'F14cCur')
Select F14cCur
Go Top
Do While !Eof()
   Do DesReserva With F14cNumMov
   Select F14cCur
   Skip
EndDo

*>
Return
