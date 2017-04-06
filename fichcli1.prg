***************	FICHA DE CLIENTES *****************
*Llena_F01p(Deriva de Actualiza)
*Llena_F32cDeriva de Actualiza)

*Vaciar_F01p
*Vaciar_F32c

*Actualiza_Textos Act_Text
********************************************

Procedure Llena_F01p
Parameters F01p

   Select F01p
   Scatter Memvar

   *> Para la Provincia   
   m.F00jCodPrv = F01pProvin
   =f3_seek('F00J')
   m.Nompro = F00jDescri       
       
Return
*******************************************************************
Procedure Llena_F32c
Parameters F32cp

*   Select F32c
*   Scatter MemVar

   *> Forma de pago.
   m.F34pCodCon = m.F32cCodFpg 
   =f3_seek("F34P")
   m.F32cDesFpg = SubStr(F34p.F34pDescri, 1, 30)
Return       
 
*******************************************************************
 
Procedure Vaciar_F01p

   Select F01p
   Scatter Memvar Blank
   m.NomPro = ""
       
Return

*******************************************************************

Procedure Vaciar_F32c
    
*   Select F32c
*   Scatter MemVar Blank

*   m.F32cFecFra = "F"
Return       
 
 
*******************************************************************
Procedure Act_Text

*> Registros de F01p	
   Lx_Select = " Select * " + ;
	           " From  F01p" + _em + ;   
	           " Where F01pCodigo  = '" + m.F32cCodPro + _cm
	            	          
    Err = SqlExec (_aSql,Lx_Select,'F01p')
    If Err < 0 
       Return
    EndIf    
    = SqlMoreResults(_aSql)
    =TableUpdate(.T.)
    
*> Registros de f32c	
   Lx_Select = " Select * " + ;
	           " From  F32c" + _em + ;   
	           "      ,F34p" + _em + ;
	           " Where F32cCodPro = '" + m.F32cCodPro + _cm
	            	          
    Err = SqlExec (_aSql,Lx_Select,'F32c')
    If Err < 0 
       Return
    EndIf    
    = SqlMoreResults(_aSql)    
    =TableUpdate(.T.)

 *> Llenar Los textos del formulario 
    If !Eof('F01p')     
     Do Llena_F01p In FichCLi1 With 'F01p'
    Else
     Do Vaciar_F01p In FichCli1      
    EndIf    
    
    If !Eof('F32c')     
     Do Llena_F32c In FichCLi1 With 'F32c'
    Else      
     Do Vaciar_F32c In FichCli1      
    EndIf   
     
   FcmFCli.St_Frame1.Page1.Refresh 
   FcmFCli.St_Frame1.Page2.Refresh 
   FcmFCli.St_Frame1.Page3.Refresh 	          
Return
