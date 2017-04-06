*> GENERAR FACTURAS MANIPULACIÓN - "FacGenM.prg"

*****	Procedimiento Principal => GenFacM 
****    Grabar F70 &&&&& Uno para cada letra

*****   FormatFecha
*****	Agregar_Error => Gestiona si se debe meter un nuevo error en _lxerr y lo mete si se tercia

************************************************************************************************
*> Calcular IVA a partir de datos de F37L. AVC _ 13.05.1999

Procedure FormatFecha
Public AnyoMes

   ** Saco dos formatos de fechas a necesarios en la creación de dos cursores
   		** Esto es para que salga el día 1 del mes siguiente
    mes=mefac+1
	if left(str(mefac,2),1)=" " then
			mefac="0"+alltrim(str(mefac))
		else
			mefac=alltrim(str(mefac))
		endif	
		anfac=alltrim(str(anfac))
		fechastring=anfac+mefac+"0"
		if mes=13 then
			mes=1
			ano=ano+1
		endif
		ano=anfac
		mes=str(mes,2)
		if left(mes,1)=" " then
			mes="0" + right(mes,1)
		endif	
		fecha='01/'+ Mes+'/'+ Ano		
        AnyoMes = AnFac + Mefac       

Return        

Procedure CrearF37CLGRUPS
Public F37clGrupS

   ** Cursores que contiene la relacion de datos
        Lx_Sele = "Select F37lCodPro, F37lDescri, F37lCodCon, F37lCodSrv, F37lCodUni, F37lCodImp, " + ;
                  "Sum(F37LCantid) As Cantidad, " + ;
                  "Sum(F37LPreUni) As Precio " + ;
                  "From F37C" + _em + ",F37L" + _em + ;
		          " Where F37cCodPro='" + CodPro + "' And " + ;
				  "       F37cCodPro = F37lCodPro  And " + ;
				  "       F37cHojTra = F37lHojTra  And " + ;					
                  "       Upper(F37cFactSN) <> 'S'       And " + ;					
                  "       Upper(F37lFactSN) = 'S'        And " + ;					                  
                  "       Upper(F37lGrupSN) = 'S'        And " + ;					
				  "	To_Char(F37cFecTra, 'YYYYMM') = '" + AnyoMes + _cm + ;
		          " Group By F37lCodPro, F37lDescri,F37lCodCon, F37lCodSrv, F37lCodUni, F37lCodImp"
		          
        Err = SqlExec (_aSql,lx_Sele, 'F37clGrupS')
        If Err < 0 then
           _Lxerr = LxErr + Message() + cr
		   Return
		EndIf		
        =SqlMoreResults(_aSql)
        

   ** Cursores que contiene la relacion de datos
        Lx_Sele = " Select F37lCodPro, F37lDescri, F37lCodCon, F37lCodSrv, F37lCodUni, F37LCantid As Cantidad, " + ;
                  " F37LPreUni As Precio" + ;
                  " From F37C" + _em + ",F37L" + _em + ;
		          " Where F37cCodPro='" + CodPro + "' And " + ;
				  "       F37cCodPro = F37lCodPro     And " + ;
				  "       F37cHojTra = F37lHojTra     And " + ;					
                  "       Upper(F37cFactSN) <> 'S'    And " + ;					
                  "       Upper(F37lFactSN) = 'S'     And " + ;					                  
                  "       Upper(F37lGrupSN)  = 'N'    And " + ;					
				  "	To_Char(F37cFecTra, 'YYYYMM') = '" + AnyoMes + _cm  
		          
        Err = SqlExec (_aSql,lx_Sele, 'F37clGrupN')
        If Err < 0 then
           _Lxerr = LxErr + Message() + cr
		   Return		   
		EndIf		
        =SqlMoreResults(_aSql)
        Sele F37CLGRUPS
        Append From dbf('F37CLGRUPN')
        Use in F37clGrupN

Return        

Procedure GrabarF70c

        Wait Clear
        Wait Window 'Guardando Cabecera de Factura' NoWait
               
        = CrtCursor ('F70c','F70CTEMP')
     	Select F70CTEMP
		Append blank
        Replace F70cCodPro with CodPro, ;
              F70cAnyFac with AnFac, ;
              F70cMesFac with MeFac, ;
			  F70cNumFac with NumFac, ;
			  F70cFecFac With Date()
				
   		 Err = F3_InsTun("F70c","F70CTEMP",'N')
   		        
		 If err= .F. Then
			=SqlRollBack(_aSql)
			_ok = Agregar_Error(0,"No se ha podido guardar la información de la cabecera de factura")			       
			If !_ok then
                ErrGral = .T.			
    			Return
			EndIf
		EndIf
		Use in F70CTEMP
Return		 

Function ControlConceptos

         Sele F37clGrupS

         m.F34nCodCon = F37lCodCon
         Ok = F3_Seek('F34N')
         If !Ok then
            =f3_sn(1,1,"No existe el concepto " + F37lCodCon + " .Revise el dato")              	  
         Return .F.                        
         Endif		              
         
         m.F34sCodCon = F37lCodSrv
         Ok = F3_Seek('F34S')
         If !Ok then
        	=f3_sn(1,1,"No existe el concepto " + F37lCodSrv + " .Revise el dato")              	  
         Return .F.        	
         Endif		              
         
         m.F34uCodCon = F37lCodUni
         Ok = F3_Seek('F34U')
         If !Ok then
        	 =f3_sn(1,1,"No existe el concepto " + F37lCodUni + " .Revise el dato")              	  
         Return .F.  	 
         Endif		                       
         
         Return .T.
Return         

Procedure GrabarF70l

        ErrGral = .F.
        Sele F37CLGRUPS
        Go Top
        Do While not Eof()
           Ok = ControlConceptos()
           If !Ok then
              ErrGral = .T.           
              Return
           EndIf   
    
        ** Generar las líneas de detalle de la factura.
           Wait Clear
           Wait Window 'Guardando Líneas de Factura' NoWait               
           
       **  Busco el Codigo de impuesto, iva y equivalencia
       	   Lx_Sele = "F34vValIva, F34vValEqv" 
       	   Lx_From = "F34V"
       	   Lx_Where = "F34vCodCon='" + F37lCodImp + "'"

       	   Ok = F3_Sql(Lx_Sele,Lx_From,Lx_Where,,,'Impu')
       	   
       	   if !Ok then
             _Lxerr = LxErr + Message() + cr       	                           
       	   	 return
       	   endif	 
           = CrtCursor ('F70L','F70LTEMP')       		 
    	   Select F70LTEMP		       
		   Append Blank
		   
		   Replace F70lCodPro With CodPro, F70lNumFac With NumFac, ;
			      F70lCodSrv With F37clGrupS.F37lCodSrv, ;
			      F70lCodCon With F37clGrupS.F37lCodCon, ;
	              F70lCodUni With F37clGrupS.F37lCodUni, ;
	              F70lDescri With F37clGrupS.F37lDescri, ;	              
	              F70lUniCal With F37clGrupS.Cantidad, ;
	              F70lUniCor With 0, ;
                  F70lPreUni With F37clGrupS.Precio, ;	              
                  F70lPreCor With 0, ;
                  F70lImplin With  F37clGrupS.Cantidad * F37clGrupS.Precio,;
                  F70lValDto With 0, ; 
                  F70lTotDto With (F70lImpLin * F70lValDto) / 100,;
                  F70lBasImp With F70lImpLin - F70lTotDto,;
                  F70lCodImp With F37clGrupS.F37lCodImp,;
                  F70lValIva With Impu.F34vValIva;
                  F70lTotIva With ((F70lBasImp * F70lValIva) /100), ;
				  F70lValEqv With Impu.F34vValEqv;                  
				  F70lTotEqv With ((F70lBasImp  * F70lValEqv) /100), ;
                  F70lTotLin With F70lBasImp + F70lTotIva + F70lTotEqv, ;
	              F70lTipLin With 'MANP'
	              
			Err = F3_InsTun("F70l","F70LTEMP",'N')
			If Err = .F. then
				= SqlRollBack(_aSql)
				_ok = Agregar_Error(0,"Error al guardar línea de detalle")
				If !_ok then					
			        ErrGral = .T.					
                    Return			        
				EndIf
			EndIf
			Sele F37clGrupS
		    Skip	
		EndDo                     
		Use in F70LTEMP		        
		Use in Impu
Return

*> Grabar información de las condiciones de pago de la factura.
Procedure GrabarF70p

        Wait Clear
        Wait Window 'Guardando Condiciones de Pago de Factura' NoWait
           
        ** Cargar datos de facturación del PROPIETARIO.
		Lx_Select = "*"
		Lx_From   = "F32C"
		Lx_Where  = "F32cCodPro='" + CodPro + _cm
		    
        m.F32cCodPro = CodPro
		Err = f3_seek('F32C')
           
        m.F70pCodPro = CodPro
        m.F70pNumFac = NumFac
		Err = f3_seek('F70P')

        *> Crear cursor con los datos del registro de condiciones de pago.
        If !Err
           Select F70P
		   Delete All
		   Append Blank
	       Replace F70pCodPro with CodPro,F70pNumFac with Numfac, ;
		           F70pDiapg1 with F32C.F32cDiapg1,F70pDiapg2 with F32C.F32cDiapg2, ;
				   F70pDiapg3 with F32C.F32cDiapg3,F70pDia1er with F32C.F32cDia1er, ;
			       F70pDiaiev with F32C.F32cDiaiev,F70pFecFra with F32C.F32cfecFra, ;
				   F70pNumRec with F32C.F32cNumRec,F70pDtoDc1 with F32C.F32cDtoDc1, ;
				   F70pDtoDc2 with F32C.F32cDtoDc2,F70pDtoDpp with F32C.F32cDtoDpp, ;
				   F70pCodFpg with F32C.F32cCodFpg,F70pDesFpg with F32C.F32cDesFpg, ;
				   F70pDomFpg with F32C.F32cDomFpg,F70pPobFpg with F32C.F32cPobFpg, ;
				   F70pCtaBan with F32C.F32cCtaBan,F70pNbaNco with F32C.F32cNbaNco
           Err = F3_InsTun("F70P",,'N')
        EndIf

		If Err=.F. then
		   = SqlRollBack(_ASql)
		   _ok = Agregar_Error(0,"No se ha podido guardar la información en el F70P")
		   If !_ok then
              ErrGral = .T.			 		   
              Return
		   Endif
		Endif
Return		

*> Grabar información de totales de factura.
Procedure GrabarF70i

         Wait Clear
         Wait Window 'Guardando Impuestos de Factura' NoWait

		 Lx_Sele = "F70lCodPro, F70lNumFac, F70lCodImp, F70lValIva, F70lValEqv, " + ;
		           "Sum(F70lBasImp) BASI, Sum(F70lTotIva) IIVA, Sum(f70lTotEqv) IEQV"
		 Lx_From  = "F70L"
		 Lx_Where = "F70lCodPro='" + CodPro + "' And " + ;
		           "F70lNumFac='" + NumFac + "'"
		 Lx_Group = "F70lCodPro, F70lNumFac, F70lValIva, F70lCodImp, F70lValEqv"
         Lx_Order=  "F70lCodPro, F70lNumFac, F70lValIva Desc"

         If Used('IMPTEMP')
            Use In IMPTEMP
         EndIf
         Err = F3_Sql(Lx_Sele, Lx_From, Lx_Where, Lx_Order, Lx_Group, "IMPTEMP")
		
      *> Buscarar registro de totales de factura.
         m.F70iCodPro = CodPro
         m.F70iNumFac = NumFac
         Err = f3_seek('F70I')

	** Creo registro en F70I.
		 Select F70I
         Delete All
         Append Blank
         Replace F70iCodPro With CodPro, ;
                F70iNumFac With NumFac

    ** Paso datos de IMPTEMP a cursor de 70I
         _NIvas = 0
		 Select IMPTEMP
		 Go Top
		 Do While !Eof()
              _NIvas = _NIvas + 1
              CIva = Iif(_NIvas < 5, Str(_NIvas, 1, 0), 'R')
			  Select F70i
              Do Case
                Case _NIvas < 5
                   Replace F70iBasIm&CIva With IMPTEMP.BASI, ;
                           F70iCodIm&CIva With IMPTEMP.F70lCodImp, ;
                           F70iValIv&CIva With IMPTEMP.F70lValIva, ;
                           F70iTotIv&CIva With IMPTEMP.IIVA, ;
                           F70iValEq&CIva With IMPTEMP.F70lValEqv, ;
                           F70iTotEq&CIva With IMPTEMP.IEQV
                 OtherWise
                    Replace F70iBasImR With F70iBasImR + IMPTEMP.BASI, ;
                           F70iTotIvR With F70iTotIvR + IMPTEMP.IIVA, ;
                           F70iTotEqR With F70iTotEqR + IMPTEMP.IEQV
              EndCase

              ** Leer el siguiente acumulado de IVA.
			  Select IMPTEMP
			  Skip
		EndDo

         *> Crear/Actualizar registro de totales de factura.
            If !Err
               Err = F3_InsTun("F70I", , 'N')
            Else
               _Where = "F70iCodPro='" + CodPro + "' And F70iNumFac='" + NumFac + "'"
               Err = F3_UpdTun("F70I", , , , , _Where, 'N', 'N')
            EndIf

		    If Err = .F. then
			   = SqlRollBack(_ASql)
			   _ok = Agregar_Error(0,"No se ha podido guardar la información en el F70I")
			   If !_ok then
			      ErrGral = .T.
				  Return
			   Endif
		    Endif

		Use in IMPTEMP 	       

Return

*> Marcar la cabecera de la Hoja de Trabajo.
Procedure GrabarF37C 

    Campos =  " F37CFECFAC,F37CFACTSN"
    Valores = " Date(),'S'" 
    Where =   " F37CCODPRO ='" + CodPro + _cm              
    
    Err = F3_Updtun ("F37C",,Campos,Valores,,Where)
    
Return
	

***** Procedimiento principal del módulo
PROCEDURE GenFacM
Parameters CodPro, AnFac, MeFac
Public ErrGral

   ErrGral = .F.
 
 **Fecha entera  
   Do FormatFecha In FacGenM 

 **Cursor principal con los datos de cada propietario     
   Do CrearF37CLGRUPS In FacGenM   
   
   If Eof('F37clGrupS') then
      _LxErr = _LxErr + 'PROPIETARIO: ' + CodPro + ' NO TIENE DATOS' + cr
      Return
   Endif   

   Public NumFac
   
 ** Guardar cabecera
   m.F70cCodPro = CodPro   
   
   Lx_Select = " Select * From F70c" + _em + ;
               " Where F70cCodPro = '" + CodPro + _cm +;
               " And F70cMesFac = '" + m.MeFac + _cm +;
               " And F70cAnyFac = '" + m.AnFac + _Cm
               
   Err = SqlExec(_ASql, Lx_Select, 'CBusca')
   Ok = Agregar_Error(Err,"Fallo al leer F70c")
   =SqlMoreResults(_aSql)
   If !Ok then
   		Return
   EndIf		  

	* Si ya existe
	If !Eof('CBusca') then
		* Si cancelar
		If m.cancelar <> 0 then
			wait Clear
			=SqlRollBack(_ASql)
			return
		else
			* Si añadir
			If m.anyadir <> 0 then
				NumFac = CBusca.F70cNumFac
			else
				* Si nueva
				If m.nueva <> 0 then    
					NumFac =Obtener_Num(.T.,CodPro,'NFRA')
					Do GrabarF70c In FacGenM 	    
				Else
					* Si sustituir
					If m.sustituir <> 0 then
						NumFac = CBusca.F70cNumFac
						** Borrar el detalle y los impuestos
						condicion = "F70lNumFac='"+NumFac+"' AND F70lCodPro='"+CodPro+_cm
						err = f3_DelTun("F70l", ,condicion,"N")
						condicion = "F70iNumFac='"+NumFac+"' AND F70iCodPro='"+CodPro+_cm
						err = f3_DelTun("F70i", ,condicion,"N")
					EndIf
				EndIf
			EndIf
		EndIf
    Else
		NumFac =Obtener_Num(.T.,CodPro,'NFRA')
		Do GrabarF70c In FacGenM
	EndIf
           
   If ErrGral = .T. Then
      Return
   EndIf             
   
 ** Guardar Detalle     	 
   Do GrabarF70l In FacGenM   
   If ErrGral = .T. Then
      Return
   EndIf          
   
 ** Guardar Condiciones de pago
 	** excepto que no sea para susituir la factura
   If m.sustituir = 0 then
	   Do GrabarF70p In FacGenM 
	   If ErrGral = .T. Then
    	  Return
	   EndIf             
   EndIf
   
 ** Guardar Impuestos  
   Do GrabarF70i In FacGenM           
   If ErrGral = .T. Then
      Return
   EndIf          
   
 ** Actualiza F37L
   Do GrabarF37c In FacGenM

 Wait Clear
 If empty(_lxerr) then
	=SqlCommit(_ASql)
 Else
	=SqlRollBack(_ASql)
	Do form st3inc
 Endif
 
RETURN

FUNCTION Agregar_Error
parameters valor_error,texto_error

	if valor_error < 1 then
		_lxerr =texto_error + cr
		do form st3inc
		RETURN .F.
	endif
	
RETURN .T.

