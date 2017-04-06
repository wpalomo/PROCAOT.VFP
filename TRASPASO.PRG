*> Procedures
*> Módulo Importaciones ......................... ImpArt  (Traspaso de Artículos)
*>                                                ImpCli  (Traspaso de Clientes)
*>                                                ImpPro  (Traspaso de Proveedores)
*>                    	                          ImpPed  (Traspaso de Pedidos)
*>                                                ImpCom  (Traspaso de Compras)
*>                                                ImpEst  (Traspaso de Estado Artículos)

*> Utilidades
*> Tratamiento inicidencias import/export         CrearIncidencias
*>                                                PrintIncidencias

*> Módulo Exportaciones.........................  ExpAlb  (Traspaso de Albaranes)
*>                                                ExpCFa  (Confirmación de Carga en BATCH)
*>                                                ExpStk  (Traspaso de Stock)
*>                                                ExpEsd  (Traspaso de Estado Documentos)
*>                                                ExpMvt  (Traspaso de Movimientos)
*>                                             ExpMvtExt  (Traspaso de Movimientos - Matching)
*>                                                ExpDvc  (Traspaso de Devoluciones Clientes)

*> Buzón de Ficheros                              Grabar_F40c

*> Agrupar ficheros en uno Sys(2000) ............ GroupFiles
*> Agrupar ficheros en uno ADir() ............... GroupFilesNew
*> Borrar grupo de ficheros ADir() .............. DeleteGroupFilesNew
*> Borrar grupo de ficheros Sys(2000) ........... DeleteGroupFiles

*> DEFINEs de MsgBox.
#DEFINE MB_OK                   0       && OK button only
#DEFINE MB_OKCANCEL             1       && OK and Cancel buttons
#DEFINE MB_ABORTRETRYIGNORE     2       && Abort, Retry, and Ignore buttons
#DEFINE MB_YESNOCANCEL          3       && Yes, No, and Cancel buttons
#DEFINE MB_YESNO                4       && Yes and No buttons
#DEFINE MB_RETRYCANCEL          5       && Retry and Cancel buttons

#DEFINE MB_ICONSTOP             16      && Critical message
#DEFINE MB_ICONQUESTION         32      && Warning query
#DEFINE MB_ICONEXCLAMATION      48      && Warning message
#DEFINE MB_ICONINFORMATION      64      && Information message

#DEFINE MB_APPLMODAL            0       && Application modal message box
#DEFINE MB_DEFBUTTON1           0       && First button is default
#DEFINE MB_DEFBUTTON2           256     && Second button is default
#DEFINE MB_DEFBUTTON3           512     && Third button is default
#DEFINE MB_SYSTEMMODAL          4096    && System Modal

*-- MsgBox return values
#DEFINE IDOK            1       && OK button pressed
#DEFINE IDCANCEL        2       && Cancel button pressed
#DEFINE IDABORT         3       && Abort button pressed
#DEFINE IDRETRY         4       && Retry button pressed
#DEFINE IDIGNORE        5       && Ignore button pressed
#DEFINE IDYES           6       && Yes button pressed
#DEFINE IDNO            7       && No button pressed

************************************************************************************************
**                                                                                            **
**                             P R O C E D I M I E N T O S                                    **
**                                                                                            **
************************************************************************************************

***********************************************************************************************
*> ------------------------------------------------------------------------------------------**
*>  						IMPORTACION DE ARTICULOS                                          *
*> Añadir equivalencia de familias. AVC - 07.04.1999                                          *
*> Añadir multilote/multiproducto. AVC - 13.04.1999                                           *
*> Redefinir factores de paletización. AVC - 23.02.2000                                       *
*> Flag explícito de control de calidad. AVC - 24.02.2000                                     *
*> Implantar tratamiento multi-fichero. AVC - 09.03.2000                                      *
*> Peso unitario viene en Kilos. AVC - 18.05.2000                                             *
*> Volumen unitario viene en litros. AVC - 18.05.2000                                         *
*> Asignar tipo producto=TipPro(5) a los que sean frío y estupefacientes. AVC - 31.08.2000    *
*> Actualizar campo 'tipo unidad'. AVC - 06.09.2000                                           *
*> Productos de frío son Multilote='N'. Resto Multilote='S'                                   *
*> ------------------------------------------------------------------------------------------**
***********************************************************************************************

Procedure ImpArt

Private FilImp, _CodArt, _TipPro, _TamAbi, _MulLot, _TipPal
Private Accion
Private _oldPoint, _oldSeparator

_oldPoint = Set('POINT')
_oldSeparator = Set('SEPARATOR')

DirArt = m.DirArt

Set Separator To '.'
Set Point To ','
Set Date  To DMY
Set Century On

*> Fichero de Artículos-----------------------------------------------------------------------
 FilImp = GroupFiles(AllTrim(DirArt))
 If !File(FilImp)
    _LxErr = "No se ha encontrado el fichero ASCII de importación de Artículos" + cr
    =Anomalias()
    Return
 EndIf

 *> Traspasar los datos del *.TXT a cursor de trabajo.
 _F08c = AllTrim(Sys(2015))
 Create Cursor (_F08c) ;
   (CodArt C(13), ;
    DesArt C(40), ;
    CodEan C(13), ;
    CodFam C(4), ;
    CodUni C(4), ;
    CtrCad C(1), ;
    DiaCad C(5), ;
    UniPac C(6), ;
    PacCaj C(6), ;
    CajPal C(6), ;
    VolUni C(10), ;
    PesUni C(10), ;
    CtrFri C(1), ;
    CtrDro C(1), ;
    CtrAmb C(1), ;
    CtrCal C(1), ;
    PCoste C(10))

 AF08c = Alias(_F08c)
 Select (AF08c)
 Append From &FilImp Type SDF


 *> Realizar la importación sobre fichero Artículos.------------------------------------------
 Select (AF08c)
 Go Top
 Do While !Eof()
    Wait Window  "Leyendo artículo: " +  + &AF08c->CodArt + "-" + Left(&AF08c->DesArt, 20) NoWait

    Selec F08c
    Zap

   *> Formar código de artículo. Si es numérico vienen rellenado con ceros
   *> a la izquierda: tomaremos los 6 últimos caracteres.
   Select (AF08c)
   _CodArt = CodArt

    *> Leer familia de tabla de equivalencias, F1PF.
    m.F1PFCodPro = m.CodPro
    m.F1PFFamDes = PadR(&AF08c->CodFam, 8)
    _ok = f3_seek('F1PF')
    If !_ok
       _CodFam = PadR(&AF08c->CodFam, 4)
    Else
       _CodFam = F1PF.F1PFCodFam
    Endif

   * Buscar el código en la tabla. Tomar propietario por defecto.
   m.F08cCodPro = CodPro
   m.F08cCodArt = _CodArt
   _ok = f3_seek('F08c')

   * Si ALTA.-----------------------------------------------------------------------------     
    Select F08c               
    If !_ok
       Append Blank
       Replace F08cCodPro With m.CodPro, ;
               F08cCodArt With _CodArt
       Accion='INSERT'
    Else
       Accion='UPDATE'
    EndIf  

    *> Comprobar si existe la familia.
    If !Empty(_CodFam)
    	m.F00gCodFam = _CodFam
	    _ok = f3_seek('F00G')
    	If (!_ok)
           Select F00g
           Delete All
           Append Blank
           Replace F00gCodFam With _CodFam, ;
                   F00gDescri With "Familia creada automaticamente"

		   err = F3_InsTun("F00g","F00g","N")
		   If err = .F.
		      _lxerr = _lxerr + "No se ha podido dar de alta una nueva familia"
		   Else
    	      Select Incidencias
              Append Blank
              Replace TipTras With 'Artículos'
              Replace Campo   With 'Familia'
              Replace Codigo  With &AF08c->CodArt
              Replace Descri  With 'Se ha dado de alta la familia ' + _CodFam + ' automáticamente'
		   EndIf
	   EndIf
    EndIf

    * Reemplaza datos de la tabla.----------------------------------------------------------
    Select F08c
    Replace F08cDescri With &AF08c->DesArt, ;
            F08cCodEan With &AF08c->CodEan, ;
	        F08cTipFam With _CodFam, ;
            F08cTipUni With &AF08c->CodUni

    *> Control de caducidad.
    If &AF08c->CtrCad == 'S'
       *> Hay caducidad, pero no se indican días de aviso.
       If Val(&AF08c->DiaCad) = 0
          Select Incidencias
          Append Blank
          Replace TipTras With 'Artículos', ;
                  Campo   With 'Caducidad', ;
                  Codigo  With &AF08c->CodArt, ;
                  Descri  With 'No se indican días de aviso'
          Replace &AF08c->DiaCad With '00001'
       EndIf
       Select F08c
 	   Replace F08cCaduca With &AF08c->CtrCad, ;
 	           F08cNumDia With Val(&AF08c->DiaCad)
 	Else
       *> No hay caducidad, pero se indican días de aviso.
       If Val(&AF08c->DiaCad) > 0
          Select Incidencias
          Append Blank
          Replace TipTras With 'Artículos', ;
                  Campo   With 'Caducidad', ;
                  Codigo  With &AF08c->CodArt, ;
                  Descri  With 'Se indican días de aviso'
       EndIf

       Select F08c
       Replace F08cCaduca With 'N', ;
               F08cNumDia With 0
    Endif    		  

    *> Control de calidad.
    Select F08c
    Replace F08cCalida With Iif(&AF08c->CtrCal=='S', 'S', 'N')
            
    *> Comprobar factores de paletización. Si error, asumir factores = 1.
    If Val(&AF08c->PacCaj) <= 0
       Select Incidencias
       Append Blank
       Replace TipTras With 'Artículos', ;
               Campo   With 'PacCaj', ;
               Codigo  With &AF08c->CodArt, ;
               Descri  With 'Factores paletización incorrectos'
       Replace &AF08c->PacCaj With Str(1, 6, 0)
    EndIf

    If Val(&AF08c->UniPac) <= 0
       Select Incidencias
       Append Blank
       Replace TipTras With 'Artículos', ;
               Campo   With 'UniPac', ;
               Codigo  With &AF08c->CodArt, ;
               Descri  With 'Factores paletización incorrectos'
       Replace &AF08c->UniPac With Str(1, 6, 0)
    EndIf

    If Val(&AF08c->CajPal) <= 0
       Select Incidencias
       Append Blank
       Replace TipTras With 'Artículos', ;
               Campo   With 'CajPal', ;
               Codigo  With &AF08c->CodArt, ;
               Descri  With 'Factores paletización incorrectos'
       Replace &AF08c->CajPal With Str(1, 6, 0)
    EndIf

    *> Factores de paletización.
    Select F08c
    Replace F08cUniVen With 1, ;
            F08cManPal With 1, ;
            F08cCajMan With 1, ;
            F08cUniPac With Iif(Val(&AF08c->UniPac) > 0, Val(&AF08c->UniPac), 1), ;
            F08cPacCaj With Iif(Val(&AF08c->PacCaj) > 0, Val(&AF08c->PacCaj), 1), ;
            F08cCajPal With Iif(Val(&AF08c->CajPal) > 0, Val(&AF08c->CajPal), 1), ;
            F08cVolUni With Iif(Val(&AF08c->VolUni) = 0, 1, Val(&AF08c->VolUni) / 10000.0) , ;
            F08cPesUni With Iif(Val(&AF08c->PesUni) = 0, 1, Val(&AF08c->PesUni) / 1000.0) 

    *> Seleccionar el tipo de producto.
    Do Case
       Case &AF08c->CtrDro == 'S' .And. &AF08c->CtrFri == 'S' && Psicotrópicos y Frío.
          _TipPro = TipPro(5)
          _TamAbi = TamAbi(2)
          _MulLot = 'S'
       Case &AF08c->CtrDro == 'S'                             && Psicotrópicos
          _TipPro = TipPro(2)
          _TamAbi = TamAbi(2)
          _MulLot = 'S'
       Case &AF08c->CtrFri == 'S'                             && Frío
          _TipPro = TipPro(1)
          _TamAbi = TamAbi(1)
          _MulLot = 'N'
       Case &AF08c->CtrAmb == 'S'                             && Ambiente
          _TipPro = TipPro(3)
          _TamAbi = TamAbi(3)
          _MulLot = 'N'
       OtherWise                                              && Ambiente (por defecto)
          _TipPro = TipPro(3)
          _TamAbi = TamAbi(3)
          _MulLot = 'N'
    EndCase

    *> Comprobar si existe el tipo de producto.
	m.F00eTipPro = _TipPro
	_ok = f3_seek('F00E')
	If (!_ok) .and. (!Empty(m.F00eTipPro))
		Select F00e
		Delete All
		Append Blank
		Replace F00eTipPro With _TipPro, ;
		        F00eDescri With 'Tipo de Producto creado automáticamente'

		err = F3_InsTun("F00e","F00e","N")
		If err = .F.
			_lxerr = _lxerr + "No se ha podido dar de alta un nuevo tipo de producto"
			=SqlRollBack(_ASql)
		Else
    	    Select Incidencias
            Append Blank
            Replace TipTras With 'Artículos', ;
                    Campo   With 'Tipo de Producto', ;
                    Codigo  With &AF08c->CodArt, ;
                    Descri  With 'Se ha dado de alta el tipo de producto '+_TipPro+' automáticamente'
		EndIf
	EndIf

    *> Comprobar si existe el tamaño de palet.
	m.F00fTamPal = _TamAbi
	_ok = f3_seek('F00F')
	If (!_ok) .and. (!Empty(m.F00fTamPal))
		Select F00f
		Delete All
		Append Blank
		Replace F00fTamPal With _TamAbi, ;
		        F00fDescri With 'Tamaño palet creado automáticamente', ;
		        F00fAbrevi With Space(1)

		err = F3_InsTun("F00f","F00f","N")
		If err = .F.
			_lxerr = _lxerr + "No se ha podido dar de alta un nuevo tamaño palet"
		Else
    	    Select Incidencias
            Append Blank
            Replace TipTras With 'Artículos', ;
                    Campo   With 'Tamaño Pal', ;
                    Codigo  With &AF08c->CodArt, ;
                    Descri  With 'Se ha dado de alta el tamaño palet ' + _TamAbi + ' automáticamente'
		EndIf
	EndIf

    *> Comprobar si existe el tamaño habitual de ubicación.
	_TipPal = _TamAbi
	m.F00mCodTam = _TipPal
	_ok = f3_seek('F00M')
	If (!_ok) .and. (!Empty(m.F00mCodTam))
		Select F00m
		Delete All
		Append Blank
		Replace F00mCodTam With _TipPal, ;
		        F00mDescri With 'Tamaño ubicación creado automáticamente', ;
		        F00mDimLar With 1, ;
		        F00mDimAlt With 1, ;
		        F00mDimAnc With 1, ;
		        F00mDimVol With 1

		err = F3_InsTun("F00m","F00m","N")
		If err = .F.
			_lxerr = _lxerr + "No se ha podido dar de alta un nuevo tamaño ubicación"
			=SqlRollBack(_ASql)
		Else
    	    Select Incidencias
            Append Blank
            Replace TipTras With 'Artículos', ;
                    Campo   With 'Tamaño Ubic', ;
                    Codigo  With &AF08c->CodArt, ;
                    Descri  With 'Se ha dado de alta el tamaño ubicación ' + _TipPal + ' automáticamente'
		EndIf
	EndIf

    Select F08c
    Replace F08cTipPro With _TipPro, ;
            F08cTamAbi With _TamAbi, ;
            F08cMulLot With _MulLot
             		  
    Replace F08cCoeRot With Iif(Type('_CoeRot') # 'U', _CoeRot, 0.00), ;
            F08cNumExt With Iif(Type('_NumExt') # 'U', _NumExt, 1), ;
            F08cTipAlm With Iif(Type('_TipAlm') # 'U', _TipAlm, 'C'), ;
            F08cTipPal With Iif(Type('_TipPal') # 'U', _TipPal, _TipPal), ;
            F08cTipEti With Iif(Type('_TipEti') # 'U', _TipEti, 'S'), ;
            F08cMulPro With Iif(Type('_MulPro') # 'U', _MulPro, 'N'), ;
            F08cNumSer With Iif(Type('_NumSer') # 'U', _NumSer, 'N')

	Replace F08cPCoste With Val(&AF08c->Pcoste) / 1000.0

    *> Comprobar si existe el tipo de unidad.
	m.F00hCodUni = PadR(&AF08c->CodUni, 4)
	_ok = f3_seek('F00H')
	If !_ok .And. !Empty(&AF08c->CodUni)
		Select F00h
		Delete All
		Append Blank
		Replace F00hCodUni With &AF08c->CodUni, ;
		        F00hDescri With 'Tipo de Unidad creado automáticamente'

		err = F3_InsTun("F00h", "F00h", "N")
		If err = .F.
			_lxerr = _lxerr + "No se ha podido dar de alta un nuevo tipo de unidad"
			=SqlRollBack(_ASql)
		Else
    	    Select Incidencias
            Append Blank
            Replace TipTras With 'Artículos', ;
                    Campo   With 'Tipo de Unidad', ;
                    Codigo  With &AF08c->CodUni, ;
                    Descri  With 'Se ha dado de alta el tipo de unidad ' + &AF08c->CodUni + ' automáticamente'
		EndIf
	EndIf

    *> Eliminar apóstrofos de la descripción.
    Select F08c
    Replace F08cDescri With StrTran(F08cDescri, "'", Space(1))

    *> Insertar/Actualizar el artículo.
    If Accion='INSERT'                    
       Wait Window  "Insertando artículo: " +  + &AF08c->CodArt + "-" + Left(&AF08c->DesArt, 20) NoWait
       _err=f3_InsTun("F08C", "F08C")
       If !_err
          _LxErr = "Error insertando artículo: " + &AF08c->CodArt + "-" + Left(&AF08c->DesArt, 20)
          =Anomalias()
          =SqlRollBack(_ASql)
          Return
       EndIf
    Else
      * Si MODIFICAR.-----------------------------------------------------------------------
       Wait Window  "Actualizando artículo: " +  + &AF08c->CodArt + "-" + Left(&AF08c->DesArt, 20) NoWait
       lx_Select = " Select * From F08c" + _em + ;
                   " Where F08cCodArt='" + _CodArt + _cm
       _err = f3_SqlExec(_ASql, lx_Select,'Lx_Inci')
       If _Err < 0
          _LxErr = "Error insertando artículo: " + &AF08c->CodArt + "-" + Left(&AF08c->DesArt, 20)
          =Anomalias()
          Return
       EndIf
       
       _err=f3_UpdTun("F08c",,,,,"F08cCodPro='" + CodPro + "' And F08cCodArt='" + F08c.F08cCodArt+ _cm)
       If !_err
          _LxErr = "Error actualizando artículo: " + &AF08c->CodArt + "-" + Left(&AF08c->DesArt, 20)
          =Anomalias()
          =SqlRollBack(_ASql)
          Return
       EndIf
               
    EndIf *bucle de ins/upd                

    *>
    Select(AF08c)
    Skip
EndDo *bucle de artículo

=SqlCommit(_ASql)

*>
Use In (AF08c)
Delete File(FilImp)
If MessageBox("Borrar archivo importación ARTICULOS", 4 + 32 + 0) = 6
   DeleteGroupFiles(AllTrim(DirArt))
EndIf

Set Separator To (_oldSeparator)
Set Point To (_oldPoint)
Return

***********************************************************************************************
*> -------------------------------------------------------------------------------------------*
*>                            IMPORTACION DE CLIENTES                                         *
*> Implantar tratamiento multi-fichero. AVC - 09.03.2000                                      *
*> Añadir el NIF del cliente. AVC - 04.04.2000                                                *
*> Ampliar campos de dirección. AVC - 22.09.2000                                              *
*> -------------------------------------------------------------------------------------------*
***********************************************************************************************

Procedure ImpCli

Private FilImp
DirCli = m.DirCli

Set Separator To '.'
Set Point To ','
Set Date  To DMY
Set Century On

   *> Fichero de Clientes-------------------------------------------------------------------
   FilImp = GroupFiles(AllTrim(DirCli))
   If !File(FilImp)
      _LxErr = "No se ha encontrado el fichero ASCII de importación de Clientes"
      =Anomalias()
      Return
    EndIf

   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F22p = AllTrim(Sys(2015))
   Create Cursor (_F22p) ;
     (CodCli C(13), ;
      NomCli C(35), ;
      NomCl2 C(35), ;
      NomCl3 C(35), ;
      NomCl4 C(35), ;
      NIFCli C(15), ;
      DirCli C(35), ;
      PobCli C(35), ;
      PrvCli C(35), ;
      CodPos C(10), ;
      NumTel C(15), ;
      NumFax C(15), ;
      CodPai C(3))

   AF22p = Alias(_F22p)
   Select (AF22p)
   Append From &FilImp Type SDF
  
   *> Realizar la importación sobre fichero Clientes.---------------------
   Go Top
   Do While !Eof()
      Wait Window  "Leyendo cliente: " +  + &AF22p->CodCli + "-" + Left(&AF22p->NomCli, 20) NoWait

      Select F22p
      Zap
           
      * Buscar el código de cliente en la tabla.
      m.F22pCodPro = CodPro
      m.F22pDirAso = &AF22p->CodCli
      _ok = f3_seek('F22P')

      * Si alta.--------------------------------------------------------------     
      Select F22p
      If !_Ok
         Append Blank
         Replace F22pCodPro With CodPro
         Replace F22pDirAso With &AF22p->CodCli
         Replace F22pFlete  With Iif(Type('_Flete') # 'U', _Flete, 'S')
         Replace F22pExtrad With Iif(Type('_Extra') # 'U', _Extra, 'N')
         Accion='INSERT'
      Else
         Accion='UPDATE'
      EndIf  
            
      * Reemplaza datos de la tabla.------------------------------------------------
      Replace F22pNomAso With &AF22p->NomCli, ;
              F22pCodNif With &AF22p->NIFCli, ;
              F22p1erDir With &AF22p->NomCl2, ;
              F22p2ndDir With &AF22p->DirCli, ;
              F22p3rdDir With &AF22p->NomCl3, ;
              F22pDPoBla With &AF22p->PobCli, ;
              F22pCProvi With PadL(Left(&AF22p->CodPos, 2), 4, '0'), ;
              F22pDProvi With &AF22p->PrvCli, ;
              F22pCodPos With &AF22p->CodPos, ;
              F22pCodPas With &AF22p->CodPai, ;
              F22pTelefo With &AF22p->NumTel, ;
              F22pNumfax With &AF22p->NumFax

      *> Eliminar apóstrofos de la descripción.
      Replace F22pNomAso With StrTran(F22pNomAso, "'", Space(1)), ;
              F22p1erDir With StrTran(F22p1erDir, "'", Space(1)), ;
              F22p2ndDir With StrTran(F22p2ndDir, "'", Space(1)), ;
              F22p3rdDir With StrTran(F22p3rdDir, "'", Space(1)), ;
              F22pDPobla With StrTran(F22pDPobla, "'", Space(1)), ;
              F22pDProvi With StrTran(F22pDProvi, "'", Space(1))

      If Accion='INSERT'                    
         Wait Window  "Insertando cliente: " +  + &AF22p->CodCli + "-" + Left(&AF22p->NomCli, 20) NoWait
         _Err=f3_InsTun("F22P", "F22P")
         If !_Err
            _LxErr = "Error insertando cliente: " +  + &AF22p->CodCli + "-" + Left(&AF22p->NomCli, 20)
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      Else
         Wait Window  "Actualizando cliente: " +  + &AF22p->CodCli + "-" + Left(&AF22p->NomCli, 20) NoWait
         _Err=f3_UpdTun("F22P",,,,,"F22pCodPro='" + CodPro + "' And F22pDirAso='" + F22p.F22pDirAso + _cm)
         If !_Err
            _LxErr = "Error actualizando cliente: " +  + &AF22p->CodCli + "-" + Left(&AF22p->NomCli, 20)
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf 

      *>
      Select (AF22p)
      Skip             
  EndDo *bucle de líneas   

*>
Use In (AF22p)
Delete File(FilImp)
If MessageBox("Borrar archivo importación CLIENTES", 4 + 32 + 0) = 6
   DeleteGroupFiles(AllTrim(DirCli))
EndIf

Return

***********************************************************************************************
*> -------------------------------------------------------------------------------------------*
*>                             IMPORTACION DE PROVEEDORES                                     *
*> Implantar tratamiento multi-fichero. AVC - 09.03.2000                                      *
*> -------------------------------------------------------------------------------------------*
***********************************************************************************************

Procedure ImpPro

Private FilImp
DirPro = m.DirPro

Set Separator To '.'
Set Point To ','
Set Date  To DMY
Set Century On

   *> Fichero de Proveedores-----------------------------------------------------------------
   FilImp = GroupFiles(AllTrim(DirPro))
   If !File(FilImp)
      _LxErr = "No se ha encontrado el fichero ASCII de importación de Proveedores"
      =Anomalias()
      Return
    EndIf

   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F01c = AllTrim(Sys(2015))
   Create Cursor (_F01c) ;
     (CodCli C(6), ;
      NomCli C(35), ;
      NIFCli C(15), ;
      DirCli C(35), ;
      PobCli C(35), ;
      PrvCli C(35), ;
      CodPos C(5), ;
      NumTel C(13), ;
      NumFax C(13))

   AF01c = Alias(_F01c)
   Select (AF01c)
   Append From &FilImp Type SDF
  
   *> Realizar la importación sobre fichero Proveedores.---------------------
   Go Top
   Do While !Eof()
      Wait Window  "Leyendo proveedor: " +  + &AF01c->CodCli + "-" + Left(&AF01c->NomCli, 20) NoWait

      Select F01c
      Zap
           
      * Buscar el código de proveedor en la tabla.
      m.F01cTipEnt = 'PROV'
      m.F01cCodigo = &AF01c->CodCli
      _ok = f3_seek('F01c')

      * Si alta.--------------------------------------------------------------     
      Select F01c
      If !_Ok
         Append Blank
         Replace F01cTipEnt With 'PROV'
         Replace F01cCodigo With &AF01c->CodCli
         Accion='INSERT'
      Else
         Accion='UPDATE'
      EndIf  
            
      * Reemplaza datos de la tabla.------------------------------------------------
      Replace F01cDescri With &AF01c->NomCli, ;
              F01cDirecc With &AF01c->DirCli, ;
              F01cNumNif With &AF01c->NIFCli, ;
              F01cPoblac With &AF01c->PobCli, ;
              F01cProvin With PadL(Left(&AF01c->CodPos, 2), 4, '0'), ;
              F01cCodPos With &AF01c->CodPos, ;
              F01cNumTel With &AF01c->NumTel, ;
              F01cNumFax With &AF01c->NumFax

      *> Eliminar apóstrofos de la descripción.
      Replace F01cDescri With StrTran(F01cDescri, "'", Space(1)), ;
              F01cDirecc With StrTran(F01cDirecc, "'", Space(1)), ;
              F01cPoblac With StrTran(F01cPoblac, "'", Space(1))

      *> Actualizar / Insertar el proveedor.
      If Accion='INSERT'                    
         Wait Window  "Insertando proveedor: " + &AF01c->CodCli + "-" + Left(&AF01c->NomCli, 20) NoWait
         _Err=f3_InsTun("F01C", "F01C")
         If !_Err
            _LxErr = "Error insertando proveedor: " + &AF01c->CodCli + "-" + Left(&AF01c->NomCli, 20)
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf

         *> Dar de alta la provincia, si cal.
         m.F00jCodPrv = PadL(Left(&AF01c->CodPos, 2), 4, '0')
         If !f3_seek('F00J')
            Select F00j
            Delete All
            Append Blank
            Replace F00jCodPrv With F01c.F01cProvin, ;
                    F00jDescri With &AF01c->PrvCli

            err = F3_InsTun("F00j","F00j","N")
            If err = .F.
               _lxerr = _lxerr + "No se ha podido dar de alta una nueva provincia" + cr
               =SqlRollBack(_ASql)
            Else
               Select Incidencias
               Append Blank
               Replace TipTras With 'Proveedores', ;
                       Campo   With 'Provincia', ;
                       Codigo  With F01c.F01cProvin, ;
                       Descri  With 'Se ha dado de alta la provincia ' + F01c.F01cProvin + ' automáticamente'
            EndIf
         EndIf
      Else
         Wait Window  "Actualizando proveedor: " +  + &AF01c->CodCli + "-" + Left(&AF01c->NomCli, 20) NoWait
         _Err=f3_UpdTun("F01C",,,,,"F01cTipEnt='PROV' And F01cCodigo='" + Right(F01c.F01cCodigo, 6) + _cm)
         If !_Err
            _LxErr = "Error actualizando proveedor: " +  + &AF01c->CodCli + "-" + Left(&AF01c->NomCli, 20)
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf 

      *>
      Select (AF01c)
      Skip             
  EndDo *bucle de líneas   

*>
Use In (AF01c)
Delete File(FilImp)
If MessageBox("Borrar archivo importación PROVEEDORES", 4 + 32 + 0) = 6
   DeleteGroupFiles(AllTrim(DirPro))
EndIf

Return

**********************************************************************************************
*> ------------------------------------------------------------------------------------------*
*>                             IMPORTACION DE PEDIDOS                                        *
*> Las observaciones son opcionales. AVC - 24.02.2000                                        *
*> Añadir campo 'ProExt', que indica si se prepara o solo se hace albarán. AVC - 29.02.2000  *
*> Implantar tratamiento multi-fichero. AVC - 09.03.2000                                     *
*> Tratamiento pedidos 'No Servir'. AVC - 14.03.2000                                         *
*> Texto observación pasa a ser de 216c. AVC - 29.03.2000                                    *
*> Generar ficha transportista. AVC - 30.03.2000                                             *
*> Añadir nº vale estupefaciente y precio línea. AVC - 30.03.2000                            *
*> Añadir nº de pedido del cliente. AVC - 04.04.2000                                         *
*> Añadir IVA y recargo de equivalencia. AVC - 04.04.2000                                    *
*> Añadir línea bonificación. AVC - 04.04.2000                                               *
*> IVA y descuentos van en la línea de detalle. AVC - 05.04.2000                             *
*> Descuentos línea, son acumulativos. AVC - 11.04.2000                                      *
*> Se graba el precio neto, en lugar del descuento. AVC - 11.04.2000                         *
*> Enviar marca de inicio observaciones cabecera y detalle. AVC - 17.04.2000                 *
*> Tratamiento de anulación de pedidos, total o parcialmente. AVC - 17.04.2000               *
*> Añadir segundo transportista para frío. AVC - 29.05.2000                                  *
*> Asignar ubicación de muelle, según el transportista. AVC - 25.07.2000                     *
*> Ampliar los datos de dirección del cliente. AVC - 19.09.2000                              *
*> Procesar pedidos en BACK ORDER. AVC - 16.10.2000                                          *
*> Recibir campo 'tipo de bonificación'. AVC - 27.10.2000                                    *
*> Vale estupefacientes, pasa de 10c. a 12c. AVC - 06.11.2000                                *
*> Corregir error de generar cabeceras sin líneas. AVC - 10.11.2000                          *
*> Control 1ªlínea observaciones con '@' en lugar de '*'. AVC - 15.11.2000                   *
*> Control estupefacientes por tipo de producto. AVC - 23.11.2000                            *
*> Eliminar incidencia 'línea a anular inexistente'. AVC - 14.12.2000                        *
*> Eliminar incidencia 'transportista en blanco' si baja pedido. AVC - 14.12.2000            *
*> Usar ADir() en lugar de Sys(2000) para borrar ficheros. AVC - 28.12.2000                  *
*> Controlar flag 'Bonificación'. AVC - 28.02.2001                                           *
*> ------------------------------------------------------------------------------------------*
**********************************************************************************************

Procedure ImpPed

Private FilImp
Private Accion
Private ImpDirPedCab, ImpDirPedPos, ImpDirPedObs, ImDirPedCli
Private _TipDoc, _UbiExpTra, _UbiExp
Private PedidoCerrado

*> Arrays para los nombres de fichero a tratar.
Declare aFilesCab(1), aFilesPos(1), aFilesCli(1), aFilesObs(1)

ImpDirPedCab = AllTrim(m.DirPedCab)
ImpDirPedPos = AllTrim(m.DirPedPos)
ImpDirPedObs = AllTrim(m.DirPedObs)
ImpDirPedCli = AllTrim(m.DirPedCli)

Set Separator To '.'
Set Point To ','
Set Date  To DMY
Set Century On
_LxErr = ''

   *> Primera parte: Comprobar que existan los ficheros de traspaso.
   ImpDirPedCab = GroupFilesNew(AllTrim(ImpDirPedCab), @aFilesCab)
   If !File(ImpDirPedCab)     && Cabecera
       _LxErr = _LxErr + "No se ha encontrado el fichero ASCII de Pedidos (Cabeceras) " + cr
   EndIf

   ImpDirPedPos = GroupFilesNew(AllTrim(ImpDirPedPos), @aFilesPos)
   If !File(ImpDirPedPos)     && Detalle
       _LxErr = _LxErr +  "No se ha encontrado el fichero ASCII de Pedidos (Detalle) " + cr
   EndIf

   ImpDirPedCli = GroupFilesNew(AllTrim(ImpDirPedCli), @aFilesCli)
   If !File(ImpDirPedCli)     && Datos cliente
       _LxErr = _LxErr +  "No se ha encontrado el fichero ASCII de Pedidos (Datos Cliente) " + cr
   EndIf

   *> Observaciones. Este fichero puede NO venir.
   ImpDirPedObs = GroupFilesNew(AllTrim(ImpDirPedObs), @aFilesObs)

   If !Empty(_LxErr)
      =Anomalias()
      =SqlRollBack(_ASql)

      If File(ImpDirPedCab)
         Delete File(ImpDirPedCab)
      EndIf

      If File(ImpDirPedPos)
         Delete File(ImpDirPedPos)
      EndIf

      If File(ImpDirPedCli)
         Delete File(ImpDirPedCli)
      EndIf

      *> Las observaciones son opcionales.
      If File(ImpDirPedObs)
         Delete File(ImpDirPedObs)
      EndIf

      Return
   EndIf

   *> Fichero de Cabeceras-------------------------------------------------------------------
   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F24c = AllTrim(Sys(2015))
   Create Cursor (_F24c) ;
     (TipOpe C(1), ;
      TipDoc C(4), ;
      NumDoc C(13), ;
      FecDoc C(8), ;
      FecPre C(8), ;
      CodCli C(10), ;
      PedCli C(15), ;
      CodTrp C(6), ;
      CodTrF C(6), ;
      ProExt C(1), ;
      PolSer C(1), ;
      Priori C(2), ;
      AlbVal C(1), ;
      Copias C(3), ;
      ImpLot C(1), ;
	  Buffer C(20), ;
      Fill01 C(10), ;
      Fill02 C(10), ;
      DtoDpp C(5), ;
      CodMon C(5), ;
      Portes C(1), ;
      SegSoc C(1), ;
      FacPro C(1), ;
      CtrDro C(1), ;
      CtrBck C(1))

   AF24c = Alias(_F24c)
   Select (AF24c)
   Append From &ImpDirPedCab Type SDF
   *> Fichero de Posiciones (Detalle)--------------------------------------------------------
   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F24l = AllTrim(Sys(2015))
   Create Cursor (_F24l) ;
     (TipDoc C(4), ;
      NumDoc C(13), ;
      LinDoc C(4), ;
      CodArt C(13), ;
      NumLot C(15), ;
      CanFis C(10), ;
      Precio C(12), ;
      ValIva C(5), ;
      ValEqv C(5), ;
      DtoPpo C(5), ;
      DtoPro C(5), ;
      DtoFar C(5), ;
      DtoSeg C(5), ;
      LBonif C(1), ;
      LAnula C(1), ;
      TipPro C(4))

   AF24l = Alias(_F24l)
   Select (AF24l)
   Append From &ImpDirPedPos Type SDF

   *> Fichero de Observaciones---------------------------------------------------------------
   *> Traspasar los datos del *.TXT a cursor de trabajo.
   *> Si línea de detalle, las diez primeras posiciones de observaciones corresponden al
   *> nº de vale de estupefacientes.
   *> Si primer carácter es un '*' indica primera línea de observaciones.
   *> Si primer carácter es un '@' indica primera línea de observaciones. AVC - 15.11.2000
   *> Inicio='R' indica primera línea de observaciones. AVC - 23.11.2000

   _F24o = AllTrim(Sys(2015))
   Create Cursor (_F24o) ;
     (TipDoc C(4), ;
      NumDoc C(10), ;
      LinDoc C(4), ;
      Observ C(50), ;
      Posici C(6), ;
      Inicio C(1))

   AF24o = Alias(_F24o)
   Select (AF24o)

   *> Las observaciones son opcionales.
   If File(ImpDirPedObs)
      Append From &ImpDirPedObs Type SDF
   EndIf

   *> Fichero de Datos Cliente---------------------------------------------------------------
   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F24t = AllTrim(Sys(2015))
   Create Cursor (_F24t) ;
     (CodCli C(10), ;
      TipDoc C(4), ;
      NumDoc C(13), ;
      NomCli C(35), ;
      NomCl2 C(35), ;
      NomCl3 C(35), ;
      NomCl4 C(35), ;
      NIFCli C(16), ;
      DirCli C(35), ;
      PobCli C(35), ;
      CodPos C(10), ;
      CodPai C(3))

   AF24t = Alias(_F24t)
   Select (AF24t)
   Append From &ImpDirPedCli Type SDF

   *> Cargar valores iniciales por defecto.
   If File('F24C.INI')
      _LeerLosDatos = .F.
      _area = FOpen('F24C.INI',0)
      Do While _area>0 .And. !FEof(_area)
         lx = FGetS(_area)
         lx = AllTrim(lx)
         If lx = '[IMPPED]'
            _LeerLosDatos = .T.
         Endif

         If !Empty(lx) .And. Left(lx, 1) # '[' .And. Left(lx, 1) # ';' .And. _LeerLosDatos
            &lx
         EndIf
      EndDo
      =FClose(_area)
   EndIf

   *> Verificar el valor del flag 'Bonificación' :
   *>   '0' debe ser 'N',
   *>   '1' debe ser 'S',
   *>   '2' debe ser 'S'.
   Select (AF24l)
   Replace All LBonif With 'N' For LBonif = '0'
   Replace All LBonif With 'S' For LBonif = '1'
   Replace All LBonif With 'S' For LBonif = '2'

   *> Realizar la importación sobre fichero.-----------------------------------------------
   Select (AF24c)
   Go Top
   Do While !Eof()
      Wait Window  "Leyendo pedido: " + &AF24c->NumDoc + " de fecha: " + DToC(SToD(&AF24c->FecDoc)) NoWait

      Selec F24c
      Zap        

      *> Flag de control de productos de frío.
      Store .F. To l_HayFri

      *> Control pedidos en BACK ORDER.
      Store .F. To PedidoCerrado
       
      *> Asignar el tipo de documento.
      _TipDoc = &AF24c->TipDoc

      _NumDoc = &AF24c->NumDoc

      *> Comprobar que existe el tipo de documento.
      m.F00kCodDoc = _TipDoc
      _ok = f3_seek('F00K')
      If !_ok
         Select Incidencias
         Append Blank
         Replace TipTras With 'Pedidos', ;
                 Campo   With 'Cabeceras', ;
                 Codigo  With &AF24c->TipDoc, ;
                 Descri  With 'No existe tipo doc'

         *> Pasar a siguiente pedido.
         Select (AF24c)
         Skip
         Loop
      EndIf

      *> Comprobar y, si cal, crear el transportista general.
      m.F01tCodigo = &AF24c->CodTrp
      If !Empty(m.F01tCodigo)
         If !f3_seek('F01T')
            _LxErr = "Error leyendo ficha transportista habitual" + cr
            =Anomalias()
            Return
         EndIf
      Else
         *> Transportista en blanco: Incidencia y asignar transportista genérico.
         *> Solo en el caso de pedido OK. Si pedido está en blanco, por ser una
         *> baja, no debe de generarse incidencia.
         If !Empty(&AF24c->CodCli)
            Select Incidencias
            Append Blank
            Replace TipTras With 'Pedidos', ;
                    Campo   With 'Cabeceras', ;
                    Codigo  With &AF24c->NumDoc, ;
                    Descri  With 'Transportista en blanco'

            *> Leer el transportista genérico (el último de F01T, '999999').
            If !f3_bottom('F01T')
               Select Incidencias
               Append Blank
               Replace TipTras With 'Pedidos', ;
                       Campo   With 'Cabeceras', ;
                       Codigo  With &AF24c->NumDoc, ;
                       Descri  With 'Grabar transportista genérico'

               *> Pasar a siguiente pedido.
               Select (AF24c)
               Skip
               Loop
            EndIf
         EndIf

         *> Guardar el transportista genérico en cursor de importación.
         Select (AF24c)
         Replace CodTrp With PadL(F01T.F01tCodigo, 10)
      EndIf

      *> Comprobar y, si cal, crear el transportista opcional de frío.
      m.F01tCodigo = Left(&AF24c->CodTrF, 6)
      If !Empty(m.F01tCodigo)
         If !f3_seek('F01T')
*            Select F01t
*            Delete All
*            Append Blank
*            Replace F01tCodigo With Left(&AF24c->CodTrF, 6), ;
*                    F01tDescri With Left(&AF24c->CodTrF, 6) + '-' + &AF24c->NumDoc

*            _err = f3_InsTun("F01T", "F01T", "N")
*            If !_Err
               _LxErr = "Error insertando ficha transportista opcional de frío" + cr
               =Anomalias()
               =SqlRollBack(_ASql)
               Return
*            EndIf
         EndIf
      Else
         *> Transportista de frío en blanco: Incidencia y asignar transportista genérico.
         Select Incidencias
         Append Blank
         Replace TipTras With 'Pedidos', ;
                 Campo   With 'Cabeceras', ;
                 Codigo  With &AF24c->NumDoc, ;
                 Descri  With 'Transportista en blanco'

         *> Leer el trasnsportista genérico (el último de F01T, '999999').
         If !f3_bottom('F01T')
            Select Incidencias
            Append Blank
            Replace TipTras With 'Pedidos', ;
                    Campo   With 'Cabeceras', ;
                    Codigo  With &AF24c->NumDoc, ;
                    Descri  With 'Grabar transportista genérico'

            *> Pasar a siguiente pedido.
            Select (AF24c)
            Skip
            Loop
         EndIf

         *> Guardar el transportista genérico en cursor de importación.
         Select (AF24c)
         Replace CodTrF With PadL(F01T.F01tCodigo, 10)
      EndIf

      *> Leer la ubicación de muelle del transportista por defecto.
      _UbiExpTra = Space(6)
      _Where = "F01uTipEnt='TRAN' And F01uCodEnt='" + &AF24c->CodTrp + "'"
      If f3_sql('*', 'F01u', _Where, , , 'F01uCur')
         Select F01uCur
         Go Top
         _UbiExpTra = F01uUbiExp
      EndIf
      _UbiExpTra = Iif(Empty(_UbiExpTra), _UbiExp, _UbiExpTra)

      *> Leer la ubicación de muelle del transportista de frío.
      _UbiExpTrf = Space(6)
      _Where = "F01uTipEnt='TRAN' And F01uCodEnt='" + &AF24c->CodTrF + "'"
      If f3_sql('*', 'F01u', _Where, , , 'F01uCur')
         Select F01uCur
         Go Top
         _UbiExpTrf = F01uUbiExp
      EndIf

      If Used('F01uCur')
         Use In F01uCur
      EndIf

       *>  Ver si el pedido ya existe en PROCAOT.-----------------------------------------------
       m.F24cCodPro = CodPro
       m.F24cTipDoc = _TipDoc
       m.F24cNumDoc = _NumDoc
       _ok = f3_seek('F24C')

       Select F24c
       If !_ok
          Append Blank
          Replace F24cCodPro With CodPro, ;
                  F24cTipDoc With _TipDoc, ;
                  F24cNumDoc With _NumDoc

          *> Valores por defecto de la cabecera del documento.
          Replace F24cOrdRut With 0, ;
                  F24cFraTot With 0, ;
                  F24cFraAct With 0, ;
                  F24cCajTot With 0, ;
                  F24cCajAct With 0, ;
                  F24cPalTot With 0, ;
                  F24cPalAct With 0, ;
                  F24cNumBul With 0, ;
                  F24cTotVol With 0, ;
                  F24cTotKgs With 0, ;
                  F24cImpBas With 0, ;
                  F24cGastos With 0, ;
                  F24cTotIva With 0, ;
                  F24cTotImp With 0, ;
                  F24cNumTrp With 0, ;
                  F24cFecAlb With {  /  /    }

          Accion='INSERT'
       Else
          *> El pedido ya existe: Comprobar si su estado permite la modificación.
          *> Si ya está procesado, puede tratarse de líneas en BACK ORDER.
          If F24cFlgEst > '0'
             Select Incidencias
             Append Blank
        	 Replace TipTras With 'Pedidos', ;
          	         Campo   With 'Cabeceras', ;
           	         Codigo  With _NumDoc, ;
           	         Descri  With 'Pedido no mod. Back Order'

             *> Tratamiento posibles líneas en Back Order.
             If &AF24c->CtrBck=='S'
                =BackOrder()
             EndIf

             *> Procesar siguiente pedido.
             Select (AF24c)
             Skip
           	 Loop
          Endif
          Accion='UPDATE'
       EndIf  

       * Reemplaza datos de la tabla.----------------------------------------------------------
       Replace F24cRutHab With Iif(Type('_RutHab') # 'U', _RutHab, F24cRutHab)
       Replace F24cVenHab With Iif(Type('_VenHab') # 'U', _VenHab, F24cVenHab)
       Replace F24cConAlb With Iif(Type('_ConAlb') # 'U', _ConAlb, 'N')
       Replace F24cAgrAlb With Iif(Type('_AgrAlb') # 'U', _AgrAlb, 'N')
       Replace F24cUbiExp With _UbiExpTra
       Replace F24cFecDoc With CToD(&AF24c->FecDoc)
       Replace F24cDirAso With &AF24c->CodCli
       Replace F24cFecPre With CToD(&AF24c->FecPre)
       Replace F24cNumPed With Left(&AF24c->PedCli,15)
       Replace F24cCodTra With Left(&AF24c->CodTrp, 6)
       Replace F24cCodTrF With Left(&AF24c->CodTrF, 6)
       Replace F24cPolSer With &AF24c->PolSer
       Replace F24cPriori With Val(&AF24c->Priori)
       Replace F24cValAlb With Iif(&AF24c->AlbVal=='1', 'N', ;
                               Iif(&AF24c->AlbVal=='2', 'S', ;
                               Iif(&AF24c->AlbVal=='3', 'T', 'S')))
       Replace F24cNumCop With Val(&AF24c->Copias)
	   Replace F24cImpLot With &AF24c->ImpLot
	   Replace F24cSegSoc With &AF24c->SegSoc
	   Replace F24cDesPpp With Val(&AF24c->DtoDpp)
	   Replace F24cPortes With &AF24c->Portes
	   Replace F24cFecCre With Date()
	   Replace F24cAlmSer With _Alma
       Replace F24cCtrDro With &AF24c->CtrDro
       Replace F24cCtrPro With &AF24c->FacPro
	   Replace F24cFlgEst With '0'


       *> Crear / Actualizar la cabecera del pedido.----------------------------------------
       *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.--------------------
       If Accion='INSERT'
          _err=f3_InsTun("F24C", "F24C", "N")
          If !_Err
             _LxErr = "Error insertando cabecera pedido: " + _NumDoc
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       Else
          _Where = "F24cCodPro=?CodPro And " + ; 
                   "F24cTipDoc=?_TipDoc And " + ; 
                   "F24cNumDoc=?_NumDoc"
          _err=f3_UpdTun("F24C", , , , , _Where, "N", "N")
          If !_Err
             _LxErr = "Error actualizando cabecera pedido: " + _NumDoc
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       EndIf *bucle de ins/upd cabecera

       *> Grabar las líneas de detalle (posiciones) de ESTE pedido.-------------------------
       *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.--------------------
       Select (AF24l)
       Locate For NumDoc = AllTrim(_NumDoc)
       If Eof()
          =SqlRollBack(_ASql)
          Select Incidencias
          Append Blank
          Replace TipTras With 'Pedidos', ;
                  Campo   With 'Cabeceras', ;
                  Codigo  With _NumDoc, ;
                  Descri  With 'Pedido sin líneas de detalle'

**********Select (AF24c)
**********Skip
**********Loop
       EndIf


       *> Bucle de lectura y grabación del detalle del pedido actual.

       Do While Found()
          _LinDoc = Left(Alltrim(&AF24l->LinDoc), 4)

          m.F24lCodPro = CodPro
          m.F24lTipDoc = _TipDoc
          m.F24lNumDoc = _NumDoc
          m.F24lLinDoc = _LinDoc

          _okl = f3_seek('F24L')
          If _okl
             Select F24l
             Go Top

             *> Línea en estado no válido.
             If F24lFlgEst > '0'
                Select Incidencias
                Append Blank
                Replace TipTras With 'Pedidos', ;
                        Campo   With 'Detalle', ;
                        Codigo  With _NumDoc, ;
                        Descri  With 'Línea en estado no válido'

                Select (AF24l)
                Continue
                Loop
             EndIf
          EndIf

          *> Control de líneas anuladas.
          _AnularLinea = &AF24l->LAnula
          If _AnularLinea=='S'
             *> No existe la línea de detalle original.
             If !_okl
             *   Select Incidencias
             *   Append Blank
             *   Replace TipTras With 'Pedidos', ;
             *           Campo   With 'Detalle', ;
             *           Codigo  With _NumDoc, ;
             *           Descri  With 'Línea a anular inexistente'
             Else
                *> Borrar la línea de detalle.
                If !f3_baja('F24L')
                   Select Incidencias
                   Append Blank
                   Replace TipTras With 'Pedidos', ;
                           Campo   With 'Detalle', ;
                           Codigo  With _NumDoc, ;
                           Descri  With 'Error al borrar línea'
                EndIf
             EndIf

             *> Leer el siguiente registro de detalle del pedido actual.
             Select(AF24l)
             Continue
             Loop
          EndIf

          *> No existe la línea de pedido: crear y mover campos.------------------------------     
          Select F24l
          If !_okl
             Append Blank
             Replace F24lCodPro With CodPro, ;
                     F24lTipDoc With _TipDoc, ;
                     F24lNumDoc With _NumDoc, ;
                     F24lLinDoc With _LinDoc
             Accion='INSERT'
          Else
             Accion='UPDATE'
          EndIf  

          *> Reemplaza datos de la tabla.-----------------------------------------------------
          _CodArt = &AF24l->CodArt

          *> Buscar datos del artículo.
          m.F08cCodPro = CodPro
          m.F08cCodArt = _CodArt
          If !f3_seek('F08c')
                Select Incidencias
                Append Blank
                Replace TipTras With 'Pedidos', ;
                        Campo   With 'Detalle', ;
                        Codigo  With _CodArt, ;
                        Descri  With 'Artículo no existe'

                Select (AF24l)
                Continue
                Loop
          EndIf

          *> Guardar el tipo de producto para control de estupefacientes.
          Replace &AF24l->TipPro With F08c.F08cTipPro

          *> Flag producto de frío.
          l_HayFri = Iif(!l_HayFri, F08cTipPro==TipPro(1), l_HayFri)

          _Precio = Val(&AF24l->Precio) / 100
          _DtoPpo = Val(&AF24l->DtoPpo) / 1000
          _DtoPro = Val(&AF24l->DtoPro) / 1000
          _DtoFar = Val(&AF24l->DtoFar) / 1000
          _DtoSeg = Val(&AF24l->DtoSeg) / 1000

          _PrecioN = _Precio
          _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoPpo / 100))
          _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoPro / 100))
          _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoFar / 100))
          _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoSeg / 100))

          Select F24l
          Replace F24lFecDoc With CToD(&AF24c->FecDoc), ;
                  F24lFecPre With CToD(&AF24c->FecPre), ;
                  F24lFecCad With {  /  /    }, ;
                  F24lCodArt With _CodArt, ;
                  F24lNumLot With &AF24l->NumLot, ;
                  F24lCanDoc With Val(&AF24l->CanFis), ;
	              F24lCanEnv With 0, ;
	              F24lCanRes With 0, ;
                  F24lPrecio With _Precio, ;
                  F24lDtoLin With _PrecioN, ;
                  F24lIvaLin With Val(&AF24l->ValIva) / 1000, ;
                  F24lEqvLin With Val(&AF24l->ValEqv) / 1000, ;
                  F24lSitStk With '1000', ;
                  F24lAlmSer With _Alma, ;
                  F24lRutHab With Iif(Type('_RutHab') # 'U', _RutHab, F24lRutHab), ;
                  F24lNumDrg With Space(10), ;
	              F24lFlgEst With '0', ;
                  F24lFlag1  With &AF24l->LBonif, ;
                  F24lFlag2  With '0', ;
                  F24lFlag3  With '0'

          If Accion='INSERT'
             _err=f3_InsTun("F24L", "F24L", "N")
             If !_Err
                _LxErr = "Error insertando detalle pedido: " + _NumDoc + "-" + &AF24l->CodArt
                =Anomalias()
                =SqlRollBack(_ASql)
                Return
             EndIf
          Else
             _Where = "F24lCodPro=?CodPro And " + ; 
                      "F24lTipDoc=?_TipDoc And " + ; 
                      "F24lNumDoc=?_NumDoc And " + ;
                      "F24lLinDoc=?_LinDoc"
             _err=f3_UpdTun("F24L", , , , , _Where, "N", "N")
             If !_Err
                _LxErr = "Error actualizando detalle pedido: " + _NumDoc + "-" + &AF24l->CodArt
                =Anomalias()
                =SqlRollBack(_ASql)
                Return
             EndIf
          EndIf *bucle de ins/upd líneas

          *>
          Select(AF24l)
          Continue
       EndDo

       *> Como se pueden producir anulaciones, hay que comprobar que el pedido
       *> no se queda sin líneas de detalle, en cuyo caso se anula la cabecera.
       _Where = "F24lCodPro=?CodPro And " + ; 
                "F24lTipDoc=?_TipDoc And " + ; 
                "F24lNumDoc=?_NumDoc"

       =f3_sql('*', 'F24l', _Where, '', '', 'F24L')
       Select F24l
       Go Top
       If Eof()
          m.F24cCodPro=CodPro
          m.F24cTipDoc=_TipDoc
          m.F24cNumDoc=_NumDoc
          =f3_baja('F24C')
          =SqlCommit(_ASql)
          Select (AF24c)
          Skip
          Loop
       EndIf

       *> Actualizar la ubicación de muelle, según haya productos de frío o no.
       If l_HayFri .And. !Empty(_UbiExpTrf) .And. !IsNull(_UbiExpTrf)
          m.F24cCodPro = CodPro
          m.F24cTipDoc = _TipDoc
          m.F24cNumDoc = _NumDoc
          If !f3_seek('F24C')
             _LxErr = "Error actualizando muelle pedido: " + _NumDoc + cr
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf

          _Where = "F24cCodPro=?CodPro  And " + ; 
                   "F24cTipDoc=?_TipDoc And " + ; 
                   "F24cNumDoc=?_NumDoc"

          If !f3_UpdTun('F24c', ,'F24cUbiExp', '_UbiExpTrf', , _Where, 'N')
             _LxErr = "Error actualizando transportista frío pedido: " + _NumDoc + cr
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       EndIf

       *> Grabación de las observaciones del pedido.-------------------------------------
       *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.-----------------
       *> Como pueden venir varias líneas de observaciones, en primer--------------------
       *> lugar se borran las ya existentes.---------------------------------------------

       *> Ancho máximo de la línea.
       _oldMemoW = Set('MEMOWIDTH')
       Set MemoWidth To 60

       *> Borrar observaciones de cabecera.
       _LinDoc = '0000'
       If f3_seek('F24O', "[m.F24oCodPro=CodPro,m.F24oTipDoc=_TipDoc,m.F24oNumDoc=_NumDoc,m.F24oLinObs=m._LinDoc]")
          =f3_baja('F24O')
       EndIf

       *> Borrar observaciones de pie.
       _LinDoc = '9999'
       If f3_seek('F24O', "[m.F24oCodPro=CodPro,m.F24oTipDoc=_TipDoc,m.F24oNumDoc=_NumDoc,m.F24oLinObs=m._LinDoc]")
          =f3_baja('F24O')
       EndIf

       *> Para controlar el nº de vale, en caso de que no lleguen observaciones,
       *> que es donde se guarda el nº de vale de estupefacientes.
       Store .F. To HayObservaciones

       Select (AF24o)
       Locate For NumDoc = AllTrim(_NumDoc)
       Do While Found()
          Store .T. To HayObservaciones

          _LinDoc = Right('000000'+alltrim(&AF24o->LinDoc), 4)
          If Val(_LinDoc) > 900
             _LinDoc = '9999'
          EndIf

          m.F24oCodPro = CodPro
          m.F24oTipDoc = _TipDoc
          m.F24oNumDoc = _NumDoc
          m.F24oLinObs = _LinDoc

          _Observ = &AF24o->Observ

          _ok = f3_seek('F24O')

          *> Si es una línea de detalle, borrar.
          If _LinDoc > '0000' .And. _LinDoc < '9000' .And. _ok
             =f3_baja('F24O')
             _ok = .F.
          EndIf

          *> Si es primera línea de observaciones, borrar.
          If &AF24o->Inicio=='R' .And. _ok
             =f3_baja('F24O')
             *** _Observ = SubStr(_Observ, 2)         Antes Left(Observ).
             _ok = .F.
          EndIf       

          Select F24o
          If !_ok
             Zap
             Append Blank
             Replace F24oCodPro With CodPro, ;
                     F24oTipDoc With _TipDoc, ;
                     F24oNumDoc With _NumDoc, ;
                     F24oLinObs With _LinDoc
             Accion='INSERT'
          Else
             Accion='UPDATE'
          EndIf  

          *> Grabar el resto de los campos de observaciones.-----------------------------
          _Imp1 = Iif(AtC('A', &AF24o->Posici)>0, 'S', 'N')
          _Imp2 = Iif(AtC('F', &AF24o->Posici)>0, 'S', 'N')
          _Imp3 = Iif(AtC('L', &AF24o->Posici)>0, 'S', 'N')
          _Imp4 = Iif(AtC('P', &AF24o->Posici)>0, 'S', 'N')
          _Imp5 = Iif(AtC('E', &AF24o->Posici)>0, 'S', 'N')
          _Imp6 = Iif(AtC('H', &AF24o->Posici)>0, 'S', 'N')

          If MemLines(F24oDesObs) < 6
             Replace F24oDesObs With Iif(Len(F24oDesObs) > 0, ;
                                         F24oDesObs + cr + AllTrim(Left(_Observ, 59)), ;
                                         AllTrim(Left(_Observ, 59))), ;
                     F24oImpObs With _Imp1 + _Imp2 + _Imp3 + _Imp4 + _Imp5 + _Imp6
          EndIf

          Replace F24oDesObs With StrTran(F24oDesObs, "'", Space(1))

          *> Grabar / actualizar observaciones.------------------------------------------
          If Accion='INSERT'
             _err=f3_InsTun("F24O", "F24O", "N")
             If !_Err
                _LxErr = "Error insertando observaciones pedido: " + _NumDoc
                =Anomalias()
                =SqlRollBack(_ASql)
                Return
             EndIf
          Else
             _Where = "F24oCodPro=?CodPro And " + ; 
                      "F24oTipDoc=?_TipDoc And " + ; 
                      "F24oNumDoc=?_NumDoc And " + ;
                      "F24oLinObs=?_LinDoc"
             _err=f3_UpdTun("F24O", , , , , _Where, "N", "N")
             If !_Err
                _LxErr = "Error actualizando observaciones pedido: " + _NumDoc
                =Anomalias()
                =SqlRollBack(_ASql)
                Return
             EndIf
          EndIf            && bucle de ins/upd obs

          *> Ver si viene el Nº de vale.
          If Val(_LinDoc) > 0 .And. Val(_LinDoc) < 9000
             _ValeEstupefacientes = Left(&AF24o->Observ, 12)

             *> Buscar el nº de línea en el cursor de líneas de detalle.
             Select (AF24l)
             Locate For NumDoc = RTrim(_NumDoc) .And. LinDoc = PadL(_LinDoc, 6, '0')

             *> Estupefacientes sin control estupefacientes.
             If (&AF24l->TipPro==TipPro(2) .Or. &AF24l->TipPro==TipPro(5)) .And. ;
                &AF24c->CtrDro#'S'

                Select Incidencias
                Append Blank
                Replace TipTras With 'Pedidos', ;
                        Campo   With 'Nº Vale', ;
                        Codigo  With _NumDoc, ;
                        Descri  With 'Estupefacientes sin control'
             EndIf

             *> Estupefacientes sin Nº de vale.
             If (&AF24l->TipPro==TipPro(2) .Or. &AF24l->TipPro==TipPro(5)) .And. ;
                Empty(_ValeEstupefacientes)

                Select Incidencias
                Append Blank
                Replace TipTras With 'Pedidos', ;
                        Campo   With 'Nº Vale', ;
                        Codigo  With _NumDoc, ;
                        Descri  With 'Estupefacientes sin nº vale'
             EndIf

             *> Nº de vale sin estupefacientes.
             If (&AF24l->TipPro#TipPro(2) .And. &AF24l->TipPro#TipPro(5)) .And. ;
                !Empty(_ValeEstupefacientes)

                Select Incidencias
                Append Blank
                Replace TipTras With 'Pedidos', ;
                        Campo   With 'Nº Vale', ;
                        Codigo  With _NumDoc, ;
                        Descri  With 'Vale sin estupefacientes'
             EndIf

             *> Grabar el nº de vale en la línea de detalle del pedido.
             m.F24lCodPro = CodPro
             m.F24lTipDoc = _TipDoc
             m.F24lNumDoc = _NumDoc
             m.F24lLinDoc = _LinDoc

             If !f3_seek('F24L')
                Select Incidencias
                Append Blank
                Replace TipTras With 'Pedidos', ;
                        Campo   With 'Nº Vale', ;
                        Codigo  With _NumDoc, ;
                        Descri  With 'Error detalle pedido'
             Else
                Select F24l
                Replace F24lNumDrg With _ValeEstupefacientes
                _Where = "F24lCodPro=?CodPro And " + ; 
                         "F24lTipDoc=?_TipDoc And " + ; 
                         "F24lNumDoc=?_NumDoc And " + ;
                         "F24lLinDoc=?_LinDoc"
                If !f3_UpdTun("F24l", ,"F24lNumDrg", , , _Where, 'N')
                   _LxErr = "Error actualizando nº de vale estupefacientes pedido: " + _NumDoc + cr
                   =Anomalias()
                   =SqlRollBack(_ASql)
                   Return
                EndIf
             EndIf
          EndIf

          *>
          Select (AF24o)
          Continue
       EndDo

       *> Restablecer el ancho standard de campos Memo.
       Set MemoWidth To _oldMemoW

       *> Controlar estupefacientes en el caso de no haber llegado observaciones.
       If !HayObservaciones .And. &AF24c->CtrDro=='S'
          Select Incidencias
          Append Blank
          Replace TipTras With 'Pedidos', ;
                  Campo   With 'Nº Vale', ;
                  Codigo  With _NumDoc, ;
                  Descri  With 'Pedido con estupefacientes no recibidos'
       EndIf

       *> Leer direcciones asociadas a propietario como datos por defecto.---------------
       m.F22pCodPro = CodPro
       m.F22pDirAso = PadR(&AF24c->CodCli, 13, ' ')
       _ok = f3_seek('F22P')
       If !_ok
          Select F22p
          Append Blank
          Replace F22pCodPro With CodPro
          Replace F22pDirAso With &AF24c->CodCli
       EndIf

       *> Grabación de datos de clientes-direcciones.------------------------------------
       *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.-----------------
       Select(AF24t)
       Locate For NumDoc = AllTrim(_NumDoc)
       If Found()
          *> Traspasar datos recibidos a ficha F22p.
          Select F22p
          Replace F22pNomAso With &AF24t->NomCli, ;
                  F22p1ErDir With &AF24t->NomCl2, ;
                  F22p2ndDir With &AF24t->DirCli, ;
                  F22p3rdDir With &AF24t->NomCl3, ;
                  F22pDPobla With &AF24t->PobCli, ;
                  F22pCodPos With &AF24t->CodPos, ;
                  F22pCProvi With PadL(Left(&AF24t->CodPos, 2), 4, '0'), ;
                  F22pCodNif With Left(&AF24t->NIFCli,15), ;
                  F22pCodPas With &AF24t->CodPai

          *> Obtener la provincia.-------------------------------------------------------
          m.F00jCodPrv = PadL(Left(&AF24t->CodPos, 2), 4, '0')
          If f3_seek('F00J')
             Replace F22p.F22pDProvi With Left(F00jDescri, 35)
          Else
             Replace F22p.F22pDProvi With Space(35)
          EndIf

          *> Eliminar apóstrofos de la descripción.
          Replace F22pNomAso With StrTran(F22pNomAso, "'", Space(1)), ;
                  F22p1erDir With StrTran(F22p1erDir, "'", Space(1)), ;
                  F22p2ndDir With StrTran(F22p2ndDir, "'", Space(1)), ;
                  F22p3rdDir With StrTran(F22p3rdDir, "'", Space(1)), ;
                  F22pDPobla With StrTran(F22pDPobla, "'", Space(1)), ;
                  F22pDProvi With StrTran(F22pDProvi, "'", Space(1))

          *> Rellenar datos de dirección en el pedido.-----------------------------------
          m.F24tCodPro = CodPro
          m.F24tTipDoc = _TipDoc
          m.F24tNumDoc = _NumDoc
          _ok = f3_seek('F24T')

          *> No existen los datos de cliente: crear.-------------------------------------
          Select F24t              
          If !_ok
             Append Blank
             Replace F24tCodPro With CodPro
             Replace F24tTipDoc With _TipDoc
             Replace F24tNumDoc With _NumDoc
             Accion='INSERT'
          Else
             Accion='UPDATE'
          EndIf

          *> Grabar el resto de los campos de direcciones.-------------------------------
          Replace F24tNomAso With F22p.F22pNomAso, ;
                  F24t1ErDir With F22p.F22p1erDir, ; 
                  F24t2ndDir With F22p.F22p2ndDir, ; 
                  F24t3rdDir With F22p.F22p3rdDir, ; 
                  F24tDPobla With F22p.F22pDPobla, ; 
                  F24tDProvi With F22p.F22pDProvi, ; 
                  F24tCodPos With F22p.F22pCodPos, ; 
                  F24tCProvi With F22p.F22pCProvi, ; 
                  F24tCodPas With F22p.F22pCodPas, ; 
                  F24tNumTel With F22p.F22pTelefo, ; 
                  F24tNumFax With F22p.F22pNumFax, ; 
                  F24tNumNif With F22p.F22pCodNif, ;
                  F24tFlete  With F22p.F22pFlete, ;
                  F24tExtrad With F22p.F22pExtrad

          *> Grabar / actualizar direcciones.--------------------------------------------
          If Accion='INSERT'
             _err=f3_InsTun("F24T", "F24T", "N")
             If !_Err
                _LxErr = "Error insertando direcciones pedido: " + _NumDoc
                =Anomalias()
                =SqlRollBack(_ASql)
                Return
             EndIf
          Else
             _Where = "F24tCodPro=?CodPro And " + ; 
                      "F24tTipDoc=?_TipDoc And " + ; 
                      "F24tNumDoc=?_NumDoc"
             _err=f3_UpdTun("F24T", , , , , _Where, "N", "N")
             If !_Err
                _LxErr = "Error actualizando direcciones pedido: " + _NumDoc
                =Anomalias()
                =SqlRollBack(_ASql)
                Return
             EndIf
          EndIf *bucle de ins/upd direc
       EndIf * If Found()

       *> Finalmente, si todo es OK, grabar el pedido.
       =SqlCommit(_ASql)

       *> Leer la siguiente cabecera del pedido.
       Select (AF24c)
       Skip
    EndDo         && bucle de cabecera
 
   Use In (AF24c)
   Use In (AF24l)
   Use In (AF24t)
   Use In (AF24o)

   Delete File(ImpDirPedCab)
   Delete File(ImpDirPedPos)
   Delete File(ImpDirPedCli)

   *> Las observaciones son opcionales.
   If File(ImpDirPedObs)
      Delete File(ImpDirPedObs)
   EndIf

   If MessageBox("Borrar archivo importación PEDIDOS", 4 + 32 + 0) = 6
      DeleteGroupFilesNew(AllTrim(m.DirPedCab), @aFilesCab)
      DeleteGroupFilesNew(AllTrim(m.DirPedPos), @aFilesPos)
      DeleteGroupFilesNew(AllTrim(m.DirPedObs), @aFilesCli)
      DeleteGroupFilesNew(AllTrim(m.DirPedCli), @aFilesObs)
   EndIf

Return

*>
*> Creación de pedidos en Back Order.
*>
Function BackOrder()
Private NewPedido
Private Accion

   Store Space(1) To NewPedido

   *> Obtener el nuevo nº de pedido.
   NewPedido = GetNewPedido(CodPro, _TipDoc, _NumDoc)
   If Empty(NewPedido)
      Select Incidencias
      Append Blank
      Replace TipTras With 'Pedidos', ;
              Campo   With 'Cabecera', ;
              Codigo  With NewPedido, ;
              Descri  With 'Error Back Order'

      =SqlRollBack(_ASql)
      Return
   EndIf

   *> Crear registro de cabecera.
   Select F24c
   Append Blank
   Replace F24cCodPro With CodPro, ;
           F24cTipDoc With _TipDoc, ;
           F24cNumDoc With NewPedido

   *> Valores por defecto de la cabecera del documento.
   Replace F24cOrdRut With 0, ;
           F24cFraTot With 0, ;
           F24cFraAct With 0, ;
           F24cCajTot With 0, ;
           F24cCajAct With 0, ;
           F24cPalTot With 0, ;
           F24cPalAct With 0, ;
           F24cNumBul With 0, ;
           F24cTotVol With 0, ;
           F24cTotKgs With 0, ;
           F24cImpBas With 0, ;
           F24cGastos With 0, ;
           F24cTotIva With 0, ;
           F24cTotImp With 0, ;
           F24cNumTrp With 0, ;
           F24cFecAlb With {  /  /    }

   *> Reemplaza datos de la tabla.
   Replace F24cRutHab With Iif(Type('_RutHab') # 'U', _RutHab, F24cRutHab), ;
           F24cVenHab With Iif(Type('_VenHab') # 'U', _VenHab, F24cVenHab), ;
           F24cConAlb With Iif(Type('_ConAlb') # 'U', _ConAlb, 'N'), ;
           F24cAgrAlb With Iif(Type('_AgrAlb') # 'U', _AgrAlb, 'N'), ;
           F24cUbiExp With _UbiExpTra, ;
           F24cFecDoc With SToD(&AF24c->FecDoc), ;
           F24cDirAso With &AF24c->CodCli, ;
           F24cFecPre With SToD(&AF24c->FecPre), ;
           F24cNumPed With Left(&AF24c->PedCli,15), ;
           F24cCodTra With Left(&AF24c->CodTrp, 6), ;
           F24cCodTrF With Left(&AF24c->CodTrF, 6), ;
           F24cPolSer With &AF24c->PolSer, ;
           F24cPriori With Val(&AF24c->Priori), ;
           F24cValAlb With Iif(&AF24c->AlbVal=='1', 'N', ;
                           Iif(&AF24c->AlbVal=='2', 'S', ;
                           Iif(&AF24c->AlbVal=='3', 'T', 'N'))), ;
           F24cNumCop With Val(&AF24c->Copias), ;
           F24cImpLot With &AF24c->ImpLot, ;
           F24cSegSoc With &AF24c->SegSoc, ;
	       F24cDesPpp With Val(&AF24c->DtoDpp), ;
	       F24cPortes With &AF24c->Portes, ;
	       F24cFecCre With Date(), ;
	       F24cAlmSer With _Alma, ;
           F24cCtrDro With &AF24c->CtrDro, ;
           F24cCtrPro With &AF24c->FacPro, ;
	       F24cFlgEst With '0'

   *> Crear la cabecera del pedido.
   *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.
   If !f3_InsTun("F24C", "F24C", "N")
      _LxErr = "Error insertando Back Order: " + NewPedido
      =Anomalias()
      =SqlRollBack(_ASql)
      Return
   EndIf

   *> Grabar las líneas de detalle (posiciones) de ESTE pedido.
   *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.
   Select (AF24l)
   Locate For NumDoc = AllTrim(_NumDoc)
   If Eof()
      Select Incidencias
      Append Blank
      Replace TipTras With 'Pedidos', ;
              Campo   With 'Cabeceras', ;
              Codigo  With NewPedido, ;
              Descri  With 'Back Order sin líneas'

      =SqlRollBack(_ASql)
      Return
   EndIf

   *> Bucle de lectura y grabación del detalle del pedido actual.
   Do While Found()
      _LinDoc = Right(&AF24l->LinDoc, 4)

      m.F24lCodPro = CodPro
      m.F24lTipDoc = _TipDoc
      m.F24lNumDoc = _NumDoc
      m.F24lLinDoc = _LinDoc

      *> Ver si esta línea ya existe en el pedido original.
      If f3_seek('F24L')
*        Select Incidencias
*        Append Blank
*        Replace TipTras With 'Pedidos', ;
*                Campo   With 'Detalle', ;
*                Codigo  With _NumDoc, ;
*                Descri  With 'Línea original ya existe'

         *> Leer siguiente línea de este pedido.
         Select (AF24l)
         Continue
         Loop
      EndIf

      *> Control de líneas anuladas. (Aquí es un error).
      *If &AF24l->LAnula=='S'
      *   Select Incidencias
      *   Append Blank
      *   Replace TipTras With 'Pedidos', ;
      *           Campo   With 'Detalle', ;
      *           Codigo  With NewPedido, ;
      *           Descri  With 'Línea a anular inexistente'

      *  *> Leer el siguiente registro de detalle del pedido actual.
      *   Select(AF24l)
      *   Continue
      *   Loop
      *EndIf

      m.F24lCodPro = CodPro
      m.F24lTipDoc = _TipDoc
      m.F24lNumDoc = NewPedido
      m.F24lLinDoc = _LinDoc

      *> Crear línea de pedido y mover campos.
      If !f3_seek('F24L')
         Select F24l
         Append Blank
         Replace F24lCodPro With CodPro, ;
                 F24lTipDoc With _TipDoc, ;
                 F24lNumDoc With NewPedido, ;
                 F24lLinDoc With _LinDoc
         Accion = 'INSERT'
      Else
         Accion = 'UPDATE'
      EndIf

      *> Reemplaza datos de la tabla.-----------------------------------------------------
      _CodArt = &AF24l->CodArt
     * _CodArt = Iif(IsAlpha(_CodArt), AllTrim(_CodArt), Right(_CodArt, 6))

      *> Buscar datos del artículo.
      m.F08cCodPro = CodPro
      m.F08cCodArt = PadR(_CodArt, 13)
      If !f3_seek('F08c')
         _LxErr = "(B) Error leyendo artículo: " + _CodArt + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      *> Flag producto de frío.
      l_HayFri = Iif(!l_HayFri, F08cTipPro==TipPro(1), l_HayFri)

      _Precio = Val(&AF24l->Precio) / 100
      _DtoPpo = Val(&AF24l->DtoPpo) / 1000
      _DtoPro = Val(&AF24l->DtoPro) / 1000
      _DtoFar = Val(&AF24l->DtoFar) / 1000
      _DtoSeg = Val(&AF24l->DtoSeg) / 1000

      _PrecioN = _Precio
      _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoPpo / 100))
      _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoPro / 100))
      _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoFar / 100))
      _PrecioN = _PrecioN - Int2(_PrecioN * (_DtoSeg / 100))

      Select F24l
      Replace F24lFecDoc With SToD(&AF24c->FecDoc), ;
              F24lFecPre With SToD(&AF24c->FecPre), ;
              F24lFecCad With {  /  /    }, ;
              F24lCodArt With _CodArt, ;
              F24lNumLot With &AF24l->NumLot, ;
              F24lCanDoc With Val(&AF24l->CanFis), ;
              F24lCanEnv With 0, ;
              F24lCanRes With 0, ;
              F24lPrecio With _Precio, ;
              F24lDtoLin With _PrecioN, ;
              F24lIvaLin With Val(&AF24l->ValIva) / 1000, ;
              F24lEqvLin With Val(&AF24l->ValEqv) / 1000, ;
              F24lSitStk With '1000', ;
              F24lAlmSer With _Alma, ;
              F24lRutHab With Iif(Type('_RutHab') # 'U', _RutHab, F24lRutHab), ;
              F24lNumDrg With Space(10), ;
	          F24lFlgEst With '0', ;
              F24lFlag1  With &AF24l->LBonif, ;
              F24lFlag2  With '0', ;
              F24lFlag3  With '0'

      If Accion = 'INSERT'
         _err=f3_InsTun("F24L", "F24L", "N")
         If !_Err
            _LxErr = "Error insertando detalle backorder: " + NewPedido + "-" + &AF24l->CodArt
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      Else
         _Where = "F24lCodPro=?CodPro And " + ; 
                  "F24lTipDoc=?_TipDoc And " + ; 
                  "F24lNumDoc=?NewPedido And " + ;
                  "F24lLinDoc=?_LinDoc"
         _err=f3_UpdTun("F24L", , , , , _Where, "N", "N")
         If !_Err
            _LxErr = "Error actualizando detalle backorder: " + NewPedido + "-" + &AF24l->CodArt
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf

      *> Leer la siguiente línea del pedido.
      Select(AF24l)
      Continue
   EndDo

   *> Comprobar que el pedido no se queda sin líneas de detalle.
   _Where = "F24lCodPro=?CodPro And " + ;
            "F24lTipDoc=?_TipDoc And " + ;
            "F24lNumDoc=?NewPedido"

   =f3_sql('*', 'F24L', _Where, '', '', 'F24L')
   Select F24l
   Go Top
   If Eof()
      m.F24cCodPro=CodPro
      m.F24cTipDoc=_TipDoc
      m.F24cNumDoc=NewPedido
      =f3_baja('F24C')

      Select Incidencias
      Append Blank
      Replace TipTras With 'Pedidos', ;
              Campo   With 'Detalle', ;
              Codigo  With NewPedido, ;
              Descri  With 'Cancelar BackOrder'

      =SqlRollBack(_ASql)
      Return
   EndIf

   *> Actualizar la ubicación de muelle, según haya productos de frío o no.
   If l_HayFri .And. !Empty(_UbiExpTrf) .And. !IsNull(_UbiExpTrf)
      m.F24cCodPro = CodPro
      m.F24cTipDoc = _TipDoc
      m.F24cNumDoc = NewPedido

      If !f3_seek('F24C')
         _LxErr = "Error actualizando muelle pedido: " + _NumDoc + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf

      _Where = "F24cCodPro=?CodPro  And " + ; 
               "F24cTipDoc=?_TipDoc And " + ; 
               "F24cNumDoc=?NewPedido"

      If !f3_UpdTun('F24c', ,'F24cUbiExp', '_UbiExpTrf', , _Where, 'N')
         _LxErr = "Error actualizando muelle frío pedido: " + NewPedido + cr
         =Anomalias()
         =SqlRollBack(_ASql)
         Return
      EndIf
   EndIf

   *> Grabación de las observaciones del pedido.
   *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.
   *> Como pueden venir varias líneas de observaciones, en primer
   *> lugar se borran las ya existentes.

   *> Ancho máximo de la línea.
   _oldMemoW = Set('MEMOWIDTH')
   Set MemoWidth To 60

   Select (AF24o)
   Locate For NumDoc = AllTrim(_NumDoc)
   Do While Found()
      _LinDoc = Right(&AF24o->LinDoc, 4)
      If Val(_LinDoc) > 900
         _LinDoc = '9999'
      EndIf

      m.F24oCodPro = CodPro
      m.F24oTipDoc = _TipDoc
      m.F24oNumDoc = NewPedido
      m.F24oLinObs = _LinDoc

      _Observ = &AF24o->Observ

      _ok = f3_seek('F24O')

      *> Si es una línea de detalle, borrar.
      If _LinDoc > '0000' .And. _LinDoc < '9000' .And. _ok
         =f3_baja('F24O')
         _ok = .F.
      EndIf

      *> Si es primera línea de observaciones, borrar.
      If SubStr(_Observ, 1, 1)=='@' .And. _ok
         =f3_baja('F24O')
         _Observ = SubStr(_Observ, 2)
         _ok = .F.
      EndIf       

      Select F24o
      If !_ok
         Zap
         Append Blank
         Replace F24oCodPro With CodPro, ;
                 F24oTipDoc With _TipDoc, ;
                 F24oNumDoc With NewPedido, ;
                 F24oLinObs With _LinDoc
         Accion='INSERT'
      Else
         Accion='UPDATE'
      EndIf

      *> Grabar el resto de los campos de observaciones.
      _Imp1 = Iif(AtC('A', &AF24o->Posici)>0, 'S', 'N')
      _Imp2 = Iif(AtC('F', &AF24o->Posici)>0, 'S', 'N')
      _Imp3 = Iif(AtC('L', &AF24o->Posici)>0, 'S', 'N')
      _Imp4 = Iif(AtC('P', &AF24o->Posici)>0, 'S', 'N')
      _Imp5 = Iif(AtC('E', &AF24o->Posici)>0, 'S', 'N')
      _Imp6 = Iif(AtC('H', &AF24o->Posici)>0, 'S', 'N')

      If MemLines(F24oDesObs) < 6
         Replace F24oDesObs With Iif(Len(F24oDesObs) > 0, ;
                                     F24oDesObs + cr + AllTrim(Left(_Observ, 59)), ;
                                     AllTrim(Left(_Observ, 59))), ;
                 F24oImpObs With _Imp1 + _Imp2 + _Imp3 + _Imp4 + _Imp5 + _Imp6
      EndIf

      Replace F24oDesObs With StrTran(F24oDesObs, "'", Space(1))

      *> Grabar/Actualizar las observaciones.
      If Accion = 'INSERT'
         _err=f3_InsTun("F24O", "F24O", "N")
         If !_Err
            _LxErr = "Error insertando observaciones backorder: " + NewPedido
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      Else
         _Where = "F24oCodPro=?CodPro And " + ; 
                  "F24oTipDoc=?_TipDoc And " + ; 
                  "F24oNumDoc=?NewPedido And " + ;
                  "F24oLinObs=?_LinDoc"
         _err=f3_UpdTun("F24O", , , , , _Where, "N", "N")
         If !_Err
            _LxErr = "Error actualizando observaciones backorder: " + NewPedido
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf

      *> Ver si viene el Nº de vale.
      If Val(_LinDoc) > 0 .And. Val(_LinDoc) < 9000
         _ValeEstupefacientes = Left(&AF24o->Observ, 12)

         *> Estupefacientes sin Nº de vale.
         If &AF24c->CtrDro=='S' .And. Empty(_ValeEstupefacientes)
            Select Incidencias
            Append Blank
            Replace TipTras With 'Pedidos', ;
                    Campo   With 'Nº Vale', ;
                    Codigo  With NewPedido, ;
                    Descri  With 'Estupefacientes sin vale'
         EndIf

         *> Nº de vale sin estupefacientes.
         If &AF24c->CtrDro#'S' .And. !Empty(_ValeEstupefacientes)
            Select Incidencias
            Append Blank
            Replace TipTras With 'Pedidos', ;
                    Campo   With 'Nº Vale', ;
                    Codigo  With NewPedido, ;
                    Descri  With 'Vale sin estupefacientes'
         EndIf

         *> Grabar el nº de vale en la línea de detalle del pedido.
         m.F24lCodPro = CodPro
         m.F24lTipDoc = _TipDoc
         m.F24lNumDoc = NewPedido
         m.F24lLinDoc = _LinDoc

         If !f3_seek('F24L')
            Select Incidencias
            Append Blank
            Replace TipTras With 'Pedidos', ;
                    Campo   With 'Nº Vale', ;
                    Codigo  With _NumDoc, ;
                    Descri  With 'Error detalle pedido'
         Else
            Select F24l
            Replace F24lNumDrg With _ValeEstupefacientes
            _Where = "F24lCodPro=?CodPro And " + ; 
                     "F24lTipDoc=?_TipDoc And " + ; 
                     "F24lNumDoc=?NewPedido And " + ;
                     "F24lLinDoc=?_LinDoc"
            If !f3_UpdTun("F24l", ,"F24lNumDrg", , , _Where, 'N')
               _LxErr = "Error actualizando vale estupefacientes BackOrder: " + NewPedido + cr
               =Anomalias()
               =SqlRollBack(_ASql)
               Return
            EndIf
         EndIf
      EndIf

      *>
      Select (AF24o)
      Continue
   EndDo

   *> Restablecer el ancho standard de campos Memo.
   Set MemoWidth To _oldMemoW

   *> Leer direcciones asociadas a propietario como datos por defecto.
   m.F22pCodPro = CodPro
   m.F22pDirAso = PadR(&AF24c->CodCli, 13, ' ')
   If !f3_seek('F22P')
      Select F22p
      Append Blank
      Replace F22pCodPro With CodPro, ;
              F22pDirAso With &AF24c->CodCli
   EndIf

   *> Grabación de datos de clientes-direcciones.
   *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.
   Select(AF24t)
   Locate For NumDoc = AllTrim(_NumDoc)
   If Found()
      *> Traspasar datos recibidos a ficha F22p.
      Select F22p
      Replace F22pNomAso With &AF24t->NomCli, ;
              F22p1ErDir With &AF24t->NomCl2, ;
              F22p2ndDir With &AF24t->DirCli, ;
              F22p3rdDir With &AF24t->NomCl3, ;
              F22pDPobla With &AF24t->PobCli, ;
              F22pCodPos With &AF24t->CodPos, ;
              F22pCProvi With PadL(Left(&AF24t->CodPos, 2), 4, '0'), ;
              F22pCodNif With Left(&AF24t->NIFCli,15), ;
              F22pCodPas With &AF24t->CodPai

      *> Obtener la provincia.
      m.F00jCodPrv = PadL(Left(&AF24t->CodPos, 2), 4, '0')
      If f3_seek('F00J')
         Replace F22p.F22pDProvi With Left(F00jDescri, 35)
      Else
         Replace F22p.F22pDProvi With Space(35)
      EndIf

      *> Eliminar apóstrofos de la descripción.
      Replace F22pNomAso With StrTran(F22pNomAso, "'", Space(1)), ;
              F22p1erDir With StrTran(F22p1erDir, "'", Space(1)), ;
              F22p2ndDir With StrTran(F22p2ndDir, "'", Space(1)), ;
              F22p3rdDir With StrTran(F22p3rdDir, "'", Space(1)), ;
              F22pDPobla With StrTran(F22pDPobla, "'", Space(1)), ;
              F22pDProvi With StrTran(F22pDProvi, "'", Space(1))

      *> Rellenar datos de dirección en el pedido.
      m.F24tCodPro = CodPro
      m.F24tTipDoc = _TipDoc
      m.F24tNumDoc = NewPedido
      _ok = f3_seek('F24T')

      *> No existen los datos de cliente: crear.-------------------------------------
      Select F24t              
      If !_ok
         Append Blank
         Replace F24tCodPro With CodPro, ;
                 F24tTipDoc With _TipDoc, ;
                 F24tNumDoc With NewPedido
         Accion='INSERT'
      Else
         Accion='UPDATE'
      EndIf

      *> Grabar el resto de los campos de direcciones.
      Replace F24tNomAso With F22p.F22pNomAso, ;
              F24t1ErDir With F22p.F22p1erDir, ; 
              F24t2ndDir With F22p.F22p2ndDir, ; 
              F24t3rdDir With F22p.F22p3rdDir, ; 
              F24tDPobla With F22p.F22pDPobla, ; 
              F24tDProvi With F22p.F22pDProvi, ; 
              F24tCodPos With F22p.F22pCodPos, ; 
              F24tCProvi With F22p.F22pCProvi, ; 
              F24tCodPas With F22p.F22pCodPas, ; 
              F24tNumTel With F22p.F22pTelefo, ; 
              F24tNumFax With F22p.F22pNumFax, ; 
              F24tNumNif With F22p.F22pCodNif, ;
              F24tFlete  With F22p.F22pFlete, ;
              F24tExtrad With F22p.F22pExtrad

      *> Grabar / actualizar direcciones.
      If Accion='INSERT'
         _err=f3_InsTun("F24T", "F24T", "N")
         If !_Err
            _LxErr = "Error insertando direcciones pedido: " + NewPedido
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      Else
         _Where = "F24tCodPro=?CodPro And " + ; 
                  "F24tTipDoc=?_TipDoc And " + ; 
                  "F24tNumDoc=?NewPedido"
         _err=f3_UpdTun("F24T", , , , , _Where, "N", "N")
         If !_Err
            _LxErr = "Error actualizando direcciones pedido: " + NewPedido
            =Anomalias()
            =SqlRollBack(_ASql)
            Return
         EndIf
      EndIf *bucle de ins/upd direc
   EndIf * If Found()

   *> Finalmente, si todo es OK, grabar el pedido.
   =SqlCommit(_ASql)

Return

******************************************************************************
*> ---------------------------------------------------------------------------
*>                       IMPORTACION DE COMPRAS                              *
*> Implementar control pedido anulado. AVC - 23.02.2000                      *
*> Implantar tratamiento multi-fichero. AVC - 09.03.2000                     *
*> Situación de stock por defecto, ctrcal. AVC - 13.03.2000                  *
*> Tipo documento pasa a ser de 2c. AVC - 06.09.2000                         *
*> Permitir reabrir pedidos anulados, siempre que no se haya procesado, es   *
*> decir, la cantidad servida sea cero. AVC - 23.11.2000                     *
*> ---------------------------------------------------------------------------
******************************************************************************

Procedure ImpCom

Private FilImp
Private Accion
DirCom = m.DirCom

Set Separator To '.'
Set Point To ','
Set Date  To DMY
Set Century On

   *> Primera parte: Agrupar y omprobar que existan los ficheros de traspaso.
   FilImp = GroupFiles(AllTrim(DirCom))
   If !File(FilImp)
       _LxErr = "No se ha encontrado el fichero ASCII de Pedidos (Compras) " + cr
       =Anomalias()
       Return
   EndIf

   *> Fichero de pedidos (Compras).-----------------------------------------------------------
   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F18c = AllTrim(Sys(2015))
   Create Cursor (_F18c) ;
     (TipDoc C(4), ;
      NumDoc C(13), ;
      CodPrv C(6), ;
      LinDoc C(4), ;
      CodArt C(13), ;
      CanFis C(10), ;
      FecPre C(8), ;
      CtrAnu C(1))

   AF18c = Alias(_F18c)
   Select (AF18c)
   Append From &FilImp Type SDF

   *> Eliminar registros de control.
   Select (AF18c)
   Delete For !IsDigit(NumDoc)

   *> Realizar la importacion sobre fichero.-----------------------------------------------
   Go Top
   Do While !Eof()
      Wait Window  "Leyendo pedido: " + &AF18c->NumDoc + " de fecha: " + DToC(SToD(&AF18c->FecPre)) NoWait

      Selec F18c
      Zap

      _NumDoc = PadR(&AF18c->NumDoc, 13, ' ')

      *>  Ver si el pedido ya existe en PROCAOT.-----------------------------------------------
      m.F18cCodPro = CodPro
      m.F18cTipDoc = '1000'
      m.F18cNumDoc = _NumDoc

      _ok = f3_seek('F18C')

       *> No existe el pedido: Crear y mover campos.-----------------------------------------
       Select F18c               
       If !_ok
          Append Blank
          Replace F18cCodPro With CodPro
          Replace F18cTipDoc With '1000'
          Replace F18cNumDoc With _NumDoc

          *> Valores por defecto de la cabecera del documento.
          Replace F18cFecPed With Date(), ;
                  F18cFecCie With {  .  .  }, ;
                  F18cNumTra With 0, ;
                  F18cCodImp With Space(1), ;
                  F18cNumPed With Space(10), ;
                  F18cBookin With Space(10)
          Accion='INSERT'
       Else
          *> El pedido ya existe: Comprobar si su estado permite la modificación.------------
          If F18cEstado > '1' .And. &AF18c->CtrAnu # 'S'
             Select Incidencias
             Append Blank
        	 Replace TipTras With 'Pedidos', ;
          	         Campo   With 'Cabecera', ;
           	         Codigo  With &AF18c->NumDoc, ;
           	         Descri  With 'Estado de pedido no modificable'
          Endif
          Accion='UPDATE'
       EndIf

       *> Reemplaza datos de la tabla.-------------------------------------------------------
       _FecPre = SubStr(&AF18c->FecPre, 7, 2) + '/' + ;
                 SubStr(&AF18c->FecPre, 5, 2) + '/' + ;
                 SubStr(&AF18c->FecPre, 1, 4)

       Select F18c
       Replace F18cTipEnt With 'PROV', ;
               F18cCodEnt With Right(&AF18c->CodPrv, 6), ;
               F18cFecPre With CToD(_FecPre), ;
               F18cAlmEnt With _Alma, ;
               F18cEstado With '0'

       *> Crear / Actualizar la cabecera del pedido.----------------------------------------
       *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.--------------------
       If Accion='INSERT'
          _err=f3_InsTun("F18c", "F18c", "N")
          If !_Err
             _LxErr = "Error insertando cabecera pedido: " + &AF18c->NumDoc
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       Else
          _Where = "F18cCodPro='" + CodPro + "' And " + ; 
                   "F18cTipDoc='1000'" + " And " + ; 
                   "F18cNumDoc='" + PadR(&AF18c->NumDoc, 13) + "'"
          _err=f3_UpdTun("F18c", , , , , _Where, "N", "N")
          If !_Err
             _LxErr = "Error actualizando cabecera pedido: " + &AF18c->NumDoc + cr
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       EndIf *bucle de ins/upd cabecera

       *> Grabar la línea de detalle (posiciones) de ESTE pedido.-------------------------
       *> OJO !!! Diferir el COMMIT hasta comprobar que el pedido es OK.--------------------
       m.F18lCodPro = CodPro
       m.F18lTipDoc = '1000'
       m.F18lNumDoc = PadR(&AF18c->NumDoc, 13, ' ')
       m.F18lLinDoc = Right(&AF18c->LinDoc, 4)
       _ok = f3_seek('F18L')

       Select F18l
       If !_ok
          Append Blank
          Replace F18lCodPro With CodPro, ;
                  F18lTipDoc With '1000', ;
                  F18lNumDoc With &AF18c->NumDoc, ;
                  F18lLinDoc With Right(&AF18c->LinDoc, 4), ;
                  F18lNumLot With Space(15), ;
                  F18lLinPed With 0, ;
                  F18lCanSer With 0, ;
                  F18lCanPte With 0
          Accion='INSERT'
       Else
          *> La línea ya existe: Comprobar si su estado permite la modificación.------------
          If F18lEstado > '0' .And. &AF18c->CtrAnu # 'S' .And. F18lCanSer > 0
             Select Incidencias
             Append Blank
        	 Replace TipTras With 'Pedidos', ;
          	         Campo   With 'Detalle', ;
           	         Codigo  With &AF18c->NumDoc, ;
           	         Descri  With 'Estado de línea no modificable'

             Select (AF18c)
             Skip
           	 Loop
          Endif
          Accion='UPDATE'
       EndIf  

       *> Reemplaza datos de la tabla.-----------------------------------------------------
       _CodArt = &AF18c->CodArt
     *  _CodArt = Iif(IsAlpha(_CodArt), AllTrim(_CodArt), Right(_CodArt, 6))

       *> Buscar el código en la tabla. Tomar propietario por defecto.
       m.F08cCodPro = CodPro
       m.F08cCodArt = PadR(_CodArt, 13)
       If !f3_seek('F08C')
          Select Incidencias
          Append Blank
          Replace TipTras With 'Compras', ;
          	      Campo   With 'Detalle', ;
           	      Codigo  With &AF18c->NumDoc, ;
           	      Descri  With 'Artículo ' + _CodArt + ' inexistente'

          Select (AF18c)
          Skip
          Loop
       EndIf

       _FecPre = SubStr(&AF18c->FecPre, 7, 2) + '/' + ;
                 SubStr(&AF18c->FecPre, 5, 2) + '/' + ;
                 SubStr(&AF18c->FecPre, 1, 4)

       Replace F18lFecPre With CToD(&AF18c->FecPre), ;
               F18lCodArt With _CodArt, ;
               F18lFecCad With {  /  /    }, ;
               F18lCanPed With Val(&AF18c->CanFis), ;
               F18lSitStk With Iif(F08c.F08cCalida=='S', _StkCal, _StkDis), ;
	           F18lEstado With Iif(&AF18c->CtrAnu # 'S', '0', '3')

       If Accion='INSERT'
          _err=f3_InsTun("F18L", "F18L", "N")
          If !_Err
             _LxErr = "Error insertando detalle pedido: " + &AF18c->NumDoc + "-" + &AF18c->CodArt + cr
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       Else
          _Where = "F18lCodPro='" + CodPro + "' And " + ; 
                   "F18lTipDoc='1000'" + " And " + ; 
                   "F18lNumDoc='" + _NumDoc + "' And " + ;
                   "F18lLinDoc='" + Right(&AF18c->LinDoc, 4) + "'"
          _err=f3_UpdTun("F18L", , , , , _Where, "N", "N")
          If !_Err
             _LxErr = "Error actualizando detalle pedido: " + &AF18c->NumDoc + "-" + &AF18c->CodArt + cr
             =Anomalias()
             =SqlRollBack(_ASql)
             Return
          EndIf
       EndIf *bucle de ins/upd líneas

       *> Actualizar el estado de la cabecera del pedido, según estado líneas.
       _Selec = "Select * From F18l" + _em + ;
                " Where F18lCodPro='" + m.CodPro + "' And " + ;
                "       F18lTipDoc='1000'" + " And " + ; 
                "       F18lNumDoc='" + _NumDoc + "'"

       _err = f3_SqlExec(_ASql, _Selec, 'F18lCur')
       If _err <= 0
          _LxErr = "Error actualizando cabecera pedido (lectura líneas) " + &AF18c->NumDoc + cr
          =Anomalias()
          Return
       EndIf

       Select F18lCur
       Count To _Total
       Count For F18lEstado='0' To _Estado0
       Count For F18lEstado>'0' To _Estado1
       Count For F18lEstado='3' To _Estado2

       _Estado = '0'
       Do Case
          Case _Estado2 >= _Total
             _Estado = '3'
          Case _Estado1 > 0
             _Estado = '1'
       EndCase

       _Selec = "Update F18c" + _em + ;
                " Set F18cEstado='" + _Estado + "'" + ;
                " Where F18cCodPro='" + m.CodPro + "' And " + ;
                "       F18cTipDoc='1000'" + " And " + ; 
                "       F18cNumDoc='" + _NumDoc + "'"

       _err = f3_SqlExec(_ASql, _Selec)
       If _err <= 0
          _LxErr = "Error actualizando cabecera pedido (actualizar estado) " + &AF18c->NumDoc + cr
          =Anomalias()
          Return
       EndIf

       *> Leer la siguiente cabecera del pedido.
       Select (AF18c)
       Skip
    EndDo *bucle de cabecera
 
   Use In (AF18c)
   Delete File(FilImp)
   If MessageBox("Borrar archivo importación COMPRAS", 4 + 32 + 0) = 6
      DeleteGroupFiles(AllTrim(DirCom))
   EndIf

 Return

***********************************************************************************************
*> -------------------------------------------------------------------------------------------*
*>                      IMPORTACION DE ESTADO DE ARTICULOS                                    *
*> Implantar tratamiento multi-fichero. AVC - 09.03.2000                                      *
*> Tratamiento muestroteca. AVC - 20.09.2000                                                  *
*> Tratamiento materiales bloqueados.                                                         *
*> -------------------------------------------------------------------------------------------*
***********************************************************************************************

Procedure ImpEst

Private FilImp, _CodArt, _NumLot, _Estado
Local _Cantid, _CantidadRestante

DirPro = m.DirEst

Set Separator To '.'
Set Point To ','
Set Date  To DMY
Set Century On

   *> Fichero de estado de artículos (control de calidad) ---------------------
   FilImp = GroupFiles(AllTrim(DirPro))
   If !File(FilImp)
      _LxErr = "No se ha encontrado el fichero ASCII de Estado de artículos" + cr
      =Anomalias()
      Return
    EndIf      

   _LxErr = ''

   *> Comprobar que existen los tipos de movimiento.
   m.F00bCodMov = '3110'
   If !f3_seek('F00B')
      _LxErr = _LxErr + 'No existe el tipo de movimiento 3110 en textos y constantes' + cr
   EndIf

   m.F00bCodMov = '3610'
   If !f3_seek('F00B')
      _LxErr = _LxErr + 'No existe el tipo de movimiento 3610 en textos y constantes' + cr
   EndIf

   *> Comprobar que existen las situaciones de stock.
   m.F00cCodStk = '9100'
   If !f3_seek('F00C')
      _LxErr = _LxErr + 'No existe la situación de stock 9100 en textos y constantes' + cr
   EndIf

   m.F00cCodStk = '8000'
   If !f3_seek('F00C')
      _LxErr = _LxErr + 'No existe la situación de stock 8000 en textos y constantes' + cr
   EndIf

   m.F00cCodStk = '8001'
   If !f3_seek('F00C')
      _LxErr = _LxErr + 'No existe la situación de stock 8001 en textos y constantes' + cr
   EndIf

   m.F00cCodStk = '1000'
   If !f3_seek('F00C')
      _LxErr = _LxErr + 'No existe la situación de stock 1000 en textos y constantes' + cr
   EndIf

   m.F00cCodStk = '1001'
   If !f3_seek('F00C')
      _LxErr = _LxErr + 'No existe la situación de stock 1001 en textos y constantes' + cr
   EndIf

   *> Chequeo de incidencias.
   If !Empty(_LxErr)
      =Anomalias()
      Return
   EndIf

   *> Traspasar los datos del *.TXT a cursor de trabajo.
   _F08c = AllTrim(Sys(2015))
   Create Cursor (_F08c) ;
     (CodArt C(13), ;
      NumLot C(15), ;
      Estado C(1), ;
      Cantid C(13))

   AF08c = Alias(_F08c)
   Select (AF08c)
   Append From &FilImp Type SDF
  
   *> Crear objetos actualización.---------------------------------------
   FncA = CreateObject('OraFncActz')
   PrmA = CreateObject('OraPrmActz')
   FncA.ObjParm = PrmA

   *> Realizar la importación de control de calidad<-->Disponible.------------
   Select (AF08c)
   Go Top
   Do While !Eof()
      *> Cantidad total a actualizar.
      _CantidadRestante = Val(&AF08c->Cantid)

      Select F08c
      Zap
           
      _CodArt = &AF08c->CodArt
      _NumLot = &AF08c->NumLot
      _Estado = &AF08c->Estado

      * Buscar el código de artículo en el maestro.
      m.F08cCodPro = m.CodPro
      m.F08cCodArt = _CodArt
      _ok = f3_seek('F08c')
      If !_ok
         Select Incidencias
         Append Blank
         Replace TipTras With 'Estado', ;
                 Campo   With 'Artículo', ;
                 Codigo  With &AF08c->CodArt, ;
                 Descri  With 'Artículo inexistente'

         Select (AF08c)
         Skip
         Loop
      EndIf

      Wait Window  "Leyendo ocupaciones artículo: " +  _CodArt + "-" + AllTrim(F08c.F08cDescri) + ' - ' + _NumLot NoWait

      *> Generar tabla con las ocupaciones del artículo/lote.-------------------------------
      _Selec = "SELECT * FROM F16c" + _em + ", F10c" + _em
      _Where = "WHERE F16cCodPro='" + m.CodPro + "'" + ;
               " AND   F16cCodArt='" + _CodArt + "'" + ;
               " AND   F16cNumLot='" + _NumLot + "'" + ;
               " AND   F16cCanRes=0" + ;
               " AND   F10cCodUbi=F16cCodUbi"
      _Order = 'ORDER BY F16cSitStk'

      *> Situación de stock a procesar.
      Do Case
         *> Pasar a bloqueado.
         Case _Estado=='B'
            _Where = _Where + " AND F16cSitStk LIKE '100%'"

         *> Pasar a control de calidad.
         Case _Estado=='C'
            _Where = _Where + " AND F16cSitStk LIKE '100%'"

         *> Pasar a disponible.
         Case _Estado=='L'
            _Where = _Where + " AND F16cSitStk LIKE '800%'"
      EndCase

      _Selec = _Selec + Space(1) + _Where + Space(1) + _Order
      Err = f3_SqlExec(_ASql, _Selec, '_F08l')
      If Err <= 0
         _LxErr = "Error al cargar las OCUPACIONES del artículo: " + _CodArt + cr + ;
                  "MENSAJE: " + Message() + cr
         =Anomalias()
         Return
      EndIf

      Select _F08l
      Go Top
      If Eof()
         Select Incidencias
         Append Blank
         Replace TipTras With 'Estado', ;
                 Campo   With 'Artículo', ;
                 Codigo  With _CodArt, ;
                 Descri  With 'Artículo/Lote sin ocupaciones'

         Select (AF08c)
         Skip
         Loop
      Else
         Do While !Eof()
            Wait Window  "Actualizando artículo: " + ;
                         _CodArt + "-" + AllTrim(F08c.F08cDescri) + ' - ' + ;
                         _NumLot + "-" +  _F08l.F16cCodUbi NoWait

            *> Asignar parámetros generales de actualización.
            FncA.ObjParm.Inicializar
            FncA.ObjParm.PUFlag = 'S'                   && Flag actualizar ubicación
            FncA.ObjParm.POFlag = 'S'                   && Flag actualizar ocupación
            FncA.ObjParm.PSFlag = 'S'                   && Flag actualizar ubicación
            FncA.ObjParm.PMFgMP = 'N'                   && Flag actualizar MPs
            FncA.ObjParm.PMFgHM = 'S'                   && Flag actualizar histórico

            _Cantid = _F08l.F16cCanFis
            If _Cantid > _CantidadRestante
               _Cantid = _CantidadRestante
            EndIf

            If _Cantid = 0
               Select _F08l
               Skip
               Loop
            EndIf

            Do Case
               *> Pasar a bloqueado.
               Case _Estado=='B'
                  _StkOri = _F08l.F16cSitStk
                  _StkDes = '9100'
                  _MvtOri = '3610'
                  _MvtDes = '3110'

               *> Pasar a control de calidad.
               Case _Estado=='C'
                  _StkOri = _F08l.F16cSitStk
                  _StkDes = '8000'
                  _MvtOri = '3610'
                  _MvtDes = '3110'

                  *> Muestroteca.
                  Do Case
                     Case _StkOri = '1001'
                       _StkDes = '8001'
                  EndCase

               *> Pasar a disponible.
               Case _Estado=='L'
                  _StkOri = _F08l.F16cSitStk
                  _StkDes = '1000'
                  _MvtOri = '3610'
                  _MvtDes = '3110'

                  *> Muestroteca.
                  Do Case
                     Case _StkOri = '8001'
                       _StkDes = '1001'
                  EndCase

               *> Error.
               Otherwise
                  Select _F08l
                  Skip
                  Loop
            EndCase

            FncA.ObjParm.PTAcc = '08'                   && Operación: Salida

            *> Asignar parámetros situación de stock.
            FncA.ObjParm.PSCPro = m.CodPro              && Código propietario
            FncA.ObjParm.PSCArt = _CodArt               && Código artículo
            FncA.ObjParm.PSCAlm = _Alma                 && Código almacén
            FncA.ObjParm.PStOld = _StkOri               && Situación stock antigua
            FncA.ObjParm.PSCFis = _Cantid               && Cantidad

            *> Asignar datos para grabar histórico de movimientos.
            FncA.ObjParm.PMNmov = Ora_NewHM()           && Nº de Movimiento HM
            FncA.ObjParm.PMMvMP = FncA.ObjParm.PMNmov   && Nº de Movimiento MP
            FncA.ObjParm.PMEnSa = 'S'                   && Salida
            FncA.ObjParm.PMTMov = _MvtOri               && Tipo Movimiento
            FncA.ObjParm.POFMov = Date()                && Fecha movimiento
            FncA.ObjParm.POFMovOra = Date()             && Fecha movimiento Oracle
            FncA.ObjParm.POCAlm = _Alma                 && Almacén
            FncA.ObjParm.POCPro = m.CodPro              && Propietario
            FncA.ObjParm.POCArt = _CodArt               && Artículo
            FncA.ObjParm.PONLot = _NumLot               && Nº de Lote
            FncA.ObjParm.POSStk = _StkOri               && Situación de stock
            FncA.ObjParm.POFCad = _F08l.F16cFecCad      && Fecha caducidad
            FncA.ObjParm.POFCadOra = _F08l.F16cFecCad   && Fecha caducidad Oracle
            FncA.ObjParm.POCFis = _Cantid               && Cantidad
            FncA.ObjParm.POCUbi = _F08l.F16cCodUbi      && Ubicación
            FncA.ObjParm.PUbOld = _F08l.F16cCodUbi      && Ubicación
            FncA.ObjParm.POTPal = _F08l.F16cTamPal      && Tamaño palet
            FncA.ObjParm.PONPal = _F08l.F16cNumPal      && Nº de Palet
            FncA.ObjParm.POFUni = _F08l.F16cUniVen      && Factor unitario
            FncA.ObjParm.POFSer = _F08l.F16cUniPac      && Factor servicio
            FncA.ObjParm.POFEnv = _F08l.F16cPacCaj      && Factor envase
            FncA.ObjParm.POFPal = _F08l.F16cCajPal      && Factor palet
            FncA.ObjParm.POPico = _F08l.F16cEsPico      && Pico/Palet completo
            FncA.ObjParm.PMFlg1 = '0'                   && Flag

            *> Función general de actualización.
            FncA.Ejecutar
            If FncA.ObjParm.PWCRtn > '49'
               _LxErr = "Error actualizando artículo: " + _CodArt + " (SALIDA) " + cr
               =Anomalias()
               =SqlRollBack(_ASql)
            EndIf

            FncA.ActHM
            If FncA.ObjParm.PWCRtn > '49'      
               _LxErr = 'Error generando registro en histórico de movimientos (SALIDA)' + cr
               =Anomalias()
               =SqlRollBack(_ASql)
               Return
            EndIf

            FncA.ObjParm.PTAcc = '07'                   && Operación: Entrada

            *> Asignar parámetros situación de stock.
            FncA.ObjParm.PStOld = _StkDes               && Situación stock antigua

            *> Asignar datos para grabar histórico de movimientos.
            FncA.ObjParm.PMNmov = Ora_NewHM()           && Nº de Movimiento HM
            FncA.ObjParm.PMMvMP = FncA.ObjParm.PMNmov   && Nº de Movimiento MP
            FncA.ObjParm.PMEnSa = 'E'                   && Entrada
            FncA.ObjParm.PMTMov = _MvtDes               && Tipo Movimiento
            FncA.ObjParm.POSStk = _StkDes               && Situación de stock

            *> Función general de actualización.
            FncA.Ejecutar
            If FncA.ObjParm.PWCRtn > '49'
               _LxErr = "Error actualizando artículo: " + _CodArt + " (ENTRADA) " + cr
               =Anomalias()
               =SqlRollBack(_ASql)
            EndIf

            FncA.ActHM
            If FncA.ObjParm.PWCRtn > '49'      
               _LxErr = 'Error generando registro en histórico de movimientos (ENTRADA)' + cr
               =Anomalias()
               =SqlRollBack(_ASql)
               Return
            EndIf

            *> Actualizar cantidad restante a actualizar.
            _CantidadRestante = _CantidadRestante - _Cantid

            *>
            Select _F08l
            Skip
         EndDo
      EndIf * if !Eof()

      *> Si queda cantidad por actualizar generar incidencia.
      If _CantidadRestante > 0
         Select Incidencias
         Append Blank
         Replace TipTras With 'Estado', ;
                 Campo   With 'Artículo', ;
                 Codigo  With _CodArt, ;
                 Descri  With 'Pendiente: ' + Str(_CantidadRestante, 7, 0)
      EndIf

      *>
      Select (AF08c)
      Skip
  EndDo *bucle de líneas

  =SqlCommit(_ASql)

  *>
  Use In (AF08c)
  Delete File(FilImp)
  If MessageBox("Borrar archivo importación ESTADOS", 4 + 32 + 0) = 6
     DeleteGroupFiles(AllTrim(DirPro))
  EndIf

  Release FncA
  Release PrmA

Return

*>-----------------------------------------------------------------------------------------
*						
************************************************************************************************
**                                                                                            **
**                             P R O C E D I M I E N T O S                                    **
**                                                                                            **
************************************************************************************************

**********************************************************************************************
*> -----------------------------------------------------------------------------------------**
*>                           EXPORTACION DE ALBARANES                                        *
*> Guardar fichero para reenvíos. AVC - 05.05.1999                                           *
*> Tomar datos de F30c. AVC - 16.03.2000                                                     *
*> Corregir error duplicación cuando hay mas de un albarán por documento. AVC - 29.09.2000   *
*> Exportar solo albaranes ya relacionados. AVC - 02.10.2000                                 *
*> Corregir error actualización albaranes traspasados. AVC - 05.10.2000                      *
*> Tener en cuenta los pedidos Back Order. AVC - 17.10.2000                                  *
*> -----------------------------------------------------------------------------------------**
**********************************************************************************************

 Procedure ExpAlb 
 Parameters Reenvio

 Private _Selec, _Where, _Set, _Fp
 Private FilImp
 Private _MbStat, _CodArt, _TipDoc

 DirDoc = m.DirDoc
 
 Set Separator To ','
 Set Point To '.'
 Set Date  To DMY
 Set Century On

 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirDoc)
    _LxErr = "El nombre del fichero de ALBARANES es erróneo" + cr
    =Anomalias()
    Return
 EndIf

 *> Comprobar si existe el fichero.
 _MbStat = IDNO
 FilImp = "'" + AllTrim(DirDoc) + "'"

 If File(&FilImp)
    _MbStat = MessageBox("Ya existe el fichero <" + AllTrim(FIlImp) + ">  Añadir albaranes?", ;
                         MB_YESNOCANCEL + MB_ICONSTOP + MB_DEFBUTTON1, ;
                         "Exportar ALBARANES")

    Do Case
       *> Añadir albaranes al fichero ya existente.
       Case _MbStat == IDYES

       *> Añadir albaranes al fichero ya existente.
       Case _MbStat == IDNO

       *> Cancelar proceso de exportación de albaranes.
       Case _MbStat == IDCANCEL
          Return

       *> Por defecto, cancelar proceso de exportación de albaranes.
       Otherwise
          Return
    EndCase
 EndIf

 *> Formar QUERY con los registros a tratar.------------------------------
 _Selec = "Select F27l" + _em + ".*, F27c" + _em + ".*, F30c" + _em + ".* " + ;
          "From F27l" + _em + ", F27c" + _em + ", F30c" + _em

 _Where = Iif(Type('Reenvio')=='C', "Where F27lNumTrp='" + Reenvio + "'", "Where F27lNumTrp=0")
 _Where = _Where + " And F27cNumAlb=F27lNumAlb And F27cCodPro=F27lCodPro And F27cTipDoc=F27lTipDoc And F27cNumDoc=F27lNumDoc"
 _Where = _Where + " And F30cAlbRep=F27cNumAlb And F30cCodEnt=F27cCodPro And F30cTipDoc=F27cTipDoc And F30cNumDoc=F27cNumDoc"
 _Where = _Where + Iif(Type('Reenvio')=='C','', " And F30cCodSit='0'")
 _Where = _Where + " And F27cCodPro = '" + m.CodPro + "'"

 _F27l = AllTrim(Sys(2015))
 
 _Selec = _Selec + Space(1) + _Where
 Err = f3_SqlExec(_ASql, _Selec, _F27l)
 If Err <= 0
    _LxErr = "Error al cargar los ALBARANES a traspasar" + cr + ;
             "MENSAJE: " + Message() + cr
    =Anomalias()
    Return
 EndIf
 
 AF27l = Alias(_F27l)
 Select (AF27l)
 Go Top
 If Eof()
    _LxErr = "No hay albaranes para exportar" + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 *> Transformar conceptos a formato SAP, según tablas de equivalencias (art, tdoc, ...)
 _F27lTmp = AllTrim(Sys(2015))
 Create Cursor (_F27lTmp) ;
   (TipDoc C(4), ;
    NumDoc C(13), ;
    NumAlb C(13), ;
    LinDoc C(4), ;
    CodArt C(13), ;
    NumLot C(15), ;
    CanPed C(10), ;
    CanSer C(10), ;
    FecTrp C(8))

  AF27lTmp = Alias(_F27lTmp)

 *> Si cal, añadir los registros del fichero ya existente.
 If _MbStat == IDYES
    Select (AF27lTmp)
    Append From &FilImp Type SDF
 EndIf

 Select (AF27l)
 Replace All F27lNumLot With Space(15) For IsNull(F27lNumLot)
 Go Top
 Do While !Eof()
    _CodArt = &AF27l->F27lCodArt
    _TipDoc = &AF27l->F27lTipDoc

    Wait Window "Exportando documento: " + AllTrim(&AF27l->F27lNumDoc) + " Artículo: " + _CodArt NoWait

    *> Leer tipo documento SAP de tabla de equivalencias, F1PD.
    _Where = "F1PDCodPro='" +  m.CodPro + "' And F1PDTipDoc='" + &AF27l->F27lTipDoc + "'"

    If f3_sql('*', 'F1PD', _Where, , , 'F1PD')
       Select F1PD
       _TipDoc = Left(F1PDTdcDes, 4)
    Endif

    *> Grabar datos en cursor para copiar a fichero de traspaso.-----------------------------
    Select (AF27lTmp)
    Append Blank
    Replace TipDoc With _TipDoc, ;
            NumDoc With &AF27l->F27lNumDoc, ;
            NumAlb With &AF27l->F30cAlbRep, ;
            LinDoc With PadL(&AF27l->F27lLinDoc, 6, '0'), ;
            CodArt With _CodArt, ;
            CanPed With PadL(AllTrim(Str(&AF27l->F27lCanPed)), 10, '0'), ;
            CanSer With PadL(AllTrim(Str(&AF27l->F27lCanSer)), 10, '0'), ;
            FecTrp With DToS(&AF27l->F27cFecAlb)

**            NumLot With Left(&AF27l->F27lNumLot, 10), ;

    *>
    Select (AF27l)
    Skip
 EndDo

*> Obtener el Nº de envío de F00A.-------------------------------------------------------
   If Type('Reenvio') # 'C'
     _NumTrp = Ora_NewTRm('N', 'N', 6)
     If Type('_NumTrp') <> 'C'
       _LxErr = "Error al leer Nº de traspaso de albaranes" + cr
       =Anomalias()
       =SqlRollBack(_ASql)
       Return
   EndIf

    *> Actualizar F27l con el Nº de envío.------------------------------------------------
    _Selec = "Update F27l" + _em
    _Set   = "Set F27lNumTrp=" + _NumTrp + "," + ;
             "    F27lFecTrp=" + _GCD(Date())

    _Where = "Where F27lNumTrp=0 And Exists" + Space(1) + ;
             "(Select * From F30c" + _em + Space(1) + ;
             "Where F30cAlbRep=F27lNumAlb And F30cCodSit='0')"

*************
* _Where = Iif(Type('Reenvio')=='C', "Where F27lNumTrp='" + Reenvio + "'", "Where F27lNumTrp=0")
* _Where = _Where + " And F27cCodPro=F27lCodPro And F27cTipDoc=F27lTipDoc And F27cNumDoc=F27lNumDoc"
* _Where = _Where + " And F30cCodEnt=F27lCodPro And F30cTipDoc=F27lTipDoc And F30cNumDoc=F27lNumDoc"
*************
 
    _Selec = _Selec + Space(1) + _Set + Space(1) + _Where
    Err = f3_SqlExec(_ASql, _Selec)
    If Err <= 0
       _LxErr = "Error al actualizar los ALBARANES a traspasar" + cr + ;
                "MENSAJE: " + Message() + cr
       =Anomalias()
       Return
    Else
       *> Guardar fichero para reenvíos.
       _Fp = FCreate('ALB' + Alltrim(_NumTrp) + '.EXP')
       =FClose(_Fp)
    EndIf
 EndIf

 *> Copiar movimientos a fichero de exportación.
 Select (AF27lTmp)
 Copy To (&FilImp) Type SDF

 =MessageBox("Traspaso de ALBARANES finalizado", MB_OK + MB_ICONINFORMATION)
 Use In (AF27L)
 Use In (AF27LTmp)

 Return

************************************************************************************************
*> -------------------------------------------------------------------------------------------**
*>                         EXPORTACION DE DOCUMENTOS EN PREPARACION                            *
*> Marcar documentos enviados. AVC - 19.10.2000                                                *
*> -------------------------------------------------------------------------------------------**
************************************************************************************************

 Procedure ExpEsd 

 Private _Selec, _Where
 Private FilImp
 Private _MbStat

 DirEsd = m.DirEsd
 
 Set Separator To ','
 Set Point To '.'
 Set Date  To DMY
 Set Century On

 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirEsd)
    _LxErr = "El nombre del fichero de ESTADO DOCUMENTOS es erróneo."
    =Anomalias()
    Return
 EndIf

 *> Comprobar si existe el fichero.
 _MbStat = IDNO
 FilImp = "'" + AllTrim(DirEsd) + "'"

 If File(&FilImp)
    _MbStat = MessageBox("Ya existe el fichero <" + AllTrim(FIlImp) + ">  Añadir documentos?", ;
                         MB_YESNOCANCEL + MB_ICONSTOP + MB_DEFBUTTON1, ;
                         "Exportar ESTADO PEDIDOS")

    Do Case
       *> Añadir pedidos al fichero ya existente.
       Case _MbStat == IDYES

       *> Añadir pedidos al fichero ya existente.
       Case _MbStat == IDNO

       *> Cancelar proceso de exportación de estado pedidos.
       Case _MbStat == IDCANCEL
          Return

       *> Por defecto, cancelar proceso de exportación de estado pedidos.
       Otherwise
          Return
    EndCase
 EndIf

 *> Formar QUERY con los registros a tratar.------------------------------
 _Selec = "Select F24cNumDoc From F24c" + _em
 _Where = "Where F24cFlag1 <= '0' And " + ;
          "F24cFlgEst Between '1' And '3' And " + ;
          "F24cCodPro='" + CodPro + "' Order By F24cNumDoc"
 _F24c = AllTrim(Sys(2015))
 
 _Selec = _Selec + Space(1) + _Where

 Wait Window 'Generando fichero traspaso estado pedidos. Un momento ...' NoWait

 Err = f3_SqlExec(_ASql, _Selec, _F24c)
 If Err < 0
    _LxErr = "Error al cargar los PEDIDOS a traspasar" + cr + ;
             "MENSAJE: " + Message() + cr
    =Anomalias()
    Return
 EndIf

 Wait Clear 

 AF24c = Alias(_F24c)
 Select (AF24c)
 Go Top
 If Eof()
    _LxErr = "No hay documentos para exportar" + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 *> Si cal, añadir los registros del fichero ya existente.
 If _MbStat == IDYES
    Select (AF24c)
    Append From &FilImp Type SDF
 EndIf

 *> Copiar documentos a fichero de exportación.
 Copy To (&FilImp) Type SDF
  
 *> Actualizar flag de envío a SAP en cabecera de documentos de salida.
 _Where = "F24cFlag1 <= '0' And F24cCodPro='" + CodPro + "' And F24cFlgEst Between '1' And '3'"
 c_Flag1 = '1'

 If !f3_UpdTun('F24c', ,'F24cFlag1', 'c_Flag1', , _Where, 'N')
    _LxErr = "Error actualizando transportista frío pedido: " + _NumDoc + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 =MessageBox("Traspaso de ESTADO PEDIDOS finalizado", MB_OK + MB_ICONINFORMATION)
 Use In (AF24c)
 Return

************************************************************************************************
*> -------------------------------------------------------------------------------------------**
*>           CONFIRMACION DE CARGA EN BATCH, PREVIO A EXPORTACION DE ALBARANES                 *
*> -------------------------------------------------------------------------------------------**
************************************************************************************************

Procedure ExpCFa

 Private FilImp, FilImpDelete

 DirCFa = m.DirCFa
 
 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirCFa)
    _LxErr = "El nombre del fichero de CONFIRMACION de CARGA es erróneo."
    =Anomalias()
    Return
 EndIf

 If !File(DirCFa)
    _LxErr = "No se ha encontrado el fichero ASCII de confirmación de carga"
    =Anomalias()
    Return
 EndIf

 Set Procedure To SaExCfCa Additive
 Set Procedure To ProcRStk Additive

 FilImp = "'" + AllTrim(DirCFa) + "'"
 FilImpDelete = AllTrim(DirCFa)

 *> Cargar fichero a cursor.
 _F24cTmp = AllTrim(Sys(2015))
 Create Cursor (_F24cTmp) ;
   (CodPro C(6), ;
    TipDoc C(4), ;
    NumDoc C(13), ;
    Estado C(1))

  AF24c = Alias(_F24cTmp)
  Append From &FilImp Type SDF

  Select (AF24c)
  Go Top
  Do While !Eof()
     If !CnfC_ProcDoc(CodPro, TipDoc, NumDoc)

        Select Incidencias
        Append Blank
        Replace TipTras With 'Stocks', ;
                Campo   With 'Carga', ;
                Codigo  With &AF24c->NumDoc, ;
                Descri  With "Documento " + &AF24c->NumDoc + " no confirmada carga"

        =Anomalias()
        =SqlRollBack(_ASql)
        Select (AF24c)
        Skip
        Loop
     EndIf

     *>
     Select (AF24c)
     Skip
  EndDo

*>
Delete File(FilImpDelete)

=MessageBox("Traspaso de CONFIRMACION de CARGA finalizado", MB_OK + MB_ICONINFORMATION)
Use In (AF24c)

Release Procedure SaExCfCa
Release Procedure ProcRStk
Return

**********************************************************************************************
*> -----------------------------------------------------------------------------------------**
*>                       EXPORTACION DE COMPARACION DE STOCKS                                *
*> Agrupar por artículo/lote/stock sap. AVC - 16.03.2000                                     *
*> Exportar equivalencia de stock como almacén SAP. AVC - 16.03.2000, 22.03.2000             *
*> No traspasar movimientos con incidencias. AVC - 04.10.2000                                *
*> No restar cantidad reservada de la física. AVC - 04.10.2000                               *
*> Traspasar solo situaciones '1000', '8000' y '9000'. AVC - 25.10.2000                      *
*> Anulado. Se parametriza por tabla equivalencia. AVC - 25.10.2000                          *
*> -----------------------------------------------------------------------------------------**
**********************************************************************************************

 Procedure ExpStk
 
 Private _Selec, _Where, _From, _Group, _CodArt, _StkFis, _StkRes, _AlmCli
 Private FilImp, _F13c, Err

 DirStk = m.DirStk
 
 Set Separator To ','
 Set Point To '.'
 Set Date  To DMY
 Set Century On

 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirStk)
    _LxErr = "El nombre del fichero de COMPARACION STOCS es erróneo" + cr
    =Anomalias()
    Return
 EndIf

 *> Comprobar si existe el fichero.
 FilImp = "'" + AllTrim(DirStk) + "'"
 If File(&FilImp)
    If MessageBox("Ya existe el fichero <" + AllTrim(FilImp) + ">  Borrar?", 4 + 32 + 0, "Exportar STOCKS") # 6
       Return
    EndIf
 EndIf

  *> Crear cursor temporal.
  _F13cTmp = AllTrim(Sys(2015))
  Create Cursor (_F13cTmp) ;
    (CodAlm C(4), ;
     CodArt C(13), ;
     NumLot C(15), ;
     SitStk C(4), ;
     CanFis C(10), ;
     FecTrp C(8), ;
     HorTrp C(6))
   AF13cTmp = Alias(_F13cTmp)

 *> Formar QUERY con los registros a tratar.------------------------------
 _Selec = "Select " + _GCSS("F16cCodUbi", 1, 4) + " As Almac, " + ;
          "F16cCodArt As Artic, " + ;
          "F16cNumLot As NLote, " + ;
          "F16cSitStk As Stock, " + ;
          "F16cCanFis As CanFis, " + ;
          "F16cCanRes As CanRes"
 _From =  "From F16c" + _em

 _Where = "Where " + _GCSS("F16cSitStk", 1, 1) + " In ('1', '8', '9') And F16cCodPro='" + CodPro + "'"

 _Group = "Group By Almac, Artic, NLote, Stock"
 _F13c = AllTrim(Sys(2015))

 Wait Window "Preparando datos parar traspasar. Un momento ..." NoWait
 
 _Selec = _Selec + Space(1) + _From + Space(1) +  _Where + Space(1)
 Err = f3_SqlExec(_ASql, _Selec, _F13c)
 If Err < 0
    _LxErr = "Error al cargar los STOCKS a traspasar" + cr + ;
             "MENSAJE: " + Message() + cr
    =Anomalias()
    Return
 EndIf
 
 AF13c = Alias(_F13c)
 Select (AF13c)
 Go Top
 If Eof()
    _LxErr = "No hay stocks para exportar" + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 *> Agrupar por artículo/lote los datos leídos.-----------------------------------
 _C13c = AllTrim(Sys(2015))
 Select Almac, Artic, NLote, Stock, Sum(CanFis) As SumCanFis, Sum(CanRes) As SumCanRes ;
    From (AF13c) ;
    Into Cursor (_C13c) ;
    Group By 1, 2, 3, 4
 
 *> Copiar datos a cursor, tranformando artículo, sit. stocks, ...
 CF13c = Alias(_C13c)
 Select(CF13c)
 Go Top
 Do While !Eof()
    Wait Window  "Exportando artículo: " + &CF13c->Artic + "-" + ;
                                           &CF13c->NLote + "-" + ;
                                           &CF13c->Stock  NoWait

    _CodArt = AllTrim(&CF13c->Artic)
    _StkFis = Stock
    _StkRes = "2000"
    _AlmCli = Almac

    *> Leer sit. stock SAP (canfis) de tabla de equivalencias, F1PS.
    m.F1PSCodPro = m.CodPro
    m.F1PSSitStk = &CF13c->Stock
    _ok = f3_seek('F1PS')
    If _ok
       Select F1PS
       _StkFis = Left(F1PSSitDes, 4)
       _AlmCli = F1PSAlmCli
    Endif

    *> Grabar en cursor de traspaso, stock físico.
    Select (AF13cTmp)
    Locate For CodAlm = _AlmCli .And. ;
               CodArt = _CodArt .And. ;
               NumLot = &CF13c->NLote .And. ;
               SitStk = _StkFis

    If Found()
       Replace CanFis With PadL(AllTrim(Str(Val(CanFis) + &CF13c->SumCanFis)), 10, '0')
    Else
       Append Blank
       Replace CodAlm With _AlmCli, ;
               CodArt With _CodArt, ;
               NumLot With &CF13c->NLote, ;
               SitStk With _StkFis, ;
               CanFis With PadL(AllTrim(Str(&CF13c->SumCanFis)), 10, '0'), ;
               FecTrp With DToS(Date()), ;
               HorTrp With Right(TToC(DateTime(), 1), 6)

    EndIf

    *> Grabar en cursor de traspaso, stock reservado, si hay.
    Select(CF13c)
    If SumCanRes > 0
       *> Leer sit. stock SAP (Canres) de tabla de equivalencias, F1PS.
       m.F1PSCodPro = m.CodPro
       m.F1PSSitStk = '2000'
       _ok = f3_seek('F1PS')
       If _ok
          Select F1PS
          _StkRes = Left(F1PSSitDes, 4)

          *> Grabar registro para la cantidad reservada.
          Select (AF13cTmp)
          Append Blank
          Replace CodAlm With _AlmCli, ;
                  CodArt With _CodArt, ;
                  NumLot With &CF13c->NLote, ;
                  SitStk With _StkRes, ;
                  CanFis With PadL(AllTrim(Str(&CF13c->SumCanRes)), 10, '0'), ;
                  FecTrp With DToS(Date()), ;
                  HorTrp With Right(TToC(DateTime(), 1), 6)
       Endif
    EndIf

    *> Leer el siguiente registro de situación de stock.--------------------------
    Select(CF13c)
    Skip
 EndDo
 
 *> Copiar Stocks a fichero de exportación.
 Select (AF13cTmp)
 Copy To (&FilImp) Type SDF
  
 =MessageBox("Traspaso de STOCKS finalizado", MB_OK + MB_ICONINFORMATION)
 Use In (AF13c)
 Use In (CF13c)
 Use In (AF13cTmp)
 Return

**********************************************************************************************
*> -----------------------------------------------------------------------------------------**
*>                        EXPORTACION DE MOVIMIENTOS DE ALMACEN                              *
*> Agrupar Mvtos. por artículo/lote/tipmov/sitstk/numdoc. AVC - 13.03.2000                   *
*> -----------------------------------------------------------------------------------------**
**********************************************************************************************

 Procedure ExpMvt
 Parameters ReEnvio

 Private _Selec, _Where, _Set, _Fp
 Private FilImp, oF02c
 Private _Centro, _SitStk, _MbStat, _TipMov

 DirMvt = m.DirMvt
 
 Set Separator To ','
 Set Point To '.'
 Set Date  To DMY
 Set Century On

 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirMvt)
    _LxErr = "El nombre del fichero de MOVIMIENTOS es erróneo."
    =Anomalias()
    Return
 EndIf

 *> Comprobar si existe el fichero.
 _MbStat = IDNO
 FilImp = "'" + AllTrim(DirMvt) + "'"

 If File(&FilImp)
    _MbStat = MessageBox("Ya existe el fichero <" + AllTrim(FIlImp) + ">  Añadir movimientos?", ;
                         MB_YESNOCANCEL + MB_ICONSTOP + MB_DEFBUTTON1, ;
                         "Exportar MOVIMIENTOS")

    Do Case
       *> Añadir movimientos al fichero ya existente.
       Case _MbStat == IDYES

       *> Añadir movimientos al fichero ya existente.
       Case _MbStat == IDNO

       *> Cancelar proceso de exportación de movimientos.
       Case _MbStat == IDCANCEL
          Return

       *> Por defecto, cancelar proceso de exportación de movimientos.
       Otherwise
          Return
    EndCase
 EndIf

 *> Leer el centro de trabajo.
 m.F02cCodAlm = _Alma
 If !f3_seek('F02C')
    _LxErr = 'No se pudo leer el almacén' + cr
    =Anomalias()
    Return
 EndIf

Select F02c
Scatter Name oF02c

 Wait Window 'Preparando movimientos a exportar. Un momento ...' NoWait

 *> Formar QUERY con los registros a tratar.------------------------------
 _Selec = "Select * From F20c" + _em
 _Where = Iif(Type('Reenvio')=='C', "Where F20cNumEnv=" + Reenvio, "Where F20cFlgEnv<>'S'")
 _Where = _Where + " And " + _GCSS("F20cTipMov", 1, 1) + " In ('1', '2', '3', '4') "
 _F20c = AllTrim(Sys(2015))
 
 _Selec = _Selec + Space(1) + _Where
 Err = f3_SqlExec(_ASql, _Selec, _F20c)
 If Err <= 0
    _LxErr = "Error al cargar los MOVIMIENTOS a traspasar" + cr + ;
             "MENSAJE: " + Message() + cr
    =Anomalias()
    Return
 EndIf
 
 AF20c = Alias(_F20c)
 Select (AF20c)
 Go Top
 If Eof()
    _LxErr = "No hay movimientos para exportar" + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 *> Transformar conceptos a formato SAP, según tablas de equivalencias (art, stk, ...)
 _F20cTmp = AllTrim(Sys(2015))
 Create Cursor (_F20cTmp) ;
   (NumMov C(10), ;
    TipMov C(4), ;
    TipDoc C(4), ;
    NumDoc C(13), ;
    LinDoc C(4), ;
    CodArt C(13), ;
    NumLot C(15), ;
    FecCad C(8), ;
    CodAlm C(4), ;
    SitStk C(4), ;
    CanFis C(10), ;
    FecTrp C(8))
 AF20cTmp = Alias(_F20cTmp)

 *> Si cal, añadir los registros del fichero ya existente.
 If _MbStat == IDYES
    Select (AF20cTmp)
    Append From &FilImp Type SDF
 EndIf

 Select (AF20c)
 Go Top
 Do While !Eof()
    _CodArt = AllTrim(&AF20c->F20cCodArt)
	_Centro = oF02c.F02cCodCen
	_TipMov = &AF20c->F20cTipMov
	_SitStk = &AF20c->F20cSitStk

    *> Leer situación stock SAP de tabla de equivalencias, F1PS para averiguar
    *> el almacén SAP(10, 20, ...), así como la situación (L/C/B).
    m.F1PSCodPro = m.CodPro
    m.F1PSSitStk = &AF20c->F20cSitStk
    _ok = f3_seek('F1PS')
    If _ok
       Select F1PS
       _SitStk = Left(F1PSSitDes, 4)
       _Centro = F1PSAlmCli
    Endif

    *> Leer tipo movimiento SAP de tabla de equivalencias, F1PM.
    m.F1PMCodPro = m.CodPro
    m.F1PMTipMov = &AF20c->F20cTipMov
    m.F1PMSitStk = &AF20c->F20cSitStk

    _ok = f3_seek('F1PM')
    If _ok
       Select F1PM
       _TipMov = Left(F1PMMovDes, 4)
    Endif

    Wait Window "Artículo: " + _CodArt + " - " + "Lote: " + &AF20c->F20cNumLot NoWait

    *> Grabar datos en cursor para copiar a fichero de traspaso.-----------------------------
    Select (AF20cTmp)
    Locate For CodArt = _CodArt .And. ;
               NumLot = &AF20c->F20cNumLot .And. ;
               TipMov = _TipMov .And. ;
               SitStk = _SitStk .And. ;
               NumDoc = &AF20c->F20cNumDoc .And. ;
               Val(LinDoc) = &AF20c->F20cLinDoc

    If Found()
       Replace CanFis With PadL(AllTrim(Str(Val(CanFis) + &AF20c->F20cCanFis)), 10, '0')
    Else
       Append Blank
       Replace NumMov With &AF20c->F20cNumMov, ;
               TipMov With _TipMov, ;
               NumDoc With &AF20c->F20cNumDoc, ;
               LinDoc With PadL(AllTrim(Str(&AF20c->F20cLinDoc, 4, 0)), 4, '0'), ;
               CodArt With _CodArt, ;
               NumLot With &AF20c->F20cNumLot, ;
               FecCad With DToS(&AF20c->F20cFecCad), ;
               CodAlm With _Centro, ;
               SitStk With _SitStk, ;
               CanFis With PadL(AllTrim(Str(&AF20c->F20cCanFis)), 10, '0'), ;
               FecTrp With DToS(Date())
    EndIf

    *>
    Select (AF20c)
    Skip
 EndDo

 *> Obtener el Nº de envío de F00A.-------------------------------------------------------
 If Type('Reenvio') # 'C'
    _NumTrp = Ora_NewTRm('N', 'N', 6)
    If Type('_NumTrp') <> 'C'
       _LxErr = "Error al leer Nº de traspaso de movimientos" + cr
       =Anomalias()
       Return
    EndIf

    *> Actualizar F20c con el Nº de envío.------------------------------------------------
    _Selec = "UPDATE F20c" + _em
    _Set   = "SET F20cFlgEnv='S'," + ;
             " F20cNumEnv=" + _NumTrp + "," + ;
             " F20cFecEnv=" + _GCD(Date())
    _Where = "WHERE F20cFlgEnv <> 'S' " + ;
             "AND " + _GCSS("F20cTipMov", 1, 1) + " IN ('1', '2', '3', '4') "
 
    _Selec = _Selec + Space(1) + _Set + Space(1) + _Where
    Err = f3_SqlExec(_ASql, _Selec)
    If Err < 0
       _LxErr = "Error al actualizar los MOVIMIENTOS a traspasar."
       =Anomalias()
       Return
    Else
       *> Guardar fichero para reenvíos.
       _Fp = FCreate('MVT' + Alltrim(_NumTrp) + '.EXP')
       =FClose(_Fp)
    EndIf
 EndIf

 *> Copiar movimientos a fichero de exportación.
 Select (AF20cTmp)
 Copy To (&FilImp) Type SDF
  
 Wait Clear
 
 =MessageBox("Traspaso de MOVIMIENTOS ALMACEN finalizado", MB_OK + MB_ICONINFORMATION)
 Use In (AF20c)
 Use In (AF20cTmp)
 Return

**********************************************************************************************
*> -----------------------------------------------------------------------------------------**
*>        EXPORTACION DE MOVIMIENTOS DE ALMACEN - FORMATO EXTENDIDO                          *
*> Envío para matching movimientos entre PROCAOT y ERP.                                      *
*> Puede recibir varios ficheros de movimientos, que deberán ser agrupados.                  *
*> -----------------------------------------------------------------------------------------**
**********************************************************************************************

 Procedure ExpMvtExt
 Parameters ReEnvio, PrimeraVez

 Private _Selec, _Where, _Set, _Fp
 Private FilImp, oF02c
 Private _Centro, _MbStat, _TipMov

 DirMvt = m.DirMvt
 
 Set Separator To ','
 Set Point To '.'
 Set Date  To DMY
 Set Century On

 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirMvt)
    _LxErr = "El nombre del fichero de reenvío de MOVIMIENTOS es erróneo" + cr
    =Anomalias()
    Return
 EndIf

 *> Comprobar si existe el fichero.
 FilImp = "'" + AllTrim(DirMvt) + "'"
 _MbStat = IDNO

 If File(&FilImp) .And. PrimeraVez
    _MbStat = MessageBox("Ya existe el fichero <" + AllTrim(FIlImp) + ">  Añadir movimientos?", ;
                         MB_YESNOCANCEL + MB_ICONSTOP + MB_DEFBUTTON1, ;
                         "Exportar MOVIMIENTOS")

    Do Case
       *> Añadir movimientos al fichero ya existente.
       Case _MbStat == IDYES

       *> Añadir movimientos al fichero ya existente.
       Case _MbStat == IDNO

       *> Cancelar proceso de exportación de movimientos.
       Case _MbStat == IDCANCEL
          Return

       *> Por defecto, cancelar proceso de exportación de movimientos.
       Otherwise
          Return
    EndCase
 EndIf

 PrimeraVez = .F.

 *> Leer el centro de trabajo.
 m.F02cCodAlm = _Alma
 If !f3_seek('F02C')
    _LxErr = 'No se pudo leer el almacén' + cr
    =Anomalias()
    Return
 EndIf

 Scatter Name oF02c
 
 Wait Window 'Preparando movimientos a exportar. Un momento ...' NoWait

 *> Formar QUERY con los registros a tratar.------------------------------
 _Selec = "Select * From F20c" + _em
 _Where = Iif(Type('Reenvio')=='C', "Where F20cNumEnv=" + Reenvio, "Where F20cFlgEnv<>'S'")
 _Where = _Where + " And " + _GCSS("F20cTipMov", 1, 1) + " In ('1', '2', '3', '4') "
 _F20c = AllTrim(Sys(2015))
 
 _Selec = _Selec + Space(1) + _Where
 Err = f3_SqlExec(_ASql, _Selec, _F20c)
 If Err < 0
    _LxErr = "Error al cargar los MOVIMIENTOS a traspasar" + cr + ;
             "MENSAJE: " + Message() + cr
    =Anomalias()
    Return
 EndIf
 
 AF20c = Alias(_F20c)
 Select (AF20c)
 Go Top
 If Eof()
    _LxErr = "No hay movimientos para exportar" + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 *> Transformar conceptos a formato SAP, según tablas de equivalencias (art, stk, ...)
 _F20cTmp = AllTrim(Sys(2015))
 Create Cursor (_F20cTmp) ;
   (FecTrp C(8), ;
    CodAlm C(4), ;
    CodArt C(13), ;
    NumLot C(15), ;
    CanFis C(10), ;
    TipMov C(4), ;
    MovPrc C(10), ;
    NumDoc C(13), ;
    NumAlb C(10))
 AF20cTmp = Alias(_F20cTmp)

 *> Si cal, añadir los registros del fichero ya existente.
 If _MbStat == IDYES
    Select (AF20cTmp)
    Append From &FilImp Type SDF
 EndIf

 Select (AF20c)
 Go Top
 Do While !Eof()
    _CodArt = AllTrim(&AF20c->F20cCodArt)
	_Centro = oF02c.F02cCodCen
	_TipMov = &AF20c->F20cTipMov

    *> Leer situación stock SAP de tabla de equivalencias, F1PS.
    m.F1PSCodPro = m.CodPro
    m.F1PSSitStk = &AF20c->F20cSitStk
    _ok = f3_seek('F1PS')
    If _ok
       Select F1PS
       _SitStk = Left(F1PSSitDes, 4)
    Endif

    *> Leer tipo movimiento ERP de tabla de equivalencias, F1PM.
    m.F1PMCodPro = m.CodPro
    m.F1PMTipMov = &AF20c->F20cTipMov
    m.F1PMSitStk = &AF20c->F20cSitStk

    _ok = f3_seek('F1PM')
    If _ok
       Select F1PM
       _TipMov = Left(F1PMMovDes, 4)
    Endif

    Wait Window "Artículo: " + _CodArt + " - " + "Lote: " + &AF20c->F20cNumLot NoWait

    *> Grabar datos en cursor para copiar a fichero de traspaso.-----------------------------
    Select (AF20cTmp)
    Locate For CodArt = _CodArt .And. ;
               NumLot = &AF20c->F20cNumLot .And. ;
               TipMov = _TipMov .And. ;
               NumDoc = &AF20c->F20cNumDoc

    If Found()
       Replace CanFis With PadL(AllTrim(Str(Val(CanFis) + &AF20c->F20cCanFis)), 10, '0')
    Else
       Append Blank
       Replace FecTrp With DToS(Date()), ;
               CodAlm With _Centro, ;
               CodArt With _CodArt, ;
               NumLot With &AF20c->F20cNumLot, ;
               CanFis With PadL(AllTrim(Str(&AF20c->F20cCanFis)), 10, '0'), ;
               TipMov With _TipMov, ;
               MovPrc With &AF20c->F20cTipMov, ;
               NumDoc With &AF20c->F20cNumDoc, ;
               NumAlb With &AF20c->F20cNumEnt
    EndIf

    *>
    Select (AF20c)
    Skip
 EndDo

 *> Obtener el Nº de envío de F00A.-------------------------------------------------------
 If Type('Reenvio') # 'C'
    _NumTrp = Ora_NewTRm('N', 'N', 6)
    If Type('_NumTrp') <> 'C'
       _LxErr = "Error al leer Nº de traspaso de movimientos" + cr
       =Anomalias()
       =SqlRollBack(_ASql)
       Return
    EndIf

    *> Actualizar F20c con el Nº de envío.------------------------------------------------
    _Selec = "UPDATE F20c" + _em
    _Set   = "SET F20cFlgEnv='S'," + ;
             " F20cNumEnv=" + _NumTrp + "," + ;
             " F20cFecEnv=" + _GCD(Date())
    _Where = "WHERE F20cFlgEnv <> 'S' " + ;
             "AND " + _GCSS("F20cTipMov", 1, 1) + " IN ('1', '2', '3', '4') "
 
    _Selec = _Selec + Space(1) + _Set + Space(1) + _Where
    Err = f3_SqlExec(_ASql, _Selec)
    If Err < 0
       _LxErr = "Error al actualizar los MOVIMIENTOS a traspasar."
       =Anomalias()
       Return
    Else
       *> Guardar fichero para reenvíos.
       _Fp = FCreate('MVT' + Alltrim(_NumTrp) + '.EXP')
       =FClose(_Fp)
    EndIf
 EndIf

 *> Agrupar los movimientos ya existentes en el fichero de exportación.
 Select (AF20cTmp)
 If File(&FilImp)
    Append From (&FilImp) Type SDF
 EndIf
    
 *> Copiar movimientos a fichero de exportación.
 Copy To (&FilImp) Type SDF

 Wait Clear

 Use In (AF20c)
 Use In (AF20cTmp)
 Return

************************************************************************************************
*> -------------------------------------------------------------------------------------------**
*>                        EXPORTACION DE DEVOLUCIONES DE CLIENTES                              *
*> Nº de vale pasa de 10c. a 15c. (Se exportan 12c). AVC - 01.12.2000                          *
*> -------------------------------------------------------------------------------------------**
************************************************************************************************

 Procedure ExpDvc
 Parameters ReEnvio

 Private _Selec, _Where, _Order, _Set, _Fp
 Private FilImp
 Private _MbStat

 DirDvc = m.DirDvc
 
 Set Separator To ','
 Set Point To '.'
 Set Date  To DMY
 Set Century On

 *> Comprobar que el nombre del fichero sea correcto.
 If Empty(DirDvc)
    _LxErr = "El nombre del fichero de DEVOLUCIONES es erróneo" + cr
    =Anomalias()
    Return
 EndIf

 *> Comprobar si existe el fichero.
 FilImp = "'" + AllTrim(DirDvc) + "'"
 _MbStat = IDNO

 If File(&FilImp)
    _MbStat = MessageBox("Ya existe el fichero <" + AllTrim(FIlImp) + ">  Añadir movimientos?", ;
                         MB_YESNOCANCEL + MB_ICONSTOP + MB_DEFBUTTON1, ;
                         "Exportar DEVOLUCIONES")

    Do Case
       *> Añadir movimientos al fichero ya existente.
       Case _MbStat == IDYES

       *> Añadir movimientos al fichero ya existente.
       Case _MbStat == IDNO

       *> Cancelar proceso de exportación de movimientos.
       Case _MbStat == IDCANCEL
          Return

       *> Por defecto, cancelar proceso de exportación de movimientos.
       Otherwise
          Return
    EndCase
 EndIf

 *> Formar QUERY con los registros a tratar.------------------------------
 _Selec = "Select * From F19l" + _em + ", F00r" + _em
 _Where = Iif(Type('Reenvio')=='C', "Where F19lEstado='1' And F19lNumEnv=" + Reenvio, "Where F19lEstado='0'")
 _Where = _Where + " And F00rCodInc(+)=F19lMotivo"
 _Order = "Order By F19lNumDev, F19lCodArt, F19lNumLot, F19lFecCad"
 
 _F19l = AllTrim(Sys(2015))
 
 _Selec = _Selec + Space(1) + _Where + Space(1) + _Order
 Err = f3_SqlExec(_ASql, _Selec, _F19l)
 If Err < 0
    _LxErr = "Error al cargar las DEVOLUCIONES a traspasar" + cr + ;
             "MENSAJE: " + Message() + cr
    =Anomalias()
    Return
 EndIf
 
 AF19l = Alias(_F19l)
 Select (AF19l)
 Go Top
 If Eof()
    _LxErr = "No hay devoluciones de clientes para exportar" + cr
    =Anomalias()
    =SqlRollBack(_ASql)
    Return
 EndIf

 *> Transformar conceptos a formato SAP, según tablas de equivalencias (art, stk, ...)
 _F19lTmp = AllTrim(Sys(2015))
 Create Cursor (_F19lTmp) ;
   (DirAso C(10), ;
    NumRef C(20), ;
    IncTxt C(216), ;
    IncCod C(3), ;
    CodArt C(13), ;
    CanDev C(10), ;
    NumLot C(15), ;
    PVenta C(12), ;
    CanBon C(10), ;
    TipImp C(1), ;
    NumVal C(12))
  AF19lTmp = Alias(_F19lTmp)

 *> Si cal, añadir los registros del fichero ya existente.
 If _MbStat == IDYES
    Select (AF19lTmp)
    Append From &FilImp Type SDF
 EndIf

 Select (AF19l)
 Go Top
 Do While !Eof()
    _CodArt = &AF19l->F19lCodArt

    *> Grabar datos en cursor para copiar a fichero de traspaso.-----------------------------
    Select (AF19lTmp)
    Append Blank

    Replace DirAso With Left(&AF19l->F19lDirAso, 10), ;
            NumRef With Space(20), ;
            IncTxt With &AF19l->F00rDescri, ;
            IncCod With Left(&AF19l->F19lMotivo, 3), ;
            CodArt With _CodArt, ;
            CanDev With PadL(AllTrim(Str(&AF19l->F19lCanDev)), 10, '0'), ;
            NumLot With &AF19l->F19lNumLot, ;
            PVenta With PadL(AllTrim(Str(&AF19l->F19lPVenta)), 12, '0'), ;
            CanBon With PadL(AllTrim(Str(&AF19l->F19lCanBon)), 10, '0'), ;
            TipImp With &AF19l->F19lTipImp, ;
            NumVal With Left(&AF19l->F19lNumVal, 12)

    *>
    Select (AF19l)
    Skip
 EndDo

 *> Obtener el Nº de envío de F00A.-------------------------------------------------------
 If Type('Reenvio') # 'C'
    _NumTrp = Ora_NewTRm('N', 'N', 6)
    If Type('_NumTrp') <> 'C'
       _LxErr = "Error al leer Nº de traspaso de devoluciones" + cr
       =Anomalias()
       =SqlRollBack(_ASql)
       Return
    EndIf

    *> Actualizar F19l con el Nº de envío.------------------------------------------------
    _Selec = "Update F19l" + _em
    _Set   = "Set F19lNumEnv=" + _NumTrp + "," + ;
             "    F19lEstado='1'," + ;
             "    F19lFecEnv=" + _GCD(Date())
    _Where = "Where F19lEstado='0'"
 
    _Selec = _Selec + Space(1) + _Set + Space(1) + _Where
    Err = f3_SqlExec(_ASql, _Selec)
    If Err < 0
       _LxErr = "Error al actualizar las DEVOLUCIONES a traspasar."
       =Anomalias()
       Return
    Else
       *> Guardar fichero para reenvíos.
       _Fp = FCreate('DVC' + Alltrim(_NumTrp) + '.EXP')
       =FClose(_Fp)
    EndIf
 EndIf

 *> Copiar devoluciones de clientes a fichero de exportación.
 Select (AF19lTmp)
 Copy To (&FilImp) Type SDF
  
 =MessageBox("Traspaso de DEVOLUCIONES CLIENTES finalizado", MB_OK + MB_ICONINFORMATION)
 Use In (AF19l)
 Use In (AF19lTmp)
 Return

 *****************************************************************************************
 **                          ACTUALIZAR BUZON DE FICHEROS
 *> Grabar fichero de confirmación de carga en BATCH. AVC - 17.01.2001
 *****************************************************************************************
 Procedure Grabar_F40c    

   *> Path = Camino del DirXXX, Campo = Campo de f40c a modificar 
    Parameter Path, Campo
    
   *> Grabo en el fichero de buzones la nueva dirección
    Lx_Select = " Select * From F40c" + _em + " Where F40cCodigo ='" + m.CodPro + _cm
    Err = f3_SqlExec(_aSql,Lx_Select,'F40c')
    If Err < 1 
       _LxErr = "Error leyendo parámetros de IMPORT/EXPORT."
       =Anomalias()
       Return
    EndIf
   
    Select F40c
    If Eof()
      Append Blank
      Go Bottom
      Replace F40cCodigo With m.CodPro
      Replace F40cTipEnt With 'PROP'   
      Accion = 'Insert'
    Else
  	   Accion = 'Update'	 
    EndIf

    *> Para Aislar El Path del nombre del archivo	[PATH: "...\"][Arhivo: "..."]
    Path = RTrim(Upper(Path))
    x = Len(Path)
   	   
    Do While Substr(Path,x,1) <> '\' .And. x > 0
      x = x - 1
    EndDo
     
   Do Case 
   		Case Campo = "Art"
            Replace F40cImpArt With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "Cli"
            Replace F40cImpCli With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "Pro"
            Replace F40cImpPro With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "PCa"
            Replace F40cImPeCa With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "PPo"
            Replace F40cImPePo With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "POb"
            Replace F40cImPeOb With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "PCl"
            Replace F40cImPeCl With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "Est"
            Replace F40cImpEst With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "Com"
            Replace F40cImpCom With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatImp With Substr(Path,1,x)
   		Case Campo = "Doc"
            Replace F40cExpDoc With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   		Case Campo = "CFa"
            Replace F40cExpCFa With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   		Case Campo = "Stk"
            Replace F40cExpStk With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   		Case Campo = "Esd"
            Replace F40cExpEsd With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   		Case Campo = "Mvt"
            Replace F40cExpMvt With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   		Case Campo = "Dvc"
            Replace F40cExpDev With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   		Case Campo = "Dvn"
            Replace F40cImpCnd With Substr(Path,x + 1,(Len(Path) - x))
            Replace F40cPatExp With Substr(Path,1,x)
   EndCase
   
   Select F40c
   If Accion  = 'Update'
   	   Err=F3_UpdTun("F40c",,,,,"F40cCodigo ='" + m.CodPro + _cm)   
   Else 	   
       Err=F3_InsTun("F40c","F40c")
   EndIf	

   If !Err
      _LxErr = "Error grabando parámetros de IMPORT/EXPORT."
      =Anomalias()
      =SqlRollBack(_ASql)
   EndIf

   =TableUpdate(.T.)

Return

*>-----------------------------------------------------------------------------------

*******************************************************************************************
*					                   FICHERO DE INCIDENCIAS
*******************************************************************************************				

Procedure CrearIncidencias

     Create Cursor Incidencias (TipTras C(10),;
                                Codigo  C(14),; 
                                Campo   C(14),;
                                Descri  C(40))  
     Return


Procedure PrintIncidencias

   SELECT Incidencias
   Go Top
   If !Eof()
      _Label1 = "LISTADO DE INCIDENCIAS"
      Do Form st3imp2 With _Label1, 'ImpExpL1', 'Incidencias'
   Endif

Return

*>----------------------------------------------------------------------------
*> Procedimiento para agrupar ficheros partiendo de una cadena válida,
*> tal como *.TXT, F*.DBF, A??.*, etc. Utiliza Sys(2000).
*>
*>   Recibe: ModeloBusqueda -----> Cadena de selección de ficheros.
*>
*> Devuelve: FicheroGlobal ------> Nombre del fichero con los datos agrupados.
*>           Si FicheroGlobal está en blanco devuelve cadena vacía.
*>----------------------------------------------------------------------------
Procedure GroupFiles
Parameters ModeloBusqueda

Local FicheroGlobal, FicheroLocal
Local Nd, Nd1
Local Cadena, oldCompatible
Local PathFicheros, aPath, nPath

*> Nombre del fichero a devolver. Fichero de trabajo.
FicheroGlobal = AllTrim(Sys(2015))

*> Obtener el path de los ficheros, pues Sys(2000) solo devuelve el nombre.
PathFicheros = JustPath(ModeloBusqueda) + "\"

*PathFicheros = Space(0)
*nPath = 1
*aPath = At('\', ModeloBusqueda, nPath)
*Do While aPath > 0
*   PathFicheros = SubStr(ModeloBusqueda, 1, aPath)
*   nPath = nPath + 1
*   aPath = At('\', ModeloBusqueda, nPath)
*EndDo

*> Crear el fichero de trabajo.
Nd1 = FCreate(FicheroGlobal)
If Nd1 <= 0
   _LxErr = 'Error al crear fichero de trabajo para agrupar ficheros' + cr
   =Anomalias()
   Return Space(0)
EndIf

=FClose(Nd1)

*> Abrir el fichero de trabajo para escritura.
Nd1 = FOpen(FicheroGlobal, 1)
If Nd1 <= 0
   _LxErr = 'Error al abrir fichero de trabajo para agrupar ficheros' + cr
    =Anomalias()

   If File(&FicheroGlobal)
      Delete File(FicheroGlobal)
   EndIf

   Return Space(0)
EndIf

*> Devuelve el nombre del primer fichero, o cadena vacía si no existe.
ModeloBusqueda = "'" + ModeloBusqueda + "'"
FicheroLocal = Sys(2000, &ModeloBusqueda)

Do While !Empty(FicheroLocal)
   Wait Window 'Agrupando fichero: ' + FicheroLocal NoWait

   FicheroLocal = "'" + PathFicheros + FicheroLocal + "'"
   Nd = FOpen(&FicheroLocal, 0)
   Do While Nd > 0 .And. !FEof(Nd)
      Cadena = FGetS(Nd, 1024)
      If !Empty(Cadena)
         =FPutS(Nd1, Cadena)
      EndIf
   EndDo

   =FClose(Nd)

   *> Devuelve el nombre del siguiente fichero, o cadena vacía si no existe.
   FicheroLocal = Sys(2000, &ModeloBusqueda, 1)
EndDo

*>
=FClose(Nd1)
Wait Clear

*> Comprobar si el fichero de trabajo está vacío.
*> Set Compatible ON: FSize devuelve el tamaño de un fichero.
*> Set Compatible OFF: FSize devuelve el tamaño de un campo.

oldCompatible = Set('COMPATIBLE')
Set Compatible ON
If FSize(FicheroGlobal) = 0
   Delete File(FicheroGlobal)
   FicheroGlobal = Space(0)
EndIf

Set Compatible &oldCompatible

Return FicheroGlobal

*>----------------------------------------------------------------------------
*> Procedimiento para agrupar ficheros partiendo de una cadena válida,
*> tal como *.TXT, F*.DBF, A??.*, etc. Utiliza ADir().
*>
*>   Recibe: ModeloBusqueda -----> Cadena de selección de ficheros.
*>           aFiles -------------> Array con los nombres de fichero (I/O).
*>
*> Devuelve: FicheroGlobal ------> Nombre del fichero con los datos agrupados.
*>           Si FicheroGlobal está en blanco devuelve cadena vacía.
*>----------------------------------------------------------------------------
Procedure GroupFilesNew
Parameters ModeloBusqueda, aFiles

Local FicheroGlobal, FicheroLocal
Local Nd, Nd1
Local Cadena, oldCompatible
Local PathFicheros, aPath, nPath
Local nInx, nFiles

*> Nombre del fichero a devolver. Fichero de trabajo.
FicheroGlobal = AllTrim(Sys(2015))

*> Obtener el path de los ficheros, pues Sys(2000) solo devuelve el nombre.
PathFicheros = JustPath(ModeloBusqueda) + "\"

*> Crear el fichero de trabajo.
Nd1 = FCreate(FicheroGlobal)
If Nd1 <= 0
   _LxErr = 'Error al crear fichero de trabajo para agrupar ficheros' + cr
   =Anomalias()
   Return Space(0)
EndIf

=FClose(Nd1)

*> Abrir el fichero de trabajo para escritura.
Nd1 = FOpen(FicheroGlobal, 1)
If Nd1 <= 0
   _LxErr = 'Error al abrir fichero de trabajo para agrupar ficheros' + cr
   =Anomalias()

   If File(&FicheroGlobal)
      Delete File(FicheroGlobal)
   EndIf
   Return Space(0)
EndIf

*> Guardar los nombres de fichero.
Dimension aFiles(1)
nFiles = ADir(aFiles, ModeloBusqueda)

For nInx = 1 To nFiles
   FicheroLocal = aFiles(nInx, 1)

   Wait Window 'Agrupando fichero: ' + FicheroLocal NoWait

   *> Añadir el path, pues ADir() solo devuelve el nombre.
   FicheroLocal = "'" + PathFicheros + FicheroLocal + "'"

   *> Abrir el fichero a cargar.
   Nd = FOpen(&FicheroLocal, 0)
   Do While Nd > 0 .And. !FEof(Nd)
      Cadena = FGetS(Nd, 1024)
      If !Empty(Cadena)
         =FPutS(Nd1, Cadena)
      EndIf
   EndDo

   *> Cerrar fichero leído.
   =FClose(Nd)
EndFor

*>
=FClose(Nd1)
Wait Clear

*> Comprobar si el fichero de trabajo está vacío.
*> Set Compatible ON: FSize devuelve el tamaño de un fichero.
*> Set Compatible OFF: FSize devuelve el tamaño de un campo.

oldCompatible = Set('COMPATIBLE')
Set Compatible ON
If FSize(FicheroGlobal) = 0
   Delete File(FicheroGlobal)
   FicheroGlobal = Space(0)
EndIf

Set Compatible &oldCompatible

Return FicheroGlobal

*>----------------------------------------------------------------------------
*> Procedimiento para borrar grupos de ficheros partiendo de una cadena válida,
*> tal como *.TXT, F*.DBF, A??.*, etc. Utiliza Sys(2000).
*>
*>   Recibe: ModeloBusqueda -----> Cadena de selección de ficheros.
*>
*> Devuelve: FicherosBorrados ------> Nº de ficheros eliminados.
*>           >0: Ha borrado algún fichero.
*>           =0: No ha borrado ningun fichero.
*>           <0: Se ha producido algún error.
*>----------------------------------------------------------------------------
Procedure DeleteGroupFiles
Parameters ModeloBusqueda

Local FicheroLocal
Local FilesDeleted
Local PathFicheros, aPath, nPath

*> Obtener el path de los ficheros, pues Sys(2000) solo devuelve el nombre.
PathFicheros = JustPath(ModeloBusqueda) + "\"

*PathFicheros = Space(0)
*nPath = 1
*aPath = At('\', ModeloBusqueda, nPath)
*Do While aPath > 0
*   PathFicheros = SubStr(ModeloBusqueda, 1, aPath)
*   nPath = nPath + 1
*   aPath = At('\', ModeloBusqueda, nPath)
*EndDo

FilesDeleted = 0                      && Ficheros borrados.

*> Devuelve el nombre del primer fichero, o cadena vacía si no existe.
ModeloBusqueda = "'" + ModeloBusqueda + "'"
FicheroLocal = Sys(2000, &ModeloBusqueda)

Do While !Empty(FicheroLocal)
   Wait Window 'Eliminando fichero: ' + FicheroLocal NoWait

   FicheroLocal = "'" + PathFicheros + FicheroLocal + "'"
   Delete File(&FicheroLocal)
   FilesDeleted = FilesDeleted + 1

   *> Devuelve el nombre del siguiente fichero, o cadena vacía si no existe.
   FicheroLocal = Sys(2000, &ModeloBusqueda, 1)
EndDo

*>
Wait Clear
Return FilesDeleted

*>----------------------------------------------------------------------------
*> Procedimiento para borrar grupos de ficheros partiendo de una cadena válida,
*> tal como *.TXT, F*.DBF, A??.*, etc. Utiliza ADir().
*>
*>   Recibe: ModeloBusqueda -----> Cadena de selección de ficheros.
*>           aFiles -------------> Array conteniendo los ficheros a borrar.
*>
*> Devuelve: FicherosBorrados ------> Nº de ficheros eliminados.
*>           >0: Ha borrado algún fichero.
*>           =0: No ha borrado ningun fichero.
*>           <0: Se ha producido algún error.
*>----------------------------------------------------------------------------
Procedure DeleteGroupFilesNew
Parameters ModeloBusqueda, aFiles

Local FicheroLocal
Local FilesDeleted
Local PathFicheros, aPath, nPath
Local nInx, nFiles

*> Obtener el path de los ficheros, pues Sys(2000) solo devuelve el nombre.
PathFicheros = JustPath(ModeloBusqueda) + "\"

FilesDeleted = 0                      && Ficheros borrados.

*> Devuelve el nombre del primer fichero, o cadena vacía si no existe.
ModeloBusqueda = "'" + ModeloBusqueda + "'"
nFiles = ALen(aFiles, 1)

For nInx = 1 To nFiles
   FicheroLocal = aFiles(nInx, 1)
   Wait Window 'Eliminando fichero: ' + FicheroLocal NoWait

   FicheroLocal = "'" + PathFicheros + FicheroLocal + "'"
   Delete File(&FicheroLocal)

   FilesDeleted = FilesDeleted + 1
EndFor

*>
Wait Clear
Return FilesDeleted

*>----------------------------------------------------------------------------
*> Redondeo de cantidades a 2 decimales.
*>----------------------------------------------------------------------------
Function Int2
Parameters DValor

Return Int(DValor * 100 + 0.5) / 100

*> Número de pedido para líneas de Back Order.
Function GetNewPedido
Parameters Propietario, TipoDoc, PedidoActual
Private PedidoNuevo
Local _Inx
Local oldSele

OldSele = Select()
Store Space(1) To PedidoNuevo

m.F24cCodPro = Propietario
m.F24cTipDoc = TipoDoc

For _Inx = 1 To 999
   PedidoNuevo = SubStr(PedidoActual, 1, 10) + PadL(AllTrim(Str(_Inx, 3, 0)), 3, '0')
   m.F24cNumDoc = PedidoNuevo
   If !f3_seek('F24C')
      Exit
   EndIf
EndFor

Select (oldSele)
Return PedidoNuevo
