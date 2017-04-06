Define Class ubicaps as container
Width = 145
Height = 21
BackStyle = 0
BorderWidth = 0
SpecialEffect = 1

ADD OBJECT ps_alm as st_say3d WITH Height = 18, InputMask = "!!", Left = 0, Top = 0, Width = 24
ADD OBJECT ps_zona as st_get WITH InputMask = "!!", Left = 25, Top = 0, Width = 24
ADD OBJECT ps_calle as st_get WITH InputMask = "!!", Left = 49, Top = 0, Width = 24
ADD OBJECT ps_fila as st_get WITH InputMask = "!!", Left = 74, Top = 0, Width = 24
ADD OBJECT ps_piso as st_get WITH InputMask = "!!", Left = 99, Top = 0, Width = 24
ADD OBJECT ps_prof as st_get WITH InputMask = "!", Left = 124, Top = 0, Width = 16


PROCEDURE Refresh
this.ps_alm.value=_alma
control_ubica=this.ubica
lx=&control_ubica
this.ps_zona.value=subs(lx,3,2)
this.ps_calle.value=subs(lx,5,2)
this.ps_fila.value=subs(lx,7,2)
this.ps_piso.value=subs(lx,9,2)
this.ps_prof.value=subs(lx,11,1)

ENDPROC
PROCEDURE Init
This.Ps_alm.Value=_alma
this.ps_zona.ubica=this.ubica
this.ps_calle.ubica=this.ubica
this.ps_fila.ubica=this.ubica
this.ps_piso.ubica=this.ubica
this.ps_prof.ubica=this.ubica
this.refresh


ENDPROC


PROCEDURE ps_zona.GotFocus
m.Tipo="Zona"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_zona.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,3,2,this.value)
lx=_alma+subs(lx,3)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


PROCEDURE ps_calle.GotFocus
m.Tipo="Calle"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_calle.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,5,2,this.value)
lx=_alma+subs(lx,3)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


PROCEDURE ps_fila.GotFocus
m.Tipo="Fila"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_fila.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,7,2,this.value)
lx=_alma+subs(lx,3)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh


ENDPROC


PROCEDURE ps_piso.GotFocus
m.Tipo="Piso"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_piso.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,9,2,this.value)
lx=_alma+subs(lx,3)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


PROCEDURE ps_prof.GotFocus
m.Tipo="Profundidad"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_prof.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,11,1,this.value)
lx=_alma+subs(lx,3)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


EndDefine 
Define Class st_gethg as st_geth
BorderStyle = 0
SpecialEffect = 0
SelectedBackColor = Rgb(0,0,128)



PROCEDURE Click
if _psql
  if this.SpecialEffect=0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
    =sq3_camp('I',this.name)
  else
    this.SpecialEffect=0
    this.BorderColor=RGB(0,0,0)
    this.Height=18
    this.top=this.top-2
    =sq3_camp('E',this.name)
  endif
endif
ENDPROC
PROCEDURE GotFocus
this.formato_bak=this.InputMask
if !empty(this.formato)
  this.InputMask=this.formato
endif
_nvarh=upper(this.hlpsust)
_itroldval=&_nvarh
if _psql=.F.
  thisform.ayuda.visible=.T.
endif
this.backcolor=RGB(0,0,128)
this.forecolor=RGB(255,255,255)


ENDPROC
PROCEDURE Init
if _psql
  This.enabled=.T.
  This.ReadOnly=.T.
  lx1=this.Name
  if atc('.'+lx1+',',_lxsele)>0
    this.SpecialEffect=1
    this.BorderColor=RGB(255,0,0)
    this.Height=14
    this.top=this.top+2
  endif
endif

ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
if this.backcolor=RGB(0,0,128)
   lx_tecla='/'+ltrim(str(nKeyCode))+'/'
   if at(lx_tecla,'/1/3/4/5/6/9/10/13/15/18/19/24/27/29/')=0
      lx_valor=this.value
      do case
         case type('lx_valor')='C'
            this.value=''
         case type('lx_valor')='N'
            this.value=0
         case type('lx_valor')='D'
            this.value={  /  /  }
      endcase
   endif
   this.backcolor=RGB(255,255,255)
   this.forecolor=RGB(0,0,0)
endif
ENDPROC
PROCEDURE LostFocus
if !empty(this.formato_bak)
   this.InputMask=this.formato_bak
else
   this.InputMask=''
endif
_nvarh=''
thisform.ayuda.visible=.F.
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
*
this.backcolor=RGB(255,255,255)
this.forecolor=RGB(0,0,0)

ENDPROC
PROCEDURE RangeHigh
nd=this.rango_fec
_rangehigh=this.value
if nd<>0 .and. _psql=.F. .and. type('_rangehigh')='D' .and. dtoc(_rangehigh)<>'  /  /    '
  _rangelow=gomonth(date(),nd)
endif
return _rangehigh
ENDPROC
PROCEDURE RangeLow
nd=this.rango_fec
_rangelow=this.value
if nd<>0 .and. _psql=.F. .and. type('_rangelow')='D' .and. dtoc(_rangelow)<>'  /  /    '
  _rangelow=gomonth(date(),-nd)
endif
return _rangelow

ENDPROC
PROCEDURE RightClick
if _psql=.F.
  =f3_hlp()
endif
ENDPROC


EndDefine 
Define Class ubicapd as container
Width = 161
Height = 23
BackStyle = 0
BorderWidth = 0
SpecialEffect = 1
ubica = .F.

ADD OBJECT ps_alm as st_say3d WITH Height = 18, InputMask = "!!!!", Left = 0, Top = 0, Width = 37
ADD OBJECT ps_zona as st_get WITH InputMask = "!!", Left = 37, Top = 0, Width = 24
ADD OBJECT ps_calle as st_get WITH Height = 18, InputMask = "!!", Left = 63, Top = 0, Width = 25
ADD OBJECT ps_fila as st_get WITH Height = 18, InputMask = "!!!", Left = 88, Top = 0, Width = 29
ADD OBJECT ps_piso as st_get WITH InputMask = "!!", Left = 118, Top = 0, Width = 24
ADD OBJECT ps_prof as st_get WITH Height = 18, InputMask = "!", Left = 142, Top = 0, Width = 12


PROCEDURE Refresh
this.ps_alm.value=_alma
control_ubica=this.ubica
_ali=alias()
select SYSPRG
lx=&control_ubica
select alias(_ali)
this.ps_zona.value=subs(lx,5,2)
this.ps_calle.value=subs(lx,7,2)
this.ps_fila.value=subs(lx,9,3)
this.ps_piso.value=subs(lx,12,2)
this.ps_prof.value=subs(lx,14,1)

ENDPROC
PROCEDURE Init
This.Ps_alm.Value=_alma
this.ps_zona.ubica=this.ubica
this.ps_calle.ubica=this.ubica
this.ps_fila.ubica=this.ubica
this.ps_piso.ubica=this.ubica
this.ps_prof.ubica=this.ubica
this.refresh


ENDPROC


PROCEDURE ps_zona.GotFocus
m.Tipo="Zona"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_zona.Valid
control_ubica=this.Ubica
lx=&control_ubica
lx=stuff(lx,5,2,this.value)
lx=_alma+subs(lx,5)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


PROCEDURE ps_calle.GotFocus
m.Tipo="Calle"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_calle.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,7,2,this.value)
lx=_alma+subs(lx,5)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


PROCEDURE ps_fila.GotFocus
m.Tipo="Fila"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_fila.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,9,3,this.value)
lx=_alma+subs(lx,5)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh


ENDPROC


PROCEDURE ps_piso.GotFocus
m.Tipo="Piso"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_piso.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,12,2,this.value)
lx=_alma+subs(lx,5)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


PROCEDURE ps_prof.GotFocus
m.Tipo="Profundidad"
ThisForm.Tipo.Refresh

ENDPROC
PROCEDURE ps_prof.Valid
control_ubica=this.ubica
lx=&control_ubica
lx=stuff(lx,14,1,this.value)
lx=_alma+subs(lx,5)
&control_ubica=lx
m.Tipo=""
ThisForm.Tipo.Refresh

ENDPROC


EndDefine 
Define Class estado as st_say3d
FontName = "Britannic Bold"
FontSize = 12
Alignment = 1
BorderColor = Rgb(192,192,192)
ControlSource = "Estado"
Height = 18
SpecialEffect = 1
Width = 113
DisabledForeColor = Rgb(0,128,64)
DisabledBackColor = Rgb(192,192,192)



EndDefine 
Define Class orafncetiq as custom

ADD OBJECT propcaot as st_geth


PROCEDURE etiqexped
Private f_Where

*> Cargar datos.------------------------------------------------------
Do Case
   Case This.PEOrig = 'MP'
      This.MPDatos          && Carga datos MP en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'OC'
      This.OCDatos          && Carga datos OC en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'BU'
      This.BUDatos          && Carga datos BU en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'DA'
EndCase

***> Buscar datos de cliente, propietario y articulo.-----------------
*> Cursor de cliente.-------------------------------------------------
=CrtCursor('F24t','EtiqF24t')
* f_Where = "F01cTipEnt = 'C' And F01cCodigo = '" + This.PDCCli + "'"
f_Where = "F24tCodPro = '" + This.PDCPro + "' And F24tTipDoc = '" + This.PDTDoc + "' And " + ;
     "F24tNumDoc = '" + This.PDNDoc + "'"

Sw = F3_Sql('*', 'F24t', f_Where, , , 'EtiqF24t')
If Sw = .F.
   Select EtiqF24t
   Append Blank
EndIf

*> Cursor de propietario.---------------------------------------------
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo = '" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf

*> Cursor de artículo.------------------------------------------------
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
EndIf


*> Imprimir la etiqueta.----------------------------------------------
* Set Printer To COM1, 9600, N, 8, 1, H, RL

If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf
Set Printer To Name (This.PDImpr)
Set Console Off
Set Printer On
Set Device To Printer

For n = 1 To This.PdNEti * IIf(This.PDTBul = "PA", 4, 1)
   ? "^XA"

   *> Recuadro de etiqueta.-------------------------------------------
   ? "^FO25,50^GB800,1150,4^FS"

   ? "^FO775,75^A0N45,35^FWR^FDDestinatario^FS"
   ? "^FO740,75^A0N45,35^FWR^FD" + EtiqF24t.F24tNomAso + "^FS"
   ? "^FO700,75^A0N45,35^FWR^FD" + EtiqF24t.F24t1erDir + "^FS"
   ? "^FO660,75^A0N45,35^FWR^FD" + EtiqF24t.F24tCodPos + "   " + EtiqF24t.F24tDPobla + "^FS"
   ? "^FO650,50^GB4,1150,4^FS"

   ? "^FO610,75^A0N40,25^FWR^FDRemitente^FS"
   ? "^FO580,75^A0N40,25^FWR^FD" + EtiqF01p.F01pDescri + "^FS"
   ? "^FO550,75^A0N40,25^FWR^FD" + EtiqF01p.F01pDirecc + "^FS"
   ? "^FO520,75^A0N40,25^FWR^FD" + EtiqF01p.F01pCodPos + "   " + EtiqF01p.F01pPoblac + "^FS"

   ? "^FO610,750^A0N40,25^FWR^FDAlmacen^FS"
   ? "^FO580,750^A0N40,25^FWR^FDT2 - OPELOG^FS"   && Datos Almacen
   ? "^FO550,750^A0N40,25^FWR^FDC/Industria, 53^FS"
   ? "^FO520,750^A0N40,25^FWR^FD08740    S.Andreu de la Barca^FS"
   ? "^FO510,50^GB4,1150,4^FS"

   *> Datos albarán.--------------------------------------------------
   If This.PDTBul <> "PK"
      ? "^FO470,75^A0N45,35^FWR^FDArticulo  : " + EtiqF08c.F08cDescri + "^FS"
      ? "^FO430,75^A0N45,35^FWR^FDLote       : " + This.PDNLot + "^FS"
      ? "^FO430,775^A0N45,35^FWR^FDCaducidad : " + ;
             IIf(This.PDFCad = _FecMin, ' ', DToC(This.PDFCad)) + "^FS"
   EndIf
   ? "^FO420,50^GB4,1150,4^FS"

   *> EAN 128 con Artículo, Cantidad, Caducidad y Lote.---------------
   Do Case
      Case This.PDTBul = "PA" .Or. This.PDTBul = "CC"
         ? "^BY2,3.0^FO300,175^BCR,100,N,N,N,N^FR^FD>;>802" + EtiqF08c.F08cCodEAN + ;
            "37" + PadL(AllTrim(Str(This.PDCant)), 7, '0') + ">817" + ;
            Right(DToS(This.PDFCad), 6) + "10>6" + AllTrim(This.PDNLot) + "^FS"
         ? "^FO250,175^A0N50,30^FWR^FD(02)" + EtiqF08c.F08cCodEAN + "(37)" + ;
            PadL(AllTrim(Str(This.PDCant)), 7, '0') + "(17)" + ;
            IIf(This.PDFCad = _FecMin, '000000', Right(DToS(This.PDFCad), 6)) + ;
            "(10)" + AllTrim(This.PDNLot) + "^FS"

      Case This.PDTBul = "PK"
         ? "^FO300,225^A0N90,60^FWR^FDPICKING    ZONA " + ;
            SubStr(This.PDCubi, 5, 2) + "^FS"
   EndCase

   ? "^FO240,50^GB4,900,4^FS"

   *> EAN 13 con Numero de MAC.---------------------------------------
   ? "^FO180,75^A0N45,35^FWR^FDAlbaran : " + This.PDNAlb + "^FS"
   ? "^FO140,75^A0N45,35^FWR^FDFecha    : " + DToC(This.PDFAlb) + "^FS"
   Do Case
      Case This.PDTBul = "PA"
         ? "^FO180,550^A0N45,35^FWR^FDPALET^FS"
      Case This.PDTBul = "CC"
         ? "^FO180,550^A0N45,35^FWR^FDCAJA COMPLETA^FS"
      Case This.PDTBul = "PK"
         ? "^FO180,550^A0N45,35^FWR^FDCAJA PICKING^FS"
   EndCase

   ? "^FO110,550^A0N45,35^FWR^FDN. BULTO^FS"
   ? "^FO070,550^A0N45,35^FWR^FD" + AllTrim(Str(This.PDNBul)) + " / " + ;
      AllTrim(Str(This.PDTotB)) + "^FS"
   ? "^BY2,3.0^FO100,1000^BCN,165,N,N,Y,N^FR^FD>;>800" + ;
      Right(AllTrim('0000' + This.PDNMac), 13) + "^FS"
   ? "^FO40,1000^A0R45,30^FWR^FD" + This.PDNMac + "^FS"
   ? "^FO25,500^GB215,4,4^FS"
   ? "^FO25,950^GB395,4,4^FS"
   ? "^XZ"
Next

Set Printer To
Set Printer Off
Set Console On
Set Device To Screen

*> Segun propiedades, actualizamos el estado del bulto.---------------
If This.PDActz = "S"
   c_Flag1 = This.PDFlg1
   f_Where = "F26vNumMac = '" + This.PDNMac + "'"
   Sw = F3_UpdTun('F26v', , 'F26vFlag1', 'c_Flag1', , f_Where, 'N', 'N')
   If Sw = .F.
      = F3_Sn(1,1, "No se ha actualizado el estado del Mac")
   EndIf
EndIf

Use In EtiqF24t
Use In EtiqF01p
Use In EtiqF08c

ENDPROC
PROCEDURE etiqartic
*>
*> Impresión de etiquetas de artículo.
Private f_Where

*> Cargar datos.------------------------------------------------------
Do Case
   Case This.PEOrig = 'MP'
      This.MPDatos          && Carga datos MP en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'OC'
      This.OCDatos          && Carga datos OC en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'DA'
EndCase

***> Buscar datos de propietario y artículo.--------------------------
*> Cursor de propietario.---------------------------------------------
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo = '" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf

*> Cursor de artículo.------------------------------------------------
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
EndIf

*> Imprimir la etiqueta.----------------------------------------------
* Set Printer To COM1, 9600, N, 8, 1, H, RL
* Set printer to "Eti.eti"

If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf
Set Printer To Name (This.PDImpr)
Set Console Off
Set Printer On
Set Device To Printer

For n = 1 To This.PdNEti
   ? "^XA"

   c_NLot = Space(6)
   c_FCad = "000000"

   *> Recuadro de etiqueta.-------------------------------------------
*   ? "^FO25,25^GB800,1150,4^FS"

   ? "^FO760,50^A0N60,45^FWR^FD" + EtiqF08c.F08cDescri + "^FS"
   ? "^FO690,50^A0N70,55^FWR^FDArticulo     : " + This.PDCArt + "^FS"

   If !Empty(This.PDNLot)
      c_NLot = AllTrim(This.PDNLot)
      ? "^FO540,50^A0N70,55^FWR^FDNum. Lote : " + This.PDNLot + "^FS"
   EndIf

   If !Empty(This.PDFCad)  .And.  This.PDFCad <> _FecMin
      c_FCad = Right(DToS(This.PDFCad), 6)
      ? "^FO390,50^A0N70,55^FWR^FDCaducidad : " + DToC(This.PDFCad) + "^FS"
   EndIf

   ? "^FO240,50^A0N70,55^FWR^FDUnid./Caja : " + AllTrim(Str(This.PDCant)) + "^FS"
   ? "^FO090,50^A0N70,55^FWR^FDFabricante : " + This.PDCPro + "^FS"
   ? "^FO025,50^A0N60,45^FWR^FD" + EtiqF01p.F01pDescri + "^FS"

   *> EAN 128 con Artículo, Cantidad, Caducidad y Lote.---------------
   ? "^BY2,3.0^FO75,975^BCN,175,N,N,N,N^FR^FD>;>802" + EtiqF08c.F08cCodEAN + ;
      "37" + PadL(AllTrim(Str(This.PDCant)), 7, '0') + ">817" + c_FCad + "10>6" + c_NLot + "^FS"
   ? "^FO90,935^A0N50,30^FWN^FD(02)" + EtiqF08c.F08cCodEAN + "(37)" + ;
      PadL(AllTrim(Str(This.PDCant)), 7, '0') + "(17)" + ;
      c_FCad + "(10)" + c_NLot + "^FS"

   ? "^XZ"
Next

Set Printer To
Set Printer Off
Set Console On
Set Device To Screen

Use In EtiqF01p
Use In EtiqF08c

*>

ENDPROC
PROCEDURE inicializar
*> Estado general de las operaciones.---------------------------------
This.PWCrtn = "00"

*> Datos origen de datos.---------------------------------------------
This.PEOrig = Space(2)

*> Datos movimiento.--------------------------------------------------
This.PMNmov = Space(10)

*> Datos ocupación.---------------------------------------------------
This.POCPro = Space(6)
This.POCArt = Space(13)
This.PONLot = Space(15)
This.POFCad = CToD(Space(8))
This.POSStk = Space(4)
This.POCUbi = Space(14)
This.PONPal = Space(10)

*> Datos de impresion etiqueta.---------------------------------------
This.PDCPro = Space(6)
This.PDTDoc = Space(4)
This.PDNDoc = Space(13)
This.PDCArt = Space(13)
This.PDNLot = Space(15)
This.PDFCad = CToD(Space(8))
This.PDCant = 0
This.PDCUbi = Space(14)
This.PDCCli = Space(6)
This.PDNAlb = Space(10)
This.PDFAlb = CToD(Space(8))
This.PDTBul = Space(2)
This.PDNBul = 0
This.PDTotB = 0
This.PDNMac = Space(9)
This.PDNEti = 1
This.PDFlg1 = "0"
This.PDActz = "N"
This.PDCtrl = Space(1)

ENDPROC
PROCEDURE mpdatos
***> A partir de PMNMov, cargar los datos necesarios.-----------------
Private f_Where

*> Cursor de movimientos pendientes.----------------------------------
=CrtCursor('F14c', 'EtiqF14c')
f_Where = "F14cNumMov = '" + This.PMNMov + "'"
Sw = F3_Sql('*', 'F14c', f_Where, , , 'EtiqF14c')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF14c
   Return
EndIf

*> Cursor de documento de salida.-------------------------------------
=CrtCursor('F24c', 'EtiqF24c')
f_Where = "F24cCodPro = '" + EtiqF14c.F14cCodPro + "' And F24cTipDoc = '" + ;
    EtiqF14c.F14cTipDoc + "' And F24cNumDoc = '" + EtiqF14c.F14cNumDoc + "'"
Sw = F3_Sql('*', 'F24c', f_Where, , , 'EtiqF24c')
If Sw = .T.
   If !Empty(This.PDNAlb)
      This.PDNAlb = EtiqF24c.F24cNumAlb
      This.PDFAlb = EtiqF24c.F24cFecAlb
   Else
      This.PDNAlb = EtiqF24c.F24cNumDoc
      This.PDFAlb = EtiqF24c.F24cFecDoc
   EndIf
   This.PDCCli = EtiqF24c.F24cDirAso
   This.PDTotB = EtiqF24c.F24cNumBul
EndIf
This.PDTDoc = EtiqF24c.F24cTipDoc
This.PDNDoc = EtiqF24c.F24cNumDoc

Select EtiqF14c
Go Top
This.PDCPro = EtiqF14c.F14cCodPro
This.PDCArt = EtiqF14c.F14cCodArt
This.PDNLot = EtiqF14c.F14cNumLot
This.PDFCad = EtiqF14c.F14cFecCad
This.PDCant = EtiqF14c.F14cCanFis
This.PDCUbi = EtiqF14c.F14cUbiOri
This.PDNMac = EtiqF14c.F14cNumMac

If !Empty(This.PDNMac)
   *> Buscar datos Mac.--------------------------------------------------
   =CrtCursor('F26v', 'EtiqF26v')
   f_Where = "F26vTipMac = '" + EtiqF14c.F14cTipMac + "' And F26vNumMac = '" + ;
       EtiqF14c.F14cNumMac + "'"
   Sw = F3_Sql('*', 'F26v', f_Where, , , 'EtiqF26v')
   If Sw = .T.
      This.PDNBul = Val(EtiqF26v.F26vNumBul)
      Do Case
         Case EtiqF26v.F26vTipOri = "P"
            This.PDTBul = "PA"
         Case EtiqF26v.F26vTipOri = "C"
            This.PDTBul = "CC"
         Case EtiqF26v.F26vTipOri = "U"
            This.PDTBul = "PK"
      EndCase
   EndIf
EndIf

If Used('EtiqF14c')
   Use In EtiqF14c
EndIf
If Used('EtiqF24c')
   Use In EtiqF24c
EndIf
If Used('EtiqF26v')
   Use In EtiqF26v
EndIf

ENDPROC
PROCEDURE ocdatos
***> A partir de datos ocupación, cargar los datos necesarios.--------
Private f_Where

*> Cursor de ocupaciones.---------------------------------------------
=CrtCursor('F16c', 'EtiqF16c')
f_Where = "F16cCodPro = '" + This.POCPro + "' And F16cCodArt = '" + ;
    This.POCArt + "' And F16cNumLot = '" + This.PONLot + "' And F16cFecCad = " + ;
    "To_Date('" + DToC(This.POFCad) + "','dd-mm-yyyy') And F16cSitStk = '" + ;
    This.POSStk + "' And F16cNumPal = '" + This.PONPal + "' And F16cCodUbi = '" + ;
    This.POCUbi + "'"

Sw = F3_Sql('*', 'F16c', f_Where, , , 'EtiqF16c')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF16c
   Return
EndIf

Select EtiqF16c
Go Top
This.PDCPro = EtiqF16c.F16cCodPro
This.PDCArt = EtiqF16c.F16cCodArt
This.PDNLot = EtiqF16c.F16cNumLot
This.PDFCad = EtiqF16c.F16cFecCad
This.PDCant = EtiqF16c.F16cCanFis
This.PDCUbi = EtiqF16c.F16cCodUbi

*>
Use In EtiqF16c

ENDPROC
PROCEDURE budatos
***> A partir de PUTMac, PUNMac, PUNBul y PUMvMP, cargar datos.-------
Private f_Where

*> Cursor de movimientos pendientes.----------------------------------
=CrtCursor('F26l', 'EtiqF26l')
f_Where = "F26lNumMov = '" + This.PBMvMP + "'"
Sw = F3_Sql('*', 'F26l', f_Where, , , 'EtiqF26l')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF26l
   Return
EndIf

*> Cursor de documento de salida.-------------------------------------
=CrtCursor('F24c', 'EtiqF24c')
f_Where = "F24cCodPro = '" + EtiqF26l.F26lCodPro + "' And F24cTipDoc = '" + ;
    EtiqF26l.F26lTipDoc + "' And F24cNumDoc = '" + EtiqF26l.F26lNumDoc + "'"
Sw = F3_Sql('*', 'F24c', f_Where, , , 'EtiqF24c')
If Sw = .T.
   If !Empty(This.PDNAlb)
      This.PDNAlb = EtiqF24c.F24cNumAlb
      This.PDFAlb = EtiqF24c.F24cFecAlb
   Else
      This.PDNAlb = EtiqF24c.F24cNumDoc
      This.PDFAlb = EtiqF24c.F24cFecDoc
   EndIf
   This.PDCCli = EtiqF24c.F24cDirAso
   This.PDTotB = EtiqF24c.F24cNumBul
EndIf
This.PDTDoc = EtiqF24c.F24cTipDoc
This.PDNDoc = EtiqF24c.F24cNumDoc

Select EtiqF26l
Go Top
This.PDCPro = EtiqF26l.F26lCodPro
This.PDCArt = EtiqF26l.F26lCodArt
This.PDNLot = EtiqF26l.F26lNumLot
This.PDFCad = EtiqF26l.F26lFecCad
This.PDCant = EtiqF26l.F26lCanFis
This.PDCUbi = EtiqF26l.F26lUbiOri
This.PDNMac = This.PBNMac
This.PDNBul = Val(This.PBNBul)
Do Case
   Case This.PBTOri = "P"
      This.PDTBul = "PA"
   Case This.PBTOri = "C"
      This.PDTBul = "CC"
   Case This.PBTOri = "U"
      This.PDTBul = "PK"
EndCase

If Used('EtiqF24c')
   Use In EtiqF24c
EndIf
If Used('EtiqF26l')
   Use In EtiqF26l
EndIf

ENDPROC
PROCEDURE etiqubicpic
*>
*> Impresión de etiquetas de ubicación (Picking).
Private f_Where

*> Cargar datos.------------------------------------------------------
Do Case
   Case This.PEOrig = 'MP'
      This.MPDatosEnt       && Carga datos MP en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'OC'
      This.OCDatos          && Carga datos OC en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'DA'
EndCase

***> Buscar datos de propietario y artículo.--------------------------
*> Cursor de propietario.---------------------------------------------
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo = '" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf

*> Cursor de artículo.------------------------------------------------
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
EndIf

*> Imprimir la etiqueta.----------------------------------------------
* Set Printer To COM1, 9600, N, 8, 1, H, RL
* Set printer to "Eti.eti"

If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf
Set Printer To Name (This.PDImpr)
Set Console Off
Set Printer On
Set Device To Printer

For n = 1 To This.PdNEti
   ? "^XA"

   c_NLot = Space(6)
   c_FCad = "000000"

   *> Recuadro de etiqueta.-------------------------------------------
*   ? "^FO25,25^GB800,1150,4^FS"

   ? "^FO760,50^A0N60,45^FWR^FD" + EtiqF08c.F08cDescri + "^FS"
   ? "^FO690,50^A0N70,55^FWR^FDArticulo     : " + This.PDCArt + "^FS"

   If !Empty(This.PDNLot)
      c_NLot = AllTrim(This.PDNLot)
      ? "^FO540,50^A0N70,55^FWR^FDNum. Lote : " + This.PDNLot + "^FS"
   EndIf

   If !Empty(This.PDFCad)  .And.  This.PDFCad <> _FecMin
      c_FCad = Right(DToS(This.PDFCad), 6)
      ? "^FO390,50^A0N70,55^FWR^FDCaducidad : " + DToC(This.PDFCad) + "^FS"
   EndIf

   ? "^FO240,50^A0N70,55^FWR^FDUnid./Caja : " + AllTrim(Str(This.PDCant)) + "^FS"
   ? "^FO090,50^A0N70,55^FWR^FDFabricante : " + This.PDCPro + "^FS"
   ? "^FO025,50^A0N60,45^FWR^FD" + EtiqF01p.F01pDescri + "^FS"

   *> EAN 128 con Artículo, Cantidad, Caducidad y Lote.---------------
   ? "^BY2,3.0^FO75,975^BCN,175,N,N,N,N^FR^FD>;>802" + EtiqF08c.F08cCodEAN + ;
      "37" + PadL(AllTrim(Str(This.PDCant)), 7, '0') + ">817" + c_FCad + "10>6" + c_NLot + "^FS"
   ? "^FO90,935^A0N50,30^FWN^FD(02)" + EtiqF08c.F08cCodEAN + "(37)" + ;
      PadL(AllTrim(Str(This.PDCant)), 7, '0') + "(17)" + ;
      c_FCad + "(10)" + c_NLot + "^FS"

   ? "^XZ"
Next

Set Printer To
Set Printer Off
Set Console On
Set Device To Screen

Use In EtiqF01p
Use In EtiqF08c

*>

ENDPROC
PROCEDURE etiqubicpal
*>
*> Impresión de etiquetas de ubicación (Ubicaciones de palet completo).
Private f_Where, ShortUbi

*> Cargar datos.------------------------------------------------------
*> Por ahora solo se toma de ubicaciones (F10c).----------------------
Do Case
   Case This.PEOrig = 'UB'
      This.UBDatos             && Carga datos Ubicaciones.
      If This.PWCRtn > "50"
         Return
      EndIf
   Otherwise
EndCase

*> Imprimir la etiqueta.----------------------------------------------
* Set Printer To COM1, 9600, N, 8, 1, H, RL
* Set printer to "Eti.eti"

If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf
Set Printer To Name (This.PDImpr)
Set Console Off
Set Printer On
Set Device To Printer

*> Calcular, si cal, el dígito de control.
If Empty(This.PDCtrl)
   This.UbicDigit
EndIf

*> Compactar el código de ubicación.
ShortUbic = SubStr(This.PDCubi, 5, 1) + ;
            SubStr(This.PDCubi, 7, 2) + ;
            SubStr(This.PDCubi, 9, 2) + ;
            SubStr(This.PDCubi,12, 1)

For n = 1 To This.PdNEti
   ? "^XA"
   ? "^FO000,100^A0R,500,300^FD" + This.PDCtrl + "^FS"
   ? "^FO300,400^A0R,200,130^FD" + SortUbic + "^FS"
   ? "^FO20,020^GB500,940,4^FS"
   ? "^FO100,350^GB0,610,4^FS"
   ? "^FO290,350^GB0,610,4^FS"
   ? "^FO020,350^GB500,0,4^FS"
   ? "^BY4,2:1,4"
   ? "^CFD,100,100"
   ? "^FO080,400^B2R,200,Y,N,N^FD12345678901234^FS"
   ? "^XZ"
Next

Set Printer To
Set Printer Off
Set Console On
Set Device To Screen

*>

ENDPROC
PROCEDURE mpdatosent
*>
*> A partir de PMNMov, cargar los datos necesarios.-----------------
Private f_Where

*> Cursor de movimientos pendientes.----------------------------------
=CrtCursor('F14c', 'EtiqF14c')
f_Where = "F14cNumMov = '" + This.PMNMov + "'"
Sw = F3_Sql('*', 'F14c', f_Where, , , 'EtiqF14c')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF14c
   Return
EndIf

*> Cursor de documento de entrada.-------------------------------------
=CrtCursor('F18c', 'EtiqF18c')
f_Where = "F18cCodPro = '" + EtiqF14c.F14cCodPro + "' And " + ;
          "F18cTipDoc = '" + EtiqF14c.F14cTipDoc + "' And " + ;
          "F18cNumDoc = '" + EtiqF14c.F14cNumDoc + "'"

Sw = F3_Sql('*', 'F18c', f_Where, , , 'EtiqF18c')
If Sw = .T.
   If !Empty(This.PDNAlb)
      This.PDNAlb = EtiqF18c.F18cNumDoc
      This.PDFAlb = EtiqF18c.F18cFecPed
   Else
      This.PDNAlb = EtiqF18c.F18cNumDoc
      This.PDFAlb = EtiqF18c.F18cFecPed
   EndIf
EndIf

This.PDTDoc = EtiqF18c.F18cTipDoc
This.PDNDoc = EtiqF18c.F18cNumDoc

Select EtiqF14c
Go Top
This.PDCPro = EtiqF14c.F14cCodPro
This.PDCArt = EtiqF14c.F14cCodArt
This.PDNLot = EtiqF14c.F14cNumLot
This.PDFCad = EtiqF14c.F14cFecCad
This.PDCant = EtiqF14c.F14cCanFis
This.PDCUbi = EtiqF14c.F14cUbiOri
This.PDNMac = EtiqF14c.F14cNumMac

If Used('EtiqF14c')
   Use In EtiqF14c
EndIf
If Used('EtiqF18')
   Use In EtiqF24c
EndIf

*>
ENDPROC
PROCEDURE etiqpalet
*>
*> Impresión de etiquetas de Palet.
Private f_Where,N_Palet,C_descri,N_Canfis,F_Fecha

n_Palet = This.PONPAL
n_CanFis = Fnc.Objparm.POCFIS
F_Fecha = Fnc.ObjParm.PoFMov
*> Cargar datos.------------------------------------------------------
Do Case
   Case This.PEOrig = 'MP'
      This.MPDatos          && Carga datos MP en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'OC'
      This.OCDatos          && Carga datos OC en prop.Datos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'DA'
EndCase

***> Buscar datos de propietario y artículo.--------------------------
*> Cursor de propietario.---------------------------------------------
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo = '" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf

*> Cursor de artículo.------------------------------------------------
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
Else
   C_Descri = EtiqF08c.F08cDescri
EndIf

*> Imprimir la etiqueta.----------------------------------------------
* Set Printer To COM1, 9600, N, 8, 1, H, RL
* Set printer to "Eti.eti"

If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

Set Printer To Name (This.PDImpr)
Set Console Off
Set Printer On
Set Device To Printer

For n = 1 To This.PdNEti

   c_NLot = Space(6)
   c_FCad = "000000"
   F_Fecha = Dtoc(F_Fecha)
   N_CanFis = Trans(N_CanFis,'999999999')

   ? "^XA"
   ? "^FO280,360^A0R,200,250^FDS^FS"
   ? "^FO280,390^A0R,50,30^FDZona^FS"
   ? "^FO300,550^A0R,150,100^FD17-220^FS"
   ? "^FO220,360^A0R,60,40^FDCODIGO      "+This.PDCArt+"^FS"
   ? "^FO160,360^A0R,60,40^FDCANTIDAD    "+N_CanFis+"^FS"
   ? "^FO100,360^A0R,60,40^FDFECHA       "+F_Fecha+"^FS"
   ? "^FO015,360^A0R,80,40^FD"+C_Descri+"^FS"
   ? "^FO20,020^GB500,940,4^FS"
   ? "^FO100,350^GB0,610,4^FS"
   ? "^FO20,350^GB500,0,4^FS"
   ? "^BY4,2:1,4"
   ? "^CFD,100,080"
   ? "^FO70,060^B2R,400,Y,N,N^FD"+N_Palet+"^FS"
   ? "^XZ"

Next

Set Printer To
Set Printer Off
Set Console On
Set Device To Screen

Use In EtiqF01p
Use In EtiqF08c

*>

ENDPROC
PROCEDURE ubicdigit
*>
*> Cálculo del dígito de control de una ubicación.
*> La ubicación se toma de This.PDCubi.



*>

ENDPROC


PROCEDURE Init
*>

************
Private _aaa, _ccc, _aa

_aa = On('Error')
on error a = 1

*> Declara público el código de propietario.
_ccc = 'This.ControlSource'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      _aaa = &_ccc
      Public &_aaa
      If Type('&_aaa') = 'L'
         Store '' To &_aaa
      EndIf
   EndIf
EndIf

*> Declara pública la descripción del propietario.
_ccc = 'This.PropDesPro'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      _aaa = &_ccc
      Public &_aaa
      If Type('&_aaa') = 'L'
         Store '' To &_aaa
      EndIf
   EndIf
EndIf

on error &_aa
************

*> Si es PROCAOT no debe pedir propietario...................................................
_ok = .T.
*If !Empty(This.PropCaot)
   control_Prop = This.PropCaot     && Variable global que contiene el propietario si es PROCAOT.
   control_Actz = This.ActCodPro    && Actualizar el campo que contiene el código (m.????)
   control_Actd = This.PropDesPro   && Actualizar el campo descripción

   If !Empty(&control_Prop)
      This.Valid
*      m.F01pCodigo = &control_Prop
*      _ok=f3_seek('F01P', , 'ALL')

      &control_Actz = Iif(!Empty(control_Actz), m.F01pCodigo, &control_Actz)
      &control_Actd = Iif(!Empty(control_Actd), m.F01pDescri, &control_Actd)

      *> Bloquear campo del propietario.
      This.Enabled = .F.
   EndIf

*EndIf

*>
This.Refresh

ENDPROC
PROCEDURE Valid
*>
*> Validar el código de propietario.........................
If _Esc
   Return
EndIf

*> Si es PROCAOT no debe pedir propietario...................................................
_ok = .T.
If !Empty(This.PropCaot)
   control_Prop = This.PropCaot     && Variable global que contiene el propietario si es PROCAOT.
   control_Actz = This.ActCodPro    && Actualizar el campo que contiene el código (m.????)
   control_Actd = This.PropDesPro   && Actualizar el campo descripción

   If !Empty(&control_Prop)
      m.F01pCodigo = &control_Prop
      _ok=f3_seek('F01P', , 'ALL')

      &control_Actz = Iif(!Empty(control_Actz), m.F01pCodigo, &control_Actz)
      &control_Actd = Iif(!Empty(control_Actd), m.F01pDescri, &control_Actd)
   Else
      F01pCodigo = This.Name
      m.F01pCodigo = &F01pCodigo
      _ok=f3_seek('F01P', , 'ALL')

      &control_Actz = Iif(!Empty(control_Actz), m.F01pCodigo, &control_Actz)
      &control_Actd = Iif(!Empty(control_Actd), m.F01pDescri, &control_Actd)
   EndIf
EndIf

*>
ThisForm.Refresh
Return _ok

ENDPROC


EndDefine 
Define Class propcaot as st_geth



PROCEDURE Init
*>

************
Private _aaa, _ccc, _aa

_aa = On('Error')
on error a = 1

*> Declara público el código de propietario.
_ccc = 'This.ControlSource'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      _aaa = &_ccc
      Public &_aaa
      If Type('&_aaa') = 'L'
         Store '' To &_aaa
      EndIf
   EndIf
EndIf

*> Declara pública la descripción del propietario.
_ccc = 'This.PropDesPro'
If Type('&_ccc') # 'U'
   If !Empty(&_ccc)
      _aaa = &_ccc
      Public &_aaa
      If Type('&_aaa') = 'L'
         Store '' To &_aaa
      EndIf
   EndIf
EndIf

on error &_aa
************

*> Si es PROCAOT no debe pedir propietario...................................................
_ok = .T.
*If !Empty(This.PropCaot)
   control_Prop = This.PropCaot     && Variable global que contiene el propietario si es PROCAOT.
   control_Actz = This.ActCodPro    && Actualizar el campo que contiene el código (m.????)
   control_Actd = This.PropDesPro   && Actualizar el campo descripción

   If !Empty(&control_Prop)
      This.Valid
*      m.F01pCodigo = &control_Prop
*      _ok=f3_seek('F01P', , 'ALL')

      &control_Actz = Iif(!Empty(control_Actz), m.F01pCodigo, &control_Actz)
      &control_Actd = Iif(!Empty(control_Actd), m.F01pDescri, &control_Actd)

      *> Bloquear campo del propietario.
      This.Enabled = .F.
   EndIf

*EndIf

*>
This.Refresh

ENDPROC
PROCEDURE Valid
*>
*> Validar el código de propietario.........................
If _Esc
   Return
EndIf

*> Si es PROCAOT no debe pedir propietario...................................................
_ok = .T.
If !Empty(This.PropCaot)
   control_Prop = This.PropCaot     && Variable global que contiene el propietario si es PROCAOT.
   control_Actz = This.ActCodPro    && Actualizar el campo que contiene el código (m.????)
   control_Actd = This.PropDesPro   && Actualizar el campo descripción

   If !Empty(&control_Prop)
      m.F01pCodigo = &control_Prop
      _ok=f3_seek('F01P', , 'ALL')

      &control_Actz = Iif(!Empty(control_Actz), m.F01pCodigo, &control_Actz)
      &control_Actd = Iif(!Empty(control_Actd), m.F01pDescri, &control_Actd)
   Else
      F01pCodigo = This.Name
      m.F01pCodigo = &F01pCodigo
      _ok=f3_seek('F01P', , 'ALL')

      &control_Actz = Iif(!Empty(control_Actz), m.F01pCodigo, &control_Actz)
      &control_Actd = Iif(!Empty(control_Actd), m.F01pDescri, &control_Actd)
   EndIf
EndIf

*>
ThisForm.Refresh
Return _ok

ENDPROC


EndDefine 
Define Class st_saydt as st_say3d



PROCEDURE MouseMove
LPARAMETERS nButton, nShift, nXCoord, nYCoord
This.ToolTipText = AllTrim(This.Value)

ENDPROC


EndDefine 
Define Class botoncaliente as commandbutton
Height = 30
Width = 33
Picture = "bmp\hot_dot.bmp"
DownPicture = "bmp\ok.bmp"
DisabledPicture = "bmp\parar.bmp"
Caption = ""



EndDefine 
Define Class codpro as st_geth



PROCEDURE Valid
If _Esc
   Return
EndIf
control_Prop=This.ControlSource
This.Value = &control_Prop
This.Refresh
ThisForm.Refresh
ENDPROC
PROCEDURE Init
control_Prop=This.PropValor
ValorProp = &control_Prop
If !Empty(ValorProp)
   This.Value   = &control_Prop
   This.Enabled = .F.
Else
   control_Prop=This.ControlSource
   This.Value = &control_Prop
EndIf
This.Refresh



ENDPROC


EndDefine 
Define Class controlubica as custom



PROCEDURE controlubica
*>
*> Validaciones varias de ubicación.

Parameters _CodUbi
Private _CodUbi

*> Comprobamos la ubicación.--------------------------------------
_Sentencia = " Select F10cCodUbi,F10cEstEnt From F10c" + _em + ;
			 " Where F10cCodUbi = '" + _CodUbi + "'"
			

*> Realizamos la consulta.----------------------------------------
_Err = SqlExec(_Asql,_Sentencia,'ComUbi')
=SqlMoreResults(_Asql)
If _Err <= 0
   =SqlRollBack(_Asql)
   =MessageBox(' Error comprobando ubicaciones ',16,_LICENCIADO)
   Return .F.
EndIf			

*> Comprobamos si existe.-----------------------------------------
Select ComUbi
Go Top
If Eof()
   =MessageBox('La ubicación no está dada de alta en el almacén ',16,_LICENCIADO)
   Return .F.
EndIf

*> Comprobar si es del mismo almacén.-----------------------------
Select ComUbi
If _Alma <> Left(ComUbi.F10cCodUbi,4)
   =MessageBox('La ubicación no es de este almacén ',16,_LICENCIADO)
   Return .F.
EndIf

*> Comprobar que no esté bloqueada.--------------------------------
If ComUbi.F10cEstEnt ='S'
   =MessageBox('La ubicación está bloqueada de entrada ',16,_LICENCIADO)
   Return .F.
EndIf

Return .T.

ENDPROC


EndDefine 
Define Class miselecall as commandbutton
Height = 28
Width = 28
Caption = "*/.."
ToolTipText = "Marcar/desmarcar todos"



PROCEDURE Click
*>
*> Marcar/desmarcar TODOS los registros.
Local Marca, miGrid

If Type('This._Grid') # 'C' .Or. Type ('This._Marca') # 'C'
   Return
EndIf

m.miGrid = This._Grid
m.Marca  = This._Marca

Select (miGrid)
Replace All &Marca With Iif(&Marca = 1, 0, 1)
Go Top

ENDPROC


EndDefine 
Define Class lotegcr as container
Width = 139
Height = 18

ADD OBJECT Color as combobox WITH Height = 17, Left = -1, Style = 2, Top = 0, Width = 60
ADD OBJECT Resto as textbox WITH Height = 17, InputMask = "XXXXXXXXXXX", Left = 57, Margin = 0, Top = 0, Width = 79


PROCEDURE initialize

*> Cargar el combo de calibres/colores del artículo.
*> Recibe:
*>	- Propietario, artículo.

Parameters cCodPro, cCodArt

Private cWhere, cField
Private nInx, cPropi, cArtic

Use In (Select ("F08VCOLL"))

This.AddProperty("aColor[1]", "")				&& Array source de calibre / colores.
This.AddProperty("ControlSource", "")			&& Origen de datos del lote en el Grid.
This.AddProperty("_lotegt", "")					&& Control origen del lote en el Grid.

cPropi = &cCodPro
cArtic = &cCodArt

cWhere = "F08vCodPro='" + cPropi + "' And F08vCodArt='" + cArtic + "'"
cField = "Distinct F08vCodCol As F08vCodCol"
=f3_sql(cField, "F08v", cWhere, , , "F08VCOLL")

nInx = 0

Select F08VCOLL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.aColor[nInx]
	This.aColor[nInx] = F08vCodCol
	Skip
EndDo

With This.Color
	.RowSourceType = 5							&& Alias
	.RowSource = "This.Parent.aColor"			&& Origen
	.Enabled = .T.
EndWith

Use In (Select ("F08VCOLL"))
Return

ENDPROC
PROCEDURE ___historial___de___modificaciones__

*> Historial de modificaciones:

*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.
*>					Modificado método This.Color.GotFocus
*>					Modificado método This.Color.LostFocus
*>					Modificado método This.Resto.GotFocus
*>					Modificado método This.Resto.LostFocus

ENDPROC
PROCEDURE Init

This.LoteCTR = Space(15)

ENDPROC


PROCEDURE Color.GotFocus

*> Posicionarse en el color actual.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

Local cColorL, nItem, lx

*> La primera vez, recuperar el valor.
If Empty(This.Parent.LoteCTR)
	If Type('This.Parent.ControlSource')=='C'
		*> Recuperar el valor del grid.
		lx = "This.Parent.LoteCTR=" + This.Parent.ControlSource
		&lx
	EndIf
EndIf

cColorL = SubStr(This.Parent.LoteCTR, 1, 4)
nItem = AScan(This.Parent.aColor, cColorL)
This.ListItemID = Iif(nItem > 0, nItem, 1)

This.Parent.Resto.Value = SubStr(This.Parent.LoteCTR, 5, 11)

ENDPROC
PROCEDURE Color.LostFocus

*> Trasladar el color actual a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

This.Parent.LoteCTR = This.Parent.aColor[This.ListItemID] + SubStr(This.Parent.LoteCTR, 5, 11)

ENDPROC


PROCEDURE Resto.GotFocus

*> Visualizar el resto del lote.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

Local cRestoL

cRestoL = SubStr(This.Parent.LoteCTR, 5, 11)
This.Value = cRestoL

ENDPROC
PROCEDURE Resto.LostFocus

*> Trasladar el resto a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

Local lx

This.Parent.LoteCTR = SubStr(This.Parent.LoteCTR, 1, 4) + Left(This.Value, 11)

*> Devolver valor al Grid y el foco al control lote del grid.
If Type('This.Parent.ControlSource')=='C'
	*> Pasar valor al grid.
	lx = "Replace " + This.Parent.ControlSource + " With '" + This.Parent.LoteCTR + "'"
	&lx
EndIf

*> Inicializar valor lote.
This.Parent.LoteCTR = Space(15)

*> Devolver el foco.
This.Parent.Parent.CurrentControl = This.Parent._lotegt

ENDPROC


EndDefine 
Define Class lotegctr as container
Width = 173
Height = 19

ADD OBJECT Color as combobox WITH Height = 17, Left = -1, Style = 2, Top = 0, Width = 60
ADD OBJECT Talla as combobox WITH Height = 17, Left = 57, Margin = 2, Style = 2, Top = 0, Width = 60
ADD OBJECT Resto as textbox WITH Height = 17, InputMask = "XXXXXXX", Left = 114, Margin = 0, Top = 0, Width = 60


PROCEDURE initialize

*> Cargar el combo de colores del artículo.
*> Recibe:
*>	- Propietario, artículo.
*>	- Artículo con colores S/N

Parameters cCodPro, cCodArt, cCttrlCol

Private cWhere, cField
Private nInx

Use In (Select ("F08VTALL"))

With This
	.AddProperty("aColor[1]", "")				&& Array source de colores.
	.AddProperty("aTalla[1]", "")				&& Array source de tallas.
	.AddProperty("ControlSource", "")			&& Origen de datos del lote en el Grid.
	.AddProperty("_lotegt", "")					&& Control origen del lote en el Grid.
	.AddProperty("_codpro", "")					&& Propietario.
	.AddProperty("_codart", "")					&& Artículo.
	.AddProperty("_ctrcol", "")					&& Control colores.
*!*		.AddProperty("_oldwidth", 0)				&& Ancho control.

	._codpro = &cCodPro
	._codart = &cCodArt
	._ctrcol = cCtrlCol
EndWith

*> Cargar los colores.
cWhere = "F08vCodPro='" + This._codpro + "' And F08vCodArt='" + This._codart + "'"
cField = "Distinct F08vCodCol As F08vCodCol"
=f3_sql(cField, "F08v", cWhere, , , "F08VCOLL")

nInx = 0

Select F08VCOLL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.aColor[nInx]
	This.aColor[nInx] = F08vCodCol
	Skip
EndDo

With This.Color
	.RowSourceType = 5							&& Alias
	.RowSource = "This.Parent.aColor"			&& Origen
	.Enabled = (.Parent._ctrcol=="S")
EndWith

Use In (Select ("F08VCOLL"))
Return

ENDPROC
PROCEDURE ___historial___de___modificaciones

*> Historial de modificaciones:

*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.
*>					Modificado método This.Color.GotFocus
*>					Modificado método This.Color.LostFocus
*>					Modificado método This.Talla.GotFocus
*>					Modificado método This.Talla.LostFocus
*>					Modificado método This.Resto.GotFocus
*>					Modificado método This.Resto.LostFocus

ENDPROC
PROCEDURE Init

This.LoteCTR = Space(15)

ENDPROC


PROCEDURE Color.LostFocus

*> Trasladar el color actual a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.

This.Parent.LoteCTR = This.Parent.aColor[This.ListItemID] + SubStr(This.Parent.LoteCTR, 5, 11)

ENDPROC
PROCEDURE Color.GotFocus

*> Posicionarse en el calibre / color actual.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.

Local cColorL, cTallaL, nItem, lx

If Empty(This.Parent.LoteCTR)
	If Type('This.Parent.ControlSource')=='C'
		*> Recuperar el valor del grid.
		lx = "This.Parent.LoteCTR=" + This.Parent.ControlSource
		&lx
	EndIf
EndIf

*!*	*> Ajustar el ancho de la columna.
*!*	With This.Parent
*!*		._oldwidth = Iif(Empty(._oldwidth), .Parent.Width, ._oldwidth)
*!*		.Parent.Width = .Width
*!*	EndWith

cColorL = SubStr(This.Parent.LoteCTR, 1, 4)
nItem = AScan(This.Parent.aColor, cColorL)
This.ListItemID = Iif(nItem > 0, nItem, 1)

This.Parent.Talla.GotFocus
This.Parent.Resto.Value = SubStr(This.Parent.LoteCTR, 9, 7)

ENDPROC


PROCEDURE Talla.LostFocus

*> Trasladar la talla actual a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.

This.Parent.LoteCTR = Left(This.Parent.LoteCTR, 4) + This.Parent.aTalla[This.ListItemID] + SubStr(This.Parent.LoteCTR, 9, 7)

ENDPROC
PROCEDURE Talla.GotFocus

*> Cargar el combo de tallas del artículo.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.

Private cWhere, cField
Private nInx, cColorL
Local cColorL, cTallaL, nItem, lx

Use In (Select ("F08VTALL"))

If Empty(This.Parent.LoteCTR)
	This.Parent.Color.GotFocus
EndIf

cColorL = SubStr(This.Parent.LoteCTR, 1, 4)

*> Cargar las tallas.
cWhere =          "F08vCodPro='" + This.Parent._codpro + "' And F08vCodArt='" + This.Parent._codart + "' And "
cWhere = cWhere + "F08vCodCol='" + cColorL + "'"

cField = "Distinct F08vCodTal As F08vCodTal"
=f3_sql(cField, "F08v", cWhere, , , "F08VTALL")

nInx = 0

Select F08VTALL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.Parent.aTalla[nInx]
	This.Parent.aTalla[nInx] = F08vCodTal
	Skip
EndDo

With This
	.RowSourceType = 5							&& Alias
	.RowSource = "This.Parent.aTalla"			&& Origen
	.Enabled = .T.
EndWith

cTallaL = SubStr(This.Parent.LoteCTR, 5, 4)
nItem = AScan(This.Parent.aTalla, cTallaL)
This.ListItemID = Iif(nItem > 0, nItem, 1)

This.Parent.Resto.Value = SubStr(This.Parent.LoteCTR, 9, 7)

Use In (Select ("F08VTALL"))
Return

ENDPROC


PROCEDURE Resto.GotFocus

*> Visualizar el resto del lote.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.

Local cRestoL

cRestoL = SubStr(This.Parent.LoteCTR, 9, 7)
This.Value = cRestoL

ENDPROC
PROCEDURE Resto.LostFocus

*> Trasladar el resto a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla, calibre / color de 5c. a 4c.

Local lx

With This.Parent
	.LoteCTR = SubStr(.LoteCTR, 1, 8) + Left(This.Value, 7)
	.Color.Enabled = .T.

	*> Devolver valor al Grid y el foco al control lote del grid.
	If Type('.ControlSource')=='C'
		*> Pasar valor al grid.
		lx = "Replace " + .ControlSource + " With '" + .LoteCTR + "'"
		&lx
	EndIf

	*> Inicializar valor lote.
	.LoteCTR = Space(15)

	*> Devolver el foco.
	.Parent.CurrentControl = ._lotegt

*!*		*> Devolver el ancho de la columna.
*!*		.Parent.Width = ._oldwidth
*!*		._oldwidth = 0
EndWith

ENDPROC


EndDefine 
Define Class lotegt as textbox
Height = 21
Width = 81
_lotegcr = "lotegcr1"
_lotegctr = "lotegctr1"



PROCEDURE GotFocus

*> Calcular el control activo durante la edición del lote en el grid.

Local lx
Private cCtrlCol, cCtrlTal

If !Empty(This._codpro) .And. !Empty(This._codart)
	*> Tomar los parámetros del artículo para definir el formato Color / Talla / Resto.
	cCtrlCol = "N"
	cCtrlTal = "N"

	lx = This._codpro
	m.F08cCodPro = &lx
	lx = This._codart
	m.F08cCodArt = &lx

	If f3_seek("F08c")
		cCtrlCol = F08c.F08cColors
		cCtrlTal = F08c.F08cTallas
	EndIf

	Do Case
		*> Tiene talla y/o color.
		Case cCtrlTal=="S"
			This.Parent.CurrentControl = This._lotegctr							&& Activar control
			lx = "=This.Parent." + This.Parent.CurrentControl + ".initialize('"
			lx = lx + This._codpro + "', '" + This._codart + "', '" + cCtrlCol + "')"
			&lx																	&& Combos Colores / tallas

			lx = "This.Parent." + This.Parent.CurrentControl + ".ControlSource='" + This.ControlSource + "'"
			&lx																	&& Para recuperar valores.
			lx = "This.Parent." + This.Parent.CurrentControl + "._lotegt='" + This.Name + "'"
			&lx																	&& Control lote en grid.

			If cCtrlCol=="S"
				lx = "This.Parent." + This.Parent.CurrentControl + ".Color.SetFocus"
			Else
				lx = "This.Parent." + This.Parent.CurrentControl + ".Talla.SetFocus"
			EndIf
			&lx																	&& Foco.

		*> Tiene solo color.
		Case cCtrlCol=="S"
			This.Parent.CurrentControl = This._lotegcr
			lx = "=This.Parent." + This.Parent.CurrentControl + ".initialize('"
			lx = lx + This._codpro + "', '" + This._codart + "')"
			&lx																	&& Combo Colores

			lx = "This.Parent." + This.Parent.CurrentControl + ".ControlSource='" + This.ControlSource + "'"
			&lx																	&& Para recuperar valores.
			lx = "This.Parent." + This.Parent.CurrentControl + "._lotegt='" + This.Name + "'"
			&lx																	&& Control lote en grid.
			lx = "This.Parent." + This.Parent.CurrentControl + ".Color.SetFocus"
			&lx																	&& Foco.

		*> Sin talla ni color.
		Otherwise
	EndCase
EndIf

Return

ENDPROC


EndDefine 
Define Class lotecr as control
Width = 160
Height = 24

ADD OBJECT Color as combobox WITH Height = 20, Left = 0, Margin = 0, Top = 0, Width = 60
ADD OBJECT Resto as textbox WITH Height = 20, InputMask = "XXXXXXXXXXX", Left = 57, Margin = 0, Top = 0, Width = 99


PROCEDURE initialize

*> Inicializar la clase. Cargar el combo con los calibres / colores del artículo.

*> Recibe:
*>	- Propietario, artículo.

Parameters cCodPro, cCodArt
Private cWhere, cField
Private nInx

Use In (Select ("F08VTALL"))

With This
	._codpro = &cCodPro
	._codart = &cCodArt
	.AddProperty("aColor[1]", "")
EndWith

nInx = 0

cWhere = "F08vCodPro='" + This._codpro + "' And F08vCodArt='" + This._codart + "'"
cField = "Distinct F08vCodCol As F08vCodCol"
=f3_sql(cField, "F08v", cWhere, , , "F08VCOLL")
Select F08VCOLL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.aColor[nInx]
	This.aColor[nInx] = F08vCodCol
	Skip
EndDo

With This.Color
	.RowSourceType = 5							&& Alias
	.RowSource = "This.Parent.aColor"			&& Origen
	.Enabled = .T.
	.GotFocus
EndWith

This.Resto.GotFocus

Use In (Select ("F08VCOLL"))
Return

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:

*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.
*>					Modificado método This.Color.LostFocus
*>					Modificado método This.Color.GotFocus
*>					Modificado método This.Resto.GotFocus
*>					Modificado método This.Resto.LostFocus

ENDPROC
PROCEDURE Init

*> Inicializar el valor del lote.
This.LoteCtr = Space(15)

ENDPROC


PROCEDURE Color.GotFocus

*> Posicionarse en el color actual.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

Local cColorL, nItem, lx

cColorL = SubStr(This.Parent.LoteCTR, 1, 4)
nItem = AScan(This.Parent.aColor, cColorL)
This.ListItemID = Iif(nItem > 0, nItem, 1)

lx = This.Parent.LabelLote
If Type('lx')=='C'
	lx = lx + ".Caption='" + _TCLTIPC + "'"
	&lx
EndIf

ENDPROC
PROCEDURE Color.LostFocus

*> Trasladar el color actual a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

This.Parent.LoteCTR = This.Parent.aColor[This.ListItemID] + SubStr(This.Parent.LoteCTR, 5, 11)

ENDPROC


PROCEDURE Resto.LostFocus

*> Trasladar el resto a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

This.Parent.LoteCTR = SubStr(This.Parent.LoteCTR, 1, 4) + Left(This.Value, 11)

ENDPROC
PROCEDURE Resto.GotFocus

*> Visualizar el resto del lote.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono / talla de 5c. a 4c.

Local cRestoL, lx

cRestoL = SubStr(This.Parent.LoteCTR, 5, 11)
This.Value = cRestoL

lx = This.Parent.LabelLote
If Type('lx')=='C'
	lx = lx + ".Caption='" + _TCLTIPL + "'"
	&lx
EndIf

ENDPROC


EndDefine 
Define Class lotectr as control
Width = 172
Height = 19

ADD OBJECT Color as combobox WITH Height = 20, Left = 0, Margin = 2, Style = 2, Top = 0, Width = 60
ADD OBJECT Talla as combobox WITH Height = 20, Left = 58, Margin = 2, Style = 2, Top = 0, Width = 60
ADD OBJECT Resto as textbox WITH Height = 20, InputMask = "XXXXXXX", Left = 116, Margin = 0, Top = 0, Width = 54


PROCEDURE initialize

*> Inicializar la clase. Cargar los combos con los tonos/tallas, calibres/colores del artículo.

*> Recibe:
*>	- Control tono/talla S/N
*>	- Control calibre/color S/N

Parameters cCodPro, cCodArt, cCtrlCol, cCtrlTal
Private cWhere, cField
Private nInx

Use In (Select ("F08VCOLL"))
Use In (Select ("F08VTALL"))

With This
	._codpro = &cCodPro
	._codart = &cCodArt
	._ctrcol = cCtrlCol
	._ctrtal = cCtrlTal
	.AddProperty("aColor[1]", "")
	.AddProperty("aTalla[1]", "")
EndWith

nInx = 0

cWhere = "F08vCodPro='" + This._codpro + "' And F08vCodArt='" + This._codart + "'"
cField = "Distinct F08vCodCol As F08vCodCol"
=f3_sql(cField, "F08v", cWhere, , , "F08VCOLL")
Select F08VCOLL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.aColor[nInx]
	This.aColor[nInx] = F08vCodCol
	Skip
EndDo

nInx = 0

cWhere = "F08vCodPro='" + This._codpro + "' And F08vCodArt='" + This._codart + "'"
cField = "Distinct F08vCodTal As F08vCodTal"
=f3_sql(cField, "F08v", cWhere, , , "F08VTALL")
Select F08VTALL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.aTalla[nInx]
	This.aTalla[nInx] = F08vCodTal
	Skip
EndDo

With This.Color
	.RowSourceType = 5							&& Alias
	.RowSource = "This.Parent.aColor"			&& Origen
	.Enabled = (cCtrlCol=='S')
	.GotFocus
EndWith

With This.Talla
	.RowSourceType = 5
	.RowSource = "This.Parent.aTalla"
	.Enabled = (cCtrlTal=='S')
	.GotFocus
EndWith

This.Resto.GotFocus

Use In (Select ("F08VCOLL"))
Use In (Select ("F08VTALL"))
Return

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:

*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.
*>					Modificado método This.Color.GotFocus
*>					Modificado método This.Color.LostFocus
*>					Modificado método This.Talla.GotFocus
*>					Modificado método This.Talla.LostFocus
*>					Modificado método This.Resto.GotFocus
*>					Modificado método This.Resto.LostFocus

ENDPROC
PROCEDURE Init

*> Inicializar el valor del lote.
This.LoteCtr = Space(15)

ENDPROC


PROCEDURE Color.GotFocus

*> Posicionarse en el color actual.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.

Local cColorL, nItem, lx

If This.Parent._ctrcol=="S"
	*> CON control de calibre / color.
	cColorL = SubStr(This.Parent.LoteCTR, 1, 4)
	nItem = AScan(This.Parent.aColor, cColorL)
	This.ListItemID = Iif(nItem > 0, nItem, 1)

	lx = This.Parent.LabelLote
	If Type('lx')=='C'
		lx = lx + ".Caption='" + _TCLTIPC + "'"
		&lx
	EndIf
Else
	*> SIN control de calibre / color.
	cColorL = Iif(Type('_ColorPorDefecto')=='C', _ColorPorDefecto, Space(4))
	nItem = AScan(This.Parent.aColor, cColorL)
	This.ListItemID = Iif(nItem > 0, nItem, 1)
	This.LostFocus
EndIf

ENDPROC
PROCEDURE Color.LostFocus

*> Trasladar el color actual a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.

This.Parent.LoteCTR = This.Parent.aColor[This.ListItemID] + SubStr(This.Parent.LoteCTR, 5, 11)

ENDPROC


PROCEDURE Talla.LostFocus

*> Trasladar la talla actual a la variable.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.

This.Parent.LoteCTR = SubStr(This.Parent.LoteCTR, 1, 4) + This.Parent.aTalla[This.ListItemID] + SubStr(This.Parent.LoteCTR, 9, 7)

ENDPROC
PROCEDURE Talla.GotFocus

*> Posicionarse en la talla actual.

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.

Private cWhere
Local cColorL, cTallaL, nInx, nItem, lx

cColorL = SubStr(This.Parent.LoteCTR, 1, 4)
cTallaL = SubStr(This.Parent.LoteCTR, 5, 4)

*> Cargar las tallas asociadas al color actualmente seleccionado.
Use In (Select ("F08VTALL"))

nInx = 0

cWhere =          "F08vCodPro='" + This.Parent._codpro + "' And F08vCodArt='" + This.Parent._codart + "' And "
cWhere = cWhere + "F08vCodCol='" + cColorL + "'"
cField = "Distinct F08vCodTal As F08vCodTal"

=f3_sql(cField, "F08v", cWhere, , , "F08VTALL")
Select F08VTALL
Go Top
Do While !Eof()
	nInx = nInx + 1
	Dimension This.Parent.aTalla[nInx]
	This.Parent.aTalla[nInx] = F08vCodTal
	Skip
EndDo

*> Borrar el contenido y asignar de nuevo.
With This
	.RowSourceType = 0						&& Esto se necesita para el Clear.
	.Clear
	.RowSourceType = 5

	nItem = AScan(.Parent.aTalla, cTallaL)
	.ListItemID = Iif(nItem > 0, nItem, 1)
EndWith

lx = This.Parent.LabelLote
If Type('lx')=='C'
	lx = lx + ".Caption='" + _TCLTIPT + "'"
	&lx
EndIf

Use In (Select ("F08VTALL"))
Return

ENDPROC


PROCEDURE Resto.LostFocus

*> Trasladar el resto a la variable.

*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.
*>					Modificado método This.Color.GotFocus

This.Parent.LoteCTR = SubStr(This.Parent.LoteCTR, 1, 8) + Left(This.Value, 7)

ENDPROC
PROCEDURE Resto.GotFocus

*> Visualizar el resto del lote.

*> 18.03.2008 (AVC) Pasar código de tono/talla, calibre/color de 5c. a 4c.
*>					Modificado método This.Color.GotFocus

Local cRestoL, lx

cRestoL = SubStr(This.Parent.LoteCTR, 9, 7)
This.Value = cRestoL

lx = This.Parent.LabelLote
If Type('lx')=='C'
	lx = lx + ".Caption='" + _TCLTIPL + "'"
	&lx
EndIf

ENDPROC


EndDefine 
Define Class lotebase as textbox
FontName = "Courier"
Height = 18
InputMask = "XXXXXXXXXXXXXXX"
Margin = 0
Width = 147
DisabledBackColor = Rgb(192,192,192)
DisabledForeColor = Rgb(0,0,128)



PROCEDURE initialize

*> Asignar propietario / artículo asociado a la clase.

Parameters cCodPro, cCodArt

With This
	._codpro = cCodPro
	._codart = cCodArt
EndWith

ENDPROC
PROCEDURE ___historial___de___modificaciones

*> Historial de modificaciones:

*> 18.03.2008 (AVC) Validar valor parámetro _GESTIONTCL.
*>					Modificado método This.GotFocus

ENDPROC
PROCEDURE GotFocus

*> Historial de modificaciones:
*> 18.03.2008 (AVC) Validar valor parámetro _GESTIONTCL (Ver MSI2.INI)
*>	S: TC asociado al artículo.
*>	N: No hay control TCL.
*>	A: Control TCL segun ficha artículo.

Local lx

With This
	If Empty(._codpro) .Or. Empty(._codart)
		*> No hay artículo asociado: Actuar como un campo de lote.
		Return
	EndIf

	*> Leer los parámetros del artículo.
	lx = ._codpro
	m.F08cCodPro = &lx
	lx = ._codart
	m.F08cCodArt = &lx

	If f3_seek("F08c")
		._ctrcol = Iif(_GESTIONTCL=='A', F08c.F08cColors, "N")
		._ctrtal = Iif(_GESTIONTCL=='A', F08c.F08cTallas, "N")
	EndIf
EndWith

ENDPROC
PROCEDURE Init

=DoDefault()

*> Inicializar las propiedades de la clase.
With This
	._ctrtal = "N"					&& Control tono / talla.
	._ctrcol = "N"   				&& Control calibre / color.
EndWith

ENDPROC


EndDefine 
Define Class lote as container
Width = 157
Height = 55

ADD OBJECT Lotebase1 as lotebase WITH Height = 17, Left = 0, Top = 0, Width = 134
ADD OBJECT Lotecr1 as lotecr WITH Top = 16, Left = 0, Width = 122, Height = 20
ADD OBJECT Lotectr1 as lotectr WITH Top = 36, Left = 0, Width = 156, Height = 20


PROCEDURE LostFocus

Private lx, lx1

*> Devolver el aspecto normal al campo.
With This
	Do Case
		*> Tiene tono / talla (con ó sin calibre / color).
		Case This.LoteBase1._ctrtal=='S'
			.Value = .LoteCTR1.LoteCTR

		*> Tiene calibre / color.
		Case This.LoteBase1._ctrcol=='S'
			.Value = .LoteCR1.LoteCTR

		*> Sin tono / talla ni calibre / color.
		Otherwise
			.Value = .LoteBase1.Value
	EndCase

	.LoteCTR1.Visible = .F.
	.LoteCR1.Visible = .F.
	.LoteBase1.Visible = .T.
	.LoteBase1.Value = .Value
	.Width = ._oldwidth

	*> Asignar valor a la variable asociada.
	If Type('.ControlSource')=='C'
		lx = .ControlSource
		&lx = .Value
	EndIf

	*> Devolver la descripción a la etiqueta.
	lx = This.LabelLote
	If Type('lx')=='C'
		lx1 = lx + ".Tag"
		lx = lx + ".Caption=" + lx1
		&lx
	EndIf
EndWith

ENDPROC
PROCEDURE Init

Local lx, lx1

=DoDefault()

*> Por defecto, mostrar sólo el control de texto.
With This
	._oldwidth = .LoteBase1.Width
	.LoteBase1.Visible = .T.
	.LoteCTR1.Visible = .F.
	.LoteCR1.Visible = .F.
	.Width = .LoteBase1.Width
	.Height = .LoteBase1.Height

	*> Guardar la etiqueta original.
	lx = This.LabelLote
	If Type('lx')=='C'
		lx1 = lx + ".Tag=" + lx + ".Caption"
		&lx1
	EndIf
EndWith

ENDPROC


PROCEDURE Lotebase1.GotFocus

Local lx

*> Copiar propiedades de usuario a propiedades privadas.
With This
	=.Initialize(.Parent.CodPro, .Parent.CodArt)
EndWith

=DoDefault()

*> Asignar variable asociada a propiedad.
If Type('This.Parent.ControlSource')=='C'
	lx = This.Parent.ControlSource
	This.Parent.Value = &lx
	This.Parent.Value = PadR(This.Parent.Value, 15)
Else
	This.Parent.Value = Space(15)
EndIf

*> Decidir cuál es el control que se muestra en edición.
Do Case
	*> Tiene colores y tallas.
*!*		Case This._ctrcol=='S' .And. This._ctrtal=='S'
*!*			With This
*!*				.Visible = .F.
*!*				.Parent.Width = .Parent.LoteCTR1.Width
*!*				.Parent.LoteCTR1.Visible = .T.
*!*				.Parent.LoteCTR1.Height = .Height
*!*				.Parent.LoteCTR1.Top = .Top
*!*				.Parent.LoteCTR1.Left = .Left
*!*				.Parent.LoteCTR1.LoteCTR = .Parent.Value
*!*				=.Parent.LoteCTR1.Initialize(.Parent.CodPro, .Parent.CodArt, This._ctrcol, This._ctrtal)
*!*			EndWith

	*> Tiene tono / talla (con ó sin calibre / color)
	Case This._ctrtal=='S'
		With This
			.Visible = .F.
			.Parent.Width = .Parent.LoteCTR1.Width
			.Parent.LoteCTR1.Visible = .T.
			.Parent.LoteCTR1.Height = .Height
			.Parent.LoteCTR1.Top = .Top
			.Parent.LoteCTR1.Left = .Left
			.Parent.LoteCTR1.LoteCTR = .Parent.Value
			.Parent.LoteCTR1.LabelLote = .Parent.LabelLote
			=.Parent.LoteCTR1.Initialize(.Parent.CodPro, .Parent.CodArt, This._ctrcol, This._ctrtal)
			.Parent.LoteCTR1.SetFocus
		EndWith

	*> Tiene calibre / color.
	Case This._ctrcol=='S'
		With This
			.Visible = .F.
			.Parent.Width = .Parent.LoteCR1.Width
			.Parent.LoteCR1.Visible = .T.
			.Parent.LoteCR1.Height = .Height
			.Parent.LoteCR1.Top = .Top
			.Parent.LoteCR1.Left = .Left
			.Parent.LoteCR1.LoteCTR = .Parent.Value
			.Parent.LoteCR1.LabelLote = .Parent.LabelLote
			=.Parent.LoteCR1.Initialize(.Parent.CodPro, .Parent.CodArt)
			.Parent.LoteCR1.SetFocus
		EndWith

	*> Sin tono / talla ni calibre / color.
	Otherwise
		This.Value = This.Parent.Value
		This.SetFocus
EndCase

ENDPROC


EndDefine 
Define Class codbar as custom
Height = 19
Width = 99



PROCEDURE inicializar

*> Inicializar las propiedades públicas de la clase.

With This
	*> Propiedades de uso interno de la clase.
	.InitLocals

	*> Propiedades correspondientes a rangos.
	.RNUbiIni = ""				&& Ubicación inicial.
	.RNUbiFin = ""				&& Ubicación final.

	.RNPalIni = ""				&& Nº palet inicial.
	.RNPalFin = ""				&& Nº palet final.	
	.RNProIni = ""				&& Propietario inicial.
	.RNProFin = ""				&& Propietario final.	
	.RNArtIni = ""				&& Artículo inicial.
	.RNArtFin = ""				&& Artículo final.	
	.RNLstIni = ""				&& Lista inicial.
	.RNLstFin = ""				&& Lista final.	
	.RNLotIni = ""				&& Lote inicial.
	.RNLotFin = ""				&& Lote final.
	.RNEntIni = ""				&& Nº entrada inicial.
	.RNEntFin = ""				&& Nº entrada final.
	.RNIdOIni = ""				&& ID ocupación inicial.
	.RNIdOFin = ""				&& ID ocupación final.

	.RNCodNum = ""				&& Código de numeración.
	.RNNumEti = ""				&& Nº de etiquetas.

	.RNMacIni = ""				&& Nº de MAC inicial.
	.RNMacFin = ""				&& Nº de MAC final.
	.RNAlbIni = ""				&& Nº de albarán de salida inicial.
	.RNAlbFin = ""				&& Nº de albarán de salida final.
	.RNTDoIni = ""				&& Tipo de documento inicial.
	.RNTDoFin = ""				&& Tipo de documento final.
	.RNNDoIni = ""				&& Nº de documento inicial.
	.RNNDoFin = ""				&& Nº de documento final.
	.RNFDoIni = ""				&& Fecha de documento inicial.
	.RNFDoFin = ""				&& Fecha de documento final.
	.RNCliIni = ""				&& Cliente inicial.
	.RNCliFin = ""				&& Cliente final.

	*> Rangos que también son propiedades (mediante This.setparams).
	.RNModo = ""				&& Modo clasificación ubicaciones (V/H).
	.RNModoBulto = ""			&& Modo impresión bultos (D/B).
	.RNFormato = ""				&& Formato de las etiquetas.
	.RNOrigen = ""				&& Origen de los datos.
	.RNPrinter = ""				&& Dispositivo de salida (generalmente impresora).
	.RNCursor = ""				&& Cursor para etiquetas directas.

	*> Otras propiedades.
	.UsrError = ""				&& Texto errores.

	*> Inicializar estado ficheros.
	.lF08cIsOpen = Used('F08c')
	If !.lF08cIsOpen
		=This.oProcaot.OpenTabla('F08c')
	EndIf

	.lF00jIsOpen = Used('F00j')
	If !.lF00jIsOpen
		=This.oProcaot.OpenTabla('F00j')
	EndIf
EndWith

Return

ENDPROC
PROCEDURE initlocals

*> Inicializar las propiedades protegidas de la clase.

*> Historial de modificaciones:
*> 16.05.2008 (AVC) Agregar propiedad This.LSNumLst
*> 19.05.2008 (AVC) Agregar propiedad This.OCDocOcu

With This
	.oCabEti = .F.				&& Registro del formato etiquetas.
	.cDetEti = .F.				&& Detalle parámetros formato.
	.nNumPrm = 0				&& Nº de parámetros del formato.
	._LDelimiter = '[#'			&& Delimitador de conceptos (izquierdo).
	._RDelimiter = '#]'			&& Delimitador de conceptos (derecho).

	*> Propiedades generales de la clase.
	._Origen = ""				&& Origen de los datos (UB/MP/OC/...)
	._Formato = ""				&& Formato etiquetas.
	._Printer = ""				&& Dispositivo de salida.
	._TipoFormato = .F.			&& Tipo de formato esperado.
	._Cursor = ""				&& Cursor etiquetas directas.

	*> Propiedades generales de uso en impresión.
	._Copias = 0				&& Copias de etiqueta.
	._Date = DToC(Date())		&& Fecha del día.
	._Logo1 = ""				&& Logo principal.
	._Logo2 = ""				&& Logo secundario.

	*> Propiedades para impresión de etiquetas de ubicación: UBxxxxxx.
	.UBCodUbi = ""				&& Código de ubicación (completo).
	.UBUbiNew = ""				&& Código de ubicación se segundo nivel (completo).
	.UBUbiShort = ""			&& Código de ubicación (reducido).
	.UBDigCtl = ""				&& Dígito de control.
	.UBDigCtlNew = ""			&& Dígito de control (Ubicación segundo nivel).

	*> Propiedades para impresión de etiquetas de picking: PKxxxxxx.
	.PKCodPro = ""				&& Propietario (para ubicaciones de picking).
	.PKCodArt = ""				&& Artículo (para ubicaciones de picking).
	.PKDesArt = ""				&& Descripción (para ubicaciones de picking).
	.PKEanArt = ""				&& Código de barras.

	*> Propiedades para impresión de etiquetas de ocupación: OCxxxxxx.
	.OCNumPal = ""				&& Nº de palet.
	.OCNumLot = ""				&& Nº de lote.
	.OCFecCad = ""				&& Fecha de caducidad.
	.OCNumEnt = ""				&& Nº de entrada.
	.OCFecEnt = ""				&& Fecha de entrada.
	.OCCanFis = ""				&& Cantidad.
	.OCPesPal = ""				&& Peso palet.
	.OCVolPal = ""				&& Volumen palet.
	.OCDocOcu = ""				&& Nº documento.

	.EXDesPro = ""				&& Nombre propietario.
	.EXDirPro = ""				&& Dirección propietario.
	.EXPobPro = ""				&& Población propietario.
	.EXProPro = ""				&& Provicia propietario.
	.EXPosPro = ""				&& Código postal propietario.
	.EXTelPro = ""				&& Teléfono propietario.
	.EXFaxPro = ""				&& Nº de FAX.
	.EXEMail  = ""				&& e-mail propietario.
	.EXPagWeb  = ""				&& Web propietario.
	.EXLogoP  = ""				&& Logo propietario.

	.EXCodTra = ""				&& Transportista.
	.EXObserv = ""				&& Observaciones.
	.EXNumDoc =	""				&& Nº documento.
	.EXFecDoc =	""				&& Fecha documento.
	.EXNumAlb =	""				&& Nº albarán.
	.EXBultos =	""				&& Bultos.
	.EXCurBul =	""				&& Bulto actual.
	.EXKilos  =	""				&& Kilos.
	.EXHojRut =	""				&& Hoja de ruta.
	.EXDesTra = ""				&& Nombre transportista.
	.EXNomAso = ""				&& Nombre envío.
	.EX1erDir = ""				&& Dirección 1.
	.EX2ndDir = ""				&& Dirección 2.
	.EX3rdDir = ""				&& Dirección 3.
	.EXDPobla = ""				&& Población.
	.EXDProvi = ""				&& Provicia.
	.EXCodPos = ""				&& Código postal.
	.EXNumTel = ""				&& Teléfono.
	.EXNumFax = ""				&& Nº de FAX.
	.EXNumNif = ""				&& NIF / DNI.

	.EXNumMac = ""				&& Nº de MAC.
	.EXPortes = ""				&& Portes (debidos / pagados).

	.LSNumLst = ""				&& Nº de lista.

	*> Propiedades para impresión de etiquetas de numeración: NUxxxxxx.
	.NUValNum = ""				&& Numeración.
EndWith

Return

ENDPROC
PROCEDURE setparams

*> Actualizar parámetros de la clase (Propiedades, rangos, ...)

*> Recibe:
*> cParams ----> Lista de parámetros opcionales, con el formato:
*>              [ORIGEN='UB',										ORG/OR
*>               FORMATO='XXXX',									FRM/FR
*>               PRINTER='XXXXXXXX',								PRT/PR
*>               COPIAS=1,											COP/CO
*>				 LLIMIT='[#',										LLI/LL
*>				 RLIMIT='#]',										RLI/RL
*>				 LOGO1='XXXX',										LO1/L1
*>				 LOGO2='XXXX']										LO2/L2

*> Devuelve: .T.

LParameters cParams

Local nd, cPrm, cVal, cUpd, cTmp

If Type('cParams') # 'C'
   Return
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      *> Origen de los datos.
      Case cPrm=='ORIGEN' .Or. cPrm=='ORG' .Or. cPrm=='OR'
         This._Origen = cVal

      *> Formato de etiquetas.
      Case cPrm=='FORMATO' .Or. cPrm=='FRM' .Or. cPrm=='FR'
         This._Formato = cVal

      *> Dispositivo de salida.
      Case cPrm=='PRINTER' .Or. cPrm=='PRT' .Or. cPrm=='PR'
         This._Printer = cVal

      *> Nº de copias de cada etiqueta.
      Case cPrm=='COPIAS' .Or. cPrm=='COP' .Or. cPrm=='CO'
         This._Copias = Val(cVal)

      *> Logo principal.
      Case cPrm=='LOGO1' .Or. cPrm=='LO1' .Or. cPrm=='L1'
         This._Logo1 = cVal

      *> Logo secundario.
      Case cPrm=='LOGO2' .Or. cPrm=='LO2' .Or. cPrm=='L2'
         This._Logo2 = cVal

      *> Delimitador izquierdo.
      Case cPrm=='LLIMIT' .Or. cPrm=='LLI' .Or. cPrm=='LL'
         This._LDelimiter = cVal

      *> Delimitador derecho.
      Case cPrm=='RLIMIT' .Or. cPrm=='RLI' .Or. cPrm=='RL'
         This._RDelimiter = cVal

      *> No es un parámetro reconocido.
      Otherwise
   EndCase

EndDo

ENDPROC
PROCEDURE _loadparametros

*> Carga los parámetros del formato elegido en memoria.
*> Método privado de la clase.

*> Recibe:
*>	- Tipo formato.
*>	- This._Formato, formato de las etiquetas.

*> Devuelve:
*>	- Resultado .T. / .F.
*>	- This.UsrError, texto del error.
*>	- This.nNumPrm, Nº de parámetros del formato.
*>	- This.cDetEti, parámetros del formato.

Parameters cTipoFormato

Local lStado
Local cDetEti

Store .T. To lStado
This.UsrError = ""

*> Validar los parámetros de cabecera.
lStado = File("CABETI.DBF")
If !lStado
	This.UsrError = "No existe el archivo de formatos"
	Return lStado
EndIf

*> Cargar los parámetros de la cabecera del formato de etiqueta.
Use CABETI In 0 Order 1 Shared NoUpdate
Select CABETI
Locate For Formato==This._Formato
lStado = Found()
If !lStado
	This.UsrError = "No se encuentra el formato"
	Use In (Select ("CABETI"))
	Return lStado
EndIf

*> Validar el tipo de formato cargado.
If !Empty(cTipoFormato) .And. Tipo <> cTipoFormato
	This.UsrError = "Tipo formato erróneo"
	Use In (Select ("CABETI"))
	lStado = .F.
	Return lStado
EndIf

*> Guardar el registro de formato en propiedad.
Scatter Name This.oCabEti

*> Guardar propiedades del formato modificables por el usuario.
With This
	._Copias = Iif(Empty(._Copias), .oCabEti.Copias, ._Copias)		&& Copias de etiquetas.
EndWith

Use In (Select ("CABETI"))

*> Formar el nombre del archivo de detalle de parámetros.
cDetEti = "DETETI" + AllTrim(This._Formato) + ".DBF"
lStado = File(cDetEti)
If !lStado
	This.UsrError = "No existe el archivo de detalle de parámetros"
	Return lStado
EndIf

*> Cargar los parámetros del formato de la etiqueta.
Use (cDetEti) In 0 Order 1 Shared NoUpdate Alias DETETI
Select DETETI
Go Top
lStado = !Eof()
If !lStado
	This.UsrError = "El archivo de detalle de parámetros está vacío"
	Use In (Select (cDetEti))
	Return lStado
EndIf

This.nNumPrm = 0								&& Contador del nº de parámetros del formato.

Do While !Eof()
	With This
		.nNumPrm = .nNumPrm + 1					&& Nº total de parámetros.
		Dimension .cDetEti[.nNumPrm]			&& Redimensionar array de formato.
		Scatter Name .cDetEti[.nNumPrm]			&& Datos de la línea.
	EndWith

	Skip
EndDo

Use In (Select ("DETETI"))
Return lStado

ENDPROC
PROCEDURE etiquetaubicacion

*> Impresión de etiquetas de ubicación.
*> Imprime tanto etiquetas de ubicación como de dígito de control, dependiendo del formato elegido.

*> Recibe:
*>	- Ubicación inicial (opcional)
*>	- Ubicación final (opcional)
*>	- Modo (V/H) (opcional)
*>	- This.RNUbiIni, ubicación inicial.
*>	- This.RNUbiFin, ubicación final.

*> Devuelve:
*>	- Estado (.T. / .F.)

*> Historial de modificaciones:
*>	- Agregar ubicación de segundo nivel para las etiquetas de señalización. AVC 20.12.2005

Parameters cUbicacionDesde, cUbicacionHasta, cModo

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNUbiIni = IIf(Type('cUbicacionDesde')=='C', cUbicacionDesde, .RNUbiIni)
	.RNUbiFin = IIf(Type('cUbicacionHasta')=='C', cUbicacionHasta, .RNUbiFin)
	.RNModo = IIf(Type('cModo')=='C', cModo, .RNModo)
EndWith

*> Validar los rangos.
lStado = This._etubvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("UB")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETUBCURSOR.
lStado = This._etubcargardatos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select ETUBCURSOR
Go Top
Do While !Eof()
	*> Asignar valores a las propiedades de la clase.
	Scatter Name oCUR
	lStado = This._etubasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()

	*> Next ubicación.
	Select ETUBCURSOR
	Skip
EndDo

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETUBCURSOR"))
Return lStado

ENDPROC
PROCEDURE etiquetaubicacionpicking

*> Impresión de etiquetas de ubicación (picking).

*> Recibe:
*>	- Ubicación inicial (opcional).
*>	- Ubicación final (opcional).
*>	- Modo (V/H) (opcional)
*>	- This.RNUbiIni, ubicación inicial.
*>	- This.RNUbiFin, ubicación final.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cUbicacionDesde, cUbicacionHasta, cModo

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNUbiIni = IIf(Type('cUbicacionDesde')=='C', cUbicacionDesde, .RNUbiIni)
	.RNUbiFin = IIf(Type('cUbicacionHasta')=='C', cUbicacionHasta, .RNUbiFin)
	.RNModo = IIf(Type('cModo')=='C', cModo, .RNModo)
EndWith

*> Validar los rangos.
lStado = This._etpkvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("PK")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETPKCURSOR.
lStado = This._etpkcargardatos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select ETPKCURSOR
Go Top
Do While !Eof()
	*> Asignar valores a las propiedades de la clase.
	Scatter Name oCUR
	lStado = This._etpkasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()

	*> Next ubicación.
	Select ETPKCURSOR
	Skip
EndDo

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETPKCURSOR"))
Return lStado

ENDPROC
PROCEDURE etiquetaocupacion

*> Impresión de etiquetas de ocupación.
*> En función del origen de los datos, lee de ocupaciones o de MPs (de entrada).

*> Recibe:
*>	- Ubicación (opcional)
*>	- Propietario / Artículo (opcional)
*>	- This.RNxxxxxx, rangos pasados como propiedades.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cUbicacion, cPropietario, cArticulo

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNUbiIni = IIf(Type('cUbicacion')=='C', cUbicacion, .RNUbiIni)
	.RNProIni = IIf(Type('cPropietario')=='C', cPropietario, .RNProIni)
	.RNArtIni = IIf(Type('cArticulo')=='C', cArticulo, .RNArtIni)

	*> Asignar rangos hasta por defecto.
	.RNUbiFin = IIf(Empty(.RNUbiFin), .RNUbiIni, .RNUbiFin)
	.RNProFin = IIf(Empty(.RNProFin), .RNProIni, .RNProFin)
	.RNArtFin = IIf(Empty(.RNArtFin), .RNArtIni, .RNArtFin)
EndWith

*> Validar los rangos.
lStado = This._etocvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("OC")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETOCCURSOR.
lStado = This._etoccargardatos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select ETOCCURSOR
Go Top
Do While !Eof()
	*> Asignar valores a las propiedades de la clase.
	Scatter Name oCUR
	lStado = This._etocasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()

	*> Next ocupación.
	Select ETOCCURSOR
	Skip
EndDo

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETOCCURSOR"))
Return lStado

ENDPROC
PROCEDURE _etubvalidarrangos

*> Validación de los rangos (etiquetas de ubicación).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacion(), impresión de etiquetas de ubicación.
*>	- This._etpkvalidarrangos(), validar rangos de etiquetas de picking.

*> Recibe:
*>	- This.RNUbiIni, ubicación desde.
*>	- This.RNUbiFin, ubicación hasta.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

*> Validar los rangos de impresión de etiquetas de ubicación.
With This
	*> Rangos desde-hasta.
	If .RNUbiIni > .RNUbiFin
		.UsrError = "Rangos desde-hasta erróneos"
		lStado = .F.
		Return lStado
	EndIf

	*> Modo ordenación ubicación.
	If Empty(.RNModo) .Or. At(.RNModo, "VH")==0
		.RNModo = "V"								&& Por defecto, por ubicación.
	EndIf
EndWith

*> En función del modo de ordenación de ubicaciones, desglosar.
If This.RNModo=='H'
	This._etdesglosarrangoubicacion
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE etiquetanumeracion

*> Impresión de etiquetas de numeración.
*> Imprime etiquetas de número de:
*>	- Palet.
*>	- MAC.

*> Recibe:
*>	- Tipo de numeración (Opcional):
*>		P: Etiqueta de nº de palet.
*>		M: Etiqueta de nº de MAC.
*>	- Nº de etiquetas a imprimir (opcional).

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cTipoNumeracion, nCantidadEtiquetas

Local lStado, nInx
Private cNumero

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNCodNum = IIf(Type('cTipoNumeracion')=='C', cTipoNumeracion, .RNCodNum)
	.RNNumEti = IIf(Type('nCantidadEtiquetas')=='N', nCantidadEtiquetas, .RNNumEti)
EndWith

*> Validar los rangos.
lStado = This._etnuvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("NU")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
With This
	For nInx = 1 To .RNNumEti
		Do Case
			*> Etiqueta de número de palet.
			Case .RNCodNum=='P'
				cNumero = Ora_NewPal()

			*> Etiqueta de número de MAC.
			Case .RNCodNum=='M'
				cNumero = Ora_NewMac()
		EndCase

		If Empty(cNumero) .Or. IsNull(cNumero)
			.UsrError = "No se pudo obtener la numeración"
			lStado = .F.
			Return lStado
		EndIf

		*> Asignar valores a las propiedades de la clase.
		.NUValNum = cNumero

		*> Imprimir la etiqueta.
		lStado = This._etimprimiretiqueta()
	EndFor
EndWith

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Return lStado

ENDPROC
PROCEDURE _etubcargardatos

*> Generar datos para la impresión de etiquetas de ubicación.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacion(), impresión de etiquetas de ubicación.

*> Recibe:

*> Genera:
*>	- ETUBCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

*> Historial de modificaciones:
*>	- Agregar ubicación de segundo nivel para las etiquetas de señalización. AVC 20.12.2005

Local lStado, cCurLevel, cNxtLevel, cNewUbi
Private cWhere, cField, cOrder

This.UsrError = ""

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("UB")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

If This.RNModo=='V'
	*> Orden de clasificación normal.
	cWhere = "F10cCodUbi Between '" + This.RNUbiIni + "' And '" + This.RNUbiFin + "'"
Else
	cWhere =          _GCSS("F10cCodUbi", 1, 4) + " Between + '" + This.cAlmI + "' And '" + This.cAlmF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 5, 2) + " Between + '" + This.cZonI + "' And '" + This.cZonF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 7, 2) + " Between + '" + This.cCalI + "' And '" + This.cCalF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 9, 3) + " Between + '" + This.cFilI + "' And '" + This.cFilF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi",12, 2) + " Between + '" + This.cPisI + "' And '" + This.cPisF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi",14, 1) + " Between + '" + This.cPrfI + "' And '" + This.cPrfF + "'"
EndIf

cField = "*"
cOrder = "F10cCodUbi"

lStado = f3_sql(cField, "F10c", cWhere, cOrder, , "ETUBCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Use In (Select ("ETUBCURSOR"))
	Return lStado
EndIf

*> Cargar la ubicación de segundo nivel.
Select ETUBCURSOR
Go Top
Do While !Eof()
	Scatter Name oUB

	cNewUbi = oUB.F10cCodUbi
	cCurLevel = SubStr(cNewUbi, 12, 2)
	cNxtLevel = PadL(AllTrim(Str(Val(cCurLevel) + 1)), 2, '0')

	cNewUbi = SubStr(cNewUbi, 1, 11) + cNxtLevel + SubStr(cNewUbi, 14, 1)
    m.F10cCodUbi = cNewUbi
    If f3_seek("F10c")
		Select ETUBCURSOR
		Replace F10cUbiNew With cNewUbi
	Else
		Select ETUBCURSOR
	EndIf

	Skip
EndDo

Return lStado

ENDPROC
PROCEDURE _etocvalidarrangos

*> Validación de los rangos (etiquetas de ocupación).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaOcupacion(), impresión de etiquetas de ocupación.

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

*> Historial de modificaciones:
*>	- Eliminar rangos por lista. AVC - 14.12.2005

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

With This
	*> Rangos desde-hasta.
	If .RNUbiIni > .RNUbiFin .Or. ;
	   .RNPalIni > .RNPalFin .Or. ;
	   .RNProIni > .RNProFin .Or. ;
	   .RNArtIni > .RNArtFin .Or. ;
	   .RNLotIni > .RNLotFin .Or. ;
	   .RNEntIni > .RNEntFin .Or. ;
	   .RNIdOIni > .RNIdOFin
		.UsrError = "Rangos desde-hasta erróneos"
		lStado = .F.
		Return lStado
	EndIf

	*> Origen de los datos.
	If Empty(.RNOrigen)
		.UsrError = "Falta origen de datos"
		lStado = .F.
		Return lStado
	EndIf

	*> Modo ordenación ubicación.
	If Empty(.RNModo) .Or. At(.RNModo, "VH")==0
		.RNModo = "V"								&& Por defecto, por ubicación.
	EndIf

	*> Asignar rangos a propiedades.
	._Origen = .RNOrigen
EndWith

*> En función del modo de ordenación de ubicaciones, desglosar.
If This.RNModo=='H'
	This._etdesglosarrangoubicacion
EndIf

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etdesglosarrangoubicacion

*> Desglosar rango ubicación en almacén, zona, calle, etc .
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacion(), impresión de etiquetas de ubicación.
*>	- This.EtiquetaOcupacion(), impresión de etiquetas de ocupación.

*> Recibe:
*>	- This.RNUbiIni, ubicación desde.
*>	- This.RNUbiFin, ubicación hasta.

*> Devuelve: .T.

With This
	.cAlmI = SubStr(.RNUbiIni, 1, 4)
	.cAlmF = SubStr(.RNUbiFin, 1, 4)
	.cZonI = SubStr(.RNUbiIni, 5, 2)
	.cZonF = SubStr(.RNUbiFin, 5, 2)
	.cCalI = SubStr(.RNUbiIni, 7, 2)
	.cCalF = SubStr(.RNUbiFin, 7, 2)
	.cFilI = SubStr(.RNUbiIni, 9, 3)
	.cFilF = SubStr(.RNUbiFin, 9, 3)
	.cPisI = SubStr(.RNUbiIni,12, 2)
	.cPisF = SubStr(.RNUbiFin,12, 2)
	.cPrfI = SubStr(.RNUbiIni,14, 1)
	.cPrfF = SubStr(.RNUbiFin,14, 1)
EndWith

Return

ENDPROC
PROCEDURE _etubasignarvalores

*> Asignar valores del registro activo a propiedades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacion(), impresión de etiquetas de ubicación.

*> Recibe:
*>	- oUbi, registro actual de la ubicación.

*> Devuelve: .T.

*> Historial de modificaciones:
*>	- Agregar ubicación de segundo nivel para las etiquetas de señalización. AVC 20.12.2005

Parameters oUBI

With This
	.UBDigCtl = Ora_DigCtl(oUBI.F10cCodUbi)
	.UBDigCtlNew = Ora_DigCtl(oUBI.F10cUbiNew)
	.UBCodUbi = oUBI.F10cCodUbi
	.UBUbiNew = oUBI.F10cUbiNew
	.UBUbiShort = Substr(oUBI.F10cCodUbi, 5, 1) + ;
				  SubStr(oUBI.F10cCodUbi, 7, 2) + ;
				  SubStr(oUBI.F10cCodUbi, 10, 2) + ;
				  Substr(oUBI.F10cCodUbi, 13, 1)
EndWith

Return

ENDPROC
PROCEDURE _etimprimiretiqueta

*> Impresión de etiquetas (general).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacion(), impresión de etiquetas de ubicación.
*>	- This.EtiquetaOcupacion(), impresión de etiquetas de ocupación.
*>	- This.EtiquetaNumeracion(), impresión de etiquetas de de numeración (palet ó MAC).

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto error.

Local lStado, nInx
Private cPrm

This.UsrError = ""
lStado = .T.

For nInx = 1 To This.nNumPrm
	*> Substituir conceptos por su valor.
	cPrm = Alltrim(This.cDetEti[nInx].Parametro)	&& Línea de caracteres de control impresora.
	lStado = This._etchgconceptbyvalue(cPrm)		&& Cambiar concepto por su valor.
	If !lStado
		Return lStado
	EndIf

	? This.cLineaEtiqueta							&& Enviar a dispositivo de salida.
EndFor

Return lStado

ENDPROC
PROCEDURE _etchgconceptbyvalue

*> Proceso para traducir un concepto de impresión por su valor.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etimprimiretiqueta(), impresión general de etiquetas.

*> Recibe:
*>	- cPrm, línea de parámetros impresora.
*>	- This._LDelimiter, delimitador izquierdo para conceptos.
*>	- This._RDelimiter, delimitador derecho para conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto error.
*>	- This.cLineaEtiqueta, buffer de impresión.

*> Codificación de conceptos:
*>	- CODPRO, código de artículo.
*>	- CODART, código de artículo.
*>	- DESART, descripción del artículo.
*>	- EANART, código de barras del artículo.

*>	- CODUBI, código de ubicación completo.
*>	- UBINEW, código de ubicación de segundo nivel.
*>	- CONTROL, dígito de control de la ubicación.
*>	- CONTROLN, dígito de control de la ubicación de segundo nivel.
*>	- UBISHORT, código de ubicación reducido.

*>	- PALET, nº de palet.
*>	- LOTE, nº de lote.
*>	- ENTRADA, nº de entrada.
*>	- FECENT, fecha de entrada.
*>	- FECCAD, fecha de caducidad.
*>	- CANTID, cantidad física.
*>	- PESPAL, peso palet.
*>	- VOLPAL, volumen palet.
*>	- DOCOCU, documento ocupación.

*>	- VALNUM, numeración (palet ó MAC).

*>	- CODTRA, código de transportista.
*>	- DESTRA, nombre del transportista.
*>	- DESPRO, nombre del propietario.
*>	- NUMDOC, nº documento.
*>	- FECDOC, fecha documento.
*>	- ALBARAN, nº albarán.
*>	- RUTA, hoja de ruta.
*>	- BULTOS, nº de bultos.
*>	- NUMBUL, bulto actual.
*>	- KILOS, nº de kilos.
*>	- MAC, nº de MAC.
*>	- PORTES, Portes (debidos / pagados).

*>	- DIRPRO, Dirección propietario.
*>	- POBPRO, Población propietario.
*>	- PROPRO, Provicia propietario.
*>	- POSPRO, Código postal propietario.
*>	- TELPRO, Teléfono propietario.
*>	- FAXPRO, Nº de FAX.
*>	- EMAIL, e-mail propietario.
*>	- WEB, Web propietario.
*>	- LOGOP, Logo propietario.
*>	- LOGOPN, Logo propietario, solo nombre.

*>	- NOMASO, Nombre envío.
*>	- 1DIR, Dirección 1.
*>	- 2DIR, Dirección 2.
*>	- 3DIR, Dirección 3.
*>	- POBLAC, Población.
*>	- PROVIN, Provicia.
*>	- CODPOS, Código postal.
*>	- NUMTEL, Teléfono.
*>	- FAX, Nº de FAX.
*>	- NIF, NIF / DNI.

*>	- LISTA, Nº de lista carga / trabajo.

*>	- COPIAS, copias etiqueta.
*>	- DATE, fecha de sistema.

*>	- SUBST, SubString del concepto(si es una cadena).
*>	- MASK, Máscara de formateo de un concepto numérico.
*>	- LTRIM, Eliminar espacios en blanco a la izquierda.
*>	- RTRIM, Eliminar espacios en blanco a la derecha.
*>	- CAT, Proceso de concatenación de conceptos.
*>	- FILE, Trata el concepto a continuación como un fichero.
*>	- SKIPR, Salta al siguiente regsitro y carga datos.

*> Historial de modificaciones:
*> 04.02.2008 (AVC) Agregar conceptos de empresa.
*>					Agregar comando SKIPR, para saltar a siguiente registro.
*> 14.03.2008 (AVC) Corregir redondeos de cantidades con decimales.
*> 16.05.2008 (AVC) Agregar conceptos de etiquetas de listas.

Parameters cPrm

Local lStado, nd1, nd2, cCodCon, cConcepto, cParams
Local lHayParametros, lConcat, cMask, nSubStD, nSubStL, lLTrim, lRTrim, cConcat, lFile, lSkipR

This.UsrError = ""
lStado = .T.
Store "" To cMask, cConcat
Store 0 To nSubStD, nSubStL
Store .F. To lHayParametros, lConcat, lLTrim, lRTrim, lFile

*> Inicializar el buffer de salida.
This.cLineaEtiqueta = ""

cParams = cPrm
nd1 = At(This._LDelimiter, cParams)

*> Selección de los parámetros recibidos.
Do While nd1 > 0
	*> Obtener el concepto a calcular.
	nd2 = At(This._RDelimiter, cParams)
	If nd2 > 0
		cCodCon = SubStr(cParams, nd1, nd2 - nd1 + 2)
		cConcepto = SubStr(cParams, nd1 + 2, nd2 - nd1  - 2)

		*> Obtener el valor del concepto a calcular.
		Do Case
			*> Código de propietario.
			Case cConcepto=='CODPRO'
				cValor = This.PKCodPro

			*> Código de artículo.
			Case cConcepto=='CODART'
				cValor = This.PKCodArt

			*> Descripción del artículo.
			Case cConcepto=='DESART'
				cValor = This.PKDesArt

			*> Código de barras del artículo.
			Case cConcepto=='EANART'
				cValor = This.PKEanArt

			*> Nº de palet.
			Case cConcepto=='PALET'
				cValor = This.OCNumPal

			*> Nº de lote.
			Case cConcepto=='LOTE'
				cValor = This.OCNumLot

			*> Nº de entrada.
			Case cConcepto=='ENTRADA'
				cValor = This.OCNumEnt

			*> Nº de documento.
			Case cConcepto=='DOCOCU'
				cValor = This.OCDocOcu

			*> Cantidad física.
			Case cConcepto=='CANTID'
				cValor = AllTrim(Str(This.OCCanFis, 12, 4))
				
				*> LRC. Si no hay cantidad, no se imprime.
				If Val(cValor)=0
					This.cLineaEtiqueta = ''
					Return lStado
				EndIf

			*> Fecha de entrada.
			Case cConcepto=='FECENT'
				cValor = DToC(This.OCFecEnt)

			*> Fecha de caducidad.
			Case cConcepto=='FECCAD'
				cValor = DToC(This.OCFecCad)

			*> Peso palet.
			Case cConcepto=='PESPAL'
				cValor = AllTrim(Str(This.OCPesPal, 12, 6))

			*> Volumen palet.
			Case cConcepto=='VOLPAL'
				cValor = AllTrim(Str(This.OCVolPal, 12, 6))

			*> Código de ubicación.
			Case cConcepto=='CODUBI'
				cValor = This.UBCodUbi

			*> Código de ubicación nivel superior.
			Case cConcepto=='UBINEW'
				cValor = This.UBUbiNew

			*> Código de ubicación reducido.
			Case cConcepto=='UBISHORT'
				cValor = This.UBUbiShort

			*> Dígito de control de la ubicación.
			Case cConcepto=='CONTROL'
				cValor = This.UBDigCtl

			*> Dígito de control de la ubicación de segundo nivel.
			Case cConcepto=='CONTROLN'
				cValor = This.UBDigCtlNew

			*> Numeración (de palet ó de MAC).
			Case cConcepto=='VALNUM'
				cValor = This.NUValNum

			*> Código de transportista.
			Case cConcepto=='CODTRA'
				cValor = This.EXCodTra

			*> Nombre del transportista.
			Case cConcepto=='DESTRA'
				cValor = This.EXDesTra

			*> Nombre del propietario.
			Case cConcepto=='DESPRO'
				cValor = This.EXDesPro

			*> Nº documento.
			Case cConcepto=='NUMDOC'
				cValor = This.EXNumDoc

			*> Fecha de documento.
			Case cConcepto=='FECDOC'
				cValor = DToC(This.EXFecDoc)

			*> Fecha de prevista.
			Case cConcepto=='FECPRE'
				cValor = DToC(This.EXFecPre)

				*> LRC. Si no hay fecha, no se imprime.
				If cValor='  /  /    '
					This.cLineaEtiqueta = ''
					Return lStado
				EndIf


			*> Albarán de salida.
			Case cConcepto=='ALBARAN'
				cValor = This.EXNumAlb

			*> Fecha albarán salida.
			Case cConcepto=='FECALB'
				cValor = IIf(Empty(This.EXFecAlb), '',DToC(This.EXFecAlb))

			*> Nº de hoja de ruta.
			Case cConcepto=='RUTA'
				cValor = This.EXHojRut

			*> Bultos.
			Case cConcepto=='BULTOS'
				cValor = AllTrim(Str(This.EXBultos))

			*> Bulto actual.
			Case cConcepto=='NUMBUL'
				cValor = AllTrim(Str(This.EXCurBul))

			*> Nº de MAC
			Case cConcepto=='MAC'
				cValor = This.EXNumMac

			*> Tipo de portes
			Case cConcepto=='PORTES'
				cValor = This.EXPortes

			*> Observaciones documento.
			Case cConcepto=='OBSCAB'
				cValor = This.EXObserv

			*> Kilos.
			Case cConcepto=='KILOS'
				cValor = AllTrim(Str(This.EXKilos, 12, 4))

			*> Nombre envío.
			Case cConcepto=='NOMASO'
				cValor = This.EXNomAso

			*> Dirección 1.
			Case cConcepto=='1DIR'
				cValor = This.EX1erDir

			*> Dirección 2.
			Case cConcepto=='2DIR'
				cValor = This.EX2ndDir

			*> Dirección 3.
			Case cConcepto=='3DIR'
				cValor = This.EX3rdDir

			*> Población.
			Case cConcepto=='POBLAC'
				cValor = This.EXDPobla

			*> Provincia.
			Case cConcepto=='PROVIN'
				cValor = This.EXDProvi

			*> Código postal.
			Case cConcepto=='CODPOS'
				cValor = This.EXCodPos

			*> Teléfono.
			Case cConcepto=='NUMTEL'
				cValor = This.EXNumTel

			*> Nº de FAX.
			Case cConcepto=='FAX'
				cValor = This.EXNumFax

			*> NIF / DNI.
			Case cConcepto=='NIF'
				cValor = This.EXNumNif

			*> Dirección propietario.
			Case cConcepto=='DIRPRO'
				cValor = This.EXDirPro

			*> Población propietario.
			Case cConcepto=='POBPRO'
				cValor = This.EXPobPro

			*> Provincia propietario.
			Case cConcepto=='PROPRO'
				cValor = This.EXProPro

			*> Código postal propietario.
			Case cConcepto=='POSPRO'
				cValor = This.EXPosPro

			*> Teléfono propietario.
			Case cConcepto=='TELPRO'
				cValor = This.EXTelPro

			*> FAX propietario.
			Case cConcepto=='FAXPRO'
				cValor = This.EXFaxPro

			*> e-mail propietario.
			Case cConcepto=='EMAIL'
				cValor = This.EXEMail

			*> Web propietario.
			Case cConcepto=='WEB'
				cValor = This.EXPagWeb

			*> Logo propietario.
			Case cConcepto=='LOGOP'
				cValor = This.EXLogoP

			*> Logo propietario (solo nombre).
			Case cConcepto=='LOGOPN'
				cValor = JustFName(This.EXLogoP)

			*> Lista trabajo / carga.
			Case cConcepto=='LISTA'
				cValor = This.LSNumLst

			*> Copias de etiqueta.
			Case cConcepto=='COPIAS'
				cValor = AllTrim(Str(This._Copias))

			*> Pais
			Case cConcepto=='CODPAS'
				cValor = Alltrim(This.EXCodPas)

			*> Fecha del sistema.
			Case cConcepto=='DATE'
				cValor = This._Date

			*> Es una máscara de formateo numérico.
			Case Left(cConcepto, 4)=='MASK'
				cMask = SubStr(cConcepto, 5)
				lHayParametros = .T.
				cValor = ""

			*> Es la definición de un substring.
			Case Left(cConcepto, 5)=='SUBST'
				nSubStD = Val(SubStr(cConcepto, 6, 3))
				nSubStL = Val(SubStr(cConcepto, 9, 3))
				lHayParametros = .T.
				cValor = ""

			*> Eliminar espacios a la izquierda.
			Case Left(cConcepto, 5)=='LTRIM'
				lLTrim = .T.
				lHayParametros = .T.
				cValor = ""

			*> Eliminar espacios a la derecha.
			Case Left(cConcepto, 5)=='RTRIM'
				lRTrim = .T.
				lHayParametros = .T.
				cValor = ""

			*> Inicio / fin de concatenación.
			Case cConcepto=='CAT'
				lConcat = !lConcat
				cValor = ""

			*> Enviar como fichero.
			Case Left(cConcepto, 4)=='FILE'
				lFile = .T.
				lHayParametros = .T.
				cValor = ""

			*> Saltar al siguiente registro.
			Case Left(cConcepto, 5)=='SKIPR'
				lSkipR = .T.
				lHayParametros = .T.
				cValor = ""

			*> No es un concepto reconocido.
			Otherwise
				cValor = cConcepto
		EndCase

		*> Formatear el valor, si cal.
		If (!Empty(cValor) .Or. Len(cValor) > 0) .And. lHayParametros
			cValor = Iif(Empty(cMask), cValor, Transform(Val(cValor), cMask))
			cValor = Iif(nSubStD==0, cValor, SubStr(cValor, nSubStD, nSubStL))
			cValor = Iif(!lLTrim, cValor, LTrim(cValor))
			cValor = Iif(!lRTrim, cValor, RTrim(cValor))
			cValor = Iif(!lFile, cValor, This._logogrftostring(cValor))

			Store "" To cMask
			Store 0 To nSubStD, nSubStL
			Store .F. To lHayParametros, lLTrim, lRTrim, lFile
		EndIf

		*> Tratamiento de conceptos que se concatenan.
		Do Case
			*> Concatenar activo: agregar concepto actual a agrupado.
			Case lConcat
				cConcat = cConcat + cValor
				cValor = ""

			*> Saltar al siguiente registro y cargar datos.
			*> On the paper, tenemos el cursor de datos como cursor activo.
			Case lSkipR

				lSkipR = .F.
				If !Eof()
					Skip
					*> Cargar datos.
					Scatter Name oCur
					=This._etbuasignarvalores(oCur)
				EndIf

			*> Fin de concatenación: Asignar concepto concatenado a la línea de impresión.
			Case !Empty(cConcat) .And. !lConcat
				cValor = cConcat
				cConcat = ""
		EndCase

		*> Reemplazar el concepto por su valor en la línea de impresión.
		cParams = StrTran(cParams, cCodCon, cValor, 1, 1)
	Else
		*> Caracteres de control de concepto erróneos.
		lStado = .F.
		This.UsrError = "Delimitadores erróneos"
		Return lStado
	EndIf

	*> Siguiente concepto a calcular.
	nd1 = At(This._LDelimiter, cParams)
EndDo

This.cLineaEtiqueta = cParams
Return lStado

ENDPROC
PROCEDURE _etvalidarrangos

*> Validación de los rangos generales.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etubvalidarrangos(), validar rangos (etiquetas de ubicación).
*>	- This._etpkvalidarrangos(), validar rangos (etiquetas de picking).
*>	- This._etocvalidarrangos(), validar rangos (etiquetas de ocupación).
*>	- This._etlsvalidarrangos(), validar rangos (etiquetas de lista).

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

With This
	*> Nombre del formato de etiqueta.
	If Empty(.RNFormato)
		.UsrError = "Falta formato etiqueta"
		lStado = .F.
		Return lStado
	EndIf

	*> Nombre del dispositivo de salida.
	If Empty(.RNPrinter)
		.UsrError = "Falta dispositivo de salida"
		lStado = .F.
		Return lStado
	EndIf

	*> Rangos OK: Pasar a propiedades.
	._Formato = .RNFormato
	._Printer = .RNPrinter
EndWith

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etpkcargardatos

*> Generar datos para la impresión de etiquetas de picking.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacionPicking(), impresión de etiquetas de picking.

*> Recibe:

*> Genera:
*>	- ETPKCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oUbi
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
Store .T. To lStado

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("PK")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

If This.RNModo=='V'
	*> Orden de clasificación normal.
	cWhere = "F10cCodUbi Between '" + This.RNUbiIni + "' And '" + This.RNUbiFin + "'"
Else
	cWhere =          _GCSS("F10cCodUbi", 1, 4) + " Between + '" + This.cAlmI + "' And '" + This.cAlmF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 5, 2) + " Between + '" + This.cZonI + "' And '" + This.cZonF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 7, 2) + " Between + '" + This.cCalI + "' And '" + This.cCalF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi", 9, 3) + " Between + '" + This.cFilI + "' And '" + This.cFilF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi",12, 2) + " Between + '" + This.cPisI + "' And '" + This.cPisF + "' And "
	cWhere = cWhere + _GCSS("F10cCodUbi",14, 1) + " Between + '" + This.cPrfI + "' And '" + This.cPrfF + "'"
EndIf

cWhere = cWhere + " And F10cPickSn In ('G', 'S', 'U')"			&& Ubicaciones de cajas, unidades y grupos.

cField = "F10cCodUbi, F10cPickSn"
cOrder = "F10cCodUbi"
cFileF = "F10c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETPKCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Use In (Select ("ETPKCURSOR"))
	Return lStado
EndIf

Select ETPKCURSOR
Go Top
Do While !Eof()
	Scatter Name oUbi

	Do Case
		*> Cajas / unidades.
		Case oUbi.F10cPickSn=='S' .Or. oUbi.F10cPickSn=='U'
			lStado = This._etpkcargardatoss()

		*> Grupos.
		Case oUbi.F10cPickSn=='G'
			lStado = This._etpkcargardatosg()

		*> Resto casos: Error.
		Otherwise
			Replace F08cCodPro With Space(6)
			Replace F08cCodArt With Space(13)
			Replace F08cDescri With Space(30)
	EndCase

	Select ETPKCURSOR
	Skip
EndDo

Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etpkvalidarrangos

*> Validación de los rangos (etiquetas de picking).
*> Utiliza las mismas validaciones que las etiquetas de ubicación.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacionPicking(), impresión de etiquetas de picking.

*> Recibe:
*>	- This.RNUbiIni, ubicación desde.
*>	- This.RNUbiFin, ubicación hasta.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos.
lStado = This._etubvalidarrangos()
Return lStado

ENDPROC
PROCEDURE _etpkasignarvalores

*> Asignar valores del registro activo a propiedades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaUbicacionPicking(), impresión de etiquetas de picking.

*> Recibe:
*>	- oUbi, registro actual de la ubicación de picking.

*> Devuelve: .T.

Parameters oUBI

With This
	.PKCodPro = oUBI.F08cCodPro
	.PKCodArt = oUBI.F08cCodArt
	.PKDesArt = oUBI.F08cDescri
	.UBDigCtl = Ora_DigCtl(oUBI.F10cCodUbi)
	.UBCodUbi = oUBI.F10cCodUbi
	.UBUbiShort = Substr(oUBI.F10cCodUbi, 5, 1) + ;
				  SubStr(oUBI.F10cCodUbi, 7, 2) + ;
				  SubStr(oUBI.F10cCodUbi,10, 2) + ;
				  Substr(oUBI.F10cCodUbi,13, 1)
EndWith

Return

ENDPROC
PROCEDURE _etpkcargardatoss

*> Cargar datos artículo para la impresión de etiquetas de picking cajas / unidades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etpkcargardatos(), impresión de etiquetas de picking.

*> Recibe:
*>	- ETPKCURSOR, cursor con los datos de las etiquetas.

*> Genera:
*>	- ETPKCURSOR, el mismo cursor con los datos artículo. Puede generar registros adicionales, si
*>				  hay varios artículos con la misma ubicación de picking asignada.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, nRecNo, oPK, oF12c, lInsert
Private cWhere

This.UsrError = ""
lStado = .T.
lInsert = .F.

Select ETPKCURSOR
If Eof() .Or. !Empty(F08cCodArt)
	*> No hay datos ó registro ya actualizado con anterioridad.
	Return lStado
EndIf

nRecNo = RecNo()
Scatter Name oPK

cWhere = "F12cCodUbi='" + oPK.F10cCodUbi + "'"
lStado = f3_sql("*", "F12c", cWhere, , , "ETPKS")
If !lStado
	Use In (select ("ETPKS"))
	Select ETPKCURSOR
	Go nRecNo
	Replace F08cCodPro With Space(6)
	Replace F08cCodArt With Space(13)
	Replace F08cDescri With Space(30)

	Return .T.
EndIf

Select ETPKS
Go Top
Do While !Eof()
	Scatter Name oF12c

	*> Cargar los datos del artículo.
	m.F08cCodPro = oF12c.F12cCodPro
	m.F08cCodArt = oF12c.F12cCodArt
	=f3_seek("F08c")

	Select ETPKCURSOR
	If lInsert
		*> Hay varios registros de picking: Insertar
		Append Blank
		Gather Name oPK
	EndIf

	*> Actualizar los datos del artículo.
	Replace F08cCodPro With F08c.F08cCodPro
	Replace F08cCodArt With F08c.F08cCodArt
	Replace F08cDescri With F08c.F08cDescri
	
	lInsert = .T.
	Select ETPKS
	Skip
EndDo


Select ETPKCURSOR
Go nRecNo

Use In (Select ("ETPKS"))
Return lStado

ENDPROC
PROCEDURE _etpkcargardatosg

*> Cargar datos artículo para la impresión de etiquetas de picking grupos.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etpkcargardatos(), impresión de etiquetas de picking.

*> Recibe:
*>	- ETPKCURSOR, cursor con los datos de las etiquetas.

*> Genera:
*>	- ETPKCURSOR, el mismo cursor con los datos artículo. Puede generar registros adicionales, si
*>				  hay varios artículos con la misma ubicación de picking asignada.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, nRecNo, oPK, oF08g, lInsert
Private cWhere

This.UsrError = ""
lStado = .T.
lInsert = .F.

Select ETPKCURSOR
If Eof() .Or. !Empty(F08cCodArt)
	*> No hay datos ó registro ya actualizado con anterioridad.
	Return lStado
EndIf

nRecNo = RecNo()
Scatter Name oPK

cWhere = "F08gCodUbi='" + oPK.F10cCodUbi + "'"
lStado = f3_sql("*", "F08G", cWhere, , , "ETPKG")
If !lStado
	Use In (select ("ETPKG"))
	Select ETPKCURSOR
	Go nRecNo
	Replace F08cCodPro With Space(6)
	Replace F08cCodArt With Space(13)
	Replace F08cDescri With Space(30)

	Return .T.
EndIf

Select ETPKG
Go Top
Do While !Eof()
	Scatter Name oF08g

	*> Cargar los datos del artículo.
	m.F08cCodPro = oF08g.F08gCodPro
	m.F08cCodArt = oF08g.F08gCodArt
	=f3_seek("F08c")

	Select ETPKCURSOR
	If lInsert
		*> Hay varios registros de picking: Insertar
		Append Blank
		Gather Name oPK
	EndIf

	*> Actualizar los datos del artículo.
	Replace F08cCodPro With F08c.F08cCodPro
	Replace F08cCodArt With F08c.F08cCodArt
	Replace F08cDescri With F08c.F08cDescri
	
	lInsert = .T.
	Select ETPKG
	Skip
EndDo


Select ETPKCURSOR
Go nRecNo

Use In (Select ("ETPKG"))
Return lStado

ENDPROC
PROCEDURE oprocaot_access

*> Inicializar clase general.
If Type('This.oProcaot') # 'O'
	This.oProcaot = CreateObject('Procaot')
EndIf

Return This.oProcaot

ENDPROC
PROCEDURE _etnuvalidarrangos

*> Validación de los rangos (etiquetas de numeración).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaNumeracion(), impresión de etiquetas de numeración.

*> Recibe:
*>	- This.RNCodNum, código de numeración.
*>	- This.RNNumEti, nº de etiquetas a realizar.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

*> Validar los rangos de impresión de etiquetas de ubicación.
With This
	*> Tipo de numeración.
	If Empty(.RNCodNum) .Or. At(.RNCodNum, "PM")==0
		This.UsrError = "Tipo numeración no válido"
		lStado = .F.
		Return lStado
	EndIf

	*> Etiquetas a imprimir.
	If .RNNumEti <= 0
		.RnNumEti = 1					&& Cómo mínimo, una etiqueta.
	EndIf
EndWith

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etoccargardatos

*> Generar datos para la impresión de etiquetas de ocupación.
*> Tomar datos de ocuàciones / MPs,en función del origen de los datos.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaOcupacion(), impresión de etiquetas de ocupación.

*> Recibe:
*>	- This.RNxxxx, rango datos.
*>	- This._Origen, origen datos:
*>		'MP', movimientos pendientes (de entrada).
*>		'OC', ocupaciones.
*>		'ET', etiquetas pendientes (F99t).

*> Genera:
*>	- ETOCCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""
Store .T. To lStado

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("OC")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

Do Case
	*> Origen: Ocupaciones.
	Case This._Origen=='OC'
		lStado = This._etoccargardatosoc()

	*> Origen: MPs de entrada.
	Case This._Origen=='MP'
		lStado = This._etoccargardatosmp()

	*> Origen: Etiquetas pendientes.
	Case This._Origen=='ET'
		lStado = This._etoccargardatoset()

	*> Resto casos: Error.
	Otherwise
EndCase

Select ETOCCURSOR
Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etoccargardatosoc

*> Cargar datos ocupación  para etiquetas de ocupación. Origen de datos: Ocupaciones.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etoccargardatos(), impresión de etiquetas de ocupación.

*> Recibe:
*>	- This.RNModo, orden ubicaciones.
*>	- ETOCCURSOR, cursor con los datos de las etiquetas.

*> Genera:
*>	- ETOCCURSOR, el mismo cursor con los datos de las ocupaciones.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oOC
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
lStado = .T.

If This.RNModo=='V'
	*> Orden de clasificación normal.
	cWhere = "F16cCodUbi Between '" + This.RNUbiIni + "' And '" + This.RnUbiFin + "' And "
Else
	cWhere =          _GCSS("F16cCodUbi", 1, 4) + " Between + '" + This.cAlmI + "' And '" + This.cAlmF + "' And "
	cWhere = cWhere + _GCSS("F16cCodUbi", 5, 2) + " Between + '" + This.cZonI + "' And '" + This.cZonF + "' And "
	cWhere = cWhere + _GCSS("F16cCodUbi", 7, 2) + " Between + '" + This.cCalI + "' And '" + This.cCalF + "' And "
	cWhere = cWhere + _GCSS("F16cCodUbi", 9, 3) + " Between + '" + This.cFilI + "' And '" + This.cFilF + "' And "
	cWhere = cWhere + _GCSS("F16cCodUbi",12, 2) + " Between + '" + This.cPisI + "' And '" + This.cPisF + "' And "
	cWhere = cWhere + _GCSS("F16cCodUbi",14, 1) + " Between + '" + This.cPrfI + "' And '" + This.cPrfF + "' And "
EndIf

*> Agregar los rangos comunes.
cWhere = cWhere + "F16cNumPal Between '" + This.RNPalIni + "' And '" + This.RNPalFin + "' And "
cWhere = cWhere + "F16cNumLot Between '" + This.RNLotIni + "' And '" + This.RNLotFin + "' And "
cWhere = cWhere + "F16cIdOcup Between '" + This.RNIdOIni + "' And '" + This.RNIdOFin + "' And "
cWhere = cWhere + "F16cCodPro Between '" + This.RNProIni + "' And '" + This.RNProFin + "' And "
cWhere = cWhere + "F16cCodArt Between '" + This.RNArtIni + "' And '" + This.RNArtFin + "'"

cField = "*"
cOrder = "F16cCodPro, F16cCodArt, F16cCodUbi"
cFileF = "F16c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETOCOC")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Use In (Select ("ETOCOC"))
	Return lStado
EndIf

Select ETOCOC
Go Top
Do While !Eof()
	Scatter Name oOC

	*> Cargar los datos del artículo.
	m.F08cCodPro = oOC.F16cCodPro
	m.F08cCodArt = oOC.F16cCodArt
	=f3_seek("F08c")

	*> Cargar los datos del propietario.
	m.F01pCodigo = oOC.F16cCodPro
	=f3_seek("F01p")

	Select ETOCCURSOR
	Append Blank

	*> Agregar datos del artículo al cursor.
	Replace CodPro With F08c.F08cCodPro
	Replace CodArt With F08c.F08cCodArt
	Replace Descri With F08c.F08cDescri
	Replace EanArt With F08c.F08cCodEan

	*> Agregar datos del propietario al cursor.
	Replace F01pDescri With F01p.F01pDescri
	Replace F01pLogo   With F01p.F01pLogo

	*> Agregar datos de la ocupación al cursor.
	Replace F10cCodUbi With oOC.F16cCodUbi
	Replace NumPal     With oOC.F16cNumPal
	Replace NumLot     With oOC.F16cNumLot
	Replace NumEnt     With oOC.F16cNotEnt
	Replace CanFis     With oOC.F16cCanFis
	Replace FecEnt     With oOC.F16cFecEnt
	Replace FecCad     With oOC.F16cFecCad
	Replace DocOcu     With oOC.F16cNumDoc

	*> Calcular peso y volumen.
	With This.oUbicObj
		.Inicializar
		.CargaPrm

		.CodPro = oOC.F16cCodPro
		.CodArt = oOC.F16cCodArt
		.CanUbi = oOC.F16cCanFis
		.PesVolAr

		Select ETOCCURSOR
	    Replace PesPal With .PesOcu
	    Replace VolPal With .VolOcu
	EndWith

	Select ETOCOC
	Skip
EndDo

Select ETOCCURSOR
Go Top

Use In (Select ("ETOCOC"))
Return lStado

ENDPROC
PROCEDURE _etoccargardatosmp

*> Cargar datos ocupación para etiquetas de ocupación. Origen de datos: MPs de entrada.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etoccargardatos(), impresión de etiquetas de ocupación.

*> Recibe:
*>	- This.RNModo, orden ubicaciones.
*>	- ETOCCURSOR, cursor con los datos de las etiquetas.

*> Genera:
*>	- ETOCCURSOR, el mismo cursor con los datos de las ocupaciones.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

*> Historial de modificaciones.
*>	- Permitir MPs sin ubicación asignada. AVC - 14.12.2005
*>	- Agregar rango por nº de entrada. AVC - 19.06.2006
*> 19.05.2008 (AVC) Agregar concepto 'Documento'.

Local lStado, oMP
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
lStado = .T.

If This.RNModo=='V'
	*> Orden de clasificación normal.
	cWhere = "(F14cUbiOri Between '" + This.RNUbiIni + "' And '" + This.RNUbiFin + "' Or F14cUbiOri='" + Space(14) + "') And "
Else
	cWhere = "F14cUbiOri='" + Space(14) + "' Or ( "
	cWhere = cWhere + _GCSS("F14cUbiOri", 1, 4) + " Between + '" + This.cAlmI + "' And '" + This.cAlmF + "' And "
	cWhere = cWhere + _GCSS("F14cUbiOri", 5, 2) + " Between + '" + This.cZonI + "' And '" + This.cZonF + "' And "
	cWhere = cWhere + _GCSS("F14cUbiOri", 7, 2) + " Between + '" + This.cCalI + "' And '" + This.cCalF + "' And "
	cWhere = cWhere + _GCSS("F14cUbiOri", 9, 3) + " Between + '" + This.cFilI + "' And '" + This.cFilF + "' And "
	cWhere = cWhere + _GCSS("F14cUbiOri",12, 2) + " Between + '" + This.cPisI + "' And '" + This.cPisF + "' And "
	cWhere = cWhere + _GCSS("F14cUbiOri",14, 1) + " Between + '" + This.cPrfI + "' And '" + This.cPrfF + "') And "
EndIf

*> Agregar los rangos comunes.
cWhere = cWhere + "F14cTipMov Between '1000' And '1999' And "
cWhere = cWhere + "F14cNumLst Between '" + This.RNLstIni + "' And '" + This.RNLstFin + "' And "
cWhere = cWhere + "F14cNumPal Between '" + This.RNPalIni + "' And '" + This.RNPalFin + "' And "
cWhere = cWhere + "F14cNumLot Between '" + This.RNLotIni + "' And '" + This.RNLotFin + "' And "
cWhere = cWhere + "F14cNumEnt Between '" + This.RNEntIni + "' And '" + This.RNEntFin + "' And "
cWhere = cWhere + "F14cIdOcup Between '" + This.RNIdOIni + "' And '" + This.RNIdOFin + "' And "
cWhere = cWhere + "F14cCodPro Between '" + This.RNProIni + "' And '" + This.RNProFin + "' And "
cWhere = cWhere + "F14cCodArt Between '" + This.RNArtIni + "' And '" + This.RNArtFin + "'"

cField = "*"
cOrder = "F14cCodPro, F14cCodArt, F14cUbiOri"
cFileF = "F14c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETOCMP")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Use In (Select ("ETOCMP"))
	Return lStado
EndIf

Select ETOCMP
Go Top
Do While !Eof()
	Scatter Name oMP

	*> Cargar los datos del artículo.
	m.F08cCodPro = oMP.F14cCodPro
	m.F08cCodArt = oMP.F14cCodArt
	=f3_seek("F08c")

	*> Cargar los datos del propietario.
	m.F01pCodigo = oMP.F14cCodPro
	=f3_seek("F01p")

	Select ETOCCURSOR
	Append Blank

	*> Agregar datos del artículo al cursor.
	Replace CodPro With F08c.F08cCodPro
	Replace CodArt With F08c.F08cCodArt
	Replace Descri With F08c.F08cDescri
	Replace EanArt With F08c.F08cCodEan

	*> Agregar datos del propietario al cursor.
	Replace F01pDescri With F01p.F01pDescri
	Replace F01pLogo   With F01p.F01pLogo

	*> Agregar datos de la ocupación al cursor.
	Replace F10cCodUbi With oMP.F14cUbiOri
	Replace NumPal     With oMP.F14cNumPal
	Replace NumLot     With oMP.F14cNumLot
	Replace NumEnt     With oMP.F14cNumEnt
	Replace CanFis     With oMP.F14cCanFis
	Replace FecEnt     With oMP.F14cFecMov
	Replace FecCad     With oMP.F14cFecCad
	Replace DocOcu     With oMP.F14cNumDoc

	*> Calcular peso y volumen.
	With This.oUbicObj
		.Inicializar
		.CargaPrm

		.CodPro = oMP.F14cCodPro
		.CodArt = oMP.F14cCodArt
		.CanUbi = oMP.F14cCanFis
		.PesVolAr

		Select ETOCCURSOR
	    Replace PesPal With .PesOcu
	    Replace VolPal With .VolOcu
	EndWith

	Select ETOCMP
	Skip
EndDo

Select ETOCCURSOR
Go Top

Use In (Select ("ETOCMP"))
Return lStado

ENDPROC
PROCEDURE _etoccargardatoset

*> Cargar datos ocupación  para etiquetas de ocupación. Origen de datos: Etiquetas pendientes.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etoccargardatos(), impresión de etiquetas de ocupación.

*> Recibe:
*>	- This.RNModo, orden ubicaciones.
*>	- ETOCCURSOR, cursor con los datos de las etiquetas.

*> Genera:
*>	- ETOCCURSOR, el mismo cursor con los datos de las ocupaciones.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oET
Private cWhere

This.UsrError = ""
lStado = .T.

If !File("F99T.DBF")
	This.UsrError = "No hay etiquetas pendientes"
	lStado = .F.
	Return lStado
EndIf

If This.RNModo=='V'
	*> Orden de clasificación normal.
	cWhere = "Between(F99tCodUbi, '" + This.RNUbiIni + "', '" + This.RnUbiFin + "') And "
Else
	cWhere =          "Between(SubStr(F99tCodUbi, 1, 4), '" + This.cAlmI + "', '" + This.cAlmF + "') And "
	cWhere = cWhere + "Between(SubStr(F99tCodUbi, 5, 2), '" + This.cZonI + "', '" + This.cZonF + "') And "
	cWhere = cWhere + "Between(SubStr(F99tCodUbi, 7, 2), '" + This.cCalI + "', '" + This.cCalF + "') And "
	cWhere = cWhere + "Between(SubStr(F99tCodUbi, 9, 3), '" + This.cFilI + "', '" + This.cFilF + "') And "
	cWhere = cWhere + "Between(SubStr(F99tCodUbi,12, 2), '" + This.cPisI + "', '" + This.cPisF + "') And "
	cWhere = cWhere + "Between(SubStr(F99tCodUbi,14, 1), '" + This.cPrfI + "', '" + This.cPrfF + "') And "
EndIf

*> Agregar los rangos comunes.
cWhere = cWhere + "Between(F99tNumLst, '" + This.RNLstIni + "', '" + This.RNLstFin + "') And "
cWhere = cWhere + "Between(F99tNumPal, '" + This.RNPalIni + "', '" + This.RNPalFin + "') And "
cWhere = cWhere + "Between(F99tNumLot, '" + This.RNLotIni + "', '" + This.RNLotFin + "') And "
cWhere = cWhere + "Between(F99tCodPro, '" + This.RNProIni + "', '" + This.RNProFin + "') And "
cWhere = cWhere + "Between(F99tCodArt, '" + This.RNArtIni + "', '" + This.RNArtFin + "')"

Select 0
Use F99t

Scan For &cWhere
	Scatter Name oET

	*> Cargar los datos del artículo.
	m.F08cCodPro = oET.F99tCodPro
	m.F08cCodArt = oET.F99tCodArt
	=f3_seek("F08c")

	*> Cargar los datos del propietario.
	m.F01pCodigo = oET.F99tCodPro
	=f3_seek("F01p")

	Select ETOCCURSOR
	Append Blank

	*> Agregar datos del artículo al cursor.
	Replace CodPro With F08c.F08cCodPro
	Replace CodArt With F08c.F08cCodArt
	Replace Descri With F08c.F08cDescri
	Replace EanArt With F08c.F08cCodEan

	*> Agregar datos del propietario al cursor.
	Replace F01pDescri With F01p.F01pDescri
	Replace F01pLogo   With F01p.F01pLogo

	*> Agregar datos de la ocupación al cursor.
	Replace F10cCodUbi With oET.F99tCodUbi
	Replace NumPal     With oET.F99tNumPal
	Replace NumLot     With oET.F99tNumLot
	Replace NumEnt     With Space(10)
	Replace CanFis     With oET.F99tCanFis
	Replace FecEnt     With Date()
	Replace FecCad     With oET.F99tFecCad

	*> Calcular peso y volumen.
	With This.oUbicObj
		.Inicializar
		.CargaPrm

		.CodPro = oET.F99tCodPro
		.CodArt = oET.F99tCodArt
		.CanUbi = oET.F99tCanFis
		.PesVolAr

		Select ETOCCURSOR
	    Replace PesPal With .PesOcu
	    Replace VolPal With .VolOcu
	EndWith
EndScan

Select ETOCCURSOR
Go Top

Use In (Select ("F99t"))
Return lStado

ENDPROC
PROCEDURE _etocasignarvalores

*> Asignar valores del registro activo a propiedades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaOcupacion(), impresión de etiquetas de ocupación.

*> Recibe:
*>	- oOcu, registro actual de la ocupación.

*> Devuelve:

*> Historial de modificaciones:
*> 19.05.2008 (AVC) Agregar propiedad 'Documento'.

Parameters oOCU

With This
	.OCNumPal = oOCU.NumPal
	.OCNumLot = oOCU.NumLot
	.OCFecCad = oOCU.FecCad
	.OCCanFis = oOCU.CanFis
	.OCPesPal = oOCU.PesPal
	.OCVolPal = oOCU.VolPal

	.OCNumEnt = oOCU.NumEnt
	.OCFecEnt = oOCU.FecEnt

	.OCDocOcu = oOCU.DocOcu

	.PKCodPro = oOCU.CodPro
	.PKCodArt = oOCU.CodArt
	.PKDesArt = oOCU.Descri
	.PKEanArt = oOCU.EanArt

	.EXDesPro = oOCU.F01pDescri
	.EXLogoP  = oOCU.F01pLogo

	.UBDigCtl = Ora_DigCtl(oOCU.F10cCodUbi)
	.UBCodUbi = oOCU.F10cCodUbi
	.UBUbiShort = Substr(oOCU.F10cCodUbi, 5, 1) + ;
				  SubStr(oOCU.F10cCodUbi, 7, 2) + ;
				  SubStr(oOCU.F10cCodUbi,10, 2) + ;
				  Substr(oOCU.F10cCodUbi,13, 1)
EndWith

Return

ENDPROC
PROCEDURE etiquetaexpedicion

*> Impresión de etiquetas de expedición - documento.

*> Recibe:
*>	- Propietario / Tipo / Nº Documento (opcionales)
*>	- This.RNxxxxxx, rangos pasados como propiedades.
*>	- This.RNModoBulto: D-Documento (1 etiqueta por documento), B-Bulto (1 etiqueta por bulto).

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropietario, cTipo, cDocumento

Local lStado, nInx, nMaxEti
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNProIni = IIf(Type('cPropietario')=='C', cPropietario, .RNProIni)
	.RNTDoIni = IIf(Type('cTipo')=='C', cTipo, .RNTDoIni)
	.RNNDoIni = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoIni)

	*> Asignar rangos hasta por defecto.
	.RNProFin = IIf(Type('cPropietario')=='C', cPropietario, .RNProFin)
	.RNTDoFin = IIf(Type('cTipo')=='C', cTipo, .RNTDoFin)
	.RNNDoFin = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoFin)
EndWith

*> Validar los rangos.
lStado = This._etexvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("EX")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETEXCURSOR.
lStado = This._etexcargardatos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select ETEXCURSOR
Go Top
Do While !Eof()
	Scatter Name oCUR

	If This.RNModoBulto=='D'
		nMaxEti = 1
	Else
		nMaxEti = oCUR.F27cBultos
	EndIf

	For nInx = 1 To nMaxEti
		*> Asignar valores a las propiedades de la clase.
		This.EXCurBul = nInx
		lStado = This._etexasignarvalores(oCUR)

		*> Imprimir la etiqueta.
		lStado = This._etimprimiretiqueta()
	EndFor

	*> Next registro.
	Select ETEXCURSOR
	Skip
EndDo

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETEXCURSOR"))
Return lStado

ENDPROC
PROCEDURE _etexvalidarrangos

*> Validación de los rangos (etiquetas de expedición-documento).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicion(), impresión de etiquetas de expedición-documento.

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

With This
	*> Rangos desde-hasta.
	If .RNProIni > .RNProFin .Or. ;
	   .RNTDoIni > .RNTDoFin .Or. ;
	   .RNNDoIni > .RNNDoFin .Or. ;
	   .RNFDoIni > .RNFDoFin .Or. ;
	   .RNAlbIni > .RNAlbFin
		.UsrError = "Rangos desde-hasta erróneos"
		lStado = .F.
		Return lStado
	EndIf

	*> Modo mpresión bultos.
	If Empty(.RNModoBulto) .Or. At(.RNModoBulto, "DB")==0
		.RNModoBulto = "D"								&& Por defecto, por documento.
	EndIf

EndWith

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etexcargardatos

*> Generar datos para la impresión de etiquetas de expedición-documento.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicion(), impresión de etiquetas de expedición-documento.

*> Recibe:

*> Genera:
*>	- ETEXCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oExp, oEnv, oAlb, oPro, oPrv, oTra, oObs
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
Store .T. To lStado

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("EX")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

*> Cargar los datos de documentos de salida.
cWhere =          "F24cCodPro Between '" + This.RNProIni + "' And '" + This.RNProFin + "' And "
cWhere = cWhere + "F24cTipDoc Between '" + This.RNTDoIni + "' And '" + This.RNTDoFin + "' And "
cWhere = cWhere + "F24cNumDoc Between '" + This.RNNDoIni + "' And '" + This.RNNDoFin + "' And "
cWhere = cWhere + "F24cFecDoc Between " + _GCD(This.RNFDoIni) + " And " + _GCD(This.RNFDoFin)

cField = "*"
cOrder = "F24cCodPro, F24cTipDoc, F24cNumDoc"
cFileF = "F24c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETEXCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Use In (Select ("ETEXCURSOR"))
	Return lStado
EndIf

Select ETEXCURSOR
Go Top
Do While !Eof()
	Scatter Name oExp

	*> Cargar los datos de la dirección de envío.
	cWhere = 		  "F24tCodPro='" + oExp.F24cCodPro + "' And "
	cWhere = cWhere + "F24tTipDoc='" + oExp.F24cTipDoc + "' And "
	cWhere = cWhere + "F24tNumDoc='" + oExp.F24cNumDoc + "'"

	lStado = f3_sql("*", "F24t", cWhere, , , "F24tCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos envío"
			Use In (Select ("F24tCur"))
			Use In (Select ("ETEXCURSOR"))
			Return lStado
		EndIf

		Select F24tCur
		Scatter Name oEnv Blank
	Else
		Select F24tCur
		Scatter Name oEnv
	EndIf

	Use In (Select("F24tCur"))

	*> Cargar las observaciones de cabecera / referencia cliente.
	cWhere = 		  "F24oCodPro='" + oExp.F24cCodPro + "' And "
	cWhere = cWhere + "F24oTipDoc='" + oExp.F24cTipDoc + "' And "
	cWhere = cWhere + "F24oNumDoc='" + oExp.F24cNumDoc + "' And "
	cWhere = cWhere + "F24oLinObs='0000'"

	lStado = f3_sql("*", "F24o", cWhere, , , "F24oCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga observaciones 0000"
			Use In (Select ("F24oCur"))
			Return lStado
		EndIf

		Select F24oCur
		Scatter Name oObs Blank Memo
	Else
		Select F24oCur
		Scatter Name oObs Memo
	EndIf

	Use In (Select("F24oCur"))

	*> Cargar datos de albarán / hoja de ruta.
	cWhere = 		  "F27cCodPro='" + oExp.F24cCodPro + "' And "
	cWhere = cWhere + "F27cTipDoc='" + oExp.F24cTipDoc + "' And "
	cWhere = cWhere + "F27cNumDoc='" + oExp.F24cNumDoc + "' And "
	cWhere = cWhere + "F30cTipEnt='PROP' And F30cCodEnt=F27cCodPro And F30cTipDoc=F27cTipDoc And F30cNumDoc=F27cNumDoc"

	lStado = f3_sql("*", "F27c,F30c", cWhere, , , "F2730Cur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos albarán"
			Use In (Select ("F2730Cur"))
			Use In (Select ("ETEXCURSOR"))
			Return lStado
		EndIf

		Select F2730Cur
		Scatter Name oAlb Blank
	Else
		Select F2730Cur
		Scatter Name oAlb
	EndIf

	Use In (Select ("F2730Cur"))

	*> Cargar los datos del propietario.
	m.F01pCodigo = oExp.F24cCodPro
	=f3_seek("F01p")
	Select F01p
	Scatter Name oPro

	*> Cargar la descripción de la provincia del propietario.
	m.F00jCodPrv = oPro.F01pProvin
	=f3_seek("F00j")
	Select F00j
	Scatter Name oPrv

	*> Cargar los datos del transportista.
	m.F01tCodigo = oExp.F24cCodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oTra

	Select ETEXCURSOR
	Gather Name oEnv
	Gather Name oAlb
	Gather Name oPro
	Gather Name oPrv
	Gather Name oTra

	Replace F24oDesObs With MLine(oObs.F24oDesObs, 1)
	Replace Portes With Iif(oExp.F24cPortes=='P', 'PAGADOS', 'DEBIDOS')

	Skip
EndDo

Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etexasignarvalores

*> Asignar valores del registro activo a propiedades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicion(), impresión de etiquetas de expedición-documento.

*> Recibe:
*>	- oExp, registro actual de datos.

*> Devuelve: .T.

Parameters oEXP

With This
	.PKCodPro = oEXP.F24cCodPro

	.EXCodTra = oEXP.F24cCodTra
	.EXNumDoc =	oEXP.F24cNumDoc
	.EXFecDoc =	oEXP.F24cFecDoc
	.EXFecPre =	oEXP.F24cFecPre

	.EXNumAlb =	oEXP.F27cNumAlb
	.EXFecAlb =	oEXP.F27cFecAlb
	.EXBultos =	oEXP.F27cBultos
	.EXKilos  =	oEXP.F27cPesoKg
	.EXPortes =	oEXP.Portes

	.EXHojRut =	oEXP.F30cHojRut
	.EXDesTra = oEXP.F01tDescri
	.EXObserv = Space(1)				&& Concepto no implementado.
	.EXObserv = oEXP.F24oDesObs

	.EXDesPro = oEXP.F01pDescri
	.EXDirPro = oEXP.F01pDirecc
	.EXPobPro = oEXP.F01pPoblac
	.EXProPro = oEXP.F00jDescri
	.EXPosPro = oEXP.F01pCodPos
	.EXTelPro = oEXP.F01pNumTel
	.EXFaxPro = oEXP.F01pNumFax
	.EXEMail  = oEXP.F01pEMail
	.EXPagWeb = oEXP.F01pPagWeb
	.EXLogoP  = oEXP.F01pLogo

	.EXNomAso = oEXP.F24tNomAso
	.EX1erDir = oEXP.F24t1erDir
	.EX2ndDir = oEXP.F24t2ndDir
	.EX3rdDir = oEXP.F24t3rdDir
	.EXDPobla = oEXP.F24tDPobla
	.EXDProvi = oEXP.F24tDProvi
	.EXCodPos = oEXP.F24tCodPos

	.EXNumTel = oEXP.F24tNumTel
	.EXNumFax = oEXP.F24tNumFax
	.EXNumNif = oEXP.F24tNumNif
	
	.EXNumMac = ""
	.EXCurBul = 0

	.EXCodPas = oEXP.F24tCodPas

	.OCCanFis = oEXP.Cantid
	.PKDesArt = oEXP.F08cDescri
EndWith

*> LRC. 19.1.9
*> Control para asegurar que se imprime la provicia destino.
If Empty(This.EXDProvi) and !Empty(This.EXCodPos)
	*> Cargar la descripción de la provincia destino.
	m.F00jCodPrv = PadL(Left(This.EXCodPos,2),4,'0')
	This.EXDProvi = IIf(f3_seek("F00j"), Alltrim(F00j.F00jDescri), This.EXDProvi)
EndIf

Return


ENDPROC
PROCEDURE crearpropiedad

*> Crear un concepto de etiqueta como propiedad de la clase.
*> Utilizado para la impresión de etiquetas directas.

*> Recibe:
*>	- Parámetros de la propiedad, con el formato: NAME=<valor>, TYPE=<valor>, LEN=<valor>
*>    Por defecto es CHAR.
*>    El parámetro LEN es opcional.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropiedad

Local lStado

lStado = This._crearpropiedad(cPropiedad)
Return lStado

ENDPROC
PROCEDURE crearpropiedadcursor

*> Crear conceptos de etiquetas como propiedades, a partir de la estructura de un cursor.
*> Utilizado para la impresión de etiquetas directas.

*> Recibe:
*>	- oCur, cursor a tomar para generar las propiedades. Si no se recibe toma el alias activo.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters oCur

Local lStado, cSelect, oCursor, nInx, cLen
Private oFields, cPropiedad

*> Texto mensaje de error.
This.UsrError = ""

lStado = .F.

If Type('oCur') # 'O'
	cSelect = Select()
	If Empty(cSelect)
		*> Error de parámetros.
		This.UsrError = "Parámetros cursor vacíos"
		Return lStado
	EndIf
	Scatter Name oCursor Blank
Else
	oCursor = oCur
EndIf

*> Cargar las propiedades en un array.
lStado = (AMembers(oFields, oCursor) > 0)
If !lStado
	This.UsrError = "No se han podido cargar las propiedades"
	Return lStado
EndIf

*> Generar las propiedades en la clase.
For nInx = 1 To ALen(oFields)
	cLen = "oCursor." + oFields(nInx)
	cPropiedad = "NAME=" + oFields(nInx)
	cPropiedad = cPropiedad + ", TYPE=" + Type(oFields(nInx))
	cPropiedad = cPropiedad + ", LEN=" + AllTrim(Str(Len(cLen)))

	lStado = This._crearpropiedad(cPropiedad)
	If !lStado
		*> El texto de error ya se habrá asignado.
		Return lStado
	EndIf
EndFor

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _crearpropiedad

*> Crear un concepto de etiqueta como propiedad de la clase.
*> Utilizado para la impresión de etiquetas directas.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.CrearPropiedadCursor()

*> Recibe:
*>	- Parámetros de la propiedad, con el formato: NAME=<valor>, TYPE=<valor>, LEN=<valor>
*>    Por defecto es CHAR.
*>    El parámetro LEN es opcional.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropiedad

Local lStado, nd, cPrm, cVal, cParams
Local cName, cType, nLen, cValor

*> Texto mensaje de error.
This.UsrError = ""

lStado = .F.
Store "" To cName, cType, cValor
Store 0 To nLen

cParams = cPropiedad
If Type('cParams') # 'C'
	This.UsrError = "Parámetro vacío"
	Return lStado
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      *> Nombre de la propiedad a definir.
      Case cPrm=='NAME'
         cName = cVal

      *> Tipo de dato de la propiedad a definir.
      Case cPrm=='TYPE'
         cType = cVal

      *> Longitud de la propiedad a definir.
      Case cPrm=='LEN'
         nLen = Val(cVal)

      *> No es un parámetro reconocido.
      Otherwise
   EndCase
EndDo

If Empty(cName)
	*> Nombre de la propiedad no definido.
	This.UsrError = "Nombre no definido"
	Return lStado
EndIf

If Empty(cType)
	*> Por defecto es un Char.
	cType = 'C'
EndIf

If Empty(nLen)
	*> Por defecto len = 1.
	nLen = 1
EndIf

*> Inicializar la propiedad.
Do Case
	Case cType=='C'
		cValor = Space(nLen)
	Case cType=='N'
		cValor = 0
	Case cType=='D'
		cValor = Date()
EndCase

This.AddProperty(cName, cValor)

lStado = .T.
Return lStado

ENDPROC
PROCEDURE crearcursoretiqueta

*> Crea el cursor base para la carga de datos para imprimir etiquetas.

*> Recibe:
*>	- Tipo de etiqueta para el que se crea el cursor.

*> Devuelve:
*>	- Resultado (.T. / .F.)
*>	- This.UsrError, texto incidencia.

Parameters cTipoEtiqueta

Local lStado

This.UsrError = ""

lStado = This._crearcursoretiqueta(cTipoEtiqueta)
Return lStado

ENDPROC
PROCEDURE _crearcursoretiqueta

*> Crea el cursor base para la carga de datos para imprimir etiquetas.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.CrearCursorEtiqueta().

*> Recibe:
*>	- Tipo de etiqueta para el que se crea el cursor.

*> Devuelve:
*>	- Resultado (.T. / .F.)
*>	- This.UsrError, texto incidencia.
*>	- Cursor.

*> Historial de modificaciones:
*>	- Agregar ubicación de segundo nivel para las etiquetas de señalización. AVC 20.12.2005
*>	- Agregar concepto código de barras. AVC 10.08.2006
*>	- Agregar concepto tipo de portes. AVC 23.08.2006
*>	- Corregir decimales en cantidad del cursor de trabajo. AVC - 14.03.2008
*> 16.05.2008 (AVC) Crear cursor etiquetas de listas.
*> 19.05.2008 (AVC) Agregar concepto 'Documento' a cursor etiqueta ocupaciones.

Parameters cTipoEtiqueta

Local lStado

This.UsrError = ""

Do Case
	*> Etiqueta de ubicación / dígito de control.
	Case cTipoEtiqueta=='UB'
		Use In (Select ("ETUBCURSOR"))
		=CrtCursor("F10c", "ETUBCURSOR")
		=AddFldToCursor("ETUBCURSOR", [NAME=F10cUbiNew,TYPE=C,LENGTH=14])
		lStado = .T.

	*> Etiqueta de picking.
	Case cTipoEtiqueta=='PK'
		Use In (Select ("EPKCURSOR"))
		=CrtCursor("F10c", "ETPKCURSOR")

		*> Cargar el artículo y la descripción, según sea picking ó grupos.
		=AddFldToCursor("ETPKCURSOR", [NAME=F08cCodPro,TYPE=C,LENGTH=6])
		=AddFldToCursor("ETPKCURSOR", [NAME=F08cCodArt,TYPE=C,LENGTH=13])
		=AddFldToCursor("ETPKCURSOR", [NAME=F08cDescri,TYPE=C,LENGTH=30])

		lStado = .T.

	*> Etiqueta de ocupación.
	Case cTipoEtiqueta=='OC'
		Use In (Select ("ETOCCURSOR"))
		=CrtFCursor("ETOCCURSOR", [TBL=F10c,FLD=F10cCodUbi])

		*> Crear el resto de campos del cursor de trabajo.
		=AddFldToCursor("ETOCCURSOR", [NAME=CodPro,TYPE=C,LENGTH=6])
		=AddFldToCursor("ETOCCURSOR", [NAME=CodArt,TYPE=C,LENGTH=13])
		=AddFldToCursor("ETOCCURSOR", [NAME=Descri,TYPE=C,LENGTH=40])
		=AddFldToCursor("ETOCCURSOR", [NAME=EanArt,TYPE=C,LENGTH=13])
		=AddFldToCursor("ETOCCURSOR", [NAME=F01pDescri,TYPE=C,LENGTH=40])
		=AddFldToCursor("ETOCCURSOR", [NAME=F01pLogo,TYPE=C,LENGTH=40])
		=AddFldToCursor("ETOCCURSOR", [NAME=NumPal,TYPE=C,LENGTH=10])
		=AddFldToCursor("ETOCCURSOR", [NAME=NumLot,TYPE=C,LENGTH=15])
		=AddFldToCursor("ETOCCURSOR", [NAME=NumEnt,TYPE=C,LENGTH=10])
		=AddFldToCursor("ETOCCURSOR", [NAME=CanFis,TYPE=N,LENGTH=9,DECIMALS=3])		&& Corregidos decimales. AVC - 14.03.2008
		=AddFldToCursor("ETOCCURSOR", [NAME=PesPal,TYPE=N,LENGTH=12,DECIMALS=3])
		=AddFldToCursor("ETOCCURSOR", [NAME=VolPal,TYPE=N,LENGTH=12,DECIMALS=3])
		=AddFldToCursor("ETOCCURSOR", [NAME=FecCad,TYPE=D])
		=AddFldToCursor("ETOCCURSOR", [NAME=FecEnt,TYPE=D])
		=AddFldToCursor("ETOCCURSOR", [NAME=DocOcu,TYPE=C,LENGTH=13])				&& Agregar documento. AVC - 19.05.2008

		lStado = .T.

	*> Etiqueta de expedición - documento.
	Case cTipoEtiqueta=='EX'
		Use In (Select ("ETEXCURSOR"))

		*> Crear la estructura base del cursor de trabajo, a partir de datos de envío.
		=CrtMCursor("F24t,F01p", "ETEXCURSOR", "C")

		*> Agregar los campos de la cabecera de documentos.
		=CrtFCursor("ETEXCURSOR", [TBL=F24c,FLD=F24cCodPro,FLD=F24cTipDoc,FLD=F24cNumDoc,FLD=F24cFecDoc,FLD=F24cFecPre,FLD=F24cCodTra,FLD=F24cPortes])

		*> Agregar observaciones.
		=AddFldToCursor("ETEXCURSOR", [NAME=F24oDesObs,TYPE=C,LENGTH=60])		&& OJO. En origen campo Memo.

		*> Agregar campos de albarán de salida.
		=CrtFCursor("ETEXCURSOR", [TBL=F27c,FLD=F27cNumAlb,FLD=F27cFecAlb,FLD=F27cPesoKg,FLD=F27cBultos])

		*> Agregar campos de la hoja de ruta.
		=CrtFCursor("ETEXCURSOR", [TBL=F30c,FLD=F30cHojRut])

		*> Agregar campos varios.
		=CrtFCursor("ETEXCURSOR", [TBL=F01t,FLD=F01tDescri,TBL=F00j,FLD=F00jDescri])
		=CrtFCursor("ETEXCURSOR", [TBL=F24l,FLD=F24lCodPro,TBL=F24l,FLD=F24lTipDoc,TBL=F24l,FLD=F24lNumDoc,TBL=F24l,FLD=F24lCodArt,FLD=F24lCanDoc,TBL=F08c,FLD=F08cDescri])
		=AddFldToCursor("ETEXCURSOR", [NAME=Cantid,TYPE=N,LENGTH=6,DECIMALS=0])

		*> Agregar campos de identificación del MAC.
		=AddFldToCursor("ETEXCURSOR", [NAME=NumMac,TYPE=C,LENGTH=9])
		=AddFldToCursor("ETEXCURSOR", [NAME=NumBul,TYPE=N,LENGTH=9,DECIMALS=0])

		*> Agregar campos de envío.
		=AddFldToCursor("ETEXCURSOR", [NAME=Portes,TYPE=C,LENGTH=10])

		lStado = .T.

	*> Etiqueta de expedición - MAC.
	Case cTipoEtiqueta=='BU'
		Use In (Select ("ETBUCURSOR"))

		*> Crear la estructura base del cursor de trabajo, a partir de datos de envío.
		=CrtMCursor("F24t,F01p", "ETBUCURSOR", "C")

		*> Agregar los campos de la cabecera de documentos.
		=CrtFCursor("ETBUCURSOR", [TBL=F24c,FLD=F24cCodPro,FLD=F24cTipDoc,FLD=F24cNumDoc,FLD=F24cFecDoc,FLD=F24cFecPre,FLD=F24cCodTra,FLD=F24cPortes])

		*> Agregar observaciones.
		=AddFldToCursor("ETBUCURSOR", [NAME=F24oDesObs,TYPE=C,LENGTH=60])		&& OJO. En origen campo Memo.

		*> Agregar campos de albarán de salida.
		=CrtFCursor("ETBUCURSOR", [TBL=F27c,FLD=F27cNumAlb,FLD=F27cFecAlb,FLD=F27cPesoKg,FLD=F27cBultos])

		*> Agregar campos de la hoja de ruta.
		=CrtFCursor("ETBUCURSOR", [TBL=F30c,FLD=F30cHojRut])

		*> Agregar campos varios.
		=CrtFCursor("ETBUCURSOR", [TBL=F01t,FLD=F01tDescri,TBL=F00j,FLD=F00jDescri])
		=CrtFCursor("ETBUCURSOR", [TBL=F26l,FLD=F26lNumMac,FLD=F26lNumLst,TBL=F26v,FLD=F26vNumMac])
		=CrtFCursor("ETBUCURSOR", [TBL=F26l,FLD=F26lCodPro,TBL=F26l,FLD=F26lTipDoc,TBL=F26l,FLD=F26lNumDoc,TBL=F26l,FLD=F26lCodArt,FLD=F26lCanFis,TBL=F08c,FLD=F08cDescri])
		=AddFldToCursor("ETBUCURSOR", [NAME=Cantid,TYPE=N,LENGTH=6,DECIMALS=0])

		*> Agregar campos de identificación del MAC.
		=AddFldToCursor("ETBUCURSOR", [NAME=NumMac,TYPE=C,LENGTH=9])
		=AddFldToCursor("ETBUCURSOR", [NAME=NumBul,TYPE=N,LENGTH=9,DECIMALS=0])

		*> Agregar campos de envío.
		=AddFldToCursor("ETBUCURSOR", [NAME=Portes,TYPE=C,LENGTH=10])

		lStado = .T.

	*> Etiqueta de lista.
	Case cTipoEtiqueta=='LS'
		Use In (Select ("ETLSCURSOR"))
		=CrtCursor("F10c", "ETLSCURSOR")
		=AddFldToCursor("ETLSCURSOR", [NAME=F80cNumLst,TYPE=C,LENGTH=6])
		lStado = .T.

	*> Tipo de etiqueta no válido.
	Otherwise
		This.UsrError = "Tipo de etiqueta no válido"
		lStado = .F.
EndCase

Return lStado

ENDPROC
PROCEDURE etiquetadirecta

*> Impresión de etiquetas directas.

*> Genera las propiedades de impresión de forma directa (desde el programa), mediante:
*>	- This.CrearPropiedad(), ó bien
*>	- This.CrearPropiedadCursor()

*> A diferencia de otras etiquetas, el cursor no se cierra aquí, sino en Main.

*> Recibe:
*>	- Cursor de trabajo, ya generado (opcional).
*>	- Formato de etiquetas (opcional).
*>	- Dispositivo de salida (opcional).

*>	- This.RNCursor, cursor con los datos.
*>	- This.RNFormato, nombre del formato de impresión.
*>	- This.RNPrinter, dispositivo de salida.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cCursor, cFormato, cPrinter

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNCursor = IIf(Type('cCursor')=='C', cCursor, .RNCursor)
	.RNFormato = IIf(Type('cFormato')=='C', cFormato, .RNFormato)
	.RNPrinter = IIf(Type('cPrinter')=='C', cPrinter, .RNPrinter)
EndWith

*> Validar los rangos.
lStado = This._etdivalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select (This._Cursor)
Go Top
Do While !Eof()
	*> Asignar valores a las propiedades de la clase.
	Scatter Name oCUR
	lStado = This._etdiasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiquetadirecta()

	*> Next ocupación.
	Select (This._Cursor)
	Skip
EndDo

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Return lStado

ENDPROC
PROCEDURE _etdivalidarrangos

*> Validación de los rangos (etiquetas directas).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaDirecta(), impresión de etiquetas directas.

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

*> Cursor de trabajo (generado en Main)
With This
	If Empty(.RNCursor)
		.UsrError = "Falta cursor de trabajo"
		lStado = .F.
		Return lStado
	EndIf

	If !Used(.RNCursor)
		.UsrError = "Cursor de trabajo no activo"
		lStado = .F.
		Return lStado
	EndIf

	*> Rangos OK: Pasar a propiedades.
	._Cursor = .RNCursor
EndWith

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etdiasignarvalores

*> Asignar valores del registro activo a propiedes. Impresión de etiqueta directa
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaDirecta(), impresión de etiquetas directas.

*> Recibe:
*>	- oReg, registro actual de datos.

*> Devuelve: Estado (.T. / .F.)

Parameters oReg

Private cPropiedad, cAsignar, oFields
Local lStado, nInx

*> Cargar las propiedades en un array.
lStado = (AMembers(oFields, oReg) > 0)
If !lStado
	This.UsrError = "No se han podido asignar las propiedades"
	Return lStado
EndIf

*> Generar las propiedades en la clase.
For nInx = 1 To ALen(oFields)
	cPropiedad = "This." + oFields(nInx)
	If Type(cPropiedad) <> 'U'
		cAsignar = cPropiedad + "=oReg." + oFields(nInx)
		&cAsignar
	EndIf
EndFor

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etimprimiretiquetadirecta

*> Impresión de etiquetas directas.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaDirecta(), impresión de etiquetas manuales.

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto error.

Local lStado, nInx
Private cPrm

This.UsrError = ""
lStado = .T.

For nInx = 1 To This.nNumPrm
	*> Substituir conceptos por su valor.
	cPrm = Alltrim(This.cDetEti[nInx].Parametro)	&& Línea de caracteres de control impresora.
	lStado = This._etchgdirectbyvalue(cPrm)			&& Cambiar concepto por su valor.
	If !lStado
		Return lStado
	EndIf

	? This.cLineaEtiqueta							&& Enviar a dispositivo de salida.
EndFor

Return lStado

ENDPROC
PROCEDURE _etchgdirectbyvalue

*> Proceso para traducir un concepto de impresión por su valor (etiquetas directas).
*> Utiliza conceptos generados y asignados de forma dinámica desde This.EtiquetaDirecta().
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etimprimiretiquetadirecta(), impresión de etiquetas directas.

*> Recibe:
*>	- cPrm, línea de parámetros impresora.
*>	- This._LDelimiter, delimitador izquierdo para conceptos.
*>	- This._RDelimiter, delimitador derecho para conceptos.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, Texto error.
*>	- This.cLineaEtiqueta, buffer de impresión.

*> Codificación de conceptos:
*> Se describen los conceptos generales (fecha, copias, ...), pues el resto son automáticos.
*>	- COPIAS, copias etiqueta.
*>	- DATE, fecha de sistema.

*>	- SUBST, SubString del concepto(si es una cadena).
*>	- MASK, Máscara de formateo de un concepto numérico.
*>	- LTRIM, Eliminar espacios en blanco a la izquierda.
*>	- RTRIM, Eliminar espacios en blanco a la derecha.
*>	- CAT, Proceso de concatenación de conceptos.
*>	- FILE, Trata el concepto a continuación como un fichero.

*> Historial de modificaciones:
*> 14.03.2008 (AVC) Corregir redondeos de cantidades con decimales.

Parameters cPrm

Local lStado, nd1, nd2, cCodCon, cConcepto, cParams
Local lHayParametros, lConcat, cMask, nSubStD, nSubStL, lLTrim, lRTrim, cConcat, lFile
Local cPropiedad, cTipoConcepto

This.UsrError = ""
lStado = .T.
Store "" To cMask, cConcat
Store 0 To nSubStD, nSubStL
Store .F. To lHayParametros, lConcat, lLTrim, lRTrim, lFile

*> Inicializar el buffer de salida.
This.cLineaEtiqueta = ""

cParams = cPrm
nd1 = At(This._LDelimiter, cParams)

*> Selección de los parámetros recibidos.
Do While nd1 > 0
	*> Obtener el concepto a calcular.
	nd2 = At(This._RDelimiter, cParams)
	If nd2 > 0
		cCodCon = SubStr(cParams, nd1, nd2 - nd1 + 2)
		cConcepto = SubStr(cParams, nd1 + 2, nd2 - nd1  - 2)

		*> Obtener el valor del concepto a calcular.
		Do Case
			*> Copias de etiqueta.
			Case cConcepto=='COPIAS'
				cValor = AllTrim(Str(This._Copias))

			*> Fecha del sistema.
			Case cConcepto=='DATE'
				cValor = This._Date

			*> Es una máscara de formateo numérico.
			Case Left(cConcepto, 4)=='MASK'
				cMask = SubStr(cConcepto, 5)
				lHayParametros = .T.
				cValor = ""

			*> Es la definición de un substring.
			Case Left(cConcepto, 5)=='SUBST'
				nSubStD = Val(SubStr(cConcepto, 6, 3))
				nSubStL = Val(SubStr(cConcepto, 9, 3))
				lHayParametros = .T.
				cValor = ""

			*> Eliminar espacios a la izquierda.
			Case Left(cConcepto, 5)=='LTRIM'
				lLTrim = .T.
				lHayParametros = .T.
				cValor = ""

			*> Eliminar espacios a la derecha.
			Case Left(cConcepto, 5)=='RTRIM'
				lRTrim = .T.
				lHayParametros = .T.
				cValor = ""

			*> Inicio / fin de concatenación.
			Case cConcepto=='CAT'
				lConcat = !lConcat
				cValor = ""

			*> Enviar como fichero.
			Case Left(cConcepto, 4)=='FILE'
				lFile = .T.
				lHayParametros = .T.
				cValor = ""

			*> Conceptos calculados.
			Otherwise
				cPropiedad = "This." + cConcepto
				cTipoDeDato = Type(cPropiedad)

				*> Obtener valor según el tipo de datos.
				Do Case
					*> Concepto alfanumérico.
					Case cTipoDeDato=='C'
						cValor = &cPropiedad

					*> Concepto numérico.
					Case cTipoDeDato=='N'
						cValor = AllTrim(Str(&cPropiedad, 12, 4))

					*> Concepto fecha.
					Case cTipoDeDato=='D'
						cValor = DtoC(&cPropiedad)

					*> Resto de casos: No se contemplan.
					Otherwise
						cValor = ""
				EndCase
		EndCase

		*> Formatear el valor, si cal.
		If (!Empty(cValor) .Or. Len(cValor) > 0) .And. lHayParametros
			cValor = Iif(Empty(cMask), cValor, Transform(Val(cValor), cMask))
			cValor = Iif(nSubStD==0, cValor, SubStr(cValor, nSubStD, nSubStL))
			cValor = Iif(!lLTrim, cValor, LTrim(cValor))
			cValor = Iif(!lRTrim, cValor, RTrim(cValor))
			cValor = Iif(!lFile, cValor, This._logogrftostring(cValor))

			Store "" To cMask
			Store 0 To nSubStD, nSubStL
			Store .F. To lHayParametros, lLTrim, lRTrim, lFile
		EndIf

		*> Tratamiento de conceptos que se concatenan.
		Do Case
			*> Concatenar activo: agregar concepto actual a agrupado.
			Case lConcat
				cConcat = cConcat + cValor
				cValor = ""

			*> Fin de concatenación: Asignar concepto concatenado a la línea de impresión.
			Case !Empty(cConcat) .And. !lConcat
				cValor = cConcat
				cConcat = ""
		EndCase

		*> Reemplazar el concepto por su valor en la línea de impresión.
		cParams = StrTran(cParams, cCodCon, cValor, 1, 1)
	Else
		*> Caracteres de control de concepto erróneos.
		lStado = .F.
		This.UsrError = "Delimitadores erróneos"
		Return lStado
	EndIf

	*> Siguiente concepto a calcular.
	nd1 = At(This._LDelimiter, cParams)
EndDo

This.cLineaEtiqueta = cParams
Return lStado

ENDPROC
PROCEDURE asignarpropiedad

*> Asignar valor a una propiedad para impresión de etiquetas directas.
*> Utilizado para la impresión de etiquetas directas.

*> Recibe:
*>	- Parámetros de la propiedad, con el formato: NAME=<valor>, VALUE=<valor>
*>    Por defecto es CHAR.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropiedad

Local lStado

lStado = This._asignarpropiedad(cPropiedad)
Return lStado

ENDPROC
PROCEDURE _asignarpropiedad

*> Asignar valor a propiedad de la clase.
*> Utilizado para la impresión de etiquetas directas.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.AsignarPropiedad()

*> Recibe:
*>	- Parámetros de la propiedad, con el formato: NAME=<valor>, VALUE=<valor>
*>    Por defecto es CHAR.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cParametros

Local lStado, nd, cPrm, cVal, cParams
Local cName, cValue, cPropiedad, cTipoDeDato

*> Texto mensaje de error.
This.UsrError = ""

lStado = .F.
Store "" To cName, cValue

cParams = cParametros
If Type('cParams') # 'C'
	This.UsrError = "Parámetro vacío"
	Return lStado
EndIf

nd = At('[', cParams)
If nd > 0
   cParams = SubStr(cParams, 2)
EndIf

nd = At(']', cParams)
If nd > 0
   cParams = SubStr(cParams, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cParams)
   *> Primer paso: separar parámetros.
   nd = At(',', cParams)
   If nd > 0
      cUpd = SubStr(cParams, 1, nd - 1)
      cParams = SubStr(cParams, nd + 1)
   Else
      cUpd = cParams
      cParams = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.

   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      *> Nombre de la propiedad a definir.
      Case cPrm=='NAME'
         cName = cVal

      *> Tipo de dato de la propiedad a definir.
      Case cPrm=='VALUE'
         cValue = cVal

      *> No es un parámetro reconocido.
      Otherwise
   EndCase
EndDo

If Empty(cName)
	*> Nombre de la propiedad no definido.
	This.UsrError = "Nombre no definido"
	Return lStado
EndIf

If Empty(cValue)
	*> Por defecto en blanco.
	cValue = ""
EndIf

cPropiedad = "This." + cName
cTipoDeDato = Type(cPropiedad)

*> Asignar valor a la propiedad.
Do Case
	Case cTipoDeDato=='C'
		&cPropiedad = cValue
	Case cTipoDeDato=='N'
		&cPropiedad = Val(cValue)
	Case cTipoDeDato=='D'
		&cPropiedad = CToD(cValue)
EndCase

lStado = .T.
Return lStado

ENDPROC
PROCEDURE etiquetaexpedicionmac

*> Impresión de etiquetas de expedición - MAC.

*> Recibe:
*>	- Propietario / Tipo / Nº Documento (opcionales)
*>	- This.RNxxxxxx, rangos pasados como propiedades.
*>	- This.RNOrigen, origen de datos (LS: Listas-F26l, BU: MACs-F26v)

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropietario, cTipo, cDocumento, cAlbaran

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNProIni = IIf(Type('cPropietario')=='C', cPropietario, .RNProIni)
	.RNTDoIni = IIf(Type('cTipo')=='C', cTipo, .RNTDoIni)
	.RNNDoIni = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoIni)
	.RNAlbIni = IIf(Type('cAlbaran')=='C', cAlbaran, .RNAlbIni)

	*> Asignar rangos hasta por defecto.
	.RNProFin = IIf(Type('cPropietario')=='C', cPropietario, .RNProFin)
	.RNTDoFin = IIf(Type('cTipo')=='C', cTipo, .RNTDoFin)
	.RNNDoFin = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoFin)
	.RNAlbFin = IIf(Type('cAlbaran')=='C', cAlbaran, .RNAlbFin)
EndWith

*> Validar los rangos.
lStado = This._etbuvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("MC")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETBUCURSOR.
lStado = This._etbucargardatos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select ETBUCURSOR
Scan For !Empty(NumBul)
	Scatter Name oCUR

	*> Asignar valores a las propiedades de la clase.
	lStado = This._etbuasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()
EndScan

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETBUCURSOR"))
Return lStado

ENDPROC
PROCEDURE _etbuvalidarrangos

*> Validación de los rangos (etiquetas de expedición-MAC).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicionMAC(), impresión de etiquetas de expedición-documentos.

*> Recibe:

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

With This
	*> Rangos desde-hasta.
	If .RNProIni > .RNProFin .Or. ;
	   .RNTDoIni > .RNTDoFin .Or. ;
	   .RNNDoIni > .RNNDoFin .Or. ;
	   .RNFDoIni > .RNFDoFin .Or. ;
	   .RNLstIni > .RNLstFin .Or. ;
	   .RNMacIni > .RNMacFin
		.UsrError = "Rangos desde-hasta erróneos"
		lStado = .F.
		Return lStado
	EndIf

	*> Origen de los datos.
	If Empty(.RNOrigen)
		.UsrError = "Falta origen de datos"
		lStado = .F.
		Return lStado
	EndIf

	*> Asignar rangos a propiedades.
	._Origen = .RNOrigen
EndWith

lStado = .T.
Return lStado

ENDPROC
PROCEDURE _etbucargardatos

*> Generar datos para la impresión de etiquetas de expedición-MAC.
*> Tomar datos de ocuàciones / MPs,en función del origen de los datos.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicionMAC(), impresión de etiquetas de expedición-MAC.

*> Recibe:
*>	- This.RNxxxx, rango datos.
*>	- This._Origen, origen datos:
*>		'LS', Listas de trabajo(F26l).
*>		'BU', MACs(F26v).

*> Genera:
*>	- ETBUCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""
Store .T. To lStado

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("BU")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

Do Case
	*> Origen: Listas de trabajo.
	Case This._Origen=='LS'
		lStado = This._etbucargardatosls()

	*> Origen: MACs.
	Case This._Origen=='BU'
		lStado = This._etbucargardatosbu()

	*> Resto casos: Error.
	Otherwise
EndCase

Select ETBUCURSOR
Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etbucargardatosls

*> Generar datos para la impresión de etiquetas de expedición-MAC.
*> Origen de datos: Listas de trabajo.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etbucargardatos(), cargar datos de etiquetas de expedición-MAC.

*> Recibe:

*> Genera:
*>	- ETBUCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oMAC, oEnv, oAlb, oPro, oPrv, oTra, cRegActual, nMacActual, cDocActual
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
Store .T. To lStado

*> Cargar los datos de documentos de salida.
cWhere =               "F24cCodPro Between '" + This.RNProIni + "' And '" + This.RNProFin + "'"
cWhere = cWhere + " And F24cTipDoc Between '" + This.RNTDoIni + "' And '" + This.RNTDoFin + "'"
cWhere = cWhere + " And F24cNumDoc Between '" + This.RNNDoIni + "' And '" + This.RNNDoFin + "'"
cWhere = cWhere + " And F24cFecDoc Between " + _GCD(This.RNFDoIni) + " And " + _GCD(This.RNFDoFin)

*> Relacionar con las listas de trabajo. OJO!! Hay que cargar todos los bultos del albarán.
cWhere = cWhere + " And F26lCodPro=F24cCodPro And F26lTipDoc=F24cTipDoc And F26lNumDoc=F24cNumDoc"
cWhere = cWhere + " And F26lNumLst Between '" + This.RNLstIni + "' And '" + This.RNLstFin + "'"
cWhere = cWhere + " And F26lNumAlb Between '" + This.RNAlbIni + "' And '" + This.RNAlbFin + "'"
*cWhere = cWhere + " And F26lNumMac Between '" + This.RNMacIni + "' And '" + This.RNMacFin + "'"

cWhere = cWhere + " And F08cCodPro=F26lCodPro And F08cCodArt=F26lCodArt"
cWhere = cWhere + " And F26lEstMov<>'4'"

cField = "*, F26lCanFis as Cantid"
cOrder = "F26lNumMac, F24cCodPro, F24cTipDoc, F24cNumDoc, F26lCodArt"
cFileF = "F24c,F26l,F08c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETBUCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Return lStado
EndIf

*> Para controlar la lista / MAC actuales.
cRegActual = Space(9)
cRefActual = Space(19)
cDocActual = Space(23)
nMacActual = 0

Select ETBUCURSOR
Delete For Empty(F26lNumMac)
Go Top
lStado = !Eof()
If !lStado
	This.UsrError = "No hay datos asignados"
	Return lStado
EndIf

Do While !Eof()
	Scatter Name oMAC

*!*		If cRegActual==oMAC.F26lNumMac
*!*			*> Es el mismo MAC.
*!*			Skip
*!*			Loop
*!*		EndIf

*!*		If cDocActual<>oMAC.F24cCodPro + oMAC.F24cTipDoc + oMAC.F24cNumDoc
*!*			*> Cambio de documento.
*!*			nMacActual = 0
*!*		EndIf

	If cRegActual==oMAC.F26lNumMac and cRefActual==oMAC.F26lCodPro + oMAC.F26lCodArt

		*> Se el el mismo MAC/Referencia acumulo cantidad.
		Select ETBUCURSOR
		Skip - 1
		Replace Cantid With Cantid + oMAC.F26lCanFis

		Skip
		Delete
				
		Skip
		Loop
	EndIf

	nMacActual = nMacActual + 1

	*> Cargar los datos de la dirección de envío.
	cWhere = 		  "F24tCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F24tTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F24tNumDoc='" + oMAC.F24cNumDoc + "'"

	lStado = f3_sql("*", "F24t", cWhere, , , "F24tCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos envío"
			Use In (Select ("F24tCur"))
			Return lStado
		EndIf

		Select F24tCur
		Scatter Name oEnv Blank
	Else
		Select F24tCur
		Scatter Name oEnv
	EndIf

	Use In (Select("F24tCur"))

	*> Cargar datos de albarán / hoja de ruta.
	cWhere = 		  "F27cCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F27cTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F27cNumDoc='" + oMAC.F24cNumDoc + "' And "
	cWhere = cWhere + "F30cTipEnt='PROP' And F30cCodEnt=F27cCodPro And F30cTipDoc=F27cTipDoc And F30cNumDoc=F27cNumDoc"

	lStado = f3_sql("*", "F27c,F30c", cWhere, , , "F2730Cur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos albarán"
			Use In (Select ("F2730Cur"))
			Return lStado
		EndIf

		Select F2730Cur
		Scatter Name oAlb Blank
	Else
		Select F2730Cur
		Scatter Name oAlb
	EndIf

	Use In (Select ("F2730Cur"))

	*> Cargar los datos del propietario.
	m.F01pCodigo = oMAC.F24cCodPro
	=f3_seek("F01p")
	Select F01p
	Scatter Name oPro

	*> Cargar la descripción de la provincia del propietario.
	m.F00jCodPrv = oPro.F01pProvin
	=f3_seek("F00j")
	Select F00j
	Scatter Name oPrv

	*> Cargar los datos del transportista.
	m.F01tCodigo = oMAC.F24cCodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oTra

	Select ETBUCURSOR
	Gather Name oEnv
	Gather Name oAlb
	Gather Name oPro
	Gather Name oPrv
	Gather Name oTra

	Replace NumMac With oMAC.F26lNumMac
	Replace NumBul With nMacActual
	Replace Portes With Iif(oMAC.F24cPortes=='P', 'PAGADOS', 'DEBIDOS')

	*> Memorizar current values.
	cRegActual = oMAC.F26lNumMac
	cRefActual = oMAC.F24cCodPro + oMAC.F26lCodArt
	cDocActual = oMAC.F24cCodPro + oMAC.F24cTipDoc + oMAC.F24cNumDoc

	Skip
EndDo

Delete For F26lNumMac < This.RNMacIni .Or. F26lNumMac > This.RNMacFin
Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etbucargardatosbu

*> Generar datos para la impresión de etiquetas de expedición-MAC.
*> Origen de datos: Volumetría.
*> Método privado de la clase.

*> Llamado desde:
*>	- This._etbucargardatos(), cargar datos de etiquetas de expedición-MAC.

*> Recibe:

*> Genera:
*>	- ETBUCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oMAC, oEnv, oAlb, oPro, oPrv, oTra, oObs
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
Store .T. To lStado

*> Cargar los datos de documentos de salida.
cWhere =          "F24cCodPro Between '" + This.RNProIni + "' And '" + This.RNProFin + "' And "
cWhere = cWhere + "F24cTipDoc Between '" + This.RNTDoIni + "' And '" + This.RNTDoFin + "' And "
cWhere = cWhere + "F24cNumDoc Between '" + This.RNNDoIni + "' And '" + This.RNNDoFin + "' And "
cWhere = cWhere + "F24cFecDoc Between " + _GCD(This.RNFDoIni) + " And " + _GCD(This.RNFDoFin) + " And "

*> Relacionar con los MACs.
cWhere = cWhere + "F26vCodPro=F24cCodPro And F26vTipDoc=F24cTipDoc And F26vNumDoc=F24cNumDoc And "
cWhere = cWhere + "F26vNumLis Between '" + This.RNLstIni + "' And '" + This.RNLstFin + "' And "
cWhere = cWhere + "F26vNumMac Between '" + This.RNMacIni + "' And '" + This.RNMacFin + "'"

cField = "*"
cOrder = "F26vNumMac, F26vNumLis, F24cCodPro, F24cTipDoc, F24cNumDoc"
cFileF = "F24c,F26v"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETBUCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Return lStado
EndIf

Select ETBUCURSOR
Go Top

Do While !Eof()
	Scatter Name oMAC

	*> Cargar los datos de la dirección de envío.
	cWhere = 		  "F24tCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F24tTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F24tNumDoc='" + oMAC.F24cNumDoc + "'"

	lStado = f3_sql("*", "F24t", cWhere, , , "F24tCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos envío"
			Use In (Select ("F24tCur"))
			Return lStado
		EndIf

		Select F24tCur
		Scatter Name oEnv Blank
	Else
		Select F24tCur
		Scatter Name oEnv
	EndIf

	Use In (Select("F24tCur"))

	*> Cargar las observaciones de cabecera / referencia cliente.
	cWhere = 		  "F24oCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F24oTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F24oNumDoc='" + oMAC.F24cNumDoc + "' And "
	cWhere = cWhere + "F24oLinObs='0000'"

	lStado = f3_sql("*", "F24o", cWhere, , , "F24oCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga observaciones 0000"
			Use In (Select ("F24oCur"))
			Return lStado
		EndIf

		Select F24oCur
		Scatter Name oObs Blank Memo
	Else
		Select F24oCur
		Scatter Name oObs Memo
	EndIf

	Use In (Select("F24oCur"))

	*> Cargar datos de albarán / hoja de ruta.
	cWhere = 		  "F27cCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F27cTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F27cNumDoc='" + oMAC.F24cNumDoc + "' And "
	cWhere = cWhere + "F30cTipEnt='PROP' And F30cCodEnt=F27cCodPro And F30cTipDoc=F27cTipDoc And F30cNumDoc=F27cNumDoc"

	lStado = f3_sql("*", "F27c,F30c", cWhere, , , "F2730Cur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos albarán"
			Use In (Select ("F2730Cur"))
			Return lStado
		EndIf

		Select F2730Cur
		Scatter Name oAlb Blank
	Else
		Select F2730Cur
		Scatter Name oAlb
	EndIf

	Use In (Select ("F2730Cur"))

	*> Cargar los datos del propietario.
	m.F01pCodigo = oMAC.F24cCodPro
	=f3_seek("F01p")
	Select F01p
	Scatter Name oPro

	*> Cargar la descripción de la provincia del propietario.
	m.F00jCodPrv = oPro.F01pProvin
	=f3_seek("F00j")
	Select F00j
	Scatter Name oPrv

	*> Cargar los datos del transportista.
	m.F01tCodigo = oMAC.F24cCodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oTra

	Select ETBUCURSOR
	Gather Name oEnv
	Gather Name oAlb
	Gather Name oPro
	Gather Name oPrv
	Gather Name oTra

	Replace F24oDesObs With MLine(oObs.F24oDesObs, 1)

	Replace F26lNumLst With oMAC.F26vNumLis
	Replace NumMac With oMAC.F26vNumMac
	Replace NumBul With Val(oMAC.F26vNumBul)
	
	Replace Portes With Iif(oMAC.F24cPortes=='P', 'PAGADOS', 'DEBIDOS')

	Skip
EndDo

Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etbuasignarvalores

*> Asignar valores del registro activo a propiedades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicionMAC(), impresión de etiquetas de expedición-MAC.

*> Recibe:
*>	- oMAC, registro actual de datos.

*> Devuelve: .T.

Parameters oMAC

With This
	.PKCodPro = oMAC.F24cCodPro

	.EXCodTra = oMAC.F24cCodTra
	.EXNumDoc =	oMAC.F24cNumDoc
	.EXFecDoc =	oMAC.F24cFecDoc
	.EXFecPre =	oMAC.F24cFecPre

	.EXNumAlb =	oMAC.F27cNumAlb
	.EXFecAlb =	oMAC.F27cFecAlb
	.EXBultos =	oMAC.F27cBultos
	.EXKilos  =	oMAC.F27cPesoKg
	.EXPortes =	oMAC.Portes

	.EXHojRut =	oMAC.F30cHojRut
	.EXDesTra = oMAC.F01tDescri
	.EXObserv = oMAC.F24oDesObs

	.EXDesPro = oMAC.F01pDescri
	.EXDirPro = oMAC.F01pDirecc
	.EXPobPro = oMAC.F01pPoblac
	.EXProPro = oMAC.F00jDescri
	.EXPosPro = oMAC.F01pCodPos
	.EXTelPro = oMAC.F01pNumTel
	.EXFaxPro = oMAC.F01pNumFax
	.EXEMail  = oMAC.F01pEMail
	.EXPagWeb = oMAC.F01pPagWeb
	.EXLogoP  = oMAC.F01pLogo

	.EXNomAso = oMAC.F24tNomAso
	.EX1erDir = oMAC.F24t1erDir
	.EX2ndDir = oMAC.F24t2ndDir
	.EX3rdDir = oMAC.F24t3rdDir
	.EXDPobla = oMAC.F24tDPobla
	.EXDProvi = oMAC.F24tDProvi
	.EXCodPos = oMAC.F24tCodPos

	.EXNumTel = oMAC.F24tNumTel
	.EXNumFax = oMAC.F24tNumFax
	.EXNumNif = oMAC.F24tNumNif

	.EXNumMac = oMAC.NumMac
	.EXCurBul = oMAC.NumBul

	.EXCodPas = oMAC.F24tCodPas

	.OCCanFis = oMAC.Cantid
	.PKDesArt = oMAC.F08cDescri

EndWith

*> LRC. 19.1.9
*> Control para asegurar que se imprime la provicia destino.
If Empty(This.EXDProvi) and !Empty(This.EXCodPos)
	*> Cargar la descripción de la provincia destino.
	m.F00jCodPrv = PadL(Left(This.EXCodPos,2),4,'0')
	This.EXDProvi = IIf(f3_seek("F00j"), Alltrim(F00j.F00jDescri), This.EXDProvi)
EndIf

Return

ENDPROC
PROCEDURE _logogrftostring

*> Convertir un logo (en formato GRF) en una cadena para enviar a la impresora.

Parameters cLogo
Private Nd, cCadenaLogo, cCadena

Store "" To cCadena, cCadenaLogo

If Empty(cLogo)
   Return cCadenaLogo					&& No se ha definido ningún logo.
EndIf

Nd = FOpen(cLogo, 0)

Do While Nd > 0 .And. !FEof(Nd)
	cCadena = FGetS(Nd)
	If !Empty(cCadena)
		cCadenaLogo = cCadenaLogo + AllTrim(cCadena) + cr
   EndIf
EndDo

=FClose(Nd)

Return cCadenaLogo

ENDPROC
PROCEDURE _printeropen

*> Asignar dispositivo de salida para las etiquetas.
*> Recibe:
*>	- This._Printer, nombre del dispositivo de salida.

*> Devuelve:
*>	- This.lPrinterIsOpen, control de impresora activa.

If !This.lPrinterIsOpen
	Set Printer To Name (This._Printer)
	Set Console Off
	Set Printer On
	Set Device To Printer

	This.lPrinterIsOpen = .T.
EndIf

Return This.lPrinterIsOpen

ENDPROC
PROCEDURE _printerclose

*> Cerrar el dispositivo de salida para las etiquetas.
*> Recibe:
*>	- This._Printer, nombre del dispositivo de salida.
*>	- This.lPrinterIsOpen, control de impresora activa.

*> Devuelve:
*>	- This.lPrinterIsOpen, control de impresora activa.

If This.lPrinterIsOpen
	Set Console On
	Set Device To Screen
	Set Printer Off
	Set Printer To Default

	This.lPrinterIsOpen = .F.
EndIf

Return

ENDPROC
PROCEDURE oubicobj_access

*> Inicializar clase ubicación
If Type('This.oUbicObj') # 'O'
	This.oUbicObj = CreateObject('OraFncUbic')
EndIf

Return This.oUbicObj

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:

*> 14.03.2008 (AVC) Corregir redondeos de cantidades con decimales.
*>					Modificado método This._etchgconceptbyvalue
*>					Modificado método This._etchgdirectbyvalue
*>					Modificado método This._crearcursoretiqueta
*> 16.05.2008 (AVC) Agregar etiquetas de tipo lista (LS)
*>					Añadido método This.EtiquetaLista
*>					Añadido método This._etlsvalidarrangos
*>					Añadido método This._etlscargardatos
*>					Añadido método This._etlsasignarvalores
*>					Añadida propiedad This.LSNumLst
*>					Modificado método This._crearcursoretiqueta
*>					Modificado método This._etchgconceptbyvalue
*>					Modificado método This.InitLocals
*> 19.05.2008 (AVC) Agregar concepto 'DOCUMENTO' a la impresión de etiquetas de palet.
*>					Añadida propiedad This.OCDocOcu
*>					Modificado método This._crearcursoretiqueta
*>					Modificado método This._etchgconceptbyvalue
*>					Modificado método This._etoccargardatosmp
*>					Modificado método This._etoccargardatosoc
*>					Modificado método This._etocasignarvalores
*>					Modificado método This.InitLocals

ENDPROC
PROCEDURE etiquetalista

*> Impresión de etiquetas de listas.
*> Imprime tanto etiquetas de listas de trabajo como de listas de carga.

*> Recibe:
*>	- Lista inicial (opcional)
*>	- Lista final (opcional)
*>	- Modo (V/H) (opcional)
*>	- This.RNLstIni, lista inicial.
*>	- This.RNLstFin, lista final.

*> Devuelve:
*>	- Estado (.T. / .F.)

*> Historial de modificaciones:
*> 16.05.2008 (AVC) Creación.

Parameters cListaDesde, cListaHasta, cModo

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNLstIni = IIf(Type('cListaDesde')=='C', cListaDesde, .RNLstIni)
	.RNLstFin = IIf(Type('cListaHasta')=='C', cListaHasta, .RNLstFin)
	.RNModo = IIf(Type('cModo')=='C', cModo, .RNModo)
EndWith

*> Validar los rangos.
lStado = This._etlsvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("LS")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETLSCURSOR.
lStado = This._etlscargardatos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen

*> Proceso de impresión de etiquetas.
Select ETLSCURSOR
Go Top
Do While !Eof()
	*> Asignar valores a las propiedades de la clase.
	Scatter Name oCUR
	lStado = This._etlsasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()

	*> Next ubicación.
	Select ETLSCURSOR
	Skip
EndDo

Use In (Select ("ETLSCURSOR"))
Return

ENDPROC
PROCEDURE _etlsvalidarrangos

*> Validación de los rangos (etiquetas de lista).
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaLista(), impresión de etiquetas de lista.

*> Recibe:
*>	- This.RNLstIni, lista desde.
*>	- This.RNLstFin, lista hasta.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado

This.UsrError = ""

*> Validar los rangos generales.
lStado = This._etvalidarrangos()
If !lStado
	Return lStado
EndIf

*> Validar los rangos de impresión de etiquetas de ubicación.
With This
	*> Rangos desde-hasta.
	If .RNLstIni > .RNLstFin
		.UsrError = "Rangos desde-hasta erróneos"
		lStado = .F.
		Return lStado
	EndIf
EndWith

Return

ENDPROC
PROCEDURE _etlscargardatos

*> Generar datos para la impresión de etiquetas de lista.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaLista(), impresión de etiquetas de lista.

*> Recibe:

*> Genera:
*>	- ETLSCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

*> Historial de modificaciones:
*> 16.05.2008 (AVC) Creación.

Local lStado
Private cWhere, cField, cOrder

This.UsrError = ""

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("LS")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

cWhere = "F80cNumLst Between '" + This.RNLstIni + "' And '" + This.RNLstFin + "' And F80cEstLst<='1'"
cOrder = "F80cNumLst"
lStado = f3_sql("F80cNumLst", "F80c", cWhere, cOrder, , "ETLSCURSOR")

Return

ENDPROC
PROCEDURE _etlsasignarvalores

*> Asignar valores del registro activo a propiedades.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaLista(), impresión de etiquetas de lista.

*> Recibe:
*>	- oLst, registro actual de la lista.

*> Devuelve: .T.

*> Historial de modificaciones:
*> 16.05.2008 (AVC) Creación.

Parameters oLST

With This
	.LSNumLst = oLST.F80cNumLst
EndWith

Return

ENDPROC
PROCEDURE etiquetaexpedicionemac

*> Impresión de etiquetas de expedición - MAC.

*> Recibe:
*>	- Propietario / Tipo / Nº Documento (opcionales)
*>	- This.RNxxxxxx, rangos pasados como propiedades.
*>	- This.RNOrigen, origen de datos (LS: Listas-F26l, BU: MACs-F26v)

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropietario, cTipo, cDocumento

Local lStado
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNProIni = IIf(Type('cPropietario')=='C', cPropietario, .RNProIni)
	.RNTDoIni = IIf(Type('cTipo')=='C', cTipo, .RNTDoIni)
	.RNNDoIni = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoIni)

	*> Asignar rangos hasta por defecto.
	.RNProFin = IIf(Type('cPropietario')=='C', cPropietario, .RNProFin)
	.RNTDoFin = IIf(Type('cTipo')=='C', cTipo, .RNTDoFin)
	.RNNDoFin = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoFin)
EndWith

*> Validar los rangos.
lStado = This._etbuvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("MC")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETBUCURSOR.
lStado = This._etbucargardatos2()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
Select ETBUCURSOR
Scan For !Empty(NumMAC)
	Scatter Name oCUR

	*> Asignar valores a las propiedades de la clase.
	lStado = This._etbuasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()
EndScan

*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETBUCURSOR"))
Return lStado

ENDPROC
PROCEDURE etiquetaexpedicionetex

*> Impresión de etiquetas de expedición - documento.

*> Recibe:
*>	- Propietario / Tipo / Nº Documento (opcionales)
*>	- This.RNxxxxxx, rangos pasados como propiedades.

*> Devuelve:
*>	- Estado (.T. / .F.)

Parameters cPropietario, cTipo, cDocumento

Local lStado, nInx, nMaxEti
Private oCUR

*> Texto mensaje de error.
This.UsrError = ""

*> Asignar parámetros a propiedades.
With This
	.RNProIni = IIf(Type('cPropietario')=='C', cPropietario, .RNProIni)
	.RNTDoIni = IIf(Type('cTipo')=='C', cTipo, .RNTDoIni)
	.RNNDoIni = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoIni)

	*> Asignar rangos hasta por defecto.
	.RNProFin = IIf(Type('cPropietario')=='C', cPropietario, .RNProFin)
	.RNTDoFin = IIf(Type('cTipo')=='C', cTipo, .RNTDoFin)
	.RNNDoFin = IIf(Type('cDocumento')=='C', cDocumento, .RNNDoFin)
EndWith

*> Validar los rangos.
lStado = This._etexvalidarrangos()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Cargar los parámetros de las etiquetas a imprimir.
lStado = This._loadparametros("EX")
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Generar los datos para la impresión: Cursor ETEXCURSOR.
lStado = This._etexcargardatos2()
If !lStado
	*> Los textos de las incidencias se generan en los métodos llamados.
	Return lStado
EndIf

*> Asignar dispositivo de salida.
This._printeropen
*!*	Set Printer To Name (This._Printer)
*!*	Set Console Off
*!*	Set Printer On
*!*	Set Device To Printer

*> Proceso de impresión de etiquetas.
*> Proceso de impresión de etiquetas.
Select ETEXCURSOR
Scan For !Empty(F24cNumDoc)
	Scatter Name oCUR

	*> Asignar valores a las propiedades de la clase.
	lStado = This._etexasignarvalores(oCUR)

	*> Imprimir la etiqueta.
	lStado = This._etimprimiretiqueta()
EndScan


*> Liberar dispositivo de salida.
*!*	Set Console On
*!*	Set Device To Screen
*!*	Set Printer Off
*!*	Set Printer To Default

Use In (Select ("ETEXCURSOR"))
Return lStado

ENDPROC
PROCEDURE _etexcargardatos2

*> Generar datos para la impresión de etiquetas de expedición-documento.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicion(), impresión de etiquetas de expedición-documento.

*> Recibe:

*> Genera:
*>	- ETEXCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError

Local lStado, oExp, oEnv, oPro, oPrv, oTra, oObs, oArt
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
Store .T. To lStado

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("EX")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

*> Cargar los datos de documentos de salida.
cWhere       =         "F24lCodPro Between '" + This.RNProIni       + "' And '" + This.RNProFin + "'"
cWhere = cWhere + " And F24lTipDoc Between '" + This.RNTDoIni       + "' And '" + This.RNTDoFin + "'"
cWhere = cWhere + " And F24lNumDoc Between '" + This.RNNDoIni       + "' And '" + This.RNNDoFin + "'"
cWhere = cWhere + " And F24cFecDoc Between  " + _GCD(This.RNFDoIni) + "  And  " + _GCD(This.RNFDoFin)

*> Relacionar con el detalle del documento.
cWhere = cWhere + " And F24cCodPro=F24lCodPro And F24cTipDoc=F24lTipDoc And F24cNumDoc=F24lNumDoc"

*> Relacionar con los albaranes del cliente.
cWhere = cWhere + " And F27cCodPro=F24cCodPro And F27cTipDoc=F24cTipDoc And F27cNumDoc=F24cNumDoc"
cWhere = cWhere + " And F27cNumAlb Between '" + This.RNAlbIni + "' And '" + This.RNAlbFin + "'"

cField = "*"
cOrder = "F24lCodArt, F24lCodPro, F24lTipDoc, F24lNumDoc"
cFileF = "F24l,F24c,F27c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETEXCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Use In (Select ("ETEXCURSOR"))
	Return lStado
EndIf

Select ETEXCURSOR
Go Top
Do While !Eof()
	Scatter Name oExp

	*> Cargar los datos de la dirección de envío.
	cWhere = 		  "F24tCodPro='" + oExp.F24cCodPro + "' And "
	cWhere = cWhere + "F24tTipDoc='" + oExp.F24cTipDoc + "' And "
	cWhere = cWhere + "F24tNumDoc='" + oExp.F24cNumDoc + "'"

	lStado = f3_sql("*", "F24t", cWhere, , , "F24tCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos envío"
			Use In (Select ("F24tCur"))
			Use In (Select ("ETEXCURSOR"))
			Return lStado
		EndIf

		Select F24tCur
		Scatter Name oEnv Blank
	Else
		Select F24tCur
		Scatter Name oEnv
	EndIf

	Use In (Select("F24tCur"))

	*> Cargar las observaciones de cabecera / referencia cliente.
	cWhere = 		  "F24oCodPro='" + oExp.F24cCodPro + "' And "
	cWhere = cWhere + "F24oTipDoc='" + oExp.F24cTipDoc + "' And "
	cWhere = cWhere + "F24oNumDoc='" + oExp.F24cNumDoc + "' And "
	cWhere = cWhere + "F24oLinObs='0000'"

	lStado = f3_sql("*", "F24o", cWhere, , , "F24oCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga observaciones 0000"
			Use In (Select ("F24oCur"))
			Return lStado
		EndIf

		Select F24oCur
		Scatter Name oObs Blank Memo
	Else
		Select F24oCur
		Scatter Name oObs Memo
	EndIf

	Use In (Select("F24oCur"))

	*> Cargar los datos del propietario.
	m.F01pCodigo = oExp.F24cCodPro
	=f3_seek("F01p")
	Select F01p
	Scatter Name oPro

	*> Cargar la descripción de la provincia del propietario.
	m.F00jCodPrv = oPro.F01pProvin
	=f3_seek("F00j")
	Select F00j
	Scatter Name oPrv

	*> Cargar los datos del transportista.
	m.F01tCodigo = oExp.F24cCodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oTra

	*> Cargar los datos del articulo.
	m.F08cCodPro = oExp.F24lCodPro
	m.F08cCodArt = oExp.F24lCodArt
	=f3_seek("F08c")
	Select F08c
	Scatter Name oArt

	Select ETEXCURSOR
	Gather Name oEnv
	Gather Name oPro
	Gather Name oPrv
	Gather Name oTra
	Gather Name oArt

	Replace Cantid     With oExp.F24lCanDoc
	Replace F24oDesObs With MLine(oObs.F24oDesObs, 1)
	Replace Portes With Iif(oExp.F24cPortes=='P', 'PAGADOS', 'DEBIDOS')

	Skip
EndDo


*> Para controlar el MAC / Documento actuales.
cRegActual = ""


*> Solo dejo activo un registro para cada MAC / Documento.
Select ETEXCURSOR
Go Top
Do While !Eof()
	Scatter Name oMAC

	If Empty(cRegActual)
		cRegActual = oMAC.F24lCodArt + oMAC.F24lCodPro + oMAC.F24lTipDoc + oMAC.F24lNumDoc
		nNumReg = RecNo()
		nItems = oMAC.Cantid
		Skip
		Loop
	EndIf

	If cRegActual==oMAC.F24lCodArt + oMAC.F24lCodPro + oMAC.F24lTipDoc + oMAC.F24lNumDoc
		*> Es el mismo Articulo / Documento: Acumular Items y borrar para eliminar duplicados.
		nItems = nItems + oMAC.Cantid
		Delete Next 1
		Skip
		Loop
	EndIf

	*> Cambio de documento: Actualizar acumulados Items y continuar.
	nCurReg = RecNo()
	Go nNumReg
	Replace Cantid With nItems
	Go nCurReg
	nItems = oMAC.Cantid
	nNumReg = nCurReg
	cRegActual = oMAC.F24lCodArt + oMAC.F24lCodPro + oMAC.F24lTipDoc + oMAC.F24lNumDoc

	Skip
EndDo

*> Actualizar el último registro.
Go nNumReg
Replace Cantid With nItems
Go Top


Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE _etbucargardatos2

*> Generar datos para la impresión de etiquetas de expedición-MAC.
*> Tomar datos de ocuàciones / MPs,en función del origen de los datos.
*> Método privado de la clase.

*> Llamado desde:
*>	- This.EtiquetaExpedicionMAC(), impresión de etiquetas de expedición-MAC.

*> Recibe:
*>	- This.RNxxxx, rango datos.
*>	- This._Origen, origen datos:
*>		'LS', Listas de trabajo(F26l).
*>		'BU', MACs(F26v).

*> Genera:
*>	- ETBUCURSOR, cursor con los datos a imprimir.

*> Devuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError


Local lStado, oMAC, oEnv, oAlb, oPro, oPrv, oTra, oObs, oArt
Private cWhere, cField, cOrder, cFileF

This.UsrError = ""
Store .T. To lStado

*> Crear el cursor de trabajo.
lStado = This._crearcursoretiqueta("BU")
If !lStado
	*> El texto de error ya viene asignando.
	Return lStado
EndIf

*> Cargar los datos del MAC a imprimir.
cWhere =               "F26lCodPro Between '" + This.RNProIni + "' And '" + This.RNProFin + "'"
cWhere = cWhere + " And F26lNumMAC Between '" + This.RNMacIni + "' And '" + This.RNMacFin + "'"

*> Relacionar con los documentos.
cWhere = cWhere + " And F26lCodPro=F24cCodPro And F26lTipDoc=F24cTipDoc And F26lNumDoc=F24cNumDoc"

cField = "*"
cOrder = "F26lNumMac, F26lCodArt, F26lCodPro, F26lTipDoc, F26lNumDoc"
cFileF = "F26l,F24c"

lStado = f3_sql(cField, cFileF, cWhere, cOrder, , "ETBUCURSOR")
If !lStado
	If _xier <= 0
		This.UsrError = "Error en carga de datos"
		Return  lStado
	EndIf

	This.UsrError = "No hay datos"
	Return lStado
EndIf

Select ETBUCURSOR
Go Top

Do While !Eof()
	Scatter Name oMAC

	*> Cargar los datos de la dirección de envío.
	cWhere = 		  "F24tCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F24tTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F24tNumDoc='" + oMAC.F24cNumDoc + "'"

	lStado = f3_sql("*", "F24t", cWhere, , , "F24tCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos envío"
			Use In (Select ("F24tCur"))
			Return lStado
		EndIf

		Select F24tCur
		Scatter Name oEnv Blank
	Else
		Select F24tCur
		Scatter Name oEnv
	EndIf

	Use In (Select("F24tCur"))

	*> Cargar las observaciones de cabecera / referencia cliente.
	cWhere = 		  "F24oCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F24oTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F24oNumDoc='" + oMAC.F24cNumDoc + "' And "
	cWhere = cWhere + "F24oLinObs='0000'"

	lStado = f3_sql("*", "F24o", cWhere, , , "F24oCur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga observaciones 0000"
			Use In (Select ("F24oCur"))
			Return lStado
		EndIf

		Select F24oCur
		Scatter Name oObs Blank Memo
	Else
		Select F24oCur
		Scatter Name oObs Memo
	EndIf

	Use In (Select("F24oCur"))

	*> Cargar datos de albarán / hoja de ruta.
	cWhere = 		  "F27cCodPro='" + oMAC.F24cCodPro + "' And "
	cWhere = cWhere + "F27cTipDoc='" + oMAC.F24cTipDoc + "' And "
	cWhere = cWhere + "F27cNumDoc='" + oMAC.F24cNumDoc + "' And "
	cWhere = cWhere + "F30cTipEnt='PROP' And F30cCodEnt=F27cCodPro And F30cTipDoc=F27cTipDoc And F30cNumDoc=F27cNumDoc"

	lStado = f3_sql("*", "F27c,F30c", cWhere, , , "F2730Cur")
	If !lStado
		If _xier <= 0
			This.UsrError = "Error carga datos albarán"
			Use In (Select ("F2730Cur"))
			Return lStado
		EndIf

		Select F2730Cur
		Scatter Name oAlb Blank
	Else
		Select F2730Cur
		Scatter Name oAlb
	EndIf

	Use In (Select ("F2730Cur"))

	*> Cargar los datos del propietario.
	m.F01pCodigo = oMAC.F24cCodPro
	=f3_seek("F01p")
	Select F01p
	Scatter Name oPro

	*> Cargar la descripción de la provincia del propietario.
	m.F00jCodPrv = oPro.F01pProvin
	=f3_seek("F00j")
	Select F00j
	Scatter Name oPrv

	*> Cargar los datos del transportista.
	m.F01tCodigo = oMAC.F24cCodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oTra

	*> Cargar los datos del articulo
	m.F01tCodigo = oMAC.F24cCodTra
	=f3_seek("F01t")
	Select F01t
	Scatter Name oTra

	m.F08cCodPro = oMAC.F26lCodPro
	m.F08cCodArt = oMAC.F26lCodArt
	=f3_seek("F08c")
	Select F08c
	Scatter Name oArt
	
	Select ETBUCURSOR
	Gather Name oEnv
	Gather Name oAlb
	Gather Name oPro
	Gather Name oPrv
	Gather Name oTra
	Gather Name oArt

	Replace Cantid     With oMAC.F26lCanFis
	Replace NumMac     With oMAC.F26lNumMac	
	Replace Portes     With Iif(oMAC.F24cPortes=='P', 'PAGADOS', 'DEBIDOS')
	Replace F24oDesObs With MLine(oObs.F24oDesObs, 1)

	Skip
EndDo


*> Solo dejo activo un registro para cada MAC / Documento.
cRegActual = ''

*> Solo dejo activo un registro para cada MAC / Documento.
Select ETBUCURSOR
Go Top
Do While !Eof()
	Scatter Name oMAC

	If Empty(cRegActual)
		cRegActual = oMac.F26lNumMac + oMac.F26lCodArt + oMAC.F26lCodPro + oMAC.F26lTipDoc + oMAC.F26lNumDoc
		nNumReg = RecNo()
		nItems = oMAC.Cantid
		Skip
		Loop
	EndIf

	If cRegActual==oMac.F26lNumMac + oMac.F26lCodArt + oMAC.F26lCodPro + oMAC.F26lTipDoc + oMAC.F26lNumDoc
		*> Es el mismo MAC / Documento: Acumular Items y borrar para eliminar duplicados.
		nItems = nItems + oMAC.Cantid
		Delete Next 1
		Skip
		Loop
	EndIf

	*> Cambio de documento: Actualizar acumulados Items y continuar.
	nCurReg = RecNo()
	Go nNumReg
	Replace Cantid With nItems
	Go nCurReg
	nItems = oMAC.Cantid
	nNumReg = nCurReg
	cRegActual = oMac.F26lNumMac + oMac.F26lCodArt + oMAC.F26lCodPro + oMAC.F26lTipDoc + oMAC.F26lNumDoc

	Skip
EndDo

*> Actualizar el último registro.
Go nNumReg
Replace Cantid With nItems
Go Top


Select ETBUCURSOR
Go Top
lStado = !Eof()

Return lStado

ENDPROC
PROCEDURE Init

=DoDefault()

*> Crear las propiedades de uso interno.
With This
	*> Rango ubicaciones, desglosado (Si This.RNModo==H).
	.AddProperty("cAlmI", "")			&& Almacén inicial.
	.AddProperty("cAlmF", "")			&& Almacén final.
	.AddProperty("cZonI", "")			&& Zona inicial.
	.AddProperty("cZonF", "")			&& Zona final.
	.AddProperty("cCalI", "")			&& Calle inicial.
	.AddProperty("cCalF", "")			&& Calle final.
	.AddProperty("cFilI", "")			&& Fila inicial.
	.AddProperty("cFilF", "")			&& Fila final.
	.AddProperty("cPisI", "")			&& Piso inicial.
	.AddProperty("cPisF", "")			&& Piso final.
	.AddProperty("cPrfI", "")			&& Profundidad inicial.
	.AddProperty("cPrfF", "")			&& Profundidad final.

	*> Propiedades para impresión de etiquetas de ubicación.
	.AddProperty("cLineaEtiqueta", "")	&& Buffer de impresión.

	*> Control de ficheros abiertos.
	.AddProperty("lF08cIsOpen", .F.)	&& Artículos.
	.AddProperty("lF00jIsOpen", .F.)	&& Provincias.
	.AddProperty("lPrinterIsOpen", .F.)	&& Dispositivo de salida.
EndWith

ENDPROC
PROCEDURE Destroy

*> Restablecer entorno de trabajo.
If !This.lF08cIsOpen
	Use In (Select ("F08c"))
EndIf

If !This.lF00jIsOpen
	Use In (Select ("F00j"))
EndIf

This._printerclose

=DoDefault()

ENDPROC


EndDefine 
