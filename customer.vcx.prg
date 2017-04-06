Define Class orafncetiq as custom
Height = 16
Width = 100



PROCEDURE etiqexped
*>
*> Impresión de etiquetas de expedición.

Private f_Where, c_CodBarTrp, c_CodBarTxt, c_Flag1
Local n, Sw

*> Cargar datos.------------------------------------------------------
Do Case
   *> Datos a partir de MPs.
   Case This.PEOrig = 'MP'
      This.MPDatos
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Datos a partir de ocupaciones.
   Case This.PEOrig = 'OC'
      This.OCDatos
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Datos a partir de bultos.
   Case This.PEOrig = 'BU'
      This.BUDatos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'DA'
EndCase

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
*   This.PDImpr = GetPrinter()
EndIf

*> La primera vez debe cargar, si hay, el logo de la etiqueta.
If !This.PbFLogoFlag
   This.EtiqExpedDefG
   This.PbFLogoFlag = .T.
EndIf

For n = 1 To This.PdNEti
	? " ~JSN^XA"
	? " ^COY,0^MMT^MD+0"
	? " ^XZ"

	? " ^XA"

	? " ^PRE^FS"
	? " ^PF0^FS"
	? " ^FO53,445^GB816,0,2,B^FS"
	? " ^FO754,445^GB0,294,2,B^FS"
	? " ^FO751,591^GB118,0,2,B^FS"
	? " ^FT89,121^AUN,59,53^FDDESTINATARIO^FS"
	? " ^FT72,511^ADN,28,20^FDTRANSPORTISTA^FS"
	? " ^FT780,527^ATN,48,42^FDBultos:^FS"
	? " ^FT990,527^ASN,40,35^FDde^FS"

	? " ^FO51,52^GB818,0,10,B^FS"
	? " ^FO51,62^GB0,672,10,B^FS"
	? " ^FO51,734^GB818,0,10,B^FS"

	? " ^BY3,3.0^FS"
	? " ^FT803,705^BEN,85,N,N^FD" + This.PbNMac + "^FS"
	? " ^FO812,685^GB57,0,42,W^FS"
	? " ^FT784,697^AFN,26,13^FD0^FS"
	? " ^FT657,105^ARN,35,31^FDPEDIDO:^FS"
	? " ^FT72,193^ADN,42,50^FD" + EtiqF24t.F24tNomAso + "^FS"
	? " ^FT787,103^APN,40,54^FD" + EtiqF26v.F26vNumDoc + "^FS"
	? " ^FT72,254^APN,40,54^FD" + EtiqF24t.F24t1erDir + "^FS"
	? " ^FT72,338^APN,40,54^FD" + EtiqF24t.F24tCodPos + "^FS"
	? " ^FT332,341^APN,40,54^FD" + EtiqF24t.F24tDPobla + "^FS"
	? " ^FT72,412^AQN,56,72^FD" + EtiqF24t.F24tDProvi + "^FS"
	? " ^FT72,575^AAN,36,40^FD" + F01t.F01tDescri + "^FS"
	? " ^FO53,624^GB700,0,2,B^FS"
	? " ^FT914,528^APN,40,54^FD" + Transform(Val(EtiqF26v.F26vNumBul), "999") + "^FS"
	? " ^FT1047,528^APN,40,54^FD" + Transform(EtiqF24c.F24cNumBul, "999") + "^FS"

	? " ^PQ1,0,1,Y"
	? " ^XZ"
Next n

*> Restaurar el entorno de impresión.
*Set Printer To
*Set Printer Off
*Set Console On
*Set Device To Screen

*> Según propiedades, actualizamos el estado del bulto.---------------
If This.PDActz = "S"
   c_Flag1 = This.PDFlg1
   f_Where = "F26vNumMac='" + This.PDNMac + "'"
   Sw = F3_UpdTun('F26v', , 'F26vFlag1', 'c_Flag1', , f_Where, 'N', 'N')
   If Sw = .F.
      _LxErr = 'No se ha podido actualizar el estado del MAC' + cr
      =Anomalias()
      _LxErr = ''
   EndIf
EndIf

Use In EtiqF24t
Use In EtiqF01p
Use In EtiqF08c

*>

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

*!*	Set Printer To Default
*!*	Set Printer Off
*!*	Set Console On
*!*	Set Device To Screen

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

*> Datos de impresión etiqueta.---------------------------------------
This.PDCPro = Space(6)
This.PDTDoc = Space(4)
This.PDNDoc = Space(13)
This.PDCArt = Space(13)
This.PDDArt = Space(25)
This.PDNLot = Space(15)
This.PDFCad = CToD(Space(8))
This.PDCant = 0
This.PDCUbi = Space(14)
This.PDCCli = Space(6)
This.PDNAlb = Space(10)
This.PDFAlb = CToD(Space(8))
This.PDTBul = Space(2)
This.PDTMac = Space(4)
This.PDNBul = 0
This.PDTotB = 0
This.PDNMac = Space(9)
This.PDNEti = 1
This.PDFlg1 = "0"
This.PDActz = "N"
This.PDCtrl = Space(1)
This.PDSize = "1"
This.PDObsv = Space(40)

*> Datos bulto.-------------------------------------------------------
This.PBMvMp = 0
This.PBNBul = 0
This.PBNMac = Space(9)
This.PBTOri = Space(1)
This.PBDart = Space(33)
This.PBFlgA = .F.

This.PBFLogo    = Space(1)
**** This.PBFLogoFlag= .F.

*>

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

Private f_Where, _Sentencia
Local Sw, _Ok

*> Cursor de ocupaciones.---------------------------------------------
=CrtCursor('F16c', 'EtiqF16c')
f_Where = "F16cCodPro = '" + This.POCPro + "' And F16cCodArt = '" + ;
    This.POCArt + "' And F16cNumLot = '" + This.PONLot + "' And F16cFecCad = " + ;
    GetCvtDate(_ENTORNO, _VERSION, This.POFCad) + " And F16cSitStk = '" + ;
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
This.PdNalb = EtiqF16c.F16cNotEnt                           && Entrada
This.PDCPro = EtiqF16c.F16cCodPro                           && Propietario
This.PDCArt = EtiqF16c.F16cCodArt                           && Artículo
This.PDNLot = EtiqF16c.F16cNumLot                           && Nº de lote
This.PDNPal = EtiqF16c.F16cNumPal                           && Nº de palet
This.PDFCad = EtiqF16c.F16cFecCad                           && Caducidad
This.PDCant = EtiqF16c.F16cCanFis                           && Cantidad
This.PDUCaj = EtiqF16c.F16cUniPac * EtiqF16c.F16cPacCaj     && Unidades caja
This.PDNCaj = Floor(EtiqF16c.F16cCanFis / This.PDUCaj)      && Nº de cajas
This.PDCUbi = EtiqF16c.F16cCodUbi                           && Ubicación

*> Obtener el peso de tabla F16t.
_Sentencia = " Select F16tPesEnt From F16t" + _em + " Where F16tNumPal ='" + This.PdNPal  +"'"
_Ok = f3_SqlExec(_Asql,_Sentencia,'pesopes')
This.PdTotb = pesopes.F16tPesEnt

*>
Use In EtiqF16c

ENDPROC
PROCEDURE budatos
*>
*> A partir de PUTMac, PUNMac, PUNBul y PUMvMP, cargar datos.-------
Private f_Where

*> Cargar información del contenido del bulto, a partir de lista de trabajo.
=CrtCursor('F26l', 'EtiqF26l')
f_Where = "F26lNumMac='" + This.PbNMac + "'"
Sw = F3_Sql('*', 'F26l', f_Where, , , 'EtiqF26l')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF26l
   Return
EndIf

*> Asignar datos a propiedades.
Select EtiqF26l
Go Top
This.PDCPro = EtiqF26l.F26lCodPro
This.PDCArt = EtiqF26l.F26lCodArt
This.PDNLot = EtiqF26l.F26lNumLot
This.PDFCad = EtiqF26l.F26lFecCad
This.PDCant = EtiqF26l.F26lCanFis
This.PDCUbi = EtiqF26l.F26lUbiOri
This.PDTMac = EtiqF26l.F26lTipMac
This.PDNMac = EtiqF26l.F26lNumMac
This.PDNLst = EtiqF26l.F26lNumLst

If Type('This.PDNBul')=='C'
   This.PDNBul = Val(This.PBNBul)
EndIf

*> Cargar datos del MAC, cabecera.
=CrtCursor('F26v', 'EtiqF26v')
f_Where = "F26vNumMac='" + This.PDNMac + "'"
Sw = F3_Sql('*', 'F26v', f_Where, , , 'EtiqF26v')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF26v
   Return
EndIf

*> Cargar información del Documento de Salida.
=CrtCursor('F24c', 'EtiqF24c')
f_Where = "F24cCodPro = '" + EtiqF26l.F26lCodPro + "' And " + ;
          "F24cTipDoc = '" + EtiqF26l.F26lTipDoc + "' And " + ;
          "F24cNumDoc = '" + EtiqF26l.F26lNumDoc + "'"

Sw = F3_Sql('*', 'F24c', f_Where, , , 'EtiqF24c')
If Sw = .T.
   This.PDCCli = EtiqF24c.F24cDirAso
   This.PDTotB = EtiqF24c.F24cNumBul
EndIf
This.PDTDoc = EtiqF24c.F24cTipDoc
This.PDNDoc = EtiqF24c.F24cNumDoc

*> Cargar datos de la ficha del cliente.
=CrtCursor('F24t','EtiqF24t')
f_Where = "F24tCodPro='" + This.PDCPro + "' And " + ;
          "F24tTipDoc='" + This.PDTDoc + "' And " + ;
          "F24tNumDoc='" + This.PDNDoc + "'"

Sw = F3_Sql('*', 'F24t', f_Where, , , 'EtiqF24t')
If Sw = .F.
   Select EtiqF24t
   Append Blank
EndIf

*> Cargar línea de observaciones.
This.PdObsv = Space(60)

m.F24oCodPro = This.PDCPro
m.F24oTipDoc = This.PDTDoc
m.F24oNumDoc = This.PDNDoc
m.F24oLinObs = '0000'
If f3_seek('F24O')
   Select F24o
   Go Top
   If SubStr(F24oImpObs, 1, 1) == 'S'
      This.PdObsv = Left(F24oDesObs, 60)
   EndIf
EndIf

*> Cargar datos de albaranes, cabecera.
=CrtCursor('F27c', 'EtiqF27c')
f_Where = "F27cCodPro='" + EtiqF24c.F24cCodPro + "' And " + ;
          "F27cTipDoc='" + EtiqF24c.F24cTipDoc + "' And " + ;
          "F27cNumDoc='" + EtiqF24c.F24cNumDoc + "'"

Sw = F3_Sql('*', 'F27c', f_Where, , , 'EtiqF27c')

*> Cargar datos de albaranes, detalle.
*> OJO!! línea documento es CHAR en F27l y NUM en F26l.
=CrtCursor('F27l', 'EtiqF27l')
f_Where = "F27lCodPro='" + EtiqF26l.F26lCodPro + "' And " + ;
          "F27lTipDoc='" + EtiqF26l.F26lTipDoc + "' And " + ;
          "F27lNumDoc='" + EtiqF26l.F26lNumDoc + "' And " + ;
          "F27lLinDoc='"  + PadL(AllTrim(Str(EtiqF26l.F26lLinDoc, 4, 0)), 4, '0') + "'"

Sw = F3_Sql('*', 'F27l', f_Where, , , 'EtiqF27l')

*> Tomar Nº y fecha de albarán y transportista al que pertenece el bulto.
Select EtiqF27l
Go Top
If !Eof()
   Select EtiqF27c
   Locate For F27cCodPro = EtiqF27l.F27lCodPro .And. ;
              F27cTipDoc = EtiqF27l.F27lTipDoc .And. ;
              F27cNumDoc = EtiqF27l.F27lNumDoc .And. ;
              F27cNumAlb = EtiqF27l.F27lNumAlb

   If Found()
      This.PDNAlb = EtiqF27c.F27cNumAlb
      This.PDFAlb = EtiqF27c.F27cFecAlb
   EndIf
EndIf

*> Cargar datos de la ficha del propietario.
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo = '" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf

*> Cargar datos de la ficha del artículo.
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
EndIf

*> Cargar datos del transportista.
m.F01tCodigo = EtiqF24c.F24cCodTra
If !f3_seek('F01T')
   Select F01t
   Append Blank
EndIf

*> Tipo de bulto.
Do Case
   Case This.PBTOri = "P"
      This.PDTBul = "PA"
   Case This.PBTOri = "C"
      This.PDTBul = "CC"
   Case This.PBTOri = "U"
      This.PDTBul = "PK"
   Case This.PBTOri = "G"
      This.PDTBul = "GR"
EndCase

*> Activar flag 'Contiene Albarán'.
This.PBNBul = Val(EtiqF26v.F26vNumBul)
This.PBFlgA = This.IsAlb(EtiqF26v.F26vNumMac)

*> Descripción de artículo (caja completa) ó texto 'Picking' (caja fracciones).
If This.PBTOri=='U'
   This.PBDArt = "   *** P I C K I N G ***     * " + This.PDTMac + " * "

Else
   =CrtCursor('F26w', 'EtiqF08c')
   f_Where = "F26lNumMac='" + This.PDNMac + "'"
   If !f3_sql('*', 'F26l', f_Where, , , 'EtiqF26l')
      Select EtiqF26l
      Append Blank
   EndIf

   =CrtCursor('F08c', 'EtiqF08c')
   f_Where = "F08cCodPro='" + EtiqF26l.F26lCodPro + "' And F08cCodArt='" + EtiqF26l.F26lCodArt + "'"
   If !f3_sql('*', 'F08c', f_Where, , , 'EtiqF08c')
      Select EtiqF08c
      Append Blank
   EndIf

   This.PBDArt = Left(EtiqF08c.F08cCodArt, 8) + Space(1) + Left(EtiqF08c.F08cDescri, 25)
EndIf

*>
If Used('IsAlbCursor')
   Use In IsAlbCursor
EndIf

*>

ENDPROC
PROCEDURE etiqubicpic
*>
*> Impresión de etiquetas de ubicación (Ubicaciones de picking).
Private f_Where, _UbicShort

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

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

*> Calcular el dígito de control
If Empty(This.PDCtrl)
   This.UbicDigit
EndIf

*> Compactar el código de ubicación.
_UbicShort = SubStr(This.PDCubi, 5, 2) + Space(1) + ;
             SubStr(This.PDCubi, 7, 2) + Space(1) + ;
             SubStr(This.PDCubi, 9, 3) + Space(1) + ;
             SubStr(This.PDCubi,12, 2)

For n = 1 To This.PdNEti
   Do Case
      *> Tamaño de etiqueta.
      Case This.PDSize = "1"
*         ? "^XA"
*         ? "^BY3,2:1,4"
*         ? "^CFD,100,100"
*         ? "^FO0075,0125^BCR,0120,Y,N,N^FD" + This.PdCUbi + "^FS"    && Ubicación - codbar.
*         ? "^FO0050,0750^A0R,0150,0075^FD" + _UbicShort + "^FS"      && Ubicación - texto.
*         ? "^FO0050,1175^A0R,0050,0030^FD" + This.PODArt + "^FS"     && Descripción.
*         ? "^FO0150,1175^A0R,0050,0030^FD" + This.POCart + "^FS"     && Artículo.
*         ? "^XZ"

			*>Nova etiqueta + petita
			? "^XA"
     		? "^BY3,4:1,4"
			? "^CFD,100,1002"
			? "^FO0050,0125^BCR,0150,Y,N,N^FD" + This.PdCUbi + "^FS"
			? "^FO0210,0125^A0R,0100,0100^FD" + _UbicShort + "^FS"
			? "^FO0320,0125^A0R,0100,050^FD" + This.PODArt + "^FS"
			? "^FO0430,0125^A0R,0100,0100^FD" + This.POCart + "^FS"
			? "^XZ"
      *> Resto de casos, error.
      Otherwise
   EndCase
Next n

*>

ENDPROC
PROCEDURE etiqubicpal
*>
*> Impresión de etiquetas de ubicación (Ubicaciones de palet completo).
*> Eliminar recuadro, por problemas de lectura scáner. AVC - 06.09.2000

Private f_Where, _UbicShort

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

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
This.OpenPrinter
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

*> Si no se recibe, calcular el dígito de control.
If Empty(This.PDCtrl)
   This.UbicDigit
EndIf

*> Compactar el código de ubicación.
_UbicShort = SubStr(This.PDCubi, 8, 1) + "." + ;
             SubStr(This.PDCubi,10, 2) + "." + ;
             SubStr(This.PDCubi,13, 1)

For n = 1 To This.PdNEti
   Do Case
  *> Tamaño de etiqueta.
  Case This.PDSize = "1"
	  *>Etiquetas abtiguas(Pequeñas)
      ? "^XA"
	  ? "^FO0040,0050^A0N,0150,0100^FD" + _UbicShort + "^FS"
	  ? "^BY2,2:1,4"
	  ? "^CFD,100,100"
 	  ? "^FO0360,0050^BCN,120,Y,N,N^FD91" + This.PdCUbi + "^FS"
	  ? "^XZ"

      *> Resto de casos, error.
      Otherwise
   EndCase
Next n

*>

ENDPROC
PROCEDURE mpdatosent
*>
*> A partir de PMNMov, cargar los datos necesarios.-----------------
Private f_Where, _Sentencia
Local Sw, _Ok

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
This.PdNalb = EtiqF14c.F14cNumEnt                           && N Entrada
This.PDCPro = EtiqF14c.F14cCodPro                           && Propietario
This.PDCArt = EtiqF14c.F14cCodArt                           && Artículo
This.PDNLot = EtiqF14c.F14cNumLot                           && Nº de lote
This.PDNPal = EtiqF14c.F14cNumPal                           && Nº de palet
This.PDFCad = EtiqF14c.F14cFecCad                           && Caducidad
This.PDCant = EtiqF14c.F14cCanFis                           && Cantidad
This.PDUCaj = EtiqF14c.F14cUniPac * EtiqF14c.F14cPacCaj     && Unidades caja
This.PDNCaj = Floor(EtiqF14c.F14cCanFis / This.PDUCaj)      && Nº de cajas
This.PDCUbi = EtiqF14c.F14cUbiOri                           && Ubicación
This.PDNMac = EtiqF14c.F14cNumMac                           && Nº de MAC

*> Buscamos en el f16t
_Sentencia = " Select F16tPesEnt From F16t" + _em + " Where F16tNumPal ='" + This.PdNPal  +"'"
_Ok = f3_SqlExec(_Asql,_Sentencia,'pesopes')

This.PdTotb = pesopes.F16tPesEnt

*> Buscamos Descri DEl Artículo.----------------------------
_Sentencia = " Select F08cDescri From F08c" + _em + " Where F08cCodPro='" + This.PDCPro  +"'" + ;
			 " And F08cCodArt = '" + This.PDCArt + "'"

_Ok = f3_SqlExec(_Asql,_Sentencia,'DesArt')

This.PDdArt = DesArt.F08cDescri

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

*> Añadir fecha de caducidad (MM/YYYY). AVC - 23.06.2000
*> Añadir resto en palets incompletos. AVC - 07.09.2000

*> Desglosar impresión en función de F08cTipEti. AVC - 13.09.2000
*>   - F08cTipEti=='S' (Etiqueta por ocupación) ------> EtiqPaletOcu.
*>   - F08cTipEti=='C' (Etiqueta por caja)      ------> EtiqPaletCaj.
*>   - F08cTipEti=='U' (Etiqueta por caja)      ------> EtiqPaletUni.
*>   - F08cTipEti=='N' (No imprime etiqueta).
*>   - Por defecto, etiqueta por ocupación.

Private f_Where

*> Cargar datos.------------------------------------------------------
Do Case

   *> Cargar datos de MPs.
   Case This.PEOrig = 'MP'
      This.MPDatosEnt
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Cargar datos de ocupación.
   Case This.PEOrig = 'OC'
      This.OCDatos
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Etiqueta palet para inventario inicial.-------------------
   Case This.PEOrig = 'DA'

EndCase

*> Si no se recibe, solicitar la impresora.
*If Empty(This.PDImpr)
 This.OpenPrinter
*EndIf

*> Obtener datos de la ficha del artículo.
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
EndIf
This.PDDArt = Left(EtiqF08c.F08cDescri, 25)

*> Obtener datos del propietario.
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo='" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf
This.PDDPro = Left(EtiqF01p.F01pDescri, 20)

*> Obtener descripción de tipo de unidad.
=CrtCursor('F00h', 'EtiqF00h')
f_Where = "F00hCodUni='" + EtiqF08c.F08cTipUni + "'"
Sw = F3_Sql('*', 'F00h', f_Where, , , 'EtiqF00h')
If Sw = .F.
   Select EtiqF00h
   Append Blank
EndIf

Select EtiqF00h
Go Top

Select EtiqF08c
Do Case
   *> Imprimir etiqueta por OCUPACION.
   Case F08cTipEti=='S'
      If This.PeOrig = 'DA'
         This.EtiqPaletOcu
      Else
	     This.EtiqPaletOcp
	  EndIf

   *> Imprimir etiqueta por CAJA.
   Case F08cTipEti=='C'
      This.EtiqPaletCaj

   *> Imprimir etiqueta por CAJA,  cantidad en caja.
   Case F08cTipEti=='U'
      This.EtiqPaletUni

   *> No imprimir etiqueta.
   Case F08cTipEti=='N'

   *> Por defecto, imprimir etiqueta por ocupación.
   Otherwise

      *> Para imprimir diferentes tipos de etiqueta.---------------
      Do Case
         *> Etiqueta palet para inventario inicial.-------------------
         Case This.PEOrig = 'DA'
		     This.EtiqPaletOcu

         *> Etiqueta palet para entrada normal.-------------------------
         OtherWise
		     This.EtiqPaletOcp

      EndCase 	  	

EndCase

*>
If Used('EtiqF08c')
   Use In EtiqF08c
EndIf

If Used('EtiqF01p')
   Use In EtiqF01p
EndIf

If Used('EtiqF00h')
   Use In EtiqF00h
EndIf

ENDPROC
PROCEDURE ubicdigit
*>
*> Calcular el dígito de control de una ubicación.
*> Toma la ubicación de This.PdCubi

This.PDCtrl = Ora_DigCtl(This.PDCUbi)
Return This.PDCtrl

ENDPROC
PROCEDURE ubdatos
*>
*> Toma los datos de una ubicación.
Private f_Where

*> Cursor de ubicaciones.---------------------------------------------
=CrtCursor('F10c', 'EtiqF10c')
f_Where = "F10cCodUbi = '" + This.PDCubi + "'"
Sw = F3_Sql('*', 'F10c', f_Where, , , 'EtiqF10c')
If Sw = .F.
   This.PWCRtn = "99"
   Use In EtiqF10c
   Return
EndIf

Select EtiqF10c
Go Top
This.PDCUbi = F10cCodUbi

*>
Use In EtiqF10c

ENDPROC
PROCEDURE etiqubicdigit
*>
*> Impresión de etiquetas de dígito de control.
Private f_Where

*> Cargar datos.------------------------------------------------------
Do Case
   *> Tomar datos de la ubicación.
   Case This.PEOrig = 'UB'
      This.UBDatos             && Carga datos Ubicaciones.
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Resto de casos, error.
   Otherwise
EndCase

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

*> Si no se recibe, calcular el dígito de control.
If Empty(This.PDCtrl)
   This.UbicDigit
EndIf

For n = 1 To This.PdNEti
   Do Case
      *> Tipo de tamaño 1.
      Case This.PDSize = "1"
         ? "^XA"
         ? "^FO0045,0070^GB0500,0800,4^FS"                       && Recuadro.

         && Si letra 'W' hay que hacerla mas pequeña.
         If This.PDCtrl='W'
            ? "^FO0150,0300^A0N,0400,0400^FD" + This.PDCtrl + "^FS"
         Else
            ? "^FO0150,0300^A0N,0500,0500^FD" + This.PDCtrl + "^FS"
         EndIf

         ? "^FO0090,0750^A0N,0075,0060^FD" + Left(This.PDCubi, 13) + "^FS"   && Ubicación.
         ? "^XZ"

      *> Otros tamaños de etiqueta, no definidos.
      Otherwise
   EndCase
Next n

*>

ENDPROC
PROCEDURE etiqexpedold
*>
*> Impresión de etiquetas de expedición.
Private f_Where

*> Cargar datos.------------------------------------------------------
Do Case
   *> Datos a partir de MPs.
   Case This.PEOrig = 'MP'
      This.MPDatos
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Datos a partir de ocupaciones.
   Case This.PEOrig = 'OC'
      This.OCDatos
      If This.PWCRtn > "50"
         Return
      EndIf

   *> Datos a partir de bultos.
   Case This.PEOrig = 'BU'
      This.BUDatos
      If This.PWCRtn > "50"
         Return
      EndIf
   Case This.PEOrig = 'DA'
EndCase

*> Datos de la ficha del cliente.
=CrtCursor('F24t','EtiqF24t')
f_Where = "F24tCodPro = '" + This.PDCPro + "' And F24tTipDoc = '" + This.PDTDoc + "' And " + ;
     "F24tNumDoc = '" + This.PDNDoc + "'"

Sw = F3_Sql('*', 'F24t', f_Where, , , 'EtiqF24t')
If Sw = .F.
   Select EtiqF24t
   Append Blank
EndIf

*> Datos de la ficha del propietario.
=CrtCursor('F01p', 'EtiqF01p')
f_Where = "F01pCodigo = '" + This.PDCPro + "'"
Sw = F3_Sql('*', 'F01p', f_Where, , , 'EtiqF01p')
If Sw = .F.
   Select EtiqF01p
   Append Blank
EndIf

*> Datos de la ficha del artículo.
=CrtCursor('F08c', 'EtiqF08c')
f_Where = "F08cCodPro = '" + This.PDCPro + "' And F08cCodArt = '" + This.PDCArt + "'"
Sw = F3_Sql('*', 'F08c', f_Where, , , 'EtiqF08c')
If Sw = .F.
   Select EtiqF08c
   Append Blank
EndIf

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

*> Configurar el entorno de impresión.
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
   ? "^FO580,750^A0N40,25^FWR^FDJANSSEN-CILAG^FS"   && Datos Almacén
   ? "^FO550,750^A0N40,25^FWR^FDC/. Río Jarama^FS"
   ? "^FO520,750^A0N40,25^FWR^FD08740    JANSSEN - TOLEDO^FS"
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
Next n

*> Restaurar el entorno de impresión.
Set Printer To Default
Set Printer Off
Set Console On
Set Device To Screen

*> Según propiedades, actualizamos el estado del bulto.---------------
If This.PDActz = "S"
   c_Flag1 = This.PDFlg1
   f_Where = "F26vNumMac = '" + This.PDNMac + "'"
   Sw = F3_UpdTun('F26v', , 'F26vFlag1', 'c_Flag1', , f_Where, 'N', 'N')
   If Sw = .F.
      _LxErr = 'No se ha podido actualizar el estado del MAC' + cr
      =Anomalias()
   EndIf
EndIf

Use In EtiqF24t
Use In EtiqF01p
Use In EtiqF08c

*>

ENDPROC
PROCEDURE etiqnumpal
*>
*> Impresión de etiquetas de nº de Palet.

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

For n = 1 To This.PdNEti
   Do Case
      *> Tipo de tamaño 1.
      Case This.PDSize = "1"
         ? "^XA"                                                    && Inicio etiqueta.

         ? "^FO0045,0070^GB0500,0800,4^FS"                          && Recuadro.
         ? "^FO0315,0070^GB000,800,4^FS"

         ? "^FO0350,0100^A0R,0120,0070^FDN. PALET:^FS"
         ? "^FO0350,0420^A0R,0120,0070^FD" + This.PDNPal + "^FS"    && Nº Palet (texto).
         ? "^FO0120,0225^BCR,160,Y,N,N^FD" + This.PDNPal + "^FS"    && Nº Palet (codbar).

         ? "^XZ"                                                    && Fin etiqueta.

      *> Otros tamaños de etiqueta, no definidos.
      Otherwise
   EndCase
Next n

*>

ENDPROC
PROCEDURE etiqnummac
*>
*> Impresión de etiquetas de nº de MAC.

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
*   This.PDImpr = GetPrinter()
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

*> Configurar el entorno de impresión.
*Set Printer To Name (This.PDImpr)
*Set Console Off
*Set Printer On
*Set Device To Printer

For n = 1 To This.PdNEti
	Do Case
		*> Tipo de tamaño 1.
		Case This.PDSize = "1"
			? " ~JSN^XA"
			? " ^COY,0^MMT^MD+0"
			? " ^XZ"

			? " ^XA"
			? " ^PRE^FS"
			? " ^PF0^FS"

			? " ^BY5,2.5^FS"
			? " ^FT586,83^BCI,150,N,N,Y^FD>;20" + This.PdNMac + "^FS"
			? " ^FT438,54^AAI,27,20^FD20" + This.PdNMac + "^FS"
			? " ^FT547,308^AVI,80,71^FDMAC:^FS"
			? " ^FT332,305^AVI,80,71^FD" + This.PdNMac + "^FS"

			? " ^PQ1,0,1,Y"
			? " ^XZ"

		*> Otros tamaños de etiqueta, no definidos.
		Otherwise
	EndCase
Next n

ENDPROC
PROCEDURE etiqexpedcab
*>
*> Impresión de etiquetas de expedición, cambio de ubicación - solo cajas.

*> Si no se recibe, obtener el nombre del dispositivo de salida.
If Empty(This.PDImpr)
   This.OpenPrinter
*   This.PDImpr = GetPrinter()
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

*> Configurar el entorno de impresora.
*Set Printer To Name (This.PDImpr)
*Set Console Off
*Set Printer On
*Set Device To Printer

For n = 1 To This.PdNEti
   Do Case
      *> Tipo de tamaño 1.
      Case This.PDSize = "1"
         ? "^XA"                                                    && Inicio etiqueta.

         ? "^FO0030,0050^GB1150,1150,5^FS"                          && Recuadro.
         ? "^FO300,125^A0R,0400,0150^FD" + SubStr(This.PDCUbi, 5) + "^FS"      && Ubicación.

         ? "^XZ"                                                    && Fin etiqueta.

      *> Otros tamaños de etiqueta, no definidos.
      Otherwise
   EndCase
Next n

*> Restaurar el entorno de impresora.
*Set Printer To
*Set Printer Off
*Set Console On
*Set Device To Screen

*>

ENDPROC
PROCEDURE isalb
*>
*> Averiguar si un bulto actual debe contener el texto 'CONTIENE ALBARAN'.
Parameters NumBul
Private _CodPro, _TipDoc, _NumDoc, _TipAlb

If !Used('IsAlbCursor')

   *> Averiguar el documento del bulto a chequear.
   f_campos = "*"
   f_where  = "F26vNumMac=?NumBul"
   If !f3_sql(f_campos, 'F26v', f_where, , , 'IsAlbCursor')
      Return .F.
   EndIf

   Select IsAlbCursor
   Go Top
   If Eof()
      Return .F.
   EndIf

   _CodPro = F26vCodPro
   _TipDoc = F26vTipDoc
   _NumDoc = F26vNumDoc
   _TipAlb = F26vFlag2

   Use In IsAlbCursor

   *> Cargar los bultos del documento al que pertenece el bulto a chequear.
   f_where  = "F26vCodPro=?_CodPro And F26vTipDoc=?_TipDoc And F26vNumDoc=?_NumDoc"
   If !f3_sql('*', 'F26v', f_where, , , 'IsAlbCursor')
      Return .F.
   EndIf
EndIf

Select IsAlbCursor
Do Case
   *> Estupefacientes.
   Case _TipAlb=='E'
      Locate For F26vFlag2=='E' .And. F26vNumMac < NumBul
      Return !Found()

   *> Resto.
   Otherwise
      Locate For F26vFlag2 # 'E' .And. F26vNumMac < NumBul
      Return !Found()
EndCase

*>

ENDPROC
PROCEDURE etiqexpeddefg
*>
*> Definir el logo para imprimir en la etiqueta de expedición.
*> La impresora de etiquetas ya debe estar abierta.

Private Nd, FicheroLogo, Cadena

If Empty(This.PbFLogo)
   Return                   && No se ha definido ningún logo.
EndIf

Cadena = Space(1)
FicheroLogo = This.PbFLogo
Nd = FOpen(FicheroLogo, 0)

Do While Nd > 0 .And. !FEof(Nd)
   Cadena = FGetS(Nd)
   If !Empty(Cadena)
      ? AllTrim(Cadena) + cr
   EndIf
EndDo

=FClose(Nd)

*>

ENDPROC
PROCEDURE etiqpaletocu
*>
*> Impresión de etiquetas de Palet.
*> Imprimir una etiqueta por ocupación (F08cTipEti=='S').

Private c_CodBar1,c_CodBar2,c_CodBar3, n, cDate

*> c_CodBar1 contendra el Nº de Palet
*> c_CodBar2 contendra el Codigo de Propietario, Nº de Artículo, Cantidad
*> c_CodBar3 contendra el Nº de Lote y la Fecha

cDate = Iif(Empty(This.PdFCad), Space(10), DToC(This.PdFCad))

c_CodBar1 = This.PdNPal
c_CodBar2 = This.PdCPro + This.PdCart + Transform(This.PdCant,'99999')
c_CodBar3 = Left(This.PdNlot, 15) + ;
            SubStr(Dtoc(This.PdFCad), 1, 2) + ;
            SubStr(Dtoc(This.PdFCad), 4, 2) + ;
            SubStr(Dtoc(This.PdFCad), 7, 4)

*> La primera vez debe cargar, si hay, el logo de la etiqueta.
If !This.PbFLogoFlag
   This.EtiqExpedDefG
   This.PbFLogoFlag = .T.
EndIf

For n = 1 To This.PdNEti

	Wait '' TimeOut 2

*!*	 ? "^XA"
*!*	 ? "^FO0000,0010^GB0830,1100,5^FS"
*!*	 ? "^FO0760,0400^A0R,0050,0030^FDF. Entrada: " + DToC(This.PdFAlb) + "^FS"
*!*	 ? "^FO0760,0750^A0R,0050,0030^FDN. Entrada: " + This.PdNAlb + "^FS"
*!*	 ? "^FO0690,0400^A0R,0060,0050^FD" + This.PdCPro + "^FS"
*!*	 ? "^FO0610,0075^A0R,0050,0030^FDArticulo: " + This.PdCArt + "^FS"
*!*	 ? "^FO0610,0400^A0R,0050,0030^FD" + This.PdDArt + "^FS"
*!*	 ? "^FO0560,0075^A0R,0050,0030^FDCantidad: " + Transform(This.PdCant, '9999') + "^FS"
*!*	 ? "^FO0560,0400^A0R,0050,0030^FD" + EtiqF00h.F00hAbrevi + "^FS"
*!*	 ? "^FO0525,0700^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
*!*	 ? "^FO0475,0700^A0R,0050,0030^FDN. Lote: " + This.PdNLot + "^FS"
*!*	 ? "^FO0425,0700^A0R,0050,0030^FDCaducidad: " + cDate + "^FS"
*!*	 If This.PdTotb > 0
*!*		 ? "^FO0375,0700^A0R,0050,0030^FDPeso: " + Transform(This.PdTotb,'9999.999') + "^FS"
*!*	 EndIf
*!*	 ? "^CFD,100,100"
*!*	 ? "^BY4,2:1,2"
*!*	 ? "^FO0430,0075^BCR,125,Y,N,N^FD" + c_CodBar1 + "^FS"
*!*	 ? "^BY3,2:1,2"
*!*	 ? "^FO0250,0075^BCR,125,Y,N,N^FD" + c_CodBar2 + "^FS"
*!*	 ? "^FO0070,0075^BCR,125,Y,N,N^FD" + c_CodBar3 + "^FS"
*!*	 ? "^PQ1"
*!*	 ? "^XZ"
   ? "^XA"

   ? "^FO0000,0010^GB0830,1100,5^FS"

   ? "^FO0760,0400^A0R,0050,0030^FDF. Entrada: " + DToC(This.PdFAlb) + "^FS"

*  ? "^FO0760,0750^A0R,0050,0030^FDN. Entrada: " + This.PdNAlb + "^FS"

   ? "^FO0690,0400^A0R,0060,0050^FD" + This.PdCPro + "^FS"

   ? "^FO0610,0075^A0R,0050,0030^FDArticulo: " + This.PdCArt + "^FS"
   ? "^FO0610,0400^A0R,0050,0030^FD" + This.PdDArt + "^FS"

   ? "^FO0560,0075^A0R,0050,0030^FDCantidad: " + Transform(This.PdCant, '99999') + "^FS"

*  ? "^FO0560,0400^A0R,0050,0030^FD" + EtiqF00h.F00hAbrevi + "^FS"

   ? "^FO0525,0700^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
   ? "^FO0475,0700^A0R,0050,0030^FDN. Lote: " + This.PdNLot + "^FS"
   ? "^FO0425,0700^A0R,0050,0030^FDCaducidad: " + cDate + "^FS"

   ? "^FO0675,0100"
   ? "^XGALGEVA,1,1^FS"

   ? "^BY4,2:1,2"
   ? "^CFD,100,100"

   ? "^BY4,2:1,2"
   ? "^FO0430,0075^BCR,125,Y,N,N^FD" + c_CodBar1 + "^FS"
   ? "^BY3,2:1,2"
   ? "^FO0250,0075^BCR,125,Y,N,N^FD" + c_CodBar2 + "^FS"
   ? "^BY3,2:1,2"
   ? "^FO0070,0075^BCR,125,Y,N,N^FD" + c_CodBar3 + "^FS"

   ? "^XZ"

Next n

*>

ENDPROC
PROCEDURE etiqpaletcaj
*>
*> Impresión de etiquetas de Palet.
*> Imprimir una etiqueta por caja, cantidad=cantidad ocupación (F08cTipEti=='C').

Private c_CodBar1,c_CodBar2,c_CodBar3, n

*> c_CodBar1 contendra el Nº de Palet
*> c_CodBar2 contendra el Codigo de Propietario, Nº de Artículo, Cantidad
*> c_CodBar3 contendra el Nº de Lote y la Fecha

c_CodBar1 = This.PdNPal
c_CodBar2 = This.PdCPro + Left(This.PdCart, 8) + Transform(This.PdCant,'99999')
c_CodBar3 = Left(This.PdNlot, 10) + dtoc(This.PdFcad)

For n = 1 To This.PdNEti

   ? "^XA"

   ? "^FO0000,0010^GB0830,1100,5^FS"

   ? "^FO0760,0400^A0R,0050,0030^FDF. Entrada:^FS"
   ? "^FO0760,0750^A0R,0050,0030^FDN. Entrada:^FS"

   ? "^FO0690,0400^A0R,0060,0050^FD" + This.PdDPro + "^FS"

   ? "^FO0610,0050^A0R,0050,0030^FDArticulo: " + This.PdCArt + "^FS"
   ? "^FO0610,0400^A0R,0050,0030^FD" + This.PdDArt + "^FS"

   ? "^FO0560,0050^A0R,0050,0030^FDCantidad: " + Transform(This.PdCant, '99999') + "^FS"

   ? "^FO0525,0700^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
   ? "^FO0475,0700^A0R,0050,0030^FDN. Lote: " + This.PdNLot + "^FS"
   ? "^FO0425,0700^A0R,0050,0030^FDCaducidad: " + Dtoc(This.PdFCad) + "^FS"

   ? "^BY4,2:1,2"
   ? "^CFD,100,100"

   ? "^FO0430,0050^BCR,125,Y,N,N^FD" + c_CodBar1 + "^FS"
   ? "^FO0250,0050^BCR,125,Y,N,N^FD" + c_CodBar2 + "^FS"
   ? "^FO0070,0050^BCR,125,Y,N,N^FD" + c_CodBar3 + "^FS"

   ? "^XZ"

Next n

*>

ENDPROC
PROCEDURE etiqpaletuni
*>
*> Impresión de etiquetas de Palet.
*> Imprimir una etiqueta por caja, cantidad=cantidad caja. (F08cTipEti=='U').

Private c_CodBar1,c_CodBar2,c_CodBar3, n

*> c_CodBar1 contendra el Nº de Palet
*> c_CodBar2 contendra el Codigo de Propietario, Nº de Artículo, Cantidad
*> c_CodBar3 contendra el Nº de Lote y la Fecha

c_CodBar1 = This.PdNPal
c_CodBar2 = This.PdCPro + Left(This.PdCart, 8) + Transform(This.PdCant,'99999')
c_CodBar3 = Left(This.PdNlot, 10) + dtoc(This.PdFcad)

For n = 1 To This.PdNEti

   ? "^XA"

   ? "^FO0000,0010^GB0830,1100,5^FS"

   ? "^FO0760,0400^A0R,0050,0030^FDF. Entrada:^FS"
   ? "^FO0760,0750^A0R,0050,0030^FDN. Entrada:^FS"

   ? "^FO0690,0400^A0R,0060,0050^FD" + This.PdDPro + "^FS"

   ? "^FO0610,0050^A0R,0050,0030^FDArtículo: " + This.PdCArt + "^FS"
   ? "^FO0610,0400^A0R,0050,0030^FD" + This.PdDArt + "^FS"

   ? "^FO0560,0050^A0R,0050,0030^FDCantidad: " + Transform(This.PdCant, '99999') + "^FS"

   ? "^FO0525,0700^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
   ? "^FO0475,0700^A0R,0050,0030^FDN. Lote: " + This.PdNLot + "^FS"
   ? "^FO0425,0700^A0R,0050,0030^FDCaducidad: " + Dtoc(This.PdFCad) + "^FS"

   ? "^BY4,2:1,2"
   ? "^CFD,100,100"

   ? "^FO0430,0050^BCR,125,Y,N,N^FD" + c_CodBar1 + "^FS"
   ? "^FO0250,0050^BCR,125,Y,N,N^FD" + c_CodBar2 + "^FS"
   ? "^FO0070,0050^BCR,125,Y,N,N^FD" + c_CodBar3 + "^FS"

   ? "^XZ"

Next n

ENDPROC
PROCEDURE openprinter
*>
*> Abrir la impresora de etiquetas.
If Empty(This.PDImpr)
   This.PDImpr = GetPrinter()
EndIf

*> Configurar el entorno de impresión.
Set Printer To Name (This.PDImpr)
Set Console Off
Set Printer On
Set Device To Printer

*> Limpiar el búffer de la impresora.
*******? "~JA"

*>

ENDPROC
PROCEDURE closeprinter
*>
*> Cerrar la impresora de etiquetas.
Set Console On
Set Device To Screen
Set Printer Off
Set Printer To Default

*>

ENDPROC
PROCEDURE etiqnumpal4
*>
*> Impresión de etiquetas de nº de Palet. (Múltiple).

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf

Do Case
   *> Inicio etiqueta + etiqueta 1.
   Case This.PdNEti <= 1
      ? "^XA"

      ? "^BY4,2:1,4"
      ? "^CFD,100,100"

      ? "^FO0980,0150^BCR,170,Y,N,N^FD" + This.PdNPal + "^FS"

   *> Etiqueta 2.
   Case This.PdNEti = 2
      ? "^FO0680,0150^BCR,170,Y,N,N^FD" + This.PdNPal + "^FS"

   *> Etiqueta 3.
   Case This.PdNEti = 3
      ? "^FO0380,0150^BCR,170,Y,N,N^FD" + This.PdNPal + "^FS"

   *> Etiqueta 4 + fin etiqueta.
   Case This.PdNEti = 4
      ? "^FO0080,0150^BCR,170,Y,N,N^FD" + This.PdNPal + "^FS"
      ? "^XZ"

   *> Cerrar etiqueta.
   Case This.PdNEti = 99
      ? "^XZ"

   *> Resto de casos: Error.
   Otherwise
EndCase

*>

ENDPROC
PROCEDURE etiqubicfila
*>
*> Impresión de etiquetas de fila (Ubicaciones de palet completo).
*> Eliminar recuadro, por problemas de lectura scáner. mik - 06.09.2000

Private f_Where, _UbicShort

*> Cargar datos.------------------------------------------------------
*> Por ahora solo se toma de ubicaciones (F10c).----------------------
*Do Case
*   Case This.PEOrig = 'UB'
*      This.UBDatos             && Carga datos Ubicaciones.
*      If This.PWCRtn > "50"
*         Return
*      EndIf
*   Otherwise
*EndCase

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
EndIf

*> Si se cancela.-------------
If Empty(This.PDImpr)
   Return
EndIf


*> Compactar el código de ubicación.
_UbicShort = SubStr(This.PDCubi, 5, 2) + Space(1) + ;
             SubStr(This.PDCubi, 7, 2) + Space(1) + ;
             SubStr(This.PDCubi, 9, 3) + Space(1)

_FIla= "Fila " + SubStr(This.PDCubi, 9, 3) +  + Space(1)

For n = 1 To This.PdNEti
   Do Case
      *> Tamaño de etiqueta.
      Case This.PDSize = "1"
*         ? "^XA"
*         ? "^FO625,250^A0R,200,130^FD" + _UbicShort + "^FS"
*         ? "^FO625,250^A0N,200,130^FD" + _FIla + "^FS"
**********? "^FO0045,0070^GB500,800,4^FS"
**********? "^FO0315,0070^GB000,800,4^FS"
*
*         ? "^BY3,2:1,4"
*         ? "^CFD,100,100"
*         ? "^FO0400,0300^BCR,160,Y,N,N^FD" + _UbicShort + "^FS"
*         ? "^XZ"


*>Etiqetas de fila provinentes de Bestlux...
*? "^XA"
*? "^DFPROVA3^FS"
*? "^PRC"
*? "^LH0,0^FS"
*? "^LL839"
*? "^MD0"
*? "^MNY"
*? "^LH0,0^FS"
*? "^BY4,3.0^FO179,200^BCN,189,Y,N,N^FR^FD>:"+ _UbicShort + "^FS"
*? "^FO46,100^A0B,79,93^CI13^FR^FD"+ _Fila + "^FS"
*? "^FO200,90^A0N,87,104^CI13^FR^FD"+ _UbicShort +"^FS"
*? "^XZ"
*? "^FX End of job"
*? "^XA"
*? "^XFPROVA3"
*? "^XZ"


**>Etiquetas de Fila nuevas.
*? "^XA"
*? "^DFPROVA3^FS"
*? "^PRC"
*? "^LH0,0^FS"
*? "^LL839"
*? "^MD02"
*? "^MNY"
*? "^LH0,0^FS"
*? "^BY4,3.0^FO179,400^BCN,500,Y,N,N^FR^FD>:"+ _UbicShort + "^FS"
*? "^FO46,400^A0B,100,180^CI13^FR^FD"+ _Fila + "^FS"
*? "^FO90,90^A0N,300,180^CI13^FR^FD"+ _UbicShort +"^FS"
*? "^XZ"
*? "^FX End of job"
*? "^XA"
*? "^XFPROVA3"
*? "^XZ"

*>Etiquetas nuevas mas pequeñas
? "^XA"
? "^PRC"
? "^LH0,0^FS"
? "^LL1441"
? "^MD0"
? "^MNY"
? "^LH0,0^FS"
? "^BY5,3.0^FO61,200^BCR,321,Y,N,N^FR^FD>:"+ _UbicShort + "^FS"
? "^FO60,47^A0N,125,100^CI13^FR^FD"+ _Fila + "^FS"
? "^FO385,200^A0R,150,175^CI13^FR^FD"+ _UbicShort + "^FS"
? "^XZ"




      *> Resto de casos, error.
      Otherwise
   EndCase
Next n

*>

ENDPROC
PROCEDURE etiqpaletocp
*>
*> Impresión de etiquetas de Palet.
*> Imprimir una etiqueta por ocupación (F08cTipEti=='S').

Private c_CodBar1, c_CodBar2, c_CodBar3, cDate, cDates, _Sel, cPesos
Local Sw, n

*> c_CodBar1 contendra el Nº de Palet
*> c_CodBar2 contendra el Codigo de Propietario, Nº de Artículo, Cantidad
*> c_CodBar3 contendra el Nº de Lote y la Fecha
If Controlar=.F.
	_Sel = "Select " + GetCvtNvl(_ENTORNO, _VERSION, "F16tPesEnt") + ;
	       " As F16tPesPal From F16t" + _em + ;
	       " Where F16tNumPal='" +  This.PdNPal + "'"

	Sw = f3_SqlExec(_Asql,_Sel,'Etiqu')
	cPesos=Etiqu.F16tPesPal
    cPesos = This.PdTotb
	cPesos = IIF(Type('cPesos')='C',cPesos,Str(cPesos,3))
Else
	_Sel = "Select " + GetCvtNvl(_ENTORNO, _VERSION, "F16tPesEnt") + ;
	       " As F16tPesPal From F16t" + _em + ;
	       " Where F16tNumPal='" +  This.PdNPal + "'"

	Sw = f3_SqlExec(_Asql,_Sel,'Etiqu')
	cPesos=Etiqu.F16tPesPal
*   cPesos = This.PdTotb
*	cPesos = IIF(Type('cPesos')='C',cPesos,Str(cPesos,3))
EndIf

*cDates=Dtoc(_FecMin)
cDates = Space(10)
This.PdFCad = Iif(This.PdfCad = _FecMin,Space(10),This.PdfCad)
cDate = Iif(Empty(This.PdFCad), Space(10), DToC(This.PdFCad))

c_CodBar1 = This.PdNPal

*> La primera vez debe cargar, si hay, el logo de la etiqueta.
If !This.PbFLogoFlag
   This.EtiqExpedDefG
   This.PbFLogoFlag = .T.
EndIf

For n = 1 To This.PdNEti

	Wait '' TimeOut 2

 If Controlar=.F.
 	? "^XA"
 	? "^FO0000,0010^GB0830,1100,5^FS"
 	? "^FO0760,0400^A0R,0050,0030^FDF. Entrada: " + DToC(This.PdFAlb) + "^FS"
 	? "^FO0760,0750^A0R,0050,0030^FDN. Entrada: " + This.PdNAlb + "^FS"
 	? "^FO0690,0400^A0R,0060,0050^FD" + This.PdCPro + "^FS"
 	? "^FO0610,0075^A0R,0050,0030^FDArticulo: " + This.PdCArt + "^FS"
 	? "^FO0610,0400^A0R,0050,0030^FD" + This.PdDArt + "^FS"
 	? "^FO0560,0075^A0R,0050,0030^FDCantidad: " + Transform(This.PdCant, '999999') + "^FS"
* 	? "^FO0560,0400^A0R,0050,0030^FD" + EtiqF00h.F00hAbrevi + "^FS"
 	? "^FO0510,0075^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
 	? "^FO0460,0075^A0R,0050,0030^FDN. Lote: " + This.PdNLot + "^FS"
 	? "^FO0410,0075^A0R,0050,0030^FDCaducidad: " + cDate + "^FS"
* 	? "^FO0410,0575^A0R,0050,0030^FDPeso: " + Str(cPesos) + "^FS"
 	? "^FO0410,0575^A0R,0050,0030^FDPeso: " + cPesos + "^FS"
 	? "^CFD,200,100"
 	? "^BY4,10:5,5"
 	? "^FO0050,0075^BCR,300,Y,N,N^FD" + c_CodBar1 + "^FS"
 	? "^PQ1"
 	? "^XZ"
 Else
 	? "^XA"
 	? "^FO0000,0010^GB0830,1100,5^FS"
 	? "^FO0760,0400^A0R,0050,0030^FDF. Entrada: " + DToC(This.PdFAlb) + "^FS"
 	? "^FO0760,0750^A0R,0050,0030^FDN. Entrada: " + This.PdNAlb + "^FS"
 	? "^FO0690,0400^A0R,0060,0050^FD" + This.PdCPro + "^FS"
 	? "^FO0610,0075^A0R,0050,0030^FDArticulo: " + This.PdCArt + "^FS"
 	? "^FO0610,0400^A0R,0050,0030^FD" + This.PdDArt + "^FS"
 	? "^FO0560,0075^A0R,0050,0030^FDCantidad: " + Transform(This.PdCant, '999999') + "^FS"
 	? "^FO0510,0075^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
 	? "^FO0460,0075^A0R,0050,0030^FDN. Lote: " + This.PdNLot + "^FS"
 	? "^FO0410,0075^A0R,0050,0030^FDCaducidad: " + cDates+ "^FS"
 	? "^FO0410,0575^A0R,0050,0030^FDPeso: " + Transform(cPesos,'9999.999') + "^FS"
 	? "^CFD,200,100"
 	? "^BY4,10:5,5"
 	? "^FO0050,0075^BCR,300,Y,N,N^FD" + c_CodBar1 + "^FS"
 	? "^PQ1"
 	? "^XZ"
 EndIf
Next n


*>

ENDPROC
PROCEDURE etiqpalemulti
*>
*> Impresión de etiquetas de Palet.
*> Imprimir una etiqueta por ocupación (F08cTipEti=='S').

Private c_CodBar1,c_CodBar2,c_CodBar3, n, cDate

*> c_CodBar1 contendra el Nº de Palet
*> c_CodBar2 contendra el Codigo de Propietario, Nº de Artículo, Cantidad
*> c_CodBar3 contendra el Nº de Lote y la Fecha


*cDates=Dtoc(_FecMin)


c_CodBar1 = This.PdNPal

*> La primera vez debe cargar, si hay, el logo de la etiqueta.
If !This.PbFLogoFlag
   This.EtiqExpedDefG
   This.PbFLogoFlag = .T.
EndIf

For n = 1 To This.PdNEti
 	? "^XA"
 	? "^FO0000,0010^GB0830,1100,5^FS"
 	? "^FO0760,0400^A0R,0050,0030^FDF. Entrada: " + DToC(This.PdFAlb) + "^FS"
 	? "^FO0760,0750^A0R,0050,0030^FDN. Entrada: " + This.PdNAlb + "^FS"
 	? "^FO0610,0075^A0R,0050,0030^FDArticulo: Multireferencia^FS"
 	? "^FO0560,0075^A0R,0050,0030^FDCantidad: ^FS"
 	? "^FO0510,0075^A0R,0050,0030^FDN Palet: " + This.PdNPal + "^FS"
 	? "^FO0460,0075^A0R,0050,0030^FDN. Lote: ^FS"
 	? "^FO0410,0075^A0R,0050,0030^FDCaducidad: ^FS"
 	? "^FO0410,0575^A0R,0050,0030^FDPeso:  ^FS"
 	? "^CFD,200,100"
 	? "^BY4,10:5,5"
 	? "^FO0050,0075^BCR,300,Y,N,N^FD" + c_CodBar1 + "^FS"
 	? "^PQ1"
 	? "^XZ"
Next n

*>

ENDPROC
PROCEDURE etiqexbas
*>
*> Impresión de etiquetas de expedición.
*> Imprimir datos artículo (Caja completa) ó 'PICKING' (caja fracciones). AVC - 23.06.2000
*> Imprimir texto 'Contiene albarán'. Si hay bulto picking, en éste. Si no hay, en el último
*> bulto. AVC - 23.06.2000
*> Reducir font de la población. AVC - 06.09.2000
*> Dirección 1 pasa a ser nombre ampliado y dirección pasa a dirección 2. AVC - 25.09.2000
*> Modificar codificación CodBar. AVC - 28.09.2000
*> Generar un único fichero de etiquetas. AVC - 10.10.2000
*> Imprimir el nº documento y, si NO es fracciones, la cantidad. AVC - 10.10.2000
*> Origen, pasa a ser '0' + PP + '0'. AVC - 28.02.2001

Private f_Where, c_CodBarTrp, c_CodBarTxt, _Sentencia
Local _Ok, n

*> Si no se recibe, solicitar la impresora.
If Empty(This.PDImpr)
    This.OpenPrinter
EndIf

*> La primera vez debe cargar, si hay, el logo de la etiqueta.
*If !This.PbFLogoFlag
*   This.EtiqExpedDefG
*   This.PbFLogoFlag = .T.
*EndIf

*> Recuperamos datos.-------------------------------------------
_Sentencia = " Select * " + ;
			 " From F24o" + _em + ;
			 " Where F24oCodPro = '" + This.PocPro + ;
			 "' And F24oTipDoc = '" + This.PdTDoc + ;
			 "' And F24oNumDoc = '" + This.PdNDoc + ;
             "' And F24oLinObs='0000'"

_Ok = f3_SqlExec(_Asql,_Sentencia,'Obs')
If _Ok < 1
   _LxErr = "Error consultando observaciones" + cr + ;
            "MENSAJE: " + Message() + cr + ;
            "SENTENCIA: " + _Sentencia + cr
	=Anomalias()
	Return .F.	
EndIf

*> Recuperamos datos.-------------------------------------------
_Sentencia = " Select F24tNomAso,F24t1ErDir,F24t2nDDir,F24tDPobla,F24tDprovi,F01TDescri " + ;
			 " From F24c" + _em + ", F24t" + _em + ", F01t" + _em + ;
			 " Where F24cCodPro = '" + This.PocPro + ;
			 "' And F24cTipDoc = '" + This.PdTDoc + ;
			 "' And F24cNumDoc = '" + This.PdNDoc + ;
			 "' And F01tCodigo = F24cCodTra " + ;
			 " And F24tCodPro = F24cCodPro " + ;
			 " And F24tTipDoc = F24cTipDoc " + ;
			 " And F24tNumDoc = F24cNumDoc "

_Ok = f3_SqlExec(_Asql,_Sentencia,'Datos')
If _Ok < 1
   _LxErr = "Error cargando datos dirección" + cr + ;
            "MENSAJE: " + Message() + cr + ;
            "SENTENCIA: " + _Sentencia + cr
	=Anomalias()
	Return .F.	
EndIf

For n = 1 To This.PdNEti

	Wait '' TimeOut 2

	?"^XA"
	?"^PRC"
	?"^LH0,0^FS"
	?"^LL1119"
	?"^MD0"
	?"^MNY"
	?"^LH0,0^FS"
*	?"^FO675,100^FR^XGRUZPJalg,1,1^FS"
*	?"^FO675,100^FR^XG" + This.PbFLogo + ",1,1^FS"

   ? "^FO0675,0100"
   ? "^XGR:ALGEVAS.GRF,1,1^FS"
	?"^FO0750,0100^AIR,37,0^CI0^FR^FDA L G E V A S A  ^FS"
	?"^FO0700,0225^A0R,30,19^CI13^FR^FDL O G I S T I C S  ^FS"

	?"^CWI,510O01JW.FNT^FS"
	?"^FO600,100^AIR,37,0^CI0^FR^FD" + Obs.F24oDesObs +"^FS"
	?"^FO550,100^AIR,37,0^CI0^FR^FDENVIO A   :^FS"

	?"^FO550,500^AIR,37,0^CI0^FR^FDNo Doc : " + This.PdNDoc + "^FS"

	?"^FO548,100^GB0,275,4^FS"
	?"^FO548,500^GB0,200,4^FS"
	?"^CWI,510O01JW.FNT^FS"
	?"^FO400,100^AIR,37,0^CI0^FR^FDDIRECCION :^FS"
	?"^FO398,100^GB0,275,4^FS"
	?"^CWI,510O01JW.FNT^FS"
	?"^FO185,100^AIR,37,0^CI0^FR^FDPOBLACION :^FS"
	?"^FO183,100^GB0,275,4^FS"
	?"^CWI,510O01JW.FNT^FS"
	?"^FO050,100^AIR,37,0^CI0^FR^FDTRANSPORTISTA:^FS"
	?"^FO048,100^GB0,325,4^FS"
	?"^FO800,575^A0R,28,19^CI13^FR^FD" + Alltrim(emp_dir) + "^FS"
	?"^FO775,575^A0R,28,19^CI13^FR^FD" + Alltrim(emp_cp) + "^FS"
	?"^FO750,575^A0R,30,19^CI13^FR^FD" + Alltrim(emp_pro) + "^FS"
	?"^FO725,675^A0R,28,19^CI13^FR^FD" + Alltrim(emp_tel) + "^FS"
	?"^FO700,675^A0R,28,19^CI13^FR^FD" + Alltrim(emp_fax) + "^FS"
	?"^FO675,575^A0R,28,19^CI13^FR^FD" + Alltrim(emp_emai) + "^FS"
	?"^CWJ,Y00O01JW.FNT^FS"
	?"^FO475,100^AJR,34,0^CI0^FR^FD" + Alltrim(Datos.F24tNomAso) + "^FS"
	?"^CWJ,Y00O01JW.FNT^FS"
	?"^FO340,100^AJR,34,0^CI0^FR^FD" + Alltrim(Datos.F24t1ErDir) + "^FS"
	?"^CWJ,Y00O01JW.FNT^FS"
	?"^FO300,100^AJR,34,0^CI0^FR^FD" + Alltrim(Datos.F24t2nDDir) + "^FS"
	?"^CWJ,Y00O01JW.FNT^FS"
	?"^FO130,100^AJR,34,0^CI0^FR^FD" + Alltrim(Datos.F24TdPobla) + "^FS"
	?"^CWJ,Y00O01JW.FNT^FS"
	?"^FO260,100^AJR,34,0^CI0^FR^FD" +  Alltrim(Datos.F24tdProvi) + "^FS"
	?"^CWJ,Y00O01JW.FNT^FS"
	?"^FO050,460^AJR,34,0^CI0^FR^FD" + Alltrim(Datos.F01tDescri) + "^FS"
	?"^FO725,575^A0R,23,19^CI13^FR^FDTelf :^FS"
	?"^FO700,575^A0R,23,19^CI13^FR^FDFax :^FS"
	?"^FO665,64^GB168,1009,4^FS"
	?"^FO665,550^GB168,0,4^FS"
	?"^FO110,62^GB0,1011,4^FS"
	?"^FO655,62^GB0,1015,4^FS"
	?"^PQ1,0,0,N"
	?"^XZ"

Next n


*!*	   ? "^XA"
*!*	   ? "^FO0030,0050^GB1150,1150,5^FS"

*!*	   ? "^FO0550,0050^GB0000,1150,5^FS"
*!*	   ? "^FO0675,0050^GB0000,1150,5^FS"
*!*	   ? "^FO0900,0050^GB0000,1150,5^FS"
*!*	   ? "^FO0900,0400^GB0275,0000,5^FS"
*!*	   ? "^FO0030,0900^GB0520,0000,5^FS"

*!*	   ? "^FO0095,0050^GB0000,0850,5^FS"
*!*	   ? "^FO0030,0580^GB0070,0000,5^FS"

*!*	   ? "^FO1070,0425^A0R,0075,0045^FD" + AllTrim(Emp_NomC) + "^FS"
*!*	   ? "^FO1000,0425^A0R,0040,0030^FD" + AllTrim(Emp_Dir) + "^FS"
*!*	   ? "^FO0950,0425^A0R,0040,0025^FD" + AllTrim(Emp_Cp) + "-" + AllTrim(Emp_Pro) + "^FS"
*!*	   ? "^FO0950,0925^A0R,0040,0025^FDTlf. " + AllTrim(Emp_Tel) + "^FS"

*!*	   ? "^FO0927,0125"
*!*	   ? "^XGALGEVA,1,1^FS"

*!*	   ? "^FO0840,0075^A0R,0050,0050^FDDestinatario:^FS"
*!*	   ? "^FO0785,0075^A0R,0060,0040^FD" + Left(EtiqF24t.F24tNomAso, 40) + "^FS"
*!*	   ? "^FO0730,0075^A0R,0060,0040^FD" + Left(EtiqF24t.F24t2ndDir, 40) + "^FS"
*!*	   ? "^FO0675,0075^A0R,0060,0040^FD" + EtiqF24t.F24tCodPos + "-" + Left(EtiqF24t.F24tDPobla, 30) + "^FS"
*!*	   ? "^FO0675,0900^A0R,0060,0040^FD" + Left(EtiqF24t.F24tDProvi, 28) + "^FS"

*!*	   ? "^FO0840,0775^A0R,0050,0050^FDAlbaran:^FS"
*!*	   ? "^FO0835,0950^A0R,0060,0040^FD" + Right(AllTrim(This.PdNAlb), 8) + "^FS"

*!*	   ? "^FO0785,0775^A0R,0030,0030^FD" + AllTrim(EtiqF24t.F24tNumDoc) + Space(2) + ;
*!*	       Iif(This.PBTOri #'U', '(' + AllTrim(Transform(This.PdCant, '@Z 9999')) + ')' + ;
*!*	                             ' - Lote: ' + AllTrim(This.PDNLot), '') + "^FS"

*!*	   ? "^FO0625,0075^A0R,0040,0040^FDAgencia:^FS"
*!*	   ? "^FO0625,0225^A0R,0040,0050^FD" + Left(F01t.F01tDescri, 30) + "^FS"

*!*	   ? "^FO0560,0075^A0R,0040,0040^FDObserv.:^FS"
*!*	   ? "^FO0560,0225^A0R,0040,0030^FD" + This.PdObsv + "^FS"

*!*	   ? "^FO0625,0900^A0R,0040,0040^FDPeso:^FS"
*!*	   ? "^FO0625,1000^A0R,0040,0040^FD" + Transform(EtiqF24c.F24cTotKgs, "9999.99") + "^FS"

*!*	   ? "^FO0450,0260^A0R,0070,0040^FD" + Left(EtiqF24t.F24tDPobla, 30) + "^FS"
*!*	   ? "^FO0385,0100^A0R,0130,0065^FD" + Left(EtiqF24t.F24tCodPos, 2) + "^FS"
*!*	   ? "^FO0405,0175^A0R,0075,0050^FD" + SubStr(EtiqF24t.F24tCodPos, 3, 3) + "^FS"

*!*	   *> Formar el código de barras del transportista.
*!*	   *> DDPPPPPOOOXXXXXXXXxxRYBBB/BBB (Antes).
*!*	   *> DDPPPPPOOOEXXXXXxxRYBBB/BBB (28.09.2000) (Antes).
*!*	   *> DDPPPPPOOOOEXXXXXxxRYBBB/BBB (28.02.2001), en donde:

*!*	   *>    D ---> Clave país destino España='00').
*!*	   *>    P ---> Código postal.
*!*	   *>    O ---> Clave plaza origen(='0' + clave provincia + '0').
*!*	   *>    E ---> Id. empresa. Siempre '2'.
*!*	   *>    X ---> Nº de albarán.
*!*	   *>    R ---> Nº correlativo, '0':Pedido venta. '1':Pedido Publicidad.
*!*	   *>    Y ---> Ultimo dígito del año en curso.
*!*	   *>    B ---> Bulto relativo s/total.
*!*	   c_CodBarTrp = "00" + ;
*!*	                 EtiqF24t.F24tCodPos + ;
*!*	                 "0" + SubStr(Emp_Cp, 1, 2) + "0" +  ;
*!*	                 "2" + ;
*!*	                 SubStr(AllTrim(This.PdNAlb), 6, 5) + ;
*!*	                 "0" + ;
*!*	                 SubStr(DToC(EtiqF24c.F24cFecDoc, 1), 4, 1) + ;
*!*	                 PadL(AllTrim(Str(Val(EtiqF26v.F26vNumBul), 4, 0)), 4, '0')

*!*	   *> Formar string cabecera del código de barras.
*!*	   *> Contenido:
*!*	   *> Nº expedición + Nº correlativo + última cifra año + bulto relativo + plaza origen.
*!*	   c_CodBarTxt = Space(2) + "2" +  ;
*!*	                 SubStr(AllTrim(This.PdNAlb), 6, 5) + Space(2) + ;
*!*	                 "0" + Space(3) + ;
*!*	                 SubStr(DToC(EtiqF24c.F24cFecDoc, 1), 4, 1) + Space(2) + ;
*!*	                 Str(Val(EtiqF26v.F26vNumBul), 3, 0) + "/" + ;
*!*	                 AllTrim(Str(EtiqF24c.F24cNumBul, 3, 0)) + Space(1) + ;
*!*	                 SubStr(Emp_Cp, 1, 2)

*!*	   ? "^FO0405,0275^A0R,0045,0045^FD" + c_CodBarTxt + "^FS"

*!*	   ? "^BY4,2:1,4"
*!*	   ? "^CFD,100,100"
*!*	   ? "^FO0160,0110^B2R,250,Y,N,N^FD" + c_CodBarTrp + "^FS"

*!*	   ? "^FO0390,1100^A0I,0060,040^FDN. MAC^FS"
*!*	   ? "^FO0090,1100^A0I,0060,060^FD" + This.PdNMac + "^FS"

*!*	   ? "^BY3,2:1,3"
*!*	   ? "^CFD,100,100"
*!*	   ? "^FO0100,0950^BCI,150,Y,N,N^FD" + This.PdNMac + "^FS"

*!*	   ? "^FO0042,0065^A0R,0045,0030^FD" + This.PBDArt + "^FS"
*!*	   ? "^FO0042,0596^A0R,0045,0035^FD" + ;
*!*	      Iif(This.PBFlgA==.T., "CONTIENE ALBARAN", " ") + "^FS"

*!*	   ? "^FO0042,0950^A0R,0030,0025^FDLISTA: " + This.PdNLst + "^FS"

*!*	   ? "^XZ"
*!*	Next n

*!*	*> Restaurar el entorno de impresión.
*!*	*Set Printer To
*!*	*Set Printer Off
*!*	*Set Console On
*!*	*Set Device To Screen

*!*	*> Según propiedades, actualizamos el estado del bulto.---------------
*!*	If This.PDActz = "S"
*!*	   c_Flag1 = This.PDFlg1
*!*	   f_Where = "F26vNumMac='" + This.PDNMac + "'"
*!*	   Sw = F3_UpdTun('F26v', , 'F26vFlag1', 'c_Flag1', , f_Where, 'N', 'N')
*!*	   If Sw = .F.
*!*	      _LxErr = 'No se ha podido actualizar el estado del MAC' + cr
*!*	      Do Form St3Inc
*!*	      _LxErr = ''
*!*	   EndIf
*!*	EndIf

*!*	Use In EtiqF24t
*!*	Use In EtiqF01p
*!*	Use In EtiqF08c

*>

ENDPROC


EndDefine 
Define Class customer as custom



PROCEDURE unidadeshueco
Parameters cTamHab, cCodPro, cCodArt, cCodSec

UsrConect = 'S'
This.UsrError = ''

*> --------------------------------------------------------------------------
*> Si no se pasa como parametro, obtengo la seccion de la referencia. -------
If Type('cCodSec') = 'L' Or Empty(cCodSec) Then
   If used('F08cCur')
      Use in F08cCur
   EndIf

   f_select = ''
   f_from   = ''
   f_where  = ''
   f_group  = ''
   f_orden  = ''

   f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
   f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F08c" + _em
   f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cCodPro ='" + cCodPro + "'"
   f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cCodArt ='" + cCodArt + "'"
   f_group  = ""

   lx_sql = f_select + f_from + f_where + f_group + f_orden

   Err = f3_SqlExec(_ASql, lx_sql, 'F08cCur')
   If Err <= 0
      If Used('F08cCur')
         Use in F08cCur
      EndIf

      UsrConect = 'C'
      This.UsrError = 'Error accediendo a BD'
      Return 0
   EndIf

   Select F08cCur
   Go Top
   If Eof()
      If used('F08cCur')
         Use in F08cCur
      EndIf

      UsrConect = 'N'
      This.UsrError = 'Referencia no encontrada'
      Return 0
   EndIf

   cCodSec = F08cCur.F08cSeccio
   Use In F08cCur
EndIf

*> --------------------------------------------------------------------------

*> --------------------------------------------------------------------------
*> Obtengo la cantidad máxima segun Seccion / Tamaño
If used('F08nCur')
   Use in F08nCur
EndIf

f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F08n" + _em
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08nCodSec ='" + cCodSec + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08nCodTam ='" + cTamHab + "'"
f_group  = ""

lx_sql = f_select + f_from + f_where + f_group + f_orden

Err = f3_SqlExec(_ASql, lx_sql, 'F08nCur')
If Err <= 0
   If used('F08nCur')
      Use in F08nCur
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return 0
EndIf

Select F08nCur
Go Top
If Eof()
   If used('F08nCur')
      Use in F08nCur
   EndIf

   UsrConect = 'N'
   This.UsrError = 'Sección/Tamaño no encontrado'
   Return 0
EndIf
*> --------------------------------------------------------------------------

*> --------------------------------------------------------------------------
*> Asignar cantidad maxima.
_TotHueco  = F08nCur.F08nCanMax

If used('F08nCur')
   Use in F08nCur
EndIf

*> --------------------------------------------------------------------------
*> Incremento en un 5 % el total de unidades que caben en el hueco y obtengo la parte entera.
*> _MargenOcupacion definido en MSI2.INI
*_TotHueco = Int(_TotHueco + (_TotHueco * _MargenOcupacion/100))

*> --------------------------------------------------------------------------

Return _TotHueco

ENDPROC
PROCEDURE unidadesocupadas
Parameters cCodUbi, cCodPro, cCodArt

Private nTotF16c, nTotF14c

Store 0 To nTotF16c, nTotF14c

UsrConect = 'S'
This.UsrError = ''

*> --------------------------------------------------------------------
*> Calculo el total ocupado en F16c. ----------------------------------
If used('F16cCur')
   Use in F16cCur
EndIf

*> Validar que existe y el estado es correcto.
f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "Sum(F16cCanFis) as F16cCanFis"
f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F16c" + _em
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F16cCodUbi ='" + cCodUbi + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F16cCodPro ='" + cCodPro + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F16cCodArt ='" + cCodArt + "'"
f_group  = " Group by F16cCodUbi, F16cCodPro, F16cCodArt"

lx_sql = f_select + f_from + f_where + f_group + f_orden
Err = f3_SqlExec(_ASql, lx_sql, 'F16cCur')
If Err <= 0
   If used('F16cCur')
      Use in F16cCur
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return UsrConect
EndIf

Select F16cCur
Go Top
If !Eof()
   nTotF16c = F16cCur.F16cCanFis
EndIf

If used('F16cCur')
   Use in F16cCur
EndIf
*> --------------------------------------------------------------------

*> --------------------------------------------------------------------
*> Calculo el total ocupado en F14c
If used('F14cCur2')
   Use in F14cCur2
EndIf

*> Validar que existe y el estado es correcto.
f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "Sum(F14cCanFis) as F14cCanFis"
f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F14c" + _em
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cUbiOri ='" + cCodUbi + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cCodPro ='" + cCodPro + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cCodArt ='" + cCodArt + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cTipMov like '1%'"
f_group  = " Group by F14cUbiOri, F14cCodPro, F14cCodArt"

lx_sql = f_select + f_from + f_where + f_group + f_orden
Err = f3_SqlExec(_ASql, lx_sql, 'F14cCur2')
If Err <= 0
   If used('F14cCur2')
      Use in F14cCur2
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return UsrConect
EndIf

Select F14cCur2
Go Top
If !Eof()
   nTotF14c = F14cCur2.F14cCanFis
EndIf

If used('F14cCur2')
   Use in F14cCur2
EndIf
*> --------------------------------------------------------------------

Return nTotF16c + nTotF14c

ENDPROC
PROCEDURE ubi_asignar_picking
*> El parametro cCerCur (CerrarCursor = 'S' o 'N') indica si se debe cerrar el cursor
*>    cArtPicking al salir del metodo. Si no se pasa el parametro, se asume 'S'. Puede
*>    interesar dejarlo abierto para poder imprimir los cambios de asignacion desde el
*>    programa que ha llamado al metodo.

Parameters cCodPro, cCodArt, cCerCur

Private cCodMar, cDescr1, cCodCol, cUbiPicking
Private PesUbi, PesMay, PesMen, c_Ubica, c_Tamaño

If Type("cCerCur") <> "C" Then
   cCerCur = "S"
EndIf

UsrConect = 'S'
This.UsrError = ''

*> --------------------------------------------------------------------------
*> Accedo a la ficha de referencia para recuperar la descripcion y color.
If used('F08cCur')
   Use in F08cCur
EndIf

*> Validar que existe y el estado es correcto.
f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F08c" + _em
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cCodPro ='" + cCodPro + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cCodArt ='" + cCodArt + "'"
f_group  = ""

lx_sql = f_select + f_from + f_where + f_group + f_orden

Err = f3_SqlExec(_ASql, lx_sql, 'F08cCur')
If Err <= 0
   If used('F08cCur')
      Use in F08cCur
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return UsrConect
EndIf

cCodMar = Space(8)    && Marca
cDescr1 = Space(15)   && Modelo
cCodCol = Space(10)   && Color

Select F08cCur
Go Top
If Eof()
   If used('F08cCur')
      Use in F08cCur
   EndIf

   UsrConect = 'N'
   This.UsrError = 'Referencia no encontrada'
   Return UsrConect
Else
   cCodMar = F08cCur.F08cCodMar
   cDescr1 = F08cCur.F08cDescr1
   cCodCol = F08cCur.F08cCodCol
EndIf
*> --------------------------------------------------------------------------

*> --------------------------------------------------------------------------
*> Accedo a todas las referencias del mismo modelo y color.
If used('F08cCur')
   Use in F08cCur
EndIf

*> Validar que existe y el estado es correcto.
f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F08c" + _em
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cCodMar ='" + cCodMar + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cDescr1 ='" + cDescr1 + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F08cCodCol ='" + cCodCol + "'"
f_group  = ""

lx_sql = f_select + f_from + f_where + f_group + f_orden

Err = f3_SqlExec(_ASql, lx_sql, 'F08cCur')
If Err <= 0
   If used('F08cCur')
      Use in F08cCur
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return UsrConect
EndIf

Select F08cCur
Go Top
If Eof()
   If used('F08cCur')
      Use in F08cCur
   EndIf

   UsrConect = 'N'
   This.UsrError = 'Referencia no encontrada'
   Return UsrConect
EndIf
*> --------------------------------------------------------------------------

*> --------------------------------------------------------------------------
*> Validar si las referencias tienen asignado picking. ----------------------
If used('cArtPicking')
   Use in cArtPicking
EndIf

Create Cursor cArtPicking (CodPro C(6),   ;
                           CodArt C(13),  ;
                           CodSec C(4),   ;
                           CodAlm C(4),   ;
                           Priori N(5,0), ;
                           CodUbi C(14),  ;
                           CanMin N(9,0), ;
                           CanMax N(9,0), ;
                           ChgAsg C(1))
*                           Largo  N(6,2), ;
*                           Ancho  N(6,2), ;
*                           Alto   N(6,2), ;

*> Vaciar ubicacion inicial de busqueda
cUbiPicking = Space(14)

Select F08cCur
Go Top
Do While !Eof()

   *> -----------------------------------------------------------------------
   *> Asignacion de ubicaciones de picking. ---------------------------------
   If used('F12cCur')
      Use in F12cCur
   EndIf

   *> Validar que existe y el estado es correcto.
   f_select = ''
   f_from   = ''
   f_where  = ''
   f_group  = ''
   f_orden  = ''

   f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
   f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F12c" + _em
   f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F12cCodPro ='" + F08cCur.F08cCodPro + "'"
   f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F12cCodArt ='" + F08cCur.F08cCodArt + "'"
   f_group  = ""

   lx_sql = f_select + f_from + f_where + f_group + f_orden

   Err = f3_SqlExec(_ASql, lx_sql, 'F12cCur')
   If Err <= 0
      If used('F12cCur')
         Use in F12cCur
      EndIf
      If Used('cArtPicking') And cCerCur = "S"
         Use In cArtPicking
      EndIf

      UsrConect = 'C'
      This.UsrError = 'Error accediendo a BD'
      Return UsrConect
   EndIf

   Select cArtPicking
   Append blank
   Replace CodPro With F08cCur.F08cCodPro
   Replace CodArt With F08cCur.F08cCodArt
   Replace CodSec With F08cCur.F08cSeccio
   Replace ChgAsg With "N"
*   Replace Largo  With F08cCur.F08cMatLar
*   Replace Ancho  With F08cCur.F08cMatAnc
*   Replace Alto   With F08cCur.F08cMatAlt

   Select F12cCur
   Go Top
   If Eof()
      Replace cArtPicking.CodUbi With Space(14)
   Else
      *> Si esta vacia la ubicacion  inicial de busqueda, la asignamos
      If Empty(cUbiPicking)
         cUbiPicking = F12cCur.F12cCodUbi
      EndIf

      Replace cArtPicking.CodUbi With F12cCur.F12cCodUbi
   EndIf
   *> -----------------------------------------------------------------------

   Select F08cCur
   Skip
EndDo
*> --------------------------------------------------------------------------

Set Procedure To Ora_Cu00 Additive

*> --------------------------------------------------------------------------
*> Asignar ubicacion de picking. --------------------------------------------
Select cArtPicking
Go Top
Do While !Eof()

   If Empty(cArtPicking.CodUbi)

      *> --------------------------------------------------------------------
      *> Buscar ubicaciones no asignadas. -----------------------------------
      If used('F10cCur')
         Use in F10cCur
      EndIf

      *> Validar que existe y el estado es correcto.
      f_select = ''
      f_from   = ''
      f_where  = ''
      f_group  = ''
      f_orden  = ''

      f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
      f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F10c" + _em
      f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cPickSN ='S'"
      *f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cFlag1  ='A'"
      *> No cargar ubicaciones ya asignadas
      f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cCodUbi not in "
      f_where  = f_where + "(select f12ccodubi from f12c" + _em + " where f12cCodUbi=f10cCodUbi)"
      *> Cargar solo ubicaciones cuyo tamaño este definido en F08N para la seccion del articulo
      f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cTamUbi In "
      f_where  = f_where + "(Select F08nCodTam From F08n" + _em + " Where F08nCodSec='" + cArtPicking.CodSec + "')"

      f_group  = ""

      f_mayor  = "And F10cCodUbi >'" + cUbiPicking + "'"
      f_ordmay  = 'Order by F10cPriori, F10cCodUbi'
      f_menor  = "And F10cCodUbi <'" + cUbiPicking + "'"
      f_ordmen  = 'Order by F10cPriori, F10cCodUbi Desc'

      *> Ubicaciones posteriores.
      lx_sql = f_select + f_from + f_where + f_mayor + f_group + f_ordmay

      Err = f3_SqlExec(_ASql, lx_sql, 'F10cMay')
      If Err <= 0
         If used('F10cCur')
            Use in F10cCur
         EndIf
         If Used('cArtPicking') And cCerCur = "S"
            Use In cArtPicking
         EndIf

         UsrConect = 'C'
         This.UsrError = 'Error accediendo a BD'
         Return UsrConect
      EndIf

***> De momento, cargar solo hacia adelante
*      *> Ubicaciones previas.
*      lx_sql = f_select + f_from + f_where + f_menor + f_group + f_ordmen
*
*      Err = f3_SqlExec(_ASql, lx_sql, 'F10cMen')
*      If Err <= 0
*         If used('F10cCur')
*            Use in F10cCur
*         EndIf
*
*         UsrConect = 'C'
*         This.UsrError = 'Error accediendo a BD'
*         Return UsrConect
*      EndIf

      *> Obtengo el peso de la ubicacion immediatamente posterior disponible.
      Select F10cMay
      Go Top
      PesMay = IIf(Eof(), 0, PesoUbi(F10cMay.F10cCodUbi))

***> De momento, cargar solo hacia adelante
      *> Obtengo el peso de la ubicacion immediatamente anterior disponible.
*      Select F10cMen
*      Go Top
*      PesMen = IIf(Eof(), 0, PesoUbi(F10cMen.F10cCodUbi))
      PesMen = 0

      *> Determino la ubicacion mas proxima.
      If PesMay=0 and PesMen=0
         If Used('cArtPicking') And cCerCur = "S"
            Use In cArtPicking
         EndIf

         UsrConect = 'N'
         This.UsrError = 'No hay ubicaciones libres'
         Return UsrConect
      EndIf

      If (PesMay<>0 and PesMen=0) or (PesMay=0 and PesMen<>0)
         If PesMay=0  && F10cMay no tiene registros
            c_Ubica  = F10cMen.F10cCodUbi
            c_Tamaño = F10cMen.F10cTamUbi
         Else         && F10cMay no tiene registros
            c_Ubica  = F10cMay.F10cCodUbi
            c_Tamaño = F10cMay.F10cTamUbi
         EndIf
      Else
         *> Obtengo el peso de la ubicacion de la referencia.
         PesUbi = PesoUbi(cUbiPicking)

         PesMay=Abs(PesMay-PesUbi)
         PesMen=Abs(PesUbi-PesMen)

         If PesMen < PesMay
            c_Ubica  = F10cMen.F10cCodUbi
            c_Tamaño = F10cMen.F10cTamUbi
         Else
            c_Ubica  = F10cMay.F10cCodUbi
            c_Tamaño = F10cMay.F10cTamUbi
         Endif
      EndIf

      *> -----------------------------------------------------------------
      *> Asignamos la ubicacion a la referencia. -------------------------

      *> Calculamos el maximo de unidades.
      nCanMax = 1

      nCanMax = This.UnidadesHueco(c_Tamaño, cArtPicking.CodPro, cArtPicking.CodArt, cArtPicking.CodSec)

      If !Used('F12c')
         =This.oPROCAOT.OpenTabla('F12c')
      EndIf

      =CrtCurSor('F12c','F12cCur')

      Append Blank
      Replace F12cCodPro With cArtPicking.CodPro
      Replace F12cCodArt With cArtPicking.CodArt
      Replace F12cCodAlm With Left(c_Ubica, 4)
      Replace F12cPriori With 1
      Replace F12cCodUbi With c_Ubica
      Replace F12cCanMin With 0
      Replace F12cCanMax With IIf(nCanMax=0, 1, nCanMax)

      *> Insertamos registro.---------------------------------------------
      Sw = F3_InsTun('F12c', 'F12cCur', 'N')
      If Sw = .F.
         If Used('cArtPicking') And cCerCur = "S"
            Use In cArtPicking
         EndIf

         UsrConect = 'N'
         This.UsrError = 'Error insertando F12c'
         Return UsrConect	
      EndIf

      *> Marcar cArtPicking como asignacion cambiada
      Select cArtPicking
      Replace CodAlm With F12cCur.F12cCodAlm
      Replace Priori With F12cCur.F12cPriori
      Replace CodUbi With F12cCur.F12cCodUbi
      Replace CanMin With F12cCur.F12cCanMin
      Replace CanMax With F12cCur.F12cCanMax
      Replace ChgAsg With "S"

      *> -----------------------------------------------------------------
   EndIf

   Select cArtPicking
   Skip
EndDo
*> --------------------------------------------------------------------------

If Used('cArtPicking') And cCerCur = "S"
   Use In cArtPicking
EndIf

Release Procedure Ora_Cu00

Return UsrConect

ENDPROC
PROCEDURE ubi_ubicar
Parameters cNumEnt, cCodPro, cCodArt, cSitStk

Private cUbicacion
Private PesMay, PesMen, c_Ubica, c_Tamaño, cMenMay, cTipUbi, cTamHab

UsrConect = 'S'
This.UsrError = ''

*> ---------------------------------------------------------
*> Recupero los valores de la ficha del articulo. ----------
If used('_F08cCur')
   Use in _F08cCur
EndIf

f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = f_select + "Select * "

f_from   = f_from  + "From F08c" + _em + " "

f_where  = f_where + " Where F08cCodPro ='" + cCodPro + "'"
f_where  = f_where + "   And F08cCodArt ='" + cCodArt + "'"
f_orden  = ""

lx_sql = f_select + f_from + f_where + f_orden

Err = f3_SqlExec(_ASql, lx_sql, '_F08cCur')
If Err <= 0
   If Used('_F08cCur')
      Use in _F08cCur
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return UsrConect
EndIf

Select _F08cCur
Go Top
If !Eof()
   cTipPro = _F08cCur.F08cTipPro
   cTamHab = _F08cCur.F08cTamAbi
EndIf

If used('_F08cCur')
   Use in _F08cCur
EndIf
*> ---------------------------------------------------------

*> ---------------------------------------------------------
If used('F14cCur')
   Use in F14cCur
EndIf

*> Validar que existe y el estado es correcto.
f_select = ''
f_from   = ''
f_where  = ''
f_group  = ''
f_orden  = ''

f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*, '" + Space(14) + "' As F12cUbPick"
f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F14c" + _em
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cNumEnt ='" + cNumEnt + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cCodPro ='" + cCodPro + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cCodArt ='" + cCodArt + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cUbiOri <>'" + Space(14) + "'"
f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cUbiDes = '" + Space(14) + "'"
f_group  = ""

lx_sql = f_select + f_from + f_where + f_group + f_orden

Err = f3_SqlExec(_ASql, lx_sql, 'F14cCur')
If Err <= 0
   If used('F14cCur')
      Use in F14cCur
   EndIf

   UsrConect = 'C'
   This.UsrError = 'Error accediendo a BD'
   Return UsrConect
EndIf
*> --------------------------------------------------------------------------


*> --------------------------------------------------------------------------
Select F14cCur
Do While !Eof()

   *> Bucle de busqueda en ubicaciones de tipo 'N' y posteriormente de tipo 'S'
   *> En el caso de tipo 'S' no puede estar asignada a ningun articulo.

   cTipUbi = 'N'
   BclTUbi = .T.

   *> -----------------------------------------------------------------------
   Do While BclTUbi

	   *> -----------------------------------------------------------------------
	   *> Si el MP tiene asignada ubicacion se descarta. ------------------------
	   If !Empty(F14cCur.F14cUbiDes)
	      Select F14cCur
	      Skip
	      loop
	   EndIf
	   *> -----------------------------------------------------------------------

	   *> -----------------------------------------------------------------------
	   cUbicacion = Space(14)
	   Store 0 To nCanMax, nCanOcu
	   *> -----------------------------------------------------------------------
	
	   *> -----------------------------------------------------------------------
	   *> Verificar si se puede ubicar en la ubicacion de picking.
	   If Empty(F14cCur.F12cUbPick) and F14cCur.F14cSitStk='1000'
	      *> Obtengo ubicacion de picking. -----------------------------------------
	      If used('F12cCur')
	         Use in F12cCur
	      EndIf
	
	      *> Validar que existe y el estado es correcto.
	      f_select = ''
	      f_from   = ''
	      f_where  = ''
	      f_group  = ''
	      f_orden  = ''
	
	      f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
	      f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F12c" + _em + ", F10c" + _em
	      f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F12cCodPro ='" + F14cCur.F14cCodPro + "'"
	      f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F12cCodArt ='" + F14cCur.F14cCodArt + "'"
	      f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cCodUbi = F12cCodUbi"
	      f_group  = ""

	      lx_sql = f_select + f_from + f_where + f_group + f_orden
	
	      Err = f3_SqlExec(_ASql, lx_sql, 'F12cCur')
	      If Err <= 0
	         If used('F12cCur')
	            Use in F12cCur
	         EndIf
	
	         UsrConect = 'C'
	         This.UsrError = 'Error accediendo a BD'
	         Return UsrConect
	      EndIf
	
	      Select F12cCur
	      Go Top
	      If !Eof()
	         cUbicacion = F12cCur.F12cCodUbi
	      EndIf
	
	      *> -----------------------------------------------------------------------
	      If !Empty(cUbicacion)
 	         *> --------------------------------------------------------------------
	         *> Calculamos el maximo de unidades a ubicar en el hueco. -------------
	         nCanMax = This.UnidadesHueco(F12cCur.F10cTamUbi, F14cCur.F14cCodPro, F14cCur.F14cCodArt)
	         *> --------------------------------------------------------------------
	         *> Calculo el numero de unidades ocupadas -----------------------------
	         nCanOcu = This.UnidadesOcupadas(F12cCur.F12cCodUbi, F14cCur.F14cCodPro, F14cCur.F14cCodArt)
	         *> --------------------------------------------------------------------
	      EndIf
	
	      If used('F12cCur')
	         Use in F12cCur
	      EndIf
	      *> -----------------------------------------------------------------------
	   EndIf
	   *> -----------------------------------------------------------------------
	
	   *> -----------------------------------------------------------------------
	   *> Si no se puede ubicar en picking, busco ubicacion alternativa. --------
	   If nCanMax <= nCanOcu

	      *> Actualizo la ubicacion de referencia.
	      If !Empty(cUbicacion)
	         Select F14cCur
	         Replace F12cUbPick With IIf(Empty(F12cUbPick), cUbicacion, F12cUbPick)  && Ubicacion de picking.
	      EndIf
	
	      Store 0 To nCanMax, nCanOcu
	
	      *> Buscamos ubicacion. ------------------------------------------------
	      cUbicacion = Space(14)
	      c_Ubica    = Space(14)

	      *> --------------------------------------------------------------------
	      *> Determino si se trata de un palet completo o no.
	      *> Si es un pico intento ubicarlas en ocupaciones parcialmente ocupadas
	      *> o en ubicaciones utilizadas en MP.
	
          *> Calculamos el maximo de unidades a ubicar en el hueco. -------------
	      nCanMax = This.UnidadesHueco(cTamHab, cCodPro, cCodArt)
	
	      If nCanMax > F14cCur.F14cCanFis   && MP con un pico.
	
   	         *> Busco ocupaciones parcialmente llenas a partir del F16c  --------
   	         f_select = ''
             f_from   = ''
             f_where  = ''
             f_group  = ''
             f_orden  = ''

             f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
	         f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F16c" + _em + ", F10c" + _em
             f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F16cCodPro = '" + cCodPro + "'"
             f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F16cCodArt = '" + cCodArt + "'"
             f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F16cSitStk = '" + F14cCur.F14cSitStk + "'"
             f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cPickSN not in ('M','I','E')"
	         f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cCodUbi = F16cCodUbi"
             f_group  = ""

             lx_sql = f_select + f_from + f_where + f_group + f_orden

             Err = f3_SqlExec(_ASql, lx_sql, 'F16cTmp')
	         If Err <= 0
                If used('F16cTmp')
                   Use in F16cTmp
                EndIf

                UsrConect = 'C'
                This.UsrError = 'Error accediendo a BD'
                Return UsrConect
             EndIf

             Select F16cTmp
             Go Top
             Do While !Eof()

	            *> --------------------------------------------------------------------
	            *> Calculamos el maximo de unidades a ubicar en el hueco. -------------
 	            nCanMax = This.UnidadesHueco(F16cTmp.F10cTamUbi, cCodPro, cCodArt)
	            *> --------------------------------------------------------------------
	            *> Calculo el numero de unidades ocupadas -----------------------------
	            nCanOcu = This.UnidadesOcupadas(F16cTmp.F16cCodUbi, cCodPro, cCodArt)
	            *> --------------------------------------------------------------------
	
	            If nCanMax - nCanOcu >= F16cTmp.F16cCanFis
	               c_Ubica  = F16cTmp.F16cCodUbi
	               c_Tamaño = F16cTmp.F10cTamUbi
                   exit
	            EndIf
	
                Select F16cTmp
                Skip
             EndDo
   	         *> -----------------------------------------------------------------

   	         *> -----------------------------------------------------------------
   	         *> Busco ocupaciones parcialmente llenas a partir del F14c  --------
	         If Empty(c_Ubica)
	
   	            f_select = ''
                f_from   = ''
                f_where  = ''
                f_group  = ''
                f_orden  = ''

                f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
	            f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F14c" + _em + ", F10c" + _em
	            f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cTipMov like '1%'"
                f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cCodPro = '" + cCodPro + "'"
                f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cCodArt = '" + cCodArt + "'"
                f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cSitStk = '" + F14cCur.F14cSitStk + "'"
	            f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F14cUbiOri <> '" + Space(14) + "'"
                f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cPickSN not in ('M','I','E')"
	            f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cCodUbi = F14cUbiOri"
                f_group  = ""

                lx_sql = f_select + f_from + f_where + f_group + f_orden

                Err = f3_SqlExec(_ASql, lx_sql, 'F14cMP')
   	            If Err <= 0
                   If used('F14cMP')
                      Use in F14cMP
                   EndIf

                   UsrConect = 'C'
                   This.UsrError = 'Error accediendo a BD'
                   Return UsrConect
                EndIf

                Select F14cMP
                Go Top
                Do While !Eof()

   	               *> --------------------------------------------------------------------
   	               *> Calculamos el maximo de unidades a ubicar en el hueco. -------------
 	               nCanMax = This.UnidadesHueco(F14cMP.F10cTamUbi, cCodPro, cCodArt)
	               *> --------------------------------------------------------------------
	               *> Calculo el numero de unidades ocupadas -----------------------------
	               nCanOcu = This.UnidadesOcupadas(F14cMP.F14cUbiOri, cCodPro, cCodArt)
   	               *> --------------------------------------------------------------------
 	
	               If nCanMax - nCanOcu > F14cCur.F14cCanFis
	                  c_Ubica  = F14cMP.F14cUbiOri
	                  c_Tamaño = F14cMP.F10cTamUbi
	                  exit
   	               EndIf
   	
                   Select F14cMP
                   Skip
                EndDo
	         EndIf
	
	         If !Empty(c_Ubica)
                lUbiVacia  = .F.
	            cUbicacion = c_Ubica
	
	            *> Bloqueo la ubicacion. ----------------------------------------
	            Store 0 to n_Peso, n_Volu
	            Do PesVolAr In Ora_Ca00 With F14cCur.F14cCodPro, F14cCur.F14cCodArt, nCanMax - nCanOcu, n_Peso, n_Volu

	            l_Ubicado=.F.
	            n_NPal=1
	            Do CargaUbi in Ora_Ca00 with c_Ubica, n_Peso, n_Volu, n_NPal, l_Ubicado
	            If !L_Ubicado
	               c_Ubica=""
 	               Store 0 To nCanMax, nCanOcu
                   Select F14cCur
                   cNumReg  = RecNo()
                   BclTUbi = .F.
	            Endif
	         EndIf
	      EndIf
          *> -----------------------------------------------------------------------

	      *> -----------------------------------------------------------------------
          If Empty(c_Ubica)

	         Store 0 To nCanMax, nCanOcu

	         If used('F10cCur')
	            Use in F10cCur
	         EndIf
	
	         *> Validar que existe y el estado es correcto.
	         f_select = ''
	         f_from   = ''
	         f_where  = ''
	         f_group  = ''
	         f_orden  = ''
	
	         f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
	         f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F10c" + _em
	         f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cEstEnt = 'N'"
	         f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cEstGen = 'L'"
	         f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cPickSN = '" + cTipUbi + "'"
	
	         *> Filtro por tipo de producto.
	         If !Empty(cTipPro)
	            f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F10cTipPro = '" + cTipPro + "'"
	         EndIf
	
	         f_group  = ""
	
	         f_mayor  = "And F10cCodUbi >'" + F14cCur.F12cUbPick + "'"
  	         f_ordmay = 'Order by F10cCodUbi'
  	         f_menor  = "And F10cCodUbi <'" + F14cCur.F12cUbPick + "'"
	         f_ordmen = 'Order by F10cCodUbi Desc'
	
	         *> Ubicaciones posteriores.
	         lx_sql = f_select + f_from + f_where + f_mayor + f_group + f_ordmay
	
	         Err = f3_SqlExec(_ASql, lx_sql, 'F10cMay')
    	     If Err <= 0
	            If used('F10cMay')
   	               Use in F10cMay
	            EndIf
	
	            UsrConect = 'C'
	            This.UsrError = 'Error accediendo a BD'
	            Return UsrConect
	         EndIf

	         *> Ubicaciones previas.
	         lx_sql = f_select + f_from + f_where + f_menor + f_group + f_ordmen
	
	         Err = f3_SqlExec(_ASql, lx_sql, 'F10cMen')
	         If Err <= 0
	            If used('F10cMen')
	               Use in F10cMen
	            EndIf
	
	            UsrConect = 'C'
	            This.UsrError = 'Error accediendo a BD'
	            Return UsrConect
	         EndIf

	         *> Obtengo el peso de la ubicacion immediatamente posterior disponible.
	         Select F10cMay
	         Go Top
	
	         *> Obtengo el peso de la ubicacion immediatamente anterior disponible.
	         Select F10cMen
	         Go Top
	
  	         *> --------------------------------------------------------------------
	         Salir = .F.
	         Do While !Salir
	
	            Select F10cMay
	            PesMay = IIf(Eof(), 0, PesoUbi(F10cMay.F10cCodUbi))
	
  	            Select F10cMen
	            PesMen = IIf(Eof(), 0, PesoUbi(F10cMen.F10cCodUbi))
	
	            *> Determino la ubicacion mas proxima.
	            If PesMay=0 and PesMen=0
	               If cTipUbi = 'N'
	                  cTipUbi = 'S'   && Buscamos en ubicaciones de tipo 'S'
	                  Salir = .T.
	                  loop
	               Else
	                  UsrConect = 'N'
	                  This.UsrError = 'No hay ubicaciones libres'
	                  Return UsrConect
	               EndIf
	            EndIf
	
	            If (PesMay<>0 and PesMen=0) or (PesMay=0 and PesMen<>0)
	               If PesMay=0  && F10cMay no tiene registros
	                  c_Ubica  = F10cMen.F10cCodUbi
	                  c_Tamaño = F10cMen.F10cTamUbi
	                  cMenMay  = 'F10cMen'
	               Else         && F10cMay no tiene registros
	                  c_Ubica  = F10cMay.F10cCodUbi
	                  c_Tamaño = F10cMay.F10cTamUbi
	                  cMenMay  = 'F10cMay'
	               EndIf
	            Else
	               *> Obtengo el peso de la ubicacion de la referencia.
	               PesUbi = PesoUbi(F14cCur.F12cUbPick)
	
 	               PesMay=Abs(PesMay-PesUbi)
	               PesMen=Abs(PesUbi-PesMen)
	
	               If PesMen < PesMay
	                  c_Ubica  = F10cMen.F10cCodUbi
 	                  c_Tamaño = F10cMen.F10cTamUbi
	                  cMenMay  = 'F10cMen'
	               Else
	                  c_Ubica  = F10cMay.F10cCodUbi
	                  c_Tamaño = F10cMay.F10cTamUbi
	                  cMenMay  = 'F10cMay'
	               Endif
	            EndIf
	
	            *> -----------------------------------------------------------------
	            *> Validamos que la ubicacion de picking no este asignada. ---------
	            If cTipUbi='S'
	
	               If used('F12cCur')
	                  Use in F12cCur
	               EndIf
	
	               *> Validar que existe y el estado es correcto.
	               f_select = ''
	               f_from   = ''
	               f_where  = ''
	               f_group  = ''
	               f_orden  = ''
	
 	               f_select = IIf(Empty(f_select), ' Select ' , f_select + '')      + "*"
	               f_from   = IIf(Empty(f_from),   ' From '   , f_from   + '')      + "F12c" + _em
	               f_where  = IIf(Empty(f_where),  ' Where '  , f_where  + ' And ') + "F12cCodUbi ='" + c_Ubica + "'"
	
	               lx_sql = f_select + f_from + f_where + f_group + f_orden
	
	               Err = f3_SqlExec(_ASql, lx_sql, 'F12cCur')
	               If Err <= 0
	                  If used('F12cCur')
	                     Use in F12cCur
	                  EndIf
	               EndIf
	
	               Select F12cCur
	               Go Top
	               If !Eof()

	                  *> Descartamos la ubicacion evaluada porque esta asignada.
	                  If cMenMay == 'F10cMay'
	                     Select F10cMay
	                     Skip
	                     If Eof()
	                        Select F10cMen
	                        If Eof()
	                           If cTipUbi = 'N'
	                              cTipUbi = 'S'   && Buscamos en ubicaciones de tipo 'S'
	                              loop
	                           Else
	                              UsrConect = 'N'
	                              This.UsrError = 'No hay ubicaciones libres'
	                              Return UsrConect
	                           EndIf
	                        Else
                               loop
	                        EndIf
                         Else
                            loop
	                     EndIf
	                  Else
	                     Select F10cMen
	                     Skip
	                     If Eof()
	                        Select F10cMay
	                        If Eof()
	                           If cTipUbi = 'N'
	                              cTipUbi = 'S'   && Buscamos en ubicaciones de tipo 'S'
	                              loop
	                           Else
	                              UsrConect = 'N'
	                              This.UsrError = 'No hay ubicaciones libres'
	                              Return UsrConect
	                           EndIf
	                        Else
                               loop
	                        EndIf
                         Else
                            loop
	                     EndIf
	                  EndIf
	               EndIf
	
	            EndIf
 	            *> -----------------------------------------------------------------
	
 	            *> -----------------------------------------------------------------
  	            *> Calculamos el maximo de unidades a ubicar en el hueco. ----------
	            nCanMax = This.UnidadesHueco(c_Tamaño, F14cCur.F14cCodPro, F14cCur.F14cCodArt)
	            *> -----------------------------------------------------------------

	            *> -----------------------------------------------------------------
	            *> Calculo el numero de unidades ocupadas --------------------------
	            nCanOcu = This.UnidadesOcupadas(c_Ubica, F14cCur.F14cCodPro, F14cCur.F14cCodArt)
 	            *> -----------------------------------------------------------------
 	
	            *> -----------------------------------------------------------------
	            *> Verificamos que hay espacio para ubicar material. ---------------
	            If nCanMax > nCanOcu
	
	               cUbicacion = c_Ubica
	
	               *> Bloqueo la ubicacion. ----------------------------------------
	               Store 0 to n_Peso, n_Volu
	               Do PesVolAr In Ora_Ca00 With F14cCur.F14cCodPro, F14cCur.F14cCodArt, nCanMax - nCanOcu, n_Peso, n_Volu
	
	               l_Ubicado=.F.
	               n_NPal=1
	               Do CargaUbi in Ora_Ca00 with c_Ubica, n_Peso, n_Volu, n_NPal, l_Ubicado
	               If !L_Ubicado
	                  c_Ubica=""
	               Else
	                  Salir = .T.   && Condicion de salida del bucle
	               Endif
	            Else
	               *> Descartamos la ubicacion evaluada porque no cabe nada.
	               If cMenMay == 'F10cMay'
	                  Select F10cMay
	                  Skip
	                  If Eof()
	                     Select F10cMen
	                     If Eof()
 	                        If cTipUbi = 'N'
	                           cTipUbi = 'S'   && Buscamos en ubicaciones de tipo 'S'
	                        Else
	                           UsrConect = 'N'
	                           This.UsrError = 'No hay ubicaciones libres'
	                           Return UsrConect
  	                        EndIf
	                     EndIf
	                  EndIf
	               Else
	                  Select F10cMen
	                  Skip
	                  If Eof()
	                     Select F10cMay
	                     If Eof()
	                        If cTipUbi = 'N'
	                           cTipUbi = 'S'   && Buscamos en ubicaciones de tipo 'S'
	                        Else
	                           UsrConect = 'N'
  	                           This.UsrError = 'No hay ubicaciones libres'
	                           Return UsrConect
	                        EndIf
	                     EndIf
	                  EndIf
	               EndIf
	            EndIf
	            *> -----------------------------------------------------------------
	
	         EndDo
	         *> --------------------------------------------------------------------
	
	         If used('F10cMay')
	            Use in F10cMay
	         EndIf
             If used('F10cMen')
	            Use in F10cMen
  	         EndIf
  	
	      EndIf
	      *> --------------------------------------------------------------------------
	
	   EndIf
	
	   If nCanMax > nCanOcu
	      If F14cCur.F14cCanFis > nCanMax - nCanOcu
	
	         *> Desgloso MP
	         nNMovOld = F14cCur.F14cNumMov
	         nCFisOld = F14cCur.F14cCanFis - (nCanMax - nCanOcu)

	         cNumMov = Ora_NewMP()
	
	         Select F14cCur
	         Replace F14cNumMov With cNumMov
	         Replace F14cCanFis With nCanMax - nCanOcu
	         Replace F14cUbiDes With F14cUbiOri
	         Replace F14cUbiOri With cUbicacion
	         If !F3_InsTun('F14c', 'F14cCur', 'N')
	            UsrConect = 'N'
	            This.UsrError = 'Error insertando MP'
	            Return UsrConect
	         EndIf

	         *> Actualizo cursor.
	         Select F14cCur
	         Replace F14cNumMov With nNMovOld
	         Replace F14cCanFis With nCFisOld
	         Replace F14cUbiOri With F14cUbiDes
	         Replace F14cUbiDes With Space(12)
	         Replace F12cUbPick With IIf(Empty(F12cUbPick), cUbicacion, F12cUbPick)  && Ubicacion de picking.

	         *> Actualizo el MP
	         f_update =            " Update F14c" + _em
	         f_update = f_update + " Set    F14cCanFis="  + Alltrim(Str(F14cCur.F14cCanFis))
	         f_update = f_update + " Where  F14cNumMov='" + F14cCur.F14cNumMov + "'"
	         _ok = f3_SqlExec(_ASql, f_update)
	         If _ok <= 0
	            UsrConect = 'N'
	            This.UsrError = 'Error actualizando MP'
	            Return UsrConect
	         EndIf
	
	         *> Mantenemos la posicion en F14c
	         Select F14cCur
	         cNumReg = Recno()
	      Else
	         *> Actualizo el MP
	         f_update =            " Update F14c" + _em
	         f_update = f_update + " Set    F14cUbiOri='" + cUbicacion + "'"
	         f_update = f_update + " ,      F14cUbiDes='" + F14cCur.F14cUbiOri + "'"
	         f_update = f_update + " Where  F14cNumMov='" + F14cCur.F14cNumMov + "'"
	         _ok = f3_SqlExec(_ASql, f_update)
	         If _ok <= 0
	            UsrConect = 'N'
	            This.UsrError = 'Error actualizando MP'
	            Return UsrConect
	         EndIf
	
	         *> Avanzamos una posicion en F14c
	         Select F14cCur
	         Skip
	         If !Eof()
                cNumReg = Recno()
	         EndIf
	      EndIf
	
          *> Abandonamos el bucle
          BclTUbi = .F.
	   EndIf
	   *> -----------------------------------------------------------------------
   EndDo
   *> ---------------------------------------------------------------------------

   Select F14cCur
   If !Eof()
      Go cNumReg
   EndIf
EndDo

If used('F14cCur')
   Use in F14cCur
EndIf
*> --------------------------------------------------------------------------

Return UsrConect

ENDPROC
PROCEDURE Init

Private oProcaot

oProcaot = CreateObject('Procaot')
This.oPROCAOT = oProcaot

ENDPROC


EndDefine 
