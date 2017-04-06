Public ST3MSI_45T0S4M1W
SET CLASSLIB TO "st3class.vcx" ADDITIVE
SET CLASSLIB TO "ffc\_hyperlink.vcx" ADDITIVE

ST3MSI_45T0S4M1W = CreateObject("ST3MSI" )
If ST3MSI_45T0S4M1W.WindowType == 1
ST3MSI_45T0S4M1W.Show(1)
else
ST3MSI_45T0S4M1W.Show()
EndIf



Define Class ST3MSI as form
BorderStyle = 2
Height = 291
Width = 549
AutoCenter = .T.
Caption = ""
MaxButton = .F.
MinButton = .F.
WindowType = 1
WindowState = 0
HelpContextID = 0
WhatsThisHelpID = 100
WhatsThisHelp = .T.
WhatsThisButton = .T.
BackColor = Rgb(192,192,192)

ADD OBJECT St_box1 as st_box WITH Top = 162, Left = 41, Height = 91, Width = 497, ColorScheme = 16
ADD OBJECT L_normal1 as l_normal WITH Caption = "Programa licenciado a", Left = 216, Top = 54, TabIndex = 2
ADD OBJECT L_stit1 as l_stit WITH FontItalic = .F., FontUnderline = .F., BackStyle = 1, Caption = "Subtit1", Left = 230, Top = 107, TabIndex = 4, ForeColor = Rgb(0,0,128), BackColor = Rgb(192,192,192)
ADD OBJECT L_tit3 as l_tit WITH FontItalic = .F., FontName = "Arial", FontSize = 14, FontUnderline = .F., BackStyle = 1, Left = 216, Top = 18, TabIndex = 1, ForeColor = Rgb(0,0,128), BackColor = Rgb(192,192,192)
ADD OBJECT St_box2 as st_box WITH Top = 73, Left = 218, Height = 27, Width = 321, ColorScheme = 17
ADD OBJECT St_bot1 as st_bot WITH AutoSize = .F., Top = 261, Left = 243, Height = 26, Width = 94, Caption = "Continuar", TabIndex = 11
ADD OBJECT Label4 as label WITH AutoSize = .T., BackStyle = 0, Caption = "Label4", Height = 17, Left = 237, Top = 78, Width = 40, TabIndex = 3, ForeColor = Rgb(0,64,64)
ADD OBJECT Label5 as label WITH AutoSize = .T., FontBold = .F., BackStyle = 0, Caption = "Label5", Height = 17, Left = 61, Top = 173, Width = 40, TabIndex = 7, ForeColor = Rgb(0,0,128)
ADD OBJECT Label6 as label WITH AutoSize = .T., FontBold = .F., BackStyle = 0, Caption = "Label6", Height = 17, Left = 61, Top = 191, Width = 40, TabIndex = 8, ForeColor = Rgb(0,0,128)
ADD OBJECT Label7 as label WITH AutoSize = .T., FontBold = .F., BackStyle = 0, Caption = "Label7", Height = 17, Left = 61, Top = 209, Width = 40, TabIndex = 9, ForeColor = Rgb(0,0,128)
ADD OBJECT Label8 as label WITH AutoSize = .T., FontBold = .F., BackStyle = 0, Caption = "Label8", Height = 17, Left = 61, Top = 227, Width = 40, TabIndex = 10, ForeColor = Rgb(0,0,128)
ADD OBJECT St_box3 as st_box WITH Top = 13, Left = 5, Height = 135, Width = 189
ADD OBJECT _hyperlinkimage1 as _hyperlinkimage WITH Stretch = 2, Height = 116, Left = 14, MouseIcon = "ico\h_point.cur", Top = 22, Width = 170
ADD OBJECT L_stit2 as l_stit WITH FontItalic = .F., FontUnderline = .F., BackStyle = 1, Caption = "Subtit2", Left = 230, Top = 130, TabIndex = 5, ForeColor = Rgb(0,0,128), BackColor = Rgb(192,192,192)


PROCEDURE validaracceso

Local lStado
Local cAAA, cBBB, cCCC
Local nAAA, nBBB, nCCC

Store .T. To lStado

Select 0
Use Apilamiento Alias Apila

Go Top
nAAA = Val(User)
cAAA = CToD(Sys(10, Abs(Val(User))))		&& Límite fecha
If cAAA < Date() .Or. nAAA <= 0
	Replace User With AllTrim(Str(Abs(nAAA) * (-1)))
	Use In (Select('Apila'))
	=f3_sn(1, 1, 'EL PROGRAMA HA EXPIRADO', 'CONTACTE CON EL DISTRIBUIDOR')
	lStado = .F.
	Return lStado
EndIf

Skip
nBBB = Val(User)
cBBB = CToD(Sys(10, Abs(Val(User))))		&& Inicio límite accesos
If (cBBB <= Date() .And. nBBB > 0) .Or. nBBB <= 0
	Select Apila
	Replace User With AllTrim(Str(Abs(nBBB) * (-1)))
	=f3_sn(1, 1, 'EL PROGRAMA ESTA PROXIMO A EXPIRAR', 'CONTACTE CON EL DISTRIBUIDOR')
Else
	Use In (Select('Apila'))
	lStado = .T.
	Return lStado
EndIf

Skip
nCCC = Val(User)
cCCC = Abs(Val(User))						&& Inicio límite accesos
If cCCC <= 0
	=f3_sn(1, 1, 'HA EXCEDIDO EL NUMERO MAXIMO DE USOS DEL PROGRAMA', 'CONTACTE CON EL DISTRIBUIDOR')
	lStado = .F.
Else
	If cCCC < 5
		=f3_sn(1, 1, 'EL PROGRAMA ESTA PROXIMO A AGOTAR EL Nº MAXIMO DE USOS', 'CONTACTE CON EL DISTRIBUIDOR')
	EndIf

	*> Actualizar el nº de usos
	cCCC = cCCC - 1
	Select Apila
	Replace User With AllTrim(Str(Abs(cCCC) * (-1)))
	lStado = .T.
EndIf

Use In (Select('Apila'))
Return lStado

ENDPROC
PROCEDURE ___historial___de___modificaciones___

*> Historial de modificaciones:

*> 05.06.2008 (AVC) Corregir tratamiento de seguridad si se trabaja con el ejecutable en lugar de trabaja ren modo desarrollo.
*>					Substituído el acceso a APILAMIENTO.VCX por APILAMIENTO.DBF.
*>					Modificado método ThisForm.ValidarAcceso.
*>					Modificado proyecto SEGURIDAD del SGA para contemplar estos cambios.


ENDPROC
PROCEDURE ___system___

*> Cargar funciones de S. O.

*> Recibe:

*> Ddevuelve:
*>	- Estado (.T. / .F.)
*>	- This.UsrError, mensajes de error.

Local lStado As Boolean, oSafe As Object, cTmp As String
Local cFileSystem As String, cSystemD As String, cSystemL As String
Local cSystemDate As String, cLicenseDate As String, nDiasPreaviso As Integer
Local dSystemDate As Date, dLicenseDate As Date

Set ClassLib To Seguridad6 Additive

oSafe = CreateObject("rsa")

*> Archivo de sistema.
cFileSystem = Addbs(Curdir()) + "SYSTEMD.INI"
If !File(cFileSystem)
	=f3_sn(1, 1, 'NO SE HA ENCONTRADO SISTEMA DE LICENCIA', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

*> Contenido del archivo de sistema.
cSystemD = oSafe.Decodificar(FileToStr(cFileSystem))
If Empty(cSystemD)
	=f3_sn(1, 1, 'SISTEMA DE LICENCIA SIN VALOR', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

*> Archivo de licencia.
cFileSystem = Addbs(Curdir()) + "SYSTEML.INI"
If !File(cFileSystem)
	=f3_sn(1, 1, 'NO SE HA ENCONTRADO DEFINICIÓN DE LICENCIA', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

*> Contenido del archivo de licencia.
cSystemL = oSafe.Decodificar(FileToStr(cFileSystem))
If Empty(cSystemL)
	=f3_sn(1, 1, 'DEFINICIÓN DE LICENCIA SIN VALOR', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

*> Fecha de sistema.
cTmp = This.GetKeyValue(cSystemD, "CLAVE")
If Empty(cTmp)
	=f3_sn(1, 1, 'NO EXISTE CLAVE DE LICENCIA', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

cSystemDate = This.GetKeyValue(cSystemD, "DIA")
cSystemDate = cSystemDate + "/" + This.GetKeyValue(cSystemD, "MES")
cSystemDate = cSystemDate + "/" + This.GetKeyValue(cSystemD, "ANY")
dSystemDate = Ctod(cSystemDate)

If Empty(cSystemDate)
	=f3_sn(1, 1, 'FECHA EXPIRACIÓN DE LICENCIA NO DEFINIDA', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

*> Fecha de licencia.
cTmp = This.GetKeyValue(cSystemL, "CLAVE")
If Empty(cTmp)
	=f3_sn(1, 1, 'CLAVE EXPIRACIÓN DE LICENCIA NO DEFINIDA', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

cLicenseDate = This.GetKeyValue(cSystemL, "DIA")
cLicenseDate = cLicenseDate + "/" + This.GetKeyValue(cSystemL, "MES")
cLicenseDate = cLicenseDate + "/" + This.GetKeyValue(cSystemL, "ANY")
dLicenseDate = Ctod(cLicenseDate)

If Empty(cLicenseDate)
	=f3_sn(1, 1, 'CLAVE EXPIRACIÓN DE LICENCIA SIN VALOR', 'CONTACTE CON EL DISTRIBUIDOR')
	Return .F.
EndIf

*> Validar la licencia.
nDiasPreaviso = Val(This.GetKeyValue(cSystemL, "PREAVISO"))

Do Case
	*> Programa expirado.
	Case Date() > dLicenseDate
		=f3_sn(1, 1, 'EL PROGRAMA HA EXPIRADO', 'CONTACTE CON EL DISTRIBUIDOR')
		Return .F.

	*> Programa próximo a expirar.
	Case Date() >= (dLicenseDate - nDiasPreaviso)
		=f3_sn(1, 1, 'EL PROGRAMA ESTA PROXIMO A EXPIRAR', 'CONTACTE CON EL DISTRIBUIDOR')
EndCase

Release ClassLib Seguridad6

Return

ENDPROC
PROCEDURE getkeyvalue

*> Obtener el valor de una clave en una cadena, con el formato <KEY>=<VALUE>;<KEY>=<VALUE>; ...
*> Método privado de la clase.

*> Recibe:
*>	- Cadena a explorar.
*>	- Clave a buscar.

*> Devuelve:
*>	- Valor de la clave (cadena vacía si no existe).

*> Llamado desde:

Parameters cCadena, cClave

Local cValor, n1, cSepa

cValor = ""
cSepa = ";"

n1 = At(cClave, cCadena)
If n1<=0
	*> No existe la cadena buscada.
	Return cValor
EndIf

*> Obtener la cadena: KEY=VALOR
cValor = Substr(cCadena, n1)
n1 = At(cSepa, cValor)
If n1 > 0
	cValor = Substr(cValor, 1, n1 - 1)
EndIf

*> Separar el valor de la clave.
n1 = At("=", cValor)
If n1 > 1
	cValor = Substr(cValor, n1 + 1)
Else
	cValor = ""
EndIf

Return cValor

ENDPROC


PROCEDURE L_stit1.Init
this.caption=f3_t('Id. producto ')+' '+_program_version
ENDPROC


PROCEDURE L_tit3.Init
this.caption=f3_t(_titprg)
ENDPROC


PROCEDURE St_bot1.Click
thisform.release
ENDPROC


PROCEDURE Label4.Init
this.caption=_licenciado
ENDPROC


PROCEDURE Label5.Init
this.caption=f3_t(_LEGAL_NOTICE1)

ENDPROC


PROCEDURE Label6.Init
this.caption=f3_t(_LEGAL_NOTICE2)

ENDPROC


PROCEDURE Label7.Init
this.caption=f3_t(_LEGAL_NOTICE3)


ENDPROC


PROCEDURE Label8.Init
this.caption=f3_t(_LEGAL_NOTICE4)

ENDPROC


PROCEDURE _hyperlinkimage1.Init
With This
	.Picture = _logo
	.ToolTipText = f3_t('Visite nuestra página web')
	.cTarget = _URL
EndWith

ENDPROC


PROCEDURE L_stit2.Init
this.caption=f3_t('No. de serie')+' ' + _n_serie + ;
	Iif(Type('_service_pack')=='C' .And. !Empty(_service_pack), " (" + Trim(_service_pack) + ")", "")

ENDPROC


EndDefine 
