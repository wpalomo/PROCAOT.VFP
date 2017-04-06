Public st3imp2_45T0S4QJY
SET CLASSLIB TO "st3class.vcx" ADDITIVE

st3imp2_45T0S4QJY = CreateObject("st3imp2" )
If st3imp2_45T0S4QJY.WindowType == 1
st3imp2_45T0S4QJY.Show(1)
else
st3imp2_45T0S4QJY.Show()
EndIf



Define Class st3imp2 as form
Height = 65
Width = 400
ShowTips = .T.
AutoCenter = .T.
BorderStyle = 2
Caption = "IMPRESION DIRECTA"
MaxButton = .F.
MinButton = .F.
WindowType = 1
BackColor = Rgb(255,255,128)
l_lxlist = ""
l_expr = ""
l_expfor = ""

ADD OBJECT bot_pan as st_bot WITH AutoSize = .F., Top = 36, Left = 12, Height = 29, Width = 29, Picture = "bmp\pantalla.bmp", Caption = "", TabIndex = 2, ToolTipText = "Impresión vista previa"
ADD OBJECT bot_impp as st_bot WITH AutoSize = .F., Top = 36, Left = 48, Height = 29, Width = 29, Picture = "bmp\printp.bmp", Caption = "", TabIndex = 3, ToolTipText = "Impresora predeterminada (Botón derecho, cambiar)"
ADD OBJECT bot_imp as st_bot WITH AutoSize = .F., Top = 36, Left = 84, Height = 29, Width = 29, Picture = "bmp\print.bmp", Caption = "", TabIndex = 4, ToolTipText = "Seleccionar otra impresora"
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 36, Left = 360, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 6, ToolTipText = "Salir"
ADD OBJECT Label1 as label WITH FontOutline = .T., Alignment = 2, BackStyle = 0, Caption = "Label1", Height = 18, Left = 0, Top = 12, Width = 384, TabIndex = 1, ColorSource = 0, ForeColor = Rgb(0,128,192), BackColor = Rgb(0,0,0), DisabledForeColor = Rgb(0,0,255), DisabledBackColor = Rgb(192,192,192)
ADD OBJECT bot_impc as st_bot WITH AutoSize = .F., Top = 36, Left = 120, Height = 29, Width = 29, Picture = "bmp\grafic.bmp", Caption = "", TabIndex = 5, ToolTipText = "Exportar listado"


PROCEDURE Release
_controlc=.F.

ENDPROC
PROCEDURE Init
*>
*> lx0 --------> Caption form.
*> lx1 --------> Nombre del report.
*> lx2 --------> Nombre del cursor.
*> lx3 --------> Condición While.
*> lx4 --------> Condición For.
*> _ls_idiom --> Idioma.
*> lx5 --------> Flag dispositivo salida.

Parameter Lx0,lx1,lx2,lx3,lx4,_ls_idiom,lx5
thisform.label1.caption=f3_t(Lx0)
_controlc=.F.
if type('lx3')<>'C' .or. empty(lx3)
  lx3='!eof()'
endif
if type('lx4')<>'C' .or. empty(lx4)
  lx4='!deleted()'
endif
if type('lx5')<>'C' .or. empty(lx5)
  lx5 = .F.
endif

lxlist='LSTW/'+lx1
if type('_ls_idiom')='N'
  if _ls_idiom>1
    lst1='LSTW'+str(_ls_idiom,1)+'/'+lx1
    lst2=lst1+'.FRX'
    lxlist=iif(file(lst2),lst1,lxlist)
  endif
endif
*

thisform.caption=f3_t(thisform.caption)
thisform.l_lxlist=lxlist
thisform.l_fichero=lx2
thisform.l_expr=lx3
thisform.l_expfor=lx4

ENDPROC


PROCEDURE bot_pan.Click
private _xfc
_xfc=thisform.l_fichero
select alias(_xfc)
r1ls=iif(eof(),0,recno())
lxlist=thisform.l_lxlist
expr=thisform.l_expr
expfor=thisform.l_expfor
select alias(_xfc)
report form (lxlist) preview while &expr for &expfor noconsole
select alias(_xfc)
if r1ls>0
  go r1ls
endif
thisform.release
ENDPROC


PROCEDURE bot_impp.Click
private _xfc
_xfc=thisform.l_fichero
select alias(_xfc)
r1ls=iif(eof(),0,recno())
lxlist=thisform.l_lxlist
expr=thisform.l_expr
expfor=thisform.l_expfor
select alias(_xfc)
report form (lxlist) to printer while &expr for &expfor noconsole
select alias(_xfc)
if r1ls>0
  go r1ls
endif
thisform.release
ENDPROC
PROCEDURE bot_impp.RightClick
*>
*> Establecer la impresora predeterminada.

Set Printer To Name GetPrinter()

*>
*> Do St3Imp2.Mpr
*>

ENDPROC


PROCEDURE bot_imp.Click
private _xfc
_xfc=thisform.l_fichero
select alias(_xfc)
r1ls=iif(eof(),0,recno())
lxlist=thisform.l_lxlist
expr=thisform.l_expr
expfor=thisform.l_expfor
select alias(_xfc)
report form (lxlist) to printer prompt while &expr for &expfor noconsole
select alias(_xfc)
if r1ls>0
  go r1ls
endif
thisform.release

ENDPROC


PROCEDURE bot_esc.Click
thisform.release
ENDPROC


PROCEDURE bot_impc.Click
*>
*> Exportar a EXCEL.
Private _xfc, r1ls, cfile
Local loMenu

*> Mostrar menú contextual.
* loMenu = NewObject("_shortcutmenu", Home() + "\FFC\_MENU")
loMenu = NewObject("_shortcutmenu", "FFC\_MENU")

With loMenu
   .AddMenuBar("Exportar a \<EXCEL", ;
               [.vresult = 1], ;
               , ;
               .F., ;
               .F., ;
               .F., ;
               .T.)

   .AddMenuBar("Exportar a \<TEXTO", ;
               [.vresult = 2], ;
               , ;
               .F., ;
               .F., ;
               .F., ;
               .F.)

   .AddMenuBar("Exportar a \<DBF", ;
               [.vresult = 3], ;
               , ;
               .F., ;
               .F., ;
               .F., ;
               .F.)

*   .AddmenuSeparator()
*   .AddMenuBar("\<No exportar", ;
*               "MessageBox('NADA !!!')")
   .ShowMenu()
EndWith

_xfc=thisform.l_fichero
select alias(_xfc)
r1ls=iif(eof(),0,recno())

Do Case
    *> Valores no procesables.
    Case Type('loMenu.vresult') # 'N'

	*> Exportar a EXCEL.
	Case loMenu.vresult==1
		cfile = GetFile("Hoja Excel:XLS", "Selección", "", 0, "EXPORTAR A EXCEL")
		If !Empty(cfile)
			lxlist=thisform.l_lxlist
			expr=thisform.l_expr
			expfor=thisform.l_expfor
			select alias(_xfc)
			Copy To (cfile) while &expr for &expfor Type XL5
		EndIf

	*> Exportar a TEXTO
	Case loMenu.vresult==2
		cfile = GetFile("Archivo de Texto:TXT", "Selección", "", 0, "EXPORTAR A TEXTO")
		If !Empty(cfile)
			lxlist=thisform.l_lxlist
			expr=thisform.l_expr
			expfor=thisform.l_expfor
			select alias(_xfc)
			Copy To (cfile) while &expr for &expfor Type SDF
		EndIf

	*> Exportar a DBF
	Case loMenu.vresult==3
		cfile = GetFile("Archivo Dbf:DBF", "Selección", "", 0, "EXPORTAR A DBF")
		If !Empty(cfile)
			lxlist=thisform.l_lxlist
			expr=thisform.l_expr
			expfor=thisform.l_expfor
			select alias(_xfc)
			Copy To (cfile) while &expr for &expfor
		EndIf
EndCase

select alias(_xfc)
if r1ls>0
  go r1ls
endif

Release loMenu

thisform.bot_esc.setfocus

ENDPROC


EndDefine 
