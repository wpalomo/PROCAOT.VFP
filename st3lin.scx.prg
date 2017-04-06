Public ST3LIN_45T0S4SSM
SET CLASSLIB TO "st3class.vcx" ADDITIVE

ST3LIN_45T0S4SSM = CreateObject("ST3LIN" )
If ST3LIN_45T0S4SSM.WindowType == 1
ST3LIN_45T0S4SSM.Show(1)
else
ST3LIN_45T0S4SSM.Show()
EndIf



Define Class ST3LIN as form
Height = 286
Width = 633
AutoCenter = .T.
BackColor = Rgb(255,255,128)
BorderStyle = 3
Caption = (f3_t('Reordenar las líneas'))
Closable = .F.
FontBold = .F.
FontName = "Courier"
MaxButton = .T.
Visible = .F.
TabStop = .T.
WindowType = 1
LockScreen = .F.
AlwaysOnTop = .T.
FontSize = 10
ColorSource = 5

ADD OBJECT Label4 as label WITH AutoSize = .T., FontSize = 8, BackStyle = 0, Caption = "", Height = 16, Left = 12, Top = 287, Width = 0, TabIndex = 4, FontBold = .T., ColorSource = 0
ADD OBJECT Grid1 as grid WITH ColumnCount = 2, FontBold = .F., FontName = "Courier New", FontSize = 8, DeleteMark = .F., GridLines = 3, Height = 242, Left = 4, Panel = 1, ReadOnly = .F., RecordMark = .F., RecordSource = "SYSLIN", RecordSourceType = 1, RowHeight = 16, ScrollBars = 2, TabIndex = 1, Top = 2, Visible = .T., Width = 627, Column1.FontBold = .F., Column1.FontName = "Courier New", Column1.FontSize = 8, Column1.ControlSource = "ST3_LIN", Column1.Width = 46, Column1.ReadOnly = .F., Column1.Visible = .T., Column2.FontBold = .F., Column2.FontName = "Courier New", Column2.FontSize = 8, Column2.BackColor = Rgb(192,192,192), Column2.ControlSource = "ST3_TXT", Column2.Enabled = .F., Column2.Width = 1000, Column2.ReadOnly = .T., Column2.Visible = .T.
ADD OBJECT "Grid1.Column1.Header1" as header WITH FontBold = .T., FontName = "Arial", FontSize = 8, Caption = "Línea"
ADD OBJECT "Grid1.Column1.ST3_LIN" as textbox WITH FontBold = .F., FontName = "Courier New", FontSize = 8, BackColor = Rgb(255,255,255), BackStyle = 0, BorderStyle = 0, ForeColor = Rgb(0,0,0), InputMask = "9999", Margin = 0, ReadOnly = .F., ColorSource = 3
ADD OBJECT "Grid1.Column2.Header1" as header WITH FontBold = .F., FontName = "Courier New", FontSize = 8, Caption = ""
ADD OBJECT "Grid1.Column2.ST3_TXT" as textbox WITH FontBold = .F., FontName = "Courier New", FontSize = 8, BackColor = Rgb(192,192,192), BorderStyle = 0, Enabled = .F., ForeColor = Rgb(0,0,0), Margin = 0, ReadOnly = .T., SpecialEffect = 1, DisabledForeColor = Rgb(0,0,128), DisabledBackColor = Rgb(192,192,192), SelectedForeColor = Rgb(192,192,192), ColorSource = 3
ADD OBJECT bot_ok as st_bot WITH AutoSize = .F., Top = 252, Left = 549, Height = 29, Width = 29, Picture = "bmp\ok.bmp", Caption = "", TabIndex = 2, ToolTipText = "Aceptar [CTRL+ENTER]"
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 252, Left = 585, Height = 29, Width = 29, Picture = "bmp\close.bmp", Caption = "", TabIndex = 3, ToolTipText = "Salir [ESC]"


PROCEDURE Init
on key
_ok=.F.
_controlc=.F.
select SYSLIN
go top
thisform.grid1.column1.ST3_LIN.setfocus
ENDPROC
PROCEDURE Resize
ancho=thisform.width
alto=thisform.height
if alto>90
  with thisform
    .grid1.height=alto-40
    .grid1.width=ancho-7
    .bot_ok.top=alto-35
    .bot_esc.top=alto-35
    .bot_ok.left=ancho-76
    .bot_esc.left=ancho-38
  endwith
endif

ENDPROC


PROCEDURE Grid1.Column1.ST3_LIN.KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
*
do case
case nKeyCode=10
  _ok=.T.
  thisform.release
case nKeyCode=27
  thisform.release
endcase
ENDPROC
PROCEDURE Grid1.Column1.ST3_LIN.When
_valor_ant=ST3_LIN

ENDPROC
PROCEDURE Grid1.Column1.ST3_LIN.Valid
if syslin.ST3_LIN<>_valor_ant
  r1=recno()
  go top
  go r1
  thisform.refresh
endif
ENDPROC


PROCEDURE bot_ok.Click
_ok=.T.
thisform.release

ENDPROC


PROCEDURE bot_esc.Click
thisform.release

ENDPROC


EndDefine 
