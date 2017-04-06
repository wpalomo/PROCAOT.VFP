Public ST3SEG_45T0S4TQ1
SET CLASSLIB TO "st3class.vcx" ADDITIVE

ST3SEG_45T0S4TQ1 = CreateObject("ST3SEG" )
If ST3SEG_45T0S4TQ1.WindowType == 1
ST3SEG_45T0S4TQ1.Show(1)
else
ST3SEG_45T0S4TQ1.Show()
EndIf



Define Class ST3SEG as form
Top = 9
Left = 16
Height = 279
Width = 600
BackColor = Rgb(192,192,192)
BorderStyle = 3
Caption = (f3_t('Control de serie'))
WindowType = 1
FontBold = .T.
FontSize = 10
ColorSource = 5

ADD OBJECT St_box3 as st_box WITH Height = 81, Left = 387, Top = 100, Width = 198
ADD OBJECT St_box1 as st_box WITH Height = 45, Left = 10, Top = 136, Width = 360, ColorScheme = 16
ADD OBJECT St_box2 as st_box WITH Height = 27, Left = 27, Top = 58, Width = 486, ColorScheme = 17
ADD OBJECT L_stit1 as l_stit WITH Caption = "Número de serie", Left = 162, Top = 9, TabIndex = 1
ADD OBJECT L_stit2 as l_stit WITH Caption = "Título", Left = 126, Top = 36, TabIndex = 3
ADD OBJECT L_normal3 as l_normal WITH Caption = "Programa licenciado a", Left = 63, Top = 64, Width = 128, TabIndex = 6
ADD OBJECT L_normal4 as l_normal WITH Caption = "Código de control", Left = 45, Top = 141, TabIndex = 13
ADD OBJECT L_normal5 as l_normal WITH Caption = "Código de acceso", Left = 45, Top = 159, TabIndex = 18
ADD OBJECT bot_ok as st_bot WITH AutoSize = .F., Top = 198, Left = 162, Height = 28, Width = 117, Caption = "Aceptar", TabIndex = 21
ADD OBJECT L_normal6 as l_normal WITH FontItalic = .T., FontName = "Arial", FontSize = 10, Caption = "Para introducir el código de acceso, llame a Penta MSI,S.A. al teléfono que", ForeColor = Rgb(0,0,128), Left = 65, Top = 241, TabIndex = 23
ADD OBJECT L_normal7 as l_normal WITH FontItalic = .T., FontName = "Arial", FontSize = 10, Caption = "se indica en la licencia, o póngase en contacto con su distribuidor", ForeColor = Rgb(0,0,128), Left = 98, Top = 259, TabIndex = 24
ADD OBJECT _CCONTROL as st_say3d WITH ControlSource = "_ccontrol", Left = 171, TabIndex = 15, Top = 141, Width = 96
ADD OBJECT _CACCESO as st_get WITH ControlSource = "_cacceso", Format = "99999-99999-99999", InputMask = "99999-99999-99999", Left = 171, TabIndex = 19, Top = 159, Width = 144
ADD OBJECT Line2 as line WITH BorderColor = Rgb(255,255,255), Height = 0, Left = 8, Top = 236, Width = 587
ADD OBJECT L_normal8 as l_normal WITH Caption = "Nombre empresa", Left = 10, Top = 108, Width = 99, TabIndex = 9
ADD OBJECT _NEM as st_get WITH ControlSource = "nem", InputMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Left = 120, TabIndex = 10, Top = 108, Width = 248
ADD OBJECT _NSERIE as st_get WITH ControlSource = "_nserie", Enabled = .F., Format = "!!!-!!!!!!!", InputMask = "!!!-!!!!!!!", Left = 297, TabIndex = 2, Top = 9, Width = 96, Comment = ""
ADD OBJECT _TITULO as st_get WITH ControlSource = "_titprg", Enabled = .F., Format = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", InputMask = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", Left = 198, TabIndex = 4, Top = 36, Width = 248, Comment = ""
ADD OBJECT _LICENCIADO as st_get WITH ControlSource = "_licenciado", Enabled = .F., Format = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", InputMask = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", Left = 234, TabIndex = 5, Top = 63, Width = 248
ADD OBJECT bot_esc as st_bot WITH AutoSize = .F., Top = 198, Left = 306, Height = 28, Width = 117, Caption = "Cancelar", TabIndex = 22
ADD OBJECT L_normal9 as l_normal WITH Caption = "Usuarios", Left = 405, Top = 106, TabIndex = 8
ADD OBJECT L_normal10 as l_normal WITH Caption = "Pantallas", Left = 405, Top = 124, TabIndex = 12
ADD OBJECT L_normal11 as l_normal WITH Caption = "Listados abiertos", Left = 405, Top = 142, TabIndex = 17
ADD OBJECT L_normal12 as l_normal WITH Caption = "Empresas abiertas", Left = 405, Top = 160, TabIndex = 20
ADD OBJECT _NUSR as st_get WITH ControlSource = "_nusr", Enabled = .F., Format = "", Height = 18, InputMask = "99", Left = 531, TabIndex = 7, TabStop = .F., Top = 105, Width = 24
ADD OBJECT _NPAN as st_get WITH ControlSource = "_npan", Enabled = .F., Format = "", Height = 18, InputMask = "99", Left = 531, TabIndex = 11, TabStop = .F., Top = 123, Width = 24
ADD OBJECT _NLST as st_get WITH ControlSource = "_nlst", Enabled = .F., Format = "", Height = 18, InputMask = "99", Left = 531, TabIndex = 14, TabStop = .F., Top = 141, Width = 24
ADD OBJECT _NCAM as st_get WITH ControlSource = "_ncam", Enabled = .F., Format = "", Height = 18, InputMask = "999", Left = 531, TabIndex = 16, Top = 159, Width = 32
ADD OBJECT Line1 as line WITH BorderColor = Rgb(128,128,128), Height = 0, Left = 9, Top = 235, Width = 587


PROCEDURE Init
_nserie=left(_n_serie,3)+'-'+subs(_n_serie,4)
_nusr=_n_usr
_npan=_n_pan
_nlst=_n_lst
_ncam=_n_cam+1
_ccontrol='     -     '
_cacceso='     -     -     '
nem=_nem
if nem='DEMOSTRACION'
  thisform._NSERIE.enabled=.T.
  thisform._TITULO.enabled=.T.
  thisform._LICENCIADO.enabled=.T.
endif

ENDPROC


PROCEDURE L_stit1.Click
thisform._NSERIE.enabled=.T.
thisform.refresh
ENDPROC


PROCEDURE L_stit2.Click
thisform._TITULO.enabled=.T.
thisform.refresh
ENDPROC


PROCEDURE L_normal3.Click
thisform._LICENCIADO.enabled=.T.
thisform.refresh
ENDPROC


PROCEDURE bot_ok.Click
if empty(NEM) .or. atc(' ',_cacceso)>0
  thisform.release
endif
lx1='28967'
lx1=lx1+'84425963012'
lx1=lx1+'547858798565896647'
lxa=stuff(lx1,10,7,'9975204')
lxa=stuff(lxa,2,3,'896')
lx1=right(_nserie,3)
lx2=left(_ccontrol,5)
lx3=subs(_ccontrol,7)
store '' to lxx1,lxx2,lxx3
lxx1=subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,4,1))+val(subs(lx3,2,1)),1)
lxx1=lxx1+subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,2,1))+val(subs(lx3,5,1)),1)
lxx1=lxx1+subs(lxa,val(subs(lx1,1,1))+val(subs(lx2,1,1))+val(subs(lx3,4,1)),1)
lxx1=lxx1+subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,5,1))+val(subs(lx3,3,1)),1)
lxx1=lxx1+subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,3,1))+val(subs(lx3,1,1)),1)
lxx2=subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,2,1))+val(subs(lx3,2,1)),1)
lxx2=lxx2+subs(lxa,val(subs(lx1,1,1))+val(subs(lx2,3,1))+val(subs(lx3,1,1)),1)
lxx2=lxx2+subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,4,1))+val(subs(lx3,3,1)),1)
lxx2=lxx2+subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,5,1))+val(subs(lx3,5,1)),1)
lxx2=lxx2+subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,1,1))+val(subs(lx3,4,1)),1)
lx4=str((_nusr*200+_npan+_ncam*350+_nlst+val(right(_nserie,5)))*7)
lx4=right(chrtran(lx4,' ','0'),5)
lxx3=subs(lxa,val(subs(lx1,3,1))+val(subs(lx4,2,1))+val(subs(lx4,2,1)),1)
lxx3=lxx3+subs(lxa,val(subs(lx1,1,1))+val(subs(lx4,3,1))+val(subs(lx4,1,1)),1)
lxx3=lxx3+subs(lxa,val(subs(lx1,2,1))+val(subs(lx4,4,1))+val(subs(lx4,3,1)),1)
lxx3=lxx3+subs(lxa,val(subs(lx1,3,1))+val(subs(lx4,5,1))+val(subs(lx4,5,1)),1)
lxx3=lxx3+subs(lxa,val(subs(lx1,3,1))+val(subs(lx4,1,1))+val(subs(lx4,4,1)),1)
if _cacceso=lxx1+'-'+lxx2+'-'+lxx3
  _correcto=.T.
  select EMPRESA
  if eof()
    append blank
    repl EMP_COD with 1
  endif
  repl EMP_CAC with _cacceso,EMP_CCO with _ccontrol,EMP_NOM with nem
  _nem=nem
  _lxori=''
  for nd=65 to 90 step 2
    _lxori=_lxori+chr(nd)
  endfor
  _lxori=_lxori+'Ú.ÑÁÍÓÉ'
  for nd=48 to 57 step 2
    _lxori=_lxori+chr(nd)
  endfor
  _lxori=_lxori+'Ù-ÇÀÌ'
  for nd=66 to 90 step 2
    _lxori=_lxori+chr(nd)
  endfor
  _lxori=_lxori+' ,ÒÈ'
  for nd=49 to 57 step 2
    _lxori=_lxori+chr(nd)
  endfor
  lx1='ô\*!áû$|aé&sù{í+d@'
  lx2='ó#òq(ú%=wâe[rt)-¡à}'
  lx3=']_çêmfîg1?;èì,¿'
  _lxdes=lx1+lx3+lx2
  lx1=chrtr(_licenciado,_lxori,_lxdes)
  _n_serie=left(_nserie,3)+subs(_nserie,5)
  if upper(left(nem,12))<>'DEMOSTRACION'
    _n_usr=_nusr
    _n_pan=_npan
    _n_lst=_nlst
    _n_cam=_ncam
  endif
  lx2=chrtr(_n_serie,_lxori,_lxdes)
  lx3=chrtr(_titprg,_lxori,_lxdes)
  lx4=str(_n_lst,2)
  lx4=chrtr(lx4,_lxori,_lxdes)
  lx5=str(_n_usr,2)
  lx5=chrtr(lx5,_lxori,_lxdes)
  lx6=str(_n_pan,2)
  lx6=chrtr(lx6,_lxori,_lxdes)
  lx7=str(_n_cam,3)
  _ali=alias()
  select SYSMEMO
  repl CODIGO with lx1+lx2+lx3+lx4+lx5+lx6+lx7
  use SYSMEMO alias 'P1' in 0
  select P1
  go top
  repl CODIGO with lx1+lx2+lx3+lx4+lx5+lx6+lx7
  use
  select alias (_ali)
  _screen.caption=trim(_titprg)+' ['+_em+' '+trim(_nem)+'] '+_usrcod
  _ok=f3_sn(1,2,'CODIGO ACEPTADO','Para acceder a esta empresa, vuelva a entrar el código de usuario')
  thisform.release
else
  _ok=f3_sn(2,1,'No ha entrado correctamente el código de acceso','Desea volver a entrarlo')
  if _ok=.F.
    thisform.release
  endif
endif

ENDPROC


PROCEDURE _CACCESO.When
if upper(left(nem,12))='DEMOSTRACION'
  nem=upper(nem)
endif
_ccontrol=space(11)
_ccacceso=space(17)
if !empty(nem)
  lxnem=chrtran(left(nem,15),' ','')+'QWERTPOIUYASDFGLKJHG'
  lx1=subs(lxnem,3,1)+subs(lxnem,5,1)+subs(lxnem,2,1)+subs(lxnem,8,1)+subs(lxnem,14,1)
  lx2=subs(lxnem,7,1)+subs(lxnem,1,1)+subs(lxnem,9,1)+subs(lxnem,12,1)+subs(lxnem,4,1)
  store '' to lx3,lx4
  for nd=1 to 5
    lx3=lx3+str(mod(asc(subs(lx1,nd,1)),10),1)
    lx4=lx4+str(mod(asc(subs(lx2,nd,1)),10),1)
  endfor
  _ccontrol=lx3+'-'+lx4
endif
thisform.refresh
if left(nem,12)='DEMOSTRACION'
  lx1='28967'
  lx1=lx1+'84425963012'
  lx1=lx1+'547858798565896647'
  lxa=stuff(lx1,10,7,'9975204')
  lxa=stuff(lxa,2,3,'896')
  lx1=right(_nserie,3)
  lx2=left(_ccontrol,5)
  lx3=subs(_ccontrol,7)
  store '' to lxx1,lxx2,lxx3
  lxx1=subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,4,1))+val(subs(lx3,2,1)),1)
  lxx1=lxx1+subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,2,1))+val(subs(lx3,5,1)),1)
  lxx1=lxx1+subs(lxa,val(subs(lx1,1,1))+val(subs(lx2,1,1))+val(subs(lx3,4,1)),1)
  lxx1=lxx1+subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,5,1))+val(subs(lx3,3,1)),1)
  lxx1=lxx1+subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,3,1))+val(subs(lx3,1,1)),1)
  lxx2=subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,2,1))+val(subs(lx3,2,1)),1)
  lxx2=lxx2+subs(lxa,val(subs(lx1,1,1))+val(subs(lx2,3,1))+val(subs(lx3,1,1)),1)
  lxx2=lxx2+subs(lxa,val(subs(lx1,2,1))+val(subs(lx2,4,1))+val(subs(lx3,3,1)),1)
  lxx2=lxx2+subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,5,1))+val(subs(lx3,5,1)),1)
  lxx2=lxx2+subs(lxa,val(subs(lx1,3,1))+val(subs(lx2,1,1))+val(subs(lx3,4,1)),1)
  lx4=str((_nusr*200+_npan+_ncam*350+_nlst+val(right(_nserie,5)))*7)
  lx4=right(chrtran(lx4,' ','0'),5)
  lxx3=subs(lxa,val(subs(lx1,3,1))+val(subs(lx4,2,1))+val(subs(lx4,2,1)),1)
  lxx3=lxx3+subs(lxa,val(subs(lx1,1,1))+val(subs(lx4,3,1))+val(subs(lx4,1,1)),1)
  lxx3=lxx3+subs(lxa,val(subs(lx1,2,1))+val(subs(lx4,4,1))+val(subs(lx4,3,1)),1)
  lxx3=lxx3+subs(lxa,val(subs(lx1,3,1))+val(subs(lx4,5,1))+val(subs(lx4,5,1)),1)
  lxx3=lxx3+subs(lxa,val(subs(lx1,3,1))+val(subs(lx4,1,1))+val(subs(lx4,4,1)),1)
  _cacceso=lxx1+'-'+lxx2+'-'+lxx3
else
  _cacceso='     -     -     '
endif
ENDPROC


PROCEDURE bot_esc.Click
thisform.release

ENDPROC


PROCEDURE L_normal9.Click
thisform._NUSR.enabled=.T.
thisform.refresh
ENDPROC


PROCEDURE L_normal10.Click
thisform._NPAN.enabled=.T.
thisform.refresh
ENDPROC


PROCEDURE L_normal11.Click
thisform._NLST.enabled=.T.
thisform.refresh
ENDPROC


PROCEDURE L_normal12.Click
thisform._NCAM.enabled=.T.
thisform.refresh
ENDPROC


EndDefine 
