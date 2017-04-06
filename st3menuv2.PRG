*  Menú único de la aplicación
*      'ST3MENUV'
* Autor Carlos Casals Fradera Vers. 3.0 Fecha 06.97
*

*>
*> Modificar para tomar parámetros locales. AVC - 17.05.1999
*> Versión V6. AVC 17.06.1999
*>
*> Reorganizar la lista de bibliotecas de clases. AVC - 12.09.2006

_screen.windowstate=2
clear
clear all
close all

Store _SCREEN.Caption To _OldCaption
Store _SCREEN.Picture To _OldPicture

* -------------------------
* VARIABLES GLOBALES
* -------------------------
* _ASql         Handle conexión con ORACLE (PROCAOT)
* _Procaot      Propietario de procaot
* _xier			Interrupción o error
* _xerrac		Error activo (1=Si)
* _xidiom		Idioma (1=Castellano, 2=Catalán)
* _xnidiom		No. de idiomas
* _xdetall		Listado detallado (Si=1)
* _xopenfc		Nombres de los ficheros abiertos
* _xfc			Nombre del fichero principal de la pantalla
* _xfc2			Nombre del fichero principal de la pantalla de Lineas
* _xfctmp		Nombre del fichero temporal de una pantalla de lineas
* _xwin			Nombre de la última ventana activada
* _xlistado		Sw que indica al objeto de la pantalla si se está al inicio o final de un listado
* _xobjw		Nombre del programa asociado a una pantalla
* _xcodmen		Código acceso a menus (Empresa/Perfil)
* _valorh		Valor con que llega una variable a la ayuda
* _tmp			Ubicación de los ficheros temporales
* _nvarh		Nombre variable para ayuda códigos
* _nvarhc   	Nombre variable para ayuda genérica 
* _lockh		Indica si la ficha está bloqueada  (.T./.F.)
* _1ernvar		Nombre del primer get de un formulario
* _1ernvarl		Nombre del primer get de un formulario (Líneas)
* _lstr1		Registros de los listados asociados a una pantalla (cada 10 bytes)
* _lxerr		Errores de Validación
* _mes			Array con la descripción de los meses
* _inicio		Se pone a .T. al cargar una pantalla y a .F. despues de activarla
* _controlc		Se pone a .T. cuando desactivas un FORM y a .F. cuando lo activas
* _xestado		Indica el estado de la ficha (ALTA BAJA MODI)
* _xestado2 	Indica el estado de la línea (ALTA,MODI)
* _antes		Indica si se accede antes o despues de realizar la ALTA,BAJA,MODI
* _altac		Indica si se ha apretado el botón de Alta Contínua
* _altacl		Idem anterior en líneas
* _r1log		No. de registro de la sesión en SYSLOG
* _logmem		Operaciones realizadas por el usuario para grabar en SYSLOG
* _logfc		Nombre del fichero de control SYSLOG
* _fnbot1		Nombre programa asociado a boton 1 del formulario ST3SN
* _fnbot2		Idem boton 2
* _fnbot3		Idem boton 3
* _empusr		Empresas a las que puede acceder un usuario (en blanco si solo una)
* _listar		Indica si estoy listado o no (Para desactivar pantalla)
* _esc			Se ha pulsado la tecla ESCAPE
* _enter		Se ha pulsado la tecla CTRL+ENTER
* _graba		Se ha pultado la tecla ALT+ENTER
* _botnav		Se ha pulsado las teclas de navegación (I,A,S,F)
* _botlst		Se quiere ir al popup de listado
* _setfocus		Variable para saber si hay que controlar el focus de un input
* _c_reg		Literal con los ficheros a los que hay que controlar sus registros
* _nerrores		No. de errores de impresión
* _intentos		No. de reintentos de impresión si la impresora no está preparada
* _transacc		Modo transacción  (T/F)
* _st3			Indica si se empieza por el programa ST3 en vez del ST3MENU  (desconecta rollback)
* _finlst		Indica el fin del listado (Se pone a .T. manualmente con la funcion ls3fin)
* _bajaok		Indica si se ha pulsado el boton OK en la pantalla st3baja2
* _fn			Número de tecla de función pulsada (Conectar a método itr)
* _ya_alta		Variable de control para dar la Alta manualmente
* _ya_baja
* _ya_modi
* _cambio_e		Cambio de empresa
* _pantalla		Nombre de la pantalla a activar al cambiar la empresa
* _ok			Variable genérica de correcto (T/F)
* _nok			Variable genérica de correcto (1/0)
* _licenciado
* _n_serie
* _titprg
* _fondo		Fondo de la pantalla
* _paso_a_modi	Al dar la alta pasa a modificar
* _usrmaxp		Usuarios de Máxima prioridad
* _listado		Variable que indica el listado elejido de un form
* _hayhlp		Indica si se va hacia una ayuda
* _progant		Programa anterior al que se accede en ese momento (Se actualiza en Activate)
* _iv			Matriz donde se guardan los % de IVA
* _mensaje_hlp	Indica si hay que dar mensaje de no encontrada ninguna ficha en ayuda
* _lxfocus		Variable a donde se dirigirá un focus despues de un inicio/iniciol
* _lcambio		Variable que indica si cambio de Pedido, albarán etc. (Ligada a ls3cambio)
* _getfocus		Variable para cambiar el foco al salir de un input
* _psql			Indica si estoy definiendo un SQL
* _hlpok		Indica si se ha seleccionado un código desde la ayuda
* _idxact       Indice activo en mantenimientos
*
* _inicio_alta
* _inicio_lalta Indica el inicio de un proceso de altas (Cabecera y Línea)
* _ccontrol		Código de control serie
* _cacceso		Código de acceso a la empresa
* _fmte,_fmte2	Formatos euro
*
* _bot_ok		Se ha pulsado OK en pantalla estándard ST3INP o en una auxiliar F_AUX
* conf			Variable para confirmar (S/N)
* usrcod		Código del usuario
* fnlst			Forma de listado (pant=1,Imprp=2,Impr=3)
* _logo			Nombre del fichero que contiene el Logo de la empresa
* cr			salto de línea
* tab			carácter tabulador
*
* --------------------------------
* VARIABLES LIGADAS A LA EMPRESA
* --------------------------------
* Todas las de fichero (Empiezan todas por EMP_)
*	_em			Código de empresa en carácteres sin espacios en blanco
*	_nem		Nombre de la empresa
*	_cif		CIF de la empresa
*	_empfich	Ubicación de los ficheros
*
* Canales abiertos
* 	SYSPRG		100
*	SYSTXT		101
*	SYSBR		103
*   SYSMEN		104
*	SYSVAR		105
*   SYSTXT30	106
*	LOGMaamm	107
*	SYSQUERY	108
*
* --------------------------------
* Control barra de herramientas
* --------------------------------
_botcont=''

Public Array _idiom(2)

Public _toolbars[11,2]
_toolbars[1,1]="Controles de formularios"
_toolbars[2,1]="Controles de informes"
_toolbars[3,1]="Distribución"
_toolbars[4,1]="Estándar"
_toolbars[5,1]="Generador de bases de datos"
_toolbars[6,1]="Generador de consultas"
_toolbars[7,1]="Generador de formularios"
_toolbars[8,1]="Generador de informes"
_toolbars[9,1]="Generador de vistas"
_toolbars[10,1]="Paleta de colores"
_toolbars[11,1]="Presentación preliminar"

FOR ndd=3 TO ALEN(_toolbars,1)
  _toolbars[ndd,2]=WVISIBLE(_toolbars[ndd,1])
  IF _toolbars[ndd,2]
    HIDE WINDOW (_toolbars[ndd,1])
  ENDIF
ENDFOR

* --------------------------------
* Tipos de Programas
* --------------------------------
* U<pantalla>	VALIDACIONES
* V<variable>	AYUDAS
* 				Parámetro 	_pantalla	VALORES		<Nombre de la pantalla activa>
*							_antes		VALORES		.T.,.F.
* W<pantalla>	PANTALLA
* 				Parámetro 	_xestado	VALORES		PROCESO,ABRIR,CERRAR
* X<fichero>	FICHERO
* 				Parámetro 	_xestado	VALORES		ALTA,BAJA,MODI
*							_antes		VALORES		.T.,.F.
* Y<proceso>	PROCESO DE VALIDACION/CALCULO
* Z<proceso>	PROCESO TRANSACCIONAL
* ------------------------
Close DataBase
Close All

Set ClassLib  To St3Class
Set ClassLib  To Alisfour    Additive
Set ClassLib  To Procaot     Additive

Set ClassLib  To Actualizar  Additive
Set ClassLib  To Inventario  Additive
Set ClassLib  To Reserva     Additive
Set ClassLib  To Preparacion Additive
Set ClassLib  To Reposicion  Additive
Set ClassLib  To Ubicar      Additive
Set ClassLib  To Stocks      Additive
Set ClassLib  To Comunicaciones  Additive

Set ClassLib  To Customer    Additive

If File("Facturacion.Vcx")
	Set ClassLib To Facturacion Additive
EndIf

Set Procedure To St3Rt
Set Procedure To Ora_Proc    Additive
Set Procedure to Wproc001    Additive
Set Procedure To WProcOra    Additive
Set Procedure To Ora_Proc    Additive
Set Procedure To FncList     Additive
Set Procedure To ProcRepos   Additive
Set Procedure To Procesos    Additive
Set Procedure To Ordenar_Ubicacion    Additive

_logo='bmp\msi.bmp'
_tmp = Sys(2023) + "\"
_sys = Sys(5) + CurDir()

Public lx,lx1,lx2,lx3,lx4,lx5,lx6,lx7,lx8,lx9,_xopenfc,_xfc,_xfc2,_xwin,_xcodmen,_c_reg,_fondo
Public _cse,_xlistado,_xobjw,_empusr,_xfctmp,_xestado2,_botnav,_botlst,_pantalla,_usrmaxp
Public _nvarhc,_nvarh,_valorh,_lstr1,_lxerr,_1ernvar,_1ernvarl,_logmem,_fnbot1,_fnbot2,_fnbot3
Public _listado,_progant,_lxfocus,_itroldval,_ccoaut,_fmte,_fmte2
Public nd,nd1,nd2,nd3,nd4,nd4,nd6,nd7,nd8,nd9,_xier,fnlst,_xdetall,bot_ok,mescont,_r1log,_lockh,_botl,_bot_ok
Public _nerrores,_fn,_nok
Public fecha,fechal,feci,fecf
Public _xaltac,_inicio,_controlc,_altac,_altacl,_listar,_esc,_enter,_setfocus,_transacc
Public _st3,_finlst,_bajaok,_ya_alta,_ya_baja,_ya_modi,_cambio_e,_ok,_paso_a_modi,_hayhlp
Public _mensaje_hlp,_fpe_cabecera,_doc_cabecera,_com_cabecera,_dsk_remesa,_emp_cl
Public _inicio_alta,_inicio_lalta,_psql,_graba,_modi_lin,_horizontal,_control_coste,_clot
Public _ayuda_especial,_hlpok

Public _ASql, _Alma, _Procaot, _Entorno, _Version

store '' to lx,lx1,lx2,lx3,lx4,lx5,lx6,lx7,lx8,lx9,_xopenfc,_xfc,_xfc2,_xwin,_xcodmen,_c_reg,_fondo
store '' to _cse,_xlistado,_xobjw,_empusr,_xfctmp,_xestado2,_botnav,_botlst,_pantalla,_usrmaxp
store '' to _nvarhc,_nvarh,_valorh,_lstr1,_lxerr,_1ernvar,_1ernvarl,_logmem,_fnbot1,_fnbot2,_fnbot3
store '' to _listado,_progant,_lxfocus,_itroldval,_ccoaut,_fmte,_fmte2
store 0 to nd,nd1,nd2,nd3,nd4,nd4,nd6,nd7,nd8,nd9,_xier,fnlst,_xdetall,bot_ok,mescont,_r1log,_lockh,_botl,_bot_ok
store 0 to _nerrores,_fn,_nok
store date() to fecha,fechal,feci,fecf
store .F. to _xaltac,_inicio,_controlc,_altac,_altacl,_listar,_esc,_enter,_setfocus,_transacc
store .F. to _st3,_finlst,_bajaok,_ya_alta,_ya_baja,_ya_modi,_cambio_e,_ok,_paso_a_modi,_hayhlp
store .T. to _mensaje_hlp,_fpe_cabecera,_doc_cabecera,_com_cabecera,_dsk_remesa,_emp_cl
store .F. to _inicio_alta,_inicio_lalta,_psql,_graba,_modi_lin,_horizontal,_control_coste,_clot
store .F. to _ayuda_especial,_hlpok
Store .Null. To _ASql

_conexion='MSI'
_digfam=3
_almdef='    '
_rec_diaas=5
_rec_fras=1
_listado_esp=.T.
_teclado_esp=.F.
conf='S'
_usrcod=space(10)
store 1 to _xerrac,_xidiom,_xnidiom
cr=chr(13)+chr(10)
tab=chr(9)
define window wnada from 1,1 to 2,2
hide window wnada
define window wlst from 15,10 to 17,60 font 'Courier',10 color RGB(0,0,0,255,255,128)
move window wlst center

* ----------------------------------
* Variables de configuración empresa
* ----------------------------------
Public Array _iv(6,2)
store '' to _nem,_cif,_empfich,_empmcon,_empnomc,_empdir,_emppob,_empcp,_emppro,_emptel,_empfax,_empemai
store '' to _ctair,_ctais,_ctairi,_ctaisi,_ctavpor,_ctavdpp,_ctacpor,_ctacdpp,_ctairpf,_cta4311,_cta5208
store '' to _ccocpor,_ccovpor,_ccocdpp,_ccovdpp,_ccoaut,_ccontrol,_cacceso,_getfocus
store 0 to _clvir,_clvis,_clvrem,_clvreme,_clvcom,_clvimp,_clvlea,_clvcia,_empacon,_emptaxf,_empivatf,_empeuro
_em='001'
_fich='fich'
* --------------------------
* Variables para listados
* --------------------------
use _sys + 'SYSGRAF' in 0
select SYSGRAF
nd=reccount()
dimension _tgraf(nd)
go top
do while !eof()
  _tgraf(recno())=TIPO
  skip
enddo
use

* --------------------------
* Abrir ficheros de sistema
* --------------------------
on error do st3err
on error

_xier=0
_xerrac=0
_lxerr=''
select 100
use _SYS + 'SYSPRG' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSPRG]'+cr,_lxerr)
Do MakePublic With Alias()

_xier=0
select 101
use _sys + 'SYSTXT' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSTXT]'+cr,_lxerr)

_xier=0
select 102
use _sys + 'SYSFC' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSFC]'+cr,_lxerr)
Do MakePublic With Alias()

_xier=0
select 103
use _sys + 'SYSMEN' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSMEN]'+cr,_lxerr)

_xier=0
select 106
use _sys + 'SYSTXT30' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSTXT30]'+cr,_lxerr)

_xier=0
if !empty(_lxerr)
  _xidiom=1
  do form st3inc 
  cancel
endif

* --------------------------
* Configuración del Terminal
* --------------------------
_titprg=''
_n_serie='VMG0010101'
_licenciado=''

lx = AllTrim(Sys(5)) + CurDir()

do case
case file('MSI2.INI')
  _area=fopen('MSI2.INI',0)
case file('PROLAB.INI')
  _area=fopen('PROLAB.INI',0)
case file(trim(lx)+'MSI.INI')
  lx1=trim(lx)+'MSI.INI'
  _area=fopen(lx1,0)
case file(trim(lx)+'PROLAB.INI')
  lx1=trim(lx)+'PROLAB.INI'
  _area=fopen(lx1,0)
otherwise
  _lxerr=iif(_xier<>0,_lxerr+'No existe fichero de configuración [ *.INI ]'+cr,_lxerr)
endcase

do while _area>0 .and. !feof(_area)
  lx=fget(_area)
  lx=alltrim(lx)
  if .not. empty(lx) .and. left(lx,1)<>'[' .and. left(lx,1)<>';'
    &lx
  endif
enddo
=fclose(_area)
_cm="'"

  Do Case
     *> No se ha creado: Abrir conexión con ORACLE.
     Case IsNull(_ASql)
        Local oProcaot, cInitFile

        oProcaot = CreateObject("FOXAPI")
        cInitFile = AllTrim(Sys(5)) + CurDir() + 'CONNDB.INI'
        _ASql = oProcaot.AbrirConexion(cInitFile)
        Release oProcaot

        If _Asql <= 0
           _LxErr = ' MENSAJE: ' + Message() + cr + ;
                    'PROGRAMA: ' + Prog() + cr + ;
                    'DATABASE: ' + _DataBase + cr + ;
                    'CONEXION: ' + _Connection + cr + ;
                    '  ESTADO: ' + Str(_ASql)
           Do Form St3Inc With .T.

           Close DataBase
           Do Rest
           Cancel
        EndIf

     *> Error (por Do Oracle): Cancelar.
     Case _ASql <= 0
           Do Rest
           Cancel

     *> Ya existe (por el Do Oracle): Seguir.
     OtherWise
  EndCase

  on error _xier=0
  for nd=1 to _asql-1
    =sqldisconnect(nd)
  endfor
  on error

if _teclado_esp
  set point to ','
  set separator to '.'
else
  set point to "."
  set separator to ","
endif
_screen.Fontsize=10
if !empty(_fondo)
  _screen.picture=_fondo
endif
if !empty(_lxerr)
  _xidiom=1
  do form st3inc 
  cancel
endif

* ----------------------------
* Pantalla de Presentación
* ----------------------------
lx1='ô\*!áû$|aé&sù{í+d@'
lx2='ó#òq(ú%=wâe[rt)-¡à}'
lx3=']_çêmfîg1?;èì,¿'
_ali=alias()
use sysmemo alias P1 in 0
select P1
go top
lx=CODIGO
use

if !empty(_ali)
   select alias(_ali)
endif

lx3=lx1+lx3+lx2
*_licenciado=left(lx,30)
*_n_serie=subs(lx,31,10)
*_titprg=subs(lx,41,30)
_n_lst=subs(lx,71,2)
_n_usr=subs(lx,73,2)
_n_pan=subs(lx,75,2)
_n_cam=val(subs(lx,77,3))

lx2=''
for nd=65 to 90 step 2
  lx2=lx2+chr(nd)
endfor

lx2=lx2+'Ú.ÑÁÍÓÉ'
for nd=48 to 57 step 2
  lx2=lx2+chr(nd)
endfor

lx2=lx2+'Ù-ÇÀÌ'
for nd=66 to 90 step 2
  lx2=lx2+chr(nd)
endfor

lx2=lx2+' ,ÒÈ'
for nd=49 to 57 step 2
  lx2=lx2+chr(nd)
endfor

*_n_serie=chrtr(_n_serie,lx3,lx2)
*_titprg=chrtr(_titprg,lx3,lx2)
*_licenciado=chrtr(_licenciado,lx3,lx2)
lx1=chrtr(_n_lst,lx3,lx2)
_n_lst=val(lx1)
lx1=chrtr(_n_usr,lx3,lx2)
_n_usr=val(lx1)
lx1=chrtr(_n_pan,lx3,lx2)
_n_pan=val(lx1)

_Ok = .T.

do form st3msi
if !_Ok
  close all
  quit
endif

* ----------------------------
* Borrar ficheros transitorios
* ----------------------------
on error _xier=0
lx=sys(5)+sys(2003)
_xier=0
cd (_tmp)
if _xier<>0
  md (_tmp)
  _xier=0
endif
cd (lx)
tmpfl=_tmp+'*.*'
lx=sys(2000,tmpfl)
_xerrac=0
do while !empty(lx)
  lx=_tmp+lx
  delete file (lx)
  lx=sys(2000,tmpfl,1)
enddo
_xier=0
on error

* ----------------------------
* Verificar fichero de control
* ----------------------------
lx=dtoc(date(), 1)
_logfc = _sys + 'SYSLOG\LOGM'+subs(lx,3,2)+subs(lx,5,2)

if .not. file(_logfc+'.DBF')
  lx1 = "'" + _logfc + '.DBF' + "'"
  copy file SYSLOG\SYSLOG.DBF to &lx1
  lx1 = "'" + _logfc + '.CDX' + "'"
  copy file SYSLOG\SYSLOG.CDX to &lx1
  lx1 = "'" + _logfc+'.FPT' + "'"
  copy file SYSLOG\SYSLOG.FPT to &lx1
endif

_lxerr=iif(_xier<>0,'No se pudo crear el fichero [SYSLOG]'+cr,'')
_xier=0

* -----------------------------
* Verificar ficheros de sistema
* -----------------------------
lx=_fich+'\EMPRESA.DBF'
select 1
use &lx order tag CODIGO alias 'EMPRESA'
do case
case _xier<>0
  _lxerr=_lxerr+'No se pudo abrir el fichero [EMPRESA]'+cr
case eof()
  do st3seg
endcase
_xier=0
if !empty(_lxerr)
  do form st3inc 
  cancel
endif
select EMPRESA
scatter memvar
_xier=0
do st3conf
use
_lxerr=iif(_xier<>0,_lxerr+'Problemas con la configuración de la empresa'+cr,'')
_dbfmemo=_tmp + Sys(2015)
_fptmemo=_dbfmemo+'.FPT'
_dbfmemo=_dbfmemo+'.DBF'
copy file SYSMEMO.DBF to (_dbfmemo)
copy file SYSMEMO.FPT to (_fptmemo)
_lxerr=iif(_xier<>0,_lxerr+'No se pudo crear el fichero [SYSMEMO]'+cr,_lxerr)
_xier=0

select 104
use (_dbfmemo) alias 'SYSMEMO' excl
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSMEMO]'+cr,_lxerr)
_xier=0
if _n_usr=1                     && Monousuario
  set excl on
  quit
endif

select 105
use _sys + 'SYSVAR' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSVAR]'+cr,_lxerr)
scatter memvar
_xier=0

select 107
use (_logfc) order tag idx1 alias 'SYSLOG'
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSLOG]'+cr,_lxerr)
_xier=0
_barra_icono=createobject("st3class.barra_iconos")

select 108
use _sys + 'SYSQUERY' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSQUERY]'+cr,_lxerr)
scatter memvar
_xier=0

select 110
use _sys + 'SYSUSER' order tag idx1
_lxerr=iif(_xier<>0,_lxerr+'No se pudo abrir el fichero [SYSUSER]'+cr,_lxerr)
_xier=0
if !empty(_lxerr)
  do form st3inc 
  cancel
endif

_xerrac=1
_xier=0

* ------------------- No existe ningún usuario

go top
if eof()
  scatter memvar blank
  m.USR_GR=' '
  m.USR_IDIOM=1
  m.USR_NIV='U'
  m.USR_EMP=1
  tit_inp='Definir un Usuario'
  txt_inp='Código/Nombre/Grupo/Idioma'
  var_inp='m.USR_COD/m.USR_NOM/$m.USR_NIV/m.USR_IDIOM'
  fmt_inp='!!!!!!//!/#Combo._idiom'
  bot_ok=0
  do form st3inp
  if bot_ok=1 .and. !empty(m.USR_COD)
    lxpas=space(6)
    lxpasd=chr(asc(subs(lxpas,1,1))+asc(subs(m.USR_COD,5,1))-int(asc(subs(m.USR_COD,3,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,2,1))+asc(subs(m.USR_COD,2,1))-int(asc(subs(m.USR_COD,1,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,3,1))+asc(subs(m.USR_COD,1,1))-int(asc(subs(m.USR_COD,4,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,4,1))+asc(subs(m.USR_COD,6,1))-int(asc(subs(m.USR_COD,3,1))/3))
    lxpasd=lxpasd+chr(asc(subs(lxpas,5,1))+asc(subs(m.USR_COD,4,1))-int(asc(subs(m.USR_COD,2,1))/3))
    m.USR_PAS=lxpasd+chr(asc(subs(lxpas,6,1))+asc(subs(m.USR_COD,3,1))-int(asc(subs(m.USR_COD,3,1))/3))
    select SYSUSER
    append blank
    gather memvar
    append blank
    repl USR_GR with 'U',USR_NOM with 'Usuario'
  else
    =f3_sn(1,1,'No existe ningún usuario de la aplicación','el programa se desconectará')
    quit  
  endif
endif
if _n_usr<>99
  select SYSLOG
  count all for empty(LOG_FECF) to nd
  if nd>=_n_usr
    do st3nusr
  endif
endif
select SYSUSER
go top
if eof()
  do sy3dusr
  go top
  if eof()
    quit
  endif
endif
scatter memvar blank

Do sy3usr
if _xier<>0 .Or. !_ok
  close all
  quit
endif
select SYSPRG

* --------- PREPARACION DEL ENTORNO Y CARGA DE MENUS


* ------------------------
* Ciclo opciones Menú
* ------------------------
finalmenu=.F.
store '' to prg,prog
select SYSMEMO
save all except _* to memo sysmemo.PRG_MEMO
select SYSPRG
on error do st3err
on error



do while finalmenu=.F.
  store 0 to _xier
  read events
  if finalmenu=.F.
    select SYSLOG
    go _r1log
    do case
    case prg='ST3PRG'      && SE HA ELEGIDO UN FORMULARIO ESTANDAR
      select SYSPRG
      if len(_xopenfc)=0  
        seek prog
        release all except _*
        conf='S'
        cr=chr(13)+chr(10)
        tab=chr(9)
        prog=PRG_PROG
        prg='ST3PRG'
        em=_em
        fich=_fich
        empfich=_empfich
        cse=_cse
        usrcod=_usrcod
        restore from memo sysmemo.PRG_MEMO additive
        _em=em
        _empfich=empfich
        _cse=cse
        _usrcod=usrcod
        
        Replace sysmemo.PRG_1  with PRG_PROG, ;
                sysmemo.PRG_2  with '', ;
                sysmemo.PRG_3  with '', ;
                sysmemo.PRG_4  with '', ;
                sysmemo.PRG_5  with '', ;
                sysmemo.PRG_6  with '', ;
                sysmemo.PRG_7  with '', ;
                sysmemo.PRG_8  with '', ;
                sysmemo.PRG_9  with '', ;
                sysmemo.PRG_10 with '', ;
                sysmemo.PRG_11 with '', ;
                sysmemo.PRG_12 with ''

        lx=_fich+'\EMPRESA.DBF'
        select 1
        use &lx order tag CODIGO alias 'EMPRESA'
        seek val(_em)
        scatter memvar
        do st3conf
        select EMPRESA
        use
        select SYSPRG
        prog=PRG_PROG
        prg='ST3PRG'
        _progant=''
        _r1log=recno('SYSLOG')
        *_logmem=syslog.LOG_MEM
      else
        if !empty(_progant)
          for nd=1 to 12
            lx1='sysmemo.PRG_'+ltrim(str(nd))
            if &lx1=_progant
              lx1='sysmemo.PRG_M'+ltrim(str(nd))
              save all except _* to memo &lx1 
              exit
            endif
          endfor
        endif
        select SYSPRG
        seek prog
      endif
      _xwin=trim(prog)
      _c_reg=''

      for _num=1 to memlines(sysprg.PRG_FICH)
        lx1=mline(sysprg.PRG_FICH,_num)
        excl=.F.
        if left(lx1,1)='*'
          excl=.T.
        endif
        do case
        case sysprg.PRG_TIPO='F' .and. _num>1 .and. subs(lx1,2,1)='*'
          _c_reg=_c_reg+'['+alltrim(chrtran(lx1,'*',''))+']'
        case sysprg.PRG_TIPO='C' .and. _num>2 .and. subs(lx1,2,1)='*'
          _c_reg=_c_reg+'['+alltrim(chrtran(lx1,'*',''))+']'
        endcase
        lx1=ltrim(chrtran(lx1,'*',''))
        abrirdbf=iif(!empty(lx1).and.atc('/'+trim(lx1)+']',_xopenfc)=0,.T.,.F.)
        lx5=_xopenfc
        if !empty(lx1)
          _xopenfc=_xopenfc+upper('['+_xwin+'/'+trim(lx1)+']')
        endif
        do case
        case abrirdbf .and. (lx1=_xwin .or. left(lx1,2)='XL')   && -- FICHERO TEMPORAL
          lx=_tmp + Sys(2015)
          dbf_or=_fich+'\'+lx1+'.DBF'
          dbf_des=lx+'.DBF'
          copy file &dbf_or to &dbf_des
          fpt_or=_fich+'\'+lx1+'.FPT'
          if file(fpt_or)
            fpt_des=lx+'.FPT'
            copy file &fpt_or to &fpt_des
          endif
          cdx_or=_fich+'\'+lx1+'.CDX'
          if file(cdx_or)
            cdx_des=lx+'.CDX'
            copy file &cdx_or to &cdx_des
            use (lx) order tag CODIGO alias (lx1) in 0 excl
          else
            use (lx) alias (lx1) in 0 excl
          endif
        case abrirdbf   && --- SE UTILIZA POR PRIMERA VEZ EN ESTA PANTALLA
          select SYSFC
          seek lx1
          if eof()
            _xopenfc=lx5
            =f3_sn(1,1,'Fichero ['+lx1+'] no creado en SYSFC (1)')
            _xier=1
          else
            *> Ha habido cambio de entorno de BD.
            If FC_ENTORN # GetEntornID(_ENTORNO, _VERSION)
               Do Sy3Fich With .T.
            EndIf

            fcfc=iif(len(lx1)>4,lx1,lx1+_em)
            sql_fc=fcfc
            fcfc=iif(!empty(_empfich).and.file(_empfich+'\'+fcfc+'.DBF'),_empfich,_fich)+'\'+fcfc
            if !empty(FC_FCS) .and. !file(fcfc+'.DBF')
              fcfc=iif(at('\',FC_FCS)=0,_fich+'\'+trim(FC_FCS),trim(FC_FCS))
            endif

            if abrirdbf
               If Type('oPRC') # 'O'
                  oPRC = CreateObject('procaot')
               EndIf

               =oPRC.OpenTabla(lx1)
               Select Alias(lx1)
               Do MakePublic With Alias()
               Go Bottom
               Scatter MemVar
            endif
          endif

        case !empty(lx1)   && ---- YA UTILIZADO EN OTRA PANTALLA
          if abrirdbf
             select SYSFC
             seek lx1
             if eof()
               _xopenfc=lx5
               =f3_sn(1,1,'Fichero ['+lx1+'] no creado en SYSFC (2)')
               _xier=1
             else
               *> Ha habido cambio de entorno de BD.
               If FC_ENTORN # GetEntornID(_ENTORNO, _VERSION)
                  Do Sy3Fich With .T.
               EndIf

               If Type('oPRC') # 'O'
                  oPRC = CreateObject('procaot')
               EndIf

               =oPRC.OpenTabla(lx1)
               Select Alias(lx1)
               Do MakePublic With Alias()
               Go Bottom
               Scatter MemVar
             endif
          endif
        endcase
        _num=iif(_xier=1,9999,_num)
      endfor

      if _xier=0
        lx1=mline(sysprg.PRG_FICH,1)
        lx1=ltrim(chrtran(lx1,'*',''))
        select alias(lx1)
        scatter memvar memo
      endif
      select SYSPRG
      if _xier=0
        for nd=1 to memlines(sysprg.PRG_VAR)
          lx1=mline(sysprg.PRG_VAR,nd)
          if !empty(lx1)
            &lx1
          endif
        endfor
        select SYSPRG
        prog=left(prog+space(10),10)
        _xwin=trim(prog)
        do case
        case eof() .or. !file('PNTW\'+_xwin+'.SCX')
          do form st3sn with 1,1,'La pantalla ['+_xwin+'] No existe'
        case PRG_OK=.F.
          do form st3sn with 1,1,'La pantalla ['+_xwin+'] No está validada'  
        case wexist(_xwin)
          activate window (_xwin)
        otherwise
          USR_COD=_usrcod
          fnlst=1
          lxprog='PNTW\'+_xwin
          _progant=''
          if at(sysprg.PRG_TIPO,'FCE')>0
            for nd=1 to 12
              lx1='sysmemo.PRG_'+ltrim(str(nd))
              if empty(&lx1)
                replace &lx1 with PRG_PROG
                lx1='sysmemo.PRG_M'+ltrim(str(nd))
                save all except _* to memo &lx1
                exit
              endif
            endfor
          endif

          do form (lxprog)
        endcase
      else
        finalmenu=.T.
        clear events
      endif
    case file(trim(prg)+'.FXP') .or. file(trim(prg)+'.PRG')
      select SYSPRG
      do &prg
    case file(trim(prg)+'.SCX')
      select SYSPRG
      do form &prg
    case file(prg)
      select SYSPRG
      &prg
    endcase
  endif
  select SYSPRG
enddo

=SqlDisconnect(_ASql)

select SYSMEMO
use
delete file (_dbfmemo)
delete file (_fptmemo)

_logmem = _logmem + 'SALIDA  : ' + DToC(Date())+' ' + Time() + cr
Select SYSLOG
Replace All LOG_FECF With DateTime(), ;
            LOG_MEM With _logmem ;
   For LOG_USR = _usrcod .And. ;
       LOG_USRM = Sys(0) .And. ;
       Empty(LOG_FECF)

set SYSMENU to default
on key
FOR i=1 TO ALEN(_toolbars,1)                && Restaura barra de herramientas
  IF _toolbars[i,2]
    SHOW WINDOW (_toolbars[i,1])
  ENDIF
ENDFOR

if left(_usrcod,5)='PENTA'
  clear all
  close all
  cancel 
else
  clear all
  close all
endif

quit

* ---------------------------
* Entrada de Opciones
* ---------------------------
Procedure rt3opc
* ----------------------
lx1=popup()
lx2=alltrim(subs(prompt(),2))
nd=bar()
select SYSMEN
seek _xcodmen+lx1
store '' to prog,prg
for nd=1 to memlines(MEN_OPC)
  lx1=mline(MEN_OPC,nd)
  menlit=f3_t(trim(subs(lx1,14)))
  menprg=subs(lx1,3,10)
  if alltrim(menlit)=lx2
    if left(lx1,1)='P'
      prog=menprg
      prg='ST3PRG'
    else
      prg=menprg
    endif
    exit
  endif
endfor
swprg=.F.
clear events
_xier=0
*
return

* -------------------------------------
* Función Confirmar salir del programa
* -------------------------------------
function rt3quit
*
do form st3sn with 2,1,'Desea realmente salir de la aplicación'
if conf='S'
  finalmenu=.T.

  Store _OldCaption To _SCREEN.Caption
  Store _OldPicture To _SCREEN.Picture

  clear events
endif
*
return .T.

Procedure MakePublic

   Parameters _Alias
   Private _oldAlias, _inx, _aaa

   _oldAlias = Alias()
   Select (_Alias)
   For _inx = 1 To FCount()
      _aaa = Field(_inx)
      Public &_aaa
   EndFor

   If !Empty(_oldAlias) .And. !IsNull(_oldAlias)
      Select (_oldAlias)
   EndIf

Return

*>
*> Declarar como PUBLIC todos los ControlSource.
Procedure MakePublicControl
   Private _inx, _aaa

_aa = On('Error')
on error a = 1
For _Inx = 1 To This.ControlCount
   If Type('This.Controls(_inx).ControlSource') # 'U'
      If !Empty(This.Controls(_Inx).ControlSource)
         _aaa = This.Controls(_Inx).ControlSource
         Public &_aaa
      EndIf
   EndIf

   *> Llamada recursiva para los controles que, a su vez, sean contenedores.
   If Type('This.Controls(_inx).ControlCount') # 'U'
      If This.Controls(_Inx).ControlCount > 0
         Do MakePublicControl With This.Name
      EndIf
   EndIf
EndFor

on error &_aa

Return
