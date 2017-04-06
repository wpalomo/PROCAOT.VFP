*>
*> Procesos de cierre diario.
*>
*> Calcular el peso a partir de F16t y, si no existe, tomar de F08c.
*> Debe estar FACGENC.PRG en la lista de procedimientos activos. AVC - 29.01.2002


PUBLIC _COMVFP

_ComVfp = 'AUTOMATICO'


* Set Default To &_JobDir

Set Exclusive off
*>
*> Obtener conexión con ORACLE para el caso de llamada externa.
Open DataBase Conexion
=Sys(3053)
_ASql = SqlConnect(_ComVfp)
Close Database

_tmp=Sys(2023) + "\"
_sys=Sys(5) + CurDir()

_xier=0
_xerrac=0
_lxerr=''
select 100
use _SYS + 'SYSPRG' order tag idx1

_xier=0
select 101
use _sys + 'SYSTXT' order tag idx1

_xier=0
select 102
use _sys + 'SYSFC' order tag idx1

_xier=0
select 103
use _sys + 'SYSMEN' order tag idx1

_xier=0
select 106
use _sys + 'SYSTXT30' order tag idx1

Set Point To .
Set Date british

Set Procedure to FacGenc Additive
Set Procedure To St3Rt Additive
Set Procedure To Ora_Proc  Additive
Set Procedure to Wproc001  Additive
Set Procedure To WProcOra  Additive
Public _Entorno,_em

_Entorno = 'ORACLE'
_Em = '001'

Private cWhere, cField, cGroup, cOrder, cFromF
Local lEstado, Pes, Vol

Fecha = Date()

If Used("F16cInfo")
   Use In F16cInfo
EndIf
If Used("F30dInfo")
   Use In F30dInfo
EndIf

*> Extraemos información de las ocupaciones.
cField = "F16cCodPro, F16cCodArt, F16cSitStk, F16cNumPal, " + GetCvtNvl(_ENTORNO, _VERSION, "Sum(F16cCanFis)") + " As Contador"
cWhere = ""
cOrder = ""
cGroup = "F16cCodPro, F16cCodArt, F16cSitStk, F16cNumPal"
cFromF = "F16c"

Wait Window 'Obteniendo datos para realizar el cierre ...' NoWait

lEstado = f3_sql(cField, cFromF, cWhere, cOrder, cGroup, "F16cInfo")
If !lEstado
	_LxErr = 'No hay datos a procesar' + cr
	Do Form St3Inc With .T.
	If Used("F16cInfo")
	   Use In F16cInfo
	EndIf
	=SqlRollBack(_ASql)
	Return lEstado
EndIf

*> Cursor de trabajo sobre tabla de cierre día.
=CrtCursor('F30D', 'F30dInfo', 'C')

Select F16cInfo
Go Top
Count To total
Store 0 To n_reg

Go Top
Do While !Eof()
	n_reg = n_reg + 1

	Wait Window 'Tratando registro Nº ' + Str(n_reg) + ' de ' + Str(total) NoWait

	*> Obtener datos de la ficha de artículo.
	m.F08cCodPro = F16cCodPro
	m.F08cCodArt = F16cCodArt
	=f3_seek('F08c')

	*> Obtener el peso del F16t.
    Pes = CalcularPesoMovimiento(F16cCodPro, F16cNumPal, Contador)
    If Pes <= 0
       *> Si no se ha pesado el palet, calcular peso a partir de la ficha del artículo.
	   Pes = F16cInfo.Contador * F08c.F08cPesUni
	EndIf

	*> El volumen se calcula siempre a partir de la ficha del artículo.
	Vol = F16cInfo.Contador * F08c.F08cVolUni

    *> Añadir peso, volumen y cantidad a cursor de F30d.
    Select F30dInfo
    Locate For F30dCodArt==F16cInfo.F16cCodArt .And. ;
               F30dSitStk==F16cInfo.F16cSitStk
    If !Found()
       Append Blank
       Replace F30dCodPro With F16cInfo.F16cCodPro, ;
               F30dCodArt With F16cInfo.F16cCodArt, ;
               F30dSitStk With F16cInfo.F16cSitStk, ;
               F30dFecha  With Fecha, ;
               F30dTotVol With 0, ;
               F30dTotPes With 0, ;
               F30dStock  With 0
    EndIf

    Replace F30dTotVol  With F30dTotVol + Vol, ;
            F30dTotPes  With F30dTotPes + Pes, ;
            F30dStock   With F30dStock  + F16cInfo.Contador

	*> Procesar siguiente artículo/Palet.
	Select F16cInfo
	Skip
EndDo

*> Borrar los datos del día a calcular, si los hay.
cFromF = "F30d"
cWhere = "F30dFecha=" + GetCvtDate(_ENTORNO, _VERSION, Fecha)

lEstado = f3_DelTun(cFromF, , cWhere, 'N')
If !lEstado
   _LxErr = 'Error borrando datos existentes' + cr
   Do Form St3Inc With .T.
   =SqlRollBack(_ASql)
   Return
EndIf

*> Insertar los datos del cursor en la tabla de cierre, F30D.
Select F30dInfo
Go Top
Do While !Eof()
   Wait Window 'Generando: ' + F30dCodPro + Space(1) + F30dCodArt NoWait

   =f3_InsTun('F30D', 'F30dInfo', 'N')

   Select F30dInfo
   Skip
EndDo

Wait Clear

=SqlCommit(_Asql)

If Used("F16cInfo")
   Use In F16cInfo
EndIf
If Used("F30dInfo")
   Use In F30dInfo
EndIf

Return lEstado
