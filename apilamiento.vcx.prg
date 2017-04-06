Define Class ubifncmol as custom



PROCEDURE inicializar
PUBLIC _TEMPORAL
PUBLIC _UbiEnct
PUBLIC _UbiTam
PUBLIC _NumMov
PUBLIC _MovPnd

Store 0 to _NumMov,_MovPnd

*> Inicializar datos
This.UbiCanFis = 0				&& Cantidad A ubicar
This.UbiCodPro = Space(6)		&& Código Propietario
This.UbiCodArt = Space(13)		&& Código Artículo
This.UbiNumPal = Space(10)		&& Número de Palet	
This.UbiCodUbi = Space(14)		&& Código de Ubicación
This.UbiNumLot = Space(15)		&& Número de Lote (TCL)

ENDPROC
PROCEDURE ubimol

*>Comenzamos A Ubicar o al menos lo intentamos.-----------------------
PRIVATE _CodError ' '
Store '' To _CodError

*> Abrimos los ficheros necesarios para Ubicar
This.OpenFich


*> Controlamos si el artículo es de masivo o no.--------------------
if F08cBrecha.F08cTipAlm <> "M"
	return
endif

*> Deberiamos controlar si ubicar en molada o en brecha.-------------


*> Verificar si tenemos palets ubicacdos de ese artículo y Donde.----
If Empty(_CodError)
	This.TlcUbicados
EndIf

*> Si no hay o no cabe buscar siguiente brecha.----------------------
If Empty(_CodError)
	If Empty(This.UbiCodUbi)
		*>Ponemos a cero lo que hallamos encontrado.-----------------
		Store 0 to _NumMov,_MovPnd
		This.NewBrecha
	EndIf
EndIf

*> Cerrramos todos los ficheros temporales.--------------------------
If Used('F14cBrecha')
	Use In F14cBrecha
EndIf
If Used('F16cBrecha')
	Use In F16cBrecha
EndIf
If Used('F11rBrecha')
	Use In F11rBrecha
EndIf
If Used('F08cBrecha')
	Use In F08cBrecha
EndIf
If Used('F10cBrecha')
	Use In F10cBrecha
EndIf
ENDPROC
PROCEDURE openfich
*> Abriremos ficheros.-----------------------

*> Abrimos el fichero de Artículos.----------
_Sentencia = "Select * From F08c001 Where F08cCodPro = '" + This.UbiCodPro + ;
			 "' And F08cCodArt ='" + This.UbiCodArt + "'"
			
_Err = SqlExec(_Asql,_Sentencia,'F08cBrecha')
=SqlMoreresults(_Asql)
If _Err < 0
  _CodError = 'Error Abriendo fichero de artículos'
  This.Errores
  Return .F.
EndIf

ENDPROC
PROCEDURE tlcubicados
*> Comprobamos si hay ese artículo + TCL en alguna Brecha.-----------------------------
**> Multilote.--------------------------------------------------------------------------
If F08cBrecha.F08cMulLot = 'N'
_Sentencia = " Select F16cCodPro,F16cCodArt " + ;
			 ",F16cNumLot,F16cCodUbi,Count(*) As Cuantos From F16c001,F10c001 Where F16cCodPro = '" + This.UbiCodPro + ;
			 "' And F16cCodArt = '"+ This.UbiCodArt + ;
			 "' And F16cCodUbi = F10cCodUbi " + ;
			 " And F16cNumLot = '"+ This.UbiNumLot + ;
			 "' And F10cTipAlm = 'M' " + ;
			 " And F10cEstEnt = 'N' " + ;
			 " And F10cPickSn <> 'E' " + ;
			 " Group by F16cCodPro,F16cCodArt,F16cNumLot,F16cCodUbi " + ;
			 " Order By Cuantos Desc "

Else
_Sentencia = " Select F16cCodPro,F16cCodArt " + ;
			 ",F16cCodUbi,Count(*) As Cuantos From F16c001,F10c001 Where F16cCodPro = '" + This.UbiCodPro + ;
			 "' And F16cCodArt = '"+ This.UbiCodArt + ;
			 "' And F16cCodUbi = F10cCodUbi " + ;
			 " And F10cTipAlm = 'M' " + ;
			 " And F10cEstEnt = 'N' " + ;
			 " And F10cPickSn <> 'E' " + ;
			 " Group by F16cCodPro,F16cCodArt,F16cCodUbi " + ;
			 " Order By Cuantos Desc "

EndIf
			
_Err = SqlExec(_Asql,_Sentencia,'F16cBrecha')

=SqlMoreresults(_Asql)

*> Si da Error.------------------------------------------------------------------------
If _Err<0
   _CodError = 'Error Abriendo Ocupaciones '
   This.Errores
   Return .F.
EndIf			

*> Comprobamos si hay ese artículo + TCL en movimientos pendientes.---------------------
		
*> Multilote.----------------
If F08cBrecha.F08cMulLot = 'N'
_Sentencia = " Select F14cCodPro,F14cCodArt " + ;
			 ",F14cNumLot,F14cUbiOri,Count(*) As Cuantos From F14c001,F10c001 Where F14cCodPro = '" + This.UbiCodPro + ;
			 "' And F14cCodArt = '"+ This.UbiCodArt + ;
			 "' And F14cUbiOri = F10cCodUbi " + ;
			 " And F14cNumLot = '"+ This.UbiNumLot + ;
			 "' And F10cTipAlm = 'M' " + ;
			 " And F10cEstEnt = 'N' " + ;
			 " And F10cPickSn <> 'E' " + ;
			 " Group by F14cCodPro,F14cCodArt,F14cNumLot,F14cUbiOri " + ;
			 " Order By Cuantos Desc"
Else
_Sentencia = " Select F14cCodPro,F1cCodArt " + ;
			 ",F14cUbiOri,Count(*) As Cuantos From F14c001,F10c001 Where F14cCodPro = '" + This.UbiCodPro + ;
			 "' And F14cCodArt = '"+ This.UbiCodArt + ;
			 "' And F14cUbiOri = F10cCodUbi " + ;
			 " And F10cTipAlm = 'M' " + ;
			 " And F10cEstEnt = 'N' " + ;
			 " And F10cPickSn <> 'E' " + ;
			 " Group by F14cCodPro,F14cCodArt,F14cUbiOri " + ;
			 " Order By Cuantos Desc "
EndIf

_Err = SqlExec(_Asql,_Sentencia,'F14cMovPnd')

=SqlMoreresults(_Asql)

*> Si da Error.------------------------------------------------------------------------
If _Err<0
   _CodError = 'Error Abriendo Mov. Pendientes '
   This.Errores
   Return .F.
EndIf			

*> Si Encontramos.-----------------------------------------------
Select F16cBrecha
_NumMov = F16cBrecha.Cuantos
Go Top
Select F14cMovPnd
_MovPnd = F14cMovPnd.Cuantos
Go Top

If _NumMov > 0 .Or. _MovPnd > 0


   *> Comprobamos Ubicaciones.-----------------------------------
   Select F16cBrecha
   Go Top
   Do While !Eof() .And. Empty(This.UbiCodUbi)

       *> Damos valor a la Ubicación a comprobar.-------------------
	   _UbiEnct = F16cBrecha.F16cCodUbi

       *> Contamos Movimientos para esas Ubicaciones.---------------
		Select F16cBrecha
		Reg_Ant = Recno()
		_NumMov = F16cBrecha.Cuantos
		GoTo Reg_Ant

		Select F14cMovPnd
        Go Top
		Locate For F14cMovPnd.F14cUbiOri = _UbiEnct
        If Found()
		   _MovPnd = F14cMovPnd.Cuantos
        EndIf

	   *>Llamamos al calculo de ubicaciones para ver si cabe.--------
	   This.CalBrecha

       Select F16cBrecha
       Skip
   EndDo

   *> Comprobamos Movimientos pendientes.-----------------------------------
   Select F14cMovPnd
   Go Top
   Do While !Eof() .And. Empty(This.UbiCodUbi)

	   _UbiEnct = F14cMovPnd.F14cUbiOri

       *> Contamos Movimientos para esas Ubicaciones.---------------
		Select F16cBrecha
        Go Top
		Locate For F16cBrecha.F16cCodUbi = _UbiEnct
        If Found()
		   _NumMov = F16cBrecha.Cuantos
        EndIf

		Select F14cMovPnd
		Reg_Ant = Recno()
		_MovPnd = F14cMovPnd.Cuantos
		GoTo Reg_Ant

	   *>Llamamos al calculo de ubicaciones para ver si cabe.--------
	   This.CalBrecha

       Select F14cMovPnd
       Skip
   EndDo

EndIf
ENDPROC
PROCEDURE newbrecha
*> Buscamos una brecha para empezar a ubicar.------------
*> Para ubicar en una nueva brecha tendremos que tener en cuenta dos cosas el tipo de ubicación debe ser 'M'.--
*> y ademas debe estar vacia.----------------------------

 *> Buscamos coeficiente de rotación.---------------------
 If aubparmz[5] = 'S'
	This.UbiCoeF

	If Empty(This.UbiCodUbi)

	   *> Deberiamos hacer lo mismo sin coef. rot..-------------------------------
	   This.UbiSinCoe
	EndIf
 Else
   *> Deberiamos hacer lo mismo sin coef. rot..-------------------------------
   This.UbiSinCoe

 EndIf

 If Empty(This.UbiCodUbi)
    *> Deberiamos hacer lo mismo sin coef. rot..-------------------------------
*    _CodError = " No hay ubicaciones vacias " + message() + cr
*    This.Errores
    Return .F.
 EndIf
ENDPROC
PROCEDURE errores
*>
*> Asignar texto de error a propiedad.
This.UsrError = _CodError + Chr(13) + ' ' + message() + cr + 'TUNDEN' + cr
*   ANTES  =Messagebox(_CodError + Chr(13) + ' ' + message() + ' ' ,16,'TUNDEN')

*> Grabar tabla con incidencias. (en Ora_Proc).
_LxErr = _CodError
=Anomalias()

Return .F.
ENDPROC
PROCEDURE calbrecha
*> Calculamos Todo.----------------------------

*> Abrimos el maestro de ubicaciones.----------
_Sentencia = " Select * From F10c001 Where F10cCodUbi = '" + _UbiEnct + "'"
_Err = SqlExec(_Asql,_Sentencia,'F10cBrecha')
=SqlMoreresults(_Asql)

If _Err<0
   _CodError = " Error abriendo maestro de ubicaciones "
   This.Errores
   Return .F.
EndIf

_TamUbi = F10cBrecha.F10cTamUbi
_TipPal = F08cBrecha.F08cTipPal

*>Abrimos el fichero de relaciones.-----------
_Sentencia = " Select * From F11r001 Where F11rTamubi = '" + _TamUbi + ;
			 "' And  F11rTamPal = '" + _TipPal + "'"
_Err = SqlExec(_Asql,_Sentencia,'F11rBrecha')
=SqlMoreresults(_Asql)

If _Err<0
   _CodError = " Error abriendo maestro de relaciones "
   This.Errores
   Return .F.
EndIf

Select F11rBrecha
Go top
If Eof()

   _CodError = " No hay relacion activa para Tamaño " + _TamUbi + " Tipo " + _Tippal
   This.Errores
   Return .F.

EndIf


*>Calculo de cuantos palets hay a ver si cabe uno mas.------------
*> Lo que cabe es .-----------------------------------------------
_CabePal = F11rbrecha.f11rNumPal * IIF(F08cBrecha.F08cNumREm>0,F08cBrecha.F08cNumRem,1)

*> Para saber lo que hemos ubicado ya en el temporal.-------------
*> Si es llamado por el componente COm.---------------------------
If _Lenguaje = 'VB'
   _Temporal = 0
Else
	This.UbiTemp
EndIf

*> Vemos si cabe uno mas.-----------------------------------------
If _CabePal - (_NumMov+_MovPnd+_Temporal) >0

   *> Si no es molada esto es una cagada.-------------------------
   If F10cBrecha.F10cTipAlm <> 'M'
	   If F10cBrecha.F10cEstGen <> 'O'
		  This.UbiCodubi = F10cBrecha.F10cCodUbi
	   EndIf
   Else
	  This.UbiCodubi = F10cBrecha.F10cCodUbi
   EndIf

EndIf
ENDPROC
PROCEDURE ubitemp
*> En principio hay tres unicas posibilidades.
*> EntMat --- Entrada de Material
*> LinPal --- Entrada por documento
*> MovUbi --- Movimiento masivo

If Used('ENTMAT')
   _FicActivo = Alias('EntMAt')
   Select(_FicActivo)
   _CampoAct = Field(9)
EndIf

If Used('LINPAL')
   _FicActivo = Alias('LinPal')
   Select(_FicActivo)
   _CampoAct = Field(8)


   *> Comprobamos que esa ubicación no tenga otro TCL.-----------------------
   Select (_FicActivo)
   Reg_Ant = RecNo()
   Go Top
   Locate For &_CampoAct = _UbiEnct
   IF Found()
      *>Multilota si o no.------------------------------
      OldFic= Select()
      _NumLote = LinPal.NumLot
      Select(OldFic)
      If F08cBrecha.F08cMulLot = 'N'
         _Comprobar =  LinPal.CodPro+ ;
        			   LinPal.CodArt+ ;
                       _NumLote   <> This.UbiCodPro+ ;
      			                     This.UbiCodArt+ ;
			                         This.UbiNumLot
      	
      Else
         _Comprobar =  LinPal.CodPro+ ;
        			   LinPal.CodArt <> This.UbiCodPro+ ;
      			                        This.UbiCodArt
			
      EndIf
      If _Comprobar
         _Temporal = 999
		 GoTo Reg_Ant
         Return
      EndIf
   EndIf
   GoTo Reg_Ant

EndIf

If Used('CREAPF14C')
   _FicActivo = Alias('CREAPF14C')
   Select(_FicActivo)
   _CampoAct = Field(20) && F14cUbiOri


   *> Comprobamos que esa ubicación no tenga otro TCL.-----------------------
   Select (_FicActivo)
   Reg_Ant = RecNo()
   Go Top
   Locate For &_CampoAct = _UbiEnct
   IF Found()
      *>Multilota si o no.------------------------------
      OldFic= Select()
      _NumLote = CREAPAL.NumLot
      Select(OldFic)
      If F08cBrecha.F08cMulLot = 'N'
         _Comprobar =  CREAPF14C.F14cCodPro+ ;
        			   CREAPF14C.F14cCodArt+ ;
                       _NumLote   <> This.UbiCodPro+ ;
      			                     This.UbiCodArt+ ;
			                         This.UbiNumLot
      	
      Else
         _Comprobar =  CREAPF14C.CodPro+ ;
        			   CREAPF14C.CodArt <> This.UbiCodPro+ ;
      			                         This.UbiCodArt
			
      EndIf
      If _Comprobar
         _Temporal = 999
		 GoTo Reg_Ant
         Return
      EndIf
   EndIf
   GoTo Reg_Ant

EndIf

If Used('MovUbi')
   _FicActivo = Alias('MovUbi')
   Select(_FicActivo)
   _CampoAct = Field(10)
EndIf

If Used('CnfEnt')
   _FicActivo = Alias('CnfEnt')
   Select(_FicActivo)
   _CampoAct = Field(3)

   *> Comprobamos que esa ubicación no tenga otro TCL.-----------------------
   Select (_FicActivo)
   Reg_Ant = RecNo()
   Go Top
   Locate For &_CampoAct = _UbiEnct
   IF Found()

      *>Multilota si o no
      If F08cBrecha.F08cMulLot = 'N'
         _Comprobar =  CnfEnt.CodPro+ ;
        			   CnfEnt.CodArt+ ;
                       CnfEnt.NumLot <> This.UbiCodPro+ ;
      			                        This.UbiCodArt+ ;
			                            This.UbiNumLot
      	
      Else
         _Comprobar =  CnfEnt.CodPro+ ;
        			   CnfEnt.CodArt <> This.UbiCodPro+ ;
      			                        This.UbiCodArt
			
      EndIf
      If _Comprobar
         _Temporal = 999
		 GoTo Reg_Ant
         Return
      EndIf
   EndIf
   GoTo Reg_Ant

EndIf

*> Contamos los registros ya ubicados en el temporal.-------------------------
Select (_FicActivo)
Reg_Ant = RecNo()
Go Top
Count to _Temporal For &_CampoAct = _UbiEnct
GoTo Reg_Ant


ENDPROC
PROCEDURE ubicoef
*>Proceso de ubicación teniendo en cuenta el coeficiente de rotación.-

EXTERNAL ARRAY aubparmz()

   *> Buscar en tipos de producto alternativos, según prioridades.
   _Selec = "Select * From F11t" + _em + ;
            " Where F11tTipPro='" + F08cBrecha.F08cTipPro + _cm + ;
            " Order By F11tPriori"

   _xier = SqlExec(_ASql, _Selec, 'F11t')
   If _xier <= 0
	   _CodError = " Error leyendo prioridades por tipo de artículo (2) " + cr + ;
	   			   " MENSAJE: " + message()
	   This.Errores
	   Return .F.
   EndIf

   =SqlMoreResults(_ASql)


   *> Bucle de búsqueda de ubicación por tipo de producto.
   Select F11t
   Go Top
   Do While !Eof()

		*> Buscamos ubicaciones vacias y con tipo 'M'.--------------
*		_Sentencia = " Select F10cTipPro,F10cCodUbi,F10cTamubi,F10cOrdent From F10c001 Where F10cTipAlm = 'M' " + ;

*		_Sentencia = " Select F10cTipPro,F10cCodUbi,F10cTamubi,F10cOrdent From F10c001  " + ;
*        	    	 " Where F10cEstEnt = 'N' " + ;
*                     " And F10cTipAlm = 'M' " + ;
*    	 			 " And F10cPickSn <> 'E' " + ;
*	            	 " And F10cCodUbi Not In(Select F16cCodUbi From F16c001) " + ;
*    		         " And F10cCodUbi Not In(Select F14cUbiOri From F14c001) "

		_Sentencia = " Select F10cTipPro,F10cCodUbi,F10cTamubi,F10cOrdent From F10c001  " + ;
        	    	 " Where F10cEstEnt = 'N' " + ;
                     " And F10cTipAlm = 'M' " + ;
    	 			 " And F10cPickSn <> 'E' "

	   *> Buscamos coeficiente de rotación.------------------------------
	   If aubparmz[5] = 'S'

	      *> Inicializa prioridades.--------------------------------------
    	  n_ProI = 999999
	      n_ProF = 0
		  f_Fichero = Select()
	      m.F11cCoefRt = F08cBrecha.F08cCoeRot
	      lxReplace = "n_ProI=F11cPriIni, n_ProF=F11cPriFin"
	      =F3_Seek('F11c', , , lxReplace)
	      Select (f_Fichero)

          *> Añadimos a la sentencia el coeficiente de rotación.-----------
	      _Sentencia = _Sentencia + " And (F10cPriori Between " + ;
    	            Str(n_ProF) + " And " + Str(n_ProI) + ")"

	   EndIf


       *> Por tipo de producto.------------------------------------------------
       _Sentencia = _Sentencia + " And F10cTipPro = '" + F11t.F11tTipDes  + "'"
*	   _Sentencia = _Sentencia + " Order By F10cTipPro,F10cTamUbi,F10cCodUbi "

	_Err = SqlExec(_Asql,_Sentencia,'F10cEnct')

	=SqlMoreresults(_Asql)
	If _Err < 0
	   _CodError = " Error buscando ubicaciones vacias " + message()
	   This.Errores
	   Return .F.
	EndIf

*    Select F10cEnct
*    Index On Str(F10cOrdEnt)+F10cTamUbi+F10cCodUbi To F10cConCoe
*    Go Top

	*> Deberiamos ordenar el fichero despues de comprobar los tamaños.----------------
	*> Puede que el ClaBrecha haga algo parecido pero de momento hacemos otro metodo.-	
    This.Optimizacion

    Do While !Eof() .And. Empty(This.UbiCodubi) .And. Empty(_CodError)


         _Trobat = .F.
         *> Miramos que no este en el f14c001.----------------------------------------
         _Sentencia = "Select Count(*) As CAntidad From F14c001 "  + ;
         			  " Where F14cUbiOri = '" + F10cEnct.F10cCodUbi + "'"
         			
		_Err = SqlExec(_Asql,_Sentencia,'F14cEnct')

		=SqlMoreresults(_Asql)
		If _Err < 0
		   _CodError = " Error buscando ubicaciones vacias " + message()
		   This.Errores
		   Return .F.
		EndIf

        Select F14cEnct
        Go Top
        If Cantidad > 0
			_Trobat = .T.
        EndIF


         *> Miramos que no este en el f14c001.----------------------------------------
         _Sentencia = "Select Count(*) As CAntidad From F16c001 "  + ;
         			  " Where F16cCodUbi = '" + F10cEnct.F10cCodUbi + "'"
         			
		_Err = SqlExec(_Asql,_Sentencia,'F16cEnct')

		=SqlMoreresults(_Asql)
		If _Err < 0
		   _CodError = " Error buscando ubicaciones vacias " + message()
		   This.Errores
		   Return .F.
		EndIf

        Select F16cEnct
        Go Top
        If Cantidad > 0
			_Trobat = .T.
        EndIF

        If _Trobat = .F.
	      *> Damos valor a la ubicación.---------------------------------------
    	  _UbiEnct = F10cEnct.F10cCodUbi

	      *>Calculamos lo que cabe.--------------------------------------------
	      This.CalBrecha

        EndIf
    	  Select F10cEnct
	      Skip
	   EndDo

	Select F11t
	Skip
	If !Empty(This.UbiCodubi) .Or. !Empty(_CodError)
       Exit
	EndIf

EndDo	

ENDPROC
PROCEDURE ubisincoe
   *> Buscar en tipos de producto alternativos, según prioridades.--
   _Selec = "Select * From F11t" + _em + ;
            " Where F11tTipPro='" + F08cBrecha.F08cTipPro + _cm + ;
            " Order By F11tPriori"

   _xier = SqlExec(_ASql, _Selec, 'F11t')
   If _xier <= 0
	   _CodError = " Error leyendo prioridades por tipo de artículo (2) " + cr + ;
	   			   " MENSAJE: " + message()
	   This.Errores
	   Return .F.
  EndIf

   =SqlMoreResults(_ASql)

   *> Bucle de búsqueda de ubicación por tipo de producto.----------
   Select F11t
   Go Top
   Do While !Eof()

		*> Buscamos ubicaciones vacias y con tipo 'M'.--------------
*		_Sentencia = " Select F10cTipPro,F10cCodUbi,F10cTamubi,F10cOrdent From F10c001 Where F10cTipAlm = 'M' " + ;

*		_Sentencia = " Select F10cTipPro,F10cCodUbi,F10cTamubi,F10cOrdent From F10c001  " + ;
*        	    	 " Where F10cEstEnt = 'N' " + ;
*                     " And F10cTipAlm = 'M' " + ;
*    	 			 " And F10cPickSn <> 'E' " + ;
*	            	 " And F10cCodUbi Not In(Select F16cCodUbi From F16c001) " + ;
*    		         " And F10cCodUbi Not In(Select F14cUbiOri From F14c001) "

		_Sentencia = " Select F10cTipPro,F10cCodUbi,F10cTamubi,F10cOrdent From F10c001  " + ;
        	    	 " Where F10cEstEnt = 'N' " + ;
                     " And F10cTipAlm = 'M' " + ;
    	 			 " And F10cPickSn <> 'E' "


       *> Por tipo de producto.------------------------------------------------
       _Sentencia = _Sentencia + " And F10cTipPro = '" + F11t.F11tTipDes  + "'"
*	   _Sentencia = _Sentencia + " Order By F10cOrdEnt,F10cTamUbi,F10cCodUbi "

	_Err = SqlExec(_Asql,_Sentencia,'F10cEnct')

	=SqlMoreresults(_Asql)
	If _Err < 0
	   _CodError = " Error buscando ubicaciones vacias " + message()
	   This.Errores
	   Return .F.
	EndIf

*    Select F10cEnct
*    Index On Str(F10cOrdent)+F10cTamUbi+F10cCodUbi To UbiSinCoe
*    Go Top

	*> Deberiamos ordenar el fichero despues de comprobar los tamaños.----------------
	*> Puede que el ClaBrecha haga algo parecido pero de momento hacemos otro metodo.-	
    This.Optimizacion

    Do While !Eof() .And. Empty(This.UbiCodubi) .And. Empty(_CodError)

         _Trobat = .F.
         *> Miramos que no este en el f14c001.----------------------------------------
         _Sentencia = "Select Count(*) As CAntidad From F14c001 "  + ;
         			  " Where F14cUbiOri = '" + F10cEnct.F10cCodUbi + "'"
         			
		_Err = SqlExec(_Asql,_Sentencia,'F14cEnct')

		=SqlMoreresults(_Asql)
		If _Err < 0
		   _CodError = " Error buscando ubicaciones vacias " + message()
		   This.Errores
		   Return .F.
		EndIf

        Select F14cEnct
        Go Top
        If Cantidad > 0
			_Trobat = .T.
        EndIF


         *> Miramos que no este en el f14c001.----------------------------------------
         _Sentencia = "Select Count(*) As CAntidad From F16c001 "  + ;
         			  " Where F16cCodUbi = '" + F10cEnct.F10cCodUbi + "'"
         			
		_Err = SqlExec(_Asql,_Sentencia,'F16cEnct')

		=SqlMoreresults(_Asql)
		If _Err < 0
		   _CodError = " Error buscando ubicaciones vacias " + message()
		   This.Errores
		   Return .F.
		EndIf

        Select F16cEnct
        Go Top
        If Cantidad > 0
			_Trobat = .T.
        EndIF

        If _Trobat = .F.
	      *> Damos valor a la ubicación.---------------------------------------
    	  _UbiEnct = F10cEnct.F10cCodUbi

	      *>Calculamos lo que cabe.--------------------------------------------
	      This.CalBrecha

        EndIf
	
	   	  Select F10cEnct
	      Skip
	   EndDo

	Select F11t
	Skip
	If !Empty(This.UbiCodubi) .Or. !Empty(_CodError)

       If Empty(_CodError)
		   *> Ponemos el Flag de ocupada.------------------------------------------
		   _Sentencia = " Update F10c001 Set F10cEstGen = 'O' " + ;
	   				" Where F10cCodUbi = '" + This.UbiCodUbi + "'"
	   				
			_Err = SqlExec(_Asql,_Sentencia)

			=SqlMoreresults(_Asql)
			If _Err < 0
			   _CodError = " Error actualizando flag de ubicación " + message()
			   This.Errores
			   Return .F.
			EndIf
		EndIf

       Exit
	EndIf

EndDo	

ENDPROC
PROCEDURE optimizacion
*> Contaremos los palets que tenemos de ese TCL y artículo para buscarle
*> el lugar mas adecuado.------------------------------------------------
_Sentencia = " Select Count(*) As Cuantos From F14c001 " + ;
			 " Where F14cCodPro = '" + This.UbiCodPro + ;
			 "' And F14cCodArt = '" + This.UbiCodArt + "'"

*> Multilote .-----------------------------------------------------------
If F08cBrecha.F08cMulLot = 'N'
	_Sentencia = _Sentencia + "And F14cNumLot = '" + This.UbiNumLot + "'"
EndIf			

_Err = Sqlexec(_Asql,_Sentencia,'Optimizar')
=SqlMoreresults(_Asql)
If _Err<0
   _CodError = " Error leyendo buffer del almacén " + cr + ;
   			   " MENSAJE: " + message()
   This.Errores
  Return .F.

EndIf


*>la cantidad real tiene en cuenta el numero de remontes.----------------------
_NumRem = IIF(F08cBrecha.F08cNumRem=0,1,F08cBrecha.F08cNumRem)
_Cantireal = IIF(Optimizar.cuantos/_NumRem>0,Optimizar.cuantos/_NumRem,1)

*>Abrimos el fichero de relaciones.--------------------------------------------
_Sentencia = " Select * From F11r001 " + ;
             " Where F11rTamPal = '" + F08cBrecha.F08cTipPal + ;
			 "' And F11rNumPal >= " + Str(_CantiReal,8) + " Order By F11rNumPal,F11rTamUbi "
_Err = SqlExec(_Asql,_Sentencia,'F11rOptim')
=SqlMoreresults(_Asql)

If _Err<0
   _CodError = " Error abriendo maestro de relaciones "
   This.Errores
   Return .F.
EndIf


**> Recogemos a partir de la calidad la prioridad de ubicación.----
**>Abrimos el fichero de relaciones.--------------------------------------------
*_Sentencia = " Select * From F00q001 " + ;
*             " Where F00qCodCal = '" + F08cBrecha.F08cCalArt + ;
*			 "' And F00qCodPro = '" + _Procaot + "'"
*
*_Err = SqlExec(_Asql,_Sentencia,'F00QOptim')
*=SqlMoreresults(_Asql)
*
*If _Err<0
*   _CodError = " Error abriendo maestro de calidades "
*   This.Errores
*   Return .F.
*EndIf


*> Ordenar el fichero de ubicaciones con un campo de orden.-------
Select F10cEnct
Go Top
Do While !Eof()

   Do Case

      *>Caso que encontremos un hueco igual.----------------------
	  Case F11rOptim.F11rTamUbi=F10cEnct.F10cTamUbi
	  	   Replace F10cOrdEnt With 1
	
      *>Caso que encontremos un hueco mayor.----------------------
	  Case F11rOptim.F11rTamUbi<F10cEnct.F10cTamUbi
	  	   Replace F10cOrdEnt With 2

      *>Caso que encontremos un hueco menor.----------------------
	  Case F11rOptim.F11rTamUbi>F10cEnct.F10cTamUbi
	  	   Replace F10cOrdEnt With 3


   EndCase
   Select F10cEnct

*   *> Por tamaño minimo de calidad.-------------------------------
*   If F00qOptim.F00qTamabi >= F10cEnct.F10cTamUbi
*	  Replace F10cOrdEnt With 3
*   EndIf

   Skip
EndDo

   F10cConCoe = fntemp(1)
   Select F10cEnct
   Index On Str(F10cOrdEnt)+F10cTamUbi+F10cCodUbi To (F10cConCoe)
   Go Top
*> Ordenamos el fichero.-------------------------------------------
*_Fichero = 'F10cEnct'
*_Campo = 3
*_Campo1 = 4
*_oldSele = Alias()

*_lx1 = Sys(2015)
*_lx2=_tmp + _lx1

*Select (_Fichero)
*_lx3 = Field(_Campo)
*_lx4 = Field(_Campo1)
*Sort To (_lx2) On &_lx4/A,&_lx3/A
*set step on

*Zap
*Append From (_lx2)
*_lx2 = _lx2 + '.Dbf'
*Delete File (_lx2)

*>
*Select (_oldSele)
*Go Top

ENDPROC


EndDefine 
