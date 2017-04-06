_Asql = SqlConnect()

_Sentencia="Select Sum(F34fMaxKgs) As MaxKgs,f34fcodart From F34f001 " + ;
           "Where F34fCodPro = '000179'" + ;
           " And F34fAnyo = '2002'" + ;
           " And F34fmes = '05'" + ;
		   " And F34fSitStk = '1000'" + ;
           " Group By F34fCodArt, F34fSitStk, F34fQuince " + ;
           " Order By F34fCodArt, F34fSitStk, F34fQuince " 

_Ok = SqlExec(_Asql,_Sentencia,'F172')

Select F172
store 0 to cantidad
Go Top
Do While !Eof()

	_Sentencia = " Select F08cpcoste,F08cPesUni,F08cVolUni from F08c001 Where F08cCodPro = '000179'" +  ;
				 " And F08cCodArt = '" + F172.F34fCodArt + "'"
	_Ok = SqlExec(_Asql,_Sentencia,'F08cdat')

	Cantidad = Cantidad + ((F172.MaxKgs*f08cdat.F08cpcoste)/F08cdat.F08cPesUni)
				 


	Select F172
	Skip
EndDo
set step on
Wait Window Str(Cantidad)
Wait Window str(Round(Cantidad, 0))