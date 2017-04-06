set step on
_sentencia="select top 1* from f16c001"
=sqlexec(_asql,_sentencia,'cSit')

_sentencia="select * from f10c001 where f10cpicksn in ('S','U')"
=sqlexec(_asql,_sentencia,'cUbi')
	
_sentencia="select distinct f24lcodart,f24lnumlot,f24lcandoc,f24lsitstk from f24l001"
=sqlexec(_asql,_sentencia,'cDoc')

Select cDoc
Go Top
Do While !Eof()

	
	Select cUbi
	Skip
	_Ubicacion = cUbi.F10cCodUbi
	
	
	IdOcup = Ora_NewOCID()
	_NumPal = Ora_NewPal()
		
	Select cSit
	Replace F16cCodArt With cDoc.F24lCodArt
	Replace F16cCanFis With 1000
	Replace F16cCanRes With 0
	Replace F16cCodUbi With _Ubicacion
	Replace F16cNumLot With cDoc.F24lNumLot
	Replace F16cSitStk With '1000'
	Replace F16cNumPal With _NumPal
	Replace F16cIdOcup With IdOcup
	
	=f3_instun('F16c','cSit') 
	
	 
	Select cDoc
	Skip
EndDo