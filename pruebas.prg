set step on
=SqlExec(_Asql,'Select * From F14c001','CNFDOCMP')

Select CNFDOCMP
Go Top
select count(distinct f14cnumdoc) from CNFDOCMP into cursor ffff


Return
