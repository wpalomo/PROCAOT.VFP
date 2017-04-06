
*> Actualización de modificaciones específicas en SYSPRG.
*> Recibe SYSPRGUPD y actualiza / crea SYSPRG.

Local oPrg

Set Step On

Select 0
Use SYSPRG Order 1

Select 0
Use SYSPRGUPD

Go Top
Do While !Eof()
	Scatter Name oPrg Memo

	Select SYSPRG
	Locate For Prg_Prog==oPrg.Prg_Prog
	If !Found()
		Append Blank
	EndIf
	Gather Name oPrg Memo

	Select SYSPRGUPD
	Skip
EndDo

Use In (Select ("SYSPRG"))
Use In (Select ("SYSPRGUPD"))
