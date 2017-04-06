* Programa que permite definir los menús
*            SY3MENU
*
define popup _sy3men scroll font 'Ms LineDraw',9
use XLMEN order tag idx1 excl in 0
select XLMEN
zap
m_emp=space(3)
m_usr=' '
m.PRG_DES=space(30)
select SYSMEN
go top
nd_menu=1
do form sy3menu
select XLMEN
use
select SYSMEN
release popup _sy3men
do st3opm
*
return .T.
