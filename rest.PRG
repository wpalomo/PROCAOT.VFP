_psql=.T.
on key
on error do nada
_transac=.F.
=sqlrollback(_ASql)
clear events
close database
close all

clear all
clear program
clear
on error
close debug
set debug off
set debug on
on key
set sysmenu to default
_screen.closable=.t.
_psql=.F.
cancel

Function nada
*
a=0
return
