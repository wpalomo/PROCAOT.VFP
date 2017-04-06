*>

Local cSCX

set step on

cSCX = Sys(2000, "pntw/*.scx")

Do While !Empty(cSCX)
   Wait Window "Procesando: " + cSCX NoWait

   Use pntw/&cSCX Exclusive
   Replace All Methods With ""
   Use

   cSCX = Sys(2000, "*.scx", 0)
EndDo

Wait Clear
