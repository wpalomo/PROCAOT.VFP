*>

Local cvcx

set step on

cvcx = Sys(2000, "*.vcx")

Do While !Empty(cvcx)
   Wait Window "Procesando: " + cvcx NoWait

   Use &cvcx Exclusive
   Replace All Methods With ""
   Use

   cvcx = Sys(2000, "*.vcx", 0)
EndDo

Wait Clear
