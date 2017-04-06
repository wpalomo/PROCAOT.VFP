
*> Save this content to a file called VFPSRV.PRG
 
#define TIMERINT  5     && # of seconds for timer interval. Also syncs to TIMERINT seconds for time of day
 
Public oService

oService = NewObject("vfpsrv")
oService.logstr("START", Curdir())

If _VFP.StartMode > 0			&& If we’re running as an EXE
      Read Events				&& Message loop
EndIf
 
Define Class vfpsrv As Form

	Procedure Init
		This.logstr("INIT", Curdir())
		This.AddObject("mytimer", "mytimer")
		With This.mytimer
			.Enabled = .T.
			.Interval = 5000					&& start it in two seconds
		EndWith
	EndProc

	Procedure Destroy
		This.logstr("DESTROY", Curdir())
	EndProc

	Procedure logstr(cMode As String, cStr As String)
		cStr = Transform(Datetime()) + Space(1) + cMode + Space(1) + cStr + Chr(13) + Chr(10)
		StrToFile(cStr, "D:\INTERCAMBIO\vfpsrv.log", .T.)
	EndProc

EndDefine

Define Class mytimer As Timer

	Procedure Timer
			oService.logstr("EXEC", Curdir())
            Clear Program
            dtNow = Datetime()  && read datetime() and seconds() close to the same instant
            nSec = Seconds()
            nsec = Ceiling((nSec + .5) / TIMERINT) * TIMERINT		&& target is # of seconds since midnight + a little for calculation time
            dtTarget = Dtot(Date()) + nSec
            This.Interval = (dtTarget - dtNow) * 1000
	EndProc

EndDefine
