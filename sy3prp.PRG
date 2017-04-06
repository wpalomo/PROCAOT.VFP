* Programa de cambio de propietario
*        SY3USR
*
private _r1

*>
*> Seleccionar el propietario por defecto.
=SqlExec(_aSql,'Select * From F01p' + _em + ' Order By F01pCodigo','Consulta')
=SqlMoreResults(_aSql)
Select Consulta

ntot=reccount()
ntot = iif(ntot==0, 1, ntot)
dimension prp_or(ntot), prp_ds(1)
store '' to prp_or, prp_ds
ntot=0

go top
do while .not. eof()
  ntot=ntot+1
  prp_or(ntot) = F01pCodigo + ' ' + F01pDescri
  skip
enddo

if ntot=0
  =f3_sn(1, 2, 'No existe ningún Propietario', 'Creado propietario')
  ntot=1
else
   Go Top
   _Procaot = F01pCodigo
   _ProDesc = F01pDescri
endif

if ntot > 1
  _ok=.F.
  =st3trasp('Cambio de propietario', ;
            'prp_or', ;
            ntot, ;
            'prp_ds', ;
            1,,,,,,, ;
            'Propietarios posibles', ;
            'Propietario por defecto')
  do case
  case _ok .and. !empty(prp_ds(1))
    _Procaot = left(prp_ds, 6)
    _ProDesc = Subs(prp_ds, 8)
  case empty(_alma)
*   cancel
  endcase
endif

*>
Do Sy3Caption
Select SYSPRG
Return .T.
