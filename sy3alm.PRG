* Programa de cambio de almacén
*        SY3USR
*
private _r1

*>
*> Seleccionar el almacén de trabajo.
=SqlExec(_aSql,'Select * From F02c' + _em + ' Order By F02cCodAlm','Consulta')
=SqlMoreResults(_aSql)
Select Consulta

ntot=reccount()
ntot = iif(ntot==0, 1, ntot)
dimension alm_or(ntot),alm_ds(1)
store '' to alm_or,alm_ds
ntot=0
go top
do while .not. eof()
  ntot=ntot+1
  alm_or(ntot)=f02ccodalm+' '+f02cdescri
  skip
enddo

if ntot=0
  =f3_sn(1,2,'No existe ningún almacén', 'Creado almacén 01')
  Insert Into F02c001 (F02cCodAlm, F02cDescri) Values('01', 'Almacén 01')
  ntot=1
endif

if ntot=1
  go top
  _alma=f02ccodalm
  _almades=f02cdescri
else
  _ok=.F.
  =st3trasp('Cambio de almacén', ;
            'alm_or', ;
            ntot, ;
            'alm_ds', ;
            1,,,,,,, ;
            'Almacenes posibles', ;
            'Almacén de trabajo')
  do case
  case _ok .and. !empty(alm_ds(1))
    _alma=left(alm_ds,4)
    _almades=subs(alm_ds,6)
  case empty(_alma)
    cancel
  endcase
endif

*>
Do Sy3Caption
Select SYSPRG
Return .T.
