* Incorpora Ventas del diario de ventas y clientes/Proveedores NUEVOS
*               MS3DIV
*
on error do st3err
select 1
use fich\DIVH001 order tag CODIGO alias 'DIVH'
scatter memvar blank
select 2
use fich\CUEN001 order tag CODIGO alias 'CUEN'
scatter memvar blank
select 3
use fich\PCON001 order tag CODIGO alias 'PCON'
scatter memvar blank
select 4
use fich\CUEA001 order tag CODIGO alias 'CUEA'
scatter memvar blank
select 5
use fich\CUAC001 order tag CODIGO alias 'CUAC'
scatter memvar blank
select 6
use fich\CCOS001 order tag CODIGO alias 'CCOS'
scatter memvar blank
select 8
use fich\CEFT001 order tag CODIGO alias 'CEFT'
scatter memvar blank
select 9
use fich\MOVC001 order tag CODIGO alias 'MOVC'
scatter memvar blank
select 10
use fich\NUMA001 order tag CODIGO alias 'NUMA'
scatter memvar blank
select 11
use fich\CDIA001 order tag CODIGO alias 'CDIA'
scatter memvar blank
select 12
use fich\LFAH001 order tag CODIGO alias 'LFAH'
scatter memvar blank
*
select 20
lx1=_fcomp+'divh93'
use (lx1) alias 'DIVHO'
go bottom
select 21
lx1=_fcomp+'cfah93'
use (lx1) order tag idx1 alias 'CFAH'
select 22
lx1=_fcomp+'lalh93'
use (lx1) order tag idx3 alias 'LALH'
store '' to INC_FRA,FIN_FRA
select DIVHO
_transacc=.T.
conf='N'
if V3_OK<>'S'
  =f3_sn(2,1,'Desea incorporar facturas de venta')
endif
if conf='S'

do while .not. bof() .and. V3_OK<>'S'
  scatter memvar
  select CFAH
  seek m.IVA_ANY+str(m.IVA_NUM)
  if !eof()
    scatter memvar
  endif
* ------------- Traspasar las líneas
  select LFAH
  seek m.IVA_ANY+'  '+str(m.IVA_NUM)
  m.ALB_LIN=iif(eof(),0,ALB_LIN)
  select LALH
  seek m.IVA_ANY+str(m.IVA_NUM)
  do while .not. eof() .and. m.IVA_ANY+str(m.IVA_NUM)=IVA_ANY+str(IVA_NUM) 
    m.IVA_NUM=m.CFT_FRA
    m.ART_PVP=m.ART_PVP1
    m.DIA_ANY=1900+val(m.IVA_ANY)
    m.ART_ART=ART_ART
    m.ART_DES=ART_DES
    m.MST_CANT=MST_CANT
    m.MST_VL=MST_VL
    m.ALB_LIN=m.ALB_LIN+2
    insert into LFAH from memvar
    skip
  enddo
  select CFAH
* ------------- Gestionar el diario de ventas
  m.TEF_TEF=right('00'+ltrim(str(m.TEF_TEF)),2)
  m.CUE_TIP='Nacional'
  store 0 to m.DIA_AS,m.TEF_POR,m.DV_POR,m.TEF_DPPE,m.TEF_DPP1,m.TEF_DPP2,m.TEF_DPP3,m.DV_DTE,m.DV_DT1,m.DV_DT2,m.DV_DT3,m.DIA_IDIV
  m.CCO_CCO=''
  m.TEF_IVAP1=m.TEF_IMP1
  m.TEF_IVAP2=m.TEF_IMP2
  m.TEF_IVAP3=m.TEF_IMP3
  m.TEF_BE=0
  do case
  case m.TEF_IVAP1=0 .and. m.TEF_B1<>0
    m.TEF_BE=m.TEF_B1
    m.TEF_B1=0
  case m.TEF_IVAP2=0 .and. m.TEF_B2<>0
    m.TEF_BE=m.TEF_B2
    m.TEF_B2=0
  case m.TEF_IVAP3=0 .and. m.TEF_B3<>0
    m.TEF_BE=m.TEF_B3
    m.TEF_B3=0
  endcase
  m.FRA_TME=m.TEF_BE
  m.FRA_TM1=m.TEF_B1
  m.FRA_TM2=m.TEF_B2
  m.FRA_TM3=m.TEF_B3
  m.CTA_CTA=str(m.CUE_CTA,4)+str(m.CUE_SCTA)
  do XCDIA with 'ALTA',.T.
  tiva=m.TEF_IVAP1+m.TEF_IVAP2+m.TEF_IVAP3+m.TEF_REP1+m.TEF_REP2+m.TEF_REP3
  m.TOT_FRA=m.TEF_BE+m.TEF_B1+m.TEF_B2+m.TEF_B3+tiva
  totfra=m.TOT_FRA
  select MOVC
  store 0 to m.DIA_IMPD,DIA_IMPH
  m.MVC_OK=' '
  m.TEF_DOC=trim(m.IVA_ANY)+'/'+ltrim(str(m.IVA_NUM))
  m.FIN_FRA=TEF_DOC
  m.INC_FRA=iif(empty(INC_FRA),TEF_DOC,INC_FRA)
  m.DIA_IMPD=m.TOT_FRA
  m.DIA_TA='FC  '
  m.DIA_LIN=2
  m.CLV_CLV=50
  select PCON
  seek m.CTA_CTA
  select DIVH                                              && Diario Histórico
  set deleted off
  set order to 'BAJA'
  seek .T.
  if eof()
    insert into DIVH from memvar
  else
    gather memvar
    recall
  endif
  set deleted on
  set order to 'CODIGO'
  wait window 'Factura '+m.IVA_ANY+str(m.IVA_NUM)
  m.CCO_CCO=space(len(m.CCO_CCO))
  insert into MOVC from memvar                             && Cliente
  m.DIA_IMPD=0
  if tiva<>0
    m.DIA_LIN=m.DIA_LIN+2
    m.DIA_IMPH=tiva
    m.CTA_CTA='4770'+str(0)
    insert into MOVC from memvar
  endif
  m.DIA_LIN=18
  m.DIA_IMPD=0
  CN1_CTA='7000'+str(0)
  m.DIA_IMPH=m.FRA_TME+m.FRA_TM1+m.FRA_TM2+m.FRA_TM3
  m.DIA_LIN=m.DIA_LIN+2
  m.CTA_CTA=m.CN1_CTA
  insert into MOVC from memvar                             && 1 Contrapartida
  do z3cuen with 'G'
  store 0 to m.TEF_IMPD,m.DIA_IMPH
  m.CTA_CTA=divh.CTA_CTA
  m.TEF_DOC=m.IVA_ANY+str(m.IVA_NUM,6)
  m.DIA_LIN=2
  total=m.TOT_FRA
  store 0 to nd1,nd2
  m.TEF_FRA=m.TEF_DOC
  m.TEF_NEF=max(1,m.TEF_NEF)
  =ycalvto(TOT_FRA,TEF_FDOC,0,TEF_NEF,TEF_APL,TEF_PER,TEF_DP1,TEF_DP2,TEF_DP3,TEF_VACI,TEF_VACF,0)
  select CEFT                                              && Cartera de efectos
  set deleted off
  set order to 'BAJA'
  for nd=1 to m.TEF_NEF
    if !empty(vto(nd,1)) .and. vto(nd,2)<>0
      m.TEF_NUM=nd
      m.TEF_VTO=vto(nd,1)
      store vto(nd,2) to m.DIA_IMPD,m.TEF_IMP
      total=total-m.DIA_IMPD
      seek .T.
      if eof()
        insert into CEFT from memvar
      else
        gather memvar
        recall
      endif
      if abs(m.TEF_IMP)>=nd2
        nd1=recno()
        nd2=abs(m.TEF_IMP)
      endif
    endif
  endfor
  set deleted on
  set order to 'CODIGO'
  if total<>0 .and. nd1>0
    go nd1
    repl TEF_IMP with TEF_IMP+total,DIA_IMPD with DIA_IMPD+total
  endif
  select MOVC
  scatter memvar blank
  select CDIA
  scatter memvar blank
  select DIVHO
  repl V3_OK with 'S'
  skip -1
enddo
_transacc=.F.

endif
*
* -------------- Cerrar ficheros
select 1
use
select 2
use
select 3
use
select 4
use
select 5
use
select 6
use
select 7
use
select 8
use
select 9
use
select 10
use
select 11
use
select 12
use
*
select 20
use
select 21
use
select 22
use
select SYSPRG
*
return

