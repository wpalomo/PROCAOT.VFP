* Configuración del programa por cambio de empresa
*              ST3CONF
*
_ali=alias()
select EMPRESA
_em=right('000'+ltrim(str(EMP_COD)),3)
_nem=EMP_NOM
_cif=EMP_DNI
_empfich=trim(EMP_FICH)
_empacon=EMP_ACON
_empmcon=EMP_MCON
_empnomc=EMP_NOMC
_empdir=EMP_DIR
_emppob=EMP_POB
_empcp=EMP_CP
_emppro=EMP_PRO
_emptel=EMP_TEL
_empfax=EMP_FAX
_empemai=EMP_EMAI
_emptaxf=EMP_TAXF
_empivatf=EMP_IVATF
_ccontrol=EMP_CCO
_cacceso=EMP_CAC
_empeuro=EMP_EURO
_ccoaut=EMP_CCAUT
_control_coste=iif(EMP_CCOS=1,.T.,.F.)
_fmte=iif(_empeuro=1,'@Z 9999,999.99','@Z 999,999,999')    && 11 carácteres
_fmte2=iif(_empeuro=1,'@Z 99,999,999.99','@Z 99999,999,999')    && 13 carácteres

_iv(1,1)=EMP_IVA1
_iv(2,1)=EMP_IVA2
_iv(3,1)=EMP_IVA3
_iv(4,1)=EMP_IVA1
_iv(5,1)=EMP_IVA2
_iv(6,1)=EMP_IVA3
_iv(1,2)=EMP_RE1
_iv(2,2)=EMP_RE2
_iv(3,2)=EMP_RE3
_iv(4,2)=EMP_RE1
_iv(5,2)=EMP_RE2
_iv(6,2)=EMP_RE3

_ctair   =iif(empty(CT_IR)   ,'4770         0',CT_IR   +iif(empty(SC_IR)   ,'',str(val(SC_IR))))
_ctais   =iif(empty(CT_IS)   ,'4720         0',CT_IS   +iif(empty(SC_IS)   ,'',str(val(SC_IS))))
_ctairi  =iif(empty(CT_IRI)  ,'4771         0',CT_IRI  +str(val(SC_IRI)))
_ctaisi  =iif(empty(CT_ISI)  ,'4721         0',CT_ISI  +str(val(SC_ISI)))
_ctavpor =iif(empty(CT_VPOR),'',CT_VPOR+iif(empty(SC_VPOR),'',str(val(SC_VPOR))))
_ctavdpp =iif(empty(CT_VDPP),'',CT_VDPP+iif(empty(SC_VDPP),'',str(val(SC_VDPP))))
_ctacpor =iif(empty(CT_CPOR),'',CT_CPOR+iif(empty(SC_CPOR),'',str(val(SC_CPOR))))
_ctacdpp =iif(empty(CT_CDPP),'',CT_CDPP+iif(empty(SC_CDPP),'',str(val(SC_CDPP))))
_ctairpf =iif(empty(CT_IRPF) ,'4750         0',CT_IRPF +iif(empty(SC_IRPF) ,'',str(val(SC_IRPF))))
_cta4311 =iif(empty(CT_4311) ,'4311         0',CT_4311 +iif(empty(SC_4311) ,'',str(val(SC_4311))))
_cta5208 =iif(empty(CT_5208) ,'5208         0',CT_5208 +iif(empty(SC_5208) ,'',str(val(SC_5208))))
_clvis   =iif(CL_IS=0,50,CL_IS)
_clvir   =iif(CL_IR=0,51,CL_IR)
_clvrem  =iif(CL_REM=0,52,CL_REM)
_clvreme =iif(CL_REME=0,53,CL_REME)
_clvcom  =iif(CL_COM=0,54,CL_COM)
_clvimp  =iif(CL_IMP=0,55,CL_IMP)
_clvlea  =iif(CL_LEA=0,56,CL_LEA)
_clvcia  =iif(CL_CIA=0,99,CL_CIA)
_fpe_cabecera=iif(EMP_FPE_CB=1,.T.,.F.)
_clot=iif(EMP_CLOT=1,.T.,.F.)
_com_cabecera=iif(EMP_COM_CB=1,.T.,.F.)
_doc_cabecera=iif(EMP_DOC_CB=1,.T.,.F.)
_dsk_remesa=iif(EMP_DSKREM=1,.T.,.F.)
_emp_cl=iif(EMP_CL=1,.T.,.F.)
_fondo=trim(EMP_FONDO)
_digfam=EMP_DIGFAM
_almdef=EMP_ALMDEF
_rec_diaas=EMP_RECAS
_rec_fras=EMP_RECFRA
if !empty(_fondo)
  _screen.picture=_fondo
endif
select alias(_ali)
*
return
