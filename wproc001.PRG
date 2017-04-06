PROCEDURE fwllsfi
   PARAMETER ppp
   SELE (tr_act)
   LOCA FOR preg=ppp
   STORE LEN(TRIM(mascaraget)) TO w_ancho,w_ample
   STORE long+w_ancho+3 TO w_ancho
   IF w_ancho<40
      w_ancho=40
   ENDIF
   IF w_ancho>78
      w_ancho=78
   ENDIF
   DEFINE WINDOW panta FROM 5,0 TO 18 , w_ancho ;
      TITLE "PANTALLA DE CONSULTA";
      PANEL;
      NOCLOSE;
      FLOAT;
      NOGROW;
      SHADOW;
      NOZOOM;
      COLOR (colwin)
   SELE (NORMAL)
   STORE SYS(21) TO w_ord
   SELE (tr_act)
   STORE RECNO() TO w_antes1
   STORE funcionget TO w_f
   ACTIVATE WINDOW wmsgdis SAME
   CLEAR
   DO WHILE .T.
      SELE (tr_act)
      w_qu="C"
      IF alfabetico="A"
         w_qu=" "
         SET FUNCTION 9 TO ";"
         SET CURSOR ON
         @ 0,2 SAY "Ordenaci¢n del Listado : [A]lfab‚tico,[C]¢digo <F9>=Fin " GET w_qu PICTURE "!" VALID(w_qu $ "AC ") DEFAULT "C"
         READ
         CLEAR GETS
         SET CURSOR OFF
         CLEAR
         IF LASTKEY()=-8
            anter=1
            EXIT
         ENDIF
      ENDIF
      IF w_qu="A"
         w_plan=REPLICATE("X",long)
         STORE posa TO w_pos
      ELSE
         w_plan=TRIM(mascaraget)
         STORE posn TO w_pos
      ENDIF
      w_cod=SPACE(LEN(TRIM(w_plan)))
      SET CURSOR ON
      STORE TRIM(w_plan) TO w_pict
      @ 0,2  SAY "C¢digo inicial: " GET w_cod PICTURE(w_pict) FUNCTION ALLTRIM(w_f)
      READ
      IF tipo=="R" .AND. .NOT. EMPTY(w_cod)
         STORE SPACE(LEN(TRIM(mascaraget))-LEN(TRIM(w_cod)))+TRIM(w_cod) TO w_cod
      ENDIF
      IF tipo=="0" .AND. .NOT. EMPTY(w_cod)
         STORE REPLICATE("0",LEN(mascaraget)-LEN(TRIM(w_cod)))+TRIM(w_cod) TO w_cod
      ENDIF
      CLEAR
      SELE (tr_act)
      STORE TRIM(MASTER) TO w_aux
      STORE TRIM(indice) TO w_ind
      IF USED(w_aux)
         SELE (w_aux)
         STORE SYS(21) TO w_ord
      ELSE
         DO fselec WITH w_aux,w_ind,"0"
         STORE SYS(21) TO w_ord
      ENDIF
      STORE 0 TO w_antes
      IF .NOT. BOF() .AND. .NOT. EOF()
         STORE RECNO() TO w_antes
      ENDIF
      IF w_qu="A"
         SET ORDER TO 2
      ELSE
         SET ORDER TO 1
      ENDIF
      GO TOP
      w_previo=TRIM(&tr_act->tipreg)
      SELE (tr_act)
      IF LEFT(tipreg,1)=="@"
         STORE RECNO() TO w_aaa
         STORE TRIM(tipreg) TO w_tp
         STORE RIGHT(w_tp,LEN(w_tp)-1) TO w_tp
         STORE VAL(w_tp) TO w_ddd
         GO w_ddd
         SELE (NORMAL)
         B=FIELD(&tr_act->preg)
         B=&b
         SELE (tr_act)
         STORE B TO w_prim
         GO w_aaa
      ELSE
         STORE TRIM(tipreg) TO w_prim
         IF LEFT(w_prim,1)=="&"
            w_prim=STRTRAN(w_prim,"&","")
            STORE &w_prim TO w_prim
         ENDIF
      ENDIF
      STORE w_prim TO w_previo
      SELE (w_aux)
      IF LEN(TRIM(w_previo))#0
         w_filtro=FIELD(1)
         SET FILTER TO &w_filtro==w_previo
      ENDIF
      STORE TRIM(w_previo+w_cod) TO w_pri
      SET NEAR ON
      IF LEN(TRIM(w_pri))#0
         SEEK w_pri
      ELSE
         GO TOP
      ENDIF
      SET NEAR OFF
      SELE (tr_act)
      STORE TRIM(mascaraget) TO w_vf
      IF SUBSTR(x2,1,2)=="XX"
         txt1="C¢digo"
         txt2="Descripci¢n"
      ELSE
         txt1="C¢digo"
         txt2=x2
      ENDIF
      SELE (w_aux)
      cam1=FIELD(&tr_act->posn)
      cam2=&tr_act->registro
      ACTIVATE WINDOW wtefu SAME
      @ 0,0 SAY LEFT("F8 Para capturar c¢digo³F9 Para salir³"+SPACE(80),80)
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
      IF !EMPTY(w_f)
         STORE "@"+TRIM(w_f)+" "+TRIM(w_vf) TO w_vf
      ENDIF
      BROWSE WINDOW panta;
         FIELDS codigo = TRAN(&cam1,w_vf),;
         (cam2) :H=txt2;
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOMODIFY;
         NOFOLLOW;
         FREEZE codigo
      SELE (w_aux)
      IF READKEY()=270 .OR. READKEY()=14
         B=&cam1
         KEYBOARD SUBSTR(B,1,w_ample)
         modifi=1
      ENDIF
      SET FILTER TO
      SET ORDER TO &w_ord
      SELE (tr_act)
      GO w_antes1
      SELE (w_aux)
      IF w_antes#0
         GO w_antes
      ENDIF
      EXIT
   ENDDO
   RELEASE WINDOW panta
   SELE (NORMAL)
   DO fwltecfun WITH 5
   RETURN
PROCEDURE srt22w
   SELE (tr_act)
   STORE RECNO() TO w_antes1
   w_previo=TRIM(tipreg)
   IF LEFT(w_previo,1)=="@"
      STORE RECNO() TO w_ccc
      GO TOP
      LOCATE FOR preg=VAL(RIGHT(w_previo,LEN(w_previo)-1))
      DO CASE
      CASE tipo="D"
         B=DTOC(mvar(preg))
      CASE tipo="N"
         B=STR(mvar(preg),LEN(TRIM(mascaraget)),ndec)
      OTHER
         B=mvar(preg)
      ENDCASE
      STORE ALLTRIM(tipreg)+B+idioma TO w_clav
      DO fselec WITH MASTER,indice,"0"
      SEEK w_clav
      STORE TRIM(f25ctxabre) TO w_camp
      STORE LEFT(w_camp,4) TO w_fich
      DO fselec WITH "FICHEROS","INDFICHE","0"
      STORE "" TO w_ind
      GO TOP
      SEEK UPPER(w_fich)
      IF LEN(TRIM(nombreind1))#0
         STORE TRIM(nombreind1) TO w_ind
      ENDIF
      DO fselec WITH w_fich,w_ind,"0"
      STORE 1 TO w_posn
      SELE (tr_act)
      GO w_ccc
   ENDIF
   DO WHILE .T.
      DEFINE WINDOW panta FROM 5,0 TO 18,55 ;
         TITLE "PANTALLA DE CONSULTA";
         PANEL;
         NOCLOSE;
         FLOAT;
         NOGROW;
         SHADOW;
         NOZOOM;
         COLOR (colwin)
      SELE (tr_act)
      STORE ALIAS() TO w_tr
      w_qu="C"
      w_plan=mascaraget
      w_cod=SPACE(LEN(TRIM(w_plan)))
      SET FUNCTION 9 TO ""
      ACTIVATE WINDOW wmsgdis SAME
      CLEAR
      SET CURSOR ON
      @ 0,1 SAY "C¢digo inicial: " GET w_cod PICTURE(TRIM(w_plan))
      READ
      CLEAR
      ACTIVATE WINDOW (pregunta) SAME
      SET CURSOR OFF
      IF LASTKEY()=-8
         anter=1
         EXIT
      ENDIF
      SELE (w_fich)
      STORE 0 TO w_antes
      IF .NOT. BOF() .AND. .NOT. EOF()
         STORE RECNO() TO w_antes
      ENDIF
      SET ORDER TO 1
      GO TOP
      STORE TRIM(w_cod) TO w_pri
      IF .NOT. EMPTY(w_pri)
         SEEK w_pri
      ELSE
         GO TOP
      ENDIF
      txt2="Descripci¢n"
      txt1="C¢digo"
      cam1=FIELD(w_posn)
      cam2=w_camp
      ACTIVATE WINDOW wmsgdis SAME
      CLEAR
      @ 0,0 SAY LEFT("F8 Para capturar c¢digo³F9 Para salir³"+SPACE(80),80)
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
      BROWSE WINDOW panta;
         FIELDS (cam1):H=txt1,;
         (cam2):H=txt2;
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOMODIFY;
         NOFOLLOW;
         FREEZE (cam1)
      SELE (w_fich)
      IF READKEY()=270 .OR. READKEY()=14
         A=EVALUATE(cam1)
         anter=0
         modifi=1
      ELSE
         anter=1
      ENDIF
      USE
      GO w_antes1 IN (tr_act)
      EXIT
   ENDDO
   RELEASE WINDOW wppide, panta
   ACTIVATE WINDOW (pregunta) SAME
   SELE (tr_act)
   RETURN
PROCEDURE srt12n
   PARAMETERS x_titulo,x_filtro,x_campos,x_recupe
   x_filt=""
   x_index=""
   SELE (NORMAL)
   IF !EMPTY(x_filtro)
      x_x=1
      x_tc=fwextrac(x_x,x_filtro,":")
      DO WHILE !EMPTY(x_tc)
         x_tcc=FIELD(VAL(x_tc))
         IF EMPTY(x_filt)
            x_filt=x_tcc+"==mvar("+x_tc+")"
         ELSE
            x_filt=x_filt+" .and. "+x_tcc+"==mvar("+x_tc+")"
         ENDIF
         x_x=x_x+1
         x_tc=fwextrac(x_x,x_filtro,":")
      ENDDO
   ENDIF
   x_rec=""
   IF !EMPTY(x_recupe)
      x_x=1
      x_tc=fwextrac(x_x,x_recupe,":")
      DO WHILE !EMPTY(x_tc)
         x_rec=x_rec+x_tc+"/"+x_tc+":"
         x_x=x_x+1
         x_tc=fwextrac(x_x,x_recupe,":")
      ENDDO
   ENDIF
   x_index=TAG(1)
   DO srt12gen WITH NORMAL,x_index ,x_titulo,x_filt,   x_campos,x_rec 
   SELE (NORMAL)
   SET ORDER TO TAG &ind_nor
   SELE (tr_act)
   DO fwdispwr
   RETURN
PROCEDURE srt12w
   SELE (NORMAL)
   STORE SYS(21) TO w_ord
   SELE (tr_act)
   STORE LEN(TRIM(mascaraget)) TO w_ancho,w_ample
   STORE long+w_ancho+4 TO w_ancho
   w_ancho=IIF(w_ancho<40,40,w_ancho)
   w_ancho=IIF(w_ancho>74,74,w_ancho)
   STORE RECNO() TO w_antes1
   STORE funcionget TO w_f
   SET FUNCTION 9 TO ";"
   w_anteri=WOUTPUT()
   ACTIVATE WINDOW wmsgdis SAME
   CLEAR
   DO WHILE .T.
      SELE (tr_act)
      w_qu="C"
      IF alfabetico=="A"
         w_qu="C"
         SET CURSOR ON
         @ 0,2 SAY "Ordenaci¢n del Listado : [A]lfab‚tico,[C]¢digo <F9>=Fin " GET w_qu PICTURE "!" VALID(w_qu $ "AC ") COLOR (colmsg)
         READ
         IF LASTKEY()=-8
            anter=1
            EXIT
         ENDIF
      ENDIF
      IF w_qu="A"
         w_plan=REPLICATE("X",long)
         STORE posa TO w_pos
      ELSE
         w_plan=TRIM(mascaraget)
         STORE posn TO w_pos
      ENDIF
      w_cod=SPACE(LEN(TRIM(w_plan)))
      SET CURSOR ON
      STORE TRIM(w_plan) TO w_pict
      CLEAR
      @ 0,2  SAY "C¢digo inicial: " GET w_cod PICTURE(w_pict) FUNCTION ALLTRIM(w_f) COLOR (colmsg)
      READ
      CLEAR
      IF LASTKEY()=-8
         anter=1
         EXIT
      ENDIF
      IF tipo=="R" .AND. !EMPTY(w_cod) .AND. w_qu=="C"
         w_cod=PADL(A,LEN(TRIM(mascaraget))," ")
      ENDIF
      IF tipo=="0" .AND. !EMPTY(w_cod) .AND. w_qu=="C"
         w_cod=PADL(A,LEN(TRIM(mascaraget)),"0")
      ENDIF
      SET CURSOR OFF
      CLEAR
      SELE (tr_act)
      STORE TRIM(MASTER) TO w_aux
      STORE TRIM(indice) TO w_ind
      STORE TRIM(indice1) TO w_ind1
      DO fselec WITH w_aux,w_ind,"0"
      STORE 0 TO w_antes
      IF .NOT. BOF() .AND. .NOT. EOF()
         STORE RECNO() TO w_antes
      ENDIF
      w_indice=IIF(w_qu=="A",w_ind1,w_ind)
      w_previo=""
      w_poster=""
      w_filtro=""
      SELE (tr_act)
      IF !EMPTY(idioma)
         STORE TRIM(idioma) TO w_poster
      ENDIF
      IF !EMPTY(tipreg)
         w_previo=tipreg
         j_campz = 0
         IF LEFT(w_previo,1)=="@"
            STORE RECNO() TO w_ccc
*           GO VAL(SUBS(w_previo,2))
            IF AT(",",w_previo)>0
               j_campf = VAL(SUBS(w_previo,2,AT(",",w_previo)-2))
               j_campz = VAL(SUBS(w_previo,AT(",",w_previo)+1))
            ELSE
               j_campf = VAL(SUBS(w_previo,2))
            ENDIF
            LOCATE FOR preg=j_campf
            DO CASE
            CASE tipo="D"
               B=DTOC(mvar(preg))
            CASE tipo="N"
               B=STR(mvar(preg),LEN(TRIM(mascaraget)),ndec)
            OTHER
               B=mvar(preg)
            ENDCASE
            STORE B TO w_previo
            STORE posn TO w_pre_cam
            GO w_ccc
            SELE (w_aux)
         ELSE
            IF LEFT(w_previo,1)=="&"
               w_previo=STRTRAN(w_previo,"&","")
               STORE &w_previo TO w_previo
            ENDIF
         ENDIF
         IF !EMPTY(w_previo)
            SELE (w_aux)
            w_filtro=FIELD(IIF(!EMPTY(j_campz),j_campz,1))+"='"+ALLTRIM(w_previo)+"'"
         ENDIF
         IF !EMPTY(w_poster)
            SELE (w_aux)
            IF !EMPTY(w_filtro)
               w_filtro=w_filtro+".and."
            ENDIF
            w_filtro=w_filtro+FIELD(w_pos+1)+"='"+ALLTRIM(w_poster)+"'"
         ENDIF
         SELE (tr_act)
      ENDIF
      STORE TRIM(w_previo+w_cod) TO w_pri
      STORE TRIM(mascaraget) TO w_vf
      IF SUBSTR(x2,1,2)=="XX"
         txt1="C¢digo"
         txt2="Descripci¢n"
      ELSE
         txt1="C¢digo"
         txt2=x2
      ENDIF
      SELE (w_aux)
      STORE 0 TO X
      FOR i=1 TO FCOUNT()
         IF TRIM(UPPER(FIELD(i)))=TRIM(UPPER(&tr_act->registro))
            EXIT
         ENDIF
      ENDFOR
      SELE (tr_act)
      w_cam=ALLTRIM(STR(posn,3,0))+"/"+txt1+":"+ALLTRIM(STR(i,3,0))+"/"+txt2
      w_rec=ALLTRIM(STR(posn,3,0))+"/"+ALLTRIM(STR(preg,3,0))
      DO srt12gen WITH w_aux,w_indice,"CONSULTA",w_filtro,w_cam,w_rec,.T.,.T.,.T.,.T.,w_pri
      IF LASTKEY()=-8
         anter=1
      ENDIF
      SET ORDER TO &w_ord
      SELE (tr_act)
      GO w_antes1
      SELE (w_aux)
      IF w_antes#0
         GO w_antes
      ENDIF
      EXIT
   ENDDO
   ACTIVATE WINDOW (w_anteri) SAME
   SELE (tr_act)
   DO fwtecfun WITH 0
   RETURN
PROCEDURE srt12wal
   PARAMETERS x_ficher,x_indice,x_titulo,x_filtro,x_campos,x_recupe
   DO fselec WITH x_ficher,x_indice,"0"
   SET INDEX TO &x_indice
   SELE (x_ficher)
   STORE KEY(1) TO x_orden
   STORE fntemp(1) TO x_index
   IF !EMPTY(x_filtro)
      STORE "For "+x_filtro TO x_filtro
   ENDIF
   SET SAFE OFF
   INDEX ON &x_orden TO (x_index) &x_filtro
   SET INDEX TO (x_index)
   SET SAFE ON
   STORE "" TO x_filtro
   x_camp=wbrcofield(x_campos,x_ficher)
   x_ri=IIF(TYPE("X_Ri")#"N",5,x_ri)
   x_ci=IIF(TYPE("X_Ci")#"N",0,x_ci)
   x_rf=IIF(TYPE("X_Rf")#"N",18,x_rf)
   x_cf=IIF(TYPE("X_Cf")#"N",79,x_cf)
   SELE (x_ficher)
   GO TOP
   DO wbrcoactfu WITH !EMPTY(b_recupe)
   DO wbrcopanta WITH x_ri,x_ci,x_rf,x_cf,x_titulo,x_camp,x_filtro
   IF READKEY()=270 .OR. READKEY()=14
      x_recu=""
      IF !EMPTY(x_recupe)
         SELE (x_ficher)
         x_x=1
         x_tc=fwextrac(x_x,x_recupe,":")
         DO WHILE !EMPTY(x_tc)
            x_tcc=FIELD(VAL(fwextrac(1,x_tc,"/")))
            x_nt=VAL(fwextrac(2,x_tc,"/"))
            reemplazo = &x_tcc
            SELECT (NORMAL)
            campo = FIELD(x_nt)
            REPLACE (campo) WITH reemplazo
            SELECT (x_ficher)
            x_x=x_x+1
            x_tc=fwextrac(x_x,x_recupe,":")
         ENDDO
      ENDIF
   ENDIF
   SELE (x_ficher)
   SET INDEX TO &x_indice
   DELE FILE (x_index)
   SELE (tr_act)
   anter=0
   RETURN
PROCEDURE srt12wa
   PARAMETERS x_ficher,x_indice,x_titulo,x_filtro,x_campos,x_recupe
   DO srt12gen WITH x_ficher,x_indice,x_titulo,x_filtro,x_campos,x_recupe
   SELE (tr_act)
   DO fwdispwr
   anter=1
   RETURN
PROCEDURE srt12gen
   PARAMETERS x_ficher,x_indice,x_titulo,x_filtro,x_campos,x_recupe,x_ri,x_ci,x_rf,x_cf,x_seekin
   w_anter=WOUTPUT()
   IF USED(x_ficher)
      SELE (x_ficher)
   ELSE
      DO fselec WITH x_ficher,x_indice,"0"
   ENDIF
   STORE SYS(22) TO x_ordant
   SET ORDER TO TAG &x_indice
   SELE (x_ficher)
   IF !EMPTY(x_filtro)
      STORE "For "+x_filtro TO x_filtro
   ENDIF
   x_camp=wbrcofield(x_campos,x_ficher)

   SELE (x_ficher)
   x_seekin=IIF(TYPE("X_Seekin")#"C","",x_seekin)
   GO TOP

   n_busca    = 1
   n_longitud = 0
   DO WHILE !EMPTY(fwextrac(n_busca,x_campos,":"))
      c_camposmos = fwextrac(n_busca,x_campos,":")
      n_barra     = AT("/",c_camposmos)
      n_longi     = 0
      IF EMPTY(n_barra)
         n_longi = LEN(c_camposmos)
      ELSE
         c_cmp   = FIELD(VAL(LEFT(c_camposmos,n_barra-1)))
         n_longi = FSIZE(c_cmp)
         n_longi = IIF(n_longi > LEN(RIGHT(c_camposmos,LEN(c_camposmos)-n_barra)),;
                       n_longi,  LEN(RIGHT(c_camposmos,LEN(c_camposmos)-n_barra)))
      ENDIF
      n_longitud = n_longitud + n_longi + 1
      n_busca    = n_busca + 1
   ENDDO

   n_longitud = IIF(EMPTY(n_longitud),LEN(c_campos),n_longitud+2)
   n_longitud = (IIF(n_longitud > 78, 78, n_longitud) /2)
   x_ri = IIF(TYPE("x_ri") # "N",            5, x_ri)
   x_ci = IIF(TYPE("x_ci") # "N",40-n_longitud, x_ci)
   x_rf = IIF(TYPE("x_rf") # "N",           18, x_rf)
   x_cf = IIF(TYPE("x_cf") # "N",40+n_longitud, x_cf)

   IF !EMPTY(x_seekin)
      SET NEAR ON
      SEEK x_seekin
      SET NEAR OFF
   ENDIF
   DO wbrcoactfu WITH !EMPTY(x_recupe)
   DO wbrcopanta WITH x_ri,x_ci,x_rf,x_cf,x_titulo,x_camp,x_filtro
   IF READKEY()=270 .OR. READKEY()=14
      x_recu=""
      IF !EMPTY(x_recupe)
         SELE (x_ficher)
         x_x=1
         x_tc=fwextrac(x_x,x_recupe,":")
         DO WHILE !EMPTY(x_tc)
            nfie=VAL(fwextrac(1,x_tc,"/"))
            x_tcc=FIELD(nfie)
            x_nt=VAL(fwextrac(2,x_tc,"/"))
            IF mvar(x_nt)#&x_tcc
               modifi=1
               mvar(x_nt)=&x_tcc
            ENDIF
            x_x=x_x+1
            x_tc=fwextrac(x_x,x_recupe,":")
         ENDDO
         anter=80
         SELE (tr_act)
         GO TOP
         LOCATE FOR preg=x_nt
         IF !EOF()
            A=mvar(x_nt)
         ENDIF
         SELE (x_ficher)
      ELSE
         anter=81
      ENDIF
   ELSE
      anter=81
   ENDIF
   SELE (x_ficher)
   IF !EMPTY(x_ordant)
      SET ORDER TO TAG &x_ordant
   ENDIF
   RELE ALL LIKE x_*
   IF !EMPTY(w_anter)
      ACTIVATE WINDOW (w_anter) SAME
   ENDIF
   RETURN
PROCEDURE wbrcoactfu
   PARAMETERS b_recup
   ant_wndw=WOUTPUT()
   ACTIVATE WINDOW wtefu SAME
   IF (b_recup)
      @ 0,0 SAY LEFT("F8 Para capturar c¢digo³F9 Para salir³"+SPACE(80),80)
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
   ELSE
      @ 0,0 SAY LEFT("F9 Para salir³"+SPACE(80),80)
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
   ENDIF
   IF !EMPTY(ant_wndw)
      ACTIVATE WINDOW (ant_wndw) SAME
   ELSE
      ACTIVATE SCREEN
   ENDIF
   RETURN
PROCEDURE wbrcopanta
   PARAMETERS b_ri,b_ci,b_rf,b_cf,b_titulo,b_camp,b_filtro
   panta="W"+SubStr(Sys(2015), 3, 7)
   DEFINE WINDOW (panta) FROM b_ri,b_ci TO b_rf,b_cf ;
      TITLE b_titulo ;
      NOCLOSE;
      FLOAT;
      NOGROW;
      SHADOW;
      NOZOOM;
      COLOR (colpan)
   ACTIVATE WINDOW (panta) SAME
   BROWSE IN WINDOW (panta);
      FIELDS &b_camp;
      &b_filtro;
      NOMENU;
      NOAPPEND;
      NODELETE;
      NOMODIFY;
      COLOR (colwin);
      NOFOLLOW
   RELE WINDOW (panta)
   RETURN
PROCEDURE wbrcofield
   PARAMETER b_campos,b_ficher
   PRIVATE b_camp
   b_camp=""
   IF !EMPTY(b_campos)
      SELE (b_ficher)
      x_x=1
      x_tc=fwextrac(x_x,b_campos,":")
      DO WHILE !EMPTY(x_tc)
         x_tcc=FIELD(VAL(fwextrac(1,x_tc,"/")))
         IF EMPTY(b_camp)
            b_camp=x_tcc+"/H='"+fwextrac(2,x_tc,"/")+"'"
         ELSE
            b_camp=b_camp+","+x_tcc+"/H='"+fwextrac(2,x_tc,"/")+"'"
         ENDIF
         x_x=x_x+1
         x_tc=fwextrac(x_x,b_campos,":")
      ENDDO
   ENDIF
   RETURN b_camp
PROCEDURE srt90
   anter=0
   RETURN
PROCEDURE srt91
   anter=1
   RETURN
PROCEDURE srt92
   anter=2
   RETURN
PROCEDURE srt93
   anter=3
   RETURN
PROCEDURE srt94
   anter=4
   RETURN
PROCEDURE srt95
   anter=5
   RETURN
PROCEDURE srt96
   anter=6
   RETURN
PROCEDURE srt97
   anter=7
   RETURN
PROCEDURE srt98
   anter=8
   RETURN
PROCEDURE srt99
   anter=9
   RETURN
PROCEDURE fwabrfic
   SELE (tr_act)
   LOCATE FOR !EMPTY(MASTER) .AND. !USED(ALLTRIM(MASTER))
   DO WHILE .NOT. EOF()
      STORE TRIM(MASTER) TO w_aux
      STORE TRIM(indice) TO w_ind
      DO fselec WITH w_aux,w_ind,"0"
      SELE (tr_act)
      CONTINUE
   ENDDO
   RETURN
PROCEDURE fwblqmas
   SELE (tr_act)
   STORE RECNO() TO w_ant
   DO fwdspcam WITH preg
   STORE 0 TO w_w
   SKIP
   DO WHILE .NOT. EOF()
      IF TRIM(otro) == "@"
         w_w= 1
         EXIT
      ENDIF
      SKIP
   ENDDO
   IF w_w=0
      GO w_ant
   ENDIF
   STORE RECNO() TO poswr
   A=mvar(preg)
   anter=1
   RETURN
PROCEDURE fwblqmen
   SELE (tr_act)
   STORE RECNO() TO w_ant
   DO fwdspcam WITH preg
   STORE 0 TO w_w
   SKIP -1
   DO WHILE .NOT. BOF()
      IF TRIM(otro) == "@"
         w_w= 1
         EXIT
      ENDIF
      SKIP -1
   ENDDO
   IF w_w=0
      GO w_ant
   ENDIF
   STORE RECNO() TO poswr
   A=mvar(preg)
   anter =1
   RETURN
PROCEDURE fwborrar
   PRIVATE texto
   DECLARE texto(1)
   texto(01) = "Est  seguro de desear eliminar este registro ?"
   IF libsino(nil,@texto,.T.)
      SELECT (NORMAL)
      IF fchklk("REGISTRO EN USO POR OTRO USUARIO NO PUEDE SER ELIMINADO")
         DELETE
         UNLOCK
         SELE (tr_act)
         RETURN .T.
      ENDIF
      SELE (tr_act)
   ENDIF
   RETURN .F.
PROCEDURE fwinipan
   mannivel=mannivel+1
   IF mannivel>4
      DO fesper WITH "Imposible A¤adir m s Pantallas ","Tecla"
      mannivel=mannivel -1
      RETURN .F.
   ENDIF
   numpan=numopc+extopc+"1"
   pregunta="WR"+numpan
   pregunta1="WR"+numopc+extopc+"2"
   STORE .F. TO swborra
   IF USED('wr000000')
      STORE .T. TO swborra
      SELECT wr000000
   ELSE
      DO fselec WITH "wr000000","wr000000","0"
   ENDIF
   SEEK numpan
   IF EOF()
      DO fesper WITH "Esta funci¢n no esta generada en wr000000 ","Tecla"
      mannivel=mannivel -1
      RETURN .F.
   ENDIF
   tr_act   = pregunta
   wantes   = wrprevio
   wdespues = wrposter
   wmodifi  = wrmodifi
   DEFINE WINDOW (pregunta) FROM wrlinini,wrcolini TO wrlinfin,wrcolfin ;
      &miborde;
      TITLE TRIM(wrtitulo);
      NOCLOSE;
      GROW;
      FLOAT;
      NOZOOM;
      SHADOW;
      COLOR (colpan)
   ACTIVATE WINDOW (pregunta) NOSHOW
   SET MEMOWIDTH TO WCOLS()
   _WRAP=.F.
   FOR lin=1 TO MEMLINES(wrtxtpan)
      @ lin-1,0 SAY MLINE(wrtxtpan,lin) COLOR (colpan)
   ENDFOR
   ACTIVATE WINDOW (pregunta)
   STORE TRIM(wrnormal) TO NORMAL
   STORE "" TO ind_nor
   IF !EMPTY(wrindic1)
      ind_nor=ind_nor+TRIM(wrindic1)
   ENDIF
   STORE wrnumcod TO nu_cod
   STORE wrcamcod TO wcamcod
   bloqueo=IIF(wrbloque=="S",.T.,.F.)
   STORE wrnuclan TO nuclan
   STORE ALLTRIM(wrcaclan) TO caclan
   IF !swborra
      USE
   ENDIF
   RETURN .T.
PROCEDURE fwmanbas
   PARAMETER numopc,extopc,ant_cla,cla_ini
   PRIVATE clave,mvar,mvar0,pregunta,pregunta1,tr_act,nu_cod,NORMAL,ind_nor
   PRIVATE ca_cod,posnor,poswr,A,pasolib,modifi,scr_man,bloqueo
   PRIVATE antnrml,swborra,wantes,wdespues,wmodifi,wcamcod
   PRIVATE camclant
   PRIVATE nuclan
   PRIVATE caclan
   STORE "" TO clave,pregunta,tr_act,nu_cod,camclant,nuclan,caclan,NORMAL
   STORE "" TO ind_nor,pregunta1,A,scr_man
   STORE "" TO antnrml,wantes,wdespues,wmodifi,wcamcod,bloqueo
   STORE "" TO camclant,nuclan,caclan
   STORE 0 TO pasolib,modifi
   IF !fwinipan()
      RETURN
   ENDIF
   DIMENSION ca_cod(nu_cod)
   IF !EMPTY(wcamcod)
      x_recupe=ALLTRIM(wcamcod)
      x_x=1
      x_tc=fwextrac(x_x,x_recupe,":")
      DO WHILE !EMPTY(x_tc)
         x_nt=VAL(x_tc)
         ca_cod(x_x)=x_nt
         x_x=x_x+1
         x_tc=fwextrac(x_x,x_recupe,":")
      ENDDO
   ENDIF
   DO fselec WITH pregunta,"","0"
   antnrml=USED(NORMAL)
   DO fselec WITH NORMAL,ind_nor,"0"
   IF !EMPTY(caclan)
      camclant=""
      x_x=1
      x_tc=fwextrac(x_x,caclan,":")
      DO WHILE !EMPTY(x_tc)
         x_fld=FIELD(VAL(x_tc))
         IF !EMPTY(camclant)
            camclant=camclant+"+"+x_fld
         ELSE
            camclant=x_fld
         ENDIF
         x_x=x_x+1
         x_tc=fwextrac(x_x,caclan,":")
      ENDDO
   ENDIF
   STORE "" TO clave
   SAVE SCREE TO scr_man
   SELE (NORMAL)
   STORE FCOUNT() TO x_ff
   DIMENSION mvar(x_ff),mvar0(x_ff)
   SCAT TO mvar0 BLANK
   SCAT TO mvar BLANK
   RELE x_ff
   IF nuclan>0
      x_x=1
      x_pos=1
      x_tc=fwextrac(x_x,caclan,":")
      DO WHILE !EMPTY(x_tc)
         x_nt=VAL(x_tc)
         x_ln=LEN(mvar(x_nt))
         mvar(x_nt)=SUBSTR(ant_cla,x_pos,x_ln)
         mvar0(x_nt)=SUBSTR(ant_cla,x_pos,x_ln)
         x_pos=x_pos+x_ln
         x_x=x_x+1
         x_tc=fwextrac(x_x,caclan,":")
      ENDDO
   ENDIF
   SELE (tr_act)
   DO fwabrfic
   GO TOP
   IF !EMPTY(cla_ini)
      i_i=1
      FOR ii=1 TO nu_cod
         camp=ca_cod(ii)
         tt=TYPE("Mvar(camp)")
         LOCATE FOR preg=camp
         DO CASE
         CASE tt="D"
            ll=LEN(TRIM(mascaraget))
            ff=SUBSTR(cla_ini,i_i,ll)
            mvar(camp)=stod(ff)
            i_i=i_i+8
         CASE tt="N"
            ll=LEN(TRIM(mascaraget))
            mvar(camp)=VAL(SUBSTR(cla_ini,i_i,ll))
            i_i=i_i+LEN(mvar(ca_cod(ii)))
         OTHER
            ll=LEN(mvar(camp))
            mvar(camp)=SUBSTR(cla_ini,i_i,ll)
            i_i=i_i+LEN(mvar(ca_cod(ii)))
         ENDCASE
      ENDFOR
      pasolib=1
      LOCATE FOR preg=ca_cod(nu_cod)
   ENDIF

   DO WHILE .T.
      SELE (tr_act)
      poswr=RECNO()
      DO fwdigdsp
      IF modifi=1 .AND. !EMPTY(wmodifi)
         DO &wmodifi
      ENDIF
      SELE (tr_act)
      IF preg=ca_cod(nu_cod) .AND. anter=0
         IF !fwcreacl() .OR. EMPTY(clave)
            LOOP
         ENDIF
         SELECT (NORMAL)
         IF SEEK(clave)
            IF bloqueo
               IF !fchklk("Registro "+clave+" EN USO por otro usuario ")
                  SELE (tr_act)
                  DO fwborram
                  RESTORE SCRE FROM scr_man
                  GO TOP
                  LOOP
               ENDIF
            ENDIF
            SELE (NORMAL)
            SCAT TO mvar
            SELE (tr_act)
            tr_act=pregunta1
            USE (tr_act)
            DO fwdispwr
            SELE (tr_act)
            LOCATE FOR preg=ca_cod(nu_cod)
            SKIP
            DO WHILE .T.
               SELE (tr_act)
               poswr=RECNO()
               DO fwdigdsp
               IF modifi=1 .AND. !EMPTY(wmodifi)
                  DO &wmodifi
               ENDIF
               SELE (tr_act)
               DO CASE
               CASE anter = 0
                  SKIP
                  IF !EOF()
                     LOOP
                  ENDIF
                  =fwctrgrb()
                  SELE (tr_act)
                  IF anter=5
                     EXIT
                  ENDIF
                  LOCATE FOR preg=ca_cod(nu_cod)
                  SKIP
                  LOOP
               CASE anter = 1
                  LOOP
               CASE anter = 2
                  SKIP -1
                  IF BOF() .OR. RECNO()<=nu_cod
                     GO BOTT
                  ENDIF
                  wn=.T.
                  cdg=TRIM(condiciget)
                  IF !EMPTY(cdg)
                     wn=&cdg
                  ENDIF
                  DO WHILE !BOF() .AND. (!wn .OR. !EMPTY(calculdisp)) .AND. RECNO()>nu_cod+1
                     SKIP -1
                     wn=.T.
                     cdg=TRIM(condiciget)
                     IF !EMPTY(cdg)
                        wn=&cdg
                     ENDIF
                  ENDDO
               CASE anter = 3
                  =fwctrgrb()
                  SELE (tr_act)
                  LOCATE FOR preg=ca_cod(nu_cod)
                  SKIP
                  LOOP
               CASE anter = 4
                  EXIT
               CASE anter = 5
                  EXIT
               CASE anter = 6
                  IF fwborrar()
                     anter=5
                     SELE (tr_act)
                     EXIT
                  ENDIF
               CASE anter = 7
                  DO fwchreg WITH -1
               CASE anter = 8
                  DO fwchreg WITH 1
               CASE anter=9
                  SELE (tr_act)
                  STORE RECNO() TO rec_ant
                  STORE .T. TO g_fin
                  SET SAFE OFF
                  SAVE SCREE TO scr_ant
                  STORE fntemp(2) TO var_ant
                  SAVE TO (var_ant)
                  SET SAFE ON
                  STORE subprog TO sss
                  STORE "" TO ret_opc
                  DO &sss
                  SELE (tr_act)
                  RESTORE FROM (var_ant) ADDITIVE
                  SET SAFE OFF
                  DELETE FILE (var_ant)
                  SET SAFE ON
                  RESTORE SCREE FROM scr_ant
                  STORE pregunta1 TO tr_act
                  USE (tr_act)
                  SELE (NORMAL)
                  SEEK clave
                  SELE (tr_act)
                  GO rec_ant
                  anter=0
                  LOOP
                  anter =10
                  SELE (NORMAL)
                  SEEK clave
                  SELE (tr_act)
                  GO poswr
                  anter=1
                  LOOP
               ENDCASE
            ENDDO
         ENDIF
      ENDIF
      SELE (tr_act)
      DO CASE
      CASE anter = 0
         SKIP
         IF EOF()
            IF fwctrgrb()
               SELE (tr_act)
               IF anter#5
                  STORE 1 TO pasolib
               ENDIF
            ENDIF
            LOCATE FOR preg=ca_cod(nu_cod)
         ENDIF
         IF anter#5
            LOOP
         ENDIF
      CASE anter=4
         SELE (NORMAL)
         UNLOCK
         SELE (tr_act)
         EXIT
      CASE anter=3
         SELE (NORMAL)
         UNLOCK
         SELE (tr_act)
         IF fwctrgrb()
            SELE (tr_act)
            STORE 1 TO pasolib
            LOCATE FOR preg=ca_cod(nu_cod)
         ENDIF
         IF anter#5
            LOOP
         ENDIF
      CASE anter = 2
         IF (RECNO()>1 .AND. RECNO()<nu_cod) .OR. RECNO()>nu_cod+1
            SKIP -1
            wn=.T.
            cdg=TRIM(condiciget)
            IF !EMPTY(cdg)
               wn=&cdg
            ENDIF
            DO WHILE !BOF() .AND. (!EMPTY(calculdisp) .OR. !wn) .AND. RECNO()>nu_cod+1
               SKIP -1
               wn=.T.
               cdg=TRIM(condiciget)
               IF !EMPTY(cdg)
                  wn=&cdg
               ENDIF
            ENDDO
         ENDIF
      ENDCASE
      IF anter=5
         SELE (NORMAL)
         UNLOCK


*> ATENCION - Si no blanqueamos Mvar, los campos que no salen en pantalla
*>            no los inicializa al a¤adir nuevo registro. Tenemos que
*>            recuperar los valores de Mvar0, donde tenemos los campos
*>            clave anteriores.

         Scat To Mvar Blank
         x_ff = FCount()
         Do While x_ff > 0
            Mvar(x_ff) = Mvar0(x_ff)
            x_ff = x_ff - 1
         EndDo
         Release x_ff
*>--------------------------------------------------------------------

         SELE (tr_act)
         DO fwborram
         tr_act=pregunta
         USE (tr_act)
         RESTORE SCRE FROM scr_man
         GO TOP
         LOOP
      ENDIF
   ENDDO
   anter=10
   SELE (tr_act)
   USE
   IF !antnrml
      SELE (NORMAL)
      USE
   ENDIF
   RELE WINDOW (pregunta)
   mannivel=mannivel-1
   RETURN
PROCEDURE fwtecfun
   PARAMETER x_accion
   DO CASE
   CASE x_accion=1
      SCAT FIELDS op2,op3,op4,op5,op6,op7,op8,op9,op0 TO x_p
      w_ant=WOUTPUT()
      ACTIVATE WINDOW wtefu SAME
      STORE "" TO x_tefu
      FOR x_x =2 TO 10
         IF !EMPTY(x_p(x_x-1))
            SET FUNCTION x_x TO ";"
            uu=ALLTRIM(x_p(x_x-1))
            x_tefu=x_tefu+"F"+ALLTRIM(STR(x_x,2,0))+" "+uu+"³"
         ELSE
            SET FUNCTION x_x TO ""
         ENDIF
      ENDFOR
      RELE x_p,x_x,uu
      @ 0,0 SAY LEFT(x_tefu+SPACE(80),80) COLOR (colpiv)
      IF !EMPTY(w_ant)
         ACTIVATE WINDOW (w_ant) SAME
      ENDIF
   CASE x_accion=0
      w_ant=WOUTPUT()
      ACTIVATE WINDOW wtefu SAME
      IF !EMPTY(w_ant)
         ACTIVATE WINDOW (w_ant) SAME
      ENDIF
      FOR x_x=2 TO 10
         SET FUNCTION x_x TO ""
      ENDFOR
   CASE x_accion = 2
      IF LASTKEY()<0
         x_op=ABS(LASTKEY())
         SELE (tr_act)
         SCAT FIELDS opi2,opi3,opi4,opi5,opi6,opi7,opi8,opi9,opi0 TO x_p
         x_opc=x_p(x_op)
         IF !EMPTY(x_opc)
            DO &x_opc
         ELSE
            anter=1
         ENDIF
      ENDIF
   ENDCASE
   RETURN
PROCEDURE fwdspcam
   PARAMETERS campo
   STORE 0 TO a_rec
   SELE (tr_act)
   IF campo#preg
      STORE RECNO() TO a_rec
      LOCATE FOR preg=campo
   ENDIF
   IF EOF() OR BOF()
      IF a_rec#0
         GO a_rec
      ENDIF
      RETURN
   ENDI
   ps=IIF(EMPTY(mascarasay),"","Picture '"+TRIM(mascarasay)+"'")+" "+IIF(EMPTY(funcionsay),"","Function '"+TRIM(funcionsay)+"'")
   cd=TRIM(calculdisp)
   IF !EMPTY(cd)
      A=&cd
      mvar(preg)=A
   ENDIF
   @ X,y SAY mvar(preg) &ps  COLOR (coltxt)
   IF BETWEEN(linea,1,40) .AND. !EMPTY(MASTER) .AND. BETWEEN(columna,1,80)
      A=mvar(preg)
      DO fwbusca WITH 0
   ENDIF
   IF a_rec#0
      GO a_rec
   ENDIF
   RETURN
PROCEDURE fwborram
   SELE (tr_act)
   GO TOP
   DO WHILE !EOF()
      STORE mvar0(preg) TO mvar(preg)
      SKIP
   ENDDO
   GO TOP
   RETURN
PROCEDURE fwdispwr
   SELE (tr_act)
   IF EOF()
      SKIP -1
   ENDI
   IF BOF()
      SKIP
   ENDI
   STORE RECNO() TO R
   GO TOP
   DO WHILE .NOT. EOF()
      DO fwdspcam WITH preg
      IF linea#0
         A=mvar(preg)
         DO fwbusca WITH 0
      ENDIF
      SKIP
   ENDDO
   GO R
   A=mvar(preg)
   RETURN
PROCEDURE fwcargapd
   SELE (tr_act)
   IF EOF()
      SKIP -1
   ENDI
   IF BOF()
      SKIP
   ENDI
   STORE RECNO() TO R
   GO TOP
   DO WHILE .NOT. EOF()
      A=mvar(preg)
      pd=IIF(EMPTY(pordefecto),"",TRIM(pordefecto))
      IF !EMPTY(pd) .AND. EMPTY(A)
         A=&pd
      ENDIF
      mvar(preg)=A
      SKIP
   ENDDO
   GO R
   A=mvar(preg)
   RETURN
PROCEDURE fwdigcam
   PARAMETER campo
   STORE 0 TO a_rec,anter
   SELE (tr_act)
   IF campo#preg
      STORE RECNO() TO a_rec
      LOCATE FOR preg=campo
   ENDIF
   IF EOF() .OR. BOF()
      IF a_rec#0
         GO a_rec
      ENDIF
      RETURN
   ENDI
   pg=IIF(EMPTY(mascaraget),"","Picture '"+TRIM(mascaraget)+"'")
   fg=IIF(EMPTY(funcionget),"","Function '"+TRIM(funcionget)+"'")
   pfg=pg+" "+fg
   lns=LEN(ALLTRIM(mascarasay))
   lng=LEN(ALLTRIM(mascaraget))
   msg=ALLTRIM(ayu)
   tp=tipo
   vl=IIF(EMPTY(valida),"","Valid("+TRIM(valida)+")")
   SET CURSOR ON
   IF lng<lns
      @  X,y SAY SPACE(lns) COLOR (coltxt)
   ENDIF
   A=mvar(preg)
   pd=IIF(EMPTY(pordefecto),"",TRIM(pordefecto))
   IF !EMPTY(pd) .AND. EMPTY(A)
      A=&pd
   ENDIF
*   pantalla = WOUTPUT()
*   ACTIVATE WINDOW wmsgDis SAME
*   @ 0, 0 SAY PADC(msg,79," ") COLOR (colmsg)
*   ACTIVATE WINDOW &pantalla
   @ X,y GET A &pfg &vl COLOR (coltxt)   MESSAGE msg
   READ
   SET CURSOR OFF
   IF LASTKEY()<0
      RETURN A
   ENDIF
   IF !EMPTY(A)
      DO CASE
      CASE tipo=="R"
         A=PADL(TRIM(A),lng," ")
      CASE tipo=="0"
         A=PADL(TRIM(A),lng,"0")
      ENDCASE
   ENDIF
   IF a_rec#0
      GO a_rec
   ENDIF
   RETURN A
PROCEDURE fwcreacl
   SELE (tr_act)
   STORE .T. TO rtr
   STORE RECNO() TO w_r
   STORE 1 TO w_cod
   STORE ant_cla TO clave
   DO WHILE w_cod<=nu_cod
      GO TOP
      LOCA FOR preg=ca_cod(w_cod)
      DO CASE
      CASE tipo="D"
         w_b=DTOS(mvar(preg))
      CASE tipo="N"
         w_b=STR(mvar(preg),LEN(TRIM(mascaraget)),ndec)
      OTHER
         w_b=mvar(preg)
      ENDCAS
      STORE clave+w_b TO clave
      w_cod=w_cod+1
   ENDDO
   IF EMPTY(clave)
      STORE .F. TO rtr
   ENDIF
   GO w_r
   RETURN rtr
PROCEDURE fwdigdsp
   SELE (tr_act)
   wn=.T.
   cdg=TRIM(condiciget)
   IF !EMPTY(cdg)
      wn=&cdg
   ENDIF
   STORE 0 TO anter
   STORE RECNO() TO rtr
   SELE (tr_act)
   A=mvar(preg)
   IF !EMPTY(calculdisp)
      STORE .F. TO wn
   ENDIF
   modifi=0
   a_anter=0
   IF pasolib=0 .AND. wn
      DO fwtecfun WITH 1
      A = fwdigcam(preg)
      rdky=READKEY()
      IF rdky>200
         modifi=1
      ENDIF
      a_anter=anter
      IF (rdky=4 .OR. rdky=260 .OR. rdky=0)
         anter=1
         IF rtr>nu_cod+1  .OR. (rtr>1 .AND. rtr<=nu_cod) .OR. nu_cod=0
            anter=2
         ENDIF
      ENDIF
      DO fwtecfun WITH 0
      IF LASTKEY()<0
         STORE RECNO() TO poswr
         DO fwtecfun WITH 2
         SELE (tr_act)
         IF anter>=80
            anter=anter-80
         ELSE
            GO poswr
         ENDIF
         IF anter=1
            GO poswr
         ENDIF
      ENDIF
   ENDIF
   SELE (tr_act)
   mvar(preg)=A
   DO fwdspcam WITH preg
   pasolib=0
   RETURN
PROCEDURE fwctrgrb
   paso=.T.
   IF .NOT. EMPTY(wantes)
      IF RIGHT(wantes,1)#")"
         wantes=wantes+"()"
      ENDIF
      paso=&wantes
   ENDIF
   IF paso
      DO fwgraba WITH bloqueo
      IF .NOT. EMPTY(wdespues)
         DO &wdespues
      ENDIF
   ENDIF
   RETURN paso
PROCEDURE fwgraba
   PARAMETER fwbloq
   STORE .T. TO g_fin
   DO WHILE .T.
      SELECT (NORMAL)
      SEEK clave
      IF EOF()
         swf=.F.
         IF !USED("ficheros")
            DO fselec WITH "FICHEROS","INDFICHE","0"
            swf=.T.
         ENDIF
         SELE ficheros
         STORE SPACE(8) TO p_clau
         STORE TRIM(UPPER(NORMAL))+SPACE(8-LEN(TRIM(NORMAL))) TO p_clau
         SEEK p_clau
         IF !LOCK()
            IF fchoqu("FICHERO EN USO POR OTRO USUARIO")
               LOOP
            ELSE
               USE
               SELE (tr_act)
               anter=1
               RETURN
            ENDIF
         ENDIF
         IF swf
            USE
         ENDIF
         SELE (NORMAL)
         APPE BLANK
         IF LOCK()
            GATH FROM mvar
            IF !fwbloq
               UNLOCK
            ENDIF
         ENDIF
         STORE .F. TO g_fin
      ENDIF
      EXIT
   ENDDO
   IF g_fin
      IF !fchklk("REGISTRO "+clave+" EN USO POR OTRO USUARIO")
         SELE (tr_act)
         GO TOP
         RETURN
      ENDIF
      GATH FROM mvar
      UNLOCK
   ENDIF
   SELE (tr_act)
   RETURN
PROCEDURE fwtratawr
   PARAMETER salida
   STORE 0 TO modifi
   DO WHILE .T.
      SELE (tr_act)
      DO fwdigdsp
      IF modifi=1 .AND. !EMPTY(wmodifi)
         DO &wmodifi
      ENDIF
      DO CASE
      CASE anter = 3
         anter=0
         RETURN
      CASE anter = 4
         anter=4
         RETURN
      CASE anter = 5
         SELE (tr_act)
         DO fwborram
         RESTORE SCRE FROM scr_man
         GO TOP
      CASE anter = 2
         SKIP -1
         IF BOF().OR. RECNO()<=nu_cod
            IF !salida
               GO BOTT
            ELSE
               GO TOP
            ENDIF
         ENDIF
         wn=.T.
         cdg=TRIM(condiciget)
         IF !EMPTY(cdg)
            wn=&cdg
         ENDIF
         DO WHILE !BOF() .AND. (!wn .OR. !EMPTY(calculdisp)) .AND. RECNO()>nu_cod+1
            SKIP -1
            wn=.T.
            cdg=TRIM(condiciget)
            IF !EMPTY(cdg)
               wn=&cdg
            ENDIF
         ENDDO
         IF BOF().OR. RECNO()<=nu_cod
            IF !salida
               GO BOTT
            ELSE
               GO TOP
            ENDIF
         ENDIF
      CASE anter = 0
         SKIP
         IF EOF()
            IF !salida
               GO TOP
            ELSE
               RETURN
            ENDIF
         ENDIF
      ENDCASE
      anter=0
   ENDDO
   RETURN
PROCEDURE fwlimbas
   PARAMETER numopc,extopc
   PRIVATE numpan,pregunta,pregunta1,tr_act,wantes,wdespues,wmodifi
   PRIVATE NORMAL,ind_nor,nu_cod,wcamcod,bloqueo,A,modifi
   PRIVATE nuclan
   PRIVATE clave,mvar,mvar0
   PRIVATE ca_cod,posnor,poswr,pasolib,scr_man
   PRIVATE antnrml
   PRIVATE camclant
   PRIVATE caclan
   STORE "" TO clave,pregunta,tr_act,nu_cod,camclant,nuclan,caclan,NORMAL
   STORE "" TO ind_nor,pregunta1,A,scr_man
   STORE "" TO antnrml,wantes,wdespues,wmodifi,wcamcod,bloqueo
   STORE "" TO camclant,nuclan,caclan
   STORE 0 TO pasolib,modifi
   IF !fwinipan()
      RETURN
   ENDIF
   DO fselec WITH NORMAL,ind_nor,"0"
   ciclico = bloqueo
   DO fselec WITH pregunta,"","0"
   CALCULATE MAX(preg) TO nu_cam
   GO TOP
   DIMENSION mvar(nu_cam),mvar0(nu_cam)
   SELE (tr_act)
   GO TOP
   DO WHILE !EOF()
      DO CASE
      CASE tipo="D"
         STORE CTOD("  .  .  ") TO mvar(preg),mvar0(preg)
      CASE tipo="N"
         STORE VAL("0."+REPLICATE("0",ndec)) TO mvar(preg),mvar0(preg)
      OTHER
         STORE SPACE(LEN(TRIM(mascaraget))) TO mvar(preg),mvar0(preg)
      ENDCASE
      SKIP
   ENDDO
   GO TOP
   SELE (NORMAL)
   IF !EMPTY(wantes)
      DO &wantes
   ENDIF
   DO fwabrfic
   GO TOP
   DO fwcargapd
   SAVE SCREE TO scr_man
   DO WHILE .T.
      salida = ciclico
      DO fwdispwr
      GO TOP
      DO fwtratawr WITH salida
      IF .NOT. EMPTY(wdespues) .AND. anter=0
         DO &wdespues
         IF anter=5
            SELE (tr_act)
            DO fwborram
            RESTORE SCREE FROM scr_man
         ENDIF
      ENDIF
      IF anter=4
         EXIT
      ENDIF
      SELECT (tr_act)
   ENDDO
   SELECT (tr_act)
   USE
   RELEASE WINDOW (pregunta)
   mannivel=mannivel-1
   RETURN
PROCEDURE fwpanaux
   PARAMETER numopc,extopc
   PRIVATE numpan,pregunta,pregunta1,tr_act,wantes,wdespues,wmodifi
   PRIVATE NORMAL,ind_nor,nu_cod,wcamcod,bloqueo,A,modifi
   PRIVATE nuclan
   PRIVATE ca_cod,posnor,poswr,pasolib,scr_man
   STORE "" TO numpan,pregunta,pregunta1,tr_act,wantes,wdespues,wmodifi
   STORE "" TO NORMAL,ind_nor,nu_cod,wcamcod,bloqueo,A
   STORE "" TO nuclan
   STORE 0 TO pasolib,modifi,poswr
   IF !fwinipan()
      RETURN
   ENDIF
   ciclico=bloqueo
   DO fselec WITH pregunta,"","0"
   SAVE SCREE TO scr_man
   IF !EMPTY(wantes)
      DO &wantes
   ENDIF
   DO WHILE .T.
      DO fwdispwr
      DO fwtratawr WITH ciclico
      IF anter=4
         EXIT
      ENDIF
      IF .NOT. EMPTY(wdespues)
         DO &wdespues
      ENDIF
      SELECT (tr_act)
   ENDDO
   SELECT (tr_act)
   USE
   RELEASE WINDOW (pregunta)
   mannivel=mannivel-1
   anter=1
   RETURN
PROCEDURE fwmsgdsp
   PARAMETERS fa_1
   w_ant=WOUTPUT()
   ACTIVATE WINDOW wmsgdis SAME
   CLEAR
   @ 0,1 SAY fa_1 COLOR (colmsg)
   IF EMPTY(w_ant)
      ACTIVATE SCREE
   ELSE
      IF LEN(w_ant)<=8
         ACTIVATE WINDOW (w_ant) SAME
      ELSE
         ACTIVATE WINDOW (pregunta) SAME
      ENDIF
   ENDIF
   RETURN
PROCEDURE fwchreg
   PARAMETERS x_reg
   SELE (NORMAL)
   STORE RECNO() TO a_rec
   STORE "" TO val_an_cl
   IF !EMPTY(camclant)
      STORE ".or. "+camclant+"#ant_cla" TO val_an_cl
   ENDIF
   DO WHILE .T.
      SKIP x_reg
      IF EOF() .OR. BOF() &val_an_cl
         IF EOF()
            DO fesper WITH "FIN DEL ARCHIVO"," Tecla ! "
         ELSE
            IF BOF()
               DO fesper WITH "PRINCIPIO DEL ARCHIVO"," Tecla ! "
            ELSE
               DO fesper WITH "NO HAY MAS FICHAS EN ESTA DIRECCION",""
            ENDIF
         ENDIF
         GO a_rec
         IF !fchklk("Registro original ha sido ocupado por otro usuario")
            anter=5
         ENDIF
         SELE (tr_act)
         RETURN .F.
      ENDIF
      UNLOCK
      IF !fchklk("Registro siguiente en uso por otro usuario")
         LOOP
      ENDIF
      EXIT
   ENDDO
   SELE (tr_act)
   STORE RECNO() TO a_tr
   SELE (NORMAL)
   SCAT TO mvar
   IF !bloqueo
      UNLOCK
   ENDIF
   SELE (tr_act)
   DO fwdispwr
   SELE (tr_act)
   GO a_tr
   SELE (NORMAL)
   STORE 1 TO ac_cod
   STORE ant_cla TO clave
   DO WHILE ac_cod<=nu_cod
      REGI=FIELD(ca_cod(ac_cod))
      IF TYPE(REGI)="D"
         STORE clave+DTOS(&regi) TO clave
      ELSE
         STORE clave+&regi TO clave
      ENDIF
      ac_cod=ac_cod+1
   ENDDO
   SELE (tr_act)
   RETURN .T.
PROCEDURE fwbusca
   PARAMETER w_obli
   PRIVATE correcto
   STORE 0 TO w_antnor,w_antaux,w_antpre
   IF USED(NORMAL)
      SELE (NORMAL)
      IF .NOT. EOF() .AND. .NOT. BOF()
         STORE RECNO() TO w_antnor
      ENDIF
   ENDIF
   SELE (tr_act)
   STORE X TO pxx
   STORE y TO pyy
   STORE ALIAS() TO w_tr
   IF READKEY()=4 .OR. READKEY()=0 .OR. READKEY()=260 .OR. READKEY()=256
      STORE 0 TO w_obli
   ENDIF
   correcto = .NOT. EMPTY(A)
   varempty=TRAN(SPACE(LEN(TRIM(mascaraget))),TRIM(mascaraget))
   IF tipo="C"
      correcto = correcto .AND. A#varempty
   ENDIF
   IF .NOT. correcto
      IF !EMPTY(registro)
         IF linea#0
            @ linea,columna SAY SPACE(long)
         ENDIF
      ENDIF
      IF w_obli=1
         anter=1
      ENDIF
      RETURN .T.
   ENDIF
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antpre
   ENDIF
   STORE ALLTRIM(MASTER) TO w_aux
   IF !USED(w_aux)
      STORE indice TO w_ind
      STORE .F. TO w_swant
      DO fselec WITH w_aux,w_ind,"0"
   ELSE
      SELE (w_aux)
      STORE .T. TO w_swant
   ENDIF
   SELE (tr_act)
   IF LEFT(tipreg,1)=="@"
      STORE RECNO() TO w_aaa
      STORE TRIM(tipreg) TO w_tp
      STORE RIGHT(w_tp,LEN(w_tp)-1) TO w_tp
*     STORE VAL(w_tp) TO w_ddd
      STORE VAL(LEFT(w_tp,IIF(AT(",",w_tp)>0,AT(",",w_tp),LEN(w_tp)))) TO w_ddd
      LOCATE FOR preg=w_ddd
      DO CASE
      CASE tipo="D"
         B=DTOS(mvar(preg))
      CASE tipo="N"
         B=STR(mvar(preg),LEN(TRIM(mascaraget)),ndec)
      OTHER
         B=mvar(preg)
      ENDCASE
      STORE B TO w_prim
      GO w_aaa
   ELSE
      STORE TRIM(tipreg) TO w_prim
      IF LEFT(w_prim,1)=="&"
         w_prim=STRTRAN(w_prim,"&","")
         STORE &w_prim TO w_prim
      ELSE
         IF EMPTY(w_prim)
            w_prim=""
         ENDIF
      ENDIF
   ENDIF
   IF !EMPTY(A)
      DO CASE
      CASE tipo=="R"
         A=PADL(TRIM(A),LEN(TRIM(mascaraget))," ")
      CASE tipo=="0"
         A=PADL(TRIM(A),LEN(TRIM(mascaraget)),"0")
      ENDCASE
   ENDIF
   IF tipo#"N"
      STORE w_prim+A+TRIM(idioma) TO w_cerca
   ELSE
      STORE w_prim+STR(A,LEN(ALLTRIM(mascaraget)),ndec)+TRIM(idioma) TO w_cerca
   ENDIF
   SELE (w_aux)
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antaux
   ENDIF
   IF EMPTY(w_cerca)
      GO BOTT
      SKIP
   ELSE
      SEEK w_cerca
   ENDIF
   IF EOF()
      DO fesper WITH "INEXISTENTE"," TECLA "
      A=varempty
      anter=81
      STORE .T. TO rrr
   ELSE
      SELE (tr_act)
      IF !EMPTY(registro)
         STORE registro TO w_ccc
         STORE linea TO w_li
         IF w_li=0
            SELE (w_aux)
            STORE &w_ccc TO w_ccc
            IF TYPE("W_ccc")="C"
               WAIT WINDOW w_ccc NOWAIT
            ENDIF
         ELSE
            IF !EMPTY(w_ccc)
               SELE (w_aux)
               STORE &w_ccc TO w_ccc
               SELE (tr_act)
               masca="Picture '"+REPLICATE("X",long)+"'"
               @ w_li,columna SAY w_ccc &masca COLOR (colpiv)
            ENDIF
         ENDIF
      ENDIF
      STORE .T. TO rrr
   ENDIF
   SELE (tr_act)
   IF w_antpre#0
      GO w_antpre
   ENDIF
   SELE (w_aux)
   IF w_antaux#0
      GO w_antaux
   ENDIF
   IF !w_swant
      USE
   ENDIF
   IF w_antnor#0 .AND. USED(NORMAL)
      SELE (NORMAL)
      GO w_antnor
   ENDIF
   RELE ALL LIKE w_*
   SELE (tr_act)
   RETURN rrr
PROCEDURE fwbuscx
   PARAMETER w_obli
   STORE 0 TO w_antnor,w_antaux,w_antpre
   SELE (NORMAL)
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antnor
   ENDIF
   SELE (tr_act)
   STORE X TO pxx
   STORE y TO pyy
   STORE ALIAS() TO w_tr
   IF LASTKEY()<0
      RETURN .T.
   ENDIF
   IF READKEY()=4 .OR. READKEY()=0 .OR. READKEY()=260 .OR. READKEY()=256
      STORE 0 TO w_obli
   ENDIF
   IF EMPTY(A) .AND. w_obli=0
      STORE SPACE(long) TO w_ccc
      STORE linea TO w_li
      STORE columna TO w_co
      IF w_li#0
         @ WROWS()-3,0
         @ w_li,w_co SAY w_ccc
      ENDIF
      RETURN .T.
   ENDIF
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antpre
   ENDIF
   SELE (tr_act)
   w_previo=TRIM(tipreg)
   IF LEFT(w_previo,1)=="@"
      STORE RECNO() TO w_ccc
      LOCATE FOR preg=VAL(RIGHT(w_previo,LEN(w_previo)-1))
      DO CASE
      CASE tipo="D"
         B=DTOC(mvar(preg))
      CASE tipo="N"
         B=STR(mvar(preg),LEN(TRIM(mascaraget)),ndec)
      OTHER
         B=mvar(preg)
      ENDCASE
      STORE TRIM(tipreg)+B+TRIM(idioma) TO w_clav
      DO fselec WITH MASTER,indice,"0"
      SEEK w_clav
      STORE TRIM(f25ctxabre) TO w_camp
      STORE LEFT(w_camp,4) TO w_fich
      DO fselec WITH "FICHEROS","INDFICHE","0"
      STORE "" TO w_ind
      GO TOP
      SEEK UPPER(w_fich)
      IF LEN(TRIM(nombreind1))#0
         STORE TRIM(nombreind1) TO w_ind
      ENDIF
      USE
      DO fselec WITH w_fich,w_ind,"0"
      STORE 1 TO w_posn
      SELE (tr_act)
      GO w_ccc
   ENDIF
   STORE w_fich TO w_aux
   SELE (w_aux)
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antaux
   ENDIF
   STORE A TO w_cerca
   IF LEN(TRIM(w_cerca))=0
      IF .NOT. EOF()
         GO BOTT
         SKIP
      ENDIF
   ELSE
      SEEK w_cerca
   ENDIF
   IF EOF()
      DO fesper WITH "INEXISTENTE"," TECLA "
      STORE .F. TO rrr
   ELSE
      STORE w_camp TO w_ccc
      STORE &w_tr->linea TO w_li
      STORE &w_tr->columna TO w_co
      IF w_li=0
         WAIT WINDOW SUBSTR(&w_ccc,1,&w_tr->long) NOWAIT
      ELSE
         @ w_li,w_co SAY SUBSTR(&w_ccc,1,&w_tr->long)
      ENDIF
      STORE .T. TO rrr
   ENDIF
   SELE (tr_act)
   IF w_antpre#0
      GO w_antpre
   ENDIF
   SELE (w_aux)
   IF w_antaux#0
      GO w_antaux
   ENDIF
   USE
   SELE (NORMAL)
   IF w_antnor#0
      GO w_antnor
   ENDIF
   SELE (tr_act)
   RETURN rrr
PROCEDURE fbusxx
   PARAMETER w_tre, w_cre, w_cla, w_cam, w_ape
   STORE 0 TO w_antnor,w_antaux,w_antpre
   SELE (NORMAL)
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antnor
   ENDIF
   STORE w_tre+w_cre+"0000" TO w_clav
   DO fselec WITH "F25c","cf250","0"
   SEEK w_clav
   STORE TRIM(f25ctxabre) TO w_camp
   STORE LEFT(w_camp,4) TO w_fich
   IF !USED(w_fich)
      DO fselec WITH "FICHEROS","INDFICHE","0"
      STORE "" TO w_ind
      GO TOP
      SEEK UPPER(w_fich)
      IF LEN(TRIM(nombreind1))#0
         STORE TRIM(nombreind1) TO w_ind
      ENDIF
      DO fselec WITH w_fich,w_ind,"0"
      STORE 1 TO w_swantes
   ELSE
      STORE 0 TO w_swantes
   ENDIF
   STORE 1 TO w_posn
   STORE w_fich TO w_aux
   SELE (w_aux)
   IF .NOT. EOF() .AND. .NOT. BOF()
      STORE RECNO() TO w_antaux
   ENDIF
   SEEK w_cla
   IF EOF()
      STORE "INEXISTENTE" TO rrr
   ELSE
      IF w_cam>0
         w_ccc=FIELD(w_cam)
      ELSE
         STORE w_camp TO w_ccc
      ENDIF
      STORE &w_ccc TO rrr
   ENDIF
   IF USED("Ficheros")
      SELE ficheros
      USE
   ENDIF
   SELE (w_aux)
   IF w_swantes=1
      IF w_antaux#0
         GO w_antaux
      ENDIF
   ELSE
      IF w_ape=0
         USE
      ENDIF
   ENDIF
   SELE (NORMAL)
   IF w_antnor#0
      GO w_antnor
   ENDIF
   IF w_ape=2
      SELECT (w_aux)
   ENDIF
   RETURN rrr
PROCEDURE fwmanlin
   PARAMETER numopc,extopc,x_claant,devolucion
   PRIVATE clave,mvar,pregunta,pregunta1,tr_act,nu_cod,NORMAL,ind_nor
   PRIVATE ca_cod,posnor,poswr,A,pasolib,modifi,scr_man, wantes
   STORE "" TO clave,mvar,pregunta,tr_act,nu_cod
   STORE 0 TO pasolib,modifi
   numpan=numopc+extopc+"1"
   pregunta="WR"+numpan
   STORE .F. TO swborra
   IF USED('wr000000')
      STORE .T. TO swborra
   ENDIF
   DO fselec WITH "wr000000","wr000000","0"
   SEEK numpan
   IF EOF()
      RETURN ""
   ENDIF
   tr_act= pregunta
   wtitu=ALLTRIM(wrtitulo)
   wantes = ALLTRIM(wrprevio)
   wdespu = ALLTRIM(wrposter)
   DEFINE WINDOW (pregunta) FROM wrlinini,wrcolini TO wrlinfin,wrcolfin;
      PANEL;
      TITLE wtitu;
      NOCLOSE;
      GROW;
      FLOAT;
      ZOOM;
      COLOR (colwin)
   STORE wrnormal TO x_fixer
   STORE "" TO ind_nor
   IF !EMPTY(wrindic1)
      ind_nor=ind_nor+TRIM(wrindic1)
   ENDIF
   STORE wrnumcod TO nu_cod
   STORE wrnuclan TO nuclan
   STORE wrcaclan TO caclan
   DIMENSION ca_cod(nu_cod)
   IF !EMPTY(wrcamcod)
      x_recupe=ALLTRIM(wrcamcod)
      x_x=1
      x_tc=fwextrac(x_x,x_recupe,":")
      DO WHILE !EMPTY(x_tc)
         x_nt=VAL(x_tc)
         ca_cod(x_x)=x_nt
         x_x=x_x+1
         x_tc=fwextrac(x_x,x_recupe,":")
      ENDDO
   ENDIF
   USE IN wr000000
   DO fselec WITH x_fixer,ind_nor,"0"
   SELE (x_fixer)
   STORE KEY(1) TO x_orden
   STORE fntemp(3) TO nnormal
   SET SAFE OFF
   COPY STRU TO (nnormal)
   DO fselec WITH nnormal,"","1"
   STORE ALIAS() TO NORMAL
   x_camcla=""
   IF nuclan>0
      DIMENSION fi_cod(nuclan),fi_cla(nuclan)
      x_camcla=""
      x_x=1
      x_pos=1
      x_tc=fwextrac(x_x,caclan,":")
      DO WHILE !EMPTY(x_tc)
         x_nt=VAL(x_tc)
         fi_cod(x_x)=FIELD(x_nt)
         s_camp=fi_cod(x_x)
         ty=TYPE(s_camp)
         DO CASE
         CASE ty="D"
            x_ln=8
            fi_cla(x_x)=stod(SUBSTR(x_claant,x_pos,x_ln))
            s_camp="Dtos("+fi_cod(x_x)+")"
         CASE ty="N"
            x_ln=FSIZE(s_camp)
            fi_cla(x_x)=VAL(SUBSTR(x_claant,x_pos,x_ln))
            s_camp=fi_cod(x_x)
         OTHER
            x_ln=FSIZE(s_camp)
            fi_cla(x_x)=SUBSTR(x_claant,x_pos,x_ln)
            s_camp=fi_cod(x_x)
         ENDCASE
         IF !EMPTY(x_camcla)
            x_camcla=x_camcla+"+"+s_camp
         ELSE
            x_camcla=s_camp
         ENDIF
         x_pos=x_pos+x_ln
         x_x=x_x+1
         x_tc=fwextrac(x_x,caclan,":")
      ENDDO
   ENDIF
   DO fwlcartem
   SET SAFE ON
   DO fselec WITH tr_act,"","0"
   x_nucamp=RECCOUNT()
   DIMENSION x_fld(x_nucamp)
   GO TOP
   x_x=1
   DO WHILE .NOT.EOF()
      SELE (NORMAL)
      IF EMPTY(&tr_act->calculdisp)
         x_fld(x_x)=FIELD(&tr_act->preg)
         SELE (tr_act)
         x_fld(x_x)=x_fld(x_x)+":H='"+TRIM(nameprogra)+"'"
         x_fld(x_x)=x_fld(x_x)+":V=fwlva("+ALLTRIM(TRAN(preg,"999"))+"):F"
         IF !EMPTY(condiciget)
            wn=TRIM(condiciget)
            x_fld(x_x)=x_fld(x_x)+":W="+wn
         ELSE
            x_fld(x_x)=x_fld(x_x)+":W=fwlwh("+ALLTRIM(TRAN(preg,"999"))+")"
         ENDIF
      ELSE
         SELE (tr_act)
         x_fld(x_x)="W"+RIGHT(STR(1000+x_x,4,9),3)+"="+TRIM(&tr_act->calculdisp)
         x_fld(x_x)=x_fld(x_x)+":H='"+TRIM(nameprogra)+"'"+":W=.f."
      ENDIF
      IF !EMPTY(mascaraget)
         x_fld(x_x)=x_fld(x_x)+":P='"+TRIM(mascaraget)+"'"
      ENDIF
      x_x=x_x+1
      SKIP
   ENDDO
   DIMENSION x_fields(5)
   x_x=1
   STORE "" TO x_fields
   FOR x_i=1 TO x_nucamp-1
      IF LEN(x_fields(x_x)+x_fld(x_i))+1>250
         x_x=x_x+1
      ENDIF
      x_fields(x_x)=x_fields(x_x)+x_fld(x_i)+","
   ENDFOR
   IF LEN(x_fields(x_x)+x_fld(x_i))+1>250
      x_x=x_x+1
   ENDIF
   x_fields(x_x)=x_fields(x_x)+x_fld(x_nucamp)
   RELE x_fld
   SELE (tr_act)
   GO TOP
   DO fwlaaux
   SELE (NORMAL)
   GO TOP
   fin_tefu="F7 Borra.Lin³F8 A¤ade.Lin³F9 Fin.Graba³F10 Cancela³"
   DO fwltecfun WITH 5
   SELE (NORMAL)
   SET DELE OFF
   BROWSE FIELDS &x_fields(1) &x_fields(2) &x_fields(3) &x_fields(4) &x_fields(5) ;
      WINDOW (pregunta)
   IF READKEY()=270 .OR. READKEY()=14
      DO fwlgraba
   ENDIF
   DO fwltecfun WITH 0
   SET DELE ON
   SELE (NORMAL)
   USE
   DELE FILE (nnormal)
   RELE WINDOW (pregunta)
   DEACTIVATE WINDOW wtefu
   anter = 0
   RETURN
PROCEDURE fwlva
   PARAMETER w_ca
   STORE LASTKEY() TO x_lstk
   SELE (tr_act)
   LOCA FOR preg=w_ca
   SELE (NORMAL)
   B=FIELD(&tr_act->preg)
   A=&b
   IF &tr_act->tipo="0" .AND. !EMPTY(A)
      l=LEN(A)
      A=RIGHT(REPLICATE("0",l)+ALLTRIM(A),l)
      REPLACE &b WITH A
   ENDIF
   IF !EMPTY(&tr_act->valida)
      B=&tr_act->valida
      IF .NOT. &b
         RETURN .F.
      ENDIF
   ENDIF
   SELE (NORMAL)
   DO fwltecfun WITH 4
   RETURN .T.
PROCEDURE fwlwh
   PARAMETER ppp
   SELE (tr_act)
   LOCATE FOR preg=ppp
   DO fwltecfun WITH 1
   wn=.T.
   cdg=TRIM(condiciget)
   IF !EMPTY(cdg)
      wn=&cdg
   ENDIF
   modifi=0
   salta=0
   SELE (NORMAL)
   RETURN wn
PROCEDURE fwlgraba
   STORE .T. TO paso
   IF .NOT. EMPTY(wantes)
      IF RIGHT(wantes,1)#")"
         wantes=wantes+"()"
      ENDIF
      paso=&wantes
   ENDIF
   IF !paso
      IF anter#21470
         anter=0
      ENDIF
      RETURN
   ENDIF
   DO fwmsgesp WITH 1,6,1,"*** Grabando datos en el disco ***"
   SELE (x_fixer)
   IF EMPTY(x_camcla)
      DELE ALL
   ELSE
      SEEK x_claant
      DO WHILE .NOT. EOF() .AND. &x_camcla = x_claant
         DELETE
         SKIP
      ENDDO
   ENDIF
   SELE (NORMAL)
   PACK
   GO TOP
   DO WHILE !EOF()
      swvacio=.F.
      SELE (NORMAL)
      FOR iii=1 TO nuclan
         camp=fi_cod(iii)
         REPLACE (camp) WITH fi_cla(iii)
      ENDFOR
      swvacio=.F.
      FOR iii=1 TO nu_cod
         camp=FIELD(ca_cod(iii))
         valo=&camp
         IF EMPTY(valo) .AND. iii=1
            swvacio=.T.
            EXIT
         ENDIF
      ENDFOR
      IF swvacio
         SKIP
         LOOP
      ENDIF
      STORE &x_orden TO clave
      SCAT TO x_cla
      SELE (x_fixer)
      SEEK clave
      IF EOF()
         APPE BLANK
      ELSE
         IF DELETED()
            RECALL
         ENDIF
      ENDIF
      GATH FROM x_cla
      SELE (tr_act)
      LOCATE FOR !EMPTY(preg) .AND. !EMPTY(calculdisp)
      DO WHILE .NOT.EOF()
         STORE preg TO w_pre
         STORE calculdisp TO w_cal
         SELE (x_fixer)
         STORE FIELD(w_pre) TO w_cam
         REPLACE (w_cam) WITH &w_cal
         SELE (tr_act)
         CONTINUE
      ENDDO
      SELE (NORMAL)
      SKIP
   ENDDO
   IF !EMPTY(wdespu)
      DO &wdespu
   ENDIF
   SELE (NORMAL)
   anter=0
   GO TOP
   DO fwmsgesp WITH 0,0,0,""
   RETURN
PROCEDURE fwlcartem
   SELE (x_fixer)
   GO TOP
   IF EMPTY(x_camcla)
      cond = "!Eof()"
      GO TOP
   ELSE
      cond = "!Eof() .and. &X_CamCla = X_ClaAnt"
      SEEK x_claant
   ENDIF
   DO WHILE &cond
      SCAT TO w_p
      SELE (NORMAL)
      APPE BLANK
      GATH FROM w_p
      SELE (x_fixer)
      SKIP
   ENDDO
   SELE (NORMAL)
   GO TOP
   IF EOF()
      APPE BLANK
   ENDIF
   RETURN
PROCEDURE fwltecfun
   PARAMETER w_f
   SELE (tr_act)
   DO CASE
   CASE w_f=1
      SCAT FIELDS op2,op3,op4,op5,op6 TO w_p
      SCAT FIELDS opi2,opi3,opi4,opi5,opi6 TO w_pi
      ACTIVATE WINDOW wtefu SAME
      CLEAR
      STORE "" TO x_tefu
      STORE 0 TO w_x
      IF !EMPTY(w_p(1)+w_p(2)+w_p(3)+w_p(4)+w_p(5))
         FOR w_x =2 TO 6
            w_lbl="F"+ALLTRIM(STR(w_x,2))
            IF !EMPTY(w_p(w_x-1))
               STORE w_pi(w_x-1) TO w_prog
               ON KEY LABEL (w_lbl) DO &w_prog
               x_tefu=x_tefu+w_lbl+">"+ALLTRIM(w_p(w_x-1))+"³"
            ELSE
               ON KEY LABEL (w_lbl)
            ENDIF
         ENDFOR
      ELSE
         ON KEY LABEL f2
         ON KEY LABEL f3
         ON KEY LABEL f4
         ON KEY LABEL f5
         ON KEY LABEL f6
      ENDIF
      @ 0,0 SAY x_tefu+fin_tefu COLOR (colpiv)
   CASE w_f=0
      ACTIVATE WINDOW wtefu SAME
      CLEAR
      ON KEY LABEL f2
      ON KEY LABEL f3
      ON KEY LABEL f4
      ON KEY LABEL f5
      ON KEY LABEL f6
      SET FUNCTION 7 TO ""
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
      SET FUNCTION 10 TO ""
   CASE w_f = 2
      IF LASTKEY()<0
         x_op=RIGHT(STR(100+ABS(LASTKEY())+1,3),1)
         SELE (tr_act)
         x_p="Opi"+x_op
         x_opc=&x_p
         IF !EMPTY(x_opc)
            DO &x_opc
         ELSE
            anter=1
         ENDIF
      ENDIF
   CASE w_f=4
      ACTIVATE WINDOW wtefu SAME
      CLEAR
      ON KEY LABEL f2
      ON KEY LABEL f3
      ON KEY LABEL f4
      ON KEY LABEL f5
      ON KEY LABEL f6
   CASE w_f=5
      ON KEY LABEL f2
      ON KEY LABEL f3
      ON KEY LABEL f4
      ON KEY LABEL f5
      ON KEY LABEL f6
      SET FUNCTION 7 TO ""
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
      SET FUNCTION 10 TO ""
      fin_tefu="F7 Borra.Lin³F8 A¤ade.Lin³F9 Fin.Graba³F10 Cancela³"
   ENDCASE
   RETURN
PROCEDURE fwlaaux
   SELE (tr_act)
   STORE RECNO() TO w_antrec
   GO TOP
   DO WHILE .NOT. EOF()
      IF EMPTY(MASTER) .OR. USED(MASTER)
         SKIP
         LOOP
      ENDIF
      STORE TRIM(MASTER) TO w_aux
      STORE TRIM(indice) TO w_ind
      w_rel=IIF(seleccion>0,.T.,.F.)
      DO fselec WITH w_aux,w_ind,"0"
      SELE (NORMAL)
      IF !fwlesrel(w_aux) .AND. w_rel
         SELE (tr_act)
         x_x=preg
         SELE (NORMAL)
         x_cl=FIELD(x_x)
         SELE (tr_act)
         IF LEFT(tipreg,1)=="@"
            STORE RECNO() TO w_aaa
            STORE TRIM(tipreg) TO w_tp
            STORE RIGHT(w_tp,LEN(w_tp)-1) TO w_tp
            STORE VAL(w_tp) TO w_ddd
            GO w_ddd
            SELE (NORMAL)
            B=FIELD(&tr_act->preg)
            B=&b
            SELE (tr_act)
            STORE B TO w_prim
            GO w_aaa
         ELSE
            STORE TRIM(tipreg) TO w_prim
            IF LEFT(w_prim,1)=="&"
               w_prim=STRTRAN(w_prim,"&","")
               STORE &w_prim TO w_prim
            ENDIF
         ENDIF
         IF !EMPTY(w_prim)
            STORE "'"+w_prim+"'+" TO w_cerca
         ELSE
            STORE "" TO w_cerca
         ENDIF
         STORE w_cerca+x_cl TO w_cerca
         IF !EMPTY(idioma)
            STORE w_cerca+"+'"+TRIM(idioma)+"'" TO w_cerca
         ENDIF
         SELE (NORMAL)
         SET RELATION TO &w_cerca INTO (w_aux) ADDITIVE
      ENDIF
      SELE (tr_act)
      SKIP
   ENDDO
   SELE (tr_act)
   GO w_antrec
   RETURN
PROCEDURE fwlbusca
   PARAMETER w_obli
   anter=0
   IF EMPTY(A) .AND. (w_obli=0 .OR. w_obli=2)
      RETURN .T.
   ENDIF
   SELE (tr_act)
   STORE TRIM(MASTER) TO w_aux
   SELE (tr_act)
   IF LEFT(tipreg,1)=="@"
      STORE RECNO() TO w_aaa
      STORE TRIM(tipreg) TO w_tp
      STORE RIGHT(w_tp,LEN(w_tp)-1) TO w_tp
      STORE VAL(w_tp) TO w_ddd
      GO w_ddd
      SELE (NORMAL)
      B=FIELD(&tr_act->preg)
      B=&b
      SELE (tr_act)
      STORE B TO w_prim
      GO w_aaa
   ELSE
      STORE TRIM(tipreg) TO w_prim
      IF LEFT(w_prim,1)=="&"
         w_prim=STRTRAN(w_prim,"&","")
         STORE &w_prim TO w_prim
      ENDIF
   ENDIF
   STORE w_prim+A+TRIM(idioma) TO w_cerca
   SELE (w_aux)
   IF EMPTY(w_cerca)
      GO BOTT
      SKIP
   ELSE
      SEEK w_cerca
   ENDIF
   IF EOF()
      DO fesper WITH "INEXISTENTE","Tecla !"
      SELE (tr_act)
      RETURN .F.
   ELSE
      IF &tr_act->columna>0
         STORE &tr_act->registro TO w_camp
         DO fwmsgdsp WITH &w_camp
      ENDIF
      SELE (NORMAL)
      RETURN .T.
   ENDIF
   RETURN .F.
PROCEDURE fwlbusrel
   PARAMETER w_obli
   IF EMPTY(A) .AND. w_obli=0
      RETURN .T.
   ENDIF
   SELE (tr_act)
   STORE MASTER TO w_aux
   STORE columna TO w_col
   STORE registro TO w_reg
   IF !USED(w_aux) .OR. seleccion=0
      RETURN fwlbusca(w_obli)
   ENDIF
   SELE (w_aux)
   IF EOF()
      DO fesper WITH "INEXISTENTE","Tecla !"
      RETURN .F.
   ELSE
      IF w_col>0
         @ 0,w_col SAY &w_reg
      ENDIF
      RETURN .T.
   ENDIF
   RETURN .F.
PROCEDURE fwlbrel0
   PARAMETER w_obli
   IF EMPTY(A) .AND. w_obli=0
      RETURN .T.
   ENDIF
   SELE (tr_act)
   STORE ALLTRIM(MASTER) TO w_aux
   IF !USED(w_aux) .OR. seleccion=0
      RETURN fwlbusca(w_obli)
   ENDIF
   SELE (w_aux)
   IF EOF()
      DO fesper WITH "INEXISTENTE","Tecla !"
      RETURN .F.
   ENDIF
   RETURN .T.
PROCEDURE wfmenus
   PARAMETER omenu, verlas
   PRIVATE womenu, hot_key, fun_key, act_menu1, act_menu2, act_menu3
   PRIVATE val_barra, se_salta, ejc_prg, miop, barra, popy, l_pad
   PRIVATE ult_menu2, inactivas
   womenu    = "ME"+omenu
   ult_menu2 = ""
   fun_key   = ""
   miop      = "000"
   popy      = ""
   barra     = 0
   inactivas = ""
   acpad     = ""
   DO fselec WITH womenu,"","0"
   SET ORDER TO 1
   DEFINE MENU (womenu) BAR AT LINE 0 IN SCREEN COLOR (colmnu)
   GO TOP
   DO WHILE !EOF()
      se_salta = IIF(me01nivepw <= milevel,"","SKIP")
      hot_key  = ALLTRIM(IIF(EMPTY(me01keycut),"","KEY "+me01keycut))
      fun_key  = IIF(EMPTY(me01txtfun),fun_key,fun_key+ALLTRIM(me01txtfun)+"³")
      ejc_prg  = ALLTRIM(me01nomprg)
      IF !verlas .AND. !EMPTY(se_salta)
         inactivas = me01opcion+":"+inactivas
         SKIP
         LOOP
      ENDIF
      DO CASE
      CASE LEFT(me01opcion,2) = "00"
         act_menu1 = "M"+omenu+me01opcion
         act_menu2 = "P"+omenu+me01opcion
         l_pad     = act_menu1
         DEFINE PAD (act_menu1) OF (womenu) PROMPT ALLTRIM(me01prompt) MESSAGE ALLTRIM(me01descri) &hot_key &se_salta
         IF EMPTY(ejc_prg)
            DEFINE POPUP (act_menu2) MARGIN RELATIVE SHADOW COLOR (colmnu)
            ON PAD (act_menu1) OF (womenu) ACTIVATE POPUP (act_menu2)
         ELSE
            ON SELECTION PAD (act_menu1) OF (womenu) DO tmenu
         ENDIF
      CASE LEFT(me01opcion,1) = "0"
         act_menu2 = "P"+omenu+"0"+LEFT(me01opcion,2)
         act_menu3 = "T"+omenu+me01opcion
         IF ("0"+LEFT(me01opcion,2)) $ inactivas
            SKIP
            LOOP
         ENDIF
         val_barra = ASC(RIGHT(me01opcion,1))
         DEFINE BAR (val_barra) OF (act_menu2) PROMPT ALLTRIM(me01prompt) MESSAGE ALLTRIM(me01descri) &hot_key &se_salta
         IF EMPTY(ejc_prg)
            IF LEFT(ALLTRIM(me01prompt),2) <> "\-"
               DEFINE POPUP (act_menu3) MARGIN RELATIVE SHADOW COLOR (colmnu)
               ON BAR (val_barra) OF (act_menu2) ACTIVATE POPUP (act_menu3)
            ENDIF
         ELSE
            ON SELECTION POPUP (act_menu2) DO tmenu
         ENDIF
         ult_menu2 = act_menu2
      OTHERWISE
         act_menu2 = "T"+omenu+"0"+LEFT(me01opcion,2)
         IF !("0" +LEFT(me01opcion,2) $ inactivas) .AND.;
               !("00"+LEFT(me01opcion,1) $ inactivas)
            val_barra = ASC(RIGHT(me01opcion,1))
            DEFINE BAR (val_barra) OF (act_menu2) PROMPT ALLTRIM(me01prompt) MESSAGE ALLTRIM(me01descri) &hot_key &se_salta
            IF (ult_menu2 <> act_menu2) .AND. !EMPTY(ult_menu2)
               ON SELECTION POPUP (ult_menu2) DO tmenu
            ENDIF
            ult_menu2 = act_menu2
         ENDIF
         SKIP
         IF EOF()
            ON SELECTION POPUP (ult_menu2) DO tmenu
         ELSE
            se_salta = IIF(me01nivepw <= milevel,"","SKIP")
            IF (!verlas .AND. !EMPTY(se_salta))
               ON SELECTION POPUP (ult_menu2) DO tmenu
            ENDIF
         ENDIF
         SKIP -1
      ENDCASE
      SKIP
   ENDDO
   RELEASE hot_key, act_menu1, act_menu2, act_menu3
   RELEASE se_salta, ejc_prg, val_barra, ult_menu2, inactivas
   anter = 0
   DO fmtecfun WITH 1
   ACTIVATE MENU (womenu)
   DO WHILE anter <> 5
      DO CASE
      CASE LASTKEY() = 27
         ACTIVATE MENU (womenu) PAD (l_pad)
      CASE LEFT(miop,2) = "00"
         ACTIVATE MENU (womenu) PAD (acpad)
      OTHERWISE
         ACTIVATE POPUP (popy) BAR (barra)
      ENDCASE
   ENDDO
   DO fmtecfun WITH 0
   RELEASE MENU (womenu)
   anter = 0
   RETURN
PROCEDURE tmenu
   PRIVATE programa, mensaje
   miop  = IIF(EMPTY(BAR()), RIGHT(PAD(),3), RIGHT(POPUP(),2)+CHR(BAR()))
   acpad = PAD()
   barra = BAR()
   popy  = POPUP()
   anter = 0
   DO fselec WITH womenu,"","0"
   SET ORDER TO 1
   IF SEEK(miop)
      programa = ALLTRIM(me01nomprg)
      mensaje  = me01descri
      IF !EMPTY(programa)
         IF pide_password()
            USE
            HIDE MENU (womenu)
            ACTIVATE SCREEN
            @ 0,0 SAY LEFT(mensaje+SPACE(80),80) COLOR N/W
            DO fmtecfun WITH 0
            DO &programa
            DO fmtecfun WITH 0
            DO fmtecfun WITH 1
            SHOW MENU (womenu)
            DO fselec WITH womenu,"","0"
            SET ORDER TO 1
         ENDIF
      ENDIF
   ENDIF
   anter = 0
   RETURN
FUNCTION pide_password
   PRIVATE valor, B
   valor = .F.
   IF me01nivepw < 99
      valor = .T.
   ELSE
      B = "    "
      ACTIVATE WINDOW wtefu SAME
      CLEAR
      ACTIVATE WINDOW wmsgdis SAME
      CLEAR
      ON KEY LABE f3
      ON KEY LABE f9
      @ 0,2 SAY "Gesti¢n reservada al instalador. Pulse c¢digo de acceso " COLOR (colmsg)
      SET CURSOR ON
      @ 0,70 GET B PICTURE "!!!!" COLOR N/N,N,N
      READ
      CLEAR
      SET CURSOR OFF
      ACTIVATE SCREEN
      valor = (UPPER(B) = "PRC0")
   ENDIF
   RETURN (valor)
PROCEDURE fmtecfun
   PARAMETER x_accion
   PRIVATE w_ant,x_x, tecla
   w_ant = WOUTPUT()
   ACTIVATE WINDOW wtefu SAME
   CLEAR
   IF x_accion = 1
      @ 0,0 SAY fun_key COLOR (colpiv)
   ELSE
      FOR x_x = 2 TO 10
         tecla = "F"+ALLTRIM(STR(x_x,2))
         SET FUNCTION x_x TO ""
         ON KEY LABEL (tecla)
      NEXT
   ENDIF
   IF EMPTY(w_ant)
      ACTIVATE SCREEN
   ELSE
      ACTIVATE WINDOW (w_ant) SAME
   ENDIF
   RETURN
PROCEDURE acasa
   DO fmtecfun WITH 0
   anter = 5
   DEACTIVATE MENU (womenu)
   RETURN
PROCEDURE fecmes
   PARAMETER fecha,meses
   SET CENTURY OFF
   STORE DAY(fecha) TO dia
   STORE MONTH(fecha) TO mes
   STORE YEAR(fecha) TO any
   STORE mes+meses TO mes
   DO WHILE mes>12
      any=any+1
      mes=mes-12
   ENDDO
   resultado=""
   DO WHILE EMPTY(resultado)
      dias=RIGHT(STR(100+dia,3,0),2)
      MESS=RIGHT(STR(100+mes,3,0),2)
      anys=RIGHT(STR(10000+any,5,0),2)
      resultado=CTOD(dias+"."+MESS+"."+anys)
      dia=dia-1
   ENDDO
   RETURN resultado
PROCEDURE fwmsgesp
   PARAMETER accion,posrow,poscol,mensaje,ACTIVA
   DO CASE
   CASE accion=1
      IF EMPTY(mensaje)
         mensaje="Preparando y/o procesando la informaci¢n. Espere por favor ..."
      ENDIF
      ant_wind=WOUTPUT()
      DEFINE WINDOW pespera FROM posrow,poscol TO posrow+2,poscol+LEN(mensaje)+3;
         NOCLOSE;
         FLOAT;
         NOGROW;
         SHADOW;
         NOZOOM;
         COLOR (colpiv)
      ACTIVATE WINDOW pespera
      @ 0,1 SAY mensaje COLOR (colpiv)
      IF .NOT. ACTIVA
         IF !EMPTY(ant_wind)
            ACTIVATE WINDOW (ant_wind)
         ELSE
            ACTIVATE SCREEN
         ENDIF
      ENDIF
   OTHERWISE
      RELE WINDOW pespera
   ENDCASE
   RETURN
PROCEDURE fselec
   PARAMETER fa_fi,fa_in,fa_mo

   fa_fi=TRIM(fa_fi)
   IF !USED(fa_fi)
      SELE 0
      fa_tx=TRIM(fa_fi)
      IF !EMPTY(fa_in)
         fa_ex=fa_in
         fa_tx = fa_tx+" order tag "+fa_ex
      ENDIF
      IF fa_mo=="1"
         fa_tx=fa_tx+" exclusive"
      ENDIF
      
      USE &fa_tx Shared Again
   ELSE
      SELE (fa_fi)
      IF EMPTY(fa_in)
         SET ORDER TO 0
      ELSE
         SET ORDER TO TAG (fa_in)
      ENDIF
   ENDIF
   RETURN
PROCEDURE ftxnum
   PARAMETER w_num,w_sexo
   IF w_num=0
      RETURN "CERO"
   ENDIF
   w_f=STR(w_num,9,0)
   w_ft=SUBSTR(w_f,1,9)
   STORE RIGHT(TRIM("000000000"+LTRIM(w_ft)),9) TO w_f
   STORE "" TO w_c,w_b
   STORE 0 TO w_l,w_x
   STORE 1 TO w_i,w_k
   DO WHILE w_i#4
      DO WHILE w_k#4
         w_l=w_l+1
         w_x=w_x+1
         IF w_k=w_l
            IF SUBSTR(w_f,w_x,1)="9"
               w_b="NOVECIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="8"
               w_b="OCHOCIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="7"
               w_b="SETECIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="6"
               w_b="SEISCIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="5"
               w_b="QUINIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="4"
               w_b="CUATROCIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="3"
               w_b="TRESCIENT"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="2"
               w_b="DOSCIENT"
            ENDIF
            IF LEN(w_b)#0
               IF w_sexo="F" .AND. w_k>1
                  w_b=w_b+"AS "
               ELSE
                  w_b=w_b+"OS "
               ENDIF
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1" .AND. VAL(SUBSTR(w_f,w_x+1,2))>0
               w_b="CIENTO "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1" .AND. VAL(SUBSTR(w_f,w_x+1,2))=0
               w_b="CIEN "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="0"
               w_b=""
            ENDIF
         ENDIF
         STORE w_c+w_b TO w_c
         w_x=w_x+1
         IF w_k=w_l
            IF SUBSTR(w_f,w_x,1)="0"
               w_b=""
            ENDIF
            IF SUBSTR(w_f,w_x,1)="9"
               w_b="NOVENTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="8"
               w_b="OCHENTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="7"
               w_b="SETENTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="6"
               w_b="SESENTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="5"
               w_b="CINCUENTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="4"
               w_b="CUARENTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="3"
               w_b="TREINTA "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="2"  .AND.  SUBSTR(w_f,w_x+1,1)<>"0"
               w_b="VEINTI"
            ENDIF
            IF SUBSTR(w_f,w_x,1)="2"  .AND.  SUBSTR(w_f,w_x+1,1)="0"
               w_b="VEINTE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="0"
               w_b="DIEZ "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="1"
               w_b="ONCE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="2"
               w_b="DOCE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="3"
               w_b="TRECE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="4"
               w_b="CATORCE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="5"
               w_b="QUINCE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="6"
               w_b="DIECISEIS "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="7"
               w_b="DIECISIETE "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="8"
               w_b="DIECIOCHO "
            ENDIF
            IF SUBSTR(w_f,w_x,1)="1"  .AND.  SUBSTR(w_f,w_x+1,1)="9"
               w_b="DIECINUEVE "
            ENDIF
            STORE w_c+w_b TO w_c
            w_x=w_x+1
         ENDIF
         IF w_k=w_l
            w_b=""
            DO CASE
            CASE SUBSTR(w_f,w_x,1)="1"
               DO CASE
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  IF SUBSTR(w_f,w_x-1,1)="0" .AND. SUBSTR(w_f,w_x-2,1)="0" .AND. w_k=2
                     w_b=""
                  ELSE
                     IF w_sexo="F" .AND. w_k>1
                        w_b="UNA "
                     ELSE
                        w_b="UN "
                     ENDIF
                  ENDIF
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  IF w_sexo="F" .AND. w_k>1
                     w_b="Y UNA "
                  ELSE
                     w_b="Y UN "
                  ENDIF
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="2"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y DOS "
               CASE (SUBSTR(w_f,w_x-1,1)="2") .OR.(SUBSTR(w_f,w_x-1,1)="0")
                  w_b="DOS "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="3"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y TRES "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="TRES "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="4"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y CUATRO "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="CUATRO "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="5"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y CINCO "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="CINCO "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="6"
               DO CASE
               CASE SUBSTR(w_f,w_x,1)="6"  .AND.  VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y SEIS "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="SEIS "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="7"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y SIETE "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="SIETE "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="8"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y OCHO "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="OCHO "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="9"
               DO CASE
               CASE VAL(SUBSTR(w_f,w_x-1,1))>2
                  w_b="Y NUEVE "
               CASE SUBSTR(w_f,w_x-1,1)="2" .OR. SUBSTR(w_f,w_x-1,1)="0"
                  w_b="NUEVE "
               ENDCASE
            CASE SUBSTR(w_f,w_x,1)="0"
               w_b=""
            OTHER
               w_b=""
            ENDCASE
            STORE w_c+w_b TO w_c
            IF w_k=1 .AND. VAL(SUBSTR(w_f,1,3))>0 .AND. VAL(SUBSTR(w_f,1,3))<2
               STORE w_c+"MILLON " TO w_c
            ENDIF
            IF w_k=1 .AND. VAL(SUBSTR(w_f,1,3))>1
               STORE w_c+"MILLONES " TO w_c
            ENDIF
            IF w_k=2 .AND. VAL(SUBSTR(w_f,4,3))>0
               STORE w_c+"MIL " TO w_c
            ENDIF
         ENDIF
         w_k=w_k+1
      ENDDO
      w_i=w_i+1
   ENDDO
   STORE w_c TO rrr
   RETURN rrr
PROCEDURE fnumro
   PARAMETER w_num
   STORE "" TO w_c
   IF w_num=0
      STORE "*" TO w_c
      RETURN w_c
   ENDIF
   IF w_num>3999
      w_c="*"
      RETURN w_c
   ENDIF
   w_nume=STR(w_num,4,0)
   w_nume=RIGHT("0000"+w_nume,4)
   w_x=1
   DO WHILE w_x<5
      w_n=SUBSTR(w_nume,w_x,1)
      DO CASE
      CASE w_x=1
         IF VAL(w_n)>0
            w_c=w_c+REPLICATE("M",VAL(w_n))
         ENDIF
      CASE w_x=2
         DO CASE
         CASE w_n="1"
            w_c=w_c+"C"
         CASE w_n="2"
            w_c=w_c+"CC"
         CASE w_n="3"
            w_c=w_c+"CCC"
         CASE w_n="4"
            w_c=w_c+"CD"
         CASE w_n="5"
            w_c=w_c+"D"
         CASE w_n="6"
            w_c=w_c+"DC"
         CASE w_n="7"
            w_c=w_c+"DCC"
         CASE w_n="8"
            w_c=w_c+"DCCC"
         CASE w_n="9"
            w_c=w_c+"CM"
         ENDCASE
      CASE w_x=3
         DO CASE
         CASE w_n="1"
            w_c=w_c+"X"
         CASE w_n="2"
            w_c=w_c+"XX"
         CASE w_n="3"
            w_c=w_c+"XXX"
         CASE w_n="4"
            w_c=w_c+"XL"
         CASE w_n="5"
            w_c=w_c+"L"
         CASE w_n="6"
            w_c=w_c+"LX"
         CASE w_n="7"
            w_c=w_c+"LXX"
         CASE w_n="8"
            w_c=w_c+"LXXX"
         CASE w_n="9"
            w_c=w_c+"XC"
         ENDCASE
      CASE w_x=4
         DO CASE
         CASE w_n="1"
            w_c=w_c+"I"
         CASE w_n="2"
            w_c=w_c+"II"
         CASE w_n="3"
            w_c=w_c+"III"
         CASE w_n="4"
            w_c=w_c+"IV"
         CASE w_n="5"
            w_c=w_c+"V"
         CASE w_n="6"
            w_c=w_c+"VI"
         CASE w_n="7"
            w_c=w_c+"VII"
         CASE w_n="8"
            w_c=w_c+"VIII"
         CASE w_n="9"
            w_c=w_c+"IX"
         ENDCASE
      ENDCASE
      w_x=w_x+1
   ENDDO
   STORE w_c TO rrr
   RELE ALL LIKE w_*
   RETURN rrr
RETURN
PROCEDURE encripta
   PARAMETERS w_clau,w_niv
   taula=" >2¨)$_%9/&*(7+k#€<1QTe3Aw­oF60-O=R‡\@W5”EsYU]IPžqr8yui¦pjaSJL¥dDf4ghlH¤;¬t:ZX¯CGVBNzxcvbnm?!,K.'[Mœ®§„«"
   w_pos=1
   claenc=""
   FOR w_pos=1 TO 6
      STORE SUBS(w_clau,w_pos,1) TO w_car
      w_tau=AT(w_car,taula)+w_niv+w_pos
      IF w_tau>LEN(taula)
         w_tau=w_tau-LEN(taula)
      ENDIF
      claenc=claenc+SUBS(taula,w_tau,1)
   ENDFOR
   RETURN claenc
PROCEDURE desencri
   PARAMETERS w_clau,w_niv
   taula=" >2¨)$_%9/&*(7+k#€<1QTe3Aw­oF60-O=R‡\@W5”EsYU]IPžqr8yui¦pjaSJL¥dDf4ghlH¤;¬t:ZX¯CGVBNzxcvbnm?!,K.'[Mœ®§„«"
   w_pos=1
   claenc=""
   FOR w_pos=1 TO 6
      STORE SUBS(w_clau,w_pos,1) TO w_car
      w_tau=AT(w_car,taula)-w_niv-w_pos
      IF w_tau<=0
         w_tau=w_tau+LEN(taula)
      ENDIF
      claenc=claenc+SUBS(taula,w_tau,1)
   ENDFOR
   RETURN claenc
PROCEDURE fchoqu
   PARAMETER txt
   DECLARE texto(3)
   texto(1) = IIF(EMPTY(txt),"Registro o fichero bloqueado por otro usuario",txt)
   texto(2) = ""
   texto(3) = "Abandonamos proceso?"
   RETURN libsino(nil,@texto,.T.)
PROCEDURE FERROR
   PARAMETERS numerr,programa, linea
   wwantwin = WOUTPUT()
   DO CASE
   CASE numerr = 125
      DIMENSION texto(03)
      texto(01) = "ATENCION PROBLEMAS CON LA IMPRESORA"
      texto(02) = ""
      texto(03) = "Prepare la impresora. Desea reanudar el trabajo?"
      IF !libsino(nil,@texto,.T.)
         CLOSE DATA
         CLOSE INDEX
         CLOSE FORMAT
         CLOSE ALTERNATE
         SET DEVI TO SCREEN
         SET PRINT TO
         ww = WOUTPUT()
         DO WHILE .T.
            IF (ww <> "WINIT") .AND. (ww <> "WTEFU") .AND. ;
                  (ww <> "WMSGDIS") .AND. !EMPTY(ww)
               RELE WINDOW (ww)
               ww = WOUTPUT()
            ELSE
               EXIT
            ENDIF
         ENDDO
         RETURN TO wfmenus
      ENDIF
      IF EMPTY(wwantwin)
         ACTIVATE SCREEN
      ELSE
         ACTIVATE WINDOW (wwantwin) SAME
      ENDIF
      SET DEVI TO PRINT
      RETRY
   CASE numerr=216
      DIMENSION texto(04)
      texto(01) = "TIPO DE PANTALLA SELECCIONADO ERRONEAMENTE"
      texto(02) = ""
      texto(03) = "Acuda a la opci¢n mantenimiento de estaciones"
      texto(04) = "y modifique Tipo de pantalla"
      = liberror(nil,@texto, .T.)
      IF EMPTY(wwantwin)
         ACTIVATE SCREEN
      ELSE
         ACTIVATE WINDOW (wwantwin) SAME
      ENDIF
      RETURN
   CASE numerr=111 .OR. numerr=108 .OR. numerr=109
      DECLARE texto(3)
      texto(01) = "ATENCION REGISTRO O FICHERO BLOQUEADO POR OTRO USUARIO"
      texto(02) = ""
      texto(03) = "Desea reanudar el trabajo? "
      IF !libsino(nil,@texto,.T.)
         CLOSE DATA
         CLOSE INDEX
         CLOSE FORMAT
         CLOSE ALTERNATE
         ww = WOUTPUT()
         DO WHILE .T.
            IF (ww <> "WINIT") .AND. (ww <> "WTEFU") .AND. ;
                  (ww <> "WMSGDIS") .AND. !EMPTY(ww)
               RELE WINDOW (ww)
               ww = WOUTPUT()
            ELSE
               EXIT
            ENDIF
         ENDDO
         RETURN TO wfmenus
      ENDIF
      IF EMPTY(wwantwin)
         ACTIVATE SCREEN
      ELSE
         ACTIVATE WINDOW (wwantwin) SAME
      ENDIF
      RETRY
   ENDCASE
   DIMENSION texto(10)
   texto(01) = "  ATENCION. SE HA PRODUCIDO UN ERROR EN EL PROCESO  "
   texto(02) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   texto(03) = ""
   texto(04) = "Error N§ ....: "+STR(numerr,6,0)
   texto(05) = "Error texto .: "+LEFT(MESSAGE(),60)
   texto(06) = "En linea ....: "+STR(linea,6,0)
   texto(07) = "Del programa : "+programa
   texto(08) = ""
   texto(09) = "COPIE LOS DATOS APARECIDOS EN ESTA PANTALLA "
   texto(10) = "Y CONTACTE CON SU INSTALADOR."
   = liberror(nil,@texto, .F.)
   CLOSE ALL
   QUIT
   RETURN
PROCEDURE fchklk
   PARAMETER txt
   DO WHILE !LOCK()
      IF fchoqu(txt)
         RELE txt
         RETURN .F.
      ENDIF
   ENDDO
   RELE txt
   RETURN .T.
PROCEDURE fprsel
   PRIVATE f_anterior, w_anterior, c_impresora
   PRIVATE n_cantidad, n_limite, n_limalt, n_limbaj
   PRIVATE menuimp, n_cta, n_fila, n_columna, c_txt, c_fichero
   f_anterior = SELECT()
   w_anterior = WOUTPUT()
   c_impresora = ""
   DO fselec WITH "mprn","","0"
   GOTO TOP
   COUNT TO n_cantidad FOR !DELETED()
   GOTO TOP
   n_cantidad = IIF(n_cantidad > 16, 16, n_cantidad)
   n_limite   = (INT(CEILING(n_cantidad / 2))+ 1) * 2
   n_limalt   = 12-INT(n_limite / 2)-1
   n_limbaj   = 12+INT(n_limite / 2)+1
   DEFINE WINDOW wprsel FROM n_limalt,22 TO n_limbaj,59 NOCLOSE SHADOW;
      FLOAT NOGROW TITLE "SELECCION DE IMPRESORA" NOZOOM COLOR (colpan)
   ACTIVATE WINDOW wprsel
   @ WROWS()-2 ,0 SAY REPLICATE("Ä",WCOLS()) COLOR (colpan)
   DEFINE MENU menuimp
   n_cta   = 1
   n_fila  = 1
   IF !EMPTY(n_cantidad)
      DO WHILE !EOF() .AND. (n_cta <= 16)
         n_columna = IIF(MOD(n_cta,2) <> 0, 2 , 20)
         c_txt = "C"+RIGHT("00"+ALLTRIM(STR(n_cta,2)),2)
         DEFINE PAD &c_txt OF menuimp ;
            PROMPT "\<"+STR(n_cta,1,0)+" "+mprnnombre;
            AT n_fila, n_columna ;
            MESSAGE "Imprimiendo por "+ALLTRIM(mprndevice);
            COLOR (colmnu)
         SKIP
         n_cta  = n_cta + 1
         n_fila = IIF(MOD(n_cta,2) = 0, n_fila, n_fila+2)
      ENDDO
      n_cta = n_cta - 1
   ENDIF
   c_txt = "C"+RIGHT("00"+ALLTRIM(STR(n_cta+1,2)),2)
   DEFINE PAD &c_txt OF menuimp PROMPT "\<Fichero     " AT WROWS()-1, 2;
      MESSAGE "Activa el fichero de impresi¢n" COLOR (colmnu)
   c_txt = "C"+RIGHT("00"+ALLTRIM(STR(n_cta+2,2)),2)
   DEFINE PAD &c_txt OF menuimp PROMPT "\<Salir       " AT WROWS()-1,20;
      MESSAGE "Avandona la selecci¢n de impresoras" COLOR (colmnu)
   ON SELECTION MENU menuimp DEACTIVATE MENU menuimp
   ACTIVATE MENU menuimp
   RELEASE MENU menuimp
   RELE WINDOW wprsel
   n_cta = VAL(RIGHT(PAD(),2))
   DO CASE
   CASE (n_cta = 0) .OR. (n_cta = (n_cantidad + 2))
      STORE "" TO prnini, prnfin, prnver, prnhor, prnnor, prnp12,;
         prncon, prnneg, prncne, prnsub, prncsu, prncur, prnccu
      prnnli = 66
   CASE n_cta = (n_cantidad + 1)
      c_fichero = SPACE(40)
      SET FUNCTION 9 TO ";"
      DEFINE WINDOW wpidefi FROM 10,5 TO 15,75 NOCLOSE SHADOW;
         FLOAT NOGROW NOZOOM COLOR (colmsg)
      ACTIVATE WINDOW wpidefi
      @ 3,1 SAY "Introduzca el camino de acceso completo. F9 para salir."
      DO WHILE .T.
         SET CURSOR ON
         @ 1,2 SAY "Nombre de fichero.: " GET c_fichero;
            PICT REPLICATE("!",40) COLOR (colmsg)
         READ
         CLEAR GETS
         SET CURSOR OFF
         IF LASTKEY()=-8
            c_fich=""
            EXIT
         ENDIF
         c_fich = ALLTRIM(c_fichero)
         IF EMPTY(c_fich)
            LOOP
         ENDIF
         IF FILE(c_fich)
            IF !libsino(nil,"EL fichero : "+c_fich+"|ya existe. Desea sobreescribirlo?", .T.)
               LOOP
            ENDIF
         ENDIF
         EXIT
      ENDDO
      SET FUNCTION 9 TO ""
      RELEASE WINDOW wpidefi
      c_impresora = c_fich
      STORE "" TO prnini, prnfin, prnver, prnhor, prnnor, prnp12,;
         prncon, prnneg, prncne, prnsub, prncsu, prncur, prnccu
      prnnli = 66
   CASE n_cta <= n_cantidad
      GOTO TOP
      FOR n_valor = 1 TO n_cta-1
         SKIP
      NEXT
      RESTORE FROM MEMO mprnchrctr ADDITIVE
      prnnli = IIF(EMPTY(prnnli), 66, prnnli)
      c_impresora = mprndevice
   ENDCASE
   _PLENGTH = prnnli
   USE IN mprn
   SELE (f_anterior)
   IF EMPTY(w_anterior)
      ACTIVATE SCREEN
   ELSE
      ACTIVATE WINDOW (w_anterior)
   ENDIF
   RETURN c_impresora
PROCEDURE fprsel2
   DEFINE WINDOW wprsel FROM 19,0 TO 23,79 NOCLOSE SHADOW FLOAT NOGROW NOZOOM COLOR (colwin)
   ACTIVATE WINDOW wprsel
   DO fselec WITH "MPRN","","0"
   GO TOP
   @ 2,0 SAY "SELECCIONE LA IMPRESORA DESEADA, asegurese que este lista para imprimir " COLOR (colpiv)
   STORE 1 TO p_col
   STORE 0 TO n_opc
   DO WHILE !EOF()
      @ 0,p_col PROMPT STR(n_opc+1,1,0)+""+mprnnombre
      p_col=p_col+13
      n_opc=n_opc+1
      SKIP
   ENDDO
   @ 1,1 PROMPT "Fichero"
   @ 1,12 PROMPT "Salir  "
   n_opc=n_opc+2
   MENU TO o_men
   IF o_men<n_opc-1
      SELE mprn
      GO o_men
      RESTORE FROM MEMO mprnchrctr ADDITIVE
      o_rtn=mprndevice
   ELSE
      IF o_men=n_opc-1
         STORE SPACE(40) TO o_rtn
         @ 2,0 SAY SPACE(78)
         @ 2,0 SAY "Nombre de fichero (Camino completo) " GET o_rtn PICT REPLICATE("!",40)
         READ
         CLEAR GETS
         o_rtn=TRIM(o_rtn)
         IF FILE(o_rtn)
            @ 2,0 SAY SPACE(78)
            STORE "N" TO rsp
            @ 2,0 SAY "El fichero : "+o_rtn+" ya existe. Desea sobreescribirlo ? " GET rsp PICT "!" VALID(rsp$"SN")
            READ
            CLEAR GETS
            IF rsp=="N"
               o_rtn=""
            ENDIF
         ENDIF
         prnini=""
         prnfin=""
         prnver=""
         prnhor=""
         prnnor=""
         prnp12=""
         prncon=""
         prnneg=""
         prncne=""
         prnsub=""
         prncsu=""
         prncur=""
         prnccu=""
      ELSE
         o_rtn=""
      ENDIF
   ENDIF
   SELE mprn
   USE
   RELE WINDOW wprsel
   RETURN o_rtn
PROCEDURE fntemp
   PARAMETER x_t
   STORE pattmp()+Sys(2015) TO x_f
   DO CASE
   CASE x_t=1
      RETURN x_f+".IDX"
   CASE x_t=2
      RETURN x_f+".MEM"
   CASE x_t=3
      RETURN x_f+".DBF"
   OTHER
      RETURN x_f
   ENDCASE
   RETURN
PROCEDURE fesper
   PARAMETERS fa_1,fa_2
   PRIVATE w_ant, aseguro, numlin,texto,i
   numlin=0
   FOR i=1 TO LEN(fa_1)
      IF SUBSTR(fa_1,i,1)=="|"
         numlin = numlin+1
      ENDIF
   ENDFOR
   numlin=numlin+1
   DIMENSION texto(numlin)
   FOR i=1 TO numlin
      texto[i]=fwextrac(i,fa_1,"|")
   ENDFOR
   w_ant=WOUTPUT()
   =soustd(612,3)
   =(libaviso("",@texto, .T.))
   IF EMPTY(w_ant)
      ACTIVATE SCREE
   ELSE
      IF LEN(w_ant)<=8
         ACTIVATE WINDOW (w_ant) SAME
      ENDIF
   ENDIF
   RELE texto
   RETURN 0
PROCEDURE fwextrac
   PARAMETER nu_camp,st_comp,SEPARA
   STORE 0 TO n_f,n_i,n_l
   STORE "" TO fw_salida
   n_f=ATC(SEPARA,st_comp,nu_camp)
   IF nu_camp>1
      n_i=ATC(SEPARA,st_comp,nu_camp-1)
      IF n_i>0
         n_i=n_i+1
      ENDIF
   ELSE
      n_i=1
   ENDIF
   IF n_f=0
      n_f=LEN(TRIM(st_comp))+1
   ENDIF
   n_l=n_f-(n_i)
   IF n_l<1
      fw_salida=""
   ELSE
      fw_salida=SUBSTR(st_comp,n_i,n_l)
   ENDIF
   RETURN fw_salida
PROCEDURE stod
   PARAMETER fff
   IF TYPE("fff")#"C"
      STORE "  .  .  " TO fff
   ELSE
      IF EMPTY(fff) .OR. LEN(fff)#8
         fff="  .  .  "
      ELSE
         fff = RIGHT(fff,2)+"."+SUBSTR(fff,5,2)+"."+LEFT(fff,4)
      ENDIF
   ENDIF
   ffff= CTOD(fff)
   RETURN ffff
PROCEDURE fwlesrel
   PARAMETER al
   X=1
   xx=TARGET(X)
   DO WHILE !EMPTY(xx)
      IF TRIM(UPPER(xx))==TRIM(UPPER(al))
         RETURN .T.
      ENDIF
      X=X+1
      xx=TARGET(X)
   ENDDO
   RETURN .F.
PROCEDURE fdocv

      c_fichero = SPACE(40)
      SET FUNCTION 9 TO ";"
      DEFINE WINDOW wpidefi FROM 10,5 TO 15,75 NOCLOSE SHADOW;
         FLOAT NOGROW NOZOOM COLOR (colmsg)
      ACTIVATE WINDOW wpidefi
      @ 3,1 SAY "Introduzca el camino de acceso completo. F9 para salir."
      DO WHILE .T.
         SET CURSOR ON
         @ 1,2 SAY "Nombre de fichero.: " GET c_fichero;
            PICT REPLICATE("!",40) COLOR (colmsg)
         READ
         CLEAR GETS
         SET CURSOR OFF
         IF LASTKEY()=-8
            c_fich=""
            EXIT
         ENDIF
         c_fich = ALLTRIM(c_fichero)
         IF EMPTY(c_fich)
            LOOP
         ENDIF
         EXIT
      ENDDO
      SET FUNCTION 9 TO ""
      RELEASE WINDOW wpidefi
      n_fic = c_fich
   IF !EMPTY(n_fic)
      ACTIVATE WINDOW wmsgdis SAME
      @ 0,0 SAY "PARA MOVERSE UTILICE: Cursores y P ginas  F9: Salir"
      SET FUNCTION 9 TO ""
      SET MEMOWIDTH TO 250
      DEFINE WINDOW wconst FROM 0,0 TO 21, 79 ;
      TITLE "PANTALLA DE CONSULTA" CLOSE FLOAT GROW SHADOW ZOOM COLOR (colpan)
      IF !EMPTY(n_fic)
         MODI COMM (n_fic) NOEDIT WINDOW wconst
      ENDIF
      RELE WINDOW wconst
      SET MEMOWIDTH TO 50
      SET FUNCTION 9 TO ""
      ACTIVATE WINDOW wmsgdis SAME
      CLEAR
   ENDIF
   ACTI SCREEN
   RETURN
PROCEDURE pattmp
   PRIVATE w_ptm
   PRIVATE w_cfg
   PRIVATE w_hnd
   PRIVATE w_str
   w_ptm=""
   w_cfg=SYS(2019)
   w_hnd=-1
   IF !EMPTY(w_cfg)
      IF FILE(w_cfg)
         w_hnd = FOPEN(w_cfg,0)
      ENDIF
      IF w_hnd > 0
         =FSEEK(w_hnd, 0)
         DO WHILE .T.
            w_str = FGETS(w_hnd)
            IF EMPTY(w_str)
               EXIT
            ENDIF
            w_str=UPPER(w_str)
            IF LEFT(w_str,8)=="TMPFILES"
               w_ptm=ALLTRIM(RIGHT(w_str,(LEN(w_str)-AT("=",w_str))))
               EXIT
            ENDIF
         ENDDO
         =FCLOSE(w_hnd)
      ENDIF
   ENDIF
   RETURN w_ptm
FUNCTION strzero
   PARAMETER valor, cantidad
   RETURN RIGHT(REPLICATE("0",cantidad)+ALLTRIM(STR(valor,cantidad)),cantidad)
PROCEDURE masusu
   PARAMETER m_opc
   DO fselec WITH "Meus","Meui","0"
   GO TOP
   DO WHILE .NOT.EOF()
      IF meuscodigo#miusu
         IF !EMPTY(meusopcion)
            IF m_opc
               IF meusopcion==m_opc
                  RETURN .T.
               ENDIF
            ELSE
               RETURN .T.
            ENDIF
         ENDIF
      ENDIF
      SKIP
   ENDDO
   RETURN .F.
FUNCTION badentry
   PRIVATE atextos
   DECLARE atextos(15)
   atextos( 1) = "­­ ATENCION !!"
   atextos( 2) = "   Se ha detectado una anormalidad en la entrada al programa  "
   atextos( 3) = "   puede ser debido a cualquiera de los siguientes casos :    "
   atextos( 4) = "                                                              "
   atextos( 5) = "      1.- Existe otro operador usando la base de datos identi_"
   atextos( 6) = "          ficado con el mismo c¢digo de usuario. (Debe abando_"
   atextos( 7) = "          nar antes de que usted pueda continuar. Pulse <N> y "
   atextos( 8) = "          vuelva a intentarlo).                               "
   atextos( 9) = "      2.- Usted Abandon¢ el programa de forma irregular en el "
   atextos(10) = "          acceso anterior. (Pulse <S> y ejecute la opci¢n de  "
   atextos(11) = "          REINDEXAR para salvar la integridad de los datos.)  "
   atextos(12) = "                                                              "
   atextos(13) = "   Cualquier otra decisi¢n diferente de las indicadas puede   "
   atextos(14) = "   afectar negativamente a la integridad de sus datos y por   "
   atextos(15) = "   tanto es encarecidamente desaconsejada.                    "
   RETURN libsino("",@atextos, .F.)
PROCEDURE wfpideop
   DEFINE WINDOW ope1 FROM 10,10 TO 16,68 SHADOW NOCLOSE FLOAT NOGROW NOZOOM COLOR (colavi)
   ACTIVATE WINDOW ope1 SAME
   SELE meus
   CLEAR
   SET CURSOR ON
   @ 1,2 SAY "Introduzca C¢digo de Operador.: " GET miusu PICT "!!!!" COLOR (colavi)
   READ
   SET CURSOR OFF
   IF !SEEK(miusu)
      DO fesper WITH "ACCESO DENEGADO. No esta en la lista de usuarios",""
      miusu = "    "
      RELE WINDOW ope1
      RETURN
   ENDIF
   IF meusopcion # "    "
      IF !badentry()
         miusu = "    "
         RELE WINDOW ope1
         RETURN
      ENDIF
   ENDIF
   STORE SPACE(6) TO miclav
   SET CURSOR ON
   @ 3,2  SAY "Introduzca clave de acceso....: " COLOR (colavi)
   @ 3,33 GET miclav COLOR ,N/N
   READ
   SET CURSOR OFF
   IF meusclaves # encripta(miclav,meusnivacc)
      DO fesper WITH "ACCESO DENEGADO. Clave incorrecta",""
      miusu = "    "
      RELE WINDOW ope1
      RETURN
   ENDIF
   REPLACE meusopcion WITH "000"
   STORE TRIM(meuspatdat) TO mipath
   STORE meusnivacc       TO milevel
   actpath = Sys(2001,"PATH")
   actpath = actpath+";"+mipath
   SET PATH TO &actpath
   RELE WINDOW ope1
   RETURN
PROCEDURE wflisusu
   PRIVATE numusu,texto,N
   DO fselec WITH "Meus","Meui","0"
   GO TOP
   COUNT TO numusu FOR !EMPTY(meusopcion)
   DIMENSION texto(numusu+4)
   texto[1]="NUMERO DE USUARIOS CONECTADOS : "+TRAN(numusu,"999")
   texto[2]="ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   texto[3]=" C¢digo Nombre                     Opci¢n "
   texto[4]="ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   N = 5
   GO TOP
   DO WHILE !EOF()
      IF !EMPTY(meusopcion)
         IF meuscodigo=miusu
            texto[n]="*"
         ELSE
            texto[n]=" "
         ENDIF
         texto[n]=texto[n]+meuscodigo+"   "+meusdescri+"       "+meusopcion
         N = N + 1
      ENDIF
      SKIP
   ENDDO
   DO libaviso WITH nil,texto,.F.
   RETURN
FUNCTION libaviso
   PARAMETERS ctitulo, atxt, lcentrado
   PRIVATE ctxttitle
   ctxttitle = IIF(TYPE('cTitulo') == 'C', ctitulo, "AVISO")
   RETURN (lbdlggeneric(1, ctxttitle, @atxt, lcentrado, colavi))
FUNCTION liberror
   PARAMETERS ctitulo, atxt, lcentrado
   PRIVATE ctxttitle
   ctxttitle = IIF(TYPE('cTitulo') == 'C', ctitulo, "­ERROR!")
   = souerror()
   RETURN (lbdlggeneric(2, ctxttitle, @atxt, lcentrado, colavi))
FUNCTION libsino
   PARAMETERS ctitulo, atxt, lcentrado
   PRIVATE ctxttitle
   ctxttitle = IIF(TYPE('cTitulo') == 'C', ctitulo, "CONFIRMAR")
   nretorno = lbdlggeneric(3, ctxttitle, @atxt, lcentrado, colpan)
   RETURN (IIF(nretorno = 1, true, false))
FUNCTION libsinocancel
   PARAMETERS ctitulo, atxt, lcentrado
   PRIVATE ctxttitle
   ctxttitle = IIF(TYPE('cTitulo') == 'C', ctitulo, "CONFIRMAR")
   RETURN (lbdlggeneric(4, ctxttitle, @atxt, lcentrado, colpan))
FUNCTION libnseek
   DECLARE atxt[7]
   atxt[1] = "El c¢digo auxiliar utilizado no ha sido encontrado en"
   atxt[2] = "el archivo correspondiente."
   atxt[3] = ""
   atxt[4] = "Verifique que el c¢digo introducido es correcto o"
   atxt[5] = "utilice el mantenimiento de ficheros auxiliares para"
   atxt[6] = "a¤adirlo al archivo correspondiente antes de proceder"
   atxt[7] = "con esta opci¢n."
   RETURN (liberror(false, @atxt, false))
FUNCTION lbdlggeneric
   PARAMETERS nwndtipo, ctitulo, xtxto, lcentrado, ccolorset
   EXTERNAL ARRAY xtxto
   PRIVATE i
   PRIVATE nlineas
   PRIVATE cstrtxt
   PRIVATE wndentry
   PRIVATE wndname
   PRIVATE nlineas
   PRIVATE nmaxlen
   PRIVATE ndrow, ndcol, narow, nacol
   PRIVATE nanchobt
   PRIVATE cstrbt
   PRIVATE nchoice
   PRIVATE xconsole
   PRIVATE xprinter
   PRIVATE cta
   PRIVATE tecla
   xconsole = SET("CONSOLE")
   xprinter = SET("PRINTER")
   wndentry = WOUTPUT()
   DECLARE lb_func(10)
   FOR cta = 1 TO 10
      tecla = "F"+ALLTRIM(STR(cta,2))
      lb_func(cta) = ON("KEY",tecla)+""
      ON KEY LABEL (tecla) DO void
   NEXT
   DO CASE
   CASE (TYPE('xTxto[1]') = 'U')
      nlineas = 1
      cstrtxt = xtxto
      DO WHILE (AT('|', cstrtxt) != 0)
         nlineas = nlineas + 1
         cstrtxt = SUBSTR(cstrtxt, AT('|', cstrtxt)+1)
      ENDDO
      DECLARE atexto[nLineas]
      cstrtxt = xtxto
      FOR i=1 TO nlineas
         IF (AT('|', cstrtxt) != 0)
            atexto[i] = SUBSTR(cstrtxt, 1, AT('|', cstrtxt)-1)
            cstrtxt = SUBSTR(cstrtxt, AT('|', cstrtxt)+1)
         ELSE
            atexto[i] = cstrtxt
         ENDIF
      NEXT
   OTHER
      DECLARE atexto[ALen(xTxto)]
      FOR i=1 TO ALEN(xtxto)
         atexto[i] = xtxto[i]
      NEXT
   ENDCASE
   DO CASE
   CASE (nwndtipo == 1)
      nanchobt = 10
      cstrbt = '"*H \!\<Aceptar" Size 1, 11'
   CASE (nwndtipo == 2)
      nanchobt = 10
      cstrbt = '"*H \!\<Aceptar" Size 1, 11'
   CASE (nwndtipo == 3)
      nanchobt = 25
      cstrbt = '"*H \!\<Si;\<No" Size 1, 10, 5'
   CASE (nwndtipo == 4)
      nanchobt = 46
      cstrbt = '"*H \!\<Si;\<No;\?\<Cancelar" Size 1, 12, 5'
   ENDCASE
   wndname = "wndID" + ALLTRIM(STR(INT(RAND() * 100), 3))
   DO WHILE (WEXIST(wndname))
      wndname = "wndID" + ALLTRIM(STR(INT(RAND() * 100), 3))
   ENDDO
   nlineas = ALEN(atexto)
   nmaxlen = MAX(LEN(ctitulo)+4, nanchobt)
   FOR i=1 TO nlineas
      nmaxlen = MAX(nmaxlen, LEN(atexto[i]))
   NEXT
   ndrow = 0
   ndcol = 0
   narow = nlineas + 5
   nacol = nmaxlen + 3
   ndcol = (SCOLS() / 2) - (nacol / 2)
   nacol = ndcol + nacol
   ndrow = (SROWS() / 2) - (narow / 2)
   narow = ndrow + narow
   DEFINE WINDOW &wndname ;
      FROM ndrow, ndcol TO narow, nacol ;
      IN SCREEN ;
      TITLE (ctitulo) ;
      &miborde ;
      NOCLOSE ;
      FLOAT ;
      NOGROW ;
      SHADOW ;
      NOZOOM ;
      COLOR (ccolorset)
   ACTIVATE WINDOW &wndname SAME
   FOR i=1 TO nlineas
      IF (lcentrado)
         @ i, 1 SAY PADC(atexto[i], nmaxlen)
      ELSE
         @ i, 1 SAY atexto[i]
      ENDIF
   NEXT
   nchoice = 1
   @ (WROWS()-2), ((WCOLS()/2) - (nanchobt/2)) ;
      GET nchoice ;
      FUNCTION &cstrbt ;
      COLOR (ccolorset)
   READ CYCLE
   RELEASE WINDOWS &wndname
   SET CONSOLE &xconsole
   SET PRINTER &xprinter
   FOR cta = 1 TO 10
      tecla = "F"+ALLTRIM(STR(cta,2))
      IF EMPTY(lb_func(cta))
         ON KEY LABEL (tecla)
      ELSE
         ON KEY LABEL (tecla) &lb_func(cta)
      ENDIF
   NEXT
   IF (!EMPTY(wndentry))
      ACTIVATE WINDOW (wndentry) SAME
   ELSE
      ACTIVATE SCREEN
   ENDIF
   RETURN (nchoice)
FUNCTION libproceso
   PARAMETERS noperacion, wndid, ctitulo, atexto, lcentrado
   PRIVATE cretorno
   EXTERNAL ARRAY atexto
   PRIVATE xconsole
   PRIVATE xprinter
   xconsole = SET("CONSOLE")
   xprinter = SET("PRINTER")
   cretorno = ""
   DO CASE
   CASE (noperacion = 0)
      cretorno = _lbpromd(ctitulo, @atexto, lcentrado, colavi)
   CASE (noperacion = 1)
      cretorno = _lbprocd(wndid, ctitulo, @atexto, lcentrado, colavi)
   CASE (noperacion = 2)
      cretorno = _lbprord(wndid)
   ENDCASE
   SET CONSOLE &xconsole
   SET PRINTER &xprinter
   RETURN (cretorno)
FUNCTION _lbpromd
   PARAMETERS xttl, xtxto, lcentrado, ccolorset
   EXTERNAL ARRAY xtxto
   PRIVATE i
   PRIVATE ctitulo
   PRIVATE wndname
   PRIVATE nlineas
   PRIVATE nmaxlen
   PRIVATE ndrow, ndcol, narow, nacol
   DO CASE
   CASE (TYPE('xTxto[1]') = 'U')
      DECLARE atexto[1]
      atexto[1] = xtxto
   OTHER
      DECLARE atexto[ALen(xTxto)]
      FOR i=1 TO ALEN(xtxto)
         atexto[i] = xtxto[i]
      NEXT
   ENDCASE
   ctitulo = "PROCESO"
   IF (TYPE('xTtl') = 'C')
      IF (!EMPTY(xttl))
         ctitulo = (ALLTRIM(xttl))
      ELSE
         ctitulo = ""
      ENDIF
   ENDIF
   wndname = "wndID" + ALLTRIM(STR(INT(RAND() * 100), 3))
   DO WHILE (WEXIST(wndname))
      wndname = "wndID" + ALLTRIM(STR(INT(RAND() * 100), 3))
   ENDDO
   nlineas = ALEN(atexto)
   nmaxlen = LEN(ctitulo)+4
   FOR i=1 TO nlineas
      nmaxlen = MAX(nmaxlen, LEN(atexto[i]))
   NEXT
   ndrow = 0
   ndcol = 0
   narow = nlineas + 3
   nacol = nmaxlen + 3
   ndcol = (SCOLS() / 2) - (nacol / 2)
   nacol = ndcol + nacol
   ndrow = (SROWS() / 2) - (narow / 2)
   narow = ndrow + narow
   DEFINE WINDOW &wndname ;
      FROM ndrow, ndcol TO narow, nacol ;
      IN SCREEN ;
      TITLE ctitulo ;
      &miborde ;
      NOCLOSE ;
      FLOAT ;
      NOGROW ;
      SHADOW ;
      NOZOOM ;
      COLOR (ccolorset)
   ACTIVATE WINDOW &wndname SAME
   FOR i=1 TO nlineas
      IF (lcentrado)
         @ i, 1 SAY PADC(atexto[i], nmaxlen)
      ELSE
         @ i, 1 SAY atexto[i]
      ENDIF
   NEXT
   RETURN (wndname)
FUNCTION _lbprocd
   PARAMETERS wndid, xttl, atexto, lcentrado, ccolorset
   PRIVATE i
   ACTIVATE WINDOW &wndid SAME
   FOR i=1 TO nlineas
      IF (lcentrado)
         @ i, 1 SAY PADC(atexto[i], nmaxlen)
      ELSE
         @ i, 1 SAY PADR(atexto[i], nmaxlen)
      ENDIF
   NEXT
   RETURN (wndid)
FUNCTION _lbprord
   PARAMETERS wndid
   RELEASE WINDOW &wndid
   RETURN (wndid)
FUNCTION libespere
   PARAMETERS noperacion, wndid
   PRIVATE cretorno
   DECLARE atxt[2]
   atxt[1] = "Espere mientras se recuperan los recursos necesarios"
   atxt[2] = "para atender su petici¢n."
   DO CASE
   CASE (noperacion = 0)
      cretorno = libproceso(0, false, false, @atxt, false)
   CASE (noperacion = 1)
      cretorno = libproceso(1, wndid, false, @atxt, false)
   CASE (noperacion = 2)
      cretorno = libproceso(2, wndid)
   ENDCASE
   RETURN (cretorno)
FUNCTION libpctbox
   PARAMETERS noperacion, wndid, xparam1, xparam2, xparam3
   PRIVATE cretorno
   PRIVATE xconsole
   PRIVATE xprinter
   xconsole = SET("CONSOLE")
   xprinter = SET("PRINTER")
   cretorno = wndid
   DO CASE
   CASE (noperacion = 0)
      cretorno = _lbpctmd(wndid, xparam1, @xparam2, xparam3)
   CASE (noperacion = 1)
      cretorno = _lbpctcd(wndid, xparam1, xparam2, xparam3)
   CASE (noperacion = 2)
      cretorno = _lbpctrd(wndid)
   ENDCASE
   SET CONSOLE &xconsole
   SET PRINTER &xprinter
   RETURN (cretorno)
FUNCTION _lbpctmd
   PARAMETERS wndid, ctitulo, xtxto, lcentrado
   EXTERNAL ARRAY atexto
   PRIVATE i
   PRIVATE ctitulo
   PRIVATE wndbox
   PRIVATE nlineas
   PRIVATE nmaxlen
   PRIVATE ndrow, ndcol, narow, nacol
   ctitulo = "PROCESO"
   IF (TYPE('xTtl') = 'C')
      ctitulo = (ALLTRIM(xttl))
   ENDIF
   DO CASE
   CASE (TYPE('xTxto[1]') = 'U')
      DECLARE atexto[1]
      atexto[1] = xtxto
   OTHER
      DECLARE atexto[ALen(xTxto)]
      FOR i=1 TO ALEN(xtxto)
         atexto[i] = xtxto[i]
      NEXT
   ENDCASE
   IF (EMPTY(wndid))
      wndbox = "wndID" + ALLTRIM(STR(INT(RAND() * 100), 3))
      DO WHILE (WEXIST(wndbox))
         wndbox = "wndID" + ALLTRIM(STR(INT(RAND() * 100), 3))
      ENDDO
   ELSE
      wndbox = wndid
   ENDIF
   nlineas = ALEN(atexto)
   nmaxlen = 50
   FOR i=1 TO nlineas
      nmaxlen = MAX(nmaxlen, LEN(atexto[i]))
   NEXT
   ndrow = 0
   ndcol = 0
   narow = nlineas + 5
   nacol = nmaxlen + 3
   ndcol = (SCOLS() / 2) - (nacol / 2)
   nacol = ndcol + nacol
   ndrow = (SROWS() / 2) - (narow / 2)
   narow = ndrow + narow + 2
   DEFINE WINDOW &wndbox ;
      FROM ndrow, ndcol TO narow, nacol ;
      IN SCREEN ;
      TITLE ctitulo ;
      &miborde ;
      NOCLOSE ;
      FLOAT ;
      NOGROW ;
      SHADOW ;
      NOZOOM ;
      COLOR 
   ACTIVATE WINDOW &wndbox SAME
   FOR i=1 TO nlineas
      IF (lcentrado)
         @ i, 1 SAY PADC(atexto[i], nmaxlen)
      ELSE
         @ i, 1 SAY atexto[i]
      ENDIF
   NEXT
   @ (WROWS()-4), 1 TO (WROWS()-2), (WCOLS()-2)
   @ (WROWS()-2), 1 SAY CHR(195)
   @ (WROWS()-2), (WCOLS()-2) SAY CHR(180)
   @ (WROWS()-2), (WCOLS()/2) SAY CHR(194)
   @ (WROWS()-1), 1 SAY '0%'
   @ (WROWS()-1), (WCOLS()/2)-1 SAY '50%'
   @ (WROWS()-1), (WCOLS()-5) SAY '100%'
   RETURN (wndbox)
FUNCTION _lbpctcd
   PARAMETERS wndbox, nactual, ntotales, ninterval
   PRIVATE npct
   PRIVATE nboxavail
   PRIVATE nboxreq
   IF (nactual % ninterval = 0 .OR. nactual = ntotales)
      ACTIVATE WINDOW &wndbox SAME
      npct = 100
      IF (ntotales != 0 .AND. nactual <= ntotales)
         npct = INT((nactual * 100) / ntotales)
      ENDIF
      nboxavail = WCOLS()-4
      nboxreq = INT((npct * nboxavail) / 100)
      @ (WROWS()-3), 2 SAY REPLICATE(CHR(219), nboxreq)
   ENDIF
   RETURN (wndbox)
FUNCTION  _lbpctrd
   PARAMETERS wndbox
   RELEASE WINDOW &wndbox
   RETURN (wndbox)
PROCEDURE void
   RETURN
FUNCTION souerror
   = soustd(150, 8)
   RETURN
FUNCTION soudone
   DECLARE afq[4]
   DECLARE atm[4]
   afq[1] = 300
   afq[2] = 100
   afq[3] = 300
   afq[4] = 100
   atm[1] = 3
   atm[2] = 3
   atm[3] = 3
   atm[4] = 3
   = soustd(@afq, @atm)
   RETURN
FUNCTION soustd
   PARAMETERS xfreq, xtime
   EXTERNAL ARRAY xfreq
   EXTERNAL ARRAY xtime
   PRIVATE i
   PRIVATE cf, ct
   PRIVATE inbell
   inbell = SET("BELL")
   SET BELL ON
   IF (TYPE('xFreq[1]') = 'U')
      DECLARE afreq[1]
      DECLARE atime[1]
      afreq[1] = xfreq
      atime[1] = xtime
   ELSE
      DECLARE afreq[ALen(xFreq)]
      DECLARE atime[ALen(xTime)]
      FOR i=1 TO ALEN(xfreq)
         afreq[i] = xfreq[i]
         atime[i] = xtime[i]
      NEXT
   ENDIF
   FOR i=1 TO ALEN(afreq)
      cf = ALLTRIM(STR(afreq[i]))
      ct = ALLTRIM(STR(atime[i]))
      SET BELL TO &cf, &ct
      ?? CHR(7)
   NEXT
   SET BELL &inbell
   SET BELL TO 512, 2
   RETURN
PROCEDURE fwmanlin2
   PARAMETER numopc, extopc, c_clave
   PRIVATE c_numpan, pregunta, tr_act, NORMAL, c_deleted
   PRIVATE c_carga, c_antes, c_despu, f_indic, c_aux, n_aux
   PRIVATE f_norma, f_trans, c_campo_clave
   PRIVATE c_campo, n_trans, tecla, n_cta, w_mensaje, n_valor, c_picture
   PRIVATE f_abierto, n_when
   
   Private l_GrbStd                && No grabar con F9+F10.
   l_GrbStd = .T.                  && No grabar con F9+F10.
   
   EXTERNAL ARRAY a_havifun()
   c_numpan = numopc + extopc + "1"
   pregunta = "WL" + c_numpan
   f_abierto= SELECT()
   c_clave  = IIF(EMPTY(c_clave), .F., c_clave)
   DO fselec WITH "wr000000","wr000000","0"
   IF !SEEK(c_numpan)
      DECLARE a_texto(1)
      a_texto(1) = "No se encuentra el "+pregunta+" apropiado"
      = liberror("ERROR!", @a_texto, true)
      USE
      RETURN
   ENDIF
   w_mensaje = libespere(0)
   DECLARE _a_fun(10)
   FOR n_cta = 1 TO 10
      _a_fun(n_cta) = ""
   NEXT
   c_deleted = SYS(2001,"DELETE")
   SET DELETE ON
   tr_act  = pregunta
   c_carga = ALLTRIM(wrantespro)
   c_antes = ALLTRIM(wrprevio)
   c_despu = ALLTRIM(wrposter)
   f_norma = ALLTRIM(wrnormal)
   f_indic = ALLTRIM(wrindic1)
   DEFINE WINDOW (pregunta) FROM wrlinini,wrcolini TO wrlinfin,wrcolfin;
      TITLE ALLTRIM(wrtitulo) NOCLOSE SHADOW NOGROW FLOAT NOZOOM COLOR (colpan)
   USE
   IF EMPTY(f_norma)
      SELE (f_abierto)
      f_norma = f_abierto
   ELSE
      DO fselec WITH f_norma, f_indic,"0"
   ENDIF
   f_trans = fntemp(3)
   SELE (f_norma)
   c_campo_clave = ""
   IF !EMPTY(f_indic)
      c_campo_clave = KEY(VAL(SYS(21)))
   ENDIF
   IF TYPE("c_clave") = "L"
      COPY TO (f_trans)
      DO fselec WITH f_trans,"","1"
      NORMAL = SELECT()
      GOTO TOP
      IF EOF()
         APPEND BLANK
      ENDIF
   ELSE
      IF SEEK(c_clave)
         COPY TO (f_trans) WHILE &c_campo_clave = c_clave
         DO fselec WITH f_trans,"","1"
         NORMAL = SELECT()
      ELSE
         COPY STRU TO (f_trans)
         DO fselec WITH f_trans,"","1"
         NORMAL = SELECT()
         APPEND BLANK
         SELE (f_norma)
      ENDIF
   ENDIF
   DO fselec WITH tr_act,"","0"
   GOTO TOP
   DECLARE a_todocampo(5)
   a_todocampo(1) = ""
   a_todocampo(2) = ""
   a_todocampo(3) = ""
   a_todocampo(4) = ""
   a_todocampo(5) = ""
   n_valor = 1
   n_cta   = 1
   c_campo = ""
   DO WHILE !EOF()
      c_campo = c_campo + ","
      n_trans = preg
      n_when  = ALLTRIM(UPPER(condiciget))
      SELE (NORMAL)
      c_aux = IIF(EMPTY(n_trans),"CAMBRO"+strzero(n_valor,4),FIELD(n_trans))
      n_aux = IIF(EMPTY(n_trans), 0, FSIZE(FIELD(n_trans)))
      SELE (tr_act)
      n_aux   = IIF(n_aux > LEN(ALLTRIM(PICTURE)) , n_aux, LEN(ALLTRIM(PICTURE)))
      n_aux   = IIF(n_aux > LEN(ALLTRIM(cabecera)), n_aux, LEN(ALLTRIM(cabecera)))
      c_campo = c_campo+c_aux+IIF(EMPTY(calculado),"","="+ALLTRIM(calculado))
      c_campo = c_campo + IIF(EMPTY(n_aux)     ,"",":"+STR(n_aux,2))
      c_campo = c_campo + IIF(EMPTY(cabecera)  ,"",":H='"+ALLTRIM(cabecera)+"'")
      c_campo = c_campo + ":F"
      IF !EMPTY(n_when)
         IF LEFT(n_when,2) == "@R"
            c_campo = c_campo + ":R:W=fwlw("+STR(RECNO(),2)+")"
         ELSE
            c_campo = c_campo + ":W=fwlw("+STR(RECNO(),2)+")"
         ENDIF
      ELSE
         c_campo = c_campo + ":W=fwlw("+STR(RECNO(),2)+")"
      ENDIF
      IF !EMPTY(n_trans) .AND. (LEFT(PICTURE,1) = "0")
         c_campo = c_campo + ":V=vl(  "+STR(RECNO(),2)+")"
         c_campo = c_campo + IIF(EMPTY(validacion),"",".AND."+ALLTRIM(validacion))
      ELSE
         c_campo = c_campo + IIF(EMPTY(validacion),"",":V="+ALLTRIM(validacion))
      ENDIF
      IF LEN(c_campo)+LEN(PICTURE)+5 > 200
         a_todocampo(n_cta) = c_campo
         c_campo = ""
         n_cta   = n_cta + 1
      ENDIF
      c_picture = IIF(LEFT(PICTURE,1)="0",REPLI("9",LEN(PICTURE)),PICTURE)
      c_campo = c_campo + IIF(EMPTY(c_picture),"",":P='"+ALLTRIM(c_picture)+"'")
      IF LEN(c_campo) >= 200
         a_todocampo(n_cta) = c_campo
         c_campo = ""
         n_cta   = n_cta + 1
      ENDIF
      n_valor = n_valor + 1
      SKIP
   ENDDO
   a_todocampo(n_cta) = ALLTRIM(c_campo)
   a_todocampo(1)     = RIGHT(a_todocampo(1),LEN(a_todocampo(1))-1)
   SELE (NORMAL)
   IF !EMPTY(c_carga)
      DO &c_carga
   ENDIF
   = libespere(2, w_mensaje)
   RELEASE n_cta, c_campo, c_numpan, n_trans
   RELEASE w_mensaje, c_aux, n_valor, c_picture
   ACTIVATE WINDOW (pregunta)
   tecla = 0
   DO WHILE (tecla <> -8 .AND. tecla <> -9)
      SELE (NORMAL)
      BROWSE FIELDS &a_todocampo(1) &a_todocampo(2) &a_todocampo(3);
         &a_todocampo(4) &a_todocampo(5) NOCLEAR IN WINDOW (pregunta);
         COLOR (colwin)
      tecla = LASTKEY()
      DO CASE
      CASE tecla = -1
         APPEND BLANK
      CASE tecla = -2
         DELETE
      CASE tecla = -8
         IF !EMPTY(c_antes)
            IF RIGHT(c_antes,1) = ")"
               tecla = IIF(!&c_antes,   0, tecla)
            ELSE
               tecla = IIF(!&c_antes(), 0, tecla)
            ENDIF
         ENDIF
      CASE tecla = -9
      ENDCASE
   ENDDO

   DO fwl2desfun
*   IF (tecla = -8)
   IF (tecla = -8)  .And.  l_GrbStd = .T.      && No grabar con F9+F10.
      SELE (f_norma)
      IF TYPE("c_clave") = "L"
         SET DELETE OFF
         DELETE ALL
         SET DELETE ON
      ELSE
         IF SEEK(c_clave)
            SET DELETE OFF
            DELETE WHILE &c_campo_clave = c_clave
            SET DELETE ON
         ENDIF
      ENDIF
      DO fwl2actualizar
   ENDIF

   IF !EMPTY(c_despu)
      DO &c_despu
   ENDIF
   USE IN (tr_act)
   IF !EMPTY(f_norma)
      USE IN (NORMAL)
   ENDIF
   DELE FILE (f_trans)
   RELE WINDOW (pregunta)
   SET DELETE &c_deleted
   RETURN
FUNCTION vl
   PARAMETERS campo
   PRIVATE c_pict, c_valor
   SELE (tr_act)
   GOTO campo
   c_pict  = ALLTRIM(PICTURE)
   SELE (NORMAL)
   IF LEFT(c_pict,1) = "0"
      c_valor = VARREAD()
      REPLACE &c_valor WITH RIGHT(REPLICATE("0",LEN(c_pict))+ALLTRIM(&c_valor),LEN(c_pict))
   ENDIF
   RETURN .T.
FUNCTION fwlw
   PARAMETER campo
   PRIVATE w_ant, c_texto, c_funcion, c_mensaje, c_calculo, c_valor
   PRIVATE n_cta, c_numero, n_campo
   EXTERNAL ARRAY _a_fun()
   DECLARE a_txtfun(10)
   FOR n_cta = 1 TO 10
      a_txtfun(n_cta) = ""
   NEXT
   a_txtfun( 2) = "F2 Ins.L¡nea³"
   a_txtfun( 3) = "F3 Bor.L¡nea³"
   a_txtfun( 9) = "F9 Grabar³"
   a_txtfun(10) = "F10 Salir³"
   SELE (tr_act)
   GOTO campo
   n_campo   = preg
   c_funcion = ALLTRIM(condiciget)
   c_calculo = ALLTRIM(calculado)
   c_mensaje = ALLTRIM(mensaje)
   SET FUNCTION  2 TO ""
   SET FUNCTION  3 TO ""
   SET FUNCTION  9 TO ""
   SET FUNCTION 10 TO ""
   ON KEY LABEL  f2
   ON KEY LABEL  f3
   ON KEY LABEL  f9
   ON KEY LABEL f10
   _a_fun(2)  = ""
   _a_fun(3)  = ""
   _a_fun(9)  = ""
   _a_fun(10) = ""
   l_GrbStd = .T.               && No grabar con F9+F10.
   IF TYPE("a_havifun(1,1)") <> "U"
      FOR n_cta = 1 TO ALEN(a_havifun,1)
         n_indice  = a_havifun(n_cta,1)
         c_numero  = "F"+ALLTRIM(STR(n_indice,2))
         SET FUNCTION n_indice TO ""
         IF EMPTY(a_havifun(n_cta,2))
            ON KEY LABEL &c_numero DO void
            a_txtfun(n_indice) = ""
            If c_Numero = "F9"            && No grabar con F9+F10.
               l_GrbStd = .F.             && No grabar con F9+F10.
            EndIf                         && No grabar con F9+F10.
         ELSE
            a_txtfun(n_indice) = c_numero+" "+ALLTRIM(a_havifun(n_cta,2))+"³"
            _a_fun(n_indice)   = ALLTRIM(a_havifun(n_cta,3))
            ON KEY LABEL &c_numero DO _reparte
         ENDIF
      NEXT
   ENDIF
   a_txtfun(4) = IIF(EMPTY(textof4),"","F4 "+ALLTRIM(textof4)+"³")
   a_txtfun(5) = IIF(EMPTY(textof5),"","F5 "+ALLTRIM(textof5)+"³")
   a_txtfun(6) = IIF(EMPTY(textof6),"","F6 "+ALLTRIM(textof6)+"³")
   a_txtfun(7) = IIF(EMPTY(textof7),"","F7 "+ALLTRIM(textof7)+"³")
   a_txtfun(8) = IIF(EMPTY(textof8),"","F8 "+ALLTRIM(textof8)+"³")
   c_texto = ""
   FOR n_cta = 1 TO 10
      c_texto = c_texto + a_txtfun(n_cta)
   NEXT
   _a_fun(4) = ALLTRIM(funcif4)
   _a_fun(5) = ALLTRIM(funcif5)
   _a_fun(6) = ALLTRIM(funcif6)
   _a_fun(7) = ALLTRIM(funcif7)
   _a_fun(8) = ALLTRIM(funcif8)
   SELE (NORMAL)
   w_ant = WOUTPUT()
   ACTIVATE WINDOW wtefu SAME
   @ 0,0 SAY LEFT(c_texto+SPACE(80),80) COLOR (colpiv)
   ACTIVATE WINDOW wmsgdis SAME
   @ 0,0 SAY PADC(c_mensaje,80) COLOR (colmsg)
   IF !EMPTY(w_ant)
      ACTIVATE WINDOW (w_ant) SAME
   ENDIF
   FOR n_cta = 4 TO 8
      c_numero = "F"+ALLTRIM(STR(n_cta,2))
      IF EMPTY(_a_fun(n_cta))
         ON KEY LABEL &c_numero
      ELSE
         ON KEY LABEL &c_numero DO _reparte
      ENDIF
   NEXT
   SET FUNCTION  4  TO ""
   SET FUNCTION  5  TO ""
   SET FUNCTION  6  TO ""
   SET FUNCTION  7  TO ""
   SET FUNCTION  8  TO ""
   IF EMPTY(c_calculo)
      IF !EMPTY(c_funcion)
         IF LEFT(c_funcion,2) == "@R"
            c_funcion = RIGHT(c_funcion,LEN(c_funcion)-2)
            IF EMPTY(c_funcion)
               RETURN .T.
            ELSE
               RETURN &c_funcion
            ENDIF
         ELSE
            RETURN &c_funcion
         ENDIF
      ENDIF
   ELSE
      IF !EMPTY(n_campo)
         c_valor = FIELD(n_campo)
         REPLACE &c_valor WITH &c_calculo
         RETURN .F.
      ELSE
         RETURN .F.
      ENDIF
   ENDIF
   RETURN .T.
PROCEDURE _reparte
   PRIVATE n_tecla
   PUSH KEY CLEAR
   EXTERNAL ARRAY _a_fun()
   n_tecla = LASTKEY()
   IF (n_tecla <= -1) .AND. (n_tecla >= -9)
      n_tecla = ABS(n_tecla) + 1
      DO _a_fun(n_tecla)
   ENDIF
   POP KEY
   RETURN
PROCEDURE fwl2desfun
   PRIVATE w_ant
   w_ant = WOUTPUT()
   ACTIVATE WINDOW wtefu SAME
   CLEAR
   ON KEY LABEL  f2
   ON KEY LABEL  f3
   ON KEY LABEL  f4
   ON KEY LABEL  f5
   ON KEY LABEL  f6
   ON KEY LABEL  f7
   ON KEY LABEL  f8
   ON KEY LABEL  f9
   ON KEY LABEL f10
   SET FUNCTION  2 TO ""
   SET FUNCTION  3 TO ""
   SET FUNCTION  4 TO ""
   SET FUNCTION  5 TO ""
   SET FUNCTION  6 TO ""
   SET FUNCTION  7 TO ""
   SET FUNCTION  8 TO ""
   SET FUNCTION  9 TO ""
   SET FUNCTION 10 TO ""
   IF !EMPTY(w_ant)
      ACTIVATE WINDOW (w_ant) SAME
   ENDIF
   RETURN
PROCEDURE fwl2actualizar
   PRIVATE wndname, cta, cantidad
   SELE (NORMAL)
   GOTO TOP
   COUNT FOR !DELETED() TO cantidad
   IF EMPTY(cantidad)
      RETURN
   ENDIF
   DECLARE atxt[1]
   atxt[1] = 'Espere mientras se actualiza el archivo principal'
   wndname = libpctbox(0, nil, nil, @atxt, true)
   cta = 1
   GOTO TOP
   DO WHILE (.NOT. EOF())
      = libpctbox(1, wndname, cta, cantidad, 1)
      SCATTER TO abuffer
      SELECT (f_norma)
      APPEND FROM ARRAY abuffer
      SELECT (NORMAL)
      SKIP
      cta = cta + 1
   ENDDO
   = libpctbox(1, wndname, cantidad, cantidad, 1)
   = libpctbox(2, wndname)
   RETURN
FUNCTION fdescri
   PARAMETERS n_campo
   PRIVATE c_valor, f_fichero, f_indice, c_previo, c_campo, c_texto
   c_campo = VARREAD()
   c_valor = &c_campo
   SELE (tr_act)
   f_fichero = ALLTRIM(fichero)
   f_indice  = ALLTRIM(indice)
   c_previo  = ALLTRIM(previoclav)
   IF LEFT(c_previo,1) = "@"
      SELE (normal)
      c_previo = FIELD(VAL(RIGHT(c_previo,LEN(c_previo)-1)))
      c_previo = &c_previo
      SELE (tr_act)
   ENDIF
   DO fselec WITH f_fichero, f_indice, "0"
   IF (TYPE("n_campo") = "N") .AND. SEEK(c_previo+c_valor)
      c_texto = FIELD(n_campo)
      c_texto = ALLTRIM(&c_texto)
      IF !EMPTY(c_texto)
         WAIT WINDOW c_texto NOWAIT
      ENDIF
   ENDIF
   SELE (NORMAL)
   RETURN .T.
FUNCTION fvalida
   PARAMETER n_numero
   PRIVATE l_retorno, c_valor
   PRIVATE f_fichero, f_indice, c_previo, c_campo, n_vari
   l_retorno = .T.
   IF LASTKEY() > 1
      c_campo = VARREAD()
      c_valor = &c_campo
      IF !(EMPTY(c_valor) .AND. (TYPE("n_numero") <> "L"))
         SELE (tr_act)
         f_fichero = ALLTRIM(fichero)
         f_indice  = ALLTRIM(indice)
         IF LEFT(previoclav,1) == "@"
            n_vari = RIGHT(previoclav, LEN(previoclav)-1)
            SELE (NORMAL)
            c_previo = FIELD(VAL(n_vari))
            c_previo = &c_previo
         ELSE
            c_previo = ALLTRIM(previoclav)
            IF LEFT(previoclav,1) == "&"
               c_previo = SUBSTR(c_previo,2)
               c_previo = &c_previo
            ENDIF
         ENDIF
         DO fselec WITH f_fichero, f_indice, "0"
         IF !SEEK(c_previo+c_valor)
            = liberror(nil,"El c¢digo introducido no se encuentra", true)
            l_retorno = .F.
         ENDIF
      ENDIF
      SELE (NORMAL)
   ENDIF
   RETURN l_retorno
PROCEDURE browcon
   PRIVATE c_desc, c_regi, f_fich, f_indice, c_clave
   SELE (tr_act)
   c_desc   = ALLTRIM(descripcion)
   c_regi   = ALLTRIM(registros)
   f_fich   = ALLTRIM(fichero)
   f_indice = ALLTRIM(indice)
   c_clave  = ALLTRIM(previoclav)
   If Left(c_clave,1) == "&"
      c_clave = Right(c_clave, len(c_clave)-1)
      c_clave = &c_clave
   EndIf
   SELE (NORMAL)
   DO srt12gen2 WITH f_fich,f_indice,"CONSULTA","",c_clave,c_desc,c_regi
   SELE (NORMAL)
   RETURN
PROCEDURE void
   RETURN
PROCEDURE srt12gen2
   PARAMETERS f_fichero, f_indice, c_titulo, c_filtro, c_clave,;
      c_campos, c_recupe, n_ri, n_ci, n_rf, n_cf, u_seekin,;
      c_txt
   PRIVATE w_anterior, w_name, c_campobro, n_cta, n_longitud, n_lencampo
   PRIVATE n_campo1, n_campo2, c_campo, n_tecla, c_campo_clave, c_retorno
   PRIVATE f_ante, c_txtcampo
   CLEAR TYPEAHEAD
   w_anterior = WOUTPUT()
   f_ante = SELE()
   IF LEFT(c_clave,1) = "@"
      c_clave = RIGHT(c_clave, LEN(c_clave)-1)
      c_clave = FIELD(VAL(c_clave))
      c_clave = &c_clave
   ENDIF
   IF !EMPTY(f_fichero)
      IF USED(f_fichero)
         SELE (f_fichero)
      ELSE
         DO fselec WITH f_fichero, f_indice, "0"
      ENDIF
   ENDIF
   IF !EMPTY(f_indice)
      SET ORDER TO TAG &f_indice
   ENDIF
   c_clave  = IIF(!EMPTY(c_clave), "KEY '" + c_clave + "'","")
   c_filtro = IIF(!EMPTY(c_filtro),c_filtro,"")
   c_campobro = ""
   n_cta      = 1
   n_longitud = 0
   n_lencampo = 0
   DO WHILE !EMPTY(ALLTRIM(fwextrac(n_cta,c_campos,":")))
      c_campobro = c_campobro + ","
      c_texto    = fwextrac(n_cta,c_campos,":")
      n_posic    = AT("/", c_texto)
      n_campo    = VAL(LEFT(c_texto,n_posic-1))
      n_lencampo = FSIZE(FIELD(n_campo))
      c_txtcampo = RIGHT(c_texto,LEN(c_texto)-n_posic)
      n_lencampo = IIF(n_lencampo>LEN(c_txtcampo),n_lencampo,LEN(c_txtcampo))+1
      n_longitud = n_longitud + n_lencampo
      c_campobro = c_campobro + FIELD(n_campo) + ":H='"+c_txtcampo+"'"
      n_cta      = n_cta + 1
   ENDDO
   c_campobro = RIGHT(c_campobro,LEN(c_campobro)-1)
   IF EMPTY(c_campobro)
      USE
      RETURN
   ENDIF
   n_longitud = IIF(EMPTY(n_longitud),LEN(c_campos),n_longitud+2)
   n_longitud = (IIF(n_longitud > 78, 78, n_longitud) /2)
   n_ri = IIF(TYPE("n_ri") # "N",            5, n_ri)
   n_ci = IIF(TYPE("n_ci") # "N",40-n_longitud, n_ci)
   n_rf = IIF(TYPE("n_rf") # "N",           18, n_rf)
   n_cf = IIF(TYPE("n_cf") # "N",40+n_longitud, n_cf)
   w_name = "WIN" + ALLTRIM(STR(INT(RAND() * 100), 3))
   DO WHILE (WEXIST(w_name))
      w_name = "WIN" + ALLTRIM(STR(INT(RAND() * 100), 3))
   ENDDO
   DEFINE WINDOW (w_name) FROM n_ri, n_ci TO n_rf, n_cf TITLE c_titulo ;
      NOCLOSE FLOAT NOGROW SHADOW NOZOOM COLOR (colpan)
   SET FUNCTION  2 TO ""
   SET FUNCTION  3 TO ""
   SET FUNCTION  4 TO ""
   SET FUNCTION  5 TO ""
   SET FUNCTION  6 TO ""
   SET FUNCTION  7 TO ""
   SET FUNCTION  8 TO ""
   SET FUNCTION  9 TO ""
   SET FUNCTION 10 TO ""
   ON KEY LABEL f2  DO void
   ON KEY LABEL f3  DO void
   ON KEY LABEL f4  DO void
   ON KEY LABEL f5  DO void
   ON KEY LABEL f6  DO void
   ON KEY LABEL f7  DO void
   ON KEY LABEL f8
   ON KEY LABEL f9
   ON KEY LABEL f10 DO void
   ACTIVATE WINDOW wtefu SAME
   IF EMPTY(c_recupe)
      c_txt = IIF(TYPE("c_txt") <> "C", "F9 Salir³", c_txt)
      @ 0,0 SAY LEFT(c_txt+SPACE(80),80) COLOR (colpiv)
      SET FUNCTION 9 TO ""
   ELSE
      c_txt = IIF(TYPE("c_txt") <> "C","F8 Captura c¢digo³F9 Salir³", c_txt)
      @ 0,0 SAY LEFT(c_txt+SPACE(80),80) COLOR (colpiv)
      SET FUNCTION 8 TO ""
      SET FUNCTION 9 TO ""
   ENDIF
   ACTIVATE WINDOW wmsgdis SAME
   CLEAR
   IF !EMPTY(f_fichero)
      SELE (f_fichero)
      c_campo_clave = KEY(VAL(SYS(21)))
      IF EMPTY(u_seekin)
         GOTO TOP
      ELSE
         IF TYPE("u_seekin") = TYPE(c_campo_clave)
            SEEK u_seekin
         ELSE
            GOTO TOP
         ENDIF
      ENDIF
   ELSE
      GOTO TOP
   ENDIF
   ACTIVATE WINDOW (w_name) SAME
   BROWSE IN WINDOW (w_name) FIELDS &c_campobro &c_filtro &c_clave;
      NOMENU NOFOLLOW NOAPPEND NODELETE NOMODIFY COLOR (colwin)
   RELEASE WINDOW (w_name)
   n_tecla = LASTKEY()
   ACTIVATE WINDOW wtefu SAME
   CLEAR
   SET FUNCTION  2 TO ""
   SET FUNCTION  3 TO ""
   SET FUNCTION  4 TO ""
   SET FUNCTION  5 TO ""
   SET FUNCTION  6 TO ""
   SET FUNCTION  7 TO ""
   SET FUNCTION  8 TO ""
   SET FUNCTION  9 TO ""
   SET FUNCTION 10 TO ""
   ON KEY LABEL f2
   ON KEY LABEL f3
   ON KEY LABEL f4
   ON KEY LABEL f5
   ON KEY LABEL f6
   ON KEY LABEL f7
   ON KEY LABEL f8
   ON KEY LABEL f9
   ON KEY LABEL f10
   SCAT TO a_valor
   IF !EMPTY(f_fichero)
      USE
   ENDIF
   IF !EMPTY(f_ante)
      SELE (f_ante)
   ENDIF
   c_retorno = ""
   IF n_tecla = -7
      n_cta = 1
      IF EMPTY(AT("/",c_recupe))
         c_retorno = a_valor(VAL(c_recupe))
      ELSE
         DO WHILE !EMPTY(ALLTRIM(fwextrac(n_cta,c_recupe,":")))
            c_texto  = fwextrac(n_cta,c_recupe,":")
            n_posic  = AT("/", c_texto)
            n_campo1 = VAL(LEFT (c_texto,n_posic-1))
            n_campo2 = VAL(RIGHT(c_texto,LEN(c_texto)-n_posic))
            c_campo  = FIELD(n_campo2)
            IF !EMPTY(c_campo)
               IF TYPE("a_valor(n_campo1)") = TYPE(c_campo)
                  REPLACE &c_campo WITH a_valor(n_campo1)
               ENDIF
            ENDIF
            n_cta = n_cta + 1
         ENDDO
      ENDIF
   ENDIF
   IF !EMPTY(w_anterior)
      ACTIVATE WINDOW (w_anterior) SAME
   ENDIF
RETURN c_retorno



*> Calcular la fecha real (A¤o < 80 --> A¤o = A¤o + 1000).------------

Function Fec2000
Parameters w_Fecha
Private n_AnyFec, c_Fecha
   If Empty(DToS(w_Fecha))
      w_Fecha = CToD(Space(8))
   Else
      n_AnyFec = Year(w_Fecha)
      If n_AnyFec < 1980
         n_AnyFec = n_AnyFec + 100
         w_Fecha = CToD(Left(DToC(w_Fecha),6) + Str(n_AnyFec,4))
      EndIf
   EndIf
Return w_Fecha



*> Calcular fecha del siguiente laborable a partir de una fecha.------

Function SigLabor
Parameters d_FecLab
   d_FecLab = d_FecLab + 1
   Do While DoW(d_FecLab) = 1  .Or.  DoW(d_FecLab) = 7
      d_FecLab = d_FecLab + 1
   EndDo
Return d_FecLab


*> Calcular fecha laborable si es festivo.----------------------------

Function IrALabor
Parameters d_FecLab
   Do While DoW(d_FecLab) = 1  .Or.  DoW(d_FecLab) = 7
      d_FecLab = d_FecLab + 1
   EndDo
Return d_FecLab



*> Calcular fecha del siguiente laborable a partir de una fecha.------

Function SigLabor
Parameters d_FecLab
   d_FecLab = d_FecLab + 1
   Do While DoW(d_FecLab) = 1  .Or.  DoW(d_FecLab) = 7
      d_FecLab = d_FecLab + 1
   EndDo
Return d_FecLab


*********************************************
*      SELECCIONAR O ABRIR UN ARCHIVO       *
*      Especial Prosan 18/02/98             *
*********************************************

Procedure FSelect
Parameters FileSel,Indic,Modo
If Select(FileSel) <> 0
   Select (FileSel)
Else
   If Modo = "0"
      Use "Fich\"+(FileSel+_em) Alias (FileSel) In 0
   Else
      Sele 0
      Use "FIch\"+(FileSel+_em) Alias (FileSel) Exclusive 
   EndIf
EndIf
If !Empty(Indic)
   Set Order to Tag &Indic
EndIf
Return

*********************************************
*   Filtro Listado Mapa de Ubicaciones      *
*      Especial Prosan 19/02/98             *
*********************************************
Function Fil
SwFound= IIF( Betw(Substr(F10cCodigo,5,2),CalleD,CalleH).and. ;
              Betw(Substr(F10cCodigo,7,2),FilaD,FilaH).and. ;
              Betw(Substr(F10cCodigo,9,2),PisoD,PisoH).and. ;
              Betw(Substr(F10cCodigo,11,1),ProfD,ProfH),.T.,.F.)
Return(SwFound)

FUNCTION f_selec
PARAMETER f_fichero, f_indice, c_estado
PRIVATE l_open, c_auxiliar
*> Abre un fichero y retorna si estaba abierto.

   *> Inicializamos variables.----------------------------------------
   f_fichero = TRIM(f_fichero)
   f_indice  = TRIM(f_indice)
   l_open    = USED(f_fichero)
  
   *> Si el fichero se encuentra abierto.-----------------------------
   IF l_open

      *> Seleccionamos el area del fichero.---------------------------
      SELE (f_fichero)
      
      *> Activamos el indice.-----------------------------------------
      IF EMPTY(f_indice)
         SET ORDER TO 0
      ELSE
         SET ORDER TO TAG (f_indice)
      ENDIF
      
   ELSE
   
      *> Nueva area.--------------------------------------------------
      SELE 0
      
      *> Controlamos el indice.---------------------------------------
      c_auxiliar = IIF(EMPTY(f_indice),""," ORDER TAG "+f_indice)

      *> Controlamos el modo de apertura.-----------------------------
      c_auxiliar = c_auxiliar + IIF(c_estado = "1"," EXCLUSIVE","")
      
      *> Anyadimos el nombre del fichero.-----------------------------
      c_auxiliar = f_fichero + " " + c_auxiliar
      
      *> Abrimos el fichero con sus indices.--------------------------
      USE &c_auxiliar
      
   ENDIF

RETURN l_open
