* Programa que comprueba una  pantalla
*           SY3PNT
*
*	_forma 	N	Pantalla Normal
*
Parameter _forma
*
dimension grids(50,2)
store 0 to tot_grid,n_grid
_xerrac=1
store '' to prg1er,lxcpro,lxdesc,prg1erl,prglvis,prglinv,lxdescl,_tipo_grid,prgmmet
lxdesc1=sysprg.PRG_DESC
store 999 to n_prg1er,n_prg1erl
select PNT
dimension campos(30)
define window comp from 9*1.2,12 to 14*1.2,68 font 'Courier',10 color RGB(0,0,0,192,192,192)
activate window comp
@   0,0    to   6,0    color RGB(255,255,255,0,0,0) && Blanco izq
@ 0.2,0.3  to 5.8,0.4  color RGB(127,127,127,0,0,0) && Gris   izq
@   0,0    to   0,56   color RGB(255,255,255,0,0,0) && Blanco sup
@ 0.2,0.3  to 0.2,56   color RGB(127,127,127,0,0,0) && Gris   sup
@   0,55.7 to 5.9,55.7 color RGB(127,127,127,0,0,0) && Gris   der
@ 0.2,55.6 to 5.8,55.6 color RGB(255,255,255,0,0,0) && Gris   izq
@ 5.7,0.4  to 5.7,56   color RGB(255,255,255,0,0,0) && Blanco inf
@ 5.8,0    to 5.9,56   color RGB(127,127,127,0,0,0) && Gris   inf
lini=2.9
linf=3
coli=3.5
colf=52.1
@ lini-.6,coli-.8 to lini-.6,colf+.8 color RGB(127,127,127,0,0,0) && Gris   sup
@ lini-.6,coli-.8 to linf+.6,coli-.8 color RGB(127,127,127,0,0,0) && Gris   izq.
@ linf+.6,coli-.8 to linf+.6,colf+.8 color RGB(255,255,255,0,0,0) && Blanco sup
@ lini-.6,colf+.8 to linf+.6,colf+.8 color RGB(255,255,255,0,0,0) && Blanco izq.
progok=.F.
_grid=.F.
totc=reccount()
go top
for r1pnt=1 to totc
  despl=0
  select PNT
  go r1pnt
  nd=min(recno(),totc)
  @ 2.4,2.9 to 3.6,(nd*50/totc)+2.9 color RGB(127,127,127,127,127,127) pattern 1 
  lx_parent=PARENT
  lx_class=CLASS
* ----------------------------- Control métodos -------------------------
  lx2=''
  for _nummet=1 to memlines(METHODS)
    lx1=mline(METHODS,_nummet)
    nd=at('&'+'&'+'*',lx1)
    if nd>0
      prgmmet=prgmmet+lx2+' > '+subs(lx1,nd+3)+cr
      lx2=''
    endif
    nd=at('PROCEDURE',lx1)
    if nd>0
      lx2=trim(PARENT)+iif(empty(PARENT),'','.')+trim(OBJNAME)+'.'+trim(subs(lx1,nd+10))+cr
    endif
  endfor
* ----------------------------- Nombre de la Pantalla -------------------
  if BASECLASS='form'
    lx_prog=trim(sysprg.PRG_PROG)
    lx_prop=PROPERTIES
    if OBJNAME<>lx_prog
      if left(lx_prop,4)='Name'
        =y3_prop('Name = "'+lx_prog+'"')
      else
        =y3_prop(chr(10)+'Name = "'+lx_prog+'"')
      endif
      replace PROPERTIES with lx_prop, ;
              OBJNAME with lx_prog
    endif

    if lx_class='f_pant2'
      nd=atc('Pageframe1.Page1.Grid1.Columncount =',lx_prop)
      if nd>0
        totcol=val(subs(lx_prop,nd+37,2))
        for num=2 to totcol
          lxcol='Pageframe1.Page1.Grid1.Column'+ltrim(str(num))+'.Fontname'
          if atc(lxcol,lx_prop)=0
            nd=atc('Pageframe1.Page1.Grid1.Column'+ltrim(str(num))+'.Name',lx_prop)
            if nd>0
              lx1='Pageframe1.Page1.Grid1.Column'+ltrim(str(num))+'.FontBold = .F.'
              lx1=iif(atc(lx1,lx_prop)=0,lx1+cr,'')
              lx2='Pageframe1.Page1.Grid1.Column'+ltrim(str(num))+'.FontName = "Courier New"'
              lx2=iif(atc(lx2,lx_prop)=0,lx2+cr,'')
              lx3='Pageframe1.Page1.Grid1.Column'+ltrim(str(num))+'.Fontsize = 8'
              lx3=iif(atc(lx3,lx_prop)=0,lx3+cr,'')
              lx_prop=left(lx_prop,nd-1)+lx1+lx2+lx3+subs(lx_prop,nd)
            endif
          endif          
        endfor          
        replace PROPERTIES with lx_prop
      endif
    endif
  endif
* ------------ GRID / Clase Pantalla con Grid con fichero ----------  (en Courier New 8)
  do case
  case BASECLASS='grid' .and. lx_class='st_grid'
    store '' to grids
    _tipo_grid=lx_class
    store 0 to tot_grid,n_grid,tot
    _alig=OBJNAME
    _grid=.F.

    for nd=1 to memlines(sysprg.PRG_GRID)
      lx=mline(sysprg.PRG_GRID,nd)
      do case
      case atc('['+_alig+']',lx)>0
        _grid=.T.
      case at('[',lx)>0
        _grid=.F.
      case .not. empty(lx) .and. _grid
        tot=tot+1
        nd1=at(',',lx)
        g_nvar=left(lx,nd1-1)
        lx=subs(lx,nd1+1)
        nd1=at(',',lx)
        grids(tot,2)=left(lx,nd1-1)
        lx=subs(lx,nd1+1)
        g_tipo=left(lx,1)
        lx=subs(lx,2)
        nd1=at(',',lx)
        nd2=at('.',lx)
        if nd1=0
          lx1=lx
          lx=''
        else
          lx1=left(lx,nd1-1)
          lx=subs(lx,nd1+1)
        endif
        do case
        case g_tipo='N' .and. nd2>0
          lng=val(left(lx1,nd2-1))
          lng=lng*7+7
        case g_tipo='D'
          lng=77
        case g_tipo='M'
          lng=32
        otherwise
          lng=val(lx1)
          lng=lng*7+7
        endcase
        nd1=iif(!empty(lx),val(lx)*7+7,lng)
        grids(tot,1)=nd1
      endcase
    endfor

    n_grid=tot
    lx_prop=PROPERTIES
    nd=atc('ColumnCount =',lx_prop)
    if nd>0
      numtot=val(subs(lx_prop,nd+14,4))
      nd1=nd-1
      lxinit=''
      do while asc(subs(lx_prop,nd1,1))>13
        lxinit=subs(lx_prop,nd1,1)+lxinit
        nd1=nd1-1
      enddo

      for num=1 to numtot
        lxcol=lxinit+'Column'+ltrim(str(num))
        despl=at(lxcol,lx_prop)
        lx=lxcol+'.FontBold = .F.'
        =y3_prop(lx)

        lx=lxcol+'.FontName = "Courier New"'
        =y3_prop(lx)

        lx=lxcol+'.Fontsize = 8'
        =y3_prop(lx)

        _readapta=.T.
        nd=at(lxcol+'.Width =',lx_prop)
        if nd>0
          nd=val(subs(lx_prop,nd+len(lxcol)+8,5))
          _readapta=iif(nd>15,.F.,.T.)
        endif
        if _readapta .and. grids(num,1)>0
          lx1=ltrim(str(grids(num,1)))
          =y3_prop(lxcol+'.Width = '+lx1)
        endif
      endfor

    endif
    select PNT
    repl PROPERTIES with lx_prop
* ----------------------------- GRID / Clase Pantalla con Grid ----------  (en Courier New 8)
  case BASECLASS='grid'
    store 0 to tot_grid,n_grid
    lx_prop=PROPERTIES
    nd=atc('ColumnCount =',lx_prop)
    if nd>0
      numtot=val(subs(lx_prop,nd+14,4))
      nd1=nd-1
      lxinit=''
      do while asc(subs(lx_prop,nd1,1))>13
        lxinit=subs(lx_prop,nd1,1)+lxinit
        nd1=nd1-1
      enddo

      for num=1 to numtot
        lxcol=lxinit+'Column'+ltrim(str(num))
        despl=at(lxcol,lx_prop)
        lx=lxcol+'.FontBold = .F.'
        =y3_prop(lx)
        lx=lxcol+'.FontName = "Courier New"'
        =y3_prop(lx)
        lx=lxcol+'.Fontsize = 8'
        =y3_prop(lx)
        _readapta=.T.
        nd=at(lxcol+'.Width =',lx_prop)
        if nd>0
          nd=val(subs(lx_prop,nd+len(lxcol)+8,5))
          _readapta=iif(nd>15,.F.,.T.)
        endif
        if _readapta
          nd=at(lxcol+'.ControlSource =',lx_prop)
          if nd>0
            select SYSVAR
            lx1=subs(lx_prop,nd+len(lxcol)+18)
            nd1=at('.',lx1)
            nd2=at('"',lx1)
            nd1=iif(nd1>nd2,0,nd1)
            if nd2>0
              nvar=subs(lx1,nd1+1,nd2-nd1-1)
              seek upper(nvar)
              if !eof()
                do case
                case left(VAR_FMT,1)='9' .or. left(VAR_FMT,1)='X' .or. left(VAR_FMT,1)='!'
                  lx1=trim(VAR_FMT)
                  lx1=ltrim(str(len(lx1)*7+7))
                  =y3_prop(lxcol+'.Width = '+lx1)
                case left(VAR_FMT,1)='r'
                  lx1=VAR_FMT
                  lx1=&lx1
                  lx1=ltrim(str(len(lx1)*7+7))
                  =y3_prop(lxcol+'.Width = '+lx1)
                case left(VAR_FMT,1)='D'
                  =y3_prop(lxcol+'.Width = 77')
                endcase
              else
                nd=at(lxcol+'.InputMask = "',lx_prop)
                if nd>0
                  lx1=subs(lx_prop,nd+13)
                  nd=at('"',lx1)
                  if nd>0
                    lx1=left(lx1,nd-1)
                   lx1=ltrim(str(len(lx1)*7+7))
                   =y3_prop(lxcol+'.Width = '+lx1)
                  endif
                endif
              endif
            endif
          endif
        endif
      endfor

    endif
    select PNT
    repl PROPERTIES with lx_prop
* ----------------------------- Cabecera de un GRID --------------------
  case BASECLASS='header'
    lx_prop=PROPERTIES
    =y3_prop('FontName = "MS Sans Serif"')
    =y3_prop('Fontsize = 8')
    =y3_prop('BackColor = 0,128,128')
    =y3_prop('ForeColor = 255,255,255')
 
    if n_grid>0
      tot_grid=tot_grid+1
      if _tipo_grid='st_grid'
        =y3_prop('Caption = "'+grids(tot_grid,2)+'"')
      else
        =y3_prop('Caption = "'+grids(tot_grid+2,2)+'"')
      endif
    endif
    select PNT
    replace PROPERTIES with lx_prop
    _grid=.T.
* ----------------------------- Campo de un GRID ----------------------
  case BASECLASS='textbox' .and. _grid
    _grid=.F.
    lx_prop=PROPERTIES
    =y3_prop('FontBold = .F.')
    =y3_prop('FontName = "Courier New"')
    =y3_prop('Fontsize = 8')
    select PNT
    repl PROPERTIES with lx_prop
  case _grid
    _grid=.F.
* ----------------------------- Lineas ---------------------------------
  case lx_class='line'
    _grid=.F.
    lx_prop=PROPERTIES
    nd_bc=at('BorderColor = 255,0,255',lx_prop)
    nd_top=at('Top = ',lx_prop)
    if (at('BorderWidth',lx_prop)=0.or.at('BorderWidth = 1',lx_prop)>0);
       .and. nd_bc>0;
       .and. (at('Height = 0',lx_prop)>0.or.at('Width = 0',lx_prop)>0)
       =y3_plin()
    endif
* --------------------------- Campos ---------------------------------
  case lx_class='st_get' .or. ;
       lx_class='propcaot' .or. ;
       lx_class='st_geth' .or. ;
       lx_class='st_geti' .or. ;
       lx_class='st_getip' .or. ;
       lx_class='st_say3d'
    _grid=.F.
    lx_prop=PROPERTIES
    nd=at('ControlSource = "',lx_prop)
    select SYSVAR
    if nd>0
      lx1=subs(lx_prop,nd+17)
      nd1=at('.',lx1)
      nd2=at('"',lx1)
      nd1=iif(nd1>nd2,0,nd1)
      if nd2>0
        nvar=subs(lx1,nd1+1,nd2-nd1-1)
        lx_prop=stuff(lx_prop,nd+17,nd2-1,'m.'+nvar)
        if left(lx_prop,4)='Name'
          =y3_prop('Name = "'+nvar+'"')
        else
          =y3_prop(chr(10)+'Name = "'+nvar+'"')
        endif
        if left(lx_prop,1)=chr(10)
          lx_prop=subs(lx_prop,2)
        endif
        seek upper(nvar)
        if !eof()
          do case
          case left(VAR_FMT,1)='9'
            lx1=trim(VAR_FMT)
            =y3_prop('InputMask = "'+lx1+'"')
            lx1=ltrim(str(len(lx1)*8+8))
            =y3_prop('Width = '+lx1)
            lx1=trim(VAR_FMT)
            nd1=at(',',lx1)
            do case
            case nd1>0
              do while nd1>0
                lx1=stuff(lx1,nd1,1,'')+'_'
                nd1=at(',',lx1)
              enddo
              =y3_prop('Formato = '+lx1)
            case atc('Formato = ',lx_prop)>0
              nd1=atc('Formato = ',lx_prop)
              lx2=subs(lx_prop,nd1)
              nd2=at(cr,lx2)
              if nd2>0
                lx_prop=stuff(lx_prop,nd1,nd2+1,'')
              else
                lx_prop=left(lx_prop,nd1-1)
              endif
            endcase
          case left(VAR_FMT,1)='r'
            lx1=VAR_FMT
            lx1=&lx1
            =y3_prop('InputMask = "'+lx1+'"')
            lx1=ltrim(str(len(lx1)*8+8))
            =y3_prop('Width = '+lx1)
          case left(VAR_FMT,1)='D'
            =y3_prop('InputMask = "99/99/9999"')
            =y3_prop('Width = 88')
          endcase
        else
          nd=at('InputMask = "',lx_prop)
          if nd>0
            lx1=subs(lx_prop,nd+13)
            nd=at('"',lx1)
            if nd>0
              lx1=left(lx1,nd-1)
              lx1=ltrim(str(len(lx1)*8+8))
              =y3_prop('Width = '+lx1)
            endif
          endif
        endif
        do case
        case lx_class='st_geti' .and. atc('Pageframe1',lx_parent)=0
          lxdesc=lxdesc+'thisform.'+trim(nvar)+cr
        case (lx_class='propcaot' .or. lx_class='st_getip') .and. atc('Pageframe1',lx_parent)=0
          lxcpro=lxcpro+'thisform.'+trim(nvar)+cr
        case lx_class='st_geti' .and. sysprg.PRG_TIPO='C' .and. atc('Pageframe1.Page1',lx_parent)>0
          lxdescl=lxdescl+'thisform.Pageframe1.Page1.'+trim(nvar)+cr
        case atc('Enabled = .F.',lx_prop)>0 .and. atc('thisform.'+trim(nvar),lxdesc1)>0
          lxdesc=lxdesc+'thisform.'+trim(nvar)+cr
        endcase
        nd=atc('TabIndex =',lx_prop)
        if nd>0 .and. lx_class<>'st_say3d'
          nd=val(subs(lx_prop,nd+10,4))
          do case
          case nd<n_prg1er .and. atc('Pageframe1',lx_parent)=0 .and. atc('St_frame1',lx_parent)=0
            n_prg1er=nd
            prg1er=trim(lx_parent)+'.'+trim(nvar)
            nd=at('.',prg1er)
            prg1er=stuff(prg1er,1,nd,'thisform.')
          case nd<n_prg1erl .and. atc('Pageframe1.Page1',lx_parent)>0
            n_prg1erl=nd
            prg1erl=trim(lx_parent)+'.'+trim(nvar)
            nd=at('.',prg1erl)
            prg1erl=stuff(prg1erl,1,nd,'thisform.')
          endcase
        endif
      endif
    endif
    select PNT
    repl PROPERTIES with lx_prop
* --------------------------- Chek Box ---------------------------------
  case lx_class='st_chek'
    _grid=.F.
    lx_prop=PROPERTIES
    nd=at('ControlSource = "',lx_prop)
    select SYSVAR
    if nd>0
      lx1=subs(lx_prop,nd+17)
      nd1=at('.',lx1)
      nd2=at('"',lx1)
      nd1=iif(nd1>nd2,0,nd1)
      if nd2>0
        nvar=subs(lx1,nd1+1,nd2-nd1-1)
        lx_prop=stuff(lx_prop,nd+17,nd2-1,'m.'+nvar)
        if left(lx_prop,4)='Name'
          =y3_prop('Name = "'+nvar+'"')
        else
          =y3_prop(chr(10)+'Name = "'+nvar+'"')
        endif
        if left(lx_prop,1)=chr(10)
          lx_prop=subs(lx_prop,2)
        endif
      endif
    endif
    select PNT
    repl PROPERTIES with lx_prop
* --------------------------- Lista Combo ---------------------------------
  case lx_class='st_combo'
    _grid=.F.
    lx_prop=PROPERTIES
    nd=at('ControlSource = "',lx_prop)
    select SYSVAR
    if nd>0
      lx1=subs(lx_prop,nd+17)
      nd1=at('.',lx1)
      nd2=at('"',lx1)
      nd1=iif(nd1>nd2,0,nd1)
      if nd2>0
        nvar=subs(lx1,nd1+1,nd2-nd1-1)
        lx_prop=stuff(lx_prop,nd+17,nd2-1,'m.'+nvar)
        if left(lx_prop,4)='Name'
          =y3_prop('Name = "'+nvar+'"')
        else
          =y3_prop(chr(10)+'Name = "'+nvar+'"')
        endif
        if left(lx_prop,1)=chr(10)
          lx_prop=subs(lx_prop,2)
        endif
      endif
    endif
    select PNT
    repl PROPERTIES with lx_prop
* --------------------------- Ultimo registro -------------------------
  case UNIQUEID='FONTINFO' .and. r1pnt<>reccount()
    _grid=.F.
    for nd=1 to fcount()
      lx=field(nd)
      campos(nd)=&lx
    endfor
    delete record recno()
    append blank
    for nd=1 to fcount()
      lx=field(nd)
      repl &lx with campos(nd)
    endfor
  endcase
* ----------------------------- Campos página 1 de cab/Líneas -----------
  if sysprg.PRG_TIPO='C' .and. atc('Pageframe1.Page1',lx_parent)>0
    lx1=trim(lx_parent)+'.'+trim(OBJNAME)
    nd=at('.',lx1)
    lx1=stuff(lx1,1,nd,'thisform.')
    if atc('column',lx1)=0
      prglvis=prglvis+lx1+'.visible=.T.'+cr
      prglinv=prglinv+lx1+'.visible=.F.'+cr
    endif
  endif
  select PNT
endfor
pack
use
select SYSPRG
progok=.T.
if empty(prg_1er) .and. (PRG_TIPO='F'.or.PRG_TIPO='C')
  do form st3sn with 1,1,'Falta ordenar la pantalla o poner algún input'
  progok=.F.
endif
prg1er=iif(empty(prg1er).or.PRG_1EROK=1,PRG_1ER,prg1er)
prg1erl=iif(empty(prg1erl).or.PRG_1ERLOK=1,PRG_1ERL,prg1erl)
repl PRG_OK with progok,PRG_1ER with prg1er, ;
     PRG_CPRO with lxcpro,PRG_DESC with lxdesc,PRG_1ERL with prg1erl,;
     PRG_LVIS with prglvis,PRG_LINV with prglinv,PRG_DESCL with lxdescl,PRG_MMET with prgmmet
* ---------------------------- Crear ficheros asociados a Grids
if !empty(sysprg.PRG_GRID)
  _nom_grid=''
  for nd=1 to memlines(sysprg.PRG_GRID)
    lx=mline(sysprg.PRG_GRID,nd)
    if nd=1 .and. at('[',lx)=0
      _nom_grid='G'+upper(trim(sysprg.PRG_PROG))
      use st_grid in 0
      select st_grid
      systmp=_tmp+sys(2015)
      copy stru extended to (systmp)
      select st_grid
      use (systmp) alias 'SYSTMP' excl
      select SYSTMP
    endif
    do case
    case at('[',lx)>0
      if !empty(_nom_grid)
        systmp1='FCTMP\'+_nom_grid
        create (systmp1) from (systmp)
        delete file (systmp+'.DBF')
        use
      endif
      nd1=at('[',lx)
      nd2=at(']',lx)
      _nom_grid=subs(lx,nd1+1,nd2-nd1-1)
      use st_grid in 0
      select st_grid
      systmp=_tmp+sys(2015)
      copy stru extended to (systmp)
      select st_grid
      use (systmp) alias 'SYSTMP' excl
      select SYSTMP
      zap
    case empty(_nom_grid) .or. at('#',lx)>0
    
    case .not. empty(lx)
      nd1=at(',',lx)
      m.FIELD_NAME=left(lx,nd1-1)
      lx=subs(lx,nd1+1)
      nd1=at(',',lx)
      lx=subs(lx,nd1+1)
      m.FIELD_TYPE=left(lx,1)
      lx=subs(lx,2)
      nd1=at(',',lx)
      nd2=at('.',lx)
      if nd1=0
        lx1=lx
        lx=''
      else
        lx1=left(lx,nd1-1)
        lx=subs(lx,nd1+1)
      endif
      m.FIELD_DEC=0
      do case
      case m.FIELD_TYPE='N' .and. nd2>0
        m.FIELD_LEN=val(left(lx1,nd2-1))
        m.FIELD_DEC=val(subs(lx1,nd2+1))
      case m.FIELD_TYPE='D'
        m.FIELD_LEN=8
      case m.FIELD_TYPE='M'
        m.FIELD_LEN=4
      otherwise
        m.FIELD_LEN=val(lx1)
      endcase
      store .T. to m.FIELD_NULL
      store .F. to m.FIELD_NOCP
      append blank
      gather memvar
    endcase
  endfor
  if !empty(_nom_grid)
    systmp1='FCTMP\'+_nom_grid
    create (systmp1) from (systmp)
    delete file (systmp+'.DBF')
    use
  endif
endif
select SYSPRG
deactivate window comp
release window comp
_xerrac=1
*
return

* ---------------------------------
* Doblar una línea
* ---------------------------------
Function y3_plin()
*
for nd=1 to fcount()
  lx=field(nd)
  campos(nd)=&lx
endfor
lx_objo=OBJNAME
lx_obj=lx_objo+'2'
nd=at('BorderColor = 255,0,255',lx_prop)     && ------------- Color
lx1=stuff(lx_prop,nd,23,'BorderColor = 0,0,0')
repl PROPERTIES with lx1
lx_prop=stuff(lx_prop,nd,23,'BorderColor = 255,255,255')
if at('Height = 0',lx_prop)>0          && ------------- Posición Horizontal
  nd=at('Top = ',lx_prop)
  lx1=subs(lx_prop,nd+6)
  nd1=at(chr(13),lx1)
  if nd1>1
    nd2=val(subs(lx_prop,nd+6,nd1-1))
    lx1=ltrim(str(nd2+1))
    lx_prop=stuff(lx_prop,nd+6,nd1-1,lx1)
  endif
else                                   && ------------- Posición Vertical
  nd=at('Left = ',lx_prop)
  lx1=subs(lx_prop,nd+7)
  nd1=at(chr(13),lx1)
  if nd1>1
    nd2=val(subs(lx_prop,nd+7,nd1-1))
    lx1=ltrim(str(nd2+1))
    lx_prop=stuff(lx_prop,nd+7,nd1-1,lx1)
  endif
endif
nd=at(lx_objo,lx_prop)                    && ------------ Nombre linea
if nd>0
  lx_prop=stuff(lx_prop,nd,len(lx_objo),lx_obj)
endif
append blank
for nd=1 to fcount()
  lx=field(nd)
  repl &lx with campos(nd)
endfor
repl OBJNAME with lx_obj,PROPERTIES with lx_prop
go r1pnt
*
return .T.

* -----------------------------------------
* Readaptar literal Properties
* -----------------------------------------
Function y3_prop
Parameter lx1
*
nd1=at('=',lx1)
if nd1>0
  lx2=left(lx1,nd1)
  nd1=at(lx1,lx_prop)
  nd2=at(lx2,lx_prop)
  if nd1=0
    do case
    case nd2=0 .and. despl=0
      lx_prop=lx1+cr+lx_prop
    case nd2=0
      lx_prop=stuff(lx_prop,despl,0,lx1+cr)
    otherwise
      lx=subs(lx_prop,nd2)
      nd3=at(chr(13),lx)
      lx_prop=stuff(lx_prop,nd2,nd3-1,lx1)
    endcase
  endif
endif
*
return
