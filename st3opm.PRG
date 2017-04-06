* Programa que Configura y Reconfigura Opciones de Menu
*     'ST3OPM'
*
* Estructura Memo MEN_OPC
*	Campo	1	(1,1)	Menú (M), Programa (P), otra opción (O)
*			2	(2,1)	Añadir Línea (/)
*			3	(3,10)	Programa
*			4	(14,30)	Literal
*
select SYSMEN
lx1=left(_em+space(3),3)
do case
case seek(lx1+_cse+'MENU      ')
  _xcodmen=lx1+_cse
case seek(lx1+' '+'MENU      ')
  _xcodmen=lx1+' '
case seek('   '+_cse+'MENU      ')
  _xcodmen='   '+_cse
otherwise
  _xcodmen='    '
endcase
seek _xcodmen+'MENU      '
if eof()
  =f3_sn(1, 1, "Usuario no asignado a grupo programas")
  _ok = .F.
  return
endif

set sysmenu to

lxalt='S'
menmen='MENU      '
nivel=1
primer_men=''
nopcpm=0
do while .not. empty(menmen)
  lx1men=menmen
  menmen=''
  do while .not. empty(lx1men)
    lxmen=left(lx1men,10)
    lx1men=iif(len(lx1men)=10,'',subs(lx1men,11))
    seek _xcodmen+lxmen 
    nopc=0
    alguna_opcion=.F.
    for ndopc=1 to memlines(MEN_OPC)
      lx1=mline(MEN_OPC,ndopc)
      menmenu=left(lx1,1)
      menlin=subs(lx1,2,1)
      menlit=f3_t(trim(subs(lx1,14)))
      menprg=subs(lx1,3,10)
      if nivel=1
        primer_men=iif(empty(primer_men),menprg,primer_men)
        lxx=upper(menlit)
        lx=''
        do while empty(lx) .and. .not.empty(lxx) .and. menprg<>'SY3PRG'
          nd=at(left(lxx,1),lxalt)
          if nd=0
            lx=left(lxx,1)
            lxalt=lxalt+lx
          else
            lxx=subs(lxx,2)
          endif
        enddo
        do case
        case left(menprg,3)='SY3'
          define pad &menprg of _MSYSMENU prompt menlit skip for !empty(_xopenfc) key ALT+S
        case empty(lx)
          define pad &menprg of _MSYSMENU prompt menlit skip for !empty(_xopenfc)
        otherwise
          lx3=menlit
          nd=at(lx,upper(lx3))
          lx3=left(lx3,nd-1)+'\<'+subs(lx3,nd)
          lx='ALT+'+lx
          define pad &menprg of _MSYSMENU prompt lx3 key &lx
        endcase
        define popup &menprg
        on pad &menprg of _MSYSMENU activate popup &menprg
        menmen=menmen+left(menprg+space(10),10)
      else
        alguna_opcion=.T.
        nopc=nopc+1
        define bar nopc of &lxmen ;
               message menlit ;
               prompt substr('123456789ABCDEFGHIJKLMNPQRTUVWXYZ',nopc,1)+'  '+menlit+'  '
        if menmenu='M'
          on bar nopc of &lxmen activate popup &menprg
          define popup &menprg
          menmen=menmen+menprg
        endif
        if menlin='/'
          nopc=nopc+1
          define bar nopc of &lxmen prompt '\-'
        endif
      endif
      nopcpm=iif(lxmen=primer_men,nopc,nopcpm)
    endfor
    if alguna_opcion
      on selection POPUP &lxmen do rt3opc
    endif
  enddo
  nivel=nivel+1
enddo
*
define bar nopcpm+1 of &primer_men prompt '\-'
define bar nopcpm+2 of &primer_men prompt 'X  '+f3_t('SALIR') message 'Abandonar el programa'
on selection bar nopcpm+2 of &primer_men do rt3quit
On Key Label Alt+F4 do rt3quit
select SYSPRG
*
return
