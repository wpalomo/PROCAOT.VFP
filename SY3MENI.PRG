* Programa de impresi�n de menus 
*        SY3MENI
*
select SYSMEN
dimension niveles(10,2)
dimension literal(10)
store 0 to niveles
store '' to literal
seek m.MEN_EMP+m.MEN_CSE+'MENU      '
if .not. eof()
  r1men=recno()
  lxl='Men� General'
  lxl=chrtran(lxl,'�',chr(160))
  lxl=chrtran(lxl,'�',chr(130))
  lxl=chrtran(lxl,'�',chr(161))
  lxl=chrtran(lxl,'�',chr(162))
  lxl=chrtran(lxl,'�',chr(163))
  lxl=chrtran(lxl,'�',chr(129))
  lxl=chrtran(lxl,'�',chr(133))
  lxl=chrtran(lxl,'�',chr(138))
  lxl=chrtran(lxl,'�',chr(141))
  lxl=chrtran(lxl,'�',chr(149))
  lxl=chrtran(lxl,'�',chr(151))
  lxl=chrtran(lxl,'�',chr(129))
  lxl=chrtran(lxl,'�',chr(165))
  lxl=chrtran(lxl,'�',chr(164))
  lxmen=lxl+cr
  for nd_1=1 to memlines(MEN_OPC)
    lx1=mline(MEN_OPC,nd_1)
    lxm_1=iif(nd_1=memlines(MEN_OPC),'     ���� ','     ���� ')
    lx_2=iif(nd_1=memlines(MEN_OPC),'          ','     �    ')
    lxl=subs(lx1,14)
    lxl=f3_t(lxl)
    lxl=chrtran(lxl,'�',chr(160))
    lxl=chrtran(lxl,'�',chr(130))
    lxl=chrtran(lxl,'�',chr(161))
    lxl=chrtran(lxl,'�',chr(162))
    lxl=chrtran(lxl,'�',chr(163))
    lxl=chrtran(lxl,'�',chr(129))
    lxl=chrtran(lxl,'�',chr(133))
    lxl=chrtran(lxl,'�',chr(138))
    lxl=chrtran(lxl,'�',chr(141))
    lxl=chrtran(lxl,'�',chr(149))
    lxl=chrtran(lxl,'�',chr(151))
    lxl=chrtran(lxl,'�',chr(129))
    lxl=chrtran(lxl,'�',chr(165))
    lxl=chrtran(lxl,'�',chr(164))
    lxmen=lxmen+'     �    '+cr+lxm_1+lxl+' ['+trim(subs(lx1,3,10))+']'+cr
    =f1_menu(subs(lx1,3,10))
    go r1men
  endfor
endif
go top
r1=recno()
report form SY3MENU for recno()=r1 preview
*
return


* ---------------------------
* Niveles de men�
* ---------------------------
Function f1_menu
Parameter menu
*
seek m.MEN_EMP+m.MEN_CSE+menu
if .not. eof()
  nivel=1
  niveles(1,1)=recno()
  niveles(1,2)=0
  do while nivel>=1
    go niveles(nivel,1)
    nd=niveles(nivel,2)+1
    inc=-1
    for nd_2=nd to memlines(MEN_OPC)
      lx1=mline(MEN_OPC,nd_2)
      lx2=iif(nd_2=memlines(MEN_OPC),'     ���� ','     ���� ')
      lx3=iif(nd_2=memlines(MEN_OPC),'          ','     �    ')
      lx=lx_2
      for ndx=1 to nivel
        lx=lx+literal(ndx)
      endfor
      lxl=subs(lx1,14)
      lxl=f3_t(lxl)
      lxl=chrtran(lxl,'�',chr(160))
      lxl=chrtran(lxl,'�',chr(130))
      lxl=chrtran(lxl,'�',chr(161))
      lxl=chrtran(lxl,'�',chr(162))
      lxl=chrtran(lxl,'�',chr(163))
      lxl=chrtran(lxl,'�',chr(129))
      lxl=chrtran(lxl,'�',chr(133))
      lxl=chrtran(lxl,'�',chr(138))
      lxl=chrtran(lxl,'�',chr(141))
      lxl=chrtran(lxl,'�',chr(149))
      lxl=chrtran(lxl,'�',chr(151))
      lxl=chrtran(lxl,'�',chr(129))
      lxl=chrtran(lxl,'�',chr(165))
      lxl=chrtran(lxl,'�',chr(164))
      lxmen=lxmen+lx+'     �    '+cr
      lxmen=lxmen+lx+lx2+lxl+' ['+trim(subs(lx1,3,10))+']'+cr
      if left(lx1,1)='M'
        seek m.MEN_EMP+m.MEN_CSE+subs(lx1,3,10)
        if eof()
          go niveles(nivel,1)
        else
          literal(nivel)=lx3
          niveles(nivel,2)=nd_2
          inc=1
          niveles(nivel+1,1)=recno()
          niveles(nivel+1,2)=0
          exit
        endif
      endif
    endfor
    nivel=nivel+inc
    if nivel>0
      literal(nivel)=''
    endif
  enddo
endif
*
return 

