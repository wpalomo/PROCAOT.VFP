* Programa para optimizar ficheros
*       SY3OPT
*
if !used('SYSFC')
  use SYSFC in 0 order tag idx1
endif
select SYSFC
nd=reccount()
if !used('SYSPRG')
  use SYSPRG in 0 order tag idx1
endif
select SYSPRG
nd=max(nd,reccount())
dimension _campos(nd,3)
_campos=''
_ficheros=''
do form sy3opt
*
return



* --------------------------------------
* Función de reconstrucción
* --------------------------------------
* *****************************************
* Reparar ficheros que no se puedan abrir
* *****************************************
Function rty3opt2
parameters nfc
*
PRIVATE lx,lx1
store 0 to _xier,ini
curpos=1
fin=2
urw=12 
fd=fopen(nfc+'.DBF',urw)
if fd=-1
  wait window "El fichero '"+nfc+"' no se ha podido abrir"
  return
endif
*
lx=fread(fd,1)				&&  N = Normal // M = Memo
tipo=iif(asc(lx)=3,'N','M')
if tipo='M'
  fm=fopen(nfc+'.FPT',urw)
  if fm=-1
    wait window "El fichero '"+nfc+".FPT' no se ha podido abrir"
    return
  endif
endif
@ 12,2 say 'Reconstruyendo fichero. Espere, por favor...'
*
fl=fseek(fd,0,fin)			&&  Cálculo tamaño fichero
ml=fseek(fm,0,fin)			&&  Cálculo tamaño fichero memo
*
=fseek(fd,10,ini)			&&  Encontrar tamaño registro
lx=fread(fd,2)
fs=val_right_to_left(lx)
*
curb=0						&&  Ver tamaño heading
desp=32
=fseek(fd,32,ini)
do while curb<>13 .and. .not.feof(fd)
  desp=desp+32
  lx=fread(fd,32)
  lx=fread(fd,1)
  =fseek(fd,-1,curpos)
  curb=asc(lx)
enddo
hs=desp+1
*
nr=(fl-1-hs)/fs
lx1=car_rigth_to_left(nr)
lx=''
for nd=len(lx1) to 1 step -1
  lx=lx+subs(lx1,nd,1)
endfor
lx=left(lx+chr(0)+chr(0)+chr(0)+chr(0),4)
=fseek(fd,4,ini)
=fwrite(fd,lx)
*
if tipo='M'
  =fseek(fm,6,ini)
  lx=fread(fm,2)
  tamblk=val_left_to_right(lx)
  nd=int(ml/tamblk)+1
  lx=car_right_to_left(nd)
  lx=right(chr(0)+chr(0)+chr(0)+chr(0)+lx,4)
  =fseek(fm,0,ini)
  =fwrite(fm,lx)
  =fclose(fm)
endif
=fclose(fd)
@ 12,2 say space(70)
_xier=0
*
return

* -----------------
function val_right_to_left
parameters v
* -----------------
PRIVATE nd0,nd1,nd,nd2
retval=0
for nd=1 to len(v)
  nd1=asc(subs(v,nd,1))
  nd0=mod(nd1,16)
  nd1=int(nd1/16)
  mult=iif(nd=1,1,16)
  for nd2=1 to nd-1
    mult=mult*16
  endfor
  retval=retval+nd0*mult
  mult=mult*16
  retval=retval+nd1*mult
endfor
return retval

* -----------------
function val_left_to_right
parameters v
* -----------------
PRIVATE nd0,nd1,nd,nd2
retval=0
lv=len(v)
for nd=lv to 1 step -1
  nd1=asc(subs(v,nd,1))
  nd0=mod(nd1,16)
  nd1=int(nd1/16)
  mult=iif(nd=lv,1,16*mult)
  retval=retval+nd0*mult
  mult=mult*16
  retval=retval+nd1*mult
endfor
return retval

* ---------------------
function car_rigth_to_left
parameters v
* ---------------------
PRIVATE nr,lx
nr=v
lx=''
do while nr>0
  nd1=mod(nr,256)
  nr=int(nr/256)
  lx=chr(nd1)+lx
enddo
return lx
