*>
*> Funciones asociadas a listado de mapa/tamaños.
Parameters Pantalla,Accion,Programa
Private C_Param
Local lStado

If Pantalla = "INICIO"

   C_Param="(" + _GCSS("F10ccodubi", 1, 4) + " = " + _cm + _Alma + _cm + ") And " + ;
           "(" + _GCSS("F10ccodubi", 5, 2) + " between " + _cm + zonaD +_cm + " And " + _cm + zonaH + _cm + ") And " + ;
           "(" + _GCSS("F10ccodubi", 7, 2) + " between " + _cm + calleD+_cm + " And " + _cm + calleH+ _cm + ") And " + ;
           "(" + _GCSS("F10ccodubi", 9, 3) + " between " + _cm + filaD +_cm + " And " + _cm + filaH + _cm + ") And " + ;
           "(" + _GCSS("F10ccodubi",12, 2) + " between " + _cm + pisoD +_cm + " And " + _cm + pisoH + _cm + ") And " + ;
           "(" + _GCSS("F10ccodubi",14, 1) + " between " + _cm + profD +_cm + " And " + _cm + profH + _cm + ")"

   lStado = f3_sql('*', 'F10c', C_Param, , , 'F10cCur')
  
   Select F10cCur
   Go Top
   Do While !eof()
      Select XLLISTAM

      *> Almacén+Zona+Calle+Piso+Prof
      Seek SubStr(F10cCur.F10cCodUbi, 1, 8) + SubStr(F10cCur.F10cCodUbi,12, 3)
      
      If Found()
         Do Case
           *> Si es una de las 50 primeras filas actualizo Filas, Textos.
           Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),1,50)
             a=(val(SubStr(F10cCur.F10cCodUbi,9,3))*3)-2
             b=(val(SubStr(F10cCur.F10cCodUbi,9,3))*4)-3

             Replace Filas With Substr(Filas ,1,a-1) + SubStr(F10cCur.F10cCodUbi,9,3) + Substr(Filas,a+3)
             Replace Texto With Substr(Texto ,1,b-1) + F10cCur.F10cTamUbi + Substr(Texto,b+4)        
           *> Si fila entre 51 y 100, actualizo Filas2 y textos2. 
           Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),51,100)
             a=((val(SubStr(F10cCur.F10cCodUbi,9,3))-50)*3)-2
             b=((val(SubStr(F10cCur.F10cCodUbi,9,3))-50)*4)-3
           
             Replace Filas2 With Substr(Filas2 ,1,a-1) + SubStr(F10cCur.F10cCodUbi,9,3) + Substr(Filas2,a+3)
             Replace Texto2 With Substr(Texto2 ,1,b-1) + F10cCur.F10cTamUbi + Substr(Texto2,b+4)
           *>Si fila  entre 100 y 150, actualizo Filas3 y textos3 
           Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),101,150)
             a=((val(SubStr(F10cCur.F10cCodUbi,9,3))-100)*3)-2
             b=((val(SubStr(F10cCur.F10cCodUbi,9,3))-100)*4)-3
           
             Replace Filas3 With Substr(Filas3 ,1,a-1) + SubStr(F10cCur.F10cCodUbi,9,3) + Substr(Filas3,a+3)
             Replace Texto3 With Substr(Texto3 ,1,b-1) + F10cCur.F10cTamUbi + Substr(Texto3,b+4)
         EndCase

      Else
      
        Append Blank
        
        Replace Almacen With SubStr(F10cCur.F10cCodUbi, 1, 4)
        Replace Zona    With SubStr(F10cCur.F10cCodUbi, 5, 2)
        Replace Calle   With SubStr(F10cCur.F10cCodUbi, 7, 2)
        Replace Piso    With SubStr(F10cCur.F10cCodUbi,12, 2)
        Replace Prof    With SubStr(F10cCur.F10cCodUbi,14, 1)

        Do Case
           *> Si es una de las 50 primeras filas actualizo Filas, Textos.
           Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),1,50)
             a=(val(SubStr(F10cCur.F10cCodUbi,9,3))*3)-2
             b=(val(SubStr(F10cCur.F10cCodUbi,9,3))*4)-3

             Replace Filas With Substr(Filas ,1,a-1) + SubStr(F10cCur.F10cCodUbi,9,3) + Substr(Filas,a+3)
             Replace Texto With Substr(Texto ,1,b-1) + F10cCur.F10cTamUbi + Substr(Texto,b+4)        
           *> Si fila entre 51 y 100, actualizo Filas2 y textos2. 
           Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),51,100)
             a=((val(SubStr(F10cCur.F10cCodUbi,9,3))-50)*3)-2
             b=((val(SubStr(F10cCur.F10cCodUbi,9,3))-50)*4)-3
           
             Replace Filas2 With Substr(Filas2 ,1,a-1) + SubStr(F10cCur.F10cCodUbi,9,3) + Substr(Filas2,a+2)
             Replace Texto2 With Substr(Texto2 ,1,b-1) + F10cCur.F10cTamUbi + Substr(Texto2,b+4)
           *>Si fila  entre 100 y 150, actualizo Filas3 y textos3 
           Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),101,150)
             a=((val(SubStr(F10cCur.F10cCodUbi,9,3))-100)*3)-2
             b=((val(SubStr(F10cCur.F10cCodUbi,9,3))-100)*4)-3
           
             Replace Filas3 With Substr(Filas3 ,1,a-1) + SubStr(F10cCur.F10cCodUbi,9,3) + Substr(Filas3,a+2)
             Replace Texto3 With Substr(Texto3 ,1,b-1) + F10cCur.F10cTamUbi + Substr(Texto3,b+4)
         EndCase
        
      EndIf

      Do Case
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),1,15)
             Linea = 1
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),16,30)
             Linea = 2
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),31,45)
           Linea = 3
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),46,60)
           Linea = 4
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),61,75)
           Linea = 5
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),76,90)
           Linea = 6
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),91,105)
           Linea = 7
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),106,120)
           Linea = 8
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),121,135)
           Linea = 9
         Case Betw(Val(SubStr(F10cCur.F10cCodUbi,9,3)),136,150)
           Linea = 10
      EndCase

      Replace Lin_Imp With Substr(Lin_Imp,1,Linea-1) + '1' + SubStr(Lin_Imp,Linea+1)
      
      Select F10cCur
      Skip      
   EndDo

   Use In F10cCur
EndIf
Return

 
