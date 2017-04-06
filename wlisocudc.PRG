*>
*> Listado de Mapa Almacén - Dígito de Control.
Parameters Pantalla,Accion,Programa
Private cSelec, cFrom, cWhere, cGroup, cOrder
Private oEtiq
Private cCtrl

Do Case
   *> Preparar el cursor anter de enviarlo al Report.
   Case Pantalla = "INICIO"
      cWhere = _GCSS("F10cCodUbi", 1, 4) + "='" + _Alma + "' And " + ;
               _GCSS("F10cCodUbi", 5, 2) + " Between '" + zonaD + "' And '" + zonaH + "' And " + ;
               _GCSS("F10cCodUbi", 7, 2) + " Between '" + calleD+ "' And '" + calleH+ "' And " + ;
               _GCSS("F10cCodUbi", 9, 3) + " Between '" + filaD + "' And '" + filaH + "' And " + ;
               _GCSS("F10cCodUbi",12, 2) + " Between '" + pisoD + "' And '" + pisoH + "' And " + ;
               _GCSS("F10cCodUbi",14, 1) + " Between '" + profD + "' And '" + profH + "'"

      cSelec = "*"
      cFrom = "F10c"
      cGroup = ""
      cOrder = "F10cCodUbi"

      =f3_sql(cSelec, cFrom, cWhere, cOrder, cGroup, "F10c")

      *> Bucle de lectura de las ubicaciones leídas.
      Select F10c
      Go Top
      Do While !Eof()
         *> Crear instancia de clase etiquetas.
         If Type('oEtiq') # 'O'
            oEtiq = CreateObject('OraFncEtiq')
         EndIf
               
         *> Pasar los datos de esta ubicación al cursor de trabajo.
         *> Almacén + Zona + Calle + Piso + Prof
         Select XLLISOCU
         Seek SubStr(F10c.F10cCodUbi, 1, 8) + SubStr(F10c.F10cCodUbi,12, 3)

         oEtiq.PdCUbi = F10c.F10cCodUbi
         cCtrl = oEtiq.UbicDigit()
               
         If !Found()
            *> Crear registro nuevo en cursor de trabajo.
            Append Blank
            Replace Almacen With SubStr(F10c.F10cCodUbi, 1, 4), ;
                    Zona    With SubStr(F10c.F10cCodUbi, 5, 2), ;
                    Calle   With SubStr(F10c.F10cCodUbi, 7, 2), ; 
                    Piso    With SubStr(F10c.F10cCodUbi,12, 2), ;
                    Prof    With SubStr(F10c.F10cCodUbi,14, 1)
         EndIf

         Do Case
            *> Si es una de las 50 primeras filas actualizo Filas, Textos.
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)), 1, 50)
               a = (Val(SubStr(F10c.F10cCodUbi, 9, 3)) * 3) - 2
               b = (Val(SubStr(F10c.F10cCodUbi, 9, 3)) * 4) - 3

               Replace Filas With SubStr(Filas, 1, a - 1) + ;
                                  SubStr(F10c.F10cCodUbi, 9, 3) + ;
                                  SubStr(Filas, a + 3)

               Replace Texto With Substr(Texto, 1, b - 1) + ;
                                  Space(1) + ;
                                  '(' + ;
                                  cCtrl + ;
                                  ')' + ;
                                  Space(1) + ;
                                  SubStr(Texto, b + 4)

            *> Si fila entre 51 y 100, actualizo Filas2 y textos2. 
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)), 51, 100)
               a = ((Val(SubStr(F10c.F10cCodUbi, 9, 3)) - 50) * 3) - 2
               b = ((Val(SubStr(F10c.F10cCodUbi, 9, 3)) - 50) * 4) - 3
           
               Replace Filas2 With SubStr(Filas2, 1, a - 1) + ;
                                   SubStr(F10c.F10cCodUbi, 9, 3) + ;
                                   SubStr(Filas2, a + 3)

               Replace Texto2 With SubStr(Texto2, 1, b - 1) + ;
                                   Space(1) + ;
                                   '(' + ;
                                   cCtrl + ;
                                   ')' + ;
                                   Space(1) + ;
                                   SubStr(Texto2, b + 4)

            *> Si fila  entre 100 y 150, actualizo Filas3 y textos3 
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)), 101, 150)
               a = ((Val(SubStr(F10c.F10cCodUbi, 9, 3)) - 100) * 3) - 2
               b = ((Val(SubStr(F10c.F10cCodUbi, 9, 3)) - 100) * 4) - 3
           
               Replace Filas3 With SubStr(Filas3, 1, a - 1) + ;
                                   SubStr(F10c.F10cCodUbi, 9, 3) + ;
                                   SubStr(Filas3, a + 3)

               Replace Texto3 With SubStr(Texto3, 1, b - 1) + ;
                                   Space(1) + ;
                                   '(' + ;
                                   cCtrl + ;
                                   ')' + ;
                                   Space(1) + ;
                                   SubStr(Texto3, b + 4)  
         EndCase

         Do Case
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),1,15)
               Linea = 1
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),16,30)
               Linea = 2
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),31,45)
               Linea = 3
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),46,60)
               Linea = 4
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),61,75)
               Linea = 5
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),76,90)
               Linea = 6
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),91,105)
               Linea = 7
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),106,120)
               Linea = 8
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),121,135)
               Linea = 9
            Case Between(Val(SubStr(F10c.F10cCodUbi, 9, 3)),136,150)
               Linea = 10
         EndCase

         Replace Lin_Imp With Substr(Lin_Imp, 1, Linea - 1) + '1' + SubStr(Lin_Imp, Linea + 1)
        
         *> Leer la siguiente ubicación.
         Select F10c
         Skip      
      EndDo
EndCase

*>
If Type('oEtiq') # 'U'
   Release oEtiq
EndIf

_xier = 0
Return
