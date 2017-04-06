*> Cabecera

   *> Descripcio ................. Listas.
   *> Modul ...................... Proceso.PRG
   *> Llenguatge ................. Visual FoxPro 3.0
   *> Sistema Operatiu ........... Windows
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... 
   *> Programador ................ 
   *> Data d'inici ............... 18.03.98
   *> Data de fi .................

*> Utilitats

   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*> Modulos........................  RegLis       (Regulación desde listas)


*>           (C)Copyright ALISFOUR 2004 BARCELONA



Procedure RegLis
*>Proceso de regulación de listas.-------------------------------------
Private l_Bien, a_Datos, c_NumOrd, c_ClaOcu, n_CanReg, n_RegF14c, ;
        l_OpnF14c, l_OpnF16c, FncA, PrmA, RLisF26l, f_Where

*> Pedir confirmaci¢n para regularizar.----------------------------
_Ok= F3_Sn(2,1,"ATENCION","Desea regularizar TODOS los movimientos marcados")

If _Ok
  Sele TraLis
  Go top

  Do While !Eof() 
    
   If Marca = 1
    If !Empty(F26LUbiDes)
       = F3_Sn(1,4,"No se puede hacer la regularización","de un movimiento de reposición")
       Skip
       Loop
    EndIf

    *> Obtener nº de registro en F14c.---------------------------------
    n_NumMov = F26lNumMov
    c_NumPal = F26lNumPal
    C_CodArt = F26lCodArt 
    N_CanFis = F26lCanfis
    Sele F14c

    *> Hacer cancelación del movimiento.-------------------------------
    l_Bien = .T.

    =CrtCursor('F26l', 'RLisF26l')
    f_Where = "F26lNumMov = '" + n_NumMov
    HayMov = F3_SQL('*', 'F26l', f_Where, , , 'RLisF26l')
    If HayMov = .F.   && No hay nada
       =F3_sn(4,1,"El movimiento no se ha encontrado")
       Select TraLis
       Skip
       Loop
    EndIf

    *> Crear objetos funcion actualizacion
    FncA = CreateObject("OraFncActz")
    PrmA = CreateObject("OraFncActz")
    PrmA.Inicializar
    FncA.ObjParm = PrmA

    Select RLisF26l
    With FncA.ObjParm
      .PUbOld = F26lUbiOri
      .PUFlag = "S"

      .POTDoc = F26lTipDoc
      .PONDoc = F26lNumDoc
      .POLDoc = F26lLinDoc
      .POFDoc = F26lFecDoc
      .PODAso = F26lDirAso
      .PONPed = F26lNumPed
      .POLPed = F26lLinPed
      .POFMov = Date()
      .POCPro = F26lCodPro
      .POCArt = F26lCodArt
      .PONLot = F26lNumLot
      .POSStk = F26lSitStk
      .POFCad = F26lFecCad
      .POCFis = F26lCanFis
      .POCAlm = Left(F26lUbiOri, 4)
      .POCUbi = F26lUbiOri
      .PONPal = F26lNumPal
      .POTPal = F26lTipPal
      .POFUni = F26lUniVen
      .POFSer = F26lUniPac
      .POFEnv = F26lPacCaj 
      .POFPal = F26lCajPal
      .POFFab = F26lFecFab
      .POFCal = F26lFecCal
      .PONAna = F26lNumAna
      .PONEnt = F26lNumEnt
      .POFlag = "S"

      .PSCAlm = Left(F26lUbiOri, 4)
      .PSCPro = F26lCodPro
      .PSCArt = F26lCodArt
      .PStOld = F26lSitStk
*      .PStNew = 
      .PSFlag = "S"

      .PTAcc = "06"
   EndWith
   FncA.Ejecutar

   
   With FncA.ObjParm
      .PMTMov = "4200"
      .PMNMov = Space(10)
      .PMFMov = Date()
      .PMNExp = F26lNumExp
      .PMStat = "0"
      .PMTUbi = F26lTipUbi
      .PMFgMP = "N"
      .PMFgHM = "S"
   EndWith
   FncA.ActHM
       
   FncA.ObjParm.PMNMov = F26lNumMov
   FncA.DelMP
   
   f_Where = "F26lNumMov = '" + n_NumMov + "'"
   =F3_DelTun("F26l", , f_Where)
   
   f_Where = "F24lCodPro='" + FncA.ObjParm.POCPro + "' And F24lTipDoc='" + FncA.ObjParm.POTDoc + ;
       "' And F24lNumDoc = '" + FncA.ObjParm.PONDoc + "' And F24lLinDoc = '" + Fnca.Objparm.POLDoc+"'"
   x_CanRes = 'F24lCanRes - FncA.ObjParm.POCFis'
   x_FlgEst = '1'
   =F3_UpdTun("F24l", , 'F24lCanRes, F24lFlgEst', 'x_CanRes, x_FlgEst', , f_Where)

   *> Incrementar stock 7000
   With FncA.ObjParm
      .PSCAlm = FncA.ObjParm.POCAlm
      .PSCPro = FncA.ObjParm.POCPro
      .PSCArt = FncA.ObjParm.POCArt
      .PStOld = "7000"
*      .PStNew = 
      .PUFlag = "N"
      .POFlag = "N"
      .PSFlag = "S"

      .PTAcc = "07"
   EndWith
   FncA.Ejecutar
   
    *> Borrar el movimiento cancelado de temporal movimientos.------
    EndIf
    Select TraLis
    Skip
  EndDo
EndIf
Select TraLis
Return


Procedure DesLis
*>Proceso de desasignar operario.-------------------------------------

      Wait Window "Desasignando Listas" NoWait
      Sele TraLis
      Do While !Eof()
         If Marca=1
            *> Inicializamos Operario y Nº Lista.---------------------
            c_CodOrd = F26lNumMov
            
            *> Sentencia (de muerte) Sql.--------------------------------------
            F_Where= " F26lNumMov = '"+C_CodOrd+_cm + _Bloqueo

            _xier=SqlExec(_asql,"Select * From F26l" + _em + " Where " + F_where,'Result') 
            If _xier>0
               =SqlMoreResults(_asql)
            Else
               =Aerror(err)
               If Err(5)= 54
                  = F3_Sn(1,1,"Movimiento bloqueado...") && Aviso
                  Return
               EndIf 
            EndIf
   
            Sele Result
            Go Top

            *> Encontramos registro.--------------------------------------------
            If !Eof()

               F_Update = "Update F26l" + _em + ;
                          "Set F26lNumLst = Space(6) And F026lcodope = Space(4) "+F_Where
                          
              _xier=SqlExec(_asql,F_Update) 

           EndIf

           If _xier > 0
              _xier=SqlExec(_asql,"commit") 
           Else 
              =aError(_odbc_err)
              lx=_odbc_err(2)+_odbc_err(3)+space(180)
              =f3_sn(1,1,left(lx,60),subs(lx,61),subs(lx,121))
             _xier=SqlExec(_asql,"RollBack") 
          EndIf

          *> Busco el registro y cambio el valor.-------------------
*         Select F14c
*         Locate For F14cNumDes = c_CodOrd
*         If Found()
*            Replace F14cCodOpe With "    "
*         EndIf

        EndIf
        Select TraLis
        Skip
     EndDo

Return
