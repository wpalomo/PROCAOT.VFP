**************************************************************
*      Funciones standard para ProDis / Procaot en Oracle    *
**************************************************************

*> Funciones ............................ F3_InsTun        Función insertar de Tunden.
*>                                        F3_UpdTun        Función actualizar de Tunden.
*>                                        F3_DelTun        Función borrar por clave de Tunden.
*>                                        F3_DelRowIdTun   Función borrar por RowId de Tunden.
*>                                        F3_SeekTun       Función buscar de Tunden.
*>                                        F3_SeekRowIdRun  Función buscar RowId de Tunden.
*>                                        F3_SqlTun        Función recuperar registros de Tunden.
*>                                        CrtCursor        Crear cursor a partir de fichero.
*>                                        CrtMCursor       Crear cursor a partir de varios ficheros.
*>                                        UpdCursor        Agregar campos a un cursor.
*>                                        AddFldToCursor   Agregar columnas a cursor por descripción.
*>                                        CrtFCursor       Crear cursor por campos de tablas.
*>                                        CrtLCursor       Crear lista de campos de un cursor.

*>=================================================================
*> F3_InsTun ................ Insertar un registro en tabla Oracle
*>=================================================================
*> Parámetros :   _FichIns    Tabla en la que se desea insertar.
*>                _FichDat    Fichero que contiene los datos.
*>                _Commit     'S' si tiene que hacer COMMIT, 'N' si no COMMIT (por defecto).

Function F3_InsTun
Parameter _FichIns, _FichDat, _Commit

Private oPRC
Local lIsUsed

_Commit = IIf(Type('_Commit') <> 'C', 'N', _Commit)
_Ali = Alias()
_Insertado = .T.

_oldPoint = Set('Point')

   *> Abrir la tabla, si cal.
   lIsUsed = Used(_FichIns)
   If !lIsUsed
      oPRC = CreateObject('procaot')
      =oPRC.OpenTabla(_FichIns)
   EndIf

   Select SYSFC
   Seek Upper(_FichIns)
   If Len(_FichIns)=4
      Sql_Fc = _FichIns + _em
   Else
      Sql_Fc = _FichIns
   EndIf

   *> Si no nos pasan fichero de datos, utilizamos _FichIns.
   If Type('_FichDat') <> 'C'
      _FichDat = _FichIns
   EndIf

   FcInsert = StrTran(Fc_Insert, "m.", _FichDat + ".")
   Set Point To '.'
   FcInsert = &FcInsert

***> Control fecha en blanco. Poner fecha mínima en lugar de nulo.----
   StsCentury = Sys(2001, 'Century')
   Set Century On
   nd = at("to_date('  /  /    ','dd-mm-yyyy')",FcInsert)
   Do While nd > 0
      FcInsert = Stuff(FcInsert, nd, 34, "to_date('" + DToC(_FecMin) + "', 'dd-mm-yyyy')")
      nd = At("to_date('  /  /    ','dd-mm-yyyy')", FcInsert)
   EndDo
   Set Century &StsCentury

   lxInsert = 'INSERT INTO ' + Sql_Fc + ' VALUES (' + FcInsert + ')'
   _xier=f3_SqlExec(_aSql, lxInsert)
   If _xier>0
      If _Commit = 'S'
         =SqlCommit(_aSql)
      EndIf
   Else
      _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
      _LxErr = _LxErr + 'SENTENCIA: ' + lxInsert + cr
      =Anomalias()
      _Insertado = .F.
   EndIf

_xier=0
Set Point To (_oldPoint)

If !lIsUsed
	Use In (_FichIns)
EndIf

If !Empty(_Ali)
   Select (_Ali)
EndIf

Return _Insertado

*>=================================================================
*> F3_UpdTun ................ Actualizar registros en tabla Oracle
*> Anular RLock().
*>=================================================================
*> Parámetros :   _FichUpd    Tabla en la que se desea actualizar.
*>                _Indice	  Código de índice
*>                _Campos     Campos que quiero actualizar
*>                _Valores    Condiciones de filtro.
*>                _FichDat    Fichero que contiene los datos.
*>                _Where      Condiciones de filtro.
*>                _Commit     'S' si tiene que hacer COMMIT, 'N' si no COMMIT (por defecto).
*>                _Rollback   'S' si tiene que hacer ROLLBACK, 'N' si no ROLLBACK (por defecto).

Function F3_UpdTun
Parameter _FichUpd, _Indice, _Campos, _Valores, _FichDat, _Where, _Commit, _Rollback
Private Num, oPRC
Local lIsUsed

Set MemoWidth To 2000

_Commit = IIf(Type('_Commit') <> 'C', 'N', _Commit)
_Rollback = IIf(Type('_Rollback') <> 'C', 'N', _Rollback)
_Ali=Alias()
_Actualizado=.T.

_oldPoint = Set('Point')

If Type('_Valores')<>'C'
  _Valores=''
EndIf
If Type('_FichDat')<>'C'
   _FichDat = _FichUpd
EndIf

   *> Abrir la tabla, si cal.
   lIsUsed = Used(_FichUpd)
   If !lIsUsed
      oPRC = CreateObject('procaot')
      =oPRC.OpenTabla(_FichUpd)
   EndIf

   Select SYSFC
   Seek Upper(_FichUpd)
   If Len(_FichUpd)=4
      Sql_Fc = _FichUpd + _Em
   Else
      Sql_Fc = _FichUpd
   EndIf
   Set Point To '.'
   FcUpdate = StrTran(Fc_Update, "m.", _FichDat + ".")
   If Type('_Campos') = 'C'               && Se definen manualmente los campos a actualizar
      FcUpdate = ''
      Do While !Empty(_Campos)
         nd = at(',',_Campos)
         If nd = 0
            Varvar  = AllTrim(_Campos)
            _Campos = ''
         Else
            Varvar  = AllTrim(Left(_Campos,nd-1))
            _Campos = Subs(_Campos,nd+1)
         EndIf
         If Empty(_Valores)
            v_Varvar = Varvar
         Else
            nd = at(',',_Valores)
            If nd=0
               v_Varvar = AllTrim(_Valores)
               _Valores = ''
            Else
               v_Varvar = AllTrim(Left(_Valores,nd-1))
               _Valores = Subs(_Valores,nd+1)
            EndIf
         EndIf
         
         *> Si se pasa _FichDat, poner fichero delante de la variable.------
         If _FichUpd <> _FichDat
            v_Varvar = _FichDat + "." + v_Varvar
         EndIf

         FcUpdate = FcUpdate + "+','+"
         Varvar = Left(Upper(Varvar) + Space(10),10)
         Select SysVar
         Seek Varvar
         Varvar = Trim(Varvar)
         Select(_FichDat)
         x_Varvar = &v_Varvar
         Do Case
            Case Type('x_Varvar')='C'
               FcUpdate=FcUpdate + "'" + Varvar + "=' + _cm+" + v_Varvar + "+_cm"
            Case Type('x_Varvar')='D' .Or. Type('x_Varvar')='T'
               FcUpdate = FcUpdate + "'" + Varvar + "=' + _GCD(v_Varvar)"

               *FcUpdate=FcUpdate +"'"+ Varvar +"=to_date('+_cm+ DToC(&v_Varvar) " + ;
               *     "+_cm+','+_cm+'dd-mm-yyyy'+_cm+')'"

*               FcUpdate=FcUpdate +"'"+ Varvar +"=to_date('+_cm+ IIf(IsNull(&v_Varvar), 
*                   DToC(&v_Varvar) " + 
*                    "+_cm+','+_cm+'dd-mm-yyyy'+_cm+')'"
            Case Type('x_Varvar')='N'
               If SysVar.Var_Dec=0
                  FcUpdate=FcUpdate +"'"+ Varvar +"='+Str("+ v_Varvar +","+LTrim(Str(SysVar.Var_Ln))+")"
               Else
                  FcUpdate=FcUpdate +"'"+ Varvar +"='+Str("+ v_varvar +","+LTrim(Str(SysVar.Var_Ln))+","+ Str(SysVar.Var_Dec,1)+")"
               EndIf
         EndCase
      EndDo
      If !Empty(FcUpdate)
         FcUpdate = Subs(FcUpdate,6)
         nd = at('"+"',FcUpdate)
         Do While nd > 0
            FcUpdate = Stuff(FcUpdate,nd,3,'')
            nd = at('"+"',FcUpdate)
         EndDo
      EndIf
   EndIf
   FcUpdate = &FcUpdate

***> Control fecha en blanco. Poner fecha mínima en lugar de nulo.----
   StsCentury = Sys(2001, 'Century')
   Set Century On
   nd = at("to_date('  /  /    ','dd-mm-yyyy')",FcUpdate)
   Do While nd > 0
      FcUpdate = Stuff(FcUpdate,nd,34,"to_date('" + DToC(_FecMin) + "', 'dd-mm-yyyy')")
      nd = at("to_date('  /  /    ','dd-mm-yyyy')",FcUpdate)
   EndDo
   Set Century &StsCentury

   *> Establecer valor del filtro de actualizacion.----------------------------
   Select SysFc
   If Type('_Where')<>'C'
      _Where = ''
   EndIf
   
   If !Empty(_Where)
      If _Where = 'ALL'
         lxClave = ''
      Else
         lxClave = _Where
      EndIf
   Else
      FcClave = Fc_Clave
      If Type('_Indice')='C'					&& Se define una condición especial
         FComp2=''
         For Num=1 To MemLines(SysFc.Fc_Clave2)
            FComp2 = MLine(SysFc.Fc_Clave2, Num)
            If Atc(_Indice+' ', FComp2)>0
               FcClave = Subs(FComp2, Len(_Indice)+1)
               Num = 9999
            EndIf
         EndFor
      EndIf
      Set Point To ","

      FcClave = StrTran(FcClave, "m.", _FichDat + ".")
      lxClave = &FcClave

      If _Teclado_Esp=.F.
         Set Point To "."
      EndIf
   EndIf

   If _Where = 'ALL'
      lxUpdate = 'UPDATE ' + Sql_Fc + ' SET ' + FcUpdate
   Else
      lxUpdate = 'UPDATE ' + Sql_Fc + ' SET ' + FcUpdate + ' WHERE ' + lxClave
   EndIf

   _xier=f3_SqlExec(_asql, lxUpdate)
   If _xier>0
      If _Commit = 'S'
         =SqlCommit(_asql)
      EndIf
      If Used(_FichDat)
         Select (_FichDat)
         * ????? If EOF()
         * ?????   Insert Into (_FichDat) From MemVar
         * ????? Else
         * ?????   Gather MemVar Memo
         * ????? EndIf
      EndIf
   Else
      _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
      _LxErr = _LxErr + 'SENTENCIA: ' + lxUpdate + cr
      =Anomalias()

      If _Rollback = 'S'
         =SqlRollBack(_Asql)
      EndIf
      _Actualizado = .F.
   EndIf

_xier=0
Set Point To (_oldPoint)

If !lIsUsed
	Use In (_FichUpd)
EndIf

If !Empty(_Ali)
   Select (_Ali)
EndIf

Return _Actualizado

*>===============================================================
*> F3_DelTun ................ Eliminar registros en tabla Oracle
*>===============================================================
*> Parámetros :   _FichDel    Tabla en la que se desea actualizar.
*>                _Indice	  Código de índice
*>                _Where      Condiciones de filtro.
*>                _Commit     'S' si tiene que hacer COMMIT, 'N' si no COMMIT (por defecto).

Function F3_DelTun
Parameter _FichDel, _Indice, _Where, _Commit
Private num
Set Memowidth To 2000

_Commit = IIf(Type('_Commit') <> 'C', 'N', _Commit)

_Ali=Alias()
_Eliminado=.T.

   Select SysFc
   Seek Upper(_FichDel)
*   Seek Left(_FichDel + Space(10),10)
   If Len(_FichDel)=4
      Sql_Fc=_FichDel + _em
   Else
      Sql_Fc=_FichDel
   EndIf
   
   If Type('_Where') <> 'C'
      _Where = ''
   EndIf
      
   If !Empty(_Where)
      If _Where = 'ALL'
         lxClave = ''
      Else
         lxClave = _Where
      EndIf
   Else
      FcClave = Fc_Clave
      If Type('_Indice')='C'					&& Se usa un índice diferente al principal
         FComp2=''
         For Num=1 To MemLines(SysFc.Fc_Clave2)
            FComp2=MLine(SysFc.Fc_Clave2, Num)
            If Atc(_Indice+' ', FComp2)>0
               FcClave=SubStr(FComp2, Len(_Indice)+1)
               Num=9999
            EndIf
         EndFor
      EndIf
      lxClave=&FcClave
   EndIf

   If _Where = 'ALL'
      lxDelete='DELETE FROM ' + Sql_Fc
   Else
      lxDelete='DELETE FROM ' + Sql_Fc+' WHERE ' + lxClave
   EndIf

   _xier=f3_SqlExec(_asql, lxDelete)
   If _xier>0
      If _Commit = 'S'
         =SqlCommit(_asql)
      EndIf
   Else
      _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
      _LxErr = _LxErr + 'SENTENCIA: ' + lxDelete + cr
      =Anomalias()
      =SqlRollBack(_Asql)
      _Eliminado=.F.
   EndIf

_xier=0

If !Empty(_Ali)
   Select (_Ali)
EndIf
Return _Eliminado

*>==============================================================
*> F3_SeekTun ............... Buscar registro.
*>==============================================================
*> Parámetros :   _Fics       Fichero en el que buscamos.
*>                _Where 	  Filtro de búsqueda.
*>                _Blancs	  Blanqueo de variables.
*>                _Replaces   Reemplazo de variables.
*>                _Var_Priv   Variables privadas.
*>                _n_Indice   Nombre del indice.

Function F3_SeekTun
Parameter _Fics, _Where, _Blancs, _Replaces, _Var_Priv, _n_Indice

Set MemoWidth To 2000
Do Case
*   Case WOnTop()<>'ST3TERM' .And. WOnTop()<>Trim(Sysprg.PRG_PROG) .And. !Empty(WOnTop())
*      Return .T.
   Case _esc=.T.
      Return .T.
EndCase
_Alis = Alias()
If Type('_Blancs')<>'C'
   _Blancs = ''
EndIf
_Blancs = Upper(_Blancs)

   Select SysFc
   Seek Upper(_Fics)
   Sql_Fc = _Fics + _Em

   lxSelect='SELECT * FROM ' + Sql_Fc + ' Where ' + _Where
   _xier = f3_SqlExec(_aSql, lxSelect, 'Consulta')
   If _xier < 0
      _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
      =Anomalias()
   EndIf
   =SqlMoreResults(_aSql)
   If Used('Consulta')
      Select Consulta
   EndIf

_Encontrado=.F.
If Type('_Var_Priv') = 'C' .And. !Empty(_Var_Priv)
   _Var_Priv = _Var_Priv+',  '
   nd = At(',',_Var_Priv)
   Do While nd>0
      lx = Left(_Var_Priv, nd-1)
      _Var_Priv = Subs(_Var_Priv, nd+1)
      nd = At('.',lx)
      If nd>0
         lx=Subs(lx, nd+1)
      EndIf
      nd = At('*',lx)
      If nd=0
         Private &lx
      Else
         Private All Like &lx
      EndIf
      nd = At(',', _Var_Priv)
   EndDo
EndIf

Do Case
   Case !EOF() And Used('Consulta')
      _Encontrado=.T.
      Scatter MemVar Memo
      If Type('_Replaces') = 'C'
         Do While !Empty(_Replaces)
            nd = At(',',_Replaces)
            If nd=0
               lx = _Replaces
               _Replaces = ''
            Else
               lx = Left(_Replaces, nd-1)
               _Replaces = Subs(_Replaces, nd+1)
            EndIf
            &lx
         EndDo
      EndIf

   Case Empty(_Blancs)
 
   Case Left(_Blancs,3) = 'ALL'
      Scatter MemVar Blank Memo

   Case Left(_Blancs,1) = '!'
      _Blancs = Subs(_Blancs,2)
      Private (_Blancs)
      Scatter MemVar Blank Memo

   Otherwise
      Do While !Empty(_Blancs)
         nd1 = At(',', _Blancs)
         If nd1 = 0
            lx1 = _Blancs
            _Blancs = ''
         Else
            lx1 = Left(_Blancs, nd1-1)
            _Blancs = IIf(Len(_Blancs)=nd1, '', Subs(_Blancs,nd1+1))
         EndIf
         Do Case
            Case Empty(lx1)
      
            Case Type(lx1) = 'C'
               &lx1 = Space(Len(&lx1))
            Case Type(lx1) = 'N'
               &lx1 = 0
            Case Type(lx1) = 'D'
               &lx1 = {  /  /  }
         EndCase
      EndDo
EndCase
If _Encontrado
   If Used(_Fics)
      Select (_Fics)
      If Eof()
         Append Blank
      EndIf
      Gather MemVar Memo
   Else
      Wait Window "Fichero " + _Fics + " no está abierto" NoWait
   EndIf
EndIf

If Used('Consulta')
   Use In Consulta
EndIf

If !Empty(_Alis)
   Select Alias (_Alis)
EndIf
Return _Encontrado

*>==============================================================
*> F3_SeekRowIdTun ............... Buscar RowId de registro.
*> Devuelve _Fics + 'RowId' si Ok.
*>==============================================================
*> Parámetros :   _Fics       Fichero en el que buscamos.
*>                _Where 	  Filtro de búsqueda.
*>                _Blancs	  Blanqueo de variables.
*>                _Replaces   Reemplazo de variables.
*>                _Var_Priv   Variables privadas.
*>                _n_Indice   Nombre del indice.

Function F3_SeekRowIdTun
Parameter _Fics, _Where, _Blancs, _Replaces, _Var_Priv, _n_Indice

Set MemoWidth To 2000
Do Case
*   Case WOnTop()<>'ST3TERM' .And. WOnTop()<>Trim(Sysprg.PRG_PROG) .And. !Empty(WOnTop())
*      Return .T.
   Case _esc=.T.
      Return .T.
EndCase
_Alis = Alias()
If Type('_Blancs')<>'C'
   _Blancs = ''
EndIf
_Blancs = Upper(_Blancs)

   Select SysFc
   Seek Upper(_Fics)
   Sql_Fc = _Fics + _Em

   lxSelect='Select ' + Sql_Fc + '.*, ' + ;
            'RowIdToChar(RowId) As ' + Sql_Fc + 'RowId' + ;
            ' From ' + Sql_Fc + ' Where ' + _Where

   _xier = f3_SqlExec(_aSql, lxSelect, 'Consulta')
   If _xier < 0
      _LxErr = Message() + 'PROGRAMA: ' + Prog() + cr + 'LINEA: ' + Str(LineNo()) + cr
      =Anomalias()
   EndIf
   =SqlMoreResults(_aSql)
   If Used('Consulta')
      Select Consulta
   EndIf

_Encontrado=.F.
If Type('_Var_Priv') = 'C' .And. !Empty(_Var_Priv)
   _Var_Priv = _Var_Priv+',  '
   nd = At(',',_Var_Priv)
   Do While nd>0
      lx = Left(_Var_Priv, nd-1)
      _Var_Priv = Subs(_Var_Priv, nd+1)
      nd = At('.',lx)
      If nd>0
         lx=Subs(lx, nd+1)
      EndIf
      nd = At('*',lx)
      If nd=0
         Private &lx
      Else
         Private All Like &lx
      EndIf
      nd = At(',', _Var_Priv)
   EndDo
EndIf

Do Case
   Case !EOF() And Used('Consulta')
      _Encontrado=.T.
      Scatter MemVar Memo
      If Type('_Replaces') = 'C'
         Do While !Empty(_Replaces)
            nd = At(',',_Replaces)
            If nd=0
               lx = _Replaces
               _Replaces = ''
            Else
               lx = Left(_Replaces, nd-1)
               _Replaces = Subs(_Replaces, nd+1)
            EndIf
            &lx
         EndDo
      EndIf

   Case Empty(_Blancs)
 
   Case Left(_Blancs,3) = 'ALL'
      Scatter MemVar Blank Memo

   Case Left(_Blancs,1) = '!'
      _Blancs = Subs(_Blancs,2)
      Private (_Blancs)
      Scatter MemVar Blank Memo

   Otherwise
      Do While !Empty(_Blancs)
         nd1 = At(',', _Blancs)
         If nd1 = 0
            lx1 = _Blancs
            _Blancs = ''
         Else
            lx1 = Left(_Blancs, nd1-1)
            _Blancs = IIf(Len(_Blancs)=nd1, '', Subs(_Blancs,nd1+1))
         EndIf
         Do Case
            Case Empty(lx1)
      
            Case Type(lx1) = 'C'
               &lx1 = Space(Len(&lx1))
            Case Type(lx1) = 'N'
               &lx1 = 0
            Case Type(lx1) = 'D'
               &lx1 = {  /  /  }
         EndCase
      EndDo
EndCase
If _Encontrado
   If Used(_Fics)
      Select (_Fics)
      Gather MemVar Memo
   Else
      Wait Window "Fichero " + _Fics + " no está abierto" NoWait
   EndIf
EndIf

If Used('Consulta')
   Use In Consulta
EndIf

If !Empty(_Alis)
   Select Alias (_Alis)
EndIf
Return _Encontrado

*>==============================================================
*> F3_SqlTun ................ Buscar registro.
*>==============================================================
*> Parámetros :   lxSelect    Campos a seleccionar.
*>                lxFrom  	  Ficheros.
*>                lxWhere 	  Filtro de registros.
*>                lxOrder     Ordenación.
*>                lxGroup     Agrupación.
*>                lxBloq      Bloqueo.
*>                lxCursor    Cursor a crear (inexistente)

Function F3_SqlTun
Parameters lxSelect, lxFrom, lxWhere, lxOrder, lxGroup, lxBloq, lxCursor

*> Comprobar que han pasado cursor y que no existe.-------------
If Empty(lxCursor)
   lxCursor = "CursorSQL"
EndIf

If Used(lxCursor)
*   =F3_Sn(1, 1, "El cursor que se intenta crear ya existe")
*   Return .F.
   Use In &lxCursor
EndIf

Set MemoWidth To 2000
_Sql_Zap=.T.
If Type('lxWhere') <> 'C'
   lxWhere = ''
EndIf
If Type('lxOrder') <> 'C'
   lxOrder = ''
EndIf
If Type('lxGroup') <> 'C'
   lxGroup = ''
EndIf
If type('lxBloq')<>'C'
  lxBloq = ''
endif

_lxCursor = '_' + lxCursor

   lx1 = lxFrom + ',    '
   lxFrom = ''
   nd = at(',', lx1)
   Do While nd>0
      lx2 = Left(lx1, nd-1)
      lx1 = SubS(lx1, nd+1)
      lxFrom = lxFrom+',' + lx2 + IIf(Len(lx2) = 4, _em, '')
      nd=at(',', lx1)
   EndDo
   lxFrom = SubS(lxFrom, 2)
   If !Empty(lxWhere)
      nd = Atc('Betw(', lxWhere)
      Do While nd > 0
         lx1 = SubS(lxWhere, nd)
         nd1 = at(')', lx1)
         lx1 = Left(lx1, nd1)                   && Expresion original
         lx2 = SubS(lx1, 6, Len(lx1)-6)         && Expresion a traducir   
         nd1 = At(',', lx2)
         lx3 = Left(lx2, nd1-1) + ' BETWEEN '
         lx2 = SubS(lx2, nd1+1)
         nd1 = At(',', lx2)
         lx3 = lx3 + Left(lx2, nd1-1) + ' AND ' + SubS(lx2, nd1+1)
         lxWhere = Stuff(lxWhere, nd, Len(lx1), lx3)
         nd = Atc('Betw(', lxWhere)
      enddo
      nd = Atc('.And.', lxWhere)
      Do While nd > 0
         lxWhere = Stuff(lxWhere, nd, 5, ' AND ')
         nd = Atc('.And.', lxWhere)
      EndDo
      nd = Atc('.Or.', lxWhere)
      Do While nd > 0
         lxWhere = Stuff(lxWhere, nd, 4, ' OR ')
         nd = Atc('.Or.', lxWhere)
      EndDo
      lxWhere = lxWhere + ' '
      nd = Atc('m.', lxWhere)
      Do While nd > 0
         lx1 = SubS(lxWhere, nd)
         nd1 = At(' ', lx1)
         If nd1 > 0
            lx1 = Left(lx1, nd1-1)
            nd1 = Len(lx1)
            lx2 = &lx1
            Do Case
               Case Type('lx2') = 'C'
                  lx2 = _cm + lx2 + _cm
               Case Type('lx2') = 'N'
            EndCase
            lxWhere = Stuff(lxWhere, nd, nd1, lx2)
         Else
            lxWhere = ''
         EndIf
         nd = Atc('m.', lxWhere)
      EndDo
   EndIf

lx = 'SELECT ' + lxSelect + ' FROM ' + lxFrom
If !Empty(lxWhere)
   lx = lx + ' WHERE ' + lxWhere
EndIf
If !Empty(lxOrder)
   lx = lx + ' ORDER BY ' + lxOrder
EndIf
If !Empty(lxGroup)
   lx = lx + ' GROUP BY ' + lxGroup
EndIf

lx = lx + " Into Cursor '" + _lxCursor + "'"
Set Talk On
&lx
Set Talk Off
lx = Dbf()
Select (lxCursor)
If _Sql_Zap
   Zap
EndIf

Append From &lx
Select (_lxCursor)
Use

Select (lxCursor)
Go Top

Return _OkSql

*>==============================================================
*> CrtCursor ................ Crear cursor a partir de fichero.
*>==============================================================
*> Parámetros :   _FichOri    Tabla con la estructura del cursor.
*>                _Cursor	  Nombre del cursor a crear.
*>                _TipDBF	  Tipo DBF ('F' - Fichero, 'C' - Cursor)

Procedure CrtCursor
Parameters _FichOri, _Cursor, _TipDBF
Private c_NomFich, b_Abierto
Private StrDbf

If Type('_TipDBF') <> 'C'
   _TipDBF = 'F'
EndIf

If _TipDBF = 'F'
   c_NomFich = _FichOri + _em
Else
   c_NomFich = _FichOri
EndIf

b_Abierto = IIf(Used(c_NomFich), .T., .F.)
Do FSelec With c_NomFich, "", "0"
=AFields(StrDbf)
If !b_Abierto
   Use
EndIf
Create Cursor (_Cursor) From Array StrDbf
Return

*>=========================================================================
*> CrtMCursor ................ Crear cursor a partir de múltiples ficheros.
*>=========================================================================
*> Parámetros :   cFichOri    Lista de ficheros con la estructura del cursor.
*>                cCursor	  Nombre del cursor a crear.
*>                cTipDBF	  Tipo DBF ('F' - Fichero, 'C' - Cursor)

Procedure CrtMCursor
Parameters cFichOri, cCursor, cTipDBF

Private StrDbf, _StrDbf, oPRC
Local nd, nInx, nLen, nCol, nEle, _FichOri, lIsUsed, _Fich

   If Type('cFichOri')<>'C'
      cFichOri = Select()
   EndIf

   *> Por defecto creará un cursor.
   If Type('cTipDBF') <> 'C'
      cTipDBF = 'C'
   EndIf

   _FichOri = cFichOri
   Store 0 To nInx, nLen, nEle, nCol

   *> Eliminar carácter '['.
   nd = At('[', _FichOri)
   If nd > 0
      _FichOri = SubStr(_FichOr, 2)      
   EndIf

   *> Eliminar carácter ']'.
   nd=at(']', _FichOri)
   If nd > 0
      _FichOri = SubStr(_FichOr, 1, nd - 1)      
   EndIf

   *> Proceso de generación del array con la estructura del cursor múltiple.
   Do While Len(_FichOri) > 0
      nd = At(',', _FichOri)
      If nd > 0
         *> Hay mas ficheros.
         _Fich = SubStr(_FichOri, 1, nd - 1)
         _FichOri = SubStr(_FichOri, nd + 1)
      Else
         *> Ultimo fichero.
         _Fich = _FichOri
         _FichOri = ''
      EndIf

      *> Abrir la tabla, si cal.
      lIsUsed = Used(_Fich)
      If !lIsUsed
         If Type('oPRC')<>'O'
            oPRC = CreateObject('procaot')
         EndIf
         =oPRC.OpenTabla(_Fich)
      EndIf

      *> Pasar la estructura de la tabla actual a un array.
      Select (_Fich)
      =AFields(_StrDbf)

      *> Cerrar la tabla actual si no estaba abierta al entrar en el programa.
      If !lIsUsed
         Use In (_Fich)
      EndIf

      *> Añadir la estructura de la tabla actual al array general.
      nInx = ALen(_StrDbf, 1)                               && Filas del array.
      nCol = ALen(_StrDbf, 2)                               && Columnas del array.

      Dimension StrDbf (nLen + nInx, nCol)                  && Redimensionar array destino.
      If nLen > 0
         nEle = AElement(StrDbf, nLen, nCol)                && Ultimo elemento del array destino.
      EndIf

      =ACopy(_StrDbf, StrDbf, 1, nInx * nCol, nEle + 1)     && Agregar al final.
      nLen = ALen(StrDbf, 1)                                && Filas del array destino.
   EndDo

   *> Crear el cursor multitabla.
   Create Cursor (cCursor) From Array StrDbf

   *> Eliminar el objeto.
   If Type('oPRC')=='O'
      Release oPRC
   EndIf

Return

*>============================================================================
*> UpdMCursor ................ Añadir columnas de un cursor a otro.
*> Parámetros :   cFichUpd    Fichero a añadir al cursor ya existente.
*>                cCursor	  Nombre del cursor a actualizar.
*>                nOrden	  Situación dentro del cursor (No Impl).
*>============================================================================
Procedure UpdCursor
Parameters cFichUpd, cCursor, nOrden

Private StrDbf, _StrDbf, oPRC
Local nd, nInx, nLen, nCol, nEle, lIsUsed, cCursorTmp

   If Type('cFichUpd')<>'C'
      cFichUpd = Select()
   EndIf

   *> El cursor debe existir, cretino !!.
   If !Used(cCursor)
      Return .F.
   EndIf

   Store 0 To nInx, nLen, nEle, nCol

   *> Abrir la tabla, si cal.
   lIsUsed = Used(cFichUpd)
   If !lIsUsed
      If Type('oPRC')<>'O'
         oPRC = CreateObject('procaot')
      EndIf
      =oPRC.OpenTabla(_Fich)
   EndIf

   *> Pasar la estructura del cursor destino a un array.
   Select (cCursor)
   =AFields(StrDbf)

   *> Crear cursor temporal para mantener los datos existentes.
   cCursorTmp = AllTrim(Sys(2015))
   Create Cursor (cCursorTmp) From Array strDbf
   Select (cCursorTmp)
   Append From Dbf(cCursor)

   *> Pasar la estructura de la tabla a añadir a un array.
   Select (cFichUpd)
   =AFields(_StrDbf)

   *> Cerrar la tabla a añadir, si no estaba abierta al entrar en el programa.
   If !lIsUsed
      Use
   EndIf

   *> Añadir la estructura de la tabla actual al array general.
   nLen = ALen(StrDbf, 1)                                && Filas del array destino.
   nInx = ALen(_StrDbf, 1)                               && Filas del array origen.
   nCol = ALen(_StrDbf, 2)                               && Columnas de los arrays.

   Dimension StrDbf (nLen + nInx, nCol)                  && Redimensionar array destino.
   nEle = AElement(StrDbf, nLen, nCol)                   && Ultimo elemento del array destino.

   =ACopy(_StrDbf, StrDbf, 1, nInx * nCol, nEle + 1)     && Agregar al final.

   *> Crear el cursor con la nueva estructura.
   Use In (cCursor)
   Create Cursor (cCursor) From Array StrDbf

   *> Agregar los datos del cursor temporal.
   Append From Dbf(cCursorTmp)
   Use In (cCursorTmp)

   *> Eliminar el objeto.
   If Type('oPRC')=='O'
      Release oPRC
   EndIf

Return

*>============================================================================
*> AddFldToCursor ........... Añadir columnas por descripción a un cursor.
*> Parámetros :   cCursor     Nombre del cursor a actualizar.
*>                cFldDesc    Características de la columna a añadir.
*>                nOrden      Situación dentro del cursor. Impl: 05.03.2007

*> Crear cursor si éste no existe. AVC - 09.01.2008
*> Corregir tratamiento campos fecha. AVC - 25.01.2008

*> cFldDesc ----> Lista de parámetros descriptivos de la columna, que corresponde
*>                a los valores devueltos por AFIELDS(), con el formato:
*>              [NAME='CodPro',                                  NM
*>               TYPE='C/N/F/M/...',                             TP
*>               LENGTH=10,                                      LN
*>               DECIMALS=0,                                     DC
*>               NULLS=S/N                                       NL
*>               VALUE='XXX'                                     VL
*>============================================================================
Procedure AddFldToCursor
Parameters cCursor, cFldDesc, nOrden

Private StrDbf, _StrDbf, oPRC
Local nd, cUpd, cPrm, cVal, nInx, nLen, nCol, cCursorTmp

If Type('cFldDesc')<>'C'
   _LxErr = 'Falta la definición de los parámetros'
   Return .F.
EndIf

If Used(cCursor)
	*> Pasar la estructura del cursor actual a un array.
	Select (cCursor)
	=AFields(_StrDbf)
Else
	*> Cursor nuevo.
	nLen = 1
	nCol = 5
	Dimension _StrDbf (nLen, nCol)

	*> Establecer valores por defecto del array de campos (by the flies).
	_StrDbf(nLen, 1) = "NONAME"
	_StrDbf(nLen, 2) = "C"
	_StrDbf(nLen, 3) = 1
	_StrDbf(nLen, 4) = 0
	_StrDbf(nLen, 5) = .F.
EndIf

*> Crear cursor temporal para mantener los datos existentes.
cCursorTmp = AllTrim(Sys(2015))
Create Cursor (cCursorTmp) From Array _StrDbf

If Used(cCursor)
	Select (cCursorTmp)
	Append From Dbf(cCursor)
	nLen = ALen(_StrDbf, 1) + 1                       && Filas del array destino.
	nCol = ALen(_StrDbf, 2)                           && Columnas de los arrays.
EndIf

*> Añadir la columna al cursor. Por defecto se añade al final.
Dimension StrDbf (nLen, nCol)                     && Redimensionar array destino.

If Type('nOrden')=='N'
   =ACopy(_StrDbf, StrDbf)
	=AIns(StrDbf, nOrden)
   nLen = nOrden
Else	
   =ACopy(_StrDbf, StrDbf)
EndIf

For nInx = 1 To nCol                              && Inicializar elemento añadido.
   StrDbf(nLen, nInx) = ''
EndFor

nd = At('[', cFldDesc)
If nd > 0
   cFldDesc = SubStr(cFldDesc, 2)
EndIf

nd = At(']', cFldDesc)
If nd > 0
   cFldDesc = SubStr(cFldDesc, 1, nd - 1)
EndIf

*> Selección de los parámetros recibidos.
Do While !Empty(cFldDesc)
   *> Primer paso: separar parámetros.
   nd = At(',', cFldDesc)
   If nd > 0
      cUpd = SubStr(cFldDesc, 1, nd - 1)
      cFldDesc = SubStr(cFldDesc, nd + 1)
   Else
      cUpd = cFldDesc
      cFldDesc = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.
   
   *> Tercer paso: Asignar el valor del parámetro a la propiedad.
   Do Case
      *> Nombre de la columna.
      Case cPrm=='NAME' .Or. cPrm=='NM'
         StrDbf(nLen, 1) = cVal

      *> Tipo de la columna.
      Case cPrm=='TYPE' .Or. cPrm=='TP'
         If AtC(cVal, 'CDLMNFIBYTG') > 0
            StrDbf(nLen, 2) = cVal
         EndIf

      *> Longitud de la columna.
      Case cPrm=='LENGTH' .Or. cPrm=='LN'
         StrDbf(nLen, 3) = Val(cVal)

      *> Nº de decimales de la columna.
      Case cPrm=='DECIMALS' .Or. cPrm=='DC'
         StrDbf(nLen, 4) = Val(cVal)

      *> Columna acepta NULLs.
      Case cPrm=='NULLS' .Or. cPrm=='NL'
         StrDbf(nLen, 5) = Iif(cVal=='S', .T., .F.)

      *> Valor inicial.
      Case cPrm=='VALUE' .Or. cPrm=='VL'

      *> No es un parámetro reconocido.
      Otherwise
   EndCase
EndDo

*> Crear el cursor con la nueva estructura.
Use In (Select(cCursor))
Create Cursor (cCursor) From Array StrDbf

*> Agregar los datos del cursor temporal.
Append From Dbf(cCursorTmp)
Use In (cCursorTmp)

Return

*>============================================================================
*> CrtFCursor................ Crear cursor a partir de descripción de campos.
*> Parámetros :   cCursor     Nombre del cursor a crear/actualizar.
*>                cCParam     Características de los campos.
*> Corregido tratamiento datos ya existentes. AVC - 12.06.2007
*> Corregido tratamiento datos ya existentes cuando el cursor no existe. AVC - 28.06.2007

*> cFldDesc ----> Lista de parámetros descriptivos de la columna, que corresponde
*>                a los valores devueltos por AFIELDS(), con el formato:
*>              [TABLE='F24c',                   TBL/TB
*>               FIELD='F24cCodPro'              FLD/FL
*>============================================================================
Procedure CrtFCursor
Parameters cCursor, cCParam

Private StrDbf, _StrDbf, StrDbfTmp, oPRC
Local nd, cUpd, cPrm, cVal, nInx, nLen, nCol, cCursorTmp
Local cCParamIn, lIsOpen, lStado

If Type('cCParam')<>'C'
   _LxErr = 'Falta la definición de los parámetros' + cr
   Return .F.
EndIf

Store .T. To lStado
Store .F. To lIsOpen
Store "" To cCursorTmp
Store 0 To nLen, nCol

*> Si ya existe el cursor, guardar datos y propiedades actuales.
If Used(cCursor)
   *> Pasar la estructura del cursor actual a un array.
   Select (cCursor)
   =AFields(_StrDbf)

   *> Crear cursor temporal para mantener los datos existentes.
   cCursorTmp = AllTrim(Sys(2015))
   Create Cursor (cCursorTmp) From Array _StrDbf
   Select (cCursorTmp)
   Append From Dbf(cCursor)
EndIf

*> Copia local de trabajo de los parámetros.
cCParamIn = cCParam

nd = At('[', cCParamIn)
If nd > 0
   cCParamIn = SubStr(cCParamIn, 2)
EndIf

nd = At(']', cCParamIn)
If nd > 0
   cCParamIn = SubStr(cCParamIn, 1, nd - 1)
EndIf

*> Procesar los parámetros recibidos.
Do While !Empty(cCParamIn)
   *> Primer paso: separar parámetros.
   nd = At(',', cCParamIn)
   If nd > 0
      cUpd = SubStr(cCParamIn, 1, nd - 1)
      cCParamIn = SubStr(cCParamIn, nd + 1)
   Else
      cUpd = cCParamIn
      cCParamIn = ''
   EndIf

   *> Segundo paso: Para cada parámetro, separar nombre de valor.
   nd = At('=', cUpd)
   If nd==0
      *> No es un formato correcto.
      Loop
   EndIf

   cPrm = Upper(AllTrim(SubStr(cUpd, 1, nd - 1)))   && Nombre del parámetro.
   cVal = SubStr(cUpd, nd + 1)                      && Valor del parámetro.
   
   *> Tercer paso: Realizar la acción apropiada, según el parámetro sea tabla o campo.
   Do Case
      *> Es un nombre de tabla.
      Case cPrm=='TABLE' .Or. cPrm=='TBL' .Or. cPrm=='TB'
         lIsOpen = Used(cVal)
         If !lIsOpen
            *> Abrir la tabla para obtener su descripción.
            If Type('oPRC')<>'O'
               oPRC = CreateObject('procaot')
            EndIf
            =oPRC.OpenTabla(cVal)
         Endif

         *> Guardar la descripción de la tabla.
         Select(cVal)
         =AFields(StrDbf)
         nCol = ALen(StrDbf, 2)                     && Columnas de los arrays.

         *> Cerrar tabla, si no estaba abierta al llamar a esta función.
         If !lIsOpen
            Use In (cVal)
         EndIf

      *> Es un nombre de columna.
      Case cPrm=='FIELD' .Or. cPrm=='FLD' .Or. cPrm=='FL'
         nLen = Iif(Type('_StrDbf')=='U', 0, ALen(_StrDbf, 1))
         Dimension _StrDbf(nLen + 1, nCol)

         For nInx = 1 To ALen(StrDbf, 1)
            If AllTrim(Upper(cVal))==AllTrim(Upper(StrDbf(nInx, 1)))
               *> Agregar descripción del campo al cursor.
               Dimension StrDbfTmp(1, nCol)
               =ACopy(StrDbf, StrDbfTmp, ((nInx - 1) * nCol) + 1, nCol)

               =ACopy(StrDbfTmp, _StrDbf, 1, -1, (nLen * nCol) + 1)
               Exit
            EndIf
         EndFor

      *> No es un parámetro reconocido.
      Otherwise
   EndCase
EndDo

*> Crear el cursor con la nueva estructura.
If lStado
   If Used(cCursor)
      Use In (cCursor)
   EndIf

   Create Cursor (cCursor) From Array _StrDbf

   *> Agregar los datos del cursor temporal.
   If Used(cCursorTmp)
      Append From Dbf(cCursorTmp)
      Use In (cCursorTmp)
   EndIf
EndIf

*> Eliminar el objeto.
If Type('oPRC')=='O'
   Release oPRC
EndIf

Return lStado

*>============================================================================
*> CrtLCursor................ Crear lista de campos Select de un cursor.
*> Parámetros :   cCursor     Nombre del cursor base.
*> Devuelve: cLista, cadena con la lista select.
*            Si error, cLista = "" y devuelve _LxErr con el error.
*>============================================================================
Procedure CrtLCursor

Parameters cCursor

Local cListaDeCampos, cLocalCursor
Local nInx

cListaDeCampos = ""

If Type('cCursor')<>'C'
	cLocalCursor = Select()
	If Empty(cLocalCursor)
	   _LxErr = 'No hay ninguna tabla definida'
	   Return cListaDeCampos
	EndIf
Else
	cLocalCursor = Select (cCursor)
EndIf

For nInx = 1 To FCount(cLocalCursor)
	If !Empty(cListaDeCampos)
		cListaDeCampos = cListaDeCampos + ", "
	EndIf
	cListaDeCampos = cListaDeCampos + Field(nInx, cLocalCursor)
EndFor

Return cListaDeCampos
