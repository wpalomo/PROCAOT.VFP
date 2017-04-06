*> Capcelera

   *> Descripcio ................. Procesos del módulo de cálculo de volumetría.
   *> M¢dulo ..................... SaliVolG.
   *> Llenguatge ................. FoxPro 2.0
   *> Sistema Operatiu ........... MS-DOS Ver 3.30 o superior
   *> Equip ...................... IBM PC/XT/AT/PS-2 y compatibles
   *> Suport ..................... Floppy Disk 5.25/3.50 y disco fijo
   *> Diseño ..................... Javier Garrido
   *> Programador ................ Javier Garrido
   *> Data d'inici ............... 07.01.1999
   *> Data de fi ................. 07.01.1999

*> Utilitats
   *> Compilador ................. FoxPro Ver 2.0
   *> Linkador ................... FoxPro Ver 2.0
   *> Editor de programas ........ 
   *> Editor de pantalles ........ TheDraw Ver 4.01
   *> Procesador de textes ....... WordPerfect Ver 5.01

*> Notes
*>
*>
*>

*>           (C)Copyright ALISFOUR 2004 BARCELONA

*> Calcular volumetria de los documentos seleccionados.---------------
Procedure GrbDocS

   Private Estado, Stat, bb

   *> Comprobar que hay registros para procesar.----------------------
   If !f3_sn(2, 1, 'Confirma los datos introducidos?')
      Return .F.
   EndIf

   *> Bucle de lectura del grid de documentos.------------------------
   c_StsVol = "NO"
   Select 'CABVOL'
   Go Top
   Locate For Marca = 1  And  Estado = 'NO'
   Do While !Eof()
      *> Crear el objeto para realizar las reservas.------------------
      RsvFnc = CreateObject("OraFncResv")        && Procedimientos.
      RsvPrm = CreateObject("OraPrmResv")        && Par metros.
      RsvFnc.ObjParm = RsvPrm
      RsvFnc.ObjParm.Inicializar

      *> Asignar valores generales.
      RsvFnc.ObjParm.PwAsql = _Asql              && Handle SQL.
      RsvFnc.ObjParm.PwEmpr = _em                && Empresa.
      RsvFnc.ObjParm.PwAlma = _Alma              && Almacén.

      *> Asignar parámetros documento a objeto reserva.
      RsvFnc.ObjParm.DscCPro = CABVOL.F24cCodPro && Propietario.
      RsvFnc.ObjParm.DscTDoc = CABVOL.F24cTipDoc && Tipo Documento.
      RsvFnc.ObjParm.DscNDoc = CABVOL.F24cNumDoc && Nº Documento.

      Wait Clear
      Wait Window 'Documento: ' + CABVOL.F24cTipdoc + '-' + CABVOL.F24cNumdoc NoWait

      RsvFnc.VolGen                           && Generar volumetría.
      If RsvFnc.ObjParm.PtStVo < "50"
         RsvFnc.Commit                        && Actualiza SQL.
         Select CABVOL
         Replace Estado With 'SI'
         c_StsVol = IIf(c_StsVol <> "ER", "OK", c_StsVol)
      Else
         RsvFnc.RollBack                      && Cancelar SQL.
         Select CABVOL
         Replace Estado With 'NO'
         c_StsVol = "ER"
      EndIf

      *> Actualizar la cabecera del documento.---------------------
      Release RsvFnc
      Release RsvPrm

      *> Siguiente documento de salida a procesar.-----------------
      Select 'CABVOL'
      Continue
   EndDo

   *> Liberar, si cal, ficheros de trabajo.------------------------
   =SqlCommit(_asql)
   Do Case
      Case c_StsVol = "NO"
         =F3_SN(1, 1, 'No se ha tratado ningún documento')
      Case c_StsVol = "OK"
         =F3_SN(1, 1, 'La generación de volumetría ha finalizado')
      Case c_StsVol = "ER"
         =F3_SN(1, 1, 'Se han producido errorres durante la generación de la volumetría')
   EndCase
Return
