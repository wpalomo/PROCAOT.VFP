*> Leemos de la bascula.----------------------

	*> Abrimos el fichero.--------------------
	_Leebas = FOPEN('COM1')
	
	If _LeeBas < 0
	   =MessageBox('Error abriendo el puerto de la bacula',16,'TUNDEN')
	   Return
	EndIf
	
	*> Leemos el Valor
	Lectura = FGETS(_Leebas)
	
	If Type('Lectura') = 'C'
		Lectura = Val(Lectura)	
	EndIf
	
	Wait Window Str(Lectura)
	
    Cierra=Fclose(_LeeBas)	
    
    If Cierra = .F.
	   =MessageBox('Error cerrando el puerto de la bacula',16,'TUNDEN')
	   Return
    EndIf