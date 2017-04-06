*-- (c) Microsoft Corporation 1995

*-- Common include file
#INCLUDE "FOXPRO.H"

#DEFINE DEBUGMODE	.F.
#DEFINE INIFILE		"TASTRADE.INI"
#DEFINE CRLF		CHR(13) + CHR(10)
#DEFINE CR			CHR(13)
#DEFINE TAB			CHR(9)

#DEFINE CURRENCY	"$"
#DEFINE AERRORARRAY	7

*-- Constants used to read the system registry
#DEFINE HKEY_LOCAL_MACHINE			-2147483646  
#DEFINE KEY_SHARED_TOOLS_LOCATION	"Software\Microsoft\Shared Tools Location"
#DEFINE KEY_NTCURRENTVERSION		"Software\Microsoft\Windows NT\CurrentVersion"
#DEFINE KEY_WIN4CURRENTVERSION		"Software\Microsoft\Windows\CurrentVersion"
#DEFINE KEY_WIN4_MSINFO				"Software\Microsoft\Shared Tools\MSInfo"
#DEFINE KEY_QUERY_VALUE				1
#DEFINE ERROR_SUCCESS				0

#DEFINE DOLLAR_FORMAT1_LOC ": $"
#DEFINE DOLLAR_FORMAT2_LOC ""
#DEFINE DOLLAR_FORMAT3_LOC "$"

#DEFINE NOUSERS_LOC			"No hay usuarios en el archivo."
#DEFINE BADPASSWORD_LOC		"La contraseña no es válida."
#DEFINE NOOPENTABLE_LOC	"No se pudo abrir la tabla de contraseñas."

#DEFINE COPYRIGHT_LOC		"Copyright 1996 Microsoft Corporation"
#DEFINE RIGHTSRSRVD_LOC		"Reservados todos los derechos"
#DEFINE ABOUT_LOC			"Acerca de "
#DEFINE VERSIONLABEL_LOC    "Versión "
#DEFINE IDPROGRAMLABEL_LOC    "Id Programa "

#DEFINE FILENOTFOUND1_LOC	[Error al abrir "]
#DEFINE FILENOTFOUND2_LOC	[".  No se encontró el archivo.]

#DEFINE M_REMOVE_KEYWORD_LOC		"Quitar palabra clave"
#DEFINE M_KEYWORD_LOC				"Palabra clave"
#DEFINE M_KEYWORDS_LOC				"Palabras clave"
#DEFINE M_KEYWORDS_NO_SPACES_LOC	"Las palabras clave no pueden contener espacios"
#DEFINE M_ALREADY_EXISTS_LOC		"ya existe"
#DEFINE M_ALREADY_EXISTS_OVER_LOC	" ya existe, ¿desea sobrescribir?"
#DEFINE M_ALREADY_IN_USE_LOC		"ya está en uso"
#DEFINE M_UPDATING_KEYWORDS_LOC		"Actualizando palabras clave"
#DEFINE M_NOKEYWORDTABLE_LOC		"No se pudo encontrar la tabla de palabras clave."
#DEFINE M_NOOPENKEYWORDTABLE_LOC	"No se pudo abrir la tabla de palabras clave."
