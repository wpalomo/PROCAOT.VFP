* DBF2RS.h - Header file for DBF2RS.prg and RS2DBF.prg.
*


*-- Reserved fields.
#DEFINE	F_RECNO_FIELD		"_RecNo"
#DEFINE	F_VARIANT_FIELD		"_Variant"

*-- Special codes
#DEFINE	EVAL_ITEM	CHR(0)+"_"+CHR(255)

*-- ASCII codes
#DEFINE TAB			CHR(9)
#DEFINE LF			CHR(10)
#DEFINE CR			CHR(13)
#DEFINE CR_LF		CR+LF
