
CREATE TABLE F00A001 (
       F00ACODNUM           char(4) NOT NULL,
       F00ADESCRI           varchar(40) NULL,
       F00AABREVI           varchar(10) NULL,
       F00ANUMERO           numeric(10) NOT NULL,
       F00AUSRLCK           char(6) NOT NULL
)
;


ALTER TABLE F00A001
       ADD PRIMARY KEY (F00ACODNUM)
;


CREATE TABLE F00B001 (
       F00BCODMOV           char(4) NOT NULL,
       F00BDESCRI           varchar(40) NULL,
       F00BABREVI           varchar(10) NULL,
       F00BENTSAL           char(1) NOT NULL,
       CHECK (F00BENTSAL IN('E','S'))
)
;


ALTER TABLE F00B001
       ADD PRIMARY KEY (F00BCODMOV)
;


CREATE TABLE F00C001 (
       F00CCODSTK           char(4) NOT NULL,
       F00CDESCRI           varchar(40) NULL,
       F00CABREVI           varchar(10) NULL,
       F00CENTSAL           char(1) NOT NULL
)
;


ALTER TABLE F00C001
       ADD PRIMARY KEY (F00CCODSTK)
;


CREATE TABLE F00D001 (
       F00DCODMAG           char(1) NOT NULL,
       F00DDESCRI           varchar(40) NULL,
       F00DABREVI           varchar(10) NULL
)
;


ALTER TABLE F00D001
       ADD PRIMARY KEY (F00DCODMAG)
;


CREATE TABLE F00E001 (
       F00ETIPPRO           char(4) NOT NULL,
       F00EDESCRI           varchar(40) NULL,
       F00EABREVI           varchar(10) NULL,
       F00ECODEST           char(4) NULL
)
;


ALTER TABLE F00E001
       ADD PRIMARY KEY (F00ETIPPRO)
;


CREATE TABLE F00F001 (
       F00FTAMPAL           char(4) NOT NULL,
       F00FDESCRI           varchar(40) NULL,
       F00FABREVI           varchar(10) NULL
)
;


ALTER TABLE F00F001
       ADD PRIMARY KEY (F00FTAMPAL)
;


CREATE TABLE F00G001 (
       F00GCODFAM           char(4) NOT NULL,
       F00GDESCRI           varchar(40) NULL,
       F00GABREVI           varchar(10) NULL
)
;


ALTER TABLE F00G001
       ADD PRIMARY KEY (F00GCODFAM)
;


CREATE TABLE F00H001 (
       F00HCODUNI           char(4) NOT NULL,
       F00HDESCRI           varchar(40) NULL,
       F00HABREVI           varchar(10) NULL
)
;


ALTER TABLE F00H001
       ADD PRIMARY KEY (F00HCODUNI)
;


CREATE TABLE F00I001 (
       F00ITIPENT           char(4) NOT NULL,
       F00IDESCRI           varchar(40) NULL,
       F00IABREVI           varchar(10) NULL
)
;


ALTER TABLE F00I001
       ADD PRIMARY KEY (F00ITIPENT)
;


CREATE TABLE F00J001 (
       F00JCODPRV           char(4) NOT NULL,
       F00JDESCRI           varchar(40) NULL,
       F00JABREVI           varchar(10) NULL
)
;


ALTER TABLE F00J001
       ADD PRIMARY KEY (F00JCODPRV)
;


CREATE TABLE F00K001 (
       F00KCODDOC           char(4) NOT NULL,
       F00KDESCRI           varchar(40) NULL,
       F00KABREVI           varchar(10) NULL,
       F00KNUMERA           char(4) NULL
)
;


ALTER TABLE F00K001
       ADD PRIMARY KEY (F00KCODDOC)
;


CREATE TABLE F00L001 (
       F00LCODRUT           char(4) NOT NULL,
       F00LDESCRI           varchar(40) NULL,
       F00LABREVI           varchar(10) NULL,
       F00LACTIVO           char(1) NOT NULL,
       CHECK (F00LACTIVO IN ('S','N'))
)
;


ALTER TABLE F00L001
       ADD PRIMARY KEY (F00LCODRUT)
;


CREATE TABLE F00M001 (
       F00MCODTAM           char(4) NOT NULL,
       F00MDESCRI           varchar(40) NULL,
       F00MABREVI           varchar(10) NULL,
       F00MDIMLAR           float NOT NULL,
       F00MDIMANC           float NOT NULL,
       F00MDIMALT           float NOT NULL,
       F00MDIMVOL           float NOT NULL
)
;


ALTER TABLE F00M001
       ADD PRIMARY KEY (F00MCODTAM)
;


CREATE TABLE F00N001 (
       F00NCODSEC           char(4) NOT NULL,
       F00NDESCRI           varchar(40) NULL,
       F00NABREVI           varchar(10) NULL
)
;


ALTER TABLE F00N001
       ADD PRIMARY KEY (F00NCODSEC)
;


CREATE TABLE F00R001 (
       F00RCODINC           char(4) NOT NULL,
       F00RDESCRI           varchar(35) NOT NULL,
       F00RABREVI           varchar(10) NULL,
       F00RCODIMP           char(1) NOT NULL,
       CHECK (F00RCODIMP IN ('C', 'T', 'O', 'A')),
       CHECK (F00RCODIMP IN ('C', 'T', 'O', 'A'))
)
;


ALTER TABLE F00R001
       ADD PRIMARY KEY (F00RCODINC)
;


CREATE TABLE F00S001 (
       F00SCODSOL           char(4) NOT NULL,
       F00SDESCRI           varchar(35) NOT NULL,
       F00SABREVI           varchar(10) NULL
)
;


ALTER TABLE F00S001
       ADD PRIMARY KEY (F00SCODSOL)
;

CREATE TABLE F00T001 (
       F00TCODCON           char(4) NOT NULL,
       F00TDESCRI           varchar(35) NULL,
       F00TABREVI           varchar(10) NULL
)
;


ALTER TABLE F00T001
       ADD PRIMARY KEY (F00TCODCON)
;


CREATE TABLE F01C001 (
       F01CTIPENT           char(4) NOT NULL,
       F01CCODIGO           char(6) NOT NULL,
       F01CDESCRI           varchar(40) NULL,
       F01CNUMNIF           varchar(12) NOT NULL,
       F01CDIRECC           varchar(35) NULL,
       F01CPOBLAC           varchar(35) NULL,
       F01CPROVIN           char(4) NOT NULL,
       F01CCODPOS           char(5) NOT NULL,
       F01CNUMTEL           varchar(13) NULL,
       F01CNUMFAX           varchar(13) NULL,
       F01CCODRUT           char(4) NULL,
       F01CEMMAIL           varchar(30) NULL,
       F01CPAGWEB           varchar(30) NULL
)
;


ALTER TABLE F01C001
       ADD PRIMARY KEY (F01CTIPENT, F01CCODIGO)
;


CREATE TABLE F01P001 (
       F01PCODIGO           char(6) NOT NULL,
       F01PDESCRI           varchar(40) NULL,
       F01PNUMNIF           varchar(12) NOT NULL,
       F01PDIRECC           varchar(35) NULL,
       F01PPOBLAC           varchar(35) NULL,
       F01PPROVIN           char(4) NOT NULL,
       F01PCODPOS           char(5) NOT NULL,
       F01PNUMTEL           varchar(13) NULL,
       F01PNUMFAX           varchar(13) NULL,
       F01PPAGWEB           varchar(30) NULL,
       F01PEMAIL            varchar(30) NULL,
       F01PLOGO             varchar(60) NULL
)
;


ALTER TABLE F01P001
       ADD PRIMARY KEY (F01PCODIGO)
;

CREATE TABLE F01W001 (
       F01WCODIGO           char(6) NOT NULL,
       F01WACCESO           char(1) NOT NULL,
       F01WLEVEL            numeric(1) NOT NULL,
       F01WCRISN            char(1) NOT NULL,
       F01WPASSWD           char(12) NOT NULL
)
;


ALTER TABLE F01W001
       ADD PRIMARY KEY (F01WCODIGO)
;



CREATE TABLE P01A001 (
       P01ACODPRO           char(6) NOT NULL,
       P01ATIPDOC           char(4) NOT NULL,
       P01AESTADO           char(1) NOT NULL,
       P01ADESCRI           char(40) NULL,
       P01AABREVI           char(10) NULL,
       P01AARGBR            numeric(3) NOT NULL,
       P01AARGBG            numeric(3) NOT NULL,
       P01AARGBB            numeric(3) NOT NULL
)
;


ALTER TABLE P01A001
       ADD PRIMARY KEY (P01ACODPRO)
;



CREATE TABLE F01T001 (
       F01TCODIGO           char(6) NOT NULL,
       F01TDESCRI           varchar(40) NULL,
       F01TNUMNIF           varchar(12) NOT NULL,
       F01TDIRECC           varchar(35) NULL,
       F01TPOBLAC           varchar(35) NULL,
       F01TPROVIN           char(4) NOT NULL,
       F01TCODPOS           char(5) NOT NULL,
       F01TNUMTEL           varchar(13) NULL,
       F01TNUMFAX           varchar(13) NULL,
       F01TCODRUT           char(4) NULL,
       F01TPTSKMT           numeric(8,2) NOT NULL,
       F01TPTSKGS           numeric(8,2) NOT NULL,
       F01TPTSVIS           numeric(8,2) NOT NULL,
       F01TPTSVOL           numeric(8,2) NOT NULL,
       F01TEMMAIL           varchar(30) NULL,
       F01TPAGWEB           varchar(30) NULL,
       F01TMUELLE           char(14) NULL
)
;


ALTER TABLE F01T001
       ADD PRIMARY KEY (F01TCODIGO)
;


CREATE TABLE F01D001 (
       F01DCODTRP           char(6) NOT NULL,
       F01DCODPRO           char(6) NOT NULL,
       F01DCODCLI           char(10) NOT NULL
)
;


ALTER TABLE F01D001
       ADD PRIMARY KEY (F01DCODTRP, F01DCODPRO)
;


CREATE TABLE F01F001 (
       F01FMAQUIN           char(4) NOT NULL,
       F01FDESCRI           varchar(40) NULL,
       F01FTIPUNI           char(4) NOT NULL,
       F01FFLAG1            char(1) NULL
)
;


ALTER TABLE F01F001
       ADD PRIMARY KEY (F01FMAQUIN, F01FTIPUNI)
;


CREATE TABLE F01G001 (
       F01GMAQUIN           char(4) NOT NULL,
       F01GDESCRI           varchar(40) NULL,
       F01GZONINI           char(2) NOT NULL,
       F01GZONFIN           char(2) NOT NULL,
       F01GFLAG1            char(1) NULL
)
;


ALTER TABLE F01G001
       ADD PRIMARY KEY (F01GMAQUIN, F01GZONINI, F01GZONFIN)
;


CREATE TABLE F01M001 (
       F01MMAQUIN           char(4) NOT NULL,
       F01MPESMAX           numeric(9,3) NOT NULL,
       F01MDESCRI           varchar(40) NULL,
       F01MALTINI           char(2) NULL,
       F01MALTFIN           char(2) NULL,
       F01MZONINI           char(2) NULL,
       F01MZONFIN           char(2) NULL,
       F01MTIPUNI           char(4) NULL
)
;


ALTER TABLE F01M001
       ADD PRIMARY KEY (F01MMAQUIN)
;


CREATE TABLE F01O001 (
       F01OCODIGO           char(6) NOT NULL,
       F01ONUMOBS           numeric(3) NULL,
       F01OOBSERV           varchar(50) NULL
)
;


ALTER TABLE F01O001
       ADD PRIMARY KEY (F01OCODIGO)
;


CREATE TABLE F01U001 (
       F01UTIPENT           char(4) NOT NULL,
       F01UCODENT           char(6) NOT NULL,
       F01UUBIEXP           char(14) NOT NULL,
       F01UMUELLE           char(2) NOT NULL,
       F01UTIPORI           char(1) NOT NULL,
       F01UACTIVO           char(1) NOT NULL
)
;


ALTER TABLE F01U001
       ADD PRIMARY KEY (F01UTIPENT, F01UCODENT, F01UUBIEXP)
;


CREATE TABLE F01V001 (
       F01VCODVEH           char(6) NOT NULL,
       F01VDESCRI           varchar(30) NOT NULL,
       F01VABREVI           varchar(10) NULL,
       F01VCODMAT           char(18) NOT NULL,
       F01VMAXKGS           numeric(8,3) NOT NULL,
       F01VMAXVOL           numeric(10,5) NOT NULL
)
;


ALTER TABLE F01V001
       ADD PRIMARY KEY (F01VCODVEH)
;


CREATE TABLE F01X001 (
       F01XCODCON           char(6) NOT NULL,
       F01XNOMBRE           varchar(40) NULL,
       F01XDIRECC           varchar(35) NULL,
       F01XCODPOS           char(5) NOT NULL,
       F01XPOBLAC           varchar(35) NULL,
       F01XPROVIN           char(4) NOT NULL,
       F01XNUMNIF           varchar(12) NULL,
       F01XNUMTEL           varchar(13) NULL,
       F01XEMMAIL           varchar(30) NULL
)
;


ALTER TABLE F01X001
       ADD PRIMARY KEY (F01XCODCON)
;


CREATE TABLE F02C001 (
       F02CCODALM           char(4) NOT NULL,
       F02CDESCRI           varchar(40) NULL,
       F02CCODCEN           char(4) NULL
)
;


ALTER TABLE F02C001
       ADD PRIMARY KEY (F02CCODALM)
;


CREATE TABLE F03C001 (
       F03CCODALM           char(4) NOT NULL,
       F03CCODZON           char(2) NOT NULL,
       F03CDESCRI           varchar(40) NULL
)
;


ALTER TABLE F03C001
       ADD PRIMARY KEY (F03CCODALM, F03CCODZON)
;


CREATE TABLE F05C001 (
       F05CCODOPE           char(4) NOT NULL,
       F05CNOMBRE           varchar(30) NULL,
       F05CABREVI           varchar(10) NULL,
       F05CACTIVO           char(1) NOT NULL,
       F05CPRESEN           char(1) NOT NULL,
       F05CCODTER           char(4) NULL,
       CHECK (F05CPRESEN IN('S','N')),
       CHECK (F05CACTIVO IN ('S','N'))
)
;


ALTER TABLE F05C001
       ADD PRIMARY KEY (F05CCODOPE)
;


CREATE TABLE F05M001 (
       F05MCODOPE           char(4) NOT NULL,
       F05MTIPMOV           char(4) NOT NULL,
       F05MPICKSN           char(1) NOT NULL,
       F05MSEL              char(1) NOT NULL,
       CHECK (F05MPICKSN IN('S','G','U','N','K','T')),
       CHECK (F05MSEL   IN('S','N'))
)
;


ALTER TABLE F05M001
       ADD PRIMARY KEY (F05MCODOPE, F05MTIPMOV)
;


CREATE TABLE F46C001 (
       F46CCODIGO           char(4) NOT NULL,
       F46CDESCRI           varchar(40) NULL,
       F46CABREVI           varchar(10) NOT NULL,
       F46CALTSOP           float NOT NULL,
       F46CANCSOP           float NOT NULL,
       F46CPRFSOP           float NOT NULL,
       F46CPESSOP           float NOT NULL,
       F46CPESMAX           float NOT NULL,
       F46CPCTUTI           numeric(3) NOT NULL
)
;


ALTER TABLE F46C001
       ADD PRIMARY KEY (F46CCODIGO)
;


CREATE TABLE F05S001 (
       F05SCODOPE           char(4) NOT NULL,
       F05SCODSOP           char(4) NOT NULL,
       F05SCNTSOP           numeric(3) NOT NULL,
       F05SSEL              char(1) NOT NULL,
       CHECK (F05SSEL IN ('S','N'))
)
;


ALTER TABLE F05S001
       ADD PRIMARY KEY (F05SCODOPE, F05SCODSOP)
;


CREATE TABLE F10T001 (
       F10TCODNUM           char(1) NOT NULL,
       F10TDESCRI           varchar(40) NULL,
       F10TABREVI           varchar(10) NULL
)
;


ALTER TABLE F10T001
       ADD PRIMARY KEY (F10TCODNUM)
;

CREATE TABLE F10S001 (
	F10SCODPRO char(6)  NOT NULL,
	F10SCODALM char(4)  NOT NULL,
	F10SSITSTK char(4)  NOT NULL,
	F10SCODZON char(2)  NOT NULL,
	F10SUBIDSD char(14) NOT NULL,
	F10SUBIHST char(14) NULL 
) 
;

CREATE INDEX IS_F10SALMSTK ON F10S001
(
	F10SCODPRO,
	F10SCODALM,
	F10SSITSTK,
	F10SCODZON,
	F10SUBIDSD
)
;


CREATE TABLE F10C001 (
       F10CCODUBI           char(14) NOT NULL,
       F10CTIPALM           char(1) NOT NULL,
       F10CTAMUBI           char(4) NOT NULL,
       F10CVOLTOT           numeric(9,3) NOT NULL,
       F10CVOLLIB           numeric(9,3) NOT NULL,
       F10CPESTOT           numeric(9,3) NOT NULL,
       F10CPESOCU           numeric(9,3) NOT NULL,
       F10CTIPPRO           char(4) NOT NULL,
       F10CPICKSN           char(1) NOT NULL,
       F10CCOORDX           numeric(5) NOT NULL,
       F10CCOORDY           numeric(5) NOT NULL,
       F10CPRIORI           numeric(5) NOT NULL,
       F10CORDENT           numeric(5) NOT NULL,
       F10CORDSAL           numeric(5) NOT NULL,
       F10CMAXPAL           numeric(5) NOT NULL,
       F10CNUMOCU           numeric(5) NOT NULL,
       F10CFECDES            NULL,
       F10CESTENT           char(1) NOT NULL,
       F10CESTSAL           char(1) NOT NULL,
       F10CESTGEN           char(1) NOT NULL,
       F10CCODUSU           char(6) NOT NULL,
       CHECK (F10CESTENT IN ('S','N')),
       CHECK (F10CESTGEN IN ('L','O','B')),
       CHECK (F10CESTSAL IN ('S','N'))
)
;

CREATE INDEX IS_MOLADAS ON F10C001
(
       F10CESTENT,
       F10CTIPALM,
       F10CPICKSN,
       F10CCODUBI,
       F10CPRIORI,
       F10CTIPPRO
)
;


ALTER TABLE F10C001
       ADD PRIMARY KEY (F10CCODUBI)
;


CREATE TABLE F05U001 (
       F05UCODOPE           char(4) NOT NULL,
       F05UUBIDSD           char(14) NOT NULL,
       F05UUBIHST           char(14) NOT NULL,
       F05USEL              char(1) NOT NULL,
       F05UTIPMOV           char(1) NOT NULL,
       CHECK (F05UUBIDSD<=F05UUBIHST)
)
;


ALTER TABLE F05U001
       ADD PRIMARY KEY (F05UCODOPE, F05UUBIDSD, F05UUBIHST)
;

CREATE TABLE F06C001 (
	F06CALIAS             char(10) NOT NULL,
	F06CDEVICE            char(50) NOT NULL,
	F06CTAREA             char(10) NOT NULL,
	F06CPRIORI            numeric(2) NOT NULL,
	F06CTIPUSU            char(2) NOT NULL,
	F06CCODUSU            char(12) NOT NULL 
)
;

ALTER TABLE F06C001
       ADD PRIMARY KEY (F06CALIAS)
;

CREATE INDEX IS_F06CTAREA ON F06C001
(
	F06CTAREA
)
;


CREATE TABLE F06P001 (
	F06PALIAS             char(10) NOT NULL,
	F06PDESCRI            char(40) NOT NULL,
	F06PABREVI            char(10) NOT NULL,
      F06PMODO              char(1)
)
;


ALTER TABLE F06P001
       ADD PRIMARY KEY (F06PALIAS)
;


CREATE TABLE F06T001 (
	F06TALIAS             char(10) NOT NULL,
	F06TDESCRI            char(40) NOT NULL,
	F06TABREVI            char(10) NOT NULL
)
;


ALTER TABLE F06T001
       ADD PRIMARY KEY (F06TALIAS)
;

CREATE TABLE F06R001 (
	F06RPROCES            char(10) NOT NULL,
	F06RTAREA             char(10) NOT NULL,
	F06RACTIVO            char(1) NOT NULL,
	F06RPRIORI            numeric(2) NOT NULL,
	F06RMODO              char(1) NOT NULL,
	F06RPARAMS            char(1) NOT NULL
)
;


ALTER TABLE F06R001
       ADD PRIMARY KEY (F06RPROCES, F06RTAREA)
;

CREATE TABLE F06S001 (
	F06SPROCES            char(10) NOT NULL,
	F06STAREA             char(10) NOT NULL,
	F06SNAME              char(10) NOT NULL,
	F06SDESCRI            char(30) NOT NULL,
	F06SVALUE             char(30) NOT NULL
)
;


ALTER TABLE F06S001
       ADD PRIMARY KEY (F06SPROCES, F06STAREA, F06SNAME)
;


CREATE TABLE F06X001 (
	F06XPROCES            char(10) NOT NULL,
	F06XNAME              char(20) NOT NULL,
	F06XDESCRI            char(30) NOT NULL,
	F06XABREVI            char(10) NOT NULL
)
;


ALTER TABLE F06X001
       ADD PRIMARY KEY (F06XPROCES, F06XNAME)
;



CREATE TABLE F08C001 (
       F08CCODPRO           char(6) NOT NULL,
       F08CCODART           char(13) NOT NULL,
       F08CDESCRI           varchar(40) NOT NULL,
       F08CCODEAN           char(13) NOT NULL,
       F08CTIPALM           char(1) NOT NULL,
       F08CTIPPRO           char(4) NOT NULL,
       F08CSECCIO           char(4) NOT NULL,
       F08CTIPFAM           char(4) NOT NULL,
       F08CCODENT           char(6) NOT NULL,
       F08CTAMABI           char(4) NOT NULL,
       F08CTIPPAL           char(4) NOT NULL,
       F08CTIPUNI           char(4) NOT NULL,
       F08CULTLOT           varchar(15) NOT NULL,
       F08CVOLUNI           float NOT NULL,
       F08CPESUNI           float NOT NULL,
       F08CUNIVEN           numeric(6) NOT NULL,
       F08CUNIPAC           numeric(6) NOT NULL,
       F08CPACCAJ           numeric(6) NOT NULL,
       F08CCAJPAL           numeric(6) NOT NULL,
       F08CCADUCA           char(1) NOT NULL,
       F08CNUMDIA           numeric(4) NOT NULL,
       F08CCALIDA           char(1) NOT NULL,
       F08CMULLOT           char(1) NOT NULL,
       F08CMULPRO           char(1) NOT NULL,
       F08CCOEROT           numeric(6) NOT NULL,
       F08CNUMEXT           numeric(5) NOT NULL,
       F08CTIPETI           char(1) NOT NULL,
       F08CPCOSTE           float NOT NULL,
       F08CCAJMAN           numeric(5) NOT NULL,
       F08CMANPAL           numeric(5) NOT NULL,
       F08CNUMREM           numeric(4) NOT NULL,
       F08CLOTOBL           char(1) NOT NULL,
       F08CMATLAR           numeric(6) NOT NULL,
       F08CMATALT           numeric(6) NOT NULL,
       F08CMATANC           numeric(6) NOT NULL,
       F08CPTOREC           numeric(6) NOT NULL,
       F08CNUMSER           char(1) NOT NULL,
       F08CCOLORS           char(1) NOT NULL,
       F08CTALLAS           char(1) NOT NULL,
	 F08CCODTCC           char(4) NOT NULL,
	 F08CCODTCT           char(4) NOT NULL,
       CHECK ((F08CCADUCA='N' AND F08CNUMDIA=0) OR (F08CCADUCA='S' AND F08CNUMDIA>0))
)
;


ALTER TABLE F08C001
       ADD PRIMARY KEY (F08CCODPRO, F08CCODART)
;


CREATE TABLE F08E001 (
       F08ECODPRO           char(6) NOT NULL,
       F08ECODART           char(13) NOT NULL,
       F08ECODBAR           char(15) NOT NULL
)
;


ALTER TABLE F08E001
       ADD PRIMARY KEY (F08ECODPRO, F08ECODART, F08ECODBAR)
;


CREATE TABLE F08G001 (
       F08GCODPRO           char(6) NOT NULL,
       F08GCODART           char(13) NOT NULL,
       F08GCODAGR           char(4) NOT NULL,
       F08GLVLAGR           numeric(3) NOT NULL,
       F08GUNIAGR           numeric(6) NOT NULL,
       F08GCODUBI           char(14) NOT NULL,
       F08GCANMIN           numeric(12,3) NOT NULL,
       F08GCANMAX           numeric(12,3) NOT NULL
)
;


ALTER TABLE F08G001
       ADD PRIMARY KEY (F08GCODPRO, F08GCODART, F08GLVLAGR)
;


CREATE TABLE F08L001 (
       F08LCODPRO           char(6) NOT NULL,
       F08LCODART           char(13) NOT NULL,
       F08LCODCOM           char(13) NOT NULL,
       F08LCANTID           float NOT NULL,
       F08LPRECIO           float NULL
)
;


ALTER TABLE F08L001
       ADD PRIMARY KEY (F08LCODPRO, F08LCODART, F08LCODCOM)
;


CREATE TABLE F08S001 (
       F08SCODPRO           char(6) NOT NULL,
       F08SCODART           char(13) NOT NULL,
       F08STIPDOC           char(4) NOT NULL,
       F08SCANTID           numeric(12,3) NOT NULL,
       F08SCODSOP           char(4) NOT NULL,
       F08SSEL              char(1) NOT NULL,
       F08SLVLAGR           char(1) NOT NULL,
       F08SCANMAC           numeric(12,3) NOT NULL,
       CHECK (F08SLVLAGR IN('S','N')),
       CHECK (F08SSEL IN('S','N'))
)
;


CREATE TABLE F08O001 (
       F08OCODPRO           CHAR(6) NOT NULL,
       F08OCODART           CHAR(13) NOT NULL,
       F08ONUMOBS           CHAR(4) NOT NULL,
       F08OOBSERV           CHAR(254))
;       


ALTER TABLE F08O001
       ADD PRIMARY KEY (F08OCODPRO, F08OCODART, F08ONUMOBS)
       
;

ALTER TABLE F08S001
       ADD PRIMARY KEY (F08SCODPRO, F08SCODART, F08STIPDOC, 
              F08SCANTID)
;


CREATE TABLE F08T001 (
       F08TCODPRO           char(6) NOT NULL,
       F08TCODART           char(13) NOT NULL,
       F08TPRIORI           numeric(5) NOT NULL,
       F08TCODALT           char(13) NULL,
       CHECK (F08TCODART<>F08TCODALT)
)
;


ALTER TABLE F08T001
       ADD PRIMARY KEY (F08TCODPRO, F08TCODART, F08TPRIORI)
;


CREATE TABLE F08U001 (
       F08UCODPRO           char(6) NOT NULL,
       F08UARTDSD           char(13) NOT NULL,
       F08UARTHST           char(13) NOT NULL,
       F08UUBIDSD           char(14) NOT NULL,
       F08UUBIHST           char(14) NOT NULL,
       CHECK (F08UUBIDSD<=F08UUBIHST),
       CHECK (F08UARTDSD<=F08UARTHST)
)
;


ALTER TABLE F08U001
       ADD PRIMARY KEY (F08UCODPRO, F08UARTDSD, F08UARTHST, 
              F08UUBIDSD)
;


CREATE TABLE F08V001 (
       F08VCODPRO           char(6) NOT NULL,
       F08VCODART           char(13) NOT NULL,
       F08VCODCOL           char(4) NOT NULL,
       F08VCODTAL           char(4) NOT NULL
)
;

ALTER TABLE F08V001
       ADD PRIMARY KEY (F08VCODPRO, F08VCODART, F08VCODCOL, F08VCODTAL)
;



CREATE TABLE F11C001 (
       F11CCOEFRT           numeric(6) NOT NULL,
       F11CPRIINI           numeric(6) NOT NULL,
       F11CPRIFIN           numeric(6) NOT NULL,
       CHECK (F11CPRIFIN<=F11CPRIINI)
)
;


ALTER TABLE F11C001
       ADD PRIMARY KEY (F11CCOEFRT)
;


CREATE TABLE F11R001 (
       F11RTAMUBI           char(4) NOT NULL,
       F11RTAMPAL           char(4) NOT NULL,
       F11RNUMPAL           numeric(5) NOT NULL
)
;


ALTER TABLE F11R001
       ADD PRIMARY KEY (F11RTAMUBI, F11RTAMPAL)
;


CREATE TABLE F11T001 (
       F11TTIPPRO           char(4) NOT NULL,
       F11TPRIORI           numeric(5) NOT NULL,
       F11TTIPDES           char(4) NOT NULL
)
;


ALTER TABLE F11T001
       ADD PRIMARY KEY (F11TTIPPRO, F11TPRIORI)
;

CREATE TABLE F12C001 (
       F12CCODPRO           char(6) NOT NULL,
       F12CCODART           char(13) NOT NULL,
       F12CCODALM           char(4) NOT NULL,
       F12CPRIORI           numeric(5) NOT NULL,
       F12CCODUBI           char(14) NOT NULL,
       F12CCANMIN           numeric(12,3) NOT NULL,
       F12CCANMAX           numeric(12,3) NOT NULL,
       CHECK (F12CCANMAX>=F12CCANMIN)
)
;


ALTER TABLE F12C001
       ADD PRIMARY KEY (F12CCODPRO, F12CCODART, F12CCODALM, 
              F12CPRIORI)
;


CREATE TABLE F12E001 (
       F12ECODPRO           char(6) NOT NULL,
       F12ECODART           char(13) NOT NULL,
       F12ECODTAL           char(4) NOT NULL,
       F12ECODCOL           char(4) NOT NULL,
       F12EPRIORI           numeric(5) NOT NULL,
       F12ECODUBI           char(14) NOT NULL,
       F12ESTKMIN           numeric(12,3) NOT NULL,
       F12ESTKMAX           numeric(12,3) NOT NULL,
       CHECK (F12ESTKMAX>=F12ESTKMIN)
)
;


ALTER TABLE F12E001
       ADD PRIMARY KEY (F12ECODPRO, F12ECODART, F12ECODTAL, F12ECODCOL, 
              F12EPRIORI)
;


CREATE TABLE F13C001 (
       F13CCODALM           char(4) NOT NULL,
       F13CCODPRO           char(6) NOT NULL,
       F13CCODART           char(13) NOT NULL,
       F13CSITSTK           char(4) NOT NULL,
       F13CCANTID           numeric(15,3) NOT NULL
)
;


ALTER TABLE F13C001
       ADD PRIMARY KEY (F13CCODALM, F13CCODPRO, F13CCODART, 
              F13CSITSTK)
;


CREATE TABLE F14C001 (
       F14CNUMMOV           char(10) NOT NULL,
       F14CNUMENT           char(10) NULL,
       F14CTIPMOV           char(4) NOT NULL,
       F14CTIPDOC           char(4) NULL,
       F14CNUMDOC           char(13) NULL,
       F14CLINDOC           numeric(4) NULL,
       F14CFECDOC           date NOT NULL,
       F14CDIRASO           char(13) NULL,
       F14CNUMPED           varchar(15) NULL,
       F14CLINPED           numeric(4) NULL,
       F14CFECMOV           date NOT NULL,
       F14CCODART           char(13) NOT NULL,
       F14CNUMLOT           char(15) NULL,
       F14CSITSTK           char(4) NOT NULL,
       F14CFECCAD           date NULL,
       F14CCANFIS           numeric(12,3) NOT NULL,
       F14CCANASG           numeric(12,3) NOT NULL,
       F14CRUTHAB           char(4) NULL,
       F14CCODALM           char(4) NOT NULL,
       F14CUBIORI           char(14) NULL,
       F14CUBIDES           char(14) NULL,
       F14CNUMPAL           char(10) NOT NULL,
       F14CTIPPAL           char(4) NOT NULL,
       F14CUNIVEN           numeric(12,3) NOT NULL,
       F14CUNIPAC           numeric(12,3) NOT NULL,
       F14CPACCAJ           numeric(12,3) NOT NULL,
       F14CCAJPAL           numeric(12,3) NOT NULL,
       F14CFECFAB           date NULL,
       F14CFECCAL           date NULL,
       F14CNUMANA           char(6) NULL,
       F14CCODPRO           char(6) NOT NULL,
       F14CCODOPE           char(4) NULL,
       F14CNUMLST           char(6) NULL,
       F14CNUMEXP           char(6) NULL,
       F14CORDREC           char(6) NOT NULL,
       F14CESTMOV           char(1) NOT NULL,
       F14CORIRES           char(1) NULL,
       F14CTIPUBI           char(1) NULL,
       F14CTIPMAS           char(4) NULL,
       F14CNUMMAS           char(9) NULL,
       F14CMACUNI           char(1) NULL,
       F14CORDRUT           numeric(3) NULL,
       F14CTENTRE           char(4) NULL,
       F14CCENTRE           char(6) NULL,
       F14CVENHAB           char(6) NULL,
       F14CTIPMAC           char(4) NULL,
       F14CNUMMAC           char(9) NULL,
       F14CSECCIO           char(2) NULL,
       F14CFLAG1            char(1) NULL,
       F14CFLAG2            char(1) NULL,
       F14CIDOCUP           char(10) NOT NULL,
       CHECK (F14CESTMOV IN('0','3'))
)
;

CREATE INDEX IK_F14CNUMDOC ON F14C001
(
       F14CCODPRO,
       F14CTIPDOC,
       F14CNUMDOC
)
;

CREATE INDEX IS_CODUBI ON F14C001
(
       F14CUBIORI
)
;

CREATE INDEX IK_F14CIDOCUP ON F14C001
(
       F14CIDOCUP
)
;

CREATE INDEX SK_RADIOCOMPROBAR ON F14C001
(
       F14CNUMPAL,
       F14CTIPMOV,
       F14CCODPRO,
       F14CCODART
)
;


ALTER TABLE F14C001
       ADD PRIMARY KEY (F14CNUMMOV)
;


CREATE TABLE F14S001 (
       F14SNUMMOV           char(10) NOT NULL,
       F14SPESMOV           float NULL,
       F14SESTADO           char(1) NULL,
       F14SFLAG1            char(1) NULL,
       F14SFLAG2            char(1) NULL
)
;


ALTER TABLE F14S001
       ADD PRIMARY KEY (F14SNUMMOV)
;


CREATE TABLE F15C001 (
       F15CCODPRO           char(6) NOT NULL,
       F15CCODART           char(13) NOT NULL,
       F15CTIPUBI           char(1) NOT NULL,
       F15CPICKIN           char(1) NOT NULL,
       F15CCRIUBI           char(1) NOT NULL,
       F15CCOEROT           char(1) NOT NULL,
       F15CFORTAM           char(1) NOT NULL,
       F15CFORZON           char(1) NOT NULL,
       F15CFORUBI           char(1) NOT NULL,
       F15CCONAUT           char(1) NOT NULL,
       F15CDESAUT           char(1) NOT NULL,
       F15CETIBUL           char(1) NOT NULL,
       F15CETIPAL           char(1) NOT NULL,
       CHECK (f15cforubi in('S','N')),
       CHECK (f15ccoerot in('S','N')),
       CHECK (f15cetibul in('S','N')),
       CHECK (f15cfortam in('S','N')),
       CHECK (f15cforzon in('S','N')),
       CHECK (f15cconaut in('S','N')),
       CHECK (f15cdesaut in('S','N')),
       CHECK (f15cetipal in('S','N')),
       CHECK (f15cpickin in('S','N')),
       CHECK ((f15ccodpro<>' ') or                           ((f15ccodpro=' ') and (f15ccodart=' '))),
       CHECK (f15ctipubi in('A','M','S')),
       CHECK (f15ccriubi in('C','P'))
)
;


ALTER TABLE F15C001
       ADD PRIMARY KEY (F15CCODPRO, F15CCODART)
;


CREATE TABLE F16C001 (
       F16CCODPRO           char(6) NOT NULL,
       F16CCODART           char(13) NOT NULL,
       F16CCODUBI           char(14) NOT NULL,
       F16CTAMPAL           char(4) NOT NULL,
       F16CNUMLOT           char(15) NOT NULL,
       F16CSITSTK           char(4) NOT NULL,
       F16CFECCAD           date NOT NULL,
       F16CTIPENT           char(4) NULL,
       F16CCODENT           char(6) NULL,
       F16CNUMANA           char(10) NULL,
       F16CCANFIS           numeric(12,3) NOT NULL,
       F16CCANRES           numeric(12,3) NOT NULL,
       F16CUNIVEN           numeric(12,3) NOT NULL,
       F16CUNIPAC           numeric(12,3) NOT NULL,
       F16CPACCAJ           numeric(12,3) NOT NULL,
       F16CCAJPAL           numeric(12,3) NOT NULL,
       F16CNOTENT           char(10) NULL,
       F16CTIPDOC           char(4) NULL,
       F16CNUMDOC           char(13) NULL,
       F16CLINDOC           numeric(4) NULL,
       F16CFECFAB           date NULL,
       F16CFECENT           date NULL,
       F16CVOLOCU           numeric(12,3) NULL,
       F16CNUMPAL           char(10) NOT NULL,
       F16CESPICO           char(1) NOT NULL,
       F16CFLAG1            char(1) NULL,
       F16CFLAG2            char(1) NULL,
       F16CIDOCUP           char(10) NOT NULL,
       CHECK (F16CESPICO IN('S','N'))
)
;

CREATE INDEX IK_F16CCODUBI ON F16C001
(
       F16CCODUBI,
       F16CCODPRO,
       F16CCODART
)
;

CREATE INDEX IK_F16CIDOCUP ON F16C001
(
       F16CIDOCUP
)
;

CREATE INDEX INV_CANT ON F16C001
(
       F16CNUMPAL
)
;

CREATE INDEX SI_LOTEDOC ON F16C001
(
       F16CNUMLOT
)
;


ALTER TABLE F16C001
       ADD PRIMARY KEY (F16CCODPRO, F16CCODART, F16CCODUBI, 
              F16CNUMPAL, F16CNUMLOT, F16CSITSTK, F16CFECCAD)
;


CREATE TABLE F16T001 (
       F16TNUMPAL           char(10) NOT NULL,
       F16TPESPAL           numeric(10,3) NOT NULL,
       F16TVOLPAL           numeric(10,3) NOT NULL,
       F16TTEMPAL           numeric(4) NOT NULL,
       F16TFACCON           float NOT NULL,
       F16TPESENT           numeric(10,3) NOT NULL,
       F16TPESSAL           numeric(10,3) NOT NULL,
       F16TFLAG             char(1) NULL,
       F16TFLAG2            char(1) NULL,
       F16TFLAG3            char(1) NULL,
       F16TACOND            char(1) NOT NULL
)
;


ALTER TABLE F16T001
       ADD PRIMARY KEY (F16TNUMPAL)
;


CREATE TABLE F18C001 (
       F18CCODPRO           char(6) NOT NULL,
       F18CTIPDOC           char(4) NOT NULL,
       F18CNUMDOC           char(13) NOT NULL,
       F18CTIPENT           char(4) NULL,
       F18CCODENT           char(6) NULL,
       F18CFECPED           date NOT NULL,
       F18CFECPRE           date NULL,
       F18CFECCIE           date NULL,
       F18CNUMTRA           numeric(5) NULL,
       F18CALMENT           char(4) NOT NULL,
       F18CESTADO           char(1) NULL,
       F18CCODIMP           char(1) NULL,
       F18CNUMPED           varchar(10) NULL,
       F18CBOOKIN           varchar(10) NULL
)
;


ALTER TABLE F18C001
       ADD PRIMARY KEY (F18CCODPRO, F18CTIPDOC, F18CNUMDOC)
;


CREATE TABLE F18M001 (
       F18MNUMENT           char(10) NOT NULL,
       F18MCODTRA           char(6) NULL,
       F18MMATRIC           varchar(15) NULL,
       F18MCODCON           char(20) NULL,
       F18MALBPRV           varchar(10) NULL,
       F18MCODOPE           char(4) NULL,
       F18MMUELLE           char(14) NULL,
       F18MFECENT           date NULL,
       F18MBOOKIN           varchar(10) NULL,
       F18MCODALM           char(4) NOT NULL,
       F18MCENTRO           char(4) NULL,
       F18MCODPRO           char(6) NOT NULL,
       F18MORIGEN           char(1) NULL,
       F18MESTADO           char(1) NULL
)
;


ALTER TABLE F18M001
       ADD PRIMARY KEY (F18MNUMENT)
;


CREATE TABLE F18I001 (
       F18INUMENT           char(10) NOT NULL,
       F18ILINOBS           char(4) NOT NULL,
       F18ICODINC           char(4) NOT NULL,
       F18IOBSERV           varchar(40) NOT NULL,
       F18ICANTID           numeric(12,3) NOT NULL,
       F18IESTADO           char(1) NULL,
       F18IFACTUR           char(1) NULL,
       F18IFLAG1            char(1) NULL,
       F18IFLAG2            char(1) NULL
)
;


ALTER TABLE F18I001
       ADD PRIMARY KEY (F18INUMENT, F18ILINOBS)
;


CREATE TABLE F18L001 (
       F18LCODPRO           char(6) NOT NULL,
       F18LTIPDOC           char(4) NOT NULL,
       F18LNUMDOC           char(13) NOT NULL,
       F18LLINDOC           char(4) NOT NULL,
       F18LLINPED           numeric(4) NOT NULL,
       F18LFECPRE           date NULL,
       F18LCODART           char(13) NOT NULL,
       F18LNUMLOT           char(15) NULL,
       F18LSITSTK           char(4) NOT NULL,
       F18LFECCAD           date NULL,
       F18LCANPED           numeric(12,3) NOT NULL,
       F18LCANSER           numeric(12,3) NOT NULL,
       F18LCANPTE           numeric(12,3) NOT NULL,
       F18LESTADO           char(1) NULL
)
;

CREATE INDEX IK_F18LCODART ON F18L001
(
       F18LCODPRO,
       F18LCODART
)
;


ALTER TABLE F18L001
       ADD PRIMARY KEY (F18LCODPRO, F18LTIPDOC, F18LNUMDOC, 
              F18LLINDOC)
;


CREATE TABLE F18N001 (
       F18NNUMENT           char(10) NOT NULL,
       F18NCODPRO           char(6) NOT NULL,
       F18NTIPDOC           char(4) NOT NULL,
       F18NNUMDOC           char(13) NOT NULL,
       F18NLINDOC           char(4) NOT NULL,
       F18NLINPED           numeric(4) NOT NULL,
       F18NCODALM           char(4) NOT NULL,
       F18NCODART           char(13) NOT NULL,
       F18NNUMLOT           char(15) NOT NULL,
       F18NFECCAD           date NULL,
       F18NSITSTK           char(4) NOT NULL,
       F18NCANPED           numeric(12,3) NOT NULL,
       F18NCANALB           numeric(12,3) NOT NULL,
       F18NCANPAL           numeric(12,3) NOT NULL,
       F18NCANCAJ           numeric(12,3) NOT NULL,
       F18NCANUNI           numeric(12,3) NOT NULL,
       F18NESTADO           char(1) NULL,
       F18NCANREC           numeric(12,3) NOT NULL,
       F18NFLG1             char(1) NULL,
       F18NFLG2             char(1) NULL,
       F18NFLG3             char(1) NULL,
       F18NCANMUE           numeric(12,3) NOT NULL
)
;


ALTER TABLE F18N001
       ADD PRIMARY KEY (F18NNUMENT, F18NCODPRO, F18NTIPDOC, 
              F18NNUMDOC, F18NLINDOC, F18NCODART, F18NNUMLOT, 
              F18NSITSTK)
;


CREATE TABLE F18O001 (
       F18ONUMENT           char(10) NOT NULL,
       F18ONUMOBS           numeric(3) NULL,
       F18OOBSERV           varchar(254) NULL
)
;


ALTER TABLE F18O001
       ADD PRIMARY KEY (F18ONUMENT)
;


CREATE TABLE F19L001 (
       F19LNUMDEV           char(10) NOT NULL,
       F19LDEVSAP           varchar(10) NOT NULL,
       F19LDIRASO           char(13) NOT NULL,
       F19LFECREC           date NULL,
       F19LALBCLI           char(10) NOT NULL,
       F19LCODPRO           char(6) NOT NULL,
       F19LCODART           char(13) NOT NULL,
       F19LNUMLOT           char(15) NOT NULL,
       F19LFECCAD           date NULL,
       F19LCANDEV           numeric(12,3) NOT NULL,
       F19LCANREC           numeric(12,3) NOT NULL,
       F19LCANBAD           numeric(12,3) NOT NULL,
       F19LCANANA           numeric(12,3) NOT NULL,
       F19LPVENTA           float NOT NULL,
       F19LCANBON           numeric(12,3) NOT NULL,
       F19LTIPIMP           char(1) NOT NULL,
       F19LNUMVAL           varchar(15) NULL,
       F19LMOTIVO           char(4) NULL,
       F19LESTADO           char(1) NOT NULL,
       F19LNUMENV           char(6) NULL,
       F19LFECENV           date NULL,
       F19LFLAG1            char(1) NULL,
       F19LFLAG2            char(1) NULL,
       CHECK (F19LESTADO IN ('0', '1', '2', '3'))
)
;

CREATE INDEX IK_F19LNUMDEV ON F19L001
(
       F19LNUMDEV
)
;


ALTER TABLE F19L001
       ADD PRIMARY KEY (F19LNUMDEV, F19LALBCLI, F19LCODPRO, 
              F19LCODART, F19LNUMLOT, F19LPVENTA)
;


CREATE TABLE F1PA001 (
       F1PACODPRO           char(6) NOT NULL,
       F1PACODALM           char(4) NOT NULL,
       F1PAALMDES           char(8) NOT NULL
)
;


ALTER TABLE F1PA001
       ADD PRIMARY KEY (F1PACODPRO, F1PACODALM, F1PAALMDES)
;


CREATE TABLE F1PD001 (
       F1PDCODPRO           char(6) NOT NULL,
       F1PDTIPDOC           char(4) NOT NULL,
       F1PDTDCDES           char(8) NOT NULL
)
;


ALTER TABLE F1PD001
       ADD PRIMARY KEY (F1PDCODPRO, F1PDTDCDES)
;


CREATE TABLE F1PF001 (
       F1PFCODPRO           char(6) NOT NULL,
       F1PFCODFAM           char(4) NOT NULL,
       F1PFFAMDES           char(8) NOT NULL
)
;


ALTER TABLE F1PF001
       ADD PRIMARY KEY (F1PFCODPRO, F1PFFAMDES)
;


CREATE TABLE F1PM001 (
       F1PMCODPRO           char(6) NOT NULL,
       F1PMTIPMOV           char(4) NOT NULL,
       F1PMSITSTK           char(4) NOT NULL,
       F1PMMOVDES           varchar(8) NOT NULL,
       F1PMPRIORI           char(4) NOT NULL
)
;


ALTER TABLE F1PM001
       ADD PRIMARY KEY (F1PMCODPRO, F1PMTIPMOV, F1PMSITSTK, 
              F1PMPRIORI)
;


CREATE TABLE F1PN001 (
       F1PNCODPRO           char(6) NOT NULL,
       F1PNCODNUM           char(4) NOT NULL,
       F1PNNUMDES           varchar(8) NOT NULL
)
;


ALTER TABLE F1PN001
       ADD PRIMARY KEY (F1PNCODPRO, F1PNCODNUM)
;


CREATE TABLE F1PP001 (
       F1PPCODPRO           char(6) NOT NULL,
       F1PPTIPPRO           char(4) NOT NULL,
       F1PPPRODES           varchar(8) NOT NULL
)
;


ALTER TABLE F1PP001
       ADD PRIMARY KEY (F1PPCODPRO, F1PPTIPPRO)
;


CREATE TABLE F1PS001 (
       F1PSCODPRO           char(6) NOT NULL,
       F1PSSITSTK           char(4) NOT NULL,
       F1PSSITDES           varchar(8) NOT NULL,
       F1PSALMCLI           char(4) NOT NULL
)
;


ALTER TABLE F1PS001
       ADD PRIMARY KEY (F1PSCODPRO, F1PSSITSTK)
;


CREATE TABLE F1PT001 (
       F1PTCODPRO           char(6) NOT NULL,
       F1PTTIPMOV           char(4) NOT NULL,
       F1PTSITSTK           char(4) NOT NULL,
       F1PTTIPDOC           char(4) NOT NULL,
       F1PTSITCLI           varchar(20) NOT NULL,
       F1PTACTIVO           char(1) NOT NULL,
       CHECK (F1PTACTIVO IN ('S','N'))
)
;


ALTER TABLE F1PT001
       ADD PRIMARY KEY (F1PTCODPRO, F1PTTIPMOV, F1PTSITSTK, 
              F1PTTIPDOC)
;


CREATE TABLE F20C001 (
       F20CNUMMOV           char(10) NOT NULL,
       F20CNMOVMP           char(10) NOT NULL,
       F20CENTSAL           char(1) NULL,
       F20CTIPMOV           char(4) NOT NULL,
       F20CNUMENT           char(10) NULL,
       F20CTIPDOC           char(4) NULL,
       F20CNUMDOC           char(13) NULL,
       F20CLINDOC           numeric(4) NULL,
       F20CFECDOC           date NULL,
       F20CDIRASO           char(13) NULL,
       F20CNUMPED           varchar(15) NULL,
       F20CLINPED           numeric(4) NULL,
       F20CFECMOV           date NOT NULL,
       F20CRUTHAB           char(4) NULL,
       F20CNUMRUT           char(4) NULL,
       F20CCODTRA           char(6) NULL,
       F20CCODALM           char(4) NOT NULL,
       F20CCODPRO           char(6) NOT NULL,
       F20CCODART           char(13) NOT NULL,
       F20CNUMLOT           char(15) NULL,
       F20CSITSTK           char(4) NOT NULL,
       F20CFECCAD           date NULL,
       F20CCANFIS           numeric(12,3) NOT NULL,
       F20CUBIORI           char(14) NOT NULL,
       F20CUBIDES           char(14) NULL,
       F20CNUMPAL           char(10) NOT NULL,
       F20CTAMPAL           char(4) NULL,
       F20CUNIVEN           numeric(12,3) NOT NULL,
       F20CUNIPAC           numeric(12,3) NOT NULL,
       F20CPACCAJ           numeric(12,3) NOT NULL,
       F20CCAJPAL           numeric(12,3) NOT NULL,
       F20CFECFAB           date NULL,
       F20CFECCAL           date NULL,
       F20CNUMANA           char(6) NULL,
       F20CCODOPE           char(4) NULL,
       F20CNUMLST           char(6) NULL,
       F20CPICOSN           char(1) NOT NULL,
       F20CORIRES           char(1) NULL,
       F20CTIPUBI           char(4) NULL,
       F20CTIPMAS           char(4) NULL,
       F20CNUMMAS           char(9) NULL,
       F20CMACUNI           char(1) NULL,
       F20CORDRUT           numeric(3) NULL,
       F20CTENTRE           char(4) NULL,
       F20CCENTRE           char(6) NULL,
       F20CVENHAB           char(6) NULL,
       F20CTIPMAC           char(4) NULL,
       F20CNUMMAC           char(9) NULL,
       F20CNUMEXP           char(6) NULL,
       F20CFLGBLQ           char(1) NULL,
       F20CFLGENV           char(1) NULL,
       F20CFECENV           date NULL,
       F20CNUMENV           numeric(7) NULL,
       F20CFECORD           date NULL,
       F20CFLAG1            char(1) NULL,
       F20CFLAG2            char(1) NULL,
       F20CSECCIO           char(2) NULL,
       F20CIDOCUP           char(10) NOT NULL,
       CHECK (F20CENTSAL IN('E','S')),
       CHECK (F20CCANFIS > 0),
       CHECK (F20CPICOSN IN ('S','N'))
)
;

CREATE INDEX ESTMAN ON F20C001
(
       F20CCODPRO,
       F20CFECMOV,
       F20CTIPMOV
)
;

CREATE INDEX NUMENT ON F20C001
(
       F20CNUMENT,
       F20CCODPRO,
       F20CCODART
)
;

CREATE INDEX SK_DIARIO ON F20C001
(
       F20CCODPRO,
       F20CCODART,
       F20CTIPDOC,
       F20CNUMDOC,
       F20CTIPMOV,
       F20CFECMOV
)
;

CREATE INDEX SK_DIFERENCIAS ON F20C001
(
       F20CCODPRO,
       F20CTIPDOC,
       F20CNUMDOC,
       F20CTIPMOV
)
;

CREATE INDEX TRPALET ON F20C001
(
       F20CCODPRO,
       F20CNUMPAL
)
;

CREATE INDEX ESTOPE ON F20C001
(
       F20CCODOPE,
       F20CFECMOV,
       F20CCODPRO,
       F20CCODART,
       F20CTIPMOV
)
;

CREATE INDEX IK_F20CCODART ON F20C001
(
       F20CCODPRO,
       F20CCODART
)
;

CREATE INDEX IK_F20CCODUBI ON F20C001
(
       F20CUBIORI,
       F20CCODPRO,
       F20CCODART
)
;

CREATE INDEX IK_F20CFECMOV ON F20C001
(
       F20CFECMOV,
       F20CCODPRO,
       F20CCODART
)
;

CREATE INDEX IK_F20CNUMPAL ON F20C001
(
       F20CNUMPAL,
       F20CUBIORI
)
;


ALTER TABLE F20C001
       ADD PRIMARY KEY (F20CNUMMOV)
;


CREATE TABLE F22C001 (
       F22CTIPENT           char(4) NOT NULL,
       F22CCODENT           char(6) NOT NULL,
       F22CDIRASO           char(13) NOT NULL,
       F22CPUNOPE           varchar(13) NULL,
       F22CNOMASO           varchar(50) NULL,
       F22C1ERDIR           varchar(40) NULL,
       F22C2NDDIR           varchar(40) NULL,
       F22C3RDDIR           varchar(40) NULL,
       F22CDPOBLA           varchar(35) NULL,
       F22CDPROVI           varchar(35) NULL,
       F22CCODPOS           char(5) NULL,
       F22CCPROVI           char(4) NULL,
       F22CCODPAS           char(3) NULL,
       F22CNUMTEL           varchar(15) NULL,
       F22CNUMFAX           varchar(15) NULL,
       F22CCODNIF           char(15) NULL,
       F22CFLETE            char(1) NULL,
       F22CEXTRAD           char(1) NULL
)
;


ALTER TABLE F22C001
       ADD PRIMARY KEY (F22CTIPENT, F22CCODENT, F22CDIRASO)
;


CREATE TABLE F22P001 (
       F22PCODPRO           char(6) NOT NULL,
       F22PDIRASO           char(13) NOT NULL,
       F22PPUNOPE           varchar(13) NULL,
       F22PNOMASO           varchar(50) NULL,
       F22P1ERDIR           varchar(40) NULL,
       F22P2NDDIR           varchar(40) NULL,
       F22P3RDDIR           varchar(40) NULL,
       F22PDPOBLA           varchar(35) NULL,
       F22PDPROVI           varchar(35) NULL,
       F22PCODPOS           char(5) NULL,
       F22PCPROVI           char(4) NULL,
       F22PCODPAS           char(3) NULL,
       F22PTELEFO           varchar(15) NULL,
       F22PNUMFAX           varchar(15) NULL,
       F22PCODNIF           char(15) NULL,
       F22PFLETE            char(1) NULL,
       F22PEXTRAD           char(1) NULL
)
;


ALTER TABLE F22P001
       ADD PRIMARY KEY (F22PCODPRO, F22PDIRASO)
;


CREATE TABLE F24C001 (
       F24CCODPRO           char(6) NOT NULL,
       F24CTIPDOC           char(4) NOT NULL,
       F24CNUMDOC           char(13) NOT NULL,
       F24CFECDOC           date NOT NULL,
       F24CDIRASO           char(13) NULL,
       F24CNUMPED           varchar(15) NULL,
       F24CFECPRE           date NULL,
       F24CRUTHAB           char(4) NULL,
       F24CORDRUT           numeric(3) NOT NULL,
       F24CTIPENT           char(4) NOT NULL,
       F24CCODENT           char(6) NOT NULL,
       F24CTENTRE           char(4) NOT NULL,
       F24CCENTRE           char(6) NOT NULL,
       F24CVENHAB           char(6) NULL,
       F24CMUELLE           char(2) NULL,
       F24CCODTRA           char(6) NOT NULL,
       F24CCODTRF           char(6) NOT NULL,
       F24CFRATOT           numeric(5) NOT NULL,
       F24CFRAACT           numeric(5) NOT NULL,
       F24CCAJTOT           numeric(5) NOT NULL,
       F24CCAJACT           numeric(5) NOT NULL,
       F24CPALTOT           numeric(5) NOT NULL,
       F24CPALACT           numeric(5) NOT NULL,
       F24CNUMBUL           numeric(5) NOT NULL,
       F24CTOTKGS           float NOT NULL,
       F24CTOTVOL           float NOT NULL,
       F24CIMPBAS           numeric(9) NOT NULL,
       F24CGASTOS           numeric(9) NOT NULL,
       F24CTOTIVA           numeric(7) NOT NULL,
       F24CTOTIMP           numeric(9) NOT NULL,
       F24CFECCRE           date NOT NULL,
       F24CNUMTRP           numeric(6) NULL,
       F24CALMSER           char(4) NULL,
       F24CALMDES           char(4) NULL,
       F24CPOLSER           char(1) NULL,
       F24CPRIORI           numeric(2) NULL,
       F24CNUMALB           varchar(13) NULL,
       F24CFECALB           date NULL,
       F24CVALALB           char(1) NULL,
       F24CCONALB           char(1) NULL,
       F24CAGRALB           char(1) NULL,
       F24CNUMCOP           numeric(1) NULL,
       F24CIMPLOT           char(1) NULL,
       F24CCONNOT           char(1) NULL,
       F24CCODIMP           char(5) NULL,
       F24CDESPPP           numeric(5,2) NULL,
       F24CCCCOBR           char(4) NULL,
       F24CDCCOBR           varchar(15) NULL,
       F24CCODMON           char(4) NULL,
       F24CDESMON           varchar(15) NULL,
       F24CTIPPED           char(1) NULL,
       F24CSEGSOC           char(1) NOT NULL,
       F24CPORTES           char(1) NULL,
       F24CUBIEXP           char(14) NOT NULL,
       F24CCTRDRO           char(1) NULL,
       F24CCTRPRO           char(1) NULL,
       F24CARTPRT           char(1) NULL,
       F24CENVIND           char(1) NULL,
       F24CFLGEST           char(1) NOT NULL,
       F24CFLAG1            char(1) NULL,
       F24CFLAG2            char(1) NULL,
       F24CFLAG3            char(1) NULL,
       F24CFLAG4            char(1) NULL
)
;


ALTER TABLE F24C001
       ADD PRIMARY KEY (F24CCODPRO, F24CTIPDOC, F24CNUMDOC)
;


CREATE TABLE F24L001 (
       F24LCODPRO           char(6) NOT NULL,
       F24LTIPDOC           char(4) NOT NULL,
       F24LNUMDOC           char(13) NOT NULL,
       F24LLINDOC           char(4) NOT NULL,
       F24LFECDOC           date NOT NULL,
       F24LFECPRE           date NULL,
       F24LRUTHAB           char(4) NULL,
       F24LTDOCCO           char(4) NULL,
       F24LNDOCCO           char(13) NULL,
       F24LCODART           char(13) NOT NULL,
       F24LNUMLOT           char(15) NULL,
       F24LSITSTK           char(4) NOT NULL,
       F24LFECCAD           date NULL,
       F24LCANDOC           numeric(12,3) NOT NULL,
       F24LCANENV           numeric(12,3) NOT NULL,
       F24LCANRES           numeric(12,3) NOT NULL,
       F24LALMSER           char(4) NULL,
       F24LPRECIO           float NULL,
       F24LDTOLIN           float NULL,
       F24LIVALIN           float NULL,
       F24LEQVLIN           float NULL,
       F24LNUMDRG           varchar(15) NULL,
       F24LFLGEST           char(1) NOT NULL,
       F24LFLAG1            char(1) NOT NULL,
       F24LFLAG2            char(1) NOT NULL,
       F24LFLAG3            char(1) NOT NULL
)
;

CREATE INDEX IK_F24LCODART ON F24L001
(
       F24LCODPRO,
       F24LCODART
)
;


ALTER TABLE F24L001
       ADD PRIMARY KEY (F24LCODPRO, F24LTIPDOC, F24LNUMDOC, 
              F24LLINDOC)
;


CREATE TABLE F24O001 (
       F24OCODPRO           char(6) NOT NULL,
       F24OTIPDOC           char(4) NOT NULL,
       F24ONUMDOC           char(13) NOT NULL,
       F24OLINOBS           char(4) NOT NULL,
       F24ODESOBS           varchar(360) NULL,
       F24OIMPOBS           varchar(8) NULL,
       F24O1ERFLG           char(1) NULL,
       F24O2NDFLG           char(1) NULL
)
;


ALTER TABLE F24O001
       ADD PRIMARY KEY (F24OCODPRO, F24OTIPDOC, F24ONUMDOC, 
              F24OLINOBS)
;


CREATE TABLE F24S001 (
       F24SCODPRO           char(6) NOT NULL,
       F24STIPDOC           char(4) NOT NULL,
       F24SNUMDOC           char(13) NOT NULL,
       F24SLINDOC           char(4) NOT NULL,
       F24SCODART           char(13) NOT NULL,
       F24SNUMSER           char(20) NOT NULL,
       F24SDESSER           varchar(40) NULL,
       F24SFLAG1            char(1) NULL,
       F24SFLAG2            char(1) NULL
)
;


ALTER TABLE F24S001
       ADD PRIMARY KEY (F24SCODPRO, F24STIPDOC, 
              F24SNUMDOC, F24SLINDOC, F24SCODART, F24SNUMSER)
;


CREATE TABLE F24T001 (
       F24TCODPRO           char(6) NOT NULL,
       F24TTIPDOC           char(4) NOT NULL,
       F24TNUMDOC           char(13) NOT NULL,
       F24TPUNOPE           varchar(13) NULL,
       F24TTIPDIR           char(1) NULL,
       F24TNOMASO           varchar(50) NULL,
       F24T1ERDIR           varchar(40) NULL,
       F24T2NDDIR           varchar(40) NULL,
       F24T3RDDIR           varchar(40) NULL,
       F24TDPOBLA           varchar(35) NULL,
       F24TDPROVI           varchar(35) NULL,
       F24TCODPOS           char(5) NULL,
       F24TCPROVI           char(4) NULL,
       F24TCODPAS           char(3) NULL,
       F24TNUMTEL           varchar(15) NULL,
       F24TNUMFAX           varchar(15) NULL,
       F24TNUMNIF           varchar(15) NULL,
       F24TFLETE            char(1) NULL,
       F24TEXTRAD           char(1) NULL
)
;


ALTER TABLE F24T001
       ADD PRIMARY KEY (F24TCODPRO, F24TTIPDOC, F24TNUMDOC)
;


CREATE TABLE F25C001 (
       F25CCODPRO           char(6) NOT NULL,
       F25CCODART           char(13) NOT NULL,
       F25CSERPIC           char(1) NOT NULL,
       F25CTIPMOV           char(1) NOT NULL,
       F25CORDEXT           char(1) NOT NULL,
       F25CDEJPIC           char(1) NOT NULL,
       F25CTIPRES           char(1) NOT NULL,
       F25CCONAUT           char(1) NOT NULL,
       F25CIMPETI           char(1) NOT NULL,
       F25CTIPPRO           char(4) NOT NULL,
       F25CREPLOT           char(1) NOT NULL,
       F25CREPCAD           char(1) NOT NULL,
       F25CREPQTY           char(1) NOT NULL,
       CHECK (F25CIMPETI IN('S','N')),
       CHECK (F25CDEJPIC IN('C','P')),
       CHECK (F25CSERPIC IN('P','S','N', 'G', 'R')),
       CHECK (F25CTIPMOV IN('M','P')),
       CHECK (F25CORDEXT IN('F','L','C','A')),
       CHECK (F25CTIPRES IN('D','L','P')),
       CHECK (F25CCONAUT IN('S','N'))
)
;


ALTER TABLE F25C001
       ADD PRIMARY KEY (F25CCODPRO, F25CCODART)
;


CREATE TABLE F26C001 (
       F26CNUMLST           char(6) NOT NULL,
       F26CCODOPE           char(4) NOT NULL,
       F26CFECCRE           date NOT NULL,
       F26CFPRMOV           date NULL,
       F26CFULMOV           date NULL,
       F26CKGSASG           numeric(12,3) NOT NULL,
       F26CBULASG           numeric(12,3) NOT NULL,
       F26CMOVASG           numeric(6) NOT NULL,
       F26CMACASG           numeric(12,3) NOT NULL,
       F26CVOLASG           numeric(12,3) NOT NULL,
       F26CKGSPTE           numeric(12,3) NOT NULL,
       F26CBULPTE           numeric(12,3) NOT NULL,
       F26CMOVPTE           numeric(6) NOT NULL,
       F26CMACPTE           numeric(12,3) NOT NULL,
       F26CVOLPTE           numeric(12,3) NOT NULL,
       F26CUBIEXP           char(14) NOT NULL,
       F26CESTLST           char(1) NOT NULL,
       F26CFLAG1            char(1) NOT NULL,
       F26CFLAG2            char(1) NOT NULL,
       CHECK (F26CESTLST IN('0','1','2','3'))
)
;


ALTER TABLE F26C001
       ADD PRIMARY KEY (F26CNUMLST)
;

-- Agregar columna nº albarán salida. AVC - 12.06.2007

CREATE TABLE F26L001 (
       F26LNUMMOV           char(10) NOT NULL,
       F26LNUMENT           char(10) NULL,
       F26LTIPMOV           char(4) NOT NULL,
       F26LTIPDOC           char(4) NULL,
       F26LNUMDOC           char(13) NULL,
       F26LLINDOC           numeric(4) NULL,
       F26LFECDOC           date NULL,
       F26LDIRASO           char(13) NULL,
       F26LNUMPED           varchar(15) NULL,
       F26LLINPED           numeric(4) NULL,
       F26LFECMOV           date NOT NULL,
       F26LFECLST           date NOT NULL,
       F26LCODART           char(13) NOT NULL,
       F26LNUMLOT           char(15) NULL,
       F26LSITSTK           char(4) NOT NULL,
       F26LFECCAD           date NULL,
       F26LCANFIS           numeric(12,3) NOT NULL,
       F26LRUTHAB           char(4) NULL,
       F26LUBIORI           char(14) NOT NULL,
       F26LUBIDES           char(14) NULL,
       F26LNUMPAL           char(10) NOT NULL,
       F26LTIPPAL           char(4) NOT NULL,
       F26LUNIVEN           numeric(12,3) NOT NULL,
       F26LUNIPAC           numeric(12,3) NOT NULL,
       F26LPACCAJ           numeric(12,3) NOT NULL,
       F26LCAJPAL           numeric(12,3) NOT NULL,
       F26LFECFAB           date NULL,
       F26LFECCAL           date NULL,
       F26LNUMANA           char(6) NULL,
       F26LCODPRO           char(6) NOT NULL,
       F26LCODOPE           char(4) NOT NULL,
       F26LTIPLST           char(1) NOT NULL,
       F26LNUMLST           char(6) NOT NULL,
       F26LNUMEXP           char(6) NULL,
       F26LORDREC           char(6) NOT NULL,
       F26LESTMOV           char(1) NOT NULL,
       F26LORIRES           char(1) NULL,
       F26LTIPUBI           char(1) NULL,
       F26LTIPMAS           char(4) NULL,
       F26LNUMMAS           char(9) NULL,
       F26LMACUNI           char(1) NULL,
       F26LORDRUT           numeric(3) NULL,
       F26LTENTRE           char(4) NULL,
       F26LCENTRE           char(6) NULL,
       F26LVENHAB           char(6) NULL,
       F26LTIPMAC           char(4) NULL,
       F26LNUMMAC           char(9) NULL,
       F26LSECCIO           char(2) NULL,
       F26LFLAG1            char(1) NULL,
       F26LFLAG2            char(1) NULL,
       F26LIDOCUP           char(10) NOT NULL,
	 F26LNUMALB		    char(13) NOT NULL
)
;

CREATE INDEX GESCOL ON F26L001
(
       F26LNUMLST,
       F26LCODOPE,
       F26LTIPDOC,
       F26LNUMDOC,
       F26LESTMOV
)
;

CREATE INDEX IK_F26LCODUBI ON F26L001
(
       F26LUBIORI,
       F26LCODPRO,
       F26LCODART,
       F26LNUMLOT,
       F26LFECCAD,
       F26LSITSTK,
       F26LNUMPAL
)
;

CREATE INDEX IK_F26LIDOCUP ON F26L001
(
       F26LIDOCUP
)
;


CREATE INDEX IK_F26LNUMDOC ON F26L001
(
       F26LCODPRO,
       F26LTIPDOC,
       F26LNUMDOC
)
;

CREATE INDEX IK_F26LNUMLST ON F26L001
(
       F26LNUMLST
)
;

CREATE INDEX IK_F26LNUMMAC ON F26L001
(
       F26LNUMMAC
)
;

CREATE INDEX SK_ACTLISTA ON F26L001
(
       F26LNUMLST,
       F26LESTMOV
)
;

CREATE INDEX SK_RADIOPETUBICA ON F26L001
(
       F26LCODOPE,
       F26LESTMOV
)
;


ALTER TABLE F26L001
       ADD PRIMARY KEY (F26LNUMMOV)
;


CREATE TABLE F26V001 (
       F26VTIPMAC           char(4) NOT NULL,
       F26VNUMMAC           char(9) NOT NULL,
       F26VNUMBUL           char(4) NOT NULL,
       F26VCODPRO           char(6) NOT NULL,
       F26VTIPDOC           char(4) NOT NULL,
       F26VNUMDOC           char(13) NOT NULL,
       F26VFECCRE           date NOT NULL,
       F26VNUMLIS           char(6) NULL,
       F26VTIPORI           char(1) NOT NULL,
       F26VESTBUL           char(1) NULL,
       F26VFLAG1            char(1) NULL,
       F26VFLAG2            char(1) NULL,
       CHECK (F26VTIPORI IN('S','N','U','G'))
)
;

CREATE INDEX IK_F26VNUMDOC ON F26V001
(
       F26VCODPRO,
       F26VTIPDOC,
       F26VNUMDOC
)
;


ALTER TABLE F26V001
       ADD PRIMARY KEY (F26VNUMMAC)
;


CREATE TABLE F26W001 (
       F26WTIPMAC           char(4) NOT NULL,
       F26WNUMMAC           char(9) NOT NULL,
       F26WNUMBUL           char(4) NOT NULL,
       F26WNMOVMP           char(10) NOT NULL,
       F26WNMOVHM           char(10) NOT NULL,
       F26WESTLIN           char(1) NULL,
       F26WFLAG1            char(1) NULL,
       F26WFLAG2            char(1) NULL
)
;

CREATE INDEX IK_F26WNUMMAC ON F26W001
(
       F26WNUMMAC
)
;


ALTER TABLE F26W001
       ADD PRIMARY KEY (F26WNMOVMP)
;


CREATE TABLE F27C001 (
       F27CCODPRO           char(6) NOT NULL,
       F27CTIPDOC           char(4) NOT NULL,
       F27CNUMDOC           char(13) NOT NULL,
       F27CDIRASO           char(13) NOT NULL,
       F27CNUMALB           char(13) NOT NULL,
       F27CFECALB           date NOT NULL,
       F27CFECEMB           date NULL,
       F27CHORENT           varchar(8) NULL,
       F27CHORSAL           varchar(8) NULL,
       F27CPALCMP           numeric(5) NULL,
       F27CCAJLIB           numeric(5) NULL,
       F27CCAJPIC           numeric(5) NULL,
       F27CPESOKG           float NULL,
       F27CVOLUME           float NULL,
       F27CBULTOS           numeric(5) NULL,
       F27CETIQUE           numeric(5) NULL,
       F27CFECLLE           date NULL,
       F27CNUMTRP           numeric(5) NULL,
       F27CFECTRP           date NULL,
       F27CHORTRP           varchar(8) NULL,
       F27CCONALB           char(1) NULL,
       F27CNUMTRU           numeric(5) NULL,
       F27CFECTRU           date NULL,
       F27CHORTRU           varchar(8) NULL,
       F27CTIPURG           char(1) NULL,
       F27CFLETE            char(1) NULL,
       F27CEXTRAR           char(1) NULL,
       F27CRUTHAB           char(4) NULL,
       F27CCODTRA           char(6) NULL,
       F27CFLGEST           char(1) NOT NULL,
       F27CTIPALB           char(1) NULL
)
;

CREATE INDEX IK_F27CNUMALB ON F27C001
(
       F27CCODPRO,
       F27CNUMALB
)
;


ALTER TABLE F27C001
       ADD PRIMARY KEY (F27CCODPRO, F27CTIPDOC, F27CNUMDOC, 
              F27CNUMALB)
;


CREATE TABLE F27L001 (
       F27LCODPRO           char(6) NOT NULL,
       F27LTIPDOC           char(4) NOT NULL,
       F27LNUMDOC           char(13) NOT NULL,
       F27LDIRASO           char(13) NOT NULL,
       F27LNUMALB           char(13) NOT NULL,
       F27LLINALB           char(4) NOT NULL,
       F27LLINDOC           char(4) NOT NULL,
       F27LCODART           char(13) NOT NULL,
       F27LNUMLOT           char(15) NULL,
       F27LFECCAD           date NULL,
       F27LSITSTK           char(4) NOT NULL,
       F27LCANPED           numeric(12,3) NOT NULL,
       F27LCANSER           numeric(12,3) NOT NULL,
       F27LBULINI           numeric(5) NULL,
       F27LBULFIN           numeric(5) NULL,
       F27LBULPIC           numeric(5) NULL,
       F27LPALETS           numeric(5) NULL,
       F27LPESBRU           float NULL,
       F27LVOLUME           float NULL,
       F27LNUMTRP           numeric(5) NULL,
       F27LFECTRP           date NULL,
       F27LHORTRP           char(8) NULL,
       F27LNUMREL           numeric(5) NULL,
       F27LFECREL           date NULL,
       F27LHORREL           char(8) NULL,
       F27LFLGEST           char(1) NOT NULL
)
;

CREATE INDEX IK_F27LNUMALB ON F27L001
(
       F27LCODPRO,
       F27LNUMALB
)
;

CREATE INDEX IK_F27LNUMDOC ON F27L001
(
       F27LCODPRO,
       F27LTIPDOC,
       F27LNUMDOC,
       F27LLINDOC
)
;

CREATE INDEX TRASPASO ON F27L001
(
       F27LNUMTRP
)
;


ALTER TABLE F27L001
       ADD PRIMARY KEY (F27LCODPRO, F27LTIPDOC, F27LNUMDOC, 
              F27LNUMALB, F27LLINALB)
;


CREATE TABLE F30C001 (
       F30CTIPENT           char(4) NOT NULL,
       F30CCODENT           char(6) NOT NULL,
       F30CTIPALB           char(1) NOT NULL,
       F30CTIPDOC           char(4) NOT NULL,
       F30CALBREP           char(13) NOT NULL,
       F30CNUMDOC           char(13) NOT NULL,
       F30CORIALB           varchar(30) NULL,
       F30CFECALB           date NOT NULL,
       F30CFECEMB           date NULL,
       F30CCONTEN           varchar(10) NULL,
       F30CBARCO            varchar(10) NULL,
       F30CNOMDES           varchar(40) NULL,
       F30CDIRDES           varchar(35) NULL,
       F30CPOBDES           varchar(35) NULL,
       F30CPRVDES           char(4) NULL,
       F30CCODPOS           char(3) NULL,
       F30CPUNOPE           varchar(13) NULL,
       F30CPALCMP           numeric(10) NULL,
       F30CCAJLIB           numeric(10) NULL,
       F30CCAJPIC           numeric(10) NULL,
       F30CBULTOS           numeric(10) NOT NULL,
       F30CPESOKG           float NOT NULL,
       F30CVOLUME           float NOT NULL,
       F30CREMBSN           char(1) NULL,
       F30CPTSREM           numeric(10) NOT NULL,
       F30COBSERV           varchar(40) NULL,
       F30CFECLLE           date NULL,
       F30CCODSIT           char(1) NOT NULL,
       F30CFECENT           date NULL,
       F30CPORTES           char(1) NOT NULL,
       F30CHOJRUT           varchar(10) NULL,
       F30CFLGSEL           char(1) NULL,
       F30CTIPURG           char(1) NULL,
       F30CFLETE            char(1) NOT NULL,
       F30CEXTRAR           char(1) NOT NULL,
       F30CIMPTRA           char(1) NULL,
       F30CFECEMI           date NULL,
       F30CNUMTRP           numeric(5) NOT NULL,
       F30CFECTRP           date NULL,
       F30CHORTRP           varchar(8) NULL,
       F30CFECCFE           date NULL,
       F30CRUTHAB           char(4) NOT NULL,
       F30CCODTRA           char(6) NOT NULL,
       F30CNUMCUE           varchar(12) NULL,
       F30CIMPORT           numeric(10,2) NOT NULL,
       CHECK (F30CTIPALB='T' OR F30CTIPALB='D')
)
;

CREATE INDEX IK_F30CHOJRUT ON F30C001
(
       F30CHOJRUT
)
;


ALTER TABLE F30C001
       ADD PRIMARY KEY (F30CTIPENT, F30CCODENT, F30CTIPALB, 
              F30CTIPDOC, F30CNUMDOC, F30CALBREP)
;



CREATE TABLE F30D001 (
       F30DCODPRO           char(6) NOT NULL,
       F30DCODART           char(13) NOT NULL,
       F30DSITSTK           char(4) NOT NULL,
       F30DFECHA            date NOT NULL,
       F30DTIPPRO           char(4) NOT NULL,
       F30DTOTVOL           float NOT NULL,
       F30DTOTPES           float NOT NULL,
       F30DSTOCK            numeric(10) NULL
)
;

ALTER TABLE F30D001
       ADD PRIMARY KEY (F30DCODPRO, F30DCODART, F30DSITSTK, F30DFECHA)
;


CREATE INDEX IK_F30DFECHA ON F30D001
(
       F30DFECHA,
       F30DCODPRO,
       F30DCODART
)
;



CREATE TABLE F30L001 (
       F30LCODPRO           char(6) NOT NULL,
       F30LCODART           char(13) NOT NULL,
       F30LSITSTK           char(4) NOT NULL,
       F30LNUMPAL           char(10) NOT NULL,
       F30LNUMLOT           char(15) NOT NULL,
       F30LCODUBI           char(14) NOT NULL,
       F30LFECHA            date NOT NULL,
       F30LTAMPAL           char(4) NOT NULL,
       F30LTIPPRO           char(4) NOT NULL,
       F30LTOTVOL           numeric(15,3) NOT NULL,
       F30LTOTPES           numeric(15,3) NOT NULL,
       F30LSTOCK            numeric(10) NULL
)
;

ALTER TABLE F30L001
       ADD PRIMARY KEY (F30LCODPRO, F30LCODART, F30LSITSTK, F30LFECHA, 
              F30LNUMPAL, F30LNUMLOT, F30LCODUBI)
;

CREATE INDEX IK_F30LFECHA ON F30L001
(
       F30LFECHA,
       F30LCODPRO,
       F30LCODART
)
;

CREATE INDEX IK_F30LPALET ON F30L001
(
       F30LCODPRO,
       F30LNUMPAL
)
;


CREATE TABLE F31C001 (
       F31CHOJRUT           char(10) NOT NULL,
       F31CFECRUT           date NOT NULL,
       F31CCODVEH           char(6) NOT NULL,
       F31CCODCHO           char(6) NOT NULL,
       F31CCODTRA           char(6) NOT NULL,
       F31CFECSAL           date NULL,
       F31CHORSAL           varchar(8) NULL,
       F31CFECINI           date NULL,
       F31CFECFIN           date NULL,
       F31CLIQUID           char(1) NULL,
       F31CFECLIQ           date NULL,
       F31CFLGSEL           char(1) NULL,
       F31CPTSLIQ           numeric(10) NOT NULL,
       F31CNUMKMS           numeric(5) NOT NULL,
       F31CALBRUT           numeric(5) NOT NULL,
       F31CPALCMP           numeric(8) NOT NULL,
       F31CCAJLIB           numeric(8) NOT NULL,
       F31CCAJPIC           numeric(8) NOT NULL,
       F31CBULTOS           numeric(8) NOT NULL,
       F31CPESOKG           float NOT NULL,
       F31CVOLUME           float NOT NULL,
       F31CNUMDES           numeric(3) NOT NULL,
       F31CNUMRET           numeric(3) NOT NULL,
       F31CPESRET           float NOT NULL,
       F31CVOLRET           float NOT NULL
)
;


ALTER TABLE F31C001
       ADD PRIMARY KEY (F31CHOJRUT)
;


CREATE TABLE F31L001 (
       F31LHOJRUT           char(10) NOT NULL,
       F31LTIPENT           char(4) NOT NULL,
       F31LCODENT           char(6) NOT NULL,
       F31LTIPALB           char(1) NOT NULL,
       F31LTIPDOC           char(4) NOT NULL,
       F31LALBREP           char(13) NOT NULL,
       F31LCODCNF           char(4) NOT NULL,
       F31LFECCNF           date NULL,
       F31LRUTHAB           char(4) NOT NULL,
       F31LCODTRA           char(6) NOT NULL,
       F31LFIRMSN           char(1) NULL,
       F31LFECENT           date NULL,
       CHECK (F31LTIPALB='T' OR F31LTIPALB='D')
)
;


ALTER TABLE F31L001
       ADD PRIMARY KEY (F31LHOJRUT, F31LTIPENT, F31LCODENT, 
              F31LTIPALB, F31LTIPDOC, F31LALBREP)
;


CREATE TABLE F32C001 (
       F32CCODPRO           char(6) NOT NULL,
       F32CDIRECT           varchar(50) NULL,
       F32CDIAPG1           numeric(2) NOT NULL,
       F32CDIAPG2           numeric(2) NOT NULL,
       F32CDIAPG3           numeric(2) NOT NULL,
       F32CDIA1ER           numeric(3) NOT NULL,
       F32CDIAIEV           numeric(2) NOT NULL,
       F32CFECFRA           char(1) NOT NULL,
       F32CNUMREC           numeric(2) NOT NULL,
       F32CDTODC1           float NOT NULL,
       F32CDTODC2           float NOT NULL,
       F32CDTODPP           float NOT NULL,
       F32CCODFPG           char(4) NOT NULL,
       F32CDESFPG           varchar(30) NULL,
       F32CDOMFPG           varchar(30) NULL,
       F32CPOBFPG           varchar(30) NULL,
       F32CCTABAN           varchar(30) NULL,
       F32CNBANCO           varchar(30) NULL,
       F32CKGSMED           numeric(6) NULL,
       F32CTEMMIN           numeric(4) NULL,
       F32CPALRSV           numeric(3) NULL,
       F32CKILMIN           numeric(3) NULL,
       F32CTIPRSV           char(1) NULL,
       F32CTIPKIL           char(1) NULL,
       F32CUNIFAC           char(3) NOT NULL,
       F32CPREMIN           float NOT NULL,
       F32CPORSEG           float NULL,
       F32CFIJVAR           char(4) NULL,
       CHECK (F32CFECFRA IN('F', 'A'))
)
;


ALTER TABLE F32C001
       ADD PRIMARY KEY (F32CCODPRO)
;


CREATE TABLE F34E001 (
       F34ECODCON           char(4) NOT NULL,
       F34EDESCRI           varchar(40) NULL,
       F34EABREVI           varchar(10) NULL,
       F34EVALREF           float NOT NULL,
       F34ECAMBIO           float NOT NULL,
       F34EFECCAL           date NOT NULL,
       F34EBASE             char(1)
)
;


ALTER TABLE F34E001
       ADD PRIMARY KEY (F34ECODCON)
;


CREATE TABLE F34N001 (
       F34NCODCON           char(4) NOT NULL,
       F34NDESCRI           varchar(40) NULL,
       F34NABREVI           varchar(10) NULL,
       F34NNUMCON           numeric(5) NOT NULL
)
;


ALTER TABLE F34N001
       ADD PRIMARY KEY (F34NCODCON)
;


CREATE TABLE F34P001 (
       F34PCODCON           char(4) NOT NULL,
       F34PDESCRI           varchar(40) NULL,
       F34PABREVI           varchar(10) NULL
)
;


ALTER TABLE F34P001
       ADD PRIMARY KEY (F34PCODCON)
;



CREATE TABLE F34V001 (
       F34VCODCON           char(4) NOT NULL,
       F34VDESCRI           varchar(40) NULL,
       F34VABREVI           varchar(10) NULL,
       F34VVALIVA           float NOT NULL,
       F34VVALEQV           float NOT NULL
)
;


ALTER TABLE F34V001
       ADD PRIMARY KEY (F34VCODCON)
;


CREATE TABLE C35C001 (
       C35CCODPRO           char(6) NOT NULL,
       C35CCODTRM           char(4) NOT NULL,
       C35CDESCRI           varchar(40) NULL,
       C35CABREVI           varchar(10) NULL,
       C35CTIPTRM           char(1) NOT NULL,
       C35CVALINI           float NOT NULL,
       C35CVALFIN           float NOT NULL,
       C35CUNIDAD           char(3) NOT NULL
)
;


ALTER TABLE C35C001
       ADD PRIMARY KEY (C35CCODPRO, C35CCODTRM)
;


CREATE TABLE C36C001 (
       C36CCODCON           char(4) NOT NULL,
       C36CDESCRI           varchar(40) NULL,
       C36CABREVI           varchar(10) NULL,
       C36CORIGEN           char(4)
)
;


ALTER TABLE C36C001
       ADD PRIMARY KEY (C36CCODCON)
;


CREATE TABLE C37C001 (
       C37CCODCON           char(4) NOT NULL,
       C37CDESCRI           varchar(40) NULL,
       C37CABREVI           varchar(10) NULL,
       C37CTRAMSN           char(1)
)
;


ALTER TABLE C37C001
       ADD PRIMARY KEY (C37CCODCON)
;


CREATE TABLE C37L001 (
       C37LCODPRO           char(6) NOT NULL,
       C37LCODTAR           char(4) NOT NULL,
       C37LCODCON           char(4) NOT NULL,
       C37LCODORG           char(4) NOT NULL,
       C37LCODTRM           char(4) NOT NULL,
       C37LAGRORG           char(1) NOT NULL,
       C37LAGRTRM           char(1) NOT NULL,
       C37LPERIOD           char(1) NOT NULL,
       C37LTIPTRM           char(1) NOT NULL,
       C37LROWID            char(10) NOT NULL
)
;


ALTER TABLE C37L001
       ADD PRIMARY KEY (C37LROWID)
;

CREATE INDEX IS_CODPROTAR ON C37L001
(
	C37LCODPRO,
	C37LCODTAR
)
;


CREATE TABLE C38C001 (
       C38CCODCON           char(4) NOT NULL,
       C38CDESCRI           varchar(40) NOT NULL,
       C38CABREVI           varchar(10) NOT NULL,
       C38CDIRECT           char(1) NOT NULL
)
;


ALTER TABLE C38C001
       ADD PRIMARY KEY (C38CCODCON)
;



CREATE TABLE C38D001 (
       C38DCODCON           char(4) NOT NULL,
       C38DDESCRI           varchar(40) NOT NULL,
       C38DABREVI           varchar(10) NOT NULL,
       C38DENTSAL           char(1) NOT NULL,
       C38DTIPCON           char(1) NOT NULL
)
;


ALTER TABLE C38D001
       ADD PRIMARY KEY (C38DCODCON)
;



CREATE TABLE C38E001 (
       C38ECODCON           char(4) NOT NULL,
       C38EDESCRI           varchar(40) NOT NULL,
       C38EABREVI           varchar(10) NOT NULL
)
;


ALTER TABLE C38E001
       ADD PRIMARY KEY (C38ECODCON)
;


CREATE TABLE C38F001 (
       C38FCODCON           char(4) NOT NULL,
       C38FDESCRI           varchar(40) NOT NULL,
       C38FABREVI           varchar(10) NOT NULL
)
;


ALTER TABLE C38F001
       ADD PRIMARY KEY (C38FCODCON)
;




CREATE TABLE F35A001 (
       F35ACODPRO           char(6) NOT NULL,
       F35AFECCAL           date NOT NULL,
       F35ATAMPAL           char(4) NOT NULL,
       F35ACODTRM           char(4) NOT NULL,
       F35ABULTOS           numeric(6) NOT NULL,
       F35APESO             numeric(10,3) NOT NULL,
       F35AVOLUM            numeric(12,3) NOT NULL,
       F35AROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35A001
       ADD PRIMARY KEY (F35AROWCON)
;

CREATE INDEX IS_F35A001 ON F35A001
(
       F35ACODPRO, F35AFECCAL, F35ATAMPAL, F35ACODTRM
)
;


CREATE TABLE F35B001 (
       F35BCODPRO           char(6) NOT NULL,
       F35BFECCAL           date NOT NULL,
       F35BTIPPRO           char(4) NOT NULL,
       F35BCODTRM           char(4) NOT NULL,
       F35BBULTOS           numeric(6) NOT NULL,
       F35BPESO             numeric(10,3) NOT NULL,
       F35BVOLUM            numeric(12,3) NOT NULL,
       F35BROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35B001
       ADD PRIMARY KEY (F35BROWCON)
;

CREATE INDEX IS_F35B001 ON F35B001
(
       F35BCODPRO, F35BFECCAL, F35BTIPPRO, F35BCODTRM
)
;



CREATE TABLE F35C001 (
       F35CCODPRO           char(6) NOT NULL,
       F35CFECCAL           date NOT NULL,
       F35CTIPBUL           char(4) NOT NULL,
       F35CCODTRM           char(4) NOT NULL,
       F35CBULTOS           numeric(6) NOT NULL,
       F35CPESO             numeric(10,3) NOT NULL,
       F35CVOLUM            numeric(12,3) NOT NULL,
       F35CROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35C001
       ADD PRIMARY KEY (F35CROWCON)
;

CREATE INDEX IS_F35C001 ON F35C001
(
       F35CCODPRO, F35CFECCAL, F35CTIPBUL, F35CCODTRM
)
;



CREATE TABLE F35D001 (
       F35DCODPRO           char(6) NOT NULL,
       F35DFECCAL           date NOT NULL,
       F35DNUMPAL           char(10) NOT NULL,
       F35DTAMPAL           char(4) NOT NULL,
       F35DCODTRM           char(4) NOT NULL,
       F35DNUMDIA           numeric(3) NOT NULL,
       F35DPESO             numeric(10,3) NOT NULL,
       F35DVOLUM            numeric(12,3) NOT NULL,
       F35DROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35D001
       ADD PRIMARY KEY (F35DCODPRO, F35DFECCAL, F35DNUMPAL)
;

CREATE INDEX IS_F35D001 ON F35D001
(
       F35DROWCON
)
;


CREATE TABLE F35E001 (
       F35ECODPRO           char(6) NOT NULL,
       F35EFECCAL           date NOT NULL,
       F35ECODIGO           char(4) NOT NULL,
       F35ECODTRM           char(4) NOT NULL,
       F35EBULTOS           numeric(6) NOT NULL,
       F35EPESO             numeric(10,3) NOT NULL,
       F35EVOLUM            numeric(12,3) NOT NULL,
       F35EROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35E001
       ADD PRIMARY KEY (F35ECODPRO, F35EFECCAL, F35ECODIGO)
;

CREATE INDEX IS_F35E001 ON F35E001
(
       F35EROWCON
)
;



CREATE TABLE F35F001 (
       F35FCODPRO           char(6) NOT NULL,
       F35FFECCAL           date NOT NULL,
       F35FTIPPRO           char(4) NOT NULL,
       F35FCODTRM           char(4) NOT NULL,
       F35FBULTOS           numeric(6) NOT NULL,
       F35FPESO             numeric(10,3) NOT NULL,
       F35FVOLUM            numeric(12,3) NOT NULL,
       F35FROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35F001
       ADD PRIMARY KEY (F35FROWCON)
;

CREATE INDEX IS_F35F001 ON F35F001
(

       F35FCODPRO, F35FFECCAL, F35FTIPPRO, F35FCODTRM
)
;

CREATE TABLE F35G001 (
       F35GCODPRO           char(6) NOT NULL,
       F35GFECCAL           date NOT NULL,
       F35GCODIGO           char(4) NOT NULL,
       F35GCODTRM           char(4) NOT NULL,
       F35GCANTID           numeric(12,3) NOT NULL,
       F35GPESO             numeric(10,3) NOT NULL,
       F35GVOLUM            numeric(12,3) NOT NULL,
       F35GROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35G001
       ADD PRIMARY KEY (F35GROWCON)
;

CREATE INDEX IS_F35G001 ON F35G001
(
       F35GCODPRO, F35GFECCAL, F35GCODIGO, F35GCODTRM
)
;



CREATE TABLE F35H001 (
       F35HCODPRO           char(6) NOT NULL,
       F35HFECCAL           date NOT NULL,
       F35HCODIGO           char(4) NOT NULL,
       F35HCODTRM           char(4) NOT NULL,
       F35HCANTID           numeric(12,3) NOT NULL,
       F35HPESO             numeric(10,3) NOT NULL,
       F35HVOLUM            numeric(12,3) NOT NULL,
       F35HROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35H001
       ADD PRIMARY KEY (F35HROWCON)
;

CREATE INDEX IS_F35H001 ON F35H001
(

       F35HCODPRO, F35HFECCAL, F35HCODIGO
)
;


CREATE TABLE F35I001 (
       F35ICODPRO           char(6) NOT NULL,
       F35IFECCAL           date NOT NULL,
       F35ICODIGO           char(4) NOT NULL,
       F35ICODTRM           char(4) NOT NULL,
       F35ICANTID           float NOT NULL,
       F35IPESO             float NOT NULL,
       F35IVOLUM            float NOT NULL,
       F35IROWDET           char(10) NOT NULL,
       F35IROWCON           char(10) NOT NULL
)
;

ALTER TABLE F35I001
       ADD PRIMARY KEY (F35IROWCON, F35IROWDET)
;

CREATE INDEX IS_F35I001 ON F35I001
(

       F35ICODPRO, F35IFECCAL, F35ICODIGO
)
;

CREATE TABLE F36D001 (
       F36DCODPRO           char(6) NOT NULL,			-- Propietario
       F36DFECCAL           date NOT NULL,			-- Fecha cálculo
       F36DNUMDOC           char(13) NOT NULL,			-- Documento (albarán / pedido / ...)
       F36DCODCON           char(4) NOT NULL,			-- Concepto
       F36DDESCRI           char(30) NOT NULL,			-- Descripción
       F36DCANTID           numeric(15, 3)NOT NULL,		-- Cantidad
       F36DPRECIO           numeric(15, 3) NOT NULL,		-- Precio
       F36DESTADO           char(1) NOT NULL,			-- Estado registro
       F36DFACTUR           char(1) NOT NULL,			-- Facturado (S/N)
       F36DROWID            char(10) NOT NULL			-- ID registro
)
;

ALTER TABLE F36D001
       ADD PRIMARY KEY (F36DROWID)
;

CREATE INDEX IS_F36D001 ON F36D001
(

       F36DCODPRO, F36DFECCAL, F36DNUMDOC, F36DCODCON
)
;


CREATE TABLE F36M001 (
       F36MCODPRO           char(6) NOT NULL,
       F36MFECCAL           date NOT NULL,
       F36MCODCON           char(4) NOT NULL,
       F36MDESCRI           char(30) NOT NULL,
       F36MUNIDAD           numeric(15, 3)NOT NULL,
       F36MPRECIO           numeric(15, 3) NOT NULL,
       F36MROWID            char(10) NOT NULL
)
;

ALTER TABLE F36M001
       ADD PRIMARY KEY (F36MROWID)
;

CREATE INDEX IS_F36M001 ON F36M001
(

       F36MCODPRO, F36MFECCAL, F36MCODCON
)
;


CREATE TABLE F36P001 (
       F36PCODPRO           char(6) NOT NULL,
       F36PNUMPAL           char(10) NOT NULL,
       F36PFECENT           date NOT NULL,
       F36PFECSAL           date NOT NULL,
       F36PFECVAL           date NOT NULL,
       F36PTAMPAL           char(4) NOT NULL,
       F36PTOTVOL           numeric(15,3) NOT NULL,
       F36PTOTPES           numeric(15,3) NOT NULL,
       F36PESTADO           char(1) NOT NULL
)
;

ALTER TABLE F36P001
       ADD PRIMARY KEY (F36PCODPRO, F36PNUMPAL)
;

CREATE INDEX IK_F36PFLGEST ON F36P001
(
       F36PESTADO,
       F36PCODPRO
)
;


CREATE TABLE F38C001 (
       F38CCODPRO           char(6) NOT NULL,
       F38CCODTAR           char(4) NOT NULL,
       F38CDESCRI           varchar(40) NULL,
       F38CABREVI           varchar(10) NULL,
       F38CFECDES           date NOT NULL,
       F38CFECHAS           date NOT NULL,
       F38CACTIVA           char(1) NOT NULL
)
;


ALTER TABLE F38C001
       ADD PRIMARY KEY (F38CCODPRO, F38CCODTAR)
;


CREATE TABLE F38L001 (
       F38LCODPRO           char(6) NOT NULL,
       F38LCODTAR           char(4) NOT NULL,
       F38LCODCON           char(4) NOT NULL,
       F38LCODTRM           char(4) NOT NULL,
       F38LCODIGO           char(4) NOT NULL,
       F38LTIPCON           char(4) NOT NULL,
       F38LDESCRI           varchar(40) NULL,
       F38LPRTCNT           char(1) NOT NULL,
       F38LPRTPRC           char(1) NOT NULL,
       F38LPRTIMP           char(1) NOT NULL,
       F38LCODIVA           char(4) NOT NULL,
       F38LROWCAB           char(10) NOT NULL,
       F38LROWID            char(10) NOT NULL
)
;


ALTER TABLE F38L001
       ADD PRIMARY KEY (F38LROWID)
;

CREATE INDEX IS_PLANTILLA ON F38L001
(
	F38LCODPRO,
	F38LCODTAR,
	F38LCODCON,
	F38LCODTRM
)
;


CREATE TABLE F38E001 (
       F38ECODPRO           char(6) NOT NULL,
       F38ECODTAR           char(4) NOT NULL,
       F38ECODCON           char(4) NOT NULL,
       F38ECODTRM           char(4) NOT NULL,
       F38ETIPCAL           char(1) NOT NULL,
       F38EPRECIO           float NOT NULL,
       F38ECANDSD           numeric(12,3) NOT NULL,
       F38ECANHST           numeric(12,3) NOT NULL,
       F38ECANMIN           numeric(12,3) NOT NULL,
       F38ECANMAX           numeric(12,3) NOT NULL,
       F38EIMPMIN           float NOT NULL,
       F38EIMPMAX           float NOT NULL,
       F38EROWDET           char(10) NOT NULL,
       F38EROWID            char(10) NOT NULL
)
;

ALTER TABLE F38E001
       ADD PRIMARY KEY (F38EROWID)
;

CREATE INDEX IS_TARIFA ON F38E001
(
	F38ECODPRO,
	F38ECODTAR,
	F38ECODCON,
	F38ECODTRM
)
;


CREATE TABLE F40C001 (
       F40CTIPENT           char(4) NOT NULL,
       F40CCODIGO           char(6) NOT NULL,
       F40CPATIMP           varchar(40) NULL,
       F40CPATEXP           varchar(40) NULL,
       F40CIMPART           varchar(20) NULL,
       F40CIMPCLI           varchar(20) NULL,
       F40CIMPPRO           varchar(20) NULL,
       F40CIMPECA           varchar(20) NULL,
       F40CIMPEPO           varchar(20) NULL,
       F40CIMPEOB           varchar(20) NULL,
       F40CIMPECL           varchar(20) NULL,
       F40CIMPEST           varchar(20) NULL,
       F40CIMPCOM           varchar(20) NULL,
       F40CIMPCND           varchar(20) NULL,
       F40CEXPDOC           varchar(20) NULL,
       F40CEXPSTK           varchar(20) NULL,
       F40CEXPESD           varchar(20) NULL,
       F40CEXPMVT           varchar(20) NULL,
       F40CEXPDEV           varchar(20) NULL,
       F40CEXPCFA           varchar(20) NULL
)
;


ALTER TABLE F40C001
       ADD PRIMARY KEY (F40CTIPENT, F40CCODIGO)
;


CREATE TABLE F50C001 (
       F50CNUMINV           char(6) NOT NULL,
       F50CCODOPE           char(4) NOT NULL,
       F50CFECCRE           date NOT NULL,
       F50CFECINI           date NULL,
       F50CFECFIN           date NULL,
       F50CUBIINI           char(14) NOT NULL,
       F50CUBIDSD           char(14) NOT NULL,
       F50CNUMUBI           numeric(6) NOT NULL,
       F50CTIPMOV           char(1) NOT NULL,
       F50CESTADO           char(1) NOT NULL,
       F50CPARIDA           char(1) NOT NULL,
       F50CFLAG1            char(1) NOT NULL,
       F50CFLAG2            char(1) NOT NULL,
       CHECK (F50CESTADO IN('0','1','2','3'))
)
;

CREATE INDEX SK_CODOPEESTADO ON F50C001
(
       F50CCODOPE,
       F50CESTADO
)
;

CREATE INDEX SK_COPOPENUMINV ON F50C001
(
       F50CCODOPE,
       F50CNUMINV
)
;


ALTER TABLE F50C001
       ADD PRIMARY KEY (F50CNUMINV)
;


CREATE TABLE F50B001 (
	F50BNUMPAL char (10) NOT NULL,
	F50BCODPRO char (6) NOT NULL,
	F50BCODART char (13) NOT NULL,
	F50BNUMLOT char (15) NOT NULL,
	F50BFECCAD date NOT NULL,
	F50BCANTID numeric(18, 3) NOT NULL,
	F50BUNIPAC numeric(18, 0) NOT NULL,
	F50BPACCAJ numeric(18, 0) NOT NULL,
	F50BCAJPAL numeric(18, 0) NOT NULL,
	F50BFECCRE date NOT NULL,
	F50BFLAG1  char (1) NOT NULL,
	F50BFLAG2  char (2) NOT NULL
)
;

ALTER TABLE F50B001
       ADD PRIMARY KEY (F50BNUMPAL, F50BCODPRO, F50BCODART)
;


CREATE TABLE F50L001 (
       F50LNUMINV           char(6) NOT NULL,
       F50LCODOPE           char(4) NOT NULL,
       F50LCODUBI           char(14) NOT NULL,
       F50LCODPRT           char(6) NOT NULL,
       F50LCODART           char(13) NOT NULL,
       F50LNUMLOT           char(15) NULL,
       F50LNUMPAT           char(10) NOT NULL,
       F50LFECCAT           date NULL,
       F50LSITSTT           char(4) NOT NULL,
       F50LCANFIT           numeric(12,3) NOT NULL,
       F50LCODPRR           char(6) NOT NULL,
       F50LCODARR           char(13) NOT NULL,
       F50LNUMLOR           char(15) NULL,
       F50LNUMPAR           char(10) NOT NULL,
       F50LFECCAR           date NULL,
       F50LSITSTR           char(4) NOT NULL,
       F50LCANFIR           numeric(12,3) NOT NULL,
       F50LROWID            char(10) NOT NULL,
       F50LESTADO           char(1) NOT NULL,
       F50LIDOCUP           char(10) NOT NULL,
       CHECK (F50LESTADO IN('0','2','3','4','5'))
)
;

ALTER TABLE F50L001
       ADD PRIMARY KEY (F50LROWID)
;

CREATE INDEX SK_OPEESTADOINV ON F50L001
(
       F50LCODOPE,
       F50LESTADO,
       F50LNUMINV
)
;

CREATE INDEX IK_F50LIDOCUP ON F50L001
(
       F50LIDOCUP
)
;


CREATE INDEX SK_OPEINVUBIESTADO ON F50L001
(
       F50LCODOPE,
       F50LNUMINV,
       F50LCODUBI,
       F50LESTADO
)
;


CREATE TABLE F60C001 (
       F60CCODPRO           char(6) NOT NULL,
       F60CCODART           char(13) NOT NULL,
       F60CFECMOV           char(6) NOT NULL,
       F60CKGSENT           numeric(12,3) NULL,
       F60CVOLENT           float NULL,
       F60CMVTENT           numeric(7) NULL,
       F60CROTENT           float NULL,
       F60CKGSDEV           numeric(12,3) NULL,
       F60CVOLDEV           float NULL,
       F60CMVTDEV           numeric(7) NULL,
       F60CROTDEV           float NULL,
       F60CKGSSAL           numeric(12,3) NULL,
       F60CVOLSAL           float NULL,
       F60CMVTSAL           numeric(7) NULL,
       F60CROTSAL           float NULL,
       F60CKGSINT           numeric(12,3) NULL,
       F60CVOLINT           float NULL,
       F60CMVTINT           numeric(7) NULL,
       F60CROTINT           float NULL,
       F60CKGSPAL           numeric(12,3) NULL,
       F60CVOLPAL           float NULL,
       F60CMVTPAL           numeric(7) NULL,
       F60CROTPAL           float NULL,
       F60CKGSCAJ           numeric(12,3) NULL,
       F60CVOLCAJ           float NULL,
       F60CMVTCAJ           numeric(7) NULL,
       F60CROTCAJ           float NULL,
       F60CKGSGRP           numeric(12,3) NULL,
       F60CVOLGRP           float NULL,
       F60CMVTGRP           numeric(7) NULL,
       F60CROTGRP           float NULL,
       F60CKGSSGR           numeric(12,3) NULL,
       F60CVOLSGR           float NULL,
       F60CMVTSGR           numeric(7) NULL,
       F60CROTSGR           float NULL,
       F60CKGSUNI           numeric(12,3) NULL,
       F60CVOLUNI           float NULL,
       F60CMVTUNI           numeric(7) NULL,
       F60CROTUNI           float NULL,
       F60CCOEROT           float NULL,
       F60CDOCSAL           numeric(6) NULL,
       F60CDOCENT           numeric(6) NULL,
       F60CCANRES           numeric(12,3) NULL,
       F60CMEDOCU           numeric(5) NULL,
       F60CPEROCU           float NULL,
       F60CFLAG1            char(1) NOT NULL,
       F60CFLAG2            char(1) NOT NULL,
       F60CFLAG3            char(1) NOT NULL
)
;


ALTER TABLE F60C001
       ADD PRIMARY KEY (F60CCODPRO, F60CCODART, F60CFECMOV)
;


CREATE TABLE F61C001 (
       F61CCODPRO           char(6) NOT NULL,
       F61CCODART           char(13) NOT NULL,
       F61CFECMOV           char(8) NOT NULL,
       F61CTIPPRO           char(4) NOT NULL,
       F61CPALENT           numeric(9) NULL,
       F61CCAJENT           numeric(9) NULL,
       F61CGRPENT           numeric(9) NULL,
       F61CSGRENT           numeric(9) NULL,
       F61CUNIENT           numeric(9) NULL,
       F61CPALDEV           numeric(9) NULL,
       F61CCAJDEV           numeric(9) NULL,
       F61CGRPDEV           numeric(9) NULL,
       F61CSGRDEV           numeric(9) NULL,
       F61CUNIDEV           numeric(9) NULL,
       F61CPALSAL           numeric(9) NULL,
       F61CCAJSAL           numeric(9) NULL,
       F61CGRPSAL           numeric(9) NULL,
       F61CSGRSAL           numeric(9) NULL,
       F61CUNISAL           numeric(9) NULL,
       F61CPALINT           numeric(9) NULL,
       F61CCAJINT           numeric(9) NULL,
       F61CGRPINT           numeric(9) NULL,
       F61CSGRINT           numeric(9) NULL,
       F61CUNIINT           numeric(9) NULL,
       F61CNUMOCU           numeric(6) NULL,
       F61CFLAG1            char(1) NOT NULL,
       F61CFLAG2            char(1) NOT NULL,
       F61CFLAG3            char(1) NOT NULL
)
;


ALTER TABLE F61C001
       ADD PRIMARY KEY (F61CCODPRO, F61CCODART, F61CFECMOV)
;


CREATE TABLE F62C001 (
       F62CCODPRO           char(6) NOT NULL,
       F62CCODOPE           char(4) NOT NULL,
       F62CFECMOV           char(6) NOT NULL,
       F62CTIPPRO           char(4) NOT NULL,
       F62CLSTSAL           numeric(5) NULL,
       F62CMVTSAL           numeric(7) NULL,
       F62CKGSSAL           numeric(12,3) NULL,
       F62CVOLSAL           float NULL,
       F62CTOTREP           numeric(5) NULL,
       F62CTIMSAL           numeric(5) NULL,
       F62CMVTENT           numeric(7) NULL,
       F62CMVTDEV           numeric(7) NULL,
       F62CPALENT           numeric(12,3) NULL,
       F62CCAJENT           numeric(12,3) NULL,
       F62CGRPENT           numeric(12,3) NULL,
       F62CUNIENT           numeric(12,3) NULL,
       F62CPALSAL           numeric(12,3) NULL,
       F62CCAJSAL           numeric(12,3) NULL,
       F62CGRPSAL           numeric(12,3) NULL,
       F62CUNISAL           numeric(12,3) NULL,
       F62CPALINT           numeric(12,3) NULL,
       F62CCAJINT           numeric(12,3) NULL,
       F62CGRPINT           numeric(12,3) NULL,
       F62CUNIINT           numeric(12,3) NULL,
       F62CFLAG1            char(1) NOT NULL,
       F62CFLAG2            char(1) NOT NULL,
       F62CFLAG3            char(1) NOT NULL
)
;


ALTER TABLE F62C001
       ADD PRIMARY KEY (F62CCODPRO, F62CCODOPE, F62CFECMOV, F62CTIPPRO)
;


CREATE TABLE F63C001 (
       F63CCODPRO           char(6) NOT NULL,
       F63CPRIORI           numeric(2) NOT NULL,
       F63CFECMOV           char(6) NOT NULL,
       F63CEXPVTA           numeric(5) NULL,
       F63CEXPPRO           numeric(5) NULL,
       F63CEXPAMB           numeric(5) NULL,
       F63CBULVTA           numeric(6) NULL,
       F63CBULPRO           numeric(6) NULL,
       F63CBULAMB           numeric(6) NULL,
       F63CPALAMB           numeric(6) NULL,
       F63CCAJAMB           numeric(6) NULL,
       F63CGRPAMB           numeric(6) NULL,
       F63CUNIAMB           numeric(6) NULL,
       F63CPALFRI           numeric(6) NULL,
       F63CCAJFRI           numeric(6) NULL,
       F63CGRPFRI           numeric(6) NULL,
       F63CUNIFRI           numeric(6) NULL,
       F63CPALPSI           numeric(6) NULL,
       F63CCAJPSI           numeric(6) NULL,
       F63CGRPPSI           numeric(6) NULL,
       F63CUNIPSI           numeric(6) NULL,
       F63CPALVAR           numeric(6) NULL,
       F63CCAJVAR           numeric(6) NULL,
       F63CGRPVAR           numeric(6) NULL,
       F63CUNIVAR           numeric(6) NULL,
       F63CKGSVTA           float NULL,
       F63CKGSPRO           float NULL,
       F63CEURVTA           float NULL,
       F63CEURPRO           float NULL,
       F63CLINVTA           numeric(9) NULL,
       F63CLINPRO           numeric(9) NULL,
       F63CFLAG1            char(1) NOT NULL,
       F63CFLAG2            char(1) NOT NULL,
       F63CFLAG3            char(1) NOT NULL
)
;


ALTER TABLE F63C001
       ADD PRIMARY KEY (F63CCODPRO, F63CPRIORI, F63CFECMOV)
;


CREATE TABLE F64C001 (
       F64CCODALM           char(6) NOT NULL,
       F64CSITSTK           numeric(2) NOT NULL,
       F64CACTIVO           char(1) NOT NULL,
       F64CFLAG1            char(1) NOT NULL,
       F64CFLAG2            char(1) NOT NULL,
       F64CFLAG3            char(1) NOT NULL
)
;


ALTER TABLE F64C001
       ADD PRIMARY KEY (F64CCODALM, F64CSITSTK)
;


CREATE TABLE F70C001 (
       F70CCODPRO           char(6) NOT NULL,
       F70CFECCAL           date NOT NULL,
       F70CNUMFAC           char(10) NOT NULL,
       F70CFECFAC           date NOT NULL,
       F70CFECCIE           date NULL,
       F70CFCHINI           date NULL,
       F70CFCHFIN           date NULL,
       F70CESTADO           char(1) NOT NULL
)
;


ALTER TABLE F70C001
       ADD PRIMARY KEY (F70CNUMFAC)
;

CREATE INDEX PK_F70C001 ON F70C001
(
       F70CCODPRO,
       F70CFECCAL
)
;

CREATE TABLE F70L001 (
       F70LCODPRO           char(6) NOT NULL,
       F70LCODTAR           char(4) NOT NULL,
       F70LNUMFAC           char(10) NOT NULL,
       F70LCODCON           char(4) NOT NULL,
       F70LCODIGO           char(4) NOT NULL,
       F70LCODTRM           char(4) NOT NULL,
       F70LTIPUNI           char(4) NOT NULL,
       F70LDESCRI           char(30) NULL,
       F70LUNICAL           float NOT NULL,
       F70LUNICOR           float NOT NULL,
       F70LPRECAL           float NOT NULL,
       F70LPRECOR           float NOT NULL,
       F70LBASIMP           float NOT NULL,
       F70LCODIVA           char(4) NOT NULL,
       F70LIMPIVA           float NOT NULL,
       F70LIMPEQV           float NOT NULL,
       F70LPRTCNT           char(1) NULL,
       F70LPRTPRC           char(1) NULL,
       F70LPRTIMP           char(1) NULL,
       F70LROWID            char(10) NOT NULL,
       F70LROWDET           char(10) NOT NULL
)
;

ALTER TABLE F70L001
       ADD PRIMARY KEY (F70LROWID)
;

CREATE INDEX IK_F70L001 ON F70L001
(
       F70LCODPRO,
       F70LNUMFAC
)
;

CREATE TABLE F70I001 (
       F70ICODPRO           char(6) NOT NULL,
       F70INUMFAC           char(10) NOT NULL,
       F70IBASIM1           float NOT NULL,
       F70ICODIM1           char(4) NOT NULL,
       F70IVALIV1           float NOT NULL,
       F70ITOTIV1           float NOT NULL,
       F70IVALEQ1           float NOT NULL,
       F70ITOTEQ1           float NOT NULL,
       F70IBASIM2           float NOT NULL,
       F70ICODIM2           char(4) NOT NULL,
       F70IVALIV2           float NOT NULL,
       F70ITOTIV2           float NOT NULL,
       F70IVALEQ2           float NOT NULL,
       F70ITOTEQ2           float NOT NULL,
       F70IBASIM3           float NOT NULL,
       F70ICODIM3           char(4) NOT NULL,
       F70IVALIV3           float NOT NULL,
       F70ITOTIV3           float NOT NULL,
       F70IVALEQ3           float NOT NULL,
       F70ITOTEQ3           float NOT NULL,
       F70IBASIM4           float NOT NULL,
       F70ICODIM4           char(4) NOT NULL,
       F70IVALIV4           float NOT NULL,
       F70ITOTIV4           float NOT NULL,
       F70IVALEQ4           float NOT NULL,
       F70ITOTEQ4           float NOT NULL,
       F70IBASIMR           float NOT NULL,
       F70ITOTIVR           float NOT NULL,
       F70ITOTEQR           float NOT NULL
)
;


ALTER TABLE F70I001
       ADD PRIMARY KEY (F70ICODPRO, F70INUMFAC)
;


CREATE TABLE F70O001 (
       F70OCODPRO           char(6) NOT NULL,
       F70ONUMFAC           char(10) NOT NULL,
       F70OOBSERV           varchar(50) NULL
)
;


ALTER TABLE F70O001
       ADD PRIMARY KEY (F70OCODPRO, F70ONUMFAC)
;


CREATE TABLE F70P001 (
       F70PCODPRO           char(6) NOT NULL,
       F70PNUMFAC           char(10) NOT NULL,
       F70PDIAPG1           numeric(2) NOT NULL,
       F70PDIAPG2           numeric(2) NOT NULL,
       F70PDIAPG3           numeric(2) NOT NULL,
       F70PDIA1ER           numeric(3) NOT NULL,
       F70PDIAIEV           numeric(2) NOT NULL,
       F70PFECFRA           char(1) NOT NULL,
       F70PNUMREC           numeric(2) NOT NULL,
       F70PDTODC1           numeric(5,2) NOT NULL,
       F70PDTODC2           numeric(5,2) NOT NULL,
       F70PDTODPP           numeric(5,2) NOT NULL,
       F70PCODFPG           char(4) NOT NULL,
       F70PDESFPG           varchar(30) NULL,
       F70PDOMFPG           varchar(30) NULL,
       F70PPOBFPG           varchar(30) NULL,
       F70PCTABAN           varchar(30) NULL,
       F70PNBANCO           varchar(30) NULL,
       CHECK (F70PFECFRA IN('F', 'A'))
)
;


ALTER TABLE F70P001
       ADD PRIMARY KEY (F70PCODPRO, F70PNUMFAC)
;


CREATE TABLE F70R001 (
       F70RNUMENT           char(10) NOT NULL,
       F70RTARIFA           char(4) NOT NULL,
       F70RTOTVOL           numeric(9,3) NOT NULL,
       F70RTOTKGS           numeric(9,3) NOT NULL,
       F70RFACVOL           numeric(9,3) NOT NULL,
       F70RFACKGS           numeric(9,3) NOT NULL,
       F70RNUMFAC           varchar(10) NULL,
       F70RANYO             char(4) NULL,
       F70RMES              char(2) NULL,
       F70RLINEA            char(4) NOT NULL
)
;


ALTER TABLE F70R001
       ADD PRIMARY KEY (F70RNUMENT, F70RTARIFA)
;


CREATE TABLE F70S001 (
       F70SNUMENT           char(13) NOT NULL,
       F70STARIFA           char(4) NOT NULL,
       F70STOTVOL           numeric(9,3) NOT NULL,
       F70STOTKGS           numeric(9,3) NOT NULL,
       F70SFACVOL           numeric(9,3) NOT NULL,
       F70SFACKGS           numeric(9,3) NOT NULL,
       F70SNUMFAC           varchar(10) NULL,
       F70SANYO             numeric(4) NULL,
       F70SMES              numeric(2) NULL,
       F70SLINEA            char(4) NOT NULL
)
;


ALTER TABLE F70S001
       ADD PRIMARY KEY (F70SNUMENT, F70STARIFA)
;


-- Listas de carga de camión - Cabecera.
CREATE TABLE F80C001 (
	F80CNUMLST char(6) NOT NULL,			-- Nº de lista de carga
	F80CFECCRE date NOT NULL,			-- Fecha de creación
	F80CFECCNF date NOT NULL,			-- Fecha de confirmación
	F80CAGRTRP char(1) NOT NULL,			-- Agrupar por transportista (S/N)
	F80CIDENTD char(1) NOT NULL,			-- Identificar destino (S/N)
	F80CESTLST char(1) NOT NULL 			-- Estado de la lista
)
;

ALTER TABLE F80C001
       ADD PRIMARY KEY (F80CNUMLST)
;



-- Listas de carga de camión - Detalle.
CREATE TABLE F80L001 (
	F80LNUMLST char(6) NOT NULL,			-- Nº de lista de carga
	F80LCODPRO char(6) NOT NULL,			-- Propietario
	F80LTIPDOC char(4) NOT NULL,			-- Tipo documento
	F80LNUMDOC char(13) NOT NULL,			-- Nº documento
	F80LESTMOV char(1) NOT NULL, 			-- Estado de la línea
	F80LORDREC numeric(6,0) NOT NULL 		-- Orden
)
;

ALTER TABLE F80L001
       ADD PRIMARY KEY (F80LCODPRO, F80LTIPDOC, F80LNUMDOC)
;

CREATE INDEX IK_F80LNUMLST ON F80L001 (F80LNUMLST, F80LCODPRO, F80LTIPDOC, F80LNUMDOC)
;

ALTER TABLE F80L001
	ADD FOREIGN KEY (F80LNUMLST)
	REFERENCES F80C001
	ON DELETE CASCADE
;


CREATE TABLE F91C001 (
	F91CCODCON char(4) NOT NULL ,
	F91CDESCRI varchar(40) NULL ,
	F91CABREVI varchar(10) NULL 
)
;

ALTER TABLE F91C001
       ADD PRIMARY KEY (F91CCODCON)
       
;


CREATE TABLE F91T001 (
	F91TCODCON char(4) NOT NULL ,
	F91TDESCRI varchar(40) NULL ,
	F91TABREVI varchar(10) NULL 
)
;

ALTER TABLE F91T001
       ADD PRIMARY KEY (F91TCODCON)
       
;


CREATE TABLE F99A001 (
       F99ACODNUM           char(4) NOT NULL,
       F99ADESCRI           varchar(40) NULL,
       F99AABREVI           varchar(10) NULL,
       F99AMINVAL           numeric(12) NOT NULL,
       F99AMAXVAL           numeric(12) NOT NULL,
       F99AINCVAL           numeric(12) NOT NULL,
       F99ACURVAL           numeric(12) NOT NULL,
       F99ACYCLE            char(1) NOT NULL,
       F99AORDER            char(1) NOT NULL,
       F99ACACHE            char(1) NOT NULL,
       F99AVALUE            numeric(2) NOT NULL,
       CHECK (F99ACYCLE IN ('Y', 'N')),
       CHECK (F99AORDER IN ('Y', 'N')),
       CHECK (F99ACACHE IN ('D', 'N', 'C')),
       CHECK (F99AORDER IN ('Y', 'N')),
       CHECK (F99ACACHE IN ('D', 'N', 'C')),
       CHECK (F99ACYCLE IN ('Y', 'N'))
)
;


ALTER TABLE F99A001
       ADD PRIMARY KEY (F99ACODNUM)
;


CREATE TABLE F99I001 (
       F99ICODNUM           char(4) NOT NULL,
       F99IDESCRI           varchar(40) NULL,
       F99IABREVI           varchar(10) NULL,
       F99IMINVAL           numeric(12,0) NOT NULL,
       F99IMAXVAL           numeric(12,0) NOT NULL,
       F99IINCVAL           numeric(12,0) NOT NULL,
       F99ICURVAL           numeric(12,0) NOT NULL,
       F99ICYCLE            char(1) NOT NULL
)
;

ALTER TABLE F99I001
       ADD PRIMARY KEY (F99ICODNUM)
;

ALTER TABLE F01C001
       ADD FOREIGN KEY (F01CTIPENT)
                             REFERENCES F00I001
;


ALTER TABLE F01D001
       ADD FOREIGN KEY (F01DCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F01D001
       ADD FOREIGN KEY (F01DCODTRP)
                             REFERENCES F01T001
;


ALTER TABLE F01U001
       ADD FOREIGN KEY (F01UTIPENT, F01UCODENT)
                             REFERENCES F01C001
;


ALTER TABLE F01W001
       ADD FOREIGN KEY (F01WCODIGO)
                             REFERENCES F01P001
;


ALTER TABLE F05M001
       ADD FOREIGN KEY (F05MCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F05M001
       ADD FOREIGN KEY (F05MTIPMOV)
                             REFERENCES F00B001
;


ALTER TABLE F05S001
       ADD FOREIGN KEY (F05SCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F05S001
       ADD FOREIGN KEY (F05SCODSOP)
                             REFERENCES F46C001
;


ALTER TABLE F10C001
       ADD FOREIGN KEY (F10CPICKSN)
                             REFERENCES F10T001
;


ALTER TABLE F10C001
       ADD FOREIGN KEY (F10CTAMUBI)
                             REFERENCES F00M001
;


ALTER TABLE F10C001
       ADD FOREIGN KEY (F10CTIPALM)
                             REFERENCES F00D001
;


ALTER TABLE F05U001
       ADD FOREIGN KEY (F05UCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F05U001
       ADD FOREIGN KEY (F05UUBIDSD)
                             REFERENCES F10C001
;


ALTER TABLE F05U001
       ADD FOREIGN KEY (F05UUBIHST)
                             REFERENCES F10C001
;


ALTER TABLE F08C001
       ADD FOREIGN KEY (F08CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F08C001
       ADD FOREIGN KEY (F08CTIPALM)
                             REFERENCES F00D001
;


ALTER TABLE F08E001
       ADD FOREIGN KEY (F08ECODPRO, F08ECODART)
                             REFERENCES F08C001
;


ALTER TABLE F08G001
       ADD FOREIGN KEY (F08GCODPRO, F08GCODART)
                             REFERENCES F08C001
;


ALTER TABLE F08G001
       ADD FOREIGN KEY (F08GCODUBI)
                             REFERENCES F10C001
;


ALTER TABLE F08L001
       ADD FOREIGN KEY (F08LCODPRO, F08LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F08L001
       ADD FOREIGN KEY (F08LCODPRO, F08LCODCOM)
                             REFERENCES F08C001
;


ALTER TABLE F08S001
       ADD FOREIGN KEY (F08SCODPRO, F08SCODART)
                             REFERENCES F08C001
;


ALTER TABLE F08S001
       ADD FOREIGN KEY (F08SCODSOP)
                             REFERENCES F46C001
;


ALTER TABLE F08S001
       ADD FOREIGN KEY (F08STIPDOC)
                             REFERENCES F00K001
;


ALTER TABLE F08T001
       ADD FOREIGN KEY (F08TCODPRO, F08TCODALT)
                             REFERENCES F08C001
;


ALTER TABLE F08T001
       ADD FOREIGN KEY (F08TCODPRO, F08TCODART)
                             REFERENCES F08C001
;


ALTER TABLE F08U001
       ADD FOREIGN KEY (F08UCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F08U001
       ADD FOREIGN KEY (F08UCODPRO, F08UARTDSD)
                             REFERENCES F08C001
;


ALTER TABLE F08V001
       ADD FOREIGN KEY (F08VCODCOL)
                             REFERENCES F91C001
;

ALTER TABLE F08V001
       ADD FOREIGN KEY (F08VCODTAL)
                             REFERENCES F91T001
;


ALTER TABLE F11R001
       ADD FOREIGN KEY (F11RTAMPAL)
                             REFERENCES F00F001
;


ALTER TABLE F11R001
       ADD FOREIGN KEY (F11RTAMUBI)
                             REFERENCES F00M001
;


ALTER TABLE F11T001
       ADD FOREIGN KEY (F11TTIPPRO)
                             REFERENCES F00E001
;


ALTER TABLE F12C001
       ADD FOREIGN KEY (F12CCODALM)
                             REFERENCES F02C001
;


ALTER TABLE F12C001
       ADD FOREIGN KEY (F12CCODPRO, F12CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F13C001
       ADD FOREIGN KEY (F13CCODALM)
                             REFERENCES F02C001
;


ALTER TABLE F13C001
       ADD FOREIGN KEY (F13CCODPRO, F13CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F13C001
       ADD FOREIGN KEY (F13CSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F14C001
       ADD FOREIGN KEY (F14CCODALM)
                             REFERENCES F02C001
;


ALTER TABLE F14C001
       ADD FOREIGN KEY (F14CCODPRO, F14CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F14C001
       ADD FOREIGN KEY (F14CSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F14C001
       ADD FOREIGN KEY (F14CTIPMOV)
                             REFERENCES F00B001
;


ALTER TABLE F16C001
       ADD FOREIGN KEY (F16CCODPRO, F16CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F16C001
       ADD FOREIGN KEY (F16CCODUBI)
                             REFERENCES F10C001
;


ALTER TABLE F16C001
       ADD FOREIGN KEY (F16CSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F16C001
       ADD FOREIGN KEY (F16CTAMPAL)
                             REFERENCES F00M001
;


ALTER TABLE F18M001
       ADD FOREIGN KEY (F18MCODALM)
                             REFERENCES F02C001
;


ALTER TABLE F18M001
       ADD FOREIGN KEY (F18MCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F18I001
       ADD FOREIGN KEY (F18INUMENT)
                             REFERENCES F18M001
;


ALTER TABLE F18L001
       ADD FOREIGN KEY (F18LCODPRO, F18LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F18L001
       ADD FOREIGN KEY (F18LCODPRO, F18LTIPDOC, F18LNUMDOC)
       REFERENCES F18C001
       ON DELETE CASCADE
;


ALTER TABLE F18L001
       ADD FOREIGN KEY (F18LSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F18N001
       ADD FOREIGN KEY (F18NCODALM)
                             REFERENCES F02C001
;


ALTER TABLE F18N001
       ADD FOREIGN KEY (F18NCODPRO, F18NCODART)
                             REFERENCES F08C001
;


ALTER TABLE F18N001
       ADD FOREIGN KEY (F18NNUMENT)
       REFERENCES F18M001
       ON DELETE CASCADE
;


ALTER TABLE F18O001
       ADD FOREIGN KEY (F18ONUMENT)
                             REFERENCES F18M001
;


ALTER TABLE F19L001
       ADD FOREIGN KEY (F19LCODPRO, F19LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F19L001
       ADD FOREIGN KEY (F19LMOTIVO)
                             REFERENCES F00R001
;


ALTER TABLE F1PD001
       ADD FOREIGN KEY (F1PDCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F1PD001
       ADD FOREIGN KEY (F1PDTIPDOC)
                             REFERENCES F00K001
;


ALTER TABLE F1PF001
       ADD FOREIGN KEY (F1PFCODFAM)
                             REFERENCES F00G001
;


ALTER TABLE F1PF001
       ADD FOREIGN KEY (F1PFCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F1PT001
       ADD FOREIGN KEY (F1PTCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F1PT001
       ADD FOREIGN KEY (F1PTSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F1PT001
       ADD FOREIGN KEY (F1PTTIPMOV)
                             REFERENCES F00B001
;


ALTER TABLE F20C001
       ADD FOREIGN KEY (F20CCODALM)
                             REFERENCES F02C001
;


ALTER TABLE F20C001
       ADD FOREIGN KEY (F20CCODPRO, F20CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F20C001
       ADD FOREIGN KEY (F20CSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F20C001
       ADD FOREIGN KEY (F20CTIPMOV)
                             REFERENCES F00B001
;


ALTER TABLE F22C001
       ADD FOREIGN KEY (F22CTIPENT, F22CCODENT)
                             REFERENCES F01C001
;


ALTER TABLE F22P001
       ADD FOREIGN KEY (F22PCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F24L001
       ADD FOREIGN KEY (F24LCODPRO, F24LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F24L001
       ADD FOREIGN KEY (F24LCODPRO, F24LTIPDOC, F24LNUMDOC)
       REFERENCES F24C001
       ON DELETE CASCADE
;


ALTER TABLE F24O001
       ADD FOREIGN KEY (F24OCODPRO, F24OTIPDOC, F24ONUMDOC)
       REFERENCES F24C001
       ON DELETE CASCADE
;


ALTER TABLE F24T001
       ADD FOREIGN KEY (F24TCODPRO, F24TTIPDOC, F24TNUMDOC)
       REFERENCES F24C001
       ON DELETE CASCADE
;


ALTER TABLE F26L001
       ADD FOREIGN KEY (F26LCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F26L001
       ADD FOREIGN KEY (F26LCODPRO, F26LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F26L001
       ADD FOREIGN KEY (F26LNUMLST)
                             REFERENCES F26C001
;


ALTER TABLE F26L001
       ADD FOREIGN KEY (F26LSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F26L001
       ADD FOREIGN KEY (F26LTIPMOV)
                             REFERENCES F00B001
;


ALTER TABLE F26V001
       ADD FOREIGN KEY (F26VNUMLIS)
                             REFERENCES F26C001
;


ALTER TABLE F26W001
       ADD FOREIGN KEY (F26WNUMMAC)
                             REFERENCES F26V001
;


ALTER TABLE F27C001
       ADD FOREIGN KEY (F27CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F27L001
       ADD FOREIGN KEY (F27LCODPRO, F27LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F27L001
       ADD FOREIGN KEY (F27LCODPRO, F27LTIPDOC, F27LNUMDOC,F27LNUMALB)
       REFERENCES F27C001
       ON DELETE CASCADE
;


ALTER TABLE F30C001
       ADD FOREIGN KEY (F30CCODENT)
                             REFERENCES F01P001
;


ALTER TABLE F30C001
       ADD FOREIGN KEY (F30CCODENT, F30CTIPDOC, F30CNUMDOC,F30CALBREP)
       REFERENCES F27C001
       ON DELETE CASCADE
;


ALTER TABLE F30C001
       ADD FOREIGN KEY (F30CTIPENT)
                             REFERENCES F00I001
;


ALTER TABLE F30D001
       ADD FOREIGN KEY (F30DCODPRO, F30DCODART)
                             REFERENCES F08C001
;


ALTER TABLE F30D001
       ADD FOREIGN KEY (F30DSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F30L001
       ADD FOREIGN KEY (F30LCODPRO, F30LCODART)
                             REFERENCES F08C001
;


ALTER TABLE F30L001
       ADD FOREIGN KEY (F30LSITSTK)
                             REFERENCES F00C001
;


ALTER TABLE F31L001
       ADD FOREIGN KEY (F31LHOJRUT)
                             REFERENCES F31C001
;


ALTER TABLE F32C001
       ADD FOREIGN KEY (F32CCODFPG)
                             REFERENCES F34P001
;


ALTER TABLE F32C001
       ADD FOREIGN KEY (F32CCODPRO)
                             REFERENCES F01P001
;



ALTER TABLE F40C001
       ADD FOREIGN KEY (F40CTIPENT)
                             REFERENCES F00I001
;


ALTER TABLE F50C001
       ADD FOREIGN KEY (F50CCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F50L001
       ADD FOREIGN KEY (F50LCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F50L001
       ADD FOREIGN KEY (F50LNUMINV)
                             REFERENCES F50C001
;


ALTER TABLE F60C001
       ADD FOREIGN KEY (F60CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F60C001
       ADD FOREIGN KEY (F60CCODPRO, F60CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F61C001
       ADD FOREIGN KEY (F61CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F61C001
       ADD FOREIGN KEY (F61CCODPRO, F61CCODART)
                             REFERENCES F08C001
;


ALTER TABLE F62C001
       ADD FOREIGN KEY (F62CCODOPE)
                             REFERENCES F05C001
;


ALTER TABLE F62C001
       ADD FOREIGN KEY (F62CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F63C001
       ADD FOREIGN KEY (F63CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F70C001
       ADD FOREIGN KEY (F70CCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F70I001
       ADD FOREIGN KEY (F70ICODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F70I001
       ADD FOREIGN KEY (F70INUMFAC)
                             REFERENCES F70C001
;


ALTER TABLE F70L001
       ADD FOREIGN KEY (F70LCODCON)
                             REFERENCES F34N001
;


ALTER TABLE F70L001
       ADD FOREIGN KEY (F70LCODIVA)
                             REFERENCES F34V001
;


ALTER TABLE F70L001
       ADD FOREIGN KEY (F70LCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F70L001
       ADD FOREIGN KEY (F70LNUMFAC)
                             REFERENCES F70C001
;

ALTER TABLE F70O001
       ADD FOREIGN KEY (F70OCODPRO)
                             REFERENCES F01P001
;

ALTER TABLE F70O001
       ADD FOREIGN KEY (F70ONUMFAC)
                             REFERENCES F70C001
;


ALTER TABLE F70P001
       ADD FOREIGN KEY (F70PCODPRO)
                             REFERENCES F01P001
;


ALTER TABLE F70P001
       ADD FOREIGN KEY (F70PNUMFAC)
                             REFERENCES F70C001
;


ALTER TABLE F70R001
       ADD FOREIGN KEY (F70RNUMENT)
                             REFERENCES F18M001
;



ALTER TABLE F38C001
       ADD FOREIGN KEY (F38CCODPRO)
                             REFERENCES F01P001
;

ALTER TABLE C35C001
       ADD FOREIGN KEY (C35CCODPRO)
                             REFERENCES F01P001
;

ALTER TABLE F38L001
       ADD FOREIGN KEY (F38LCODPRO)
                             REFERENCES F01P001
;

ALTER TABLE F38L001
       ADD FOREIGN KEY (F38LCODPRO,F38LCODTAR)
                             REFERENCES F38C001
;

ALTER TABLE F38L001
       ADD FOREIGN KEY (F38LCODCON)
                             REFERENCES C36C001
;

ALTER TABLE F38L001
       ADD FOREIGN KEY (F38LTIPCON)
                             REFERENCES F34N001
;

ALTER TABLE F38L001
       ADD FOREIGN KEY (F38LCODIVA)
                             REFERENCES F34V001
;


ALTER TABLE F38L001
       ADD FOREIGN KEY (F38LROWCAB)
                             REFERENCES C37L001
;


ALTER TABLE F38E001
       ADD FOREIGN KEY (F38ECODPRO)
                             REFERENCES F01P001
;

ALTER TABLE F38E001
       ADD FOREIGN KEY (F38ECODPRO,F38ECODTAR)
                             REFERENCES F38C001
;

ALTER TABLE F38E001
       ADD FOREIGN KEY (F38EROWDET)
                             REFERENCES F38L001
;


ALTER TABLE C36C001
       ADD FOREIGN KEY (C36CORIGEN)
                             REFERENCES C37C001
;

ALTER TABLE C37L001
       ADD FOREIGN KEY (C37LCODPRO, C37LCODTAR)
                             REFERENCES F38C001
;


