#INCLUDE SIVIGILA.H
#INCLUDE CLS_UPGD.H

DEFINE CLASS UPGD as Custom

sSourceTableName = 'UPGD'
sDefaultKey = 'COD_PRE+COD_SUB'

sCOD_PRESTADOR = .NULL.
dFEC_CAR = .NULL.
sCOD_PRE = .NULL.
sCOD_SUB = .NULL.
sRAZ_SOC = .NULL.
sDIR = .NULL.
sREP_LEG = .NULL.
sCOR_ELE = .NULL.
sRES_NOT = .NULL.
sTEL = .NULL.
dFEC_CON = .NULL.
nNAT_JUR = .NULL.
nNIV = .NULL.
nNOT_ACT = .NULL.
nUNI_ANA = .NULL.
nCOV = .NULL.
nTAL_HUM = .NULL.
nTEC_DIS = .NULL.
nCOM = .NULL.
nFAX_MOD = .NULL.
nTIE_COR = .NULL.
nINT = .NULL.
nTEL_FAX = .NULL.
nRAD_TEL = .NULL.
nACT_SIV = .NULL.
sNIT_UPGD = .NULL.
sES_UNI_NOT = .NULL.
sESTADOUPGD = .NULL.
sLOC_O_ZONA = .NULL.
dFEC_INICAR = .NULL.
sNOTIF_IAAS = .NULL.
sHOSP_UNIVE = .NULL.
sREG_EXCEPC = .NULL.
sTOT_CAMAS = .NULL.
sNOTIF_IAD = .NULL. 
sNOTIF_ISO = .NULL. 
sNOTIF_CAB = .NULL. 

sUCI_A = .NULL.
*Indica si la UPGD tiene UCI A (1=Sí 2=No)

sUCI_P = .NULL.
*Indica si la UPGD tiene UCI P (1=Sí 2=No)

sUCI_N = .NULL.
*Indica si la UPGD tiene UCI N (1=Sí 2=No)

nUCI_A = .NULL.
*Número de unidades del tipo UCI A que tiene la UPGD

nCAMAS_UCI_A = .NULL.
*Número de camas que tiene la UPGD en las unidades UCI A

nUCI_P = .NULL.
*Número de unidades del tipo UCI P que tiene la UPGD

nCAMAS_UCI_P = .NULL.
*Número de camas que tiene la UPGD en las unidades UCI P

nUCI_N = .NULL.
*Número de unidades del tipo UCI N que tiene la UPGD

nCAMAS_UCI_N = .NULL.
*Número de camas que tiene la UPGD en las unidades UCI N

nCAMAS_UCI = .NULL.
*Número de camas que tiene la UPGD en todas las UCI

sCOMITE_INF = .NULL.
sBI_PROFESI = .NULL.
sIAAS_ULTIM = .NULL.
sINF_TENDEN = .NULL.
sSOCIAL_TEN = .NULL.
sLAB_MICROB = .NULL.
sLAB_PROPIO = .NULL.
sLABS_CONTR = .NULL.
sIDENT_GYE = .NULL.
sPRUE_SUCEP = .NULL.
sLAB_AUTOMA = .NULL.
sVITEK = .NULL.
sMICROSCAN = .NULL.
sPHOENIX = .NULL.
sLAB_CCI = .NULL.
sLAB_CCE = .NULL.
sMICR_CDI = .NULL.
sWHONET = .NULL.
sINFORM_PAT = .NULL.
sLAB_CON_PE = .NULL.
sLAB_REM_CE = .NULL.
sLAB_REPORT = .NULL.
sVIGILA_CAB = .NULL.
sPSF_PARCAB = .NULL.
sMED_PARCAB = .NULL.
sINF_ANTIOB = .NULL.
sQUIEN_VCAB = .NULL.
sSER_CIRGEN = .NULL.
sSER_GINECO = .NULL.
sSER_CIRCAR = .NULL.
sSER_CESARE = .NULL.
sSER_PAR_VA = .NULL.
sSER_COLECI = .NULL.
sSER_HERNIO = .NULL.
sSER_REVASC = .NULL.

SaveResult = .NULL.
*Resultado obtenido al guardar un registro de UPGD

FUNCTION Init(sUPGDCode as String)
	
	LOCAL sSQLCmd as String
	LOCAL nOldWorkArea as Number
	
	nOldWorkArea = SELECT()
	
	sUPGDCode = PADR(SUBSTR(sUPGDCode,1,10),10,' ') + PADR(SUBSTR(sUPGDCode,11,2),2,' ')
	sSQLCmd = 'SELECT * FROM ' + This.sSourceTableName + ' WHERE COD_PRE == "' + LEFT(sUPGDCode,10) + '" AND COD_SUB == "' + RIGHT(sUPGDCode,2) + '" INTO CURSOR rsUPGDDescription'
	&sSQLCmd
	IF _TALLY > 0 THEN
		WITH THIS
			.dFEC_CAR = rsUPGDDescription.FEC_CAR
			
			.sCOD_PRE = rsUPGDDescription.COD_PRE
			.sCOD_SUB = rsUPGDDescription.COD_SUB 
			.sCOD_PRESTADOR = .sCOD_PRE + .sCOD_SUB 
			
			.sRAZ_SOC = UPPER(ALLTRIM(rsUPGDDescription.RAZ_SOC))
			
			.sDIR = rsUPGDDescription.DIR 
			.sREP_LEG = rsUPGDDescription.REP_LEG 
			.sCOR_ELE = rsUPGDDescription.COR_ELE 
			.sRES_NOT = rsUPGDDescription.RES_NOT 
			.sTEL = rsUPGDDescription.TEL 
			.dFEC_CON = rsUPGDDescription.FEC_CON 
			.nNAT_JUR = rsUPGDDescription.NAT_JUR 
			.nNIV = rsUPGDDescription.NIV 
			.nNOT_ACT = rsUPGDDescription.NOT_ACT 
			.nUNI_ANA = rsUPGDDescription.UNI_ANA 
			.nCOV = rsUPGDDescription.COV 
			.nTAL_HUM = rsUPGDDescription.TAL_HUM 
			.nTEC_DIS = rsUPGDDescription.TEC_DIS 
			.nCOM = rsUPGDDescription.COM 
			.nFAX_MOD = rsUPGDDescription.FAX_MOD 
			.nTIE_COR = rsUPGDDescription.TIE_COR 
			.nINT = rsUPGDDescription.INT 
			.nTEL_FAX = rsUPGDDescription.TEL_FAX 
			.nRAD_TEL = rsUPGDDescription.RAD_TEL 
			.nACT_SIV = rsUPGDDescription.ACT_SIV 
			.sNIT_UPGD = rsUPGDDescription.NIT_UPGD 
			.sES_UNI_NOT = rsUPGDDescription.ES_UNI_NOT 
			.sESTADOUPGD = rsUPGDDescription.ESTADOUPGD 
			.sLOC_O_ZONA = rsUPGDDescription.LOC_O_ZONA 
			.dFEC_INICAR = rsUPGDDescription.FEC_INICAR 
			.sNOTIF_IAAS = rsUPGDDescription.NOTIF_IAAS 
			.sNOTIF_IAD = rsUPGDDescription.NOTIF_IAD
			.sNOTIF_ISO = rsUPGDDescription.NOTIF_ISO
			.sNOTIF_CAB = rsUPGDDescription.NOTIF_CAB
			.sHOSP_UNIVE = rsUPGDDescription.HOSP_UNIVE 
			.sREG_EXCEPC = rsUPGDDescription.REG_EXCEPC 
			.sTOT_CAMAS = rsUPGDDescription.TOT_CAMAS 
			.sCOMITE_INF = rsUPGDDescription.COMITE_INF 
			.sBI_PROFESI = rsUPGDDescription.BI_PROFESI 
			.sIAAS_ULTIM = rsUPGDDescription.IAAS_ULTIM 
			.sINF_TENDEN = rsUPGDDescription.INF_TENDEN 
			.sSOCIAL_TEN = rsUPGDDescription.SOCIAL_TEN 
			.sLAB_MICROB = rsUPGDDescription.LAB_MICROB 
			.sLAB_PROPIO = rsUPGDDescription.LAB_PROPIO 
			.sLABS_CONTR = rsUPGDDescription.LABS_CONTR 
			.sIDENT_GYE = rsUPGDDescription.IDENT_GYE 
			.sPRUE_SUCEP = rsUPGDDescription.PRUE_SUCEP 
			.sLAB_AUTOMA = rsUPGDDescription.LAB_AUTOMA 
			.sVITEK = rsUPGDDescription.VITEK 
			.sMICROSCAN = rsUPGDDescription.MICROSCAN 
			.sPHOENIX = rsUPGDDescription.PHOENIX 
			.sLAB_CCI = rsUPGDDescription.LAB_CCI 
			.sLAB_CCE = rsUPGDDescription.LAB_CCE 
			.sMICR_CDI = rsUPGDDescription.MICR_CDI 
			.sWHONET = rsUPGDDescription.WHONET 
			.sINFORM_PAT = rsUPGDDescription.INFORM_PAT 
			.sLAB_CON_PE = rsUPGDDescription.LAB_CON_PE 
			.sLAB_REM_CE = rsUPGDDescription.LAB_REM_CE 
			.sLAB_REPORT = rsUPGDDescription.LAB_REPORT 
			.sVIGILA_CAB = rsUPGDDescription.VIGILA_CAB 
			.sPSF_PARCAB = rsUPGDDescription.PSF_PARCAB 
			.sMED_PARCAB = rsUPGDDescription.MED_PARCAB 
			.sINF_ANTIOB = rsUPGDDescription.INF_ANTIOB 
			.sSER_CIRCAR = rsUPGDDescription.SER_CIRCAR
			.sSER_CESARE = rsUPGDDescription.SER_CESARE
			.sSER_PAR_VA = rsUPGDDescription.SER_PAR_VA
			.sSER_COLECI = rsUPGDDescription.SER_COLECI
			.sSER_HERNIO = rsUPGDDescription.SER_HERNIO
			.sSER_REVASC = rsUPGDDescription.SER_REVASC

			*Establece si la UPGD tiene o no UCI exclusiva adulto, pediátrica y/o neonatal
			.sUCI_A = '2'
			.sUCI_N = '2'
			.sUCI_P = '2'
			sSQLCmd = "SELECT DISTINCT TIPO_UCI FROM UPGD_UCIS WHERE COD_PRE=='" + LEFT(sUPGDCode,10)  + "' AND COD_SUB=='" +;
					 RIGHT(sUPGDCode,2) + "' AND ACTIVA='1' INTO CURSOR rsUCIs"
			&sSQLCmd 
			SCAN
				DO CASE
					CASE rsUCIs.TIPO_UCI='A' 
						.sUCI_A = '1'
					CASE rsUCIs.TIPO_UCI='P' 
						.sUCI_P = '1'
					CASE rsUCIs.TIPO_UCI='N' 
						.sUCI_N = '1'
					OTHERWISE
				ENDCASE
			ENDSCAN			


			*Establece cuántas UCIs tiene la UPGD y de qué tipo son
			sSQLCmd = "SELECT TIPO_UCI, COMP_ESPAC, COUNT(*) as n, SUM(TOT_UCI) as s FROM upgd_ucis WHERE COD_PRE=='" + LEFT(sUPGDCode,10)  + "' AND COD_SUB=='" +;
					 	RIGHT(sUPGDCode,2) + "'  AND ACTIVA='1' GROUP BY TIPO_UCI, COMP_ESPAC INTO CURSOR rsUCIs"
			&sSQLCmd 
			SCAN
				DO CASE
					CASE rsUCIs.TIPO_UCI='A' 
						.nUCI_A = rsUCIs.N
						.nCAMAS_UCI_A = rsUCIs.S
					CASE rsUCIs.TIPO_UCI='P' 
						.nUCI_P = rsUCIs.N
						.nCAMAS_UCI_P = rsUCIs.S
					CASE rsUCIs.TIPO_UCI='N' 
						.nUCI_N = rsUCIs.N
						.nCAMAS_UCI_N = rsUCIs.S
					OTHERWISE
				ENDCASE
			ENDSCAN			

			*Establece el total de camas en UCIs que tiene la UPGD
			sSQLCmd = "SELECT SUM(TOT_UCI) as s FROM upgd_ucis WHERE COD_PRE=='" + LEFT(sUPGDCode,10)  + "' AND COD_SUB=='" +;
					 	RIGHT(sUPGDCode,2) + "' AND ACTIVA='1' INTO CURSOR rsUCIs"
			&sSQLCmd 
			.nCAMAS_UCI = rsUCIs.S

			.sQUIEN_VCAB = rsUPGDDescription.QUIEN_VCAB
			.sSER_CIRGEN = rsUPGDDescription.SER_CIRGEN
			.sSER_GINECO = rsUPGDDescription.SER_GINECO
			.sSER_CIRCAR = rsUPGDDescription.SER_CIRCAR
		ENDWITH
	ELSE
		WITH THIS
			.sCOD_PRESTADOR = ''
			.dFEC_CAR = .NULL.
			.sCOD_PRE = .NULL.
			.sCOD_SUB = .NULL.
			.sRAZ_SOC = .NULL.
			.sDIR = .NULL.
			.sREP_LEG = .NULL.
			.sCOR_ELE = .NULL.
			.sRES_NOT = .NULL.
			.sTEL = .NULL.
			.dFEC_CON = .NULL.
			.nNAT_JUR = -1
			.nNIV = -1
			.sNOTIF_IAD = .NULL. 
			.sNOTIF_ISO = .NULL. 
			.sNOTIF_CAB = .NULL. 
			.nNOT_ACT = -1
			.nUNI_ANA = -1
			.nCOV = -1
			.nTAL_HUM = -1
			.nTEC_DIS = -1
			.nCOM = -1
			.nFAX_MOD = -1
			.nTIE_COR = -1
			.nINT = -1
			.nTEL_FAX = -1
			.nRAD_TEL = -1
			.nACT_SIV = .NULL.
			.sNIT_UPGD = .NULL.
			.sES_UNI_NOT = .NULL.
			.sESTADOUPGD = .NULL.
			.sLOC_O_ZONA = .NULL.
			.dFEC_INICAR = .NULL.
			.sNOTIF_IAAS = .NULL.
			.sHOSP_UNIVE = .NULL.
			.sREG_EXCEPC = .NULL.
			.sTOT_CAMAS = .NULL.
			.sUCI_A = .NULL.
			.sUCI_P = .NULL.
			.sUCI_N = .NULL.
			.nUCI_A = .NULL.
			.nCAMAS_UCI_A = .NULL.
			.nUCI_P = .NULL.
			.nCAMAS_UCI_P = .NULL.
			.nUCI_N = .NULL.
			.nCAMAS_UCI_N = .NULL.
			.nCAMAS_UCI = .NULL.
			.sCOMITE_INF = .NULL.
			.sBI_PROFESI = .NULL.
			.sIAAS_ULTIM = .NULL.
			.sINF_TENDEN = .NULL.
			.sSOCIAL_TEN = .NULL.
			.sLAB_MICROB = .NULL.
			.sLAB_PROPIO = .NULL.
			.sLABS_CONTR = .NULL.
			.sIDENT_GYE = .NULL.
			.sPRUE_SUCEP = .NULL.
			.sLAB_AUTOMA = .NULL.
			.sVITEK = .NULL.
			.sMICROSCAN = .NULL.
			.sPHOENIX = .NULL.
			.sLAB_CCI = .NULL.
			.sLAB_CCE = .NULL.
			.sMICR_CDI = .NULL.
			.sWHONET = .NULL.
			.sINFORM_PAT = .NULL.
			.sLAB_CON_PE = .NULL.
			.sLAB_REM_CE = .NULL.
			.sLAB_REPORT = .NULL.
			.sVIGILA_CAB = .NULL.
			.sPSF_PARCAB = .NULL.
			.sMED_PARCAB = .NULL.
			.sINF_ANTIOB = .NULL.
			.sQUIEN_VCAB = .NULL.
			.sSER_CIRGEN = .NULL.
			.sSER_GINECO = .NULL.
			.sSER_CIRCAR = .NULL.
			.sSER_CESARE = .NULL.
			.sSER_PAR_VA = .NULL.
			.sSER_COLECI = .NULL.
			.sSER_HERNIO = .NULL.
			.sSER_REVASC = .NULL.
		ENDWITH
	ENDIF
	SELECT (nOldWorkArea) 
ENDFUNC


FUNCTION Destroy
ENDFUNC

PROCEDURE Refresh(sUPGDCode as String)

	This.Init(sUPGDCode )
	
ENDPROC


FUNCTION IsActive()
	RETURN (This.nACT_SIV = 1)
ENDFUNC


FUNCTION informIAAS()
	RETURN (This.sNOTIF_IAAS = '1' AND This.nNIV>=2) &&Una UPGD informa IAAS si notifica IAAS y es de nivel 2 o superior
ENDFUNC

FUNCTION hasUCI(sUCIType as String)

	sUCIType = ALLTRIM(sUCIType)
	DO CASE
		CASE sUCIType='A' 
			RETURN (This.sUCI_A = '1')
		CASE sUCIType='P' 
			RETURN (This.sUCI_P = '1')
		CASE sUCIType='N' 
			RETURN (This.sUCI_N = '1')
	ENDCASE
	
ENDFUNC

FUNCTION hasAnyUCI()

	RETURN This.hasUCI('A') OR This.hasUCI('P') OR This.hasUCI('N') 
	
ENDFUNC

FUNCTION hasGeneralSurgery()
	RETURN (This.sSER_CIRGEN = '1')
ENDFUNC

FUNCTION hasGynecology()
	RETURN (This.sSER_GINECO = '1')
ENDFUNC

FUNCTION hasHeartSurgery()
	RETURN (This.sSER_CIRCAR = '1')
ENDFUNC

FUNCTION hasCaesarean()
	RETURN (This.sSER_CESARE= '1')
ENDFUNC

FUNCTION hasBirthing()
	RETURN (This.sSER_PAR_VA= '1')
ENDFUNC

FUNCTION hasCholecystectomy()
	RETURN (This.sSER_COLECI= '1')
ENDFUNC

FUNCTION hasHerniorrhaphy()
	RETURN (This.sSER_HERNIO= '1')
ENDFUNC

FUNCTION hasRevascularization()
	RETURN (This.sSER_REVASC= '1')
ENDFUNC

FUNCTION informIAD()
	RETURN (This.sNOTIF_IAD = '1' AND This.nNIV>=2) &&Una UPGD informa IAD si notifica IAD y es de nivel 2 o superior
ENDFUNC

FUNCTION informCAB()
	RETURN (This.sNOTIF_CAB = '1')
ENDFUNC

FUNCTION informISO()
	RETURN (This.sNOTIF_ISO = '1') 
ENDFUNC

PROCEDURE Save(bNotIsNew AS Byte)
	
	*Guarda, a partir de variables de memoria, un registro de UPGD en la tabla This.sSourceTableName y retorna ;
	en This.SaveResult uno de los siguientes valores: ;
	NEW_BD_INDEX_RECORD_INSERTED = 0 ;
	NEW_BD_ADJUSTMENT_RECORD_INSERTED = 1 ;
	NEW_BD_NO_RECORD_INSERTED = -1

	SET PROCEDURE TO SIVIGILAMESSENGER ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE

	LOCAL sBDTableName as string
	sBDTableName = This.sSourceTableName
	
	STORE .T. TO bContinue
	
	nReturnedValue=.F.
	sResultMsg=''
	nCurrentWorkArea=SELECT(0)
	IF SelectTable(sBDTableName) THEN
			
		IF bNotIsNew = 0 THEN
			*Se debe agregar un nuevo registro o ficha   
	
			*Libera la variable que da cuenta del estado de notificación del registro para que el valor del campo correspondiente sea asignado por Default
			RELEASE m.EST_NOTIF
		
			INSERT INTO &sBDTableName FROM MEMVAR 
			IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
				goErrorlog.resetErrorLog()
				This.SaveResult = NO_UPGD_RECORD_INSERTED 
				bContinue = .F.
				RETURN 0
			ELSE
				This.SaveResult = NEW_UPGD_RECORD_INSERTED 
			ENDIF
		ELSE
			GATHER MEMVAR
			IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
				goErrorlog.resetErrorLog()
				This.SaveResult = UPGD_RECORD_UPDATE_ERROR
				bContinue = .F.
			ELSE
				This.SaveResult = UPGD_RECORD_UPDATED
			ENDIF
		ENDIF

		IF bContinue THEN
			REPLACE NOTIFICA WITH 0
		ENDIF
	ENDIF
	SELECT (nCurrentWorkArea)

ENDPROC 

ENDDEFINE &&CLASS UPGD

