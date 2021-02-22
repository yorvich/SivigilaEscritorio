#INCLUDE SIVIGILA.H

#DEFINE DATE_FREE_EVENTS ",357,351,352,"
*Eventos para los cuales no aplican reglas de validación para las fechas de incio de síntomas, hospitalización y/o consulta;
en la captura de datos básicos


DEFINE CLASS Event as Custom

sSourceTableName = 'EVENTOS'
sDefaultKey = 'COD_EVE'

HIDDEN  sEventsWithoutBdpluscd
sEventsWithoutBdpluscd = ',650,652,680,860,870,890,995,353,354,359,362,349,994,'
*Los eventos correspondientes a estos códigos no obedecen el esquema de datos básicos + datos complementarios

HIDDEN sAlertsOnBasicDataEvents
sAlertsOnBasicDataEvents = ',607,'
*Los eventos correspondientes a estos códigos dan lugar a la producción de una alerta de ocurrencia disparable ;
tan pronto se disponga de los datos básicos de notificación

HIDDEN sNeedsContrastEvents
sNeedsContrastEvents = ',453,'
*Los eventos correspondientes a estos códigos necesitan disparar un regla de validación al momento de Guardar;
y tal regla involucra variables de datos básicos con datos complementarios

HIDDEN sNeedsTokenEvents
sNeedsTokenEvents = ',607,'
*Los eventos correspondientes a estos códigos necesitan tener un token asignado por el INS para que el usuario pueda ;
diligenciar datos complementarios

HIDDEN sNeedsComplementaryData
sNeedsComplementaryData =  ',357,'
*Los eventos correspondientes a estos códigos necesitan tener diligenciados sus datos complementarios antes de poder ;
registrar resultados de laboratorios

HIDDEN sNotCompulsoryLabsEvents
sNotCompulsoryLabsEvents = ',345,346,348,'
*Los eventos correspondientes a estos códigos pueden violar la regla que obliga a tener diligenciados los resultados de laboratorio ;
cuando han sido confirmados por laboratorio

HIDDEN sNeedsComplementaryData2
sNeedsComplementaryData2 =  ',342,'
*Los eventos correspondientes a estos códigos necesitan tener diligenciados sus datos complementarios cuando se va a hacer una;
notificación inmediata de ellos


sCOD_EVE = .NULL.
sNOM_EVE = .NULL.
sINMEDIATA = .NULL.
nTIP_NOT = .NULL.
sFORMULARIO = .NULL.
nVERSION = .NULL.
sCLA_PER = .NULL.
sGRU_PER = .NULL.
nEXA_LAB = .NULL.
nCON_FINAL = .NULL.
sVAL_SEM = .NULL.
nFLU_NOT = .NULL.
nEVE_NAL = .NULL.
dFEC_OBLIGA = .NULL.
bINICIO_SIN = .NULL.
sESTADO_EVE = .NULL.
bACTIVO = .NULL.
bADMITE_NI = .NULL.
nLATENCIA_DC = .NULL.
sAJUSTES_NA = .NULL.
sEDAD_MAX = .NULL.
sUNIDAD_MED = .NULL.
sEDAD_MIN = .NULL.
sUNIMED_MIN = .NULL.
bLAB_INCRUS = .NULL.
sDC_FORM = .NULL.
sDC_TABLA = .NULL.
bSUPER_FAST = .NULL.
sCONDICION_SUPER_FAST = .NULL.
nDIAS_AJUSTE = .NULL.
sCLASIF_INICIAL = .NULL.
sACTUALIZABLES_POR_CONFIRMACION = .NULL.
sMANEJA_CONTACTOS = .NULL.

nFORMULARIO = .NULL.
*Número del "formulario de captura" de datos complementarios

bHasComplementaryData = .F.
*Indica si el evento maneja datos complementarios asociados a los datos básicos

bCatchLabs = .T.
*Indica si el evento captura datos de laboratorio

bIsIndividual = .T.
*Indica si el evento es de caracter individual

bIsDefined = .T.
*Inica que el evento está definido en la tabla sSourceTableName

nAvailableClasifications = .NULL.
*Número de clasificaciones permitidas para el evento

bNotBDPlusCDEvent =  .NULL.
*Indica si el evento no obedece el esquema datos básicos más datos complementarios

bAlertOnBDOnly =  .NULL.
*Indica si el evento debe producir una alerta vía email solamente con los datos básicos

bSymptomsBeginingMMandatory = .T.
*Indica si la fecha de inicio de síntomas para el evento es obligatoria o no

FUNCTION Init(sEventCode as String)
	
	LOCAL sSQLCmd as String
	
	sSQLCmd = 'SELECT * FROM ' + This.sSourceTableName + ' WHERE COD_EVE == "' + IIF(EMPTY(sEventCode),'NA',sEventCode) + '" INTO CURSOR rsEventDescription'
	&sSQLCmd
	IF _TALLY > 0 THEN
		WITH THIS
			.bIsDefined = .T.
			
			.sCOD_EVE = rsEventDescription.COD_EVE
			.sNOM_EVE = ALLTRIM(rsEventDescription.NOM_EVE)
			.sINMEDIATA = rsEventDescription.INMEDIATA
			
			.nTIP_NOT = rsEventDescription.TIP_NOT
			.bIsIndividual = BETWEEN(.nTIP_NOT,1,2)
					
			IF !EMPTY(rsEventDescription.FORMULARIO) THEN
				.sFORMULARIO = PADL(ALLTRIM(rsEventDescription.FORMULARIO),2,'0')
			 	.bHasComplementaryData = .T.
			ELSE
				.sFORMULARIO = rsEventDescription.FORMULARIO
				.bHasComplementaryData = .F.
			ENDIF
			.nVERSION = rsEventDescription.VERSION

			.sCLA_PER = ALLTRIM(rsEventDescription.CLA_PER)
			.nAvailableClasifications = LEN(ALLTRIM(.sCLA_PER))

			.sGRU_PER = ALLTRIM(rsEventDescription.GRU_PER)
			
			.nEXA_LAB = rsEventDescription.EXA_LAB
			.bCatchLabs = (rsEventDescription.EXA_LAB=1)
			
			.nCON_FINAL = rsEventDescription.CON_FINAL
			.sVAL_SEM = ALLTRIM(rsEventDescription.VAL_SEM)
			.nFLU_NOT = rsEventDescription.FLU_NOT
			.nEVE_NAL = rsEventDescription.EVE_NAL
			.dFEC_OBLIGA = rsEventDescription.FEC_OBLIGA
			.bINICIO_SIN = rsEventDescription.INICIO_SIN
			.sESTADO_EVE = rsEventDescription.ESTADO_EVE
			.bACTIVO = rsEventDescription.ACTIVO
			.bADMITE_NI = rsEventDescription.ADMITE_NI
			.nLATENCIA_DC = rsEventDescription.LATENCIA_DC
			.sAJUSTES_NA = rsEventDescription.AJUSTES_NA

			.sEDAD_MAX = rsEventDescription.EDAD_MAX
			.sUNIDAD_MED = rsEventDescription.UNIDAD_MED
			.sEDAD_MIN = rsEventDescription.EDAD_MIN
			.sUNIMED_MIN = rsEventDescription.UNIMED_MIN

			.bLAB_INCRUS = rsEventDescription.LAB_INCRUS
			.sDC_FORM = ALLTRIM(rsEventDescription.DC_FORM)
			.sDC_TABLA = ALLTRIM(rsEventDescription.DC_TABLA)
			
			.nFORMULARIO = INT(VAL(.sFORMULARIO))
			.bSUPER_FAST = rsEventDescription.SUPER_FAST 
			.sCONDICION_SUPER_FAST = rsEventDescription.CONDICION_SUPER_FAST 
			.nDIAS_AJUSTE = rsEventDescription.DIAS_AJUSTE 
			.sCLASIF_INICIAL = rsEventDescription.CLASIF_INICIAL 
			.sACTUALIZABLES_POR_CONFIRMACION = rsEventDescription.ACTUALIZABLES_POR_CONFIRMACION
			.sMANEJA_CONTACTOS = rsEventDescription.MANEJA_CONTACTOS
			
			.bNotBDPlusCDEvent = ((',' + ALLTRIM(.sCOD_EVE) + ',') $ This.sEventsWithoutBdpluscd)
			
			.bAlertOnBDOnly = ((',' + ALLTRIM(.sCOD_EVE) + ',') $ This.sAlertsOnBasicDataEvents)
			
			.bSymptomsBeginingMMandatory = ('INI_SIN' $ ALLTRIM(rsEventDescription.VAL_SEM))
		ENDWITH
	ELSE
		WITH THIS
			.sCOD_EVE = sEventCode 
			.sNOM_EVE = .NULL.
			.sINMEDIATA = .NULL.
			.nTIP_NOT = .NULL.
			.sFORMULARIO = .NULL.
			.nVERSION = .NULL.
			.sCLA_PER = .NULL.
			.sGRU_PER = .NULL.
			.nEXA_LAB = .NULL.
			.nCON_FINAL = .NULL.
			.sVAL_SEM = .NULL.
			.nFLU_NOT = .NULL.
			.nEVE_NAL = .NULL.
			.dFEC_OBLIGA = .NULL.
			.bINICIO_SIN = .NULL.
			.sESTADO_EVE = .NULL.
			.bACTIVO = .NULL.
			.bADMITE_NI = .NULL.
			.nLATENCIA_DC = .NULL.
			.sAJUSTES_NA = .NULL.
			.sEDAD_MAX = .NULL.
			.sUNIDAD_MED = .NULL.
			.sEDAD_MIN = .NULL.
			.sUNIMED_MIN = .NULL.
			.bLAB_INCRUS = .NULL.
			.sDC_FORM = .NULL.
			.sDC_TABLA = .NULL.

			.nFORMULARIO = .NULL.
			.bSUPER_FAST = .NULL.
			.sCONDICION_SUPER_FAST = .NULL.
			.nDIAS_AJUSTE = .NULL.
			.sCLASIF_INICIAL = .NULL.
			.sACTUALIZABLES_POR_CONFIRMACION = .NULL.
			.sMANEJA_CONTACTOS = .NULL.
			
			.bHasComplementaryData = .F.
			.bCatchLabs = .F.
			.bIsIndividual = .T.
			.bIsDefined = .F.
			.nAvailableClasifications = .NULL.
			.bNotBDPlusCDEvent = .NULL.
			.bAlertOnBDOnly =  .NULL.
			.bSymptomsBeginingMMandatory = .NULL.
		ENDWITH
	ENDIF
ENDFUNC


FUNCTION Destroy
ENDFUNC


FUNCTION clasificationsToWords() as String

	LOCAL sEventClasifications as String
	
	sEventClasifications = ''
	
	IF '1' $ This.sCLA_PER THEN
		sEventClasifications = sEventClasifications + '1- Sospechoso' + CHR(13) + CHR(10)
	ENDIF
	
	IF '2' $ This.sCLA_PER THEN
		sEventClasifications = sEventClasifications + '2- Probable' + CHR(13) + CHR(10)
	ENDIF

	IF '3' $ This.sCLA_PER THEN
		sEventClasifications = sEventClasifications + '3- Conf. Laboratorio' + CHR(13) + CHR(10)
	ENDIF

	IF '4' $ This.sCLA_PER THEN
		sEventClasifications = sEventClasifications + '4- Conf. Clínica' + CHR(13) + CHR(10)
	ENDIF

	IF '5' $ This.sCLA_PER THEN
		sEventClasifications = sEventClasifications + '5- Conf. Nexo Epidemiológico' + CHR(13) + CHR(10)
	ENDIF

	RETURN sEventClasifications
ENDFUNC


FUNCTION isValidClasification(sClasification as String) as Boolean 

	RETURN ALLTRIM(sClasification) $ This.sCLA_PER

ENDFUNC


FUNCTION symptomDateIsCompulsory() as Boolean 

	RETURN This.bINICIO_SIN

ENDFUNC

FUNCTION IsNegativeEvent() as Boolean 

	RETURN ALLTRIM(This.sCOD_EVE) == '000'

ENDFUNC

FUNCTION IsDateFreeEvent() as Boolean 

	RETURN (',' + ALLTRIM(This.sCOD_EVE) + ',') $ DATE_FREE_EVENTS

ENDFUNC


FUNCTION hasComplementaryData() as Boolean 

	RETURN !ISNULL(This.sDC_TABLA) AND !EMPTY(This.sDC_TABLA)
	
ENDFUNC


PROCEDURE Refresh(sEventCode as String)

	This.Init(sEventCode)
	
ENDPROC

PROCEDURE SendAlert(sExportationPath as String, bInTestMode as Boolean )
	
	LOCAL nOldWorkArea as Integer 
	nOldWorkArea = SELECT(0)
	
	LOCAL sAttachements as String 
	sAttachements = ''
	
	LOCAL sExportedFileName as String
	sExportedFileName = ''
	
	LOCAL bSendMail as Boolean 
	bSendMail = .T.
	
	IF VARTYPE(bInTestMode) != 'L' THEN 
		bInTestMode = .T.
	ENDIF 
	
	IF VARTYPE(sExportationPath) = 'C' AND !EMPTY(sExportationPath) THEN 
		sExportationPath = ADDBS(sExportationPath)
	ELSE
		sExportationPath = '.\'
	ENDIF 

	WITH this
		IF .bSUPER_FAST 
			IF !EMPTY(.sCONDICION_SUPER_FAST) THEN
				bSendMail = (EVALUATE(.sCONDICION_SUPER_FAST))
			ENDIF			
			IF bSendMail THEN
				
				*Genera un archivo texto con un informe de notificación legible en cualquier dispositivo
				sExportedFileName = 'Notificacion_inmediata' + "_" + .sCOD_EVE + STRTRAN(TTOC(DATETIME(),3),":","_")
				DO printDBPlusDCToFile WITH sExportationPath  + sExportedFileName 
				
				*Envía el email adjuntando tanto el reporte como el archivo excel
				IF !bInTestMode  THEN
					sAttachements = sExportationPath + sExportedFileName + '.TXT'
					sAttachements = sAttachements + ';' + sExportationPath + sExportedFileName + '.XLS'
					
					*Construye el subject del email basándose en los datos del caso registrado que printDBPlusDCToFile ha construido(rsPaciente y rsDC)
					sSubject = ' COD_' + .sCOD_EVE + ' ' + 'COD_ASE_' + IIF(EMPTY(rsPaciente.COD_ASE),'NR',rsPaciente.COD_ASE) + ;
								' ' + 'COD_DPTO_NOTIFICADOR_' + LEFT(rsPaciente.COD_PRE,2) +;
								' ' + 'COD_DPTO_RESIDENCIA_' + LEFT(rsPaciente.MUN_PRO,2) + ' ' + 'COD_DPTO_PROCEDENCIA_' + LEFT(rsPaciente.COD_MUN,2) +;
								' ' + 'COD_MUN_PROCEDENCIA_' + rsPaciente.COD_MUN 
								
					sSendMailCmd = "DO SendMailManual WITH 48, sSubject , '' , sAttachements IN " + PATH_TO_SIVIGILAMailer + "SendMail.fxp"
					&sSendMailCmd
				ENDIF
			ENDIF
		ENDIF
	ENDWITH 

	SELECT (nOldWorkArea)
ENDPROC 


FUNCTION needsContrast() as Boolean 
	
	RETURN !ISNULL(This.sCOD_EVE) AND (',' + ALLTRIM(This.sCOD_EVE) + ',' $ This.sNeedsContrastEvents)
	
ENDFUNC

FUNCTION needsTokenForCD() as Boolean 
	
	RETURN !ISNULL(This.sCOD_EVE) AND (',' + ALLTRIM(This.sCOD_EVE) + ',' $ This.sNeedsTokenEvents)
	
ENDFUNC

FUNCTION hasInitClasification() as Boolean 
	*Retorna .T. si el evento tienen una clasificación inicial obligatoria para los casos nuevos que de él se presenten
	RETURN !EMPTY(NVL(This.sCLASIF_INICIAL,''))

ENDFUNC

FUNCTION checkEpidemiologicalWeek(seed as String, nPriority as Byte) AS Boolean

	*Retorna .T. si la fecha especificada en seed determina la semana epidemiológica de ocurrencia del evento. nPriority contendrá el valor;
	de prioridad asignado a seed según el orden que ocupe en la lista de campos determinantes de la semana epidemiológica del evento; en caso de que ;
	la lista contenga un solo elemento o de que seed no sea determinante de la semana, nPriority será cero (0). Por ejemplo, si seed aparece ;
	en el primer lugar en una lista de determinantes de longitud 3, nPriority será 3; si aparece en el segundo	lugar en la lista, nPriority será 2 y ;
	si aparece en el tercer lugar en la lista, nPriority será 1.
	
	DIMENSION aDiscriminants(1)
	
	bIsDeterminant = ((','+UPPER(seed)+',') $ (','+This.sVAL_SEM+','))
	nPriority = 0
	IF bIsDeterminant THEN 
		nDiscriminants = ALINES(aDiscriminants, This.sVAL_SEM, 1+4+8, ',')
		IF nDiscriminants > 1 THEN 
			nPriority = nDiscriminants - ASCAN(aDiscriminants, seed, -1, -1, -1, 15) + 1
		ENDIF 
	ENDIF 
	RELEASE aDiscriminants
	
	RETURN bIsDeterminant
ENDFUNC

FUNCTION ManageContacts() as Boolean 
	RETURN !EMPTY(NVL(This.sMANEJA_CONTACTOS,''))
ENDFUNC 


FUNCTION ComplementaryDataHasFields(sFieldList as String) as Boolean
	*Retorna .T. si todos los campos de la lista separada por comas sFieldList pertenecen a la la tabla de datos complementarios del evento;
	en caso contrario retorna .F.
	
	STORE .T. TO bContinue 
	
	STORE  SYS(2015) TO sTmpRSName
*SET STEP ON 	
	DIMENSION aFieldList(1)

	IF VARTYPE(sFieldList )='C' AND !EMPTY(sFieldList) THEN 
		OPEN DATABASE BDSivigila
		nListElements = ALINES(aFieldList,sFieldList,1+4+8,',')
		iField = 1
		DO WHILE  iField<=nListElements AND bContinue
			TRY 
				sFieldNameToQuery = This.sDC_TABLA + '.' + aFieldList[iField]
				vTmp = DBGETPROP("&sFieldNameToQuery ","Field","Caption" )
			CATCH TO oException
				bContinue = .F.
			ENDTRY 
			iField = iField +1
		ENDDO 
		
		*TRY  
			*sSQLCmd = 'SELECT ' + sFieldList  + ' FROM ' + This.sDC_TABLA + ' WHERE COD_EVE=="' + This.sCOD_EVE + '" INTO CURSOR ' + sTmpRSName
		*	sSQLCmd = 'SELECT * FROM SIVIGILATABLESSTRUC WHERE TABLE_NAME="' + This.sDC_TABLA + '" AND ALLTRIM(FIELD_NAME) $ "' +;
						 sFieldList + '" INTO CURSOR ' + sTmpRSName
		*	&sSQLCmd
		*	IF _tally = 
		*CATCH TO oException
		*	bReturnedVal = .F.
		*ENDTRY 
	ELSE
		bContinue = .F.
	ENDIF 
	
	RELEASE aFieldList
	
	RETURN bContinue 
ENDFUNC 

*JLGB : 22/01/2010
FUNCTION MandatoryComplementaryData() as Boolean 
	
	RETURN !ISNULL(This.sCOD_EVE) AND (',' + ALLTRIM(This.sCOD_EVE) + ',' $ This.sNeedsComplementaryData)
ENDFUNC 


FUNCTION GetRetainigFields(sAdjustment as String)
	
	*Devuelve un objeto de la forma ["FieldName_1","FieldName_2",..., "FieldName_n"] que contiene la lista de campos de datos complementarios ;
	dependientes del ajuste sAdjustment. Los campos dependientes del ajuste, son aquellos que deben ser retenidos a la hora de producir un único ;
	registro con los datos de un caso, es decir, campos cuyo valor debe mantenerse al hacer el procesamiento de ajustes de los registos que ;
	componen un caso del evento.
	
	*El objeto devuelto puede ser accesado usando la sintaxis obj.get(1), obj.get(2), ..., obj.get(n) y n puede ser determinado por obj.nSize ;
	En caso de que el evento no tenga campos complementarios dependientes o no tenga campos complementarios dependientes correspondientes al ;
	ajuste sAdjustment , retorna .NULL.
	
	SET PROCEDURE TO (PATH_TO_VFPJson_CLASS + '\json') ADDITIVE

	IF !ISNULL(This.sACTUALIZABLES_POR_CONFIRMACION) THEN 
		oJson = json_decode(This.sACTUALIZABLES_POR_CONFIRMACION)
		IF !ISNULL(oJson) THEN 
			DO CASE 
				CASE sAdjustment $ CONFIRMATION_ADJUSTMENTS AND VARTYPE(oJson._ajustesConfirmatorios)='O'
					RETURN oJson._ajustesConfirmatorios._Dependientes
				CASE sAdjustment = CLINICAL_DISCARD_ADJUSTMENT AND VARTYPE(oJson._ajustesDeDescarte)='O'
					RETURN oJson._ajustesDeDescarte._Dependientes 
				OTHERWISE
					RETURN .NULL.
			ENDCASE
		ELSE
			RETURN .NULL.
		ENDIF
	ENDIF 
		
ENDFUNC 


FUNCTION CompulsoryLabs() as Boolean 
	
	IF !ISNULL(This.sCOD_EVE)
		IF LAB_CONFIRMATION_ADJUSTMENT $ This.sCLA_PER THEN 
			IF (',' + ALLTRIM(This.sCOD_EVE) + ',' $ This.sNotCompulsoryLabsEvents) THEN 
				RETURN .F.
			ELSE
				RETURN .T.
			ENDIF 	
		ELSE
			RETURN .F.
		ENDIF
	ELSE
		RETURN .F.
	ENDIF 
	
ENDFUNC

FUNCTION MandatoryComplementaryData2() as Boolean 
	
	RETURN !ISNULL(This.sCOD_EVE) AND (',' + ALLTRIM(This.sCOD_EVE) + ',' $ This.sNeedsComplementaryData2)
ENDFUNC 

ENDDEFINE


