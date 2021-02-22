#INCLUDE SIVIGILA.H
#INCLUDE CLS_BASIC_DATA.H

DEFINE CLASS BasicData as Custom

HIDDEN sSourceTableName
sSourceTableName = 'PACIENTE'
*Nombre de la tabla que almacena datos básicos de notificación en el sistema y sbre la cual se basa la clase BasicData 

HIDDEN sDefaultKey
sDefaultKey = 'AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB'
*Campos que, por default, constituyen la "clave" que identifica un caso notificado de datos básicos. Un caso puede tener m´sa de un registro;
normalmente, estará constituido por un registro índice y uno o más registros de ajuste

HIDDEN sLastRecordSet
sLastRecordSet = ''
*Nombre del cursor que contiene los registros de un caso identificado por la clave sDefaultKey

*Atributos de la clase BasicData que corresponden uno-a-uno con campos de la tabla sSourceTableName
sCOD_EVE = .NULL.
dFEC_NOT = .NULL.
sSEMANA = .NULL.
sAÑO = .NULL.
sCOD_PRE = .NULL.
sCOD_SUB = .NULL.
sPRI_NOM = .NULL.
sSEG_NOM = .NULL.
sPRI_APE = .NULL.
sSEG_APE = .NULL.
sTIP_IDE = .NULL.
sNUM_IDE = .NULL.
sEDAD = .NULL.
sUNI_MED = .NULL.
sSEXO = .NULL.
sPAIS = .NULL.
sCOD_MUN = .NULL.
sAREA = .NULL.
sBAR_VER = .NULL.
sVEREDA = .NULL.
sDIR_RES = .NULL.
sOCUPACION = .NULL.
sTIP_SS = .NULL.
sCOD_ASE = .NULL.
sPER_ETN = .NULL.
sGRU_POB = .NULL.
sMUN_PRO = .NULL.
dFEC_CON = .NULL.
dINI_SIN = .NULL.
sTIP_CAS = .NULL.
sPAC_HOS = .NULL.
dFEC_HOS = .NULL.
sCON_FIN = .NULL.
dFEC_DEF = .NULL.
sAJUSTE = .NULL.
sTELEFONO = .NULL.
dFECHA_NTO = .NULL.
sCER_DEF = .NULL.
sCBMTE = .NULL.
dFEC_AJU = .NULL.
sFM_FUERZA = .NULL.
sFM_UNIDAD = .NULL.
sFM_GRADO = .NULL.
sNOM_DIL_FI = .NULL.
sTEL_DIL_FI = .NULL.
sCOD_SEC = .NULL.
nPERIODO = .NULL.
nNOTIFICA = .NULL.
nFLU_NOT = .NULL.
nSEM_REC = .NULL.
sINMEDIATA = .NULL.
sCOD_PAIS_R = .NULL.
sLOCALIDAD = .NULL.
sCEN_POBLAD = .NULL.
dFECHACARGA = .NULL.
sVERSION = .NULL.
sGP_DISCAPA = .NULL.
sGP_DESPLAZ = .NULL.
sGP_MIGRANT = .NULL.
sGP_CARCELA = .NULL.
sGP_GESTAN = .NULL.
sGP_INDIGEN = .NULL.
sGP_POBICFB = .NULL.
sGP_MAD_COM = .NULL.
sGP_DESMOVI = .NULL.
sGP_PSIQUIA = .NULL.
sGP_VIC_VIO = .NULL.
sGP_OTROS = .NULL.
bINGR_X_NI = .NULL.
sESTADOTRAN = .NULL.
sID_VEREDA = .NULL.
*sINGR_X_RA = .NULL.
sUNI_MODIF = .NULL.
*dFEC_REACTI = .NULL.
*sCOD_UNI_SA = .NULL.
*sTIPO_DONAN = .NULL.
*sTIPO_PRUEB = .NULL.
nEST_INGR = .NULL.
nEST_NOTIF = .NULL.
sFUENTE = .NULL.
sESTRATO = .NULL.
sSEM_GES = .NULL.
sGRUPO_ETNI = .NULL.
sNACIONALID = .NULL.
*-----------------------------------------------------------------------------------------------------------------------------------------------*

*Atributos de la clase BasicData que no corresponden con campos de la tabla sSourceTableName

sSystemUpdates = ""
*Lista de campos separada por '|' que han sido modificados o establecidos para un caso por algún proceso propio del sistema

SaveResult = .NULL.
*Resultado obtenido al guardar un registro de datos básicos

bEpidemWeekCalculated = .F.
*Indica si la semana epidemiológica de ocurrencia del evento ya se ha calculado

nEpidemWeekCurrentPriority = 0
*Prioridad utilizada para el cálculo de la semana epidemiológica de ocurrencia del evento. Dado que un evento puede establecer su semana de;
ocurrencia a partir de varios campos distintos, este atributo indica cuál fue la prioridad correspondiente al campo utilizado para calcular;
dicha semana.

*-----------------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION Init(sBDKey as String)
	*Construye un recordset con los registros que conforman el caso de notifcación de datos básicos	según la clave de agrupamiento sBDKey. ;
	Esta clave debe ser alguna realización o valor concreto de This.sDefaultKey. ;
	El recordset construido quedará con un nombre establecido en This.sLastRecordSet 
	
	LOCAL sSQLCmd as String
	LOCAL nOldWorkArea as Number
	
	nOldWorkArea = SELECT()
	
	sRSBasicDataRecs = 'rs' + SYS(2015)
	sSQLCmd = 'SELECT * FROM ' + This.sSourceTableName + ' WHERE ' + This.sDefaultKey + ' == "' + sBDKey  + '" INTO CURSOR ' + sRSBasicDataRecs 
	&sSQLCmd
	This.sLastRecordSet = sRSBasicDataRecs 
	IF _TALLY > 0 THEN
		WITH This
			.sCOD_EVE = &sRSBasicDataRecs..COD_EVE
			.dFEC_NOT = &sRSBasicDataRecs..FEC_NOT
			.sSEMANA = &sRSBasicDataRecs..SEMANA
			.sAÑO = &sRSBasicDataRecs..AÑO
			.sCOD_PRE = &sRSBasicDataRecs..COD_PRE
			.sCOD_SUB = &sRSBasicDataRecs..COD_SUB
			.sPRI_NOM = &sRSBasicDataRecs..PRI_NOM
			.sSEG_NOM = &sRSBasicDataRecs..SEG_NOM
			.sPRI_APE = &sRSBasicDataRecs..PRI_APE
			.sSEG_APE = &sRSBasicDataRecs..SEG_APE
			.sTIP_IDE = &sRSBasicDataRecs..TIP_IDE
			.sNUM_IDE = &sRSBasicDataRecs..NUM_IDE
			.sEDAD = &sRSBasicDataRecs..EDAD
			.sUNI_MED = &sRSBasicDataRecs..UNI_MED
			.sSEXO = &sRSBasicDataRecs..SEXO
			.sPAIS = &sRSBasicDataRecs..PAIS
			.sCOD_MUN = &sRSBasicDataRecs..COD_MUN
			.sAREA = &sRSBasicDataRecs..AREA
			.sBAR_VER = &sRSBasicDataRecs..BAR_VER
			.sVEREDA = &sRSBasicDataRecs..VEREDA
			.sDIR_RES = &sRSBasicDataRecs..DIR_RES
			.sOCUPACION = &sRSBasicDataRecs..OCUPACION
			.sTIP_SS = &sRSBasicDataRecs..TIP_SS
			.sCOD_ASE = &sRSBasicDataRecs..COD_ASE
			.sPER_ETN = &sRSBasicDataRecs..PER_ETN
			.sGRU_POB = &sRSBasicDataRecs..GRU_POB
			.sMUN_PRO = &sRSBasicDataRecs..MUN_PRO
			.dFEC_CON = &sRSBasicDataRecs..FEC_CON
			.dINI_SIN = &sRSBasicDataRecs..INI_SIN
			.sTIP_CAS = &sRSBasicDataRecs..TIP_CAS
			.sPAC_HOS = &sRSBasicDataRecs..PAC_HOS
			.dFEC_HOS = &sRSBasicDataRecs..FEC_HOS
			.sCON_FIN = &sRSBasicDataRecs..CON_FIN
			.dFEC_DEF = &sRSBasicDataRecs..FEC_DEF
			.sAJUSTE = &sRSBasicDataRecs..AJUSTE
			.sTELEFONO = &sRSBasicDataRecs..TELEFONO
			.dFECHA_NTO = &sRSBasicDataRecs..FECHA_NTO
			.sCER_DEF = &sRSBasicDataRecs..CER_DEF
			.sCBMTE = &sRSBasicDataRecs..CBMTE
			.dFEC_AJU = &sRSBasicDataRecs..FEC_AJU
			.sFM_FUERZA = &sRSBasicDataRecs..FM_FUERZA
			.sFM_UNIDAD = &sRSBasicDataRecs..FM_UNIDAD
			.sFM_GRADO = &sRSBasicDataRecs..FM_GRADO
			.sNOM_DIL_FI = &sRSBasicDataRecs..NOM_DIL_FI
			.sTEL_DIL_FI = &sRSBasicDataRecs..TEL_DIL_FI
			.sCOD_SEC = &sRSBasicDataRecs..COD_SEC
			.nPERIODO = &sRSBasicDataRecs..PERIODO
			.nNOTIFICA = &sRSBasicDataRecs..NOTIFICA
			.nFLU_NOT = &sRSBasicDataRecs..FLU_NOT
			.nSEM_REC = &sRSBasicDataRecs..SEM_REC
			.sINMEDIATA = &sRSBasicDataRecs..INMEDIATA
			.sCOD_PAIS_R = &sRSBasicDataRecs..COD_PAIS_R
			.sLOCALIDAD = &sRSBasicDataRecs..LOCALIDAD
			.sCEN_POBLAD = &sRSBasicDataRecs..CEN_POBLAD
			.dFECHACARGA = &sRSBasicDataRecs..FECHACARGA
			.sVERSION = &sRSBasicDataRecs..VERSION
			.sGP_DISCAPA = &sRSBasicDataRecs..GP_DISCAPA
			.sGP_DESPLAZ = &sRSBasicDataRecs..GP_DESPLAZ
			.sGP_MIGRANT = &sRSBasicDataRecs..GP_MIGRANT
			.sGP_CARCELA = &sRSBasicDataRecs..GP_CARCELA
			.sGP_GESTAN = &sRSBasicDataRecs..GP_GESTAN
			.sGP_INDIGEN = &sRSBasicDataRecs..GP_INDIGEN
			.sGP_POBICFB = &sRSBasicDataRecs..GP_POBICFB
			.sGP_MAD_COM = &sRSBasicDataRecs..GP_MAD_COM
			.sGP_DESMOVI = &sRSBasicDataRecs..GP_DESMOVI
			.sGP_PSIQUIA = &sRSBasicDataRecs..GP_PSIQUIA
			.sGP_VIC_VIO = &sRSBasicDataRecs..GP_VIC_VIO
			.sGP_OTROS = &sRSBasicDataRecs..GP_OTROS
			.bINGR_X_NI = &sRSBasicDataRecs..INGR_X_NI
			.sESTADOTRAN = &sRSBasicDataRecs..ESTADOTRAN
			.sID_VEREDA = &sRSBasicDataRecs..ID_VEREDA
			*.sINGR_X_RA = &sRSBasicDataRecs..INGR_X_RA
			.sUNI_MODIF = &sRSBasicDataRecs..UNI_MODIF
			*.dFEC_REACTI = &sRSBasicDataRecs..FEC_REACTI
			*.sCOD_UNI_SA = &sRSBasicDataRecs..COD_UNI_SA
			*.sTIPO_DONAN = &sRSBasicDataRecs..TIPO_DONAN
			*.sTIPO_PRUEB = &sRSBasicDataRecs..TIPO_PRUEB
			.nEST_INGR = &sRSBasicDataRecs..EST_INGR
			.nEST_NOTIF = &sRSBasicDataRecs..EST_NOTIF
			.sFUENTE = &sRSBasicDataRecs..FUENTE
			.sESTRATO = &sRSBasicDataRecs..ESTRATO
			.sSEM_GES = &sRSBasicDataRecs..SEM_GES
			.sGRUPO_ETNI = &sRSBasicDataRecs..GRUPO_ETNI
			.sNACIONALID = &sRSBasicDataRecs..NACIONALID
			
			*Establece qué campos han sido modificados por el sistema mediante algún proceso automático  
			.sSystemUpdates = ""
			bContinue = .T.
			SELECT (sRSBasicDataRecs) 
			GO TOP 
			DO WHILE bContinue 
				IF SIVIGILA_MARK $ &sRSBasicDataRecs..NOM_DIL_FI
					nInitPos = AT('[',&sRSBasicDataRecs..NOM_DIL_FI) + 1
					nFinalPos = AT(']',&sRSBasicDataRecs..NOM_DIL_FI) - 1
					.sSystemUpdates = This.unCodify(SUBSTR(&sRSBasicDataRecs..NOM_DIL_FI, nInitPos, nFinalPos - nInitPos +1))
					bContinue = .F.
				ELSE
					SKIP
					bContinue = (!EOF())
				ENDIF
				
			ENDDO 

			.bEpidemWeekCalculated = .F.
		ENDWITH
	ELSE
		WITH THIS
			.sCOD_EVE = .NULL.
			.dFEC_NOT = .NULL.
			.sSEMANA = .NULL.
			.sAÑO = .NULL.
			.sCOD_PRE = .NULL.
			.sCOD_SUB = .NULL.
			.sPRI_NOM = .NULL.
			.sSEG_NOM = .NULL.
			.sPRI_APE = .NULL.
			.sSEG_APE = .NULL.
			.sTIP_IDE = .NULL.
			.sNUM_IDE = .NULL.
			.sEDAD = .NULL.
			.sUNI_MED = .NULL.
			.sSEXO = .NULL.
			.sPAIS = .NULL.
			.sCOD_MUN = .NULL.
			.sAREA = .NULL.
			.sBAR_VER = .NULL.
			.sVEREDA = .NULL.
			.sDIR_RES = .NULL.
			.sOCUPACION = .NULL.
			.sTIP_SS = .NULL.
			.sCOD_ASE = .NULL.
			.sPER_ETN = .NULL.
			.sGRU_POB = .NULL.
			.sMUN_PRO = .NULL.
			.dFEC_CON = .NULL.
			.dINI_SIN = .NULL.
			.sTIP_CAS = .NULL.
			.sPAC_HOS = .NULL.
			.dFEC_HOS = .NULL.
			.sCON_FIN = .NULL.
			.dFEC_DEF = .NULL.
			.sAJUSTE = .NULL.
			.sTELEFONO = .NULL.
			.dFECHA_NTO = .NULL.
			.sCER_DEF = .NULL.
			.sCBMTE = .NULL.
			.dFEC_AJU = .NULL.
			.sFM_FUERZA = .NULL.
			.sFM_UNIDAD = .NULL.
			.sFM_GRADO = .NULL.
			.sNOM_DIL_FI = .NULL.
			.sTEL_DIL_FI = .NULL.
			.sCOD_SEC = .NULL.
			.nPERIODO = -1
			.nNOTIFICA = -1
			.nFLU_NOT = -1
			.nSEM_REC = -1
			.sINMEDIATA = .NULL.
			.sCOD_PAIS_R = .NULL.
			.sLOCALIDAD = .NULL.
			.sCEN_POBLAD = .NULL.
			.dFECHACARGA = .NULL.
			.sVERSION = .NULL.
			.sGP_DISCAPA = .NULL.
			.sGP_DESPLAZ = .NULL.
			.sGP_MIGRANT = .NULL.
			.sGP_CARCELA = .NULL.
			.sGP_GESTAN = .NULL.
			.sGP_INDIGEN = .NULL.
			.sGP_POBICFB = .NULL.
			.sGP_MAD_COM = .NULL.
			.sGP_DESMOVI = .NULL.
			.sGP_PSIQUIA = .NULL.
			.sGP_VIC_VIO = .NULL.
			.sGP_OTROS = .NULL.
			.bINGR_X_NI = .NULL.
			.sESTADOTRAN = .NULL.
			.sID_VEREDA = .NULL.
			*.sINGR_X_RA = .NULL.
			.sUNI_MODIF = .NULL.
			*.dFEC_REACTI = .NULL.
			*.sCOD_UNI_SA = .NULL.
			*.sTIPO_DONAN = .NULL.
			*.sTIPO_PRUEB = .NULL.
			.nEST_INGR = -1
			.nEST_NOTIF = -1
			.sFUENTE = .NULL.
			.sESTRATO = .NULL.
			.sSEM_GES = .NULL.
			.sGRUPO_ETNI = .NULL.
			.sNACIONALID = .NULL.
			
			.sSystemUpdates = ""
			.bEpidemWeekCalculated = .F.
		ENDWITH
	ENDIF
	SELECT (nOldWorkArea) 
ENDFUNC


FUNCTION Destroy
ENDFUNC

PROCEDURE Refresh(sBDKey as String)
	TRY 
		USE IN (This.sLastRecordSet)
	CATCH TO oException
	ENDTRY 
	
	This.Init(sBDKey)
ENDPROC


FUNCTION unCodify(sHexCode as String) AS String 
	
	sFieldList = ''
	
	SET PROCEDURE TO Utilities ADDITIVE 
	nOldSelect = SELECT()
	
	sBinCode = hex2bin(sHexCode)
	nBinCodeLength = LEN(sBinCode)
	
	SELECT * FROM SIVIGILATablesStruc WHERE table_name='PACIENTE' INTO CURSOR rsBDFields
	SELECT rsBDFields
*SET STEP ON 	
	FOR iBit=nBinCodeLength TO 1 STEP -1
		IF SUBSTR(sBinCode,iBit,1)='1' THEN 
			sFieldList = sFieldList + ALLTRIM(rsBDFields.FIELD_NAME) + '|'
		ENDIF 
		SKIP 
	NEXT iBit 
	
	IF !EMPTY(sFieldList) THEN
		sFieldList = SUBSTR(sFieldList,1,LEN(sFieldList)-1)
	ENDIF
	USE IN rsBDFields
	USE IN SIVIGILATablesStruc 
	
	SELECT (nOldSelect)
	
	RETURN sFieldList 

ENDFUNC 


PROCEDURE Save(oPersonData as Object)
	*Guarda, a partir de variables de memoria, un registro de datos básicos en la tabla This.sSourceTableName y retorna ;
	en This.SaveResult uno de los siguientes valores: ;
	NEW_BD_INDEX_RECORD_INSERTED = 0 ;
	NEW_BD_ADJUSTMENT_RECORD_INSERTED = 1 ;
	NEW_BD_NO_RECORD_INSERTED = -1
	
	LOCAL nOldWorkArea as Integer 
	nOldWorkArea = SELECT(0)

	LOCAL sBDTableName as string
	sBDTableName = This.sSourceTableName
	
	LOCAL SQLClause as String 
	
	LOCAL oEvent as Object 
	oEvent = NEWOBJECT("Event","Event.fxp",.NULL.,m.Cod_Eve)
	
	LOCAL bNewRecAdded as Boolean
	*Indica si el registro actual es un nuevo registro que se ha agregado a la tabla de datos básicos. Utilizada básicamente para;
	determinar si, dado un registro de datos básicos, el correspondiente de datos complementarios debe quedar listo para ser;
	notificado cuando se den ajustes al primero

	bNewRecAdded = .T.
	
	*Determina si el registro del Paciente existe o no en la tabla PACIENTE y si se debe agregar una nueva ficha;
	o actualizar una existente. El tratamiento incluye aquellos casos en los que se ha hecho varios ajustes a un;
	mismo registro.
	SQLClause="SELECT " + sBDTableName + ".Cod_Eve, " + sBDTableName + ".Año, " + sBDTableName + ".Semana, " +;
				sBDTableName + ".Tip_ide, " + sBDTableName + ".Num_Ide," +;
				sBDTableName + ".Ajuste, RECNO() AS nReg, " + sBDTableName + ".Notifica FROM " + sBDTableName + " WHERE " +;
				sBDTableName + ".AJUSTE + " + sBDTableName + ".AÑO + " + sBDTableName + ".SEMANA + " +;
				sBDTableName + ".COD_EVE + " + sBDTableName + ".TIP_IDE +" +;
				sBDTableName + ".NUM_IDE == '" +  m.AJUSTE + m.AÑO + m.SEMANA + m.COD_EVE + m.TIP_IDE + m.NUM_IDE + "'" +;
				" AND " + sBDTableName + ".COD_PRE + " + sBDTableName + ".COD_SUB == '" + m.COD_PRE + m.COD_SUB + "'" +;
				" ORDER BY " + sBDTableName + ".Notifica ASC" +;
				" INTO ARRAY aExistingPatients"
	&SQLClause
*SET STEP ON 	
	IF _TALLY = 0 THEN
		*Se debe agregar un nuevo registro o ficha   
		m.fec_aju = DATE()
		m.NOTIFICA = 0
		
		*Establece el valor del código a asignar al campo NOM_DIL_FI en función de oPersonData, si es que aplica 
		vCode = 0
		IF !ISNULL(oPersonData) THEN 
			IF m.PRI_NOM = oPersonData.get("PrimerNombre") THEN 
				vCode = bin2dec('000000000000000001000000')
			ENDIF 
			IF m.SEG_NOM = oPersonData.get("SegundoNombre") THEN 
				vCode = BITOR(vCode, bin2dec('0000000000000000010000000'))
			ENDIF 
			IF m.PRI_APE = oPersonData.get("PrimerApellido") THEN 
				vCode = BITOR(vCode, bin2dec('0000000000000000100000000'))
			ENDIF 
			IF m.SEG_APE = oPersonData.get("SegundoApellido") THEN 
				vCode = BITOR(vCode, bin2dec('0000000000000001000000000'))
			ENDIF 

			IF oPersonData.get('EstadoAfiliacion') $ 'SA,SU' THEN 
				*El tipo de aseguramiento fue fijado por el sistema según consulta del BDUA
				vCode = BITOR(vCode, bin2dec('0010000000000000000000000'))
			ENDIF 

			IF m.COD_ASE = oPersonData.get("EPSAfiliacion") THEN 
				vCode = BITOR(vCode, bin2dec('0100000000000000000000000')) &&COD_ASE 
				vCode = BITOR(vCode, bin2dec('0010000000000000000000000')) &&TIP_SS
			ENDIF 

			*Los valores de TIP_IDE y NUM_IDE siempre deben incorporarse al código vCode
			vCode = BITOR(vCode, bin2dec('0000000000000010000000000')) &&TIP_IDE 
			vCode = BITOR(vCode, bin2dec('0000000000000100000000000')) &&NUM_IDE 
					
			vCode = UPPER('[' + dec2basx(vCode,16) + ']')
		ENDIF 
		IF VARTYPE(vCode) = 'N' THEN 
			vCode = ''
		ENDIF 
		
		INSERT INTO (sBDTableName) FROM MEMVAR
		IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
			goErrorlog.resetErrorLog()
			*JLGB : 04/01/2020
			This.SaveResult = NEW_BD_NO_RECORD_INSERTED 
			RETURN 0
		ENDIF

		glDisableRules = .T.

		SELECT (sBDTableName)
		IF !(SIVIGILA_MARK $ NOM_DIL_FI) THEN 
			IF !EMPTY(vCode) THEN 
				REPLACE NOM_DIL_FI WITH ALLTRIM(NOM_DIL_FI) + ' ' + SIVIGILA_MARK + vCode 
			ENDIF 
		ELSE
			*El campo NOM_DIL_FI ya había sido establecido por el sistema; por tanto, hay que eliminar la marca anterior y establecer una nueva
			REPLACE NOM_DIL_FI WITH ALLTRIM(SUBSTR(NOM_DIL_FI,1,AT(SIVIGILA_MARK,NOM_DIL_FI)-1)) + ' ' + SIVIGILA_MARK + vCode 
		ENDIF 
		
		IF (&sBDTableName..AJUSTE $ CONFIRMATION_OR_DISCARD_ADJUSTMENTS) OR  (&sBDTableName..AJUSTE = CHANGE_ANY_VALUE_ADJUSTMENT) THEN
			*Se trata de un registro nuevo correspondiente a un ajuste que cambia la clasificación del caso o a un ;
			ajuste que cambia cualquier valor de cualquier variable, por tanto;	es necesario guardar la identificación;
			de la unidad que hace el ajuste
			SELECT (sBDTableName)
			s1=_SCREEN.sNotifierUnit
			REPLACE UNI_MODIF WITH s1 &&_SCREEN.sNotifierUnit
			IF !EMPTY(&sBDTableName..AJUSTE)  THEN
				This.SaveResult = NEW_BD_ADJUSTMENT_RECORD_INSERTED
			ELSE 
				*Se trata de un registro de notificación negativa que, por esencia, tiene valor de ajuste vacío
				This.SaveResult = NEW_BD_INDEX_RECORD_INSERTED	
			ENDIF 
		ELSE
			*Se trata del registro inicial o índice del caso. Si el evento pertenece al sistema de alertas tempranas, envía un email.
			IF oEvent.bAlertOnBDOnly THEN 
				oEvent.SendAlert(_RTA_PLANOS, _SCREEN.bTestMode)
			ENDIF 
			This.SaveResult = NEW_BD_INDEX_RECORD_INSERTED	
		ENDIF
		glDisableRules = .F.
		
	ELSE
		sAdjustment = aExistingPatients(1,6)
		nNotificationWeek = aExistingPatients(1,8)
		IF sAdjustment = CHANGE_ANY_VALUE_ADJUSTMENT THEN
			*El registro encontrado corresponde a un ajuste que permite cambiar cualquier dato del registro;
			original excepto su clasificación inicial.
			IF nNotificationWeek>0 THEN
				*El registro ya ha sido notificado y dada la naturaleza del ajuste, se puede permitir ingresar ;
				un nuevo registro.
				m.fec_aju = DATE()
				m.NOTIFICA = 0
				INSERT INTO PACIENTE FROM MEMVAR
				IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
					goErrorlog.resetErrorLog()
					RETURN 0
				ENDIF
				SELECT (sBDTableName)
				REPLACE  UNI_MODIF WITH _SCREEN.sNotifierUnit
				
				This.SaveResult = NEW_BD_ADJUSTMENT_RECORD_INSERTED
			ELSE
				*El registro aún no ha sido notificado y debe mostrarse para que sea editado
				bNewRecAdded = .F.
				SELECT (sBDTableName)
				GO aExistingPatients(1,7)
				SCATTER MEMVAR
				This.SaveResult = UPDATABLE_RECORD_ALREADY_EXIST
			ENDIF
		ELSE
			*Ya existe un registro correspondiente a este ajuste y debe mostrarse para que sea editado
			bNewRecAdded = .F.
			SELECT (sBDTableName)
			GO aExistingPatients(1,7)
			SCATTER MEMVAR
			This.SaveResult = UPDATABLE_RECORD_ALREADY_EXIST
		ENDIF
	ENDIF

	IF m.cod_eve='200' THEN
		*ToDo: implementar un método que permita actualizar datos complementarios para ciertos eventos con base en las variables de;
		datos básicos
		*Se trata del evento Cólera (Cod. 200)

		*Si el caso es confirmado por nexo, la variable "Caso asociado a un brote?" en datos complementarios debe ser Sí
		*IF m.tip_cas='3' THEN
		*	UPDATE EVENTOS_46 SET ASO_BRO='1' WHERE AÑO=m.año AND SEMANA=m.semana AND COD_EVE=m.cod_eve AND ;
		*		TIP_IDE=m.tip_ide AND NUM_IDE=m.num_ide AND COD_PRE=m.cod_pre AND COD_SUB=m.cod_sub
		*ENDIF

		*IF m.Ajuste='3' THEN
			*Se está cambiando el valor de la clasificación del caso y por tanto, la variable "Caso asociado a un brote?" ;
			en datos complementarios debe ser Sí
		*	UPDATE EVENTOS_46 SET ASO_BRO='1' WHERE AÑO=m.año AND SEMANA=m.semana AND COD_EVE=m.cod_eve AND ;
				TIP_IDE=m.tip_ide AND NUM_IDE=m.num_ide AND COD_PRE=m.cod_pre AND COD_SUB=m.cod_sub AND ASO_BRO!='1'
				
		*	IF _TALLY>0 THEN
				*la ficha de datos complementarios asociada también debe quedar lista para ser notificada
		*		UPDATE EVENTOS_46 SET NOTIFICA=0 WHERE AÑO=m.año AND SEMANA=m.semana AND COD_EVE=m.cod_eve AND ;
				TIP_IDE=m.tip_ide AND NUM_IDE=m.num_ide AND COD_PRE=m.cod_pre AND COD_SUB=m.cod_sub
		*	ENDIF				
		*ENDIF
	ENDIF   

	IF bNewRecAdded THEN
		IF oEvent.bHasComplementaryData THEN
			*El evento tiene datos complementarios
			
			IF (&sBDTableName..AJUSTE $ CONFIRMATION_OR_DISCARD_ADJUSTMENTS) OR (&sBDTableName..AJUSTE =  CHANGE_ANY_VALUE_ADJUSTMENT) THEN
				*Se trata de un registro de ajuste, por tanto la ficha más reciente de datos complementarios asociada, si existe, debe ;
				quedar lista para ser notificada
				
				SET PROCEDURE TO CasesHandler ADDITIVE
				nRecIdToUpdate = lastCaseRecord(oEvent.sDC_TABLA,"AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB ='" +;
												+ m.año + m.semana + m.cod_eve + m.tip_ide + m.num_ide + m.cod_pre + m.cod_sub + "'")
				IF nRecIdToUpdate > 0 THEN
					SELECT (oEvent.sDC_TABLA)
					GO nRecIdToUpdate 

					*Desactiva las reglas de validación previendo que el registro de datos complementarios viole reglas de vaidación
					glDisableRules = .T.
					SCATTER MEMVAR FIELDS EXCEPT ESTADOTRAN  &&El campo EstadoTran se asigna mediante una regla por default establecida en la tabla
					INSERT INTO (oEvent.sDC_TABLA) FROM MEMVAR 

		*ToDo: capturar el error de violación de validación que produzca y obligar a visitar la ficha de datos complementarios en forma;
		similar a como se hace con la notificación inmediata con datos parciales

					sCmd = 'REPLACE NOTIFICA WITH 0, AJUSTE WITH  ' + sBDTableName + '.' + 'AJUSTE, EST_NOTIF WITH 2'
					&sCmd
					
					*Reactiva las reglas de validación
					glDisableRules = .F.
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	RELEASE oEvent 
	SELECT (nOldWorkArea)

ENDPROC


PROCEDURE Update(nRecId as Number)
	*actualiza, a partir de variables de memoria, el registro de datos básicos identificado por nRecId en la tabla This.sSourceTableName y retorna ;
	en This.SaveResult uno de los siguientes valores: ;
	BD_RECORD_UPDATED 1 ;
	BD_RECORD_UPDATE_ERROR 0

	LOCAL nOldWorkArea as Integer 
	nOldWorkArea = SELECT(0)

	LOCAL sBDTableName as string
	sBDTableName = This.sSourceTableName
	
	LOCAL SQLClause as String 
	
	LOCAL oEvent as Object 
	oEvent = NEWOBJECT("Event","Event.fxp",.NULL.,m.Cod_Eve)
	
	LOCAL oStoredEvent as object
	
	SELECT (sBDTableName)
	GO nRecId 
	
	BEGIN TRANSACTION 
	
	IF (m.AÑO!=&sBDTableName..AÑO) OR (m.SEMANA!=&sBDTableName..SEMANA) OR (m.COD_PRE+m.COD_SUB!=&sBDTableName..COD_PRE+&sBDTableName..COD_SUB) ;
		OR (m.TIP_IDE!=&sBDTableName..TIP_IDE) OR (m.NUM_IDE!=&sBDTableName..NUM_IDE) THEN
		*Se está cambiando un campo de la clave del registro; por tanto es necesario actualizar también los datos complementarios asociados al registro;
		de datos básicos
		oStoredEvent = NEWOBJECT("Event","Event.fxp",.NULL.,&sBDTableName..COD_EVE)
		IF oStoredEvent.bHasComplementaryData THEN
			SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
			UpdateSIVIGILAComplementaryData("PACIENTE", "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB", ;
											"PACIENTE.AÑO+PACIENTE.SEMANA+PACIENTE.COD_EVE+PACIENTE.TIP_IDE+PACIENTE.NUM_IDE+PACIENTE.COD_PRE+PACIENTE.COD_SUB='"  + ;
											PACIENTE.AÑO + PACIENTE.SEMANA + PACIENTE.COD_EVE + PACIENTE.TIP_IDE + PACIENTE.NUM_IDE + PACIENTE.COD_PRE + PACIENTE.COD_SUB + "'", , + ;
											"AÑO = '" + m.AÑO + "', SEMANA = '" + m.SEMANA + "', COD_PRE ='" + m.COD_PRE + "', COD_SUB = '" + m.COD_SUB + ;
											"', TIP_IDE = '" + m.TIP_IDE + "', NUM_IDE = '" + m.NUM_IDE + "'")
		ENDIF
		RELEASE oStoredEvent 
	ENDIF

	SELECT (sBDTableName)
	GO nRecId 
	GATHER MEMVAR
	IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
		goErrorlog.resetErrorLog()
		ROLLBACK 
		This.SaveResult = BD_RECORD_UPDATE_ERROR
		RETURN 
	ELSE
		END TRANSACTION
	ENDIF

	glDisableRules = .T.
	REPLACE VERSION WITH gsSIVIGILARecordVersion
	glDisableRules = .F.
	
	This.SaveResult = BD_RECORD_UPDATED

	IF m.cod_eve='200' THEN
		*ToDo: implementar un método que permita actualizar datos complementarios para ciertos eventos con base en las variables de;
		datos básicos
		*Se trata del evento Cólera (Cod. 200)

		*Si el caso es confirmado por nexo, la variable "Caso asociado a un brote?" en datos complementarios debe ser Sí
		*IF m.tip_cas='3' THEN
		*	UPDATE EVENTOS_46 SET ASO_BRO='1' WHERE AÑO=m.año AND SEMANA=m.semana AND COD_EVE=m.cod_eve AND ;
		*		TIP_IDE=m.tip_ide AND NUM_IDE=m.num_ide AND COD_PRE=m.cod_pre AND COD_SUB=m.cod_sub
		*ENDIF

		*IF m.Ajuste='3' THEN
			*Se está cambiando el valor de la clasificación del caso y por tanto, la variable "Caso asociado a un brote?" ;
			en datos complementarios debe ser Sí
		*	UPDATE EVENTOS_46 SET ASO_BRO='1' WHERE AÑO=m.año AND SEMANA=m.semana AND COD_EVE=m.cod_eve AND ;
				TIP_IDE=m.tip_ide AND NUM_IDE=m.num_ide AND COD_PRE=m.cod_pre AND COD_SUB=m.cod_sub AND ASO_BRO!='1'
				
		*	IF _TALLY>0 THEN
				*la ficha de datos complementarios asociada también debe quedar lista para ser notificada
		*		UPDATE EVENTOS_46 SET NOTIFICA=0 WHERE AÑO=m.año AND SEMANA=m.semana AND COD_EVE=m.cod_eve AND ;
				TIP_IDE=m.tip_ide AND NUM_IDE=m.num_ide AND COD_PRE=m.cod_pre AND COD_SUB=m.cod_sub
		*	ENDIF				
		*ENDIF
	ENDIF   

	RELEASE oEvent 
	SELECT (nOldWorkArea)

ENDPROC

FUNCTION checkEpidemiologicalWeek(seed as String, nPriority as Byte) AS Boolean

	*Retorna .T. si la fecha especificada en seed determina la semana epidemiológica de ocurrencia del evento; en este caso nPriority contendrá el valor;
	de prioridad asignado a seed según el orden que ocupe en la lista de campos determinantes de la semana epidemiológica del evento; en caso contrario, ;
	nPriority será cero (0): si, por ejemplo, seed aparece en el primer lugar en la lista de determinantes, nPriority será 1; si aparece en el segundo;
	lugar en la lista, nPriority será 2 y así sucesivamente.
	
	DIMENSION aDiscriminants(1)
	
	bIsDeterminant = ((','+UPPER(seed)+',') $ (','+This.sVAL_SEM+','))
	nPriority = 0
	IF bIsDeterminant THEN 
		nDiscriminants = ALINES(aDiscriminants, This.sVAL_SEM, 1+4+8, ',')
		nPriority = ASCAN(aDiscriminants, seed, -1, -1, -1, 15)
	ENDIF 
	RELEASE aDiscriminants
	
	RETURN bIsDeterminant
ENDFUNC

ENDDEFINE &&CLASS BasiData

