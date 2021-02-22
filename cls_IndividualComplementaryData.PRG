#INCLUDE SIVIGILA.H

DEFINE CLASS IndividualComplementaryData as Custom

HIDDEN sSourceTableName
sSourceTableName = ''
*Nombre de la tabla que almacena datos complementarios de notificación en el sistema y sobre la cual se basa la clase IndividualComplementaryData 

HIDDEN sDefaultKey
sDefaultKey = 'AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB'
*Campos que, por default, constituyen la "clave" que identifica un caso notificado de datos complementarios. Un caso puede tener más de un registro;
normalmente: estará constituido por un registro índice y uno o más registros de ajuste

HIDDEN sLastRecordSet
sLastRecordSet = ''
*Nombre del cursor que contiene los registros de un caso identificado por la clave sDefaultKey


*Atributos de la clase IndividualComplementaryData que corresponden uno-a-uno con campos de la tabla sSourceTableName
sCOD_EVE = .NULL.
sSEMANA = .NULL.
sAÑO = .NULL.
sCOD_PRE = .NULL.
sCOD_SUB = .NULL.
sTIP_IDE = .NULL.
sNUM_IDE = .NULL.
sAJUSTE = .NULL.
dFEC_AJU = .NULL.
nNOTIFICA = .NULL.
nFLU_NOT = .NULL.
sINMEDIATA = .NULL.
dFECHACARGA = .NULL.
nEST_NOTIF = .NULL.
*-----------------------------------------------------------------------------------------------------------------------------------------------*

*Atributos de la clase IndividualComplementaryData que NO corresponden a campos de la tabla sSourceTableName

bIsDefined = .T.
*Indica que el dato complementario está definido en la tabla sSourceTableName

oComplementaryData  = .NULL.
*Objeto que contiene los datos complementarios NO comunes específicos de la notificación correspondiente al caso identificado por This.sDefaultKey
*-----------------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION Init(sEventCode as string, sBDKey as String)
	*Construye un recordset con los registros que conforman el caso de notifcación de datos complementarios	del evento con código sEventCode;
	según la clave de agrupamiento sBDKey. Esta clave debe ser alguna realización o valor concreto de This.sDefaultKey. ;
	El recordset construido quedará con un nombre establecido en This.sLastRecordSet 
	
	LOCAL sSQLCmd as String

	LOCAL nOldWorkArea as Number
	nOldWorkArea = SELECT()

	LOCAL oEvent as Object
	oEvent = NEWOBJECT("Event","Event.fxp",.NULL.,sEventCode )
	
	
	IF oEvent.bHasComplementaryData THEN 
		sRSIndividualComplementaryDataRecs = 'rs' + SYS(2015)
		sSQLCmd = 'SELECT RECNO() AS RecId,* FROM ' + oEvent.sDC_TABLA + ' WHERE ' + This.sDefaultKey + ' == "' + sBDKey  +;
					'" ORDER BY FEC_AJU INTO CURSOR ' + sRSIndividualComplementaryDataRecs + ' NOFILTER'
		&sSQLCmd
		This.sLastRecordSet = sRSIndividualComplementaryDataRecs 
		IF _TALLY > 0 THEN
			WITH This
				.bIsDefined = .T.
				.sCOD_EVE = &sRSIndividualComplementaryDataRecs..COD_EVE
				.sSEMANA = &sRSIndividualComplementaryDataRecs..SEMANA
				.sAÑO = &sRSIndividualComplementaryDataRecs..AÑO
				.sCOD_PRE = &sRSIndividualComplementaryDataRecs..COD_PRE
				.sCOD_SUB = &sRSIndividualComplementaryDataRecs..COD_SUB
				.sTIP_IDE = &sRSIndividualComplementaryDataRecs..TIP_IDE
				.sNUM_IDE = &sRSIndividualComplementaryDataRecs..NUM_IDE
				.sAJUSTE = &sRSIndividualComplementaryDataRecs..AJUSTE
				.dFEC_AJU = &sRSIndividualComplementaryDataRecs..FEC_AJU
				.nNOTIFICA = &sRSIndividualComplementaryDataRecs..NOTIFICA
				.nFLU_NOT = &sRSIndividualComplementaryDataRecs..FLU_NOT
				.sINMEDIATA = &sRSIndividualComplementaryDataRecs..INMEDIATA
				.dFECHACARGA = &sRSIndividualComplementaryDataRecs..FECHACARGA
				.nEST_NOTIF = &sRSIndividualComplementaryDataRecs..EST_NOTIF
			ENDWITH
		ELSE
			WITH THIS
				.bIsDefined = .F.
				.sCOD_EVE = .NULL.
				.sSEMANA = .NULL.
				.sAÑO = .NULL.
				.sCOD_PRE = .NULL.
				.sCOD_SUB = .NULL.
				.sTIP_IDE = .NULL.
				.sNUM_IDE = .NULL.
				.sAJUSTE = .NULL.
				.dFEC_AJU = .NULL.
				.nNOTIFICA = .NULL.
				.nFLU_NOT = .NULL.
				.sINMEDIATA = .NULL.
				.dFECHACARGA = .NULL.
				.nEST_NOTIF = .NULL.
			ENDWITH
		ENDIF
	ENDIF
	
	RELEASE oEvent
	SELECT (nOldWorkArea) 
ENDFUNC


FUNCTION Destroy
ENDFUNC

PROCEDURE Refresh(sEventCode as string, sBDKey as String)
	IF !EMPTY(This.sLastRecordSet) THEN 
		TRY 
			USE IN (This.sLastRecordSet)
		CATCH TO oException
		ENDTRY 
	ENDIF
		
	This.Init(sEventCode,sBDKey)
ENDPROC


FUNCTION lastCaseRecord(sFilter as String)
	
	*Retorna de la tabla This.sLastRecordSet, el número (RECNO()) del último registro correspondiente a un caso que cumpla ;
	con la condición sFilter(opcional); en caso de que no exista tal registro, retorna el número del último registro de This.sLastRecordSet.;
	El  último registro de un caso será el que ocupe el primer puesto al ordenar descendentemente el recordset que satisfaga ;
	el criterio sFilter	según la clave de ordenación ADJUSTMENT_DATE_FIELDNAME DESC, RECNO() DESC.
	
	#DEFINE ADJUSTMENT_DATE_FIELDNAME 'FEC_AJU'
		
	LOCAL sReturnValue as Number, nCurrentWorkArea as Number 

	DIMENSION aLastComplementaryRecord(1)
	
	sSourceTablename = This.sLastRecordSet
	sReturnValue = 0
	
	nCurrentWorkArea = SELECT()
	
	IF VARTYPE(sFilter)='L' THEN
		sFilter = '.T.'
	ENDIF
	
	sOrderByClause = ' ORDER BY ' + ADJUSTMENT_DATE_FIELDNAME + ' DESC, RecId DESC'
	
	SELECT RecId FROM &sSourceTablename WHERE &sFilter &sOrderByClause INTO ARRAY aLastComplementaryRecord
	IF _TALLY > 0 THEN
		sReturnValue = aLastComplementaryRecord(1)
	ELSE 
		SELECT (sSourceTablename)
		GO BOTTOM 
		sReturnValue = &sSourceTablename..RecId
	ENDIF
	RELEASE aLastComplementaryRecord
	SELECT (nCurrentWorkArea)
	
	RETURN sReturnValue 
ENDFUNC

*JLGB 28/01/2020
*Indicar a los demas proccesos que se encontraron registros en datos complementarios para el evento
FUNCTION IsDefined() as Boolean 
	RETURN this.bIsDefined 
ENDFUNC
 
FUNCTION RecordsetToMem() AS Boolean 
	
	IF this.bIsDefined THEN
		LOCAL oEvent as Objec
		oEvent = NEWOBJECT("Event","Event.fxp",.NULL.,This.sCOD_EVE)
			
		nRectogoto = This.lastCaseRecord()
		SELECT (oEvent.sDC_TABLA)
		GO nRectogoto
		SCATTER NAME this.oComplementaryData 
		
		RELEASE oEvent
	ENDIF
     
	RETURN this.bIsDefined
ENDFUNC



ENDDEFINE &&CLASS IndividualComplementaryData 

