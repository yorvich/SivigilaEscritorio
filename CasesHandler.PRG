#INCLUDE SIVIGILA.h

FUNCTION lastClasification(sFilter as String)
		
	sReturnValue = ''
		
	sSourceTablename = 'PACIENTE'
	sAdjustmentFieldName = 'AJUSTE'
	sAdjustmentDateFieldName = 'FEC_AJU'
	
	SELECT * FROM &sSourceTablename WHERE &sFilter INTO CURSOR rsCaseRecords
	IF _TALLY > 0 THEN

		sCurrentCaseClasification = rsCaseRecords.TIP_CAS
		bFormerCase=.F.
		bPseudoFormerCase=.F.

		SELECT rsCaseRecords
		dAdjustmentDate = CTOD('')
		dLastSignificantAdjustmentDate = CTOD('')
		
		SCAN
			IF VAL(rsCaseRecords.&sAdjustmentFieldName) = 0 AND !bFormerCase THEN
				*Se trata del registro o reporte inicial del paciente
				bFormerCase=.T.
			ENDIF
			
			IF 	VAL(rsCaseRecords.&sAdjustmentFieldName) > VAL(CLINICAL_DISCARD_ADJUSTMENT) THEN
				*Se trata de un registro que contiene datos modificados del registro inicial del caso excepto en el tipo de caso
				IF rsCaseRecords.&sAdjustmentDateFieldName >= dAdjustmentDate THEN
					bFormerCase = .T.
					dAdjustmentDate = rsCaseRecords.&sAdjustmentDateFieldName
				ENDIF
			ENDIF
			
			IF 	(VAL(rsCaseRecords.&sAdjustmentFieldName) < VAL(CHANGE_ANY_VALUE_ADJUSTMENT)) AND (rsCaseRecords.&sAdjustmentFieldName <> '0' ;
				AND !EMPTY(rsCaseRecords.&sAdjustmentFieldName)) THEN
				*Se trata de un registro que contiene una modificación de la clasificación del caso originalmente registrada
				
				IF rsCaseRecords.&sAdjustmentDateFieldName >= dLastSignificantAdjustmentDate THEN
					sCurrentCaseClasification = rsCaseRecords.&sAdjustmentFieldName
					dLastSignificantAdjustmentDate = rsCaseRecords.&sAdjustmentDateFieldName
				ENDIF
				
				IF !bFormerCase AND !bPseudoFormerCase THEN
					*Aún no se han capturado los datos de las variables asociadas al paciente, por tanto, se capturan pero el caso;
					no puede considerarse como el registro inicial del paciente
					bPseudoFormerCase=.T.
				ENDIF	
			ENDIF
		ENDSCAN

		sReturnValue = sCurrentCaseClasification
		*IF !EMPTY(dLastSignificantAdjustmentDate) THEN
			*Se ha detectado una fecha de ajuste que dió lugar a que se modificara la clasificación inicial del caso
		*	sAssignmentInstruction = 'm.' + This.sAdjustmentDateFieldName + ' = dLastSignificantAdjustmentDate'
		*	&sAssignmentInstruction 
		*ENDIF
	ENDIF	
	RETURN sReturnValue 
ENDFUNC


PROCEDURE lastClasificationTst(sFilter as String)
	*SET STEP ON 
	*?lastClasification("AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='201436459 RC515353243        760010000001'")
	?lastClasification(sFilter)
ENDPROC


FUNCTION lastCaseRecord(sSourceTablename as String,sFilter as String)
	
	*Retorna de la tabla sSourceTablename, el número (RECNO()) del último registro correspondiente a un caso que cumpla ;
	con la condición sFilter(opcional).;
	El criterio para determinar el último registro de un caso es: si en la tabla sSourceTablename existe el campo;
	ADJUSTMENT_DATE_FIELDNAME, el último registro será el que al ordenar descendentemente el recordset que satisfaga ;
	el criterio sFilter	según la clave de ordenación ADJUSTMENT_DATE_FIELDNAME DESC, RECNO() DESC ocupe el primer puesto; 
	en caso contrario, el último registro será aquel que al ordenar descendentemente de acuerdo con RECNO(), ;
	ocupe el primer puesto
	
		
	LOCAL sOldProc as String, sReturnValue  as Number, nCurrentWorkArea as Number 
	
	sReturnValue = 0
		
	#DEFINE ADJUSTMENT_DATE_FIELDNAME 'FEC_AJU'
	
	nCurrentWorkArea = SELECT()
	
	IF VARTYPE(sFilter)='L' THEN
		sFilter = '.T.'
	ENDIF
	
	sOldProc = SET("Procedure")
	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	IF isField(sSourceTablename, ADJUSTMENT_DATE_FIELDNAME) THEN
		sOrderByClause = ' ORDER BY ' + ADJUSTMENT_DATE_FIELDNAME + ' DESC, nRec DESC'
	ELSE
		sOrderByClause = ' ORDER BY nRec DESC'
	ENDIF
	
	SELECT *, RECNO() as nRec FROM &sSourceTablename WHERE &sFilter &sOrderByClause INTO CURSOR rsCaseRecords
	IF _TALLY > 0 THEN
		sReturnValue = rsCaseRecords.nRec
	ENDIF
	SET PROCEDURE TO &sOldProc 
	
	SELECT (nCurrentWorkArea)
	
	RETURN sReturnValue 
ENDFUNC


PROCEDURE lastCaseRecordTst()

	?lastCaseRecord('EVENTOS_05','NUM_IDE="91175941"')
	*?lastCaseRecord('PACIENTE','NUM_IDE="91175941"')
ENDPROC


PROCEDURE AdjustIndividualCase_V00(sEventCode AS String, sFilter as String)
	
	PUBLIC gsXLSFilter as String
	gsXLSFilter = sFilter
	*Variable pública usada en los pre-procesamientos que dependan de ella -según se encuentren establecidos en tables.ExportQueries

	LOCAL oCurrentEvent  as object
	oCurrentEvent = NEWOBJECT("Event","Event.fxp",.NULL.,sEventCode)	

	LOCAL sWHERE_SQL as String 
	sWHERE_SQL = ' '
	
	LOCAL nRecordsToTraverse as Number, nIndividualNotifications as Number 
	nRecordsToTraverse = 0
	nIndividualNotifications = 0

	LOCAL nMajor, nMinor, nBuild
	nMajor = 0
	nMinor = 0
	nBuild = 0
	
	LOCAL sTmpTable as string
	sTmpTable = ADDBS(SYS(2023)) + SYS(2015)
	
	SET PROCEDURE TO PlainsLib ADDITIVE 
	
	* Se determina si el evento tiene datos complementarios
	HasComplementaryData = .T.

 	*Garantiza que el procesamiento de ajustes siempre se lleve a cabo de acuerdo con la última versión del sistema definida;
	por sus identificadores nMajor.nMinor.nBuild según se encuentran definidos en main
	DO resetSivigilaVersion_V02 WITH nMajor, nMinor, nBuild	IN main

	sLinkerExpr = ''
	sPostCmds = ''
	sEventTable = ''
	XlsQuery = GetQryString("E" + PADL(oCurrentEvent.nFORMULARIO, 2, '0'), @sEventTable, nMajor, nMinor, nBuild, .T., .T., ;
							@sLinkerExpr, , , @sPostCmds, !oCurrentEvent.isActive())

	*Establece las condiciones de filtrado para la extracción de registros
	IF !EMPTY(ALLTRIM(sEventCode )) THEN 
		sWHERE_SQL = sWHERE_SQL + ' A.COD_EVE ="' + sEventCode  + '" AND '
	ENDIF 
	
	sWHERE_SQL = sWHERE_SQL + IIF(AT("#",sFilter)=0,sFilter,STRTRAN(sFilter,"#")) 

	IF OCCURS('#',XlsQuery) > 0  THEN 
		*El query de exportación tiene placeholder, por tanto es reemplazado por la condición WHERE 
		XlsQuery = STRTRAN(XlsQuery, '#', sWHERE_SQL )
	ELSE 
		XlsQuery =  XlsQuery + ' WHERE ' + sWHERE_SQL
	ENDIF
	
	XlsQuery =  XlsQuery + ' INTO TABLE "' + (sTmpTable) + '" DATABASE SIVIGILATemp'

	sOldDB = SET("Database")
	OPEN DATABASE SIVIGILATemp
	SET  DATABASE TO SIVIGILATemp
	TRY
		REMOVE TABLE JUSTSTEM(sTmpTable) DELETE
	CATCH TO oException
	ENDTRY
	&XlsQuery 
	nRecordsToTraverse = RECCOUNT(JUSTSTEM(sTmpTable))

	WAIT 'Procesando ' + STR(nRecordsToTraverse) + ' registros del evento ' + sEventCode  WINDOW NOWAIT TIMEOUT 10
	*?'Inicio: ' + TIME()
*SET STEP ON 
	IF nRecordsToTraverse > 0
		* se encontraron registros. Se procesan  entonces los ajustes que quedarán en un cursor de nombre rsAdjustedCases

		nIndividualNotifications = _TALLY 
		
		sTagExpr = 'AÑO+SEMANA+COD_EVE+TIP_IDE_+NUM_IDE_+COD_PRE+COD_SUB'
		oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.fxp")
		oAdjustmentsProcessor.sSourceTablename = JUSTSTEM(sTmpTable)
		oAdjustmentsProcessor.sAdjustmentFieldName = 'AJUSTE_'
		oAdjustmentsProcessor.sAdjustmentDateFieldName = 'FEC_AJU_'
		IF EMPTY(sLinkerExpr) THEN
			oAdjustmentsProcessor.processAdjustments(sTagExpr)
		ELSE
			oAdjustmentsProcessor.processAdjustments(sLinkerExpr)
		ENDIF
		RELEASE oAdjustmentsProcessor
		
		SELECT rsAdjustedCases

		*Ejecuta post comandos de exportación si es que existen
        IF !EMPTY(sPostCmds) THEN
        	TRY
				&sPostCmds
			CATCH TO oException
			ENDTRY 
		ENDIF

	ENDIF

	TRY
		REMOVE TABLE JUSTSTEM(sTmpTable) DELETE
	CATCH TO oException
	ENDTRY
	SET DATABASE TO &sOldDB

	RELEASE gsXLSFilter
	
	*?'Final: ' + TIME()

ENDPROC 

PROCEDURE AdjustIndividualCase(sEventCode AS String, sFilter as String)
	
	LOCAL oCurrentEvent  as object
	oCurrentEvent = NEWOBJECT("Event","Event.fxp",.NULL.,sEventCode)	

	LOCAL sWHERE_SQL as String 
	sWHERE_SQL = ' '
	
	LOCAL nRecordsToTraverse as Number, nIndividualNotifications as Number 
	nRecordsToTraverse = 0
	nIndividualNotifications = 0

	LOCAL sTmpTable as string
	sTmpTable = ADDBS(SYS(2023)) + SYS(2015)
	
	SET PROCEDURE TO PlainsLib ADDITIVE 
	
	* Se determina si el evento tiene datos complementarios
	HasComplementaryData = .T.
	
	sEventTable = ''

	*Establece las condiciones de filtrado para la extracción de registros
	IF !EMPTY(ALLTRIM(sEventCode )) THEN 
		sWHERE_SQL = sWHERE_SQL + ' COD_EVE ="' + sEventCode  + '" AND '
	ENDIF 
	sWHERE_SQL = sWHERE_SQL + sFilter
	
	*Establece la instrucción SQL que permite unir datos básicos y complementarios del evento sEventCode. El renombramiento de los campos;
	COD_EVE_A y TIP_SS obedece a los requerimientos de nombres del procesador de ajustes
	XlsQuery = 'SELECT A.COD_EVE_A AS COD_EVE, A.TIP_SS AS TIP_SS_, A.* FROM (select * from ' +;
				'(select * from Paciente A where &sWHERE_SQL) AS rsDB ' +;
				'left outer join (SELECT * FROM ' + oCurrentEvent.sDC_TABLA + ' WHERE &sWHERE_SQL ORDER BY AJUSTE,FEC_AJU) as H  ' +;
				'on rsDB.cod_eve = H.cod_eve AND rsDB.año = H.año AND rsDB.semana = H.semana AND  rsDB.tip_ide = H.tip_ide  ' +;
				'AND rsDB.num_ide = H.num_ide  AND  rsDB.cod_pre = H.cod_pre AND rsDB.cod_sub = H.cod_sub) as A'
 
	XlsQuery =  XlsQuery + ' INTO TABLE "' + (sTmpTable) + '" DATABASE SIVIGILATemp'

	sOldDB = SET("Database")
	OPEN DATABASE SIVIGILATemp
	SET  DATABASE TO SIVIGILATemp
	TRY
		REMOVE TABLE JUSTSTEM(sTmpTable) DELETE
	CATCH TO oException
	ENDTRY
	&XlsQuery 
	nRecordsToTraverse = RECCOUNT(JUSTSTEM(sTmpTable))

	WAIT 'Procesando ' + STR(nRecordsToTraverse) + ' registros del evento ' + sEventCode  WINDOW NOWAIT TIMEOUT 10
	*?'Inicio: ' + TIME()
*SET STEP ON 
	IF nRecordsToTraverse > 0
		* se encontraron registros. Se procesan  entonces los ajustes que quedarán en un cursor de nombre rsAdjustedCases

		nIndividualNotifications = _TALLY 
		
		sTagExpr = 'AÑO_A+SEMANA_A+COD_EVE+TIP_IDE_A+NUM_IDE_A+COD_PRE_A+COD_SUB_A'
		oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.fxp")
		oAdjustmentsProcessor.sSourceTablename = JUSTSTEM(sTmpTable)
		oAdjustmentsProcessor.sAdjustmentFieldName = 'AJUSTE_A'
		oAdjustmentsProcessor.sAdjustmentDateFieldName = 'FEC_AJU_A'
		oAdjustmentsProcessor.processAdjustments(sTagExpr)

		RELEASE oAdjustmentsProcessor
		
		SELECT rsAdjustedCases
	ENDIF

	TRY
		REMOVE TABLE JUSTSTEM(sTmpTable) DELETE
	CATCH TO oException
	ENDTRY
	SET DATABASE TO &sOldDB

	
	*?'Final: ' + TIME()

ENDPROC 


PROCEDURE AdjustIndividualCaseTst(sEventCode AS String, sFilter as String )
SET STEP ON 
	*DO AdjustIndividualCase_V00 WITH sEventCode, sFilter 
	DO AdjustIndividualCase WITH sEventCode, sFilter 
ENDPROC 


PROCEDURE makeNotified(nNewValue as byte, sReplacementCondition as string, bUseMasterSlaveRelationship  as Boolean )

	*Establece el campo NOTIFICA de la tabla de datos básicos PACIENTE al valor nNewValue para los registros que cumplan la condición;
	sReplacementCondition. Si se pasa bUseMasterSlaveRelationship y es .T., también actualiza los registros relacionados que se;
	encuentren en la tabla de datos complementarios y en la tabla de laboratorios.
	
	SET PROCEDURE TO SivigilaUtilities ADDITIVE 

	glDisableRules=.T.

	IF bUseMasterSlaveRelationship THEN
		*Establece una línea base de pacientes para los cuales se hará la actualización de registros de datos complementarios, laboratorios ;
		teniendo en cuenta las condiciones de filtrado sReplacementCondition 
		sSQLClause= 'SELECT DISTINCT ' + STRTRAN(qualifyFields(getKeyFields('PACIENTE'), '+' , 'RecordSource'), '+', ',') +;
					' FROM PACIENTE AS RecordSource WHERE ' + sReplacementCondition +;
					' INTO CURSOR rsPatientsBaseLine NOFILTER'
		&sSQLClause
	ENDIF

	UPDATE PACIENTE SET NOTIFICA = nNewValue WHERE &sReplacementCondition 

	IF bUseMasterSlaveRelationship THEN
		=UpdateSIVIGILAComplementaryData('rsPatientsBaseLine', getKeyFields('PACIENTE'), , , 'NOTIFICA = ' + ALLTRIM(STR(nNewValue)) )
	ENDIF
	
	glDisableRules=.F.
ENDPROC


PROCEDURE makeNotifiedTst(nNewValue as Byte, sReplacementCondition as string, bUseMasterSlaveRelationship  as Boolean )
SET STEP ON 
	DO makeNotified WITH nNewValue, sReplacementCondition, bUseMasterSlaveRelationship  
ENDPROC 