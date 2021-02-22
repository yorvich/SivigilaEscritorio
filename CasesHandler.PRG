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
