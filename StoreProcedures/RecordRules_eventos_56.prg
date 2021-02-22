FUNCTION RecValidationRuleFor_eventos_56

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos se debe ingresarse un tipo de defecto, ya sea funcional o malformación congénita'
			bIsValid = !EMPTY(EVENTOS_56.DEFMET_COD) OR !EMPTY(EVENTOS_56.DEFSE1_COD) OR !EMPTY(EVENTOS_56.DEFSE2_COD) OR !EMPTY(EVENTOS_56.MALFO1_COD) OR !EMPTY(EVENTOS_56.MALFO2_COD) OR !EMPTY(EVENTOS_56.MALFO3_COD) OR !EMPTY(EVENTOS_56.MALFO4_COD) OR !EMPTY(EVENTOS_56.MALFO5_COD)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

