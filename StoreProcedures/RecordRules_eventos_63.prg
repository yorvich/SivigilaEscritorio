FUNCTION RecValidationRuleFor_eventos_63

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Las variables segunda neoplasia y recaida son excluyentes'
			bIsValid = EVENTOS_63.CONSX2_NEO != '1' OR EVENTOS_63.RECAIDA = '2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las variables segunda neoplasia y recaida son excluyentes'
			bIsValid = EVENTOS_63.RECAIDA != '1' OR  EVENTOS_63.CONSX2_NEO = '2'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

