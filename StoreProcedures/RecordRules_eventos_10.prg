FUNCTION RecValidationRuleFor_eventos_10

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si la variable bajo peso al nacer  es igual a 2, el peso debe ser superior o igual a 2500 gramos'
			bIsValid = EVENTOS_10.BPN!='2' OR VAL(EVENTOS_10.PESO)>=2500
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la variable bajo peso al nacer  es igual a 1, el peso debe ser inferior a 2500 gramos'
			bIsValid = EVENTOS_10.BPN!='1' OR VAL(EVENTOS_10.PESO)<2500
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

