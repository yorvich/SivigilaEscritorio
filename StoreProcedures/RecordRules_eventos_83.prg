FUNCTION RecValidationRuleFor_eventos_83

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El Zscore talla/edad debe estar entre -6de y 6de inclusive'
			bIsValid = EVENTOS_83.ZSCORE_TE >= -8 AND EVENTOS_83.ZSCORE_TE <=6
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'No cumple la definición de caso: el Zscore peso/talla debe estar entre -6DE y -2DE'
			bIsValid = EVENTOS_83.EDEMA!='2' OR (EVENTOS_83.ZSCORE_PT > -6 AND EVENTOS_83.ZSCORE_PT < -2)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

