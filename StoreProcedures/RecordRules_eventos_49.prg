FUNCTION RecValidationRuleFor_eventos_49

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables índice de Dean por diente debe ser mayor de 0'
			bIsValid = (VAL(DEAN_16)+VAL(DEAN_15)+VAL(DEAN_13)+VAL(DEAN_12)+VAL(DEAN_11)+VAL(DEAN_21)+VAL(DEAN_22)+VAL(DEAN_23)+VAL(DEAN_25)+VAL(DEAN_26)+VAL(DEAN_36)+VAL(DEAN_46))>0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

