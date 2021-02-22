FUNCTION RecValidationRuleFor_eventos_90

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Mes y año de vigilancia deben ser iguales o inferiores al mes y año de la fecha de notificación.'
			bIsValid = isLessThanOrEqual(VAL(EVENTOS_90.MES),VAL(EVENTOS_90.AÑO),MONTH(EVENTOS_90.FEC_NOT),YEAR(EVENTOS_90.FEC_NOT))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

