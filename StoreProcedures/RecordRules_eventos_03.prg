FUNCTION RecValidationRuleFor_eventos_03

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si se selecciona Paucibasilar los valores permitidos para el número de lesiones son de 1 a 5'
			bIsValid = EVENTOS_03.CLA_CLINIC != '1' OR BETWEEN(VAL(EVENTOS_03.NUM_LESION),1,5)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = ' Si se selecciona Multibacilar, los valores permitidos para el número de lesiones son de 6 a 99'
			bIsValid = EVENTOS_03.CLA_CLINIC != '2' OR BETWEEN(VAL(EVENTOS_03.NUM_LESION),6,99)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

