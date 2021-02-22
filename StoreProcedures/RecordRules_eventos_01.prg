FUNCTION RecValidationRuleFor_eventos_01

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Se está notificando un evento de Fiebre Amarilla (310), por tanto el valor en el campo Fiebre debe ser SI (1)'
			bIsValid = EVENTOS_01.FIEBRE='1'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

