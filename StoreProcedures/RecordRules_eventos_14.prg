FUNCTION RecValidationRuleFor_eventos_14

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables de datos clínicos debe registrarse en la opción 1=sí'
			bIsValid = OCCURS('1', EVENTOS_14.FIEBRE + EVENTOS_14.AMIGDALITI + EVENTOS_14.FARINGITIS + EVENTOS_14.LARINGITIS + EVENTOS_14.PRE_MEM) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

