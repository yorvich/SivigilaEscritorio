FUNCTION RecValidationRuleFor_eventos_82

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables de datos clínicos debe registrarse en la opción 1=sí'
			bIsValid = OCCURS('1', EVENTOS_82.EDEMA + EVENTOS_82.DELGADEZ + EVENTOS_82.PIEL_RESE + EVENTOS_82.HIPERPIGM + EVENTOS_82.LES_CABEL + EVENTOS_82.PALIDEZ + EVENTOS_82.DIARREA + EVENTOS_82.DESHIDRATA + EVENTOS_82.FIEBRE + EVENTOS_82.TOS + EVENTOS_82.DIF_RESPIR) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

