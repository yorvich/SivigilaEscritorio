FUNCTION RecValidationRuleFor_eventos_23

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El número de expuestos NO DEBE ser menor al número de enfermos'
			bIsValid = EVENTOS_23.EXPUESTOS >= EVENTOS_23.ENFERMOS
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El período de incubación más largo NO puede ser MENOR que el período de incubación más corto'
			bIsValid = BETWEEN(compareTimePeriod(EVENTOS_23.Per_in_cor,EVENTOS_23.Per_in_lar),-1,0)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El número de expuestos DEBE ser mayor que 1'
			bIsValid = EVENTOS_23.EXPUESTOS > 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

