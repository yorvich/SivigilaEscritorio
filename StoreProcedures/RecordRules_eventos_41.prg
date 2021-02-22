FUNCTION RecValidationRuleFor_eventos_41

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La diferencia entre las variables  Semanas de gestación y edad gestacional no puede ser superior a 4'
			bIsValid = ABS(EVENTOS_41.SEM_GES - EVENTOS_41.EDAD_GES) <= 4
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Número de embarazos previos no puede ser inferior a la suma de Numero de pérdidas (abortos) y Numero de cesáreas'
			bIsValid = EVENTOS_41.NO_EMBARAZ >= (EVENTOS_41.NO_ABORTOS + EVENTOS_41.NO_CESAREA)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

