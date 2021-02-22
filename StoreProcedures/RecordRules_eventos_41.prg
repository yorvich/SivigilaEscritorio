FUNCTION RecValidationRuleFor_eventos_41

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La diferencia entre las variables  Semanas de gestaci�n y edad gestacional no puede ser superior a 4'
			bIsValid = ABS(EVENTOS_41.SEM_GES - EVENTOS_41.EDAD_GES) <= 4
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El N�mero de embarazos previos no puede ser inferior a la suma de Numero de p�rdidas (abortos) y Numero de ces�reas'
			bIsValid = EVENTOS_41.NO_EMBARAZ >= (EVENTOS_41.NO_ABORTOS + EVENTOS_41.NO_CESAREA)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

