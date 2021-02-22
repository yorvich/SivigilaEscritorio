FUNCTION RecValidationRuleFor_eventos_80

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La suma de las variables Día Paciente UCI_N  no puede ser mayor al producto del número de camas por el número de días del mes notificado'
			bIsValid = VAL(EVENTOS_80.N_D_P1_S_N) + VAL(EVENTOS_80.N_D_P2_S_N) + VAL(EVENTOS_80.N_D_P3_S_N) + VAL(EVENTOS_80.N_D_P4_S_N) + VAL(EVENTOS_80.N_D_P5_S_N) <= (VAL(EVENTOS_80.CAM_UCIV_N) * daysOfMonth(VAL(EVENTOS_80.AÑO), VAL(EVENTOS_80.MES)))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

