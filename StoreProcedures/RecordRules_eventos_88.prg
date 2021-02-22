FUNCTION RecValidationRuleFor_eventos_88

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La variable días paciente excede el valor permitido de acuerdo al número de camas'
			bIsValid = VAL(EVENTOS_88.N_D_P1_S_N) + VAL(EVENTOS_88.N_D_P2_S_N) + VAL(EVENTOS_88.N_D_P3_S_N) + VAL(EVENTOS_88.N_D_P4_S_N) + VAL(EVENTOS_88.N_D_P5_S_N) <= (VAL(EVENTOS_88.CAM_UCI_N) * daysOfMonth(VAL(EVENTOS_88.AÑO), VAL(EVENTOS_88.MES)))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La variable días paciente excede el valor permitido de acuerdo al número de camas'
			bIsValid = VAL(EVENTOS_88.N_D_P1_SIN) + VAL(EVENTOS_88.N_D_P2_SIN) + VAL(EVENTOS_88.N_D_P3_SIN) + VAL(EVENTOS_88.N_D_P4_SIN) + VAL(EVENTOS_88.N_D_P5_SIN) <= (VAL(EVENTOS_88.CAM_UCI_IN) * daysOfMonth(VAL(EVENTOS_88.AÑO), VAL(EVENTOS_88.MES)))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

