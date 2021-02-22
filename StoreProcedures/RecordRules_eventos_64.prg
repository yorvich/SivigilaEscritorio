FUNCTION RecValidationRuleFor_eventos_64

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el tipo de lesión es única, no puede haber más de una localización anatómica comprometida'
			bIsValid = EVENTOS_64.TIP_LES!='1' OR OCCURS('1',EVENTOS_64.CCC + EVENTOS_64.MAN_DED + EVENTOS_64.TRONCO + EVENTOS_64.MIE_SUP + EVENTOS_64.MIE_INF + EVENTOS_64.PIES_DEDOS + EVENTOS_64.GENIT_EXT) = 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la clasificación de la notificación es 2, entonces la clasificación del caso en datos básicos solo puede ser 2 = Probable o 3 = Confirmado por Laboratorio'
			bIsValid = (CLAS_NOTIF!='2' OR (PACIENTE.TIP_CAS='2' OR PACIENTE.TIP_CAS='3'))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

