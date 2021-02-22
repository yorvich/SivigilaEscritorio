FUNCTION RecValidationRuleFor_eventos_02

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'la Fecha de exposici�n NO debe ser mayor que la fecha de notificaci�n'
			bIsValid = EVENTOS_02.FEC_EXP <= getBasicDataValue('FEC_NOT', 'PACIENTE', 'A�O + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB', EVENTOS_02.A�O + EVENTOS_02.SEMANA+EVENTOS_02.COD_EVE+EVENTOS_02.TIP_IDE+EVENTOS_02.NUM_IDE+EVENTOS_02.COD_PRE+EVENTOS_02.COD_SUB )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

