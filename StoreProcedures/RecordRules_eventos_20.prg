FUNCTION RecValidationRuleFor_eventos_20

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de inicio de investigación no puede ser anterior a la fecha de inicio de síntomas'
			bIsValid = EMPTY(EVENTOS_20.fec_ii) OR (EVENTOS_20.fec_ii >= fieldValue('INI_SIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_20.AÑO + EVENTOS_20.SEMANA+EVENTOS_20.COD_EVE+EVENTOS_20.TIP_IDE+EVENTOS_20.NUM_IDE+EVENTOS_20.COD_PRE+EVENTOS_20.COD_SUB  + "'", "AJUSTE DESC"))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

