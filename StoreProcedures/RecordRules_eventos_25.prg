FUNCTION RecValidationRuleFor_eventos_25

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de informe no puede se menor a la fecha de inicio de síntomas'
			bIsValid = EVENTOS_25.fec_inf >= fieldValue('INI_SIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_25.AÑO + EVENTOS_25.SEMANA+EVENTOS_25.COD_EVE+EVENTOS_25.TIP_IDE+EVENTOS_25.NUM_IDE+EVENTOS_25.COD_PRE+EVENTOS_25.COD_SUB  + "'", "AJUSTE DESC")
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

