FUNCTION RecValidationRuleFor_eventos_05

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Fecha de la Última Dosis No puede ser superior a la fecha de agresión'
			bIsValid = EVENTOS_05.f_ult_dos  <= EVENTOS_05.fec_exp
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'En los datos de localización anatómica, no puede registrar a todos NO (2) debe haber por lo menos un SI (1)'
			bIsValid = AT('1', EVENTOS_05.ccc + EVENTOS_05.man_ded + EVENTOS_05.tronco + EVENTOS_05.mie_sup + EVENTOS_05.mie_inf + EVENTOS_05.PIES_DEDOS + EVENTOS_05.GENIT_EXT) != 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

