FUNCTION RecValidationRuleFor_eventos_39

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'En datos clínicos debe haber por lo menos uno'
			bIsValid = AT('1', EVENTOS_39.DOL_CUE + EVENTOS_39.DOL_GAR + EVENTOS_39.IMP_HABLAR + EVENTOS_39.DISFAGIA + EVENTOS_39.CONVULSION + EVENTOS_39.CON_MUSCUL + EVENTOS_39.RIG_MU_ABD + EVENTOS_39.ESP_GENERA + EVENTOS_39.RIG_NUCA + EVENTOS_39.AFE_NER_CR + EVENTOS_39.TRISMUS + EVENTOS_39.OPISTÓTONO +  EVENTOS_39.FIEBRE) != 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

