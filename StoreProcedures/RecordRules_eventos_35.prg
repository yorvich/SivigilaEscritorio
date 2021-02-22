FUNCTION RecValidationRuleFor_eventos_35

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el tipo de lesión es única, no puede haber más de una localización anatómica comprometida'
			bIsValid = EVENTOS_35.TIP_LES!='1' OR OCCURS('1',EVENTOS_35.CCC + EVENTOS_35.MAN_DED + EVENTOS_35.TRONCO + EVENTOS_35.MIE_SUP + EVENTOS_35.MIE_INF + EVENTOS_35.PIES_DEDOS + EVENTOS_35.GENIT_EXT) = 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

