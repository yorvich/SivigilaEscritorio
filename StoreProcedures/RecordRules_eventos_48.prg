FUNCTION RecValidationRuleFor_eventos_48

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si la  paciente fue remitida, la Institución de referencia es de obligatorio diligenciamiento'
			bIsValid = EVENTOS_48.PTE_REMTDA!='1' OR !EMPTY(EVENTOS_48.INST_REFE1)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

