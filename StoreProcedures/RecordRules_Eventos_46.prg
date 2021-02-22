FUNCTION RecValidationRuleFor_Eventos_46

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si hubo desplazamiento en los últimos cinco días, al menos se debe diligenciar una fecha de llegada'
			bIsValid = EVENTOS_46.DESP!='1' OR !EMPTY(EVENTOS_46.FEC_LLEG1) OR !EMPTY(EVENTOS_46.FEC_LLEG2) OR !EMPTY(EVENTOS_46.FEC_LLEG3) OR !EMPTY(EVENTOS_46.FEC_LLEG4)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

