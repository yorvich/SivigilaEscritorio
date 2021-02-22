FUNCTION RecValidationRuleFor_eventos_75

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si Profilaxis antibi�tica quir�rgica es 1, debe marcarse "Tiempo entre finalizaci�n de profilaxis e incisi�n quir�rgica o parto"'
			bIsValid = EVENTOS_75.PRF_ANTIB!='1' OR !EMPTY(EVENTOS_75.TIEM_ENTRE)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

