FUNCTION RecValidationRuleFor_eventos_52

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si "La desnutrici�n fue la causa b�sica de muerte" es 2=No, es obligatorio diligenciar "La desnutrici�n fue una causa asociada o estado patol�gico de muerte"'
			bIsValid = EVENTOS_52.DES_CBMTE != '2' OR !EMPTY(EVENTOS_52.DES_CPAT)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

