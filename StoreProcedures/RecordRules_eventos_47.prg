FUNCTION RecValidationRuleFor_eventos_47

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Los datos registrados en tipo de documento de identidad, edad y unidad de medida no son consistentes'
			bIsValid = isValidIdentificationDoc(EVENTOS_47.TIP_DOC_RN, '3', ALLTRIM(STR(EVENTOS_47.EDAD_RN)))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

