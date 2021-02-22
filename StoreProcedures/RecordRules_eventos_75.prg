FUNCTION RecValidationRuleFor_eventos_75

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si Profilaxis antibiótica quirúrgica es 1, debe marcarse "Tiempo entre finalización de profilaxis e incisión quirúrgica o parto"'
			bIsValid = EVENTOS_75.PRF_ANTIB!='1' OR !EMPTY(EVENTOS_75.TIEM_ENTRE)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

