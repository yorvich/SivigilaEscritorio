FUNCTION RecValidationRuleFor_eventos_38

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si ha sido valorado por ninguno es igual a 1, las restantes valoraciones deben ser 2; Si ha sido valorado por ninguno es 2, almenos una de las restantes valoraciones debe ser 1'
			bIsValid = EVENTOS_38.VAL_NINGUN!='2' OR (EVENTOS_38.val_med_gr='1' OR EVENTOS_38.val_pediat='1' OR EVENTOS_38.val_endocr='1' OR EVENTOS_38.val_neurol='1' OR EVENTOS_38.val_geneti='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

