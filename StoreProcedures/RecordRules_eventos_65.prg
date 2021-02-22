FUNCTION RecValidationRuleFor_eventos_65

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Las opciones 6 = QUININA INTRAVENOSA y 7 = ARTESUNATO, de la variable tratamiento, solo son admisibles cuando se seleccione la opción 1 en la variable complicación'
			bIsValid = !(EVENTOS_65.TRATAMIENT $ ' 6 7 ') OR EVENTOS_65.COMPLICACI='1'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la malaria presenta complicaciones, al menos una de las complicaciones específicas debe ser igual a 1'
			bIsValid = EVENTOS_65.COMPLICACI!='1' OR OCCURS('1',EVENTOS_65.COM_CEREBR+EVENTOS_65.COM_RENAL+EVENTOS_65.COM_HEPATI+EVENTOS_65.COM_PULMON+EVENTOS_65.COM_HEMATO+EVENTOS_65.COM_OTRAS)>0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

