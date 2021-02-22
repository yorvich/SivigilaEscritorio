FUNCTION RecValidationRuleFor_SeguimientoContactos

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si marcó 1 en "Sintomático" , debe diligenciar en 1=Sí, al menos uno de los síntomas'
			bIsValid = SeguimientoContactos.SINTOMATIC!='1' OR OCCURS('1',SeguimientoContactos.TOS+SeguimientoContactos.FIEBRE+SeguimientoContactos.DIF_RES+SeguimientoContactos.ODINOFAGIA+SeguimientoContactos.ADINAMIA)>=1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

