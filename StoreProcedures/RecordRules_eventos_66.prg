FUNCTION RecValidationRuleFor_eventos_66

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'En menores de 5 años, las opciones 1. Ocupacional, 3. Intencional suicida y 10. Automedicación/autoprescripción, no aplican para "tipo de exposición"'
			bIsValid = !(PACIENTE.UNI_MED!='1' OR (PACIENTE.UNI_MED='1' AND VAL(PACIENTE.EDAD)<5)) OR INLIST(EVENTOS_66.TIP_EXP,'2','4','6','8','9')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

