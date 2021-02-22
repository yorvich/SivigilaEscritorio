FUNCTION RecValidationRuleFor_eventos_04

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Se debe marcar por lo menos  un si (1) en cualquiera de los miembros entre parálisis,  paresia y flacidez'
			bIsValid = AT('1',EVENTOS_04.Par_msd + EVENTOS_04.Para_msd + EVENTOS_04.Fla_msd + EVENTOS_04.Par_msi + EVENTOS_04.Para_msi + EVENTOS_04.Fla_msi + EVENTOS_04.Par_mid + EVENTOS_04.Para_mid + EVENTOS_04.Fla_mid + EVENTOS_04.Par_mii + EVENTOS_04.Para_mii + EVENTOS_04.Fla_mii) != 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

