FUNCTION RecValidationRuleFor_Eventos_45

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el paciente pertenece al grupo poblacional Gestantes, es obligatorio diligenciar el trimestre de gestación'
			bIsValid = fieldValue('GP_GESTAN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_45.AÑO + EVENTOS_45.SEMANA+EVENTOS_45.COD_EVE+EVENTOS_45.TIP_IDE+EVENTOS_45.NUM_IDE+EVENTOS_45.COD_PRE+EVENTOS_45.COD_SUB + "'", "AJUSTE DESC" )!='1' OR !EMPTY(EVENTOS_45.TRIMESTRE)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

