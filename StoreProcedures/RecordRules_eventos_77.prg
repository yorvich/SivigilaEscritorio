FUNCTION RecValidationRuleFor_eventos_77

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Se debe elegir por lo menos un mecanismo'
			bIsValid = OCCURS('1', EVENTOS_77.AHORCAMIEN + EVENTOS_77.ARMA_CORTO + EVENTOS_77.ARMA_FUEGO + EVENTOS_77.INMOLACION + EVENTOS_77.LANZ_AGUA + EVENTOS_77.LANZ_VACIO + EVENTOS_77.LANZ_VEHIC + EVENTOS_77.INTOXICACI) >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la variable intoxicación es 1, debe diligenciar cual tipo de sustancia ocasionó la intoxicación'
			bIsValid = EVENTOS_77.INTOXICACI != '1' OR !EMPTY(EVENTOS_77.TIPO_SUSTA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la variable  trastorno psiquiátricos es 1, es obligatorio que se presente por lo menos 1 de los trastornos psiquiátricos detallados'
			bIsValid = EVENTOS_77.ANTEC_TRAN != '1' OR OCCURS('1', EVENTOS_77.TRAN_DEPRE + EVENTOS_77.TRAST_PERS + EVENTOS_77.TRANS_PERS + EVENTOS_77.ESQUIZOFRE)>=1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

