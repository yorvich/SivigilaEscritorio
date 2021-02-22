FUNCTION RecValidationRuleFor_eventos_23

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El n�mero de expuestos NO DEBE ser menor al n�mero de enfermos'
			bIsValid = EVENTOS_23.EXPUESTOS >= EVENTOS_23.ENFERMOS
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El per�odo de incubaci�n m�s largo NO puede ser MENOR que el per�odo de incubaci�n m�s corto'
			bIsValid = BETWEEN(compareTimePeriod(EVENTOS_23.Per_in_cor,EVENTOS_23.Per_in_lar),-1,0)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El n�mero de expuestos DEBE ser mayor que 1'
			bIsValid = EVENTOS_23.EXPUESTOS > 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

