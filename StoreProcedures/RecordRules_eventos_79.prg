FUNCTION RecValidationRuleFor_eventos_79

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Cuando se marque la opcion 1 = LEIAG en la variable "Resultado biopsia exocervix",  no puede marcar la opcion "infiltrante" de la variable "Grado histopatológico"'
			bIsValid = EVENTOS_79.RES_B_EXOC!='1' OR EVENTOS_79.GRADO_HIST!='2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables de tratamiento debe registrarse en la opción 1=sí'
			bIsValid = EVENTOS_79.SEG_TRAT_I!='1' OR OCCURS('1', EVENTOS_79.RADIOTERAP + EVENTOS_79.QUIRURGICO + EVENTOS_79.QUIMIOTERA + EVENTOS_79.HORMONOTER + EVENTOS_79.CUID_PALIA + EVENTOS_79.INMUNOTERA) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

