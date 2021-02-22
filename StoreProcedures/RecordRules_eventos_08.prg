FUNCTION RecValidationRuleFor_eventos_08

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si en Atención hospitalaria, Empleó Suero  = Si, En la variable tiempo transcurrido, El valor para horas debe estar entre 0 y 24'
			bIsValid = EVENTOS_08.EMP_SUE != '1' OR !EMPTY(EVENTOS_08.tie_adm) OR BETWEEN(VAL(SUBSTR(EVENTOS_08.tie_adm,1,2)),0,24)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en Atención hospitalaria, Empleó Suero  = Si,  En la variable tiempo transcurrido, El valor para minutos debe estar entre 0 y 59'
			bIsValid = EVENTOS_08.EMP_SUE != '1' OR !EMPTY(EVENTOS_08.tie_adm) OR BETWEEN(VAL(SUBSTR(EVENTOS_08.tie_adm,4,2)),0,59)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en Atención hospitalaria, Empleó Suero  = Si,  la variable tiempo transcurrido no puede ser 00:00'
			bIsValid = EVENTOS_08.EMP_SUE!='1' OR (VAL(STRTRAN(EVENTOS_08.tie_adm,':')) != 0 )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

